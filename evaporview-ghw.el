;;; evaporview-ghw.el --- GHW (GHDL waveform) reader for evaporview  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pcsf

;; Author: Pcsf <7274075+Pcsf@users.noreply.github.com>
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/Pcsf/evaporview

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reads GHDL waveform (GHW) files into the same wave model the VCD
;; reader produces.  Unlike GHDL's VCD output, GHW keeps record signals.
;;
;; Mapping:
;; - design units, instances, blocks and generate iterations become scopes;
;;   scopes that hold no signal anywhere below them are dropped;
;; - a record signal becomes a scope of type "record" holding its fields;
;; - a one-dimensional array of std_logic, bit or boolean becomes one
;;   variable, bit string left element first;
;; - any other array becomes a scope of type "array" whose elements are
;;   named like mem(3);
;; - integers and physical types are two's complement bit strings, reals are
;;   decimal strings, other enumerations keep their literal names.
;;
;; The file format has no specification beyond GHDL's own reader, which this
;; file follows.  Times are femtoseconds.  Files compressed with gzip are
;; read when Emacs has zlib; bzip2 files are refused.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'evaporview-vcd)

(declare-function zlib-available-p "decompress.c")
(declare-function zlib-decompress-region "decompress.c")

(define-error 'evaporview-ghw-eof "Unexpected end of GHW data")

;;;; Type table

(cl-defstruct (evaporview-ghw--ty (:constructor evaporview-ghw--ty-make)
                                  (:copier nil))
  kind name id base el lits wkt rngs dims fields nbr tab ctab)

(defconst evaporview-ghw--k-b2 22)
(defconst evaporview-ghw--k-e8 23)
(defconst evaporview-ghw--k-i32 25)
(defconst evaporview-ghw--k-i64 26)
(defconst evaporview-ghw--k-f64 27)
(defconst evaporview-ghw--k-p32 28)
(defconst evaporview-ghw--k-p64 29)
(defconst evaporview-ghw--k-array 31)
(defconst evaporview-ghw--k-record 32)
(defconst evaporview-ghw--k-sscalar 34)
(defconst evaporview-ghw--k-sarray 35)
(defconst evaporview-ghw--k-uarray 37)
(defconst evaporview-ghw--k-srecord 38)
(defconst evaporview-ghw--k-urecord 39)

;;;; Reader state

(defvar evaporview-ghw--be nil "Non-nil when the file stores words big endian.")
(defvar evaporview-ghw--version 0)
(defvar evaporview-ghw--strings nil)
(defvar evaporview-ghw--types nil)
(defvar evaporview-ghw--sigtype nil "Base type of each signal element.")
(defvar evaporview-ghw--emap nil "For each signal element, its (TRACK . POSITION) list.")
(defvar evaporview-ghw--tracks nil "Hash table from track key to track.")
(defvar evaporview-ghw--track-list nil)
(defvar evaporview-ghw--dirty nil)
(defvar evaporview-ghw--vars nil)
(defvar evaporview-ghw--paths nil)
(defvar evaporview-ghw--sigs nil)

(cl-defstruct (evaporview-ghw--trk (:constructor evaporview-ghw--trk-make)
                                   (:copier nil))
  enc sig tab ctab buf cur dirty width)

;;;; Byte decoding

(defsubst evaporview-ghw--byte ()
  "Return the byte at point and advance."
  (let ((c (char-after)))
    (if c
        (progn (forward-char) c)
      (signal 'evaporview-ghw-eof nil))))

(defun evaporview-ghw--uleb ()
  "Read an unsigned LEB128 integer."
  (let ((r 0) (sh 0) c)
    (while (progn (setq c (evaporview-ghw--byte)
                        r (logior r (ash (logand c 127) sh))
                        sh (+ sh 7))
                  (>= c 128)))
    r))

(defun evaporview-ghw--sleb ()
  "Read a signed LEB128 integer."
  (let ((r 0) (sh 0) c)
    (while (progn (setq c (evaporview-ghw--byte)
                        r (logior r (ash (logand c 127) sh))
                        sh (+ sh 7))
                  (>= c 128)))
    (if (/= 0 (logand c 64)) (- r (ash 1 sh)) r)))

(defun evaporview-ghw--word (n)
  "Read an N-byte unsigned integer in the file's byte order."
  (let ((r 0))
    (dotimes (i n)
      (let ((c (evaporview-ghw--byte)))
        (setq r (if evaporview-ghw--be
                    (logior (ash r 8) c)
                  (logior r (ash c (* 8 i)))))))
    r))

(defun evaporview-ghw--i32 ()
  (let ((r (evaporview-ghw--word 4)))
    (if (>= r #x80000000) (- r #x100000000) r)))

(defun evaporview-ghw--i64 ()
  (let ((r (evaporview-ghw--word 8)))
    (if (>= r (ash 1 63)) (- r (ash 1 64)) r)))

(defun evaporview-ghw--f64 ()
  "Read an IEEE 754 double."
  (let* ((w (evaporview-ghw--word 8))
         (m (logand w (1- (ash 1 52))))
         (e (logand (ash w -52) 2047))
         (v (cond ((= e 2047) (if (= m 0) 1.0e+INF 0.0e+NaN))
                  ((= e 0) (ldexp (float m) -1074))
                  (t (ldexp (float (logior m (ash 1 52))) (- e 1075))))))
    (if (>= w (ash 1 63)) (- v) v)))

(defun evaporview-ghw--tag ()
  "Return the next four-byte section tag without its padding, or nil at end."
  (when (<= (+ (point) 4) (point-max))
    (let ((s (buffer-substring-no-properties (point) (+ (point) 3))))
      (forward-char 4)
      s)))

(defun evaporview-ghw--skip (n)
  "Move past N bytes."
  (if (<= (+ (point) n) (point-max))
      (forward-char n)
    (signal 'evaporview-ghw-eof nil)))

(defun evaporview-ghw--zeros ()
  "Consume the four zero bytes that open a section body."
  (dotimes (_ 4)
    (unless (eq (evaporview-ghw--byte) 0)
      (error "GHW: malformed section header at byte %d" (1- (point))))))

;;;; Strings

(defun evaporview-ghw--decode (raw)
  "Return RAW, a unibyte byte string, as text."
  (if (string-match-p "[^\0-\177]" raw)
      (decode-coding-string raw 'iso-8859-1)
    raw))

(defun evaporview-ghw--read-strings ()
  (evaporview-ghw--zeros)
  (let* ((n (evaporview-ghw--i32))
         (tab (make-vector (1+ n) "<anon>"))
         (prev "")
         (plen 0))
    (evaporview-ghw--i32)
    (dotimes (i n)
      (let ((start (point)) c)
        (while (let ((b (evaporview-ghw--byte)))
                 (setq c b)
                 (not (or (< b 32) (<= 128 b 159)))))
        (let ((raw (concat (substring prev 0 (min plen (length prev)))
                           (buffer-substring-no-properties start (1- (point)))))
              (sh 5))
          (aset tab (1+ i) (evaporview-ghw--decode raw))
          (setq prev raw plen (logand c 31))
          (while (>= c 128)
            (setq c (evaporview-ghw--byte)
                  plen (logior plen (ash (logand c 31) sh))
                  sh (+ sh 5))))))
    (unless (equal (evaporview-ghw--tag) "EOS")
      (error "GHW: malformed string table"))
    (setq evaporview-ghw--strings tab)))

(defun evaporview-ghw--strid ()
  (let ((id (evaporview-ghw--uleb)))
    (if (< id (length evaporview-ghw--strings))
        (aref evaporview-ghw--strings id)
      (error "GHW: invalid string reference %d" id))))

;;;; Types

(defun evaporview-ghw--typeid ()
  (let ((id (evaporview-ghw--uleb)))
    (or (and (>= id 1) (<= id (length evaporview-ghw--types))
             (aref evaporview-ghw--types (1- id)))
        (error "GHW: invalid type reference %d" id))))

(defun evaporview-ghw--read-range ()
  "Read a range; return (DIR LEFT RIGHT KIND), DIR 1 for downto."
  (let* ((b (evaporview-ghw--byte))
         (kind (logand b 127))
         (dir (if (>= b 128) 1 0)))
    (cond
     ((memq kind (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
      (let* ((l (evaporview-ghw--byte)) (r (evaporview-ghw--byte)))
        (list dir l r kind)))
     ((memq kind (list evaporview-ghw--k-i32 evaporview-ghw--k-p32
                       evaporview-ghw--k-i64 evaporview-ghw--k-p64))
      (let* ((l (evaporview-ghw--sleb)) (r (evaporview-ghw--sleb)))
        (list dir l r kind)))
     ((= kind evaporview-ghw--k-f64)
      (let* ((l (evaporview-ghw--f64)) (r (evaporview-ghw--f64)))
        (list dir l r kind)))
     (t (error "GHW: unsupported range kind %d" kind)))))

(defun evaporview-ghw--range-length (rng)
  (let ((n (if (= (nth 0 rng) 1)
               (1+ (- (nth 1 rng) (nth 2 rng)))
             (1+ (- (nth 2 rng) (nth 1 rng))))))
    (max 0 n)))

(defun evaporview-ghw--base (ty)
  "Return the type TY is a subtype of, or TY itself when it is a base type."
  (while (memq (evaporview-ghw--ty-kind ty)
               (list evaporview-ghw--k-sscalar evaporview-ghw--k-sarray
                     evaporview-ghw--k-uarray))
    (setq ty (evaporview-ghw--ty-base ty)))
  ty)

(defun evaporview-ghw--nscalars (ty)
  "Number of scalar elements of TY, or -1 when unbounded."
  (let ((k (evaporview-ghw--ty-kind ty)))
    (cond ((memq k (list evaporview-ghw--k-b2 evaporview-ghw--k-e8 24
                         evaporview-ghw--k-i32 evaporview-ghw--k-i64
                         evaporview-ghw--k-f64 evaporview-ghw--k-p32
                         evaporview-ghw--k-p64 evaporview-ghw--k-sscalar))
           1)
          ((memq k (list evaporview-ghw--k-sarray evaporview-ghw--k-record
                         evaporview-ghw--k-srecord))
           (evaporview-ghw--ty-nbr ty))
          (t -1))))

(defun evaporview-ghw--read-bounds (base)
  "Read the bounds that turn unbounded BASE into a subtype."
  (let ((k (evaporview-ghw--ty-kind base)))
    (cond ((memq k (list evaporview-ghw--k-array evaporview-ghw--k-uarray))
           (evaporview-ghw--read-array-subtype base))
          ((memq k (list evaporview-ghw--k-record evaporview-ghw--k-urecord))
           (evaporview-ghw--read-record-subtype base))
          (t (error "GHW: cannot bound type kind %d" k)))))

(defun evaporview-ghw--read-array-subtype (base)
  (let* ((arr (evaporview-ghw--base base))
         (n 1) rngs el nels)
    (unless (= (evaporview-ghw--ty-kind arr) evaporview-ghw--k-array)
      (error "GHW: array subtype of a non-array"))
    (setq nels (evaporview-ghw--nscalars (evaporview-ghw--ty-el arr)))
    (dotimes (_ (length (evaporview-ghw--ty-dims arr)))
      (let ((r (evaporview-ghw--read-range)))
        (push r rngs)
        (setq n (* n (evaporview-ghw--range-length r)))))
    (if (>= nels 0)
        (setq el (evaporview-ghw--ty-el arr))
      (setq el (evaporview-ghw--read-bounds (evaporview-ghw--ty-el arr))
            nels (evaporview-ghw--nscalars el)))
    (evaporview-ghw--ty-make :kind evaporview-ghw--k-sarray :base base
                             :rngs (nreverse rngs) :el el :nbr (* n nels))))

(defun evaporview-ghw--record-fields (rec)
  (evaporview-ghw--ty-fields
   (if (= (evaporview-ghw--ty-kind rec) evaporview-ghw--k-urecord)
       (evaporview-ghw--ty-base rec)
     rec)))

(defun evaporview-ghw--read-record-subtype (base)
  (let ((rec (if (= (evaporview-ghw--ty-kind base) evaporview-ghw--k-urecord)
                 (evaporview-ghw--ty-base base)
               base)))
    (if (>= (evaporview-ghw--ty-nbr rec) 0)
        (evaporview-ghw--ty-make :kind evaporview-ghw--k-srecord :base rec
                                 :fields (evaporview-ghw--ty-fields rec)
                                 :nbr (evaporview-ghw--ty-nbr rec))
      (let ((n 0) fields)
        (dolist (f (append (evaporview-ghw--ty-fields rec) nil))
          (let* ((ft (cdr f))
                 (cnt (evaporview-ghw--nscalars ft)))
            (when (< cnt 0)
              (setq ft (evaporview-ghw--read-bounds ft)
                    cnt (evaporview-ghw--nscalars ft)))
            (push (cons (car f) ft) fields)
            (setq n (+ n cnt))))
        (evaporview-ghw--ty-make :kind evaporview-ghw--k-srecord :base rec
                                 :fields (vconcat (nreverse fields))
                                 :nbr n)))))

(defun evaporview-ghw--read-type (id kind)
  (let ((ty
         (cond
          ((memq kind (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
           (let* ((name (evaporview-ghw--strid))
                  (n (evaporview-ghw--uleb))
                  (lits (make-vector n nil)))
             (dotimes (i n) (aset lits i (evaporview-ghw--strid)))
             (evaporview-ghw--ty-make :kind kind :name name :lits lits :wkt 0)))
          ((memq kind (list evaporview-ghw--k-i32 evaporview-ghw--k-i64
                            evaporview-ghw--k-f64))
           (evaporview-ghw--ty-make :kind kind :name (evaporview-ghw--strid)))
          ((memq kind (list evaporview-ghw--k-p32 evaporview-ghw--k-p64))
           (let ((name (evaporview-ghw--strid)))
             (when (>= evaporview-ghw--version 1)
               (dotimes (_ (evaporview-ghw--uleb))
                 (evaporview-ghw--strid)
                 (evaporview-ghw--sleb)))
             (evaporview-ghw--ty-make :kind kind :name name)))
          ((= kind evaporview-ghw--k-sscalar)
           (let* ((name (evaporview-ghw--strid))
                  (base (evaporview-ghw--typeid))
                  (rng (evaporview-ghw--read-range)))
             (evaporview-ghw--ty-make :kind kind :name name :base base
                                      :rngs (list rng))))
          ((= kind evaporview-ghw--k-array)
           (let* ((name (evaporview-ghw--strid))
                  (el (evaporview-ghw--typeid))
                  (nd (evaporview-ghw--uleb))
                  dims)
             (dotimes (_ nd) (push (evaporview-ghw--typeid) dims))
             (evaporview-ghw--ty-make :kind kind :name name :el el
                                      :dims (nreverse dims))))
          ((= kind evaporview-ghw--k-sarray)
           (let* ((name (evaporview-ghw--strid))
                  (base (evaporview-ghw--typeid))
                  (sa (evaporview-ghw--read-array-subtype base)))
             (setf (evaporview-ghw--ty-name sa) name)
             sa))
          ((= kind evaporview-ghw--k-uarray)
           (let* ((name (evaporview-ghw--strid))
                  (base (evaporview-ghw--typeid)))
             (evaporview-ghw--ty-make :kind kind :name name :base base)))
          ((= kind evaporview-ghw--k-record)
           (let* ((name (evaporview-ghw--strid))
                  (nf (evaporview-ghw--uleb))
                  (fields (make-vector nf nil))
                  (n 0))
             (dotimes (i nf)
               (let* ((fname (evaporview-ghw--strid))
                      (ft (evaporview-ghw--typeid))
                      (cnt (evaporview-ghw--nscalars ft)))
                 (aset fields i (cons fname ft))
                 (setq n (if (or (< n 0) (< cnt 0)) -1 (+ n cnt)))))
             (evaporview-ghw--ty-make :kind kind :name name :fields fields
                                      :nbr n)))
          ((= kind evaporview-ghw--k-srecord)
           (let* ((name (evaporview-ghw--strid))
                  (base (evaporview-ghw--typeid))
                  (sr (evaporview-ghw--read-record-subtype base)))
             (setf (evaporview-ghw--ty-name sr) name)
             sr))
          ((= kind evaporview-ghw--k-urecord)
           (let* ((name (evaporview-ghw--strid))
                  (base (evaporview-ghw--typeid)))
             (evaporview-ghw--ty-make :kind kind :name name :base base)))
          (t (error "GHW: unsupported type kind %d" kind)))))
    (setf (evaporview-ghw--ty-id ty) id)
    ty))

(defun evaporview-ghw--read-types ()
  (evaporview-ghw--zeros)
  (let ((n (evaporview-ghw--i32)))
    (setq evaporview-ghw--types (make-vector n nil))
    (dotimes (i n)
      (aset evaporview-ghw--types i
            (evaporview-ghw--read-type i (evaporview-ghw--byte))))
    (unless (eq (evaporview-ghw--byte) 0)
      (error "GHW: malformed type section"))))

(defun evaporview-ghw--read-wkt ()
  (evaporview-ghw--zeros)
  (let (code)
    (while (/= 0 (setq code (evaporview-ghw--byte)))
      (let ((ty (evaporview-ghw--typeid)))
        (when (memq (evaporview-ghw--ty-kind ty)
                    (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
          (setf (evaporview-ghw--ty-wkt ty) code))))))

;;;; Value encoding

(defun evaporview-ghw--bit-like-p (ty)
  (and (memq (evaporview-ghw--ty-kind ty)
             (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
       (memq (evaporview-ghw--ty-wkt ty) '(1 2 3))))

(defun evaporview-ghw--prepare-tab (ty)
  "Fill the value-to-string tables of base enumeration TY."
  (unless (evaporview-ghw--ty-tab ty)
    (let* ((lits (evaporview-ghw--ty-lits ty))
           (tab (if (evaporview-ghw--bit-like-p ty)
                    (if (= (evaporview-ghw--ty-wkt ty) 3)
                        (vconcat
                         (mapcar (lambda (l)
                                   (if (>= (length l) 3)
                                       (downcase (substring l 1 2))
                                     (downcase l)))
                                 lits))
                      (vector "0" "1"))
                  lits)))
      (setf (evaporview-ghw--ty-tab ty) tab
            (evaporview-ghw--ty-ctab ty)
            (vconcat (mapcar (lambda (s) (if (> (length s) 0) (aref s 0) ?x))
                             tab)))))
  ty)

(defun evaporview-ghw--bits (v width)
  "Return V as a two's complement bit string of WIDTH bits."
  (let ((s (make-string width ?0)))
    (dotimes (k width)
      (when (= 1 (logand (ash v (- k)) 1)) (aset s (- width 1 k) ?1)))
    s))

(defun evaporview-ghw--trk-value (trk)
  "Return the current value of TRK as the string the wave model stores."
  (let ((raw (evaporview-ghw--trk-cur trk))
        (enc (evaporview-ghw--trk-enc trk)))
    (cond
     ((eq enc :vec) (copy-sequence (evaporview-ghw--trk-buf trk)))
     ((eq enc :bit)
      (let ((tab (evaporview-ghw--trk-tab trk)))
        (if (< raw (length tab)) (aref tab raw) "x")))
     ((eq enc :enum)
      (let ((tab (evaporview-ghw--trk-tab trk)))
        (if (< raw (length tab)) (aref tab raw) (format "?%d" raw))))
     ((eq enc :real) (number-to-string raw))
     (t (let ((w (evaporview-ghw--trk-width trk)))
          (evaporview-ghw--bits (logand raw (1- (ash 1 w))) w))))))

(defun evaporview-ghw--change (sig time value)
  "Record VALUE for SIG at TIME, keeping only real changes."
  (let ((times (evaporview-sig-times sig))
        (values (evaporview-sig-values sig)))
    (cond
     ((null times)
      (setf (evaporview-sig-times sig) (list time)
            (evaporview-sig-values sig) (list value)))
     ((eql (car times) time)
      (if (and (cdr values) (equal (cadr values) value))
          (setf (evaporview-sig-times sig) (cdr times)
                (evaporview-sig-values sig) (cdr values))
        (setcar values value)))
     ((equal (car values) value))
     (t (setf (evaporview-sig-times sig) (cons time times)
              (evaporview-sig-values sig) (cons value values))))))

;;;; Hierarchy

(defun evaporview-ghw--tname (ty)
  "Return the VHDL type name of TY, falling back to its base type's."
  (let (name)
    (while (and ty (or (null (setq name (evaporview-ghw--ty-name ty)))
                       (equal name "<anon>")))
      (setq ty (evaporview-ghw--ty-base ty)))
    (if ty (evaporview-ghw--ty-name ty) "unknown")))

(defun evaporview-ghw--scalar-enc (base)
  (let ((k (evaporview-ghw--ty-kind base)))
    (cond ((evaporview-ghw--bit-like-p base) :bit)
          ((memq k (list evaporview-ghw--k-b2 evaporview-ghw--k-e8)) :enum)
          ((memq k (list evaporview-ghw--k-i32 evaporview-ghw--k-p32)) :i32)
          ((memq k (list evaporview-ghw--k-i64 evaporview-ghw--k-p64)) :i64)
          ((= k evaporview-ghw--k-f64) :real)
          (t nil))))

(defun evaporview-ghw--sub (cell name type)
  "Add a scope NAME of TYPE under CELL = (SCOPE . PATH); return its cell."
  (let ((scope (evaporview-scope--make :name name :type type)))
    (push scope (evaporview-scope-children (car cell)))
    (cons scope (if (equal (cdr cell) "") name (concat (cdr cell) "." name)))))

(defun evaporview-ghw--enum-width (count)
  "Number of bits needed to encode COUNT enumeration literals."
  (let ((w 1))
    (while (< (ash 1 w) count) (setq w (1+ w)))
    w))

(defun evaporview-ghw--add-var (cell name tyname enc etype elems off n range)
  "Declare a variable NAME of N elements of ELEMS from OFF.
ENC is the value encoding and ETYPE the base type of each element."
  (let* ((key (cons enc (cons (evaporview-ghw--ty-id etype)
                              (append (cl-subseq elems off (+ off n)) nil))))
         (trk (gethash key evaporview-ghw--tracks)))
    (unless trk
      (let* ((width (cond ((eq enc :vec) n)
                          ((eq enc :bit) 1)
                          ((eq enc :i32) 32)
                          ((memq enc '(:i64 :real)) 64)
                          (t (evaporview-ghw--enum-width
                              (length (evaporview-ghw--ty-lits etype))))))
             (id (number-to-string (hash-table-count evaporview-ghw--tracks)))
             (sig (evaporview-sig--make
                   :id id :width width :real-p (eq enc :real)
                   :enum (and (eq enc :enum) (evaporview-ghw--ty-lits etype)))))
        (when (memq enc '(:bit :enum :vec)) (evaporview-ghw--prepare-tab etype))
        (setq trk (evaporview-ghw--trk-make
                   :enc enc :sig sig :width width
                   :tab (evaporview-ghw--ty-tab etype)
                   :ctab (evaporview-ghw--ty-ctab etype)
                   :buf (and (eq enc :vec) (make-string n ?u))))
        (puthash key trk evaporview-ghw--tracks)
        (puthash id sig evaporview-ghw--sigs)
        (push trk evaporview-ghw--track-list)
        (dotimes (i n)
          (let ((e (aref elems (+ off i))))
            (unless (aref evaporview-ghw--sigtype e)
              (aset evaporview-ghw--sigtype e etype))
            (push (cons trk i) (aref evaporview-ghw--emap e))))))
    (let* ((sig (evaporview-ghw--trk-sig trk))
           (path (if (equal (cdr cell) "") name (concat (cdr cell) "." name)))
           (var (evaporview-var--make :name name :path path :type tyname
                                      :width (evaporview-sig-width sig)
                                      :range range
                                      :id (evaporview-sig-id sig))))
      (push var (evaporview-scope-vars (car cell)))
      (push var evaporview-ghw--vars)
      (unless (gethash path evaporview-ghw--paths)
        (puthash path var evaporview-ghw--paths)))))

(defun evaporview-ghw--index-name (idxty rng idx)
  (if (memq (nth 3 rng) (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
      (let ((lits (evaporview-ghw--ty-lits (evaporview-ghw--base idxty))))
        (if (< idx (length lits)) (aref lits idx) (number-to-string idx)))
    (number-to-string idx)))

(defun evaporview-ghw--emit-array (cell name ty elems off)
  "Declare array signal NAME as an array scope holding its elements."
  (let* ((arr (evaporview-ghw--base ty))
         (rngs (evaporview-ghw--ty-rngs ty))
         (lens (mapcar #'evaporview-ghw--range-length rngs))
         (el (evaporview-ghw--ty-el ty))
         (stride (evaporview-ghw--nscalars el))
         (count (apply #'* lens))
         (scope (and (> count 0) (evaporview-ghw--sub cell name "array"))))
    (dotimes (k count)
      (let ((rest k) names)
        (cl-loop for rng in (reverse rngs)
                 for len in (reverse lens)
                 for idxty in (reverse (evaporview-ghw--ty-dims arr))
                 do (let* ((j (% rest len))
                           (idx (if (= (nth 0 rng) 1)
                                    (- (nth 1 rng) j)
                                  (+ (nth 1 rng) j))))
                      (push (evaporview-ghw--index-name idxty rng idx) names)
                      (setq rest (/ rest len))))
        (evaporview-ghw--emit scope
                              (concat name "(" (mapconcat #'identity names ",")
                                      ")")
                              el elems (+ off (* k stride)))))))

(defun evaporview-ghw--emit (cell name ty elems off)
  "Declare signal NAME of type TY, whose elements start at OFF in ELEMS."
  (let* ((base (evaporview-ghw--base ty))
         (k (evaporview-ghw--ty-kind base)))
    (cond
     ((evaporview-ghw--scalar-enc base)
      (evaporview-ghw--add-var cell name (evaporview-ghw--tname ty)
                               (evaporview-ghw--scalar-enc base)
                               base elems off 1 nil))
     ((= k evaporview-ghw--k-array)
      (let* ((n (evaporview-ghw--ty-nbr ty))
             (el (evaporview-ghw--ty-el ty))
             (ebase (evaporview-ghw--base el)))
        (cond
         ((and (= (length (evaporview-ghw--ty-rngs ty)) 1)
               (evaporview-ghw--bit-like-p ebase))
          (when (> n 0)
            (let ((rng (car (evaporview-ghw--ty-rngs ty))))
              (evaporview-ghw--add-var
               cell name (evaporview-ghw--tname ty) :vec ebase elems off n
               (format "[%d:%d]" (nth 1 rng) (nth 2 rng))))))
         (t (evaporview-ghw--emit-array cell name ty elems off)))))
     ((memq k (list evaporview-ghw--k-record evaporview-ghw--k-srecord))
      (let ((sub (evaporview-ghw--sub cell name "record"))
            (o off))
        (cl-loop for f across (evaporview-ghw--ty-fields base)
                 do (evaporview-ghw--emit sub (car f) (cdr f) elems o)
                    (setq o (+ o (evaporview-ghw--nscalars (cdr f)))))))
     (t (error "GHW: cannot display signal %s of type kind %d" name k)))))

(defun evaporview-ghw--read-iter (ty)
  "Read a for-generate iteration value of type TY as display text."
  (let* ((base (evaporview-ghw--base ty))
         (k (evaporview-ghw--ty-kind base)))
    (cond ((memq k (list evaporview-ghw--k-b2 evaporview-ghw--k-e8))
           (let ((v (evaporview-ghw--byte))
                 (lits (evaporview-ghw--ty-lits base)))
             (if (< v (length lits)) (aref lits v) (number-to-string v))))
          ((memq k (list evaporview-ghw--k-i32 evaporview-ghw--k-p32
                         evaporview-ghw--k-i64 evaporview-ghw--k-p64))
           (number-to-string (evaporview-ghw--sleb)))
          ((= k evaporview-ghw--k-f64)
           (number-to-string (evaporview-ghw--f64)))
          (t (error "GHW: unsupported generate iterator type")))))

(defun evaporview-ghw--prune (scope)
  "Reverse SCOPE's lists into declaration order; drop scopes without signals.
Return non-nil when SCOPE or anything below it holds a variable."
  (setf (evaporview-scope-vars scope) (nreverse (evaporview-scope-vars scope)))
  (let (kept)
    (dolist (c (nreverse (evaporview-scope-children scope)))
      (when (evaporview-ghw--prune c) (push c kept)))
    (setf (evaporview-scope-children scope) (nreverse kept)))
  (or (evaporview-scope-vars scope) (evaporview-scope-children scope)))

(defconst evaporview-ghw--scope-kinds
  '((3 . "block") (4 . "generate") (5 . "generate") (6 . "instance")
    (7 . "package") (14 . "generic")))

(defun evaporview-ghw--read-hie (root)
  "Read the hierarchy section, building scopes and variables under ROOT."
  (evaporview-ghw--zeros)
  (evaporview-ghw--i32)
  (evaporview-ghw--i32)
  (let* ((nel (evaporview-ghw--i32))
         (stack (list (cons root "")))
         code)
    (when (< nel 0) (error "GHW: malformed hierarchy section"))
    (setq evaporview-ghw--sigtype (make-vector (1+ nel) nil)
          evaporview-ghw--emap (make-vector (1+ nel) nil))
    (while (/= 0 (setq code (evaporview-ghw--byte)))
      (cond
       ((= code 15) (when (cdr stack) (pop stack)))
       (t
        (let ((name (evaporview-ghw--strid)))
          (cond
           ((= code 13))
           ((assq code evaporview-ghw--scope-kinds)
            (when (= code 5)
              (let ((ty (evaporview-ghw--typeid)))
                (setq name (concat name "(" (evaporview-ghw--read-iter ty)
                                   ")"))))
            (push (evaporview-ghw--sub (car stack) name
                                       (cdr (assq code
                                                  evaporview-ghw--scope-kinds)))
                  stack))
           ((and (>= code 16) (<= code 21))
            (let* ((ty (evaporview-ghw--typeid))
                   (n (evaporview-ghw--nscalars ty))
                   (elems (make-vector (max n 0) 0)))
              (when (< n 0) (error "GHW: signal %s has an unbounded type" name))
              (dotimes (i n)
                (let ((e (evaporview-ghw--uleb)))
                  (when (or (= e 0) (> e nel))
                    (error "GHW: invalid signal number %d" e))
                  (aset elems i e)))
              (evaporview-ghw--emit (car stack) name ty elems 0)))
           (t (error "GHW: unsupported hierarchy entry %d" code)))))))))

;;;; Values

(defun evaporview-ghw--read-value (ty)
  "Read one scalar value of base type TY."
  (let ((k (evaporview-ghw--ty-kind ty)))
    (cond ((or (= k evaporview-ghw--k-b2) (= k evaporview-ghw--k-e8))
           (evaporview-ghw--byte))
          ((= k evaporview-ghw--k-f64) (evaporview-ghw--f64))
          (t (evaporview-ghw--sleb)))))

(defun evaporview-ghw--set (i raw)
  "Store RAW as the value of signal element I."
  (dolist (e (aref evaporview-ghw--emap i))
    (let ((trk (car e)))
      (if (eq (evaporview-ghw--trk-enc trk) :vec)
          (let ((ct (evaporview-ghw--trk-ctab trk)))
            (aset (evaporview-ghw--trk-buf trk) (cdr e)
                  (if (< raw (length ct)) (aref ct raw) ?x)))
        (setf (evaporview-ghw--trk-cur trk) raw))
      (unless (evaporview-ghw--trk-dirty trk)
        (setf (evaporview-ghw--trk-dirty trk) t)
        (push trk evaporview-ghw--dirty)))))

(defun evaporview-ghw--flush (time)
  "Record the new value of every track touched since the last flush at TIME."
  (dolist (trk evaporview-ghw--dirty)
    (setf (evaporview-ghw--trk-dirty trk) nil)
    (evaporview-ghw--change (evaporview-ghw--trk-sig trk) time
                            (evaporview-ghw--trk-value trk)))
  (setq evaporview-ghw--dirty nil))

(defun evaporview-ghw--read-snapshot ()
  "Read a snapshot; return its time."
  (evaporview-ghw--zeros)
  (let ((time (evaporview-ghw--i64))
        (types evaporview-ghw--sigtype))
    (dotimes (i (length types))
      (let ((ty (aref types i)))
        (when ty (evaporview-ghw--set i (evaporview-ghw--read-value ty)))))
    (unless (equal (evaporview-ghw--tag) "ESN")
      (error "GHW: malformed snapshot"))
    (dolist (trk evaporview-ghw--track-list)
      (unless (evaporview-ghw--trk-dirty trk)
        (setf (evaporview-ghw--trk-dirty trk) t)
        (push trk evaporview-ghw--dirty)))
    (evaporview-ghw--flush time)
    time))

(defun evaporview-ghw--typed-index ()
  "Return a vector mapping the Nth typed signal element to its number."
  (let ((idx (list 0)))
    (dotimes (i (length evaporview-ghw--sigtype))
      (when (and (> i 0) (aref evaporview-ghw--sigtype i)) (push i idx)))
    (vconcat (nreverse idx))))

(defun evaporview-ghw--read-changes (time dense typed)
  "Read one group of value changes and record them at TIME.
DENSE is non-nil when every signal number has a type; otherwise TYPED maps
the running count to a signal number."
  (let ((types evaporview-ghw--sigtype)
        (i 0) (p 0) d)
    (while (/= 0 (setq d (evaporview-ghw--uleb)))
      (if dense
          (setq i (+ i d))
        (setq p (+ p d)
              i (if (< p (length typed)) (aref typed p) 0)))
      (let ((ty (and (> i 0) (< i (length types)) (aref types i))))
        (unless ty (error "GHW: change for unknown signal %d" i))
        (evaporview-ghw--set i (evaporview-ghw--read-value ty))))
    (evaporview-ghw--flush time)))

(defun evaporview-ghw--read-dump (wave)
  "Read snapshots and cycles up to the end of the data into WAVE."
  (let* ((n (length evaporview-ghw--sigtype))
         (dense (cl-loop for i from 1 below n
                         always (aref evaporview-ghw--sigtype i)))
         (typed (and (not dense) (evaporview-ghw--typed-index)))
         (start nil) (end nil) tag)
    (condition-case nil
        (while (setq tag (evaporview-ghw--tag))
          (cond
           ((equal tag "SNP")
            (setq end (evaporview-ghw--read-snapshot))
            (unless start (setq start end)))
           ((equal tag "CYC")
            (let ((time (evaporview-ghw--i64)) (more t))
              (unless start (setq start time))
              (while more
                (evaporview-ghw--read-changes time dense typed)
                (setq end time)
                (let ((d (evaporview-ghw--sleb)))
                  (if (= d -1)
                      (setq more nil)
                    (setq time (+ time d)))))
              (unless (equal (evaporview-ghw--tag) "ECY")
                (error "GHW: malformed cycle"))))
           ((equal tag "DIR")
            (evaporview-ghw--skip 4)
            (evaporview-ghw--skip (* 8 (evaporview-ghw--i32)))
            (evaporview-ghw--tag))
           ((equal tag "TAI") (evaporview-ghw--skip 8))
           (t (error "GHW: unknown section %s" tag))))
      (evaporview-ghw-eof
       (setq evaporview-ghw--dirty nil)))
    (setf (evaporview-wave-start wave) (or start 0)
          (evaporview-wave-end wave) (or end 0))))

;;;; Entry points

(defun evaporview-ghw--read-header ()
  (unless (and (<= (+ (point) 16) (point-max))
               (equal (buffer-substring-no-properties (point) (+ (point) 9))
                      "GHDLwave\n"))
    (user-error "Not a GHW file"))
  (forward-char 9)
  (unless (and (eq (evaporview-ghw--byte) 16) (eq (evaporview-ghw--byte) 0))
    (user-error "Unsupported GHW format version"))
  (setq evaporview-ghw--version (evaporview-ghw--byte))
  (when (> evaporview-ghw--version 1)
    (user-error "Unsupported GHW version %d" evaporview-ghw--version))
  (let ((order (evaporview-ghw--byte)))
    (unless (memq order '(1 2)) (user-error "Unsupported GHW byte order"))
    (setq evaporview-ghw--be (= order 2)))
  (forward-char 3))

(defun evaporview-ghw-parse-buffer (&optional file)
  "Parse the uncompressed GHW data in the current unibyte buffer.
FILE is recorded as the origin of the wave."
  (save-excursion
    (goto-char (point-min))
    (let* ((root (evaporview-scope--make :name "" :type "root"))
           (evaporview-ghw--strings nil)
           (evaporview-ghw--types nil)
           (evaporview-ghw--sigtype nil)
           (evaporview-ghw--emap nil)
           (evaporview-ghw--tracks (make-hash-table :test 'equal))
           (evaporview-ghw--track-list nil)
           (evaporview-ghw--dirty nil)
           (evaporview-ghw--vars nil)
           (evaporview-ghw--paths (make-hash-table :test 'equal))
           (evaporview-ghw--sigs (make-hash-table :test 'equal))
           (wave (evaporview-wave--make :file file :timescale '(1 . "fs")))
           tag)
      (evaporview-ghw--read-header)
      (condition-case nil
          (while (not (equal (setq tag (evaporview-ghw--tag)) "EOH"))
            (cond ((equal tag "STR") (evaporview-ghw--read-strings))
                  ((equal tag "TYP") (evaporview-ghw--read-types))
                  ((equal tag "WKT") (evaporview-ghw--read-wkt))
                  ((equal tag "HIE") (evaporview-ghw--read-hie root))
                  (t (error "GHW: unknown section %s" tag))))
        (evaporview-ghw-eof (error "GHW: file ends inside its header")))
      (unless evaporview-ghw--sigtype (error "GHW: no hierarchy section"))
      (evaporview-ghw--read-dump wave)
      (evaporview-ghw--prune root)
      (maphash (lambda (_id sig)
                 (setf (evaporview-sig-times sig)
                       (vconcat (nreverse (evaporview-sig-times sig)))
                       (evaporview-sig-values sig)
                       (vconcat (nreverse (evaporview-sig-values sig)))))
               evaporview-ghw--sigs)
      (setf (evaporview-wave-root wave) root
            (evaporview-wave-vars wave) (vconcat (nreverse evaporview-ghw--vars))
            (evaporview-wave-sigs wave) evaporview-ghw--sigs
            (evaporview-wave-paths wave) evaporview-ghw--paths)
      wave)))

(defun evaporview-ghw-parse-file (file)
  "Parse the GHW file FILE.  The file is only read."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (cond
     ((looking-at "\x1f\x8b")
      (unless (and (fboundp 'zlib-available-p) (zlib-available-p)
                   (zlib-decompress-region (point-min) (point-max)))
        (user-error "%s is gzip-compressed and this Emacs cannot decompress it; write the waveform uncompressed (GHDL --wave=file.ghw)"
                    (file-name-nondirectory file))))
     ((looking-at "BZh")
      (user-error "%s is bzip2-compressed; write the waveform uncompressed (GHDL --wave=file.ghw)"
                  (file-name-nondirectory file))))
    (evaporview-ghw-parse-buffer (expand-file-name file))))

(provide 'evaporview-ghw)

;;; evaporview-ghw.el ends here

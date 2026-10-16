;;; evaporview-vcd.el --- VCD reader and signal model for evaporview  -*- lexical-binding: t; -*-

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

;; Reads IEEE 1364 Value Change Dump files into a scope tree, a list of
;; variables and one change list per identifier code.  Values are kept as
;; lowercase bit strings, MSB first, at the declared width; real variables
;; keep the decimal string from the dump.
;;
;; A change list holds only real changes: a value equal to the previous one
;; is dropped, and several changes at one timestamp keep the last.

;;; Code:

(require 'cl-lib)

(cl-defstruct (evaporview-wave (:constructor evaporview-wave--make))
  file timescale start end root vars sigs paths)

(cl-defstruct (evaporview-scope (:constructor evaporview-scope--make))
  name type children vars)

(cl-defstruct (evaporview-var (:constructor evaporview-var--make))
  name path type width range id)

(cl-defstruct (evaporview-sig (:constructor evaporview-sig--make))
  id width real-p times values enum)

(defun evaporview-var-display-name (var)
  "Return VAR's name followed by its declared range."
  (concat (evaporview-var-name var) (or (evaporview-var-range var) "")))

(defun evaporview-wave-find-var (wave path)
  "Return the variable of WAVE whose dotted PATH (without range) is PATH."
  (gethash path (evaporview-wave-paths wave)))

(defun evaporview-wave-var-sig (wave var)
  "Return the change list WAVE holds for VAR."
  (gethash (evaporview-var-id var) (evaporview-wave-sigs wave)))

;;;; Reader

(defconst evaporview-vcd--scalars
  (let ((v (make-vector 128 nil)))
    (dotimes (i 128) (aset v i (string (downcase i))))
    v)
  "One shared string per scalar value character.")

(defsubst evaporview-vcd--token ()
  "Return the next whitespace-delimited token, or nil at end of buffer."
  (skip-chars-forward " \t\n\r\f")
  (unless (eobp)
    (let ((start (point)))
      (skip-chars-forward "^ \t\n\r\f")
      (buffer-substring-no-properties start (point)))))

(defun evaporview-vcd--tokens-to-end ()
  "Return the tokens up to the next $end, consuming it."
  (let (tok acc)
    (while (and (setq tok (evaporview-vcd--token)) (not (equal tok "$end")))
      (push tok acc))
    (nreverse acc)))

(defun evaporview-vcd--timescale (tokens)
  (let ((s (apply #'concat tokens)))
    (if (string-match "\\`\\([0-9]+\\)\\([a-z]+\\)\\'" s)
        (cons (string-to-number (match-string 1 s)) (match-string 2 s))
      '(1 . "ns"))))

(defun evaporview-vcd--extend (value width)
  "Pad or trim bit string VALUE to WIDTH per IEEE 1364."
  (let ((len (length value)))
    (cond ((= len width) value)
          ((> len width) (substring value (- len width)))
          (t (let ((c (aref value 0)))
               (concat (make-string (- width len) (if (memq c '(?0 ?1)) ?0 c))
                       value))))))

(defun evaporview-vcd--parse-header (wave)
  "Read declarations up to $enddefinitions into WAVE.
Return a hash table from identifier code to signal."
  (let* ((root (evaporview-scope--make :name "" :type "root"))
         (stack (list root))
         (sigs (make-hash-table :test 'equal))
         (paths (make-hash-table :test 'equal))
         (vars nil)
         tok)
    (while (and (setq tok (evaporview-vcd--token))
                (not (equal tok "$enddefinitions")))
      (cond
       ((equal tok "$timescale")
        (setf (evaporview-wave-timescale wave)
              (evaporview-vcd--timescale (evaporview-vcd--tokens-to-end))))
       ((equal tok "$scope")
        (let* ((args (evaporview-vcd--tokens-to-end))
               (scope (evaporview-scope--make :type (car args)
                                              :name (or (cadr args) ""))))
          (push scope (evaporview-scope-children (car stack)))
          (push scope stack)))
       ((equal tok "$upscope")
        (evaporview-vcd--tokens-to-end)
        (when (cdr stack) (pop stack)))
       ((equal tok "$var")
        (let* ((args (evaporview-vcd--tokens-to-end))
               (type (nth 0 args))
               (width (string-to-number (nth 1 args)))
               (id (nth 2 args))
               (ref (nth 3 args))
               (range (and (nthcdr 4 args) (apply #'concat (nthcdr 4 args)))))
          (when (and (null range)
                     (string-match "\\`\\(.+?\\)\\(\\[[-0-9:]+\\]\\)\\'" ref))
            (setq range (match-string 2 ref)
                  ref (match-string 1 ref)))
          (let* ((path (mapconcat #'identity
                                  (nreverse
                                   (cons ref (mapcar #'evaporview-scope-name
                                                     (butlast stack))))
                                  "."))
                 (var (evaporview-var--make :name ref :path path :type type
                                            :width width :range range :id id)))
            (push var (evaporview-scope-vars (car stack)))
            (push var vars)
            (unless (gethash path paths) (puthash path var paths))
            (unless (gethash id sigs)
              (puthash id (evaporview-sig--make
                           :id id :width width
                           :real-p (member type '("real" "realtime"))
                           :times nil :values nil)
                       sigs)))))
       ((string-prefix-p "$" tok) (evaporview-vcd--tokens-to-end))))
    (evaporview-vcd--tokens-to-end)
    (evaporview-vcd--finish-scopes root)
    (setf (evaporview-wave-root wave) root
          (evaporview-wave-vars wave) (vconcat (nreverse vars))
          (evaporview-wave-paths wave) paths
          (evaporview-wave-sigs wave) sigs)
    sigs))

(defun evaporview-vcd--finish-scopes (scope)
  (setf (evaporview-scope-children scope)
        (nreverse (evaporview-scope-children scope))
        (evaporview-scope-vars scope)
        (nreverse (evaporview-scope-vars scope)))
  (mapc #'evaporview-vcd--finish-scopes (evaporview-scope-children scope)))

(defsubst evaporview-vcd--change (sig time value)
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

(defun evaporview-vcd--parse-body (wave sigs)
  (let ((time 0) (start nil) tok)
    (while (setq tok (evaporview-vcd--token))
      (let ((c (aref tok 0)))
        (cond
         ((eq c ?#)
          (setq time (string-to-number (substring tok 1)))
          (unless start (setq start time)))
         ((eq c ?$)
          (when (equal tok "$comment") (evaporview-vcd--tokens-to-end)))
         ((memq c '(?b ?B))
          (let ((sig (gethash (evaporview-vcd--token) sigs)))
            (when sig
              (evaporview-vcd--change
               sig time (evaporview-vcd--extend (downcase (substring tok 1))
                                                (evaporview-sig-width sig))))))
         ((memq c '(?r ?R))
          (let ((sig (gethash (evaporview-vcd--token) sigs)))
            (when sig (evaporview-vcd--change sig time (substring tok 1)))))
         (t
          (let ((sig (gethash (substring tok 1) sigs)))
            (when sig
              (evaporview-vcd--change
               sig time
               (if (= (evaporview-sig-width sig) 1)
                   (aref evaporview-vcd--scalars (if (< c 128) c ??))
                 (evaporview-vcd--extend (string (downcase c))
                                         (evaporview-sig-width sig))))))))))
    (maphash (lambda (_id sig)
               (setf (evaporview-sig-times sig)
                     (vconcat (nreverse (evaporview-sig-times sig)))
                     (evaporview-sig-values sig)
                     (vconcat (nreverse (evaporview-sig-values sig)))))
             sigs)
    (setf (evaporview-wave-start wave) (or start 0)
          (evaporview-wave-end wave) time)))

(defun evaporview-vcd--finish-sig (sig)
  "Turn SIG's accumulated change lists into vectors in time order."
  (setf (evaporview-sig-times sig) (vconcat (nreverse (evaporview-sig-times sig)))
        (evaporview-sig-values sig) (vconcat (nreverse (evaporview-sig-values sig))))
  sig)

(defun evaporview-vcd--bit-index (var)
  "The bit number of a one-bit VAR declared as NAME [N], else nil."
  (let ((r (evaporview-var-range var)))
    (and r (= (evaporview-var-width var) 1)
         (string-match "\\`\\[\\([0-9]+\\)\\]\\'" r)
         (string-to-number (match-string 1 r)))))

(defun evaporview-vcd--join-sigs (members id)
  "One signal whose value concatenates MEMBERS, first member leftmost."
  (let ((out (evaporview-sig--make :id id :width (length members)))
        (times (sort (delete-dups (cl-mapcan (lambda (s) (append (evaporview-sig-times s) nil))
                                             members))
                     #'<)))
    (dolist (tm times)
      (evaporview-vcd--change out tm (mapconcat (lambda (s) (or (evaporview-sig-value-at s tm) "x"))
                                                members "")))
    (evaporview-vcd--finish-sig out)))

(defun evaporview-vcd--merge-bits (wave)
  "Join vectors that a simulator wrote as one-bit variables into buses.
Questa, for one, declares a vector `cnt' as `cnt [3]' ... `cnt [0]'."
  (let ((sigs (evaporview-wave-sigs wave))
        (paths (evaporview-wave-paths wave))
        (fate (make-hash-table :test 'eq)))
    (cl-labels
        ((walk (scope)
           (let ((groups nil))
             (dolist (v (evaporview-scope-vars scope))
               (when (evaporview-vcd--bit-index v)
                 (let ((g (assoc (evaporview-var-name v) groups)))
                   (if g (setcdr g (append (cdr g) (list v)))
                     (push (list (evaporview-var-name v) v) groups)))))
             (dolist (g groups)
               (let ((members (cdr g)))
                 (when (cdr members)
                   (let* ((first (car members))
                          (id (concat "~" (evaporview-var-path first)))
                          (merged (evaporview-var--make
                                   :name (car g) :path (evaporview-var-path first)
                                   :type (evaporview-var-type first) :width (length members)
                                   :range (format "[%d:%d]"
                                                  (evaporview-vcd--bit-index first)
                                                  (evaporview-vcd--bit-index (car (last members))))
                                   :id id)))
                     (puthash id (evaporview-vcd--join-sigs
                                  (mapcar (lambda (v) (gethash (evaporview-var-id v) sigs)) members)
                                  id)
                              sigs)
                     (puthash (evaporview-var-path first) merged paths)
                     (puthash first merged fate)
                     (dolist (v (cdr members)) (puthash v 'drop fate))))))
             (setf (evaporview-scope-vars scope)
                   (evaporview-vcd--apply-fate (evaporview-scope-vars scope) fate))
             (mapc #'walk (evaporview-scope-children scope)))))
      (walk (evaporview-wave-root wave)))
    (when (> (hash-table-count fate) 0)
      (setf (evaporview-wave-vars wave)
            (vconcat (evaporview-vcd--apply-fate (append (evaporview-wave-vars wave) nil) fate))))))

(defun evaporview-vcd--apply-fate (vars fate)
  (delq nil (mapcar (lambda (v)
                      (let ((f (gethash v fate)))
                        (cond ((eq f 'drop) nil) (f f) (t v))))
                    vars)))

(defun evaporview-vcd-parse-buffer (&optional file)
  "Parse the VCD text in the current buffer; FILE is recorded as its origin."
  (save-excursion
    (goto-char (point-min))
    (let ((wave (evaporview-wave--make :file file :timescale '(1 . "ns"))))
      (evaporview-vcd--parse-body wave (evaporview-vcd--parse-header wave))
      (evaporview-vcd--merge-bits wave)
      wave)))

(defun evaporview-vcd-parse-string (string)
  "Parse VCD text STRING."
  (with-temp-buffer
    (insert string)
    (evaporview-vcd-parse-buffer)))

(defun evaporview-vcd-parse-file (file)
  "Parse the VCD file FILE.  The file is only read."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (evaporview-vcd-parse-buffer (expand-file-name file))))

;;;; Queries

(defun evaporview-sig--index (sig time)
  "Index of the last change of SIG at or before TIME, or -1."
  (let* ((times (evaporview-sig-times sig))
         (lo 0) (hi (1- (length times))) (ans -1))
    (while (<= lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (aref times mid) time)
            (setq ans mid lo (1+ mid))
          (setq hi (1- mid)))))
    ans))

(defun evaporview-sig--lower-bound (sig time)
  "Index of the first change of SIG at or after TIME."
  (let* ((times (evaporview-sig-times sig))
         (lo 0) (hi (length times)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (aref times mid) time) (setq lo (1+ mid)) (setq hi mid))))
    lo))

(defun evaporview-sig-value-at (sig time)
  "Value of SIG at TIME, or nil before its first change."
  (let ((i (evaporview-sig--index sig time)))
    (and (>= i 0) (aref (evaporview-sig-values sig) i))))

(defun evaporview-sig-next-change (sig time)
  "Time of the first change of SIG after TIME, or nil."
  (let ((i (1+ (evaporview-sig--index sig time))))
    (and (< i (length (evaporview-sig-times sig)))
         (aref (evaporview-sig-times sig) i))))

(defun evaporview-sig-prev-change (sig time)
  "Time of the last change of SIG before TIME, or nil.
The initial value is not a change."
  (let ((i (1- (evaporview-sig--lower-bound sig time))))
    (and (>= i 1) (aref (evaporview-sig-times sig) i))))

(defun evaporview-sig--edge-p (from to kind)
  (let ((hi '(?1 ?h)) (lo '(?0 ?l)))
    (if (eq kind 'rise)
        (and (memq (aref to 0) hi) (not (memq (aref from 0) hi)))
      (and (memq (aref to 0) lo) (not (memq (aref from 0) lo))))))

(defun evaporview-sig-next-edge (sig time kind)
  "Time of the first KIND edge (`rise' or `fall') of SIG after TIME, or nil."
  (let* ((times (evaporview-sig-times sig))
         (values (evaporview-sig-values sig))
         (i (max 1 (1+ (evaporview-sig--index sig time))))
         (n (length times)) found)
    (while (and (not found) (< i n))
      (when (evaporview-sig--edge-p (aref values (1- i)) (aref values i) kind)
        (setq found (aref times i)))
      (setq i (1+ i)))
    found))

(defun evaporview-sig-prev-edge (sig time kind)
  "Time of the last KIND edge (`rise' or `fall') of SIG before TIME, or nil."
  (let* ((times (evaporview-sig-times sig))
         (values (evaporview-sig-values sig))
         (i (1- (evaporview-sig--lower-bound sig time))) found)
    (while (and (not found) (>= i 1))
      (when (evaporview-sig--edge-p (aref values (1- i)) (aref values i) kind)
        (setq found (aref times i)))
      (setq i (1- i)))
    found))

(defun evaporview-sig-search (sig time direction pred)
  "Time of the next change of SIG whose value satisfies PRED.
DIRECTION is `forward' (after TIME) or `backward' (before TIME)."
  (let* ((times (evaporview-sig-times sig))
         (values (evaporview-sig-values sig))
         (n (length times)) found)
    (if (eq direction 'forward)
        (let ((i (1+ (evaporview-sig--index sig time))))
          (while (and (not found) (< i n))
            (when (funcall pred (aref values i)) (setq found (aref times i)))
            (setq i (1+ i))))
      (let ((i (1- (evaporview-sig--lower-bound sig time))))
        (while (and (not found) (>= i 0))
          (when (funcall pred (aref values i)) (setq found (aref times i)))
          (setq i (1- i)))))
    found))

(defun evaporview-sig-slice (sig msb lsb)
  "Return a new signal holding bits MSB..LSB of SIG (LSB is bit 0)."
  (let* ((w (evaporview-sig-width sig))
         (from (- w 1 msb)) (to (- w lsb))
         (times (evaporview-sig-times sig))
         (values (evaporview-sig-values sig))
         (out (evaporview-sig--make :id (format "%s[%d:%d]" (evaporview-sig-id sig)
                                                msb lsb)
                                    :width (1+ (- msb lsb)))))
    (dotimes (i (length times))
      (evaporview-vcd--change out (aref times i)
                              (substring (aref values i) from to)))
    (setf (evaporview-sig-times out)
          (vconcat (nreverse (evaporview-sig-times out)))
          (evaporview-sig-values out)
          (vconcat (nreverse (evaporview-sig-values out))))
    out))

(provide 'evaporview-vcd)
;;; evaporview-vcd.el ends here

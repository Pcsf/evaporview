;;; evaporview-ghw-test.el --- ERT suite for the GHW reader  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/evaporview-ghw-test.el -f ert-run-tests-batch-and-exit
;;
;; test/data/rec.ghw and test/data/ghw_tb.ghw are GHDL 6.0 output of
;; test/data/rec_tb.vhd and test/data/ghw_tb.vhd; ghw_tb.ghw.gz is the same
;; waveform gzip-compressed.  The change lists below were derived from
;; "ghwdump -s" of those files, independently of the reader.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'evaporview-ghw)
(require 'evaporview-format)

(defconst evaporview-ghw-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun evaporview-ghw-test--file (name)
  (expand-file-name (concat "data/" name) evaporview-ghw-test--dir))

(defun evaporview-ghw-test--wave (name)
  (evaporview-ghw-parse-file (evaporview-ghw-test--file name)))

(defun evaporview-ghw-test--changes (wave path)
  "Return the change list of the variable at PATH as ((TIME . VALUE) ...)."
  (let* ((var (evaporview-wave-find-var wave path))
         (sig (and var (evaporview-wave-var-sig wave var))))
    (and sig (cl-mapcar #'cons (evaporview-sig-times sig)
                        (evaporview-sig-values sig)))))

(defun evaporview-ghw-test--tree (scope)
  "Return SCOPE as (NAME TYPE VARNAMES CHILD...)."
  (append (list (evaporview-scope-name scope) (evaporview-scope-type scope)
                (mapcar #'evaporview-var-name (evaporview-scope-vars scope)))
          (mapcar #'evaporview-ghw-test--tree
                  (evaporview-scope-children scope))))

(defun evaporview-ghw-test--temp (bytes)
  "Write unibyte string BYTES to a temporary file and return its name."
  (let ((file (make-temp-file "evaporview-ghw" nil ".ghw")))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert bytes))
    file))

(defun evaporview-ghw-test--prefix (name end)
  "Return the first END bytes of test file NAME as a unibyte string.
A negative END counts from the end of the file."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (evaporview-ghw-test--file name))
    (buffer-substring-no-properties
     (point-min) (if (< end 0) (+ (point-max) end) (1+ end)))))

(defconst evaporview-ghw-test--expected
  '(
("ghw_tb.clk" (0 . "0") (5000000 . "1") (10000000 . "0") (15000000 . "1")
    (20000000 . "0") (25000000 . "1") (30000000 . "0") (35000000 . "1")
    (40000000 . "0") (45000000 . "1") (50000000 . "0") (55000000 . "1")
    (60000000 . "0"))
("ghw_tb.sl" (0 . "u") (10000000 . "1") (20000000 . "x") (40000000 . "0"))
("ghw_tb.slv_d" (0 . "0000") (10000000 . "1010") (20000000 . "10z1")
    (40000000 . "0000"))
("ghw_tb.slv_t" (0 . "0001") (10000000 . "1000"))
("ghw_tb.u8" (0 . "00000000") (10000000 . "10100101"))
("ghw_tb.s8" (0 . "00000000") (10000000 . "11110000")
    (30000000 . "10000000"))
("ghw_tb.int" (0 . "00000000000000000000000000000000")
    (10000000 . "11111111111111111111111111111101")
    (20000000 . "00000000000000011000011010100000")
    (30000000 . "01111111111111111111111111111111")
    (40000000 . "10000000000000000000000000000000"))
("ghw_tb.nat" (0 . "00000000000000000000000000000000")
    (10000000 . "00000000000000000000000000000111")
    (30000000 . "00000000000000000000000000001111"))
("ghw_tb.re" (0 . "0.0") (10000000 . "1.5") (20000000 . "-0.25"))
("ghw_tb.flag" (0 . "0") (10000000 . "1") (30000000 . "0"))
("ghw_tb.b" (0 . "0") (10000000 . "1"))
("ghw_tb.state" (0 . "idle") (10000000 . "load") (20000000 . "shift")
    (30000000 . "done") (40000000 . "idle"))
("ghw_tb.rec.count" (0 . "000") (10000000 . "101"))
("ghw_tb.rec.inner.flag" (0 . "0") (10000000 . "1"))
("ghw_tb.rec.inner.word" (0 . "0000") (10000000 . "1100")
    (20000000 . "0011"))
("ghw_tb.rec.ok" (0 . "0") (10000000 . "1"))
("ghw_tb.mem.mem(0)" (0 . "00000000") (20000000 . "00000001"))
("ghw_tb.mem.mem(1)" (0 . "00000000"))
("ghw_tb.mem.mem(2)" (0 . "00000000") (10000000 . "01011010")
    (20000000 . "11111111"))
("ghw_tb.mem.mem(3)" (0 . "00000000"))
("ghw_tb.tm"
    (0 . "0000000000000000000000000000000000000000000000000000000000000000") (10000000 . "0000000000000000000000000000000000000000001011011100011011000000"))
("ghw_tb.q" (0 . "uu") (5000000 . "01") (15000000 . "10")
    (25000000 . "11") (35000000 . "00") (45000000 . "01") (55000000 . "10"))
("ghw_tb.gen(0).g" (0 . "0") (10000000 . "1"))
("ghw_tb.gen(1).g" (0 . "0") (20000000 . "1"))
)
  "Per variable of ghw_tb.ghw: path and its full change list.")

(defconst evaporview-ghw-test--meta
  '(("ghw_tb.clk" 1 nil "std_logic")
    ("ghw_tb.sl" 1 nil "std_logic")
    ("ghw_tb.slv_d" 4 "[3:0]" "std_ulogic_vector")
    ("ghw_tb.slv_t" 4 "[0:3]" "std_ulogic_vector")
    ("ghw_tb.u8" 8 "[7:0]" "unresolved_unsigned")
    ("ghw_tb.s8" 8 "[7:0]" "unresolved_signed")
    ("ghw_tb.int" 32 nil "integer")
    ("ghw_tb.nat" 32 nil "integer")
    ("ghw_tb.re" 64 nil "real")
    ("ghw_tb.flag" 1 nil "boolean")
    ("ghw_tb.b" 1 nil "bit")
    ("ghw_tb.state" 2 nil "state_t")
    ("ghw_tb.rec.count" 3 "[2:0]" "unresolved_unsigned")
    ("ghw_tb.rec.inner.flag" 1 nil "std_logic")
    ("ghw_tb.rec.inner.word" 4 "[3:0]" "std_ulogic_vector")
    ("ghw_tb.rec.ok" 1 nil "boolean")
    ("ghw_tb.mem.mem(0)" 8 "[7:0]" "std_ulogic_vector")
    ("ghw_tb.mem.mem(1)" 8 "[7:0]" "std_ulogic_vector")
    ("ghw_tb.mem.mem(2)" 8 "[7:0]" "std_ulogic_vector")
    ("ghw_tb.mem.mem(3)" 8 "[7:0]" "std_ulogic_vector")
    ("ghw_tb.tm" 64 nil "time")
    ("ghw_tb.q" 2 "[1:0]" "std_ulogic_vector")
    ("ghw_tb.leaf.clk" 1 nil "std_logic")
    ("ghw_tb.leaf.q" 2 "[1:0]" "std_ulogic_vector")
    ("ghw_tb.gen(0).g" 1 nil "std_logic")
    ("ghw_tb.gen(1).g" 1 nil "std_logic"))
  "Declaration-ordered (PATH WIDTH RANGE TYPE) of ghw_tb.ghw.")

;;;; rec.ghw: record signals

(ert-deftest evaporview-ghw-record-scopes ()
  (let ((w (evaporview-ghw-test--wave "rec.ghw")))
    (should (equal (evaporview-ghw-test--tree (evaporview-wave-root w))
                   '("" "root" nil
                     ("rec_tb" "instance" ("clk" "rst")
                      ("r" "record" ("cnt" "busy"))
                      ("rin" "record" ("cnt" "busy"))))))
    (should (equal (mapcar #'evaporview-var-path (evaporview-wave-vars w))
                   '("rec_tb.clk" "rec_tb.rst" "rec_tb.r.cnt" "rec_tb.r.busy"
                     "rec_tb.rin.cnt" "rec_tb.rin.busy")))))

(ert-deftest evaporview-ghw-record-vars ()
  (let* ((w (evaporview-ghw-test--wave "rec.ghw"))
         (cnt (evaporview-wave-find-var w "rec_tb.r.cnt"))
         (busy (evaporview-wave-find-var w "rec_tb.r.busy")))
    (should (= (evaporview-var-width cnt) 4))
    (should (equal (evaporview-var-range cnt) "[3:0]"))
    (should (= (evaporview-var-width busy) 1))
    (should (null (evaporview-var-range busy)))
    (should (equal (evaporview-wave-timescale w) '(1 . "fs")))
    (should (= (evaporview-wave-start w) 0))
    (should (= (evaporview-wave-end w) 50000000))
    (should (equal (evaporview-wave-file w)
                   (expand-file-name (evaporview-ghw-test--file "rec.ghw"))))))

(ert-deftest evaporview-ghw-record-values ()
  (let ((w (evaporview-ghw-test--wave "rec.ghw")))
    (should (equal (evaporview-ghw-test--changes w "rec_tb.rst")
                   '((0 . "1") (12000000 . "0"))))
    (should (equal (evaporview-ghw-test--changes w "rec_tb.r.cnt")
                   '((0 . "0000") (15000000 . "0001") (25000000 . "0010")
                     (35000000 . "0011") (45000000 . "0100"))))
    (should (equal (evaporview-ghw-test--changes w "rec_tb.r.busy")
                   '((0 . "0") (15000000 . "1") (25000000 . "0")
                     (35000000 . "1") (45000000 . "0"))))
    ;; The snapshot holds 0 and the first cycle at time 0 raises it: the
    ;; later value wins.
    (should (equal (evaporview-ghw-test--changes w "rec_tb.rin.busy")
                   '((0 . "1") (15000000 . "0") (25000000 . "1")
                     (35000000 . "0") (45000000 . "1"))))
    (should (equal (evaporview-ghw-test--changes w "rec_tb.rin.cnt")
                   '((0 . "0001") (15000000 . "0010") (25000000 . "0011")
                     (35000000 . "0100") (45000000 . "0101"))))))

;;;; ghw_tb.ghw: every supported kind

(ert-deftest evaporview-ghw-tb-scope-tree ()
  (let ((w (evaporview-ghw-test--wave "ghw_tb.ghw")))
    (should (equal
             (evaporview-ghw-test--tree (evaporview-wave-root w))
             '("" "root" nil
               ("ghw_tb" "instance"
                ("clk" "sl" "slv_d" "slv_t" "u8" "s8" "int" "nat" "re" "flag"
                 "b" "state" "tm" "q")
                ("rec" "record" ("count" "ok")
                 ("inner" "record" ("flag" "word")))
                ("mem" "array" ("mem(0)" "mem(1)" "mem(2)" "mem(3)"))
                ("leaf" "instance" ("clk" "q"))
                ("gen(0)" "generate" ("g"))
                ("gen(1)" "generate" ("g"))))))))

(ert-deftest evaporview-ghw-tb-var-metadata ()
  (let ((w (evaporview-ghw-test--wave "ghw_tb.ghw")))
    (should (equal (mapcar (lambda (v)
                             (list (evaporview-var-path v)
                                   (evaporview-var-width v)
                                   (evaporview-var-range v)
                                   (evaporview-var-type v)))
                           (evaporview-wave-vars w))
                   evaporview-ghw-test--meta))
    (should (equal (evaporview-wave-timescale w) '(1 . "fs")))
    (should (= (evaporview-wave-start w) 0))
    (should (= (evaporview-wave-end w) 60000000))))

(ert-deftest evaporview-ghw-tb-change-lists ()
  (let ((w (evaporview-ghw-test--wave "ghw_tb.ghw")))
    (dolist (case evaporview-ghw-test--expected)
      (ert-info ((car case))
        (should (equal (evaporview-ghw-test--changes w (car case))
                       (cdr case)))))))

(ert-deftest evaporview-ghw-tb-sig-properties ()
  (let* ((w (evaporview-ghw-test--wave "ghw_tb.ghw"))
         (sig (lambda (path)
                (evaporview-wave-var-sig w (evaporview-wave-find-var w path)))))
    (should (evaporview-sig-real-p (funcall sig "ghw_tb.re")))
    (should-not (evaporview-sig-real-p (funcall sig "ghw_tb.int")))
    (should (= (evaporview-sig-width (funcall sig "ghw_tb.re")) 64))
    (should (equal (evaporview-sig-enum (funcall sig "ghw_tb.state"))
                   ["idle" "load" "shift" "done"]))
    (should (= (evaporview-sig-width (funcall sig "ghw_tb.state")) 2))
    (should (null (evaporview-sig-enum (funcall sig "ghw_tb.slv_d"))))
    (should (null (evaporview-sig-enum (funcall sig "ghw_tb.flag"))))
    (should (vectorp (evaporview-sig-times (funcall sig "ghw_tb.sl"))))
    (should (vectorp (evaporview-sig-values (funcall sig "ghw_tb.sl"))))))

(ert-deftest evaporview-ghw-tb-shared-signals ()
  "A port and the signal it connects to share one change list."
  (let* ((w (evaporview-ghw-test--wave "ghw_tb.ghw"))
         (id (lambda (p) (evaporview-var-id (evaporview-wave-find-var w p)))))
    (should (equal (funcall id "ghw_tb.clk") (funcall id "ghw_tb.leaf.clk")))
    (should (equal (funcall id "ghw_tb.q") (funcall id "ghw_tb.leaf.q")))
    (should-not (equal (funcall id "ghw_tb.q") (funcall id "ghw_tb.clk")))
    (dolist (v (append (evaporview-wave-vars w) nil))
      (should (gethash (evaporview-var-id v) (evaporview-wave-sigs w))))))

(ert-deftest evaporview-ghw-tb-queries ()
  "The reader's output works with the model's query functions."
  (let* ((w (evaporview-ghw-test--wave "ghw_tb.ghw"))
         (sig (evaporview-wave-var-sig
               w (evaporview-wave-find-var w "ghw_tb.slv_d"))))
    (should (equal (evaporview-sig-value-at sig 25000000) "10z1"))
    (should (equal (evaporview-sig-value-at sig 5000000) "0000"))))

;;;; Containers and damaged files

(ert-deftest evaporview-ghw-gzip ()
  (let ((file (evaporview-ghw-test--file "ghw_tb.ghw.gz")))
    (if (and (fboundp 'zlib-available-p) (zlib-available-p))
        (should (equal (evaporview-ghw-test--changes
                        (evaporview-ghw-parse-file file) "ghw_tb.int")
                       (cdr (assoc "ghw_tb.int"
                                   evaporview-ghw-test--expected))))
      (should-error (evaporview-ghw-parse-file file) :type 'user-error))))

(ert-deftest evaporview-ghw-bzip2-refused ()
  (let ((file (evaporview-ghw-test--temp "BZh91AY&SY")))
    (unwind-protect
        (let ((err (should-error (evaporview-ghw-parse-file file)
                                 :type 'user-error)))
          (should (string-match-p "uncompressed" (cadr err))))
      (delete-file file))))

(ert-deftest evaporview-ghw-not-ghw ()
  (let ((file (evaporview-ghw-test--temp "$date today $end\n")))
    (unwind-protect
        (should-error (evaporview-ghw-parse-file file) :type 'user-error)
      (delete-file file))))

(ert-deftest evaporview-ghw-truncated-dump ()
  "A file cut inside the value data keeps everything before the cut."
  (let* ((full (evaporview-ghw-test--wave "ghw_tb.ghw"))
         (file (evaporview-ghw-test--temp
                (evaporview-ghw-test--prefix "ghw_tb.ghw" -160))))
    (unwind-protect
        (let ((w (evaporview-ghw-parse-file file)))
          (should (< (evaporview-wave-end w) (evaporview-wave-end full)))
          (should (equal (evaporview-ghw-test--changes w "ghw_tb.sl")
                         '((0 . "u") (10000000 . "1") (20000000 . "x"))))
          (should (= (evaporview-wave-end w) 30000000)))
      (delete-file file))))

(ert-deftest evaporview-ghw-truncated-header ()
  (let ((file (evaporview-ghw-test--temp
               (evaporview-ghw-test--prefix "ghw_tb.ghw" 300))))
    (unwind-protect
        (should-error (evaporview-ghw-parse-file file))
      (delete-file file))))

(ert-deftest evaporview-ghw-integer-encoding ()
  ;; Values chosen so the LEB128 sign bit (bit 6 of the last byte) differs
  ;; from its neighbours; expected values are ghwdump's.
  (let ((ch (evaporview-ghw-test--changes
             (evaporview-ghw-test--wave "ghw_int_tb.ghw") "ghw_int_tb.n")))
    (should (equal (mapcar #'car ch)
                   '(0 10000000 20000000 30000000 40000000 50000000 60000000)))
    (should (equal (mapcar (lambda (c) (evaporview-format-bits (cdr c) 'signed)) ch)
                   '("0" "40" "-70" "63" "64" "-64" "8191")))))

(provide 'evaporview-ghw-test)

;;; evaporview-ghw-test.el ends here

;;; evaporview-test.el --- ERT suite for evaporview  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l test/evaporview-test.el -f ert-run-tests-batch-and-exit
;;
;; test/data/counter.vcd is real GHDL 6.0 output of test/data/counter_tb.vhd.
;; test/format-cases.json is shared with the browser viewer's suite
;; (test/viewer.test.ts), so both formatters answer to the same table.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'evaporview)
(require 'evaporview-html)

(defconst evaporview-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun evaporview-test--file (name)
  (expand-file-name name evaporview-test--dir))

(defconst evaporview-test--vcd "$date today $end
$timescale 10ps $end
$scope module top $end
$var wire 1 ! clk $end
$var wire 8 \" data [7:0] $end
$var real 64 # vr $end
$scope module sub $end
$var wire 1 ! clk_alias $end
$var wire 4 $ nib [3:0] $end
$upscope $end
$upscope $end
$enddefinitions $end
$comment ignored #99 1! $end
#0
$dumpvars
0!
b1 \"
r0.5 #
bz $
$end
#10
1!
b1x \"
#20
0!
bx \"
b10 $
#30
1!
0!
1!
#40
1!
b10 $
")

(defconst evaporview-test--render-vcd "$timescale 1ns $end
$scope module t $end
$var wire 1 a clk $end
$var wire 8 b d [7:0] $end
$var wire 1 c z $end
$upscope $end
$enddefinitions $end
#0
0a
b00000000 b
zc
#20
1a
#40
0a
b00101010 b
xc
#60
1a
#80
0a
b11111111 b
1c
#100
")

(defun evaporview-test--wave ()
  (evaporview-vcd-parse-string evaporview-test--vcd))

(defun evaporview-test--sig (wave path)
  (evaporview-wave-var-sig wave (evaporview-wave-find-var wave path)))

;;;; Parser

(ert-deftest evaporview-test-header ()
  (let* ((w (evaporview-test--wave))
         (root (evaporview-wave-root w))
         (top (car (evaporview-scope-children root))))
    (should (equal (evaporview-wave-timescale w) '(10 . "ps")))
    (should (equal (evaporview-scope-name top) "top"))
    (should (equal (mapcar #'evaporview-scope-name (evaporview-scope-children top))
                   '("sub")))
    (should (equal (mapcar #'evaporview-var-name (evaporview-scope-vars top))
                   '("clk" "data" "vr")))
    (should (= (length (evaporview-wave-vars w)) 5))
    (let ((data (evaporview-wave-find-var w "top.data")))
      (should (= (evaporview-var-width data) 8))
      (should (equal (evaporview-var-range data) "[7:0]"))
      (should (equal (evaporview-var-display-name data) "data[7:0]")))
    (should (eq (evaporview-test--sig w "top.clk")
                (evaporview-test--sig w "top.sub.clk_alias")))
    (should (evaporview-sig-real-p (evaporview-test--sig w "top.vr")))
    (should (= (evaporview-wave-start w) 0))
    (should (= (evaporview-wave-end w) 40))))

(ert-deftest evaporview-test-changes ()
  (let* ((w (evaporview-test--wave))
         (clk (evaporview-test--sig w "top.clk"))
         (data (evaporview-test--sig w "top.data"))
         (nib (evaporview-test--sig w "top.sub.nib")))
    ;; Same-time glitches keep the last value; repeats are not changes.
    (should (equal (evaporview-sig-times clk) [0 10 20 30]))
    (should (equal (evaporview-sig-values clk) ["0" "1" "0" "1"]))
    ;; IEEE 1364 left-extension: 0/1 pad with 0, x with x, z with z.
    (should (equal (evaporview-sig-values data)
                   ["00000001" "0000001x" "xxxxxxxx"]))
    (should (equal (evaporview-sig-values nib) ["zzzz" "0010"]))
    (should (equal (evaporview-sig-times nib) [0 20]))
    (should (equal (evaporview-sig-values (evaporview-test--sig w "top.vr"))
                   ["0.5"]))))

(ert-deftest evaporview-test-ghdl-fixture ()
  (let* ((w (evaporview-vcd-parse-file (evaporview-test--file "data/counter.vcd")))
         (cnt (evaporview-test--sig w "counter_tb.cnt"))
         (bus4 (evaporview-test--sig w "counter_tb.bus4"))
         (ana (evaporview-test--sig w "counter_tb.ana"))
         (done (evaporview-test--sig w "counter_tb.done")))
    (should (equal (evaporview-wave-timescale w) '(1 . "fs")))
    (should (equal (mapcar #'evaporview-scope-name
                           (evaporview-scope-children (evaporview-wave-root w)))
                   '("standard" "textio" "std_logic_1164" "numeric_std" "counter_tb")))
    (should (equal (evaporview-var-range (evaporview-wave-find-var w "counter_tb.cnt"))
                   "[7:0]"))
    (should (evaporview-wave-find-var w "counter_tb.dut.r"))
    (should (= (evaporview-wave-end w) 159000000))
    (should (equal (evaporview-sig-value-at cnt 15000000) "00000001"))
    (should (equal (evaporview-sig-value-at cnt 14999999) "00000000"))
    (should (equal (evaporview-sig-value-at bus4 12000000) "10x1"))
    (should (equal (evaporview-sig-value-at bus4 0) "zzzz"))
    (should (equal (evaporview-sig-value-at done 0) "u"))
    (should (evaporview-sig-real-p ana))
    (should (equal (evaporview-format-value ana (evaporview-sig-value-at ana 52000000)
                                            'real)
                   "-1"))))

(ert-deftest evaporview-test-questa-split-vectors ()
  ;; Real Questa 2021.2 output: records under `$scope fork', vectors as bits.
  (let* ((w (evaporview-vcd-parse-file (evaporview-test--file "data/rec-questa.vcd")))
         (cnt (evaporview-wave-find-var w "rec_tb.r.cnt"))
         (rin (evaporview-wave-find-var w "rec_tb.rin.cnt")))
    (should (equal (sort (mapcar #'evaporview-var-path (evaporview-wave-vars w)) #'string<)
                   '("rec_tb.clk" "rec_tb.r.busy" "rec_tb.r.cnt"
                     "rec_tb.rin.busy" "rec_tb.rin.cnt" "rec_tb.rst")))
    (should (= (evaporview-var-width cnt) 4))
    (should (equal (evaporview-var-range cnt) "[3:0]"))
    (should (equal (evaporview-var-display-name cnt) "cnt[3:0]"))
    (should (equal (mapcar #'evaporview-var-name
                           (evaporview-scope-vars
                            (cadr (evaporview-scope-children
                                   (car (evaporview-scope-children (evaporview-wave-root w)))))))
                   '("cnt" "busy")))
    (let ((s (evaporview-wave-var-sig w cnt)) (r (evaporview-wave-var-sig w rin)))
      (should (equal (evaporview-sig-value-at s 0) "0000"))
      (should (equal (evaporview-sig-value-at s 15) "0001"))
      (should (equal (evaporview-sig-value-at r 0) "0001"))
      (should (equal (evaporview-sig-value-at r 15) "0010"))
      (should (equal (aref (evaporview-sig-times s) 0) 0)))))

(ert-deftest evaporview-test-enum-signal ()
  ;; State-machine types arrive from GHW with their literal names as values.
  (let ((fsm (evaporview-sig--make :id "f" :width 2 :enum ["IDLE" "LOAD" "SHIFT"]
                                   :times [0 50 100] :values ["IDLE" "LOAD" "SHIFT"]))
        (onoff (evaporview-sig--make :id "o" :width 1 :enum ["OFF" "ON"]
                                     :times [0 50] :values ["OFF" "ON"])))
    (should (equal (evaporview-format-value fsm "LOAD" 'hex) "LOAD"))
    (should (= (evaporview-value-number fsm "SHIFT" 'hex) 2.0))
    (let ((row (evaporview-render-row fsm 'hex 'digital 0 10 12 1)))
      (should (equal (substring-no-properties row) "IDLE═╳LOAD╳…"))
      (should (eq (get-text-property 0 'face row) 'evaporview-color-1)))
    ;; A two-literal enum is still drawn with its names, not as a bit.
    (should (equal (substring-no-properties (evaporview-render-row onoff 'hex 'digital 0 10 8))
                   "OFF══╳ON"))))

(ert-deftest evaporview-test-open-does-not-modify-dump ()
  (let* ((file (evaporview-test--file "data/counter.vcd"))
         (before (with-temp-buffer (insert-file-contents-literally file)
                                   (secure-hash 'sha256 (current-buffer))))
         (session (make-temp-file "evw-session" nil ".json")))
    (unwind-protect
        (with-current-buffer (evaporview-open file)
          (evaporview-add-signal "counter_tb.cnt")
          (evaporview-save-session session)
          (kill-buffer))
      (delete-file session))
    (should (equal before (with-temp-buffer (insert-file-contents-literally file)
                                            (secure-hash 'sha256 (current-buffer)))))))

;;;; Formatting

(defun evaporview-test--hex-to-bits (hex width)
  (let ((bits (mapconcat
               (lambda (c)
                 (let ((n (string-to-number (string c) 16)) (s ""))
                   (dotimes (_ 4) (setq s (concat (if (= (logand n 1) 1) "1" "0") s)
                                        n (ash n -1)))
                   s))
               hex "")))
    (if (> (length bits) width)
        (substring bits (- (length bits) width))
      (concat (make-string (- width (length bits)) ?0) bits))))

(ert-deftest evaporview-test-format-table ()
  (let ((cases (let ((json-object-type 'alist) (json-array-type 'list)
                     (json-key-type 'symbol))
                 (json-read-file (evaporview-test--file "format-cases.json"))))
        (failures nil))
    (should (> (length cases) 50))
    (dolist (c cases)
      (let* ((fmt (intern (alist-get 'fmt c)))
             (real (alist-get 'real c))
             (bits (or (alist-get 'bits c)
                       (and (alist-get 'hex c)
                            (evaporview-test--hex-to-bits (alist-get 'hex c)
                                                          (alist-get 'width c)))))
             (got (if real (evaporview-format-real real)
                    (evaporview-format-bits bits fmt))))
        (unless (equal got (alist-get 'out c))
          (push (list c got) failures))))
    (should (equal failures nil))))

(ert-deftest evaporview-test-format-time ()
  (should (equal (evaporview-format-time 12000000 '(1 . "fs") "ns") "12 ns"))
  (should (equal (evaporview-format-time 12500000 '(1 . "fs") "ns") "12.5 ns"))
  (should (equal (evaporview-format-time 3 '(10 . "ps") "ps") "30 ps"))
  (should (equal (evaporview-format-time 3 '(10 . "ps") "ns") "0.03 ns"))
  (should (equal (evaporview-format-time 2 '(1 . "us") "ns") "2000 ns"))
  (should (equal (evaporview-auto-time-unit 159000000 '(1 . "fs")) "ns"))
  (should (equal (evaporview-auto-time-unit 40 '(10 . "ps")) "ps"))
  ;; 1 / 1e-6 is 999999.99... in floating point; it is still 1 MHz.
  (should (equal (evaporview-format-frequency 1000000000 '(1 . "fs")) "1 MHz"))
  (should (equal (evaporview-format-frequency 10000000 '(1 . "fs")) "100 MHz"))
  (should (equal (evaporview-format-frequency 3 '(1 . "s")) "0.333333 Hz")))

;;;; Navigation

(ert-deftest evaporview-test-navigation ()
  (let* ((w (evaporview-vcd-parse-file (evaporview-test--file "data/counter.vcd")))
         (clk (evaporview-test--sig w "counter_tb.clk"))
         (cnt (evaporview-test--sig w "counter_tb.cnt")))
    (should (= (evaporview-sig-next-change clk 0) 5000000))
    (should (= (evaporview-sig-next-change clk 5000000) 10000000))
    (should (= (evaporview-sig-prev-change clk 10000000) 5000000))
    (should (null (evaporview-sig-prev-change clk 0)))
    (should (null (evaporview-sig-next-change cnt 159000000)))
    (should (= (evaporview-sig-next-edge clk 0 'rise) 5000000))
    (should (= (evaporview-sig-next-edge clk 5000000 'rise) 15000000))
    (should (= (evaporview-sig-next-edge clk 0 'fall) 10000000))
    (should (= (evaporview-sig-prev-edge clk 12000000 'fall) 10000000))
    (should (= (evaporview-sig-prev-edge clk 12000000 'rise) 5000000))
    (should (null (evaporview-sig-prev-edge clk 5000000 'rise)))
    ;; Value search walks changes only, both directions.
    (should (= (evaporview-sig-search cnt 0 'forward
                                      (lambda (v) (equal v "00000011")))
               35000000))
    (should (= (evaporview-sig-search cnt 100000000 'backward
                                      (lambda (v) (equal v "00000001")))
               15000000))
    (should (null (evaporview-sig-search cnt 0 'forward #'ignore)))))

(ert-deftest evaporview-test-slice ()
  (let* ((w (evaporview-test--wave))
         (data (evaporview-test--sig w "top.data"))
         (low (evaporview-sig-slice data 1 0))
         (high (evaporview-sig-slice data 7 4)))
    (should (= (evaporview-sig-width low) 2))
    (should (equal (evaporview-sig-values low) ["01" "1x" "xx"]))
    ;; Bits 7..4 never change from 0000 until x: one change is dropped.
    (should (equal (evaporview-sig-times high) [0 20]))
    (should (equal (evaporview-sig-values high) ["0000" "xxxx"]))))

;;;; Rendering

(defun evaporview-test--row (wave path fmt)
  (substring-no-properties
   (evaporview-render-row (evaporview-test--sig wave path) fmt 'digital 0 10 10)))

(ert-deftest evaporview-test-render-rows ()
  (let ((w (evaporview-vcd-parse-string evaporview-test--render-vcd)))
    (should (equal (evaporview-test--row w "t.clk" 'binary) "▁▁│▔│▁│▔│▁"))
    (should (equal (evaporview-test--row w "t.d" 'hex) "00══╳2A═╳…"))
    (should (equal (evaporview-test--row w "t.z" 'binary) "────│▒▒▒│▔"))
    ;; Several changes inside one column collapse into one mark.
    (should (equal (substring-no-properties
                    (evaporview-render-row (evaporview-test--sig w "t.clk")
                                           'binary 'digital 0 50 2))
                   "┃┃"))
    ;; A clock faster than the columns is one uniform band, not a beat of
    ;; one-edge and two-edge columns: 5 ns half period at 7 ns per column.
    (let* ((vcd (concat "$timescale 1ns $end\n$scope module t $end\n"
                        "$var wire 1 a clk $end\n$upscope $end\n$enddefinitions $end\n"
                        (mapconcat (lambda (i) (format "#%d\n%da\n" (* 5 i) (% i 2)))
                                   (number-sequence 0 40) "")))
           (row (substring-no-properties
                 (evaporview-render-row
                  (evaporview-test--sig (evaporview-vcd-parse-string vcd) "t.clk")
                  'binary 'digital 0 7 20))))
      (should (equal (substring row 1) (make-string 19 ?┃))))
    ;; Analog style maps the value range onto eight block heights.
    (should (equal (substring-no-properties
                    (evaporview-render-row (evaporview-test--sig w "t.d")
                                           'unsigned 'analog 0 10 10))
                   "▁▁▁▁▂▂▂▂██"))))

(ert-deftest evaporview-test-view-render ()
  (with-current-buffer (evaporview-open (evaporview-test--file "data/counter.vcd"))
    (unwind-protect
        (let ((evaporview--width-override 100))
          (evaporview-add-signal "counter_tb.clk")
          (evaporview-add-signal "counter_tb.cnt")
          (evaporview-set-marker 15000000)
          (evaporview-set-alt-marker 5000000)
          (evaporview-redraw)
          (goto-char (point-min))
          (should (search-forward "Δ 10 ns" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^ *clk +│ 1 +│" nil t))
          (should (re-search-forward "^ *cnt\\[7:0\\] +│ 01 +│" nil t))
          ;; The marker column carries the marker face on every row.
          (let ((col (evaporview--time-column 15000000)))
            (should (memq 'evaporview-marker-face
                          (evaporview-test--faces (get-text-property
                                         (+ (line-beginning-position) col)
                                         'face))))))
      (kill-buffer))))

(defun evaporview-test--faces (f) (if (listp f) f (list f)))

(ert-deftest evaporview-test-row-spacing ()
  (evaporview-test--with-view
    (let ((evaporview-row-spacing 1))
      (evaporview-add-signal "counter_tb.clk")
      (evaporview-add-signal "counter_tb.cnt")
      (evaporview-set-marker 15000000)
      (evaporview-redraw)
      (evaporview-goto-item 0)
      (should (looking-at " *clk +│"))
      ;; A blank line separates the rows and keeps the column frame.
      (forward-line 1)
      (should (looking-at " +│ +│ +$"))
      (should (= (evaporview--row-index) 0))
      (let ((col (evaporview--time-column 15000000)))
        (should (memq 'evaporview-marker-face
                      (evaporview-test--faces
                       (get-text-property (+ (line-beginning-position) col) 'face)))))
      (forward-line 1)
      (should (looking-at " *cnt\\[7:0\\] +│"))
      (should (= (evaporview--row-index) 1))
      (evaporview-goto-item 1)
      (should (looking-at " *cnt"))
      ;; Line motion steps whole rows.
      (evaporview-goto-item 0)
      (call-interactively (key-binding (kbd "C-n")))
      (should (= (evaporview--row-index) 1))
      (should (looking-at " *cnt"))
      (call-interactively (key-binding (kbd "C-p")))
      (should (looking-at " *clk")))
    (let ((evaporview-row-spacing 0))
      (evaporview-redraw)
      (evaporview-goto-item 0)
      (forward-line 1)
      (should (looking-at " *cnt")))))

;;;; Editing, undo, sessions

(defmacro evaporview-test--with-view (&rest body)
  (declare (indent 0))
  `(with-current-buffer (evaporview-open (evaporview-test--file "data/counter.vcd"))
     (unwind-protect (let ((evaporview--width-override 100)) ,@body)
       (kill-buffer))))

(defun evaporview-test--paths ()
  (mapcar #'evaporview-item-path (evaporview-view-items evaporview--view)))

(ert-deftest evaporview-test-edit-and-undo ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-add-signal "counter_tb.cnt")
    (evaporview-add-signal "counter_tb.bus4")
    (should (equal (evaporview-test--paths)
                   '("counter_tb.clk" "counter_tb.cnt" "counter_tb.bus4")))
    (evaporview-goto-item 1)
    (evaporview-move-item-up)
    (should (equal (evaporview-test--paths)
                   '("counter_tb.cnt" "counter_tb.clk" "counter_tb.bus4")))
    (evaporview-goto-item 0)
    (evaporview-set-format 'unsigned)
    (evaporview-set-color 3)
    (evaporview-rename "count")
    (let ((it (car (evaporview-view-items evaporview--view))))
      (should (eq (evaporview-item-format it) 'unsigned))
      (should (= (evaporview-item-color it) 3))
      (should (equal (evaporview-item-name it) "count")))
    (evaporview-goto-item 2)
    (evaporview-remove-item)
    (should (= (length (evaporview-view-items evaporview--view)) 2))
    (evaporview-undo)
    (should (= (length (evaporview-view-items evaporview--view)) 3))
    (evaporview-undo)
    (should (equal (evaporview-item-name (car (evaporview-view-items evaporview--view)))
                   nil))
    (evaporview-redo)
    (should (equal (evaporview-item-name (car (evaporview-view-items evaporview--view)))
                   "count"))))

(ert-deftest evaporview-test-slice-split-group ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.cnt")
    (evaporview-goto-item 0)
    (evaporview-split-signal 4)
    (let ((items (evaporview-view-items evaporview--view)))
      (should (= (length items) 3))
      (should (equal (mapcar #'evaporview-item-slice (cdr items)) '((7 . 4) (3 . 0))))
      (should (equal (evaporview-item-label (nth 1 items)) "cnt[7:4]")))
    (evaporview-goto-item 1)
    (evaporview-group-items "nibbles" 1 2)
    (let* ((items (evaporview-view-items evaporview--view))
           (g (nth 1 items)))
      (should (= (length items) 2))
      (should (eq (evaporview-item-kind g) 'group))
      (should (= (length (evaporview-item-children g)) 2)))
    (evaporview-redraw)
    (goto-char (point-min))
    (should (search-forward "▾ nibbles" nil t))
    (should (search-forward "cnt[3:0]" nil t))))

(ert-deftest evaporview-test-session-roundtrip ()
  (let ((file (make-temp-file "evw-session" nil ".json")))
    (unwind-protect
        (progn
          (evaporview-test--with-view
            (evaporview-add-signal "counter_tb.cnt")
            (evaporview-add-signal "counter_tb.clk")
            (evaporview-goto-item 0)
            (evaporview-set-format 'unsigned)
            (evaporview-split-signal 4)
            (evaporview-group-items "g" 1 2)
            (evaporview-set-marker 15000000)
            (evaporview-save-session file))
          (evaporview-test--with-view
            (evaporview-load-session file)
            (let ((items (evaporview-view-items evaporview--view)))
              (should (equal (mapcar #'evaporview-item-kind items)
                             '(signal group signal)))
              (should (eq (evaporview-item-format (car items)) 'unsigned))
              (should (equal (evaporview-item-slice
                              (car (evaporview-item-children (nth 1 items))))
                             '(7 . 4))))
            (should (= (evaporview-view-marker evaporview--view) 15000000))))
      (delete-file file))))

(ert-deftest evaporview-test-session-from-browser ()
  ;; Written by the browser viewer's serializer (test/viewer.test.ts asserts it).
  (evaporview-test--with-view
    (evaporview-load-session (evaporview-test--file "data/session-from-viewer.json"))
    (let ((items (evaporview-view-items evaporview--view)))
      (should (equal (evaporview-item-path (car items)) "counter_tb.clk"))
      (should (eq (evaporview-item-format (nth 1 items)) 'hex))
      (should (eq (evaporview-item-kind (nth 2 items)) 'group)))
    (should (= (evaporview-view-alt-marker evaporview--view) 5000000))))

(ert-deftest evaporview-test-edge-commands ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-goto-item 0)
    (evaporview-set-marker 0)
    (evaporview-next-edge)
    (should (= (evaporview-view-marker evaporview--view) 5000000))
    (evaporview-next-edge)
    (should (= (evaporview-view-marker evaporview--view) 10000000))
    (evaporview-prev-rising-edge)
    (should (= (evaporview-view-marker evaporview--view) 5000000))
    (evaporview-next-falling-edge)
    (should (= (evaporview-view-marker evaporview--view) 10000000))
    (evaporview-zoom-fit)
    (should (= (evaporview-view-start evaporview--view) 0))
    (let ((tpc (evaporview-view-tpc evaporview--view)))
      (evaporview-zoom-in)
      (should (= (evaporview-view-tpc evaporview--view) (/ tpc 2.0))))))

;;;; WaveDrom

(ert-deftest evaporview-test-wavedrom ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-add-signal "counter_tb.cnt")
    (evaporview-set-marker 15000000)
    (evaporview-set-alt-marker 35000000)
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
           (unclocked (json-read-from-string (evaporview-wavedrom-string)))
           (clocked (json-read-from-string
                     (evaporview-wavedrom-string "counter_tb.clk" 'rise)))
           (lane (lambda (doc n) (nth n (alist-get 'signal doc)))))
      ;; Every clk/cnt change in [15, 35] ns is an event.
      (should (equal (alist-get 'wave (funcall lane unclocked 0)) "10101"))
      (should (equal (alist-get 'wave (funcall lane unclocked 1)) "=.=.="))
      (should (equal (alist-get 'data (funcall lane unclocked 1)) '("01" "02" "03")))
      (should (equal (alist-get 'wave (funcall lane clocked 0)) "p.."))
      (should (equal (alist-get 'wave (funcall lane clocked 1)) "==="))))
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-add-signal "counter_tb.bus4")
    (evaporview-set-marker 0)
    (evaporview-set-alt-marker 159000000)
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
           (doc (json-read-from-string (evaporview-wavedrom-string))))
      (should (= (length (alist-get 'wave (car (alist-get 'signal doc)))) 32)))))

(ert-deftest evaporview-test-wavedrom-real ()
  ;; Real values are text, not bits: they are data lanes, never "x".
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.ana")
    (evaporview-set-marker 50000000)
    (evaporview-set-alt-marker 66000000)
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
           (lane (car (alist-get 'signal (json-read-from-string (evaporview-wavedrom-string))))))
      (should (equal (alist-get 'wave lane) "===="))
      (should (equal (alist-get 'data lane) '("0" "-1" "-0.75" "-0.5"))))))

(ert-deftest evaporview-test-wavedrom-head ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-set-marker 15000000)
    (evaporview-set-alt-marker 35000000)
    (let* ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol)
           (plain (json-read-from-string (evaporview-wavedrom-string)))
           (clocked (json-read-from-string
                     (evaporview-wavedrom-string "counter_tb.clk" 'rise))))
      (should (equal (alist-get 'text (alist-get 'head plain))
                     "counter.vcd  15 ns - 35 ns"))
      (should-not (assq 'tick (alist-get 'head plain)))
      ;; Clocked diagrams number their cycles.
      (should (equal (alist-get 'tick (alist-get 'head clocked)) 0)))))

(ert-deftest evaporview-test-wavedrom-org-block ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.cnt")
    (evaporview-set-marker 15000000)
    (evaporview-set-alt-marker 35000000)
    (let ((block (evaporview-wavedrom-org-block)))
      (should (string-match-p "\\`#\\+begin_src wavedrom :file counter-15ns-35ns\\.svg\n" block))
      (should (string-match-p "\n#\\+end_src\n\\'" block))
      (should (string-match-p "\"cnt\\[7:0\\]\"" block)))))

(ert-deftest evaporview-test-ewavedrom-missing ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-set-marker 0)
    (evaporview-set-alt-marker 20000000)
    (cl-letf (((symbol-function 'evaporview--ewavedrom-available-p) #'ignore))
      (should-error (evaporview-wavedrom-svg) :type 'user-error))))

(ert-deftest evaporview-test-ewavedrom-render ()
  (skip-unless (locate-library "ewavedrom"))
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.clk")
    (evaporview-add-signal "counter_tb.cnt")
    (evaporview-set-marker 15000000)
    (evaporview-set-alt-marker 35000000)
    (let ((svg (evaporview-wavedrom-svg "counter_tb.clk" 'rise)))
      (should (string-prefix-p "<svg" svg))
      (should (string-match-p ">cnt\\[7:0\\]<" svg))
      (should (string-match-p ">02<" svg)))
    (let ((file (make-temp-file "evw" nil ".svg")))
      (unwind-protect
          (progn (evaporview-wavedrom-export-svg file)
                 (should (string-prefix-p "<svg" (with-temp-buffer
                                                   (insert-file-contents file)
                                                   (buffer-string)))))
        (delete-file file)))
    (let ((src (evaporview-wavedrom-preview)))
      (unwind-protect
          (with-current-buffer src
            (should (derived-mode-p 'ewavedrom-mode))
            (should (string-match-p "\"signal\"" (buffer-string)))
            (should (with-current-buffer (ewavedrom--preview-buffer-name src)
                      (string-prefix-p "<svg" ewavedrom--preview-svg))))
        (kill-buffer src)))))

;;;; HTML export

(ert-deftest evaporview-test-html-self-contained ()
  (evaporview-test--with-view
    (evaporview-add-signal "counter_tb.cnt")
    (let ((html (evaporview-html-string evaporview--view)))
      (should (string-match-p "counter_tb" html))
      (should (string-match-p "<canvas\\|getContext" html))
      (should-not (evaporview-html-external-refs html))
      ;; The scanner must catch a planted reference, or its silence means nothing.
      (should (evaporview-html-external-refs
               "<script src=\"https://cdn.example/x.js\"></script>"))
      (should (evaporview-html-external-refs "<link href='//x/y.css'>"))
      (should (evaporview-html-external-refs "fetch('data.json')"))
      (should-not (string-match-p "</" (evaporview-html--json-escape "{\"n\":\"</script>\"}")))
      (should (= (evaporview-test--count "<script" html)
                 (evaporview-test--count "</script>" html))))))

(defun evaporview-test--count (needle hay)
  (let ((n 0) (start 0))
    (while (setq start (string-match (regexp-quote needle) hay start))
      (setq n (1+ n) start (1+ start)))
    n))

;;;; Documentation

(ert-deftest evaporview-test-readme-lists-keys ()
  (let ((readme (with-temp-buffer
                  (insert-file-contents (evaporview-test--file "../README.org"))
                  (buffer-string)))
        (map (copy-keymap evaporview-mode-map))
        (missing nil))
    (set-keymap-parent map nil)
    (map-keymap
     (lambda (_ev cmd)
       (when (and (symbolp cmd) (commandp cmd)
                  (not (string-match-p (regexp-quote (symbol-name cmd)) readme)))
         (push cmd missing)))
     map)
    (should (equal missing nil))))

(provide 'evaporview-test)
;;; evaporview-test.el ends here

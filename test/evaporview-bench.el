;;; evaporview-bench.el --- Parse cost on a generated dump  -*- lexical-binding: t; -*-

;;; Commentary:

;; make bench.  Writes build/bench.vcd (64 signals: one clock, 31 more 1-bit,
;; 32 buses of 32 bits) and reports parse and HTML-export time and size.

;;; Code:

(require 'evaporview)
(require 'evaporview-html)

(defun evaporview-bench--write (file steps)
  (with-temp-file file
    (insert "$timescale 1ps $end\n$scope module bench $end\n")
    (dotimes (i 32) (insert (format "$var wire 1 a%d bit%d $end\n" i i)))
    (dotimes (i 32) (insert (format "$var wire 32 b%d bus%d [31:0] $end\n" i i)))
    (insert "$upscope $end\n$enddefinitions $end\n")
    (let ((changes 0))
      (dotimes (s steps)
        (insert (format "#%d\n" (* s 1000)))
        (insert (format "%da0\n" (% s 2)))
        (setq changes (1+ changes))
        (dotimes (i 31)
          (when (= 0 (% s (+ 2 i)))
            (insert (format "%da%d\n" (% (/ s (+ 2 i)) 2) (1+ i)))
            (setq changes (1+ changes))))
        (dotimes (i 32)
          (when (= 0 (% s (+ 4 (* 2 i))))
            (let ((v (logand (* s (+ 7 i) 2654435761) #xffffffff)) (bits ""))
              (dotimes (_ 32) (setq bits (concat (if (= 1 (logand v 1)) "1" "0") bits)
                                    v (ash v -1)))
              (insert "b" bits (format " b%d\n" i))
              (setq changes (1+ changes))))))
      changes)))

(let* ((file (expand-file-name "build/bench.vcd"))
       (changes (evaporview-bench--write file 200000))
       (size (file-attribute-size (file-attributes file)))
       (t0 (float-time))
       (wave (evaporview-vcd-parse-file file))
       (t1 (float-time)))
  (ignore wave)
  (garbage-collect)
  (message "bench: %.1f MB, %d value changes, parse %.2f s (%.0f changes/s)"
           (/ size 1048576.0) changes (- t1 t0) (/ changes (- t1 t0)))
  (let* ((t2 (float-time))
         (out (expand-file-name "build/bench.html")))
    (with-current-buffer (evaporview-open file)
      (evaporview-add-signal "bench.bit0")
      (evaporview-add-signal "bench.bus0")
      (evaporview-html-write evaporview--view out)
      (kill-buffer))
    (message "bench: open + HTML export %.2f s, page %.1f MB"
             (- (float-time) t2)
             (/ (file-attribute-size (file-attributes out)) 1048576.0))))

;;; evaporview-bench.el ends here

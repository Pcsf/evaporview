;;; tour.el --- Open the SPI example ready to explore  -*- lexical-binding: t; -*-

;;; Commentary:

;; make try  (or: emacs -Q -L . -L ../ewavedrom -l examples/tour.el)

;;; Code:

(require 'evaporview)
(load (expand-file-name "sessions.el" (file-name-directory load-file-name)) nil t)

(let ((buf (evaporview-open (expand-file-name "spi.vcd" evaporview-examples-dir))))
  (switch-to-buffer buf)
  (delete-other-windows)
  (evaporview-netlist)
  (select-window (get-buffer-window buf))
  (evaporview-redraw)
  (when (require 'ewavedrom nil t)
    (evaporview-wavedrom-preview "spi_tb.sclk" 'rise)
    (select-window (get-buffer-window buf)))
  (message "%s" (substitute-command-keys
                 "Try: n/N edge steps, ]/[ rising edges, f format, W WaveDrom, B browser, \\[describe-mode] all keys")))

;;; tour.el ends here

;;; sessions.el --- Curated views of the example dumps  -*- lexical-binding: t; -*-

;;; Commentary:

;; `make examples' runs this in batch to write each dump's session file, which
;; `evaporview-open' then loads on its own.  examples/showcase.el reuses it.

;;; Code:

(require 'evaporview)

(defconst evaporview-examples-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun evaporview-examples--ns (n)
  (evaporview-ticks n "ns" (evaporview-wave-timescale (evaporview-view-wave evaporview--view))))

(defun evaporview-examples--add (path &rest props)
  "Add PATH and apply PROPS (:format :color :style :name) to it."
  (evaporview-goto-item (max 0 (1- (length evaporview--rows))))
  (evaporview-add-signal path)
  (let ((it (evaporview--current)))
    (when (plist-get props :format) (setf (evaporview-item-format it) (plist-get props :format)))
    (when (plist-get props :color) (setf (evaporview-item-color it) (plist-get props :color)))
    (when (plist-get props :style) (setf (evaporview-item-style it) (plist-get props :style)))
    (when (plist-get props :name) (setf (evaporview-item-name it) (plist-get props :name)))
    it))

(defun evaporview-examples--group (name from to)
  (evaporview-group-items name from to))

(defun evaporview-examples-spi ()
  (evaporview-examples--add "spi_tb.cs_n" :color 5)
  (evaporview-examples--add "spi_tb.sclk" :color 2)
  (evaporview-examples--add "spi_tb.mosi" :color 1)
  (evaporview-examples--add "spi_tb.miso" :color 3)
  (evaporview-examples--add "spi_tb.busy" :color 6)
  (evaporview-examples--add "spi_tb.tx_byte" :color 1)
  (evaporview-examples--add "spi_tb.rx_byte" :color 3)
  (evaporview-examples--add "spi_tb.bit_cnt" :format 'unsigned :color 4)
  (evaporview-examples--group "byte" 5 7)
  (evaporview-set-marker (evaporview-examples--ns 40))
  (evaporview-set-alt-marker (evaporview-examples--ns 360)))

(defun evaporview-examples-uart ()
  (evaporview-examples--add "uart_tb.tx" :color 2)
  (evaporview-examples--add "uart_tb.busy" :color 6)
  (evaporview-examples--add "uart_tb.tx_data" :format 'ascii :color 4 :name "char")
  (evaporview-examples--add "uart_tb.tx_data" :format 'hex :color 4)
  (evaporview-examples--add "uart_tb.bit_idx" :format 'unsigned :color 5)
  (evaporview-set-marker (evaporview-examples--ns 200))
  (evaporview-set-alt-marker (evaporview-examples--ns 1160)))

(defun evaporview-examples-nco ()
  (evaporview-examples--add "nco_tb.phase" :format 'unsigned :style 'analog :color 7)
  (evaporview-examples--add "nco_tb.amp" :style 'analog :color 5)
  (evaporview-examples--add "nco_tb.sine" :style 'analog :color 1)
  (evaporview-examples--add "nco_tb.sample" :format 'signed :style 'analog :color 3)
  (evaporview-examples--add "nco_tb.sample" :format 'signed :color 3)
  (evaporview-examples--add "nco_tb.sample_f32" :format 'float :color 2)
  (evaporview-set-marker (evaporview-examples--ns 1000))
  (evaporview-set-alt-marker (evaporview-examples--ns 2000)))

(defun evaporview-examples-open (name)
  "Open example NAME (\"spi\", \"uart\" or \"nco\") with a fresh curated view."
  (let* ((file (expand-file-name (concat name ".vcd") evaporview-examples-dir))
         (evaporview-auto-load-session nil)
         (buf (evaporview-open file)))
    (with-current-buffer buf
      (setf (evaporview-view-items evaporview--view) nil)
      (evaporview-redraw)
      (funcall (intern (concat "evaporview-examples-" name)))
      (evaporview-zoom-fit)
      (setf (evaporview-view-undo evaporview--view) nil))
    buf))

(defun evaporview-examples-write-sessions ()
  "Write the session file of every example next to its dump."
  (dolist (name '("spi" "uart" "nco"))
    (with-current-buffer (evaporview-examples-open name)
      (let ((evaporview--width-override 120))
        (evaporview-zoom-fit)
        (evaporview-save-session (evaporview--default-session-file)))
      (kill-buffer))))

(provide 'evaporview-examples)
;;; sessions.el ends here

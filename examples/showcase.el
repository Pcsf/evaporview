;;; showcase.el --- Build the example gallery  -*- lexical-binding: t; -*-

;;; Commentary:

;; `make showcase' runs this in batch, then screenshots the pages it writes.
;; The Emacs view is captured as HTML with the colors its faces resolve to on
;; a dark background, so the picture shows the buffer as a frame draws it.

;;; Code:

(require 'evaporview)
(require 'evaporview-html)
(load (expand-file-name "sessions.el" (file-name-directory load-file-name)) nil t)

(defconst evaporview-showcase-out (expand-file-name "build/showcase/"))
(defconst evaporview-showcase-img (expand-file-name "img/" evaporview-examples-dir))

(defun evaporview-showcase--color (faces attr)
  (cl-some (lambda (f)
             (let ((c (and (facep f) (face-attribute f attr nil t))))
               (and (stringp c) c)))
           (if (listp faces) faces (list faces))))

(defun evaporview-showcase--escape (s)
  (replace-regexp-in-string
   "[&<>]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (_ "&gt;"))) s t t))

(defun evaporview-showcase-buffer-html (title)
  "The current buffer as a dark, monospace HTML page titled TITLE."
  (let ((pos (point-min)) (parts nil))
    (while (< pos (point-max))
      (let* ((next (next-single-property-change pos 'face nil (point-max)))
             (faces (get-text-property pos 'face))
             (fg (evaporview-showcase--color faces :foreground))
             (bg (evaporview-showcase--color faces :background))
             (text (evaporview-showcase--escape (buffer-substring-no-properties pos next))))
        (push (if (or fg bg)
                  (format "<span style=\"%s%s\">%s</span>"
                          (if fg (format "color:%s;" fg) "")
                          (if bg (format "background:%s;" bg) "")
                          text)
                text)
              parts)
        (setq pos next)))
    (concat "<!doctype html><meta charset=\"utf-8\"><title>" title "</title>"
            "<style>body{margin:0;background:#1e1e1e;color:#d4d4d4}"
            "pre{margin:0;padding:12px 14px;font:15px/1.2 \"JetBrains Mono\","
            "\"DejaVu Sans Mono\",monospace}</style><pre>"
            (apply #'concat (nreverse parts)) "</pre>")))

(defun evaporview-showcase--write (file text)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert text))))

(defun evaporview-showcase-build ()
  (setq frame-background-mode 'dark)
  (frame-set-background-mode (selected-frame))
  (make-directory evaporview-showcase-out t)
  (make-directory evaporview-showcase-img t)
  (dolist (name evaporview-examples)
    (with-current-buffer (evaporview-open (evaporview-examples-file name))
      (let ((evaporview--width-override 104))
        (evaporview-zoom-fit)
        (evaporview-showcase--write
         (expand-file-name (concat name "-emacs.html") evaporview-showcase-out)
         (evaporview-showcase-buffer-html name))
        (evaporview-html-write evaporview--view
                               (expand-file-name (concat name "-browser.html")
                                                 evaporview-showcase-out))
        (pcase name
          ("spi" (evaporview-wavedrom-export-svg
                  (expand-file-name "spi-wavedrom.svg" evaporview-showcase-img)
                  "spi_tb.sclk" 'rise))
          ("uart" (evaporview-wavedrom-export-svg
                   (expand-file-name "uart-wavedrom.svg" evaporview-showcase-img)))))
      (kill-buffer))))

(evaporview-showcase-build)

;;; showcase.el ends here

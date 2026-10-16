;;; evaporview-html.el --- Single-file browser viewer for evaporview  -*- lexical-binding: t; -*-

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

;; Writes the current view as one .html file: the parsed dump, the session,
;; and the viewer's stylesheet and script from assets/ are all inlined, so the
;; page opens from file:// with no network and can be mailed or copied as is.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'browse-url)
(require 'evaporview)

(defconst evaporview-html--assets
  (expand-file-name "assets" (file-name-directory
                              (or load-file-name buffer-file-name default-directory)))
  "Directory holding viewer.html, viewer.css and viewer.js.")

(defun evaporview-html--slurp (name)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents (expand-file-name name evaporview-html--assets)))
    (buffer-string)))

(defun evaporview-html--json-escape (json)
  "Make JSON safe inside a <script> element."
  (replace-regexp-in-string
   "<!--" "<\\\\u0021--"
   (replace-regexp-in-string "</" "<\\\\/" json t)
   t))

(defun evaporview-html--scope (scope index)
  (concat "{\"name\":" (json-encode-string (evaporview-scope-name scope))
          ",\"type\":" (json-encode-string (or (evaporview-scope-type scope) ""))
          ",\"vars\":["
          (mapconcat (lambda (v) (number-to-string (gethash v index)))
                     (evaporview-scope-vars scope) ",")
          "],\"children\":["
          (mapconcat (lambda (c) (evaporview-html--scope c index))
                     (evaporview-scope-children scope) ",")
          "]}"))

(defun evaporview-html-data (view)
  "The data block of VIEW's page, as a JSON string."
  (let* ((wave (evaporview-view-wave view))
         (vars (evaporview-wave-vars wave))
         (var-index (make-hash-table :test 'eq))
         (sig-index (make-hash-table :test 'eq))
         (sigs nil))
    (dotimes (i (length vars))
      (let ((sig (evaporview-wave-var-sig wave (aref vars i))))
        (puthash (aref vars i) i var-index)
        (unless (gethash sig sig-index)
          (puthash sig (hash-table-count sig-index) sig-index)
          (push sig sigs))))
    (concat
     "{\"file\":" (json-encode-string
                   (file-name-nondirectory (or (evaporview-view-file view) "")))
     ",\"timescale\":[" (number-to-string (car (evaporview-wave-timescale wave)))
     "," (json-encode-string (cdr (evaporview-wave-timescale wave))) "]"
     ",\"start\":" (number-to-string (evaporview-wave-start wave))
     ",\"end\":" (number-to-string (evaporview-wave-end wave))
     ",\"scope\":" (evaporview-html--scope (evaporview-wave-root wave) var-index)
     ",\"vars\":["
     (mapconcat
      (lambda (v)
        (concat "{\"name\":" (json-encode-string (evaporview-var-name v))
                ",\"range\":" (json-encode-string (or (evaporview-var-range v) ""))
                ",\"path\":" (json-encode-string (evaporview-var-path v))
                ",\"type\":" (json-encode-string (or (evaporview-var-type v) ""))
                ",\"width\":" (number-to-string (evaporview-var-width v))
                ",\"sig\":" (number-to-string
                             (gethash (evaporview-wave-var-sig wave v) sig-index))
                "}"))
      vars ",")
     "],\"sigs\":["
     (mapconcat
      (lambda (s)
        (concat "{\"width\":" (number-to-string (evaporview-sig-width s))
                ",\"real\":" (if (evaporview-sig-real-p s) "true" "false")
                (if (evaporview-sig-enum s)
                    (concat ",\"enum\":[" (mapconcat #'json-encode-string (evaporview-sig-enum s) ",") "]")
                  "")
                ",\"t\":[" (mapconcat #'number-to-string (evaporview-sig-times s) ",")
                "],\"v\":[" (mapconcat #'json-encode-string (evaporview-sig-values s) ",")
                "]}"))
      (nreverse sigs) ",")
     "],\"session\":"
     (let ((json-encoding-pretty-print nil))
       (json-encode (evaporview-session-data view)))
     "}")))

(defun evaporview-html-string (view)
  "VIEW as one self-contained HTML page."
  (let ((page (evaporview-html--slurp "viewer.html")))
    (dolist (sub `(("/*EVW:TITLE*/" . ,(concat "evaporview – "
                                               (file-name-nondirectory
                                                (or (evaporview-view-file view) ""))))
                   ("/*EVW:CSS*/" . ,(evaporview-html--slurp "viewer.css"))
                   ("/*EVW:DATA*/" . ,(evaporview-html--json-escape
                                       (evaporview-html-data view)))
                   ("/*EVW:JS*/" . ,(evaporview-html--json-escape
                                     (evaporview-html--slurp "viewer.js")))))
      (let ((at (cl-search (car sub) page)))
        (unless at (error "Viewer template lacks %s" (car sub)))
        (setq page (concat (substring page 0 at) (cdr sub)
                           (substring page (+ at (length (car sub))))))))
    page))

(defconst evaporview-html--external-patterns
  '("\\(?:src\\|href\\)[ \t]*=[ \t]*[\"']?[a-z]*:?//"
    "url([ \t]*[\"']?[a-z]*:?//"
    "@import"
    "\\_<fetch[ \t]*("
    "XMLHttpRequest"
    "https?://")
  "Patterns that mean a page would reach outside itself.")

(defun evaporview-html-external-refs (html)
  "Return the external references found in HTML, or nil."
  (let (hits)
    (dolist (re evaporview-html--external-patterns)
      (let ((start 0))
        (while (string-match re html start)
          (push (match-string 0 html) hits)
          (setq start (match-end 0)))))
    (nreverse hits)))

(defun evaporview-html-write (view file)
  "Write VIEW's page to FILE."
  (let ((html (evaporview-html-string view))
        (coding-system-for-write 'utf-8))
    (with-temp-file file (insert html))
    file))

;;;###autoload
(defun evaporview-browse (&optional file)
  "Open the current view in the browser as a self-contained page.
The page goes to the temporary directory, or to FILE when given
\(interactively, with a prefix argument)."
  (interactive (list (and current-prefix-arg
                          (read-file-name "Write page to: " nil nil nil
                                          (concat (file-name-nondirectory
                                                   (evaporview-view-file evaporview--view))
                                                  ".html")))))
  (let ((out (or file (expand-file-name
                       (format "evaporview-%s.html"
                               (file-name-nondirectory (evaporview-view-file evaporview--view)))
                       temporary-file-directory))))
    (evaporview-html-write evaporview--view out)
    (browse-url-of-file out)
    (message "Opened %s" out)))

(defun evaporview-html-export-file (dump out &optional paths)
  "Write a page for DUMP to OUT, showing PATHS; for batch use."
  (with-current-buffer (evaporview-open dump)
    (unwind-protect
        (progn
          (dolist (p paths) (evaporview-add-signal p))
          (evaporview-html-write evaporview--view out))
      (kill-buffer))))

(provide 'evaporview-html)
;;; evaporview-html.el ends here

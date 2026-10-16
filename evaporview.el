;;; evaporview.el --- VaporView-style waveform viewer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pcsf

;; Author: Pcsf <7274075+Pcsf@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: hardware, tools, vhdl, verilog
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

;; A waveform viewer for VCD dumps modelled on VaporView for VS Code, written
;; in Emacs Lisp only: no external program, no network, no compiled module.
;;
;;   M-x evaporview-open RET dump.vcd RET
;;
;; The waveform buffer draws signals as text, so it works in a terminal
;; session as well as in a graphical frame.  A netlist side window lists the
;; scopes; RET on a variable adds it.  `B' writes the same view as one
;; self-contained HTML file and opens it in the browser for mouse-driven use.
;;
;; Sessions (signal list, formats, colors, groups, markers) are saved as JSON
;; that the browser viewer reads and writes too.  See README.org for keys.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'evaporview-vcd)
(require 'evaporview-format)

(autoload 'evaporview-browse "evaporview-html" nil t)
(declare-function dired-get-filename "dired")

(defgroup evaporview nil
  "VaporView-style waveform viewer."
  :group 'tools
  :prefix "evaporview-")

(defcustom evaporview-value-width 12
  "Width of the value-at-marker column."
  :type 'integer)

(defcustom evaporview-name-max-width 32
  "Widest the signal name column may grow."
  :type 'integer)

(defcustom evaporview-row-spacing 1
  "Blank lines between signal rows.
Real lines rather than `line-spacing', so the gap shows on a terminal too."
  :type 'integer)

(defcustom evaporview-netlist-width 34
  "Width of the netlist side window."
  :type 'integer)

(defcustom evaporview-auto-load-session t
  "When non-nil, opening a dump loads its session file if one exists."
  :type 'boolean)

(defface evaporview-color-1 '((((background dark)) :foreground "#56b4e9")
                              (t :foreground "#0072b2"))
  "Waveform color 1.")
(defface evaporview-color-2 '((((background dark)) :foreground "#ff9a52")
                              (t :foreground "#d55e00"))
  "Waveform color 2.")
(defface evaporview-color-3 '((((background dark)) :foreground "#3fd1a0")
                              (t :foreground "#00875f"))
  "Waveform color 3.")
(defface evaporview-color-4 '((((background dark)) :foreground "#f08ac0")
                              (t :foreground "#b8408c"))
  "Waveform color 4.")
(defface evaporview-color-5 '((((background dark)) :foreground "#f0d34a")
                              (t :foreground "#8c6d00"))
  "Waveform color 5.")
(defface evaporview-color-6 '((((background dark)) :foreground "#7fb6ff")
                              (t :foreground "#1a8fd0"))
  "Waveform color 6.")
(defface evaporview-color-7 '((((background dark)) :foreground "#b88aff")
                              (t :foreground "#7b3fa0"))
  "Waveform color 7.")
(defface evaporview-color-8 '((((background dark)) :foreground "#a9b8c0")
                              (t :foreground "#5c6b73"))
  "Waveform color 8.")
(defface evaporview-x-face '((t :foreground "red"))
  "Unknown (x, u, w, -) values.")
(defface evaporview-z-face '((((background dark)) :foreground "gold")
                             (t :foreground "dark orange"))
  "High-impedance values.")
(defface evaporview-marker-face '((((background dark)) :background "#264f78")
                                  (t :background "#b5d5ff"))
  "Marker column.")
(defface evaporview-alt-marker-face '((((background dark)) :background "#5a3d1e")
                                      (t :background "#ffe0b3"))
  "Alt-marker column.")
(defface evaporview-ruler-face '((t :inherit shadow))
  "Time ruler.")
(defface evaporview-group-face '((t :inherit bold))
  "Group rows.")

;;;; Model

(cl-defstruct (evaporview-item (:constructor evaporview-item--make)
                               (:copier nil))
  kind path name format color style slice children expanded sig)

(cl-defstruct (evaporview-view (:constructor evaporview-view--make))
  wave file items marker alt-marker start tpc unit undo redo)

(defvar-local evaporview--view nil "The view shown in this buffer.")
(defvar-local evaporview--rows nil "Vector of (ITEM . DEPTH), one per row.")
(defvar-local evaporview--netlist-buffer nil)
(defvar-local evaporview--last-cols nil)
(defvar evaporview--width-override nil
  "When non-nil, the waveform width in columns (tests bind it).")

(defconst evaporview--header-lines 4)

(defun evaporview--var (path)
  (or (evaporview-wave-find-var (evaporview-view-wave evaporview--view) path)
      (user-error "No signal %s in this dump" path)))

(defun evaporview--item-sig (item)
  "The change list ITEM shows, slicing and caching as needed."
  (or (evaporview-item-sig item)
      (let* ((wave (evaporview-view-wave evaporview--view))
             (var (evaporview-wave-find-var wave (evaporview-item-path item)))
             (sig (and var (evaporview-wave-var-sig wave var)))
             (slice (evaporview-item-slice item)))
        (when (and sig slice)
          (setq sig (evaporview-sig-slice sig (car slice) (cdr slice))))
        (setf (evaporview-item-sig item) sig))))

(defun evaporview-item-label (item)
  "Name shown for ITEM."
  (or (evaporview-item-name item)
      (if (eq (evaporview-item-kind item) 'group) "group"
        (let* ((var (evaporview-wave-find-var (evaporview-view-wave evaporview--view)
                                              (evaporview-item-path item)))
               (slice (evaporview-item-slice item)))
          (cond ((null var) (evaporview-item-path item))
                ((null slice) (evaporview-var-display-name var))
                ((= (car slice) (cdr slice))
                 (format "%s[%d]" (evaporview-var-name var) (car slice)))
                (t (format "%s[%d:%d]" (evaporview-var-name var)
                           (car slice) (cdr slice))))))))

(defun evaporview--make-signal (path &optional slice fmt)
  (let* ((item (evaporview-item--make :kind 'signal :path path :slice slice
                                      :color 1 :style 'digital))
         (sig (evaporview--item-sig item)))
    (setf (evaporview-item-format item)
          (or (and fmt (> (evaporview-sig-width sig) 1) fmt)
              (evaporview-format-default sig)))
    item))

(defun evaporview--copy-items (items)
  (mapcar (lambda (it)
            (let ((c (evaporview-item--make
                      :kind (evaporview-item-kind it) :path (evaporview-item-path it)
                      :name (evaporview-item-name it)
                      :format (evaporview-item-format it)
                      :color (evaporview-item-color it)
                      :style (evaporview-item-style it)
                      :slice (evaporview-item-slice it)
                      :expanded (evaporview-item-expanded it)
                      :sig (evaporview-item-sig it))))
              (setf (evaporview-item-children c)
                    (evaporview--copy-items (evaporview-item-children it)))
              c))
          items))

(defun evaporview--signal-items (items)
  "All signal items in ITEMS, depth first."
  (cl-mapcan (lambda (it)
               (if (eq (evaporview-item-kind it) 'group)
                   (evaporview--signal-items (evaporview-item-children it))
                 (list it)))
             items))

;;;; Undo

(defun evaporview--checkpoint ()
  "Record the signal list before an edit."
  (push (evaporview--copy-items (evaporview-view-items evaporview--view))
        (evaporview-view-undo evaporview--view))
  (setf (evaporview-view-redo evaporview--view) nil))

(defun evaporview-undo ()
  "Undo the last edit of the signal list."
  (interactive)
  (let ((v evaporview--view))
    (unless (evaporview-view-undo v) (user-error "Nothing to undo"))
    (push (evaporview-view-items v) (evaporview-view-redo v))
    (setf (evaporview-view-items v) (pop (evaporview-view-undo v)))
    (evaporview-redraw)))

(defun evaporview-redo ()
  "Redo the last undone edit."
  (interactive)
  (let ((v evaporview--view))
    (unless (evaporview-view-redo v) (user-error "Nothing to redo"))
    (push (evaporview-view-items v) (evaporview-view-undo v))
    (setf (evaporview-view-items v) (pop (evaporview-view-redo v)))
    (evaporview-redraw)))

;;;; Row rendering

(defconst evaporview--blocks "▁▂▃▄▅▆▇█")

(defun evaporview--text-sig-p (sig)
  "Non-nil when SIG carries text values (real numbers, enum literals), not bits."
  (or (evaporview-sig-real-p sig) (evaporview-sig-enum sig)))

(defun evaporview--color-face (n)
  (intern (format "evaporview-color-%d" (or n 1))))

(defun evaporview--bit-char (c)
  (pcase c
    ((or ?1 ?h) "▔") ((or ?0 ?l) "▁") (?z "─") (_ "▒")))

(defun evaporview--bit-face (c face)
  (pcase c
    ((or ?1 ?h ?0 ?l) face) (?z 'evaporview-z-face) (_ 'evaporview-x-face)))

(defun evaporview--value-face (value face)
  (cond ((string-match-p "\\`z+\\'" value) 'evaporview-z-face)
        ((string-match-p "[^01]" value) 'evaporview-x-face)
        (t face)))

(defvar evaporview--range-cache (make-hash-table :test 'eq :weakness 'key))

(defun evaporview--analog-range (sig fmt)
  (let ((key (gethash sig evaporview--range-cache)))
    (if (and key (eq (car key) fmt)) (cdr key)
      (let (lo hi)
        (mapc (lambda (v)
                (let ((x (evaporview-value-number sig v fmt)))
                  (when (and x (= x x) (< (abs x) 1.0e+INF))
                    (setq lo (if lo (min lo x) x) hi (if hi (max hi x) x)))))
              (evaporview-sig-values sig))
        (puthash sig (cons fmt (cons lo hi)) evaporview--range-cache)
        (cons lo hi)))))

(defun evaporview--columns (sig t0 tpc ncols)
  "Per column of the view: (CHANGES BEFORE AFTER), values as strings.
CHANGES counts changes inside the column, the initial value excluded."
  (let* ((times (evaporview-sig-times sig))
         (values (evaporview-sig-values sig))
         (p (evaporview-sig--lower-bound sig t0))
         (out (make-vector ncols nil)))
    (dotimes (c ncols)
      (let* ((ce (+ t0 (* (1+ c) tpc)))
             (q (evaporview-sig--lower-bound sig ce))
             (n (- q (max p 1))))
        (aset out c (list (max n 0)
                          (and (> q 0) (aref values (1- (max p 1))))
                          (and (> q 0) (aref values (1- q)))))
        (setq p q)))
    (ignore times)
    out))

(defun evaporview-render-row (sig fmt style t0 tpc ncols &optional color)
  "Render SIG from time T0 at TPC ticks per column over NCOLS columns.
FMT formats bus labels, STYLE is `digital' or `analog', COLOR is 1..8."
  (let* ((face (evaporview--color-face color))
         (cols (evaporview--columns sig t0 tpc ncols))
         (chars (make-vector ncols ?\s))
         (faces (make-vector ncols nil)))
    (cond
     ((and (eq style 'analog)
           (or (evaporview--text-sig-p sig) (> (evaporview-sig-width sig) 1)))
      (let* ((range (evaporview--analog-range sig fmt))
             (lo (car range)) (hi (cdr range)))
        (dotimes (c ncols)
          (let* ((v (nth 2 (aref cols c)))
                 (x (evaporview-value-number sig v fmt)))
            (cond ((null v))
                  ((or (null x) (/= x x))
                   (aset chars c ?▒) (aset faces c 'evaporview-x-face))
                  (t (let ((lvl (if (or (null lo) (= hi lo)) 0
                                  (round (* 7 (/ (- x lo) (- hi lo)))))))
                       (aset chars c (aref evaporview--blocks (max 0 (min 7 lvl))))
                       (aset faces c face))))))))
     ((and (= (evaporview-sig-width sig) 1) (not (evaporview--text-sig-p sig)))
      (dotimes (c ncols)
        (pcase-let ((`(,n ,before ,after) (aref cols c)))
          (cond
           ((null after))
           ((or (= n 0) (null before))
            (aset chars c (aref (evaporview--bit-char (aref after 0)) 0))
            (aset faces c (evaporview--bit-face (aref after 0) face)))
           ;; Changes in a neighbouring column too: no level is visible between
           ;; them, so mark the run uniformly instead of counting edges.
           ((or (> n 1)
                (and (> c 0) (> (car (aref cols (1- c))) 0))
                (and (< (1+ c) ncols) (> (car (aref cols (1+ c))) 0)))
            (aset chars c ?┃) (aset faces c face))
           (t (aset chars c ?│) (aset faces c face))))))
     (t
      (let ((c 0))
        (while (< c ncols)
          (pcase-let ((`(,n ,before ,after) (aref cols c)))
            (cond
             ((null after) (setq c (1+ c)))
             ((and (> n 0) before)
              (aset chars c ?╳) (aset faces c face) (setq c (1+ c)))
             (t
              (let ((end (1+ c)))
                (while (and (< end ncols) (= 0 (car (aref cols end))))
                  (setq end (1+ end)))
                (let* ((len (- end c))
                       (label (evaporview-format-value sig after fmt))
                       (text (cond ((<= (length label) len)
                                    (concat label (make-string (- len (length label)) ?═)))
                                   ((= len 1) "…")
                                   (t (concat (substring label 0 (1- len)) "…"))))
                       (vf (if (evaporview--text-sig-p sig) face
                             (evaporview--value-face after face))))
                  (dotimes (i len)
                    (aset chars (+ c i) (aref text i))
                    (aset faces (+ c i) vf)))
                (setq c end)))))))))
    (let ((out (concat chars)))
      (dotimes (c ncols)
        (when (aref faces c)
          (put-text-property c (1+ c) 'face (aref faces c) out)))
      out)))

;;;; Layout

(defun evaporview--wave-cols ()
  (or evaporview--width-override
      (let ((win (get-buffer-window (current-buffer))))
        (max 10 (- (if win (window-body-width win) 100)
                   (evaporview--wave-offset) 1)))))

(defun evaporview--visible-rows ()
  (let (rows)
    (cl-labels ((walk (items depth)
                  (dolist (it items)
                    (push (cons it depth) rows)
                    (when (and (eq (evaporview-item-kind it) 'group)
                               (evaporview-item-expanded it))
                      (walk (evaporview-item-children it) (1+ depth))))))
      (walk (evaporview-view-items evaporview--view) 0))
    (vconcat (nreverse rows))))

(defun evaporview--name-width ()
  (let ((w 8))
    (mapc (lambda (row)
            (setq w (max w (+ 1 (* 2 (cdr row))
                              (if (eq (evaporview-item-kind (car row)) 'group) 2 0)
                              (string-width (evaporview-item-label (car row)))))))
          (evaporview--visible-rows))
    (min w evaporview-name-max-width)))

(defun evaporview--wave-offset ()
  (+ (evaporview--name-width) 3 evaporview-value-width 3))

(defun evaporview--time-column (time)
  "Column, from the start of a row, where TIME is drawn; nil if off view."
  (let* ((v evaporview--view)
         (c (floor (/ (- time (evaporview-view-start v)) (float (evaporview-view-tpc v))))))
    (when (and (>= c 0) (< c (evaporview--wave-cols)))
      (+ (evaporview--wave-offset) c))))

(defun evaporview--fit (s w)
  "S truncated or padded to width W."
  (if (> (string-width s) w)
      (concat (truncate-string-to-width s (1- w)) "…")
    (concat s (make-string (- w (string-width s)) ?\s))))

(defun evaporview--fmt-time (ticks)
  (let ((v evaporview--view))
    (evaporview-format-time ticks (evaporview-wave-timescale (evaporview-view-wave v))
                            (evaporview-view-unit v))))

(defun evaporview--nice-step (min-ticks)
  (let ((p (expt 10 (max 0 (floor (log (max 1 min-ticks) 10)))))
        (step nil))
    (dolist (m '(1 2 5 10) step)
      (when (and (null step) (>= (* m p) min-ticks)) (setq step (* m p))))))

(defun evaporview--ruler (ncols)
  "Label line and tick line of the time ruler."
  (let* ((v evaporview--view) (t0 (evaporview-view-start v))
         (tpc (evaporview-view-tpc v))
         (step (evaporview--nice-step (* 12 tpc)))
         (labels (make-vector ncols ?\s))
         (ticks (make-vector ncols ?─))
         (tick (* step (ceiling (/ t0 (float step)))))
         (free 0))
    (while (< tick (+ t0 (* ncols tpc)))
      (let ((c (floor (/ (- tick t0) (float tpc)))))
        (when (< c ncols)
          (aset ticks c ?┬)
          (let ((s (evaporview--fmt-time tick)))
            (when (and (>= c free) (<= (+ c (length s)) ncols))
              (dotimes (i (length s)) (aset labels (+ c i) (aref s i)))
              (setq free (+ c (length s) 1))))))
      (setq tick (+ tick step)))
    (cons (concat labels) (concat ticks))))

(defun evaporview--status ()
  (let* ((v evaporview--view)
         (m (evaporview-view-marker v)) (a (evaporview-view-alt-marker v))
         (wave (evaporview-view-wave v))
         (cols (evaporview--wave-cols))
         (end (+ (evaporview-view-start v) (* cols (evaporview-view-tpc v)))))
    (concat " " (file-name-nondirectory (or (evaporview-view-file v) ""))
            "  │ M " (if m (evaporview--fmt-time m) "-")
            "  │ A " (if a (evaporview--fmt-time a) "-")
            (if (and m a)
                (format "  │ Δ %s (%s)" (evaporview--fmt-time (abs (- m a)))
                        (evaporview-format-frequency (- m a) (evaporview-wave-timescale wave)))
              "")
            "  │ " (evaporview--fmt-time (round (evaporview-view-start v)))
            " – " (evaporview--fmt-time (round end)))))

(defun evaporview--mark-column (time face)
  "Give the column of TIME FACE on every line of the waveform area."
  (let ((col (and time (evaporview--time-column time))))
    (when col
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (while (not (eobp))
          (let ((pos (+ (line-beginning-position) col)))
            (when (< pos (line-end-position))
              (add-face-text-property pos (1+ pos) face)))
          (forward-line 1))))))

(defun evaporview-redraw ()
  "Render the view into the current buffer, keeping point on its row."
  (interactive)
  (let* ((inhibit-read-only t)
         (v evaporview--view)
         (row (evaporview--row-index))
         (col (current-column))
         (rows (evaporview--visible-rows))
         (namew (evaporview--name-width))
         (ncols (evaporview--wave-cols))
         (ruler (evaporview--ruler ncols))
         (lead (make-string (+ namew 3 evaporview-value-width 3) ?\s))
         (spacer (concat (make-string namew ?\s) " │ "
                         (make-string evaporview-value-width ?\s) " │ "
                         (make-string ncols ?\s) "\n"))
         (first t))
    (setq evaporview--rows rows evaporview--last-cols ncols)
    (erase-buffer)
    (insert (propertize (evaporview--status) 'face 'mode-line-inactive) "\n")
    (insert lead (propertize (car ruler) 'face 'evaporview-ruler-face) "\n")
    (insert lead (propertize (cdr ruler) 'face 'evaporview-ruler-face) "\n")
    (insert (make-string namew ?─) "─┼─" (make-string evaporview-value-width ?─)
            "─┼─" (make-string ncols ?─) "\n")
    (mapc
     (lambda (r)
       (let* ((it (car r)) (depth (cdr r))
              (group (eq (evaporview-item-kind it) 'group))
              (label (concat (make-string (1+ (* 2 depth)) ?\s)
                             (if group (if (evaporview-item-expanded it) "▾ " "▸ ") "")
                             (evaporview-item-label it)))
              (sig (and (not group) (evaporview--item-sig it)))
              (m (evaporview-view-marker v))
              (value (if (and sig m)
                         (evaporview-format-value sig (evaporview-sig-value-at sig m)
                                                  (evaporview-item-format it))
                       "")))
         (unless first
           (dotimes (_ (max 0 evaporview-row-spacing)) (insert spacer)))
         (setq first nil)
         (insert (propertize (evaporview--fit label namew)
                             'face (if group 'evaporview-group-face
                                     (evaporview--color-face (evaporview-item-color it))))
                 " │ " (evaporview--fit value evaporview-value-width) " │ "
                 (if sig
                     (evaporview-render-row sig (evaporview-item-format it)
                                            (evaporview-item-style it)
                                            (evaporview-view-start v)
                                            (evaporview-view-tpc v) ncols
                                            (evaporview-item-color it))
                   "")
                 "\n")))
     rows)
    (evaporview--mark-column (evaporview-view-alt-marker v) 'evaporview-alt-marker-face)
    (evaporview--mark-column (evaporview-view-marker v) 'evaporview-marker-face)
    (if row (evaporview-goto-item (min row (max 0 (1- (length rows)))))
      (goto-char (point-min)))
    (move-to-column col)))

;;;; Point, selection

(defun evaporview--row-stride ()
  (1+ (max 0 evaporview-row-spacing)))

(defun evaporview--row-index ()
  "Index into the rows of the line at point, or nil on the header.
A spacer line belongs to the row above it."
  (let ((l (- (line-number-at-pos) evaporview--header-lines 1)))
    (and evaporview--rows (>= l 0)
         (let ((i (/ l (evaporview--row-stride))))
           (and (< i (length evaporview--rows)) i)))))

(defun evaporview--current ()
  (let ((i (evaporview--row-index)))
    (and i (car (aref evaporview--rows i)))))

(defun evaporview--current-or-error ()
  (or (evaporview--current) (user-error "No signal on this line")))

(defun evaporview--selected-sig ()
  (let ((it (evaporview--current)))
    (and it (eq (evaporview-item-kind it) 'signal) (evaporview--item-sig it))))

(defun evaporview-goto-item (n)
  "Move point to row N (0-based) of the signal list."
  (goto-char (point-min))
  (forward-line (+ evaporview--header-lines (* n (evaporview--row-stride)))))

(defun evaporview-next-row (&optional n)
  "Move point N rows down, skipping the spacer lines."
  (interactive "p")
  (let ((col (current-column)) (i (evaporview--row-index)))
    (if (and i evaporview--rows)
        (evaporview-goto-item (max 0 (min (+ i n) (1- (length evaporview--rows)))))
      (forward-line n))
    (move-to-column col)))

(defun evaporview-previous-row (&optional n)
  "Move point N rows up, skipping the spacer lines."
  (interactive "p")
  (evaporview-next-row (- n)))

(defun evaporview--goto (item)
  (let ((i (cl-position item evaporview--rows :key #'car)))
    (when i (evaporview-goto-item i))))

(defun evaporview--siblings (item)
  "(PARENT . LIST) where LIST holds ITEM; PARENT nil means top level."
  (let ((found nil))
    (cl-labels ((walk (parent items)
                  (dolist (it items)
                    (cond ((eq it item) (setq found (cons parent items)))
                          ((eq (evaporview-item-kind it) 'group)
                           (walk it (evaporview-item-children it)))))))
      (walk nil (evaporview-view-items evaporview--view)))
    found))

(defun evaporview--set-siblings (parent list)
  (if parent (setf (evaporview-item-children parent) list)
    (setf (evaporview-view-items evaporview--view) list)))

;;;; Signal list editing

(defun evaporview--read-path ()
  (completing-read "Add signal: "
                   (mapcar #'evaporview-var-path
                           (evaporview-wave-vars (evaporview-view-wave evaporview--view)))
                   nil t))

(defun evaporview-add-signal (path)
  "Add the variable PATH after the current row."
  (interactive (list (evaporview--read-path)))
  (evaporview--var path)
  (evaporview--checkpoint)
  (let* ((item (evaporview--make-signal path))
         (cur (evaporview--current))
         (sib (and cur (evaporview--siblings cur))))
    (if (and sib (not (eq (evaporview-item-kind cur) 'group)))
        (let ((pos (1+ (cl-position cur (cdr sib)))))
          (evaporview--set-siblings (car sib)
                                    (append (cl-subseq (cdr sib) 0 pos) (list item)
                                            (nthcdr pos (cdr sib)))))
      (setf (evaporview-view-items evaporview--view)
            (append (evaporview-view-items evaporview--view) (list item))))
    (evaporview-redraw)
    (evaporview--goto item)
    item))

(defun evaporview-remove-item ()
  "Remove the signal or group on this line."
  (interactive)
  (let* ((it (evaporview--current-or-error))
         (sib (evaporview--siblings it)))
    (evaporview--checkpoint)
    (evaporview--set-siblings (car sib) (delq it (copy-sequence (cdr sib))))
    (evaporview-redraw)))

(defun evaporview--move (delta)
  (let* ((it (evaporview--current-or-error))
         (sib (evaporview--siblings it))
         (list (copy-sequence (cdr sib)))
         (i (cl-position it list))
         (j (+ i delta)))
    (when (and (>= j 0) (< j (length list)))
      (evaporview--checkpoint)
      (setf (nth i list) (nth j list) (nth j list) it)
      (evaporview--set-siblings (car sib) list)
      (evaporview-redraw)
      (evaporview--goto it))))

(defun evaporview-move-item-up ()
  "Move the current row up among its siblings."
  (interactive)
  (evaporview--move -1))

(defun evaporview-move-item-down ()
  "Move the current row down among its siblings."
  (interactive)
  (evaporview--move 1))

(defun evaporview--edit (fn)
  "Apply FN to the current item as one undoable edit."
  (let ((it (evaporview--current-or-error)))
    (evaporview--checkpoint)
    (funcall fn it)
    (evaporview-redraw)))

(defun evaporview-set-color (n)
  "Draw the current signal in color N (1-8); the digit keys call this."
  (interactive (list (if (and (characterp last-command-event)
                              (<= ?1 last-command-event ?8))
                         (- last-command-event ?0)
                       (read-number "Color (1-8): "))))
  (evaporview--edit (lambda (it) (setf (evaporview-item-color it) (max 1 (min 8 n))))))

(defun evaporview-set-format (fmt)
  "Show the current signal's values in format FMT."
  (interactive (list (intern (completing-read "Format: "
                                              (mapcar #'symbol-name evaporview-formats)
                                              nil t))))
  (evaporview--edit (lambda (it) (setf (evaporview-item-format it) fmt))))

(defun evaporview-rename (name)
  "Rename the current row to NAME; an empty name restores the default."
  (interactive (list (read-string "Name: " (evaporview-item-label
                                            (evaporview--current-or-error)))))
  (evaporview--edit (lambda (it) (setf (evaporview-item-name it)
                                       (if (string-empty-p name) nil name)))))

(defun evaporview-toggle-analog ()
  "Toggle the current signal between digital and analog drawing."
  (interactive)
  (evaporview--edit (lambda (it) (setf (evaporview-item-style it)
                                       (if (eq (evaporview-item-style it) 'analog)
                                           'digital 'analog)))))

(defun evaporview--insert-after (item new)
  (let* ((sib (evaporview--siblings item))
         (pos (1+ (cl-position item (cdr sib)))))
    (evaporview--set-siblings (car sib) (append (cl-subseq (cdr sib) 0 pos) new
                                                (nthcdr pos (cdr sib))))))

(defun evaporview--base-bits (item)
  "(MSB . LSB) of ITEM within its variable."
  (or (evaporview-item-slice item)
      (cons (1- (evaporview-sig-width (evaporview--item-sig item))) 0)))

(defun evaporview-slice-signal (msb lsb)
  "Add a signal made of bits MSB..LSB of the current signal."
  (interactive
   (let ((s (read-string "Bit range (msb:lsb): ")))
     (if (string-match "\\`\\s-*\\([0-9]+\\)\\s-*\\(?::\\s-*\\([0-9]+\\)\\)?\\s-*\\'" s)
         (let ((m (string-to-number (match-string 1 s))))
           (list m (if (match-string 2 s) (string-to-number (match-string 2 s)) m)))
       (user-error "Expected msb:lsb"))))
  (let* ((it (evaporview--current-or-error))
         (base (evaporview--base-bits it)))
    (unless (and (>= msb lsb) (<= (+ (cdr base) msb) (car base)))
      (user-error "Range outside %d:0" (- (car base) (cdr base))))
    (evaporview--checkpoint)
    (evaporview--insert-after
     it (list (evaporview--make-signal (evaporview-item-path it)
                                       (cons (+ (cdr base) msb) (+ (cdr base) lsb))
                                       (evaporview-item-format it))))
    (evaporview-redraw)))

(defun evaporview-split-signal (n)
  "Add one signal per N bits of the current signal (1 = bits, 4 = nibbles...)."
  (interactive (list (read-number "Bits per signal (1, 4, 8, 16...): " 1)))
  (let* ((it (evaporview--current-or-error))
         (base (evaporview--base-bits it))
         (new nil)
         (msb (car base)))
    (while (>= msb (cdr base))
      (let ((lsb (max (cdr base) (- msb n -1))))
        (push (evaporview--make-signal (evaporview-item-path it) (cons msb lsb)
                                       (evaporview-item-format it))
              new)
        (setq msb (1- lsb))))
    (evaporview--checkpoint)
    (evaporview--insert-after it (nreverse new))
    (evaporview-redraw)))

(defun evaporview-group-items (name from to)
  "Group rows FROM..TO (0-based, same level) under a new group NAME.
Interactively, the rows of the region, or the current row."
  (interactive
   (let* ((a (if (use-region-p)
                 (save-excursion (goto-char (region-beginning)) (evaporview--row-index))
               (evaporview--row-index)))
          (b (if (use-region-p)
                 (save-excursion (goto-char (1- (region-end))) (evaporview--row-index))
               a)))
     (unless (and a b) (user-error "Select signal rows"))
     (list (read-string "Group name: " "group") a b)))
  (let* ((items (mapcar (lambda (i) (car (aref evaporview--rows i)))
                        (number-sequence from to)))
         (sib (evaporview--siblings (car items)))
         (list (cdr sib))
         (pos (cl-position (car items) list)))
    (unless (cl-every (lambda (it) (memq it list)) items)
      (user-error "Rows to group must share one level"))
    (evaporview--checkpoint)
    (let ((group (evaporview-item--make :kind 'group :name name :expanded t
                                        :children items)))
      (evaporview--set-siblings
       (car sib) (append (cl-subseq list 0 pos) (list group)
                         (cl-remove-if (lambda (it) (memq it items))
                                       (nthcdr pos list))))
      (evaporview-redraw)
      (evaporview--goto group))))

(defun evaporview-toggle-group ()
  "Expand or collapse the group on this line."
  (interactive)
  (let ((it (evaporview--current-or-error)))
    (when (eq (evaporview-item-kind it) 'group)
      (setf (evaporview-item-expanded it) (not (evaporview-item-expanded it)))
      (evaporview-redraw)
      (evaporview--goto it))))

;;;; Markers and navigation

(defun evaporview--ensure-visible (time)
  (let* ((v evaporview--view) (span (* (evaporview--wave-cols) (evaporview-view-tpc v))))
    (unless (and (>= time (evaporview-view-start v))
                 (< time (+ (evaporview-view-start v) span)))
      (setf (evaporview-view-start v)
            (max (evaporview-wave-start (evaporview-view-wave v)) (- time (/ span 2.0)))))))

(defun evaporview--read-time (prompt)
  (let* ((v evaporview--view)
         (s (read-string (format "%s (%s): " prompt (evaporview-view-unit v)))))
    (unless (string-match "\\`\\s-*\\([-+0-9.eE]+\\)\\s-*\\([a-z]*\\)\\s-*\\'" s)
      (user-error "Not a time: %s" s))
    (evaporview-ticks (string-to-number (match-string 1 s))
                      (if (string-empty-p (match-string 2 s)) (evaporview-view-unit v)
                        (match-string 2 s))
                      (evaporview-wave-timescale (evaporview-view-wave v)))))

(defun evaporview-set-marker (time)
  "Place the marker at TIME (ticks)."
  (interactive (list (evaporview--read-time "Marker at")))
  (setf (evaporview-view-marker evaporview--view) time)
  (evaporview--ensure-visible time)
  (evaporview-redraw))

(defun evaporview-set-alt-marker (time)
  "Place the alt marker at TIME (ticks)."
  (interactive (list (evaporview--read-time "Alt marker at")))
  (setf (evaporview-view-alt-marker evaporview--view) time)
  (evaporview--ensure-visible time)
  (evaporview-redraw))

(defun evaporview-swap-markers ()
  "Exchange marker and alt marker."
  (interactive)
  (let ((v evaporview--view))
    (cl-rotatef (evaporview-view-marker v) (evaporview-view-alt-marker v))
    (evaporview-redraw)))

(defun evaporview--unit-ticks ()
  (let ((v evaporview--view))
    (max 1 (evaporview-ticks 1 (evaporview-view-unit v)
                             (evaporview-wave-timescale (evaporview-view-wave v))))))

(defun evaporview--step (alt finder)
  "Move the marker (or ALT marker) to (FINDER SIG TIME) of the selected signal."
  (let* ((v evaporview--view)
         (from (or (if alt (evaporview-view-alt-marker v) (evaporview-view-marker v))
                   (evaporview-view-start v)))
         (sig (evaporview--selected-sig))
         (to (if sig (funcall finder sig from)
               (funcall finder nil from))))
    (if (null to) (message "No more changes")
      (if alt (evaporview-set-alt-marker to) (evaporview-set-marker to)))))

(defun evaporview-next-edge ()
  "Marker to the next change of the selected signal (one time unit without one)."
  (interactive)
  (evaporview--step nil (lambda (s tm) (if s (evaporview-sig-next-change s tm)
                                         (+ tm (evaporview--unit-ticks))))))

(defun evaporview-prev-edge ()
  "Marker to the previous change of the selected signal."
  (interactive)
  (evaporview--step nil (lambda (s tm) (if s (evaporview-sig-prev-change s tm)
                                         (max 0 (- tm (evaporview--unit-ticks)))))))

(defun evaporview-alt-next-edge ()
  "Alt marker to the next change of the selected signal."
  (interactive)
  (evaporview--step t (lambda (s tm) (if s (evaporview-sig-next-change s tm)
                                       (+ tm (evaporview--unit-ticks))))))

(defun evaporview-alt-prev-edge ()
  "Alt marker to the previous change of the selected signal."
  (interactive)
  (evaporview--step t (lambda (s tm) (if s (evaporview-sig-prev-change s tm)
                                       (max 0 (- tm (evaporview--unit-ticks)))))))

(defun evaporview--edge-step (dir kind)
  (let ((sig (evaporview--selected-sig)))
    (unless (and sig (= (evaporview-sig-width sig) 1))
      (user-error "Select a 1-bit signal"))
    (evaporview--step nil (lambda (s tm) (if (eq dir 'next)
                                             (evaporview-sig-next-edge s tm kind)
                                           (evaporview-sig-prev-edge s tm kind))))))

(defun evaporview-next-rising-edge ()
  "Marker to the next rising edge of the selected 1-bit signal."
  (interactive)
  (evaporview--edge-step 'next 'rise))

(defun evaporview-prev-rising-edge ()
  "Marker to the previous rising edge of the selected 1-bit signal."
  (interactive)
  (evaporview--edge-step 'prev 'rise))

(defun evaporview-next-falling-edge ()
  "Marker to the next falling edge of the selected 1-bit signal."
  (interactive)
  (evaporview--edge-step 'next 'fall))

(defun evaporview-prev-falling-edge ()
  "Marker to the previous falling edge of the selected 1-bit signal."
  (interactive)
  (evaporview--edge-step 'prev 'fall))

(defun evaporview--normalize (s)
  (let ((d (downcase (string-trim s))))
    (if (string-match "\\`0+\\(.\\)" d) (substring d (match-beginning 1)) d)))

(defun evaporview--search (direction)
  (let* ((it (evaporview--current-or-error))
         (sig (or (evaporview--selected-sig) (user-error "Not a signal")))
         (want (evaporview--normalize (read-string "Find value: ")))
         (fmt (evaporview-item-format it))
         (from (or (evaporview-view-marker evaporview--view)
                   (evaporview-view-start evaporview--view)))
         (hit (evaporview-sig-search
               sig from direction
               (lambda (v) (equal want (evaporview--normalize
                                        (evaporview-format-value sig v fmt)))))))
    (if hit (evaporview-set-marker hit) (message "Value not found"))))

(defun evaporview-search-forward ()
  "Move the marker to the next time the selected signal takes a value."
  (interactive)
  (evaporview--search 'forward))

(defun evaporview-search-backward ()
  "Move the marker to the previous time the selected signal takes a value."
  (interactive)
  (evaporview--search 'backward))

;;;; Zoom and scroll

(defun evaporview--zoom (factor)
  (let* ((v evaporview--view)
         (span (* (evaporview--wave-cols) (evaporview-view-tpc v)))
         (m (evaporview-view-marker v))
         (anchor (if (and m (>= m (evaporview-view-start v))
                          (< m (+ (evaporview-view-start v) span)))
                     m (+ (evaporview-view-start v) (/ span 2.0))))
         (tpc (max 0.05 (* (evaporview-view-tpc v) factor))))
    (setf (evaporview-view-start v)
          (max (evaporview-wave-start (evaporview-view-wave v))
               (- anchor (* (- anchor (evaporview-view-start v))
                            (/ tpc (evaporview-view-tpc v)))))
          (evaporview-view-tpc v) tpc)
    (evaporview-redraw)))

(defun evaporview-zoom-in ()
  "Zoom in by two around the marker."
  (interactive)
  (evaporview--zoom 0.5))

(defun evaporview-zoom-out ()
  "Zoom out by two around the marker."
  (interactive)
  (evaporview--zoom 2.0))

(defun evaporview-zoom-fit ()
  "Show the whole dump."
  (interactive)
  (let* ((v evaporview--view) (w (evaporview-view-wave v))
         (span (max 1 (- (evaporview-wave-end w) (evaporview-wave-start w)))))
    (setf (evaporview-view-start v) (evaporview-wave-start w)
          (evaporview-view-tpc v) (/ (float span) (max 1 (1- (evaporview--wave-cols)))))
    (evaporview-redraw)))

(defun evaporview-zoom-markers ()
  "Zoom to the span between marker and alt marker."
  (interactive)
  (let* ((v evaporview--view) (m (evaporview-view-marker v)) (a (evaporview-view-alt-marker v)))
    (unless (and m a (/= m a)) (user-error "Place both markers first"))
    (setf (evaporview-view-start v) (min m a)
          (evaporview-view-tpc v) (/ (float (abs (- m a)))
                                     (max 1 (1- (evaporview--wave-cols)))))
    (evaporview-redraw)))

(defun evaporview--scroll (cols)
  (let ((v evaporview--view))
    (setf (evaporview-view-start v)
          (max (evaporview-wave-start (evaporview-view-wave v))
               (+ (evaporview-view-start v) (* cols (evaporview-view-tpc v)))))
    (evaporview-redraw)))

(defun evaporview-scroll-left ()
  "Scroll the view half a screen earlier."
  (interactive)
  (evaporview--scroll (- (/ (evaporview--wave-cols) 2))))

(defun evaporview-scroll-right ()
  "Scroll the view half a screen later."
  (interactive)
  (evaporview--scroll (/ (evaporview--wave-cols) 2)))

(defun evaporview-goto-start ()
  "Scroll to the start of the dump."
  (interactive)
  (setf (evaporview-view-start evaporview--view)
        (evaporview-wave-start (evaporview-view-wave evaporview--view)))
  (evaporview-redraw))

(defun evaporview-goto-end ()
  "Scroll to the end of the dump."
  (interactive)
  (let ((v evaporview--view))
    (setf (evaporview-view-start v)
          (max (evaporview-wave-start (evaporview-view-wave v))
               (- (evaporview-wave-end (evaporview-view-wave v))
                  (* (1- (evaporview--wave-cols)) (evaporview-view-tpc v)))))
    (evaporview-redraw)))

(defun evaporview-set-time-unit (unit)
  "Show times in UNIT."
  (interactive (list (completing-read "Time unit: " (mapcar #'car evaporview-time-units)
                                      nil t)))
  (setf (evaporview-view-unit evaporview--view) unit)
  (evaporview-redraw))

;;;; Mouse

(defun evaporview--event-time (event)
  "Time under mouse EVENT, snapped to a change of the row within one column."
  (let* ((posn (event-start event))
         (pos (posn-point posn)))
    (when pos
      (save-excursion
        (goto-char pos)
        (let* ((col (- pos (line-beginning-position) (evaporview--wave-offset)))
               (v evaporview--view) (tpc (evaporview-view-tpc v))
               (tm (round (+ (evaporview-view-start v) (* (+ col 0.5) tpc))))
               (sig (evaporview--selected-sig)))
          (when (>= col 0)
            (or (and sig
                     (let ((n (evaporview-sig-next-change sig (- tm tpc 1))))
                       (and n (<= (abs (- n tm)) tpc) n)))
                tm)))))))

(defun evaporview-mouse-set-marker (event)
  "Select the clicked row and place the marker at the click."
  (interactive "e")
  (mouse-set-point event)
  (let ((tm (evaporview--event-time event)))
    (when tm (evaporview-set-marker tm))))

(defun evaporview-mouse-set-alt-marker (event)
  "Place the alt marker at the click."
  (interactive "e")
  (let ((tm (save-excursion (mouse-set-point event) (evaporview--event-time event))))
    (when tm (evaporview-set-alt-marker tm))))

(defun evaporview-mouse-zoom-in (_event)
  "Zoom in from the mouse wheel."
  (interactive "e")
  (evaporview-zoom-in))

(defun evaporview-mouse-zoom-out (_event)
  "Zoom out from the mouse wheel."
  (interactive "e")
  (evaporview-zoom-out))

(defun evaporview-mouse-scroll-left (_event)
  "Scroll earlier from the mouse wheel."
  (interactive "e")
  (evaporview--scroll (- (max 1 (/ (evaporview--wave-cols) 8)))))

(defun evaporview-mouse-scroll-right (_event)
  "Scroll later from the mouse wheel."
  (interactive "e")
  (evaporview--scroll (max 1 (/ (evaporview--wave-cols) 8))))

;;;; Sessions

(defun evaporview--item-json (it)
  (if (eq (evaporview-item-kind it) 'group)
      `((type . "group") (name . ,(evaporview-item-name it))
        (expanded . ,(if (evaporview-item-expanded it) t :json-false))
        (items . ,(vconcat (mapcar #'evaporview--item-json (evaporview-item-children it)))))
    `((type . "signal") (path . ,(evaporview-item-path it))
      (name . ,(evaporview-item-name it))
      (format . ,(symbol-name (evaporview-item-format it)))
      (color . ,(evaporview-item-color it))
      (style . ,(symbol-name (evaporview-item-style it)))
      (slice . ,(let ((s (evaporview-item-slice it))) (and s (vector (car s) (cdr s))))))))

(defun evaporview-session-data (view)
  "VIEW's session as an alist ready for `json-encode'."
  (let ((cols (if (eq evaporview--view view) (evaporview--wave-cols) 100)))
    `((version . 1)
      (file . ,(file-name-nondirectory (or (evaporview-view-file view) "")))
      (marker . ,(evaporview-view-marker view))
      (altMarker . ,(evaporview-view-alt-marker view))
      (viewStart . ,(round (evaporview-view-start view)))
      (viewEnd . ,(round (+ (evaporview-view-start view) (* cols (evaporview-view-tpc view)))))
      (timeUnit . ,(evaporview-view-unit view))
      (items . ,(vconcat (mapcar #'evaporview--item-json (evaporview-view-items view)))))))

(defun evaporview-session-json (view)
  "VIEW's session as a JSON string."
  (let ((json-encoding-pretty-print t))
    (json-encode (evaporview-session-data view))))

(defun evaporview--default-session-file ()
  (concat (evaporview-view-file evaporview--view) ".evaporview.json"))

(defun evaporview-save-session (file)
  "Write the session to FILE (next to the dump by default)."
  (interactive (list (if current-prefix-arg
                         (read-file-name "Save session to: ")
                       (evaporview--default-session-file))))
  (let ((json (evaporview-session-json evaporview--view)))
    (with-temp-file file (insert json "\n")))
  (message "Session saved to %s" file))

(defun evaporview--item-from-json (j)
  (let ((get (lambda (k) (let ((v (cdr (assq k j)))) (if (eq v :json-false) nil v)))))
    (if (equal (funcall get 'type) "group")
        (evaporview-item--make
         :kind 'group :name (funcall get 'name) :expanded (funcall get 'expanded)
         :children (delq nil (mapcar #'evaporview--item-from-json (funcall get 'items))))
      (let* ((path (funcall get 'path))
             (slice (funcall get 'slice))
             (fmt (and (funcall get 'format) (intern (funcall get 'format)))))
        (if (not (evaporview-wave-find-var (evaporview-view-wave evaporview--view) path))
            (progn (message "Session: %s is not in this dump" path) nil)
          (let ((it (evaporview--make-signal path (and slice (let ((s (append slice nil))) (cons (car s) (cadr s)))))))
            (when (memq fmt evaporview-formats) (setf (evaporview-item-format it) fmt))
            (setf (evaporview-item-name it) (funcall get 'name)
                  (evaporview-item-color it) (or (funcall get 'color) 1)
                  (evaporview-item-style it) (if (equal (funcall get 'style) "analog")
                                                 'analog 'digital))
            it))))))

(defun evaporview-apply-session (data)
  "Load session alist DATA (as read by `json-read') into this view."
  (let ((v evaporview--view)
        (get (lambda (k) (cdr (assq k data)))))
    (evaporview--checkpoint)
    (setf (evaporview-view-items v)
          (delq nil (mapcar #'evaporview--item-from-json (funcall get 'items)))
          (evaporview-view-marker v) (funcall get 'marker)
          (evaporview-view-alt-marker v) (funcall get 'altMarker))
    (when (assoc (funcall get 'timeUnit) evaporview-time-units)
      (setf (evaporview-view-unit v) (funcall get 'timeUnit)))
    (let ((s (funcall get 'viewStart)) (e (funcall get 'viewEnd)))
      (when (and (numberp s) (numberp e) (> e s))
        (setf (evaporview-view-start v) s
              (evaporview-view-tpc v) (/ (float (- e s)) (evaporview--wave-cols)))))
    (evaporview-redraw)))

(defun evaporview-load-session (file)
  "Load the session in FILE."
  (interactive (list (read-file-name "Load session: " nil nil t
                                     (and evaporview--view
                                          (evaporview--default-session-file)))))
  (evaporview-apply-session
   (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol))
     (json-read-file file))))

;;;; WaveDrom

(defconst evaporview-wavedrom-max-events 32)

(defun evaporview--wd-char (value)
  (cond ((null value) "x")
        ((string-match-p "\\`z+\\'" value) "z")
        ((string-match-p "[^01hl]" value) "x")))

(defun evaporview-wavedrom-string (&optional clock-path edge)
  "WaveJSON for the shown signals between marker and alt marker.
Without CLOCK-PATH every change is an event; with it, each EDGE (`rise'
or `fall') of that 1-bit signal is one, sampled at the edge."
  (let* ((v evaporview--view)
         (m (evaporview-view-marker v)) (a (evaporview-view-alt-marker v))
         (_ (unless (and m a) (user-error "Place both markers first")))
         (lo (min m a)) (hi (max m a))
         (items (evaporview--signal-items (evaporview-view-items v)))
         (clock (and clock-path
                     (or (cl-find clock-path items :key #'evaporview-item-path :test #'equal)
                         (car (setq items (cons (evaporview--make-signal clock-path) items))))))
         (events
          (if clock
              (let ((sig (evaporview--item-sig clock)) (tm (1- lo)) acc)
                (while (and (setq tm (evaporview-sig-next-edge sig tm (or edge 'rise)))
                            (<= tm hi))
                  (push tm acc))
                (nreverse acc))
            (let ((ts (list lo)))
              (dolist (it items)
                (let* ((sig (evaporview--item-sig it)) (tm lo))
                  (while (and (setq tm (evaporview-sig-next-change sig tm)) (<= tm hi))
                    (push tm ts))))
              (sort (delete-dups ts) #'<))))
         (events (cl-subseq events 0 (min (length events) evaporview-wavedrom-max-events))))
    (json-encode
     `((head . ((text . ,(format "%s  %s - %s"
                                 (file-name-nondirectory (or (evaporview-view-file v) ""))
                                 (evaporview--fmt-time lo) (evaporview--fmt-time hi)))
                ,@(and clock '((tick . 0)))))
       (signal
        . ,(vconcat
            (mapcar
             (lambda (it)
               (let* ((sig (evaporview--item-sig it)) (prev 'none) (data nil)
                      (bit (and (= (evaporview-sig-width sig) 1)
                                (not (evaporview--text-sig-p sig))))
                      (wave
                       (if (eq it clock)
                           (concat (if (eq edge 'fall) "n" "p")
                                   (make-string (max 0 (1- (length events))) ?.))
                         (mapconcat
                          (lambda (tm)
                            (let ((val (evaporview-sig-value-at sig tm)))
                              (prog1
                                  (cond ((equal val prev) ".")
                                        ((and (not (evaporview--text-sig-p sig))
                                              (evaporview--wd-char val)))
                                        (bit (if (memq (aref val 0) '(?1 ?h)) "1" "0"))
                                        (t (push (evaporview-format-value
                                                  sig val (evaporview-item-format it))
                                                 data)
                                           "="))
                                (setq prev val))))
                          events ""))))
                 `((name . ,(evaporview-item-label it)) (wave . ,wave)
                   ,@(and data `((data . ,(vconcat (nreverse data))))))))
             items)))))))

(defun evaporview--wavedrom-args (clocked)
  "Interactive (CLOCK EDGE) for the WaveDrom commands; nil unless CLOCKED."
  (if (not clocked) (list nil nil)
    (list (completing-read "Clock: " (mapcar #'evaporview-item-path
                                             (evaporview--signal-items
                                              (evaporview-view-items evaporview--view)))
                           nil t)
          (intern (completing-read "Edge: " '("rise" "fall") nil t)))))

(defun evaporview-wavedrom-copy (&optional clock edge)
  "Copy the marker-to-alt-marker range as WaveDrom JSON.
With a prefix argument, ask for a CLOCK and EDGE to sample on."
  (interactive (evaporview--wavedrom-args current-prefix-arg))
  (let ((json (evaporview-wavedrom-string clock edge)))
    (kill-new json)
    (message "WaveDrom JSON copied (%d chars)" (length json))))

(defun evaporview--wavedrom-pretty (clock edge)
  (let ((json-encoding-pretty-print t)
        (json-object-type 'alist) (json-array-type 'vector) (json-key-type 'symbol))
    (json-encode (json-read-from-string (evaporview-wavedrom-string clock edge)))))

(defun evaporview--wavedrom-file-stem ()
  (let ((v evaporview--view))
    (replace-regexp-in-string
     " " "" (format "%s-%s-%s"
                    (file-name-base (or (evaporview-view-file v) "wave"))
                    (evaporview--fmt-time (min (evaporview-view-marker v)
                                               (evaporview-view-alt-marker v)))
                    (evaporview--fmt-time (max (evaporview-view-marker v)
                                               (evaporview-view-alt-marker v)))))))

(defun evaporview-wavedrom-org-block (&optional clock edge)
  "The marker range as an Org `wavedrom' source block (see ob-ewavedrom)."
  (unless (and (evaporview-view-marker evaporview--view)
               (evaporview-view-alt-marker evaporview--view))
    (user-error "Place both markers first"))
  (format "#+begin_src wavedrom :file %s.svg\n%s\n#+end_src\n"
          (evaporview--wavedrom-file-stem)
          (evaporview--wavedrom-pretty clock edge)))

(defun evaporview-wavedrom-copy-org-block (&optional clock edge)
  "Copy the marker range as an Org `wavedrom' block, ready for a document.
With a prefix argument, ask for a CLOCK and EDGE to sample on."
  (interactive (evaporview--wavedrom-args current-prefix-arg))
  (kill-new (evaporview-wavedrom-org-block clock edge))
  (message "Org wavedrom block copied; paste it into an Org file"))

;;;; ewavedrom

(declare-function ewavedrom-render-string "ewavedrom")
(declare-function ewavedrom-mode "ewavedrom")
(declare-function ewavedrom-preview "ewavedrom")

(defun evaporview--ewavedrom-available-p ()
  (require 'ewavedrom nil t))

(defun evaporview--require-ewavedrom ()
  (unless (evaporview--ewavedrom-available-p)
    (user-error "This needs ewavedrom on the load-path; `w' still copies the JSON")))

(defun evaporview-wavedrom-svg (&optional clock edge)
  "The marker range drawn by ewavedrom, as SVG text."
  (evaporview--require-ewavedrom)
  (ewavedrom-render-string (evaporview-wavedrom-string clock edge)))

(defun evaporview-wavedrom-export-svg (file &optional clock edge)
  "Write the marker range as a WaveDrom SVG FILE, drawn by ewavedrom.
With a prefix argument, ask for a CLOCK and EDGE to sample on."
  (interactive
   (progn (evaporview--require-ewavedrom)
          (append (list (read-file-name "Write SVG: " nil nil nil
                                        (concat (evaporview--wavedrom-file-stem) ".svg")))
                  (evaporview--wavedrom-args current-prefix-arg))))
  (let ((svg (evaporview-wavedrom-svg clock edge))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert svg))
    (message "Wrote %s" file)
    file))

(defun evaporview-wavedrom-preview (&optional clock edge)
  "Show the marker range as an editable WaveJSON buffer with its diagram.
The source buffer is in `ewavedrom-mode', so edges, arrows and labels
can be added and re-rendered there.  With a prefix argument, ask for a
CLOCK and EDGE to sample on.  Return the source buffer."
  (interactive (progn (evaporview--require-ewavedrom)
                      (evaporview--wavedrom-args current-prefix-arg)))
  (evaporview--require-ewavedrom)
  (let ((json (evaporview--wavedrom-pretty clock edge))
        (buf (get-buffer-create (format "*evaporview wavedrom: %s*"
                                        (evaporview--wavedrom-file-stem)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert json "\n"))
      (ewavedrom-mode)
      (goto-char (point-min))
      (ewavedrom-preview))
    (when (called-interactively-p 'any) (pop-to-buffer buf))
    buf))

;;;; Netlist

(defvar-local evaporview--wave-buffer nil)
(defvar-local evaporview--netlist-open nil "Expanded scope paths.")
(defvar-local evaporview--netlist-filter nil)

(defun evaporview--netlist-insert (scope prefix depth)
  (let ((indent (make-string (* 2 depth) ?\s))
        (filter evaporview--netlist-filter))
    (dolist (child (evaporview-scope-children scope))
      (let* ((path (concat prefix (evaporview-scope-name child)))
             (open (or filter (member path evaporview--netlist-open))))
        (when (or (null filter) (evaporview--scope-matches child path filter))
          (insert indent)
          (insert-text-button (concat (if open "▾ " "▸ ") (evaporview-scope-name child))
                              'action #'evaporview--netlist-toggle 'evw-scope path
                              'evw-scope-obj child 'face 'evaporview-group-face
                              'follow-link t)
          (insert (propertize (concat "  " (evaporview-scope-type child))
                              'face 'shadow)
                  "\n")
          (when open
            (evaporview--netlist-insert child (concat path ".") (1+ depth))))))
    (dolist (var (evaporview-scope-vars scope))
      (when (or (null filter)
                (string-match-p (regexp-quote filter) (evaporview-var-path var)))
        (insert indent "  ")
        (insert-text-button (evaporview-var-display-name var)
                            'action #'evaporview--netlist-add 'evw-var var
                            'follow-link t)
        (insert (propertize (format "  %s %d" (evaporview-var-type var)
                                    (evaporview-var-width var))
                            'face 'shadow)
                "\n")))))

(defun evaporview--scope-matches (scope path filter)
  (or (cl-some (lambda (v) (string-match-p (regexp-quote filter) (evaporview-var-path v)))
               (evaporview-scope-vars scope))
      (cl-some (lambda (c) (evaporview--scope-matches
                            c (concat path "." (evaporview-scope-name c)) filter))
               (evaporview-scope-children scope))))

(defun evaporview-netlist-refresh ()
  "Redraw the netlist."
  (interactive)
  (let ((inhibit-read-only t) (line (line-number-at-pos))
        (wave (with-current-buffer evaporview--wave-buffer
                (evaporview-view-wave evaporview--view))))
    (erase-buffer)
    (insert (propertize (concat " Netlist"
                                (if evaporview--netlist-filter
                                    (format "  [filter: %s]" evaporview--netlist-filter)
                                  ""))
                        'face 'mode-line-inactive)
            "\n")
    (evaporview--netlist-insert (evaporview-wave-root wave) "" 0)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun evaporview--netlist-toggle (button)
  (let ((path (button-get button 'evw-scope)))
    (setq evaporview--netlist-open
          (if (member path evaporview--netlist-open)
              (delete path evaporview--netlist-open)
            (cons path evaporview--netlist-open)))
    (evaporview-netlist-refresh)))

(defun evaporview--netlist-add (button)
  (let ((path (evaporview-var-path (button-get button 'evw-var))))
    (with-current-buffer evaporview--wave-buffer
      (evaporview-add-signal path))
    (message "Added %s" path)))

(defun evaporview-netlist-add-scope ()
  "Add every variable of the scope on this line."
  (interactive)
  (let* ((b (button-at (line-beginning-position)))
         (b (or b (save-excursion (back-to-indentation) (button-at (point)))))
         (scope (and b (button-get b 'evw-scope-obj))))
    (unless scope (user-error "Not on a scope"))
    (let ((paths (mapcar #'evaporview-var-path (evaporview-scope-vars scope))))
      (with-current-buffer evaporview--wave-buffer
        (dolist (p paths) (evaporview-add-signal p))))))

(defun evaporview-netlist-filter (filter)
  "Show only variables whose path contains FILTER; empty clears it."
  (interactive (list (read-string "Filter netlist: " evaporview--netlist-filter)))
  (setq evaporview--netlist-filter (if (string-empty-p filter) nil filter))
  (evaporview-netlist-refresh))

(defvar evaporview-netlist-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB") #'forward-button)
    (define-key m (kbd "<backtab>") #'backward-button)
    (define-key m (kbd "A") #'evaporview-netlist-add-scope)
    (define-key m (kbd "/") #'evaporview-netlist-filter)
    (define-key m (kbd "C-s") #'evaporview-netlist-filter)
    (define-key m (kbd "g") #'evaporview-netlist-refresh)
    m)
  "Keymap for `evaporview-netlist-mode'.")

(define-derived-mode evaporview-netlist-mode special-mode "EVW-Netlist"
  "Scope tree of a waveform dump.  RET or click on a variable adds it.
\\{evaporview-netlist-mode-map}"
  (setq truncate-lines t))

(defun evaporview-netlist ()
  "Show or hide the netlist side window."
  (interactive)
  (let* ((wave-buf (current-buffer))
         (buf (or (and (buffer-live-p evaporview--netlist-buffer) evaporview--netlist-buffer)
                  (setq evaporview--netlist-buffer
                        (get-buffer-create (format "*evaporview netlist: %s*"
                                                   (buffer-name wave-buf))))))
         (win (get-buffer-window buf)))
    (if win (delete-window win)
      (with-current-buffer buf
        (unless (derived-mode-p 'evaporview-netlist-mode) (evaporview-netlist-mode))
        (setq evaporview--wave-buffer wave-buf)
        (evaporview-netlist-refresh))
      (select-window
       (display-buffer-in-side-window
        buf `((side . left) (slot . 0) (window-width . ,evaporview-netlist-width)
              (preserve-size . (t . nil))))))))

;;;; Mode

(defvar evaporview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") #'evaporview-add-signal)
    (define-key m (kbd "l") #'evaporview-netlist)
    (define-key m (kbd "d") #'evaporview-remove-item)
    (define-key m (kbd "<delete>") #'evaporview-remove-item)
    (define-key m (kbd "DEL") #'evaporview-remove-item)
    (define-key m (kbd "M-<up>") #'evaporview-move-item-up)
    (define-key m (kbd "M-<down>") #'evaporview-move-item-down)
    (define-key m (kbd "M-p") #'evaporview-move-item-up)
    (define-key m (kbd "M-n") #'evaporview-move-item-down)
    (dotimes (i 8) (define-key m (kbd (format "%d" (1+ i))) #'evaporview-set-color))
    (define-key m (kbd "<f2>") #'evaporview-rename)
    (define-key m (kbd "r") #'evaporview-rename)
    (define-key m (kbd "f") #'evaporview-set-format)
    (define-key m (kbd "A") #'evaporview-toggle-analog)
    (define-key m (kbd ":") #'evaporview-slice-signal)
    (define-key m (kbd "S") #'evaporview-split-signal)
    (define-key m (kbd "G") #'evaporview-group-items)
    (define-key m (kbd "TAB") #'evaporview-toggle-group)
    (define-key m [remap next-line] #'evaporview-next-row)
    (define-key m [remap previous-line] #'evaporview-previous-row)
    (define-key m (kbd "<right>") #'evaporview-next-edge)
    (define-key m (kbd "<left>") #'evaporview-prev-edge)
    (define-key m (kbd "n") #'evaporview-next-edge)
    (define-key m (kbd "N") #'evaporview-prev-edge)
    (define-key m (kbd "M-<right>") #'evaporview-alt-next-edge)
    (define-key m (kbd "M-<left>") #'evaporview-alt-prev-edge)
    (define-key m (kbd "]") #'evaporview-next-rising-edge)
    (define-key m (kbd "[") #'evaporview-prev-rising-edge)
    (define-key m (kbd "}") #'evaporview-next-falling-edge)
    (define-key m (kbd "{") #'evaporview-prev-falling-edge)
    (define-key m (kbd "+") #'evaporview-zoom-in)
    (define-key m (kbd "=") #'evaporview-zoom-in)
    (define-key m (kbd "-") #'evaporview-zoom-out)
    (define-key m (kbd "0") #'evaporview-zoom-fit)
    (define-key m (kbd "z") #'evaporview-zoom-markers)
    (define-key m (kbd ",") #'evaporview-scroll-left)
    (define-key m (kbd ".") #'evaporview-scroll-right)
    (define-key m (kbd "<home>") #'evaporview-goto-start)
    (define-key m (kbd "<end>") #'evaporview-goto-end)
    (define-key m (kbd "m") #'evaporview-set-marker)
    (define-key m (kbd "M") #'evaporview-set-alt-marker)
    (define-key m (kbd "x") #'evaporview-swap-markers)
    (define-key m (kbd "/") #'evaporview-search-forward)
    (define-key m (kbd "\\") #'evaporview-search-backward)
    (define-key m (kbd "u") #'evaporview-undo)
    (define-key m (kbd "C-/") #'evaporview-undo)
    (define-key m (kbd "C-_") #'evaporview-undo)
    (define-key m (kbd "U") #'evaporview-redo)
    (define-key m (kbd "C-?") #'evaporview-redo)
    (define-key m (kbd "C-x C-s") #'evaporview-save-session)
    (define-key m (kbd "C-c C-l") #'evaporview-load-session)
    (define-key m (kbd "w") #'evaporview-wavedrom-copy)
    (define-key m (kbd "W") #'evaporview-wavedrom-preview)
    (define-key m (kbd "C-c C-w") #'evaporview-wavedrom-copy-org-block)
    (define-key m (kbd "C-c C-e") #'evaporview-wavedrom-export-svg)
    (define-key m (kbd "B") #'evaporview-browse)
    (define-key m (kbd "t") #'evaporview-set-time-unit)
    (define-key m (kbd "g") #'evaporview-redraw)
    (define-key m (kbd "C-c C-r") #'evaporview-reload)
    (define-key m [mouse-1] #'evaporview-mouse-set-marker)
    (define-key m [M-mouse-1] #'evaporview-mouse-set-alt-marker)
    (define-key m [mouse-2] #'evaporview-mouse-set-alt-marker)
    (define-key m [C-wheel-up] #'evaporview-mouse-zoom-in)
    (define-key m [C-wheel-down] #'evaporview-mouse-zoom-out)
    (define-key m [C-mouse-4] #'evaporview-mouse-zoom-in)
    (define-key m [C-mouse-5] #'evaporview-mouse-zoom-out)
    (define-key m [S-wheel-up] #'evaporview-mouse-scroll-left)
    (define-key m [S-wheel-down] #'evaporview-mouse-scroll-right)
    (define-key m [S-mouse-4] #'evaporview-mouse-scroll-left)
    (define-key m [S-mouse-5] #'evaporview-mouse-scroll-right)
    (define-key m [wheel-left] #'evaporview-mouse-scroll-left)
    (define-key m [wheel-right] #'evaporview-mouse-scroll-right)
    m)
  "Keymap for `evaporview-mode'.")

(defun evaporview--on-resize (where)
  "Redraw viewers whose width changed; WHERE is a window or a frame."
  (dolist (win (if (windowp where) (list where) (window-list where)))
    (with-current-buffer (window-buffer win)
      (when (and (derived-mode-p 'evaporview-mode)
                 (not (equal evaporview--last-cols (evaporview--wave-cols))))
        (with-selected-window win (evaporview-redraw))))))

(define-derived-mode evaporview-mode special-mode "EVaporView"
  "Waveform viewer modelled on VaporView.
\\{evaporview-mode-map}"
  (setq truncate-lines t
        buffer-undo-list t)
  (hl-line-mode 1)
  (add-hook 'window-size-change-functions #'evaporview--on-resize nil t))

(declare-function evaporview-ghw-parse-file "evaporview-ghw")

(defun evaporview--parse-dump (file)
  "Read the dump FILE: GHW by its extension, VCD otherwise."
  (if (string-match-p "\\.ghw\\'" file)
      (progn (require 'evaporview-ghw)
             (evaporview-ghw-parse-file file))
    (evaporview-vcd-parse-file file)))

;;;###autoload
(defun evaporview-open (file)
  "Open the dump FILE (VCD, or GHW from GHDL) in a viewer and return its buffer."
  (interactive
   (list (read-file-name "Waveform dump: " nil
                         (or (and (derived-mode-p 'dired-mode)
                                  (fboundp 'dired-get-filename)
                                  (dired-get-filename nil t))
                             buffer-file-name)
                         t)))
  (setq file (expand-file-name file))
  (let ((buf (get-buffer-create (format "*evaporview: %s*" (file-name-nondirectory file)))))
    (with-current-buffer buf
      (unless (and evaporview--view (equal (evaporview-view-file evaporview--view) file))
        (evaporview-mode)
        (let ((wave (evaporview--parse-dump file)))
          (setq evaporview--view
                (evaporview-view--make
                 :wave wave :file file :items nil
                 :start (evaporview-wave-start wave) :tpc 1
                 :unit (evaporview-auto-time-unit (evaporview-wave-end wave)
                                                  (evaporview-wave-timescale wave)))))
        (evaporview-zoom-fit)
        (let ((session (evaporview--default-session-file)))
          (when (and evaporview-auto-load-session (file-readable-p session))
            (evaporview-load-session session)
            (setf (evaporview-view-undo evaporview--view) nil)))))
    (when (called-interactively-p 'any)
      (switch-to-buffer buf)
      (with-current-buffer buf
        (evaporview-redraw)
        (unless (and (buffer-live-p evaporview--netlist-buffer)
                     (get-buffer-window evaporview--netlist-buffer))
          (evaporview-netlist))))
    buf))

(defun evaporview-reload ()
  "Re-read the dump from disk, keeping signals, markers and view."
  (interactive)
  (let* ((v evaporview--view)
         (data (evaporview-session-data v)))
    (setf (evaporview-view-wave v) (evaporview--parse-dump (evaporview-view-file v)))
    (evaporview-apply-session data)
    (message "Reloaded %s" (evaporview-view-file v))))

(provide 'evaporview)
;;; evaporview.el ends here

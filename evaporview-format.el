;;; evaporview-format.el --- Value and time formatting for evaporview  -*- lexical-binding: t; -*-

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

;; Turns bit strings into the display formats VaporView offers.  The browser
;; viewer implements the same rules; test/format-cases.json binds the two.
;;
;; Decimal conversion runs on base-10^9 limbs instead of bignums, so buses of
;; any width print exactly and the arithmetic stays inside fixnums.

;;; Code:

(require 'cl-lib)
(require 'evaporview-vcd)

(defconst evaporview-formats
  '(binary hex octal unsigned signed ascii float real)
  "Value formats, in menu order.")

(defun evaporview-format--2state-p (bits)
  (not (string-match-p "[^01]" bits)))

(defun evaporview-format--radix (bits group)
  "Right-justified digits of BITS, GROUP bits per digit."
  (let* ((len (length bits))
         (pad (mod (- group (mod len group)) group))
         (s (concat (make-string pad ?0) bits))
         (out nil))
    (dotimes (i (/ (length s) group))
      (let ((d (substring s (* i group) (* (1+ i) group))))
        (push (cond ((not (string-match-p "[^z]" d)) "z")
                    ((string-match-p "[^01]" d) "x")
                    (t (upcase (format (if (= group 4) "%x" "%o")
                                       (string-to-number d 2)))))
              out)))
    (apply #'concat (nreverse out))))

(defconst evaporview-format--limb 1000000000)

(defun evaporview-format--limbs (bits)
  "Little-endian base-10^9 limbs of the unsigned number in BITS."
  (let ((limbs (list 0)) (i 0) (len (length bits)))
    (while (< i len)
      (let* ((n (min 30 (- len i)))
             (carry (string-to-number (substring bits i (+ i n)) 2))
             (mul (ash 1 n)))
        (setq limbs (mapcar (lambda (l)
                              (let ((v (+ (* l mul) carry)))
                                (setq carry (/ v evaporview-format--limb))
                                (% v evaporview-format--limb)))
                            limbs))
        (while (> carry 0)
          (setq limbs (append limbs (list (% carry evaporview-format--limb)))
                carry (/ carry evaporview-format--limb)))
        (setq i (+ i n))))
    limbs))

(defun evaporview-format--limbs-add1 (limbs)
  (let ((carry 1))
    (setq limbs (mapcar (lambda (l)
                          (let ((v (+ l carry)))
                            (setq carry (/ v evaporview-format--limb))
                            (% v evaporview-format--limb)))
                        limbs))
    (if (> carry 0) (append limbs (list carry)) limbs)))

(defun evaporview-format--limbs-string (limbs)
  (let ((rev (reverse limbs)))
    (apply #'concat (format "%d" (car rev))
           (mapcar (lambda (l) (format "%09d" l)) (cdr rev)))))

(defun evaporview-format--unsigned (bits)
  (evaporview-format--limbs-string (evaporview-format--limbs bits)))

(defun evaporview-format--signed (bits)
  (if (eq (aref bits 0) ?0)
      (evaporview-format--unsigned bits)
    (let ((inv (apply #'string (mapcar (lambda (c) (if (eq c ?0) ?1 ?0)) bits))))
      (concat "-" (evaporview-format--limbs-string
                   (evaporview-format--limbs-add1
                    (evaporview-format--limbs inv)))))))

(defun evaporview-format--ascii (bits)
  (let* ((pad (mod (- 8 (mod (length bits) 8)) 8))
         (s (concat (make-string pad ?0) bits))
         (out nil))
    (dotimes (i (/ (length s) 8))
      (let ((c (string-to-number (substring s (* i 8) (* (1+ i) 8)) 2)))
        (push (if (and (>= c 32) (<= c 126)) (string c) ".") out)))
    (apply #'concat (nreverse out))))

(defun evaporview-format-number (x)
  "Format float X like C printf %g."
  (cond ((/= x x) "nan")
        ((= x 1.0e+INF) "inf")
        ((= x -1.0e+INF) "-inf")
        (t (format "%g" x))))

(defun evaporview-format--float (bits)
  "IEEE 754 value of BITS for widths 16, 32 and 64, else nil."
  (let ((spec (assq (length bits) '((16 5 10) (32 8 23) (64 11 52)))))
    (when spec
      (let* ((ebits (nth 1 spec)) (mbits (nth 2 spec))
             (bias (1- (ash 1 (1- ebits))))
             (neg (eq (aref bits 0) ?1))
             (e (string-to-number (substring bits 1 (1+ ebits)) 2))
             (m (string-to-number (substring bits (1+ ebits)) 2))
             (mag (cond ((= e (1- (ash 1 ebits)))
                         (if (= m 0) 1.0e+INF 0.0e+NaN))
                        ((= e 0) (ldexp (float m) (- 1 bias mbits)))
                        (t (ldexp (float (+ (ash 1 mbits) m)) (- e bias mbits))))))
        (if (and neg (= mag mag)) (- mag) mag)))))

(defun evaporview-format-bits (bits fmt)
  "Format the bit string BITS (MSB first) in format FMT.
Formats that cannot show non-2-state bits fall back to binary."
  (let ((two (evaporview-format--2state-p bits)))
    (pcase fmt
      ('hex (evaporview-format--radix bits 4))
      ('octal (evaporview-format--radix bits 3))
      ((guard (not two)) bits)
      ('unsigned (evaporview-format--unsigned bits))
      ('signed (evaporview-format--signed bits))
      ('ascii (evaporview-format--ascii bits))
      ('float (let ((x (evaporview-format--float bits)))
                (if x (evaporview-format-number x) bits)))
      (_ bits))))

(defun evaporview-format-real (string)
  "Format the decimal STRING of a real variable."
  (evaporview-format-number (float (string-to-number string))))

(defun evaporview-format-value (sig value fmt)
  "Format VALUE of SIG in FMT; nil VALUE is shown as empty."
  (cond ((null value) "")
        ((evaporview-sig-real-p sig) (evaporview-format-real value))
        (t (evaporview-format-bits value fmt))))

(defun evaporview-format-default (sig)
  "Initial format for SIG."
  (cond ((evaporview-sig-real-p sig) 'real)
        ((= (evaporview-sig-width sig) 1) 'binary)
        (t 'hex)))

(defun evaporview-value-number (sig value fmt)
  "VALUE of SIG as a float for analog drawing, or nil if not 2-state."
  (cond ((null value) nil)
        ((evaporview-sig-real-p sig) (float (string-to-number value)))
        ((not (evaporview-format--2state-p value)) nil)
        ((eq fmt 'float) (evaporview-format--float value))
        (t (let ((u (float (string-to-number (evaporview-format--unsigned value)))))
             (if (and (eq fmt 'signed) (eq (aref value 0) ?1))
                 (- u (expt 2.0 (length value)))
               u)))))

;;;; Time

(defconst evaporview-time-units
  '(("fs" . 0) ("ps" . 3) ("ns" . 6) ("us" . 9) ("ms" . 12) ("s" . 15))
  "Time units and their power of ten in femtoseconds.")

(defun evaporview--unit-power (unit)
  (or (cdr (assoc unit evaporview-time-units)) 6))

(defun evaporview-time-value (ticks timescale unit)
  "TICKS of TIMESCALE expressed in UNIT, exact when integral."
  (let ((n (* ticks (car timescale)))
        (d (- (evaporview--unit-power (cdr timescale))
              (evaporview--unit-power unit))))
    (cond ((>= d 0) (* n (expt 10 d)))
          ((and (integerp n) (zerop (% n (expt 10 (- d))))) (/ n (expt 10 (- d))))
          (t (/ (float n) (expt 10 (- d)))))))

(defun evaporview-format-time (ticks timescale unit)
  "Format TICKS of TIMESCALE as a time in UNIT, e.g. \"12.5 ns\"."
  (let ((v (evaporview-time-value ticks timescale unit)))
    (concat (if (integerp v) (format "%d" v)
              (if (= v (ffloor v)) (format "%d" (truncate v)) (format "%g" v)))
            " " unit)))

(defun evaporview-auto-time-unit (end timescale)
  "Largest unit in which END ticks of TIMESCALE is at least 1."
  (let ((best (cdr timescale)))
    (dolist (u evaporview-time-units)
      (when (and (>= (evaporview--unit-power (car u))
                     (evaporview--unit-power (cdr timescale)))
                 (>= (evaporview-time-value end timescale (car u)) 1))
        (setq best (car u))))
    best))

(defun evaporview-ticks (value unit timescale)
  "Ticks of TIMESCALE nearest to VALUE in UNIT."
  (round (* value (expt 10.0 (- (evaporview--unit-power unit)
                                (evaporview--unit-power (cdr timescale)))))
         (car timescale)))

(defun evaporview-format-frequency (ticks timescale)
  "Frequency whose period is TICKS of TIMESCALE, e.g. \"100 MHz\"."
  (if (or (null ticks) (zerop ticks)) ""
    (let* ((s (* (abs ticks) (car timescale)
                 (expt 10.0 (- (evaporview--unit-power (cdr timescale)) 15))))
           (f (/ 1.0 s))
           (units '((1e12 . "THz") (1e9 . "GHz") (1e6 . "MHz") (1e3 . "kHz")))
           (u (cl-find-if (lambda (u) (>= f (car u))) units)))
      (if u (format "%g %s" (/ f (car u)) (cdr u)) (format "%g Hz" f)))))

(provide 'evaporview-format)
;;; evaporview-format.el ends here

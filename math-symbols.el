;;; package --- Summary:
 ;; Search for non-ascii symbols using helm

;; Copyright (C) 2017  Dodge Coates

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Author: Dodge W. Coates
;;; URL: https://github.com/dwcoates/math-symbols

;;; Commentary:
;;
;; Use `math-symbols-get-symbols' to spawn an helm menu with which to
;; search for unicode Symbols.  Symbols are pulled from the .dat
;; files.

;;; Code:

(require 'helm)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; READ/WRITE ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar math-sym-data-dir (file-name-directory (or load-file-name buffer-file-name)))

(defvar math-sym-alist nil)

(defun math-sym-get-data (file-name)
  "Read ms data from file into a description/symbol alist and return it"
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (concat math-sym-data-dir file-name))
      (let ((raw-data-list (split-string (buffer-string) "\n" t)))
        (mapcar
         ;; create an assosiation from raw data (a line from a .dat file)
         (lambda (raw-str)
           `(,(concat (substring raw-str 0 1) "\t" (substring raw-str 2))
             ,(substring raw-str 0 1)))
         raw-data-list)
        ))
    ))

(defun math-sym--read-data ()
  (setq math-sym-alist
        (cl-sort (apply 'append
                        (mapcar 'math-sym-get-data
                                (remove-if-not (lambda (string) (search ".dat" string))
                                               (directory-files math-sym-data-dir))))
                 (lambda (a1 a2)
                   (string-lessp (car a1) (car a2)))
                 )))

(math-sym--read-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; HELM DISPLAY;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun math-symbols-get-symbols ()
  (interactive)
  (helm :sources '(math-sym-helm-sources)))

(defvar math-sym-helm-sources
  '((name . "Unicode symbols list")
    (candidates . symbols-defun)
    (action . (("Insert symbol" . insert-symbol)))
    ))

(defun insert-symbol (symbol)
  (insert (car symbol)))

(defun symbols-defun ()
   math-sym-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; KEY COMMAND ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c o s") 'math-symbols-get-symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; MISC UTILS ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proper-words (string)
  "Capitalize first letter of each word in string. Words delimited by whitespace"
  (string-join
   (mapcar (lambda (word)
             (let ((first-letter (substring word 0 1))
                   (rest (substring word 1)))
               (concat (capitalize first-letter) rest)))
           (split-string string " " t))
   " "))

(defun prettify-symbols-datum ()
  "Make the description for unicode symbol data at point 'proper'"
  (interactive)
  (let* ((ugly-line (dwc/get-line))
         (desc (substring ugly-line 2))
         (rest (substring ugly-line 0 2)))
    (beginning-of-line)
    (kill-line)
    (insert (concat rest (proper-words desc)))
    (newline)
    ))

(provide 'math-symbols)

;;; math-symbols ends here

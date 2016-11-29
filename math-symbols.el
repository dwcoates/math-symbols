;;; Code:

(require 'helm)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; READ/WRITE ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwc--ms-data-dir (file-name-directory load-file-name))
(defvar dwc--ms-symbols-alist nil)

(defun dwc--ms-get-data (file-name)
  "Read ms data from file into a description/symbol alist and return it"
  (save-excursion
    (with-temp-buffer
      (insert-file-contents (file-truename file-name))
      (let ((raw-data-list (split-string (buffer-string) "\n" t)))
        (mapcar
         ;; create assosiation list from raw-data-list (list of lines from .dat file)
         (lambda (raw-str)
           `(,(concat (substring raw-str 0 1) "\t" (substring raw-str 2))
             ,(substring raw-str 0 1)))
         raw-data-list)
        ))
    ))

(defun ms--read-data ()
  (setq dwc--ms-symbols-alist
        (cl-sort (apply 'append (mapcar
                                 'dwc--ms-get-data
                                 (remove-if-not (lambda (string) (search ".dat" string))
                                                (directory-files dwc--ms-data-dir))))
                 (lambda (a1 a2)
                   (string-lessp (car a1) (car a2)))
                 )))

(ms--read-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; HELM DISPLAY;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dwc-helm-ms-get-symbols ()
  (interactive)
  (helm :sources '(dwc--ms-helm-sources))
  )

(defvar dwc--ms-helm-sources
  '((name . "Unicode symbols list")
    (candidates . dwc--symbols-defun)
    (action . (("Insert symbol" . dwc--insert-symbol)))
    ))

(defun dwc--insert-symbol (symbol)
  (insert (car symbol))
  )

(defun dwc--symbols-defun ()
   dwc--ms-symbols-alist
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; KEY COMMAND ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c o s") 'dwc-helm-ms-get-symbols)

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
   " ")
  )

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
    )
  )

(provide 'math-symbols)

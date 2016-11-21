
;;; Code:

(require 'helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; READ/WRITE ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dwc--ms-data-file (let ((default-directory (file-name-directory load-file-name)))
                            (file-truename "symbols.dat")))
(defvar dwc--data-delimiter "*")
(defvar dwc--ms-symbols-alist nil)

(defun dwc-ms-read-data ()
  "Read ms data from file into a description/symbol alist"
  (save-excursion
    (with-temp-buffer
      (insert-file-contents dwc--ms-data-file)
      (let ((raw-data-list (split-string (buffer-string) "\n" t)))
        (setq dwc--ms-symbols-alist
              (mapcar
               ;; create assosiation list from raw-data-list
               (lambda (raw-str)
                 ;; turn the raw string into a data association
                 (let ((raw-list (split-string
                                  raw-str dwc--data-delimiter t " ")))
                   `(,(concat (cadr raw-list) "  " (car raw-list)) .
                     ,(cadr raw-list))))
               raw-data-list)
               ))
      )))

(dwc-ms-read-data)

(defun dwc-ms-write-data (data-alist)
  "Save list of description/symbol associations to data file"
  (save-excursion
     (with-temp-file dwc--ms-data-file
      (insert-file-contents dwc--ms-data-file)
      (insert "\n")
      (insert
       (mapconcat (lambda (desc-symbol)
                    (concat (car desc-symbol)
                            dwc--data-delimiter
                            (cdr desc-symbol)))
                  data-alist
                  "\n"
                  ))
      (sort-lines nil (point-min) (point-max))
      (delete-duplicate-lines (point-min) (point-max) nil nil nil nil)
      )))

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
  (insert symbol)
  )

(defun dwc--symbols-defun ()
   dwc--ms-symbols-alist
   )

(provide 'math-symbols)

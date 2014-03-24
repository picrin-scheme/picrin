;;; ADD ME TO YOUR .emacs.d/init.el

(defun scheme-add-keywords (face-name keyword-rules)
  (let* ((keyword-list (mapcar #'(lambda (x)
                                   (symbol-name (cdr x)))
                               keyword-rules))
         (keyword-regexp (concat "(\\("
                                 (regexp-opt keyword-list)
                                 "\\)[ \n]")))
    (font-lock-add-keywords 'scheme-mode
                            `((,keyword-regexp 1 ',face-name))))
  (mapc #'(lambda (x)
            (put (cdr x)
                 'scheme-indent-function
                 (car x)))
        keyword-rules))

(scheme-add-keywords
 'font-lock-keyword-face
 '((1 . when)
   (1 . unless)
   (1 . define-library)
   (0 . import)
   (0 . export)
   (1 . letrec*)
   (1 . define-values)
   (1 . define-record-type)
   (1 . parameterize)
   (0 . values)))

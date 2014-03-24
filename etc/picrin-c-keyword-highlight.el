;;; ADD ME TO YOUR .emacs.d/init.el

(defun font-lock-user-keywords (mode &optional keywords)
  "Add user highlighting on KEYWORDS to given MODE.
See `font-lock-add-keywords' and `font-lock-defaults'."
  (unless mode
    (error "mode should be non-nil "))
  (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
  (font-lock-add-keywords mode keywords)
  (put mode 'font-lock-user-keywords keywords))

(font-lock-user-keywords
 'c-mode
 '(("pic_try" . font-lock-keyword-face)
   ("pic_catch" . font-lock-keyword-face)
   ("pic_for_each" . font-lock-keyword-face)))

(require 'generic-x)

(define-generic-mode 'model-mode
  ()
  '("in" "if" "," "@pv")
  '(("\\(#.*$\\)" 1 'font-lock-comment-face)
    ("%[a-zA-Z0-9_]+" . 'font-lock-variable-name-face)
    ("\[\\(.*\\)\]" 1 'font-lock-type-face)
    ("[a-zA-Z][a-zA-Z0-9_]+" . 'font-lock-string-face)
    )
  '(".mdl\\'")
  nil)

(provide 'model-mode)

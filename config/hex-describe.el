(define-generic-mode hex-describe-mode
  '("#")
  nil
  '(("^;.*" . font-lock-function-name-face)
    ("\\*.*\\*" . 'bold)
    ("\\[\\([1-9][0-9]*d[1-9][0-9]*\\(?:x[1-9][0-9]*\\)?\\(?:\\+[1-9][0-9]*\\)?\\)\\]"
     (0 font-lock-keyword-face) (1 font-lock-string-face t))
    "\\[.*?\\]"
    ("^[0-9]+," . font-lock-type-face))
  '("hex-describe-default-.*\\.txt$")
  nil
  "A major mode to edit Hex Describe data files.")

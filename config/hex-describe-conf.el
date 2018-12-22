(define-generic-mode hex-describe-mode
  '("#")
  nil
  '(("^;\\(.*\\)" (1 'underline))
    ("\\*\\*.*?\\*\\*" . 'bold)
    ("\\*.*?\\*" . 'italic)
    ("\\[\\([1-9][0-9]*d[1-9][0-9]*\\(?:x[1-9][0-9]*\\)?\\(?:\\+[1-9][0-9]*\\)?\\)\\]"
     (0 'bold) (1 'underline t))
    ("\\[[^][\n]*\\(\\[[^][\n]*\\]\\)[^][\n]*\\]"
     (0 'bold) (1 'underline t))
    "\\[[^][\n]*\\]"
    ("^[0-9]+," . 'bold))
  '("hex-describe-.*\\.txt$")
  nil
  "A major mode to edit Hex Describe data files.")

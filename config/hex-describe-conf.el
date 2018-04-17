(define-generic-mode hex-describe-mode
  '("#")
  nil
  '(("^;.*" . font-lock-function-name-face)
    ("\\*.*?\\*" . 'bold)
    ("\\[\\([1-9][0-9]*d[1-9][0-9]*\\(?:x[1-9][0-9]*\\)?\\(?:\\+[1-9][0-9]*\\)?\\)\\]"
     (0 font-lock-keyword-face) (1 font-lock-string-face t))
    ("\\[[^][\n]*\\(\\[[^][\n]*\\]\\)[^][\n]*\\]"
     (0 font-lock-keyword-face) (1 font-lock-constant-face t))
    "\\[[^][\n]*\\]"
    ("^[0-9]+," . font-lock-type-face))
  '("hex-describe-.*\\.txt$")
  nil
  "A major mode to edit Hex Describe data files.")

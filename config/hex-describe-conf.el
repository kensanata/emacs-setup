(define-generic-mode hex-describe-mode
  '("#")
  nil
  '(("^;\\(.*\\)" (1 'underline))
    ("\\[\\([1-9][0-9]*d[1-9][0-9]*\\(?:x[1-9][0-9]*\\)?\\(?:\\+[1-9][0-9]*\\)?\\)\\]"
     (0 'bold) (1 'underline t))
    ("\\[[^][\n]*\\(\\[[^][\n]*\\]\\)[^][\n]*\\]"
     (0 'bold) (1 'underline t))
    "\\[[^][\n]*\\]"
    ("^[0-9]+," . 'bold)
    ("\\*\\*.*?\\*\\*" . 'bold)
    ("\\*.*?\\*" . 'italic))
  '("hex-describe-.*\\.txt$")
  nil
  "A major mode to edit Hex Describe data files.")

(add-hook 'hex-describe-mode-hook 'turn-on-flyspell)
(add-hook 'hex-describe-mode-hook 'idle-highlight-mode)
(add-hook 'hex-describe-mode-hook 'typo-mode)

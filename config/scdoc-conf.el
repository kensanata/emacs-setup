(define-derived-mode scdoc-mode text-mode "SC"
  "A mode to write scdoc files in."
  :group 'text
  (font-lock-add-keywords nil
    '(("^# .*" . 'info-title-2)
      ("^## .*" . 'info-title-3)
      ("^\\*[^*]+\\*" . 'bold)
      ("\\*[^*]+\\*" . 'bold)
      ("^\\( \\|   +\\)\\b" . 'makefile-space); two spaces is ok in list item continuation
      ("\\B_[^_ ]*_([0-9]+)" . 'info-xref); don't do it inside words
      ("\\B_[^_]*_" . 'underline)))
  (setq fill-column 80))

;; The text files in man folders are scdoc sources
(add-to-list 'auto-mode-alist '("man/.*\\.[0-9]+.txt\\'" . scdoc-mode))

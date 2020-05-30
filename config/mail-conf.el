;; I use aerc to read mail and emacsclient as my mail editor

(define-derived-mode eml-mode mail-mode "Email"
  "Write emails via emacsclient."
  (when (buffer-file-name)
    (set (make-local-variable 'server-temp-file-regexp) ""))
  (local-set-key (kbd "C-c C-c") 'server-edit)
  (auto-fill-mode 1))

(add-to-list 'auto-mode-alist '("\\.eml\\'" . eml-mode))

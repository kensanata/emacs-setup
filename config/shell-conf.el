(add-to-list 'auto-mode-alist '("\\.fish\\'" . shell-script-mode))
(add-hook 'sh-mode-hook 'asc:sh-mode-init)
(defun asc:sh-mode-init ()
  (local-set-key "TAB" 'sh-basic-indent-line))

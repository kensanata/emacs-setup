(use-package pydoc)
(add-hook 'python-mode-hook 'asc:init-python-mode)

(defun asc:init-python-mode ()
  (idle-highlight-mode 1)
  (local-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t))

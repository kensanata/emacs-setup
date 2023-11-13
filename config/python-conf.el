;; (use-package pydoc :after python)
(add-hook 'python-mode-hook 'asc:init-python-mode)

(defun asc:init-python-mode ()
  (idle-highlight-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace t t))

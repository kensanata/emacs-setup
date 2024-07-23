(add-hook 'compilation-mode-hook 'asc:compilation-mode-init)

(defun asc:compilation-mode-init ()
  (local-set-key (kbd "C-z") #'bury-buffer))

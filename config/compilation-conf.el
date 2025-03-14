(add-hook 'compilation-mode-hook 'asc:compilation-mode-init)

(defun asc:compilation-mode-init ()
  (local-set-key (kbd "C-z") #'bury-buffer))

(add-hook 'compilation-filter-hook 'ansi-mode-for-compilation)

(defun ansi-mode-for-compilation ()
  (let ((begin compilation-filter-start)
        (end (point)))
    (ansi-color-apply-on-region begin end)))

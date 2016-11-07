;; Oracle
(setenv "NLS_LANG" "GERMAN_SWITZERLAND.UTF8")

(add-hook 'sql-mode-hook 'asc:sql-init)

(defun asc:sql-init ()
  (local-set-key (kbd "C-z") 'sql-oracle)
  (local-set-key (kbd "C-c C-d") 'asc:sql-describe-word-at-point)
  (local-set-key (kbd "C-<return>") 'sql-send-paragraph))

(add-hook 'sql-interactive-mode-hook 'asc:sql-interactive-init)

(defun asc:sql-interactive-init ()
  (local-set-key (kbd "C-z")
		 (lambda ()
		   (interactive)
		   (if (> (count-windows) 1)
		       (delete-window)
		     (bury-buffer)))))

(defun asc:sql-describe-word-at-point ()
  (interactive)
  (let ((word (symbol-name (symbol-at-point))))
    (when word
      (sql-send-string (concat "describe " word)))))

(setq sql-database nil
      sql-user "postgres"
      sql-password "manager"
      sql-product 'postgres)

(add-hook 'sql-mode-hook 'asc:sql-init)

(defun asc:sql-init ()
  (local-set-key (kbd "C-z") 'sql-postgres)
  (local-set-key (kbd "C-<return>") 'sql-send-paragraph))

(add-hook 'sql-interactive-mode-hook 'asc:sql-interactive-init)

(defun asc:sql-interactive-init ()
  (local-set-key (kbd "C-z")
		 (lambda ()
		   (interactive)
		   (if (> (count-windows) 1)
		       (delete-window)
		     (bury-buffer)))))

;; Oracle
(setenv "NLS_LANG" "GERMAN_SWITZERLAND.UTF8")

(setq sql-database "dev"
      sql-user "devasc"
      sql-password "devascpw")

(add-hook 'sql-mode-hook 'asc:sql-init)

(defun asc:sql-init ()
  (local-set-key (kbd "C-z") 'sql-oracle)
  (local-set-key (kbd "C-c d") 'asc:sql-describe-word-at-point)
  (local-set-key (kbd "C-c c") 'asc:sql-code-word-at-point)
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

(defun asc:sql-code-word-at-point ()
  (interactive)
  (let ((num (number-at-point)))
    (when num
      (sql-send-string (format "select ors_util.get_user_code(%d) from dual;" num)))))

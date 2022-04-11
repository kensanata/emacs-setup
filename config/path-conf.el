;; Perlbrew
(let ((init-file "~/.perlbrew/init"))
  (if (file-exists-p init-file)
      (with-temp-buffer
	(insert-file-contents init-file)
	(while (re-search-forward "export \\([A-Z_]+\\)=\"\\(.*\\)\"" nil t)
	  (message "Setting %s=%s" (match-string 1) (match-string 2))
	  (setenv (match-string 1) (match-string 2)))
	(when (getenv "PERLBREW_MANPATH")
	  (setenv "MANPATH"
		  (concat (getenv "PERLBREW_MANPATH") path-separator
			  (replace-regexp-in-string "\n" "" (shell-command-to-string "manpath")))))
	(when (getenv "PERLBREW_PATH")
	  (setenv "PATH"
		  (concat (getenv "PERLBREW_PATH") path-separator
			  (getenv "PATH")))
	  (dolist (path (split-string (getenv "PERLBREW_PATH") path-separator))
	    (unless (member path exec-path)
	      (add-to-list 'exec-path path)))))
    (message "Did not find %s" init-file)))

;; ~/.local

(setenv "MANPATH"
	(concat "/home/alex/.local/share/man" path-separator
		(getenv "MANPATH")))
(setenv "PATH"
	(concat "/home/alex/.local/bin" path-separator
		(getenv "PATH")))

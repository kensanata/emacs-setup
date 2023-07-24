;; Perlbrew
(let ((init-file "~/.perlbrew/init"))
  (if (file-exists-p init-file)
      (with-temp-buffer
	(insert-file-contents init-file)
	(goto-char (point-min))
	(while (re-search-forward "export \\([A-Z_]+\\)=\"\\(.*\\)\"" nil t)
	  (message "Setting %s=%s" (match-string 1) (match-string 2))
	  (setenv (match-string 1) (match-string 2)))
	(goto-char (point-min))
	(while (re-search-forward "setenv \\([A-Z_]+\\) \"\\(.*\\)\"" nil t)
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

;; fish
(let ((init-file "~/.config/fish/config.fish"))
  (if (file-exists-p init-file)
      (with-temp-buffer
	(insert-file-contents init-file)
	(goto-char (point-min))
        (when (search-forward "set -x PATH" nil t)
          (let ((end (line-end-position))
                (paths))
            (while (re-search-forward "[~/]\\S-+" end t)
              (let ((path (expand-file-name (match-string 0))))
                (add-to-list 'paths path)))
            (setq exec-path (nconc (reverse paths) exec-path)))))
    (message "Did not find %s" init-file)))

;; ~/.local

(setenv "MANPATH"
	(concat "/home/alex/.local/share/man" path-separator
		(getenv "MANPATH")))
(setenv "PATH"
	(concat "/home/alex/.local/bin" path-separator
		(getenv "PATH")))

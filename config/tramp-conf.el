(setq tramp-completion-function-alist
      '(("scp"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))
	("ssh"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))))

(defvar putty-directory "c:/Program Files/PuTTY"
  "The directory containing pscp.exe and plink.exe on Windows.")

(defun sibirocobombus ()
  (interactive)
  "Connect to my favorite server using Tramp."
  (if (not (eq window-system 'w32))
      (find-file "/ssh:alex@alexschroeder.ch#882:")
    (when (and (not (string-match putty-directory (getenv "PATH")))
	       (file-directory-p putty-directory))
      (setenv "PATH" (concat putty-directory ";" (getenv "PATH"))))
    (find-file "/plink:alex@alexschroeder.ch#882:")))

(defun the-shire ()
  (interactive)
  "Connect to a backup server using Tramp."
  (find-file "/kensanata@theshire.emacs.cl:"))

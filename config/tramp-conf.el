(setq tramp-completion-function-alist
      '(("scp"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))
	("ssh"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))))

(defun sibirocobombus ()
  (interactive)
  "Connect to my favorite server using Tramp."
  (find-file "/ssh:alex@sibirocobombus#882:"))

(defun the-shire ()
  (interactive)
  "Connect to a backup server using Tramp."
  (find-file "/kensanata@theshire.emacs.cl:"))

(setq tramp-completion-function-alist
      '(("scp"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))
	("ssh"
	 (tramp-parse-netrc "~/.authinfo.gpg")
	 (tramp-parse-rhosts "/etc/hosts.equiv")
	 (tramp-parse-shosts "~/.ssh/known_hosts"))))

(defvar putty-directory "c:/Portable Programs/PuTTY"
  "The directory containing pscp.exe and plink.exe on Windows.")

(setq tramp-default-method "ssh")
(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))))

(defun sibirocobombus ()
  (interactive)
  "Connect to my favorite server using Tramp.
On Windows, make sure PAGEANT is running and the sibirocobombus
key has been added to it."
  (find-file "/plink:alex@alexschroeder.ch#882:"))

(defun megabombus ()
  (interactive)
  "Connect to our other laptop server using Tramp."
  (find-file "/alex@megabombus.local:"))

(defun the-shire ()
  (interactive)
  "Connect to a backup server using Tramp."
  (find-file "/kensanata@theshire.emacs.cl:"))

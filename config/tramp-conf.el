(setq tramp-completion-use-auth-sources nil) ;; don't ask for my passphrase

(defvar putty-directory "c:/Portable Programs/PuTTY"
  "The directory containing pscp.exe and plink.exe on Windows.")

(setq tramp-default-method "ssh")
(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))))

(defun sibirocobombus ()
  "Connect to my favorite server using Tramp.
On Windows, make sure PAGEANT is running and the sibirocobombus
key has been added to it."
  (interactive)
  (find-file "/alex@alexschroeder.ch#882:"))

(defun megabombus ()
  "Connect to our other laptop server using Tramp."
  (interactive)
  (find-file "/alex@megabombus.local:"))

(defun the-shire ()
  "Connect to a backup server using Tramp."
  (interactive)
  (find-file "/kensanata@theshire.emacs.cl:"))

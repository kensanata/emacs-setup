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

(setq tramp-default-method "ssh")
(when (eq window-system 'w32)
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))))


(defun asc:ssh-parse-environment ()
  "Parse the environment file containing the ssh-agent output"
  (with-temp-buffer
    (insert-file-contents-literally "~/.ssh/environment")
    (when (search-forward-regexp "SSH_AUTH_SOCK=\\([^;#\n]*\\)" nil t)
      (setenv "SSH_AUTH_SOCK" (match-string 1)))
    (when (search-forward-regexp "SSH_AGENT_PID=\\([0-9]+\\)" nil t)
      (setenv "SSH_AGENT_PID" (match-string 1)))))

(defun asc:ssh-agent-live-p ()
  "Return non-nil if the agent is live.
This checks whether the SSH_AGENT_PID environment variable holds
the pid of a ssh-agent process."
  (with-temp-buffer
    (call-process "ps" nil t nil "-ef")
    (goto-char (point-min))
    (keep-lines (getenv "SSH_AGENT_PID"))
    (message (buffer-string))
    (search-forward-regexp "ssh-agent$" nil t)))

(defun asc:ssh-agent-start ()
  "Start the ssh-agent.
The environment is saved to ~/.ssh/environment.
Use `asc:ssh-parse-environment' to read it."
  (with-temp-buffer
    (call-process "ssh-agent" t)
    (goto-char (point-min))
    (replace-regexp "^echo" "#echo")
    (write-region (point-min) (point-max) "~/.ssh/environment")))

(defun asc:ssh-agent ()
  "Make sure the ssh-agent runs as started by .bashrc."
  (interactive)
  (asc:ssh-parse-environment)
  (if (asc:ssh-agent-live-p)
      (message "SSH agent %s is up and running" (getenv "SSH_AGENT_PID"))
    (asc:ssh-agent-start)
    (asc:ssh-parse-environment)
    (if (asc:ssh-agent-live-p)
	(message "SSH agent %s started" (getenv "SSH_AGENT_PID"))
      (error "Could not start ssh-agent"))))

(defun sibirocobombus ()
  (interactive)
  "Connect to my favorite server using Tramp."
  (asc:ssh-agent)
  (find-file "/plink:alex@alexschroeder.ch#882:"))

(defun megabombus ()
  (interactive)
  "Connect to our other laptop server using Tramp."
  (find-file "/alex@megabombus.local:"))

(defun the-shire ()
  (interactive)
  "Connect to a backup server using Tramp."
  (find-file "/kensanata@theshire.emacs.cl:"))

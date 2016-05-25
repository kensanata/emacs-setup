;; https://www.emacswiki.org/emacs/Gmail,_Gnus_and_GPG_on_a_Mac
(defun gpg-restart-agent ()
  "This kills and restarts the gpg-agent.

To kill gpg-agent, we use killall. If you know that the agent is
OK, you should just reload the environment file using
`gpg-reload-agent-info'."
  (interactive)
  (shell-command "killall gpg-agent")
  (shell-command "gpg-agent --daemon --enable-ssh-support --write-env-file")
  ;; read the environment file instead of parsing the output
  (gpg-reload-agent-info))

(defun gpg-reload-agent-info ()
  "Reload the ~/.gpg-agent-info file."
  (interactive)
  (let ((file (expand-file-name "~/.gpg-agent-info")))
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file file)
	(goto-char (point-min))
	(while (re-search-forward "\\([A-Z_]+\\)=\\(.*\\)" nil t)
	  (setenv (match-string 1) (match-string 2)))))))

(defun gpg-agent-startup ()
  "Initialize the gpg-agent if necessary.

Note that sometimes the gpg-agent can be up and running and still
be useless, in which case you should restart it using
`gpg-restart-agent'."
  (gpg-reload-agent-info)
  (let ((pid (getenv "SSH_AGENT_PID")))
    (unless (and pid (member (string-to-number pid) (list-system-processes)))
      (gpg-restart-agent))))

(gpg-agent-startup)

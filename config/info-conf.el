;; On my laptop with its external drive, this operation takes a while.
;; That's why we're running it off an idle timer in order to improve
;; startup time.

(run-with-idle-timer
 10 nil
 (lambda ()
   (setq Info-additional-directory-list
	 (nconc '("/usr/local/opt/emacs-mac/share/info/emacs"
		  "~/src/emacs-live/packs/stable/git-pack/lib/magit/Documentation")
		(delete "/usr/local/opt/emacs-mac/share/info"
			(file-expand-wildcards "/usr/local/opt/*/share/info"))))
   (message "Info-additional-directory-list was set")))

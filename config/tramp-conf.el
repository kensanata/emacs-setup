(setq tramp-completion-use-auth-sources nil ;; don't ask for my passphrase
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-auto-save-visited t
      tramp-allow-unsafe-temporary-files t) ;; See (tramp) Auto-save File Lock and Backup

(setq tramp-default-method "ssh")

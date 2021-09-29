;; no need for cookies within Emacs
;; use M-x url-cookie-list to check
(setq url-cookie-trusted-urls '()
      url-cookie-untrusted-urls '(".*")
      ;; ↓ run (url-setup-privacy-info) after changing
      url-privacy-level 'paranoid)

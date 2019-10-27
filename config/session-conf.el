(when (asc:package-install 'session)
  (require 'session)
  (add-hook 'after-init-hook 'session-initialize))

(setq history-length 500
      session-globals-max-size 500)

;; prevent point from being wrong in commit messages by magit
(setq session-name-disable-regexp "\\.git/[A-Z_]+\\'")
;; prevent parse errors for fontified text in the kill-ring
(add-to-list 'session-globals-exclude 'kill-ring)

(asc:package-install 'session)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; prevent point from being wrong in commit messages by magit
(setq session-name-disable-regexp "\\`\\.git/[A-Z_]+\\'")

;; nothing required for GnuPG 2.1
(unless (eq (window-system) 'w32)
  (setq epa-pinentry-mode 'loopback))

;; don't hide the password as I type
(defalias 'read-passwd 'read-string)

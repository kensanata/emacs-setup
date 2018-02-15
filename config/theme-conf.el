(setq custom-theme-directory "~/.emacs.d/emacs-setup/lib")
;; Use M-x enable-theme and M-x disable-theme
(condition-case err
    (load-theme 'pink-bliss t t)
  (error))

;; (load-theme 'foggy-night t)

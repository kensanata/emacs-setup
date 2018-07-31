(setq custom-theme-directory "~/.emacs.d/emacs-setup/lib")

;; Use M-x enable-theme and M-x disable-theme
(condition-case err
    (load-theme 'pink-bliss t t)
  (error))

(add-to-list 'custom-theme-load-path "/home/alex/src/brutalist-theme.el")
(condition-case err
    (load-theme 'brutalist t t)
  (error))

(asc:package-install 'foggy-night-theme)
(load-theme 'foggy-night t)
(set-face-background 'cursor "#aaa")
(defface rcirc-late-fix-face '((t (:foreground "white" :underline t)))
  "Overwriting the default")

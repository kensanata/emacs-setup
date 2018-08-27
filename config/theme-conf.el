(setq custom-theme-directory "~/.emacs.d/emacs-setup/lib")

;; Use M-x enable-theme and M-x disable-theme
(condition-case err
    (load-theme 'pink-bliss t t)
  (error))

(add-to-list 'custom-theme-load-path "/home/alex/src/brutalist-theme.el")
(condition-case err
    (load-theme 'brutalist t t)
  (error))

(custom-theme-set-faces
 'brutalist
 '(rcirc-my-nick ((t (:inherit bold))))
 '(rcirc-dim-nick ((t (:foreground "dim gray"))))
 '(rcirc-prompt ((t (:inherit bold)))))

(enable-theme 'brutalist)

(asc:package-install 'foggy-night-theme)
(condition-case err
    (progn
      (load-theme 'foggy-night t t)
      (set-face-background 'cursor "#aaa"))
  (error))

(custom-theme-set-faces
 'foggy-night
 '(rcirc-late-fix-face ((t (:foreground "white" :underline t)))))

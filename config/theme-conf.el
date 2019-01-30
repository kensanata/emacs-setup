(setq custom-theme-directory "~/.emacs.d/emacs-setup/lib")

;; Use M-x enable-theme and M-x disable-theme
(condition-case err
    (load-theme 'pink-bliss t t)
  (error))

(asc:package-install 'brutalist-theme)
(condition-case err
    (load-theme 'brutalist t t)
    (load-theme 'brutalist-dark t t)
  (error))

(custom-theme-set-faces
 'brutalist
 ;; rcirc-my-nick needs to have a foreground for `rcirc-colors' to work
 '(rcirc-my-nick ((t (:foreground "blue"))))
 '(rcirc-dim-nick ((t (:foreground "dim gray"))))
 '(rcirc-prompt ((t (:inherit bold)))))

(require 'calendar)
(setq calendar-latitude [47 22 north]
      calendar-longitude [8 33 east])
(let* ((date (calendar-current-date))
       (data (solar-sunrise-sunset date))
       (sunrise (car (car data)))
       (sunset (car (cadr data)))
       (time (decode-time))
       (now (+ (nth 2 time) (/ (nth 1 time) 60.0))))
  (if (and (< sunrise now) (< now sunset))
      (enable-theme 'brutalist)
    (enable-theme 'brutalist-dark)))

(asc:package-install 'foggy-night-theme)
(condition-case err
    (progn
      (load-theme 'foggy-night t t)
      (set-face-background 'cursor "#aaa"))
  (error))

(custom-theme-set-faces
 'foggy-night
 '(rcirc-late-fix-face ((t (:foreground "white" :underline t)))))

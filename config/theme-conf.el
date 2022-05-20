;; (setq custom-theme-directory "~/.emacs.d/emacs-setup/lib")
;; (load-theme 'pink-bliss t t)

(use-package brutalist-theme)

;; Use M-x enable-theme and M-x disable-theme
(load-theme 'brutalist t t)
(load-theme 'brutalist-dark t t)

(custom-theme-set-faces
 'brutalist
 ;; rcirc-my-nick needs to have a foreground for `rcirc-colors' to work
 '(rcirc-my-nick ((t (:foreground "blue"))))
 '(rcirc-dim-nick ((t (:foreground "dim gray"))))
 '(rcirc-prompt ((t (:inherit bold))))
 '(dictionary-reference-face ((t (:inherit default))))
 '(wgrep-delete-face ((t (:inherit font-lock-comment-face :strike-through t))))
 '(wgrep-done-face ((t (:inherit default))))
 '(wgrep-face ((t (:inherit default))))
 '(wgrep-file-face ((t (:inherit default)))))

(custom-theme-set-variables
 'brutalist
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"]))

(custom-theme-set-faces
 'brutalist-dark
 ;; don't actually make the other modelines unreadable
 '(mode-line-inactive ((t (:foreground "gray70"))))
 ;; rcirc-my-nick needs to have a foreground for `rcirc-colors' to work
 '(message-header-cc ((t (:inherit bold))))
 '(message-header-name ((t (:inherit default))))
 '(message-header-newsgroup ((t (:inherit default))))
 '(message-header-other ((t (:inherit default))))
 '(message-header-subject ((t (:inherit bold))))
 '(message-header-to ((t (:inherit bold))))
 '(message-header-xheader ((t (:inherit default))))
 '(message-cited-text-1 ((t (:inherit default))))
 '(message-cited-text-2 ((t (:inherit default))))
 '(message-cited-text-3 ((t (:inherit default))))
 '(message-cited-text-4 ((t (:inherit default))))
 '(rcirc-my-nick ((t (:foreground "light blue"))))
 '(rcirc-dim-nick ((t (:foreground "gray70"))))
 '(rcirc-prompt ((t (:inherit bold))))
 '(dictionary-reference-face ((t (:inherit default))))
 '(idle-highlight ((t (:background "gray30"))))
 '(wgrep-delete-face ((t (:inherit font-lock-comment-face :strike-through t))))
 '(wgrep-done-face ((t (:inherit default))))
 '(wgrep-face ((t (:inherit default))))
 '(wgrep-file-face ((t (:inherit default))))
 '(j-adverb-face ((t (:inherit bold))))
 '(j-conjunction-face ((t (:inherit bold))))
 '(j-other-face ((t (:inherit bold))))
 '(j-verb-face ((t (:inherit bold))))
 '(js2-error ((t (:inherit shadow))))
 '(js2-warning ((t (:background "grey40"))))
 '(swiper-line-face ((t (:background "grey40"))))
 '(mastodon-boost-fave-face ((t (:inherit success))))
 '(mastodon-boosted-face ((t (:inherit bold))))
 '(mastodon-cw-face ((t (:inherit bold))))
 '(mastodon-display-name-face ((t (:inherit font-lock-string-face))))
 '(mastodon-handle-face ((t (:inherit font-lock-comment-face)))))

(custom-theme-set-variables
 'brutalist-dark
 '(ansi-color-names-vector
   ["black" "salmon" "yellow green" "light goldenrod" "steel blue" "orchid" "turquoise" "#eeeee8"]))

;; always use the dark theme
(enable-theme 'brutalist-dark)

;; (require 'solar)

;; ;; set these to your current location
;; (setq calendar-latitude [47 22 north]
;;       calendar-longitude [8 33 east])

;; (defun enable-appropriate-brutalist-theme ()
;;   "Enable the brutalist or brutalist-dark theme
;; depending on the time of day."
;;   (interactive)
;;   (let* ((date (calendar-current-date))
;; 	 (data (solar-sunrise-sunset date))
;; 	 (sunrise (car (car data)))
;; 	 (sunset (car (cadr data)))
;; 	 (time (decode-time))
;; 	 (now (+ (nth 2 time) (/ (nth 1 time) 60.0))))
;;     (cond ((< now sunrise)
;; 	   (message "%dh and %02dmin to go until sunrise"
;; 		    (truncate (- sunrise now))
;; 		    (round (mod (* 60 (- sunrise now)) 60))))
;; 	  ((< now sunset)
;; 	   (message "%dh and %02dmin to go until sunset"
;; 		    (truncate (- sunset now))
;; 		    (round (mod (* 60 (- sunset now)) 60))))
;; 	  (t
;; 	   (message "%dh and %02dmin to go until sunrise"
;; 		    (truncate (- (+ 24 sunrise) now))
;; 		    (- 60 (round (mod (* 60 (- (+ 24 sunrise) now)) 60))))))
;;     (if (and (< sunrise now) (< now sunset))
;; 	(enable-theme 'brutalist)
;;       (enable-theme 'brutalist-dark))))

;; (enable-appropriate-brutalist-theme)

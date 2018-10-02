;; See https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(cond ((find-font (font-spec :name "Iosevka"))
       ;; on windows, this defaults to a different font?
       (dolist (face '(default fixed-pitch))
	 (set-face-attribute
	  face nil
	  :family "Iosevka"
	  :height (if (eq (window-system) 'w32) 180 220))))
      ((find-font (font-spec :name "Noto Mono"))
       (set-face-attribute 'default nil :family "Noto Mono" :height 200)))

(defun iosevka ()
  "Switch to Iosevka font."
  (interactive)
  (set-face-attribute 'default nil :family "Iosevka"))

(defun noto ()
  "Switch to Noto font."
  (interactive)
  (set-face-attribute 'default nil :family "Noto Mono"))

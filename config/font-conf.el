;; See https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(cond ((and (eq (window-system) 'w32)
	    (find-font (font-spec :name "Consolas")))
       (set-face-attribute 'default nil :family "Consolas" :height 130))
      ((find-font (font-spec :name "Iosevka"))
       (set-face-attribute 'default nil :family "Iosevka" :height 160))
      ((find-font (font-spec :name "Noto Mono"))
       (set-face-attribute 'default nil :family "Noto Mono" :height 140)))

(defun iosevka ()
  "Switch to Iosevka font."
  (interactive)
  (set-face-attribute 'default nil :family "Iosevka"))

(defun noto ()
  "Switch to Noto font."
  (interactive)
  (set-face-attribute 'default nil :family "Noto Mono"))

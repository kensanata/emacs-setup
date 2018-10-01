;; See https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(cond ((find-font (font-spec :name "Iosevka"))
       (set-face-attribute 'default nil :family "Iosevka" :height 220)
       ;; on windows, this defaults to a different font?
       (set-face-attribute 'fixed-pitch nil :family "Iosevka" :weight 'light :height 220))
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

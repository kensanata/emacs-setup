
(cond ((find-font (font-spec :name "Fira Code")))
      (set-face-attribute 'default nil :family "Fira Code" :height 160)
      (when (functionp 'mac-auto-operator-composition-mode)
	(mac-auto-operator-composition-mode))
      ((find-font (font-spec :name "Consolas"))
       (set-face-attribute 'default nil :family "Consolas" :height 160)))

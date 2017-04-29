;; See https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(cond ((and (eq (window-system) 'w32)
	    (find-font (font-spec :name "Consolas")))
       (set-face-attribute 'default nil :family "Consolas" :height 130))
      ((find-font (font-spec :name "Fira Code"))
       (set-face-attribute 'default nil :family "Fira Code" :height 140)
       (when (functionp 'mac-auto-operator-composition-mode)
	 (mac-auto-operator-composition-mode))))

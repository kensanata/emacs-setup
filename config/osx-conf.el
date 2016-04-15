(when (eq (window-system) 'mac)

  (set-face-attribute 'default nil :family "Fira Code" :height 140)
  (when (functionp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

  ;; We don't need option as alt because elsewhere we installed the
  ;; german-prefix input method.
  (setq mac-option-modifier 'meta
	mac-command-modifier 'hyper))

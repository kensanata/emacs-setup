(when (eq (window-system) 'mac)
  ;; We don't need option as alt because elsewhere we installed the
  ;; german-prefix input method.
  (setq mac-option-modifier 'meta
	mac-command-modifier 'hyper))

(when (eq (window-system) 'mac)
  (set-face-attribute 'default nil :family "Fira Code" :height 160)
  (when (functionp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)))

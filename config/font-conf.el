(when (find-font (font-spec :name "Iosevka Term"))
  (dolist (face '(default fixed-pitch fixed-pitch-serif))
    (set-face-attribute
     face nil
     :family "Iosevka Term"
     :height (if (eq (window-system) 'w32) 180 220)))
  (dolist (face '(variable-pitch))
    (set-face-attribute
     face nil
     :family "Iosevka Etoile"
     :height (if (eq (window-system) 'w32) 180 220))))

(defun iosevka ()
  "Switch to Iosevka font."
  (interactive)
  (set-face-attribute 'default nil :family "Iosevka Term")
  (set-face-attribute 'default nil :family "Iosevka Etoile"))

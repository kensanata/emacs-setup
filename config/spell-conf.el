(defun german ()
  "Switch ispell dictionary to 'swiss'."
  (interactive)
  (ispell-change-dictionary "swiss8")
  (asc:flyspell)
  (set-input-method 'german-prefix))

(defun english ()
  "Switch ispell dictionary to 'british'."
  (interactive)
  (ispell-change-dictionary "british")
  (asc:flyspell)
  (set-input-method nil))

(defun asc:flyspell ()
  "Ensure flyspell is running."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode t))
  (flyspell-buffer))

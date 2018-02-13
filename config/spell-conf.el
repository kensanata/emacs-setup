(defun german ()
  "Switch ispell dictionary to 'swiss'."
  (interactive)
  (ispell-change-dictionary "swiss8")
  (asc:flyspell))

(defun english ()
  "Switch ispell dictionary to 'american'."
  (interactive)
  (ispell-change-dictionary "american")
  (asc:flyspell))

(defun asc:flyspell ()
  "Ensure flyspell is running."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode t))
  (flyspell-buffer))

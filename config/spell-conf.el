(setq ispell-program-name "hunspell")

(defun swiss ()
  "Switch ispell dictionary to 'swiss'."
  (interactive)
  (ispell-change-dictionary "de_CH")
  (asc:flyspell)
  (typo-mode 1)
  (setq typo-language "German")
  (set-input-method 'german-prefix))

(defun english ()
  "Switch ispell dictionary to 'british'."
  (interactive)
  (ispell-change-dictionary "en_GB")
  (asc:flyspell)
  (typo-mode 1)
  (setq typo-language "English")
  (set-input-method nil))

(defun asc:flyspell ()
  "Ensure flyspell is running."
  (interactive)
  (unless flyspell-mode
    (flyspell-mode t))
  (flyspell-buffer))

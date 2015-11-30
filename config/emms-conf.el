;; Changes to my Windows setup.
;; Sadly, I installed mpg123 instead of mpg321.

(dolist (dir '("C:/Program Files/mp3info-0.8.5-win"
	       "C:/Program Files (x86)/mpg123-1.22.0-x86-64"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)))

(let ((dir (expand-file-name (concat (getenv "USERPROFILE") "/Music"))))
  (when (file-directory-p dir)
    (setq emms-source-file-default-directory dir)))

(define-emms-simple-player mpg123 '(file url)
  (emms-player-simple-regexp "mp3" "mp2")
  "mpg123")
(setq emms-player-list '(emms-player-mpg123))

(require 'emms-setup)
(emms-standard)

;; Most of these changes are to get things to work on my Windows
;; machine. Sadly, I installed mpg123 instead of mpg321 on the Windows
;; machine.

(use-package emms :defer 10 :config (asc:emms-setup))

(defun asc:emms-setup ()
  (require 'emms-setup)
  (emms-standard)
  (if (executable-find "mpg321")
      (setq emms-player-list '(emms-player-mpg321))
    (define-emms-simple-player mpg123 '(file url)
      (emms-player-simple-regexp "mp3" "mp2")
      "mpg123")
    (setq emms-player-list '(emms-player-mpg123)))
  (when (executable-find "ogg123")
    (add-to-list 'emms-player-list 'emms-player-ogg123)))

(dolist (dir '("C:/Portable Programs/mp3info-0.8.5-win"
	       "C:/Portable Programs/mpg123-1.22.0-x86-64"))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)))

(dolist (dir (list (concat (getenv "USERPROFILE") "/Music")
		   "~/Music"))
  (when (file-directory-p dir)
    (setq emms-source-file-default-directory (expand-file-name dir))))

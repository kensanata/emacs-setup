(eval-after-load "dired"
  '(require 'dired-x))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (local-set-key "E" 'emms-play-dired)
	    (local-set-key (kbd "C-c w") 'wdired-change-to-wdired-mode)))

;; Homebrew: brew install coreutils gives us GNU ls
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-deletion-confirmer 'y-or-n-p
      dired-clean-up-buffers-too nil
      delete-by-moving-to-trash t
      ;; trash-directory "~/.Trash/emacs"
      dired-dwim-target t
      ;; I installed Skim via Homebrew: brew search skim
      ;; and created a shell script as follows:
      ;; #!/bin/sh
      ;; osascript -e "tell application \"Skim\"" -e "open \"$(pwd)/$1\"" -e "end tell"
      dired-guess-shell-alist-user
      (if (file-exists-p "/usr/bin/evince")
	  '(("\\.pdf\\'" "evince")
	    ("\\.jpg\\'" "feh"))
	'(("\\.pdf\\'" "skim")
	  ("." "open")))
      dired-listing-switches "-alv")

(add-hook 'image-dired-thumbnail-mode-hook
	  (lambda ()
	    (local-set-key (kbd "t l") 'asc:file-label-edit)
	    (local-set-key (kbd "S-SPC") 'image-dired-display-previous-thumbnail-original)))

(defun asc:file-label-edit ()
  "Edit the text file for the image at point.
This is used for `image-dired'."
  (interactive)
  (find-file (concat (file-name-sans-extension
		      (image-dired-original-file-name))
		     ".txt")))

(setq image-dired-thumb-margin 5)

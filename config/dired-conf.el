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
	    (local-set-key (kbd "t a") 'asc:add-to-album)
	    (local-set-key (kbd "S-SPC") 'image-dired-display-previous-thumbnail-original)))

(defun asc:file-label-edit ()
  "Edit the text file for the image at point.
This is used for `image-dired'."
  (interactive)
  (find-file (concat (file-name-sans-extension
		      (image-dired-original-file-name))
		     ".txt")))

(defun asc:add-to-album ()
  "Show current thumbnail, tag it with 'album', and add a label."
  (interactive)
  (image-dired-display-thumbnail-original-image)
  (let ((tag "album"))
    (image-dired-write-tags (list (cons (image-dired-original-file-name) tag))))
  (image-dired-update-property
   'tags (image-dired-list-tags (image-dired-original-file-name)))
  (asc:file-label-edit))

(setq image-dired-thumb-margin 5)

(require 'dired-x)

(defun asc:image-dired-exif-mode (dir)
  "Create a virtual buffer for images sorted by creation date."
  (interactive "DDirectory: ")
  (with-current-buffer (get-buffer-create "*images*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let* ((files (directory-files dir 'full "jpg$"))
	     (prog (make-progress-reporter "Reading files " 0 (length files)))
	     (i 0))
	(insert dir ":\n")
	(dolist (file files)
	  (let* ((attr (file-attributes file 'string))
		 (default-directory dir)	; required for eshell remote command
	     	 (created (eshell-command-to-value
			   (eshell-remote-command
			    "exiftool" (list "-p" "${dateTimeOriginal#;DateFmt('%Y-%m-%d %H:%M:%S')}"
					     "-q" "-f" (file-name-nondirectory file))))))
	    (insert (format "%s % 3d %s %s % 10d %s %s\n"
			    (file-attribute-modes attr)
			    (file-attribute-link-number attr)
			    (file-attribute-user-id attr)
			    (file-attribute-group-id attr)
			    (file-attribute-size attr)
			    (substring created 0 -1) ; drop trailing newline
			    ;; (format-time-string "%F %R" (file-attribute-modification-time attr))
			    (file-name-nondirectory file))))
	  (progress-reporter-update prog (setq i (1+ i))))
	(progress-reporter-done prog))
      (dired-virtual-mode))
    (switch-to-buffer (current-buffer))))

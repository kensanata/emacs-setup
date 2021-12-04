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
      dired-guess-shell-alist-user
      	  '(("\\.pdf\\'" "evince * &")
	    ("\\.jpg\\'" "feh"))
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

(defun asc:image-dired-exif-data (dir)
  "Return alist of filenames and Date Time Original.
Do this for all files in DIR, possibly remote."
  (message "Reading exifdata for %s..." dir)
  (let* ((default-directory dir)
	 (data (shell-command-to-string
		"exiftool -p '${dateTimeOriginal#;DateFmt(\"%Y-%m-%d %H:%M:%S\")} $fileName' -q -f .")))
    (message "Reading exifdata for %s...done" dir)
    (mapcar (lambda (line)
	      (if (string= (substring line 0 2) "- ")
		  (let ((name (substring line 2)))
		    (cons name		; filename
			  (format-time-string
			   "%F %R" (file-attribute-modification-time
				    (file-attributes name)))))
		(cons
		 (substring line 20)	  ; filename
		 (substring line 0 19)))) ; date and time
	    (split-string data "\n" t))))

(defun asc:image-dired-exif-mode (dir)
  "Create a virtual buffer for images sorted by creation date.
Use the apostrophe to edit the text files matching the image file."
  (interactive "DDirectory: ")
  (with-current-buffer (get-buffer-create "*images*")
    (let ((inhibit-read-only t)
	  (data (asc:image-dired-exif-data dir)))
      (erase-buffer)
      (let* ((files (sort (directory-files dir nil "jpg$" t)
			  (lambda (f1 f2)
			    (string< (cdr (assoc f1 data))
				     (cdr (assoc f2 data))))))
	     (prog (make-progress-reporter "Reading files " 0 (length files)))
	     (i 0))
	(insert dir ":\n")
	(dolist (file files)
	  (let ((attr (file-attributes (expand-file-name file dir) 'string)))
	    (insert (format "%s % 3d %s %s % 10d %s %s\n"
			    (file-attribute-modes attr)
			    (file-attribute-link-number attr)
			    (file-attribute-user-id attr)
			    (file-attribute-group-id attr)
			    (file-attribute-size attr)
			    (cdr (assoc file data))
			    ;; (format-time-string "%F %R" (file-attribute-modification-time attr))
			    file)))
	  (progress-reporter-update prog (setq i (1+ i))))
	(progress-reporter-done prog))
      (dired-virtual-mode)
      (local-set-key (kbd "'") 'asc:label-edit))
    (switch-to-buffer (current-buffer))))

(defun asc:label-edit ()
  "Edit the text file for the image at point.
This is used for `asc:image-dired-exif-mode'."
  (interactive)
  (find-file (concat (file-name-sans-extension
		      (dired-file-name-at-point))
		     ".txt")))

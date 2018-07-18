(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))
(auto-image-file-mode 0)

(defadvice image-toggle-display-text (before asc:editable-images activate)
  "Remove the intangible text property from the buffer."
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	(modified (buffer-modified-p)))
    (remove-list-of-text-properties (point-min) (point-max)
				    '(intangible))
    (set-buffer-modified-p modified)))

(add-hook 'image-dired-display-image-mode-hook 'asc:image-dired-display)

(defun asc:image-dired-display ()
  "Some more initialisation for `image-dired-display-image-mode'."
  (local-set-key (kbd "p") 'asc:image-dired-display-previous)
  (local-set-key (kbd "n") 'asc:image-dired-display-next)
  (local-set-key (kbd "d") 'asc:image-dired-display-delete))

(defun asc:image-dired-display-previous ()
  "Show previous image based on the *image-dired* buffer."
  (interactive)
  (with-current-buffer (get-buffer "*image-dired*")
    (image-dired-backward-image)
    (image-dired-display-thumbnail-original-image)))

(defun asc:image-dired-display-next ()
  "Show next image based on the *image-dired* buffer."
  (interactive)
  (with-current-buffer (get-buffer "*image-dired*")
    (image-dired-forward-image)
    (image-dired-display-thumbnail-original-image)))

(defun asc:image-dired-display-delete ()
  "Mark image as deleted in the *image-dired* buffer."
  (interactive)
  (with-current-buffer (get-buffer "*image-dired*")
    (image-dired-flag-thumb-original-file)
    (image-dired-display-thumbnail-original-image)))

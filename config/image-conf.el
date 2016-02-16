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

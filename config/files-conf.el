;; Load up to 100MB without asking for confirmation (default is 10MB)
(setq large-file-warning-threshold 100000000)

(defun asc:move-mentioned-files (regexp target)
  "Move all the filenames matching REGEXP to TARGET.
The REGEXP matches any string in the buffer.
If TARGET does not exist, it is created.
This is useful for the output of where-was-it."
  (interactive (list (read-string "Filenames matching: " "IMG_[0-9]+\\.JPG")
		     (read-directory-name "Move to directory: ")))
  (unless (file-directory-p target)
    (make-directory target t))
  (save-excursion
    (goto-char (if (region-active-p) (region-beginning) (point-min)))
    (let ((n 0))
      (while (re-search-forward regexp (if (region-active-p) (region-end)) t)
	(rename-file (match-string 0) (expand-file-name (match-string 0) target))
	(setq n (1+ n)))
      (message "Moved %d files." n))))

(defun asc:find-file-as-root (filename &optional wildcards)
  "Find file as root, using su."
  (interactive (find-file-read-args "Find file as root: " nil))
  (find-file (concat "/sudo:root@melanobombus:" filename) wildcards))

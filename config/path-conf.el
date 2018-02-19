;; local python binaries
(defun asc:add-to-PATH (path)
  (setq path (expand-file-name path))
  (let ((paths (parse-colon-path (getenv "PATH"))))
    (unless (member path paths)
      (setenv "PATH" (mapconcat 'identity (cons path paths) path-separator)))))

(asc:add-to-PATH "~/.local/bin")

;; local python binaries
(defun asc:add-to-env (var path)
  (let ((paths (parse-colon-path (getenv var))))
    (unless (member path paths)
      (setenv var (mapconcat 'identity (cons path paths) path-separator)))))

(defun asc:add-to-PATH (path)
  (setq path (expand-file-name path))
  (asc:add-to-env "PATH" path))

(asc:add-to-PATH "~/.local/bin/")

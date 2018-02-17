;; local python binaries
(defun asc:add-to-PATH (path)
  (setq path (expand-file-name path))
  (let ((paths (split-string (getenv "PATH") ":")))
    (unless (member path paths)
      (setenv "PATH" (mapconcat 'identity (cons path paths) ":")))))

(asc:add-to-PATH "~/.local/bin")

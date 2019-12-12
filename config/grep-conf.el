;; default to case insensitive
(eval-after-load "grep"
  '(grep-apply-setting 'grep-command "grep --color -niH -e "))

(defun grep-word-at-point (word)
  "Grep for thing at point.
Per default, the same extension is used as the file-name of the
current buffer. Use a prefix argument to override either."
  (interactive (list (thing-at-point 'symbol)))
  (let* ((file-name (buffer-file-name))
	 (extension (and file-name (file-name-extension file-name)))
	 (pattern (if extension (concat "*." extension) "*")))
    (when current-prefix-arg
      (setq word (read-string "Grep for: " word)
	    pattern (read-string "Files to grep: " pattern)))
    (grep (format "grep --color -niH -e \"%s\" %s" word pattern))))

(global-set-key (kbd "C-c s") 'grep-word-at-point)

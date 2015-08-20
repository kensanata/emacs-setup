;;; bs instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'bs-show)

(setq bs-configurations
      '(("all" nil nil nil nil nil)
        ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
	("oddmuse" nil nil nil
	 (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'oddmuse-mode)))) nil)
        ("dired" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'dired-mode)))) nil)
	("gnus" "^\\*\\(Group\\|Summary\\|Article\\|Server\\|\\(un\\)?sent\\b\\)" nil "" nil nil)
        ("rcirc" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'rcirc-mode))))
	 rcirc-sort-buffers)))

(defun rcirc-sort-name (buf)
  "Return server process and buffer name as a string."
  (when (boundp 'rcirc-server-buffer)
    (with-current-buffer buf
      (downcase (concat (if rcirc-server-buffer
			    (buffer-name rcirc-server-buffer)
			  " ")
			" "
			(or rcirc-target ""))))))

(defun rcirc-sort-buffers (a b)
  "Sort buffers A and B using `rcirc-sort-name'."
  (string< (rcirc-sort-name a)
	   (rcirc-sort-name b)))

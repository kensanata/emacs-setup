;;; bs instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'bs-show)

(setq bs-configurations
      '(;;("gnus" "^\\*\\(Group\\|Summary\\|Article\\|Server\\|\\(un\\)?sent\\b\\)" nil "" nil nil)
	("all" nil nil nil nil nil)
        ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
        ("c" nil nil nil
	 (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'c-mode))))
	 bs-sort-buffer-interns-are-last)
	("oddmuse" nil nil nil
	 (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'oddmuse-mode))))
	 bs-sort-buffer-interns-are-last)
        ("dired" nil nil nil
         (lambda (buf)
           (with-current-buffer buf
             (not (eq major-mode 'dired-mode))))
	 bs-sort-buffer-interns-are-last)))

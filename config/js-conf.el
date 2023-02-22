(use-package mmm-mode :ensure t
  :config (setq mmm-global-mode 'maybe))

(defvar js-indent-level);; silence compiler
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil
		  js-indent-level 2)
	    (idle-highlight-mode 1)
	    ;; https://alexschroeder.ch/wiki/2016-03-06_Javascript_Comments
	    (setq-local comment-auto-fill-only-comments t)
	    (setq-local comment-multi-line t)
	    (auto-fill-mode 1)
	    (setq c-comment-prefix-regexp "//+\\|/?\\**")
	    (let ((c-buffer-is-cc-mode t))
	      (c-setup-paragraph-variables))
	    (local-set-key (kbd "RET") 'c-indent-new-comment-line)
	    ;; (rainbow-identifiers-mode)
	    (when (and (buffer-file-name)
		       (string-match "\.svg$" (buffer-file-name)))
	      (save-excursion
		(goto-char (point-min))
		(when (search-forward "<script type=\"application/javascript\"><![CDATA[\n" nil t)
		  (narrow-to-region (point) (point-max)))))))

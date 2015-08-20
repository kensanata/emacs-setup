
(defvar js-indent-level);; silence compiler
(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil
		  js-indent-level 2)
	    (idle-highlight-mode 1)
	    ;; (rainbow-identifiers-mode)
	    (when (string-match "\.svg$" (buffer-file-name))
	      (save-excursion
		(goto-char (point-min))
		(when (search-forward "<script type=\"application/javascript\"><![CDATA[\n" nil t)
		  (narrow-to-region (point) (point-max)))))))

(add-hook 'typescript-mode-hook 'as:typescript-init)
(defun as:typescript-init ()
  (local-set-key (kbd "C-c t") 'sgml-tag)
  (setq typescript-indent-level 2
	indent-tabs-mode nil)
  (require 'grep)
  (grep-apply-setting 'grep-find-command
		      '("/cygwin64/bin/find . -type f -name '*.ts' -exec grep  -nH length {} \";\""
			. 54))
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (idle-highlight-mode 1))

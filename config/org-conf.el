(setq org-replace-disputed-keys t)
(add-hook 'org-mode-hook
	  'asc:org-mode-setup)
(defun asc:org-mode-setup ()
  (global-set-key (kbd "C-c <up>") 'org-move-subtree-up)
  (global-set-key (kbd "C-c <down>") 'org-move-subtree-down))

(add-hook 'bookmark-bmenu-mode-hook
	  (lambda ()
	    (local-set-key (kbd "TAB") 'next-line)
	    (local-set-key (kbd "<backtab>") 'previous-line)))

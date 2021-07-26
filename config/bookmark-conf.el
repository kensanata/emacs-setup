(add-hook 'bookmark-bmenu-mode-hook
	  (lambda ()
	    (local-set-key (kbd "TAB") #'next-line)
	    (local-set-key (kbd "<backtab>") #'previous-line)
	    (local-set-key "j" #'bookmark-jump)
	    (local-set-key "m" #'bookmark-bmenu-search)
	    (local-set-key "M" #'bookmark-bmenu-mark)))

(setq custom-theme-directory "~/.emacs.d/alex")
;; sadly, this needs to be updated whenever pink-bliss.el is canged.
(add-to-list 'custom-safe-themes
	     "502ea305ab80a0d0e4c2c64f8ffbf960c815e19f37d5717dac3fa4483fd5729b")
(load-theme 'pink-bliss)

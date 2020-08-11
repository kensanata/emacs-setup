(setq ps-print-header nil
      ps-print-footer nil
      ps-paper-type 'a4
      ps-font-family 'Iosevka
      ps-font-size 10)

(eval-after-load "ps-print"
  '(unless (assoc 'Iosevka ps-font-info-database)
     (setq ps-font-info-database
	   (append
	    '((Iosevka
	       (fonts (normal      . "Iosevka")
		      (bold        . "Iosevka-Bold")
		      (italic      . "Iosevka-Oblique")
		      (bold-italic . "Iosevka-BoldOblique"))
	       (size . 10.0)
	       (line-height . 12.5)
	       (space-width . 5.99906)
	       (avg-char-width . 5.99906)))
	    ps-font-info-database))))

(global-set-key (kbd "C-c p") 'ps-print-buffer)

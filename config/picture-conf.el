(require 'cl-lib)
;; getting rid of image-mode for svg files is hard
(setq auto-mode-alist
      (cl-delete-if (lambda (item)
		   (eq (cdr item) 'image-mode))
		 auto-mode-alist))

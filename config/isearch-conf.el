;; isearch occur no more: using swiper instead
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package swiper :ensure t
  :bind ("C-c o" . swiper-thing-at-point)
  :bind ("C-o" . swiper)
  :bind ("C-M-o" . swiper-all))

(define-key isearch-mode-map (kbd "C-o") 'swiper-from-isearch)

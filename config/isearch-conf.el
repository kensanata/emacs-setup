;; isearch occur no more: using swiper instead
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(define-key isearch-mode-map (kbd "C-o") 'swiper-from-isearch)

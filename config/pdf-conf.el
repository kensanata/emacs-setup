(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

(add-hook 'pdf-view-mode-hook 'pdf-isearch-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-history-minor-mode)
(add-hook 'pdf-view-mode-hook 'pdf-occur-global-minor-mode)

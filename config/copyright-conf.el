(use-package copyright
  :defer t
  :config
  (setq copyright-limit 500)
  (setq copyright-year-ranges t)
  (add-hook 'before-save-hook 'copyright-update))

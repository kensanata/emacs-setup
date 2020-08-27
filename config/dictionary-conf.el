;; I have my own
(setq dictionary-server "localhost")

(use-package dictionary
  :bind ("C-c d" . dictionary-lookup-definition))

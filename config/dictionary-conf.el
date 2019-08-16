;; I have my own
(setq dictionary-server "localhost")
(asc:package-install 'dictionary)
(global-set-key (kbd "C-c d") 'dictionary-lookup-definition)

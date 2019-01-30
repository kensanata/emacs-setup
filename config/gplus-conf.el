(add-to-list 'load-path "~/src/google-plus-to-oddmuse")

(autoload 'gplus-browse-archive "gplus-browser.el"
  "Browse G+ Archive" t)

(global-set-key (kbd "C-c a") 'gplus-browse-archive)

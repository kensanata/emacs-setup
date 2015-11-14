This repository contains my Emacs setup. It is installed as
`~/.emacs.d/alex`.

My init file (`~/.emacs.d/init.el`) looks as follows:

```
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "...")
 '(custom-safe-themes
   (quote
    ("..." default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; after custom-safe-themes
(load-file "~/.emacs.d/alex/init.el")
```

Basically we're not touching the `custom-set-variables` and the
`custom-set-faces` sections. All we're doing is load my own init file.
This init file will then load stuff in the [config](config) directory,
which in turn might load more Emacs libraries, or libraries I got from
elsewhere. These are stored in the [lib](lib) directory.

This repository contains my Emacs setup. It is installed as
`~/.emacs.d/alex`.

My init file (`~/.emacs.d/init.el`) starts with the following:

```
(load-file "~/.emacs.d/alex/init.el")
```

The idea is that the [config](config) subdirectory contains all the
configurations and that the [lib](lib) subdirectory contains all the
necessary libraries. When a library is available from ELPA or MELPA,
however, there should not be a copy in the `lib` subdirectory. These
libraries you need to install yourself (see below).

# Packages installed from ELPA/MELPA

Evaluate the following to install all the packages you need:

```
(dolist (pkg '(emms
	       expand-region
	       git-gutter
	       idle-highlight-mode
	       markdown-mode
	       session
	       swiper;; ivy-mode
	       ))
  (unless (package-installed-p pkg)
    (package-install pkg)))
```

# Optional other packages

* [oddmuse-curl](http://www.emacswiki.org/emacs?OddmuseCurl#toc1)

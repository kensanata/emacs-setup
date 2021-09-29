This repository contains my Emacs setup. It is installed as
`~/.emacs.d/emacs-setup`.

My init file (`~/.emacs.d/init.el`) starts with the following:

```
(load-file "~/.emacs.d/emacs-setup/init.el")
```

The idea is that the [config](config) subdirectory contains all the
configurations and that the [lib](lib) subdirectory contains all the
necessary libraries. When a library is available from ELPA or MELPA,
however, there should not be a copy in the `lib` subdirectory. These
libraries you need to install yourself (see below).

# Packages installed from ELPA/MELPA

The invocations of `use-package` should install all the other packages
from MELPA.

# Optional other packages

* [oddmuse-curl](http://www.emacswiki.org/emacs?OddmuseCurl#toc1)

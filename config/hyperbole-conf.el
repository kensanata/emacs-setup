(use-package hyperbole
  :ensure t
  :config
  (setq hbmap:dir-user "~/.emacs.d/hyperb"
        initial-buffer-choice (expand-file-name
                               hbmap:filename
                               hbmap:dir-user))
  (hyperbole-mode 1))

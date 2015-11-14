(require 'dired-x)

;; Homebrew: brew install coreutils gives us GNU ls
(add-to-list 'exec-path "/usr/local/opt/coreutils/libexec/gnubin")

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always
      dired-deletion-confirmer 'y-or-n-p
      dired-clean-up-buffers-too nil
      delete-by-moving-to-trash t
      ;; trash-directory "~/.Trash/emacs"
      dired-dwim-target t
      dired-guess-shell-alist-user '(("." "open"))
      dired-listing-switches "-alv")

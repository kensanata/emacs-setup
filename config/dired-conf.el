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
      ;; I installed Skim via Homebrew: brew search skim
      ;; and created a shell script as follows:
      ;; #!/bin/sh
      ;; osascript -e "tell application \"Skim\"" -e "open \"$(pwd)/$1\"" -e "end tell"
      dired-guess-shell-alist-user
      '(("\\.pdf\\'" "skim")
	("." "open"))
      dired-listing-switches "-alv")

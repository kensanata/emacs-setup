(setq desktop-restore-frames nil
      desktop-load-locked-desktop t
      ;; don't save remote files, don't save encrypted files
      desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\|\\.gpg\\'\\)"
      desktop-modes-not-to-save '(tags-table-mode dired-mode rmail-mode eww-mode))
(desktop-save-mode 1)

;; save minibuffer history
(savehist-mode 1)

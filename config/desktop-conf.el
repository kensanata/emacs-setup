(setq desktop-restore-frames nil
      desktop-load-locked-desktop t
      ;; don't save remote files, don't save encrypted files
      ;; desktop-files-not-to-save "\\(\\`/[^/:]*:\\|(ftp)\\'\\|\\.gpg\\'\\)"
      desktop-modes-not-to-save '(tags-table-mode dired-mode rmail-mode eww-mode Info-mode)
      desktop-files-not-to-save "")

(desktop-save-mode 1)

(add-to-list 'desktop-globals-to-save 'asc:file-names-seen)

;; save minibuffer history
(savehist-mode 1)

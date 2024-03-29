(setq desktop-restore-frames nil
      desktop-load-locked-desktop t
      ;; don't save any buffers
      desktop-modes-not-to-save '(tags-table-mode dired-mode rmail-mode eww-mode Info-mode)
      ;; don't save any files
      desktop-files-not-to-save "")

(desktop-save-mode 1)

(add-to-list 'desktop-globals-to-save 'asc:file-names-seen)

;; save minibuffer history
(savehist-mode 1)

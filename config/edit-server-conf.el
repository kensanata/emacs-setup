(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start)))))

  ;; :config (setq edit-server-new-frame-alist
  ;;               '((name . "Edit with Emacs FRAME")
  ;;                 (top . 200)
  ;;                 (left . 200)
  ;;                 (width . 80)
  ;;                 (height . 25)
  ;;                 (minibuffer . t)
  ;;                 (menu-bar-lines . t)
  ;;                 (window-system . x)))

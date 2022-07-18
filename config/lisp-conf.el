;; https://www.quicklisp.org/beta/

;; generated by (ql:quickload "quicklisp-slime-helper")
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook 'asc:lisp-init)

(defun asc:lisp-init ()
  (local-set-key (kbd "C-z") (lambda () (interactive) (switch-to-buffer "*slime-repl sbcl*"))))

(add-hook 'slime-repl-mode-hook 'asc:slime-repl-mode-init)

(defun asc:slime-repl-mode-init ()
  (local-set-key (kbd "C-z") #'bury-buffer))


;; load after installation, handled by (ql:add-to-init-file) adding stuff to ~/.sbclrc
;;     (load "~/quicklisp/setup.lisp")

;;     (ql:system-apropos "thing")
;;     (ql:quickload "thing")
;;     (ql:uninstall "thing")
;;     (ql:update-dist "quicklisp")
;;     (ql:update-client)

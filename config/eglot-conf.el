;; Note that LSP integration only works in a "project": a VCS
;; repository, or in the case of Marksman, a directory containing an
;; (empty) .marksman.toml file.

(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'perl-mode-hook 'eglot-ensure)
(add-hook 'markdown-mode-hook 'eglot-ensure)
(add-hook 'scheme-mode-hook 'eglot-ensure)
(autoload 'eglot-ensure "eglot" "Start Eglot session for current buffer if there isn’t one.")

(use-package eglot :ensure t
  :config (progn
            (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
            (add-to-list 'eglot-server-programs '(scheme-mode . ("chicken-lsp-server")))))

;; (custom-set-variables '(eglot-ignored-server-capabilites '(list :documentHighlightProvider :hoverProvider :signatureHelpProvider)))

;; (setq-default eglot-workspace-configuration
;;     '((:gopls .
;;         ((staticcheck . t)
;;          (matcher . "CaseSensitive")))))

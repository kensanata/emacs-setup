(asc:package-install 'idle-highlight-mode)

;; (autoload 'idle-highlight-mode "idle-highlight-mode" nil t)
(eval-after-load "idle-highlight"
  (lambda ()
    (set-face-background 'idle-highlight "lemon chiffon")))

;; https://bzg.fr/en/emacs-strip-tease.html/
;; http://bzg.fr/emacs-hide-mode-line.html

(defvar-local hide-mode-line nil
  "Store the model line if it is hidden.
If `hidden-mode-line-mode' is active, this variable contains the
value of `mode-line-format'. If the mode line is not hidden, this
variable is nil.")

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled. "
             (substitute-command-keys
	      "Use \\[hidden-mode-line-mode] to show it again.")))))

;; Use F12 to toggle modelines
(global-set-key (kbd "<f12>") 'hidden-mode-line-mode)

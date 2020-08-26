;; https://bzg.fr/en/emacs-strip-tease.html/
;; http://bzg.fr/emacs-hide-mode-line.html

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global nil
  :variable hidden-mode-line-mode
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
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; Activate hidden-mode-line-mode
(hidden-mode-line-mode 1)

;; If you want to hide the mode-line in all new buffers
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Command to toggle the display of the mode-line as a header
(defun mode-line-in-header ()
  (interactive)
  (cond ((not header-line-format)
	 (setq header-line-format (or hide-mode-line mode-line-format)
               mode-line-format nil))
	((not hide-mode-line)
	 (setq mode-line-format header-line-format
	       header-line-format nil))
	(t
	 (setq header-line-format nil)))
  (set-window-buffer nil (current-buffer)))

(global-set-key (kbd "<f12>") 'mode-line-in-header)

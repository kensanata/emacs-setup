;; default to case insensitive
(eval-after-load "grep"
  '(grep-apply-setting 'grep-command "rg --no-heading "))

(defun grep-word-at-point (word)
  "Grep for thing at point."
  (interactive (list (thing-at-point 'symbol)))
  (grep (format "rg --no-heading \"%s\" #" word)))

(global-set-key (kbd "C-c s") 'grep-word-at-point)

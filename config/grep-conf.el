(use-package "grep"
  ;; the null device is a problem if I want to use git grep instead of
  ;; grep and grep doesn't need it because my grep has -H support
  :config (grep-apply-setting 'grep-use-null-device nil))

(defun grep-word-at-point (word)
  "Grep for thing at point."
  (interactive (list (thing-at-point 'symbol)))
  (grep (format "rg --no-heading \"%s\" #" word)))

(global-set-key (kbd "C-c s") 'grep-word-at-point)

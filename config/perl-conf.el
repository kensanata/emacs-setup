(setq perl-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
(add-hook 'perl-mode-hook 'as/perl-init)

(defun as/perl-init ()
  (require 'cperl-mode);; for help
  (local-set-key (kbd "C-h f") 'cperl-perldoc)
  (setq fill-column 80)
  (set (make-local-variable 'eldoc-documentation-function)
       'as/cperl-eldoc-documentation-function)
  (idle-highlight-mode 1)
  (eldoc-mode 1))

(defun as/cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

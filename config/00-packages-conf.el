(when (require 'package nil t)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(when (fboundp 'package-install)
  (condition-case err
      (or (package-installed-p 'use-package)
	  (package-install 'use-package))
    ((error "message" format-args)
     (package-refresh-contents)
     (package-install 'use-package))))

;; We might have already added idle-highlight-mode to various hooks
;; and when we come here and discover that it isn't in fact already
;; installed, we have a problem: at least one of these hooks will be
;; called *during the installation* and prevent it! That's why we have
;; this fix.
(unless (fboundp 'idle-highlight-mode)
  (defun idle-highlight-mode ()))

(defun asc:package-install (pkg)
  "Install package.
If we're getting an error saying that the package is unavailable,
we'll refresh package content and try again. If we don't do this,
then the system will never work on a fresh install because the
actual package contents are missing and thus `package-install'
will never find anything."
  (when (fboundp 'package-install)
    (condition-case err
	(or (package-installed-p pkg)
	    (package-install pkg))
      ((error "message" format-args)
       (package-refresh-contents)
       (package-install pkg)))))

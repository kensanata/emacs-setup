;; Add MELPA
(require 'package)
(add-to-list 'package-archives
     	     '("melpa" . "https://melpa.org/packages/") t)

;; Make sure we have use-package installed
(condition-case err
    (or (package-installed-p 'use-package)
	(package-install 'use-package))
  ((error "message" format-args)
   (package-refresh-contents)
   (package-install 'use-package)))

;; Ensure all the packages we "use" are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

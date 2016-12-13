(asc:package-install 'company)
(global-company-mode 1)

;; support CamelCase as illustrated here:
;; https://github.com/company-mode/company-mode/issues/60
(setq company-dabbrev-downcase nil)

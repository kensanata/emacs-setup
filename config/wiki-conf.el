(use-package mediawiki :ensure t)

(setq mediawiki-site-alist
      ;; passwords are set via ~/.authinfo.gpg
      '(("Wikipedia" "https://en.wikipedia.org/w/" "Alex Schröder" nil nil "Main Page")
        ("Deutsche Wikipedia" "https://de.wikipedia.org/w/" "Alex Schröder" nil nil "Hauptseite")
        ("Alrik" "https://alrik.snafu.zone/" "Alex Schröder" nil nil "Alrik"))
      font-mediawiki-sedate-face 'italic
      font-mediawiki-italic-face 'italic
      font-mediawiki-bold-face 'bold
      font-mediawiki-math-face 'default
      font-mediawiki-string-face 'font-lock-string-face
      font-mediawiki-verbatim-face 'fixed)

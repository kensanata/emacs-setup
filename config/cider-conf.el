(when (file-directory-p (concat (getenv "HOME") "/.lein"))
  (setenv "LEIN_HOME" (concat (getenv "HOME") "/.lein")))
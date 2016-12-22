(message "%S" load-path)
(when (asc:package-install 'git-gutter)
  (require 'git-gutter)
  (global-git-gutter-mode +1))

(message "%S" load-path)
(asc:package-install 'git-gutter)
(require 'git-gutter)
(global-git-gutter-mode +1)

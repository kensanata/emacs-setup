;; default to case insensitive
(eval-after-load "grep"
  '(grep-apply-setting 'grep-command "grep --color -niH -e "))

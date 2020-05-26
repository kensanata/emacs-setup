(setenv "LANG" "en_GB.UTF-8")

(when (eq (window-system) 'w32)
  ;; (add-to-list 'dynamic-library-alist '(png "libpng15-15.dll"))
  ;; make sure cygwin comes first, for ssh
  (let ((path "C:/cygwin64/bin"))
    (setq exec-path (cons path (delete path exec-path))))
  (add-to-list 'exec-path "C:/Program Files (x86)/Growl for Windows")
  (setenv "RSH" "SSH")
  (setenv "CYGWIN" "nodosfilewarning")
  (prefer-coding-system 'utf-8)
  (setq default-file-name-coding-system 'cp1252)
  (setenv "CVS_RSH" "ssh")
  (setenv "WikiDataDir" "test-data")
  (eval-after-load "grep"
    '(grep-apply-setting 'grep-find-command
			 "c:/Portable\\ Programs/PortableGit/usr/bin/find.exe . -type f -exec grep.exe --color -niH -e  \"{}\" \";\""))
  (setq visible-bell t)
  (setq mouse-drag-copy-region t)
  (setq history-length 1000)
  (setq tramp-auto-save-directory (getenv "TEMP")))

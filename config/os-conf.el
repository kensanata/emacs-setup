(when (eq (window-system) 'w32)
  
  (add-to-list 'dynamic-library-alist '(png "libpng15-15.dll"))
  (add-to-list 'exec-path "C:/cygwin64/bin")
  (add-to-list 'exec-path "C:/Program Files (x86)/Growl for Windows")
  (setenv "RSH" "SSH")
  (setenv "LANG" "de_CH.UTF8")
  (setenv "CYGWIN" "nodosfilewarning")
  (prefer-coding-system 'utf-8)
  (setq default-file-name-coding-system 'cp1252)
  (setenv "CVS_RSH" "ssh")
  (setenv "WikiDataDir" "test-data")
  (setq visible-bell t)
  (setq mouse-drag-copy-region t)
  (setq history-length 1000)
  ;; Tramp
  ;; Download http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html
  ;; Docs http://the.earth.li/~sgtatham/putty/0.60/htmldoc/Chapter7.html#plink
  (setq tramp-auto-save-directory (getenv "TEMP"))
  ;; Printing
  (setq ps-printer-name "\\\\fileserver06\\Zuerich_PDF-Datei"))

(defun atreus-layout ()
  "If using the Atreus on a Mac, we want the Alt key to be Meta."
  (interactive)
  (setq mac-option-modifier 'meta)
  (setq default-input-method 'german-prefix)
  (set-input-method 'german-prefix))

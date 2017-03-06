;;; authinfo.el -- hiding unnecessary passwords
;; 
;; Copyright (C) 2017  Alex Schroeder <alex@gnu.org>
;; 
;; Latest version:
;; https://github.com/kensanata/emacs-setup/blob/master/config/authinfo-conf.el
;; 
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;; 
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; I keep some passwords in my ~/.authinfo.gpg file. When I look for a
;; password, I open the file and search for the correct host name
;; (i.e. the machine name). Sometimes there are people near my screen
;; and I don't feel comfortable looking for passwords in such an
;; environment, specially if there is an entire page full of them.
;; This mode is trying to limit such exposure.
;;
;; Things I might want to add in the future:
;; - hide passwords even if we show the line
;; - provide a keybinding to copy invisible passwords
;; - add a timer to wipe the clipboard containing the password after 10s

;;; Code

(eval-after-load "conf-mode"
  '(add-to-list 'conf-space-keywords-alist
		(cons "\\.authinfo"
		      (regexp-opt
		       '("machine"
			 "login"
			 "password"
			 "port"))))

(define-derived-mode authinfo-mode conf-space-mode "Auth Info"
  "Major mode to edit ~/.authinfo.gpg files."
  (authinfo-hide-buffer)
  (add-hook 'pre-command-hook 'authinfo-remember-position)
  (add-hook 'post-command-hook 'authinfo-show-line t t))

(defun authinfo-hide-buffer ()
  "Hide every line using the `display' text property."
  (save-excursion
    (goto-char (point-min))
    (let ((start (point))
	  (done nil))
      (while (not done)
	(put-text-property start (line-end-position)
			   'display "* SECRET *")
	(setq done (= (forward-line 1) 1)
	      start (point))))))

(defun authinfo-show-buffer ()
  "Show every line by removing the `display' text property."
  (save-excursion
    (goto-char (point-min))
    (let ((start (point))
	  (done nil))
      (while (not done)
	(remove-text-properties start (line-end-position) '(display))
	(setq done (= (forward-line 1) 1))))))

(defvar-local authinfo-last-position 0
  "The last position.
This is used to hide the old line.")

(defun authinfo-remember-position ()
  "Remember the current position to hide the old line."
  (setq authinfo-last-position (point)))
  
(defun authinfo-show-line ()
  "Hide the old line and show the new line, if on a new line."
  (when (or (< (line-end-position) authinfo-last-position)
	    (> (line-beginning-position) authinfo-last-position))
    (remove-text-properties (line-beginning-position)
			    (line-end-position)
			    '(display))
    (save-excursion
      (goto-char authinfo-last-position)
      (put-text-property (line-beginning-position)
			 (line-end-position)
			 'display "* SECRET *"))))

(provide 'authinfo)

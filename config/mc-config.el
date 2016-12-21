;; I have not decided whether iedit and multiple-cursors do the same
;; thing or not.
(asc:package-install 'iedit)
(asc:package-install 'multiple-cursors)
(asc:package-install 'mc-extras)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
(define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)
(define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)
(define-key rectangle-mark-mode-map (kbd "C-. C-,") 'mc/rect-rectangle-to-multiple-cursors)
(define-key cua--rectangle-keymap   (kbd "C-. C-,") 'mc/cua-rectangle-to-multiple-cursors)

(run-with-idle-timer 10 nil
		     (lambda ()
		       (require 'mc-extras)
		       (mc/cua-rectangle-setup)))

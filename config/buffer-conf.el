;;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Possible elements: mark modified read-only locked name size mode
;; process filename
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 40 40 :left :elide)
              " " filename-and-process)
        (mark " " (name 16 -1)
              " " filename)))


;; midnight
(midnight-mode 1)

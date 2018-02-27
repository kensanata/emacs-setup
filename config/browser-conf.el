;; I want to use eww.
(setq browse-url-browser-function 'eww-browse-url)

(defun browse-url-feh (url &rest args)
  "Browse URL at point using feh(1)."
  (interactive (browse-url-interactive-arg "Image URL: "))
  (async-shell-command (concat "feh " url)))

;; labels don't come in paragraphs
(eval-after-load "shr"
  '(defun shr-tag-label (dom)
     (shr-generic dom)))

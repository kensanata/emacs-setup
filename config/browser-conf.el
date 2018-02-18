;; I want to use eww.
(setq browse-url-browser-function 'eww-browse-url)

(defun browse-url-feh (url &rest args)
  "Browse URL at point using feh(1)."
  (interactive (browse-url-interactive-arg "Image URL: "))
  (async-shell-command (concat "feh " url)))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(add-to-list 'exec-path (expand-file-name "~/go/bin"))

(add-hook 'go-mode-hook 'asc:go-init)
(add-hook 'godoc-mode-hook 'asc:go-init)

(defun asc:go-init ()
  (setq fill-column 120)
  (local-set-key (kbd "C-h f") 'asc:godoc))

(defun asc:godoc ()
  "Run godoc on the symbol."
  (interactive)
  (let (from pkg func)
    (save-excursion
      (skip-chars-backward "[:alnum:].")
      (setq from (point))
      (skip-chars-forward "[:alnum:]")
      (setq pkg (buffer-substring from (point)))
      (skip-chars-forward "[:alnum:].")
      (setq func (buffer-substring from (point))))
    (when (and (eq major-mode 'godoc-mode)
               (string-match " \\([^*]+\\)" (buffer-name)))
      (setq pkg (match-string 1 (buffer-name))
            func (concat pkg "." func)))
    (cond ((string= pkg "")
           (error "No package at point"))
          ((string= pkg func)
           (godoc pkg))
          (t
           (godoc (completing-read "Which one: " (list pkg func) nil t func))))))

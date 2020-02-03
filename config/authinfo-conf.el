(setq auth-sources '("~/.authinfo.gpg"))

(autoload 'authinfo-copy-password "authinfo" "Copy password." t)

(defun qr-code-region (start end)
  "Show a QR code of the region.
A simple way to transfer text to the phone..."
  (interactive "r")
  (let ((buf (get-buffer-create "*QR*"))
	(inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))
    (let ((coding-system-for-read 'raw-text))
      (shell-command-on-region start end "qrencode -o -" buf))
    (switch-to-buffer buf)
    (image-mode)))

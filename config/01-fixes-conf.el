;; the default Emacs on OSX is doesn't know about window-system
(dolist (fun '(window-system find-font font-spec session))
  (unless (fboundp fun)
    (fset fun (lambda (&rest ignore)))))

(provide 'session)

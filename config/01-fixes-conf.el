;; the default Emacs on OSX is doesn't know about window-system
(dolist (fun '(window-system find-font font-spec))
  (unless (fboundp fun)
    (fset fun (lambda (&rest ignore)))))

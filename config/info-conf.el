(setq Info-additional-directory-list
      (append (list 
               "/usr/local/opt/emacs-mac/share/info/emacs"
               "~/src/emacs-live/packs/stable/git-pack/lib/magit/Documentation")
              (delete "/usr/local/opt/emacs-mac/share/info"
                      (file-expand-wildcards "/usr/local/opt/*/share/info"))))

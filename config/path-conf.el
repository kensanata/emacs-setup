;; local python binaries
(defun asc:add-to-env (var path)
  (let ((paths (parse-colon-path (getenv var))))
    (unless (member path paths)
      (setenv var (mapconcat 'identity (cons path paths) path-separator)))))

(defun asc:add-to-PATH (path)
  (setq path (expand-file-name path))
  (asc:add-to-env "PATH" path))

(asc:add-to-PATH "~/.local/bin/")

;; local man path: 
(setenv "MANPATH" ":/home/alex/perl5/man")

;; Perl 5
;; This is wrong: on sibirocobombus this bin is full of scripts wanting 5.24
;; (asc:add-to-PATH "~/perl5/bin/")
;; This is wrong: "ListUtil.c: loadable library and perl binaries are mismatched (got handshake key 0xd600000, needed 0xd880000)"
;; (setenv "PERL5LIB" "/home/alex/perl5/lib/perl5")

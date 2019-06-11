;; local python binaries
(defun asc:add-to-env (var path)
  (let ((paths (parse-colon-path (getenv var))))
    (unless (member path paths)
      (let (uniq)
	(dolist (elem paths)
	  (unless (member elem uniq)
	    (setq uniq (cons elem uniq))))
	(setenv var (mapconcat 'identity (cons path (nreverse uniq)) path-separator))))))

(defun asc:add-to-PATH (path)
  (setq path (expand-file-name path))
  (asc:add-to-env "PATH" path))

(asc:add-to-PATH "~/.local/bin")

;; local man path: 
(setenv "MANPATH" "/home/alex/perl5/perlbrew/perls/perl-5.28.1/man:/home/alex/perl5/man:/usr/local/man:/usr/local/share/man:/usr/share/man")

(defun mandb ()
  "Update the man database after installing new packgages for Perl.
The important part is that we run mandb for the current user, using the
current MANPATH environment variable."
  (interactive)
  (shell-command "mandb"))

;; Perl 5
;; This is wrong: on sibirocobombus this bin is full of scripts wanting 5.24
(asc:add-to-PATH "~/perl5/bin/")
(asc:add-to-PATH "~/perl5/perlbrew/perls/perl-5.28.1/bin")
;; This is wrong: "ListUtil.c: loadable library and perl binaries are mismatched (got handshake key 0xd600000, needed 0xd880000)"
(setenv "PERL5LIB" "/home/alex/perl5/lib/perl5")

;; Perl 6
(asc:add-to-PATH "~/rakudo/bin/")
(asc:add-to-PATH "~/rakudo/share/perl6/site/bin/")

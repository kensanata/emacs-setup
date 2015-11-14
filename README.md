This repository contains my Emacs setup. It is installed as
`~/.emacs.d/alex`.

My init file (`~/.emacs.d/init.el`) starts with the following:

```
(load-file "~/.emacs.d/alex/init.el")
```

== Security Issue? ==

There's one tricky issue in this setup: We're loading the
pink-bliss-theme. Ordinarily, Emacs would ask you whether you consider
it to be safe and store this information in in a setting that gets
loaded by `custom-set-variables`. If our code runes *before*
`custom-set-variables`, then Emacs will ask us every single time. If
our code runs *after* `custom-set-variables`, then Emacs will start
failing at some point in the future when you customize a variable. At
this point, Emacs will add `custom-set-variables`. Since our code runs
would be running *after* `custom-set-variables`, our `load-path`
doesn't allow Emacs to find `session.el` in its ELPA directory. And
thus it we're at a loss: both variants are not cool. So I've decided
to go for a third variant: In `pink-conf.el` I'm manually telling
Emacs that `pink-bliss-theme.el` is safe by adding the necessary hash
to `custom-safe-themes`.

If you know of a different way to solve this problem, let me know!

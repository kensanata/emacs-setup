(autoload 'mu-open "mu" "Play on MUSHes and MUDs" t)
(add-hook 'mu-connection-mode-hook 'ansi-color-for-comint-mode-on)
(setq mu-worlds
      '(["Lambda Moo" "lambda.moo.mud.org" 8888 "kensanata" "geZUJ"]))

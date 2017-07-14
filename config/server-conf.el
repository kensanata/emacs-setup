;; If we run with idle timer, then it doesn't matter if we run into an
;; error. The remaining init code still runs.
(run-with-idle-timer
 10 nil
 (lambda ()
   (server-start)))

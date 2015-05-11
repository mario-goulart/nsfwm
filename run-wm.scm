#! /bin/sh
#| # -*- scheme -*-
exec csi -s $0 "$@"
|#

(use nsfwm)

(let ((args (command-line-arguments)))
  (start-wm (and (not (null? args))
                 (car args))))

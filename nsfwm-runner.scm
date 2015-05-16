(module nsfwm-runner ()

(import chicken scheme)
(use files)
(use nsfwm)

(include "nsfwm-version.scm")

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage:
  #this [-h | --help]
  #this [-v | --version]
  #this [<config file>]

EOF
port)
    (when exit-code (exit exit-code))))

(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (when (or (member "-v" args)
            (member "--version" args))
    (print nsfwm-version)
    (exit 0))

  (start-wm (and (not (null? args))
                 (car args))))

) ;; end module

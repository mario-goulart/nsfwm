(module nsfwm-repl-cmd ()

(import scheme)
(import (chicken base)
        (chicken file)
        (chicken io)
        (chicken load)
        (chicken pathname)
        (chicken port)
        (chicken process-context))
(import linenoise simple-logger srfi-18 xlib)
(import nsfwm nsfwm-repl)

(include "nsfwm-version.scm")

(define DISPLAY (or (get-environment-variable "DISPLAY") ":0"))
(define dpy (xopendisplay DISPLAY))
(unless dpy
  (die! "Could not open display ($DISPLAY=~a)" DISPLAY))

(define screen (xdefaultscreen dpy))
(define root (xrootwindow dpy screen))

(load-history-from-file (repl-history-file))
(set-history-length! (repl-history-length))

(define (nsfwm-eval line)
  ;; This is extremely ugly, fragil, insecure, slow, subject to race
  ;; conditions and probably subject to other problems.  Eventually
  ;; this should be replaced by an implementation with Unix domain
  ;; sockets.
  (with-output-to-file eval-in
    (lambda ()
      (write `(begin ,@(with-input-from-string line read-list)))))
  (send-key root eval-key modifier: eval-mod dpy: dpy root: root)
  (let loop ()
    (if (file-exists? eval-done)
        (begin
          (display (with-input-from-file eval-out read-string))
          (delete-file* eval-done))
        (begin
          (thread-sleep! 0.05)
          (loop)))))

(define (mainloop)
  (let loop ()
    (let ((line (linenoise "#;> ")))
      (cond
       ((or (not line) (eof-object? line) (equal? line ",q"))
        (save-history-to-file (repl-history-file))
        (exit))
       (else
        (history-add line)
        (nsfwm-eval line)
        (loop))))))

(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage:
  #this [-h | --help]
  #this [-v | --version]
  #this [<options>]

<options>
-c|--config <config file>  Path to configuration file
-e|--expr <expr>           Scheme expression to be evaluated on the server
-f|--file <file>           File with Scheme code to be evaluated on the server

EOF
;;| This is to prevent Emacs' syntax highlighter from screwing up
port)
    (when exit-code (exit exit-code))))

(let ((config-file #f))
  (let loop ((args (command-line-arguments)))
    (unless (null? args)
      (let ((arg (car args)))
        (cond
         ((member arg '("-h" "-help" "--help"))
          (usage 0))
         ((member arg '("-v" "--version"))
          (print nsfwm-version)
          (exit 0))
         ((member arg '("-c" "--config"))
          (when (null? (cdr args))
            (die! "--config: Missing argument."))
          (set! config-file (cadr args))
          (loop (cddr args)))
         ((member arg '("-e" "--expr"))
          (when (null? (cdr args))
            (die! "--expr: Missing argument."))
          (nsfwm-eval (cadr args))
          (exit 0))
         ((member arg '("-f" "--file"))
          (when (null? (cdr args))
            (die! "--file: Missing argument."))
          (nsfwm-eval (with-input-from-file (cadr args) read-string))
          (exit 0))
         (else
          (die! "Invalid argument: ~a" arg))))))

  (when config-file
    (load config-file))

  (mainloop))

) ;; end module

(module nsfwm-repl

(enable-repl
 repl-history-file
 repl-history-length
 eval-in
 eval-out
 eval-done
 eval-mod
 eval-key)

(import scheme)
(import (chicken base)
        (chicken bitwise)
        (chicken condition)
        (chicken file)
        (chicken pathname)
        (chicken process-context))
(import xlib)
(import nsfwm)

(define (config-dir)
  (let ((cdir (make-pathname
               (or (get-environment-variable "XDG_CONFIG_HOME")
                   (list (get-environment-variable "HOME") ".config"))
               "nsfwm")))
    (create-directory cdir 'parents)
    cdir))

(define eval-state-dir
  (let ((state-dir #f))
    (lambda ()
      (unless state-dir
        (let* ((xdg-state-home (get-environment-variable "XDG_STATE_HOME")))
          (set! state-dir
                (make-pathname
                 (if xdg-state-home
                     (list xdg-state-home "nsfwm")
                     (list (get-environment-variable "HOME")
                           ".local"
                           "state"
                           "nsfwm"))
                 "eval"))
          (create-directory state-dir 'parents)))
      state-dir)))

(define repl-history-file
  (make-parameter
   (make-pathname (config-dir) "repl-history")))

(define repl-history-length
  (make-parameter 300))

(define eval-in (make-pathname (eval-state-dir) "in"))
(define eval-out (make-pathname (eval-state-dir) "out"))
(define eval-done (make-pathname (eval-state-dir) "done"))

(define eval-key "e")
(define eval-key-code XK_LCE)
(define eval-mod (bitwise-ior MOD1MASK MOD2MASK MOD3MASK MOD4MASK SHIFTMASK))

(define (nsfwm-server-eval)
  (with-output-to-file eval-out
    (lambda ()
      (handle-exceptions exn
        (begin
          (print-call-chain)
          (print-error-message exn))
        (display (eval (with-input-from-file eval-in read)))
        (newline))))
  (with-output-to-file eval-done (cut display "")))

(define (enable-repl)
  (bind-key! eval-mod eval-key-code nsfwm-server-eval))

) ;; end module

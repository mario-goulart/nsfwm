(module nsfwm

(start-wm

 ;; Keys
 global-keymap
 make-key

 ;; Focus
 focus-mode

 ;; Hooks
 add-hook!
 remove-hook!
 map-window-hook
 enter-workspace-hook

 ;; Screen
 screen-width
 screen-height

 ;; Windows
 window-exists?
 same-window?
 window-name
 all-windows
 get-window-by-id
 get-windows-by-name
 delete-window-by-id!
 add-window!
 window-visible?
 window-mapped?
 mapped-windows
 selected-window
 move-window!
 raise-window!
 root-window?
 select-next-window!
 hide-window!
 show-window!
 toggle-window-visibility!
 resize-window!
 window-maximized?
 maximize-window!
 unmaximize-window!
 toggle-maximize-window!
 destroy-window!
 window-corners
 windows-overlap?
 bump-window-right!
 bump-window-left!
 bump-window-up!
 bump-window-down!
 grow-window-right!
 grow-window-left!
 grow-window-up!
 grow-window-down!

 ;; window object
 window?
 window-id
 window-sticky?
 window-position-x
 window-position-y
 window-width
 window-height
 window-border-width
 window-border-color/selected
 window-border-color/unselected
 set-window-sticky?!
 set-window-cycle-skip?!

 ;; Window decoration
 default-window-border-width
 default-window-border-color/selected
 default-window-border-color/unselected
 set-window-decoration!

 ;; Workspaces
 num-workspaces
 set-num-workspaces!
 current-workspace
 workspace-windows
 window-in-workspace?
 switch-to-workspace!
 add-window-to-workspace!
 remove-window-from-workspace!
 move-window-to-workspace!
 find-window-in-workspaces

 ;; Pointer
 warp-pointer!
 )

(import chicken scheme foreign)
(use data-structures extras irregex lolevel ports posix (srfi 1 4))
(use matchable
     (rename xlib
             (screen-width xscreen-width)
             (screen-height xscreen-height)))


;; Horrible hack.  The xlib egg doesn't bind XSetErrorHandler, so we
;; implement an error handler in C.  It just ignores errors.
(foreign-declare "#include <X11/Xlib.h>")
(foreign-code "
int ignore_xerror(Display *dpy, XErrorEvent *e){ return 0; }
XSetErrorHandler(ignore_xerror);
")

;;; Basic globals
(define dpy #f)
(define root #f)
(define screen #f)
(define selected #f)

(define workspaces-hidden #f)
(define workspaces #f)
(define num-workspaces 1)
(define current-workspace 0)


;;; Configurable parameters

(define global-keymap
  (make-parameter '()))

(define focus-mode
  (make-parameter 'click))

(define map-window-hook
  (make-parameter '()))

(define enter-workspace-hook
  (make-parameter '()))

(define default-window-border-width
  (make-parameter 3))

(define default-window-border-color/selected
  (make-parameter "#55aaaa"))

(define default-window-border-color/unselected
  (make-parameter "#9eeeee"))


;;; Hooks

(define (run-hooks! hooks-param . args)
  (for-each (lambda (hook-id/proc)
              (let ((hook-proc (cadr hook-id/proc)))
                (apply hook-proc args)))
            (hooks-param)))

(define (add-hook! hook-param hook-id hook-proc)
  (hook-param
   (cons (list hook-id hook-proc)
         (hook-param))))

(define (remove-hook! hooks-param hook-id)
  (hooks-param
   (remove (lambda (hook-id/proc)
             (eq? hook-id (car hook-id/proc)))
           (hooks-param))))

;;; Screen
(define (screen-width)
  (xscreen-width (xdefaultscreenofdisplay dpy)))

(define (screen-height)
  (xscreen-height (xdefaultscreenofdisplay dpy)))


;;; Windows

(define-record window
  id
  sticky?
  cycle-skip?
  position-x
  position-y
  orig-position-x
  orig-position-y
  height
  width
  orig-height
  orig-width
  border-width
  border-color/selected
  border-color/unselected)

(define-record-printer (window obj out)
  (fprintf
   out
   "#<window id: ~a name: ~S x: ~a y: ~a width: ~a height: ~a sticky?: ~a cycle-skip?: ~a>"
   (window-id obj)
   (window-name obj)
   (window-position-x obj)
   (window-position-y obj)
   (window-width obj)
   (window-height obj)
   (window-sticky? obj)
   (window-cycle-skip? obj)))

(define %make-window make-window)

(define (make-window window-id)
  (%make-window window-id
                #f #f #f #f #f #f #f #f #f #f
                (default-window-border-width)
                (default-window-border-color/selected)
                (default-window-border-color/unselected)))

(define (set-window-sticky?! window yes?)
  (if yes?
      (let loop ((workspace 0))
        (unless (fx= workspace num-workspaces)
          (add-window-to-workspace! window workspace)
          (loop (fx+ workspace 1))))
      (when (window-sticky? window)
        (let loop ((workspace 0))
          (unless (fx= workspace num-workspaces)
            (unless (fx= workspace current-workspace)
              (remove-window-from-workspace! window workspace))
            (loop (fx+ workspace 1))))))
  (window-sticky?-set! window yes?))

(define (set-window-cycle-skip?! window yes?)
  (let ((wss (find-window-in-workspaces window)))
    (for-each
     (lambda (workspace)
       (if yes?
           (%move-window-to-uncyclable-stack! window workspace)
           (%move-window-to-cyclable-stack! window workspace)))
     wss))
  (window-cycle-skip?-set! window yes?))

(define (window-position-set! window x y)
  (window-position-x-set! window x)
  (window-position-y-set! window y))

(define *windows* '())

(define (window-name window)
  (let-location ((window-name c-string*))
    (if (fx= (xfetchname dpy (window-id window) (location window-name)) 0)
        #f
        (let ((window-name window-name))
          window-name))))

(define (all-windows)
  (map cdr *windows*))

(define (get-window-by-id id)
  (alist-ref id *windows* fx=))

(define (get-windows-by-name str/regex)
  (let ((matcher
         (cond ((string? str/regex) string=?)
               ((irregex? str/regex) irregex-match)
               (else
                (debug "get-windows-by-name: invalid object ~a" str/regex)
                #f))))
    (if matcher
        (let loop ((windows (all-windows)))
          (if (null? windows)
              '()
              (let* ((window (car windows))
                     (wname (window-name window)))
                (if wname
                    (let ((maybe-match (matcher str/regex wname)))
                      (if (or (irregex-match-data? maybe-match)
                              maybe-match)
                          (cons window (loop (cdr windows)))
                          (loop (cdr windows))))
                    (loop (cdr windows))))))
        '())))

(define (delete-window-by-id! id)
  (set! *windows* (alist-delete! id *windows* equal?)))

(define (add-window! id)
  (let ((window (make-window id)))
    (set! *windows* (alist-update id window *windows* equal?))
    window))

(define (window-exists? id)
  (and (get-window-by-id id) #t))

(define (same-window? w1 w2)
  (and w1 w2
       (fx= (window-id w1) (window-id w2))))

(define (selected-window)
  (get-window-by-id selected))

(define window-visible?
  (let ((wa (make-xwindowattributes)))
    (lambda (window)
      (let ((id (window-id window)))
        (xgetwindowattributes dpy id wa)
        (fx= (xwindowattributes-map_state wa) ISVIEWABLE)))))

(define (window-mapped? window)
  (window-visible? window))

(define (mapped-windows)
  (filter window-mapped? (all-windows)))

(define (move-window! window x y)
  (window-position-set! window x y)
  (xmovewindow dpy (window-id window) x y))

(define (raise-window! window)
  (xraisewindow dpy (window-id window)))

(define (root-window? window)
  (and (integer? window) (fx= window root)))

(define (set-window-decoration! window
                                #!key border-width
                                      border-color/selected
                                      border-color/unselected)
  (when border-width
    (window-border-width-set! window border-width))
  (when border-color/selected
    (window-border-color/selected-set! window border-color/selected))
  (when border-color/unselected
    (window-border-color/unselected-set! window border-color/unselected))
  (let ((wid (window-id window)))
    (xsetwindowborderwidth dpy wid (window-border-width window))
    (xsetwindowborder dpy wid (get-color
                               (window-border-color/unselected window)))))

(define (hide-window! window)
  (%move-window-to-uncyclable-stack! window current-workspace)
  (xunmapwindow dpy (window-id window)))

(define (show-window! window)
  (unless (window-cycle-skip? window)
    (%move-window-to-cyclable-stack! window current-workspace))
  (xmapwindow dpy (window-id window)))

(define (toggle-window-visibility! window)
  (if (window-visible? window)
      (hide-window! window)
      (show-window! window)))

(define (resize-window! window width height)
  (let* ((wid (window-id window))
         (border-width (fx* 2 (window-border-width window)))
         (new-width (fx- width border-width))
         (new-height (fx- height border-width)))
    (when (and (fx> new-width 0) (fx> new-height 0))
      (window-width-set! window width)
      (window-height-set! window height)
      (xresizewindow dpy wid new-width new-height))))

(define (maximize-window! window)
  (let ((root-info (x-get-geometry dpy root)))
    ;; Save current window geometry in the window object itself
    (window-orig-position-x-set! window (window-position-x window))
    (window-orig-position-y-set! window (window-position-y window))
    (window-orig-width-set! window (window-width window))
    (window-orig-height-set! window (window-height window))
    ;; Actually maximize
    (resize-window! window
                    (x-get-geometry-info-width root-info)
                    (x-get-geometry-info-height root-info))
    (move-window! window 0 0)))

(define (unmaximize-window! window)
  ;; Resize and move
  (resize-window! window
                  (window-orig-width window)
                  (window-orig-height window))
  (move-window! window
                (window-orig-position-x window)
                (window-orig-position-y window))
  ;; Reset orig geometry, so window-maximize? knows whether the
  ;; window is maximized or not
  (window-orig-position-x-set! window #f)
  (window-orig-position-y-set! window #f)
  (window-orig-width-set! window #f)
  (window-orig-height-set! window #f))

(define (window-maximized? window)
  ;; When the orig geometry atributes are #f, window is not maximized.
  (window-orig-position-x window))

(define (toggle-maximize-window! window)
  (if (window-maximized? window)
      (unmaximize-window! window)
      (maximize-window! window)))

(define (destroy-window! window)
  (let ((workspaces (find-window-in-workspaces window)))
    (for-each (lambda (workspace)
                (remove-window-from-workspace! window workspace))
              workspaces)
    (delete-window-by-id! (window-id window))
    (xdestroywindow dpy (window-id window))))

(define (window-corners window)
  ;; Return (top-left-x top-left-y bottom-right-x bottom-right-y)
  (let ((top-left-x (window-position-x window))
        (top-left-y (window-position-y window)))
    (list top-left-x
          top-left-y
          (fx+ top-left-x (window-width window))
          (fx+ top-left-y (window-height window)))))

(define (window-on-the-left? w1-rx w2-lx)
  ;; w1 on the left of w2? (without overlapping)
  (fx< w1-rx w2-lx))

(define (window-on-the-right? w1-lx w2-rx)
  ;; w1 on the right of w2? (without overlapping)
  (fx> w1-lx w2-rx))

(define (window-above? w1-ry w2-ly)
  ;; w1 above w2? (without overlapping)
  (fx< w1-ry w2-ly))

(define (window-below? w1-ly w2-ry)
  ;; w1 below w2? (without overlapping)
  (fx> w1-ly w2-ry))

(define (windows-overlap? w1-corners w2-corners)
  (match-let (((w1-lx w1-ly w1-rx w1-ry) w1-corners)
              ((w2-lx w2-ly w2-rx w2-ry) w2-corners))
    (not (or (window-on-the-left? w1-rx w2-lx)
             (window-on-the-right? w1-lx w2-rx)
             (window-above? w1-ry w2-ly)
             (window-below? w1-ly w2-ry)))))

(define (find-closest-window-x-right window)
  (match-let* ((w1-corners (window-corners window))
               ((w1-lx w1-ly w1-rx w1-ry) w1-corners)
               (w1-width (window-width window))
               (windows-on-the-way
                (let loop ((windows (workspace-windows current-workspace)))
                  (if (null? windows)
                      '()
                      (match-let* ((w2 (car windows))
                                   (w2-corners (window-corners w2))
                                   ((w2-lx w2-ly w2-rx w2-ry) w2-corners))
                        (if (or (same-window? window w2)
                                (windows-overlap? w1-corners w2-corners)
                                (window-above? w1-ry w2-ly)
                                (window-below? w1-ly w2-ry)
                                (window-on-the-left? w2-lx w1-rx))
                            (loop (cdr windows))
                            (cons w2 (loop (cdr windows))))))))
               (closest-window-x
                (if (null? windows-on-the-way)
                    (screen-width)
                    (apply min (map window-position-x windows-on-the-way)))))
    closest-window-x))

(define (find-closest-window-x-left window)
  (match-let* ((w1-corners (window-corners window))
               ((w1-lx w1-ly w1-rx w1-ry) w1-corners)
               (w1-width (window-width window))
               (windows-on-the-way
                (let loop ((windows (workspace-windows current-workspace)))
                  (if (null? windows)
                      '()
                      (match-let* ((w2 (car windows))
                                   (w2-corners (window-corners w2))
                                   ((w2-lx w2-ly w2-rx w2-ry) w2-corners))
                        (if (or (same-window? window w2)
                                (windows-overlap? w1-corners w2-corners)
                                (window-above? w1-ry w2-ly)
                                (window-below? w1-ly w2-ry)
                                (window-on-the-right? w2-lx w1-rx))
                            (loop (cdr windows))
                            (cons w2 (loop (cdr windows))))))))
               (closest-window-x
                (if (null? windows-on-the-way)
                    0
                    (apply max (map (lambda (w)
                                      (fx+ (window-position-x w)
                                           (window-width w)))
                                    windows-on-the-way)))))
    closest-window-x))

(define (find-closest-window-y-above window)
  (match-let* ((w1-corners (window-corners window))
               ((w1-lx w1-ly w1-rx w1-ry) w1-corners)
               (w1-height (window-width window))
               (windows-on-the-way
                (let loop ((windows (workspace-windows current-workspace)))
                  (if (null? windows)
                      '()
                      (match-let* ((w2 (car windows))
                                   (w2-corners (window-corners w2))
                                   ((w2-lx w2-ly w2-rx w2-ry) w2-corners))
                        (if (or (same-window? window w2)
                                (windows-overlap? w1-corners w2-corners)
                                (window-on-the-left? w1-rx w2-lx)
                                (window-on-the-right? w1-lx w2-rx)
                                (window-above? w1-ly w2-ry))
                            (loop (cdr windows))
                            (cons w2 (loop (cdr windows))))))))
               (closest-window-y
                (if (null? windows-on-the-way)
                    0
                    (apply max (map (lambda (w)
                                      (fx+ (window-position-y w)
                                           (window-height w)))
                                    windows-on-the-way)))))
    closest-window-y))

(define (find-closest-window-y-below window)
  (match-let* ((w1-corners (window-corners window))
               ((w1-lx w1-ly w1-rx w1-ry) w1-corners)
               (w1-width (window-height window))
               (windows-on-the-way
                (let loop ((windows (workspace-windows current-workspace)))
                  (if (null? windows)
                      '()
                      (match-let* ((w2 (car windows))
                                   (w2-corners (window-corners w2))
                                   ((w2-lx w2-ly w2-rx w2-ry) w2-corners))
                        (if (or (same-window? window w2)
                                (windows-overlap? w1-corners w2-corners)
                                (window-on-the-left? w1-rx w2-lx)
                                (window-on-the-right? w1-lx w2-rx)
                                (window-below? w1-ry w2-ly))
                            (loop (cdr windows))
                            (cons w2 (loop (cdr windows))))))))
               (closest-window-y
                (if (null? windows-on-the-way)
                    (screen-height)
                    (apply min (map window-position-y windows-on-the-way)))))
    closest-window-y))

(define (bump-window-right! window)
  (move-window! window
                (fx- (find-closest-window-x-right window)
                     (window-width window))
                (window-position-y window)))

(define (bump-window-left! window)
  (move-window! window
                (find-closest-window-x-left window)
                (window-position-y window)))

(define (bump-window-up! window)
  (move-window! window
                (window-position-x window)
                (find-closest-window-y-above window)))

(define (bump-window-down! window)
  (move-window! window
                (window-position-x window)
                (fx- (find-closest-window-y-below window)
                     (window-height window))))

(define (grow-window-right! window)
  (let* ((closest-x (find-closest-window-x-right window))
         (new-width (fx- closest-x (window-position-x window))))
    (resize-window! window new-width (window-height window))))

(define (grow-window-left! window)
  (let* ((closest-x (find-closest-window-x-left window))
         (new-width (fx+ (window-width window)
                         (fx- (window-position-x window)
                              closest-x))))
    (resize-window! window new-width (window-height window))
    (move-window! window closest-x (window-position-y window))))

(define (grow-window-up! window)
  (let* ((closest-y (find-closest-window-y-above window))
         (new-height (fx+ (window-height window)
                          (fx- (window-position-y window)
                               closest-y))))
    (resize-window! window (window-width window) new-height)
    (move-window! window (window-position-x window) closest-y)))

(define (grow-window-down! window)
  (let* ((closest-y (find-closest-window-y-below window))
         (new-height (fx- closest-y (window-position-y window))))
    (resize-window! window (window-width window) new-height)))


;;; Window cycling
(define (%cycle-windows! workspace upwards?)
  (let ((stack (workspace-cyclable-windows workspace)))
    (if (or (null? stack) (null? (cdr stack)))
        stack
        (let ((new-stack (if upwards?
                             (append (cdr stack)
                                     (list (car stack)))
                             (cons (last stack)
                                   (butlast stack)))))
          (workspace-cyclable-windows-set! workspace new-stack)
          new-stack))))

(define (cycle-windows-upwards! workspace)
  (%cycle-windows! workspace #t))

(define (cycle-windows-downwards! workspace)
  (%cycle-windows! workspace #f))

(define (select-next-window! #!optional (cycler! cycle-windows-upwards!))
  (let ((stack (cycler! current-workspace)))
    (unless (null? stack)
      (let ((next-window (car stack)))
        (xraisewindow dpy (window-id next-window))
        (focus-window! next-window)))))

(define (window-depth window workspace)
  (let ((wid (window-id window)))
    (list-index (lambda (w)
                  (fx= (window-id w) wid))
                (workspace-windows workspace))))


;;; Workspaces

(define-record workspace uncyclable-windows cyclable-windows)

(define %workspace-cyclable-windows workspace-cyclable-windows)
(define %workspace-cyclable-windows-set! workspace-cyclable-windows-set!)
(define %workspace-uncyclable-windows workspace-uncyclable-windows)
(define %workspace-uncyclable-windows-set! workspace-uncyclable-windows-set!)

(define (workspace-windows workspace)
  (let ((ws (vector-ref workspaces workspace)))
    (append (%workspace-cyclable-windows ws)
            (%workspace-uncyclable-windows ws))))

(define (workspace-cyclable-windows workspace)
  (%workspace-cyclable-windows (vector-ref workspaces workspace)))

(define (workspace-uncyclable-windows workspace)
  (%workspace-uncyclable-windows (vector-ref workspaces workspace)))

(define (workspace-cyclable-windows-set! workspace cyclable-windows)
  (let ((ws (vector-ref workspaces workspace)))
    (%workspace-cyclable-windows-set! ws cyclable-windows)))

(define (workspace-uncyclable-windows-set! workspace uncyclable-windows)
  (let ((ws (vector-ref workspaces workspace)))
    (%workspace-uncyclable-windows-set! ws uncyclable-windows)))

(define (%move-window-to-uncyclable-stack! window workspace)
  (let* ((ws (vector-ref workspaces workspace))
         (wid (window-id window))
         (cyclable (%workspace-cyclable-windows ws))
         (uncyclable (%workspace-uncyclable-windows ws)))
    (when (member window cyclable same-window?)
      (%workspace-cyclable-windows-set!
       ws
       (remove (lambda (w)
                 (fx= wid (window-id w)))
               cyclable)))
    (unless (member window uncyclable same-window?)
      (%workspace-uncyclable-windows-set!
       ws
       (cons window uncyclable)))))

(define (%move-window-to-cyclable-stack! window workspace)
  (let* ((ws (vector-ref workspaces workspace))
         (wid (window-id window))
         (cyclable (%workspace-cyclable-windows ws))
         (uncyclable (%workspace-uncyclable-windows ws)))
    (when (member window uncyclable same-window?)
      (%workspace-uncyclable-windows-set!
       ws
       (remove (lambda (w)
                 (fx= wid (window-id w)))
               uncyclable)))
    (unless (member window cyclable same-window?)
      (%workspace-cyclable-windows-set!
       ws
       (cons window cyclable)))))

(define (set-num-workspaces! n)
  (if workspaces ;; workspaces have been initialized before
      (let ((cur-len (vector-length workspaces)))
        (debug "cur workspaces len: ~a" cur-len)
        (if (< n cur-len)
            (begin
              ;; Move all windows to the last workspace
              (let loop ((workspace-to-remove (fx- n 1)))
                (unless (fx= workspace-to-remove cur-len)
                  (for-each
                   (lambda (window)
                     (move-window-to-workspace! window
                                                (fx- n 1)))
                   (workspace-windows workspace-to-remove))))
              (set! workspaces
                    (vector-resize workspaces n)))
            (begin
              (set! workspaces
                    (vector-resize workspaces n #f))
              (let loop ((i cur-len))
                (when (fx< i n)
                  (vector-set! workspaces i (make-workspace '() '()))
                  (loop (fx+ i 1)))))))
      (begin
        (set! workspaces-hidden (make-vector n #f))
        (set! workspaces (make-vector n #f))
        (let loop ((i 0))
          (when (fx< i n)
            (vector-set! workspaces-hidden i (make-workspace '() '()))
            (vector-set! workspaces i (make-workspace '() '()))))))
  (set! num-workspaces n))

(define (switch-to-workspace! workspace)
  (for-each hide-window! (mapped-windows))
  (set! current-workspace workspace)
  (when (fx< workspace num-workspaces)
    (for-each show-window! (workspace-windows workspace))
    (run-hooks! enter-workspace-hook workspace)))

(define (add-window-to-workspace! window workspace)
  (if (or (window-cycle-skip? window)
          (not (window-visible? window)))
      (workspace-uncyclable-windows-set!
       workspace
       (cons window (workspace-uncyclable-windows workspace)))
      (workspace-cyclable-windows-set!
       workspace
       (cons window (workspace-cyclable-windows workspace))))
  (when (fx= workspace current-workspace)
    (show-window! window)))

(define (remove-window-from-workspace! window workspace)
  (let ((wid (window-id window)))
    (if (or (window-cycle-skip? window)
            (not (window-visible? window)))
        (workspace-uncyclable-windows-set!
         workspace
         (remove (lambda (w)
                   (fx= (window-id w) wid))
                 (workspace-uncyclable-windows workspace)))
        (workspace-cyclable-windows-set!
         workspace
         (remove (lambda (w)
                   (fx= (window-id w) wid))
                 (workspace-cyclable-windows workspace))))
    (when (fx= workspace current-workspace)
      (hide-window! window))))

(define (move-window-to-workspace! window workspace #!optional from)
  (remove-window-from-workspace! window (or from current-workspace))
  (add-window-to-workspace! window workspace))

(define (window-in-workspace? window workspace)
  (let ((wid (window-id window)))
    (let loop ((windows (workspace-windows workspace)))
      (if (null? windows)
          #f
          (let ((w (car windows)))
            (if (fx= (window-id w) wid)
                #t
                (loop (cdr windows))))))))

(define (find-window-in-workspaces window)
  (let loop ((workspace 0))
    (if (fx= workspace num-workspaces)
        '()
        (if (window-in-workspace? window workspace)
            (cons workspace (loop (fx+ workspace 1)))
            (loop (fx+ workspace 1))))))

;; Pointer

(define (warp-pointer! x y)
  (xwarppointer dpy NONE root 0 0 0 0 x y))


;; Utils

(define (debug fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (flush-output (current-error-port)))


;; intermediate glue

(define LASTEvent 35)
(define True 1)
(define False 0)

(define-record x-get-geometry-info root x y width height border-width depth)

(define (x-get-geometry dpy id)
  (let-location ((root         unsigned-long)
                 (x            unsigned-int32)
                 (y            unsigned-int32)
                 (width        unsigned-int32)
                 (height       unsigned-int32)
                 (border-width unsigned-int32)
                 (depth        unsigned-int32))
    (xgetgeometry dpy
                  id
                  (location root)
                  (location x)
                  (location y)
                  (location width)
                  (location height)
                  (location border-width)
                  (location depth))
    (make-x-get-geometry-info root x y width height border-width depth)))

(define-record x-query-tree-info root parent children)

(define (x-query-tree dpy id)
  (let-location ((root      unsigned-int32)
                 (parent    unsigned-int32)
                 (children  (c-pointer u32vector))
                 (nchildren unsigned-int32))
    (xquerytree dpy id (location root) (location parent) (location children) (location nchildren))
    (let ((kids (make-blob (* 4 nchildren)))
          (memcpy (foreign-lambda bool "C_memcpy" blob c-pointer integer)))
      (memcpy kids children (* nchildren 4))
      (xfree children)
      (make-x-query-tree-info root parent (u32vector->list (blob->u32vector kids))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define move-cursor   #f)
(define resize-cursor #f)

(define click-root-window   'click-root-window)
(define click-client-window 'click-client-window)

(define handlers (make-vector LASTEvent #f))

(define-record button target mask button procedure)

(define-record key mod keysym procedure)

(define num-lock-mask 0)

(define (clean-mask mask)
  (bitwise-and mask (bitwise-not (bitwise-ior num-lock-mask LOCKMASK))))

(define +button-mask+ (bitwise-ior BUTTONPRESSMASK BUTTONRELEASEMASK))

(define +mouse-mask+ (bitwise-ior +button-mask+ POINTERMOTIONMASK))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mod-key MOD1MASK)

(define use-grab #f)

(define buttons #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-color
  (let ((color (make-xcolor)))
    (lambda (name)
      (let ((colormap (xdefaultcolormap dpy screen)))
        (xallocnamedcolor dpy colormap name color color)
        (xcolor-pixel color)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grab-buttons window focused)
  (xungrabbutton dpy ANYBUTTON ANYMODIFIER window)
  (when focused
    (for-each
     (lambda (b)
       (if (eq? (button-target b) click-client-window)
           (for-each
            (lambda (modifier)
              (xgrabbutton dpy
                           (button-button b)
                           (bitwise-ior (button-mask b) modifier)
                           window
                           0 +button-mask+ GRABMODEASYNC GRABMODESYNC NONE
                           NONE))
            (list 0 LOCKMASK num-lock-mask
                  (bitwise-ior num-lock-mask LOCKMASK)))
           (xgrabbutton dpy ANYBUTTON ANYMODIFIER window False +button-mask+
                        GRABMODEASYNC GRABMODESYNC NONE NONE)))
     buttons)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grab-keys)
  (xungrabkey dpy ANYKEY ANYMODIFIER root)
  (for-each
   (lambda (k)
     (let ((code (xkeysymtokeycode dpy (key-keysym k))))
       ;; Kludge for now. Some FFIs return a Scheme char, others a number.
       (let ((code (if (char? code) (char->integer code) code)))
         (for-each
          (lambda (modifier)
            (xgrabkey dpy code (bitwise-ior (key-mod k) modifier)
                      root 1 GRABMODEASYNC GRABMODEASYNC))
          (list 0 LOCKMASK num-lock-mask (bitwise-ior num-lock-mask LOCKMASK))))))
   (global-keymap)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (key-press ev)
  (let ((keysym (xkeycodetokeysym dpy (xkeyevent-keycode ev) 0))) ; index is 1 for shifted keys (as XK_P) 0 for lower case keys (XK_LCP)
    (let ((key (find (lambda (k)
                       (and (fx= (key-keysym k) keysym)
                            (fx= (clean-mask (key-mod k))
                                 (clean-mask (xkeyevent-state ev)))))
                     (global-keymap))))
      (debug "Key code ~A pressed event ~A (P should be ~A) list  ~A keyevent-state ~A -> found ~a"
             (xkeyevent-keycode ev)
             keysym XK_P
             (map (lambda(k) `(,(key-keysym k) ,(clean-mask (key-mod k))))
                  (global-keymap))
             (clean-mask (xkeyevent-state ev)) key)
      (when key ((key-procedure key))))))

(vector-set! handlers KEYPRESS key-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enter-notify ev)
  (when (eq? (focus-mode) 'enter-exit)
    (cond ((and (not (fx= (xcrossingevent-mode ev) NOTIFYNORMAL))
                (not (fx= (xcrossingevent-window ev) root)))
           (debug "  enter-notify : mode is not NOTIFYNORMAL"))
          ((and (fx= (xcrossingevent-detail ev) NOTIFYINFERIOR)
                (not (fx= (xcrossingevent-window ev) root)))
           (debug "  enter-notify : detail is NOTIFYINFERIOR"))
          ((get-window-by-id (xcrossingevent-window ev)) =>
           (lambda (window)
             (focus-window! window)
             (raise-window! window)))
          (else (focus-window! root)))))

(vector-set! handlers ENTERNOTIFY enter-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-focus! ev)
  (when (and (integer? selected)
             (not (fx= (xfocuschangeevent-window ev) selected)))
    (xsetinputfocus dpy selected REVERTTOPOINTERROOT CURRENTTIME)))

(vector-set! handlers FOCUSIN set-focus!)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-window! id)
  (xsetwindowborderwidth dpy id (default-window-border-width))
  (xsetwindowborder dpy id (get-color (default-window-border-color/unselected)))
  (xselectinput dpy id (bitwise-ior ENTERWINDOWMASK
                                    FOCUSCHANGEMASK
                                    PROPERTYCHANGEMASK
                                    STRUCTURENOTIFYMASK))
  (grab-buttons id #f)
  (let ((window (add-window! id)))
    (xmapwindow dpy id)
    (let* ((info (x-get-geometry dpy id))
           (x (x-get-geometry-info-x info))
           (y (x-get-geometry-info-y info))
           (width (fx+ (x-get-geometry-info-width info)
                       (default-window-border-width)))
           (height (fx+ (x-get-geometry-info-height info)
                        (default-window-border-width))))
      (window-position-set! window x y)
      (window-width-set! window width)
      (window-height-set! window height))
    (add-window-to-workspace! window current-workspace)
    (run-hooks! map-window-hook window))
  (xsync dpy False))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-request
  (let ((wa (make-xwindowattributes)))
    (lambda (ev)
      (let ((id (xmaprequestevent-window ev)))
        (when (and (not (fx= (xgetwindowattributes dpy id wa) 0))
                   (fx= (xwindowattributes-override_redirect wa) 0)
                   (not (get-window-by-id id)))
          (map-window! id))))))

(vector-set! handlers MAPREQUEST map-request)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define configure-request
  (let ((wc (make-xwindowchanges)))
    (lambda (ev)
      (set-xwindowchanges-x!             wc (xconfigurerequestevent-x            ev))
      (set-xwindowchanges-y!             wc (xconfigurerequestevent-y            ev))
      (set-xwindowchanges-width!         wc (xconfigurerequestevent-width        ev))
      (set-xwindowchanges-height!        wc (xconfigurerequestevent-height       ev))
      (set-xwindowchanges-border_width!  wc (xconfigurerequestevent-border_width ev))
      (set-xwindowchanges-sibling!       wc (xconfigurerequestevent-above        ev))
      (set-xwindowchanges-stack_mode!    wc (xconfigurerequestevent-detail       ev))
      (xconfigurewindow dpy
                        (xconfigurerequestevent-window ev)
                        (xconfigurerequestevent-value_mask ev)
                        wc)
      (xsync dpy False))))

(vector-set! handlers CONFIGUREREQUEST configure-request)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (focus-window! window)
  (debug "  focus : start")
  (let ((wid (window-id window)))
    (when (and selected (not (equal? wid selected)))
      (grab-buttons selected #f)
      (xsetwindowborder dpy selected (get-color (default-window-border-color/unselected))))
    (if wid ;; is this test necessary?
        (begin
          (grab-buttons wid #t)
          (xsetwindowborder dpy wid (get-color (default-window-border-color/selected)))
          (xsetinputfocus dpy wid REVERTTOPOINTERROOT CURRENTTIME))
        (xsetinputfocus dpy root REVERTTOPOINTERROOT CURRENTTIME))
    (set! selected wid)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-press ev)
  (debug "   button-press : start")
  (let* ((window-id (xbuttonevent-window ev))
         (window (get-window-by-id window-id)))
    (when window-id
      (focus-window! window)
      (raise-window! window))
    (let ((click (if window-id click-client-window click-root-window)))
      (let ((button
             (find
              (lambda (b)
                (and (eq? (button-target b) click)
                     (fx= (button-button b)
                          (xbuttonevent-button ev))
                     (fx= (clean-mask (button-mask b))
                          (clean-mask (xbuttonevent-state ev)))))
              buttons)))
        (when button
          (unless (root-window? window)
            (raise-window! window))
          ((button-procedure button)))))))

(vector-set! handlers BUTTONPRESS button-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (destroy-notify ev)
  (let ((window (get-window-by-id (xdestroywindowevent-window ev))))
    (when window
      (destroy-window! window)
      (select-next-window!))))

(vector-set! handlers DESTROYNOTIFY destroy-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-mouse)
  (debug "  move-mouse : start")
  (let ((window-id selected))
    (when (fx= (xgrabpointer dpy root False +mouse-mask+
                             GRABMODEASYNC GRABMODEASYNC
                             NONE move-cursor CURRENTTIME)
               GRABSUCCESS)
      (xraisewindow dpy window-id)
      (when use-grab (xgrabserver dpy))
      (let ((ev (make-xevent)))
        (let loop ()
          (xmaskevent dpy
                      (bitwise-ior +mouse-mask+
                                   EXPOSUREMASK
                                   SUBSTRUCTUREREDIRECTMASK)
                      ev)
          (let ((type (xanyevent-type ev)))
            (cond ((or (fx= type CONFIGUREREQUEST)
                       (fx= type EXPOSE)
                       (fx= type MAPREQUEST))
                   ((vector-ref handlers type) ev))
                  ((fx= type MOTIONNOTIFY)
                   (let ((x (xmotionevent-x ev))
                         (y (xmotionevent-y ev))
                         (window (get-window-by-id window-id)))
                     (window-position-set! window x y)
                     (xmovewindow dpy window-id x y)
                     (xsync dpy False))))
            (unless (fx= type BUTTONRELEASE) (loop)))
      (when use-grab (xungrabserver dpy))
      (xungrabpointer dpy CURRENTTIME))))))


(define (resize-mouse)
  (let* ((window-id selected)
         (window (get-window-by-id window-id)))
    (when (fx= (xgrabpointer dpy root False +mouse-mask+
                             GRABMODEASYNC GRABMODEASYNC
                             NONE resize-cursor CURRENTTIME)
               GRABSUCCESS)
      (when use-grab (xgrabserver dpy))
      (let* ((ev (make-xevent))
             (ResizeMask (bitwise-ior +mouse-mask+
                                      EXPOSUREMASK
                                      SUBSTRUCTUREREDIRECTMASK))
             (info (x-get-geometry dpy window-id))
             (window-x (x-get-geometry-info-x info))
             (window-y (x-get-geometry-info-y info)))
        (let loop ()
          (xmaskevent dpy ResizeMask ev)
          (let ((type (xanyevent-type ev)))
            (cond ((or (fx= type CONFIGUREREQUEST)
                       (fx= type EXPOSE)
                       (fx= type MAPREQUEST))
                   ((vector-ref handlers type) ev))
                  ((fx= type MOTIONNOTIFY)
                   (let* ((x (xmotionevent-x ev))
                          (y (xmotionevent-y ev))
                          (new-width  (fx- x window-x))
                          (new-height (fx- y window-y)))
                     (xresizewindow dpy window-id new-width new-height)
                     (window-position-set! window x y)
                     (window-width-set! window new-width)
                     (window-height-set! window new-height)
                     (xsync dpy False))))
            (unless (fx= type BUTTONRELEASE)
              (loop))))
        (when use-grab (xungrabserver dpy))
        (xungrabpointer dpy CURRENTTIME)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! buttons
      (list
       (make-button click-client-window mod-key BUTTON1 move-mouse)
       (make-button click-client-window mod-key BUTTON3 resize-mouse)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Defaults

(global-keymap '())

(map-window-hook
 `((focus-window! ,focus-window!)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define event-loop
  (let ((ev (make-xevent)))
    (lambda ()
      (xsync dpy 0)
      (let loop ()
        (xnextevent dpy ev)
        (debug "event-loop : received event of type ~A" (xanyevent-type ev))
        (let ((handler (vector-ref handlers (xanyevent-type ev))))
          (when handler
            (handle-exceptions exn
              (begin
                (print-call-chain (current-error-port))
                (print-error-message exn (current-error-port))
                (flush-output (current-error-port)))
              (handler ev))))
        (loop)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-wm #!optional config-file)
  (set! dpy (xopendisplay (or (get-environment-variable "DISPLAY") 0)))
  (set! screen (xdefaultscreen dpy))
  (set! root (xrootwindow dpy screen))
  (set! move-cursor   (xcreatefontcursor dpy XC_FLEUR))
  (set! resize-cursor (xcreatefontcursor dpy XC_SIZING))

  (xselectinput dpy root (bitwise-ior SUBSTRUCTUREREDIRECTMASK
                                      SUBSTRUCTURENOTIFYMASK
                                      BUTTONPRESSMASK
                                      ENTERWINDOWMASK
                                      LEAVEWINDOWMASK
                                      STRUCTURENOTIFYMASK
                                      PROPERTYCHANGEMASK))

  (set-num-workspaces! num-workspaces)

  ; grab all open windows and manage them
  (for-each map-window!
            (x-query-tree-info-children (x-query-tree dpy root)))

  (when config-file
    (load config-file))

  (grab-keys)

  (debug "Entering event loop...")
  (event-loop))

) ;; end module

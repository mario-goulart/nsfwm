(module nsfwm

(start-wm

 ;; Keys
 global-keymap
 make-key
 send-key
 bind-key!
 unbind-key!

 ;; Focus
 focus-mode

 ;; Debug
 enable-debug?
 nsfwm-debug

 ;; Hooks
 add-hook!
 remove-hook!
 map-window-hook
 enter-workspace-hook
 before-mainloop-hook

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
 window-transient-for
 mapped-windows
 selected-window
 move-window!
 raise-window!
 root-window?
 select-window!
 select-next-window!
 window-selected?
 hide-window!
 show-window!
 toggle-window-visibility!
 resize-window!
 window-maximized?
 maximized-window-area
 maximize-window!
 maximize-window-vertically!
 unmaximize-window!
 toggle-maximize-window!
 toggle-maximize-window-vertically!
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
 centralize-window!
 centralize-window-vertically!
 centralize-window-horizontally!
 zoom-window-in!
 zoom-window-out!
 resize-window-to-pointer-position!
 iconify-window!
 uniconify-window!

 ;; window object
 window?
 window-id
 window-sticky?
 window-iconified?
 window-position-x
 window-position-y
 window-orig-position-x
 window-orig-position-y
 window-width
 window-width-set!
 window-orig-width
 window-height
 window-height-set!
 window-orig-height
 window-border-width
 window-border-width-set!
 window-border-color/selected
 window-border-color/selected-set!
 window-border-color/unselected
 window-border-color/unselected-set!
 window-cycle-skip?
 set-window-sticky?!
 set-window-cycle-skip?!

 ;; Window decoration
 default-window-border-width
 default-window-border-color/selected
 default-window-border-color/unselected
 set-window-decoration!

 ;; Workspace record
 workspace?
 workspace-id
 workspace-selected-window
 workspace-selected-window-set!
 workspace-cyclable-windows
 workspace-cyclable-windows-set!
 workspace-uncyclable-windows
 workspace-uncyclable-windows-set!

 ;; Workspaces
 num-workspaces
 get-workspace-by-id
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
 query-pointer
 pointer?
 pointer-root
 pointer-child
 pointer-root-x
 pointer-root-y
 pointer-win-x
 pointer-win-y
 pointer-mask
 )

(import scheme)
(cond-expand
 (chicken-4
  (import chicken foreign)
  (use data-structures extras irregex lolevel ports posix (srfi 1 4))
  (use matchable
       (rename xlib
               (screen-width xscreen-width)
               (screen-height xscreen-height))))
 (chicken-5
  (import (chicken base)
          (chicken bitwise)
          (chicken blob)
          (chicken condition)
          (chicken fixnum)
          (chicken foreign)
          (chicken format)
          (chicken gc)
          (chicken irregex)
          (except (chicken memory) pointer?)
          (chicken process signal)
          (chicken process-context)
          (chicken process-context posix)
          (chicken string))
  (import matchable srfi-1 srfi-4)
  (import (rename xlib
                  (screen-width xscreen-width)
                  (screen-height xscreen-height))))
 (else
  (error "Unsupported CHICKEN version.")))

(include "keys.scm")
(include "properties.scm")

;; Horrible hack.  The xlib egg doesn't bind XSetErrorHandler, so we
;; implement an error handler in C.  It just ignores errors.
(foreign-declare "#include <X11/Xlib.h>")
(foreign-code "
int ignore_xerror(Display *dpy, XErrorEvent *e){
    fprintf(stderr, \"NSWFM error: serial: %lu, error code: %u, request code: %u, minor code: %u.  Ignored.\\n\",
            e->serial, e->error_code, e->request_code, e->minor_code);
    return 0;
}
XSetErrorHandler(ignore_xerror);
")

;;; Basic globals
(define *dpy* #f)
(define *root* #f)
(define *screen* #f)
(define *selected* #f)

(define *workspaces-hidden* #f)
(define *workspaces* #f)
(define *num-workspaces* 1)
(define *current-workspace-id* 0)


;;; Configurable parameters

(define global-keymap
  (make-parameter '()))

(define focus-mode
  (make-parameter 'click))

(define map-window-hook
  (make-parameter '()))

(define enter-workspace-hook
  (make-parameter '()))

(define before-mainloop-hook
  (make-parameter '()))

(define default-window-border-width
  (make-parameter 3))

(define default-window-border-color/selected
  (make-parameter "#333333"))

(define default-window-border-color/unselected
  (make-parameter "#cccccc"))

;;; Keys

(define (send-key window key #!key (modifier NOEVENTMASK) (dpy *dpy*) (root *root*))
  (let ((ev (make-xkeyevent))
        ;; Accept both window ids and window objects (handy when
        ;; targeting the root window, for which we don't have an
        ;; object)
        (win-id (if (window? window)
                    (window-id window)
                    root))
        (key-code
         (char->integer
          (xkeysymtokeycode dpy (xstringtokeysym key)))))
    (set-xkeyevent-type! ev KEYPRESS)
    (set-xkeyevent-display! ev dpy)
    (set-xkeyevent-root! ev root)
    (set-xkeyevent-window! ev win-id)
    (set-xkeyevent-state! ev modifier)
    (set-xkeyevent-keycode! ev key-code)
    (set-xkeyevent-same_screen! ev 1)
    (set-xkeyevent-subwindow! ev NONE)
    (xsendevent dpy win-id 1 KEYPRESSMASK ev)
    (xflush dpy)))

(define (parse-key-seq key-seq)
  (let* ((str? (string? key-seq))
         (tokens (and str? (map string->symbol (string-split key-seq))))
         ;; (modifier . key) or key
         (maybe-last-key (key->xkey (last (if str? tokens key-seq))))
         (last-key (if (pair? maybe-last-key)
                       (cdr maybe-last-key)
                       maybe-last-key))
         (modifiers* (butlast (if str? tokens key-seq)))
         (modifiers (map mod->xkey
                         (if (pair? maybe-last-key)
                             (cons (car maybe-last-key) modifiers*)
                             modifiers*))))
    (values (apply bitwise-ior modifiers) last-key)))

(define bind-key!
  (case-lambda
   ;; Example: (bind-key! (bitwise-ior SHIFTMASK CONTROLMASK) XK_LCX thunk)
   ((modifier key thunk)
    (%unbind-key! modifier key)
    (global-keymap
     (cons (make-key modifier key thunk)
           (global-keymap))))
   ((key-seq thunk)
    ;; Examples:
    ;; (bind-key! '(Shift Control x) thunk)
    ;; (bind-key! "Shift Control x" thunk)
    (let-values (((modifier key) (parse-key-seq key-seq)))
      (%unbind-key! modifier key)
      (global-keymap
       (cons (make-key modifier key thunk)
             (global-keymap)))))))

(define (%unbind-key! modifier ksym)
  (global-keymap
   (remove (lambda (key)
             (and (eq? (key-mod key) modifier)
                  (eq? (key-sym key) ksym)))
           (global-keymap))))

(define unbind-key!
  (case-lambda
   ((modifier key)
    (%unbind-key! modifier key))
   ((key-seq)
    (let-values (((modifier key) (parse-key-seq key-seq)))
      (%unbind-key! modifier key)))))

;;; Hooks

(define (run-hooks! hooks-param . args)
  (for-each (lambda (hook-id/proc)
              (let ((hook-proc (cadr hook-id/proc)))
                (apply hook-proc args)))
            (hooks-param)))

(define (add-hook! hook-param hook-id hook-proc)
  (hook-param
   (append (hook-param)
           (list (list hook-id hook-proc)))))

(define (remove-hook! hooks-param hook-id)
  (hooks-param
   (remove (lambda (hook-id/proc)
             (eq? hook-id (car hook-id/proc)))
           (hooks-param))))

;;; Screen
(define (screen-width)
  (xscreen-width (xdefaultscreenofdisplay *dpy*)))

(define (screen-height)
  (xscreen-height (xdefaultscreenofdisplay *dpy*)))


;;; Windows

(define-record window
  id
  sticky?
  cycle-skip?
  iconified?
  forcibly-hidden?
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
                #f #f #f #f #f #f #f #f #f #f #f #f
                (default-window-border-width)
                (default-window-border-color/selected)
                (default-window-border-color/unselected)))

(define (iconify-window! window)
  (%hide-window! window)
  (window-iconified?-set! window #t)
  (set-wm-state! window ICONICSTATE)
  (select-next-window!))

(define (uniconify-window! window #!key workspace)
  ;; If workspace is provided, uniconify window in that workspace (only)
  (set-wm-state! window NORMALSTATE)
  (window-iconified?-set! window #f)
  (show-window! window)
  (when workspace
    (move-window-to-workspace! window workspace))
  (select-window! window))

(define (set-window-sticky?! window yes?)
  (if yes?
      (let loop ((wsid 0))
        (unless (fx= wsid *num-workspaces*)
          (add-window-to-workspace! window (get-workspace-by-id wsid))
          (loop (fx+ wsid 1))))
      (when (window-sticky? window)
        (let loop ((wsid 0))
          (unless (fx= wsid *num-workspaces*)
            (unless (fx= wsid *current-workspace-id*)
              (remove-window-from-workspace! window (get-workspace-by-id wsid)))
            (loop (fx+ wsid 1))))))
  (window-sticky?-set! window yes?))

(define (set-window-cycle-skip?! window yes?)
  (let ((workspaces (find-window-in-workspaces window)))
    (for-each
     (lambda (workspace)
       (if yes?
           (%move-window-to-uncyclable-stack! window workspace)
           (%move-window-to-cyclable-stack! window workspace)))
     workspaces))
  (window-cycle-skip?-set! window yes?))

(define (window-position-set! window x y)
  (window-position-x-set! window x)
  (window-position-y-set! window y))

(define *windows* '())

(define (window-name window)
  (let ((name
         (let-location ((window-name c-string*))
           (if (fx= (xfetchname *dpy*
                                (window-id window)
                                (location window-name))
                    0)
               #f
               (let ((window-name window-name))
                 window-name)))))
    (or name
        (%window-get-text-property window "_NET_WM_NAME"))))

(define (all-windows)
  (map cdr *windows*))

(define (get-window-by-id id)
  (alist-ref id *windows* fx=))

(define (get-windows-by-name str/regex)
  (let ((matcher
         (cond ((string? str/regex) string=?)
               ((irregex? str/regex) irregex-match)
               (else
                (nsfwm-debug "get-windows-by-name: invalid object ~a" str/regex)
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
  (get-window-by-id *selected*))

(define (window-selected? window)
  (and *selected*
       (fx= (window-id window) *selected*)))

(define (window-transient-for window)
  (%window-get-window-property window "WM_TRANSIENT_FOR"))

(define window-visible?
  (let ((wa (make-xwindowattributes)))
    (lambda (window)
      (let ((id (window-id window)))
        (xgetwindowattributes *dpy* id wa)
        (fx= (xwindowattributes-map_state wa) ISVIEWABLE)))))

(define (window-mapped? window)
  (window-visible? window))

(define (mapped-windows)
  (filter window-mapped? (all-windows)))

(define (move-window! window x y)
  (window-position-set! window x y)
  (xmovewindow *dpy* (window-id window) x y))

(define (raise-window! window)
  (xraisewindow *dpy* (window-id window)))

(define (select-window! window)
  (raise-window! window)
  (focus-window! window)
  (ewmh-set-active-window! (window-id window)))

(define (root-window? window)
  ;; window can be either a window object or a window identifier
  (if (integer? window)
      (fx= window *root*)
      (fx= (window-id window) *root*)))

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
    (xsetwindowborderwidth *dpy* wid (window-border-width window))
    (xsetwindowborder *dpy* wid (get-color
                               (window-border-color/unselected window)))))

(define (%hide-window! window #!optional (workspace (current-workspace)))
  ;; For procedures which deal with workspaces (i.e., hidding windows
  ;; to implement the concept of workspaces).
  (%move-window-to-uncyclable-stack! window workspace)
  (xunmapwindow *dpy* (window-id window)))

(define (hide-window! window)
  ;; For users to hide windows.
  (window-forcibly-hidden?-set! window #t)
  (%hide-window! window)
  (let ((cyclable-windows (workspace-cyclable-windows (current-workspace))))
    (if (null? cyclable-windows)
        (set! *selected* #f)
        (select-stack-head! cyclable-windows))))

(define (show-window! window)
  (window-forcibly-hidden?-set! window #f)
  (unless (window-cycle-skip? window)
    (%move-window-to-cyclable-stack! window (current-workspace)))
  (xmapwindow *dpy* (window-id window)))

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
      (xresizewindow *dpy* wid new-width new-height))))

(define (resize-window-to-pointer-position! #!optional window)
  ;; Grow/shrink windows my moving corners in the direction of the
  ;; pointer position.
  (let* ((window (or window (selected-window)))
         (pointer-info (query-pointer))
         (win-x (window-position-x window))
         (win-y (window-position-y window))
         (win-width (window-width window))
         (win-height (window-height window))
         (win-vcenter (+ win-y (/ win-height 2)))
         (win-hcenter (+ win-x (/ win-width 2)))
         (pointer-x (pointer-root-x pointer-info))
         (pointer-y (pointer-root-y pointer-info))
         (quadrant
          (cond ((and (>= pointer-x win-hcenter)
                      (<= pointer-y win-vcenter))
                 'northeast)
                ((and (>= pointer-x win-hcenter)
                      (>= pointer-y win-vcenter))
                 'southeast)
                ((and (<= pointer-x win-hcenter)
                      (<= pointer-y win-vcenter))
                 'northwest)
                ((and (<= pointer-x win-hcenter)
                      (>= pointer-y win-vcenter))
                 'southwest))))
    (case quadrant
      ((northeast)
       (resize-window! window
                       (- pointer-x win-x)
                       (+ win-height (- win-y pointer-y)))
       (move-window! window win-x pointer-y))
      ((southeast)
       (resize-window! window
                       (- pointer-x win-x)
                       (- pointer-y win-y)))
      ((northwest)
       (resize-window! window
                       (- (+ win-x win-width) pointer-x)
                       (+ win-height (- win-y pointer-y)))
       (move-window! window pointer-x pointer-y))
      ((southwest)
       (resize-window! window
                       (- (+ win-x win-width) pointer-x)
                       (- pointer-y win-y))
       (move-window! window pointer-x win-y)))))

(define maximized-window-area
  ;; When #f the area of the root window will be used.  Otherwise, the
  ;; following format is expected:
  ;; (top-left-corner-x top-left-corner-y window-width window-height)
  (make-parameter #f))

(define (maximize-window! window)
  (let* ((maximized-area
          (or (maximized-window-area)
              (let ((root-info (x-get-geometry *dpy* *root*)))
                (list 0
                      0
                      (x-get-geometry-info-width root-info)
                      (x-get-geometry-info-height root-info)))))
         (new-x (car maximized-area))
         (new-y (cadr maximized-area))
         (new-width (caddr maximized-area))
         (new-height (cadddr maximized-area)))
    ;; Save current window geometry in the window object itself
    (window-orig-position-x-set! window (window-position-x window))
    (window-orig-position-y-set! window (window-position-y window))
    (window-orig-width-set! window (window-width window))
    (window-orig-height-set! window (window-height window))
    ;; Actually maximize
    (resize-window! window new-width new-height)
    (move-window! window new-x new-y)))

(define (maximize-window-vertically! window)
  (let* ((maximized-area
          (or (maximized-window-area)
              (let ((root-info (x-get-geometry *dpy* *root*)))
                (list 0
                      0
                      (x-get-geometry-info-width root-info)
                      (x-get-geometry-info-height root-info)))))
         (new-y (cadr maximized-area))
         (new-height (cadddr maximized-area)))
    ;; Save current window geometry in the window object itself
    (window-orig-position-y-set! window (window-position-y window))
    (window-orig-height-set! window (window-height window))
    ;; Actually maximize vertically
    (resize-window! window (window-width window) new-height)
    (move-window! window (window-position-x window) new-y)))

(define (toggle-maximize-window-vertically! window)
  (if (window-maximized? window)
      (unmaximize-window! window)
      (maximize-window-vertically! window)))

(define (unmaximize-window! window)
  ;; Resize and move
  (resize-window! window
                  (or (window-orig-width window)
                      (window-width window))
                  (or (window-orig-height window)
                      (window-height window)))
  (move-window! window
                (or (window-orig-position-x window)
                    (window-position-x window))
                (or (window-orig-position-y window)
                    (window-position-y window)))
  ;; Reset orig geometry, so window-maximize? knows whether the
  ;; window is maximized or not
  (window-orig-position-x-set! window #f)
  (window-orig-position-y-set! window #f)
  (window-orig-width-set! window #f)
  (window-orig-height-set! window #f))

(define (window-maximized? window)
  ;; When the orig geometry attributes are #f, window is not maximized.
  (or (window-orig-position-x window)
      ;; Window is maximized vertically
      (window-orig-position-y window)))

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
    (xdestroywindow *dpy* (window-id window))
    (ewmh-set-wm-client-list!)))

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
                (let loop ((windows (workspace-windows (current-workspace))))
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
                (let loop ((windows (workspace-windows (current-workspace))))
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
                (let loop ((windows (workspace-windows (current-workspace))))
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
                (let loop ((windows (workspace-windows (current-workspace))))
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

(define (bump-window-right! window #!key (until 'window))
  (let ((closest-x
         (case until
           ((window) (find-closest-window-x-right window))
           ((pointer) (pointer-root-x (query-pointer)))
           (else (error 'bump-window-right! "Invalid `until'" until)))))
    (move-window! window
                  (fx- closest-x (window-width window))
                  (window-position-y window))))

(define (bump-window-left! window #!key (until 'window))
  (let ((closest-x
         (case until
           ((window) (find-closest-window-x-left window))
           ((pointer) (pointer-root-x (query-pointer)))
           (else (error 'bump-window-left! "Invalid `until'" until)))))
    (move-window! window closest-x (window-position-y window))))

(define (bump-window-up! window #!key (until 'window))
  (let ((closest-y
         (case until
           ((window) (find-closest-window-y-above window))
           ((pointer) (pointer-root-y (query-pointer)))
           (else (error 'bump-window-up! "Invalid `until'" until)))))
    (move-window! window (window-position-x window) closest-y)))

(define (bump-window-down! window #!key (until 'window))
  (let ((closest-y
         (case until
           ((window) (find-closest-window-y-below window))
           ((pointer) (pointer-root-y (query-pointer)))
           (else (error 'bump-window-down! "Invalid `until'" until)))))
    (move-window! window
                  (window-position-x window)
                  (fx- closest-y (window-height window)))))

(define (grow-window-right! window #!key (until 'window))
  (let* ((closest-x
          (case until
            ((window) (find-closest-window-x-right window))
            ((pointer) (pointer-root-x (query-pointer)))
            (else (error 'grow-window-right! "Invalid `until'" until))))
         (new-width (fx- closest-x (window-position-x window))))
    (resize-window! window new-width (window-height window))))

(define (grow-window-left! window #!key (until 'window))
  (let* ((closest-x
          (case until
            ((window) (find-closest-window-x-left window))
            ((pointer) (pointer-root-x (query-pointer)))
            (else (error 'grow-window-left! "Invalid `until'" until))))
         (new-width (fx+ (window-width window)
                         (fx- (window-position-x window)
                              closest-x))))
    (resize-window! window new-width (window-height window))
    (move-window! window closest-x (window-position-y window))))

(define (grow-window-up! window #!key (until 'window))
  (let* ((closest-y
          (case until
            ((window) (find-closest-window-y-above window))
            ((pointer) (pointer-root-y (query-pointer)))
            (else (error 'grow-window-up! "Invalid `until'" until))))
         (new-height (fx+ (window-height window)
                          (fx- (window-position-y window)
                               closest-y))))
    (resize-window! window (window-width window) new-height)
    (move-window! window (window-position-x window) closest-y)))

(define (grow-window-down! window #!key (until 'window))
  (let* ((closest-y
          (case until
            ((window) (find-closest-window-y-below window))
            ((pointer) (pointer-root-y (query-pointer)))
            (else (error 'grow-window-down! "Invalid `until'" until))))
         (new-height (fx- closest-y (window-position-y window))))
    (resize-window! window (window-width window) new-height)))

(define (centralize-window-horizontally! window)
  (let ((new-x (- (quotient (screen-width) 2)
                  (quotient (window-width (selected-window)) 2)))
        (new-y (window-position-y window)))
    (move-window! window new-x new-y)))

(define (centralize-window-vertically! window)
  (move-window! window
                (window-position-x window)
                (- (quotient (screen-height) 2)
                   (quotient (window-height (selected-window)) 2))))

(define (centralize-window! window)
  (centralize-window-horizontally! window)
  (centralize-window-vertically! window))

(define (zoom-window! window in/out step)
  (when window
    (let ((borders-width (fx* (window-border-width window) 2)))
      (resize-window! window
                      (in/out (fx+ (window-width window) borders-width) step)
                      (in/out (fx+ (window-height window) borders-width) step)))))

(define (zoom-window-in! window #!key (step 10))
  (zoom-window! window fx+ step))

(define (zoom-window-out! window #!key (step 10))
  (zoom-window! window fx- step))

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

(define (remove-window-from-stack stack window)
  ;; Return a copy of stack without window
  (let ((wid (window-id window)))
    (remove (lambda (win)
              (fx= wid (window-id win)))
            stack)))

(define (stack-preempt-window! workspace window)
  ;; Move window to the head of the stack
  (let ((stack (workspace-cyclable-windows workspace)))
    (unless (or (null? stack)
                (null? (cdr stack))
                (not window))
      (let ((wid (window-id window)))
        ;; Fast path
        (unless (fx= wid (window-id (car stack)))
          (let ((new-stack
                 (cons window
                       (remove-window-from-stack stack window))))
            (workspace-cyclable-windows-set! workspace new-stack)
            new-stack))))))

(define (select-stack-head! stack)
  (unless (null? stack)
    (select-window! (car stack))))

(define (select-next-window! #!optional (cycler! cycle-windows-upwards!))
  (let ((stack (cycler! (current-workspace))))
    (unless (null? stack)
      (select-stack-head! stack))))

(define (window-depth window workspace)
  (let ((wid (window-id window)))
    (list-index (lambda (w)
                  (fx= (window-id w) wid))
                (workspace-windows workspace))))


;;; Workspaces

(define-record workspace id uncyclable-windows cyclable-windows selected-window)

(define-record-printer (workspace obj out)
  (fprintf
   out
   "#<workspace id: ~a selected-window: ~S>"
   (workspace-id obj)
   (workspace-selected-window obj)))

(define %make-workspace make-workspace)
(define (make-workspace id)
  (%make-workspace id '() '() #f))

(define (num-workspaces)
  *num-workspaces*)

(define (current-workspace)
  (get-workspace-by-id *current-workspace-id*))

(define (workspace-windows workspace)
  (append (workspace-cyclable-windows workspace)
          (workspace-uncyclable-windows workspace)))

(define (%move-window-to-uncyclable-stack! window workspace)
  (let* ((wid (window-id window))
         (cyclable (workspace-cyclable-windows workspace))
         (uncyclable (workspace-uncyclable-windows workspace)))
    (when (member window cyclable same-window?)
      (workspace-cyclable-windows-set!
       workspace
       (remove (lambda (w)
                 (fx= wid (window-id w)))
               cyclable)))
    (unless (member window uncyclable same-window?)
      (workspace-uncyclable-windows-set!
       workspace
       (cons window uncyclable)))))

(define (%move-window-to-cyclable-stack! window workspace)
  (let* ((wid (window-id window))
         (cyclable (workspace-cyclable-windows workspace))
         (uncyclable (workspace-uncyclable-windows workspace)))
    (when (member window uncyclable same-window?)
      (workspace-uncyclable-windows-set!
       workspace
       (remove (lambda (w)
                 (fx= wid (window-id w)))
               uncyclable)))
    (unless (member window cyclable same-window?)
      (workspace-cyclable-windows-set!
       workspace
       (cons window cyclable)))))

(define (get-workspace-by-id wsid)
  (vector-ref *workspaces* wsid))

(define (set-num-workspaces! n)
  (if *workspaces* ;; *workspaces* have been initialized before
      (let ((cur-len (vector-length *workspaces*)))
        (nsfwm-debug "cur *workspaces* len: ~a" cur-len)
        (if (< n cur-len)
            (begin
              ;; Move all windows to the last workspace
              (let loop ((wsid-to-remove (fx- n 1)))
                (unless (fx= wsid-to-remove cur-len)
                  (let ((workspace-to-remove
                         (get-workspace-by-id wsid-to-remove)))
                    (for-each
                     (lambda (window)
                       (move-window-to-workspace! window
                                                  workspace-to-remove))
                     (workspace-windows workspace-to-remove))))
                (set! *workspaces*
                      (vector-resize *workspaces* n))))
            (begin
              (set! *workspaces*
                    (vector-resize *workspaces* n #f))
              (let loop ((i cur-len))
                (when (fx< i n)
                  (vector-set! *workspaces* i (make-workspace i))
                  (loop (fx+ i 1)))))))
      (begin
        (set! *workspaces-hidden* (make-vector n #f))
        (set! *workspaces* (make-vector n #f))
        (let loop ((i 0))
          (when (fx< i n)
            (vector-set! *workspaces-hidden* i (make-workspace i))
            (vector-set! *workspaces* i (make-workspace i))))))
  (set! *num-workspaces* n)
  (ewmh-set-number-of-desktops! n))

(define (select-last-selected-window-in-workspace! workspace)
  (let ((last-selected-win (workspace-selected-window workspace)))
    (if (and last-selected-win
             (window-in-workspace? last-selected-win workspace))
        (begin
          (stack-preempt-window! workspace last-selected-win)
          (select-window! last-selected-win))
        ;; The selected window might have moved from the workspace.
        ;; In this case, select the next cyclable one (if any)
        (let ((cyclable-windows (workspace-cyclable-windows workspace)))
          (unless (null? cyclable-windows)
            (select-window! (car cyclable-windows)))))))

(define (switch-to-workspace! next-workspace-id)
  (when (fx>= next-workspace-id *num-workspaces*)
    (error 'switch-to-workspace! "Invalid workspace id" next-workspace-id))

  ;; Actions before switching workspaces
  (for-each %hide-window! (mapped-windows))
  ;; Beware that the selected window might be moved from the workspace
  ;; at some point not have workspace-selected-window updated.
  (workspace-selected-window-set! (current-workspace) (selected-window))

  ;; Switching to the new workspace
  (set! *current-workspace-id* next-workspace-id)
  (ewmh-set-current-desktop! next-workspace-id)
  (let ((next-workspace (get-workspace-by-id next-workspace-id)))
    ;; At this point all windows in next-workspace are in the
    ;; uncyclable windows list (put there by %hide-workspace), so
    ;; workspace-windows will basically return that list.
    (for-each (lambda (window)
                (unless (or (window-forcibly-hidden? window)
                            (window-iconified? window))
                  (show-window! window)))
              (workspace-windows next-workspace))
    (select-last-selected-window-in-workspace! next-workspace)
    (run-hooks! enter-workspace-hook next-workspace)))

(define (add-window-to-workspace! window workspace)
  (if (or (window-cycle-skip? window)
          (not (window-visible? window)))
      (workspace-uncyclable-windows-set!
       workspace
       (cons window (workspace-uncyclable-windows workspace)))
      (workspace-cyclable-windows-set!
       workspace
       (cons window (workspace-cyclable-windows workspace))))
  (when (fx= (workspace-id workspace) *current-workspace-id*)
    (show-window! window)))

(define (remove-window-from-workspace! window workspace)
  ;; %hide-window! will move window to the uncyclable list
  (%hide-window! window workspace)
  (let ((wid (window-id window)))
    (workspace-uncyclable-windows-set!
     workspace
     (remove (lambda (w)
               (fx= (window-id w) wid))
             (workspace-uncyclable-windows workspace)))
    (when (same-window? window (workspace-selected-window workspace))
      (let ((cyclable-windows (workspace-cyclable-windows workspace)))
        (workspace-selected-window-set! workspace
                                        (if (null? cyclable-windows)
                                            #f
                                            (car cyclable-windows)))))))

(define (move-window-to-workspace! window workspace #!optional from)
  (let ((origins (if from
                     (list from)
                     (find-window-in-workspaces window))))
    (for-each
     (lambda (workspace)
       (remove-window-from-workspace! window workspace))
     origins))
  (add-window-to-workspace! window workspace))

(define (window-in-workspace? window workspace)
  (let ((wid (window-id window)))
    (let loop ((windows (workspace-windows workspace)))
      (if (null? windows)
          #f
          (or (fx= (window-id (car windows)) wid)
              (loop (cdr windows)))))))

(define (find-window-in-workspaces window)
  (let loop ((wsid 0))
    (if (fx= wsid *num-workspaces*)
        '()
        (let ((workspace (get-workspace-by-id wsid)))
          (if (window-in-workspace? window workspace)
              (cons workspace (loop (fx+ wsid 1)))
              (loop (fx+ wsid 1)))))))

;; Pointer

(define (warp-pointer! x y)
  (xwarppointer *dpy* NONE *root* 0 0 0 0 x y))

(define-record pointer root child root-x root-y win-x win-y mask)

(define-record-printer (pointer obj out)
  (fprintf
   out
   "#<pointer: root: ~a child: ~a root-x: ~a root-y: ~a win-x: ~a win-y: ~a mask: ~a>"
   (pointer-root obj)
   (pointer-child obj)
   (pointer-root-x obj)
   (pointer-root-y obj)
   (pointer-win-x obj)
   (pointer-win-x obj)
   (pointer-mask obj)))

(define (query-pointer #!optional (win-id *root*))
  (let-location ((root      unsigned-int32)
                 (child     unsigned-int32)
                 (root-x    int32)
                 (root-y    int32)
                 (win-x     int32)
                 (win-y     int32)
                 (mask      unsigned-int32))
    (xquerypointer *dpy*
                   win-id
                   (location root)
                   (location child)
                   (location root-x)
                   (location root-y)
                   (location win-x)
                   (location win-y)
                   (location mask))
    (make-pointer root child root-x root-y win-x win-y mask)))

;; Utils
(define enable-debug?
  (make-parameter
   (and (get-environment-variable "NSFWM_DEBUG") #t)))

(define (nsfwm-debug fmt . args)
  (when (enable-debug?)
    (apply fprintf (append (list (current-error-port)
                                 (string-append fmt "\n"))
                           args))
    (flush-output (current-error-port))))


;; intermediate glue

(define True 1)
(define False 0)

(define-record x-get-geometry-info root x y width height border-width depth)

(define (x-get-geometry *dpy* id)
  (let-location ((root         unsigned-long)
                 (x            unsigned-int32)
                 (y            unsigned-int32)
                 (width        unsigned-int32)
                 (height       unsigned-int32)
                 (border-width unsigned-int32)
                 (depth        unsigned-int32))
    (xgetgeometry *dpy*
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

(define handlers (make-vector LASTEVENT #f))

(define-record button target mask button procedure)

(define-record-printer (button obj out)
  (fprintf out "#<button ~a target: ~a mask: ~a>"
           (button-button obj)
           (button-target obj)
           (button-mask obj)))

(define-record key mod sym procedure)

(define-record-printer (key obj out)
  (fprintf out "#<key mod: ~a sym: ~a>" (key-mod obj) (key-sym obj)))

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
      (let ((colormap (xdefaultcolormap *dpy* *screen*)))
        (xallocnamedcolor *dpy* colormap name color color)
        (xcolor-pixel color)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grab-buttons window focused)
  (xungrabbutton *dpy* ANYBUTTON ANYMODIFIER window)
  (when focused
    (for-each
     (lambda (b)
       (if (eq? (button-target b) click-client-window)
           (for-each
            (lambda (modifier)
              (xgrabbutton *dpy*
                           (button-button b)
                           (bitwise-ior (button-mask b) modifier)
                           window
                           0 +button-mask+ GRABMODEASYNC GRABMODESYNC NONE
                           NONE))
            (list 0 LOCKMASK num-lock-mask
                  (bitwise-ior num-lock-mask LOCKMASK)))
           (xgrabbutton *dpy* ANYBUTTON ANYMODIFIER window False +button-mask+
                        GRABMODEASYNC GRABMODESYNC NONE NONE)))
     buttons)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grab-keys)
  (xungrabkey *dpy* ANYKEY ANYMODIFIER *root*)
  (for-each
   (lambda (k)
     (let ((code (xkeysymtokeycode *dpy* (key-sym k))))
       ;; Kludge for now. Some FFIs return a Scheme char, others a number.
       (let ((code (if (char? code) (char->integer code) code)))
         (for-each
          (lambda (modifier)
            (xgrabkey *dpy* code (bitwise-ior (key-mod k) modifier)
                      *root* 1 GRABMODEASYNC GRABMODEASYNC))
          (list 0 LOCKMASK num-lock-mask (bitwise-ior num-lock-mask LOCKMASK))))))
   (global-keymap)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%key-press ev index)
  ;; index is 1 for shifted keys (as XK_P) 0 for lower case keys (XK_LCP)
  (let* ((keysym (xkeycodetokeysym *dpy* (xkeyevent-keycode ev) index))
         (key
          (find
           (lambda (k)
             (and (fx= (key-sym k) keysym)
                  (fx= (clean-mask (key-mod k))
                       (clean-mask (xkeyevent-state ev)))))
           (global-keymap))))
    (nsfwm-debug
     "Key code ~A pressed event ~A list ~A keyevent-state ~A -> found ~a"
     (xkeyevent-keycode ev)
     keysym
     (map (lambda(k) `(,(key-sym k) ,(clean-mask (key-mod k))))
          (global-keymap))
     (clean-mask (xkeyevent-state ev)) key)
    (when key
      ((key-procedure key))
      #t)))

(define (key-press ev)
  (unless (%key-press ev 0) ;; lowercase
    (%key-press ev 1))) ;; uppercase

(vector-set! handlers KEYPRESS key-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enter-notify ev)
    (cond ((and (not (fx= (xcrossingevent-mode ev) NOTIFYNORMAL))
                (not (fx= (xcrossingevent-window ev) *root*)))
           (nsfwm-debug "  enter-notify : mode is not NOTIFYNORMAL"))
          ((and (fx= (xcrossingevent-detail ev) NOTIFYINFERIOR)
                (not (fx= (xcrossingevent-window ev) *root*)))
           (nsfwm-debug "  enter-notify : detail is NOTIFYINFERIOR"))
          ((get-window-by-id (xcrossingevent-window ev)) =>
           (lambda (window)
             (when (eq? (focus-mode) 'enter-exit)
               (select-window! window))))))

(vector-set! handlers ENTERNOTIFY enter-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-focus! ev)
  (when (and (integer? *selected*)
             (not (fx= (xfocuschangeevent-window ev) *selected*)))
    (xsetinputfocus *dpy* *selected* REVERTTOPOINTERROOT CURRENTTIME)))

(vector-set! handlers FOCUSIN set-focus!)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-window! id)
  (xsetwindowborderwidth *dpy* id (default-window-border-width))
  (xsetwindowborder *dpy* id (get-color (default-window-border-color/unselected)))
  (xselectinput *dpy* id (bitwise-ior BUTTONPRESSMASK
                                      BUTTONRELEASEMASK
                                      ENTERWINDOWMASK
                                      FOCUSCHANGEMASK
                                      PROPERTYCHANGEMASK
                                      STRUCTURENOTIFYMASK))
  (grab-buttons id #f)
  (let ((window (add-window! id)))
    (nsfwm-debug "Mapping window ~a" window)
    (xmapwindow *dpy* id)
    (let* ((info (x-get-geometry *dpy* id))
           (x (x-get-geometry-info-x info))
           (y (x-get-geometry-info-y info))
           (width (fx+ (x-get-geometry-info-width info)
                       (default-window-border-width)))
           (height (fx+ (x-get-geometry-info-height info)
                        (default-window-border-width))))
      (window-position-set! window x y)
      (window-width-set! window width)
      (window-height-set! window height)
      (let ((y-bottom (fx+ y height)))
        (when (> y-bottom (screen-height))
          (move-window! window x 0))))
    (add-window-to-workspace! window (current-workspace))
    (run-hooks! map-window-hook window))
  (xsync *dpy* False)
  (set-wm-state! id NORMALSTATE)
  (ewmh-set-wm-client-list!))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-request
  (let ((wa (make-xwindowattributes)))
    (lambda (ev)
      (let ((id (xmaprequestevent-window ev)))
        (when (and (not (fx= (xgetwindowattributes *dpy* id wa) 0))
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
      (xconfigurewindow *dpy*
                        (xconfigurerequestevent-window ev)
                        (xconfigurerequestevent-value_mask ev)
                        wc)
      (xsync *dpy* False))))

(vector-set! handlers CONFIGUREREQUEST configure-request)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (focus-window! window)
  (nsfwm-debug "  focus : start")
  (let ((wid (window-id window)))
    (when (and *selected* (not (equal? wid *selected*)))
      (grab-buttons *selected* #f)
      (xsetwindowborder *dpy* *selected* (get-color (default-window-border-color/unselected))))
    (if wid ;; is this test necessary?
        (begin
          (grab-buttons wid #t)
          (xsetwindowborder *dpy* wid (get-color (default-window-border-color/selected)))
          (xsetinputfocus *dpy* wid REVERTTOPOINTERROOT CURRENTTIME))
        (xsetinputfocus *dpy* *root* REVERTTOPOINTERROOT CURRENTTIME))
    (set! *selected* wid)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-press ev)
  (nsfwm-debug "   button-press : start")
  (let* ((window-id (xbuttonevent-window ev))
         (window (get-window-by-id window-id)))
    (unless (root-window? window-id)
      (select-window! window))
    (let ((click (if window click-client-window click-root-window)))
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
          ((button-procedure button)))))))

(vector-set! handlers BUTTONPRESS button-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (destroy-notify ev)
  (let ((window (get-window-by-id (xdestroywindowevent-window ev))))
    (when window
      (destroy-window! window)
      (select-stack-head! (workspace-cyclable-windows (current-workspace))))))

(vector-set! handlers DESTROYNOTIFY destroy-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-mouse)
  (nsfwm-debug "  move-mouse : start")
  (let ((window-id *selected*))
    (when (fx= (xgrabpointer *dpy* *root* False +mouse-mask+
                             GRABMODEASYNC GRABMODEASYNC
                             NONE move-cursor CURRENTTIME)
               GRABSUCCESS)
      (xraisewindow *dpy* window-id)
      (when use-grab (xgrabserver *dpy*))
      (let ((ev (make-xevent)))
        (let loop ()
          (xmaskevent *dpy*
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
                     (xmovewindow *dpy* window-id x y)
                     (xsync *dpy* False))))
            (unless (fx= type BUTTONRELEASE) (loop)))
      (when use-grab (xungrabserver *dpy*))
      (xungrabpointer *dpy* CURRENTTIME))))))


(define (resize-mouse)
  (let* ((window-id *selected*)
         (window (get-window-by-id window-id)))
    (when (fx= (xgrabpointer *dpy* *root* False +mouse-mask+
                             GRABMODEASYNC GRABMODEASYNC
                             NONE resize-cursor CURRENTTIME)
               GRABSUCCESS)
      (when use-grab (xgrabserver *dpy*))
      (let* ((ev (make-xevent))
             (ResizeMask (bitwise-ior +mouse-mask+
                                      EXPOSUREMASK
                                      SUBSTRUCTUREREDIRECTMASK))
             (info (x-get-geometry *dpy* window-id))
             (window-x (x-get-geometry-info-x info))
             (window-y (x-get-geometry-info-y info)))
        (let loop ()
          (xmaskevent *dpy* ResizeMask ev)
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
                     (xresizewindow *dpy* window-id new-width new-height)
                     (window-position-set! window x y)
                     (window-width-set! window new-width)
                     (window-height-set! window new-height)
                     (xsync *dpy* False))))
            (unless (fx= type BUTTONRELEASE)
              (loop))))
        (when use-grab (xungrabserver *dpy*))
        (xungrabpointer *dpy* CURRENTTIME)))))

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
      (xsync *dpy* 0)
      (let loop ()
        (xnextevent *dpy* ev)
        (nsfwm-debug "event-loop : received event of type ~A" (xanyevent-type ev))
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


(define (setup-ewmh-support! dpy root)
  (let ((ewmh-win-id (xcreatesimplewindow dpy root 0 0 1 1 0 0 0)))
    (window-property-set! ewmh-win-id
                          "_NET_SUPPORTING_WM_CHECK"
                          (make-window-property ewmh-win-id))
    (window-property-set! root
                          "_NET_SUPPORTING_WM_CHECK"
                          (make-window-property ewmh-win-id))
    (window-property-set! ewmh-win-id
                          "_NET_WM_NAME"
                          (make-utf8-property "nsfwm"))
    (window-property-set! ewmh-win-id
                          "_NET_WM_PID"
                          (make-number-property (current-process-id)))))

(define (ewmh-set-root-property! property-name property-obj)
  (window-property-set! *root* property-name property-obj))

(define (ewmh-set-number-of-desktops! num-workspaces)
  (ewmh-set-root-property! "_NET_NUMBER_OF_DESKTOPS"
                           (make-number-property num-workspaces)))

(define (ewmh-set-current-desktop! workspace-id)
  (ewmh-set-root-property! "_NET_CURRENT_DESKTOP"
                           (make-number-property workspace-id)))

(define (ewmh-set-desktop-geometry! width height)
  (ewmh-set-root-property! "_NET_DESKTOP_GEOMETRY"
                           (make-numbers-property (list width height))))

(define (ewmh-set-wm-pid!)
  (ewmh-set-root-property! "_NET_WM_PID"
                           (make-number-property (current-process-id))))

(define (ewmh-set-wm-client-list!)
  (let ((window-ids (map car *windows*)))
    (ewmh-set-root-property! "_NET_CLIENT_LIST"
                             (make-windows-property window-ids))
    (ewmh-set-root-property! "_NET_CLIENT_LIST_STACKING"
                             (make-windows-property window-ids))))

(define (ewmh-set-active-window! window-id)
  (ewmh-set-root-property! "_NET_ACTIVE_WINDOW"
                           (make-window-property window-id)))

(define (ewmh-set-showing-desktop?! showing?)
  (ewmh-set-root-property! "_NET_SHOWING_DESKTOP"
                           (make-number-property (if showing? 1 0))))

(define (set-wm-state! window state)
  ;; At the moment only support NormalState, setting icon window to 0
  (let ((win-id (if (window? window)
                    (window-id window)
                    window))
        (wm-state (make-wm-state-property (list state 0))))
    (window-property-set! win-id "WM_STATE" wm-state)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-wm #!optional config-file)
  (define dpy-number (or (get-environment-variable "DISPLAY") "0"))
  (set! *dpy* (xopendisplay dpy-number))
  (set! *screen* (xdefaultscreen *dpy*))

  ;; Check if another window manager is running
  (unless (= (xgetselectionowner *dpy*
                                 (xinternatom *dpy* (sprintf "WM_S~a" *screen*) 0))
             NONE)
    (fprintf (current-error-port)
             (string-append
              "ERROR: Another window manager is already running on display ~a. "
              "Aborting.\n")
             dpy-number)
    (exit 1))


  (set! *root* (xrootwindow *dpy* *screen*))
  (set! move-cursor   (xcreatefontcursor *dpy* XC_FLEUR))
  (set! resize-cursor (xcreatefontcursor *dpy* XC_SIZING))

  (xselectinput *dpy* *root* (bitwise-ior SUBSTRUCTUREREDIRECTMASK
                                          SUBSTRUCTURENOTIFYMASK
                                          KEYPRESSMASK
                                          BUTTONPRESSMASK
                                          BUTTONRELEASEMASK
                                          ENTERWINDOWMASK
                                          LEAVEWINDOWMASK
                                          STRUCTURENOTIFYMASK
                                          PROPERTYCHANGEMASK))

  (set-num-workspaces! *num-workspaces*)

  ;; Set default cursor shape
  (let ((cursor (xcreatefontcursor *dpy* XC_LEFT_PTR)))
    (xdefinecursor *dpy* *root* cursor))

  ; grab all open windows and manage them
  (for-each map-window!
            (x-query-tree-info-children (x-query-tree *dpy* *root*)))

  ;; EWMH support
  (setup-ewmh-support! *dpy* *root*)
  (ewmh-set-current-desktop! *current-workspace-id*)
  (ewmh-set-desktop-geometry! (screen-width) (screen-height))
  (ewmh-set-wm-pid!)
  (ewmh-set-showing-desktop?! #f)

  (set-signal-handler! signal/hup
    (lambda (_)
      (when config-file
        (fprintf (current-error-port)
                 "nsfwm: Received SUGHUP.  Reloading configuration.\n")
        (load config-file))))

  (when config-file
    (load config-file))

  (grab-keys)

  (nsfwm-debug "Entering event loop...")
  (run-hooks! before-mainloop-hook)
  (event-loop))

) ;; end module

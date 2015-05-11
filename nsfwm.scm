(module nsfwm

(start-wm

 ;; Keys
 global-keymap
 make-key

 ;; Hooks
 add-hook!
 remove-hook!
 map-window-hook
 enter-workspace-hook

 ;; Windows
 window?
 get-window-by-id
 delete-window-by-id!
 add-window!
 window-viewable?
 window-mapped?
 mapped-windows

 ;; Workspaces
 num-workspaces
 switch-to-workspace!
 )

(import chicken scheme foreign)
(use ports extras xlib data-structures (srfi 1 4) lolevel posix)

;; Horrible hack.  The xlib egg doesn't bind XSetErrorHandler, so we
;; implement an error handler in C.  It just ignores errors.
(foreign-declare "#include <X11/Xlib.h>")
(foreign-code "
int ignore_xerror(Display *dpy, XErrorEvent *e){ return 0; }
XSetErrorHandler(ignore_xerror);
")

;;; Configurable parameters

(define global-keymap
  (make-parameter '()))

(define map-window-hook
  (make-parameter '()))

(define num-workspaces
  (make-parameter 10))

(define enter-workspace-hook
  (make-parameter '()))

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

;;; Windows

(define *windows* '())

(define (get-window-by-id id)
  (alist-ref id *windows* equal?))

(define (delete-window-by-id! id)
  (set! *windows* (alist-delete! id *windows* equal?)))

(define (add-window! id)
  (set! *windows* (alist-update id id *windows* equal?)))

(define (window? id)
  (and (alist-ref id *windows* equal?) #t))

(define window-viewable?
  (let ((wa (make-xwindowattributes)))
    (lambda (id)
      (xgetwindowattributes dpy id wa)
      (= (xwindowattributes-map_state wa) ISVIEWABLE))))

(define (window-mapped? id)
  (and (window? id)
       (window-viewable? id)))

(define (mapped-windows)
  (filter window-mapped?
          (x-query-tree-info-children (x-query-tree dpy root))))


;;; Workspaces

(define workspaces-hidden (make-vector (num-workspaces) '()))

(define workspaces (make-vector (num-workspaces) '()))

(define current-workspace 0)

(define (switch-to-workspace! i)
  (define (unmap-window id)
    (xunmapwindow dpy id))
  (define (map-window id)
    (xmapwindow dpy id))
  (vector-set! workspaces current-workspace (mapped-windows))
  (for-each unmap-window (mapped-windows))
  (when (vector-ref workspaces i)
    (for-each map-window (vector-ref workspaces i)))
  (set! current-workspace i)
  (run-hooks! enter-workspace-hook i))


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

(define (x-fetch-name dpy id)
  (let-location ((window-name c-string*))
    (if (= (xfetchname dpy id (location window-name)) 0)
        #f
        (let ((window-name window-name))
          window-name))))

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

(define dpy    #f)
(define screen #f)
(define root   #f)

(define move-cursor   #f)
(define resize-cursor #f)

(define click-root-window   'click-root-window)
(define click-client-window 'click-client-window)

(define handlers (make-vector LASTEvent #f))

(define-record button click mask button procedure)

(define-record key mod keysym procedure)

(define num-lock-mask 0)

(define (clean-mask mask)
  (bitwise-and mask (bitwise-not (bitwise-ior num-lock-mask LOCKMASK))))

(define +button-mask+ (bitwise-ior BUTTONPRESSMASK BUTTONRELEASEMASK))

(define +mouse-mask+ (bitwise-ior +button-mask+ POINTERMOTIONMASK))

(define selected #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define border-width 5)

;; (define normal-border-color   "#cccccc")
;; (define selected-border-color "#0066ff")

(define normal-border-color   "#9eeeee")
(define selected-border-color "#55aaaa")

(define menu-background-color "#e9ffe9")
(define selected-menu-background-color "#448844")

(define menu-foreground-color "black")
(define selected-menu-foreground-color "white")

(define other-background-color "#eaffff")

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
       (if (eq? (button-click b) click-client-window)
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
                       (and (= (key-keysym k) keysym)
                            (= (clean-mask (key-mod k))
                               (clean-mask (xkeyevent-state ev)))))
                     (global-keymap))))
      (printf "Key code ~A pressed event ~A (P should be ~A) list  ~A keyevent-state ~A -> found ~a~%"
              (xkeyevent-keycode ev)
              keysym XK_P
              (map (lambda(k) `(,(key-keysym k) ,(clean-mask (key-mod k))))
                   (global-keymap))
              (clean-mask (xkeyevent-state ev)) key)
      (when key ((key-procedure key))))))

(vector-set! handlers KEYPRESS key-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enter-notify ev)
  (cond ((and (not (= (xcrossingevent-mode ev) NOTIFYNORMAL))
              (not (= (xcrossingevent-window ev) root)))
         (printf "  enter-notify : mode is not NOTIFYNORMAL~%"))
        ((and (= (xcrossingevent-detail ev) NOTIFYINFERIOR)
              (not (= (xcrossingevent-window ev) root)))
         (printf "  enter-notify : detail is NOTIFYINFERIOR~%"))
        ((get-window-by-id (xcrossingevent-window ev)) => focus-window!)
        (else (focus-window! #f))))

(vector-set! handlers ENTERNOTIFY enter-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-focus! ev)
  (when (and (integer? selected)
             (not (= (xfocuschangeevent-window ev) selected)))
    (xsetinputfocus dpy selected REVERTTOPOINTERROOT CURRENTTIME)))

(vector-set! handlers FOCUSIN set-focus!)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-window! id)
  (xsetwindowborderwidth dpy id border-width)
  (xsetwindowborder dpy id (get-color normal-border-color))
  (xselectinput dpy id (bitwise-ior ENTERWINDOWMASK
                                    FOCUSCHANGEMASK
                                    PROPERTYCHANGEMASK
                                    STRUCTURENOTIFYMASK))
  (grab-buttons id #f)
  (add-window! id)
  (xmapwindow dpy id)
  (run-hooks! map-window-hook id)
  (xsync dpy False))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-request
  (let ((wa (make-xwindowattributes)))
    (lambda (ev)
      (let ((id (xmaprequestevent-window ev)))
        (when (and (not (= (xgetwindowattributes dpy id wa) 0))
                   (= (xwindowattributes-override_redirect wa) 0)
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
  (printf "  focus : start~%")
  (when (and selected (not (equal? window selected)))
    (grab-buttons selected #f)
    (xsetwindowborder dpy selected (get-color normal-border-color)))
  (if window
      (begin
        (grab-buttons window #t)
        (xsetwindowborder dpy window (get-color selected-border-color))
        (xsetinputfocus   dpy window REVERTTOPOINTERROOT CURRENTTIME))
      (xsetinputfocus dpy root REVERTTOPOINTERROOT CURRENTTIME))
  (set! selected window))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-press ev)
  (printf "  button-press : start~%")
  (let ((window (get-window-by-id (xbuttonevent-window ev))))
    (when window (focus-window! window))
    (let ((click (if window click-client-window click-root-window)))
      (let ((button
             (find
              (lambda (b)
                (and (eq? (button-click b) click)
                     (= (button-button b)
                        (xbuttonevent-button ev))
                     (= (clean-mask (button-mask b))
                        (clean-mask (xbuttonevent-state ev)))))
              buttons)))
        (when button
          ((button-procedure button)))))))

(vector-set! handlers BUTTONPRESS button-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (destroy-notify ev)
  (let ((window (get-window-by-id (xdestroywindowevent-window ev))))
    (when window
      (delete-window-by-id! window))))

(vector-set! handlers DESTROYNOTIFY destroy-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-mouse)
  (printf "  move-mouse : start~%")
  (let ((window selected))
    (when (and window
               (= (xgrabpointer dpy root False +mouse-mask+
                                GRABMODEASYNC GRABMODEASYNC
                                NONE move-cursor CURRENTTIME)
                  GRABSUCCESS))
      (xraisewindow dpy window)
      (when use-grab (xgrabserver dpy))
      (let ((ev (make-xevent)))
        (let loop ()
          (xmaskevent dpy
                      (bitwise-ior +mouse-mask+
                                   EXPOSUREMASK
                                   SUBSTRUCTUREREDIRECTMASK)
                      ev)
          (let ((type (xanyevent-type ev)))
            (cond ((or (= type CONFIGUREREQUEST)
                       (= type EXPOSE)
                       (= type MAPREQUEST))
                   ((vector-ref handlers type) ev))
                  ((= type MOTIONNOTIFY)
                   (xmovewindow dpy
                                window
                                (xmotionevent-x ev)
                                (xmotionevent-y ev))
                   (xsync dpy False)))
            (unless (= type BUTTONRELEASE) (loop)))
      (when use-grab (xungrabserver dpy))
      (xungrabpointer dpy CURRENTTIME))))))


(define (resize-mouse)
  (let ((window selected))
    (when (and window
               (= (xgrabpointer dpy root False +mouse-mask+
                                GRABMODEASYNC GRABMODEASYNC
                                NONE resize-cursor CURRENTTIME)
                  GRABSUCCESS))
      (when use-grab (xgrabserver dpy))
      (let ((ev (make-xevent)))
        (define ResizeMask
          (bitwise-ior +mouse-mask+
                       EXPOSUREMASK
                       SUBSTRUCTUREREDIRECTMASK))
        (define window-x #f)
        (define window-y #f)
        (let ((info (x-get-geometry dpy window)))
          (set! window-x (x-get-geometry-info-x info))
          (set! window-y (x-get-geometry-info-y info)))
        (let loop ()
          (xmaskevent dpy ResizeMask ev)
          (let ((type (xanyevent-type ev)))
            (cond ((or (= type CONFIGUREREQUEST)
                       (= type EXPOSE)
                       (= type MAPREQUEST))
                   ((vector-ref handlers type) ev))
                  ((= type MOTIONNOTIFY)
                   (let ((new-width  (- (xmotionevent-x ev) window-x))
                         (new-height (- (xmotionevent-y ev) window-y)))
                     (xresizewindow dpy window new-width new-height)
                     (xsync dpy False))))
            (unless (= type BUTTONRELEASE)
              (loop))))
        (when use-grab (xungrabserver dpy))
        (xungrabpointer dpy CURRENTTIME)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! buttons
      (list
       (make-button click-client-window mod-key BUTTON1 move-mouse)
       (make-button click-client-window mod-key BUTTON3 resize-mouse)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-window)
  (let ((windows (mapped-windows)))
    (display windows)
    (unless (null? windows)
      (let ((window (car windows)))
        (xraisewindow dpy window)
        (focus-window! window)))))

(define maximize-window
  (let ((last-window #f)
        (last-geom   #f))
    (lambda ()
      (let ((window (get-window-by-id selected)))
        (cond ((and window
                    (equal? window last-window))
               (xresizewindow dpy
                              window
                              (x-get-geometry-info-width  last-geom)
                              (x-get-geometry-info-height last-geom))
               (xmovewindow dpy
                            window
                            (x-get-geometry-info-x last-geom)
                            (x-get-geometry-info-y last-geom))
               (set! last-window #f))
              (window
               (set! last-window window)
               (set! last-geom (x-get-geometry dpy window))
               (let ((root-info (x-get-geometry dpy root)))
                 (xresizewindow dpy window
                                (- (x-get-geometry-info-width  root-info)
                                   border-width
                                   border-width)
                                (- (x-get-geometry-info-height root-info)
                                   0
                                   border-width
                                   border-width))
                 (xmovewindow dpy window 0 0))))))))

;;; Defaults

(global-keymap
 (list (make-key mod-key XK_RETURN (lambda () (system "xterm &")))
       (make-key mod-key XK_TAB    next-window)
       (make-key mod-key XK_F9     maximize-window)
       (make-key mod-key XK_LCQ    exit)
       (make-key mod-key XK_1 (lambda () (switch-to-workspace! 0)))
       (make-key mod-key XK_2 (lambda () (switch-to-workspace! 1)))
       (make-key mod-key XK_3 (lambda () (switch-to-workspace! 2)))
       (make-key mod-key XK_4 (lambda () (switch-to-workspace! 3)))
       (make-key mod-key XK_5 (lambda () (switch-to-workspace! 4)))
       (make-key mod-key XK_6 (lambda () (switch-to-workspace! 5)))
       (make-key mod-key XK_7 (lambda () (switch-to-workspace! 6)))
       (make-key mod-key XK_8 (lambda () (switch-to-workspace! 7)))
       (make-key mod-key XK_9 (lambda () (switch-to-workspace! 8)))
       (make-key mod-key XK_0 (lambda () (switch-to-workspace! 9)))))

(map-window-hook
 `((focus-window! ,focus-window!)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define event-loop
  (let ((ev (make-xevent)))
    (lambda ()
      (xsync dpy 0)
      (let loop ()
        (xnextevent dpy ev)
        (printf "event-loop : received event of type ~A~%" (xanyevent-type ev))
        (let ((handler (vector-ref handlers (xanyevent-type ev))))
          (when handler
            (handler ev)))
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

  ; grab all open windows and manage them
  (for-each map-window!
            (x-query-tree-info-children (x-query-tree dpy root)))

  (grab-keys)

  (when config-file
    (if (file-read-access? config-file)
        (load config-file)
        (fprintf (current-error-port)
                 "Could not read ~a.  Ignoring.\n"
                 config-file)))

  (print "Entering event loop...")
  (event-loop))

) ;; end module

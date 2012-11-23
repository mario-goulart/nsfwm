(module cwm
(start-wm)

(import chicken scheme foreign ports extras)
(use xlib lookup-table srfi-1 srfi-4 lolevel posix)

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
    (xgetgeometry dpy id (location root) (location x) (location y) (location width) (location height) (location border-width) (location depth))
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

(define (c-false? val) (= val 0))
(define (c-true?  val) (not (c-false? val)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dpy    #f)
(define screen #f)
(define root   #f)

(define move-cursor   #f)
(define resize-cursor #f)

(define click-root-window   'click-root-window)
(define click-client-window 'click-client-window)

(define windows (make-dict equal?))

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

(define keys #f)

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
   keys))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (key-press ev)
  (let ((keysym (xkeycodetokeysym dpy (xkeyevent-keycode ev) 0))) ; index is 1 for shifted keys (as XK_P) 0 for lower case keys (XK_LCP)
    (let ((key (find (lambda (k)
		       (and (= (key-keysym k) keysym)
			    (= (clean-mask (key-mod k))
			       (clean-mask (xkeyevent-state ev)))))
		     keys)))
      (printf "Key code ~A pressed event ~A (P should be ~A) list  ~A keyevent-state ~A -> found ~a~%" (xkeyevent-keycode ev) keysym XK_P (map (lambda(k) `(,(key-keysym k) ,(clean-mask (key-mod k)))) keys) (clean-mask (xkeyevent-state ev)) key)
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
	((dict-ref windows (xcrossingevent-window ev) #f) => focus-window!)
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
  (dict-set! windows id id)
  (xmapwindow dpy id)
  (xsync dpy False))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-request
  (let ((wa (make-xwindowattributes)))
    (lambda (ev)
      (let ((id (xmaprequestevent-window ev)))
	(when (and (c-true? (xgetwindowattributes dpy id wa))
                   (= (xwindowattributes-override_redirect wa) 0)
                   (not (dict-ref windows id #f)))
          (map-window! id))))))

(vector-set! handlers MAPREQUEST map-request)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define configure-request
  (let ((wc (make-xwindowchanges)))
    (lambda (ev)
      (set-xwindowchanges-x!		 wc (xconfigurerequestevent-x            ev))
      (set-xwindowchanges-y!		 wc (xconfigurerequestevent-y            ev))
      (set-xwindowchanges-width!	 wc (xconfigurerequestevent-width        ev))
      (set-xwindowchanges-height!	 wc (xconfigurerequestevent-height       ev))
      (set-xwindowchanges-border_width!	 wc (xconfigurerequestevent-border_width ev))
      (set-xwindowchanges-sibling!	 wc (xconfigurerequestevent-above        ev))
      (set-xwindowchanges-stack_mode!	 wc (xconfigurerequestevent-detail       ev))
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
  (let ((window (dict-ref windows (xbuttonevent-window ev) #f)))
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
  (let ((window (dict-ref windows (xdestroywindowevent-window ev) #f)))
    (when window
      (dict-delete! windows window))))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desktops-hidden (make-vector 10 '()))

(define (hide-mouse)
  (printf "selected ~A~%" selected)
  (when selected
    (let ((selection selected)
          (already-hidden (vector-ref desktops-hidden current-desktop)))
      (xunmapwindow dpy selection)
      (vector-set! desktops-hidden
                   current-desktop
                   (cons selection already-hidden)))
       (set! selected #f)))

(define (unhide id)
  (vector-set! desktops-hidden
	       current-desktop
	       (remove
		(lambda (i) (eq? i id))
		(vector-ref desktops-hidden current-desktop)))
  (xmapwindow dpy id))

(define (hidden-window-names)
  (with-output-to-string
   (lambda ()
     (printf " ")
     (for-each
      (lambda (name)
	(printf "~A "  name))
      (filter
       (lambda (name) name)
       (map
	(lambda (id)
	  (x-fetch-name dpy id))
	(vector-ref desktops-hidden current-desktop)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Desktops
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define viewable?
  (let ((wa (make-xwindowattributes)))
    (lambda (id)
      (xgetwindowattributes dpy id wa)
      (= (xwindowattributes-map_state wa) ISVIEWABLE))))

(define (window? id)
  (dict-exists? windows id))

(define (mapped-window? id)
  (and (viewable? id)
       (window? id)))

(define (mapped-windows)
  (filter mapped-window?
	  (x-query-tree-info-children (x-query-tree dpy root))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desktops (make-vector 10 '()))

(define current-desktop 0)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (switch-to-desktop i)

  (define (unmap-window id)
    (xunmapwindow dpy id))

  (define (map-window id)
    (xmapwindow dpy id))

  (vector-set! desktops current-desktop (mapped-windows))
  (for-each unmap-window (mapped-windows))
  (when (vector-ref desktops i)
    (for-each map-window (vector-ref desktops i)))
  (set! current-desktop i))


(set! buttons
      (list
       (make-button click-client-window mod-key BUTTON1 move-mouse)
       (make-button click-client-window mod-key BUTTON2 hide-mouse)
       (make-button click-client-window mod-key BUTTON3 resize-mouse)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (cycle-mapped-windows)
;;   (let ((windows (mapped-windows)))
;;     (if (>= (length windows) 2)
;; 	(let ((window (list-ref windows 1)))
;; 	  (XRaiseWindow dpy window)
;; 	  (focus-window! window)))))

(define (next-window)
  (let ((windows (mapped-windows)))
    (display windows)
    (unless (null? windows)
      (let ((window (car windows)))
        (xraisewindow dpy window)
        (focus-window! window)))))

;; (define (maximize)
;;   (if (dict-ref windows selected #f)
;;       (let ((root-info (x-get-geometry dpy root)))
;; 	(XResizeWindow dpy selected
;; 		       (- (x-get-geometry-info-width  root-info)
;; 			  border-width
;; 			  border-width)
;; 		       (- (x-get-geometry-info-height root-info)
;; 			  18 ;; dzen-height
;; 			  border-width
;; 			  border-width))
;; 	(XMoveWindow dpy selected 0 18))))

(define maximize
  (let ((last-window #f)
	(last-geom   #f))
    (lambda ()
      (let ((window (dict-ref windows selected #f)))
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

(set! keys
      (list (make-key mod-key XK_RETURN (lambda () (system "xterm &")))
	    (make-key mod-key XK_TAB    next-window)
	    (make-key mod-key XK_F9     maximize)
	    (make-key mod-key XK_LCQ      exit)
	    (make-key mod-key XK_1 (lambda () (switch-to-desktop 0)))
	    (make-key mod-key XK_2 (lambda () (switch-to-desktop 1)))
	    (make-key mod-key XK_3 (lambda () (switch-to-desktop 2)))
	    (make-key mod-key XK_4 (lambda () (switch-to-desktop 3)))
	    (make-key mod-key XK_5 (lambda () (switch-to-desktop 4)))
	    (make-key mod-key XK_6 (lambda () (switch-to-desktop 5)))
	    (make-key mod-key XK_7 (lambda () (switch-to-desktop 6)))
	    (make-key mod-key XK_8 (lambda () (switch-to-desktop 7)))
	    (make-key mod-key XK_9 (lambda () (switch-to-desktop 8)))
	    (make-key mod-key XK_0 (lambda () (switch-to-desktop 9)))))

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

(define (start-wm)
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

  ;(xseterrorhandler
  ; (lambda (dpy ee)
  ;   (fmt #t "Error handler called" nl)
  ;   1))

  (print "Entering event loop...")
  (event-loop))

) ;; end module

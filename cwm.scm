
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (import (rnrs)
;; 	(xitomatl fmt)
;; 	(ypsilon process)
;; 	(psilab xlib ffi)
;; 	(psilab xlib keysym)
;; 	(psilab xlib util)
;; 	(psilab xlib util x-get-geometry)
;; 	(psilab xlib util x-query-tree)
;; 	(psilab xlib util x-fetch-name))
(import foreign)
(use xlib lookup-table srfi-1 srfi-9 srfi-4 lolevel posix)

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

(define clients (make-dict equal?))

(define handlers (make-vector LASTEvent #f))

(define-record button click mask button procedure)

(define-record key mod keysym procedure)

(define num-lock-mask 0)

(define (clean-mask mask)
  (bitwise-and mask (bitwise-not (bitwise-ior num-lock-mask LOCKMASK))))

(define ButtonMask (bitwise-ior BUTTONPRESSMASK BUTTONRELEASEMASK))

(define mouse-mask (bitwise-ior ButtonMask POINTERMOTIONMASK))

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

(define (grab-buttons client focused)
  (xungrabbutton dpy ANYBUTTON ANYMODIFIER client)
  (if focused
      (for-each
       (lambda (b)
	 (if (eq? (button-click b) click-client-window)
	     (for-each
	      (lambda (modifier)
		(xgrabbutton dpy
			     (button-button b)
			     (bitwise-ior (button-mask b) modifier)
			     client
			     0 ButtonMask GRABMODEASYNC GRABMODESYNC NONE
			     NONE))
	      (list 0 LOCKMASK num-lock-mask
		    (bitwise-ior num-lock-mask LOCKMASK)))
	     (xgrabbutton dpy ANYBUTTON ANYMODIFIER client False ButtonMask
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
	((dict-ref clients (xcrossingevent-window ev) #f) => focus)
	(else (focus #f))))

(vector-set! handlers ENTERNOTIFY enter-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (focus-in ev)
  (if (and (integer? selected)
	   (not (= (xfocuschangeevent-window ev) selected)))
      (xsetinputfocus dpy selected REVERTTOPOINTERROOT CURRENTTIME)))

(vector-set! handlers FOCUSIN focus-in)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (manage id)
  (xsetwindowborderwidth dpy id border-width)
  (xsetwindowborder dpy id (get-color normal-border-color))
  (xselectinput dpy id (bitwise-ior ENTERWINDOWMASK
                                    FOCUSCHANGEMASK
                                    PROPERTYCHANGEMASK
                                    STRUCTURENOTIFYMASK))
  (grab-buttons id #f)
  (dict-set! clients id id)
  (xmapwindow dpy id)
  (xsync dpy False))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-request
  (let ((wa (make-xwindowattributes)))
    (lambda (ev)
      (let ((id (xmaprequestevent-window ev)))
	(if (and (c-true? (xgetwindowattributes dpy id wa))
		 (= (xwindowattributes-override_redirect wa) 0)
		 (not (dict-ref clients id #f)))
	    (manage id))))))

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

(define (focus client)

  (printf "  focus : start~%")

  (if (and selected (not (equal? client selected)))
      (begin
	(grab-buttons selected #f)
	(xsetwindowborder dpy selected (get-color normal-border-color))))

  (if client
      (begin
	(grab-buttons client #t)
	(xsetwindowborder dpy client (get-color selected-border-color))
	(xsetinputfocus   dpy client REVERTTOPOINTERROOT CURRENTTIME))
      (xsetinputfocus dpy root REVERTTOPOINTERROOT CURRENTTIME))

  (set! selected client))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (button-press ev)
  (printf "  button-press : start~%")
  (let ((client (dict-ref clients (xbuttonevent-window ev) #f)))
    (if client (focus client))
    (let ((click (if client click-client-window click-root-window)))
      (let ((button
	     (find
	      (lambda (b)
		(and (eq? (button-click b) click)
		     (= (button-button b)
			(xbuttonevent-button ev))
		     (= (clean-mask (button-mask b))
			(clean-mask (xbuttonevent-state ev)))))
	      buttons)))
	(if button
	    ((button-procedure button)))))))

(vector-set! handlers BUTTONPRESS button-press)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (destroy-notify ev)
  (let ((client (dict-ref clients (xdestroywindowevent-window ev) #f)))
    (if client
	(dict-delete! clients client))))

(vector-set! handlers DESTROYNOTIFY destroy-notify)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-mouse)
  (printf "  move-mouse : start~%")
  (let ((client selected))
    (if (and client
	     (= (xgrabpointer dpy root False mouse-mask
			      GRABMODEASYNC GRABMODEASYNC
			      NONE move-cursor CURRENTTIME)
		GRABSUCCESS))
	(begin
	  (xraisewindow dpy client)
	  (if use-grab (xgrabserver dpy))
	  (let ((ev (make-xevent)))
	    (let loop ()
	      (xmaskevent dpy
			  (bitwise-ior mouse-mask
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
				    client
				    (xmotionevent-x ev)
				    (xmotionevent-y ev))
		       (xsync dpy False)))
		(if (not (= type BUTTONRELEASE)) (loop)))))
	  (if use-grab (xungrabserver dpy))
	  (xungrabpointer dpy CURRENTTIME)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desktops-hidden (make-vector 10 '()))

(define (hide-mouse)
  (printf "selected ~A~%" selected)
  (unless (not selected)
    (let ((selection selected)
          (already-hidden (vector-ref desktops-hidden current-desktop)))
      (xunmapwindow dpy selection)
      (vector-set! desktops-hidden
                   current-desktop
                   (cons selection already-hidden)))
       (set! selected #f))
  (update-dzen))

(define (unhide id)
  (vector-set! desktops-hidden
	       current-desktop
	       (remove
		(lambda (i) (eq? i id))
		(vector-ref desktops-hidden current-desktop)))
  (xmapwindow dpy id)
  (update-dzen))

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

(define (dmenu-hidden)
  (let ((tbl (filter cdr
		     (map
		      (lambda (id)
			(cons id (x-fetch-name dpy id)))
		      (vector-ref desktops-hidden current-desktop)))))
    (unless (null? tbl)
	    (receive (in out id) 
	     (process (sprintf "dmenu -b -nb '~A' -sb '~A' -nf '~A' -sf '~A' "
			       menu-background-color
			       selected-menu-background-color
			       menu-foreground-color
			       selected-menu-foreground-color))
	 (let ((i 0))
	   (for-each
	    (lambda (cell)
	      (fprintf out "~A ~A~%" i (cdr cell))
	      (printf "~A ~A~!" i (cdr cell))
	      (set! i (+ i 1)))
	    tbl))
	 (fprintf out "~!")
;	 (flush-output-port in)
	 (close-output-port out)
	 (let ((result (read in)))
	   (printf "Got table ~A and result ~A~%" tbl result)
           (close-input-port in)
	   (if (integer? result)
	       (unhide (car (list-ref tbl result)))
	       (update-dzen)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resize-mouse)
  (let ((client selected))
    (if (and client
	     (= (xgrabpointer dpy root False mouse-mask
			      GRABMODEASYNC GRABMODEASYNC
			      NONE resize-cursor CURRENTTIME)
		GRABSUCCESS))
	(begin
	  (if use-grab (xgrabserver dpy))
	  (let ((ev (make-xevent)))
	    (define ResizeMask
	      (bitwise-ior mouse-mask
			   EXPOSUREMASK
			   SUBSTRUCTUREREDIRECTMASK))
	    (define client-x #f)
	    (define client-y #f)
	    (let ((info (x-get-geometry dpy client)))
	      (set! client-x (x-get-geometry-info-x info))
	      (set! client-y (x-get-geometry-info-y info)))
	    (let loop ()
	      (xmaskevent dpy ResizeMask ev)
	      (let ((type (xanyevent-type ev)))
		(cond ((or (= type CONFIGUREREQUEST)
			   (= type EXPOSE)
			   (= type MAPREQUEST))
		       ((vector-ref handlers type) ev))
		      ((= type MOTIONNOTIFY)
		       (let ((new-width  (- (xmotionevent-x ev) client-x))
			     (new-height (- (xmotionevent-y ev) client-y)))
			 (xresizewindow dpy client new-width new-height)
			 (xsync dpy False))))
		(if (not (= type BUTTONRELEASE))
		    (loop))))
	    (if use-grab (xungrabserver dpy))
	    (xungrabpointer dpy CURRENTTIME))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktops
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define is-viewable?
  (let ((wa (make-xwindowattributes)))
    (lambda (id)
      (xgetwindowattributes dpy id wa)
      (= (xwindowattributes-map_state wa) ISVIEWABLE))))

(define (is-client? id)
  (dict-exists? clients id))

(define (mapped-client? id)
  (and (is-viewable? id)
       (is-client? id)))

(define (mapped-clients)
 (filter mapped-client?
	  (x-query-tree-info-children (x-query-tree dpy root))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define desktops (make-vector 10 '()))

(define current-desktop 0)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (switch-to-desktop i)

  (define (unmap-client id)
    (xunmapwindow dpy id))

  (define (map-client id)
    (xmapwindow dpy id))

  (vector-set! desktops current-desktop (mapped-clients))

  (for-each unmap-client (mapped-clients))

  (if (vector-ref desktops i)
      (for-each map-client (vector-ref desktops i)))

  (set! current-desktop i)

  (update-dzen))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pager)
  (with-output-to-string
   (lambda ()
     (printf "[ ")
     (let ((n (vector-length desktops)))
       (let loop ((i 0))
	 (if (>= i n)
	     (printf " ]")
	     (begin
	       (cond ((= i current-desktop)
		      (printf "x"))
		     ((not (null? (vector-ref desktops i)))
		      (printf "-"))
		     ((not (null? (vector-ref desktops-hidden i)))
		      (printf "_"))
		     (else
		      (printf " ")))
	       (if (< i (- n 1))
		   (printf " | "))
	       (loop (+ i 1)))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dzen
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [ |_|_| |x| | |-| | | ]

(define dzen-stdin
  (if (= 0 (system "which dzen2"))
      (let ((p (open-output-pipe (sprintf "dzen2 -bg ~A -fg ~A" other-background-color menu-foreground-color))))
                     (set-buffering-mode! p #:line)
                     p)
      #f))

(define (update-dzen)
  (when dzen-stdin
    (fprintf dzen-stdin "~A ~A~%~!" (pager) (hidden-window-names))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dmenu-unmapped
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dmenu-unmapped)

  (define (not-viewable? c)
    (not (is-viewable? c)))

  (let* ((tbl (map
              (lambda (id)
                (cons id (x-fetch-name dpy id)))
              (filter not-viewable?
                       (dict-keys clients))))
         (ids (filter cdr tbl))
         (i 0)
         (windows (with-output-to-string
                    (lambda () (for-each
                                (lambda (cell)
                                  (printf "~A ~A~%" i  (cdr cell))
                                  (set! i (+ i 1)))
                                ids)))))
           (let ((result (with-input-from-pipe
                             (sprintf "echo ~A | dmenu -b -nb ~A -sb ~A -nf ~A -sf ~A"
                                      windows
                                      menu-background-color
                                      selected-menu-background-color
                                      menu-foreground-color
                                      selected-menu-foreground-color)
                           read)))
             (when (integer? result)
                 (xmapwindow dpy (car (list-ref tbl result)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! buttons
      (list
       (make-button click-client-window mod-key BUTTON1 move-mouse)
       (make-button click-client-window mod-key BUTTON2 hide-mouse)
       (make-button click-client-window mod-key BUTTON3 resize-mouse)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (cycle-mapped-clients)
;;   (let ((clients (mapped-clients)))
;;     (if (>= (length clients) 2)
;; 	(let ((client (list-ref clients 1)))
;; 	  (XRaiseWindow dpy client)
;; 	  (focus client)))))

(define (next-client)
  (let ((clients (mapped-clients)))
    (display clients)
    (if (not (null? clients))
	(let ((client (car clients)))
	  (xraisewindow dpy client)
	  (focus client)))))

;; (define (maximize)
;;   (if (dict-ref clients selected #f)
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
  (let ((last-client #f)
	(last-geom   #f))
    (lambda ()
      (let ((client (dict-ref clients selected #f)))
	(cond ((and client
		    (equal? client last-client))
	       (xresizewindow dpy
			      client
			      (x-get-geometry-info-width  last-geom)
			      (x-get-geometry-info-height last-geom))
	       (xmovewindow dpy
			    client
			    (x-get-geometry-info-x last-geom)
			    (x-get-geometry-info-y last-geom))
	       (set! last-client #f))
	      (client
	       (set! last-client client)
	       (set! last-geom (x-get-geometry dpy client))
	       (let ((root-info (x-get-geometry dpy root)))
		 (xresizewindow dpy client
				(- (x-get-geometry-info-width  root-info)
				   border-width
				   border-width)
				(- (x-get-geometry-info-height root-info)
				   18 ;; dzen-height
				   border-width
				   border-width))
		 (xmovewindow dpy client 0 18))))))))

(define (dmenu-run)
  (system (sprintf "dmenu_run -b -nb '~A' -sb '~A' -nf '~A' -sf '~A' &"
                   menu-background-color
                   selected-menu-background-color
                   menu-foreground-color
                   selected-menu-foreground-color)))


(set! keys
      (list (make-key mod-key XK_RETURN (lambda () (system "xterm &")))
	    (make-key mod-key XK_TAB    next-client)
	    (make-key mod-key XK_LCP      dmenu-run)
	    (make-key mod-key XK_LCU      dmenu-unmapped)
	    (make-key mod-key XK_LCH      dmenu-hidden)
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
	  (if handler
	      (handler ev)))
	(loop)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;grab all open windows and manage them
(for-each manage
          (x-query-tree-info-children (x-query-tree dpy root)))

(grab-keys)

;(xseterrorhandler
; (lambda (dpy ee)
;   (fmt #t "Error handler called" nl)
;   1))

(update-dzen)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "cons-wm is setup~%")

(event-loop)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


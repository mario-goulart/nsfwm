(foreign-declare "#include <X11/extensions/Xrandr.h>")

(define (xrandr-query-extension dpy)
  (let-location ((ev-offset int 0)
                 (err-offset int 0))
    (let ((query-ext
           (foreign-lambda bool "XRRQueryExtension"
                           c-pointer (c-pointer int) (c-pointer int))))
      (if (query-ext dpy (location ev-offset) (location err-offset))
          (cons ev-offset err-offset)
          #f))))

(define (xrandr-query-version dpy)
  (let-location ((major int 0)
                 (minor int 0))
    (let ((query-version
           (foreign-lambda bool "XRRQueryVersion"
                           c-pointer (c-pointer int) (c-pointer int))))
      (if (query-version dpy (location major) (location minor))
          (cons major minor)
          #f))))

(define (xrandr-select-input dpy window-id mask)
  (foreign-lambda void "XRRSelectInput" c-pointer int int))

(define xrandr-screen-change-notify
  (foreign-value "RRScreenChangeNotify" int))

(define xrandr-screen-change-notify-mask
  (foreign-value "RRScreenChangeNotifyMask" int))

(define xrandr-update-configuration
  (foreign-lambda int "XRRUpdateConfiguration" (c-pointer "XEvent")))

;; Code adapted from the xlib-utils egg for CHICKEN 4, by John J
;; Foerch.

(define-record property type format data count)

(define (%make-u32-property prop-type)
  (lambda (u32)
    (let-location ((data unsigned-long u32))
      (make-property prop-type 32 (location data) 1))))

(define (%make-u32-list-property prop-type)
  (lambda (list-of-u32)
    (let* ((vec (list->u32vector list-of-u32))
           (len (u32vector-length vec))
           (lvec ((foreign-lambda* c-pointer ((u32vector s) (int length))
                   "unsigned long * lvec = malloc(sizeof(unsigned long) * length);"
                   "int i;"
                   "for (i = 0; i < length; i++) {"
                   "    lvec[i] = s[i];"
                   "}"
                   "C_return(lvec);")
                  vec len)))
      (set-finalizer! lvec free)
      (make-property prop-type 32 lvec len))))

(define (%make-text-property encoding)
  (lambda (s)
    (let* ((b (blob->u8vector/shared
               (string->blob s)))
           (l (u8vector-length b))
           (p (make-xtextproperty))
           (t ((foreign-lambda* c-pointer ((u8vector b) (int l))
                "unsigned char *t = malloc(l);"
                "memcpy(t, b, l);"
                "C_return(t);") b l)))
      (set-xtextproperty-value! p t)
      (set-xtextproperty-encoding! p encoding)
      (set-xtextproperty-format! p 8)
      (set-xtextproperty-nitems! p l)
      p)))


(define (make-atom-property atom-name)
  (let ((data (xinternatom *dpy* atom-name 0)))
    ((%make-u32-property "ATOM") data)))

(define make-number-property (%make-u32-property "CARDINAL"))

(define make-numbers-property (%make-u32-list-property "CARDINAL"))

(define make-window-property (%make-u32-property "WINDOW"))

(define make-windows-property (%make-u32-list-property "WINDOW"))

(define make-text-property (%make-text-property XA_STRING))

(define (make-utf8-property str)
  (let ((utf8-atom (xinternatom *dpy* "UTF8_STRING" 0)))
    (make-property "UTF8_STRING" 8 (location str) (string-length str))))

(define (window-property-set! window key value)
  (xchangeproperty *dpy*
                   window
                   (xinternatom *dpy* key 0)
                   (xinternatom *dpy* (property-type value) 0)
                   (property-format value)
                   PROPMODEREPLACE
                   (property-data value)
                   (property-count value)))

(define (%window-get-text-property window prop)
  ;; FIXME: implement it properly
  (let ((win-id (window-id window))
        (prop-atom (xinternatom *dpy* prop 0)))
    (let-location ((type        unsigned-long)
                   (format      int32)
                   (nitems      unsigned-long)
                   (bytes-after unsigned-long)
                   (data        c-string*))
      (define (get-prop long-length)
        (eq? SUCCESS
             (xgetwindowproperty *dpy*
                                 (window-id window)
                                 prop-atom
                                 0
                                 long-length
                                 0
                                 ANYPROPERTYTYPE
                                 (location type)
                                 (location format)
                                 (location nitems)
                                 (location bytes-after)
                                 (location data))))
      ;; Just to set bytes-after with the total number of bytes to read
      (and (get-prop 0)
           (fx= format 8) ;; Only support strings for now
           (get-prop (fx+ 1 (fx/ bytes-after 4)))
           data))))

(define (%window-get-window-property window prop)
  (let ((win-id (window-id window))
        (property (xinternatom *dpy* prop 0)))
    (let-location ((xa_ret_type unsigned-long)
                   (ret_format int)
                   (ret_nitems unsigned-long)
                   (ret_bytes_after unsigned-long)
                   (ret_window_id unsigned-c-string*))
      (xgetwindowproperty *dpy* win-id property
                          0 4 0
                          ANYPROPERTYTYPE
                          (location xa_ret_type)
                          (location ret_format)
                          (location ret_nitems)
                          (location ret_bytes_after)
                          (location ret_window_id))
      (if (= NONE xa_ret_type) ;; property did not exist
          #f
          (let ((ret-window-id
                 ((foreign-lambda* unsigned-long ((c-pointer data))
                    "unsigned long** longs = (unsigned long**)data;"
                    "C_return(longs[0][0]);")
                  (location ret_window_id))))
            (if (= NONE ret-window-id)
                #f
                ret-window-id))))))

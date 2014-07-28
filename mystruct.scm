;; My own structures.
;; Richard van Roy (c) 2014
;; License: do what you like.
;; Usage:
;; (define-mystruct <name> <slot-def> [<wrapper>])
;; slot-dev: 
;; <slot-name>
;; (<slot-name> <default-value> [getter] [setter])
;; (<slot-name> <default-value> _ [setter])
;; (<slot-name> <default-value> getter _) ; make slot read-only
;; (<slot-name> <default-value> _ _)      ; make slot read-only
;; (<name>:create #!optinal values)
;; (<name>? obj)
;; (<name>:<slot-name> obj)
;; (set! (<name>:<slot-name> obj) value)

(module mystruct
	(define-mystruct)
	(import chicken scheme)
	(use srfi-1 matchable)

 (define-syntax %define-mystruct
   (ir-macro-transformer
    (lambda (exp inj cmp?)
      (apply (lambda (_ name wrapper #!rest slots)
	       
	       `(begin	      
		  (define (,(inj (symbol-append (inj name) ':create))
			   #!optional
			   ,@(map (lambda (x) (list (car x) (cadr x))) slots))
		    (,wrapper
		     (vector ',(inj name) ,@(map car slots))))

		  (define (,(symbol-append (inj name) '?) obj)
		    (and (vector? obj)
			 (eq? (vector-ref obj 0) ',(inj name))))

		  ,@(map 
		     (lambda (slot count)
		       (apply (lambda (slot-name _ getter setter)
				`(define ,(symbol-append (inj name) ':
							 (inj slot-name))
				   (getter-with-setter
				    (lambda (obj)
				      (unless
				       (,(symbol-append (inj name) '?) obj)
				       (print "wrong object"))
				      (,getter
				       (vector-ref obj ,(+ count 1))))
				    (lambda (obj value)
				      (unless
				       (,(symbol-append (inj name) '?) obj)
				       (print "wrong object"))
				      (vector-set! obj ,(+ count 1)
						   (,setter obj value))))))
			      slot))
		     slots (iota (length slots)))))
	     exp))))

 (define-syntax define-mystruct
   (syntax-rules (_)

     ((_ name ((slot-name default)
	       rest ...)
	 wrapper ...)
      (define-mystruct name 
	(rest ... (slot-name default (lambda (x) x)))
	wrapper ...))

     ((_ name ((slot-name default getter)
	       rest ...)
	 wrapper ...)
      (define-mystruct name
	(rest ... (slot-name default getter (lambda (o v) v)))
	wrapper ...))

     ((_ name ((slot-name default _ setter)
	       rest ...)
	 wrapper ...)
      (define-mystruct name
	(rest ... (slot-name default (lambda (x) x) setter))
	wrapper ...))

     ((_ name ((slot-name default getter _) rest ...))
      (define-mystruct name
     	(rest ... (slot-name default getter
     		    (lambda (_) (error "read only"))))
     	(lambda (x) x)))

     ((_ name ((slot-name default getter setter) ...))
      (define-mystruct name
	((slot-name default getter setter) ...)
	(lambda (x) x)))


     ((_ name ((slot-name default getter setter) ...)
	 wrapper)
      (%define-mystruct name wrapper
			(slot-name default getter setter) ...))

     ((_ name (slot-dev rest ...)
	 wrapper ...)
      (define-mystruct name
	((slot-dev #f) rest ...)
	wrapper ...))
     ))

)
 

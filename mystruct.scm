;; My own structures.
;; Richard van Roy (c) 2014
;; License: do what you like.
;;
;; Usage:
;; (define-mystruct <name> <slots> <wrapper>)
;; slots: 
;; <slot-name>
;; (<slot-name> #!key value getter-wrap setter-wrap)
;; (<name>:create values ...)
;; (<name>:make (key value) ...)
;; (<name>? obj)
;; (<name>:<slot-name> obj)
;; (set! (<name>:<slot-name> obj) value)

(module mystruct
	(define-mystruct)
	;; for using #!key and #!optional inside a macro.
	(import-for-syntax chicken scheme)
	(import chicken scheme)
	(use srfi-1 matchable)

(define-syntax define-mystruct
  (ir-macro-transformer
   (lambda (exp inj cmp)
     (apply
      (lambda (_ <name> <slots> #!optional <wrapper>)

	;; Wrapper functions default to stub functions that simply
	;; returns the value passed to it.
	(define (prep-slot <slot/name>)
	  (apply (lambda (<slot-name> #!key value
				 (getter-wrap (lambda (v) v))
				 (setter-wrap (lambda (o v) v)))
		   (let ((<value> value)
			 (<getter-wrap> getter-wrap)
			 (<setter-wrap> setter-wrap))
		    `(,<slot-name> ,<value> ,<getter-wrap> ,<setter-wrap>))) 
		 (if (list? <slot/name>) <slot/name> `(,<slot/name> #f))))

	(define (create-name)
	  (symbol-append (inj <name>) ':create))

	(define (make-name)
	  (symbol-append (inj <name>) ':make))

	(define (pred-name)
	  (symbol-append (inj <name>) '?))

	(define (ref-name <slot-name>)
	  (symbol-append (inj <name>) ': (inj <slot-name>)))

	(define (init-args <prepped-slots>)
	  (map (lambda (<slot>)
		 (list (car <slot>)
		       (cadr <slot>)))
	       <prepped-slots>))

	(define (slot-names <prepped-slots>)
	  (map car <prepped-slots>))

	(define (check-type)
	  `(unless (,(pred-name) obj)
		   (error (sprintf "~a: wrong object, expected object of type '~a'." 
				   obj ',<name>))))

	(define (slot-ref <slot> <pos>)
	  (apply (lambda (<slot-name> <value> <getter-wrap> <setter-wrap>)
		   `(define ,(ref-name <slot-name>)
		      (getter-with-setter
		       (lambda (obj)
			 ,(check-type)
			 (,<getter-wrap> (vector-ref obj ,<pos>)))
		       (lambda (obj value)
			 ,(check-type)
			 (vector-set! obj ,<pos> (,<setter-wrap> obj value))))))
		 <slot>))

	(let ((<prepped-slots> (map prep-slot <slots>)))
	 `(begin
	    ;; An initialiser function that takes its slot arguments as a
	    ;; key-value pair.
	    (define (,(make-name) #!key ,@(init-args <prepped-slots>))
	      (,(if <wrapper> <wrapper> (lambda (x) x))
	       (vector ',<name> ,@(slot-names <prepped-slots>))))

	    ;; An initialiser function that takes its slot arguments as a
	    ;; list with the same order as specified in define-mystruct.
	    (define (,(create-name) #!optional ,@(init-args <prepped-slots>))
	      (,(if <wrapper> <wrapper> (lambda (x) x))
	       (vector ',<name> ,@(slot-names <prepped-slots>))))

	    ;; A predicate function that checks if an object is of this type.
	    (define (,(pred-name) obj)
	      (and
	       (vector? obj)
	       (eq? (vector-ref obj 0) ',<name>)))

	    ;; Getters and setters for every slot specified.
	    ,@(map slot-ref <prepped-slots> (iota (length <prepped-slots>) 1))
	    )))  exp))))

)

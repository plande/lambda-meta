(define-module (lambda syntax)
  #:use-module (grand scheme)
  #:use-module (lambda focus))

(define (merge-bindings bindings . bindings*)
  (define (merge-bindings a b)
    (and a b
	 (fold-left (lambda (bindings (key . value))
		      (and bindings
			   (cond ((assoc key bindings)
				  => (lambda ((key . value*))
				       (and (equal? value value*)
					    bindings)))
				 (else
				  `((,key . ,value)
				    . ,bindings)))))
		    a
		    b)))
  (fold-left merge-bindings bindings bindings*))

(e.g.
 (merge-bindings '((a . 1) (b . 2))
		 '((c . 3) (b . 2))
		 '((a . 1)))
 ===> ((c . 3) (a . 1) (b . 2)))

(e.g.
 (merge-bindings '((a . 1) (b . 2))
		 '((a . 2)))
 ===> #false)

(define (zip-bindings list-of-bindings)
  (match list-of-bindings
    ((((names . values) ...) ...)
     ;(assert (apply eq? names))
     (match names
       ((names . _)
	(apply map list names values))
       (()
	'())))))

(e.g.
 (zip-bindings '(((a . 1) (b . 2) (c . 3))
		 ((a . 4) (b . 5) (c . 6))
		 ((a . 7) (b . 8) (c . 9))))
 ===> ((a 1 4 7) (b 2 5 8) (c 3 6 9)))

(define (carry #;from prefix #;to suffix #;until success?)
  (let ((result (success? prefix suffix)))
    (if (or result (null? prefix))
	result
	(let (((initial ... last) prefix))
	  (carry #;from initial #;to `(,last . ,suffix)
			#;until success?)))))

(e.g.
 (carry #;from '(3 2 1) #;to '() #;until (lambda (prefix suffix)
					   (and (= (sum prefix)
						   (sum suffix))
						`(,prefix ,suffix))))
 ===> ((3) (2 1)))

(define (...? x)
  (eq? x '...))

(define (bind pattern #;to form . bound-variables)
  (match pattern
    (('quote literal)
     (and (equal? form literal)
	  bound-variables))

    ((repetition (? ...?) . remaining)
     (bind-sequence repetition remaining form
		    bound-variables))

    ((head/pattern . tail/pattern)
     (match form
       ((head/form . tail/form)
	(let ((bound (apply bind head/pattern head/form
			    bound-variables)))
	  (and bound
	       (apply bind tail/pattern tail/form bound))))
       (_
	#false)))

    (_
     (if (symbol? pattern)
         (merge-bindings `((,pattern . ,form)) bound-variables)
         (and (equal? pattern form)
              bound-variables)))))

(define (prefix-length condition? l)
  (define (traverse l n)
    (match l
      ((head . tail)
       (if (condition? head)
	   (traverse tail (+ n 1))
	   n))
      (_
       n)))
  (traverse l 0))

(e.g.
 (prefix-length even? '(2 4 6 7 8 10)) ===> 3)

(define (bind-sequence repeated-pattern remaining-pattern
		       form bound-variables)
  (define (successful-match? prefix suffix)
    (let* ((bindings (map (lambda (form)
			    (bind repeated-pattern form))
			  prefix))
	   (zipped (zip-bindings bindings))
	   (merged (merge-bindings bound-variables zipped)))
      (and merged (apply bind remaining-pattern suffix
			 merged))))
  
  (let* ((limit (prefix-length (lambda (constituent)
				 (bind repeated-pattern
				       constituent))
			       form))
	 (prefix rest (split-at form limit)))
    (carry #;from prefix #;to rest
		  #;until successful-match?)))

(define (used-symbols expression)
  (match expression
    (('quote literal)
     '())

    ((repeated '... . rest)
     (union (used-symbols repeated)
	    (used-symbols rest)))

    ((head . tail)
     (union (used-symbols head)
	    (used-symbols tail)))

    (_ 
     (if (symbol? expression)
	 `(,expression)
	 '()))))

(define (unique-symbol x)
  (make-symbol (->string x)))

(define (fill template #;with bindings)
  (let* ((missing (difference (used-symbols template)
			      (map (lambda ((key . value))
				     key)
				   bindings)))
	 (bindings `(,@(map (lambda (symbol)
			      `(,symbol . ,(unique-symbol symbol)))
			    missing) ,@bindings)))
    (fill-template template bindings)))

(define (fill-template template bindings)
  (match template
    (('quote literal)
     literal)

    ((repeated (? ...?) . rest)
     `(,@(fill-sequence repeated bindings)
       . ,(fill-template rest #;with bindings)))

    ((head . tail)
     `(,(fill-template head #;with bindings)
       . ,(fill-template tail #;with bindings)))

    (_
     (cond ((and (symbol? template) (assoc template bindings))
	    => (lambda ((key . value))
		 value))

	   (else
	    template)))))

(define (unzip-bindings bindings keys)
  (let* ((unzipped (filter (lambda ((key . value))
			     (is key member keys))
			   bindings))
	 (((names . values) ...) unzipped))
    (map (lambda (singular-values)
	   `(,@(map (lambda (name value)
		      `(,name . ,value))
		    names singular-values)
	     ,@bindings))
	 (if (null? values)
	     '()
	     (transpose values)))))

(e.g.
 (unzip-bindings '((a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4)) '(a c e))
 ===> (((a . 1) (c . 1) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 2) (c . 2) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 3) (c . 3) (a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))))

(define (fill-sequence template bindings)
  (let* ((symbols (used-symbols template))
	 (binding-sequences (unzip-bindings bindings symbols)))
    (map (lambda (bindings)
	   (fill-template template bindings))
	 binding-sequences)))

(define core-macros
  '((('let ((name value) ...)
       . body)
     (('lambda (name ...) . body) value ...))

    (('let* () . body)
     ('begin . body))

    (('let* ((name-1 value-1)
	     (name-2 value-2) ...)
       . body)
     ('let ((name-1 value-1))
       ('let* ((name-2 value-2) ...)
         . body)))

    (('and)
     #true)

    (('and last)
     last)

    (('and first . rest)
     ('if first ('and . rest) #false))

    (('or)
     #false)

    (('or last)
     last)

    (('or first . rest)
     ('let ((result first))
       ('if result result ('or . rest))))))


(define (expansions expression macros)

  (define (unfold f x)
    
    (define (unfold-into result x)
      (let* ((f/x (f x)))
	(if f/x
	    (unfold-into `(,f/x . ,result) f/x)
	    result)))
    
    (unfold-into '() x))
  
  (define ((expansion form) macro)
    (and-let* (((pattern template) macro)
	       (bindings (bind pattern form)))
      `(,(fill template bindings) ,macro)))

  (define (transform `(,expression . ,_))
    (and-let* ((locus `(,expansion ,macro)
		      (find-focus (lambda (form focus)
				    (and (focus-on-expression? focus)
					 (any (expansion form) macros)))
				  expression))
	       (expanded (replace-subexpression #;of expression #;at locus
						     #;with expansion)))
      `(,expanded ,locus ,macro)))

  (and-let* ((((expressions . loci+macros) ...) (unfold transform
							`(,expression)))
	     (final (if (null? expressions)
			expression
			(first expressions))))
    (values final loci+macros)))

(e.g.
 (expansions '(let* ((x 5)
		     (y 10))
		(and (even? x)
		     (even? y))) core-macros)
 ===> ((lambda (x) ((lambda (y) (begin (if (even? x) (even? y) #f))) 10)) 5)

 (((0 2 0 2 1 2) (('and last)
		  last))
  ((0 2 0 2 1) (('and first . rest)
		('if first ('and . rest) #f)))
  ((0 2 0 2) (('let* () . body)
	      ('begin . body)))
  ((0 2) (('let ((name value) ...) . body)
	  (('lambda (name ...) . body) value ...)))
  ((0 2) (('let* ((name-1 value-1) (name-2 value-2) ...)
	    . body)
	  ('let ((name-1 value-1))
	    ('let* ((name-2 value-2) ...) . body))))
  (() (('let ((name value) ...) . body)
       (('lambda (name ...) . body) value ...)))
  (() (('let* ((name-1 value-1) (name-2 value-2) ...)
	 . body)
       ('let ((name-1 value-1))
	 ('let* ((name-2 value-2) ...) . body))))
  ))


(define (impanded expression loci+macros)
  (fold-left (lambda (expression `(,locus (,template ,pattern)))
	       (let* ((subexpression (focus expression locus))
		      (bindings (bind pattern subexpression))
		      (impanded (fill template bindings)))
		 (replace-subexpression expression locus impanded)))
	     expression
	     loci+macros))

(e.g.
 (let* ((original '(let* ((x 5)
			  (y 10))
		     (and (even? x)
			  (even? y))))
	(expanded loci+macros (expansions original core-macros))
	(impanded (impanded expanded loci+macros)))
   (equal? original impanded)))

(define (expand expression macros)

  (define (fix f x)
    (let ((f/x (f x)))
      (if (equal? x f/x)
	  x
	  (fix f f/x))))
  
  (define (transform expression)
    (let ((result (any (lambda ((pattern template))
			 (let ((bindings (bind pattern
					       expression)))
			   (and bindings
				`(,bindings ,template))))
		       macros)))
      (match result
	((bindings template)
	 (fill template bindings))

	(_
	 expression))))

  (define (expand expression)
    (match expression
      (('quote _)
       expression)

      (('lambda args body)
       `(lambda ,args ,(expand body)))

      (('if condition then else)
       `(if ,(expand condition)
	    ,(expand then)
	    ,(expand else)))

      ((operator . operands)
       (let ((transformed (fix transform expression)))
	 (if (equal? expression transformed)
	     `(,(expand operator) . ,(map expand operands))
	     (expand transformed))))
      
      (_
       expression)))

  (expand expression))




	 

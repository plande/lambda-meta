(define-module (lambda meta)
  #:use-module (grand scheme)
  #:use-module (lambda symbol)
  #:use-module (ice-9 regex))

(define (extend bindings #;with keys #;mapped-to targets)
  (match `(,keys ,targets)
    (`((,key . ,keys) (,target . ,targets))
     (extend `((,key . ,target) . ,bindings) #;with keys
	     #;mapped-to targets))
    (`(() ())
     bindings)
    
    (`(,key ,target)
     `((,key . ,target) . ,bindings))))

(e.g.
 (extend '((a . a*) (b . b*)) #;with '(a . c) '(a** . c**))
 ===> ((c . c**) (a . a**) (a . a*) (b . b*)))

(publish
 (define alpha-parameter/new-name-for-symbol
   (make-parameter (lambda _ (gensym "_"))))

 (define ((alpha bindings) expression)
   (match expression
     (`(quote ,literal)
      expression)

     (`(if ,condition ,consequent ,alternative)
      (let* ((condition* ((alpha bindings) condition))
	     (consequent* ((alpha bindings) consequent))
	     (alternative* ((alpha bindings) alternative)))
	`(if ,condition* ,consequent* ,alternative*)))

     (`(lambda ,args ,body)
      (let* ((args* (new-names args))
	     (bindings* (extend bindings #;with args
				#;mapped-to args*))
	     (body* ((alpha bindings*) body)))
	`(lambda ,args* ,body*)))

     (`(,operator . ,operands)
      `(,((alpha bindings) operator)
	. ,(map (alpha bindings) operands)))

     (_
      (cond ((and (symbol? expression)
		  (assoc expression bindings))
	     => (lambda ((key . value))
		  value))
	    (else
	     expression)))))
 where
 (define (new-names args)
   (match args
     ((first . rest)
      `(,((alpha-parameter/new-name-for-symbol) first)
	. ,(new-names rest)))
     (()
      '())
     (tail
      ((alpha-parameter/new-name-for-symbol) tail)))))

(define (alpha-normalized expression)
  (let* ((suffixes (filter-map (lambda (symbol)
				 (and-let* ((s (symbol->string symbol))
					    (ms (string-match "^[?]([0-9]+)$" s))
					    (ns (match:substring ms 1)))
				   (string->number ns)))
			       (unbound-symbols expression)))
	 (initial (apply max 0 suffixes)))
    (parameterize ((alpha-parameter-new-name-for-symbol
		    (next-symbol "?" initial)))
      ((alpha '()) expression))))

(e.g.
 (alpha-normalized '(lambda (x) (+ x x)))
 ===> (lambda (?1) (+ ?1 ?1)))

(define (alpha-equivalent? a b)
  (equal? (alpha-normalized a)
	  (alpha-normalized b)))

(e.g.
 (alpha-equivalent? '(lambda (x) (+ x x))
		    '(lambda (y) (+ y y))))

(define (idempotent? function value)
  (equal? (function value)
	  (function (function value))))

(e.g.
 (and (idempotent? alpha-normalized '(lambda (x) (* x x)))
      (idempotent? alpha-normalized '(lambda (x)
				       (* ((lambda (x) (+ x x)) x)
					  ((lambda (x) (* x 3)) x))))))

(define-module (lambda meta)
  #:use-module (grand scheme)
  #:use-module (ice-9 regex))

(define new-name-for-symbol
  (make-parameter (lambda _ (gensym "_"))))

(define ((next-symbol prefix initial) . _)
  (set! initial (+ initial 1))
  (pass initial number->string
	(lambda (s) (string-append prefix s))
	string->symbol))

(define (new-names args)
  (match args
    ((first . rest)
     `(,((new-name-for-symbol) first) . ,(new-names rest)))
    (()
     '())
    (tail
     ((new-name-for-symbol) tail))))

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
     `(,((alpha bindings) operator) . ,(map (alpha bindings) operands)))

    (_
     (cond ((and (symbol? expression)
		 (assoc expression bindings))
	    => (lambda ((key . value))
		 value))
	   (else
	    expression)))))

(define (symbols expression)
  (match expression
    (`(,head . ,tail)
     (union (symbols head) (symbols tail)))
    (_
     (if (symbol? expression)
	 `(,expression)
	 '()))))

(e.g.
 (symbols '(+ (* 2 3) (/ 4 5))) ===> (* / +))

(define (unbound-symbols expression)
  (match expression
    (`(quote ,literal)
     '())
    
    (`(if ,condition ,consequent ,alternative)
     (union (unbound-symbols condition)
            (unbound-symbols consequent)
            (unbound-symbols alternative)))

    (`(lambda ,args ,body)
     (difference (unbound-symbols body) args))

    (`(,operator . ,operands)
     (union (unbound-symbols operator)
            (unbound-symbols operands)))

    (_
     (if (symbol? expression)
         `(,expression)
         '()))))

(e.g.
 (unbound-symbols '(lambda (x) (+ x y))) ===> (y +))


(define (alpha-normalized expression)
  (let* ((suffixes (filter-map (lambda (symbol)
				 (and-let* ((s (symbol->string symbol))
					    (ms (string-match "^[?]([0-9]+)$" s))
					    (ns (match:substring ms 1)))
				   (string->number ns)))
			       (unbound-symbols expression)))
	 (initial (apply max 0 suffixes)))
    (parameterize ((new-name-for-symbol (next-symbol "?" initial)))
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



(e.g.
 (let ((idemp-test (lambda (expr) (alpha-equivalent? expr (alpha-normalized expr)))))
   (and (idemp-test '(lambda (x) (* x x)))
        (idemp-test '(lambda (x) (* ((lambda (x) (+ x x)) x) ((lambda (x) (* x 3)) x)))))))

    



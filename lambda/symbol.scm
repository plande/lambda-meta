(define-module (lambda symbol)
  #:use-module (grand scheme)
  #:export (next-symbol symbols unbound-symbols))


(define ((next-symbol prefix initial) . _)
  (set! initial (+ initial 1))
  (pass initial number->string
	(lambda (s) (string-append prefix s))
	string->symbol))

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

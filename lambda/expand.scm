(define-module (lambda expand)
  #:use-module (grand scheme)
  #:export (bind
	    fill
	    merge-bindings
	    expand
	    expand/track
	    impand))

(define-syntax (assert condition)
  (unless condition
    (error "Assertion failed: "'condition)))

(define (merge-bindings bindings . bindings*)
  (define (merge-bindings a b)
    (and a b
         (fold-left
	  (lambda (bindings `(,key . ,value))
            (and bindings
                 (cond ((assoc key bindings)
                        => (lambda (`(,key . ,value*))
                             (and (equal? value value*)
                                  bindings)))
                       (else
                        `((,key . ,value)
                          . ,bindings)))))
          a
          b)))
  (fold-left merge-bindings bindings bindings*))

(e.g.
 (same-set? (merge-bindings '((a . 1) (b . 2))
			    '((a . 1) (c . 3)))
	    '((a . 1) (b . 2) (c . 3))))

(e.g.
 (not (merge-bindings '((a . 1) (b . 2))
		      '((a . 4) (c . 3)))))

(define (zip-bindings list-of-bindings)
  (let (((((names . values) ...) ...) list-of-bindings))
    (assert (apply equal? names))
    (match names
      (`(,names . ,_)
       (apply map list names values))
      ('()
       '()))))

(e.g.
 (zip-bindings '(((a . 1) (b . 2) (c . 3))
		 ((a . 4) (b . 5) (c . 6))
		 ((a . 7) (b . 8) (c . 9))))
 ===> ((a 1 4 7) (b 2 5 8) (c 3 6 9)))


(define (carry #;from prefix #;to suffix
		      #;until success&=>)
  (let ((result (success&=> prefix suffix)))
    (if (or result (null? prefix))
	result
    ;;else
	(let (((initial ... last) prefix))
	  (carry #;from initial #;to `(,last . ,suffix)
			#;until success&=>)))))

(e.g. ;; from (grand list)
 (is '(a b c) prefix? '(a b c d e)))

(e.g.
 (carry '(a  m a n  a  p l a n  a  c a n a l)
	'(p a n a m a)
	(lambda (prefix suffix)
	  (and (is prefix prefix? (reverse suffix))
	       `(,prefix ,suffix))))
 ===> ((a  m a n  a  p l a n  a)
       (c a n a l  p a n a m a)))

(define (prefix-length condition? l)
  (define (traverse l n)
    (match l
      ((head . tail)
       (if (condition? head)
	   (traverse tail (+ n 1))
	   ;;else
           n))
      (_
       n)))
  (traverse l 0))

(e.g.
 (prefix-length even? '(0 2 4 5 6 7)) ===> 3)

;; workaround for literal ... symbol in the Shinn matcher
(define ...? (is _ eq? '...))

(define (bind pattern #;to form)

  (define (bind-sequence repeated-pattern
			 remaining-pattern
			 form bound-variables)
    (define (bind-repeated form)
      (bind repeated-pattern form '()))

    (define (successful-match prefix suffix)
      (let* ((bindings (map bind-repeated prefix))
             (zipped (zip-bindings bindings))
             (merged (merge-bindings bound-variables
				     zipped)))
	(and merged
	     (bind remaining-pattern suffix merged))))
    
    (let* ((limit (prefix-length bind-repeated form))
	   (prefix rest (split-at form limit)))
      (carry #;from prefix #;to rest
		    #;until successful-match)))

  (define (bind pattern form bound-variables)
    (match pattern
      (`(quote ,literal)
       (and (equal? form literal)
            bound-variables))
      (`(,repetition ,(? ...?) . ,remaining)
       (bind-sequence repetition remaining form
                      bound-variables))
      (`(,head/pattern . ,tail/pattern)
       (and-let* ((`(,head/form . ,tail/form) form)
		  (bound (bind head/pattern head/form
			       bound-variables)))
         (bind tail/pattern tail/form bound)))
      (_
       (if (symbol? pattern)
           (merge-bindings bound-variables
			   `((,pattern . ,form)))
           (and (equal? pattern form)
		bound-variables)))))
  (bind pattern form '()))
  
(e.g.
 (same-set?
  (bind '(a b . c) '(1 2 3))
  '((a . 1) (b . 2) (c . (3)))))

(e.g.
 (same-sets? (bind '('let ((names values) ...) . body)
		   '(let ((a 5) (b 10)) (+ a b)))
             '((names . (a b))
	       (values . (5 10))
	       (body . ((+ a b))))))

(e.g. (not (bind '(a b c) '(1 2))))

(define (used-symbols expression)
  (match expression
    (`(quote ,literal)
     '())

    (`(,repeated ,(? ...?) . ,rest)
     (union (used-symbols repeated)
	    (used-symbols rest)))

    (`(,head . ,tail)
     (union (used-symbols head)
	    (used-symbols tail)))
    (_ 
     (if (symbol? expression)
	 `(,expression)
	 '()))))

(e.g.
 (same-sets? (used-symbols '(a ... a b ... (c) 'd))
             '(a b c)))

(define (transpose list-of-lists)
  (if (null? list-of-lists)
      '()
      (apply map list list-of-lists)))

(e.g. (transpose '((1 2 3)
                   (4 5 6))) ===> ((1 4)
                                   (2 5)
                                   (3 6)))

(define (unzip-bindings bindings keys)
  (let* ((unzipped (only (lambda (`(,key . ,value))
			   (and (is key member keys)
				(is value list?)
				(isnt value null?)))
			   bindings))
	 (((names . values) ...) unzipped))
    (map (lambda (singular-values)
	   `(,@(map (lambda (name value)
		      `(,name . ,value))
		    names singular-values)
	     ,@bindings))
	 (transpose values))))

(e.g.
 (unzip-bindings '((a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
		 '(a c e))
 ===> (((a . 1) (c . 1)
	(a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 2) (c . 2)
	(a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))
       ((a . 3) (c . 3)
	(a 1 2 3) (b 1 2 3) (c 1 2 3) (d . 4))))

(define (unique-symbol source)
  "every call to unique-symbol produces a distinct \
symbol which somehow resembles the source expression"
  (gensym (->string source)))
  
(define (fill template #;with bindings)

  (define (fill-sequence template bindings)
    (let* ((symbols (used-symbols template))
	   (binding-sequences (unzip-bindings bindings
					      symbols)))
      (map (lambda (bindings)
	     (fill-template template bindings))
	   binding-sequences)))

  (define (fill-template template #;with bindings)
    (match template
      (`(quote ,literal)
       literal)
      (`(,repeated ,(? ...?) . ,rest)
       `(,@(fill-sequence repeated bindings)
	 . ,(fill-template rest #;with bindings)))
      (`(,head . ,tail)
       `(,(fill-template head #;with bindings)
	 . ,(fill-template tail #;with bindings)))
      (_
       (cond ((and (symbol? template)
                   (assoc template bindings))
              => (lambda (`(,key . ,value))
                   value))
             (else
              template)))))

  (let* ((missing (difference (used-symbols template)
			      (map (lambda (`(,key . ,value))
				     key)
				   bindings)))
	 (bindings `(,@(map
			(lambda (symbol)
			  `(,symbol
			    . ,(unique-symbol symbol)))
			missing)
		     ,@bindings)))
    (fill-template template bindings)))

(e.g.
 (fill '('+ a b ...) '((a . 1) (b . (2 3 4))))
 ===> (+ 1 2 3 4))

(define ((transform macros return) expression)
  (let ((mapping (any (lambda (macro)
			(let* ((`(,pattern ,template)
				macro)
			       (bindings (bind
					 pattern
					 expression)))
			  (and bindings
			       `(,macro ,bindings))))
		      macros)))
    (match mapping
      (`(,macro ,bindings)
       (let ((`(,pattern ,template) macro))
	 (return (fill template bindings) macro)))
      (_
       (return expression)))))

(define ((argument n) . args)
  (list-ref args n))

(e.g. ((argument 0) 1 2 3) ===> 1)

(define (expand expression macros)
  "complete expansion of an expression"
  (define (expand expression)
    (match expression
      (`(quote ,_) expression)
      (`(lambda ,args ,body) `(lambda ,args ,(expand body)))
      (`(if ,condition ,then ,else)
       `(if ,(expand condition)
	    ,(expand then)
	    ,(expand else)))
      (`(,operator . ,operands)
       (let ((transformed ((transform macros (argument 0))
			   expression)))
	 (if (equal? expression transformed)
	     `(,(expand operator)
	       . ,(map expand operands))
             (expand transformed))))
      (_
       expression)))

  (expand expression))

(define (focus expression path)
  (fold-left list-ref expression path))

(e.g.
 (let ((e '(let ((delta (- (* b b) (* 4 a c))))
	     (if (is delta >= 0)
		 (values (- (- b) (/ (sqrt delta)
				     (* 2 a)))
			 (+ (- b) (/ (sqrt delta)
				     (* 2 a))))))))
   (and (equal? (focus e '(0)) 'let)
	(equal? (focus e '(1 0 0)) 'delta)
	(equal? (focus e '(1 0 1 2)) '(* 4 a c))
	(equal? (focus e '(2 1)) '(is delta >= 0)))))

(define (expand/track expression macros) 
  
  (define (expand expression rules path)
    (match expression
      (`(quote ,_) `(,expression ,rules))
      (`(lambda ,args ,body)
       (let ((`(,body* ,rules*) (expand body rules
					`(2 . ,path))))
	 `((lambda ,args ,body*) ,rules*)))
      (`(if ,condition ,then ,else)
       (let* ((`(,condition* ,rules*) (expand
				       condition
				       rules
				       `(1 . ,path)))
	      (`(,then* ,rules**) (expand then rules*
					  `(2 . ,path)))
	      (`(,else* ,rules***) (expand else rules**
					   `(3 . ,path))))
       `((if ,condition* ,then* ,else*) ,rules***)))
      (`(,operator . ,operands)
       (match ((transform macros list) expression)
	 (`(,expression* ,macro)
	  (expand expression*
		  `((,path ,macro) . ,rules)
		  path))
	 (`(,expression*)
	  ;;(assert (eq? expression expression*))
	  (fold-right (lambda (op index `(,ops ,rules))
			(let* ((path* `(,index . ,path))
			       (`(,op* ,rules*) (expand
						 op
						 rules
						 path*)))
			  `((,op* . ,ops) ,rules*)))
		      `(() ,rules)
		      expression
		      (iota (length expression))))))
      (_
       `(,expression ,rules))))
  (let ((`(,result ,track) (expand expression '() '())))
    (values result
	    (map (lambda (`(,backpath . ,transform))
		   `(,(reverse backpath) . ,transform))
		 track))))

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

    (('and first second . rest)
     ('if first ('and second . rest) #false))

    (('or)
     #false)

    (('or last)
     last)

    (('or first second . rest)
     ('let ((result first))
       ('if result result ('or second . rest))))))

(e.g.
 (expand/track '(let* ((a 5) (b 10)) (+ a b))
	       core-macros)
 ===>
 ((lambda (a) ((lambda (b) (begin (+ a b))) 10)) 5)
 (((0 2 0 2) (('let* () . body)
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
	 ('let* ((name-2 value-2) ...) . body))))))


(define (replace-subexpression #;of expression #;at path
			       #;with replacement)
  (match path
    (`(,index . ,subpath)
     (let ((prefix `(,subexpression . ,suffix)
		   (split-at expression index)))
       `(,@prefix
         ,(replace-subexpression
	   #;of subexpression #;at subpath
		#;with replacement)
	 ,@suffix)))
    ('()
     replacement)))

(e.g.
 (replace-subexpression #;of '(define delta
				(- (* b b) (* 4 a c)))
			     #;at '(2 1)
				  #;with '(expt b 2))
 ===> (define delta
	(- (expt b 2) (* 4 a c))))
					    
(define (impand expression track)
  (fold-left (lambda (expression `(,path (,pattern ,template)))
	       (let* ((target (focus expression path))
		      (bindings (bind template target)))
		 (if bindings
		     (replace-subexpression
		      #;of expression
			   #;at path
				#;with (fill pattern
					     bindings))
		     expression)))
	     expression
	     track))

(e.g.
 (impand
  '((lambda (a) ((lambda (b) (begin (+ a b))) 10)) 5)
  '(((0 2 0 2) (('let* () . body)
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
	   ('let* ((name-2 value-2) ...) . body))))))
 ===> (let* ((a 5)
	     (b 10))
	(+ a b)))


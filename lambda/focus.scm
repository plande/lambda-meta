(define-module (lambda focus)
  #:use-module (grand scheme)
  #:export (focus?
	    focus
	    
	    next-focus
	    previous-focus
	    focus-on-location?
	    focus-on-expression?
	    focus-embracing?
	    broader-focus+location
	    focus-depth
	    find-focus

	    splice-subexpression
	    replace-subexpression
	    remove-subexpression

	    ))

(define (focus? x)
  (or (natural? x)
      (null? x)
      (and-let* (((h . t) x)
		 ((natural? h))
		 ((focus? t))))))

(e.g.
 (and (focus? 0)
      (focus? '())
      (focus? '(0 1 0))
      (focus? '(0 1 0 . 0))))

(define (broader-focus+location focus)
  (match focus
    ((prefix . rest)
     (let ((expression location (broader-focus+location rest)))
       (values `(,prefix . ,expression) location)))
    (_
     (values '() focus))
    ))

(e.g.
 (broader-focus+location '(1 2 . 3)) ===> (1 2) 3)

(define (focus expression #;on locus)
  "Select a subexpression pointed to by a focus."
  (match locus
    (()
     expression)
    ((h . t)
     (focus (list-ref expression h) #;on t))
    (_
     (drop expression locus))
    ))

(e.g.
 (focus '(+ (* 2 3) (/ 3 5)) '(1 0)) ===> *)

(e.g.
 (focus '(+ (* 2 3) (/ 3 5)) '(1 . 1)) ===> (2 3))

(define (next-focus #;to focus #;in expression)
  (match `(,expression ,focus)
    (((_ . _) ())
     0)
    ((() ())
     0)
    (((head . tail) (n . next))
     (let* ((subexpression (list-ref expression n))
	    (subfocus (next-focus #;to next #;in subexpression)))
       (if (null? subfocus)
	   (+ n 1)
	   `(,n . ,subfocus))))
    (((head . tail) n)
     (let ((subexpression (drop expression n)))
       (if (pair? subexpression)
	   `(,n)
	   '())))
    (_
     '())
    ))

(e.g.
 (let ((exp '(+ (* 1 2) (/ 3 4))))
   (unfold-left-until null? (lambda (c)
			      (next-focus c exp))
		      #;starting-from 0))
 ===> (0 (0) 1 (1) (1 . 0) (1 0) (1 . 1) (1 1) (1 . 2) (1 2) (1 . 3)
	 2 (2) (2 . 0) (2 0) (2 . 1) (2 1) (2 . 2) (2 2) (2 . 3) 3))

(define (previous-focus #;to focus #;in expression)

  (define (some-previous-focus focus)
    (match focus
      (()
       (length. expression))
      ((0)
       0)
      ((super ... n)
       `(,@super . ,n))
      (_
       (let* ((parent location (broader-focus+location focus)))
	 (if (= location 0)
	     parent
	     `(,@parent ,(- location 1)))))))

  (let ((foci (unfold-right-until
	       (lambda (c) (or (equal? c focus) (null? c)))
	       #;using (lambda (c) (next-focus #;to c #;in expression))
		       #;starting-from (some-previous-focus focus))))
    (match foci
      ((previous . _)
       previous)
      (_
       '())
      )))

(e.g.
 (let ((exp '(+ (* 1 2) (/ 3 4))))
   (unfold-right-until null? (lambda (c)
			       (previous-focus c exp))
		       #;starting-from 3))
 ===> (0 (0) 1 (1) (1 . 0) (1 0) (1 . 1) (1 1) (1 . 2) (1 2) (1 . 3)
	 2 (2) (2 . 0) (2 0) (2 . 1) (2 1) (2 . 2) (2 2) (2 . 3) 3))

(define (focus-on-location? focus)
  ;;(assert (focus? focus))
  (or (natural? focus)
      (and-let* (((n . subfocus) focus))
	(focus-on-location? subfocus))))

(define (focus-on-expression? focus)
  ;;(assert (focus? focus))
  (list? focus))

(define (splice-subexpression x #;to expression #;at focus)
  ;;(assert (points-to-location? focus))
  (match focus
    ((n . subfocus)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(splice-subexpression x #;to subexpression #;at subfocus)
	 ,@suffix)))
    (_
     (let ((prefix suffix (split-at expression focus)))
       (if (null? suffix)
	   `(,@prefix ,@x) ;; handle dotted pairs
	   `(,@prefix ,@x ,@suffix))))
    ))

(e.g.
 (splice-subexpression '(c) #;to '(* (+ a b) (/ d e)) #;at '(1 . 3))
 ===> (* (+ a b c) (/ d e)))

(define (replace-subexpression #;of expression #;at focus #;with x)
  (match focus
    ((n . subfocus)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(replace-subexpression #;of subexpression #;at subfocus
				      #;with x)
	 ,@suffix)))
    (()
     x)
    (_
     `(,@(take expression focus) ,@x))
    ))

(e.g.
 (replace-subexpression #;of '(* (+ a 1) (/ c d)) #;at '(1 2)
			     #;with 'b)
 ===> (* (+ a b) (/ c d)))

(define (remove-subexpression #;of expression #;at/after focus)
  (cond ((natural? focus)
	 (take expression focus))
	((null? focus)
	 '())
	(else
	 (let* (((n . subfocus) focus)
		(prefix (subexpression . suffix) (split-at expression n)))
	   `(,@prefix
	     ,@(if (null? subfocus)
		   '()
		   `(,(remove-subexpression #;of subexpression
						 #;at subfocus)))
	     ,@suffix)))
	))

(e.g.
 (remove-subexpression #;of '(* (+ a c b) (/ c d)) #;at '(1 2))
 ===> (* (+ a b) (/ c d)))

(e.g.
 (remove-subexpression #;of '(* (+ a b c) (/ c d))
			    #;after '(1 . 1))
 ===> (* (+) (/ c d)))

(define (focus-embracing? outer inner)
  "Is outer focus embracing inner?"
  (or (null? outer)
      (and-let* (((a . a*) outer)
		 ((b . b*) inner)
		 ((= a b))
		 ((focus-embracing? a* b*))))))

(e.g.
 (focus-embracing? '(1 0) '(1 0 0 . 1)))

(define (focus-depth focus)
  (match focus
    ((n . subfocus)
     (+ 1 (focus-depth subfocus)))
    (()
     0)
    (_
     1)
    ))

(e.g.
 (focus-depth '(1 2 . 3)) ===> 3)


;; do czego ma nam sluzyc fold-focus?
;; ogolnie chodzi o to, zeby przeiterowac
;; wyrazenie, dysponujac informacja
;; o polozeniu biezacego kursora.

;; no dobrze, ale po co? co chcesz osiagnac
;; przy pomocy tej jakze osobliwej abstrakcji?

;; chodzi o ekspansje makr...

;; no, to teraz rozmawiamy!
;; w takim razie nie interesuje Cie fold-focus,
;; tylko find-focus

(define (focus-head locus)
  (let* ((prefix tail (proper-list+dotted-tail locus))
	 (n (if (null? tail) 0 tail)))
    `(,@prefix ,n)))

(e.g.
 (focus-head '(1)) ===> (1 0))

(define (focus-tail locus)
  (let* ((prefix tail (proper-list+dotted-tail locus))
	 (n (if (null? tail) 0 tail)))
    `(,@prefix . ,(+ n 1))))


(define* (find-focus satisfying? expression current-focus #:= '())
  (or (and-let* ((result (satisfying? expression current-focus)))
	(values current-focus result))
      
      (and-let* ((`(,head . ,tail) expression))
	(or (find-focus satisfying? head
			(focus-head current-focus))
	    (find-focus satisfying? tail
			(focus-tail current-focus))))))

	

  

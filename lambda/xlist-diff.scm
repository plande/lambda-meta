(use-modules (grand scheme) (lambda focus) (lambda fragments))

(define (edit list diff)
  (define (patch list erratum)
    (match erratum
      (`(,index)
       (skip index list))
      (`(,index ,element)
       (insert element #;into list #;at index))
      (`(,n ,k ,m)
       (let* ((prefix list (split-at list n))
	      (moved suffix (split-at list k))
	      (list `(,@prefix ,@suffix))
	      (prefix suffix (split-at list m)))
	 `(,@prefix ,@moved ,@suffix)))))
  (fold-left patch list diff))

(e.g.
 (edit '(t a r k a) '((3 2 0)))
 ===> (k a t a r))

(e.g.
 (edit '(t a r k a) '((0 3 2)))
 ===> (k a t a r))

(e.g.
 (edit '(k a r t a) '((3 2 2)))
 ===> (k a t a r))

(e.g.
 (edit '(k a r t a) '((2 2 3)))
 ===> (k a a r t))

(e.g.
 (edit '(k a r t a) '((2 2 3) (2 2 3)))
 ===> (k a t a r))

(e.g.
 (edit '(k a r t a) '((2 2 3) (2 2 3) (2 2 3)))
 ===> (k a r t a))


(define/memoized (xlist-diff a b)
  
  (define ((next i) change)
    (match change
      (`(,n ,k ,m)
       `(,(+ n i) ,k ,(+ m i)))
      (`(,n . ,*)
       `(,(+ n i) . ,*))))
	 
  (match `(,a ,b)
    (`(,a ,a)
     '())
    
    (`((,a0 . ,a*) (,b0 . ,b*))
     (apply argmin length
	    `((0) . ,(xlist-diff a b*))
	    `((0 ,a0) . ,(map (next 1) (xlist-diff a* b)))
	    (if (equal? a0 b0)
		(map (next 1) (xlist-diff a* b*))
		`((0) (0 ,a0) . ,(map (next 1) (xlist-diff a* b*))))
	    
	    (collect `((,k ,i ,0) . ,(map (next i) (xlist-diff a- b-)))
		     for i in (numbers #:from (length a) #:to 1)
		     for a- in `(,(drop a i))
		     for k in (fragment-indices (is a (prefix= i) _) b)
		     for b- in `(,(omit i #;elements-at k #;in b)))))
    
    (`((,a0 . ,a*) ,b)
     `(,@(xlist-diff a* b) (0 ,a0)))
    
    (`(,a (,b0 . ,b*))
     `((0) . ,(xlist-diff a b*)))
    ))


(define (?xlist-diff a b)
  (let ((delta (xlist-diff a b)))
    (values (edit b delta)
	    delta)))

(xlist-diff '(s z m a t a) '(s z t a m a))


(?xlist-diff '(t a r k a) '(k a r t a))


(edit '(k a r t a) '((0 2 3)))

;;0 1 2 3 4 5
'(r t a k a)

(let* ((a b (values '(t a r k a) '(k a r t a)))
       (`(,x ,y) (xlist-diff a b)))
  (values (edit b `(,x)) y))


(?xlist-diff '(m a k a r o n) '(a r o m a t))

;; trzeba się zastanowić nad reprezentacją swapa


(?xlist-diff '(t a r a s) '(r a s t a))

(?xlist-diff '(p o d a) '(o p a d))

(?xlist-diff '(d u p a) '(u p a d l))

(?xlist-diff '(a n a g r a m) '(g r a n a m a))







(define-module (lambda fragments)
  #:use-module (grand scheme)
  #:export (index prefix= fragment-indices common-prefix-length))

(define (index condition list)

  (define (index-from start list)
    (and-let* ((`(,head . ,tail) list))
      (if (condition head)
	  start
	  (index-from (+ start 1) tail))))

  (index-from 0 list))

(e.g.
 (index even? '(1 3 5 6 7 8 9))
 ===> 3)

(e.g.
 (not (index even? '(1 3 5))))


(define ((prefix= n) a b)
  (or (= n 0)
      (and-let* ((`(,a0 . ,a*) a)
		 (`(,b0 . ,b*) b)
		 ((equal? a0 b0)))
	((prefix= (- n 1)) a* b*))))


(e.g.
 (is '(g e o r g e) (prefix= 3) '(g e o l o g y)))

(e.g.
 (isnt '(g e o r g e) (prefix= 4) '(g e o l o g y)))

(define (fragment-indices desired? list)
  
  (define (gather #;from list #;at index #;into result)
    (match list
      (`(,head . ,tail)
       (gather #;from tail #;at (+ index 1)
		      #;into (if (desired? list)
				 `(,index . ,result)
				 result)))
      (_
       result)))
  
  (gather #;from list #;at 0 #;into '()))

(e.g.
 (fragment-indices (is '(k u) prefix? _) '(k u k u r y k u))
 ===> (6 2 0))

(e.g.
 (fragment-indices (is '(k u t a s) (prefix= 2) _) '(k u k u r y k u))
 ===> (6 2 0))

(e.g.
 (fragment-indices (is '() prefix? _) '(1 2 3))
 ===> (2 1 0))

(define (common-prefix-length a b)

  (define (common-prefix-count n . a+b)
    (match a+b
      (`((,x . ,a) (,x . ,b))
       (common-prefix-count (+ n 1) a b))
      (_
       n)))

  (common-prefix-count 0 a b))

(e.g.
 (common-prefix-length '(g e o r g e) '(g e o l o g y))
 ===> 3)

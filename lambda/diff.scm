(use-modules (grand scheme) (lambda focus))

(define/memoized (edit-distance a b)
  ;; Levinshtein distance
  (match `(,a ,b)
    (`(,a ())
     (length a))
    (`(() ,b)
     (length b))
    (`((,a0 . ,a*) (,b0 . ,b*))
     (min (+ (edit-distance a* b) 1)
	  (+ (edit-distance a b*) 1)
	  (+ (edit-distance a* b*)
	     (if (equal? a0 b0) 0 2))))))

(define (insert element #;into list #;at position)
  (let ((prefix suffix (split-at list position)))
    `(,@prefix ,element ,@suffix)))

(define (edit list diff)
  (define (patch list erratum)
    (match erratum
      (`(,index)
       (skip index list))
      (`(,index ,element)
       (insert element #;into list #;at index))))
  (fold-left patch list diff))

(e.g.
 (edit '(a b c d) '((0) (2 a))) ===> (b c a d))

(e.g.
 (edit '() '((0 d) (0 c) (0 b) (0 a)))
 ===> (a b c d))

;;delta = a - b
;;a = b + delta
;; delta - to, co trzeba dodac do listy b, zeby
;; otrzymac liste a

(assert (lambda (a b)
	  (if (and (list? a) (list? b))
	      (equal? (edit b (list-diff a b)) a))))


(define (edit+1 `(,n . ,*))
  `(,(+ n 1) . ,*))

(define/memoized (list-diff a b)
    
  (match `(,a ,b)
    (`(,a ())
     (fold-left (lambda (result element)
		  `((0 ,element) . ,result))
		'()
		a))
    (`(() ,b)
     (fold-left (lambda (result element)
		  `((0) . ,result))
		'()
		b))
    (`((,a0 . ,a*) (,b0 . ,b*))
     (argmin length
	     `((0) . ,(list-diff a b*))
	     `((0 ,a0) . ,(map edit+1 (list-diff a* b)))
	     (if (equal? a0 b0)
		 (map edit+1 (list-diff a* b*))
		 `((0) (0 ,a*) . ,(map edit+1 (list-diff a* b*))))))))


(assert (lambda (a b)
	  (if (and (list? a) (list? b))
	      (= (length (list-diff a b 0))
		 (edit-distance a b)))))

(define (check-list-diff a b)
  (let* ((d (list-diff a b))
	 (n (edit-distance a b))
	 (n* (length d)))
    (values d (edit b d) n n*)))

(check-list-diff '(a b c) '(a b d))

(check-list-diff '(d i a m e n t y) '(d i a m o n d s))

(check-list-diff '(a d a) '(t a t a))

(check-list-diff '(t a t a) '(a d a))

(check-list-diff '(d u p a) '(b m w))

(check-list-diff '(n a b u c h o d o n o z o r)
		 '(k o n s t a n t y n o p o l i t a n c z y k))

(check-list-diff '(t a r a p a t a) '(t a t a r a t a))

(define (patch tree erratum)
  (match erratum
    (`(,locus)
     (remove-subexpression #;of tree #;at/after locus))
    (`(,locus ,insertion)
     (splice-subexpression `(,insertion) #;at locus))
    (`(,locus ,left ,right)
     (rebrace tree #;at locus #;by left #;and right))))

(define (rebrace tree #;at locus #;by left #;and right)
  ;;(assert (and (focus-on-expression? locus)
  ;;             (list? (focus tree locus))))
  (let* (((context ... offset) locus)
	 (siblings (focus tree context))
	 (younger (subject . older) (split-at siblings offset))
	 (peers `(,@younger ,@subject ,@older))
	 (start (+ left offset))
	 (size (+ right (length subject) (- left)))
	 (younger* peers* (split-at peers start))
	 (subject* older* (split-at peers* size))
	 (siblings* `(,@younger* ,subject* ,@older*)))
    (replace-subexpression tree context siblings*)))

(e.g.
 (rebrace '(1 (2 (3) 4 5 6) 7) #;at '(1 1) #;by -1 2)
 ===> (1 ((2 3 4 5) 6) 7))

(e.g.
 (rebrace '(1 (2 (3) 4 5 6) 7) #;at '(1 1) #;by 1 2)
 ===> (1 (2 3 (4 5) 6) 7))


;; W przypadku odleglosci Levninshteina mamy do czynienia
;; z nastepujacymi edycjami:
;; - wstawienie elementu
;; - usuniecie elementu
;; - 


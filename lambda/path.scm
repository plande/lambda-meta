(use-modules (grand scheme))

(define* (optimial-path #:from initial-state ; a
			#:on weighted-graph ; a -> [(a, Real)]
			#:until success? ; a -> Boolean
			#:guided-by remaining-cost-estimate ; a -> Real
			) ; -> a
  (define (probably-shorter? `(,estimate-a ,cost-a ,node-a)
			     `(,estimate-b ,cost-b ,node-b))
    (is estimate-a < estimate-b))
  
  (define (walk paths visited-nodes)
    (and-let* ((`((,_ ,cost-so-far ,path) . ,paths) paths)
	       (`(,current-node . ,_) path))
    (define (estimate-total-cost `(,node ,weight))
      (let* ((total-cost (+ cost-so-far weight))
	     (estimate (+ total-cost (remaining-cost-estimate node))))
	`(,estimate ,total-cost ,node)))
    
    (define (update-paths paths `(,estimate ,alternative-cost ,node))
      (let ((new-path `(,estimate ,alternative-cost (,node . ,path))))
	(match (find (lambda (`(,_ ,_ (,end . ,_)))
		       (equal? end node))
		     paths)
	  (`(,previous-estimate ,established-cost ,existing-path)
	   (if (is established-cost <= alternative-cost)
	       paths
	       (let ((paths (filter (lambda (`(,_ ,_ (,end . ,_)))
				      (isnt end equal? node))
				    paths)))
		 (merge `(,new-path) paths probably-shorter?))))
	  (_
	   (merge `(,new-path) paths probably-shorter?)))))
    
      (if (success? current-node)
	  (values (reverse path) cost-so-far)
	  (let* ((neighbours (map estimate-total-cost
				  (filter (lambda (`(,node ,weight))
					    (isnt node member visited-nodes))
					  (weighted-graph current-node))))
		 (paths (fold-left update-paths paths neighbours)))
	    (walk paths (union `(,current-node) visited-nodes))))))
  
  (walk `((+inf.0 0 (,initial-state))) '()))

(e.g.
 (let ((graph-from-wikipedia
	'((start (a 1.5) (d 2))
	  (a (b 2) (start 1.5))
	  (b (c 3) (a 2))
	  (c (end 4) (b 3))
	  (d (e 3) (start 2))
	  (e (end 2) (d 3))
	  (end (c 4) (e 2))))

       (heurisics
	'((a . 4)
	  (b . 2)
	  (c . 4)
	  (d . 4.5)
	  (e . 2)
	  (end . 0))))

   (define ((callable alist) key)
     (assoc-ref alist key))
   
   (optimial-path #:on (callable graph-from-wikipedia)
		  #:from 'start
		  #:until (is _ equal? 'end)
		  #:guided-by (callable heurisics)))
 ===> (start d e end) 7)

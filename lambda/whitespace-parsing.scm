(define-module (lambda whitespace-parsing)
  #:use-module (grand scheme)
  #:export (prn/ws read/ws))

;; warm-up: printing compound objects (without whitespace)
(define (prn obj)
  (cond
   ((pair? obj)
    (write-char #\()
    (prn-pair obj)
    (write-char #\)))

   (else
    (write obj))))

(define (prn-pair p)
  (match p
    (`(,first ,second . ,rest)
     (prn first)
     (write-char #\space)
     (prn-pair `(,second . ,rest)))
    (`(,last)
     (prn last))
    (`(,last . ,tail)
     (prn last)
     (display " . ")
     (prn tail))))

(e.g.
 (with-output-to-string
   (lambda ()
     (prn '( (a   b  .  c)  d   (e . f) . (   )  ) )))
 ===> "((a b . c) d (e . f))")


;; printing compound objects (with whitespaces/comments)

(define-syntax-rule (space! arg)
  (match arg
    (`(,head . ,tail)
     (display head)
     (set! arg tail)
     arg)
    (_
     (warn "ran out of whitespace")
     (write-char #\space)
     arg)))

(define (prn/ws obj ws)
  (cond ((pair? obj)
	 (write-char #\()
	 (space! ws)
	 (set! ws (prn-pair/ws obj ws))
	 (space! ws)
	 (write-char #\)))
	
	((null? obj)
	 (write-char #\()
	 (space! ws)
	 (write-char #\)))
	
	(else
	 (write obj)))
  ws)

(define (prn-pair/ws p ws)
  (match p
    (`(,first ,second . ,rest)
     (set! ws (prn/ws first ws))
     (space! ws)
     (prn-pair/ws `(,second . ,rest) ws))
    
    (`(,last)
     (prn/ws last ws))
    
    (`(,last . ,tail)
     (set! ws (prn/ws last ws))
     (space! ws)
     (write-char #\.)
     (space! ws)
     (prn/ws tail ws))))

(e.g.
 (with-output-to-string
   (lambda ()
     (prn/ws
      '(define (! n)
	 (if (= n 0)
	     1
	     (* n (! (- n 1)))))
      '(""#;define" "#;<""#;!" "#;n" #;int"#;>" ; -> int
  "#;<""#;if" "#;<""#;=" "#;n" "#;0""#;>"
    "#;1" #| base case |#
   "#;<""#;*" "#;n" "#;<""#;!" "#;<""#;-" "#;n" "#;1""#;>
   ""#;>""#;>""#;>""#;>""))))
 ===> "(define (! n #;int) ; -> int
  (if (= n 0)
    1 #| base case |#
   (* n (! (- n 1)))))")


(e.g.
 (with-output-to-string
   (lambda ()
     (prn/ws
      '(a (b (c . d)
	     . e)
	  ( ) . f)
      '(""#;a" "#;<""#;b" "#;<""#;c" "#;." "#;d""#;>"
      "#;." "#;e""#;>"
   "#;<" "#;>" "#;." "#;f""#;>))))
 ===> "(a (b (c . d)
      . e)
   ( ) . f)")


;; reading/parsing


(define (delimiter? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memq c '(#\( #\; #\)))))

(define (digit->number digit)
  (- (char->integer digit) (char->integer #\0)))

(e.g. (digit->number #\5) ===> 5)

(define (list->atom list)
  (if (every char-numeric? list)
      ((number/base 10) (map digit->number list))
      (list->symbol list)))

(e.g.
 (list->atom '(#\1 #\2 #\3)) ===> 123)

(e.g.
 (list->atom '(#\a #\t #\o #\m)) ===> atom)

(define (read-whitespace/comments)
  (define (read-line-comment result)
    (let ((c (read-char)))
      (cond ((eof-object? c)
	     result)
	    ((eq? c #\newline)
	     `(,c . ,result))
	    (else
	     (read-line-comment `(,c . ,result))))))
  (define (read-delimited-comment result)
    (let ((c (read-char)))
      (cond ((eof-object? c)
	     result)
	    ((and (eq? c #\|)
		  (eq? (peek-char) #\#))
	     `(,(read-char) ,c . ,result))
	    (else
	     (read-portion (read-delimited-comment
			    `(,c . ,result)))))))
  (define (read-portion result)
    (let ((c (read-char)))
      (cond ((eof-object? c)
	     result)
	    ((char-whitespace? c)
	     (read-portion `(,c . ,result)))
	    ((eq? c #\;)
	     (read-portion (read-line-comment
			    `(,c . ,result))))
	    ((and (eq? c #\#)
		  (eq? (peek-char) #\|))
	     (read-delimited-comment `(,(read-char)
				       ,c . ,result)))
	    (else
	     (unread-char c)
	     result))))
  (list->string (reverse (read-portion '()))))

(define (read-string)
  (define (scan escape? result)
    (let ((c (read-char)))
      (cond ((eof-object? c)
	     result)
	    (escape?
	     (scan #f `(,@(match c
			    (#\n '(#\newline))
			    (#\t '(#\tab))
			    (#\r '(#\return))
			    (#\newline '())
			    (_ `(,c #\\)))
			. ,result)))
	    ((eq? c #\")
	     result)
	    ((eq? c #\\)
	     (scan #t result))
	    (else
	     (scan #f `(,c . ,result))))))
  (list->string (reverse (scan #f '()))))

(define (read-atom)
  (define (accumulate result)
    (let ((c (peek-char)))
      (if (delimiter? c)
	  result
	  (accumulate `(,(read-char) . ,result)))))
  (list->atom (reverse (accumulate '()))))

(define (read/ws)
  (define (read-expressions expressions whitespaces)
    (let ((c (read-char)))
      (cond ((eq? c #\()
	     (let ((expression spaces
			       (read/ws)))
	       (read-expressions
		`(,@expressions ,expression)
		`(,@whitespaces ,@spaces))))
	    ((eof-object? c)
	     (values expressions whitespaces))
	    ((eq? c #\))
	     (values expressions
		     `(,@whitespaces
		       ,(read-whitespace/comments))))
	    ((or (char-whitespace? c)
		 (eq? c #\;)
		 (and (eq? c #\#)
		      (memq (peek-char) '(#\| #\;))))
	     (read-expressions
	      expressions
	      `(,@whitespaces ,(read-whitespace/comments))))
	    (else
	     (unread-char c)
	     (let* ((atom (read-atom))
		    (space (read-whitespace/comments)))
	     (read-expressions
	      `(,@expressions ,atom)
	      `(,@whitespaces ,space)
	      ))))))
  (read-expressions '() `(,(read-whitespace/comments))))

(e.g.
 (with-input-from-string
     "(define (! n #|int|#) ; -> int
  (if (= n 0)
    1 #| base case |#
   (* n (! (- n 1)))))"
   read/ws)
 ===>
 ((define (! n)
    (if (= n 0)
	1
	(* n (! (- n 1))))))
 (""#;<""#;define" "#;<""#;!" "#;n" #|int|#"#;>" ; -> int
  "#;<""#;if" "#;<""#;=" "#;n" "#;0""#;>"
    "#;1" #| base case |#
   "#;<""#;*" "#;n" "#;<""#;!" "#;<""#;-" "#;n" "#;1""#;>""#;>
   ""#;>""#;>""#;>""))

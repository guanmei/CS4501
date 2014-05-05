--for testing only
(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(define my-count (counter 5))
(my-count 3)
(my-count 6)
(my-count 5)

;(define (deal deck sum) 
;	(cond ((> dealer 17)
;		dealer)
;		(else 
;			(deal deck-1 (+ sum drawnCard)))))


(define (print lst) 
	(if (null? (cdr lst)) 
		(car lst)
		((print (car lst)) (print (cdr lst)))))
		
		
(define (print lst) 
	(if (eq? (length lst) 1 
	#\newline
	(cons (car lst) (print (cdr lst)))))
	
	(define (print lst)
	(if (< (length lst) 4)
	#f
	(cons (car lst) (cons (cadr lst) (cons (cadr (cdr lst)) (cons (cadr (cdr (cdr lst))) (print (cdr (cdr (cdr (cdr lst)))))))))))
	
(define (start num) (quarters 0 num))	
	
	
(define board 
	(list
		(list 1 2 3 4)
		(list 5 6 7 8)
		(list 9 10 11 12)
		(list 13 14 15 0)))
		
(define (print lst)
	(cons (car lst) (cdr lst)))

	
	(define places 
	(list 
		(list 25 29 "The Corner")
		(list 3 4 "Downtown Mall")))

(define (calc coordpair) 
	(+ (* (- (car coordpair) (car (car places))) (- (car coordpair) (car (car places)))) (* (- (cadr coordpair) (cadr (car places))) (- (cadr coordpair) (cadr (car places))))))
	
	
(define (start coordpair) (calc coordpair))
(define (f x y) (+ x y))
(f 1 2)
(f 5 2)

(define dest
	(list 
		(list 25 29 "The Corner")
		(list 3 4 "Downtown Mall")))

(define (calc coordpair places) 
	(+ (* (- (car coordpair) (car (car places))) (- (car coordpair) (car (car places)))) (* (- (cadr coordpair) (cadr (car places))) (- (cadr coordpair) (cadr (car places))))))
	
(define (findmin coordpair lst) 
	(if (< (calc coordpair lst) (calc coordpair (cdr list)))
		(cons (calc coordpair lst) '())
		(findmin coordpair (cdr lst))))

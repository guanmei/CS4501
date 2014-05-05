(load "stdlib.scm")


(define (quarters numq val)
	(if (< val 25)
	(cons '"Quarters:" (cons numq (dimes 0 val)))
 	(quarters (+ 1 numq) (- val 25))))

(define (dimes numd val)
	(if (< val 10)
	(cons '"Dimes:" (cons numd (pennies 0 val)))
	(dimes (+ 1 numd) (- val 10))))

(define (pennies nomfup val)
	(if (= val 0)
	(cons '"Pennies:" (cons nomfup '()))
	(pennies (+ 1 nomfup) (- val 1))))	
	
(define (getchange num) 
	(quarters 0 num))	

(define (startprog quarters dimes pennies)
	(
	
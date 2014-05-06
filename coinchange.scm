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
	
(define (start num) 
	(quarters 0 num))


(define (start function num) 
	(if (eq? function "getchange")
		(quarters 0 num)
		(if (eq? function "addchange")
		(quarters 0 num)
		"Error: not a recognized command")))	
		
			(if (< (- (cadddr reg) num) 0)
		(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (cadddr reg) (cons (car (cddddr reg)) (cons 0 '())))))))
		
		(define (sub10d num)
	(if (< (- (car reg) num) 0)
		(set! reg (cons 0 (cdr reg)))
		(set! reg (cons (- (car reg) num) (cdr reg)))))

(define (sub1d num)
	(if (< (- (cadr reg) num) 0)
		(set! reg (cons (car reg) (cons 0 (cddr reg))))
		(set! reg (cons (car reg) (cons (- (cadr reg) num) (cddr reg))))))

(define (subquarters num)
	(if (< (- (caddr reg) num) 0)
		(set! reg (cons (car reg) (cons (cadr reg) (cons 0 (cdddr reg)))))
		(set! reg (cons (car reg) (cons (cadr reg) (cons (- (caddr reg) num) (cdddr reg)))))))
	
(define (subdimes num)
	(if (< (- (cadddr reg) num) 0)
		(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons 0 '())))))
		(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (- (cadddr reg) num) '())))))))
		
(define (subnickels num)
	(if (< (- (cadddr reg) num) 0)
		(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (cadddr reg) (cons 0 (cdr (cddddr reg))))))))
		(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (cadddr reg) (cons (- (car (cddddr reg)) num) (cdr (cddddr reg))))))))))
		
		
		(define (findall amount)
		(define (make-change-with-coins amount coins)
			(cond ((< amount 0) 0)
			((= amount 0) 1)
			((null? coins) 0)
			(else (+ (make-change-with-coins (- amount (car coins)) coins)
				   (make-change-with-coins amount (cdr coins))))))
			(make-change-with-coins amount '(25 10 5 1)))

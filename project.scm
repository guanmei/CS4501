(load "stdlib.scm")

(define reg '(100 1000 10 10 10 100))

(define (getchange num) 
	(cons (dollar10 0 (quotient num 100)) (quarters 0 (mod num 100))))
	
(define (addchange type numtoadd)
	(if (string=? type "25c")
		(addquarters numtoadd)
		(if (string=? type "10c") 
			(adddimes numtoadd)
			(if (string=? type "5c")
				(addnickels numtoadd)
				(if (string=? type "1c")
					(addpennies numtoadd)
					(if (string=? type "10d")
						(add10d numtoadd)
						(if (string=? type "1d")
							(add1d numtoadd)
							"Error: not a valid value!")))))))
			
(define (printreg reg)
	(cons '"$10:" (cons (car reg) (cons '"$1:" (cons (cadr reg) (cons '"Quarters:" (cons (caddr reg) (cons '"Dimes:" (cons (cadddr reg) (cons '"Nickels:" (cons (car (cddddr reg)) (cons '"Pennies:" (cdr (cddddr reg))))))))))))))
		
(define (add10d num)
	(set! reg (cons (+ (car reg) num) (cdr reg))))

(define (add1d num)
	(set! reg (cons (car reg) (cons (+ (cadr reg) num) (cddr reg)))))

(define (addquarters num)
	(set! reg (cons (car reg) (cons (cadr reg) (cons (+ (caddr reg) num) (cdddr reg))))))
	
(define (adddimes num)
	(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (+ (cadddr reg) num) (cddddr reg)))))))
	
(define (addnickels num)
	(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (cadddr reg) (cons (+ (car (cddddr reg)) num) (cdr (cddddr reg)))))))))
	
(define (addpennies num)
	(set! reg (cons (car reg) (cons (cadr reg) (cons (caddr reg) (cons (cadddr reg) (cons (car (cddddr reg)) (cons (+ (cdr (cddddr reg)) num) '()))))))))

(define (subcash num10d num1d)
	(set! reg (cons (- (car reg) num10d) (cons (- (cadr reg) num1d) (cddr reg)))))
	
(define (subchange numq numd numn nump)
	(set! reg (cons (car reg) (cons (cadr reg) (cons (- (caddr reg) numq) (cons (- (cadddr reg) numd) (cons (- (car (cddddr reg)) numn) (cons (- (cdr (cddddr reg)) nump) '()))))))))
		
(define (quarters numq val)
	(if (= numq (caddr reg))
		(cons '"Quarters:" (cons numq (dimes numq 0 val)))
		(if (< val 25)
			(cons '"Quarters:" (cons numq (dimes numq 0 val)))
			(quarters (+ 1 numq) (- val 25)))))

(define (dimes numq numd val)
	(if (= numd (cadddr reg))
		(cons '"Dimes:" (cons numd (nickels numq numd 0 val)))
		(if (< val 10)
			(cons '"Dimes:" (cons numd (nickels numq numd 0 val)))
			(dimes numq (+ 1 numd) (- val 10)))))
	
(define (nickels numq numd numn val)
	(if (= numn (car (cddddr reg)))
		(cons '"Nickels:" (cons numn (pennies numq numd numn 0 val)))
		(if (< val 5)
			(cons '"Nickels: " (cons numn (pennies numq numd numn 0 val)))
			(nickels numq numd (+ 1 numn) (- val 5)))))

(define (pennies numq numd numn nomfup val)
	(if (= nomfup (cdr (cddddr reg)))
		(cons '"Pennies:" (cons nomfup (cons "Register is out of change! Please add more :)" '()))) 
		(if (= val 0)
			(cons '"Pennies:" (cons nomfup (cons "\n" (cons "Change remaining in register:" (cons (subchange numq numd numn nomfup) '())))))
			(pennies numq numd numn (+ 1 nomfup) (- val 1)))))
			
(define (dollar10 numd10 val) 
	(if (= numd10 (car reg))
		(cons '"$10:" (cons numd10 (dollar1 numd10 0 val)))
		(if (< val 10) 
			(cons '"$10 bills:" (cons numd10 (dollar1 numd10 0 val))) 
			(dollar10 (+ 1 numd10) (- val 10)))))

(define (dollar1 numd10 numd1 val) 
	(if (= numd1 (cadr reg))
		(cons '"$1:" (cons numd1 (cons "Register is out of cash! Please add more ^_^" '())))
		(if (= val 0) 
			(cons '"$1 bills:" (cons numd1 (cons "\n" (cons "Cash remaining in register:" (cons (subcash numd10 numd1) "\n"))))) 
			(dollar1 numd10 (+ 1 numd1) (- val 1)))))
			
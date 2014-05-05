; Copyright 2008 Uwe Hollerbach <uh@alumni.caltech.edu>
; Portions of this were derived from Jonathan Tang's haskell
; tutorial "Write yourself a scheme in 48 hours" and are thus
; Copyright Jonathan Tang (but there isn't much of his stuff left).

; This file is part of haskeem.
; haskeem is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; haskeem is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with haskeem; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

; $Id: stdlib.scm,v 1.44 2010-01-18 05:15:29 uwe Exp $

; The haskeem standard library

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

(define (nth n lst)
  (cond ((or (not (integer? n))
	     (negative? n)
	     (null? lst))
	 (raise "bad input to nth"))
	((zero? n) (car lst))
	(else (nth (- n 1) (cdr lst)))))

; TODO: This is a hack: it looks (and, indeed, it is) quite idiotic,
; but it's the right thing: "eval" is currently a special form in
; haskeem, so "(eval foo)" works, but (map eval (list foo)) fails
; (without this hack).  In the latter expression, "eval" is seen as an
; unbound variable; well, this binds it to the right thing. I have to
; figure out how to fix this...

(define (eval x) (eval x))

; ditto for force

(define (force x) (force x))

(define (append lst . lsts)
  (foldl (lambda (l1 l2) (foldr cons l2 l1)) lst lsts))

; This is defined this way, rather than as (map proc . lsts), so that
; the machinery will check that there is at least one list argument:
; just plain (map proc) is illegal.

(define (map proc lst . lsts)
  (set! lsts (append (list lst) lsts))
  (letrec ((m1 (lambda (func l1)
		 (if (null? l1)
		     '()
		     (cons (func (car l1)) (m1 func (cdr l1)))))))
    (set! lst (m1 length lsts))
    (if (/= (apply min lst) (apply max lst))
	(raise "map: unequal list lengths"))
    (letrec ((m2 (lambda (l2)
		   (if (null? (car l2))
		       '()
		       (cons (apply proc (m1 car l2)) (m2 (m1 cdr l2)))))))
      (m2 lsts))))

(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (curry func arg1) (lambda args (apply func (append (list arg1) args))))

(define (compose f g) (lambda args (apply f (apply g args))))

(define fcdr (compose force cdr))

;;;(define (fcdr x)
;;;  (let ((cx (cdr x)))
;;;    (if (promise? cx)
;;;	(force cx)
;;;	cx)))

(define (cycle lst)
  (letrec ((next (lambda (cur)
		   (if (null? cur)
		       (next lst)
		       (cons (car cur) (delay (next (cdr cur))))))))
    (next lst)))

(define (unzip lst)
  (letrec*
   ((cars '())
    (cdrs '())
    (iter (lambda (l)
	    (if (null? l)
		(list (reverse cars) (reverse cdrs))
		(begin (set! cars (cons (caar l) cars))
		       (set! cdrs (cons (cadar l) cdrs))
		       (iter (cdr l)))))))
   (iter lst)))

; Generate a list of n repetitions of c

(define (replicate c n)
  (if (<= n 0)
      '()
      (cons c (replicate c (- n 1)))))

; Generate a list of the numbers s, s+1, s+2, ... upto s+n-1

(define (upfrom s n)
  (if (<= n 0)
      '()
      (cons s (upfrom (+ s 1) (- n 1)))))

; This is taken from some SRFI, I don't recall which one.
; Generate a list of n values, index i >= 0 and i < n,
; where the ith value is computed by (proc i)

(define (list-tabulate n proc)
  (let ((acc '())
	(i (- n 1)))
    (do ((i (- n 1) (- i 1)))
	((negative? i) acc)
      (set! acc (cons (proc i) acc)))))

(define (list-unhead lst n)
  (if (and (list? lst) (number? n))
      (list-tail lst (- (length lst) n))
      (raise "list-unhead expects a list and a number!")))

(define (list-untail lst n)
  (if (and (list? lst) (number? n))
      (list-head lst (- (length lst) n))
      (raise "list-untail expects a list and a number!")))

; return all elements of a list except the nth (counting from 0)

(define (list-unref lst n)
  (cond ((negative? n) lst)
	((zero? n) (cdr lst))
	(else (append (list-head lst n) (list-tail lst (+ n 1))))))

(define (filter pred lst)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (partition pred lst)
  (letrec* ((true-vals '())
	    (false-vals '())
	    (iter (lambda (l)
		    (if (null? l)
			(list (reverse true-vals) (reverse false-vals))
			(begin (if (pred (car l))
				   (set! true-vals (cons (car l) true-vals))
				   (set! false-vals (cons (car l) false-vals)))
			       (iter (cdr l)))))))
	   (iter lst)))

(define (memp test? lst)
  (cond ((null? lst) #f)
	((test? (car lst)) lst)
	(else (memp test? (cdr lst)))))

(define (memv obj lst) (memp (lambda (x) (eqv? x obj)) lst))

(define (assp test? lst)
  (cond ((null? lst) #f)
	((test? (caar lst)) (car lst))
	(else (assp test? (cdr lst)))))

(define (assv obj lst) (assp (lambda (x) (eqv? x obj)) lst))

; merge-sort "lst" using comparison function "is-lt?" which when
; called as (is-lt? a b) returns #t if a is less than b, ie, should
; come ahead of b in the sorted list.
; this is a stable sort as required by R6RS

(define (list-sort is-lt? lst)
  (letrec* ((aux-mrg (lambda (lst1 lst2)
			 (cond ((null? lst1) lst2)
			       ((null? lst2) lst1)
			       ((is-lt? (car lst2) (car lst1))
				(cons (car lst2) (aux-mrg lst1 (cdr lst2))))
			       (else
				(cons (car lst1) (aux-mrg (cdr lst1) lst2))))))
	    (aux-srt (lambda (lt)
		       (let* ((len (length lt))
			      (half (quotient len 2)))
			 (if (<= len 1)
			     lt
			     (aux-mrg (aux-srt (list-head lt half))
				      (aux-srt (list-tail lt half))))))))
	   (if (list? lst)
	       (aux-srt lst)
	       (raise "list-sort expects a list!"))))

(define (list-drop-while drop? lst)
  (if (and (> (length lst) 0) (drop? (car lst)))
      (list-drop-while drop? (cdr lst))
      lst))

(define (list-take-while keep? lst)
  (letrec ((tw (lambda (l a)
		 (if (and (> (length l) 0) (keep? (car l)))
		     (tw (cdr l) (cons (car l) a))
		     (list (reverse a) l)))))
    (tw lst '())))

; Remove adjacent duplicates in a list: '(1 2 2 3 2 2 1 1) -> '(1 2 3 2 1)

(define (list-remove-dups lst)
  (letrec ((lrd (lambda (l a)
		  (if (null? l)
		      (reverse a)
		      (lrd (list-drop-while (lambda (x) (eqv? x (car l)))
					    (cdr l))
			   (cons (car l) a))))))
    (lrd lst '())))

(define (find proc lst)
  (cond ((null? lst) #f)
	((proc (car lst)) (car lst))
	(else (find proc (cdr lst)))))

; This returns a partition of the input list: the longest prefix of the
; input list for which (pred val) returns #t, and the remainder of the list.

(define (span pred lst)
  (cond ((null? lst) (list () ()))
	((pred (car lst))
	 (let ((st (span pred (cdr lst))))
	   (list (cons (car lst) (car st)) (cadr st))))
	(else (list () lst))))

; This turns a list into a list-of-lists, where each sub-list is a group
; of elements which compare equal according to the comparison function:
; for example
;	(list-group-by eqv? (string->char "Mississippi"))
; returns
;	((#\M) (#\i) (#\s #\s) (#\i) (#\s #\s) (#\i) (#\p #\p) (#\i))
; with equal characters grouped together.

(define (list-group-by cmp lst)
  (if (null? lst)
      ()
      (let ((st (span (curry cmp (car lst)) (cdr lst))))
	(cons (cons (car lst) (car st)) (list-group-by cmp (cadr st))))))

; This is defined this way, rather than as (for-each proc . lsts), so
; that the machinery will check that there is at least one list argument:
; just plain (for-each proc) is illegal.

(define (for-each proc lst . lsts)
  (set! lsts (append (list lst) lsts))
  (set! lst (map length lsts))
  (if (/= (apply min lst) (apply max lst))
      (raise "for-each: unequal list lengths"))
  (letrec ((doit (lambda (ls)
		   (if (null? (car ls))
		       #t
		       (begin (apply proc (map car ls))
			      (doit (map cdr ls)))))))
    (doit lsts)))

; Ditto for this routine

(define (vector-for-each proc vec . vecs)
  (set! vecs (append (list vec) vecs))
  (set! vec (map vector-length vecs))
  (if (/= (apply min vec) (apply max vec))
      (raise "vector-for-each: unequal vector lengths"))
  (let* ((len (car vec))
	 (res (make-vector len)))
    (do ((i 0 (+ i 1)))
	((= i len) res)
      (vector-set! res i
		   (apply proc (map (lambda (v) (vector-ref v i)) vecs))))))

; This takes either a single list-of-lists, ie, ((1 2) (a b)) which means
; do all combinations of items from the set [1,2] and of items from the set
; [a,b], or multiple lists of the individual sets; in the latter case, it's
; required to specify the empty list as the first arg, in order to avoid
; confusing the case where ((1 2) (a b)) means "first do the vector-valued
; arg (1 2), then do the vector-valued arg (a b)".
;
;   (for-all-combinations proc '((1 2) (a b))) =>	(1 a) (1 b) (2 a) (2 b)
;   (for-all-combinations proc '() '(1 2) '(a b)) =>	(1 a) (1 b) (2 a) (2 b)
;
; note one more level of parentheses compared to the first case
;
;   (for-all-combinations proc '(((1 2) (a b)))) =>	(1 2) (a b)
;
; note that the last list is the same as in the first case:
; this is why the '() is needed
;
;   (for-all-combinations proc '() '((1 2) (a b))) =>	(1 2) (a b)

(define (for-all-combinations proc lst . lsts)
  (if (eqv? (null? lst) (null? lsts))
      (raise "for-all-combinations: bad args"))
  (if (null? lst)
      (set! lst lsts))
  (cond ((null? lst) #f)
	((= 1 (length lst))
	 (for-each proc (car lst)))
	(else
	 (for-each
	  (lambda (x) (for-all-combinations (curry proc x) (cdr lst)))
	  (car lst)))))

(define (string-split-by drop? str)
  (letrec ((aux-sb (lambda (l a)
		     (let* ((proto
			     (list-take-while (lambda (c) (not (drop? c))) l))
			    (rest (list-drop-while drop? (cadr proto)))
			    (app (cons (char->string (car proto)) a)))
		       (if (zero? (length rest))
			   (reverse app)
			   (aux-sb rest app))))))
    (aux-sb (list-drop-while drop? (string->char str)) '())))

(define (string-join-by join . strs)
  (if (list? (car strs)) (set! strs (car strs)))
  (let* ((jc (string->char join))
	 (joiner (lambda (str1 str2)
		   (cond ((= (length str2) 0) str1)
			 ((= (length str1) 0) str2)
			 (else (append str1 jc str2))))))
    (char->string (foldl joiner '() (map string->char strs)))))

(define (expmod a n m)
  (cond ((negative? n) (raise "expmod needs a non-negative exponent"))
	((zero? n) (modulo 1 m))
	((even? n) (expmod (modulo (* a a) m) (/ n 2) m))
	(else (modulo (* a (expmod a (- n 1) m)) m))))

; This doesn't necessarily belong in the standard library... but it's fun :-)
; Lucas-Lehmer test for primality of Mersenne numbers:
; let p be a prime > 2, and define
;
;  M_p = 2^p - 1
;  s_i = 4			if i == 0
;        s_{i-1}^2 - 2		otherwise
;
; then M_p is prime IFF s_{p-2} = 0 mod M_p

(define (mersenne-prime? e)
  (if (= e 2)
      #t
      (letrec* ((candidate (- (expt 2 e) 1))
		(loop (lambda (s c)
			(if (zero? c)
			    s
			    (loop (modulo (- (* s s) 2) candidate) (- c 1))))))
	       (zero? (modulo (loop 4 (- e 2)) candidate)))))

; like list-head, except we force the cdr of the stream before recursing,
; so that there will be a there there; also, we don't test for list-ness,
; because a stream is a dotted-pair composed of a value and a promise,
; not a real list

(define (stream-head strm n)
  (if (<= n 0)
      '()
      (cons (car strm) (stream-head (fcdr strm) (- n 1)))))

(define (stream-map func str . strs)
  (set! strs (append (list str) strs))
  (letrec ((m1 (lambda (fn l1)
		 (if (null? l1)
		     '()
		     (cons (fn (car l1)) (m1 fn (cdr l1))))))
	   (m2 (lambda (ls)
		 (cons (apply func (m1 car ls))
		       (delay (m2 (m1 fcdr ls)))))))
    (m2 strs)))

; scale a stream by a number; this optimizes the 0 and 1 cases, although
; the former does assume that there are no infinities in the stream; if
; there were any, they should properly get turned into NaNs.

(define (stream-scale scale strm)
  (cond ((zero? scale) (letrec ((z (cons 0 (delay z)))) z))
	((= 1 scale) strm)
	(else (cons (* scale (car strm))
		    (delay (stream-scale scale (fcdr strm)))))))

; stream-take-while: analogous to list-take-while, except it doesn't
; check the length of the list, and it forces the cdr so that there
; will be a there there when we look at it.

(define (stream-take-while keep? strm)
  (letrec ((tw (lambda (s a)
		 (if (keep? (car s))
		     (tw (fcdr s) (cons (car s) a))
		     (list (reverse a) s)))))
    (tw strm '())))

; exact integer sqrt returns a two-element list: A -> (SA R) where
;	A = SA^2 + R, 0 <= R <= 2*SA
; it proceeds via Newton-Raphson iteration from a good initial guess at
; the square root

(define (exact-integer-sqrt A)
  (cond ((negative? A) (raise "negative input to exact-integer-sqrt"))
	((zero? A) '(0 0))
	((= A 1) '(1 0))
	(else (letrec* ((x0 (expt 2 (ceiling (/ (ilog A) 2))))
			(xn 0)
			(sq (lambda (x) (* x x)))
			(iter (lambda (xo)
				(set! xn (floor (/ (+ A (sq xo)) (* 2 xo))))
				(when (< 1 (abs (- xn xo)))
				      (iter xn))
				(set! xo (- A (sq xn)))
				(when (negative? xo)
				      (set! xn (- xn 1))
				      (set! xo (- A (sq xn))))
				(list xn xo))))
		       (iter x0)))))

; Exact integer cbrt returns a two-element list: A -> (CA R) where
;	A = CA^3 + R, CA = trunc of exact cube root and R has the same sign
; as CA. It proceeds via Newton-Raphson iteration from a good initial guess
; at the cube root.

(define (exact-integer-cbrt A)
  (cond ((negative? A) (map - (exact-integer-cbrt (- A))))
	((zero? A) '(0 0))
	((= A 1) '(1 0))
	(else (letrec* ((x0 (expt 2 (ceiling (/ (ilog A) 3))))
			(xn 0)
			(sq (lambda (x) (* x x)))
			(cb (lambda (x) (* x x x)))
			(iter (lambda (xo)
				(set! xn (floor (/ (+ A (* 2 (cb xo)))
						   (* 3 (sq xo)))))
				(when (< 1 (abs (- xn xo)))
				      (iter xn))
				(set! xo (- A (cb xn)))
				(when (negative? xo)
				      (set! xn (- xn 1))
				      (set! xo (- A (cb xn))))
				(list xn xo))))
		       (iter x0)))))

(define (newline . port)
  (if (zero? (length port))
      (write-string #\linefeed)
      (write-string (car port) #\linefeed)))

(defmacro (assert some-cond)
  `(when (not ,some-cond)
	 (write-string stderr "assertion failure: ")
	 (display ',some-cond stderr)
	 (newline stderr)
	 (raise "assertion failure!")))

(defmacro (when pred . actions) `(if ,pred (begin ,@actions)))
(defmacro (unless pred . actions) `(if (not ,pred) (begin ,@actions)))

; These take zero or more trips through the loop

(defmacro (while some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
	 ((not ,some-cond) ,mc)
       ,@some-actions)))

(defmacro (until some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
	 (,some-cond ,mc)
       ,@some-actions)))

; These take at least one trip through the loop

(defmacro (do-while some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
	 ((and (positive? ,mc) (not ,some-cond)) ,mc)
       ,@some-actions)))

(defmacro (do-until some-cond . some-actions)
  (let ((mc (gensym)))
    `(do ((,mc 0 (+ ,mc 1)))
	 ((and (positive? ,mc) ,some-cond) ,mc)
       ,@some-actions)))

; Load a file, looking in a pre-specified list of directories

(defmacro (load-library fname)
  (letrec* ((paths (string-split-by (lambda (c) (eqv? c #\:))
				    (get-environment "HASKEEM_LIBRARY_PATH")))
	    (find (lambda (ps)
		    (if (null? ps)
			#f
			(let ((fp (string-join-by "/" (car ps) fname)))
			  (if (file-exists? fp)
			      fp
			      (find (cdr ps)))))))
	    (filepath (find paths)))
	   (if filepath
	       `(load ,filepath)
	       `(write-string "unable to find file '" ,fname "'\n"))))

; A stack generator:
; (make-stack . init)	-> stacker, optionally initialized with init value(s)
;			   as though pushed in order from left to right
; (stacker 'push val)	-> push val onto stack
; (stacker 'pop)	-> return top of stack with popping
; (stacker 'top)	-> return top of stack without popping
;	in both of the above cases, return #f if the stack is empty
; (stacker 'empty?)	-> return #t if the stack is empty, #f otherwise
; (stacker 'dump)	-> make the stack empty, returning all of its content

(define (make-stack . init)
  (let* ((stk (reverse init))
	 (ret #f)
	 (fn (lambda (op . val)
	       (cond ((eqv? 'push op) (if (null? val)
					  (raise "nothing to push onto stack!")
					  (set! stk (cons (car val) stk))))
		     ((eqv? 'pop op) (if (null? stk)
					 #f
					 (begin (set! ret (car stk))
						(set! stk (cdr stk))
						ret)))
		     ((eqv? 'top op) (if (null? stk) #f (car stk)))
		     ((eqv? 'empty? op) (null? stk))
		     ((eqv? 'dump op) (set! ret stk) (set! stk '()) ret)
		     (else (raise "stack error: unknown op!"))))))
    fn))

; Shamelessly stolen from Dorai Sitaram "Teach Yourself Scheme in
; Fixnum Days"... I could independently write the single-variable
; version of this, (fluid-let1 (var val) body), but here the nested
; quasi-quotes got just a little hairy... yow!

(defmacro (fluid-let vals . body)
  (let ((svals (map (lambda (ig) (gensym)) vals))
	(result (gensym))
	(vars (map car vals))
	(tvals (map cadr vals)))
    `(let ,(map (lambda (s v) `(,s ,v)) svals vars)
       ,@(map (lambda (v t) `(set! ,v ,t)) vars tvals)
       (let ((,result (begin ,@body)))
	 ,@(map (lambda (v s) `(set! ,v ,s)) vars svals)
	 ,result))))

; if desired, this can be enabled; that's a nice confirmation in the REPL
; that everything is ok

; (write-string "stdlib loaded ok\n")
;; ==================== Exercise 1.1 
10					; result: 10
(+ 5 3 4)				; result: 12
(- 9 1)					; result: 8
(/ 6 2)					; result: 3
(+ (* 2 4) (- 4 6))			; result: 6
(define a 3)				; result: value: a
(define b (+ a 1))			; result: value: b
(+ a b (* a b))				; result: 19
(= a b)					; result: #f
(if (and (> b a)
	 (< b (* a b)))
    b
    a)					; result: 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))			; result: 16
(+ 2 (if (> b a) b a))			; result: 6
(* (cond ((> a b) a)
	 ((< a b) b)
	 (else -1))
   (+ a 1))				; result: 16

;; ==================== Exercise 1.2
(/ (+ 5
      4
      (- 2
	 (- 3
	    (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))		; result: -37/150

;; ==================== Exercise 1.3
(define (sum-of-squares a b c)
  (let ((square (lambda (n) (* n n))))
    (+ (square a) (square b) (square c))))

(sum-of-squares 1 2 3)			; result: 14

;; ==================== Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; -> ((if (> b 0) + -) a b)
;; -> if b > 0, (+ a b) -> a + b
;; -> if b <= 0, (- a b) -> a - b
;; -> this indicates a + abs(b)

;; ==================== Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))

;; 1. With applicative-order evaluation interpreter
;; This procedure never returns, because of 'p' procedure.
;; 'p' procedure call itself and never terminate.

;; 2. With normal-order evaluation interpreter
;; First, expand test procedure to 'if' statement.
;; And then, apply x = 0, y = (p), then (= x 0) returns
;; #t and then that 'if' statement returns 0, terminated.


;; ==================== 1.1.7 Example: Square Roots by Newton's Method
(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt 2)


;; ==================== Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

;; (sqrt 10)

;; Maximum recursion depth exceeded. Scheme is applicative-order evaluation
;; interpreter.(By exercise 1.5) And, simple 'if' statement only select
;; statement by logical result. But 'new-if' performs inner statement first,
;; and then doing logical evaluation. So, 'new-if' recursively call 'sqrt-iter'
;; function.


;; ==================== Exercise 1.7
(sqrt 0.0001)

(sqrt 1e15)

;; This two example show small and large number isn't appropriate.
;; Because, when small number is smaller than 0.001, test is not
;; possible for that number.(0.001 isn't appropriate)
;; Large number is not possible. Because 'guess' is stopped at certain level
;; and that value is not 'good-enough'. Then doing infinite loop.
;; Large number guess have large pivot.

(define (good-enough? guess x)
  (let ((next-guess (improve guess x)))
    (< (abs (- next-guess guess)) 0.001)))

(sqrt 0.0001)				; result: Value: 1.0120218365353947e-2

(sqrt 1e15)				; result: Value: 31622776.601683907


;; ==================== Exercise 1.8
(define (improve-cuberoot guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough-cuberoot? guess x)
  (let ((next-guess (improve-cuberoot guess x)))
    (< (abs (- next-guess guess)) 0.001)))

(define (cuberoot-iter guess x)
  (if (good-enough-cuberoot? guess x)
      guess
      (cuberoot-iter (improve-cuberoot guess x) x)))

(define (cuberoot x) (cuberoot-iter 1.0 x))

(cuberoot 100)				; result: Value: 4.641590111046459
(cuberoot 27)				; result: Value: 3.0000005410641766


;; ==================== 1.1.8 Procedures as Black-Box Abstractions
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


;; ==================== Exercise 1.9
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;; (1)
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; (2)
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))


;; (1) substitution model
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
;; ==> recursive process

;; (2) substitution model
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
;; ==> iterative process

;; ==================== Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

;; (1)
(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; ...
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
;; ...
1024

;; (2)
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
;; ...
(A 1 16)
;; ...
65536

(expt 2 16)

;; (3)
(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 4)
;; ...
65536

;; (4)
(define (f n) (A 0 n))
;; f(n) = 2n

;; (5)
(define (g n) (A 1 n))
;; g(n) = 2^n

;; (6)
(define (h n) (A 2 n))
(h n)
(A 2 n)
(A 1 (A 2 (- n 1)))
(A 1 (A 1 (A 2 (- n 2))))
(A 1 (A 1 ... (A 1 (A 2 1))))
;; => (A 1 appears n-1 times
(A 1 (A 1 ... (A 1 2)))
;; => n-1 times >>(2^)2^2
;; => 2^2^...^2 << n + 1 times
;; h(n) = (pi) 2 from 1 to n + 1

;; ==================== 1.2.2 Tree Recursion
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))


(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(cc 20 5)

;; ==================== Exercise 1.11
;; recursive process
(define (f1 n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

;; iterative process
;; a -> f(n - 1)
;; b -> f(n - 2)
;; c -> f(n - 3)
;; d -> f(n)

(define (f2 n)
  (define (f2-iter a b c d count)
    (cond ((= count (+ n 1)) d)
	  ((< count 3) (f2-iter d a b count (+ count 1)))
	  (else (f2-iter d a b
			 (+ d (* 2 a) (* 3 b))
			 (+ count 1)))))
  (f2-iter 0 0 0 0 0))

(f1 10)					; result: Value: 1892
(f2 10)					; result: Value: 1892

;; ==================== Exercise 1.12

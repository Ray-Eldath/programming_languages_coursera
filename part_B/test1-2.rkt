#lang racket

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))

(define (fact-bad n)
  (my-if-bad (= n 0)
             1
             (* n (fact-bad (- n 1)))))

(define (my-if-delayed e1 e2 e3)
  (if e1 (e2) (e3)))

(define (fact-delayed n)
  (my-if-delayed (= n 0)
                 (lambda () 1)
                 (lambda () (* n (fact-delayed (- n 1))))))

(define (slow-add x y)
  (begin
    (define (slow-id y z)
      (if (= z 0)
          y
          (slow-id y (- z 1))))
    (+ (slow-id x 50000000) y)))

(define (slow-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (slow-mult (- x 1) y-thunk))]))

(define (my-delay th)
  (mcons #f th))

(define-syntax my-delay-macro
  (syntax-rules ()
    [(my-delay-macro th) (mcons #f (lambda () th))]))

(define (my-force d)
  (if (mcar d)
      (mcdr d)
      (begin (set-mcdr! d ((mcdr d)))
             (set-mcar! d #t)
             (mcdr d))))

(define (fast-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (my-force y-thunk)]
        [#t (+ (my-force y-thunk) (fast-mult (- x 1) y-thunk))]))

(define (gen-first-order-stream f initial)
  (begin (define (helper value)
           (cons value (lambda () (helper (f value)))))
         (helper initial)))

(define (powers-of-two)
  (begin (define (powers-of-two-gen value)
           (cons value (lambda () (powers-of-two-gen (* 2 value)))))
         (powers-of-two-gen 2)))

(define (powers-of-two-gen)
  (gen-first-order-stream (lambda (x) (* 2 x)) 2))

(define (ones)
  (gen-first-order-stream (lambda (x) 1) 1))

(define (gen-second-order-stream f i1 i2)
  (begin (define (helper a b)
           (cons b (lambda () (helper b (f a b)))))
         (helper i1 i2)))

(define (fib)
  (begin (define (fib-gen a b)
           (cons b (lambda () (fib-gen b (+ a b)))))
         (fib-gen 0 1)))

(define (fib-gen)
  (gen-second-order-stream (lambda (a b) (+ a b)) 0 1))

(define (powers-of-two-gen2)
  (gen-second-order-stream (lambda (a b) (* b 2)) -1 2))

(define (take stream n)
  (begin (define (helper stream n acc)
           (if (= n 0)
               acc
               (helper ((cdr stream)) (- n 1) (cons (car stream) acc))))
         (reverse (helper (stream) n null))))

(define (times-until stream predicate)
  (begin (define (helper stream acc)
           (if (predicate (car stream))
               acc
               (helper ((cdr stream)) (+ acc 1))))
         (helper (stream) 1)))
#lang racket

(provide (all-defined-out))

(define cube1
  (lambda (x)
    (* (* x x) x)))

(define cube2
  (lambda (x)
    (* x x x)))

(define (cube3 x)
  (* x x x))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(define ((pow2 x) y)
  (pow1 x y))

(define d
  (+ 4 (* 5 6 7 8)))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (my-list . args)
  (if (null? args)
      null
      (cons (car args) (my-list (cdr args)))))

(define (my-map xs f)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map (cdr xs) f))))

(define (my-filter xs f)
  (if (null? xs)
      null
      (if (f (car xs))
          (cons (car xs)
                (my-filter (cdr xs) f))
          (my-filter (cdr xs) f))))

(define (sum-all xs)
  (if (null? xs)
      0
      (if (list? xs)
          (if (number? (car xs))
              (+ (car xs) (sum-all (cdr xs)))
              (if (list? (car xs))
                  (+ (sum-all (car xs)) (sum-all (cdr xs)))
                  (sum-all (cdr xs))))
          0)))

(define (sum-all-cond xs)
  (cond [(null? xs) 0]
        [(list? xs)
         (cond [(number? (car xs)) (+ (car xs) (sum-all (cdr xs)))]
               [(list? (car xs)) (+ (sum-all-cond (car xs)) (sum-all-cond (cdr xs)))]
               [#t 0])]
        [#t 0]))
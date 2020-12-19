#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (cond [(> low high) null]
        [(= low high) (cons low null)]
        [#t (cons low (sequence (+ low stride) high stride))]))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([evaluated (s)])
        (cons (car evaluated) (stream-for-n-steps (cdr evaluated) (- n 1))))))

;; 5
(define (funny-number-stream)
  (begin (define (funny-number-stream-gen value)
           (cons (if (= (remainder value 5) 0)
                     (- value)
                     value)
                 (lambda () (funny-number-stream-gen (+ value 1)))))
         (funny-number-stream-gen 1)))

;; 6
(define (dan-then-dog)
  (begin (define (dan-then-dog-gen value)
           (cons value
                 (lambda () (if (equal? value "dan.jpg")
                                (dan-then-dog-gen "dog.jpg")
                                (dan-then-dog-gen "dan.jpg")))))
         (dan-then-dog-gen "dan.jpg")))

;; 7
(define (stream-add-zero s)
  (lambda () (let ([evaluated (s)])
               (cons (cons 0 (car evaluated)) (stream-add-zero (cdr evaluated))))))

;; 8
(define (cycle-lists xs ys)
  (lambda () (begin (define (helper n)
                      (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                            (lambda () (helper (+ n 1)))))
                    (helper 0))))

;; 9
(define (vector-assoc v vec)
  (begin (define veclen (vector-length vec))
         (define (helper n)
           (if (= n veclen)
               #f
               (let ([c (vector-ref vec n)])
                 (cond [(not (pair? c)) (helper (+ n 1))]
                       [(equal? v (car c)) c]
                       [#t (helper (+ n 1))]))))
         (helper 0)))

;; 10
(define (cached-assoc xs n)
  (begin (define cache (make-vector n #f))
         (define slot 0)
         (lambda (v)
           (let ([cache-entry (vector-assoc v cache)])
             (if cache-entry
                 (begin #| (displayln "use cache") |#
                        cache-entry)
                 (let ([evaluated (assoc v xs)])
                   (if evaluated
                       (begin (vector-set! cache slot evaluated)
                              #| (displayln cache)
                              (displayln slot) |#
                              (if (= slot (- n 1))  ; slot = n - 1, in which n is the size of the cache
                                  (set! slot 0)
                                  (set! slot 1))
                              evaluated)
                       evaluated)))))))

;; challenge problem 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([upper-bound e1]
              [helper (lambda ()
                        (if (>= e2 upper-bound)
                            #t
                            (helper)))])
       (helper))]))


; helper definitions: not part of the course
(define ones (lambda () (cons 1 ones)))
(define (take stream n)
  (stream-for-n-steps stream n))
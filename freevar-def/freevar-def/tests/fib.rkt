#lang racket/base

(require freevar-def)

(module+ test
  (require rackunit))

(define/freevar (fib n)
  #:freevars (init0 init1)
  (for/fold ([a init0]
             [b init1]
             [fib-list '()]
             #:result (reverse fib-list))
            ([i (in-range n)])
    (values b (+ a b) (cons a fib-list))))

(define init0 2)
(define (set-init0! new-value)
  (set! init0 new-value))

(define fib-2-13
  (let ([init1 13])
    fib))

(module+ test
  (check-equal?
   (fib-2-13 5)
   (list 2 13 (+ 2 13) (+ 13 (+ 2 13)) (+ (+ 2 13) (+ 13 (+ 2 13)))))

  (let ([old-init0 init0])
    (set-init0! -1)
    (check-equal?
     (fib-2-13 5)
     (list -1 13 (+ -1 13) (+ 13 (+ -1 13)) (+ (+ -1 13) (+ 13 (+ -1 13)))))
    (set-init0! old-init0)))

(module+ test
  (check-equal?
   (let ([init0 0]
         [init1 1])
     (fib 7))
   (list 0 1 1 2 3 5 8))

  (check-equal?
   (with-freevar fib ([init1 b])
     (define b 4)
     (fib 5))
   (list 2 4 (+ 2 4) (+ 4 (+ 2 4)) (+ (+ 2 4) (+ 4 (+ 2 4)))))

  (check-equal?
   (let ([b -1])
     (with-freevar fib ([init1 b])
       (fib 5)))
   (list 2 -1 (+ 2 -1) (+ -1 (+ 2 -1)) (+ (+ 2 -1) (+ -1 (+ 2 -1))))))

(define/with-freevar fib-same fib
  [init0 S]
  [init1 S])

(module+ test
  (check-equal?
   (let ([S 3])
     (fib-same 5))
   (list 3 3 6 9 15)))

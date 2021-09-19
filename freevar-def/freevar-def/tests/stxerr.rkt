#lang racket/base

(require freevar-def)

(module+ test
  (require rackunit
           syntax/macro-testing))

(define/freevar (fib n)
  #:freevars (init0 init1)
  (for/fold ([a init0]
             [b init1]
             [fib-list '()]
             #:result (reverse fib-list))
            ([i (in-range n)])
    (values b (+ a b) (cons a fib-list))))

(define/freevar (fibimm n)
  #:freevars (init0 init1)
  #:immediate
  (for/fold ([a init0]
             [b init1]
             [fib-list '()]
             #:result (reverse fib-list))
            ([i (in-range n)])
    (values b (+ a b) (cons a fib-list))))

(define init0 2)
(define (set-init0! new-value)
  (set! init0 new-value))

(module+ test
  (check-exn
   #rx"init1.+unbound"
   (λ () (convert-syntax-error fib)))

  (check-exn
   #rx"init1.+unbound"
   (λ () (convert-syntax-error fibimm)))

  (check-exn
   #rx"X.+unbound"
   (λ ()
     (convert-syntax-error
      (with-freevar fib ([init1 X])
        fib))))

  (check-exn
   #rx"X.+unbound"
   (λ ()
     (convert-syntax-error
      (with-freevar fibimm ([init1 X])
        fibimm))))

  (check-equal?
   (let ()
     (define F fib)
     (define init1 1)
     (F 5))
   (list 2 1 (+ 2 1) (+ 1 (+ 2 1)) (+ (+ 2 1) (+ 1 (+ 2 1)))))

  (check-exn
   #rx"init1.+undefined.+cannot use before initialization"
   (λ ()
     (define F fibimm)
     (define init1 1)
     F))

  )

#lang racket/base

(require racket/list
         racket/syntax)

(provide format-original-identifiers
         fix-app
         check-duplicate-freevar-or-param
         build-datum-hash)

(define (format-original-identifiers identifiers #:context ctxt #:source src)
  (for/list ([var (in-list identifiers)])
    (syntax-property
     (format-id ctxt "~a" var #:source src)
     'original-for-check-syntax #t)))

(define (fix-app ctxt app-stx)
  (define app-datum (syntax-e app-stx))
  (datum->syntax ctxt app-datum app-stx app-stx))

(define (check-duplicate-freevar-or-param freevars params)
  (or (check-duplicate-identifier (append freevars params))
      (cdr (check-duplicates
            (map cons (map syntax-e freevars) freevars)
            #:key car
            #:default '(#f . #f)))))

(define (build-datum-hash fvs-stx new-fvs-stx)
  (for/hash ([fv (in-list (syntax-e fvs-stx))]
             [new-fv (in-list (syntax-e new-fvs-stx))])
    (values (syntax-e fv) (syntax-e new-fv))))

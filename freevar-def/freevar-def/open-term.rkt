#lang racket/base

(require syntax/parse
         syntax/transformer
         "private/syntax-utils.rkt")

(provide (struct-out open-term)
         open-term-set-freevars)

(struct open-term (proc-stx freevars-name args-name immediate?)
  #:property prop:procedure (λ (self stx) (link-freevars self stx)))

(define (open-term-set-freevars who open-term-id map)
  (define (fail)
    (raise-syntax-error who
                        "the binding is not defined by define/freevar"
                        open-term-id))
  (define self
    (syntax-local-value open-term-id fail))
  (unless (open-term? self)
    (fail))
  (define original-fvs (open-term-freevars-name self))
  (define new-fvs
    (for/list ([fv (in-list original-fvs)])
      (hash-ref map fv (λ () fv))))
  (open-term (open-term-proc-stx self)
             new-fvs
             (open-term-args-name self)
             (open-term-immediate? self)))

(define (link-freevars self stx)
  (define/syntax-parse target (open-term-proc-stx self))
  (syntax-parse stx
    [proc-src:id
     #:with (fv ...) (format-original-identifiers (open-term-freevars-name self)
                                                  #:context stx
                                                  #:source #'proc-src)
     #:with (arg ...) (generate-temporaries (open-term-args-name self))
     (cond
       [(open-term-immediate? self)
        (fix-app stx
                 (syntax/loc stx
                   (target fv ...)))]
       [else
        (quasisyntax/loc stx
          (λ (arg ...)
            #,(fix-app stx
                       (syntax/loc stx
                         (target fv ... arg ...)))))])]
    [(proc-src:id . args)
     #:with (fv ...) (format-original-identifiers (open-term-freevars-name self)
                                                  #:context stx
                                                  #:source #'proc-src)
     (cond
       [(open-term-immediate? self)
        (fix-app stx
                 (quasisyntax/loc stx
                   (#,(fix-app stx
                               (syntax/loc stx
                                 (target fv ...)))
                    . args)))]
       [else
        (fix-app stx
                 (syntax/loc stx
                   (target fv ... . args)))])]))

#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     "open-term.rkt"
                     "private/syntax-utils.rkt"))

(provide define/freevar
         with-freevar
         define/with-freevar)

(define-syntax (define/freevar stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        #:freevars (fv:id ...+)
        (~optional (~and #:immediate immediate-flag))
        body:expr ...+)
     #:attr dup-id (check-duplicate-freevar-or-param (syntax-e #'(fv ...))
                                                     (syntax-e #'(arg ...)))
     #:do [(when (attribute dup-id)
             (raise-syntax-error 'define/freevar
                                 "duplicated argument or free variable name"
                                 stx
                                 (attribute dup-id)))]
     #:with immediate? (if (attribute immediate-flag) #t #f)
     #:with name-with-fvs (format-id #'fresh-stx "~a/fvs" #'name)
     #:with defn-with-fvs
     (cond
       [(attribute immediate-flag)
        #`(λ (fv ...)
            #,(syntax-property
               (syntax/loc stx
                 (λ (arg ...) body ...))
               'inferred-name
               (syntax-e #'name)))]
       [else
        (syntax-property
         (syntax/loc stx
           (λ (fv ... arg ...) body ...))
         'inferred-name
         (syntax-e #'name))])
     #`(begin
         (define name-with-fvs defn-with-fvs)
         (define-syntax name
           (open-term #'name-with-fvs
                      '(fv ...)
                      '(arg ...)
                      'immediate?)))]))

(define-syntax (with-freevar stx)
  (syntax-parse stx
    [(_ term-with-fv:id ([fv:id new-fv:id] ...) body:expr ...+)
     #:with new-fv-map (build-datum-hash #'(fv ...) #'(new-fv ...))
     (with-disappeared-uses (record-disappeared-uses #'term-with-fv)
       #'(let-syntax ([term-with-fv
                       (open-term-set-freevars 'with-freevar
                                               (quote-syntax term-with-fv)
                                               'new-fv-map)])
           body ...))]))

(define-syntax (define/with-freevar stx)
  (syntax-parse stx
    [(_ new-name:id original-term-with-fv:id [fv:id new-fv:id] ...)
     #:with new-fv-map (build-datum-hash #'(fv ...) #'(new-fv ...))
     (with-disappeared-uses (record-disappeared-uses #'original-term-with-fv)
       #'(define-syntax new-name
           (open-term-set-freevars 'define/with-freevar
                                   (quote-syntax original-term-with-fv)
                                   'new-fv-map)))]))

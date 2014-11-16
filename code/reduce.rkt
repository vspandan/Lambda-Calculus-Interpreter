#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "fb.rkt")
(require "subst.rkt")
(require rackunit)

(provide
  beta-redex?
  beta-reduce
  eta-redex?
  eta-reduce
  redex?
 )
  
;;; beta-redex? : ast? -> boolean?
(define beta-redex? 
  (lambda (a)
    (cond
      [(id-ref-ast? a) #f]
      [(function-ast? a) (beta-redex? (function.body a))]
      ;[(function-ast? a) #f]
      [(app-ast? a) (if (function-ast?  (app.rator a)) #t (or (beta-redex? (app.rator a)) (beta-redex? (app.rand a))))]
      ;[(app-ast? a) (if (function-ast?  (app.rator a)) #t #f)]
      [else #f]
     ))
)

(check-false (beta-redex? (id-ref 'x)) "beta-redex? 01")

(check-false
  (beta-redex?
    (app (id-ref 'x) (id-ref 'y)))
  "beta-redex? 02")

(check-false
  (beta-redex?
     (function 'x (id-ref 'x)))
  "beta-redex? 03")

(check-true
  (beta-redex?
    (app (function 'x (id-ref 'x)) (id-ref 'y)))
  "beta-redex? 04")

;;; beta-reduce: beta-redex? -> ast?
(define beta-reduce 
  (lambda (a)
    (cond
      [(id-ref-ast? a) a]
      [(function-ast? a) (function (function.formal a) (beta-reduce (function.body a)))]
      ;[(function-ast? a) a]
      [(app-ast? a) (if (function-ast?  (app.rator a)) (apply-subst (make-unit-subst (function.formal (app.rator a)) (app.rand a)) (function.body (app.rator a))) (app (beta-reduce (app.rator a)) (beta-reduce (app.rand a))))]
      ;[(app-ast? a) (if (function-ast?  (app.rator a)) (apply-subst (make-unit-subst (function.formal (app.rator a)) (app.rand a)) (function.body (app.rator a))) a)]
     )
))

(check-true
 (alpha-equal?
   (beta-reduce (app (function 'x (id-ref 'x)) (id-ref 'y)))
   (id-ref 'y)))

;;; eta-redex? : ast? -> boolean?
(define eta-redex? 
  (lambda (a)
    (cond
      [(id-ref-ast? a) #f]
      ;[(function-ast? a) (if (and (app-ast? (function.body a)) (function-ast? (app.rator (function.body a)))) #t #f) ]
      [(function-ast? a) (if (and (app-ast? (function.body a)) (function-ast? (app.rator (function.body a))) (id-ref-ast? (app.rand (function.body a))) (equal? (function.formal a) (id-ref.id (app.rand (function.body a))))) #t (eta-redex? (function.body a))) ]
      ;[(app-ast? a) #f]
      [(app-ast? a) (or (eta-redex? (app.rator a)) (eta-redex? (app.rand a)))]
      ))
)

;;; eta-reduce: eta-redex? -> ast?
(define eta-reduce 
  (lambda (a)
    (cond
      [(id-ref-ast? a) a]
      ;[(function-ast? a) (if (and (app-ast? (function.body a)) (function-ast? (app.rator (function.body a)))) (app.rator (function.body a))  a) ]
      [(function-ast? a) (if (and (app-ast? (function.body a)) (function-ast? (app.rator (function.body a))) (id-ref-ast? (app.rand (function.body a))) (equal? (function.formal a) (id-ref.id (app.rand (function.body a))))) (app.rator (function.body a))  (function (function.formal a) (eta-reduce (function.body a)))) ]
      ;[(app-ast? a) a]
      [(app-ast? a) (app (eta-reduce (app.rator a)) (eta-reduce (app.rand a)))]
      ))
)
  
(define redex? 
  (lambda (a)
    (or (beta-redex? a) (eta-redex? a)))
  )
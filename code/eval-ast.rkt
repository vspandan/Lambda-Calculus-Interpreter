#lang racket

(require eopl/eopl)
(require "ast.rkt")
(require "fb.rkt")
(require "ct.rkt")
(require "parser.rkt")
(require "subst.rkt")
(require "reduce.rkt")
(require rackunit)
(provide
 single-step 
 eval-ast)

;;; Given an ast a, returns the rule name (beta or eta)  and the new ast if the
;;; ast a is reducible, #f and the same ast a if a is not
;;; reducible.

;;; single-step : (ast?  . -> . (list (one-of/c #f 'beta 'eta) ast?))
(define single-step 
  (lambda (a)
    (cond
      [(beta-redex? a) (list 'beta (beta-reduce a))]
      [(eta-redex? a) (list 'eta (eta-reduce a))]
      [else (list #f a)]
      ))
)

;;; Transform ast a by single-step evaluation n times.  If a
;;; is found irreducible before completing the n single
;;; steps, return a.

;;; eval-ast : ast? nat? -> ast?
(define eval-ast 
  (lambda (a n)
    (cond
      [(zero? n) a]
      [(equal? 'beta (first (single-step a))) (eval-ast (second (single-step a)) (sub1 n))]
      [(equal? 'eta (first (single-step a))) (eval-ast (second (single-step a)) (sub1 n))]
      [(equal? #f (first (single-step a))) a]
      )))



(check-true
 (alpha-equal?
   (eval-ast (parse (ct '((lambda (x) y) (lambda (z) (z x))))) 3)
     (parse (ct 'y)))"eval-ast 01")

(check-true
 (alpha-equal?
   (eval-ast (parse (ct '(let ([x y] [y z]) (z x x)))) 2)
   (parse (ct '(z y y))))
 "eval-ast 02")

(check-true
 (alpha-equal?
   (eval-ast
     (parse (ct '(let ([x (lambda (x) (x x))])
                           (x x))))
     3)
   (parse (ct '(let ([x (lambda (x) (x x))])
                 (x x)))))
 "eval-ast 03: omega")

(check-true
  (alpha-equal?
    (eval-ast
      (parse (ct '(let ([x y] [y z])
                    (w (x y)))))
      2)
    (parse (ct '(w (y z)))))
  "eval-ast 04")



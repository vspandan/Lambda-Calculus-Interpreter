#lang racket
(require eopl/eopl)

(require "../code/ast.rkt")
(require "../code/ct.rkt")
(require "../code/fb.rkt")
(require "../code/parser.rkt")
(require "../code/eval-ast.rkt")
(require "../code/reduce.rkt")
(require "../code/subst.rkt")

(require rackunit)
 

(check-true
 (alpha-equal?
   (eval-ast (function 'x (app (function 'y (app (id-ref 'y) (id-ref 'y))) (id-ref 'x) )) 3)
     (function 'z (app (id-ref 'z) (id-ref 'z)))) )

(check-true
 (alpha-equal?
   (eval-ast (parse (ct '(let ([x y] [y z]) (z x x)))) 2)
   (parse (ct '(z y y))))
 )

(check-true
 (alpha-equal?
   (eval-ast
     (parse (ct '(let ([x (lambda (x) (x x))])
                           (x x))))
     3)
   (parse (ct '(let ([x (lambda (x) (x x))])
                 (x x)))))
 )

(check-true
  (alpha-equal?
    (eval-ast
      (parse (ct '(let ([x y] [y z])
                    (w (x y)))))
      2)
    (parse (ct '(w (y z)))))
  )

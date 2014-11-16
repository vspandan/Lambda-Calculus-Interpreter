#lang racket

(require eopl/eopl)

(require "ast.rkt")

(require racket/set)

(provide
  free-vars
  free?)

;;; free-vars : ast? -> (set-of id?)
(define free-vars
  (lambda (a)
    (cases ast a
      [id-ref (x) (set x)]
      [function (formal body)
        (set-remove (free-vars body) formal)]
      [app (rator rand)
        (set-union (free-vars rator) (free-vars rand))])))

;;; free? checks if id x is free in  ast a. 
;;; free?: [id?  ast?] -> boolean?
(define free?
  (lambda (id a)
    (set-member? (free-vars a) id)))

(require rackunit)

(define set-empty (set))

(check-equal? (free-vars (id-ref 'x)) (set 'x))
(check-equal? (free-vars (function 'x (id-ref 'x))) set-empty)
(check-equal?  (free? 'x (id-ref 'x)) #t)
(check-equal?  (free? 'x (function 'x (id-ref 'x))) #f)
(check-equal?  (free? 'y (function 'x (id-ref 'x))) #f)
(check-equal?  (free? 'y (function 'x (id-ref 'y))) #t)
(check-equal?  (free? 'y (function 'x (app (id-ref 'y) (id-ref 'x)))) #t)
(check-equal?  (free? 'x (function 'x (app (id-ref 'y) (id-ref 'x)))) #f)







  
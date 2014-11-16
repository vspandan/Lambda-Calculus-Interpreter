#lang racket

;;; A unit substitution is a singleton substitution with a
;;; domain of just one id.  
(require eopl/eopl)

(require "ast.rkt")
(require "fb.rkt")
(require "ct.rkt")
(require "parser.rkt")

(provide
  unit-subst
  unit-subst?
  make-unit-subst
  unit-subst-id
  unit-subst-ast
  fresh?
  apply-subst
  alpha-convert
  alpha-equal?
  )
 

(define-datatype unit-subst unit-subst?
  [make-unit-subst (id id?) (ast ast?)])

;;; unit-subst-id : unit-subst? -> id?
(define unit-subst-id 
  (lambda (a)
    (cases unit-subst a
      [make-unit-subst (id ast) id]
      )))

;;; unit-subst-id : unit-subst? -> ast?
(define unit-subst-ast 
  (lambda (a)
    (cases unit-subst a
      [make-unit-subst (id ast) ast]
      ))
)

;;; [id? unit-subst?] -> boolean?
(define fresh? 
  (lambda (z a)
    (cond
      [(eq? z (unit-subst-id a)) #f]
      [(id-ref-ast? (unit-subst-ast a)) (if (eq? (id-ref.id (unit-subst-ast a)) z) #f #t)]
      [(app-ast? (unit-subst-ast a)) (and (fresh? z (make-unit-subst (unit-subst-id a) (app.rator (unit-subst-ast a)))) (fresh? z (make-unit-subst (unit-subst-id a) (app.rand (unit-subst-ast a)))))]
      [(function-ast? (unit-subst-ast a)) (and (fresh? z (make-unit-subst (unit-subst-id a) (id-ref (function.formal (unit-subst-ast a))))) (fresh? z (make-unit-subst (unit-subst-id a) (function.body (unit-subst-ast a)))))]
      ; [else #t]
      )))

(require rackunit)

(check-equal? (fresh? 'x (make-unit-subst 'z (id-ref 'w))) #t)
(check-equal? (fresh? 'x (make-unit-subst 'z (id-ref 'x))) #f)
(check-equal? (fresh? 'x (make-unit-subst 'z (id-ref 'x))) #f)
(check-equal? (fresh? 'x (make-unit-subst 'x (id-ref 'y))) #f)



;;; [unit-subst?  ast?] -> ast?
(define apply-subst 
  (lambda (b a)
    (cases ast a
      [id-ref (id) (if (eq? id (unit-subst-id b)) (unit-subst-ast b) a)]
      [app (x y) (app (apply-subst b x) (apply-subst b y))]
      [function (id y) (if (fresh? id b) (function id (apply-subst b y)) (let ([rand (gensym)]) (apply-subst b (function.body (function rand (alpha-convert rand id y))))))]
      )))



;;; alpha-convert : [id? id? ast?] -> function-ast?
(define alpha-convert
  (lambda (z formal body)
    (let ([new-subst (make-unit-subst formal (id-ref z))])
      (function z (apply-subst new-subst body)))))



;;; alpha-equal? return true if its two argument ast's are
;;; alpha-equivalent, false otherwise.
;;; alpha-equal? : [ast? ast?] -> boolean?
(define alpha-equal? 
  (lambda (a b)
     (cond
       [(and (id-ref-ast? a) (id-ref-ast? b)) (equal? a b)] 
       [(and (app-ast? a) (app-ast? b)) (and (alpha-equal? (app.rator a) (app.rator b) ) (alpha-equal? (app.rand a) (app.rand b)))]
       [(and (function-ast? a) (function-ast? b)) (alpha-equal? (function.body a) (function.body (alpha-convert (function.formal a) (function.formal b) (function.body b))))]
       [else #f]
       )))


;;; unit tests
;;; ===========

;;; s1 = [x:z]
(define s1 (make-unit-subst 'x (id-ref 'z)))

(check-equal? (apply-subst s1 (id-ref 'y)) (id-ref 'y) "apply-subst test 01")
(check-equal? (apply-subst s1 (id-ref 'x)) (id-ref 'z) "apply-subst test 02")

(check-equal?
  (apply-subst
    s1
    (app
      (id-ref 'x)
      (id-ref 'y)))
  (app (id-ref 'z)
    (id-ref 'y))
   "apply-subst test 03")

(check-equal?
  (apply-subst s1 (function 'y (id-ref 'y)))
  (function 'y (id-ref 'y))
    "apply-subst test 04")

;;; substitution  application involving renaming.
;;; ==============================================

;;; Naive rule for pushing substitution inside a lambda:
;;; (lambda (x) e)[s]
;;; = (lambda (x) e[s])

;;; 

;;; s1 = [x:z]

(define parse-ct
  (lambda (exp)
    (parse (ct exp))))

;;; Freshness condition 1 violated because formal is in the
;;; domain of s1.
;;; (lambda (x) x)[x:z]  ;; formal is in dom(s)
;;; Naive substitution results in
;;; (lambda (x) z)       ;; incorrect!
;;; Solution:
;;; Rename bound variable to w. 
;;; Apply-subst automatically does
;;; on-the-fly renaming. 

(check-true
 (alpha-equal?
   (apply-subst  s1 (parse-ct '(lambda (x) x)))
   (parse-ct '(lambda (w) w)))
 "apply-subst 09")

;;; Freshness condition 2  violated because formal 
;;; is  free in the  cod of s1.
;;; (lambda (z) x)[x:z]  ;; formal is free in cod(s)
;;; Naively applying the substitution rule results in
;;; (lambda (z) z)     ;; z in now inadvertently captured. 
;;; Solution:
;;; Rename to (lambda (w) x)[x:z]
;;;
;;; Apply-subst does
;;; on-the-fly renaming to restore  freshness.

(check-true
 (alpha-equal?
   (apply-subst  s1 (parse (ct '(lambda (z) x))))
   (parse (ct '(lambda (w) z))))
 "apply-subst? 10")

      

(check-false
  (alpha-equal?
    (id-ref 'x)
    (id-ref 'y))
  "alpha-equal? 01")

(check-true
  (alpha-equal?
    (id-ref 'x)
    (id-ref 'x))
  "alpha-equal? 02")

(check-false
  (alpha-equal?
    (app (id-ref 'x) (id-ref 'y))
    (app (id-ref 'x) (id-ref 'x)))
  "alpha-equal? 03")

(check-true
  (alpha-equal?
    (app (id-ref 'x) (id-ref 'x))
    (app (id-ref 'x) (id-ref 'x)))
  "alpha-equal? 04")


(check-true
  (alpha-equal?
    (function 'x (id-ref 'x))
    (function 'y (id-ref 'y)))
  "alpha-equal? 05")

(check-false
  (alpha-equal?
    (function 'x (id-ref 'y))
    (function 'y (id-ref 'y)))
  "alpha-equal? 06")

(check-false
  (alpha-equal?
    (function 'x (id-ref 'y))
    (function 'y (id-ref 'x)))
  "alpha-equal? 07")

(check-true
  (alpha-equal?
    (function 'x (id-ref 'z))
    (function 'y (id-ref 'z)))
  "alpha-equal? 08")





                

              
                

        

  

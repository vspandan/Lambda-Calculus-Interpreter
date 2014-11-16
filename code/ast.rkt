#lang racket


;;; =======================================
;;; Abstract Syntax for the LAMBDA CALCULUS
;;; =======================================

;;; <ast> ::= 
;;;           
;;;           <id-ref-ast> |
;;;           <function-ast> |
;;;           <app-ast>

;;; <function-ast> ::= (function <id> <ast>)
;;; <app-ast>      ::= (app  <ast>  <ast> )

(require eopl/eopl)

(provide
  ast
  ast?
  id?
  id-ref
  function
  app
  id-ref-ast?
  function-ast?
  app-ast?
  id-ref.id
  function.formal
  function.body
  app.rator
  app.rand
  check-ast?)


;;; id? returns true if the argument is a symbol not eq? to 'lambda
;;; id? : any/c -> boolean?
(define id?
  (lambda (thing)
    (and (symbol? thing) (not (eq? thing 'lambda)))))

;;; definition of the ast of lambda calculus expressions
(define-datatype ast ast?
  [id-ref (id id?)]
  [function (formal id?)  (body ast?)]
  [app (rator ast?) (rand ast?)])


;;; id-ref-ast? returns true if its ast argument is an id-ref
;;; id-ref-ast? :  ast? -> boolean?
;;;
(define id-ref-ast?
  (lambda (a)
    (cases ast a
      [id-ref (id) #t]
      [else #f])))

;;; function-ast? returns true if its ast argument is a
;;; function
;;; function-ast? : ast? -> boolean?
(define function-ast?
  (lambda (a)
    (cases ast a
      [function (formal body) #t]
      [else #f])))

;;; app-ast? returns true if its ast argument is an app
;;; app-ast? : ast? -> boolean?
(define app-ast?
  (lambda (a)
    (cases ast a
      [app (rator rand) #t]
      [else #f])))

;;; id-ref.id returns the id part of an id-ref ast
;;; id-ref.id : id-ref-ast? -> id?
(define id-ref.id
  (lambda (a)
    (cases ast a
      [id-ref (id) id]
      [else (error 'id-ref.id "contract violation: id-ref.id expects an id-ref-ast?, given ~a" a)])))


;;; function.formal returns the formal field of a  function-ast
;;; function.formal: function-ast? -> id?
(define function.formal
  (lambda (a)
    (cases ast a
      [function (formal body) formal]
      [else (error 'function.formal
              "contract violation: function.formal expects an function-ast?, given ~a" a)])))


;;; function.body returns the body field of a function ast
;;; function.body: function-ast? -> ast?
(define function.body
  (lambda (a)
    (cases ast a
      [function (formal body) body]
      [else (error 'function.body
              "contract violation: function.body expects an function-ast?, given ~a" a)])))

;;; app.rator returns the rator field of an app ast
;;; app.rator: app-ast? -> ast?
(define app.rator
  (lambda (a)
    (cases ast a
      [app (rator rand) rator]
      [else (error 'app.rator "contract violation: app.rator expects an app-ast?, given ~a" a)])))

;;; app.rand returns the rand field of an app ast
;;; app.rand: app-ast? -> ast?
(define app.rand
  (lambda (a)
    (cases ast a
      [app (rator rand) rand]
      [else (error 'app.rand "contract violation: app.rand expects an app-ast?, given ~a" a)])))


;;; unit Testing
;;; ============

;;; Racket's unit testing framework
(require rackunit)


(define-simple-check (check-ast? thing)
   (ast? thing))

(check-ast? (id-ref 'x) "id-ref-x test")

(check-ast?
  (app (id-ref 'x) (id-ref 'y))
  "ifte-test1")


(check-ast?
  (function
    'x
    (app (app (id-ref '+)  (id-ref 'x)) (id-ref 'y))) "function-test")


(check-exn    ;; app constructor is incorrectly invoked
 exn:fail?
  (lambda () 
    (app (id-ref '+))) "app test")


















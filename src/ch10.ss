;;; Chapter 10: What is the Value of All This?

;; entry = pair of lists of equal length whose first list is a set.
;; table = list of entries. AKA environment.
;; value = operation that returns the value of an expression. AKA eval.
;; value is an interpreter!
;; numbers            are of type *const.
;; #t and #f          are of type *const.
;; quoted expressions are of type *quote.
;; undefined symbols  are of type *identifier.
;; lambda expressions are of type *lambda.
;; cond expressions   are of type *cond.
;; applied lambda expressions are of type *application.

(load "ch9.ss")

'((hello world) (Travis Hopkins)) ; entry example
'((a b c d) (e f g h)) ; another entry example

;; page 175
(define new-entry ; create entries from a set and corresponding list
  build) ; page 119

;; page 175
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

;; page 176
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names)
        entry-f name)
      ((eq? (car names) name)
        (car values))
      (else
        (lookup-in-entry-help name
                              (cdr names)
                              (cdr values)
                              entry-f)))))

;; page 176
(define extend-table
  cons)

;; page 177
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table)
        (table-f name))
      (else
        (lookup-in-entry name
                         (first table)
                         (lambda (name)
                           (lookup-in-table name
                                            (cdr table)
                                            table-f)))))))

;; page 181
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e)
        (atom-to-action e))
      (else
        (list-to-action e)))))

;; page 181
(define atom-to-action
  (lambda (e)
    (cond
      ((or (number? e)
           (eq? e #t)
           (eq? e #f)
           (eq? e (quote cons))
           (eq? e (quote car))
           (eq? e (quote cdr))
           (eq? e (quote null?))
           (eq? e (quote eq?))
           (eq? e (quote atom?))
           (eq? e (quote zero?))
           (eq? e (quote add1))
           (eq? e (quote sub1))
           (eq? e (quote number?)))
        *const)
      (else
        *identifier))))

;; page 182
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
        (cond
          ((eq? (car e) (quote quote))
            *quote)
          ((eq? (car e) (quote lambda))
            *lambda)
          ((eq? (car e) (quote cond))
            *cond)
          (else
            *application)))
      (else
        *application))))

;; page 182
(define value
  (lambda (e)
    (meaning e (quote ()))))

;; page 182
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; page 183
(define *const
  (lambda (e table)
    (cond
      ((number? e)
        e)
      ((eq? e #t)
        #t)
      ((eq? e #t)
        #t)
      (else
        (build (quote primitive) e)))))

;; page 183
(define *quote
  (lambda (e table)
    (text-of e)))

;; page 183
(define text-of
  second)

;; page 183
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

;; page 183
(define intial-table
  (lambda (name)
    (car (quote ()))))

;; page 184
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table
                 (cdr e)))))

;; page 184
(define table-of
  first)

;; page 184
(define formals-of
  second)

;; page 184
(define body-of
  third)

;; page 185
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
        (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
      (else
        (evcon (cdr lines) table)))))

;; page 185
(define else?
  (lambda (x)
    (cond
      ((atom? x)
        (eq? x (quote else)))
      (else
        #f))))

;; page 185
(define else?
  (lambda (x)
    (and (atom? x)
         (eq? x (quote else)))))

;; page 185
(define question-of
  first)

;; page 185
(define answer-of
  second)

;; page 185
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;; page 185
(define cond-lines-of
  cdr)

;; page 186
(define evlist
  (lambda (args table)
    (cond
      ((null? args)
        (quote ()))
      (else
        (cons (meaning (car args) table)
              (evlist (cdr args) table))))))

;; page 186
(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlist (arguments-of e) table))))

;; page 187
(define function-of
  car)

;; page 187
(define arguments-of
  cdr)

;; page 187
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

;; page 187
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

;; page 187
(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
        (apply-primitive (second fun) vals))
      ((non-primitive? fun)
        (apply-closure (second fun) vals)))))

;; page 188
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
        (cons (first vals) (second vals))))
      ((eq? name (quote car))
        (car (first vals)))
      ((eq? name (quote cdr))
        (cdr (first vals)))
      ((eq? name (quote null?))
        (null? (first vals)))
      ((eq? name (quote eq?))
        (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
        (atom? (first vals)))
      ((eq? name (quote zero?))
        (zero? (first vals)))
      ((eq? name (quote add1))
        (add1 (first vals)))
      ((eq? name (quote sub1))
        (sub1 (first vals)))
      ((eq? name (quote number?))
        (number? (first vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((or (eq? name (quote car))
           (eq? name (quote cdr))
           (eq? name (quote null?))
           (eq? name (quote atom?))
           (eq? name (quote zero?))
           (eq? name (quote add1))
           (eq? name (quote sub2))
           (eq? name (quote number?)))
        (name (first vals)))
      ((or (eq? name (quote cons))
           (eq? name (quote eq?)))
        (name (first vals) (second vals))))))

;; page 188
(define :atom?
  (lambda (x)
    (cond
      ((atom? x)
        #t)
      ((null? x)
        #f)
      ((eq? (car x) (quote primitive))
        #t)
      ((eq? (car x) (quote non-primitive))
        #t)
      (else
        #f))))

;; page 189
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry
                 (formals-of closure)
                 vals)
               (table-of closure)))))


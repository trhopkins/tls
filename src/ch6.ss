;;; Chapter 6: Shadows (p. 96)

;; An arithmetic expression is either an atom (including numbers), or
;; two arithmetic expressions combined with o+, o*, or o^. Infix
;; notation places the arithmetic operator in between its operands.
;; Prefix notation places it before its operands (like Scheme), and
;; postfix notation places it after its operands. We use lists to
;; represent arithmetic expressions, but they can include anything.

(load "ch5.ss")

;; page 101
(define numbered? ; return #t if all operands are numbers
  (lambda (aexp) ; arithmetic expression
    (cond
      ((atom? aexp)
        (number? aexp))
      (else
        (and
          (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp)))))))))

;; The Seventh Commandment
;; Recur on the subparts that are of the same nature:
;; On the sublists of a list.
;; On the subexpressions of an arithmetic expression.

;; page 103
(define infix-value ; return the value of an arithmetic expression
  (lambda (nexp) ; infix numbered arithmetic expression
    (cond
      ((atom? nexp)
        nexp)
      ((eq? (car (cdr nexp)) (quote o+)) ; use first-class behavior?
        (o+ (value (car nexp))
            (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote o*))
        (o* (value (car nexp))
            (value (car (cdr (cdr nexp))))))
      (else
        (o^ (value (car nexp))
            (value (car (cdr (cdr nexp)))))))))

;; page 104
(define prefix-value ; return the value of an arithmetic expression
  (lambda (nexp) ; prefix numbered arithmetic expression
    (cond
      ((atom? nexp)
        nexp)
      ((eq? (car nexp) (quote o+))
        (o+ (value (car (cdr nexp)))
            (value (car (cdr (cdr nexp))))))
      ((eq? (car nexp) (quote o*))
        (o* (value (car (cdr nexp)))
            (value (car (cdr (cdr nexp))))))
      (else
        (o^ (value (car (cdr nexp)))
            (value (car (cdr (cdr nexp)))))))))

;; page 105
(define 1st-sub-exp ; return the first subexpression
  (lambda (aexp) ; arithmetic expression
    (car (cdr aexp))))

;; page 105
(define 2nd-sub-exp ; return the second subexpression
  (lambda (aexp) ; arithmetic expression
    (car (cdr (cdr aexp)))))

;; page 106
(define operator ; return the operation to perform
  (lambda (aexp) ; arithmetic expression
    (car aexp)))

;; page 106
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp)
        nexp)
      ((eq? (operator nexp) (quote o+))
        (o+ (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote o*))
        (o* (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp))))
      (else
        (o^ (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

;; The Eighth Commandment
;; use help functions to abstract from representations.

;; page 108
(define sero?
  (lambda (ln) ; list representation of a number, page 107
    (null? ln)))

;; page 108
(define edd1
  (lambda (ln)
    (cons (quote ())
          ln)))

;; page 108
(define zub1
  (lambda (ln)
    (cdr ln)))

;; page 109?
(define lo+
  (lambda (ln lm)
    (cond
      ((sero? lm)
        ln)
      (else
        (edd1 (lo+ ln (zub1 lm)))))))

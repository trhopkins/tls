;;; Chapter 2: Do It Again... (p. 14)

;; Are you rested?

;; A *lat* is a list of atoms.
;; When evaluating a function, answer every question in order.
;; **define** names things.
;; **lambda** defines a function.
;; **cond** asks a question.
;; **else** is a question which is always true.
;; **or** evaluates two questions, one at a time. If either is #t, return #t

(load "ch1.ss")

;; page 16
(define lat? ; if l is a list of atoms, return #t
  (lambda (l)
    (cond
      ((null? l)
        #t)
      ((atom? (car l))
        (lat? (cdr l)))
      (else
        #f))))

;; page 22
(define member? ; if a is a member of lat, return #t
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
        (or (eq? a (car lat))
            (member? a (cdr lat)))))))

;; The First Commandment (Preliminary)
;; Always ask null? as the first question in expressing any function


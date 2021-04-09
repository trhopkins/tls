;;; Chapter 5: *Oh My Gawd*: It's Full of Stars!

;; *-functions recur on the car as well as the cdr of a list whenever the car
;; is a list itself. They work on empty lists, atoms consed onto lists, and
;; lists consed onto lists. That's also why they ask three questions about
;; lists of S-expressions (see the First Commandment).
;; and evaluates arguments one at a time until one is false. If none
;; are false, it evaluates to true.
;; or evaluates arguments one at a time until one is true If none are
;; true, it evaluates to false.
;; and/or can sometimes not evaluate arguments because they work in
;; normal order, lazily evaluating until necessary. If there are
;; side-effects, sometimes they will not occur.
;; rember is not a *-function because it does not recur on (car l).

(load "ch4.ss")

;; page 81
(define rember* ; rember for general lists (includes S-expressions)
  (lambda (a l)
    (cond
      ((null? l)
        (quote ()))
      ((atom? (car l))
        (cond
          ((eq? a (car l))
            (rember* a (cdr l)))
          (else
            (cons (car l)
                  (rember* a (cdr l))))))
      (else
        (cons (rember* a (car l))
              (rember* a (cdr l)))))))

;; page 82
(define insertR* ; insertR for general lists
  (lambda (new old l)
    (cond
      ((null? l)
        (quote ()))
      ((atom? (car l))
        (cond
          ((eq? old (car l))
            (cons old
                  (cons new
                        (insertR* new old (cdr l)))))
          (else
            (cons (car l)
                  (insertR* new old (cdr l))))))
      (else
        (cons (insertR* new old (car l))
              (insertR* new old (cdr l)))))))

;; The First Commandment (final version)
;; When recurring on a list of atoms, lat, ask two questions about it: null?,
;; and else.
;; When recurring on a number, n, ask two questions about it: zero?, and else.
;; When recurring on a list of S-expressions, l, ask three questions about it:
;; null?, eq?, and else.

;; The Fourth Commandment (final version)
;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat).
;; When recurring on a number, n use (sub1 n).
;; When recurring on a list of S-expressions, use (car l) and (cdr l) if
;; neither (null? l) nor (atom? (car l)) are true.
;; It must be changed to be closer to termination. The changing argument must
;; be tested in the termination condition:
;; When using cdr, test termination with null?.
;; When using sub1, test termination with zero?.

;; page 85
(define occur* ; count instances of a in l
  (lambda (a l)
    (cond
      ((null? l)
        0)
      ((atom? (car l))
        (cond
          ((eq? a (car l))
            (add1 (occur* a (cdr l))))
          (else
            (occur* a (cdr l)))))
      (else
        (o+ (occur* a (car l))
            (occur* a (cdr l)))))))

;; page 85
(define subst* ; general subst
  (lambda (new old l)
    (cond
      ((null? l)
        (quote ()))
      ((atom? (car l))
        (cond
          ((eq? old (car l))
            (cons new
                  (subst* new old (cdr l))))
          (else
            (cons (car l)
                  (subst* new old (cdr l))))))
      (else
        (cons (subst* new old (car l))
              (subst* new old (cdr l)))))))

;; page 86
(define insertL* ; general insertL
  (lambda (new old l)
    (cond
      ((null? l)
        (quote ()))
      ((atom? (car l))
        (cond
          ((eq? old (car l))
            (cons new
                  (cons old
                        (cdr l))))
          (else
            (cons (car l)
                  (insertL* new old (cdr l))))))
      (else
        (cons (insertL* new old (car l))
              (insertL* new old (cdr l)))))))

;; page 87
(define member* ; general member
  (lambda (a l)
    (cond
      ((null? l)
        #f)
      ((atom? (car l))
        (cond
          ((eq? a (car l))
            #t)
          (else
            (member* a (cdr l)))))
      (else
        (or
          (member* a (car l))
          (member* a (cdr l)))))))

;; page 87
(define alt-member* ; general member, different atom? consequent
  (lambda (a l)
    (cond
      ((null? l)
        #f)
      ((atom? (car l))
        (or ; tadaah
          (eq? a (car l))
          (alt-member* a (cdr l))))
      (else
        (or
          (alt-member* a (car l))
          (alt-member* a (cdr l)))))))

;; page 88
(define leftmost ; return first atom in list
  (lambda (l)
    (cond
      ((atom? (car l))
        (car l))
      (else
        (leftmost (car l))))))

#|
;; page 92
#; (define eqlist?
  (lambda (l1 l2)
    (cond
      ((and
         (null? l1)
         (null? l2))
        #t)
      ((or
         (null? l1)
         (null? l2))
        #f)
      ((and
         (atom? (car l1))
         (atom? (car l2)))
        (and
          (eqan? (car l1) (car l2)) ; page 78
          (eqlist? (cdr l1) (cdr l2))))
      ((or
         (atom? (car l1))
         (atom? (car l2)))
        #f)
      (else
        (and
          (eqlist? (car l1) (car l2))
          (eqlist? (cdr l1) (car l2)))))))
|#

;; page 92
(define equal? ; compare two S-expressions
  (lambda (s1 s2)
    (cond
      ((and
         (atom? s1)
         (atom s2))
        (eqan? s1 s2)) ; page 78
      ((or
         (atom? s1)
         (atom? s2))
        #f)
      (else ; may be unnecessary?
        (eqlist? s1 s2)))))

;; page 93
(define eqlist? ; rewritten with equal?
  (lambda (l1 l2)
    (cond
      ((and
         (null? l1)
         (null? l2))
        #t)
      ((or
         (null? l1)
         (null? l2))
        #f)
      (else
        (and
          (equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))

;; The Sixth Commandment
;; Simplify only after the function is correct.

;; page 94
(define rember ; rewritten to handle S-expressions
  (lambda (s l)
    (cond
      ((null? l)
        (quote ()))
      (else
        (cond
          ((equal? s (car l))
            (cdr l))
          (else
            (cons (car l)
                  (rember s (cdr l)))))))))

;; page 95
(define rember ; rewritten to be simplified
  (lambda (s l)
    (cond
      ((null? l)
        (quote ()))
      ((equal? s (car l))
        (cdr l))
      (else
        (cons (car l)
              (rember s (cdr l)))))))


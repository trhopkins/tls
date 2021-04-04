;;; Chapter 3: Cons the Magnificent (p. 32)

;; A typical element is the type of item in the list.
;; The typical element of a tup is a number.
;; The typical element of a lat is an atom.
;; The typical element of a list is an S-expression.
;; The typical element of a number is 1.
;; The natural recursion of a list is (cdr l). Same for lat.
;; The natural recursion of a number is (sub1 n).
;; Get the typical element of a list or lat with (car s).

;; The Second Commandment
;; Use cons to build lists.

(load "ch2.ss")

;; page 36
(define rember ; remove member from list
  (lambda (a lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? a (car lat))
        (cdr lat))
      (else
        (cons (car lat)
              (rember a (cdr lat)))))))

;; page 44
(define firsts ; return a list of the first S-expression for each list in l
  (lambda (l)
    (cond
      ((null? l)
        (quote ()))
      (else
        (cons (car (car l))
              (firsts (cdr l)))))))

;; The Third Commandment
;; When building a list, describe the first typical element, and then cons it
;; onto the natural recursion.

;; page 50
(define insertR ; insert new to the right of old
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons old
              (cons new
                    (cdr lat))))
      (else
        (cons (car lat)
              (insertR new old (cdr lat)))))))

;; page 51
(define insertL ; insert new to the left of old
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons new lat))
      (else
        (cons (car lat)
              (insertL new old (cdr lat)))))))

;; page 51
(define subst ; substitute the first instance of old with new
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons new
              (cdr lat)))
      (else
        (cons (car lat)
              (subst new old (cdr lat)))))))

;; page 52
(define subst2 ; replace the first instance of o1 or o2 with new
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)
        (quote ()))
      ((or (eq? o1 (car lat))
           (eq? o2 (car lat)))
        (cons new
              (cdr lat)))
      (else
        (cons (car lat)
              (subst2 new o1 o2 (cdr lat)))))))

;; page 53
(define multi-rember ; delete all instances of a from lat
  (lambda (a lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? a (car lat))
        (multi-rember a (cdr lat)))
      (else
        (cons (car lat)
              (rember a (cdr lat)))))))

;; page 56
(define multi-insertR ; insert new to the left of every old
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons old
              (cons new
                    (multi-insertR new old (cdr lat)))))
      (else
        (cons (car lat)
              (multi-insertR new old (cdr lat)))))))

;; page 56
(define multi-insertL ; insert new to the right of every old
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons new
              (cons old
                    (multi-insertR new old (car lat)))))
      (else
        (cons (car lat)
        (multi-insertL new old (cdr lat)))))))

;; The Fourth Commandment (Preliminary)
;; Always change at least one argument while recurring. It must be changed to
;; be closer to termination. The changing argument must be tested in the
;; termination condition:
;; When using cdr, test termination with null?

;; page 57
(define multi-subst ; replace every old with new
  (lambda (new old lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? old (car lat))
        (cons new
              (multi-subst new old (cdr lat))))
      (else
        (cons (car lat)
              (multi-subst new old (cdr lat)))))))

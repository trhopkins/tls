;;; Chapter 7: Friends and Relations (p. 110)

;; A set is a list containing no duplicates.
;; union is similar to or.
;; intersect is similar to and.
;; difference is similar to n o- m (better example?).
;; A pair is a list containing two S-expressions.

(load "ch6.ss")

;; page 111
(define set?
  (lambda (lat)
    (cond
      ((null? lat)
        #t)
      ((member? (car lat) (cdr lat))
        #f)
      (else
        (set? (cdr lat))))))

;; page 112
(define makeset ; using rember
  (lambda (lat)
    (cond
      ((null? lat)
        (quote ()))
      ((member? (car lat) (cdr lat))
        (makeset (cdr lat)))
      (else
        (cons (car lat)
              (makeset (cdr lat)))))))

;; page 112
(define makeset ; using multi-rember
  (lambda (lat)
    (cond
      ((null? lat)
        (quote ()))
      (else
        (cons (car lat)
              (makeset (multi-rember (car lat) (cdr lat))))))))

;; page 113
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1)
        #t)
      ((member? (car set1) set2)
        (subset? (cdr set1) set2))
      (else
        #f)))) ; unusual

;; page 114
(define subset? ; using and
  (lambda (set1 set2)
    (cond
      ((null? set1)
        #t)
      (else
        (and (member? (car set1) set2)
             (subset? (cdr set1) set2))))))

;; page 115
(define eqset? ; simplest version
  (lambda (set1 set2)
    (and (subset set1 set2)
         (subset set2 set1))))

;; page 115
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1)
        #f)
      ((member? (car set1) set2)
        #t)
      (else
        (intersect? (cdr set1) set2)))))

;; page 115
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1)
        #f)
      (else
        (or (member? (car set1) set2)
            (intersect? (cdr set1) set2))))))

;; page 116
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1)
        (quote ()))
      ((member? (car set1) set2)
        (cons (car set1)
              (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)))))

;; page 116
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1)
        set2)
      ((member? (car set1) set2)
        (union (cdr set1) set2))
      (else
        (cons (car set1)
              (union (cdr set1) set2))))))

;; page 117
(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1)
        (quote ()))
      ((member? (car set1) set2)
        (difference (cdr set1) set2))
      (else
        (cons (car set1)
              (difference (cdr set1) set2))))))

;; page 117
(define intersectall
  (lambda (l-set) ; list of sets
    (cond
      ((null? (cdr l-set))
        (car l-set))
      (else
        (intersect (car l-set) (intersectall (cdr l-set)))))))

;; page 118
(define a-pair?
  (lambda (l)
      (o= (length l) 2)))

;; page 118
(define a-pair?
  (lambda (l)
      (o= (length l) 2)))


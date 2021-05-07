;;; Chapter 7: Friends and Relations (p. 110)

;; A set is a list containing no duplicates.
;; union is similar to or.
;; intersect is similar to and.
;; difference is similar to n o- m (better example?).
;; A pair is a list containing two S-expressions.
;; A rel is a set of pairs.
;; A fun (function, finite function) is a set of pairs with unique
;; firsts.
;; A fun is one-to-one if its outputs contain no duplicates. In other
;; words, it passes the vertical line test.
;; ((chocolate chip) (doughy cookie)) is a one-to-one function.

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
(define a-pair? ; my solution. Pair of atoms?
  (lambda (l)
      (o= (length l) 2)))

;; page 118
(define a-pair?
  (lambda (x)
    (cond
      ((null? x)
        #f)
      ((atom? x)
        #f)
      ((null? (cdr x))
        #f)
      ((null? (cdr (cdr x)))
        #t)
      (else
        #f))))

;; page 119
(define first
  (lambda (p) ; pair
    (car p)))

;; page 119
(define second
  (lambda (p) ; pair
    (car (cdr p))))

;; page 119
(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                (quote ())))))

;; page 119
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

;; page 120
(define fun? ; horizontal line test
  (lambda (rel)
    (set? (firsts rel))))

;; page 120
(define revrel
  (lambda (rel)
    (cond
      ((null? rel)
        (quote ()))
      (else
        (cons (build (second (car rel))
                     (first (car rel)))
              (revrel (cdr rel)))))))

;; page 121
(define revpair ; helper for revrel
  (lambda (p)
    (build (second p)
           (first p))))

;; page 121
(define revrel ; rewritten with revpair
  (lambda (rel)
    (cond
      ((null? rel)
        (quote ()))
      (else
        (cons (revpair (car rel))
              (revrel (cdr rel)))))))

;; page 44
(define seconds ; return a list of the second item in each S-expression
  (lambda (l)
    (cond
      ((null? l)
        (quote ()))
      (else
        (cons (cdr (car l))
              (firsts (cdr l)))))))

;; page 122
(define fullfun? ; vertical line test, sequel to fun?
  (lambda (fun)
    (set? (seconds fun))))

;; page 122
(define one-to-one? ; clever fullfun rewrite
  (lambda (fun)
    (fun? (revrel fun))))

;; page 123
;(define cookies ; if it doesn't compile, use your imagination
  ;(lambda ()
    ;(bake ; will this have side effects?
      ;(quote (350 degrees))
      ;(quote (12 minutes))
    ;(mix
      ;(quote (walnuts 1 cup))
      ;(quote (chocolate-chips 16 ounces))
      ;(mix
        ;(mix
          ;(quote (flour 2 cups))
          ;(quote (oatmeal 2 cups))
          ;(quote (salt .5 teaspoons))
          ;(quote (baking-powder 1 teaspoon))
          ;(quote (baking-soda 1 teaspoon)))
        ;(mix
          ;(quote (eggs 2 large))
          ;(quote (vanilla 1 teaspoon))
          ;(cream
            ;(quote (butter 1 cup))
            ;(quote sugar 2 cups)))))))


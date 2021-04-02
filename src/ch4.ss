;;; Chapter 4: Numbers Games (p. 58)

;; We use real integers only (0, 1, 2, ...).
;; zero? is like null?.
;; add1 is like cons.
;; sub1 is like cdr.
;; A tup is a list containing only numbers.
;; 0 is like (quote ()) (terminal case).
;; (sub1 m) is the natural recursion of a number.
;; 0 is the base case of a number, like () to lat.
;; Compare atoms with eq?, and numbers with o= (different).
;; number? returns true if an atom is actually a number.

;; page xiii
(define add1 ; primitive increment
  (lambda (n)
    (+ n 1)))

;; page xiii
(define sub1 ; primitive decrement
  (lambda (n)
    (- n 1)))

;; page 60
(define o+ ; add n and m
  (lambda (n m)
    (cond
      ((zero? m)
        n)
      (else
        (add1 (o+ n (sub1 m))))))) ; not the most efficient way

;; iterative solution to adding
#; (define o+ ; add n and m
  (lambda (n m)
    (cond
      ((zero? m)
        n)
      (else
        (o+ (add1 n) (sub1 m))))))

;; page 61
(define o- ; subtract m from n
  (lambda (n m)
    (cond
      ((zero? m)
        n)
      (else
        (sub1 (o+ n (sub1 m)))))))

;; The First Commandment (first revision)
;; When recurring on a list of atoms, lat, as two questions about it: null?
;; and else.
;; When recurring on a number, n, ask two questions about it: zero? and else.

;; page 64
(define addtup ; build an integer with tup
  (lambda (tup)
    (cond
      ((null? tup)
        0)
      (else
        (o+ (car tup) (addtup (cdr tup)))))))

;; The Fourth Commandment (first revision)
;; Always change at least one argument while recirring. it must be changed to
;; be closer to termination. The changing argument must be tested in the
;; termination condition:
;; When using cdr, test termination with null?.
;; When using sub1, test termination with zero?.

;; page 65
(define o* ; multiply n and m
  (lambda (n m)
    (cond
      ((zero? m)
        0)
      (else
        (o+ n (o* n (sub1 m)))))))

;; The Fifth Commandment
;; When building a number with o+, always use 0 for the value of the
;; terminating line, for adding 0 does not change the value of an addition.
;; When building a number with o*, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value of a
;; multiplication.
;; When building a list with cons, always consider () for the value of the
;; terminating line.

;; page 69
(define tup+ ; add each number in tup1 and tup2, return list of sums
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
        (quote ()))
      (else
        (cons (o+ tup1 tup2)
              (tup+ (cdr tup1) (cdr tup2)))))))

;; page 71
(define new-tup+ ; tup+, but allows for different lengths
  (lambda (tup1 tup2)
    (cond
      #; ((and (null? tup1) (null? tup2))
        (quote ()))
      ((null? tup1)
        tup2)
      ((null? tup2)
        tup1)
      (else
        (cons (o+ tup1 tup2)
              (tup+ (cdr tup1) (cdr tup2)))))))

;; page 72
(define o> ; if n is greater than m, #t
  (lambda (n m)
    (cond
      ((zero? n) ; order matters
        #f)
      ((zero? m)
        #t)
      (else
        (o> (sub1 n) (sub1 m))))))

;; page 73
(define o< ; if n is less than m, #t
  (lambda (n m)
    (cond
      ((zero? m) ; order matters
        #f)
      ((zero? n)
        #t)
      (else
        (o< (sub1 n) (sub1 m))))))

;; page 74
(define o= ; if n equals m, #t
  (lambda (n m)
    (cond
      ((o> n m)
        #f)
      ((o< n m)
        #f)
      (else
        #t))))

;; page 74
(define o^ ; raise n to the power m, like expt
  (lambda (n m)
    (cond
      ((zero? m)
        1)
      (else
        (o* n (o^ n (sub1 m)))))))

;; page 75
(define o/ ; divide n by m, like quotient
  (lambda (n m)
    (cond
      ((o< n m)
        #f)
      (else
        (add1 (o/ (o- n m) m)))))) ; this recursion breaks pattern so far

;; page 76
(define length ; number of atoms in lat
  (lambda (lat)
    (cond
      ((null? lat)
        0)
      (else
        (add1 (length (cdr lat)))))))

;; page 76
(define pick ; return the nth item in lat
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) ; indices start at 1
        (car lat))
      (else
        (pick (sub1 n) (cdr lat))))))

;; page 77
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n))
        (cdr lat))
      (else
        (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;; page 77
(define no-nums ; remove all numbers from lat
  (lambda (lat)
    (cond
      ((null? lat)
        (quote ()))
      ((number? (car lat))
        (no-nums (cdr lat)))
      (else
        (cons (car lat) (no-nums (cdr lat)))))))

;; page 78
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat)
        (quote ()))
      ((number? (car lat))
        (cons (car lat) (no-nums (cdr lat)))) ; non-standard recursion stuff
      (else
        (no-nums (cdr lat))))))

;; page 78
(define eqan? ; compare two numbers or two atoms
  (lambda (n m)
    (cond
      ((and (number? n) (number? m))
        (o= n m))
      ((and (atom? n) (atom? m)) ; requires preface atom? function
        (eq? n m))
      (else
        #f)))) ; different from book, but preferred reasoning

;; page 78
(define occur ; number of times a appears in lat
  (lambda (a lat)
    (cond
      ((null? lat)
        0)
      ((eq? a (car lat))
        (add1 (occur a (cdr lat))))
      (else
        (occur a (cdr lat))))))

;; page 79
(define one? ; return #t if n == 1
  (lambda (n)
    (cond
      ((zero? (sub1 n))
        #t)
      (else
        #f))))

;; page 79
(define new-one? ; another way to write one?
  (lambda (n)
    (o= n 1)))

;; page 79
(define new-rempick ; rewritten to use one?
  (lambda (n lat)
    (cond
      ((one? n)
        (cdr lat))
      (else
        (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


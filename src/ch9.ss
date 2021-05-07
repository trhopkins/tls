;;; Chapter 9: And Again, and Again, and Again...

;; basically putting some funky list traversals here
;; keep-looking is an 'unnatural' recursion, which may not reach its goal. It
;; does not recur on the cdr of the lat.
;; pick and keep-looking are 'mutually recursive'; they recur on each other.
;; a 'partial' function is something which may not find its answer. We are more
;; concerned with 'total' functions, which will always have some answer to
;; give, unless the data they recur on is infinite.
;; the natural recursion of a total function is self-similar to its output.

(load "ch8.ss")

;; page 149
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;; page 150
(define keep-looking ; example of unnatural mutual recursion
  (lambda (a sorn lat) ; sorn = symbol or number
    (cond
      ((number? sorn)
        (keep-looking a (pick sorn lat) lat))
      (else
        (equal? a sorn)))))

;; page 151
(define eternity ; most partial, 'unnatural' function
  (lambda (x)
    (eternity x)))

;; page 152
(define shift ; ((a b) (c d)) -> (a (b (c d)))
  (lambda (pair)
    (build (first (first pair)) ; page 119
           (build (second (first pair))
                  (second pair)))))

;; page 152
(define align ; partial function; not guaranteed to make progress
  (lambda (pora) ; pora = pair or atom
    (cond
      ((atom? pora)
        pora)
      ((a-pair? (first pora)) ; page 118
        (align (shift pora)))
      (else
        (build (first pora)
               (align (second pora)))))))

;; page 153
(define length*
  (lambda (pora)
    (cond
      ((atom? pora)
        1)
      (else
        (o+ (length* (first pora))
            (length* (second pora)))))))

;; page 154
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora)
        1)
      (else
        (o+ (o* (weight* (first pora))
                2) ; pretty-printing: align arguments vertically
            (weight* (second pora)))))))

;; page 154
(define shuffle ; partial for pair of pairs
  (lambda (pora)
    (cond
      ((atom? pora)
        pora)
      ((a-pair? (first pora))
        (shuffle (revpair pora))) ; page 121
      (else
        (build (first pora)
               (shuffle (second pora)))))))

;; page 155
(define collatz ; partial for non-zero n? Nobody knows.
  (lambda (n)
    (cond
      ((one? n)
        1)
      ((even? n)
        (collatz (/ n 2)))
      (else
        (collatz (add1 (o* 3 n)))))))

;; page 156
(define ackermann ; total, but grows so fast might as well be partial
  (lambda (n m)
    (cond
      ((zero? n)
        (add1 m))
      ((zero? m)
        (ackermann (sub1 n) 1))
      (else
        (ackermann (sub1 n) (ackermann n (sub1 m)))))))

;; page 157
;(define stops? ; if f returns a value, #t. Halting problem?
  ;(lambda (f) ; function
    ;(cond
      ;((does-stop? f) ; can be proven but not defined formally
        ;#t)
      ;(else
        ;#f))))

;; page 158
(define last-type ; interesting input for stops?
  (lambda (x)
    (and (stops? last-try)
         (eternity x))))

;; page 160
(lambda (l) ; anonymous length_=0, hangs otherwise
  (cond
    ((null? l)
      0)
    (else (add1 (eternity (cdr l))))))

(lambda (l) ; anonymous length_<=1, hangs otherwise
  (cond
    ((null? l)
      0)
    (else
      (add1
        ((lambda (l)
           (cond
             ((null? l)
               0)
             (else
               (add1
                 (eternity (cdr l))))))
         (cdr l))))))

;; page 162
(lambda (length) ; return length_=0
  (lambda (l)
    (cond
      ((null? l)
        0)
      (else
        (add1 (length (cdr l)))))))

;; page 163
((lambda (f) ; apply length_=0 to...
   (lambda (l) ; basic length function
     (cond
       ((null? l)
         0)
       (else
         (add1 (f (cdr l)))))))
 ((lambda (g) ; apply length_<=1 to...
    (lambda (l) ; basic length function again
      (cond
        ((null? l)
          0)
        (else
          (add1 (g (cdr l)))))))
  eternity)) ; g applied to eternity

;; page 163
((lambda (f) ; first layer, length_=0
   (lambda (l) ; basic length function
     (cond
       ((null? l)
         0)
       (else
         (add1 (f (cdr l)))))))
 ((lambda (g) ; second layer, length_<=1
    (lambda (l) ; basic length function
      (cond
        ((null? l)
          0)
        (else
          (add1 (g (cdr l)))))))
  ((lambda (h) ; third layer, length_<=2
     (lambda (l) ; basic length function
       (cond
         ((null? l)
           0)
         (else
           (add1 (g (cdr l)))))))
   eternity))) ; if length > 2, hang

;; page 164
((lambda (mk-length) ; generalize f, g, and h for length_=0
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (g (cdr l))))))))

;; page 164
((lambda (mk-length) ; generalized for length_<=1
   (mk-length
     (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (length (cdr l))))))))

;; page 164
((lambda (mk-length) ; generalized for length_<=2
   (mk-length
     (mk-length
       (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (length (cdr l))))))))

;; page 164
((lambda (mk-length) ; generalized for length_<=3
   (mk-length
     (mk-length
       (mk-length
         (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (length (cdr l))))))))

;; page 165
((lambda (mk-length) ; reinterpreted
   (mk-length mk-length)) ; pass mk-length to itself. Still length_=0
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (length (cdr l))))))))

;; page 165
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length) ; only change, demonstration of lexical scoping
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 (mk-length (cdr l))))))))

;; page 166
((lambda (mk-length) ; apply once for length_<=1
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 ((mk-length eternity) (cdr l)))))))) ; new recursion

;; page 167
((lambda (mk-length) ; apply once for length_<=1
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 ((mk-length mk-length) (cdr l)))))))) ; newer recursion

#|
;; page 168
((lambda (mk-length) ; for some reason Chez Scheme just hangs on this function
  (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l)
            0)
          (else
            (add1 (length (cdr l)))))))
    (mk-length mk-length))))
|#

;; page 170
((lambda (mk-length) ; mk-length applied within length's recursion
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 ((mk-length mk-length) (cdr l))))))))

;; page 171
((lambda (mk-length) ; mk-length applied within length's recursion
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l)
         0)
       (else
         (add1 ((lambda (x)
                  ((mk-length mk-length) x))
                (cdr l))))))))

;; page 171
((lambda (mk-length) ; restore length back to its original form
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l)
            0)
          (else
            (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; page 172
(define Y ; Y combinator
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


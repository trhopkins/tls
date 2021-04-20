;;; Chapter 8: Lambda The Ultimate (p. 124)

;; functions are first-class citizens: they can be manipulated as data.
;; Currying: input a function and return a function.
;; Collectors are also known as continuations.
;; Collector functions look at every atom in a lat, and add those not matching a to lat1, and those matching to lat2 (or just anything meeting your condition). Then it performs col on lat1 and lat2.

(load "ch7.ss")

;; page 126
(define rember-f ; new comparison abstraction
  (lambda (test? a l)
    (cond
      ((null? l)
        (quote ()))
      ((test? a (car l))
        (cdr l))
      (else
        (cons (car l)
              (rember-f test? a (cdr l)))))))

;; page 127
(define eq?-c ; currying example
  (lambda (a)
    (lambda (x) ; returned function
      (eq? a x))))

;; page 128
(define eq?-salad
  (eq?-c 'salad))

;; page 128
((eq?-c 'salad) 'salad) ; equivalent to eq?-salad

;; page 128
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l)
          (quote ()))
        ((test? a (car l))
          (cdr l))
        (else
          (cons (car l)
                ((rember-f test?) a (cdr l))))))))

;; page 130
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l)
          (quote ()))
        ((test? old (car l))
          (cons new
                (cons old
                      (cdr l))))
        (else
          (cons (car l)
                ((insertL-f test?) new old (cdr l))))))))

;; page 130
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l)
          (quote ()))
        ((test? old (car l))
          (cons old
                (cons new
                      (cdr l))))
        (else
          (cons (car l)
                ((insertR-f test?) new old (cdr l))))))))

;; page 131
(define seqL
  (lambda (new old l)
    (cons new
          (cons old l))))

;; page 131
(define seqR
  (lambda (new old l)
    (cons old
          (cons new l))))

;; page 132
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l)
          (quote ()))
        ((test? old (car l))
          (seq new old l))
        (else
          (cons (car l)
                ((insert-g test? side) new old (cdr l))))))))

;; page 132
(define insertL
  (insert-g seqL))

;; page 132
(define insertR
  (insert-g seqR))

;; page 132
(define insertL
  (insert-g
    (lambda (new old l)
      (cons new
            (cons old
                  l)))))

;; page 132
(define insertR
  (insert-g
    (lambda (new old l)
      (cons old
            (cons new
                  l)))))

;; page 133
(define seqS ; used by insert-g for subst
  (lambda (new old l)
    (cons new l))) ; ignore old

;; page 133
(define subst
  (insert-g seqS))

;; page 133
(define seqrem ; used by insert-g for rember
  (lambda (new old l)
    l)) ; ignore all

;; page 133
(define rember
  (lambda (a l)
    ((insert-g seqrem) 'ignore 'ignore l)))

;; page 134
(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a (quote o+))
        o+)
      ((eq? a (quote o*))
        o*)
      (else
        o^))))

;; The Ninth Commandment
;; Abstract common patterns with a new function.

;; page 135
(define atom-to-function
  (lambda (nexp)
    (cond
      ((atom? nexp)
        nexp) ; page 106
      (else
        ((atom-to-function (operator nexp))
         (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp)))))))

;; page 135
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat)
          (quote ()))
        ((test? a (car l))
          ((multirember-f test?) a (cdr l)))
        (else
          (cons (car l)
                ((multirember-f test?) a (cdr l))))))))

;; page 136
(define multirember-eq?
  (multirember-f eq?))

;; page 136
(define eq?-tuna
  (eq?-c (quote tuna))) ; callback to page 128

;; page 137
(define multiremberT
  (lambda (test? a lat)
    (cond
      ((null? lat)
        (quote ()))
      ((test? a lat)
        (multiremberT test? a (cdr lat)))
      (else
        (cons (car lat)
              (multiremberT test? a (cdr lat)))))))

;; page 137
(define multirember&co
  (lambda (a lat col) ; collector/continuation added
    (cond
      ((null? lat)
        (col (quote ())
             (quote ()))) ; different base case
      ((eq? a (car lat))
        (multirember&co a (cdr lat)
          (lambda (newlat seen) ; new continuation
            (col newlat
                 (cons (car lat)
                       seen)))))
      (else
        (multirember&co a (cdr lat)
          (lambda (newlat seen) ; new continuation
            (col (cons (car lat)
                       newlat)
                 seen))))))) ; pretty printing?

;; page 138
(define a-friend
  (lambda (x y) ; ignore x
    (null? y)))

;; page 139
(define new-friend ; new tuna-collector
  (lambda (newlat seen)
    (a-friend newlat
         (cons (quote tuna)
               seen))))

;; page 139
(define latest-friend ; else from multirember&co with (tuna) for lat
  (lambda (newlat seen)
    (a-friend (cons (quote and)
                    newlat)
              seen)))

;; page 140
(define last-friend
  (lambda (x y)
    (length x)))

;; The Tenth Commandment
;; Build functions to collect more than one value at a time.

;; page 141
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat)
        (quote ()))
      ((eq? oldL (car lat))
        (cons new
              (cons oldL
                    (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat))
        (cons oldR
              (cons new
                    (multiinsertLR new oldL oldR (cdr lat)))))
      (else
        (cons (car lat)
              (multiinsertLR new oldL oldR (cdr lat)))))))

;; page 142
(define multiinsertLR&co ; count the instances of oldL and oldR
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
        (col (quote ())
             0
             0)) ; base case is a pair of numbers
      ((eq? oldL (car lat))
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons new
                       (cons oldL
                             newlat))
                 (add1 L)
                 R))))
      ((eq? oldR (car lat))
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR
                       (cons new
                             newlat))
                 L
                 (add1 R)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons (car lat)
                       newlat)
                 L
                 R)))))))

;; page 144
(define even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

;; page 144
(define evens-only*
  (lambda (l)
    (cond
      ((null? l)
        (quote ()))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (cons (car l)
                  (evens-only* (cdr l))))
          (else
            (evens-only* (cdr l)))))
      (else
        (cons (evens-only* (car l))
              (evens-only* (cdr l)))))))

;; page 145
(define evens-only*&co ; stolen from pkrumins
  (lambda (l col)
    (cond
      ((null? l)
        (col (quote ()) 1 0))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (evens-only*&co (cdr l)
              (lambda (newl p s)
                (col (cons (car l) newl)
                     (o* (car l) p)
                     s))))
          (else
            (evens-only*&co (cdr l)
              (lambda (newl p s)
                (col newl
                     p
                     (o+ (car l) s)))))))
      (else
        (evens-only*&co (car l)
          (lambda (al ap as)
            (evens-only*&co (cdr l)
              (lambda (dl dp ds)
                (col (cons al
                           dl)
                     (o* ap dp)
                     (o+ as ds))))))))))


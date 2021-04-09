(define test
  (lambda (n) ; number
    (+ n 1)))

(display (test 3))
(newline)

(define test
  (lambda (n) ; number
    (+ n 2)))

(begin
  (display (test 3)))


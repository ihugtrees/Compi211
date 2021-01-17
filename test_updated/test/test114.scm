(map (lambda (x) x) '(1 -2 3 -4.2 5 -6))

(define abs (lambda (n)
    (if (> n 0)
        n
        (* n -1))))
(map abs '(1 -2 3 -4.2 5 -6))
(map (lambda (x y) (* x y)) '(1 2 3 4) '(8 7 6 5))
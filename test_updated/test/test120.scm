(define min 
    (lambda args
        (fold-left (lambda (x y) (if (< x y)
                                        x
                                        y)) (car args) args)))
(apply min '(6 8 3 2 5))
(apply min  5 1 3 '(6 8 3 2 5))
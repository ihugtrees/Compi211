(define foo 
    (lambda (x y z . w) 
        w))
(foo 1 2 3 4 5)
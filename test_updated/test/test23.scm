(define foo 
    (lambda (x)
        (lambda (y)
            (+ x y))))
((foo 1) 2)
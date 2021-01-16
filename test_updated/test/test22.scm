(define foo 
    (lambda (x)
        (lambda (y)
            (lambda (z)
                (if x y z)))))
(((foo #t) 1) 'moshe)
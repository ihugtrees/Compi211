(define foo 
    (lambda (x y z . w) 
        (cons x (cons y (cons z w)))))
(foo 1 2 3 4 5)
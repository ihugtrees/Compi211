(define id (lambda (x) x))
(set! id (lambda (x y) y))
(id 3 4)
(+ 1 2 3 4)
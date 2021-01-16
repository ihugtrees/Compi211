(define foo
    (lambda (n)
        (+ n 1)))
(define goo
    (lambda (x)
        (foo x)))
(goo 0)
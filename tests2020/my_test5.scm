(define foo (lambda() (lambda(y) (+ 6 y))))
((foo) 3)
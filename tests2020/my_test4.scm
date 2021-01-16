(define foo (lambda() (lambda(y) (+ 3 y))))
((foo) 3)
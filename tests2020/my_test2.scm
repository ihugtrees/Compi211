(define foo (lambda(x) (lambda(y) (+ x y))))
((foo 3) 2)
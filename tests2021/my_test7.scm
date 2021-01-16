(define foo (lambda(x)
              (lambda(y) (+ x y))))
(define goo (lambda (z)
              (lambda(x) (+ 2 z))))

((foo 3) ((goo 3) 2))
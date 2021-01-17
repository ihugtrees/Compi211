(begin ((if (or #t 3) (lambda (x) x) (lambda(y) (* 2 y))) 5)
((if #f (lambda (x) x) (lambda(y) (* 2 y))) 4))

((lambda (x) (if (or x #t) x (* 2 x))) 3)
((lambda (x) (if (or x #t) x (* 2 x))) #f)
(define id (lambda (x) x))
(null? '())
(apply id '(5))
(car (cons 1 3))
(cdr (cons 1 3))
(+ 1 2)

(fold-left + 0 '(1 2 3))
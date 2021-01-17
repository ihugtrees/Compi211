(define list (lambda x x))
(define foo (lambda (x y z w)
    (list x y z w)))
(apply foo 1 'b '(c 4))
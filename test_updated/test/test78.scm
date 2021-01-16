(define foo 
    (lambda (n args)
        (if (= n 0)
            'moshe
            (begin (apply list args)
            (foo (- n 1) args))
            )))
(foo 3 '(1 2 3))
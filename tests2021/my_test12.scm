(define rest '(4 3))
((lambda (x a . y) rest) 1 2 rest)

((lambda x x) '(2 3))
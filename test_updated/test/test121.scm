(define first
  (lambda (l)
    (apply (lambda (x . y) x)
             l)))
(define rest
  (lambda (l)
    (apply (lambda (x . y) y) l)))

(first '(a b c d))
(rest '(a b c d))
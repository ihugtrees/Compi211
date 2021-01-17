(define foo 
      (lambda (x y z . w) 
          (cons (lambda vardic 
                    (cons  (cons x (cons y (cons z '()))) (cons w vardic)))
                (lambda (foo goo boo)
                    (cons goo w)))))
(let* ((a (foo 1 2 3 4 5))
       (b ((car a)))
       (c ((cdr a) 'moshe 'moshe 'moshe)))
    (if b c))
(define foo 
      (lambda (x y z . w) 
          (lambda vardic 
             (cons  (cons x (cons y (cons z '()))) (cons w vardic)))))
((foo 1 2 3 4 5) 6 7 8 9 10 11 12 13 14 15 'moshe)
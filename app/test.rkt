(define map
  (lambda (f lat)
    (if (null? lat)
      null
      (cons (f (car lat)) (map f (cdr lat))))))

(define foldr
  (lambda (rec b lat)
    (if (null? lat)
      b
      (rec (car lat) (foldr rec b (cdr lat))))))

(define foldl
  (lambda (rec b lat)
    (if (null? lat)
      b
      (foldl rec (rec b (car lat)) (cdr lat)))))

(define filter
  (lambda (f lat)
    (if (null? lat)
      null
      (if (f (car lat))
        (cons (car lat) (filter f (cdr lat)))
        (filter f (cdr lat))))))

(define add1
  (lambda (n)
    (+ 1 n)))

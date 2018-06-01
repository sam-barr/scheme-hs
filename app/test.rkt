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
    (cond
      [(null? lat) null]
      [(f (car lat)) (cons (car lat) (filter f (cdr lat)))]
      [else (filter f (cdr lat))])))

(define rember-all
  (lambda (a lat)
    (cond
      [(null? lat) null]
      [(eq? a (car lat)) (rember-all a (cdr lat))]
      [else (cons (car lat) (rember-all a (cdr lat)))])))

(define add1
  (lambda (n)
    (+ 1 n)))

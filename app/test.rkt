(define map
  (lambda (f lat)
    (if (eq? null lat)
      null
      (cons (f (car lat)) (map f (cdr lat))))))

(define add1
  (lambda (n)
    (+ 1 n)))

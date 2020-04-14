
; Tail recursion

(define (replicate x n)
   (define (helper x n result)
      (cond ((= n 0) result)
          (else (append result x))(helper x (- n 1) result)))
    (helper x n ())
  )

(define (accumulate combiner start n term)
  (if (= n 0)
      start
      (combiner (term n) (accumulate combiner start (- n 1) term)))
)

(define (accumulate-tail combiner start n term)
  (define (helper m value)
      (if (= n m)
          (combiner (term m) value)
          (helper (+ m 1) (combiner (term m) value))))
  (if (= n 0)
      start
      (helper 1 start))
)

; Streams

(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

(define multiples-of-three
  (map-stream (lambda (x) x*3))
)


(define (nondecreastream s)
    


(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))
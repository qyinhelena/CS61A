(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (cons first lst)) rests))

(define (zip pairs)
  (list (map car pairs) (map cdar pairs)))

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper s n)
    (if (null? s) nil
      (cons (list n (car s)) (helper (cdr s) (+ n 1)))))
  (helper s 0)
  )
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (cond
    ((null? denoms) '())
    ((= total 0) '(()))
    ((> (car denoms) total) (list-change total (cdr denoms)))
    (else
      (define with_car (list-change (- total (car denoms)) denoms))
      (define without_car (list-change total (cdr denoms)))
      (append (cons-all (car denoms) with_car) without_car))
  )
)
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons form (cons params (let-to-lambda body)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) (map let-to-lambda (cadr (zip values))))
           
           ;(append (cons (cons 'lambda (cons (car (zip values)) (map let-to-lambda body))) nil)
                    ;(map let-to-lambda (cadr (zip values))))
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (cons (car expr)(map let-to-lambda (cdr expr)))
         ;(map let-to-lambda expr)
         ; END PROBLEM 18
         )))

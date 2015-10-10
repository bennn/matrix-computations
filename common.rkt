#lang racket/base

(provide
  matrix
  ;; Opaque type representing matrices

  matrix?
  ;; (-> Matrix Boolean)

  matrix-dimension
  ;; (-> Matrix Index)

  list->matrix
  ;; (-> (Listof A) Matrix)
  ;; Recursively convert a list into a matrix
)

;; =============================================================================

(define-syntax-rule (raise-dimension-error M dim1 dim2)
  (error 'matrix-ref*
    (format "Matrix ~a has dimension ~a, but given ~a arguments." (object-name M) dim1 dim2)))

(define-syntax-rule (matrix-ref* M i*)
  (let ([M-d (matrix-dimension M)]
        [I-d (length i*)])
    (unless (= M-d I-d)
      (raise-dimension-error M M-d I-d))
    (for/fold ([m M])
              ([i (in-list i*)])
      (vector-ref (matrix-row m) i))))

(define (matrix-print M port mode)
  (define r (matrix-row M))
  (case mode
   [(#t) (write r port)]
   [(#f) (display r port)]))

(struct matrix (
  dimension ;; Index
  row ;; (Vectorof A)
) #:methods gen:custom-write
  [(define write-proc matrix-print)]
  #:property prop:procedure
  (lambda (M . i*)
    (matrix-ref* M i*))
)

(define (coerce-dimension v)
  (if (matrix? v)
    (matrix-dimension v)
    0))

(define (list->matrix x*)
  (define row
    (for/vector ([x (in-list x*)])
      (if (list? x)
        (list->matrix x)
        x)))
  (define dim
    (cond
     [(zero? (vector-length row))
      0]
     [else
      (define D_0 (coerce-dimension (vector-ref row 0)))
      (for ([v (in-vector row)]
            [i (in-naturals)])
        (let ([D_n (coerce-dimension v)])
          (unless (= D_0 D_n)
            (error 'list->matrix "Expected rows to have equal dimension, but row 0 has dimension ~a and row ~a has dimension ~a." D_0 i D_n))
          D_0))
      (+ 1 D_0)]))
  (matrix dim
          (vector->immutable-vector row)))

;; =============================================================================

(module+ test
  (require
    rackunit
    rackunit-abbrevs
  )

  ;; -- 
  (check-apply* matrix-dimension
   [(list->matrix '())
    == 0]
   [(list->matrix '(1 a x))
    == 1]
   [(list->matrix '(1 2 1))
    == 1]
   [(list->matrix '((1 0 0) (0 1 0) (0 0 1)))
    == 2]
   [(list->matrix '(((1 0 0) (0 1 0) (0 0 1))
                    ((1 0 0) (0 1 0) (0 0 1))))
    == 3]
   )

  ;; -- 
  (check-exn* #rx"dimension" list->matrix
   ;['(() 1)] ;; I guess this is okay
   ['(1 2 3 4 (5))]
   ['((1 2 3)
      (4 (5)))]
  )

  ;; -- accessors
  (let ([M (list->matrix '())])
    (check-equal? (M) M))

  (let ([M (list->matrix '((4 9 2)
                           (3 5 7)
                           (8 1 6)))])
    (check-apply* M
     [0 0 == 4]
     [0 1 == 9]
     [2 2 == 6]))

)

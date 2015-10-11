#lang racket/base

;; Basic algorithms and notation
;; (Chapter 1, Section 1)

(provide
  ;; --------------------------------------------------------------------------
  ;; --- 1.1.1. Matrix Notation

  matrix
  ;; Opaque type representing matrices
  ;; Access entries of a matrix M by applying it: (M i j)

  matrix?
  ;; (-> Matrix Boolean)

  dimension
  ;; (-> Matrix Index)
  ;; Depth

  cardinal
  ;; (-> Matrix Index)
  ;; Number of rows

  list->matrix
  ;; (-> (Listof A) Matrix)
  ;; Recursively convert a list into a matrix

  ;matrix->list TODO generalize this first
  ;; (-> Matrix (Listof A))

  ;; --------------------------------------------------------------------------
  ;; --- 1.1.2. Matrix Operations

  transpose
  ;; (-> Matrix Matrix)

  matrix-+
  ;; (-> Matrix Matrix Matrix)

  matrix-*
  ;; (-> (U Scalar Matrix) (U Scalar Matrix) Matrix)

  ;; ---------------------------------------------------------------------------
  ;; --- 1.1.3 TODO
)

;; =============================================================================
;; === 1.1.1.

(define-syntax-rule (raise-dimension-error M dim1 dim2)
  (error 'matrix-ref*
    (format "Matrix ~a has dimension ~a, but given ~a arguments." (object-name M) dim1 dim2)))

(define-syntax-rule (matrix-ref* M i*)
  (let ([M-d (matrix-dimension M)]
        [I-d (length i*)])
    (unless (<= I-d M-d)
      (raise-dimension-error M M-d I-d))
    (for/fold ([m M])
              ([i (in-list i*)])
      (vector-ref (matrix-row m) (sub1 i)))))

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

(define dimension matrix-dimension)
(define (cardinal M) (vector-length (matrix-row M)))

(define (coerce-dimension v)
  (if (matrix? v)
    (matrix-dimension v)
    0))

(define (coerce-length v)
  (if (matrix? v)
    (vector-length (matrix-row v))
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
      (define L_0 (coerce-length (vector-ref row 0)))
      (for ([v (in-vector row)]
            [i (in-naturals)])
        (let ([D_n (coerce-dimension v)]
              [L_n (coerce-length v)])
          (unless (= D_0 D_n)
            (error 'list->matrix "Expected rows to have equal dimension, but row 0 has dimension ~a and row ~a has dimension ~a." D_0 i D_n))
          (unless (= L_0 L_n)
            (error 'list->matrix "Expected rows to have equal length, but row 0 has length ~a and row ~a has length ~a." L_0 i L_n))
          D_0))
      (+ 1 D_0)]))
  (matrix dim
          (vector->immutable-vector row)))

;; =============================================================================
;; === 1.1.2.

(define (transpose M)
  (define D (dimension M))
  (unless (= 2 D)
    (raise-argument-error 'transpose "a 2-dimensional matrix" M))
  (define num-rows (cardinal M))
  (define num-cols (cardinal (M 1)))
  (define trans-row
    (vector->immutable-vector
      (build-vector num-cols
        (lambda (j)
          (vector->immutable-vector
            (build-vector num-rows
              (lambda (i)
                (M (+ 1 i) (+ 1 j)))))))))
  (matrix D trans-row))

(define (matrix-+ M0 M1)
  (define (plus i j)
    (let ([i+ (add1 i)] [j+ (add1 j)])
      (+ (M0 i+ j+) (M1 i+ j+))))
  (matrix-map/2 plus M0 M1 #:src 'matrix-+))

(define (assert-2D M0 M1 src)
  (define D0 (dimension M0))
  (define D1 (dimension M1))
  (unless (= 2 D0) (raise-argument-error src "a 2-dimensional matrix" M0))
  (unless (= 2 D1) (raise-argument-error src "a 2-dimensional matrix" M1)))

(define (matrix-map/2 f M0 M1 #:src [src 'matrix-map/2])
  (assert-2D M0 M1 src)
  (define r0 (cardinal M0))
  (define r1 (cardinal M1))
  (unless (= r0 r1)
    (raise-argument-error src (format "a matrix with ~a rows" r0) M1))
  (define c0 (cardinal (M0 1)))
  (define c1 (cardinal (M1 1)))
  (unless (= c0 c1)
    (raise-argument-error src (format "a matrix with ~a columns" c0) M1))
  ;; We're closed over M0 and M1, so things are good
  (matrix-map M0 f))

(define (matrix-* M0 M1)
  (define M0? (matrix? M0))
  (define M1? (matrix? M1))
  (cond
   [(and M0? M1?)
    (matrix-*/2 M0 M1)]
   [(and M0? (not M1?))
    (matrix-map M0 (lambda (i j) (* (M0 (add1 i) (add1 j)) M1)))]
   [(and (not M0?) M1?)
    (matrix-map M1 (lambda (i j) (* M0 (M1 (add1 i) (add1 j)))))]
   [else
    (raise-argument-error 'matrix-* "a matrix" M1)]))

(define (matrix-*/2 M0 M1)
  (assert-2D M0 M1 'matrix-*/2)
  (define k0 (cardinal (M0 1)))
  (define k1 (cardinal M1))
  (unless (= k0 k1)
    (raise-argument-error 'matrix-*/2 (format "a matrix with ~a rows" k0) M1))
  (define (mul i j)
    (let ([i+ (add1 i)] [j+ (add1 j)])
      (for/sum ([k+ (in-range 1 (+ 1 k0))])
        (* (M0 i+ k+) (M1 k+ j+)))))
  (define M (cardinal M0))
  (define N (cardinal (M1 1)))
  (matrix 2
    (vector->immutable-vector
      (build-vector M
        (lambda (i)
          (vector->immutable-vector
            (build-vector N
              (lambda (j)
                (mul i j)))))))))

(define (matrix-map M f)
  (define r (cardinal M))
  (define c (cardinal (M 1)))
  (matrix (dimension M)
    (vector->immutable-vector
      (build-vector r
        (lambda (i)
          (vector->immutable-vector
            (build-vector c
              (lambda (j)
                (f i j)))))))))

(define (matrix->list M)
  (define D (dimension M))
  (unless (= 2 D)
    (raise-argument-error 'matrix->list "a 2-dimensional matrix" M))
  (for/list ([v (in-vector (matrix-row M))])
    (vector->list v)))

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

  ;; -- dimension mismatch
  (check-exn* #rx"dimension" list->matrix
   ;['(() 1)] ;; I guess this is okay
   ['(1 2 3 4 (5))]
   ['((1 2 3)
      (4 (5)))]
  )

  ;; -- length mismatch
  (check-exn* #rx"length" list->matrix
   ['((1 2 3) (4))]
   ['((1) (1 2) (1 2 3))])

  ;; -- accessors
  (let ([M (list->matrix '())])
    (check-equal? (M) M))

  (let ([M (list->matrix '((4 9 2)
                           (3 5 7)
                           (8 1 6)))])
    (check-apply* M
     [1 1 == 4]
     [1 3 == 2]
     [3 1 == 8]
     [3 3 == 6]))

   ;; -- cardinal
   (check-apply* (compose1 cardinal list->matrix)
    ['() == 0]
    ['(1) == 1]
    ['(1 1) == 2]
    ['(1 1 1) == 3]
    ['((0 0 1)
       (2 4 2))
     == 2]
    ['((((deep))))
     == 1])

   ;; -- transpose
   (check-apply* (compose1 matrix->list transpose list->matrix)
    ['((1 0 0)
       (0 1 0)
       (0 0 1))
     ==
     '((1 0 0)
       (0 1 0)
       (0 0 1))]
    ['((1 2)
       (3 4))
     ==
     '((1 3)
       (2 4))]
    ['((1 2 3 4)
       (5 6 7 8))
     ==
     '((1 5)
       (2 6)
       (3 7)
       (4 8))])

  ;; -- matrix-+
  (check-apply* (lambda (L0 L1)
                  (matrix->list
                    (matrix-+ (list->matrix L0) (list->matrix L1))))
   ['((1 1 1))
    '((1 1 1))
    ==
    '((2 2 2))]
   ['((0 1 0)
      (1 0 1)
      (0 1 0))
    '((0 0 0)
      (0 5 0)
      (0 0 0))
    ==
    '((0 1 0)
      (1 5 1)
      (0 1 0))])

  ;; -- matrix-*
  (check-equal?
    (matrix->list (matrix-* (list->matrix '((1 2 3))) 2))
    '((2 4 6)))
  (check-equal?
    (matrix->list (matrix-* 2 (list->matrix '((3 6 9)))))
    '((6 12 18)))

  (check-apply* (lambda (L0 L1)
                  (matrix->list
                    (matrix-* (list->matrix L0) (list->matrix L1))))
   ['((1 0)
      (0 1))
    '((5 6)
      (7 8))
    ==
    '((5 6)
      (7 8))]
   ['((3 2 1)
      (6 6 6))
    '((2 2)
      (3 8)
      (0 4))
    ==
    '((12 26)
      (30 84))])

  (check-exn* exn:fail:contract? matrix-*
   [1 2]
   [(list->matrix '(1 2))
    (list->matrix '(3 4))]
   [(list->matrix '(1 2 3))
    (list->matrix '((1 1) (1 1)))])
)

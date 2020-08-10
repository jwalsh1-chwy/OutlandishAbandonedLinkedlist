;; Pairs
(define sh/wal/pair
  (lambda (f r)
    (lambda (op)
      (op f r))))

(define sh/wal/first
  (lambda (p)
    (p (lambda (f r)
         f))))

(define sh/wal/rest
  (lambda (p)
    (p (lambda (f r)
         r))))


;;  Numbers
(define sh/wal/gcd
  (lambda (a b)
      (cond
       ((= a b) a)
       ((> a b) (sh/wal/gcd (- a b) b))
       ((< a b) (sh/wal/gcd a (- b a))))))


;; Rational Numbers
(define sh/wal/make-rat
  (lambda (n d)
    (sh/wal/pair n d)))

(define sh/wal/numer0
  (lambda (x)
    (sh/wal/first x)))

(define sh/wal/denom0
  (lambda (x)
    (sh/wal/rest x)))

(define half (sh/wal/make-rat 1 2))

(sh/wal/numer0 half)
(sh/wal/denom0 half)


;; TODO Map
(define (sh/wal/map op sequence)
  "Reduce a list to another structure"
  (if (not (null? sequence))
      (cons (op (car sequence))
           (sh/wal/map op (cdr sequence)))))

(define sh/wal/inc
  (lambda (n)
    (+ 1 n)))

(sh/wal/map sh/wal/inc (list 1 2 3 4 5))


;; Reduce
(define (sh/wal/accumulate op initial sequence)
  "Reduce a list to another structure"
  (if (null? sequence)
      initial
      (op (car sequence)
          (sh/wal/accumulate op initial (cdr sequence)))))

(sh/wal/accumulate + 0 (list 1 2 3 4 5))


;; Types
(define twenty-four-seven (sh/wal/make-rat 120 35))

(sh/wal/gcd
 (sh/wal/numer0 twenty-four-seven)
 (sh/wal/denom0 twenty-four-seven))

(define (sh/wal/numer x)
  (let ((g (sh/wal/gcd (sh/wal/first x) (sh/wal/rest x))))
    (/ (sh/wal/first x) g)))

(define (sh/wal/denom x)
  (let ((g (sh/wal/gcd (sh/wal/first x) (sh/wal/rest x))))
    (/ (sh/wal/rest x) g)))

(sh/wal/numer twenty-four-seven)
(sh/wal/denom twenty-four-seven)


;; Sequences
(define (sh/wal/list-ref items n)
  (if (= n 0)
      (sh/wal/first items)
      (sh/wal/list-ref (sh/wal/rest items) (- n 1))))

(define squares (sh/wal/pair
                 1
                 (sh/wal/pair
                  4
                  (sh/wal/pair
                   9
                   (sh/wal/pair
                    16
                    (sh/wal/pair 25 36))))))

(sh/wal/list-ref squares 3)


;; Trees

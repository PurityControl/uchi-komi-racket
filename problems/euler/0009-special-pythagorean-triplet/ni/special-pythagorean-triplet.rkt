#lang racket

(require rackunit rackunit/text-ui)

(define (product-pythagorean-triplet number)
  (for*/first ([a (in-range 1 (+ 1 number))]
              [b (in-range 1 (+ 1 number))]
              [c (in-range 1 (+ 1 number))]
              #:when (and
                      (equal? number (+ a b c))
                      (< a b c)
                      (equal? (+ (sqr a) (sqr b)) (sqr c))))
    (* a b c )))

(define file-tests
  (test-suite
   "Tests for special pythagorean triplet"
     (check-equal? 31875000 (product-pythagorean-triplet 1000))))


(run-tests file-tests)

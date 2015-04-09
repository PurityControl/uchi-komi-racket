#lang racket

(require rackunit rackunit/text-ui)
(require math/number-theory)

(define (first-triangle-with-num-divisors num-div)
  (define (next-triangle current-triangle increment lo-triangles)
    (if (>= (num-divisors current-triangle) num-div)
        current-triangle
        (next-triangle (+ current-triangle increment)
                       (+ increment 1)
                       (cons current-triangle lo-triangles))))
  (next-triangle 1 2 '()))

(define (divisor? dividend divisor)
  (zero? (modulo dividend divisor)))

(define (num-divisors dividend)
  (length (divisors dividend)))

(define file-tests
  (test-suite
   "Tests for highly divisible triangular number"
   (check-equal? 28 (first-triangle-with-num-divisors 5))
   (check-equal? 76576500 (first-triangle-with-num-divisors 501))))

(run-tests file-tests)
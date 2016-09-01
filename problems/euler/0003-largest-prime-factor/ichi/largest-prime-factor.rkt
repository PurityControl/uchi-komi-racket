#lang racket

(require rackunit rackunit/text-ui)
(require math/number-theory)

(define (largest-prime-factor number)
  (apply max (prime-divisors number)))

(define file-tests
  (test-suite
  "Test for largest prime factor"
    (check-equal? 6857 (largest-prime-factor 600851475143))))

(run-tests file-tests)

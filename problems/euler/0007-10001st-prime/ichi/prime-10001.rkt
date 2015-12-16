#lang racket

(require rackunit rackunit/text-ui)
(require math/number-theory)

(define (prime-at-position x)
  (nth-prime (sub1 x)))

(define file-tests
  (test-suite
   "Tests for 1001st prime"
   (check-equal? 13 (prime-at-position 6))
   (check-equal? 104743 (prime-at-position 10001))))


(run-tests file-tests)

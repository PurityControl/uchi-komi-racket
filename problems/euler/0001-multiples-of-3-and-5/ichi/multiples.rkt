#lang racket

(require rackunit rackunit/text-ui)

(define (multiple-of-3-or-5? x)
  (or (zero? (modulo x 3)) (zero? (modulo x 5))))

(define (sum-multiples-to limit)
  (apply + (filter multiple-of-3-or-5? (range limit))))

(define file-tests
  (test-suite
  "Tests for multiples of 3 and 5"
    (check-equal? 233168 (sum-multiples-to 1000))))

(run-tests file-tests)

#lang racket

(require rackunit rackunit/text-ui)

(define (multiple-of? divisors)
  (lambda (x)
    (for/or ((divisor divisors)) (zero? (modulo x divisor)))))

(define (sum-multiples-to limit)
  (apply + (filter (multiple-of? '(3 5)) (range limit))))

(define file-tests
  (test-suite
  "Tests for multiples of 3 and 5"
    (check-equal? 233168 (sum-multiples-to 1000))))

(run-tests file-tests)

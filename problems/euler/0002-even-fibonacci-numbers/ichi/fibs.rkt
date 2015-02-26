#lang racket

(require rackunit rackunit/text-ui)

(define (fibs-to limit)
  (define (acc-fibs a b lo-fibs)
    (if (< limit a)
        lo-fibs
        (acc-fibs b (+ a b) (cons a lo-fibs))))
  (acc-fibs 1 2 '()))

(define (sum-even-fibs-to limit)
  (apply + (filter even? (fibs-to limit))))

(define file-tests
  (test-suite
  "Tests for sum of even fibs to 4,000,000"
    (check-equal? 4613732 (sum-even-fibs-to 4000000))))

(run-tests file-tests)

#lang racket

(require rackunit rackunit/text-ui)

(define (square-sums lonumbers)
  "square the sum of the list"
  (sqr (apply + lonumbers)))

(define (sum-squares lonumbers)
  "sum the squares of the list"
  (apply + (map sqr lonumbers)))

(define (sum-square-difference lonumbers)
  "difference between square of sums of the the list and sum the squares of the list"
  (- (square-sums lonumbers) (sum-squares lonumbers)))


(define file-tests
  (test-suite
   "Tests for sum square difference"
   (check-equal? 385 (sum-squares (sequence->list (in-range 1 11))))
   (check-equal? 3025 (square-sums (sequence->list (in-range 1 11))))
   (check-equal? 2640 (sum-square-difference (sequence->list (in-range 1 11))))
   (check-equal? 25164150 (sum-square-difference (sequence->list (in-range 1 101))))))


(run-tests file-tests)

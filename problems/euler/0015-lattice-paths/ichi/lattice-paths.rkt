#lang racket

(require rackunit rackunit/text-ui math/number-theory)

;; Integer Integer -> Integer
;; Given the x and y dimensions of a grid returns the number of distinct
;; lattice paths from top left to bottom right. X and Y must be positive
;; integers greater than zero
(define (num-lattice-paths x y)
  (/ (factorial (+ x y))
     (* (factorial x)
        (factorial y))))

(define file-tests
  (test-suite
   "test how many routes are available from the top left to bottom right of an
n x n grid"
   (check-equal? (num-lattice-paths 2 2) 6)
   (check-equal? (num-lattice-paths 20 20) 137846528820)))

(run-tests file-tests)
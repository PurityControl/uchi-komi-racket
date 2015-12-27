#lang racket

(require rackunit rackunit/text-ui "eratosthenes.rkt")

(define file-tests
  (test-suite
   "test-suite for the primes library"
   (check-equal? (primes-to 20) '(2 3 5 7 11 13 17 19))
   (check-equal?
    (primes-to 100)
    '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
   (check-equal? (length (primes-to 1000)) 168)
   (check-equal? (length (primes-to 7919)) 1000)))

(run-tests file-tests)

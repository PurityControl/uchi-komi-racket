#lang racket

(require math/number-theory)
(require rackunit rackunit/text-ui)

(define (sum-primes-to number)
  (if (equal? 2 number)
      0
      (let ([current-prime (prev-prime number)])
        (+ current-prime (sum-primes-to current-prime)))))

(define file-tests
  (test-suite
   "Tests for special pythagorean triplet"
     (check-equal? 17 (sum-primes-to 10))
     (check-equal? 142913828922 (sum-primes-to 2000000))))


(run-tests file-tests)

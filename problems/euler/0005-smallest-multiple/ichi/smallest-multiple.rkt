#lang racket

(require rackunit rackunit/text-ui)

(define (multiple? multiple lofactors)
  "returns true when all items in list are factors of multiple"
  (for/and ([x lofactors])
    (zero? (modulo multiple x))))

(define (smallest-multiple lofactors)
  "returns smallest multiple for the list of factors"
  (define (smallest-multiple multiple lofactors)
    (if (multiple? multiple lofactors)
        multiple
        (smallest-multiple (+ multiple (apply max lofactors)) lofactors)))
  (smallest-multiple (apply max lofactors) lofactors))

(define file-tests
  (test-suite 
   "Tests for smallest-multiple"
   (check-true (multiple? 2520 (in-range 1 10)))
   (check-false (multiple? 2521 (in-range 1 11)))
   (check-equal? 2520 (smallest-multiple (sequence->list (in-range 1 11))))
   (check-equal? 232792560 (smallest-multiple (sequence->list (in-range 1 21))))))


(run-tests file-tests)
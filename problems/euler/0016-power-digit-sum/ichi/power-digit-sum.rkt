#lang racket

(require rackunit rackunit/text-ui)

(define (sum-of-digits lo-nums)
  (apply + lo-nums))

(define (number->list num)
  (map string->number 
              (filter-not (lambda (x) (string=? "" x)) 
                          (string-split (number->string num) ""))))

(define file-tests
  (test-suite
   "test-suite for power digit sum"
   (check-equal? (sum-of-digits (number->list (expt 2 1000))) 1366)))

(run-tests file-tests)



 
#lang racket

(require rackunit rackunit/text-ui)

(define (palindrome? number)
  "true if a number is a palindrome"
  (string=?
   (list->string (reverse (string->list (number->string number))))
   (number->string number)))

(define (palindrome-products start end)
  "filters products of numbers from start to end that are palindromes"
  (for*/list ([pr1 (in-range start (+ 1 end))]
              [pr2 (in-range start (+ 1 end))]
              #:when (palindrome? (* pr1 pr2)))
    (* pr1 pr2)))

(define (max-palindrome-product start end)
  (apply max (palindrome-products start end)))

(define file-tests
  (test-suite 
   "Tests for palindrome product"
   (check-true (palindrome? 12321))
   (check-false (palindrome? 123))
   (check-eq? 906609 (max-palindrome-product 100 999))))

(run-tests file-tests)
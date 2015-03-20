#lang racket

(require rackunit rackunit/text-ui)

(define (list-triplets-totalling number)
  (for*/list ([a (in-range 1 (+ 1 number))]
              [b (in-range 1 (+ 1 number))]
              [c (in-range 1 (+ 1 number))]
              #:when (equal? number (+ a b c)))
    (list a b c )))

(define (filter-pythagorean lotriplets)
  (define (pythagorean? triplet)
    (equal? (sqr (first triplet)) (+ (sqr (second triplet)) (sqr (third triplet)))))
  (filter pythagorean? lotriplets))

(define (product-pythagorean-triplet number)
  "returns all product of pythagorean triplets totalling number"
  (map (lambda (x) (apply * x)) (filter-pythagorean (list-triplets-totalling number))))


(define file-tests
  (test-suite
   "Tests for special pythagorean triplet"
     (check-equal? 31875000 (car (product-pythagorean-triplet 1000)))))


(run-tests file-tests)

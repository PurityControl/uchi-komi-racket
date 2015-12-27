#lang racket

(provide primes-to)

;; Number -> List Of Number
;; Given a limit returns all the prime numbers up to that limit
;; performant up to about 1,000,000 entries
(define (primes-to limit)
  (cond
    [(= limit 2) '(2)]
    [(= limit 3) '(2 3)]
    [else
     (eratosthenes limit)]))

;; Number -> List Of Number
;; Populates the list of all numbers prior to sieving
;; primes greater that 3 must have the form 6k +/- 1
;; so populates with only these numbers
(define (fill-sieve limit)
  (local [(define (fill-sieve limit k acc)
           (cond
             [(> (sub1 (* 6 k)) limit) acc]
             [(> (add1 (* 6 k)) limit) (cons (sub1 (* 6 k)) acc)]
             [else
              (fill-sieve limit
                          (add1 k)
                          (cons (add1 (* 6 k)) (cons (sub1 (* 6 k)) acc)))]))]
    (reverse (fill-sieve limit 1 '()))))

;; Number -> List Of Number
;; Given a limit sieves out all non primes
(define (eratosthenes limit)
  (local [(define (eratosthenes limit primes sieve)
            (cond
              [(> (car sieve) (sqrt limit)) (append (reverse primes) sieve)]
              [else
               (eratosthenes
                limit
                (cons (car sieve) primes)
                (filter
                 (lambda (x) (not(zero? (modulo x (car sieve)))))
                 (cdr sieve)))]))]
    (eratosthenes limit '(3 2) (fill-sieve limit))))
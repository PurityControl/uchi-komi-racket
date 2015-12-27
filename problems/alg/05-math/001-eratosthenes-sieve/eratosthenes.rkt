#lang racket

(provide primes-to)

(define (primes-to limit)
  (cond
    [(= limit 2) '(2)]
    [(= limit 3) '(2 3)]
    [else
     (eratosthenes limit)]))

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
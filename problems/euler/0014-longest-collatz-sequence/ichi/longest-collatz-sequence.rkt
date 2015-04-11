#lang racket

(require rackunit rackunit/text-ui)

(define (collatz-sequence start)
  (define (collatz-accumulater number acc)
    (cond
      [(equal? 1 number) (cons number acc)]
      [(even? number) (collatz-accumulater (/ number 2) (cons number acc))]
      [else (collatz-accumulater (add1 (* 3 number)) (cons number acc))]))
  (reverse (collatz-accumulater start '())))

(define (collatz-sequence-length start)
  (length (collatz-sequence start)))

(define (collatz-length-number-alist start end)
  (map (lambda (x) (cons (collatz-sequence-length x) x)) (range start end)))

(define (collatz-number-with-largest-length-to end)
  (define seq (collatz-length-number-alist 1 end))
  (define largest (apply max (map car seq)))
  (cdr (assoc largest seq)))

(define file-tests
  (test-suite
   "test-suite for the longast collatz sequence up to 1,000,0000"
   (check-equal? (collatz-sequence-length 13) 10)
   (check-equal? (collatz-sequence 13) '(13 40 20 10 5 16 8 4 2 1))))

(run-tests file-tests)


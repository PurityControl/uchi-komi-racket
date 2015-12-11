#lang racket

(require rackunit rackunit/text-ui)

(define num-hash (make-hash))
(define num-magnitude (make-hash))

(hash-set! num-hash 1 'one)
(hash-set! num-hash 2 'two)
(hash-set! num-hash 3 'three)
(hash-set! num-hash 4 'four)
(hash-set! num-hash 5 'five)
(hash-set! num-hash 6 'six)
(hash-set! num-hash 7 'seven)
(hash-set! num-hash 8 'eight)
(hash-set! num-hash 9 'nine)
(hash-set! num-hash 10 'ten)
(hash-set! num-hash 11 'eleven)
(hash-set! num-hash 12 'twelve)
(hash-set! num-hash 13 'thirteen)
(hash-set! num-hash 14 'fourteen)
(hash-set! num-hash 15 'fifteen)
(hash-set! num-hash 16 'sixteen)
(hash-set! num-hash 17 'seventeen)
(hash-set! num-hash 18 'eighteen)
(hash-set! num-hash 19 'nineteen)
(hash-set! num-hash 20 'twenty)
(hash-set! num-hash 30 'thirty)
(hash-set! num-hash 40 'forty)
(hash-set! num-hash 50 'fifty)
(hash-set! num-hash 60 'sixty)
(hash-set! num-hash 70 'seventy)
(hash-set! num-hash 80 'eighty)
(hash-set! num-hash 90 'ninety)

(hash-set! num-magnitude 3 'hundred)
(hash-set! num-magnitude 4 'thousand)

(define (has-magnitude? magnitude number)
  (>= (quotient number (expt 10 (sub1 magnitude))) 1))

(define (magnitude-quotient magnitude number)
  (quotient number (expt 10 (sub1 magnitude))))

(define (and-required? number)
  (and (> number 100)
       (not (zero? (modulo number 100)))))

(define (number-letter-count number)
  (apply + (map (compose string-length symbol->string) (number-to-words number))))

(define (rm-largest-mag number)
  (string->number (substring (number->string number) 1)))

(define (two-digits->word digits)
  (cond
    ((or (<= digits 20) 
         (zero? (modulo digits 10))) (hash-ref num-hash digits))
    (#t (list (hash-ref num-hash (rm-largest-mag digits))
              (hash-ref num-hash (* (quotient digits 10) 10))))))
  
(define (number-to-words number)
  (define (word-collector num-part lowords)
    (cond
      ((has-magnitude? 4 num-part) (word-collector
                                    (rm-largest-mag num-part)
                                    (flatten (list (hash-ref num-magnitude 4)
                                                   (hash-ref num-hash (magnitude-quotient 4 num-part))
                                                   lowords))))
      ((has-magnitude? 3 num-part) (word-collector
                                    (rm-largest-mag num-part)
                                    (flatten (list (hash-ref num-magnitude 3)
                                                   (hash-ref num-hash (magnitude-quotient 3 num-part))
                                                   lowords))))
      ((or (has-magnitude? 2 num-part)
           (has-magnitude? 1 num-part)) (word-collector
                                         0
                                         (flatten (list (two-digits->word num-part)
                                                        (if (and-required? number) 'and '())
                                                        lowords))))
      (#t (reverse lowords))))
  (word-collector number '()))

(define file-tests
  (test-suite
   "test-suite for the number letter counts to one thousand"
   (check-equal? (number-letter-count 1) 3)
   (check-equal? (number-letter-count 9) 4)
   (check-false (and-required? 100))
   (check-false (and-required? 99))
   (check-false (and-required? 200))
   (check-false (and-required? 1000))
   (check-true (and-required? 101))
   (check-true (and-required? 222))
   (check-true (has-magnitude? 4 1000))
   (check-true (has-magnitude? 3 999))
   (check-true (has-magnitude? 2 99))
   (check-true (has-magnitude? 1 9))
   (check-false (has-magnitude? 5 1000))
   (check-false (has-magnitude? 4 999))
   (check-false (has-magnitude? 3 99))
   (check-false (has-magnitude? 2 9))
   (check-equal? (apply + (map number-letter-count (range 1 6))) 19)
   (check-equal? (apply + (map number-letter-count (range 1 1001))) 21124)))

(run-tests file-tests)

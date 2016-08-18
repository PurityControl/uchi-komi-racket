#lang racket

(define days-in-months
  ; january 1901 has a 2 extra days from the last Sunday
  (lambda (year)
    (cond
      [(= year 1901) '(33 28 31 30 31 30 31 31 30 31 30 31)]
      [(leap? year)  '(31 29 31 30 31 30 31 31 30 31 30 31)]
      [ else         '(31 28 31 30 31 30 31 31 30 31 30 31)])))


(define (leap? year)
  (if (zero? (modulo year 4))
      (or (zero? (modulo year 400))
          (not (zero? (modulo year 100))))
      false))

(define all-days
  (apply append
         (map days-in-months
              (sequence->list (in-range 1901 2001)))))

(define cumulative-days
  (lambda (lod total)
    (cond
      [(null? lod) '()]
      [else
       (cons (+ (car lod) total)
             (cumulative-days (cdr lod) (+ total (car lod))))])))


(define (sunday? day_count)
  (zero? (modulo day_count 7)))

(count sunday? (cumulative-days all-days 0))

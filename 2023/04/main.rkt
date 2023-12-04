#lang racket

(define (numbers string)
  (map string->number
       (regexp-match* #rx"[0-9]+" string)))

(define (winning-numbers str)
  (numbers (second (string-split (first (string-split str "|")) ":"))))

(define (my-numbers str)
  (numbers (last (string-split str "|"))))

(define (my-winning-numbers wns mns)
  (filter (λ (mn) (member mn wns)) mns))

(define (score card)
  (let ([matches
         (length
          (my-winning-numbers
           (winning-numbers card)
           (my-numbers card)))])
    (if (> matches 0)
        (expt 2 (sub1 matches)) 0)))

(define input (file->lines "input.txt"))

(foldl
 (λ (line result)
   (+ result (score line)))
 0
 input)

#lang racket

(define (as-range pair)
  (apply
   inclusive-range
   (map
    string->number (string-split pair "-"))))

(define (pairs line) (map as-range (string-split line ",")))

(define (fully-contained? sets)
  (let ([intersection (sort (apply set-intersect sets) <)])
    (or
     (equal? intersection (first sets))
     (equal? intersection (last sets)))))

; part 1
; (length (filter fully-contained? (map pairs (file->lines "input.txt"))))

; part 2
(define (overlaps? sets)
  (not (empty? (apply set-intersect sets))))

(length (filter overlaps? (map pairs (file->lines "input.txt"))))

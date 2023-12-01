#lang racket

(define input
  (map
   (λ (row)
     (map string->number (map string (string->list row))))
   (file->lines "input.txt")))

(define num-rows (length input))

(define num-cols (length (first input)))

(define (top col row)
  (reverse
   (map
    (λ (x)
      (list-ref (list-ref input x) col)) (range row))))

(define (bottom col row)
  (map
   (λ (x)
     (list-ref (list-ref input x) col)) (range (+ row 1) num-rows)))

(define (left col row)
  (reverse (take (list-ref input row) col)))

(define (right col row)
  (drop (list-ref input row) (+ col 1)))

(define (is-visible? x xs)
  (or
   (empty? xs)
   (foldl
    (λ (el result)
      (and result (< el x))) #t xs)))

(define (walk-grid proc)
  (map
   (λ (row)
     (map
      (λ (col)
        (let
            ([current (list-ref (list-ref input row) col)])
          (proc current col row)))
      (range num-cols)))
   (range num-rows)))

; part 1
(define visible-trees
  (walk-grid
   (λ (current col row)
     (or
      (is-visible? current (right col row))
      (is-visible? current (left col row))
      (is-visible? current (bottom col row))
      (is-visible? current (top col row)))
     )))

(length (filter-not false? (flatten visible-trees)))

; part 2
(define (viewing-distance x lst)
  (let ([index (index-where lst (λ (y) (>= y x)))])
    (if index
        (+ 1 index)
        (length lst))))

(define (scenic-score current col row)
  (*
   (viewing-distance current (right col row))
   (viewing-distance current (left col row))
   (viewing-distance current (top col row))
   (viewing-distance current (bottom col row))))

(first
 (sort
  (flatten
   (walk-grid
    (λ (current col row)
      (scenic-score current col row))
    )) >))

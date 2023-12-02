#lang racket

(define input (file->lines "input.txt"))

(define bag
  (hash
   "red" 12
   "green" 13
   "blue" 14))

(define (every? proc lst)
  (foldl
   (λ (x result)
     (and result (proc x)))
   #t
   lst))

(define (round-possible? round)
  (every?
   (λ (cubes)
     (let
         ([parts (string-split (string-trim cubes))])
       parts
       (<=
        (string->number (first parts))
        (hash-ref bag (last parts)))))
   (string-split round ",")))

(define (game-possible? game)
  (every? round-possible? (string-split game ";")))

(define (game-id line)
  (string->number
   (last
    (string-split
     (first
      (string-split line ":"))))))

(define (rounds line)
  (last (string-split line ":")))

; part 1
(foldl
 +
 0
 (filter-map
  (λ (line)
    (and
     (game-possible? (rounds line))
     (game-id line)))
  input))

; part 2
(define (max-cubes rounds)
  (foldl
   (λ (round maxes)
     (foldl
      (λ (x cubes)
        (let ([parts (string-split x)])
          (let ([n (string->number (first parts))]
                [color (last parts)])
            (if (< (hash-ref cubes color) n)
                (hash-set cubes color n)
                cubes))))
      maxes
      (string-split round ",")))
   (hash "red" 0 "blue" 0 "green" 0)
   (string-split rounds ";")))

(define (power cubes)
  (foldl
   (λ (x result)
     (* result (cdr x)))
   1
   (hash->list cubes)))

(foldl
 (λ (line result)
   (+ (power (max-cubes (rounds line)))
      result)) 0 input)

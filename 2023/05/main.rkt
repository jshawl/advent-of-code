#lang racket

(define input (string-split (file->string "input.txt") "\n\n"))

(define (string->numbers str)
  (map string->number (string-split str " ")))

(define seeds
  (map string->number
       (drop (string-split (first input) " ") 1)))

(define maps
  (map
   (λ (x)
     (map string->numbers (drop (string-split x "\n") 1)))
   (drop input 1)))

(define (lookup map-line key)
  (let ([diff (- key (second map-line))])
    (if (or (>= diff (last map-line)) (< diff 0)) #f (+ (first map-line) diff))))

(define (lookup-ranges ranges key)
  (or
   (foldl
    (λ (x result)
      (or result (lookup x key)))
    #f
    ranges)
   key))

(define (lookup-maps maps key)
  (foldl
   (λ (x result)
     (println (list "lookup" key "in" x ":" (lookup-ranges x (or result key))))
     (lookup-ranges x (or result key)))
   #f
   maps))

(apply
 min
 (map
  (λ (seed)
    (lookup-maps maps seed))
  seeds))

#lang racket

(define input (file->lines "input.txt"))

(define moves
  (hash
   "R" '(1 0)
   "U" '(0 -1)
   "L" '(-1 0)
   "D" '(0 1)))

(define rope
  (hash
   "H" '(0 0)
   "T" '(0 0)))

(define (next a b [greedy? #f])
  (let ([diff (- a b)])
    (if (< (abs diff) (if greedy? 1 2))
        b
        (if (< diff 0)
            (- b 1)
            (+ b 1)))))

(define (follow h t)
  (let ([hx (first h)]
        [hy (last h)]
        [tx (first t)]
        [ty (last t)])
    (list
     (next hx tx (> (abs (- hy ty)) 1))
     (next hy ty (> (abs (- hx tx)) 1)))))

(define (move-head a b)
  (list
   (+ (first a) (first b))
   (+ (last a) (last b))))

(define (move direction count init)
  (foldl
   (λ (_ state)
     (let ([h (move-head
               (hash-ref (hash-ref state 'rope) "H")
               (hash-ref moves direction))])
       (let ([t (follow h (hash-ref (hash-ref state 'rope) "T"))])
         (hash
          'rope (hash "H" h "T" t)
          'tail-positions (append
                           (hash-ref state 'tail-positions)
                           (list t))))))
   init
   (range 1 (add1 count))))

(define all-moves
  (foldl
   (λ (line result)
     (let ([parts (string-split line)])
       (move
        (first parts)
        (string->number (last parts))
        result)))
   (hash 'rope rope 'tail-positions '())
   input))

; part 1
(println "should be 5619")
(length (remove-duplicates (hash-ref all-moves 'tail-positions)))

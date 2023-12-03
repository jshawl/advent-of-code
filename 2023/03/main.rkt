#lang racket

(define input
  (map
   (λ(x)
     (map string (string->list x)))
   (file->lines "input.txt")))

(define max-row (sub1 (length input)))

(define max-col (sub1 (length (first input))))

(define (reduce-grid proc init lst)
  (foldl
   (λ(x row result)
     (foldl
      (λ(cur col r)
        (proc row col cur r))
      result
      x
      (range (length x))))
   init
   lst
   (range (length lst))))

(define adjacencies
  (list
   '(0 -1)  ; top
   '(1 -1)  ; top-right
   '(1 0)   ; right
   '(1 1)   ; bottom-right
   '(0 1)   ; bottom
   '(-1 1)  ; bottom-left
   '(-1 0)  ; left
   '(-1 -1))) ; top-left

(define (next-pos a b)
  (list
   (+ (first a) (first b))
   (+ (last a) (last b))))

(define (grid-ref col row)
  (if (or (> col max-col) (> row max-row) (< col 0) (< row 0))
      ""
      (list-ref (list-ref input row) col)))

(define (is-symbol? str) (equal? str "*"))

(define (has-adjacent-symbol? col row)
  (foldl
   (λ (x result)
     (or
      result
      (if
       (is-symbol?
        (apply grid-ref (next-pos (list col row) x)))
       (next-pos (list col row) x)
       #f)))
   #f
   adjacencies))

(define (hash-append hsh key value)
  (let ([key-exists? (hash-has-key? hsh key)])
    (hash-set
     hsh
     key
     (if key-exists?
         (append (hash-ref hsh key) (list value))
         (list value)))))

(define
  reduced
  (reduce-grid
   (λ (row col cur all)
     (let ([number (regexp-match #rx"[0-9]" cur)]
           [scanned (hash-ref all 'scanned)]
           [valid-numbers (hash-ref all 'valid-numbers)]
           [valid? (or (hash-ref all 'valid) (has-adjacent-symbol? col row))])
       (if number
           (hash 'scanned (append scanned number) 'valid valid? 'valid-numbers valid-numbers)
           (hash
            'scanned '() ; reset
            'valid #f ; reset
            'valid-numbers (if (empty? scanned)
                               valid-numbers
                               (if (hash-ref all 'valid) ; previous valid, not current
                                   (hash-append valid-numbers valid? (string->number (string-join scanned "")))
                                   valid-numbers)
                               )))
       ))
   (hash 'scanned '() 'valid-numbers (hash) 'valid #f)
   input))

(foldl
 +
 0
 (filter-map
  (λ (x)
    (and
     (eq? 2 (length (drop x 1)))
     (apply * (drop x 1))))
  (hash->list (hash-ref reduced 'valid-numbers))))

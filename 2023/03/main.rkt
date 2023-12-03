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
     ;  (println (list "row result:" result))
     (foldl
      (λ(cur col r)
        ; (println (list "col result:" r))
        (proc row col cur r))
      ; (println (list row col cur result))
      ; result)
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
   '(-1 -1) ; top-left
   ))

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
      (is-symbol? (apply grid-ref (next-pos (list col row) x)))))
   #f
   adjacencies))

; (println "check 2 0:")
; (has-adjacent-symbol? 2 0)

(define
  reduced
  (reduce-grid
   (λ(row col cur all)
     (println (list "checking" cur col row))
     (let ([number (regexp-match #rx"[0-9]" cur)]
           [scanned (hash-ref all 'scanned)]
           [valid-numbers (hash-ref all 'valid-numbers)]
           [valid? (or (hash-ref all 'valid) (has-adjacent-symbol? col row))])
       (println (list cur "valid?" valid?))
       (if number
           (hash 'scanned (append scanned number) 'valid valid? 'valid-numbers valid-numbers)
           (hash
            'scanned '() ; reset
            'valid #f ; reset
            'valid-numbers (if (empty? scanned)
                               valid-numbers
                               (if (hash-ref all 'valid) ; previous valid, not current
                                   (append valid-numbers (list (string->number (string-join scanned ""))))
                                   valid-numbers)
                               )))
       ))
   (hash 'scanned '() 'valid-numbers '() 'valid #f)
   input))

; (foldl + 0 (hash-ref reduced 'valid-numbers))
(hash-ref reduced 'valid-numbers)

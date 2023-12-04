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

(define (matches card)
  (length
   (my-winning-numbers
    (winning-numbers card)
    (my-numbers card))))

(define (score card)
  (let ([mtchs (matches card)])
    (if (> mtchs 0)
        (expt 2 (sub1 mtchs)) 0)))

(define input (file->lines "input.txt"))

; part 1
(foldl
 (λ (line result)
   (+ result (score line)))
 0
 input)

; part 2
(define (make-hash lst n)
  (foldl (λ (x result) (hash-set result x n)) (hash) lst))

(define (hash-increment hsh counts)
  (foldl
   (λ (x result)
     (let ([key (car x)]
           [value (cdr x)])
       (if (hash-has-key? result key)
           (hash-set result key (+ (hash-ref result key) value))
           (hash-set result key value))))
   hsh
   (hash->list counts)))

(define counts
  (foldl
   (λ (card i result)
     (let ([copies (range (+ i 2) (+ i (matches card) 2))])
       (let ([next (hash-increment result (hash (add1 i) 1))])
         (hash-increment next (make-hash copies (hash-ref next (+ i 1)))))))
   (hash)
   input
   (range (length input))))

(foldl
 (λ (x result)
   (+ result (cdr x)))
 0
 (hash->list counts))

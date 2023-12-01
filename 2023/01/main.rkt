#lang racket

(define input (file->lines "input.txt"))

(define (non-numeric str) (not (string->number str)))

; part 1
; (foldl
;  +
;  0
;  (map
;   (λ (line)
;     (let ([chars (map string (string->list line))])
;       (string->number
;        (string-join
;         (list
;          (first (dropf chars non-numeric))
;          (last (dropf-right chars non-numeric)))
;         "")))
;     ) input))

; part 2
(define numbers
  (map
   symbol->string
   '(one two three four five six seven eight nine)))

(define (replace-nums str)
  (foldl
   (λ (x i result)
     (string-replace result x (number->string (+ i 1))))
   str
   numbers
   (range (length numbers))))

(define (unspell str [reversed? #f])
  (let ([proc (if reversed? foldr foldl)])
    (proc
     (λ (x unspelled)
       (replace-nums
        (if reversed?
            (string-append x unspelled)
            (string-append unspelled x))))
     ""
     (map string (string->list str)))))

(define (get-digit str [reversed? #f])
  (let ([taker (if reversed? last first)]
        [dropper (if reversed? dropf-right dropf)])
    (taker
     (dropper
      (map
       string
       (string->list (unspell str reversed?)))
      non-numeric))))

(foldl
 +
 0
 (map
  (λ (line)
    (string->number
     (string-join
      (list
       (get-digit line)
       (get-digit line #t))
      ""))
    ) input))

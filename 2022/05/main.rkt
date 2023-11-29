#lang racket

(define input (string-split (file->string "input.txt") "\n\n"))
(define ascii-state (first input))
(define instructions (string-split (last input) "\n"))
(define columns
  (floor (/ (string-length (last (string-split ascii-state "\n"))) 3)))
(define boxes (drop (reverse (string-split ascii-state "\n")) 1))

(define (ascii-to-list line)
  (map
   (lambda (x)
     (if (< (string-length line) (* x 4)) " "
         (substring line (+ (* x 4) 1) (+ (* x 4) 2))))
   (range columns)))

(define (zip all)
  (apply map list (apply list all)))

(define (filter-empty lst)
  (filter-not
   (lambda (x)
     (equal? x " ")) lst))

(define state (map filter-empty (zip (map ascii-to-list boxes))))

(define (map-index f xs)
  (map f xs (range (length xs))))

(define (move instruction one-by-one? state)
  (let ([instructions (map string->number (regexp-match* #rx"[0-9]+" instruction))])
    (let ([count (first instructions)]
          [from (- (second instructions) 1)]
          [to (- (third instructions) 1)])
      (map-index (lambda (col index)
                   (cond
                     [(eq? index from) (drop-right col count)]
                     [(eq? index to)
                      (append col (let ([picked-up (take-right (list-ref state from) count)]) (if one-by-one? (reverse picked-up) picked-up)))]
                     [else col])
                   ) state))))

(define (last-safe lst)
  (if (empty? lst) #f (last lst)))

(define (tops state [one-by-one? #t])
  (string-join
   (filter string?
           (map
            last-safe
            (foldl
             (lambda (instruction result)
               (move instruction one-by-one? result))
             state instructions))) ""))

; part 1
(tops state)

; part 2
(tops state #f)

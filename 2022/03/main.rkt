#lang racket

(define (split string)
  (let ([length (/ (length (string->list string)) 2)])
    (list
     (substring string 0 length)
     (substring string length (* length 2)))))

(define (item-type rucksacks)
  (first
   (apply set-intersect rucksacks)))

(define (priority c)
  (+
   (index-of
    (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") c)
   1))

(define (line-priority line)
  (priority
   (item-type
    (map string->list (split line)))))

; part 1
; (foldl + 0 (map line-priority (file->lines "input.txt")))

(define (group-n n lst [all '()])
  (if
   (empty? lst)
   all
   (group-n
    n
    (if (< (length lst) n) '() (drop lst n))
    (append all (list (if (< (length lst) n) lst (take lst n)))))))

(define (badge-item-type lines)
  (priority (first (apply set-intersect (map string->list lines)))))

; part 2
(foldl + 0 (map badge-item-type (group-n 3 (file->lines "input.txt"))))

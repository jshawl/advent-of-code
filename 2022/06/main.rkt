#lang racket

(define marker-size 4)
(define input (first (file->lines "input.txt")))

(define (group-by-n str size)
  (map
   (lambda (i) (substring str i (+ i size)))
   (range (- (string-length str) (- size 1)))))

(define (all-different? str)
  (eq?
   (set-count (list->set (string->list str)))
   (string-length str)))

(define (part-one)
  (+
   (index-where (group-by-n input marker-size) all-different?)
   marker-size))

(part-one)

(define (part-two)
  (+
   (index-where (group-by-n input 14) all-different?)
   14))

(part-two)

#lang racket

(define (group-by-sum line sums)
  (if (equal? line "")
      (append sums '(0))
      (list-set sums
                (- (length sums) 1)
                (+ (last sums) (string->number line)))))

(define calories (foldl group-by-sum
                        (list 0)
                        (file->lines "input.txt")))

(define part-1
  (first (sort calories >)))

(define part-2
  (foldl
   + 0 (take (sort calories >) 3)))

(println part-1)
(println part-2)

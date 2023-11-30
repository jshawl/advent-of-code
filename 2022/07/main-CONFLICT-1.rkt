#lang racket

(define input (file->lines "input.txt"))

(define (set-cwd x result)
  (let ([directory (regexp-match #rx"cd (.*)" x)])
    (if directory
        (if (equal? (last directory) "..")
            (hash-set result 'cwd (drop-right (hash-ref result 'cwd) 1))
            (hash-set
             (hash-set result 'cwd (append (hash-ref result 'cwd) (list (last directory))))
             (string-join (append (hash-ref result 'cwd) (list (last directory))) "/")
             0))
        result)))

(define (set-files x result)
  (let ([f (regexp-match #rx"[0-9]+" x)])
    (if f
        (foldl
         (λ (dir out)
           (hash-update out (string-join (hash-ref result 'cwd) "/") (λ (x) (+ x (string->number (first f))))))
         result (hash-ref result 'cwd))
        result)))

(define (reducer x result)
  (set-cwd x result))

; (foldl + 0 (filter (λ (x) (and (number? x) (< x 100000))) (flatten (hash->list (foldl reducer (hash 'cwd '()) input)))))

(foldl reducer (hash 'cwd '()) input)

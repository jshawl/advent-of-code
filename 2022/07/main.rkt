#lang racket

(define input (file->lines "input.txt"))

(define (stack lst [sep ""])
  (foldl
   (λ (i result)
     (append
      result
      (list (string-join (take lst (+ i 1)) sep))))
   '()
   (range (length lst))))

(define (add-file-to-paths result f cwd)
  (foldl
   (λ (i r)
     (let ([path (last (stack (take cwd (+ i 1)) "/"))]
           [size (string->number f)])
       (if (hash-has-key? r path)
           (hash-set r path (append (hash-ref r path) (list size)))
           (hash-set r path (list size)))))
   result
   (range (length cwd))))

(define filesystem
  (hash-remove
   (foldl
    (λ (x result)
      (let ([cwd (hash-ref result 'cwd)]
            [directory (regexp-match #rx"cd (.*)" x)]
            [f (regexp-match #rx"[0-9]+" x)])
        (cond [directory
               (if (equal? (last directory) "..")
                   (hash-set result 'cwd (drop-right cwd 1))
                   (hash-set result 'cwd (append cwd (list (last directory)))))]
              [f (add-file-to-paths result (last f) cwd)]
              [else result])))
    (hash 'cwd '())
    input)
   'cwd))

(define (less-than-100k? n) (< n 100000))

(define (sum lst) (foldl + 0 lst))

; part 1
(sum
 (filter
  less-than-100k?
  (hash-map
   filesystem
   (λ (k v)
     (sum v)))))

; part 2
(define unused (- 70000000 (sum (hash-ref filesystem "/"))))

(define required (- 30000000 unused))

(first
 (sort
  (filter
   (λ (k)
     (> k required))
   (hash-map
    filesystem
    (λ (k v)
      (sum v))))
  <))

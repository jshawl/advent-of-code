#lang racket

(define input (file->lines "input.txt"))

(define cycles
  (foldl
   (λ (x result)
     (let
         ([add (regexp-match #rx"addx (.*)" x)]
          [register (last result)])
       (cond
         [add (append result (list register) (list (+ (string->number (last add)) register)))]
         [else (append result (list register))])
       )) '(1) input))

(define (signal-strength cycle)
  (* cycle (list-ref cycles (- cycle 1))))

; part 1
(+
 (signal-strength 20)
 (signal-strength 60)
 (signal-strength 100)
 (signal-strength 140)
 (signal-strength 180)
 (signal-strength 220))

; part 2
(define (group-n n lst [all '()])
  (if
   (empty? lst)
   all
   (group-n
    n
    (if (< (length lst) n) '() (drop lst n))
    (append all (list (if (< (length lst) n) lst (take lst n)))))))

(define crt
  (map
   (λ (x i)
     (if
      (member (modulo (+ i 1) 40) (range x (+ x 3)))
      "#"
      "."))
   cycles
   (range (length cycles))))

(map (λ (x) (println (string-join x ""))) (group-n 40 crt))

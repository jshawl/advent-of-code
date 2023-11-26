#lang racket

(define rules (hash
               'A 'Z
               'B 'X
               'C 'Y
               'X 'C
               'Y 'A
               'Z 'B))

(define inverse-rules
  (make-hash
   (hash-map rules
             (λ (x y) (cons y x)))))

(define (get-winner theirs mine)
  (cond
    [(equal? (hash-ref rules theirs) mine) theirs]
    [(equal? (hash-ref rules mine) theirs) mine]))

(define effort
  (hash
   'X 1
   'Y 2
   'Z 3))

(define (win-score theirs mine)
  (cond
    [(equal? mine (get-winner theirs mine)) 6]
    [(equal? theirs (get-winner theirs mine)) 0]
    [else 3]))

(define (score theirs mine)
  (+ (win-score theirs mine) (hash-ref effort mine)))

; (define (score-line line)
;   (score
;    (string->symbol (substring line 0 1))
;    (string->symbol (substring line 2 3))))

; part 1
; (foldl + 0 (map score-line (file->lines "input.txt")))

(define (infer-move theirs win-lose-draw)
  (cond
    [(equal? win-lose-draw 'X) (hash-ref rules theirs)]
    [(equal? win-lose-draw 'Y) (as-mine theirs)]
    [(equal? win-lose-draw 'Z) (hash-ref inverse-rules theirs)]))

(define (as-mine theirs)
  (hash-ref
   (hash
    'A 'X
    'B 'Y
    'C 'Z)
   theirs))

(infer-move 'C 'X)

(define (score-line line)
  (let ([theirs (string->symbol (substring line 0 1))])
    (score
     theirs
     (infer-move theirs (string->symbol (substring line 2 3))))))

(foldl + 0 (map score-line (file->lines "input.txt")))

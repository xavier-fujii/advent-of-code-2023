#lang racket

(define lines
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ;   (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))

(define instructions (first lines))
(define nodes-paths (drop lines 1))

(define paths-hash
  (foldl
   (lambda (node-path result-hash)
     (let ([from (substring node-path 0 3)]
           [left (substring node-path 7 10)]
           [right (substring node-path 12 15)])
       (hash-set result-hash from (cons left right))))
   (hash)
   nodes-paths))

; ------ part one ------
(define (run1 current-node step)
  (letrec
      ([instruction (string-ref instructions (remainder step (string-length instructions)))]
       [paths (hash-ref paths-hash current-node)]
       [next-node (if (eq? instruction #\L) (car paths) (cdr paths))])
    ; (display next-node)
    ; (display "\n")
    (if
     (or (string=? next-node "ZZZ"))
     (+ 1 step)
     (run1 next-node (+ 1 step)))
    ))

; (define part-one-ans (run "AAA" 0))
; (display part-one-ans)
; (display "\n")
; ------ part one ------

; ------ part two ------
(define ghost-start-nodes
  (filter
   (lambda (l) (string-suffix? l "A"))
   (map (lambda (l) (substring l 0 3)) nodes-paths)))

(define (get-next-node current-node ins)
  (letrec
      ([paths (hash-ref paths-hash current-node)]
       [next-node (if (eq? ins #\L) (car paths) (cdr paths))])
    next-node))

(define (run2 node step)
  (letrec
      ([instruction (string-ref instructions (remainder step (string-length instructions)))]
       [next-node (get-next-node node instruction)]
       [ends-z (string-suffix? next-node "Z")])
    (if
     (or ends-z )
     (+ 1 step)
     (run2 next-node (+ 1 step)))))

(define (get-node-step node) (run2 node 0))

(define ghost-steps (map get-node-step ghost-start-nodes))
; (display ghost-steps)
; (display "\n")

; should find list common multiply rather
(display (* (string-length instructions) (apply * (map (lambda (a) (/ a (string-length instructions))) ghost-steps))))
(display "\n")
; ------ part two ------

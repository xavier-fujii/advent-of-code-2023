#lang racket

(define lines
  (with-input-from-file "input.txt"
    ;   (with-input-from-file "sample.txt"
    (thunk
     (sequence->list (in-lines)))))


(define (get-game-set l)
  (foldl
   (lambda (a result)
     (list
      (if (string-contains? a "red") (max (string->number (car (regexp-match #px"[\\d]+" a))) (first result)) (first result))
      (if (string-contains? a "green") (max (string->number (car (regexp-match #px"[\\d]+" a))) (second result)) (second result))
      (if (string-contains? a "blue") (max (string->number (car (regexp-match #px"[\\d]+" a))) (third result)) (third result))
      ))
   (list 0 0 0)
   l))

(define
  (get-game str)
  (letrec (
           [split-result (string-split str ":" #:trim? #t)]
           [game-id (string->number (car (regexp-match #px"[\\d]+" (first split-result))))]
           [game-sets
            (map
             (lambda (str) (get-game-set (string-split (string-trim str) ",")))
             (string-split (last split-result) ";" #:trim? #t))]
           )
    (list game-id (get-biggest-set game-sets))))

(define (get-biggest-set l)
  (foldl
   (lambda (a result) (list
                       (max (first a) (first result))
                       (max (second a) (second result))
                       (max (third a) (third result))
                       ))
   (list 0 0 0)
   l))

(define games (map get-game lines))
(display games)
(display "\n")

; ------ part one ------
; only 12 red cubes, 13 green cubes, and 14 blue cubes
; game list format: ((id (r g b))
; if possible return game id
; if not return 0
(define (get-possible-id game)
  (let ([possible
         (and
          (>= 12 (first (second game)))
          (>= 13 (second (second game)))
          (>= 14 (third (second game)))
          )
         ])
    (if possible (first game) 0)
    ))

(define part-one-answer (foldl + 0
                               (map (lambda (game) (get-possible-id game) ) games)))
(display part-one-answer)
(display "\n")
; ------ part one ------


; ------ part two ------
(define (get-power game)
  (*
   (first (second game))
   (second (second game))
   (third (second game))))


(define part-two-answer (foldl + 0
                               (map (lambda (game) (get-power game) ) games)))
(display part-two-answer)
(display "\n")
; ------ part two ------

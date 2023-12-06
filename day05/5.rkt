#lang racket

(define lines
  (filter non-empty-string?
          ; (with-input-from-file "input.txt"
          (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))

(define seeds
  (map string->number
       (string-split (string-replace  (car lines) "seeds: " "") " ")))

(define indexes
  (filter number?
          (for/list
              ([line lines]
               [index (range 0 (length lines))])
            (if (string-contains? line "map:") index #f))))

; (display indexes)
; (display "\n")

(define raw-maps (for/list
                     ([start indexes]
                      [end (append (cdr indexes) (list (length lines)))])
                   (map (lambda (pos) (list-ref lines pos)) (range (+ start 1) end))))

(define
  (parse-raw-map raw-map)
  (map
   (lambda (line) (map string->number (string-split line " ")))
   raw-map))
(define maps (map (lambda (raw-map) (parse-raw-map raw-map)) raw-maps))
(display (first maps))

(define (lookup number map) ())

(display "\n")
; ------ part one ------


; ------ part one ------


; ------ part two ------
; ------ part two ------

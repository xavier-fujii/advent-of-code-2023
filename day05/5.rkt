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

(define raw-maps (for/list
                     ([start indexes]
                      [end (append (cdr indexes) (list (length lines)))])
                   (map (lambda (pos) (list-ref lines pos)) (range (+ start 1) end))))

(define (parse-raw-map raw-map)
  (map
   (lambda (line) (map string->number (string-split line " ")))
   raw-map))

(define maps
  (map (lambda (raw-map) (parse-raw-map raw-map)) raw-maps))

(define (get-value-by-map-rule number map-rule)
  (let ([destination-range-start (first map-rule)]
        [source-range-start (second map-rule)]
        [range-length (third map-rule)])
    (if (and (>=  number source-range-start) (< number (+ source-range-start range-length)))
        (+ (- number source-range-start) destination-range-start)
        number)))

(define (get-value-by-map number map)
  (foldl (lambda (map-rule result) (get-value-by-map-rule result map-rule)) number map))

(define (get-value-by-maps number maps)
  (foldl (lambda (map result) (get-value-by-map result map)) number maps))

; (display (get-value-by-map 79 (first maps)))
; (display (get-value-by-maps 79 maps))
; (display "\n")

; ------ part one ------
(define locations (map (lambda (seed) (get-value-by-maps seed maps)) seeds))
(define part-two-answer (apply min locations))
(display part-two-answer)
(display "\n")
; ------ part one ------

; ------ part two ------
; ------ part two ------

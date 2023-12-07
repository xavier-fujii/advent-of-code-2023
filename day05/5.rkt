#lang racket

(define lines
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ; (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))

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
        #f)))

(define (get-value-by-map number map-item)
  (let ([values (filter number? (map (lambda (map-rule) (get-value-by-map-rule number map-rule)) map-item))])
    (if (empty? values) number (first values))))

(define (get-value-by-maps number maps)
  (foldl (lambda (map result) (get-value-by-map result map)) number maps))


; ------ part one ------
(define seeds-part-one
  (map string->number
       (string-split (string-replace  (car lines) "seeds: " "") " ")))

(define locations-1 (map (lambda (seed) (get-value-by-maps seed maps)) seeds-part-one))
(define part-one-answer (apply min locations-1))
(display part-one-answer)
; (display "\n")
; ------ part one ------

; ------ part two ------
(define seeds-ranges
  (map
   (lambda (idx)
     (let ([start (list-ref seeds-part-one idx)]
           [length (list-ref seeds-part-one (+ 1 idx))]
           ) (cons start (+ start length))))
   (range 0 (length seeds-part-one) 2)))

(display  seeds-ranges)
(display "\n")


; (define part-two-answer (apply min locations-2))
; (display part-two-answer)
; ------ part two ------

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
(display "\n")
; ------ part one ------

; ------ part two ------
(define (get-source-ranges map-item)
  (sort
   (map
    (lambda (map-rule) (cons (second map-rule) (+ (second map-rule) (- (third map-rule) 1))))
    map-item)
   (lambda (a b) (< (car a) (car b)))))

(define seeds-ranges
  (sort
   (map
    (lambda (idx)
      (let ([start (list-ref seeds-part-one idx)]
            [length (list-ref seeds-part-one (+ 1 idx))]
            )
        (cons start (- (+ start length) 1))))
    (range 0 (length seeds-part-one) 2)
    )
   (lambda (a b) (< (car a) (car b)))))

(define (divide-range-by-range tb-div-range rang)
  (let ([tb-start (car tb-div-range)]
        [tb-end (cdr tb-div-range)]
        [start (car rang)]
        [end (cdr rang)])
    (cond
      [(< tb-end start)  (list tb-div-range)]
      [(> tb-start end)  (list tb-div-range)]
      [(and (< tb-end end) (> tb-start start))  (list tb-div-range)]
      [(and (> tb-end end) (< tb-start start))  (list
                                                 (cons tb-start (- start 1))
                                                 (cons start end)
                                                 (cons (+ end 1) tb-end))]
      [(and (< start tb-end) (> start tb-start)) (list (cons tb-start (- start 1)) (cons start  tb-end))]
      [(and (< end tb-end) (> end tb-start)) (list (cons tb-start end) (cons (+ end 1) tb-end))]
      [else (list tb-div-range)])))

(define (divide-ranges-by-range tb-div-ranges rang)
  (apply append (list) (map (lambda (r) (divide-range-by-range r rang)) tb-div-ranges)))

(define (divide-ranges-by-ranges tb-div-ranges rangs)
  (foldl (lambda (a result) (divide-ranges-by-range result a)) tb-div-ranges rangs))

(define (get-mapped-ranges input-ranges map-item)
  (sort (let ([new-vs
               (divide-ranges-by-ranges input-ranges (get-source-ranges map-item))])
          (map
           (lambda (v) (cons
                        (get-value-by-map (car v) map-item)
                        (get-value-by-map (cdr v) map-item)))
           new-vs))
        (lambda (a b) (< (car a) (car b)))))

(define final-results
  (foldl
   (lambda (map-item result)
     (get-mapped-ranges result map-item))
   seeds-ranges
   maps))

(define part-two-answer (car (first final-results)))
(display part-two-answer)
(display "\n")
; ------ part two ------
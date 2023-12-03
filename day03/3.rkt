#lang racket

(define lines
  (with-input-from-file "input.txt"
    ;   (with-input-from-file "sample.txt"
    (thunk
     (sequence->list (in-lines)))))

(define lines-with-index
  (for/list
      ([str lines]
       [line-index (range 0 (length lines))])
    (cons line-index str))
  )

(define/contract
  (not-overflow? pos)
  (-> pair? boolean?)
  (not (let ([line-index (car pos)]
             [column-index (cdr pos)]
             ) (or
                (< line-index 0)
                (< column-index 0)
                (>= line-index (length lines))
                (>= column-index (string-length (first lines)))
                ))))


(define all-numbers-non-flat
  (map (lambda (l)
         (let
             ([str (cdr l)]
              [line-index (car l)]
              )
           (for/list
               (
                [pos (regexp-match-positions* #px"[\\d]+" str)]
                [number-str (regexp-match* #px"[\\d]+" str)]
                )
             (list
              ; number itself
              (string->number number-str)
              ; line index
              line-index
              ; start position, included
              (car pos)
              ; end position, excluded
              (cdr pos)
              ; to be checked positions
              (let ([start-pos (car pos)] [end-pos (cdr pos)])
                (filter not-overflow?
                        (append (list
                                 (cons line-index (- start-pos 1))
                                 (cons line-index end-pos)
                                 )
                                (map
                                 (lambda (a) (cons (- line-index 1) a))
                                 (range (- start-pos 1) (+ end-pos 1)))
                                (map
                                 (lambda (a) (cons (+ line-index 1) a))
                                 (range (- start-pos 1) (+ end-pos 1))))))
              )
             ))) lines-with-index ))

(define all-numbers (foldl append '() all-numbers-non-flat))

; ------ part one ------

(define
  (is-part-number num)
  (letrec (
           [line-index (second num)]
           [start-pos  (third num)]
           [end-pos (fourth num)]
           [to-be-checked-positions (fifth num)])
    (foldl
     (lambda (pos result) (or (is-pos-valid-for-part-number pos) result))
     false
     to-be-checked-positions)))

(define (is-pos-valid-for-part-number pos) (is-symbol pos))

(define (is-symbol pos)
  (letrec (
           [line-index (car pos)]
           [column-index (cdr pos)]
           [char (string-ref (list-ref lines line-index) column-index)]
           )
    (not (or
          (equal? char #\1)
          (equal? char #\2)
          (equal? char #\3)
          (equal? char #\4)
          (equal? char #\5)
          (equal? char #\6)
          (equal? char #\7)
          (equal? char #\8)
          (equal? char #\9)
          (equal? char #\.)
          ))))

(define all-part-numbers (filter is-part-number all-numbers))
(define part-one-answer (foldl + 0 (map (lambda (n) (car n)) all-part-numbers)))
(display part-one-answer)
(display "\n")
; ------ part one ------

; ------ part two ------

(define/contract
  (is-asterisk? pos)
  (-> pair? boolean?)
  (equal? #\* (string-ref (list-ref lines (car pos)) (cdr pos))))

(define/contract
  (is-adjacent-to-asterisk? number)
  (-> list? boolean?)
  (let ([to-be-checked-positions (fifth number)])
    (foldl (lambda (pos result) (or (is-asterisk? pos) result)) false to-be-checked-positions)))

(define all-adjacent-to-asterisk-numbers
  (map (lambda (numbers) (filter is-adjacent-to-asterisk? numbers ))
       all-numbers-non-flat))


(define all-asterisks
  (filter is-asterisk?
          (foldl append '()
                 (map
                  (lambda (line-index)
                    (map
                     (lambda (col-index) (cons line-index col-index))
                     (range 0 (string-length (first lines))) )
                    ) (range 0 (length lines))))))

(define (is-number-asterisk-adjacent? number asterisk)
  (let ([to-be-checked-positions (fifth number)])
    (foldl
     (lambda (pos result) (or (and (equal? (car asterisk) (car pos)) (equal? (cdr asterisk) (cdr pos))) result))
     false
     to-be-checked-positions)
    )
  )

(define/contract
  (get-asterisk-ratio asterisk)
  (-> pair? number?)
  (letrec (
           [line-index (car asterisk)]
           [to-be-checked-numbers
            (append
             '()
             (if (equal? line-index 0) '() (list-ref all-adjacent-to-asterisk-numbers (- line-index 1)))
             (list-ref all-adjacent-to-asterisk-numbers  line-index )
             (if (equal? line-index (- (length lines) 1)) '() (list-ref all-adjacent-to-asterisk-numbers (+ line-index 1)))
             )]
           [
            adjacent-numbers
            (filter (lambda (n) (is-number-asterisk-adjacent? n asterisk)) to-be-checked-numbers)
            ]
           )
    (if (> (length adjacent-numbers) 1)
        (* (first (first adjacent-numbers)) (first (second adjacent-numbers)) )
        0
        )))

(define part-two-answer (foldl (lambda (asterisk result) (+ result (get-asterisk-ratio asterisk))) 0 all-asterisks))
(display part-two-answer)
(display "\n")

; ------ part two ------


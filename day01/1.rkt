#lang racket

(define lines
  ; (with-input-from-file "sample.txt"
  (with-input-from-file "input.txt"
    (thunk
     (sequence->list (in-lines)))))

; ------ part one ------
(define/contract
  (range-1-to-9? n)
  (-> number? boolean?)
  ; judge if number is
  ; greater or equal than 1
  ; and smaller or equal than 9
  (and (> n 0) (< n 10)))

(define/contract
  (cal-from-number-list l)
  (-> list? number?)
  ; calculate value from a list contains number
  (+ (* (first l) 10) (last l)))

(define/contract
  (cal-from-char-list l)
  (-> list? number?)
  (cal-from-number-list
   (filter range-1-to-9? (map (lambda (c) (- (char->integer c) 48)) l))))

(define part-one-answer
  (foldl + 0
         (map
          (lambda (str) (cal-from-char-list (string->list str)))
          lines)))

(display part-one-answer)
; ------ part one ------

; ------ part two ------
(define dict (list
              (cons "1" 1)
              (cons "2" 2)
              (cons "3" 3)
              (cons "4" 4)
              (cons "5" 5)
              (cons "6" 6)
              (cons "7" 7)
              (cons "8" 8)
              (cons "9" 9)
              (cons "one" 1)
              (cons "two" 2)
              (cons "three" 3)
              (cons "four" 4)
              (cons "five" 5)
              (cons "six" 6)
              (cons "seven" 7)
              (cons "eight" 8)
              (cons "nine" 9)
              ))

(define (reverse-str s)
  (list->string (reverse (string->list s))))

(define
  (get-digit str should-reverse)
  (cdr
   (foldl
    (lambda
        (a result)
      (if (< (car a) (car result)) a result))
    (cons 999 0)
    (map
     (lambda (item)
       (let
           ([match-result
             (list
              (regexp-match-positions
               (if should-reverse (reverse-str (car item)) (car item))
               (if should-reverse (reverse-str str) str)) (cdr item))
             ])
         (if
          (false? (first match-result))
          ; if not match, return pair (999 . digit)
          (cons 999 0)
          ; if match, return pair (index . digit)
          (cons (car (first (first match-result))) (second match-result)))
         )
       )
     dict)))
  )

(define (get-first-digit str) (get-digit str false))
(define (get-second-digit str) (get-digit str true))
(define (get-number str) (+ (* (get-first-digit str) 10) (get-second-digit str)))

(define part-two-answer
  (foldl + 0
         (map get-number lines)))

(display part-two-answer)
; ------ part two ------
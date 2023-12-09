#lang racket

(define lines-raw
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ;   (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))

(define (string-numbers lines) (map (lambda (l) (map string->number (string-split l " "))) lines))
(define lines (string-numbers lines-raw) )

(define (all-zero numbers)
  (foldl (lambda (a result) (and result (= a 0))) #t numbers))

(define (get-next-level numbers)
  (map (lambda (idx) (- (list-ref numbers idx) (list-ref numbers (- idx 1)))) (range 1 (length numbers) )))


; ------ part one ------
(define (get-next-value numbers)
  (if (all-zero numbers)
      0
      (+ (first numbers)
         (letrec
             ([next-level-nums (get-next-level numbers)]
              [next-level-nums-sums (apply + next-level-nums)]
              [next-level-next-value (get-next-value next-level-nums)])
           (+ next-level-nums-sums next-level-next-value))
         )))

(define part-1-ans (apply + (map get-next-value lines)))
(display part-1-ans)
(display "\n")

; ------ part one ------

; ------ part two ------
(define (get-previous-v numbers)
  (if
   (all-zero numbers)
   0
   (- (first numbers) (get-previous-v (get-next-level numbers)))))

(define part-2-ans (apply + (map get-previous-v lines)))
(display part-2-ans)
(display "\n")

; ------ part two ------

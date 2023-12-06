#lang racket

(define lines
  (with-input-from-file "input.txt"
    ;   (with-input-from-file "sample.txt"
    (thunk
     (sequence->list (in-lines)))))

(define (parse-to-numbers str)
  (map string->number (string-split (string-replace str "  " " ") " ")))

(define raw-cards
  (map
   (lambda (line)
     (let ([split-results (regexp-split #px"[:|]" line)])
       (list
        (parse-to-numbers (second split-results))
        (parse-to-numbers (third split-results))
        (string->number (string-trim (string-replace (first split-results) "Card" ""))))))
   lines))

(define/contract
  (get-matching-number raw-card)
  (-> list? number?)
  (letrec ([winnings-numbers (first raw-card)]
           [owned-numbers (second raw-card)]
           [matched-count
            (foldl
             (lambda
                 (owned-number result)
               (+ result (if (member owned-number winnings-numbers) 1 0)))
             0
             owned-numbers)])
    matched-count))

; ------ part one ------
(define (get-points raw-card)
  (let ([matched-count (get-matching-number raw-card)] )
    (if (eq? matched-count 0) 0 (expt 2 (- matched-count 1)))))

(define part-one-answer (foldl + 0 (map get-points raw-cards)))
(display part-one-answer)
(display "\n")
; ------ part one ------

; ------ part two ------
(define cards
  (map
   (lambda (raw-card)
     (list
      (third raw-card) ; card index
      (get-matching-number raw-card) ; card matching number
      1 ; card count
      ))
   raw-cards))

; reduce matching number to 0 while increasing card count
; when all cards' matching number are zero, work is done
; In the end, sum all cards' count

(define
  (run current-card-index cards)
  (if (> current-card-index (length cards)) cards
      (letrec
          ([current-card (list-ref cards (- current-card-index 1))]
           [matching-number (second current-card)]
           [current-card-count (third current-card)]
           [processed-by-one-round
            (map
             (lambda (card)
               (let ([is-current (eq? current-card-index (first card))]
                     [should-clone (and
                                    (> (first card) current-card-index)
                                    (<= (first card) (+ current-card-index matching-number))
                                    )])
                 (if
                  is-current
                  (list (first card ) 0 (third card))
                  (if should-clone (list (first card) (second card) (+ current-card-count (third card))) card)))
               ) cards)])
        (run (+ current-card-index 1) processed-by-one-round)
        ))
  )
(define processed-cards (run 1 cards))
; (display processed-cards)
; (display "\n")
(define part-two-answer (foldl + 0 (map (lambda (card) (third card)) processed-cards)))
(display part-two-answer)
(display "\n")
; ------ part two ------

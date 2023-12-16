#lang racket

(define lines-raw
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ; (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))
(define tiles (map (lambda (line) (string->list line)) lines-raw))

(define (get-tile pos) (list-ref (list-ref tiles (car pos)) (cdr pos)))

(define/contract (get-left pos) (-> pair? pair?) (cons (cons (car pos) (- (cdr pos) 1)) "left"))
(define/contract (get-right pos) (-> pair? pair?) (cons (cons (car pos) (+ (cdr pos) 1)) "right"))
(define/contract (get-up pos) (-> pair? pair?) (cons (cons (- (car pos) 1) (cdr pos)) "upward"))
(define/contract (get-down pos) (-> pair? pair?) (cons (cons (+ (car pos) 1)(cdr pos)) "downward"))

(define (get-is-valid pos)
  (define rowIdx (car pos))
  (define colIdx (cdr pos))

  (and
   (>= rowIdx 0)
   (>= colIdx 0)
   (< colIdx (length (first tiles)))
   (< rowIdx (length tiles))
   ))

; ------ part one ------

(define (fill-tiles-by-beams pos direction)

  ; value is like (has-tile-been-reached, list-of-directions-don't-need-to-pass-through)
  (define records-hash-energized (make-hash))
  (define records-hash-stop-directions (make-hash))

  (for ([rowIdx (range 0 (length tiles))])
    (for ([colIdx (range 0 (length (first tiles)))])
      (hash-set! records-hash-energized (cons rowIdx colIdx) #f)
      (hash-set! records-hash-stop-directions (cons rowIdx colIdx) (list))
      )
    )

  (define (run pos beam-direction)
    (hash-set! records-hash-energized pos #t)

    (define next-pos-beam-dirs
      (match (cons (get-tile pos) beam-direction)
        [(cons #\- "upward") (list (get-left pos) (get-right pos))]
        [(cons #\- "downward") (list (get-left pos) (get-right pos))]
        [(cons #\- "left") (list (get-left pos) (get-right pos))]
        [(cons #\- "right") (list (get-right pos) (get-left pos))]

        [(cons #\| "upward") (list (get-up pos) (get-down pos))]
        [(cons #\| "downward") (list (get-down pos) (get-up pos))]
        [(cons #\| "left") (list (get-up pos) (get-down pos))]
        [(cons #\| "right") (list (get-up pos) (get-down pos))]

        [(cons #\. "upward") (list (get-up pos))]
        [(cons #\. "downward") (list (get-down pos))]
        [(cons #\. "left") (list (get-left pos))]
        [(cons #\. "right") (list (get-right pos))]

        [(cons #\\ "upward") (list (get-left pos))]
        [(cons #\\ "downward") (list (get-right pos))]
        [(cons #\\ "left") (list (get-up pos))]
        [(cons #\\ "right") (list (get-down pos ))]

        [(cons #\/ "upward") (list (get-right pos))]
        [(cons #\/ "downward") (list (get-left pos))]
        [(cons #\/ "left") (list (get-down pos))]
        [(cons #\/ "right") (list (get-up pos))]))

    (define next-pos-beam-dirs-valid (filter (lambda (a) (get-is-valid (car a))) next-pos-beam-dirs))

    (define should-stop-directions (hash-ref records-hash-stop-directions pos))

    (hash-set! records-hash-stop-directions pos (map (lambda (a) (cdr a)) next-pos-beam-dirs-valid))

    (define next-pos-beam-dirs-should-go
      (filter
       (lambda
           (pos-and-dir)
         (boolean? (index-of should-stop-directions (cdr pos-and-dir))))
       next-pos-beam-dirs-valid))

    (for ([pos-and-dir next-pos-beam-dirs-should-go])
      (run (car pos-and-dir) (cdr pos-and-dir)))
    )

  (run pos direction)

  records-hash-energized)


(define (get-energized-number hash-energized)
  (define result   (foldl
                    (lambda (a result) (+ result (if a 1 0)))
                    0
                    (flatten (map
                              (lambda (rowIdx) (map
                                                (lambda (colIdx) (hash-ref hash-energized (cons rowIdx colIdx)))
                                                (range 0 (length (first tiles)))))
                              (range 0 (length tiles))))))
  result)

(define part-1-ans (get-energized-number (fill-tiles-by-beams (cons 0 0) "right")))

(display part-1-ans)
(display "\n")

; ------ part one ------




; ------ part two ------
; could use cache to reduce time complexity, but I'm kind of lazy and want to spend my time on advent of TypeScript and memeroizing English words

(define top-row
  (map (lambda (colIdx) (cons (cons 0 colIdx) "downward")) (range 0 (length (first tiles)))))
(define bottom-row
  (map (lambda (colIdx) (cons (cons (- (length tiles) 1) colIdx) "upward")) (range 0 (length (first tiles)))))
(define leftmost-column
  (map (lambda (rowIdx) (cons (cons rowIdx 0) "right")) (range 0 (length tiles)))
  )
(define rightmost-column
  (map (lambda (rowIdx) (cons (cons rowIdx (- (length (first tiles)) 1)) "left")) (range 0 (length tiles)))
  )

(define all-positions (append top-row bottom-row leftmost-column rightmost-column))
(define all-numbers (map (lambda (a) (get-energized-number (fill-tiles-by-beams (car a) (cdr a)))) all-positions))
(display (apply max all-numbers))
(display "\n")


; ------ part two ------

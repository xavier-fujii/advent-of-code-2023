#lang racket

(define lines-raw
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ;   (with-input-from-file "sample2.txt"
            (thunk
             (sequence->list (in-lines))))))

(define tiles
  (map (lambda (line-raw) (string->list line-raw)) lines-raw))

; ------ part one ------
(define (find-element-indices value matrix)
  (define num-rows (length matrix))
  (define num-cols (if (zero? num-rows) 0 (length (first matrix))))

  (define (search-indices row col)
    (cond ((equal? row num-rows) #f) ; element not found
          ((equal? col num-cols) (search-indices (+ row 1) 0)) ; move to the next row
          ((equal? (list-ref (list-ref matrix row) col) value) (cons row col)) ; element found
          (else (search-indices row (+ col 1))))) ; move to the next column

  (search-indices 0 0))

(define start-tile #\S)
(define start-pos (find-element-indices start-tile tiles))

(define (get-tile pos)
  (list-ref (list-ref tiles (car pos)) (cdr pos)))

(define (get-start-tile-real pos)
  (define (get-tile-with-overflow pos)
    (if (or
         (< (car pos) 0)
         (< (cdr pos) 0)
         (> (car pos) (length tiles))
         (> (cdr pos) (length (first tiles))))
        #\.
        (get-tile pos)))

  (define left  (string (get-tile-with-overflow (cons (car pos) (- (cdr pos) 1)))))
  (define right (string (get-tile-with-overflow (cons (car pos) (+ (cdr pos) 1)))))
  (define top   (string (get-tile-with-overflow (cons (- (car pos) 1) (cdr pos)))))
  (define down  (string (get-tile-with-overflow (cons (+ (car pos) 1) (cdr pos)))))

  (cond
    [(and (string-contains? "|7F"  top) (string-contains? "|JL"  down)) #\|]
    [(and (string-contains? "-FL" left) (string-contains? "-7J" right)) #\-]
    [(and (string-contains? "|JL" down) (string-contains? "-7J" right)) #\F]
    [(and (string-contains? "-FL" left) (string-contains? "|JL"  down)) #\7]
    [(and (string-contains? "|7F"  top) (string-contains? "-FL"  left)) #\J]
    [(and (string-contains? "|7F"  top) (string-contains? "-7J" right)) #\L]
    ))

(define start-tile-real (get-start-tile-real start-pos))

(define start-direction
  (match start-tile-real
    [#\-  "go-right"]
    [#\|   "go-down"]
    [#\L  "go-right"]
    [#\F  "go-right"]
    [#\J   "go-left"]
    [#\7   "go-left"]))

(define (get-tile-for-moving pos)
  (if
   (equal? pos start-pos)
   start-tile-real
   (get-tile pos)))

(define/contract
  (move-tile current-position move-direction)
  (-> pair? string? pair?)

  (define row-idx (car current-position))
  (define col-idx (cdr current-position))

  (define next-pos (match move-direction
                     ["go-right"   (cons row-idx (+ col-idx 1))]
                     ["go-left"    (cons row-idx (- col-idx 1))]
                     ["go-down"    (cons (+ row-idx 1) col-idx)]
                     ["go-up"      (cons (- row-idx 1) col-idx)]))

  (define next-tile (get-tile-for-moving next-pos))

  (define next-move-direction
    (match (cons next-tile move-direction)
      [(cons #\- "go-right")  move-direction]
      [(cons #\-  "go-left")  move-direction]
      [(cons #\|  "go-down")  move-direction]
      [(cons #\|    "go-up")  move-direction]
      [(cons #\L  "go-down")      "go-right"]
      [(cons #\L  "go-left")         "go-up"]
      [(cons #\F  "go-left")       "go-down"]
      [(cons #\F    "go-up")      "go-right"]
      [(cons #\J  "go-down")       "go-left"]
      [(cons #\J "go-right")         "go-up"]
      [(cons #\7 "go-right")       "go-down"]
      [(cons #\7    "go-up")       "go-left"]))

  (cons next-pos next-move-direction))

(define (run steps current-tile-pos move-direction)

  (define next-move-info (move-tile current-tile-pos move-direction))
  (define next-tile-pos (car next-move-info))
  (define next-move-direction (cdr next-move-info))
  (define next-tile (get-tile next-tile-pos))

  (if
   (equal? next-tile start-tile)
   (+ 1 steps)
   (run (+ 1 steps) next-tile-pos next-move-direction)
   )
  )

(define steps-result (run 0 start-pos start-direction))
(define part-one-answer (/ steps-result 2))
(display part-one-answer)
(display "\n")
; ------ part one ------


; ------ part two ------
(define (calc-loops loops steps current-tile-pos move-direction)

  (define next-move-info (move-tile current-tile-pos move-direction))
  (define next-tile-pos (car next-move-info))
  (define next-move-direction (cdr next-move-info))
  (define next-tile (get-tile next-tile-pos))

  (define new-loops (append loops (list (cons next-tile-pos next-move-direction))))

  (if
   (equal? next-tile start-tile)
   loops
   (calc-loops new-loops (+ 1 steps) next-tile-pos next-move-direction)
   )
  )

(define loops
  (calc-loops
   (list (cons start-pos start-direction))
   0
   start-pos start-direction))

(define (get-go-up-or-go-down pos direction)
  (define tile (get-tile-for-moving pos))

  (cond
    [(eq? #\F tile) (if (eq? direction "go-right") "go-up"   direction)]
    [(eq? #\7 tile) (if (eq? direction "go-left")  "go-up"   direction)]
    [(eq? #\J tile) (if (eq? direction "go-left")  "go-down" direction)]
    [(eq? #\L tile) (if (eq? direction "go-right") "go-down" direction)]
    [(eq? #\| tile) direction]
    [(eq? #\- tile) "none"]
    [else "none"]))

(define loops-hash
  (foldl
   (lambda (a result-hash) (hash-set result-hash (car a) (get-go-up-or-go-down (car a) (cdr a))))
   (hash)
   loops))

(define (get-inner-count lst)
  ; remove all "none" type tile
  (define lst2 (filter (lambda (a) (not (eq? "none" a))) lst))

  ; combine multiple adjecent go-up to one go-up
  ; combine multiple adjecent go-down to one go-down
  ; this step is necesary for changing value should-count
  (define lst3 (foldl
                (lambda (c result)
                  (cond
                    [(empty? result) (append result (list c))]
                    [(and (eq? c "go-up") (eq? c (last result))) result]
                    [(and (eq? c "go-down") (eq? c (last result))) result]
                    [else (append result (list c))]))
                (list)
                lst2))

  (define (run count should-count lt)
    (if (empty? lt)
        count
        (match (cons (first lt) should-count)
          [(cons "go-up"   #t) (run count       #f (list-tail lt 1))]
          [(cons "go-up"   #f) (run count       #t (list-tail lt 1))]
          [(cons "go-down" #t) (run count       #f (list-tail lt 1))]
          [(cons "go-down" #f) (run count       #t (list-tail lt 1))]
          [(cons #f        #t) (run (+ count 1) #t (list-tail lt 1))]
          [(cons #f        #f) (run count       #f (list-tail lt 1))])))

  (run 0 #f lst3)
  )

; scan row by row, get inner tiles of each row
(define (get-inner-counts-of-row row-idx)
  (get-inner-count (map
                    (lambda (col-idx)
                      (if (hash-has-key? loops-hash (cons row-idx col-idx))
                          (hash-ref loops-hash (cons row-idx col-idx))
                          #f))
                    (range 0 (length (first tiles))))))


(define part-2-ans
  (apply + (map (lambda (row-idx) (get-inner-counts-of-row row-idx)) (range 0 (length tiles)))))

(display part-2-ans)
(display "\n")
; ------ part two ------

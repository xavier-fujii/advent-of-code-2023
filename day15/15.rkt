#lang racket

(define lines-raw
  (filter non-empty-string?
          (with-input-from-file "input.txt"
            ; (with-input-from-file "sample.txt"
            (thunk
             (sequence->list (in-lines))))))
(define strings (string-split (first lines-raw) ","))

(define/contract (my-hash str)  (-> string? number?)
  (define chars (string->list str))
  (define result
    (foldl
     (lambda (a result) (remainder (* 17 (+ result (char->integer a))) 256))
     0
     chars))
  result)

; ------ part one ------
; (define part-1-ans (apply + (map my-hash strings)))
; (display part-1-ans)
; (display "\n")

; ------ part one ------

; ------ part two ------

(define (process-labels labels)
  (if
   (string-contains? labels "-")
   (list "remove"
         (substring labels 0 (- (string-length labels) 1)))
   (list "insert"
         (substring labels 0 (- (string-length labels) 2))
         (string->number (substring labels (- (string-length labels) 1) (string-length labels))))
   ))


(define labels (map process-labels strings))

(define empty-boxes (map (lambda _a (list)) (range 0 256)))

(define (run-by-label-insert label boxes)
  (define modified-box-index (my-hash (second label)))
  (define new-lens (cons (second label) (third label)))
  (define box (list-ref boxes modified-box-index))

  (define lenses-labels (map (lambda (lens)  (car lens)) box))
  (define search-index (index-of lenses-labels (second label)))

  (define modified-box (if
                        (number? search-index )
                        (map (lambda (lens) (if
                                             (equal? (car lens) (second label) )
                                             new-lens
                                             lens
                                             )) box)
                        (append box (list new-lens))
                        ))

  (define result (map
                  (lambda (idx) (if
                                 (= modified-box-index idx)
                                 modified-box
                                 (list-ref boxes idx)))
                  (range 0 (length boxes))))
  result)

(define (run-by-label-remove label boxes)
  (define modified-box-index (my-hash (second label)))
  (define box (list-ref boxes modified-box-index))
  (define modified-box
    (filter (lambda (lens) (not (equal? (car lens) (second label)))) box))

  (define result (map
                  (lambda (idx) (if
                                 (= modified-box-index idx)
                                 modified-box
                                 (list-ref boxes idx)))
                  (range 0 (length boxes))))
  result)

(define (run-by-label label boxes)
  (if
   (eq? (first label) "insert")
   (run-by-label-insert label boxes)
   (run-by-label-remove label boxes)))

(define boxes (foldl run-by-label empty-boxes labels))


(define (calc-box box)
  (define result (map
                  (lambda (idx)
                    (* (+ idx 1) (cdr (list-ref box idx))))
                  (range 0 (length box))))
  (apply + result))

(define part-2-ans (apply +
                          (map
                           (lambda (idx) (* (+ idx 1) (calc-box (list-ref boxes idx))))
                           (range 0 (length boxes)))))

(display part-2-ans)
(display "\n")
; ------ part two ------



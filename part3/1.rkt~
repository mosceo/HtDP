;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(check-expect (make-row '(1 "title")) '(tr (td "1") (td "title")))

(define (make-row l)
  `(tr (td ,(number->string (first l)))
       (td ,(second l))))


;-------------
(check-expect (ranking '("flag" "mask" "fire"))
              '((1 "flag") (2 "mask") (3 "fire")))
(define (ranking los)
  (reverse (add-ranks (reverse los))))


;-------------
(check-expect (add-ranks '("flag" "mask" "fire"))
              '((3 "flag") (2 "mask") (1 "fire")))
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))






;------------
(define (make-ranking los)
  (make-ranking-aux (ranking los)))
ы

;-------------
(check-expect (make-ranking-aux '((1 "title1") (2 "title2")))
                                `(,(make-row '(1 "title1"))
                                  ,(make-row '(2 "title2"))))
(define (make-ranking-aux lst)
  (cond [(empty? lst) '()]
        [else (cons (make-row (first lst))
                    (make-ranking-aux (rest lst)))]))

               
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

(make-ranking one-list)






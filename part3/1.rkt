;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;====================

; (Exercise 262.)
;
; Number -> Matrix
; creates an identity matrix of a given size
(check-expect (identityM 0) '())
(check-expect (identityM 1) '((1)))
(check-expect (identityM 3) '((1 0 0) (0 1 0) (0 0 1)))

(define (identityM n)
  (local ((define (zero n) 0)
          (define (zeros n) (build-list n zero))
          (define (create-row i)
            (append (zeros i) (list 1) (zeros (- n i 1)))))
    (map create-row (range 0 n 1))))


;----------------+
; Specifications |
;----------------+


; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)


; N -> [List-of Posn]
; generate n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3) (n-inside-playground? 3))

(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))


(define (n-inside-playground? cnt)
  (local ((define (inside? p)
            (and (< (posn-x p) WIDTH) (< (posn-y p) HEIGHT))))
    (lambda (lst)
      (and (= (length lst) cnt)
           (andmap inside? lst)))))

(check-satisfied (list (make-posn 0 0)
                       (make-posn 0 0)
                       (make-posn 0 0))
                 (n-inside-playground? 3))
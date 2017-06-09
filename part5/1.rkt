;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
;--------------------------


(define (substr s n)
  (if (<= (string-length s) n) ""
      (substring s n)))


; String Number -> [List-of String]
; partition a string into several substrings of a given size
(check-expect (partition "abcdefgh" 3) (list "abc" "def" "gh"))
(check-expect (partition "abcd" 2) (list "ab" "cd"))
(check-expect (partition "abcd" 1) (list "a" "b" "c" "d"))

(define (partition s n)
  (local ((define leftover (substr s n)))
    (cond [(equal? leftover "") (list s)]
          [else (cons (substring s 0 n) (partition leftover n))])))





(filter < lon)
(filter (lambda (x) (not (< x n))))
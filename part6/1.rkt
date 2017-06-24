;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
;==========================

; ... -> ...
; Creates a list of n elements by applying proc
; to the integers from 0 to (sub1 n) in order.
(check-expect (build-l*st 10 add1) (build-list 10 add1))

(define (build-l*st n f)
  (local ((define (build-l*st/a n a)
            (cond [(< n 0) a]
                  [else (build-l*st/a (sub1 n) (cons (f n) a))])))
    (build-l*st/a (sub1 n) '())))
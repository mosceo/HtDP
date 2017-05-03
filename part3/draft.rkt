;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/string)
;====================


(define (prefixes chars)
  (local
    ((define (f char pfx)
       (cons (cons char (first pfx))
             pfx)))
    ; -- IN --
    (foldl f
           (list '())
           chars)))


(prefixes (list "a" "b" "c" "d"))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname draft) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require racket/string)
(require 2htdp/universe)
(require 2htdp/abstraction)
;====================


; [List-of [List-of String]] -> [List-of Number]
; determines the number of words on each line 
(define (words-on-line lls)
  (map length lls))

(words-on-line (list (list 'a 'b 'c) (list) (list 'a 'b) (list) (list 'a)))

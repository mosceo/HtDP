;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname itunes-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)
(require 2htdp/batch-io)
(require 2htdp/abstraction)
;==========================

(define ITUNES-LOCATION "itunes.xml")
(define itunes-tracks (read-itunes-as-lists ITUNES-LOCATION))

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date
 
; String -> LLists
; creates a list of lists representation for all tracks in 
; file-name, which must be an XML export from iTunes 
; (define (read-itunes-as-lists file-name)
;   ...)

; Examples
(define assoc1 (list "Name" "Take Your Time"))
(define assoc2 (list "Album" "Nevermind"))
(define assoc3 (list "Year" 2008))

(define lassoc1 (list assoc1 assoc2 assoc3))

(define llist1 (list lassoc1))

;---------------------------------------------

; [X] String LAssoc X -> [Association or X]
(check-expect (find-association "Year" lassoc1 #false) assoc3)
(check-expect (find-association "Duration" lassoc1 #false) #false)

(define (find-association key lassoc default)
  (cond [(empty? lassoc) default]
        [(string=? key (first (first lassoc))) (first lassoc)]
        [else (find-association key (rest lassoc) default)]))


; LList -> Number
; Consumes an element of LList and produces the total amount of play time.

(define (total-time/list tt)
  (for/sum ([t tt])
    (second (find-association "Total Time" t 0))))


; LList -> [List-of Strings]
; Consumes an LLists and produces the Strings that
; are associated with a Boolean attribute.

(define (boolean-attributes llist)
  (create-set (filter (lambda (x) (string? x))
                      (for*/list ([lassoc llist] [assoc lassoc])
                        (if (boolean? (second assoc)) (first assoc) #false)))))


; [List-of String] -> [List-of String]
; Consumes a List-of-strings and constructs one that contains
; every String from the given list exactly once.
(check-expect (create-set (list "foo")) (list "foo"))
(check-expect (create-set (list "foo" "foo")) (list "foo"))
(check-expect (create-set (list "foo" "bar" "foo" "bar" "flag"))
              (list "foo" "bar" "flag"))

(define (create-set ss)
  (foldr (lambda (s ss) (if (member? s ss) ss (cons s ss)))
         '() ss))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; A FT is one of: 
; – NP
; – (make-child FT FT String N String)


; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
 
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else (or (string=? (child-eyes a-ftree) "blue")
              (blue-eyed-child? (child-father a-ftree))
              (blue-eyed-child? (child-mother a-ftree)))]))


; FT -> Boolean
; does a-ftree contain a child
; structure with "blue" in the eyes field
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)

(define (count-persons ftree)
  (cond
    [(no-parent? ftree) 0]
    [else (+ 1
             (count-persons (child-father ftree))
             (count-persons (child-mother ftree)))]))



; FT -> Boolean
; Consumes a family tree and produces a list of all eye colors in the tree.
; (An eye color may occur more than once). 

(define (eye-colors ftree)
  (cond
    [(no-parent? ftree) '()]
    [else (append (list (child-eyes ftree))
                  (eye-colors (child-father ftree))
                  (eye-colors (child-mother ftree)))]))


; A FF (short for family forest) is a [List-of FT]
; interpretation a family forest represents several
; families (say a town) and their ancestor trees


; [List-of FT] -> Boolean
; does the forest contain any child with "blue" eyes
 
(define (blue-eyed-child-in-forest? a-forest)
  (for/or ([ft a-forest])
    (blue-eyed-child? ft)))

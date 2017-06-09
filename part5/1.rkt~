;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
;--------------------------


;===========================================================
; 23 Simultaneous Processing
;===========================================================

; [List-of Symbol] [List-of Number] -> [List-of '(Symbol Number)]
; returns all permutations of symbols with numbers
(check-expect (cross '(a) '(1)) '((a 1)))
(check-expect (cross '(a) '(1 2)) '((a 1) (a 2)))
(check-expect (cross '(a b) '(1)) '((a 1) (b 1)))
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross sl nl)
  (local ((define (cross-sym s nl)
            (map (lambda (n) (list s n)) nl)))
    (cond [(or (empty? sl) (empty? nl)) '()]
          [else
           (append (cross-sym (first sl) nl)
                   (cross (rest sl) nl))])))


(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String).


; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combines names and phones pairwise into a list of phone records
(check-expect (zip '("jane" "jack") '(10 20)) (list (make-phone-record "jane" 10)
                                                    (make-phone-record "jack" 20)))

(define (zip names phones)
  (cond [(empty? names) '()]
        [else (cons
               (make-phone-record (first names) (first phones))
               (zip (rest names) (rest phones)))]))


;----------
; Ex. 390 ;
;----------

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 


(define tree0 (make-branch 'A (make-branch 'B 'C)))


; TOS [List-of Direction] -> TOS
; follows a path and returns what is there
(check-expect (tree-pick tree0 (list 'left)) 'A)
(check-expect (tree-pick tree0 (list 'right 'left)) 'B)
(check-expect (tree-pick tree0 (list 'right 'right)) 'C)
(check-error  (tree-pick tree0 (list 'left 'left)))

(define (tree-pick tree path)
  (cond [(and (symbol? tree) (empty? path)) tree]
        [(and (symbol? tree) (cons? path)) (error "end of tree")]
        [(and (branch? tree) (empty? path)) tree]
        [(and (branch? tree) (cons? path))
         (cond [(equal? (first path) 'left)  (tree-pick (branch-left tree) (rest path))]
               [(equal? (first path) 'right) (tree-pick (branch-right tree) (rest path))])]))


;----------
; Ex. 394 ;
;----------

; [List-of Number] [List-of Number] -> [List-of Number]
; Merges two lists that are sorted in ascending order.
; A number occurs in the output as many times as it occurs
; on the two input lists together.
(check-expect (merge '() '()) '())
(check-expect (merge '(1 1 3) '(0 0 5)) '(0 0 1 1 3 5))
(check-expect (merge '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))

(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [ (< (first l1) (first l2))
          (cons (first l1) (merge (rest l1) l2))]
        [else (cons (first l2) (merge l1 (rest l2)))]))













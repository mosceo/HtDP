;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BinaryTree (short for BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

; Example 1
(define BT1 (make-node 15 'd (make-node 87 'h NONE NONE) NONE))


; BinaryTree Number -> Boolean
; ...
(check-expect (contains-bt? BT1 0) #false)
(check-expect (contains-bt? BT1 87) #true)
(check-expect (contains-bt? BT1 70) #false)

(define (contains-bt? bt key)
  (cond [(equal? bt NONE) #false]
        [(equal? (node-ssn bt) key) #true]
        [else (or (contains-bt? (node-left bt) key)
                  (contains-bt? (node-right bt) key))]))


; BinaryTree Number -> [String or Boolean]
; ...
(check-expect (search-bt? BT1 0) #false)
(check-expect (search-bt? BT1 87) 'h)
(check-expect (search-bt? BT1 70) #false)

(define (search-bt? bt key)
  (cond [(equal? bt NONE) #false]
        [(equal? (node-ssn bt) key) (node-name bt)]
        [else (local ((define lb (search-bt? (node-left bt) key))
                      (define rb (search-bt? (node-right bt) key)))
                (cond [(not (boolean? lb)) lb]
                      [(not (boolean? rb)) rb]
                      [else #false]))]))


(define (search-bst bst key)
  (cond [(equal? bst NONE) NONE]
        [(equal? key (node-ssn bst)) (node-name bst)]
        [(< key (node-ssn bst)) (search-bst (node-left bst) key)]
        [else (search-bst (node-right bst) key)]))


;===============================================================================
;-------------------------------------------------------------------------------
;===============================================================================

; Any -> Boolean
; checks if a given object is of type atom

(define (atom? x)
  (or (number? x) (string? x) (symbol? x)))


; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '() 'a 'b) '())
(check-expect (substitute 'a 'a 'b) 'b)
(check-expect (substitute '(a) 'a 'b) '(b))
(check-expect (substitute '(a (1 (a) b) (30 (a))) 'a 'x)
              '(x (1 (x) b) (30 (x))))


(define (substitute sexp old new)
  (cond [(atom? sexp) (if (equal? sexp old) new sexp)]
        [else (map (lambda (s) (substitute s old new)) sexp)]))








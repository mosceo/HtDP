;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
;--------------------------

(define WRONG "WRONG")

(define-struct add [left right])
(define-struct mul [left right])

(define-struct fun [name expr])

; An BSL-Atom is one of:
; – Number
; – Boolean
; – Symbol

; A BSL-expr is one of:
; – BSL-Atom
; – (make-add BSL-expr BSL-expr)
; – (make-mul BSL-expr BSL-expr)
; – (make-fun Symbol BSL-expr)

; A BSL-value is one of:
; – Number


;====================================
; Interpreting the interactive area |
;====================================

; Any -> Boolean
; checks if a given object is of type atom
(define (atom? x)
  (or (number? x) (string? x)
      (symbol? x) (boolean? x)))


; S-expr -> BSL-expr
(check-expect (parse 5) 5)
(check-expect (parse 'x) 'x)
(check-expect (parse #true) #true)
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(check-expect (parse '(* 1 2)) (make-mul 1 2))
(check-expect (parse '(foo x)) (make-fun 'foo 'x))
(check-expect (parse '(+ (* 1 2) (foo 5)))
              (make-add (make-mul 1 2) (make-fun 'foo 5)))
(check-expect (parse '(foo (+ (exp 2) y)))
              (make-fun 'foo (make-add (make-fun 'exp 2) 'y)))

(check-error (parse "flag"))
(check-error (parse '(+ 1 2 3)))
(check-error (parse '(p 2 3)))
(check-error (parse '(foo (bar 12 3))))

(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))


; SL -> BSL-expr
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(and (= L 2) (symbol? (first s)))
       (make-fun (first s) (parse (second s)))]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))


; Atom -> BSL-expr
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(symbol? s) s]
    [(boolean? s) s]
    [(string? s) (error WRONG)]))


;===================================
; Interpreting the definition area |
;===================================

; BSL-def-con is a (list Symbol BSL-expr)
; BSL-def-fun is a (list Symbol Symbol BSL-expr)

; BSL-def is one of:
; – BSL-def-con
; – BSL-fun-con

; BSL-da is [List-of BSL-def]


; [List-of S-expr] -> BSL-da
(check-expect (da (list '(define x 10) '(define (foo x) (+ x 1))))
                  (list (list 'x 10) (list 'foo 'x (make-add 'x 1))))

(define (da ss)
  (map da-def ss))


; S-expr -> BSL-def
; create a constant definition or a function definition
; from an S-expression
(check-expect (da-def '(define x 10)) (list 'x 10))
(check-expect (da-def '(define x (+ y 1))) (list 'x (make-add 'y 1)))
(check-expect (da-def '(define (foo x) (+ x 1)))
              (list 'foo 'x (make-add 'x 1)))
(check-error (da-def '(def x 10)))
(check-error (da-def '(define foo x (+ x 1))))

(define (da-def s)
  (cond [(define-con? s)
         (list (second s) (parse (third s)))]
        [(define-fun? s)
         (list (first (second s)) (second (second s))
               (parse (third s)))]
        [else (error WRONG)]))


; S-expr -> Boolean
(define (define-con? s)
  (and (equal? (first s) 'define)
       (= (length s) 3)
       (symbol? (second s))))


; S-expr -> Boolean
(define (define-fun? s)
  (and (equal? (first s) 'define)
       (= (length s) 3)
       (list? (second s))))


;=============
; Evaluation |
;=============

; S-expr [List-of S-expr] -> BSL-value
; takes what represents an expression and what represents
; the definition area and computes the expression
(define defs '((define x 1)
               (define y (+ x 1))
               (define pi 3)
               (define (inc x) (+ x 1))
               (define (sqr foo) (* foo foo))))
; -IN-
(check-expect (interpreter 1 defs) 1)
(check-expect (interpreter 'pi defs) 3)
(check-expect (interpreter '(+ x y) defs) 3)
(check-expect (interpreter '(+ x (* y 3)) defs) 7)
(check-expect (interpreter '(inc 2) defs) 3)
(check-expect (interpreter '(inc pi) defs) 4)
(check-expect (interpreter '(+ (sqr (* (inc x) (inc 3))) y) defs)
              66)

(define (interpreter se lse)
  (eval (parse se) (da lse)))


; BSL-expr BSL-da -> BSL-value
(define (eval e da)
  (cond [(number? e) e]
        [(boolean? e) e]
        [(symbol? e) (eval (second (assq e da)) da)]
        [(add? e) (+ (eval (add-left e) da)
                     (eval (add-right e) da))]
        [(mul? e) (* (eval (mul-left e) da)
                     (eval (mul-right e) da))]
        [(fun? e) (eval-fun (assq (fun-name e) da)
                            (eval (fun-expr e) da)
                            da)]
        [else (error WRONG)]))


; BSL-def-fun BSL-value BSL-da -> BSL-value
; apply a function to a given argument
(define (eval-fun f arg da)
  (local ((define param (second f))
          (define body  (third f))
          (define new-da (cons (list param arg) da)))
    ; -IN-
    (eval body new-da)))

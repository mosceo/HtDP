;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))
(define ex5 'λ)
(define ex6 '(λ λ))
(define ex7 '(λ (x) (λ (x) (x x))))
(define ex8 '(λ (x) (x x)))


; Lam -> Boolean
; is the expression a variable?
(check-expect (is-var? ex4) #false)
(check-expect (is-var? ex5) #true)
(check-expect (is-var? ex6) #false)

(define (is-var? x)
  (symbol? x))


; Lam -> Boolean
; is the expression a lambda-expression?
(check-expect (is-λ? ex1) #true)
(check-expect (is-λ? ex3) #true)
(check-expect (is-λ? ex4) #false)
(check-expect (is-λ? ex5) #false)

(define (is-λ? x)
  (and (cons? x) (= (length x) 3)))


; Lam -> Boolean
; is the expression an application of a function?
(check-expect (is-app? ex3) #false)
(check-expect (is-app? ex4) #true)
(check-expect (is-app? ex5) #false)
(check-expect (is-app? ex6) #true)

(define (is-app? x)
  (and (cons? x) (= (length x) 2)))


; Lam -> Symbol
; extract the parameter from a lambda-expression
(check-expect (λ-para ex1) 'x)

(define (λ-para x)
  (first (second x)))


; Lam -> Lam
; extract the body from a λ expression
(check-expect (λ-body ex7) ex8)

(define (λ-body x)
  (third x))


; Lam -> Symbol
; extract the function from an application
(check-expect (app-fun '(x y)) 'x)

(define (app-fun x)
  (first x))


; Lam -> Symbol
; extract the function from an application
(check-expect (app-arg '(x y)) 'y)

(define (app-arg x)
  (second x))



(symbol-append a b)
(define (undeclareds le0)
  (local (; Lam [List-of Symbol] -> Lam
          ; accumulator declareds is a list of all λ 
          ; parameters on the path from le0 to le
          (define (undeclareds/a le declareds)
            (cond
              [(is-var? le)
               (if (member? le declareds)
                   (symbol-append '*DEC: le)
                   (symbol-append '*UNDEC: le))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para declareds)))
                 (list 'λ (list para)
                       (undeclareds/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
                 (list (undeclareds/a fun declareds)
                       (undeclareds/a arg declareds)))])))
    (undeclareds/a le0 '())))


(check-expect (symbol-append 'foo 'bar) 'foobar)
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))


(define ex100 '(λ (*undeclared) ((λ (x) (x *undeclared)) y)))
(undeclareds ex100)




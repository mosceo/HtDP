;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An Atom is one of: 
; – Number
; – String
; – Symbol

; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)


; Any -> Boolean
; checks if a given object is of type atom

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)))


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count 3 'a) 0)
(check-expect (count '(a b a) 'a) 2)
(check-expect (count '(((foo 1 2) "apple" foo) bar ((foo))) 'foo) 3)

(define (count sexp sy)
  (local (; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))
          ; Atom Symbol -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    ; -IN-          
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))


; S-expr -> Number
; Consumes an S-expression and determines its depth. An atom has a depth of 1.
; The depth of a list of S-expressions is the maximum depth of its items plus 1.
(check-expect (depth 3) 1)
(check-expect (depth '(a b c)) 2)
(check-expect (depth '(a (b (foo)))) 4)

(define (depth sexp)
  (cond [(atom? sexp) 1]
        [else (+ (lmax (map depth sexp)) 1)]))


; [List-of Number] -> Number
; determines a maximum number in a list
(check-expect (lmax (list 1)) 1)
(check-expect (lmax (list 1 2 3)) 3)
(check-expect (lmax (list 1 5 3)) 5)

(define (lmax l)
  (foldr max (first l) (rest l)))


; S-expr Symbol Symbol -> SL
; Consumes an S-expression s and two symbols, old and new.
; The result is like s with all occurrences of old replaced by new.
(check-expect (substitute 'a 'a 'b) 'b)
(check-expect (substitute '(a) 'a 'b) '(b))
(check-expect (substitute '(a (1 (a) b) (30 (a))) 'a 'x)
              '(x (1 (x) b) (30 (x))))

(define (substitute sexp old new)
  (cond [(atom? sexp) (if (equal? sexp old) new sexp)]
        [else (map (lambda (s) (substitute s old new))
                   sexp)]))


;------------------
; Reformulation 1 |
;------------------

; An S-expr is one of:
; – Number
; – String
; – Symbol
; – [List-of S-expr]


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count-2 3 'a) 0)
(check-expect (count-2 '(a b a) 'a) 2)
(check-expect (count-2 '(((foo 1 2) "apple" foo) bar ((foo))) 'foo) 3)

(define (count-2 sexp sy)        
  (cond [(number? sexp) 0]
        [(string? sexp) 0]
        [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
        [else (foldr    ; sum numbers in list
               + 0
               (map (lambda (s) (count-2 s sy)) sexp))]))


;------------------
; Reformulation 2 |
;------------------

; An [S-expr X Y Z ...] is one of: 
; – X
; – Y
; – Z
; – ...
; – SL

; An SL is one of: 
; – '()
; – (cons S-expr SL)


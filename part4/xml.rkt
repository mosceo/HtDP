;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require 2htdp/image)


;===========================================================
; 22.1  X-expressions
;===========================================================

; An Xexpr-attr-content is a list:
; – [List-of Xexpr]
; – (cons [List-of Attribute] [List-of Xexpr])

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An Xexpr is a list:
; – (cons Symbol Xexpr-attr-content)


(define a0 '((initial "X")))
 
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))


; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))


; [List-of Attribute] or Xexpr -> Boolean
; is the given value a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [(cons? (first x)) #true]
    [else #false]))


; Xexpr -> Symbol
; extracts the name of an element
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe)
  (first xe))


; Xexpr -> [List-of Xexpr]
; extracts the name of an element
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))

(define (xexpr-content xe)
  (local ((define attr-content (rest xe)))
    (cond [(empty? attr-content) '()]
          [(list-of-attributes? (first attr-content))
           (rest attr-content)]
          [else attr-content])))


; [List-of Attribute] Symbol -> [Maybe String]
; If the attributes list associates the symbol with a string,
; the function retrieves this string; otherwise it returns #false.
(check-expect (find-attr 'foo '((foo 1) (bar 2) (xyz 3))) 1)
(check-expect (find-attr 'xyz '((foo 1) (bar 2) (xyz 3))) 3)
(check-expect (find-attr 'gen '((foo 1) (bar 2) (xyz 3))) #false)

(define (find-attr s loa)
  (local ((define res (assq s loa)))
    (cond [(boolean? res) res]
          [else (second res)])))


;===========================================================
; 22.2  XML Enumerations
;===========================================================

; An XWord is '(word ((text String)))

(define xw1 '(word ((text ""))))
(define xw2 '(word ((text "apple"))))
(define xw3 '(word ((text "apple tree"))))


; Xexpr -> Boolean
; check if a given Xexpr is an XWord
(check-expect (word? xw1) #true)
(check-expect (word? '(word)) #false)
(check-expect (word? '(word ((tex "apple")))) #false)
(check-expect (word? '(word ((text "apple") (sec "str")))) #false)
(check-expect (word? '(word ((text "apple")) (word))) #false)
 
(define (word? xe)
  (local ((define name (xexpr-name xe))
          (define attr (xexpr-attr xe))
          (define cont (xexpr-content xe)))
    (and (equal? name 'word)
         (equal? (length attr) 1)
         (equal? (first (first attr)) 'text)
         (empty? cont))))


; XWord -> String
; extract the value of the only attribute 'text'
(check-expect (word-text xw1) "")
(check-expect (word-text xw2) "apple")

(define (word-text xw)
  (find-attr 'text (xexpr-attr xw)))


; An XItem is one of: 
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum)))
; 
; An XEnum is one of:
; – (cons 'ul [List-of XItem])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem]))

(define en0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define en0-nest
  '(ul
    (li (word ((text "one"))))
    (li (ul
         (li (word ((text "nest-one"))))
         (li (word ((text "nest-two"))))))
    (li (word ((text "two"))))))

(define li1 '(li (word ((text "one")))))
(define li2 '(li ((attr "a")) (word ((text "two")))))
(define en1 `(ul ,li1 ,li2))

(define li3 `(li ,en1))
(define en2 `(ul ,li1 ,li3))


(define SIZE 18)
(define COLOR "black")
(define BULLET (beside (circle 3 'solid 'black)
                       (text " " SIZE COLOR)))


; XItem -> Image
; renders one XItem as an image
(define (render-item item)
  (local ((define content (first (xexpr-content item))))
    (beside/align
     'center BULLET
     (cond
       [(word? content)
        (text (word-text content) SIZE 'black)]
       [else (render-enum content)]))))


; XEnum -> Image
; renders an XEnum as an image
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem Image -> Image 
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

 
; Image -> Image
; marks item with bullet  
(define (bulletize item)
  (beside/align 'center BULLET item))


;(render-enum en0-nest)
;(render-enum en2)


; XEnum -> Number
; count all "hello" that appear in an enumeration
(check-expect (count-hello
               '(ul
                 (li (word ((text "one"))))
                 (li (word ((text "two")))))) 0)
(check-expect (count-hello
               '(ul
                 (li (word ((text "hello"))))
                 (li (word ((text "two"))))
                 (li (word ((text "hello")))))) 2)
(check-expect (count-hello
               '(ul
                 (li (word ((text "hello"))))
                 (li (ul
                      (li (word ((text "one"))))
                      (li (word ((text "two"))))
                      (li (word ((text "hello"))))))
                 (li (word ((text "hello")))))) 3)

(define (count-hello en)
  (local ((define content (xexpr-content en)))
    (for/sum ([item content])
      (local ((define m-word (first (xexpr-content item))))
        (cond [(word? m-word)
               (if (string=? (word-text m-word) "hello") 1 0)]
              [else (count-hello item)])))))


;===========================================================
; 22.3 DSL
;===========================================================

; A FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (list FSM-State FSM-State)
; A FSM-State is a String that specifies a color

; data examples 
(define fsm-traffic
  '(("red" "green")
    ("green" "yellow")
    ("yellow" "red")))


; FSM FSM-State -> FSM-State 
; match the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw
             (lambda (current)
               (overlay (text current 18 "black")
                        (square 100 "solid" current)))]
            [on-key
             (lambda (current key-event)
               (find transitions current))]))


; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find '(("a" "A") ("b" "B")) "a") "A")
(check-expect (find '(("a" "A") ("b" "B")) "b") "B")
(check-error  (find '(("a" "A") ("b" "B")) "c"))

(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))


; An XMachine is a nested list of this shape:
; `(machine ((initial ,FSM-State)) [List-of X1T])
; An X1T is a nested list of this shape:
; `(action ((state ,FSM-State) (next ,FSM-State)))

; data examples 
(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))


; XMachine -> FSM-State 
; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))


; XMachine -> FSM-State 
; extracts and translates the transition table from xm0
(check-expect (xm-state0 xm0) "red")

(define (xm-state0 xm0)
  (find-attr 'initial (xexpr-attr xm0)))


; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)

(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr 'state (xexpr-attr xa))
                  (find-attr 'next  (xexpr-attr xa)))))
    (map xaction->action (xexpr-content xm))))


; run
;(simulate "red" fsm-traffic)


;===========================================================
; 22.3 Reading XML
;===========================================================

; read Xexpr from file
;(simulate-xmachine
; (read-plain-xexpr "files/machine-configuration.xml"))


(define URL "files/stock.xml")
(define FONTSIZE 22) ; font size 
             
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
             
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local (; [StockWorld -> StockWorld]
          (define (retrieve-stock-data __w)
            (local ((define x (read-plain-xexpr URL)))
              (make-data (get x co "price")
                         (get x co "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    (define (word sel col)
                      (text (sel w) FONTSIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    ; -- IN --
    (big-bang (retrieve-stock-data 'no-use)
              [on-tick retrieve-stock-data 15]
              [to-draw render-stock-data])))


; Xexpr String String -> String
; from a given X-expression extracts the value
; of a certain property for a given company

(define (get x co attr)
  "nice")

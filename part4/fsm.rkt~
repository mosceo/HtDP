;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname xml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
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
  

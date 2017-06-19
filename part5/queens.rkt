;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;===========================
; The n queens puzzle
;===========================
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)


; A Queen is a Posn
; interpretation: a queen is represented by its position on
; the board as (make-posn column row)

; Board is a structure:
;  (make-board Number [List-of Queen])
; interpretation: the number is the size of a square board,
;  the list contains the queens on the board
; constraint: no queen on the board threatens any other
(define-struct board (size queens))


; Example #1 of a board having this layout:

; ■ ■ ◎ ■
; ◎ ■ ■ ■
; ■ ■ ■ □
; ■ ◎ ■ ■

(define loq-ex1 (list (make-posn 2 0) (make-posn 0 1) (make-posn 1 3)))
(define board-ex1 (make-board 4 loq-ex1))


;===========================
; Board
;===========================

; N -> Board 
; creates the initial n by n board
(define (board0 n)
  (make-board n empty))


; Board Queen -> Board 
; add a queen to a board
; constraint: the board should remain valid after this operation,
;  nothing is checked by the function
(check-expect (add-queen (make-board 4 (list (make-posn 3 4))) (make-posn 0 0))
              (make-board 4 (list (make-posn 0 0) (make-posn 3 4))))

(define (add-queen board queen)
  (make-board (board-size board)
              (cons queen (board-queens board))))


; Board Queen -> Board 
; check if a queen can be added to a board
(define (board-add? board queen)
  (and (board-fit? board queen)
       (not (list-threatens? (board-queens queen)))))


; Board Queen -> Boolean
(check-expect (board-fit? (board0 10) (make-posn 0 0)) #true)
(check-expect (board-fit? (board0 10) (make-posn 3 5)) #true)
(check-expect (board-fit? (board0 10) (make-posn 10 5)) #false)
(check-expect (board-fit? (board0 10) (make-posn 3 10)) #false)

(define (board-fit? board queen)
  (and (< (posn-x queen) (board-size board))
       (< (posn-y queen) (board-size board))))


; Queen Queen -> Boolean
; check if two queens threaten each other
; (taking the same spot means threat)
(check-expect (threatens? (make-posn 3 5) (make-posn 3 5)) #true)
(check-expect (threatens? (make-posn 3 5) (make-posn 3 10)) #true)
(check-expect (threatens? (make-posn 3 5) (make-posn 1 5)) #true)
(check-expect (threatens? (make-posn 3 5) (make-posn 5 7)) #true)
(check-expect (threatens? (make-posn 3 5) (make-posn 5 3)) #true)
(check-expect (threatens? (make-posn 3 5) (make-posn 1 7)) #true)

(define (threatens? q1 q2)
  (or (= (posn-x q1) (posn-x q2))
      (= (posn-y q1) (posn-y q2))
      (= (abs (- (posn-x q1) (posn-x q2)))
         (abs (- (posn-y q1) (posn-y q2))))))


; [List-of Queen] Queen -> Boolean
; check if a given queen is threatened by any queen in the list
(check-expect (list-threatens? loq-ex1 (make-posn 1 1)) #true)
(check-expect (list-threatens? loq-ex1 (make-posn 2 3)) #true)
(check-expect (list-threatens? loq-ex1 (make-posn 3 2)) #false)

(define (list-threatens? loq queen)
  (ormap (lambda (q) (threatens? q queen)) loq))


;===========================
; Graphics
;===========================

; board size
(define SIZE 10)
; size of a square in pixels
(define WIDTH 50)
; width of a separating line in pixels
(define LINE-WIDTH 3)
; image of a queen
(define QUEEN (bitmap "images/queen.png"))
; image of a square
(define SQUARE (overlay (square WIDTH "solid" "white")
                        (square (+ WIDTH 2) "solid" "black")))
; image of a threatened square
(define SQUARE-THREAT (overlay (square WIDTH "solid" "lightgray")
                               (square (+ WIDTH 2) "solid" "black")))
; image of a taken square
(define SQUARE-QUEEN (overlay QUEEN SQUARE-THREAT))




; Number Number -> Image
; create the image of an empty board

(define (board->image board)
  (local ((define length (+ (* size (+ width 1)) 1))
          (define scene (square length "solid" "yellow")))
    (add-line scene 0 0 0 (+ length 1) "black")))








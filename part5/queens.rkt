;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname queens) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;===========================
; THE N QUEENS PUZZLE
;===========================
;
; Given a chess board, put as many queens on it as you can.
; By clicking on a square you can put or remove a queen.
; The squares that are threatened by current queens have
; a distinguished color, you can't put new queens on them.
; There is also a function that solves the puzzle
; by producing a board with queens.
;
;===========================
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)


; A Square is a Posn
; interpretation: (make-posn col row) denotes a square on the board

; A Queen is a Square

; A Size is a Positive Number

; Board is a structure:
;  (make-board Size [List-of Queen])
; interpretation: a board is represented by its size and a list of queens
; constraint: no queen on the board can threaten any other
(define-struct board (size queens))


; Example #1 of a board with the following layout:
; ■ ■ ◎ ■
; ◎ ■ ■ ■
; ■ ■ ■ □
; ■ ◎ ■ ■

(define loq-ex1 (list (make-posn 2 0) (make-posn 0 1) (make-posn 1 3)))
(define board-ex1 (make-board 4 loq-ex1))

; Example #2 of a board with the following layout:
; □

(define board-ex2 (make-board 1 empty))

; Example #3 of a board with the following layout:
; ■ ■ ◎ ■
; ◎ ■ ■ ■
; ■ ■ ■ ◎
; ■ ◎ ■ ■

(define loq-ex3 (list (make-posn 3 2) (make-posn 2 0) (make-posn 0 1) (make-posn 1 3)))
(define board-ex3 (make-board 4 loq-ex3))


;===========================
; Board
;===========================

; Size -> Board 
; creates the initial n by n board
(define (board0 n)
  (make-board n empty))


; Board Queen -> Board 
; add a queen to a board
; constraint: the board should remain valid after this operation
;  (nothing is checked by the function!)
(check-expect (add-queen (make-board 4 (list (make-posn 3 4))) (make-posn 0 0))
              (make-board 4 (list (make-posn 0 0) (make-posn 3 4))))

(define (add-queen board queen)
  (make-board (board-size board)
              (cons queen (board-queens board))))


; Board Queen -> Board 
; check if a queen can be added to a board
(check-expect (add-queen? board-ex1 (make-posn 1 2)) #false)
(check-expect (add-queen? board-ex1 (make-posn 3 2)) #true)

(define (add-queen? board queen)
  (and (fit? board queen)
       (not (square-threat? board queen))))


; Board Queen -> Board 
; remove a queen from a board
(check-expect (remove-queen board-ex3 (make-posn 3 2)) board-ex1)

(define (remove-queen board queen)
  (make-board (board-size board)
              (remove queen (board-queens board))))


; Board Square -> Boolean
; check if a square is part of a given board
(check-expect (fit? (board0 8) (make-posn 0 0)) #true)
(check-expect (fit? (board0 8) (make-posn 3 5)) #true)
(check-expect (fit? (board0 8) (make-posn 8 5)) #false)
(check-expect (fit? (board0 8) (make-posn 3 8)) #false)

(define (fit? board square)
  (and (< (posn-x square) (board-size board))
       (< (posn-y square) (board-size board))))


; Board Square -> Boolean
(check-expect (square-threat? board-ex1 (make-posn 2 0)) #true)
(check-expect (square-threat? board-ex1 (make-posn 1 3)) #true)
(check-expect (square-threat? board-ex1 (make-posn 3 2)) #false)

(define (square-threat? board square)
  (list-queen-path? (board-queens board) square))


; Board Square -> Boolean
(check-expect (square-queen? board-ex1 (make-posn 2 0)) #true)
(check-expect (square-queen? board-ex1 (make-posn 1 3)) #true)
(check-expect (square-queen? board-ex1 (make-posn 2 3)) #false)
(check-expect (square-queen? board-ex1 (make-posn 1 2)) #false)

(define (square-queen? board square)
  (member? square (board-queens board)))


; [List-of Queen] Square -> Boolean
; check if a given square is threatened by any queen in the list
(check-expect (list-queen-path? loq-ex1 (make-posn 1 1)) #true)
(check-expect (list-queen-path? loq-ex1 (make-posn 2 3)) #true)
(check-expect (list-queen-path? loq-ex1 (make-posn 3 2)) #false)

(define (list-queen-path? los square)
  (ormap (lambda (s) (queen-path? s square)) los))


; Square Square -> Boolean
; check if there is a queen-path from one square to the other
(check-expect (queen-path? (make-posn 3 5) (make-posn 3 5)) #true)
(check-expect (queen-path? (make-posn 3 5) (make-posn 3 10)) #true)
(check-expect (queen-path? (make-posn 3 5) (make-posn 1 5)) #true)
(check-expect (queen-path? (make-posn 3 5) (make-posn 5 7)) #true)
(check-expect (queen-path? (make-posn 3 5) (make-posn 5 3)) #true)
(check-expect (queen-path? (make-posn 3 5) (make-posn 1 7)) #true)

(define (queen-path? s1 s2)
  (or (= (posn-x s1) (posn-x s2))
      (= (posn-y s1) (posn-y s2))
      (= (abs (- (posn-x s1) (posn-x s2)))
         (abs (- (posn-y s1) (posn-y s2))))))


;===========================
; Graphics
;===========================

; board size
(define SIZE 8)
; size of a square without the border in pixels
(define WIDTH 50)
; each cell will have a 1 pixel border,
; so its real width on will differ by 2 pixels
(define BWIDTH 52)
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
  (local ((define size (board-size board))
          (define list-of-lines
            (for/list ([j size]) (line->image board j))))
    (foldr above empty-image list-of-lines)))


; Number Number -> Image
; create the image of an empty board
(define (line->image board j)
  (local ((define size (board-size board))
          (define list-of-squares
            (for/list ([i size]) (square->image board i j))))
    (foldr beside empty-image list-of-squares)))


; Board Number Number -> Image
; create an image for a square
(define (square->image board i j)
  (cond [(square-queen? board (make-posn i j)) SQUARE-QUEEN]
        [(square-threat? board (make-posn i j)) SQUARE-THREAT]
        [else SQUARE]))


;===========================
; Big-bang
;===========================

; Board Number Number MouseEvent
; when a user clicks on a square, try to add a queen there,
; if not possible do nothing
(define (mouse-h board x y event)
  (cond [(mouse=? event "button-down")
         (click-on-square board (coords->square x y))]
        [else board]))


; Board Square -> Board
; put a queen on a given square if possible,
; otherwise do nothing
(define (click-on-square board square)
  (cond [(square-queen? board square) (remove-queen board square)]
        [(add-queen? board square) (add-queen board square)]
        [else board]))


; Number Number -> Square
; determine the square that contains these pixel-coordinates
(check-expect (coords->square (/ BWIDTH 2) (* BWIDTH 1.5)) (make-posn 0 1))

(define (coords->square x y)
  (make-posn (floor (/ x BWIDTH))
             (floor (/ y BWIDTH))))

;-------------------
; LAUNCH THE WORLD |
;-------------------
(big-bang
 (board0 SIZE)
 [on-mouse mouse-h]
 [to-draw board->image])


;===========================
; Solver
;===========================

; Number -> Board
; solve a board of a given size (find a board of size N with N queens)
; produce #false if a board can't be solved (for example, a board of size 2)
(check-expect (length (board-queens (solve 1))) 1)
(check-expect (solve 2) #false)
(check-expect (solve 3) #false)
(check-expect (length (board-queens (solve 4))) 4)
(check-expect (length (board-queens (solve 5))) 5)

(define (solve size)
  (solve-board (board0 size) 0))


; Board Number -> [Maybe Board]
; given a partially solved board with queens on the first n columns,
; solve this board or return #false
(check-expect (solve-board board-ex1 2) #false)
(check-expect (solve-board board-ex1 3) board-ex3)

(define (solve-board board n)
  (cond [(>= n (board-size board)) board]
        [else
         (local ((define candidates (put-queen-column board n)))
           (r-ormap (lambda (c) (solve-board c (add1 n))) candidates))]))


; Examples to test put-queen-column
(define board-solve-0 (make-board 4 (list (make-posn 0 1))))
(define board-solve-1 (list (make-board 4 (list (make-posn 1 3)
                                                (make-posn 0 1)))))
(define board-solve-2 (list (make-board 4 (list (make-posn 2 0) (make-posn 0 1)))
                            (make-board 4 (list (make-posn 2 2) (make-posn 0 1)))))
(define board-solve-3 (list (make-board 4 (list (make-posn 3 0) (make-posn 0 1)))
                            (make-board 4 (list (make-posn 3 2) (make-posn 0 1)))
                            (make-board 4 (list (make-posn 3 3) (make-posn 0 1)))))

; Board Number -> [List-of Board]
; given a board with a free column, produce a list
; of possible boards with a queen at that column
(check-expect (put-queen-column board-ex1 2) '())
(check-expect (put-queen-column board-solve-0 1) board-solve-1)
(check-expect (put-queen-column board-solve-0 2) board-solve-2)
(check-expect (put-queen-column board-solve-0 3) board-solve-3)

(define (put-queen-column board i)
  (filter board?
          (for/list ([j (board-size board)])
            (local ((define candidate (make-posn i j)))
              (cond [(add-queen? board candidate) (add-queen board candidate)]
                    [else #false])))))


; [Any -> Any] [List-of Any] -> [Maybe Any]
; produce the first item in the list for which the given function
; evaluates to not false, otherwise produce false
(check-expect (r-ormap odd? (list)) #false)
(check-expect (r-ormap (lambda (x) (if (= x 2) 200 #false)) (list 1 3 5)) #false)
(check-expect (r-ormap (lambda (x) (if (= x 2) 200 #false)) (list 1 3 2)) 200)

(define (r-ormap f lst)
  (cond [(empty? lst) #false]
        [else (local ((define val (f (first lst))))
                (cond [(equal? val #false) (r-ormap f (rest lst))]
                      [else val]))]))


;----------------------
; SHOW A SOLVED BOARD |
;----------------------
;(board->image (solve 12))

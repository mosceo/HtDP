;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;===============================================================================
; TETRIS
;===============================================================================
; A simple tetris game.


(require 2htdp/image)
(require 2htdp/universe)


;===============================================================================
; World constants
;===============================================================================

; size of the board in number of blocks
(define BW 20)
(define BH 30)
; size of one block in pixels
(define W 20)

(define BLOCK (square W "solid" "blue"))
(define EMPTY-BOARD (empty-scene (* BW W) (* BH W)))


;===============================================================================
; Data definitions
;===============================================================================

;;;; Block

; A Block is a Posn
; a block on the board with certain board coordinates
; example: (make-posn 0 4) represents a block at (0, 4)

; this wrapper will help us create blocks easier
(define (b x y)
  (make-posn x y))


;;;; Piece

(define-struct piece [size blocks])
; A Piece is a structure:
;   (make-piece Number List-of-Blocks)
; a piece for the game; we encode a piece as a bunch of blocks
; in a square of a specified size

; □■□
; ■■■
; □□□
(define PIECE1 (make-piece 3 (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))))


;;; GamePiece

(define-struct gamepiece [pos piece])
; A GamePiece is a structure:
;   (make-gamepiece Posn Piece)
; represents a piece in a real game; the Posn is the position of the piece
; on the board and all coordinates in the piece are relative to this position


;;;; Board

; A Row is a List-of-Numbers
; a list of x-coordinates that are occupied by blocks at that row
; example: (list 3 1 7)

; A Board is a List-of-Rows
; a list of rows represents a board, with row 0 being the topmost
; example: (list (list)
;                (list 0 3)
;                (list 5 2 1))
; this data structure represents a board of height 3, with no blocks
; at row 0, three blocks at row 1 and two blocks at row 2


;;;; Game

(define-struct game [board gamepiece over?]) 
; A Game is a structure:
;   (make-game Board GamePiece Boolean)
; represents a state of the game with a given board, a moving piece
; and a boolean which defines whether the game is over


;===============================================================================
; Draw
;===============================================================================

; Game -> Image
; produce an image of a given game (board and piece)
(define (drawh g)
  (draw-gamepiece (game-gamepiece g)
                  (draw-board (game-board g) EMPTY-BOARD)))


; Board Image -> Image
; draw a given board (a board is just a list of rows)
(define (draw-board board im)
  (cond [(empty? board) im]
        [else (draw-board (rest board)
                          (draw-row (first board) im))]))


; Row Image -> Image
; draw a row on the board
(define (draw-row row im)
  (draw-blocks row im))


; List-of-Blocks Image -> Image
; draw a list of blocks on the board
(define (draw-blocks bs im)
  (cond [(empty? bs) im]
        [else (draw-blocks (rest bs)
                           (draw-block (first bs) im))]))


; Block Image -> Image
; draw a block on the board
(define (draw-block b im)
  (place-image BLOCK (x b) (y b) im))


; Block -> Number
; compute canvas x-coordinate for a given block
(define (x b)
  (+ (* W (posn-x b)) (/ W 2)))


; Block -> Number
; compute canvas y-coordinate for a given block
(define (y b)
  (+ (* W (posn-y b)) (/ W 2)))


; GamePiece Image -> Image
; draw a given piece on the board
(define (draw-gamepiece gamepiece im)
  (draw-blocks (gamepiece->blocks gamepiece) im))


; GamePiece -> List-of-Blocks
; produce a list of blocks that represent a given piece on the board
(check-expect (gamepiece->blocks (make-gamepiece (make-posn 2 5) PIECE1))
              (list (b 3 5) (b 2 6) (b 3 6) (b 4 6))) 

(define (gamepiece->blocks gp)
  (ladd (gamepiece-pos gp) (piece-blocks (gamepiece-piece gp))))


; Posn List-of-Posns -> List-of-Posns
; add a given posn to each posn in the list
(check-expect (ladd (b 1 2) (list (b 0 0) (b -1 4) (b 5 10)))
              (list (b 1 2) (b 0 6) (b 6 12)))

(define (ladd p lop)
  (cond [(empty? lop) '()]
        [else (cons (add p (first lop))
                    (ladd p (rest lop)))]))


; Posn Posn -> Posn
; add two posns
(check-expect (add (make-posn 1 10) (make-posn -2 5)) (make-posn -1 15))

(define (add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))


;===============================================================================
; Tick
;===============================================================================

; Game -> Game
; handle tick events
(define (tickh g)
  (cond [(game-over? g) g]
        [else (down-event g)]))


; Game -> Game
; handle the event when the piece has to be moved down
(define (down-event g)
  (cond [(down-event? g)
         (make-game (game-board g)
                    (gp-down (game-gamepiece g))
                    (game-over? g))]
        [else (land-event g)]))


; Game -> Game
; the piece lands and the next one appears (if no gameover)
(define (land-event g)
  (cond [(gp-legal? (game-gamepiece g) (game-board g))
         (land g)]
        [else (set-gameover (land g))]))


; Game -> Game
; current gamepiece is added to the board, new gamepiece begins falling
(define (land g)
  (make-game (gp+board (game-gamepiece g) (game-board g))
             (make-gamepiece (make-posn 5 -3) PIECE1)
             (game-over? g)))


; GamePiece Board -> Board
; land gamepiece on the board
(define (gp+board gp board)
  (blocks+board (gp->blocks gp) board))


; List-of-Blocks Board -> Board
; land blocks on the board
(define (blocks+board bs board)
  (cond [(empty? bs) board]
        [else (blocks+board (rest bs)
                            (block+board (first bs) board))]))

; Block -> Board
; land a given block on the board; if it is outside, do nothing
(define (block+board b board)
  (cond [(not (inside? b)) board]
        [else (add-to-row (posn-x b) (posn-y b) board)]))


; Number Number Board -> Board
; add a given number to a specified row of a given board
(check-expect (add-to-row 5 0 (list (list) (list 1) (list 2 3)))
              (list (list 5) (list 1) (list 2 3)))
(check-expect (add-to-row 5 1 (list (list) (list 1) (list 2 3)))
              (list (list) (list 5 1) (list 2 3)))
(check-expect (add-to-row 5 2 (list (list) (list 1) (list 2 3)))
              (list (list) (list 1) (list 5 2 3)))

(define (add-to-row n i board)
  (cond [(= i 0) (cons (cons n (first board))
                       (rest board))]
        [else (add-to-row n (- i 1) (rest board))]))


; Game -> Game
; set a given game as over
(define (set-gameover g)
  (make-game (game-board g)
             (game-gamepiece g)
             #true))


; Game -> Boolean
; check if the piece in a game can be moved down
(define (down-event? g)
  (gp-legal? (gp-down (game-gamepiece g)) (game-board g))) 


; GamePiece -> GamePiece
; move a gamepiece down
(check-expect (gp-down (make-gamepiece (make-posn 2 5) PIECE1))
              (make-gamepiece (make-posn 2 6) PIECE1))

(define (gp-down gp)
  (make-gamepiece (add (make-posn 0 1) (gamepiece-pos gp))
                  (gamepiece-piece gp)))


; GamePiece -> GamePiece
; move a gamepiece to the left
(check-expect (gp-left (make-gamepiece (make-posn 2 5) PIECE1))
              (make-gamepiece (make-posn 1 5) PIECE1))

(define (gp-left gp)
  (make-gamepiece (add (make-posn -1 0) (gamepiece-pos gp))
                  (gamepiece-piece gp)))


; GamePiece -> GamePiece
; move a gamepiece to the left
(check-expect (gp-right (make-gamepiece (make-posn 2 5) PIECE1))
              (make-gamepiece (make-posn 3 5) PIECE1))

(define (gp-right gp)
  (make-gamepiece (add (make-posn 1 0) (gamepiece-pos gp))
                  (gamepiece-piece gp)))


; GamePiece Board -> Boolean
; check if a given gamepiece fits on the given board
(define (gp-legal? gp board)
  (bs-legal? (gamepiece->blocks gp) board))

; List-of-Blocks Board -> Boolean
; check if a given list of blocks can be placed on a given board
(define (bs-legal? bs board)
  (cond [(empty? bs) #true]
        [else (and (legal? (first bs) board)
                   (bs-legal? (rest bs) board))]))


; Block Board -> Boolean
; check if a given block can be placed on a given board
(define (legal? b board)
  (and (inside? b) (fits? b board)))


; Block Board -> Boolean
; check if a given block doesn't overlap with some other block on a board
(check-expect (fits? (b 0 1) (list (list (b 0 1)) (list (b 1 1) (b 1 2))))
              #false)
(check-expect (fits? (b 1 2) (list (list (b 0 1)) (list (b 1 1) (b 1 2))))
              #false)
(check-expect (fits? (b 1 3) (list (list (b 0 1)) (list (b 1 1) (b 1 2))))
              #true)
(define (fits? b board)
  (cond [(empty? board) #true]
        [else [and (not (member b (first board)))
                   (fits? b (rest board))]]))


; Block -> Boolean
; check if a given block is inside the board
(check-expect (inside? (b 0 0)) #true)
(check-expect (inside? (b 1 1)) #true)
(check-expect (inside? (b 2 1000)) #false)
(check-expect (inside? (b -1 0)) #false)

(define (inside? b)
  (and (>= (posn-x b) 0) (< (posn-x b) BW)
       (>= (posn-y b) 0) (< (posn-y b) BH)))


;===============================================================================
; Key
;===============================================================================




;===============================================================================
; Launch the world
;===============================================================================


; Number -> Board
; create an empty board with a given number of rows
(check-expect (empty-board 0) (list))
(check-expect (empty-board 1) (list '()))
(check-expect (empty-board 3) (list '() '() '()))

(define (empty-board h)
  (cond [(= h 0) '()]
        [else (cons '() (empty-board (- h 1)))]))


(define INIT-GAME
  (make-game (empty-board BH) (make-gamepiece (make-posn 3 5) PIECE1) #false))


(big-bang
  INIT-GAME
 [on-tick tickh 0.35]
; [on-key keyh]
 [to-draw drawh])

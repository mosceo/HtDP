;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;=======================================
; TETRIS
;=======================================
; A simple tetris implementation.
;
; Author: Roman Kunin (mosceo@gmail.com)
; Source: https://github.com/mosceo/htdp

(require 2htdp/image)
(require 2htdp/universe)


;=======================================
; World constants
;=======================================

; size of the board in number of blocks
(define BW 12)
(define BH 20)
; size of one block in pixels
(define W 23)

(define BLOCK (overlay (square (- W 1) "solid" "blue")
                       (square W "solid" "white")))
(define EMPTY-BOARD (empty-scene (* BW W) (* BH W)))
(define GAME-OVER-IMAGE (text "GAME OVER" 18 "black"))
; how oftet a tick event fires (in sec)
(define RATE 0.25)


;=======================================
; Data definitions
;=======================================

;--------
; Block ;
;--------

; A Block is a Posn
; a block on the board with certain board coordinates
; example: (make-posn 0 4) represents a block at (0, 4)

; this wrapper will help create blocks easier
(define (b x y)
  (make-posn x y))


;--------
; Piece ;
;--------

(define-struct piece [size blocks])
; A Piece is a structure:
;   (make-piece Number List-of-Blocks)
; a piece for the game; we encode a piece as a bunch of blocks
; in a square of a specified size


;------------
; GamePiece ;
;------------

(define-struct gamepiece [pos piece])
; A GamePiece is a structure:
;   (make-gamepiece Posn Piece)
; represents a piece in a real game; the Posn is the position of the piece
; on the board and all coordinates in the piece are relative to this position


;--------
; Board ;
;--------

; A Row is a List-of-Numbers
; a list of x-coordinates that are occupied by blocks at that row
; example: (list 3 1 7)

; A Board is a List-of-Rows
; a list of rows represents a board, with row 0 being the topmost
; example: (list (list)
;                (list 0 3)
;                (list 5 2 1))
; this data structure represents a board of height 3, with no blocks
; at row 0, two blocks at row 1 and three blocks at row 2


;-------
; Game ;
;-------

(define-struct game [board gamepiece over?]) 
; A Game is a structure:
;   (make-game Board GamePiece Boolean)
; represents a state of the game with a given board, a moving piece
; and a boolean which defines whether the game is over


;=======================================
; Pieces
;=======================================

; □ ■ □
; ■ ■ ■
; □ □ □
(define PIECE1 (make-piece 3 (list (b 1 0) (b 0 1) (b 1 1) (b 2 1))))

; □ ■ □ □
; □ ■ □ □
; □ ■ □ □
; □ ■ □ □
(define PIECE2 (make-piece 4 (list (b 1 0) (b 1 1) (b 1 2) (b 1 3))))

; ■ ■
; ■ ■
(define PIECE3 (make-piece 2 (list (b 0 0) (b 0 1) (b 1 0) (b 1 1))))

; □ □ □
; ■ ■ ■
; □ □ ■
(define PIECE4 (make-piece 3 (list (b 0 1) (b 1 1) (b 2 1) (b 2 2))))

; □ □ □
; ■ ■ ■
; ■ □ □
(define PIECE5 (make-piece 3 (list (b 0 1) (b 1 1) (b 2 1) (b 0 2))))

; □ □ □
; ■ ■ □
; □ ■ ■
(define PIECE6 (make-piece 3 (list (b 0 1) (b 1 1) (b 1 2) (b 2 2))))

; □ □ □
; □ ■ ■
; ■ ■ □
(define PIECE7 (make-piece 3 (list (b 0 2) (b 1 1) (b 1 2) (b 2 1))))

; a list of all pieces
(define PIECES (list PIECE1 PIECE2 PIECE3 PIECE4 PIECE5 PIECE6 PIECE7))


;=======================================
; Create a game
;=======================================

; Number -> Board
; create an empty board with a given number of rows
(check-expect (empty-board 0) (list))
(check-expect (empty-board 1) (list '()))
(check-expect (empty-board 3) (list '() '() '()))

(define (empty-board h)
  (cond [(= h 0) '()]
        [else (cons '() (empty-board (- h 1)))]))


; None -> Piece
; get a random piece
(define (random-piece dummy)
  (ith (random (length PIECES)) PIECES))


; None -> Piece
; create a gamepiece in its initial position from a given piece
(define (new-gamepiece piece)
  (make-gamepiece (init-pos piece)
                  piece))


; Piece -> Pos
; compute the initial position for a piece
; (right above the board and in the middle)
(define (init-pos piece)
  (make-posn (quotient (- BW (piece-size piece)) 2)
             (- (piece-size piece))))


; Number List -> Any
; get an element from a list by index
(check-expect (ith 0 (list 1 2 3)) 1)
(check-expect (ith 2 (list 1 2 3)) 3)

(define (ith i lst)
  (cond [(= i 0) (first lst)]
        [else (ith (- i 1) (rest lst))]))


(define INIT-GAME
  (make-game (empty-board BH) (new-gamepiece (random-piece 0)) #false))


;=======================================
; Draw
;=======================================

; Game -> Image
; produce an image of a given game
(define (drawh g)
  (cond [(game-over? g) (draw-gameover (draw-board-gp g))]
        [else (draw-board-gp g)]))


; Game -> Image
; draw the board and gamepiece for a given game
(define (draw-board-gp g)
  (draw-gamepiece (game-gamepiece g)
                  (draw-board (game-board g) EMPTY-BOARD)))


; Image -> Image
(define (draw-gameover im)
  (overlay GAME-OVER-IMAGE im))


; Board Image -> Image
; draw a given board
(define (draw-board board im)
  (draw-rows board 0 im))


; List-of-Rows Image -> Image
; draw a list of rows, where n is the index of the first row
(define (draw-rows rows n im)
  (cond [(empty? rows) im]
        [else (draw-rows (rest rows) (+ n 1)
                         (draw-row (first rows) n im))]))


; Row Image -> Image
; draw a row on the board
(define (draw-row row n im)
  (cond [(empty? row) im]
        [else (draw-row (rest row) n (draw-block-xy (first row) n im))]))


; Number Number Image -> Image
; draw a block on the board
(define (draw-block-xy x y im)
  (place-image BLOCK (cx x) (cy y) im))


; Number -> Number
; compute a canvas x-coordinate from a board x-coordinate
(define (cx x)
  (+ (* W x) (/ W 2)))


; Number -> Number
; compute a canvas y-coordinate from a board y-coordinate
(define (cy y)
  (+ (* W y) (/ W 2)))


; GamePiece Image -> Image
; draw a given piece on the board
(define (draw-gamepiece gamepiece im)
  (draw-blocks (gp->blocks gamepiece) im))


; Row Image -> Image
; draw a row on the board
(define (draw-blocks bs im)
  (cond [(empty? bs) im]
        [else (draw-blocks (rest bs)
                           (draw-block (first bs) im))]))


; Block Image -> Image
; draw a block on the board
(define (draw-block b im)
  (draw-block-xy (posn-x b) (posn-y b) im))


; GamePiece -> List-of-Blocks
; produce a list of blocks that represent a given piece on the board
(check-expect (gp->blocks (make-gamepiece (make-posn 2 5) PIECE1))
              (list (b 3 5) (b 2 6) (b 3 6) (b 4 6))) 

(define (gp->blocks gp)
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


;=======================================
; Tick
;=======================================

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
  (cond [(gp-fits? (game-gamepiece g) (game-board g))
         (land g)]
        [else (set-gameover (land g))]))


; Game -> Game
; add current gamepiece to the board, new gamepiece begins falling
(define (land g)
  (make-game (clean (gp+board (game-gamepiece g) (game-board g)))
             (new-gamepiece (random-piece 2))
             (game-over? g)))


; Board -> Board
; remove full rows in a given board
(define (clean board)
  (replenish (remove-full-rows board)))


; Board -> Board
; if a board lacks row, add empty rows to the top
(define (replenish board)
  (cond [(< (length board) BH) (replenish (cons '() board))]
        [else board]))

  
; Board -> Board
; remove full rows in a given board (it may lack rows afterward)
(define (remove-full-rows board)
  (cond [(empty? board) '()]
        [(full-row? (first board)) (remove-full-rows (rest board))]
        [else (cons (first board) (remove-full-rows (rest board)))]))


; Row -> Boolean
; check if a row is full with blocks
(define (full-row? row)
  (= BW (length row)))


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
        [else (cons (first board)
                    (add-to-row n (- i 1) (rest board)))]))


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
; check if a given gamepiece is legally positioned
; (doesn't overlap with other blocks and inside/above the board)
(define (gp-legal? gp board)
  (bs-legal? (gp->blocks gp) board))


; GamePiece Board -> Boolean
; check if a given gamepiece can land on the board
(define (gp-fits? gp board)
  (bs-fit? (gp->blocks gp) board))


; GamePiece Board -> Boolean
; check if given blocks can land on the board
(define (bs-fit? bs board)
  (cond [(empty? bs) #true]
        [else (and (fit? (first bs) board)
                   (bs-fit? (rest bs) board))]))


; GamePiece Board -> Boolean
; check if given blocks can land on the board
(define (fit? b board)
  (and (inside? b) (nooverlap? b board)))


; List-of-Blocks Board -> Boolean
; check if a given list of blocks are legally positioned
(define (bs-legal? bs board)
  (cond [(empty? bs) #true]
        [else (and (legal? (first bs) board)
                   (bs-legal? (rest bs) board))]))


; Block Board -> Boolean
; check if a given block can be placed on a given board
(define (legal? b board)
  (and (inside/above? b) (nooverlap? b board)))


; Block Board -> Boolean
; check if a given block doesn't overlap with some other block on a board
(check-expect (nooverlap? (b 0 0) (list (list 0) (list 1 3) (list 7 4 2)))
              #false)
(check-expect (nooverlap? (b 4 2) (list (list 0) (list 1 3) (list 7 2 4)))
              #false)
(check-expect (nooverlap? (b 2 1) (list (list 0) (list 1 3) (list 7 4 2)))
              #true)
(check-expect (nooverlap? (b 0 -1) (list (list 0) (list 1 3) (list 7 4 2)))
              #true)

(define (nooverlap? b board)
  (cond [(not (inside? b)) #true]
        [else (not (member (posn-x b) (get-row (posn-y b) board)))]))


; Number Board -> Row
; get a row given its index
(check-expect (get-row 0 (list (list 0) (list 1 3) (list 7 4 2)))
              (list 0))
(check-expect (get-row 2 (list (list 0) (list 1 3) (list 7 4 2)))
              (list 7 4 2))

(define (get-row n board)
  (ith n board))


; Block -> Boolean
; check if a given block is inside the board
(check-expect (inside? (b 0 0)) #true)
(check-expect (inside? (b 1 1)) #true)
(check-expect (inside? (b 2 1000)) #false)
(check-expect (inside? (b -1 0)) #false)

(define (inside? b)
  (and (>= (posn-x b) 0) (< (posn-x b) BW)
       (>= (posn-y b) 0) (< (posn-y b) BH)))


; Block -> Boolean
; check if a given block is inside the board
(check-expect (inside/above? (b 0 0)) #true)
(check-expect (inside/above? (b 1 -2)) #true)
(check-expect (inside/above? (b 2 1000)) #false)
(check-expect (inside/above? (b -1 0)) #false)

(define (inside/above? b)
  (and (>= (posn-x b) 0) (< (posn-x b) BW)
       (< (posn-y b) BH)))


;=======================================
; Key
;=======================================

; Game KeyEvent -> Game
; handle key events
(define (keyh g event)
  (cond [(game-over? g) g]
        [(key=? event "left") (left-event g)]
        [(key=? event "right") (right-event g)]
        [(key=? event "down") (down-event g)]
        [(key=? event "up") (up-event g)]
        [else g]))


; Game -> Game
; move the gamepiece in a game to the left (if possible)
(define (left-event g)
  (set-gp g (gp-left (game-gamepiece g))))


; Game -> Game
; move the gamepiece in a game to the right (if possible)
(define (right-event g)
  (set-gp g (gp-right (game-gamepiece g))))


; Game GamePiece -> Game
; set a new gamepiece for a current game
(define (set-gp g gp)
  (if (gp-legal? gp (game-board g))
      (make-game (game-board g)
                 gp
                 (game-over? g))
      g))


; Game -> Game
; rotate the gamepiece in a given game (if possible)
(define (up-event g)
  (set-gp g (gp-rotate (game-gamepiece g))))


; GamePiece -> GamePiece
; rotate a given gamepiece
(define (gp-rotate gp)
  (make-gamepiece (gamepiece-pos gp)
                  (p-rotate (gamepiece-piece gp))))


; Piece -> Piece
; rotate a given piece by 90 degrees to the right
(define (p-rotate piece)
  (make-piece (piece-size piece)
              (bs-transform90 (piece-size piece) (piece-blocks piece))))


; Number List-of-Blocks -> List-of-Blocks
; apply transform90 to each block on the list
(define (bs-transform90 size bs)
  (cond [(empty? bs) '()]
        [else (cons (transform90 size (first bs))
                    (bs-transform90 size (rest bs)))]))


; Number Block -> Block
; compute new position of a given block in a square of a given size,
; when square rotates 90 degrees to the right
(check-expect (transform90 3 (b 1 0)) (b 2 1))
(check-expect (transform90 3 (b 1 1)) (b 1 1))
(check-expect (transform90 3 (b 2 2)) (b 0 2))

(define (transform90 size bl)
  (b (- size (posn-y bl) 1)
     (posn-x bl)))


;=======================================
; Launch the world
;=======================================

(big-bang
  INIT-GAME
 [on-tick tickh RATE]
 [on-key keyh]
 [to-draw drawh])

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;===============================================================================
; WORM
;===============================================================================
; You direct the worm who eats fruit and grows. One fruit - one piece to the body.
; If you hit the wall or your own body, you lose.


(require 2htdp/image)
(require 2htdp/universe)


;===============================================================================
; World constants
;===============================================================================

; the board size in number of cells
(define CW 20)
(define CH 20)
; size of one cell in pixels
(define W 20)

(define WORM-PIECE (square W "solid" "blue"))
(define FRUIT (square W "solid" "green"))

(define BACKGROUND (empty-scene (* CW W) (* CH W)))
(define GAME-OVER-IMAGE (text "Press 'space' to start a new game" 18 "black"))


;===============================================================================
; Data definitions
;===============================================================================

; Direction is one of:
; - "left"
; - "right"
; - "up"
; - "down"
; the direction in which the worm moves


(define-struct game [dir worm fruit]) 
; A Game is a structure:
;   (make-game Direction List-of-Posns Posn)
; represents a state of the game where a worm is a list of posns,
; that contain the coordinates of his body
;
; example: (make-game "left" (list (make-posn 2 2) (make-posn 3 2)) (make-posn 5 5))
; interpretation: the worm's body takes up two cells: (2, 2) and (3, 2),
;  it is headed left, the fruit is located at (10, 20)

; Notice: we represent a worm as a List-of-Posns, where each posn contains
;  the board coordinates of one piece of the worm.
;  The first posn is the worm's head, the second posn is its neck and so on.
;  The worm always consists of at least one piece, so the list is never empty.

; initial game: the worm is in the center and headed left
(define DEFAULT-GAME
  (make-game "left" (list (make-posn (/ CW 2) (/ CH 2))) (make-posn 5 5)))

; examples and test cases
(define GAME1 (make-game "left" (list (make-posn 1 1)) (make-posn 5 5)))
(define GAME2 (make-game "left" (list (make-posn 1 1) (make-posn 2 1)) (make-posn 5 5)))


;===============================================================================
; Draw
;===============================================================================

; Game -> Image
; produce an image for a given game state
(define (drawh g)
  (draw-game-over
   g (draw-fruit
      g (draw-worm g BACKGROUND))))


; Game Image -> Image
; in case of game over, put a text in the center
(define (draw-game-over g im)
  (cond [(game-over? g) (overlay GAME-OVER-IMAGE im)]
        [else im]))


; Game Image -> Image
; draw the fruit
(define (draw-fruit g im)
  (place-image FRUIT (x (game-fruit g)) (y (game-fruit g)) im))


; Game Image -> Image
; draw the worm
(define (draw-worm g im)
  (draw-worm-pieces (game-worm g) im))


; List-of-Posns Image -> Image
; draw the pieces of a worm's body
(define (draw-worm-pieces worm im)
  (cond [(empty? worm) im]
        [else (draw-worm-piece (first worm)
                               (draw-worm-pieces (rest worm) im))]))


; Posn Image -> Image
; draw a worm piece at a given position on the grid
(define (draw-worm-piece p im)
  (place-image WORM-PIECE (x p) (y p) im))


; Posn -> Number
; compute canvas x-coordinate from board-coordinates
(define (x p)
  (+ (* W (posn-x p)) (/ W 2)))


; Posn -> Number
; compute the canvas y-coordinate from board-coordinates
(define (y p)
  (+ (* W (posn-y p)) (/ W 2)))


;===============================================================================
; Tick
;===============================================================================

; Game -> Game
; handle tick events
(define (tickh g)
  (cond [(game-over? g) g]
        [else (move g)]))


; Game -> Game
; move the worm, possibly eat fruit and grow
(define (move g)
  (cond [(fruit-ahead? g) (extend-new-fruit g)]
        [else (extend-cut g)]))


; Game -> Boolean
; check if there is fruit ahead the worm
(define (fruit-ahead? g)
  (equal? (ahead g) (game-fruit g)))


; Game -> Game
; extend the worm and cut its tail
(define (extend-cut g)
  (make-game (game-dir g)
             (remove-last (cons (ahead g) (game-worm g)))
             (game-fruit g)))


; List -> List
; remove the last element in a list
(check-expect (remove-last (list 1)) '())
(check-expect (remove-last (list 1 2)) (list 1))
(check-expect (remove-last (list 1 2 3)) (list 1 2))

(define (remove-last lst)
  (cond [(empty? (rest lst)) '()]
        [else (cons (first lst) (remove-last (rest lst)))]))
  

; Game -> Game
; extend the worm without cutting its tail,
; change the fruit's position
(define (extend-new-fruit g)
  (make-game (game-dir g)
             (cons (ahead g) (game-worm g))
             (new-fruit (cons (ahead g) (game-worm g)))))


; Game -> Posn
; compute the cell's coordinates where the worm is headed
(define (ahead g)
  (pos-next (head g) (game-dir g)))


; Posn Direction -> Posn
; compute the position next to the given position at a given direction
(check-expect (pos-next (make-posn 10 10) "left")  (make-posn 9 10))
(check-expect (pos-next (make-posn 10 10) "right") (make-posn 11 10))
(check-expect (pos-next (make-posn 10 10) "up")    (make-posn 10 9))
(check-expect (pos-next (make-posn 10 10) "down")  (make-posn 10 11))

(define (pos-next pos dir)
  (cond [(string=? dir "left")
         (make-posn (- (posn-x pos) 1) (posn-y pos))]
        [(string=? dir "right")
         (make-posn (+ (posn-x pos) 1) (posn-y pos))]
        [(string=? dir "up")
         (make-posn (posn-x pos) (- (posn-y pos) 1))]
        [(string=? dir "down")
         (make-posn (posn-x pos) (+ (posn-y pos) 1))]))


; Game -> Posn
; get the position of the worm's head
(define (head g)
  (first (game-worm g)))


; Game -> Posn
; get the position of the worm's neck (second piece)
(define (neck g)
  (second (game-worm g)))


; Game -> List-of-Posns
; get the worm's tail (all its body except the head)
(define (tail g)
  (rest (game-worm g)))


; List-of-Posns -> Posn
; compute the position for new fruit
; (it can't be placed on the worm)
(define (new-fruit worm)
  (new-fruit-random worm (random-pos 1)))


; List-of-Posns Posn -> Posn
; return the given position if it doesn't interfere with the worm,
; otherwise call itself recursively with a new random position
(check-satisfied (new-fruit-random (list (make-posn 1 1) (make-posn 2 1))
                                   (make-posn 1 1)) posn?)
(define (new-fruit-random worm pos)
  (cond [(member pos worm) (new-fruit-random worm (random-pos 1))]
        [else pos]))


; Any -> Posn
; produce random position for the board
(define (random-pos dummy)
  (make-posn (random CW) (random CH)))


; Game -> Boolean
; check if the worm went outside the board or hit himself
(define (game-over? g)
  (or (outside? (head g))
      (member (head g) (tail g))))


; Posn -> Boolean
; check if a given position is outside the grid
(define (outside? p)
  (or (< (posn-x p) 0) (>= (posn-x p) CW)
      (< (posn-y p) 0) (>= (posn-y p) CH)))


;===============================================================================
; Key
;===============================================================================

; Game KeyEvent -> Game
; handle key events
(define (keyh g event)
  (cond [(and (game-over? g) (key=? event " ")) DEFAULT-GAME]
        [(game-over? g) g]
        [(or (key=? event "left") (key=? event "right") (key=? event "up")
             (key=? event "down")) (set-direction g event)]
        [else g]))


; Game Direction -> Game
; set new direction for the worm
; (in case of illegal direction, don't change the game)
(define (set-direction g dir)
  (cond [(legal-direction? g dir)
         (make-game dir (game-worm g) (game-fruit g))]
        [else g]))


; Game Direction -> Boolean
; check if a given direction doesn't direct the head to its neck
(check-expect (legal-direction? GAME1 "right") #true)
(check-expect (legal-direction? GAME2 "right") #false)
(check-expect (legal-direction? GAME2 "down")  #true)

(define (legal-direction? g dir)
  (cond [(only-head? g) #true]
        [else (not (equal? (pos-next (head g) dir)
                           (neck g)))]))


; Game -> Boolean
; check if the worm consists of one piece
(check-expect (only-head? GAME1) #true)
(check-expect (only-head? GAME2) #false)

(define (only-head? g)
  (empty? (rest (game-worm g))))


;===============================================================================
; Launch the world
;===============================================================================

(big-bang
 DEFAULT-GAME
 [on-tick tickh 0.10]
 [on-key keyh]
 [to-draw drawh])

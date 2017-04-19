;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;==================================================================================
; FEEDING WORM
;==================================================================================
; You direct the worm who eats fruit and grows.
; ...
; ...
; ...

(require 2htdp/image)
(require 2htdp/universe)


;==================================================================================
; World constants
;==================================================================================

; number of cells
(define CW 40)
(define CH 40)
; size of one cell
(define W 10)
; one part of a worm's body
(define WORM-PIECE (square W "solid" "blue"))
(define FRUIT (square W "solid" "green"))


(define BACKGROUND (empty-scene (* CW W) (* CW W)))
(define GAME-OVER-TEXT "Press space to start a new game")

;==================================================================================
; Data definitions
;==================================================================================

; Direction is one of:
; - "left"
; - "right"
; - "up"
; - "down"
; it is the direction in which the worm moves

(define-struct game [dir worm fruit]) 
; A Game is a structure:
;   (make-game Direction List-of-posns Posn Number Boolean)
; represents a state of the game where a worm is a list of posns,
; that contain the coordinates of his body
;
; example: (make-game "left" (list (make-posn 2 2) (make-posn 3 2)) (make-posn 10 20))
; interpretation: the worm has a body that takes up two cells, it is headed left,
;                 the fruit is located at (10, 20)


; initial game
(define DEFAULT-GAME
  (make-game "left" (list (make-posn (/ CW 2) (/ CH 2))) (make-posn 5 5)))

; examples and test cases
(define GAME1 (make-game "left" (list (make-posn 1 1)) (make-posn 5 5)))
(define GAME2 (make-game "left" (list (make-posn 1 1) (make-posn 2 1)) (make-posn 5 5)))

;==================================================================================
; Draw
;==================================================================================

; Game -> Image
; render a given game
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
(define (draw-fruit g im)
  (place-image FRUIT (x (game-fruit g)) (y (game-fruit g)) im))

; List-of-Posns Image -> Image
(define (draw-worm g im)
  (draw-worm-pieces (game-worm g) im))

; List-of-Posns Image -> Image
(define (draw-worm-pieces worm im)
  (cond [(empty? worm) im]
        [else (draw-worm-piece (first worm)
                               (draw-worm-pieces (rest worm) im))]))

; Posn Image -> Image
; draws a worm piece at a given position on the grid
(define (draw-worm-piece p im)
  (place-image WORM-PIECE (x p) (y p) im))

; Posn -> Number
; compute the canvas x-coordinate from grid-coordinates
(define (x p)
  (+ (* W (posn-x p)) (/ W 2)))

; Posn -> Number
; compute the canvas y-coordinate from grid-coordinates
(define (y p)
  (+ (* W (posn-y p)) (/ W 2)))


;==================================================================================
; Tick
;==================================================================================

; Game -> Game
; handle tick events
(define (tickh g)
  (cond [(game-over? g) g]
        [else (move g)]))


; Game -> Game
; move the worm
(define (move g)
  (cond [(fruit-ahead? g) (extend-new-fruit g)]
        [else (extend-cut g)]))


; Game -> Boolean
; check if there is fruit ahead the worm
(define (fruit-ahead? g)
  (equal? (ahead g) (game-fruit g)))


; Game -> Game
; extend the worm, cut its tail
(define (extend-cut g)
  (make-game (game-dir g)
             (remove-last (cons (ahead g) (game-worm g)))
             (game-fruit g)))


; List -> List
; remove the last element in a list
(define (remove-last lst)
  (cond [(empty? (rest lst)) '()]
        [else (cons (first lst) (remove-last (rest lst)))]))
  

; Game -> Game
; extend the worm without cutting its tail,
; change fruit's position
(define (extend-new-fruit g)
  (make-game (game-dir g)
             (cons (ahead g) (game-worm g))
             (new-fruit (cons (ahead g) (game-worm g)))))


; Game -> Posn
; compute coordinates of the cell where the worm is headed
(define (ahead g)
  (pos-next (head g) (game-dir g)))


; Posn Direction -> Posn
; compute the position next the given at a given direction
(check-expect (pos-next (make-posn 10 10) "left") (make-posn 9 10))
(check-expect (pos-next (make-posn 10 10) "right") (make-posn 11 10))
(check-expect (pos-next (make-posn 10 10) "up") (make-posn 10 9))
(check-expect (pos-next (make-posn 10 10) "down") (make-posn 10 11))

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


; Game -> List-of-Posn
; get the worm's tail (its body except the head)
(define (tail g)
  (rest (game-worm g)))


; List-of_Posns -> Posn
; compute the position for new fruit
; (it can't be places where the worm is)
(define (new-fruit worm)
  (new-fruit-random worm (random-pos 1)))


; List-of-Posns Posn -> Posn
; return the given position if it doesn't interfere with the worm,
; otherwise call itself recursively with a new random position
(define (new-fruit-random worm pos)
  (cond [(member pos worm) (new-fruit-random worm pos)]
        [else pos]))


; Any -> Posn
; produce random position for the grid
(define (random-pos dummy)
  (make-posn (random CW) (random CH)))


; Game -> Boolean
; check if the next position of the worm's head is outside the grid
(define (game-over? g)
  (or (outside? (head g))
      (member (head g) (tail g))))


; Posn -> Boolean
; check if a given position is outside the grid
(define (outside? p)
  (or (< (posn-x p) 0) (>= (posn-x p) CW)
      (< (posn-y p) 0) (>= (posn-y p) CH)))


;==================================================================================
; Key handler
;==================================================================================

; Game KeyEvent -> Game
; handle key events
(define (keyh g event)
  (cond [(and (game-over? g) (key=? event " ")) DEFAULT-GAME]
        [(game-over? g) g]
        [(or (key=? event "left") (key=? event "right") (key=? event "up")
             (key=? event "down")) (set-direction g event)]
        [else g]))


; Game Direction -> Game
; set new direction for the worm, if it's a legal direction
(define (set-direction g dir)
  (cond [(legal-direction? g dir)
         (make-game dir (game-worm g) (game-fruit g))]
        [else g]))


; Game Direction -> Boolean
; check if new direction doesn't direct the head to its neck
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


;==================================================================================
; Launch the world
;==================================================================================

(big-bang
 DEFAULT-GAME
 [on-tick  tickh 0.10]
 [on-key keyh]
 [to-draw  drawh])


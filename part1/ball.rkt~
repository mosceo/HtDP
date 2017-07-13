;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ball) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;==================================================================================
; BOUNCING BALL
;==================================================================================
; A ball is moving on the canvas. If it hits the wall it bounces.
; The speed of the ball decreases with time. You can throw the ball:
; click on the ball, begin dragging it and click again while dragging.
; Its new direction and speed will depend on how it was dragged.

(require 2htdp/image)
(require 2htdp/universe)


;==================================================================================
; Constants and data definitions
;==================================================================================

(define WIDTH 600)
(define HEIGHT 400)
(define RADIUS 30)

(define X-MIN RADIUS)
(define X-MAX (- WIDTH RADIUS))
(define Y-MIN RADIUS)
(define Y-MAX (- HEIGHT RADIUS))

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define BALL (circle RADIUS "solid" "blue"))
(define MV #i0.05)
(define THR 15)
(define SD #i0.992)

(define EPSILON 0.001)

(define-struct world [pos delta drag offset])
; A WorldState is a structure :
;   (make-world (make-posn Number Number) (make-posn Number Number)
;               #false (make-posn Number Number))
; representing a given world state where
; 'pos' - (x, y) coordinates of the ball on canvas
; 'delta' - (dx, dy) direction and speed
; 'drag' - is user dragging the ball now?
; 'offset' - (when dragged) cursor's position relative to the center of the ball


; for unit tests
(define TW1 (make-world (make-posn 100 80) (make-posn 20 15) #false (make-posn 5 5)))
(define TW2 (make-world (make-posn 50 40) (make-posn 20 15) #true (make-posn 5 5)))
(define TW3 (make-world (make-posn 80 70) (make-posn (* SD 20) (* SD 15))
                        #false (make-posn 5 5)))


;==================================================================================
; Tick
;==================================================================================

; WorldState -> WorldState
; time handler; moves the ball with every tick (if not dragged)
(check-expect (tick TW2) TW2)
(define (tick w)
  (if (drag? w) w
      (world-given-pos w (next-pos w))))

; WorldState -> Posn
; computes next desirable position for the world
(define (next-pos w)
  (add-delta (world-pos w) (world-delta w) MV))

; WorldState -> Boolean
; checks if the ball being dragged
(check-expect (drag? TW1) #false)
(check-expect (drag? TW2) #true)
(define (drag? w)
  (world-drag w))

; WorldState Posn -> WorldState
; compute new world state given next desirable (possibly incorrect) position
(check-within (world-given-pos TW1 (make-posn 80 70)) TW3 EPSILON)
(define (world-given-pos w pos)
  (make-world (pos-bounds pos)
              (redirect-delta (slow-down (world-delta w)) pos)
              (world-drag w)
              (world-offset w)))

; Posn -> Posn
; decreases delta components to slow down the ball
(check-within (slow-down (make-posn 50 100)) (make-posn (* SD 50) (* SD 100)) EPSILON)
(define (slow-down delta)
  (mult delta SD))

; Posn -> Posn
; correct the position of the ball to be withing the bounds of the canvas
(define (pos-bounds pos)
  (make-posn (x-bounds (posn-x pos)) (y-bounds (posn-y pos))))

; Number -> Number
; correct the x-coordinate of the ball to be withing the bounds
(define (x-bounds x)
  (number-bounds x X-MIN X-MAX))

; Number -> Number
; corrects the y-coordinate of the ball to be withing the bounds
(define (y-bounds y)
  (number-bounds y Y-MIN Y-MAX))

; Number Number Number -> Number
; compute new x, that is withing the given bounds
(check-expect (number-bounds 5 0 10) 5)
(check-expect (number-bounds 0 0 10) 0)
(check-expect (number-bounds -1 0 10) 0)
(check-expect (number-bounds 10 0 10) 10)
(check-expect (number-bounds 12 0 10) 10)
(define (number-bounds x min max)
  (cond [(< x min) min]
        [(> x max) max]
        [else x]))

; Number Number Number -> Boolean
; check if the number is not within given bounds
(check-expect (number-out? 5 0 10) #false)
(check-expect (number-out? -1 0 10) #true)
(check-expect (number-out? 11 0 10) #true)
(define (number-out? x min max)
  (or (< x min) (> x max)))

; Posn Posn -> Posn
; given a desirable position of the ball, compute new delta for the world
; (given position might call for switching directions)
(check-expect (redirect-delta (make-posn 10 20) (make-posn 100 200)) (make-posn 10 20))
(check-expect (redirect-delta (make-posn 10 20) (make-posn -1 200)) (make-posn -10 20))
(check-expect (redirect-delta (make-posn 10 20) (make-posn 9000 200)) (make-posn -10 20))
(check-expect (redirect-delta (make-posn 10 20) (make-posn 100 -2)) (make-posn 10 -20))
(check-expect (redirect-delta (make-posn 10 20) (make-posn 100 9000)) (make-posn 10 -20))
(define (redirect-delta delta pos)
  (make-posn (if (number-out? (posn-x pos) X-MIN X-MAX)
                 (- (posn-x delta)) (posn-x delta))
             (if (number-out? (posn-y pos) Y-MIN Y-MAX)
                 (- (posn-y delta)) (posn-y delta))))


;==================================================================================
; Mouse
;==================================================================================

; WorldState Number Number MouseEvent -> WorldState
; handle mouse events
(define (mouse w x y event)
  (cond [(and (mouse=? event "button-down") (drag? w))
         (stop-drag w x y)]
        [(and (mouse=? event "button-down") (over-ball? w x y))
         (begin-drag w x y)]
        [(and (mouse=? event "move") (drag? w))
         (move-drag w x y)]
        [else w]))

; WorldState Number Number -> Boolean
; check if the mouse cursor is over the ball
(define (over-ball? w x y)
  (< (distance (world-pos w) (make-posn x y)) RADIUS))

; WorldState Number Number -> WorldState
; given mouse coordinates, change the world state with the moving ball
; to the state where the ball is being dragged
(define (begin-drag w x y)
  (make-world (world-pos w)
              (make-posn 0 0)
              #true
              (minus (make-posn x y) (world-pos w))))

; WorldState Number Number -> WorldState
; given mouse coordinates and the world state where the ball is being dragged,
; compute new world state
(define (move-drag w x y)
  (make-world (drag-new-pos w x y)
              (compute-delta w x y)
              #true
              (world-offset w)))

; WorldState Number Number -> WorldState
; given mouse coordinates, change the world state where the ball is being dragged,
; to the state where the ball is moving again
(define (stop-drag w x y)
  (make-world (world-pos w)
              (world-delta w)
              #false
              (make-posn 0 0)))

; WorldState Number Number -> Posn
; given mouse coordinates and the world state where the ball is being dragged,
; compute new position of the ball
(define (drag-new-pos w x y)
  (pos-bounds (minus (make-posn x y) (world-offset w))))

; WorldState Number Number -> Posn
; given mouse coordinates and the world state where the ball is being dragged,
; compute new delta
(define (compute-delta w x y)
  (mult (minus (drag-new-pos w x y)
               (world-pos w)) THR))


;==================================================================================
; Render
;==================================================================================

; WorldState -> Image
; renders a given world state
(define (render w)
  (place-image BALL (posn-x (world-pos w)) (posn-y (world-pos w)) BACKGROUND))


;==================================================================================
; Working with Posn's
;==================================================================================

; Posn Posn -> Posn
; add two posns together
(check-expect (add (make-posn 1 2) (make-posn 3 4)) (make-posn 4 6))
(define (add p q)
  (make-posn (+ (posn-x p) (posn-x q))
             (+ (posn-y p) (posn-y q))))

; Posn Posn -> Posn
; subtract second posn from the first
(check-expect (minus (make-posn 5 4) (make-posn 3 1)) (make-posn 2 3))
(define (minus p q)
  (make-posn (- (posn-x p) (posn-x q))
             (- (posn-y p) (posn-y q))))

; Posn Number -> Posn
; multiply posn components by a number
(check-expect (mult (make-posn 1 2) 3) (make-posn 3 6))
(define (mult p k)
  (make-posn (* k (posn-x p)) (* k (posn-y p))))

; Posn Posn -> Posn
; add second posn multiplied by a number to the first posn
(check-expect (add-delta (make-posn 1 2) (make-posn 3 4) 3) (make-posn 10 14))
(define (add-delta p1 p2 c)
  (make-posn (+ (posn-x p1) (* c (posn-x p2)))
             (+ (posn-y p1) (* c (posn-y p2)))))

; Posn Posn -> Number
; computes the distance between two posns
(check-within (distance (make-posn 1 2) (make-posn 1 2)) 0 EPSILON)
(check-within (distance (make-posn 1 2) (make-posn 4 6)) 5 EPSILON)
(check-within (distance (make-posn 4 6) (make-posn 1 2)) 5 EPSILON)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


;==================================================================================
; Launch the world
;==================================================================================

(big-bang
 (make-world (make-posn 100 100) (make-posn 100 100) #false (make-posn 0 0))
 [on-tick  tick]
 [on-mouse mouse]
 [to-draw  render])





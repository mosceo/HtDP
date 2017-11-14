;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;=======================================
; SPACE INVADERS
;=======================================
; An UFO is descending and changing its direction randomly.
; You have to hit it with a missile to save the planet from invaders.
; Press "left" or "right" to move the rocket launcher.
; When you press "space", the launcher fires a missile.
;
; Author: Roman Kunin (mosceo@gmail.com)
; Source: https://github.com/mosceo/htdp

(require 2htdp/image)
(require 2htdp/universe)


;=======================================
; World constants
;=======================================

; > Canvas
(define WIDTH 400)
(define HEIGHT 600)
(define CLOUD (bitmap "cloud.png"))
(define TREE (bitmap "tree.png"))
; the y-coordinate for trees
(define TREE-Y (- HEIGHT (/ (image-height TREE) 2)))
(define LEFT-MOUNTAIN (polygon (list (make-posn 0 (* HEIGHT 0.75))
                                     (make-posn 0 HEIGHT)
                                     (make-posn (* WIDTH 0.5) HEIGHT))
                               "solid"
                               "burlywood"))
(define RIGHT-MOUNTAIN (polygon (list (make-posn WIDTH (* HEIGHT 0.85))
                                      (make-posn WIDTH HEIGHT)
                                      (make-posn (* WIDTH 0.25) HEIGHT))
                                "solid"
                                "brown"))
; add mountains
(define BACKGROUND-MOUNTAINS
  (overlay/align
   "right" "bottom" RIGHT-MOUNTAIN
   (overlay/align
    "left" "bottom"
    LEFT-MOUNTAIN (empty-scene WIDTH HEIGHT))))
; add clouds
(define BACKGROUND-MOUNTAINS-CLOUDS
  (place-image
   CLOUD (* WIDTH 0.75) (* HEIGHT 0.15)
   (place-image CLOUD (* WIDTH 0.25) (* HEIGHT 0.20) BACKGROUND-MOUNTAINS)))
; add trees
(define BACKGROUND
  (place-image
   TREE (* WIDTH 0.75) TREE-Y
   (place-image
    TREE (* WIDTH 0.35) TREE-Y
    (place-image
     TREE (* WIDTH 0.15) TREE-Y BACKGROUND-MOUNTAINS-CLOUDS))))


; > Missile launcher
(define LAUNCHER (bitmap "car.png"))
; it moves by that many pixels when "left" or "right" is pressed
(define LAUNCHER-DX 5)
; the y-coordinate of the launcher (never changes)
(define LAUNCHER-Y (- HEIGHT (/ (image-height LAUNCHER) 2)))

; > UFO
(define UFO (bitmap "ufo.png"))
(define UFO-WIDTH (image-width UFO))
(define UFO-RADIUS (/ (image-height UFO) 2))
; UFO changes its x-direction randomly; to achive this we introduce
; a special number that we call "mileage"; every time UFO's x-coordinate
; changes by some amount, we subtract this amount from mileage;
; when mileage becomes negative, we change UFO's x-direction;
; we choose mileage as a random number between these bounds
(define UFO-MILEAGE-MIN (/ WIDTH 4))
(define UFO-MILEAGE-MAX WIDTH)

(define UFO-VEL-X 100)
(define UFO-VEL-Y 50)
; next UFO will have its elocity vector multiplied by this constant
(define UFO-SPEEDUP 1.1)
; when UFO reaches this y-coordinate, it is considered landed
(define UFO-LANDING-Y (- HEIGHT (/ (image-height UFO) 2)))
; initial y-coordinate for an UFO
(define UFO-Y (- (image-height UFO)))

; > Missile
(define MISSILE (bitmap "missile.png"))
(define MSL-VEL -500)
(define MSL-VEL-VEC (make-posn 0 MSL-VEL))
; initial y-coordinate for a missile
(define MISSILE-Y (- HEIGHT (image-height LAUNCHER)))

; > Other
; velocities are multiplied by this constant (28 ticks per sec)
(define FRAC 1/28)
; just a very small number (for tests)
(define EPSILON 0.00001)


;=======================================
; Data definitions
;=======================================

; PosnOrFalse is one of:
; - (make-posn x y)
; - #false

(define-struct ws [launcher ufo missile score])
; A WS (World State) is a structure:
;   (make-world Number (make-ufo ...) PosnOrFalse Number)
;
; example: (make-world 150 (make-ufo ...) #false 7)
; interpretation: launcher's coordinate is (150, X), UFO is represented by
; its structure (read below), no missile, current score is 7

(define-struct ufo [pos vel mileage])
; An UFO is a structure:
;   (make-ufo Posn Posn Number)
;
; example: (make-ufo (make-posn 10 20) (make-posn -100 50) 244)
; interpretation: UFO's position is (10, 20), its velocity vector is (-100, 50);
; when it moves 244 pixels along the x-coordinate, it changes its x-direction


;=======================================
; Render
;=======================================

; WorldState -> Image
; render a given world state
(define (render ws)
  (draw-game-over
   ws (draw-score
       ws (draw-missile
           ws (draw-ufo
               ws (draw-launcher
                   ws BACKGROUND))))))


; WS Image -> Image
; check if game is over and make appropriate rendering
(define (draw-game-over ws im)
  (cond [(game-over? ws) (overlay (text "GAME OVER" 40 "black") im)]
        [else im]))


; WS Image -> Image
; render launcher on the screen
(define (draw-launcher ws im)
  (place-image LAUNCHER (ws-launcher ws) LAUNCHER-Y im))


; WS Image -> Image
; render UFO on the screen
(define (draw-ufo ws im)
  (place-image UFO (posn-x (ufo-pos (ws-ufo ws))) (posn-y (ufo-pos (ws-ufo ws))) im))


; WS Image -> Image
; if there is a missile, render it
(define (draw-missile ws im)
  (cond [(missile? ws)
         (place-image MISSILE
                      (posn-x (ws-missile ws)) (posn-y (ws-missile ws))
                      im)]
        [else im]))


; WS Image -> Image
; render the score on the screen
(define (draw-score ws im)
  (place-image
   (overlay (text (number->string (ws-score ws)) 30 "black")
            (square 40 "solid" "gray"))
   (- WIDTH 30) 30
   im))


; WS -> Boolean
; check if there is a missile in the world
(define (missile? ws)
  (not (boolean? (ws-missile ws))))


;=======================================
; Tick handler
;=======================================

; WS -> WS
; handle clock events (by default 28 ticks per second)
(define (tickh ws)
  (cond [(game-over? ws) ws]
        [(missile-hit? ws)
         (make-ws (ws-launcher ws)
                  (next-ufo (ws-ufo ws))
                  #false
                  (+ (ws-score ws) 1))]
        [else
         (make-ws (ws-launcher ws)
                  (ufo-next-state (ws-ufo ws))         ; move ufo
                  (missile-next-state (ws-missile ws)) ; move missile
                  (ws-score ws))]))


; WS -> Boolean
; check if game is over (UFO landed)
(define (game-over? ws)
  (>= (posn-y (ufo-pos (ws-ufo ws))) UFO-LANDING-Y)) 


; WS -> Boolean
; check if the missile hit the UFO
(define (missile-hit? ws)
  (and (missile? ws)
       (< (distance (ufo-pos (ws-ufo ws))
                    (ws-missile ws))
          UFO-RADIUS)))


; UFO -> UFO
; compute new UFO from the previous (destroyed) one;
; new UFO is moving faster
(define (next-ufo ufo)
  (make-ufo (make-posn (random WIDTH) UFO-Y)
            (make-posn (* UFO-SPEEDUP (posn-x (ufo-vel ufo)))
                       (* UFO-SPEEDUP (posn-y (ufo-vel ufo))))
            (random2 UFO-MILEAGE-MIN UFO-MILEAGE-MAX)))


; UFO -> UFO
; compute the UFO's next state
; move it on canvas, change mileage, maybe change direction
(define (ufo-next-state ufo)
  (ufo-check-mileage
   (ufo-correct-pos
    (make-ufo (pos+vel (ufo-pos ufo) (ufo-vel ufo))
              (ufo-vel ufo)
              (- (ufo-mileage ufo)
                 (abs (* FRAC (posn-x (ufo-vel ufo)))))))))


; UFO -> UFO
; keep UFO inside the canvas; if it tried to get out, change its x-direction
(define (ufo-correct-pos ufo)
  (cond [(or (< (posn-x (ufo-pos ufo)) 0)
             (> (posn-x (ufo-pos ufo)) WIDTH))
         (make-ufo (make-posn (number-bounds (posn-x (ufo-pos ufo)) 0 WIDTH)
                              (posn-y (ufo-pos ufo)))
                   (redirect (ufo-vel ufo))
                   (ufo-mileage ufo))]
        [else ufo]))


; UFO -> UFO
; if mileage is negative, change direction and set new mileage
(define (ufo-check-mileage ufo)
  (cond [(< (ufo-mileage ufo) 0)
         (make-ufo (ufo-pos ufo)
                   (redirect (ufo-vel ufo))
                   (random2 UFO-MILEAGE-MIN UFO-MILEAGE-MAX))]
        [else ufo]))


; Posn Posn -> Posn
; update the position vector every 1 clock tick
(define (pos+vel pos vel)
  (make-posn (+ (posn-x pos) (* FRAC (posn-x vel)))
             (+ (posn-y pos) (* FRAC (posn-y vel)))))


; Posn -> Posn
; change the x direction of the velocity vector
(define (redirect vel)
  (make-posn (- (posn-x vel)) (posn-y vel)))


; PosnOrFalse -> PosnOrFalse
; compute next state fot the missile
(define (missile-next-state msl)
  (cond [(boolean? msl) msl]
        [(< (posn-y msl) 0) #false]
        [else (pos+vel msl MSL-VEL-VEC)]))


; Number Number -> Number
; compute random number between 'min' and 'max'
(define (random2 min max)
  (+ (random (- max min)) min))


; Posn Posn -> Number
; compute the distance between two points
(check-within (distance (make-posn 1 2) (make-posn 1 2)) 0 EPSILON)
(check-within (distance (make-posn 1 2) (make-posn 4 6)) 5 EPSILON)

(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


;=======================================
; Key handler
;=======================================

; WS KeyEvent -> WS
; handle key events
(define (keyh ws event)
  (cond [(game-over? ws) ws]
        [(key=? event "left") (move-launcher ws (- LAUNCHER-DX))]
        [(key=? event "right") (move-launcher ws LAUNCHER-DX)]
        [(key=? event " ") (fire-missile ws)]
        [else ws]))


; WS Number -> WS
; move launcher along x-coordinate for a given amount
(define (move-launcher ws dx)
  (make-ws (number-bounds (+ (ws-launcher ws) dx) 0 WIDTH)
           (ws-ufo ws)
           (ws-missile ws)
           (ws-score ws)))


; WS -> WS
; launcher fires a missile;
; if there is already a missile, nothing happens
(define (fire-missile ws)
  (cond [(missile? ws) ws]
        [else (make-ws (ws-launcher ws)
                       (ws-ufo ws)
                       (make-posn (ws-launcher ws) MISSILE-Y)
                       (ws-score ws))]))


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


;=======================================
; Launch the world
;=======================================

(big-bang
 (make-ws (/ WIDTH 2)
          (make-ufo (make-posn (random WIDTH) 0) (make-posn UFO-VEL-X UFO-VEL-Y) 200)
          #false 0)
 [on-tick  tickh]
 [on-key keyh]
 [to-draw  render])

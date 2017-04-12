;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;==================================================================================
; SPACE INVADERS
;==================================================================================
; An UFO is descending. The rocket launcher fires missiles.
; ...
; ...
; ...

(require 2htdp/image)
(require 2htdp/universe)


;==================================================================================
; World constants
;==================================================================================

; sizes of the objects in the world
(define WIDTH 400)
(define HEIGHT 600)
(define UFO-WIDTH 30)

; other world constants
(define MSL-VEL -500) ; velocity of the missile
(define FRAC 1/28)  ; velocities are multiplied by this constant (28 ticks per sec)
(define UFO-MILEAGE-MIN (/ WIDTH 4))
(define UFO-MILEAGE-MAX WIDTH)
(define UFO-VEL-X 100)
(define UFO-VEL-Y 50)
(define UFO-SPEEDUP 1.1)


; graphical objects
(define UFO (circle UFO-WIDTH "solid" "blue"))
(define LAUNCHER (above (rectangle 5 5 "solid" "red")
                        (rectangle 50 15 "solid" "black")))
(define MISSILE (circle 2 "solid" "red"))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define LAUNCHER-DX 5)

(define EPSILON 0.00001)

; computed constants
(define UFO-LANDING-Y (- HEIGHT (/ 2 (image-height UFO))))
(define UFO-Y (- (image-height UFO)))   ; initial constants for UFO
(define LAUNCHER-Y (- HEIGHT (/ (image-height LAUNCHER) 2)))
(define MSL-VEL-VEC (make-posn 0 MSL-VEL))
(define MISSILE-Y (- HEIGHT (image-height LAUNCHER)))


;==================================================================================
; Data definitions
;==================================================================================

(define-struct ws [launcher ufo missile score])
; A WS (World State) is a structure :
;   (make-world Number (make-ufo ...) Posn Number)
; representing a given world state with the x-coordinate of the launcher,
; coordinates of the missile (if any), score and the ufo-structure

(define-struct ufo [pos vel mileage])
; An UFO is a structure :
;   (make-ufo Posn Posn Number)
; representing an UFO with certain coordinates, velocity
; and distance to travel before changing direction (left <-> right)


;==================================================================================
; Render
;==================================================================================

; WorldState -> Image
; renders a given world state
(define (render ws)
  (draw-score
   ws (draw-missile
       ws (draw-ufo
           ws (draw-launcher
               ws BACKGROUND)))))

(define (draw-launcher ws im)
  (place-image LAUNCHER (ws-launcher ws) LAUNCHER-Y im))

(define (draw-ufo ws im)
  (place-image UFO (posn-x (ufo-pos (ws-ufo ws))) (posn-y (ufo-pos (ws-ufo ws))) im))

(define (draw-missile ws im)
  (cond [(missile? ws)
         (place-image MISSILE
                      (posn-x (ws-missile ws)) (posn-y (ws-missile ws))
                      im)]
        [else im]))

(define (draw-score ws im)
  (place-image
   (overlay (text (number->string (ws-score ws)) 30 "black")
            (square 40 "solid" "gray"))
   (- WIDTH 30) 30
   im))

; WS -> Boolean
; checks if there is a misisle in the world
(define (missile? ws)
  (not (boolean? (ws-missile ws))))


;==================================================================================
; Tick handler
;==================================================================================

; WS -> WS
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
(define (game-over? ws)
  (>= (posn-y (ufo-pos (ws-ufo ws))) UFO-LANDING-Y)) 

; WS -> Boolean
; has the missile hit the UFO?
(define (missile-hit? ws)
  (and (missile? ws)
       (< (distance (ufo-pos (ws-ufo ws))
                    (ws-missile ws))
          20)))

; UFO -> UFO
; compute new UFO from the previous (destroyed) one;
; new UFO is moving faster
(define (next-ufo ufo)
  (make-ufo (make-posn (random WIDTH) UFO-Y)
            (make-posn (* UFO-SPEEDUP (posn-x (ufo-vel ufo)))
                       (* UFO-SPEEDUP (posn-y (ufo-vel ufo))))
            (random2 UFO-MILEAGE-MIN UFO-MILEAGE-MAX)))

; UFO -> UFO
(define (ufo-next-state ufo)
  (ufo-check-mileage
   (ufo-correct-pos
    (make-ufo (pos+vel (ufo-pos ufo) (ufo-vel ufo))
              (ufo-vel ufo)
              (- (ufo-mileage ufo)
                 (abs (* FRAC (posn-x (ufo-vel ufo)))))))))

; UFO -> UFO
; if x-coordinate of UFO is out of the scene, change its direction
(define (ufo-correct-pos ufo)
  (cond [(or (< (posn-x (ufo-pos ufo)) 0)
             (> (posn-x (ufo-pos ufo)) WIDTH))
         (make-ufo (make-posn (number-bounds (posn-x (ufo-pos ufo)) 0 WIDTH)
                              (posn-y (ufo-pos ufo)))
                   (redirect (ufo-vel ufo))
                   (ufo-mileage ufo))]
        [else ufo]))

; UFO -> UFO
; ...
(define (ufo-check-mileage ufo)
  (cond [(< (ufo-mileage ufo) 0)
         (make-ufo (ufo-pos ufo)
                   (redirect (ufo-vel ufo))
                   (random2 UFO-MILEAGE-MIN UFO-MILEAGE-MAX))]
        [else ufo]))

(define (pos+vel pos vel)
  (make-posn (+ (posn-x pos) (* FRAC (posn-x vel)))
             (+ (posn-y pos) (* FRAC (posn-y vel)))))

; Posn -> Posn
; change the x direction of the velocity vector
(define (redirect vel)
  (make-posn (- (posn-x vel)) (posn-y vel)))

; Posn -> Posn
(define (missile-next-state msl)
  (cond [(boolean? msl) msl]
        [(< (posn-y msl) 0) #false]
        [else (pos+vel msl MSL-VEL-VEC)]))

; Number Number -> Number
(define (random2 min max)
  (+ (random (- max min)) min))

; Posn Posn -> Number
; computes the distance between two points
(check-within (distance (make-posn 1 2) (make-posn 1 2)) 0 EPSILON)
(check-within (distance (make-posn 1 2) (make-posn 4 6)) 5 EPSILON)

(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


;==================================================================================
; Key handler
;==================================================================================

(define (keyh ws event)
  (cond [(game-over? ws) ws]
        [(key=? event "left") (move-launcher ws (- LAUNCHER-DX))]
        [(key=? event "right") (move-launcher ws LAUNCHER-DX)]
        [(key=? event " ") (fire-missile ws)]
        [else ws]))

(define (move-launcher ws dx)
  (make-ws (number-bounds (+ (ws-launcher ws) dx) 0 WIDTH)
           (ws-ufo ws)
           (ws-missile ws)
           (ws-score ws)))

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


;==================================================================================
; Launch the world
;==================================================================================

(big-bang
 (make-ws
  (/ WIDTH 2)
  (make-ufo (make-posn (random WIDTH) 0) (make-posn UFO-VEL-X UFO-VEL-Y) 200)
  #false 0)
 [on-tick  tickh]
 [on-key keyh]
 [to-draw  render])







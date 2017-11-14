;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;=======================================
; HAPPY CAT
;=======================================
; A cat wanders randomly on the screen. It has a happiness level
; and there is a gauge on the screen. The cat gets tired from all
; the walking and its happiness level goes down. The cat's speed
; depends on its happiness level. It moves faster when happy.
;
; Use your mouse and keyboard to feed and stroke the cat.
; If you press "up", you feed the cat (an image of food appears
; on the screen for a few moments). If you move your cursor
; over the cat, you stroke it (the cat is moving slowly around the
; cursor). Feeding and stroking increases your pet's happiness level.
;
; Author: Roman Kunin (mosceo@gmail.com)
; Source: https://github.com/mosceo/htdp

(require math)
(require 2htdp/image)
(require 2htdp/universe)


;=======================================
; Constants and data definitions
;=======================================

(define CAT (bitmap "cat.png"))
(define FOOD (bitmap "food.png"))
(define CAT-RADIUS (/ (image-width CAT) 2))
(define WIDTH 600)
(define HEIGHT 400)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "white"))
(define GAUGE-WIDTH 20)
(define GAUGE-HEIGHT 200)

(define SPEED-STROKE 1)
(define SPEED-MAX 10)
(define TICKS-DIR-NORMAL  50)
(define TICKS-DIR-STROKE 10)
(define TICKS-STROKE 50)
(define TICKS-FOOD 20)
(define HAP-TIME 0.2)
(define HAP-FOOD 10)
(define HAP-STROKE 0.3)

(define PCENTER (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
(define EPSILON 0.001)

(define-struct world [pos dir hap stroke food counter])
; A WorldState is a structure :
;   (make-world (make-posn Number Number) Number Number Number Number Number)
; representing a given world state where
; 'pos' - the position of the cat on canvas
; 'dir' - the direction (in degrees) in which the cat is moving
; 'hap' - a number [0..100], which represents the happines of the cat
; 'stroke' - a non-negative number, meaning for how many ticks the cat
;            will remain in a special state after it has been stroked
; 'food' - a non-negative number, meaning for how many ticks the cat
;          will remain in a special state after it has been stroked
; 'counter' - records the number of ticks since the beggining of the world


;=======================================
; Tick
;=======================================

; WorldState -> WorldState
; handles clock events
(define (tick w)
  (make-world (new-pos w)
              (new-dir w)
              (minus0 (world-hap w) HAP-TIME)
              (dec0 (world-stroke w))
              (dec0 (world-food w))
              (+ (world-counter w) 1)))


; WorldState -> Posn
; computes new position for a given world state
(define (new-pos w)
  (make-posn (rmod (new-x w) WIDTH)
             (rmod (new-y w) HEIGHT)))


; WorldState -> Number
; computes new x-coordinate for a given world state
(check-within (new-x (make-world (make-posn 10 20) 270 50 0 0 1))
              10 EPSILON)
(define (new-x w)
  (+ (x w) (* (dx w) (speed w))))


; WorldState -> Number
; computes new y-coordinate for a given world state
(check-within (new-y (make-world (make-posn 10 20) 270 50 0 0 1))
              (+ 20 (/ SPEED-MAX 2)) EPSILON)
(define (new-y w)
  (+ (y w) (* (dy w) (speed w))))


; WorldState -> Number
; returns the x-coordinate of the cat
(check-expect (x (make-world (make-posn 10 20) 270 80 0 0 123)) 10)
(define (x w)
  (posn-x (world-pos w)))


; WorldState -> Number
; returns the y-coordinate of the cat
(check-expect (y (make-world (make-posn 10 20) 270 80 0 0 123)) 20)
(define (y w)
  (posn-y (world-pos w)))


; WorldState -> Number
; change in x if the speed is 1
(check-within (dx (make-world PCENTER 270 80 0 0 123)) 0 EPSILON)
(define (dx w)
  (cos (rad w)))


; WorldState -> Number
; change in y if the speed is 1
(check-within (dy (make-world PCENTER 270 80 0 0 123)) 1 EPSILON)
(define (dy w)
  (- (sin (rad w))))


; WorldState -> Number
; computes the direction in radians for a given world state
(check-within (rad (make-world PCENTER 270 80 0 0 123)) (degrees->radians 270) EPSILON)
(define (rad w)
  (degrees->radians (world-dir w)))


; WorldState -> Number
; compute the speed in a given world state
(check-expect (speed (make-world PCENTER 270 80 2 0 123)) SPEED-STROKE)
(check-expect (speed (make-world PCENTER 270 100 0 0 123)) SPEED-MAX)
(check-expect (speed (make-world PCENTER 270 50 0 0 123)) (/ SPEED-MAX 2))
(define (speed w)
  (cond [(stroke? w) SPEED-STROKE]
        [else (* (hap01 w) SPEED-MAX)]))


; WorldState -> Direction
; computes the new direction
(check-expect (new-dir (make-world PCENTER 270 80 0 0 1)) 270)
(define (new-dir w)
  (if (change-dir? w) (random 360) (world-dir w)))


; WorldState -> Boolean
; is it the time to change the direction?
(check-expect (change-dir? (make-world PCENTER 270 80 0 0 1)) #false)
(check-expect (change-dir? (make-world PCENTER 270 80 0 0 TICKS-DIR-NORMAL)) #true)
(check-expect (change-dir? (make-world PCENTER 270 80 0 0 TICKS-DIR-STROKE)) #false)
(check-expect (change-dir? (make-world PCENTER 270 80 2 0 TICKS-DIR-STROKE)) #true)
(define (change-dir? w)
  (cond [(stroke? w) (zero? (modulo (world-counter w) TICKS-DIR-STROKE))]
        [else        (zero? (modulo (world-counter w) TICKS-DIR-NORMAL))]))


; WorldState -> Boolean
; has the cat been stroked recently?
(check-expect (stroke? (make-world PCENTER 270 80 0 0 0)) #false)
(check-expect (stroke? (make-world PCENTER 270 80 5 0 0)) #true)
(define (stroke? w)
  (> (world-stroke w) 0))


; Number -> Number
; decrements a number, but doesn't let the result drop below zero 
(check-expect (dec0 3) 2)
(check-expect (dec0 1) 0)
(check-expect (dec0 0) 0)
(define (dec0 n)
  (minus0 n 1))


; Number -> Number
; subtracts one number from the other, but doesn't let the result drop below zero
(check-expect (minus0 3 1) 2)
(check-expect (minus0 1 1) 0)
(check-expect (minus0 1 1.1) 0)
(define (minus0 x dx)
  (max (- x dx) 0))


; Number -> Number
; almost the same as 'module', but works for inexect numbers
(check-expect (rmod -1 5) 4)
(check-expect (rmod 3 5) 3)
(check-expect (rmod 7 5) 2)
(define (rmod x v)
  (cond [(< x 0) (+ x v)]
        [(> x v) (- x v)]
        [else x]))


;=======================================
; Render
;=======================================

; WorldState -> Image
; renders the world state
(define (render w)
  (food-icon w (cat-gauge w)))


; WorldState -> Image
; creates an image of the scene with the cat and its happiness gauge
(define (cat-gauge w)
  (overlay/align "right" "top" (gauge w)
                 (place-image CAT (x w) (y w) BACKGROUND)))


; WorldState Image -> Image
; if the cat has been recently fed, adds the food icon to the image
(define (food-icon w im)
  (if (food? w) (overlay FOOD im) im))


; WorldState -> Image
; creates the image of the happiness gauge
(define (gauge w)
  (overlay/align "center" "bottom"
                 (rectangle GAUGE-WIDTH (* (hap01 w) GAUGE-HEIGHT) "solid" "blue")
                 (empty-scene GAUGE-WIDTH GAUGE-HEIGHT)))


; WorldState -> Boolean
; has the cat been fed recently?
(check-expect (food? (make-world PCENTER 270 80 0 0 123)) #false)
(check-expect (food? (make-world PCENTER 270 80 0 2 123)) #true)
(define (food? w)
  (> (world-food w) 0))


; WorldState -> Number
; converts the happiness level to the range [0..1]
(define (hap01 w)
  (/ (world-hap w) 100))


;=======================================
; Mouse
;=======================================

; WorldState Number Number MouseEvent -> WorldState
; handles mouse events
(check-expect (mouse (make-world (make-posn 10 20) 270 20 0 0 1) 10 20 "enter")
              (make-world (make-posn 10 20) 270 20 0 0 1))
(check-expect (mouse (make-world (make-posn 10 20) 270 20 0 0 1) 8 22 "move")
              (make-world (make-posn 10 20) 270 (+ 20 HAP-STROKE) TICKS-STROKE 0 1))
(define (mouse w mx my event)
  (if (and (mouse=? event "move") (over-cat? w mx my))
      (stroke w) w))


; WorldState Number Number -> Boolean
; is the mouse cursor hovers over the cat?
(check-expect (over-cat? (make-world (make-posn 10 20) 270 20 0 0 1) 8 22) #true)
(check-expect (over-cat? (make-world (make-posn 10 20) 270 20 0 0 1) 800 220) #false)
(define (over-cat? w mx my)
  (<= (distance (world-pos w) (make-posn mx my))
      CAT-RADIUS))


; WorldState -> WorldState
; updates a given world state after the cat has been stroked
(check-expect (stroke (make-world PCENTER 270 20 0 0 1))
              (make-world PCENTER 270 (+ 20 HAP-STROKE) TICKS-STROKE 0 1))
(check-expect (stroke (make-world PCENTER 270 99.999 0 0 1))
              (make-world PCENTER 270 100 TICKS-STROKE 0 1))
(define (stroke w)
  (make-world (world-pos w) (world-dir w) (new-hap-stroke w)
              TICKS-STROKE (world-food w) (world-counter w)))


; WorldState -> Number
; computes the new happines level after the cat has been stroked
(check-expect (new-hap-stroke (make-world PCENTER 270 20 0 2 123)) (+ 20 HAP-STROKE))
(check-expect (new-hap-stroke (make-world PCENTER 270 99.999 0 2 123)) 100)
(define (new-hap-stroke w)
  (min 100 (+ HAP-STROKE (world-hap w))))


; Posn Posn -> Number
; computes the distance between two points
(check-within (distance (make-posn 1 2) (make-posn 1 2)) 0 EPSILON)
(check-within (distance (make-posn 1 2) (make-posn 4 6)) 5 EPSILON)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


;=======================================
; Key
;=======================================

; WorldState -> WorldState
; handles keyboard events
(check-expect (key (make-world PCENTER 270 20 0 0 0) "down")
              (make-world PCENTER 270 20 0 0 0))
(check-expect (key (make-world PCENTER 270 20 0 0 0) "up")
              (make-world PCENTER 270 (+ 20 HAP-FOOD) 0 TICKS-FOOD 0))
(check-expect (key (make-world PCENTER 270 99 0 2 0) "up")
              (make-world PCENTER 270 100 0 TICKS-FOOD 0))
(define (key w event)
  (if (key=? event "up") (food w) w))


; WorldState -> WorldState
; updates a given world state after the cat has been fed
(define (food w)
  (make-world (world-pos w) (world-dir w) (new-hap-food w)
              (world-stroke w) TICKS-FOOD (world-counter w)))


; WorldState -> Number
; computes the new happines level after the cat has been fed
(check-expect (new-hap-food (make-world PCENTER 270 20 0 2 123)) (+ 20 HAP-FOOD))
(check-expect (new-hap-food (make-world PCENTER 270 99 0 2 123)) 100)
(define (new-hap-food w)
  (min 100 (+ HAP-FOOD (world-hap w))))


;=======================================
; Launch the world
;=======================================

(big-bang
 (make-world (make-posn (random WIDTH) (random HEIGHT)) (random 360) 100 0 0 0)
 [on-tick  tick]
 [on-mouse mouse]
 [on-key   key]
 [to-draw  render])

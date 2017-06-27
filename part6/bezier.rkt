;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bezier) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/abstraction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Posn Posn -> Number
; compute the manhattan distance
(check-expect (dist (make-posn 10 20) (make-posn 15 10)) 15)

(define (dist a b)
  (+ (abs (- (posn-x a) (posn-x b)))
     (abs (- (posn-y a) (posn-y b)))))


; Posn Posn -> Posn
; compute the middle point between two
(check-expect (mid-point (make-posn 10 40) (make-posn 30 20))
              (make-posn 20 30))

(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2)
             (/ (+ (posn-y a) (posn-y b)) 2)))


; Posn Posn Posn Image -> Image
; connect a and c with a curve
(define (bezier/r a b c)
  (local ((define scene1 (bezier/r a b c (empty-scene 400 400)))
          (define P (square 5 "solid" "blue"))
          (define scene2 (place-image P (posn-x a) (posn-y a) scene1))
          (define scene3 (place-image P (posn-x b) (posn-y b) scene2))
          (define scene4 (place-image P (posn-x c) (posn-y c) scene3)))
    scene4))


(define (bezier/r a b c image)
  (cond [(< (dist a c) 1) (add-line image (posn-x a) (posn-y a)
                                    (posn-x c) (posn-y c) "red")]
        [else (local ((define ba (mid-point a b))
                      (define bc (mid-point b c))
                      (define abc (mid-point ba bc))
                      (define scene1 (bezier/r a ba abc image))
                      (define scene2 (bezier/r c bc abc scene1)))
                scene2)]))




(bezier (make-posn 10 10) (make-posn 10 390) (make-posn 390 390) (empty-scene 400 400))















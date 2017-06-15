;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname elimination) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]


(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define TM ; a triangular SOE
  (list (list 2 2  3 10)
        (list   3  9 21)
        (list      1  2)))

(define S '(1 1 2)) ; a Solution

(define M2
  (list (list 2  3  3 8)
        (list 2  3 -2 3)
        (list 4 -2  2 4)))

(define TM2
  (list (list 2  3  3   8)
        (list   -8 -4 -12)
        (list      -5  -5)))

(define S2 '(1 1 1))


(define EPSILON 0.00001)


; Number Number -> Boolean
; check if two numbers are almost equal
(check-expect (close? 1 1) #t)
(check-expect (close? 1 1.000000000000000001) #t)
(check-expect (close? 1 2) #f)

(define (close? a b)
  (< (abs (- a b)) EPSILON))


; Equation -> [List-of Number]
; extract the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))


; Equation -> Number
; extract the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))


; SOE Solution -> Boolean
; check if a given solution is indeed a solution for a given matrix
(check-expect (check-solution M S) #t)
(check-expect (check-solution M '(1 2 3)) #f)

(define (check-solution soe sol)
  (local ((define res (andmap (lambda (e) (close? (plug-in (lhs e) sol) (rhs e))) soe)))
    (if (boolean? res) res #t)))


; [List-of Number] Solution -> Number
; compute the left-hand side of an equation for a given set of values
; (compute the dot product of two vectors)
(check-expect (plug-in '(2) '(3)) 6)
(check-expect (plug-in '(2 1 0) '(3 -7 4)) -1)

(define (plug-in co values)
  (cond [(empty? (rest co)) (* (first co) (first values))]
        [else (+ (* (first co) (first values))
                 (plug-in (rest co) (rest values)))]))


; Equation Equation -> Equation 
; subtract a multiple of the second equation from the first,
; so that the resulting Equation has a 0 in the first position
(check-expect (subtract '(2 5 10) '(2 1 2)) '(0 4 8))
(check-expect (subtract '(4 5 10) '(-2 1 2)) '(0 7 14))

(define (subtract e1 e2)
  (local ((define m (/ (first e1) (first e2))))
    (map (lambda (n1 n2) (- n1 (* m n2))) e1 e2)))


; A TM is a [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix


; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate (list '(1 2))) (list '(1 2)))
(check-expect (triangulate M) TM)
(check-expect (triangulate M2) TM2)

(define (triangulate M)
  (cond [(= (length M) 1) M]
        [else (local ((define L (right-form M))
                      (define e1 (first L))
                      (define sL (map (lambda (e) (rest (subtract e e1))) (rest L))))
                (cons e1 (triangulate sL)))]))


; SOE -> SOE
; rotate the matrix until c11 is not zero
(check-expect (right-form (list (list 0 1 1 1)
                                (list 7 2 2 2)
                                (list 0 3 3 3)))
              (list (list 7 2 2 2)
                    (list 0 3 3 3)
                    (list 0 1 1 1)))

(define (right-form M)
  (local ((define e11 (first (first M))))
    (cond [(all-zeros? M) (error "there is no right-form")]
          [(not (close? e11 0)) M]
          [else (right-form (rotate M))])))


; SOE -> SOE
; swap the first and the last equations in a system
(check-expect (rotate (list '(1 1 0) '(2 2 0))) (list '(2 2 0) '(1 1 0)))
(define (rotate M)
  (append (rest M) (list (first M))))


; SOE -> Boolean
; check if all leading entries in a system are zeros
(check-expect (all-zeros? (list (list 0 1 1 2) (list 0 2 2 2) (list 0 3 3 2))) #t)
(check-expect (all-zeros? (list (list 0 1 1 2) (list 0 2 2 2) (list 1 3 3 2))) #f)

(define (all-zeros? M)
  (not (ormap (lambda (e) (not (close? 0 (first e)))) M)))


; TM -> Solution
; find the solution given a triangular matrix
(check-expect (solve TM) S)

(define (solve M)
  (cond [(= (length M) 1) (list (solve-entry (first M) '()))]
        [else (local ((define sol (solve (rest M)))
                      (define x (solve-entry (first M) sol)))
                (cons x sol))]))


; Equation Solution -> Number
; find the value of the first variable given values for others
(check-expect (solve-entry (list 2 10) (list)) 5)
(check-expect (solve-entry (list 2 1 3 10) (list 2 4)) -2)

(define (solve-entry e sol)
  (/ (- (rhs e) (plug-in (lhs e) (cons 0 sol)))
     (first e)))


; SOE -> Solution
; find a solution for a given system of linear equations
(check-expect (gauss M) S)
(check-expect (gauss M2) S2)

(define (gauss M)
  (solve (triangulate M)))

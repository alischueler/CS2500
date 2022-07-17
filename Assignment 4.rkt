;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |04 Assignment|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise 1

; A Shape is one of: 
; - Circle 
; - Square
; - Rectangle

(define-struct circl (x y r outline c))
; A Circle is a (make-circl Number Number Number Boolean Symbol)
; interpretation: x and y determine the center of the circle,
;   r the radius, outline whether it's outlined or solid,
;   and c its color

(define CIRCLE1 (make-circl 0 0 15 'solid 'blue))
(define CIRCLE2 (make-circl 100 50 15 'solid 'red))

(define-struct squar (x y size outline c))
; A Square is a (make-squar Number Number Number Boolean Symbol)
; interpretation: x and y determine the center of the sqaure on the canvas,
; size determines the side lengths of the square, outline determines whether its outlines, or solid
; c represents the color of the square

(define SQUARE1 (make-squar 0 0 15 'outline 'blue))
(define SQUARE2 (make-squar 100 50 15 'outline 'red))

(define-struct recta (x y width height outline c))
; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
; interpretation: x and y determine the center of the rectangle on the canvas,
; width determines the width of the rectangle, height determines the height of the rectangle,
; outline determines whether it is outlined or solid, c represents the color

(define REC1 (make-recta 0 0 15 30 'solid 'green))
(define REC2 (make-recta 100 50 30 40 'solid 'purple))

(define BG (empty-scene 500 500))

; Exercise 2
;; shape-temp: Shape -> ?
#;(define (shape-temp s)
    (cond [(circl? s) ... (circle-temp s)]
          [(squar? s) ... (square-temp s)]
          [(recta? s) ... (rectangle-temp s)]))

;; circle-temp: Circle -> ?
#;(define (circle-temp s)
    (circl-x s) ... (circl-y s) ... (circl-r s) ... (circl-outline s) ... (circl-c s))

;; square-temp: Square -> ?
#;(define (square-temp s)
    (squar-x s) ... (squar-y s) ... (squar-size s) ... (squar-outline s) ... (squar-c s))

;; rectangle-temp: Rectangle -> ?
#;(define (rectangle-temp s)
    (recta-x s) ... (recta-y s) ... (recta-width s) ... (recta-height s) ... (recta-outline s)
    (recta-c s))

; Exercise 3
;; shape-shift-x : Shape Number -> Shape
;; moves a given shape left if given a negative number and right when given a positive one
(check-expect (shape-shift-x CIRCLE1 3) (make-circl 3 0 15 'solid 'blue))
(check-expect (shape-shift-x CIRCLE2 -10) (make-circl 90 50 15 'solid 'red))
(check-expect (shape-shift-x SQUARE1 3) (make-squar 3 0 15 'outline 'blue))
(check-expect (shape-shift-x SQUARE2 -10) (make-squar 90 50 15 'outline 'red))
(check-expect (shape-shift-x REC1 3) (make-recta 3 0 15 30 'solid 'green))
(check-expect (shape-shift-x REC2 -10) (make-recta 90 50 30 40 'solid 'purple))
(define (shape-shift-x sh delta)
  (cond [(circl? sh)
         (make-circl (+ (circl-x sh) delta) (circl-y sh)
                     (circl-r sh) (circl-outline sh) (circl-c sh))]
        [(squar? sh)
         (make-squar (+ (squar-x sh) delta)
                     (squar-y sh) (squar-size sh) (squar-outline sh) (squar-c sh))]
        [(recta? sh)
         (make-recta (+ (recta-x sh) delta)
                     (recta-y sh) (recta-width sh)
                     (recta-height sh) (recta-outline sh) (recta-c sh))]))

; Exercise 4
;; shape-in? : Shape Posn -> Boolean
;; determines if a given Posn is on or within the bounds of a given shape
(check-expect (shape-in? CIRCLE1 (make-posn 400 400)) #false)
(check-expect (shape-in? CIRCLE2 (make-posn 110 60)) #true)
(check-expect (shape-in? CIRCLE1 (make-posn 15 15)) #true)
(check-expect (shape-in? SQUARE1 (make-posn 400 400)) #false)
(check-expect (shape-in? SQUARE2 (make-posn 101 52)) #true)
(check-expect (shape-in? SQUARE1 (make-posn 7.5 7.5)) #true)
(check-expect (shape-in? REC1 (make-posn 400 400)) #false)
(check-expect (shape-in? REC2 (make-posn 110 60)) #true)
(check-expect (shape-in? REC1 (make-posn 7.5 15)) #true)
(define (shape-in? sh p)
  (cond [(circl? sh) (circle-in? sh p)]
        [(squar? sh) (squar-in? sh p)]
        [(recta? sh) (recta-in? sh p)]))

;; circle-in? : Circle Posn -> Boolean
;; determines if a given posn is on or within the bounds of a circle
(check-expect (circle-in? CIRCLE1 (make-posn 400 400)) #false)
(check-expect (circle-in? CIRCLE2 (make-posn 110 60)) #true)
(check-expect (circle-in? CIRCLE1 (make-posn 15 15)) #true)
(define (circle-in? sh p)
  (and (<= (- (circl-x sh) (circl-r sh)) (posn-x p) (+ (circl-x sh) (circl-r sh)))
       (<= (- (circl-y sh) (circl-r sh)) (posn-y p) (+ (circl-y sh) (circl-r sh)))))

;; squar-help : Square Posn -> Boolean
;; determines if a given posn is on or within the bounds of a given square
(check-expect (squar-in? SQUARE1 (make-posn 400 400)) #false)
(check-expect (squar-in? SQUARE2 (make-posn 101 52)) #true)
(check-expect (squar-in? SQUARE1 (make-posn 7.5 7.5)) #true)
(define (squar-in? sh p)
  (and (<= (- (squar-x sh) (/ (squar-size sh) 2)) (posn-x p)
           (+ (squar-x sh) (/ (squar-size sh) 2)))
       (<= (- (squar-y sh) (/ (squar-size sh) 2)) (posn-y p)
           (+ (squar-y sh) (/ (squar-size sh) 2)))))

;; recta-help : Rectangle Posn -> Boolean
;; determines if a given posn is on or within the bounds of a given rectangle
(check-expect (recta-in? REC1 (make-posn 400 400)) #false)
(check-expect (recta-in? REC2 (make-posn 110 60)) #true)
(check-expect (recta-in? REC1 (make-posn 7.5 15)) #true)
(define (recta-in? sh p)
  (and (<= (- (recta-x sh) (/ (recta-width sh) 2)) (posn-x p)
           (+ (recta-x sh) (/ (recta-width sh) 2)))
       (<= (- (recta-y sh) (/ (recta-height sh) 2)) (posn-y p)
           (+ (recta-y sh) (/ (recta-height sh) 2)))))

        
; Exercise 5
;; shape-draw : Shape Image -> Image
;; draws a given shape onto a background at a given position
(check-expect (shape-draw CIRCLE1 BG) (place-image (circle 15 'solid 'blue) 0 0 BG))
(check-expect (shape-draw CIRCLE2 BG) (place-image (circle 15 'solid 'red) 100 50 BG))
(check-expect (shape-draw SQUARE1 BG) (place-image (square 15 'outline 'blue) 0 0 BG))
(check-expect (shape-draw SQUARE2 BG) (place-image (square 15 'outline 'red) 100 50 BG))
(check-expect (shape-draw REC1 BG) (place-image (rectangle 15 30 'solid 'green) 0 0 BG))
(check-expect (shape-draw REC2 BG) (place-image (rectangle 30 40 'solid 'purple) 100 50 BG))
(define (shape-draw sh bg)
  (cond [(circl? sh) (draw-circle sh bg)]
        [(squar? sh) (draw-square sh bg)]
        [(recta? sh) (draw-rectangle sh bg)]))

;; draw-circle : Circle -> Image
;; draws a circle onto a background at a given position
(check-expect (draw-circle CIRCLE1 BG) (place-image (circle 15 'solid 'blue) 0 0 BG))
(check-expect (draw-circle CIRCLE2 BG) (place-image (circle 15 'solid 'red) 100 50 BG))
(define (draw-circle sh bg)
  (place-image (circle (circl-r sh) (circl-outline sh) (circl-c sh))
               (circl-x sh) (circl-y sh) bg))

;; draw-square : Square -> Image
;; draws a square onto a background at a given position
(check-expect (draw-square SQUARE1 BG) (place-image (square 15 'outline 'blue) 0 0 BG))
(check-expect (draw-square SQUARE2 BG) (place-image (square 15 'outline 'red) 100 50 BG))
(define (draw-square sh bg)
  (place-image (square (squar-size sh) (squar-outline sh) (squar-c sh))
               (squar-x sh) (squar-y sh) bg))
  
;; draw-rectangle : Rectangle -> Image
;; draws a rectangle onto a background at a given expression
(check-expect (draw-rectangle REC1 BG) (place-image (rectangle 15 30 'solid 'green) 0 0 BG))
(check-expect (draw-rectangle REC2 BG) (place-image (rectangle 30 40 'solid 'purple) 100 50 BG))
(define (draw-rectangle sh bg)
  (place-image (rectangle (recta-width sh) (recta-height sh) (recta-outline sh) (recta-c sh))
               (recta-x sh) (recta-y sh) bg))
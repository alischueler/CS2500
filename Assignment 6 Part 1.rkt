;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |06 Assignment 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; Data Definitions
; An Invader is a Posn
; INTERP: represents the location of the invader
 
; A Bullet is a Posn
; INTERP: represents the location of a bullet
 
; A Location is a Posn
; INTERP: represents a location of a spaceship
 
; A Direction is one of:
; - 'left
; - 'right
; INTERP: represent direction of movement for the spaceship
 
(define-struct ship (dir loc))
; A Ship is (make-ship Direction Location)
; INTERP: represent the spaceship with its current direction
;         and movement

; A List of Invaders (LoI) is one of
; - '()
; - (cons Invader LoI)
 
; A List of Bullets (LoB) is one of
; - '()
; - (cons Bullet LoB)
 
(define-struct world (ship invaders ship-bullets invader-bullets))
; A World is (make-world Ship LoI LoB LoB)
; INTERP: represent the ship, the current list of invaders, the inflight spaceship bullets
;         and the inflight invader bullets
 

;;; Constants
(define WIDTH 500)
(define HEIGHT 500)
 
(define MAX-SHIP-BULLETS 3)
 
(define MAX-INVADER-BULLETS 10)
 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
 
(define SPACESHIP-BULLET-IMAGE (circle 2 'solid 'black))
 
(define SHIP-WIDTH 25)
 
(define SHIP-HEIGHT 15)
 
(define SPACESHIP-IMAGE (rectangle SHIP-WIDTH SHIP-HEIGHT 'solid 'black))
 
(define INVADER-SIDE 20)
 
(define INVADER-IMAGE (square INVADER-SIDE 'solid 'red))
 
(define INVADER-BULLET-IMAGE (circle 2 'solid 'red))
 
(define SHIP-SPEED 10)
 
(define BULLET-SPEED 10)

(define YPOS 480)

(define MAX-X (- WIDTH (/ SHIP-WIDTH 2)))
(define MIN-X (+ (/ SHIP-WIDTH 2) 0))
 
(define SHIP-INIT (make-ship 'left (make-posn 250 480)))
(define SHIP0 (make-ship 'left (make-posn 368 YPOS)))
(define SHIP1 (make-ship 'right (make-posn 450 YPOS)))
(define SHIP2 (make-ship 'right (make-posn MAX-X YPOS)))
(define SHIP3 (make-ship 'left (make-posn MIN-X YPOS)))
(define SHIP4 (make-ship 'left (make-posn MAX-X YPOS)))
(define SHIP5 (make-ship 'right (make-posn MIN-X YPOS)))
 
(define INVADERS-INIT
  (list (make-posn 100 20) (make-posn 140 20) (make-posn 180 20)
        (make-posn 220 20) (make-posn 260 20) (make-posn 300 20)
        (make-posn 340 20) (make-posn 380 20) (make-posn 420 20)
        (make-posn 100 50) (make-posn 140 50) (make-posn 180 50)
        (make-posn 220 50) (make-posn 260 50) (make-posn 300 50)
        (make-posn 340 50) (make-posn 380 50) (make-posn 420 50)
        (make-posn 100 80) (make-posn 140 80) (make-posn 180 80)
        (make-posn 220 80) (make-posn 260 80) (make-posn 300 80)
        (make-posn 340 80) (make-posn 380 80) (make-posn 420 80)
        (make-posn 100 110) (make-posn 140 110) (make-posn 180 110)
        (make-posn 220 110) (make-posn 260 110) (make-posn 300 110)
        (make-posn 340 110) (make-posn 380 110) (make-posn 420 110)))
(define INV1 (list (make-posn 100 50)
                   (make-posn 140 50)
                   (make-posn 180 50)))
(define INV2 (list (make-posn 200 100)
                   (make-posn 240 100)
                   (make-posn 280 100)))
              
(define BUL-LIST0 (list (make-posn 50 300)
                        (make-posn 30 400)
                        (make-posn 50 200)))
(define BUL-LIST1 (list (make-posn 100 200)
                        (make-posn 200 100)
                        (make-posn 300 200)))

(define BUL-LIST2 (list
                   (make-posn 450 470)
                   (make-posn 400 420)
                   (make-posn 350 370)
                   (make-posn 300 320)
                   (make-posn 250 270)
                   (make-posn 200 220)
                   (make-posn 150 170)
                   (make-posn 100 120)
                   (make-posn 50 70)
                   (make-posn 25 20)))
(define BUL-LIST3 (list
                   (make-posn 470 450)
                   (make-posn 420 400)
                   (make-posn 370 350)
                   (make-posn 320 300)
                   (make-posn 270 250)
                   (make-posn 220 200)
                   (make-posn 170 150)
                   (make-posn 120 100)
                   (make-posn 70 50)
                   (make-posn 20 25)))

(define BUL-LIST4 (list
                   (make-posn 470 450)
                   (make-posn 420 400)
                   (make-posn 370 350)))
 
(define WORLD-INIT (make-world SHIP-INIT INVADERS-INIT empty empty))
(define WORLD0 (make-world SHIP1 INV1 BUL-LIST0 BUL-LIST1))
(define WORLD1 (make-world SHIP0 INV2 empty empty))


;;; Templates
;; direction-temp: Direction -> ?
#;(define (direction-temp d)
    (cond [(string=? "left" d)... ]
          [(string=? "right" d)... ]))

;; ship-temp: Ship -> ?
#;(define (ship-temp sh)
    ...(direction-temp (ship-dir sh)) ... (posn-temp (ship-loc sh))... )

;; posn-temp: Posn -> ?
#;(define (posn-temp p)
    ...(posn-x p) ... (posn-y p) ...)

;; lob-temp: LoB -> ?
#;(define (lob-temp alob)
    (cond [(empty? alob) ...]
          [(cons? alob) ... (posn-temp (first alob)) ... (lob-temp (rest alob))]))
 
;; loi-temp LoI -> >
#;(define (loi-temp aloi)
    (cond [(empty? aloi) ...]
          [(cons? aloi) ... (posn-temp (first aloi)) ... (loi-temp (rest aloi))]))

;; world-temp: World -> ?
#;(define (world-temp w)
    ... (ship-temp (world-ship w))...
    (loi-temp (world-invaders w))...
    (lob-temp (world-ship-bullets w))...
    (lob-temp (world-invader-bullets w)) ...)

; world-draw : World -> Image
; Draw the world on the canvas
(check-expect (world-draw WORLD0)
              (place-image
               SPACESHIP-IMAGE 450 480
               (place-image
                SPACESHIP-BULLET-IMAGE 50 300
                (place-image
                 SPACESHIP-BULLET-IMAGE 30 400
                 (place-image
                  SPACESHIP-BULLET-IMAGE 50 200
                  (place-image
                   INVADER-BULLET-IMAGE 100 200
                   (place-image
                    INVADER-BULLET-IMAGE 200 100
                    (place-image
                     INVADER-BULLET-IMAGE 300 200
                     (place-image
                      INVADER-IMAGE 100 50
                      (place-image
                       INVADER-IMAGE 140 50
                       (place-image
                        INVADER-IMAGE 180 50 BACKGROUND)))))))))))
(check-expect (world-draw WORLD1)
              (place-image
               SPACESHIP-IMAGE 368 480
               (place-image
                INVADER-IMAGE 200 100
                (place-image
                 INVADER-IMAGE 240 100
                 (place-image
                  INVADER-IMAGE 280 100 BACKGROUND)))))
(define (world-draw aworld)
  (draw-ship (world-ship aworld)
             (draw-bullets (world-ship-bullets aworld) SPACESHIP-BULLET-IMAGE
                           (draw-bullets (world-invader-bullets aworld) INVADER-BULLET-IMAGE
                                         (draw-invaders (world-invaders aworld))))))

;; draw-ship: Ship Image -> Image
;; draws the ship onto the background
(check-expect (draw-ship SHIP0 BACKGROUND) (place-image SPACESHIP-IMAGE 368 480 BACKGROUND))
(check-expect (draw-ship SHIP1 BACKGROUND) (place-image SPACESHIP-IMAGE 450 480 BACKGROUND))
(define (draw-ship p img-to-draw-on)
  (place-image SPACESHIP-IMAGE (posn-x (ship-loc p)) YPOS img-to-draw-on))

;; draw-bullets: LoB Image -> Image
;; draws the bullets onto the background
(check-expect (draw-bullets BUL-LIST0 SPACESHIP-BULLET-IMAGE BACKGROUND)
              (place-image SPACESHIP-BULLET-IMAGE 50 300
                           (place-image SPACESHIP-BULLET-IMAGE 30 400
                                        (place-image SPACESHIP-BULLET-IMAGE 50 200 BACKGROUND))))
(check-expect (draw-bullets BUL-LIST1 INVADER-BULLET-IMAGE BACKGROUND)
              (place-image INVADER-BULLET-IMAGE 100 200
                           (place-image INVADER-BULLET-IMAGE 200 100
                                        (place-image INVADER-BULLET-IMAGE 300 200 BACKGROUND))))
(define (draw-bullets alob img-to-draw img-to-draw-on)
  (cond [(empty? alob) img-to-draw-on]
        [else (draw-first-bullet img-to-draw (first alob)
                                 (draw-bullets (rest alob) img-to-draw img-to-draw-on))]))

;; draw-first-bullet : Image Bullet Image -> Image
;; draws a bullet onto the background
(check-expect (draw-first-bullet INVADER-BULLET-IMAGE (make-posn 40 50) BACKGROUND)
              (place-image INVADER-BULLET-IMAGE 40 50 BACKGROUND))
(check-expect (draw-first-bullet SPACESHIP-BULLET-IMAGE (make-posn 100 340) BACKGROUND)
              (place-image SPACESHIP-BULLET-IMAGE 100 340 BACKGROUND))
(define (draw-first-bullet img-to-draw abull img-to-draw-on)
  (place-image img-to-draw (posn-x abull) (posn-y abull) img-to-draw-on))

;; draw-invaders : LoI -> Image
;; draws invaders onto the background
(check-expect (draw-invaders INV1)
              (place-image INVADER-IMAGE 100 50
                           (place-image INVADER-IMAGE 140 50
                                        (place-image INVADER-IMAGE 180 50 BACKGROUND))))
(check-expect (draw-invaders INV2)
              (place-image INVADER-IMAGE 200 100
                           (place-image INVADER-IMAGE 240 100
                                        (place-image INVADER-IMAGE 280 100 BACKGROUND))))
(define (draw-invaders aloi)
  (cond [(empty? aloi) BACKGROUND]
        [else (draw-first-invader (first aloi)
                                  (draw-invaders (rest aloi)))]))

;; draw-first-invader : Invader -> Image
;; draws an invader onto the background
(check-expect (draw-first-invader (make-posn 50 50) BACKGROUND)
              (place-image INVADER-IMAGE 50 50 BACKGROUND))
(check-expect (draw-first-invader (make-posn 100 30) BACKGROUND)
              (place-image INVADER-IMAGE 100 30 BACKGROUND))
(define (draw-first-invader inv img-to-draw-on)
  (place-image INVADER-IMAGE (posn-x inv) (posn-y inv) img-to-draw-on))

;; move-spaceship: Ship -> Ship
;; moves the ship in the appropriate direction
(check-expect (move-spaceship SHIP0) (make-ship 'left (make-posn 358 480)))
(check-expect (move-spaceship SHIP1) (make-ship 'right (make-posn 460 480)))
(check-expect (move-spaceship SHIP2) (make-ship 'right (make-posn 487.5 480)))
(check-expect (move-spaceship SHIP3) (make-ship 'left (make-posn 12.5 480)))
(check-expect (move-spaceship SHIP4) (make-ship 'left (make-posn 477.5 480)))
(check-expect (move-spaceship SHIP5) (make-ship 'right (make-posn 22.5 480)))
(define (move-spaceship aship)
  (if (symbol=? 'right (ship-dir aship))
      (moving-right (ship-loc aship) (ship-dir aship))
      (moving-left (ship-loc aship) (ship-dir aship))))

;; moving-right: Location Direction -> Ship
;; creates a Ship from a given location and Direction when moving right
(check-expect (moving-right (make-posn 360 480) 'right) (make-ship 'right (make-posn 370 480)))
(check-expect (moving-right (make-posn MAX-X 480) 'right) (make-ship 'right (make-posn 487.5 480)))
(check-expect (moving-right (make-posn MIN-X 480) 'right) (make-ship 'right (make-posn 22.5 480)))
(define (moving-right aloc adir)
  (if (= (posn-x aloc) MAX-X)
      (make-ship adir (make-posn (posn-x aloc) YPOS))
      (make-ship adir (make-posn (+ (posn-x aloc) SHIP-SPEED) YPOS))))

;; moving-left: Location Direction -> Ship
;; creates a Ship from a given location and Direction when moving left
(check-expect (moving-left (make-posn 360 480) 'left) (make-ship 'left (make-posn 350 480)))
(check-expect (moving-left (make-posn MAX-X 480) 'left) (make-ship 'left (make-posn 477.5 480)))
(check-expect (moving-left (make-posn MIN-X 480) 'left) (make-ship 'left (make-posn 12.5 480)))
(define (moving-left aloc adir)
  (if (= (posn-x aloc) MIN-X)
      (make-ship adir (make-posn (posn-x aloc) YPOS))
      (make-ship adir (make-posn (- (posn-x aloc) SHIP-SPEED) YPOS))))
 
;; move-spaceship-bullets : LoB -> LoB
;; move each spaceship bullet in the list upwards by SPEED units
(check-expect (move-spaceship-bullets (list )) '())
(check-expect (move-spaceship-bullets BUL-LIST0) (list
                                                  (make-posn 50 290)
                                                  (make-posn 30 390)
                                                  (make-posn 50 190)))
(check-expect (move-spaceship-bullets BUL-LIST4) (list
                                                  (make-posn 470 440)
                                                  (make-posn 420 390)
                                                  (make-posn 370 340)))
(define (move-spaceship-bullets alosb)
  (cond [(empty? alosb) '()]
        [(cons? alosb)
         (cons (new-spaceship-bullet (first alosb)) (move-spaceship-bullets (rest alosb)))]))

;; new-spaceship-bullet : Bullet -> Bullet
;; moves a bullet upwards by SPEED units
(check-expect (new-spaceship-bullet (make-posn 100 50)) (make-posn 100 40))
(check-expect (new-spaceship-bullet (make-posn 400 360)) (make-posn 400 350))
(define (new-spaceship-bullet abull)
  (make-posn (posn-x abull) (- (posn-y abull) BULLET-SPEED)))

 
; move-invader-bullets : LoB -> LoB
; move each bullet in the list downwards by SPEED units
(check-expect (move-invader-bullets (list )) '())
(check-expect (move-invader-bullets BUL-LIST1) (list
                                                (make-posn 100 210)
                                                (make-posn 200 110)
                                                (make-posn 300 210)))
(check-expect (move-invader-bullets BUL-LIST3) (list
                                                (make-posn 470 460)
                                                (make-posn 420 410)
                                                (make-posn 370 360)
                                                (make-posn 320 310)
                                                (make-posn 270 260)
                                                (make-posn 220 210)
                                                (make-posn 170 160)
                                                (make-posn 120 110)
                                                (make-posn 70 60)
                                                (make-posn 20 35)))
(define (move-invader-bullets aloib)
  (cond [(empty? aloib) '()]
        [(cons? aloib)
         (cons (new-invader-bullet (first aloib)) (move-invader-bullets (rest aloib)))]))

;; new-invader-bullet : Bullet -> Bullet
;; moves a bullet downwards by SPEED units
(check-expect (new-invader-bullet (make-posn 100 50)) (make-posn 100 60))
(check-expect (new-invader-bullet (make-posn 400 360)) (make-posn 400 370))
(define (new-invader-bullet abul)
  (make-posn (posn-x abul) (+ (posn-y abul) BULLET-SPEED)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |06 ASSIGNMENT|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define BULLET-RAD 2)

(define SHIP-MAX-X (- WIDTH (/ SHIP-WIDTH 2)))

(define SHIP-MIN-X (+ (/ SHIP-WIDTH 2) 0))
 
(define SHIP-INIT (make-ship 'left (make-posn 250 YPOS)))
(define SHIP0 (make-ship 'left (make-posn 368 YPOS)))
(define SHIP1 (make-ship 'right (make-posn 450 YPOS)))
(define SHIP2 (make-ship 'right (make-posn SHIP-MAX-X YPOS)))
(define SHIP3 (make-ship 'left (make-posn SHIP-MIN-X YPOS)))
(define SHIP4 (make-ship 'left (make-posn SHIP-MAX-X YPOS)))
(define SHIP5 (make-ship 'right (make-posn SHIP-MIN-X YPOS)))
 
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
(define BUL-LIST5 (list (make-posn 355.5 472.5)
                        (make-posn 30 50)))
(define BUL-LIST6 (list (make-posn 355.5 SHIP-MAX-X)
                        (make-posn 40 70)))
(define BUL-LIST7 (list (make-posn 380.5 472.5)
                        (make-posn 35 85)))
(define BUL-LIST8 (list (make-posn 380.5 SHIP-MAX-X)
                        (make-posn 25 64)))
(define BUL-LIST9 (list (make-posn 300 SHIP-MAX-X)
                        (make-posn 25 64)))
(define BUL-LIST10 (list (make-posn 360 400)
                         (make-posn 25 64)))
(define BUL-LIST11 (list (make-posn 360 YPOS)
                         (make-posn 25 64)))
(define BUL-LIST12 (list (make-posn 200 200)
                         (make-posn 25 64)))
(define BUL-LIST13 (list (make-posn 450 480)
                         (make-posn 30 70)))
(define BUL-LIST14 (list (make-posn 40 356)
                         (make-posn 23 57)
                         (make-posn 362 484)))

(define WORLD-INIT (make-world SHIP-INIT INVADERS-INIT empty empty))
(define WORLD0 (make-world SHIP1 INV1 BUL-LIST0 BUL-LIST1))
(define WORLD1 (make-world SHIP0 INV2 empty empty))
(define WORLD2 (make-world SHIP1 INV1 empty BUL-LIST13))
(define WORLD3 (make-world SHIP0 INV2 BUL-LIST4 BUL-LIST14))

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
               SPACESHIP-IMAGE 450 YPOS
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
               SPACESHIP-IMAGE 368 YPOS
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
(check-expect (draw-ship SHIP0 BACKGROUND) (place-image SPACESHIP-IMAGE 368 YPOS BACKGROUND))
(check-expect (draw-ship SHIP1 BACKGROUND) (place-image SPACESHIP-IMAGE 450 YPOS BACKGROUND))
(define (draw-ship aship img-to-draw-on)
  (place-image SPACESHIP-IMAGE (posn-x (ship-loc aship)) YPOS img-to-draw-on))

;; draw-bullets: LoB Image Image -> Image
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

;; draw-first-invader : Invader Image -> Image
;; draws an invader onto the background
(check-expect (draw-first-invader (make-posn 50 50) BACKGROUND)
              (place-image INVADER-IMAGE 50 50 BACKGROUND))
(check-expect (draw-first-invader (make-posn 100 30) BACKGROUND)
              (place-image INVADER-IMAGE 100 30 BACKGROUND))
(define (draw-first-invader inv img-to-draw-on)
  (place-image INVADER-IMAGE (posn-x inv) (posn-y inv) img-to-draw-on))

;; change-world : World -> World
;; changes the world at every tick
(check-random (change-world WORLD1)
              (make-world (make-ship 'left (make-posn 358 YPOS))
                          INV2
                          '()
                          (list
                           (make-posn (posn-x (list-ref INV2 (random (length INV1))))
                                      (+ 10 (posn-y (list-ref INV2 (random (length INV1)))))))))
(check-random (change-world WORLD0)
              (make-world (make-ship 'right (make-posn 460 YPOS))
                          INV1
                          (list
                           (make-posn 50 290)
                           (make-posn 30 390)
                           (make-posn 50 190))
                          (list (make-posn (posn-x (list-ref INV1 (random (length INV1))))
                                           (+ 10 (posn-y (list-ref INV1 (random (length INV1))))))
                                (make-posn 100 210) (make-posn 200 110)
                                (make-posn 300 210))))
(check-expect (change-world (make-world SHIP1 '() BUL-LIST0 BUL-LIST4))
              (make-world (make-ship 'right (make-posn 460 YPOS))
                          '()
                          (list
                           (make-posn 50 290)
                           (make-posn 30 390)
                           (make-posn 50 190))
                          (list
                           (make-posn 470 460)
                           (make-posn 420 410)
                           (make-posn 370 360))))
(check-expect (change-world (make-world SHIP0 INV2 BUL-LIST1 BUL-LIST2))
              (make-world (make-ship 'left (make-posn 358 YPOS))
                          (list (make-posn 240 100)
                                (make-posn 280 100))
                          (list
                           (make-posn 100 190)
                           (make-posn 300 190))
                          (list
                           (make-posn 450 480)
                           (make-posn 400 430)
                           (make-posn 350 380)
                           (make-posn 300 330)
                           (make-posn 250 280)
                           (make-posn 200 230)
                           (make-posn 150 180)
                           (make-posn 100 130)
                           (make-posn 50 80)
                           (make-posn 25 30))))
(check-random (change-world (make-world SHIP0 INV2 (list
                                                    (make-posn 300 520)
                                                    (make-posn 245 60))
                                        (list
                                         (make-posn 200 50)
                                         (make-posn 1 300))))
              (make-world (make-ship 'left (make-posn 358 YPOS))
                          INV2
                          (list
                           (make-posn 245 50))
                          (list
                           (make-posn (posn-x (list-ref INV2 (random (length INV1))))
                                      (+ 10 (posn-y (list-ref INV2 (random (length INV1))))))
                           (make-posn 200 60))))
(define (change-world aworld)
  (remove-hits-out-of-bounds
   (make-world (move-spaceship (world-ship aworld))
               (world-invaders aworld)
               (move-spaceship-bullets (world-ship-bullets aworld))
               (move-invader-bullets
                (invaders-fire (world-invader-bullets aworld) (world-invaders aworld))))))

;; move-spaceship: Ship -> Ship
;; moves the ship in the appropriate direction
(check-expect (move-spaceship SHIP0) (make-ship 'left (make-posn 358 YPOS)))
(check-expect (move-spaceship SHIP1) (make-ship 'right (make-posn 460 YPOS)))
(check-expect (move-spaceship SHIP2) (make-ship 'right (make-posn SHIP-MAX-X YPOS)))
(check-expect (move-spaceship SHIP3) (make-ship 'left (make-posn SHIP-MIN-X YPOS)))
(check-expect (move-spaceship SHIP4) (make-ship 'left (make-posn 477.5 YPOS)))
(check-expect (move-spaceship SHIP5) (make-ship 'right (make-posn 22.5 YPOS)))
(define (move-spaceship aship)
  (if (symbol=? 'right (ship-dir aship))
      (moving-right (ship-loc aship) (ship-dir aship))
      (moving-left (ship-loc aship) (ship-dir aship))))

;; moving-right: Location Direction -> Ship
;; creates a Ship from a given location and Direction when moving right
(check-expect (moving-right (make-posn 360 YPOS) 'right) (make-ship 'right (make-posn 370 YPOS)))
(check-expect (moving-right (make-posn SHIP-MAX-X YPOS) 'right)
              (make-ship 'right (make-posn SHIP-MAX-X YPOS)))
(check-expect (moving-right (make-posn SHIP-MIN-X YPOS) 'right)
              (make-ship 'right (make-posn 22.5 YPOS)))
(define (moving-right aloc adir)
  (if (< (posn-x aloc) SHIP-MAX-X)
      (make-ship adir (make-posn (+ (posn-x aloc) SHIP-SPEED) YPOS))
      (make-ship adir (make-posn (posn-x aloc) YPOS))))

;; moving-left: Location Direction -> Ship
;; creates a Ship from a given location and Direction when moving left
(check-expect (moving-left (make-posn 360 YPOS) 'left) (make-ship 'left (make-posn 350 YPOS)))
(check-expect (moving-left (make-posn SHIP-MAX-X YPOS) 'left)
              (make-ship 'left (make-posn 477.5 YPOS)))
(check-expect (moving-left (make-posn SHIP-MIN-X YPOS) 'left)
              (make-ship 'left (make-posn SHIP-MIN-X YPOS)))
(define (moving-left aloc adir)
  (if (> (posn-x aloc) SHIP-MIN-X)
      (make-ship adir (make-posn (- (posn-x aloc) SHIP-SPEED) YPOS))
      (make-ship adir (make-posn (posn-x aloc) YPOS))))
 
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

; invaders-fire : LoB LoI -> LoB
; fire from a random invader to hit the ship
(check-expect (invaders-fire (list ) (list )) '())
(check-expect (invaders-fire (list (make-posn 30 40) (make-posn 40 200)) (list ))
              (list (make-posn 30 40) (make-posn 40 200)))
(check-expect (invaders-fire BUL-LIST2 INV2) BUL-LIST2)
(check-expect (invaders-fire BUL-LIST3 INV1) BUL-LIST3)
(check-random (invaders-fire (list ) INV2) (list (list-ref INV2 (random (length INV2)))))
(check-random (invaders-fire BUL-LIST0 INV1) (list (list-ref INV1 (random (length INV1)))
                                                   (make-posn 50 300)
                                                   (make-posn 30 400)
                                                   (make-posn 50 200)))
(check-random (invaders-fire BUL-LIST1 INV2) (list (list-ref INV2 (random (length INV2)))
                                                   (make-posn 100 200)
                                                   (make-posn 200 100)
                                                   (make-posn 300 200)))
(define (invaders-fire aloib aloi)
  (cond [(empty? aloi) aloib]
        [(empty? aloib) (cons (list-ref aloi (random (length aloi))) aloib)]
        [(cons? aloib) (if (< 0 (length aloib) 10)
                           (cons (list-ref aloi (random (length aloi))) aloib) aloib)]))

; remove-hits-and-out-of-bounds: World -> World 
; remove any invaders that have been hit by a spaceship bullet and
; Remove any bullets that are out of bounds
(check-expect (remove-hits-out-of-bounds WORLD1) (make-world SHIP0 INV2 empty empty))
(check-expect (remove-hits-out-of-bounds (make-world SHIP1 '()
                                                     (list (make-posn 35 55) (make-posn 200 100))
                                                     (list (make-posn 30 40) (make-posn 234 120))))
              (make-world SHIP1 '() (list (make-posn 35 55) (make-posn 200 100))
                          (list (make-posn 30 40) (make-posn 234 120))))
(check-expect (remove-hits-out-of-bounds
               (make-world SHIP0 INV1 '() (list (make-posn 100 500) (make-posn 400 503))))
              (make-world SHIP0 INV1 '() '()))
(check-expect (remove-hits-out-of-bounds
               (make-world SHIP0  INV1 (list (make-posn 99 52) (make-posn 300 50))
                           (list (make-posn 30 500) (make-posn 30 46))))
              (make-world SHIP0 (list (make-posn 140 50) (make-posn 180 50))
                          (list (make-posn 300 50)) (list (make-posn 30 46))))
(check-expect (remove-hits-out-of-bounds
               (make-world SHIP0  INV2 (list (make-posn 30 50) (make-posn 70 55) (make-posn 500 12))
                           (list (make-posn 20 40) (make-posn 200 35))))
              (make-world SHIP0  (list (make-posn 200 100) (make-posn 240 100) (make-posn 280 100))
                          (list (make-posn 30 50) (make-posn 70 55))
                          (list (make-posn 20 40) (make-posn 200 35))))
(define (remove-hits-out-of-bounds aworld)
  (make-world (world-ship aworld)
              (invader-hit (world-invaders aworld) (world-ship-bullets aworld))
              (remove-bullets-out
               (remove-lob-when-hits (world-ship-bullets aworld) (world-invaders aworld)))
              (remove-bullets-out (world-invader-bullets aworld))))

;; invader-hit: LoI LoB -> LoI
;; removes invaders that have been hit by a ship bullet
(check-expect (invader-hit '() (list (make-posn 35 55) (make-posn 200 100)))
              '())
(check-expect (invader-hit INV1  '())
              (list (make-posn 100 50) (make-posn 140 50) (make-posn 180 50)))
(check-expect (invader-hit INV2  '())
              (list (make-posn 200 100) (make-posn 240 100) (make-posn 280 100)))
(check-expect (invader-hit INV1 (list (make-posn 99 52) (make-posn 300 50)))
              (list (make-posn 140 50) (make-posn 180 50)))
(check-expect (invader-hit INV1 (list (make-posn 100 300) (make-posn 10 10)))
              (list (make-posn 100 50) (make-posn 140 50) (make-posn 180 50)))
(check-expect (invader-hit INV2 (list (make-posn 30 50) (make-posn 276 101)))
              (list (make-posn 200 100) (make-posn 240 100)))
(check-expect (invader-hit INV2 (list (make-posn 30 50) (make-posn 70 55)))
              (list (make-posn 200 100) (make-posn 240 100) (make-posn 280 100)))
(define (invader-hit aloi alosb)
  (cond [(empty? aloi) '()]
        [(empty? alosb) aloi]
        [(cons? aloi)
         (invader-hit (bull-match-aloi (first alosb) aloi) (rest alosb))]))

;; bull-match-aloi : Bullet LoI -> LoI
;; gives a list of invaders that have not been hit by a bullet
(check-expect (bull-match-aloi (make-posn 30 40) (list )) '())
(check-expect (bull-match-aloi (make-posn 30 40) (list (make-posn 30 40) (make-posn 50 60)))
              (list (make-posn 50 60)))
(check-expect (bull-match-aloi (make-posn 50 60) (list (make-posn 30 40) (make-posn 50 60)))
              (list (make-posn 30 40)))
(define (bull-match-aloi abull aloi)
  (cond [(empty? aloi) '()]
        [(cons? aloi) (if (invader-outline (first aloi) abull)
                          (bull-match-aloi abull (rest aloi))
                          (cons (first aloi) (bull-match-aloi abull (rest aloi))))]))

;; remove-lob-when-hits : LoB LoI -> LoB
;; removes any ship bullet that hit an invader
(check-expect (remove-lob-when-hits
               (list )
               (list (make-posn 30 40) (make-posn 200 100)))
              '())
(check-expect (remove-lob-when-hits
               (list (make-posn 30 40) (make-posn 200 100))
               (list ))
              (list (make-posn 30 40) (make-posn 200 100)))
(check-expect (remove-lob-when-hits
               (list (make-posn 30 40) (make-posn 50 60))
               (list (make-posn 30 40) (make-posn 200 100)))
              (list (make-posn 50 60)))
(check-expect (remove-lob-when-hits
               (list (make-posn 50 60) (make-posn 300 430))
               (list (make-posn 30 40) (make-posn 50 60)))
              (list (make-posn 300 430)))
(check-expect (remove-lob-when-hits
               (list (make-posn 30 40) (make-posn 50 60))
               (list (make-posn 300 40) (make-posn 50 60)))
              (list (make-posn 30 40)))
(check-expect (remove-lob-when-hits
               (list (make-posn 20 80) (make-posn 50 60))
               (list (make-posn 50 60) (make-posn 200 100)))
              (list (make-posn 20 80)))
(define (remove-lob-when-hits alosb aloi)
  (cond [(empty? alosb) '()]
        [(empty? aloi) alosb]
        [(cons? alosb)
         (invader-hit (inv-match-alosb (first aloi) alosb) (rest aloi))]))

;; inv-match-alosb : Inv LoB -> LoB
;; gives a list of bullets that have not hit an invader
(check-expect (inv-match-alosb (make-posn 30 40) (list )) '())
(check-expect (inv-match-alosb (make-posn 30 40) (list (make-posn 30 40) (make-posn 50 60)))
              (list (make-posn 50 60)))
(check-expect (inv-match-alosb (make-posn 50 60) (list (make-posn 30 40) (make-posn 50 60)))
              (list (make-posn 30 40)))
(define (inv-match-alosb inv alosb)
  (cond [(empty? alosb) '()]
        [(cons? alosb) (if (invader-outline inv (first alosb))
                           (inv-match-alosb inv (rest alosb))
                           (cons (first alosb) (inv-match-alosb inv (rest alosb))))]))

;; invader-outline : Invader Bullet -> Boolean
;; determines if a bullet is within the bounds of an invader
(check-expect (invader-outline (make-posn 30 50) (make-posn 100 100)) #false)
(check-expect (invader-outline (make-posn 30 50) (make-posn 40 60)) #true)
(check-expect (invader-outline (make-posn 30 50) (make-posn 35 55)) #true)
(check-expect (invader-outline (make-posn 30 50) (make-posn 35 70)) #false)
(check-expect (invader-outline (make-posn 30 50) (make-posn 70 55)) #false)
(define (invader-outline inv abull)
  (and (<= (- (x-pos inv) (/ INVADER-SIDE 2))
           (x-pos abull) (+ (x-pos inv) (/ INVADER-SIDE 2)))
       (<= (- (y-pos inv) (/ INVADER-SIDE 2))
           (y-pos abull) (+ (y-pos inv) (/ INVADER-SIDE 2)))))

;; remove-bullets-out : LoB -> LoB
;; removes any bullets that are out of bounds
(check-expect (remove-bullets-out (list )) '())
(check-expect (remove-bullets-out
               (list (make-posn 30 40) (make-posn 400 500))) (list (make-posn 30 40)))
(check-expect (remove-bullets-out
               (list (make-posn 300 500) (make-posn 400 50))) (list (make-posn 400 50)))
(check-expect (remove-bullets-out
               (list (make-posn 30 498) (make-posn 2 50))) (list (make-posn 30 498) (make-posn 2 50)))
(check-expect (remove-bullets-out
               (list (make-posn 500 500) (make-posn 2 50))) (list (make-posn 2 50)))
(define (remove-bullets-out alob)
  (cond [(empty? alob) '()]
        [(cons? alob) (if (bullet-out? (first alob))
                          (remove-bullets-out (rest alob))
                          (cons (first alob) (remove-bullets-out (rest alob))))]))

;; bullet-out? Bullet -> Boolean
;; is the given boolean out of bounds?
(check-expect (bullet-out? (make-posn 30 50)) #false)
(check-expect (bullet-out? (make-posn 2 50)) #false)
(check-expect (bullet-out? (make-posn 30 498)) #false)
(check-expect (bullet-out? (make-posn 501 520)) #true)
(check-expect (bullet-out? (make-posn 2 520)) #true)
(check-expect (bullet-out? (make-posn 501 498)) #true)
(define (bullet-out? abull)
  (or (x-pos-out? (x-pos abull))
      (y-pos-out? (y-pos abull))))

;; x-pos-out? : Number -> Boolean
;; is the x-position of the bullet out of bounds?
(check-expect (x-pos-out? 2) #false)
(check-expect (x-pos-out? 498) #false)
(check-expect (x-pos-out? 200) #false)
(check-expect (x-pos-out? 600) #true)
(check-expect (x-pos-out? 1) #true)
(define (x-pos-out? xpos)
  (or (> (+ 0 BULLET-RAD) xpos)
      (< (- WIDTH BULLET-RAD) xpos)))

;; y-pos-out? : Number -> Boolean
;; is the y position of the bullet out of bounds?
(check-expect (y-pos-out? 2) #false)
(check-expect (y-pos-out? 498) #false)
(check-expect (y-pos-out? 200) #false)
(check-expect (y-pos-out? 600) #true)
(check-expect (y-pos-out? 1) #true)
(define (y-pos-out? ypos)
  (or (> (+ 0 BULLET-RAD) ypos)
      (< (- HEIGHT BULLET-RAD) ypos)))

;; stop-world : World -> Boolean
;; checks if the ship was hit by an invader bullet or if there are zero invaders left
(check-expect (stop-world WORLD0) #false)
(check-expect (stop-world WORLD1) #false)
(check-expect (stop-world WORLD2) #true)
(check-expect (stop-world WORLD3) #true)
(check-expect (stop-world (make-world SHIP1 '() BUL-LIST0 BUL-LIST1)) #true)
(check-expect (stop-world (make-world SHIP1 INV1 BUL-LIST0 BUL-LIST1)) #false)
(check-expect (stop-world (make-world SHIP1 INV2 BUL-LIST1 BUL-LIST2)) #false)
(define (stop-world aworld)
  (or (ship-hit? (world-ship aworld) (world-invader-bullets aworld))
      (zero-invaders? (world-invaders aworld))))

; ship-hit : Ship LoB -> Boolean
; true if a bullet hit the ship, false otherwise
(check-expect (ship-hit? SHIP0 '()) #false)
(check-expect (ship-hit? SHIP0 BUL-LIST5) #true)
(check-expect (ship-hit? SHIP0 BUL-LIST6) #true)
(check-expect (ship-hit? SHIP0 BUL-LIST7) #true)
(check-expect (ship-hit? SHIP0 BUL-LIST8) #true)
(check-expect (ship-hit? SHIP0 BUL-LIST9) #false)
(check-expect (ship-hit? SHIP0 BUL-LIST10) #false)
(check-expect (ship-hit? SHIP0 BUL-LIST11) #true)
(check-expect (ship-hit? SHIP0 BUL-LIST12) #false)
(define (ship-hit? aship aloib)
  (cond [(empty? aloib) #false]
        [(cons? aloib)
         (or (ship-outline aship (first aloib))
             (ship-hit? aship (rest aloib)))]))

;; ship-outline: Ship Bullet -> Boolean
;; determines if a bullet is within the bounds of a ship
(check-expect (ship-outline SHIP0 (make-posn 30 50)) #false)
(check-expect (ship-outline SHIP0 (make-posn 300 487)) #false)
(check-expect (ship-outline SHIP0 (make-posn 360 400)) #false)
(check-expect (ship-outline SHIP0 (make-posn 360 480)) #true)
(check-expect (ship-outline SHIP0 (make-posn 355.5 480)) #true)
(check-expect (ship-outline SHIP0 (make-posn 360 487.5)) #true)
(define (ship-outline aship abull)
  (and (x-pos-in? (x-pos (ship-loc aship)) (x-pos abull))
       (y-pos-in? (y-pos abull))))

;; x-pos-in? : Number -> Boolean
;; is the x position of the bullet within the bounds of the ship?
(check-expect (x-pos-in? 100 200) #false)
(check-expect (x-pos-in? 200 212.5) #true)
(check-expect (x-pos-in? 200 187.5) #true)
(check-expect (x-pos-in? 100 100) #true)
(define (x-pos-in? xpos-ship xpos-bullet)
  (<= (- xpos-ship (/ SHIP-WIDTH 2))
      xpos-bullet
      (+ xpos-ship (/ SHIP-WIDTH 2))))

;; y-pos-in? : Number -> Boolean
;; is the y position of the bullet within the bounds of the ship?
(check-expect (y-pos-in? 2) #false)
(check-expect (y-pos-in? 487.5) #true)
(check-expect (y-pos-in? 472.5) #true)
(check-expect (y-pos-in? 480) #true)
(define (y-pos-in? ypos-bullet)
  (<= (- YPOS (/ SHIP-HEIGHT 2))
      ypos-bullet
      (+ YPOS (/ SHIP-HEIGHT 2))))

;; x-pos: Location -> Number
;; determines the x position of a given Location
(check-expect (x-pos (make-posn 400 350)) 400)
(check-expect (x-pos (make-posn 256 400)) 256)
(define (x-pos aloc)
  (posn-x aloc))

;; y-pos: Location -> Number
;; determines the y position of a given Location
(check-expect (y-pos (make-posn 400 350)) 350)
(check-expect (y-pos (make-posn 256 400)) 400)
(define (y-pos aloc)
  (posn-y aloc))

;; zero-invaders? : LoI -> Boolean
;; are there zero invaders in the list?
(check-expect (zero-invaders? (list )) #true)
(check-expect (zero-invaders? INV1) #false)
(check-expect (zero-invaders? INV2) #false)
(define (zero-invaders? aloi)
  (cond [(empty? aloi) #true]
        [(cons? aloi) #false]))

;; move-world: World KeyEvent -> World
;; moves the world based on a given KeyEvent
(check-expect (move-world WORLD0 "left")
              (make-world (make-ship 'left (make-posn 450 YPOS))
                          INV1 BUL-LIST0 BUL-LIST1))
(check-expect (move-world WORLD0 "right")
              (make-world SHIP1
                          INV1 BUL-LIST0 BUL-LIST1))
(check-expect (move-world WORLD0 "l")
              (make-world SHIP1
                          INV1 BUL-LIST0 BUL-LIST1))
(check-expect (move-world WORLD1 "left")
              (make-world SHIP0
                          INV2 empty empty))
(check-expect (move-world WORLD1 "right")
              (make-world (make-ship 'right (make-posn 368 YPOS))
                          INV2 empty empty))
(check-expect (move-world WORLD1 "r")
              (make-world SHIP0
                          INV2 empty empty))
(define (move-world aworld ake)
  (make-world (change-ship-dir (world-ship aworld) ake)
              (world-invaders aworld)
              (fire-spaceship-bullets (world-ship-bullets aworld) ake (world-ship aworld))
              (world-invader-bullets aworld)))

;; change-ship-dir : Ship KeyEvent -> Ship
;; creates a new ship moving in the given direction
(check-expect (change-ship-dir SHIP0 "c") (make-ship 'left (make-posn 368 YPOS)))
(check-expect (change-ship-dir SHIP1 "x") (make-ship 'right (make-posn 450 YPOS)))
(check-expect (change-ship-dir SHIP2 "right") (make-ship 'right (make-posn SHIP-MAX-X YPOS)))
(check-expect (change-ship-dir SHIP3 "left") (make-ship 'left (make-posn SHIP-MIN-X YPOS)))
(check-expect (change-ship-dir SHIP4 "right") (make-ship 'right (make-posn SHIP-MAX-X YPOS)))
(check-expect (change-ship-dir SHIP5 "left") (make-ship 'left (make-posn SHIP-MIN-X YPOS)))
(define (change-ship-dir aship ake)
  (make-ship (direction (ship-dir aship) ake) (ship-loc aship)))

;; direction : Direction KeyEvent -> Direction
;; changes the passed in direction of the ship
(check-expect (direction 'left " ") 'left)
(check-expect (direction 'right "z") 'right)
(check-expect (direction 'right "right") 'right)
(check-expect (direction 'left "left") 'left)
(check-expect (direction 'left "right") 'right)
(check-expect (direction 'right "left") 'left)
(define (direction direc ake)
  (cond [(string=? "left" ake) 'left]
        [(string=? "right" ake) 'right]
        [else direc]))

;; fire-spaceship-bullets : LoB KeyEvent Ship -> LoB
;; fires a spaceship bullet when a certain KeyEvent is passed in
(check-expect (fire-spaceship-bullets '() "z" SHIP0)
              '())
(check-expect (fire-spaceship-bullets '() " " SHIP0)
              (list (make-posn 368 YPOS)))
(check-expect (fire-spaceship-bullets (list (make-posn 30 3)) "x" SHIP1)
              (list (make-posn 30 3)))
(check-expect (fire-spaceship-bullets (list (make-posn 30 3)) " " SHIP1)
              (list (make-posn 450 YPOS) (make-posn 30 3)))
(check-expect (fire-spaceship-bullets (list (make-posn 30 3) (make-posn 20 5)) " " SHIP1)
              (list (make-posn 450 YPOS) (make-posn 30 3) (make-posn 20 5)))
(check-expect (fire-spaceship-bullets BUL-LIST0 " " SHIP1)
              BUL-LIST0)
(check-expect (fire-spaceship-bullets BUL-LIST0 "s" SHIP1)
              BUL-LIST0)
(define (fire-spaceship-bullets alosb ake aship)
  (cond [(empty? alosb) (if (string=? " " ake)
                            (list (ship-loc aship))
                            '())]
        [(cons? alosb) (if (and (< 0 (length alosb) 3) (string=? " " ake))
                           (cons (ship-loc aship) alosb)
                           alosb)]))

;;; Main Function
(define (run-game aworld)
  (big-bang aworld
    [to-draw world-draw]
    [on-tick change-world]
    [on-key move-world]
    [stop-when stop-world]))

(run-game WORLD-INIT)
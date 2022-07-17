;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |05 Assignment|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;Exercise 5

; A Size is one of:
; - "small"
; - "medium"
; - "large"
 
(define-struct drip-coffee [cream size])
(define DRIP0 (make-drip-coffee #false "small"))
(define DRIP1 (make-drip-coffee #true "large"))

(define-struct latte [size])
(define LATTE0 (make-latte "medium"))
(define LATTE1 (make-latte "large"))
(define LATTE2 (make-latte "small"))

(define-struct cortado [size])
(define CORT0 (make-cortado "small"))
(define CORT1 (make-cortado "medium"))


; A Coffee is one of:
; - (make-drip-coffee Boolean Size)
; - (make-latte Size)
; - (make-cortado Size)
; INTERPRETATION: Represents three possible coffee orders.  Each order 
; has a size; drip coffee might also have cream in it.
 
; A CoffeeOrder (List-of-Coffee) is one of:
; - '()
; - (cons Coffee CoffeeOrder)
; INTERPRETATION: The list of coffee orders at a local coffee shop
(define LOC0 (cons DRIP0 (cons LATTE0 (cons CORT0 '()))))
(define LOC1 (cons DRIP1 (cons LATTE1 (cons CORT1 '()))))
(define LOC2 '())
(define LOC3 (cons LATTE0 (cons LATTE2 (cons CORT1 '()))))
(define LOC4 (cons CORT1 (cons DRIP1 '())))

(define LOC0R (cons CORT0 (cons LATTE0 (cons DRIP0 '()))))
(define LOC1R (cons CORT1 (cons LATTE1 (cons DRIP1 '()))))
(define LOC3R (cons CORT1 (cons LATTE2 (cons LATTE0 '()))))
(define LOC4R (cons DRIP1 (cons CORT1 '())))


; A MaybeCoffee is one of
; - #false
; - Coffee
; INTERPRETATION: Represents maybe having a Coffee
(define maybecoffee0 #false)
(define maybeecoffee1 CORT0)

;; coffee-temp : Coffee -> ?
#;(define (coffee-temp ac)
    (cond [(drip-coffee? ac) ... (drip-coffee-temp ac)]
          [(latte? ac) ... (latte-temp ac)]
          [(cortado? ac) ... (cortado-temp ac)]))

;; drip-coffee-temp : drip-coffee -> ?
#;(define (drip-coffee-temp adc)
    (...(drip-coffee-cream adc)) ... (size-temp (drip-coffe-size adc)))

;; latte-temp : latte -> ?
#;(define (latte-temp al)
    (...(size-temp (latte-size al))))

;; cortado-temp : cortado -> ?
#;(define (cortado-temp acort)
    (... (size-temp (cortado-size acort))))

;; size-temp : Size -> ?
#;(define (size-temp s)
    (cond [(string=? s "small") ...]
          [(string=? s "medium") ...]
          [(string=? s "large") ...]))
  
;; coffeeorder-temp : CoffeeOrder -> ?
#;(define (coffeeorder-temp aco)
    (cond [(empty? aco) ...]
          [(cons? aco) ... (coffee-temp (first aco))
                       ... (coffeeorder-temp (rest aco))]))

;; maybecoffee : MaybeCoffee -> ?
#;(define (maybecoffe-temp mc)
    (cond [(false? mc) ...]
          [(else? mc) ... (coffee-temp ac)]))

;; last-latte : CoffeeOrder -> Latte
;; returns the last latte in the given CoffeeOrder
(check-expect (last-latte LOC2) #false)
(check-expect (last-latte LOC0) (make-latte "medium"))
(check-expect (last-latte LOC1) (make-latte "large"))
(check-expect (last-latte LOC3) (make-latte "small"))
(check-expect (last-latte LOC4) #false)
(define (last-latte aco)
  (cond [(empty? aco) #false]
        [(cons? aco) (check-order (reverse aco))]))

;; check-order : CoffeOrder -> Latte 
;; finds the most recent latte in the reversed CoffeeOrder
(define (check-order aco)
  (cond [(empty? aco) #false]
        [else (is-latte aco)]))

;; is-latte : CoffeeOrder -> Latte
;; checks if the most recent coffee in the reversed CoffeeOrder is a Latte
(check-expect (is-latte LOC0R) (make-latte "medium"))
(check-expect (is-latte LOC1R) (make-latte "large"))
(check-expect (is-latte LOC3R) (make-latte "small"))
(check-expect (is-latte LOC4R) #false)
(define (is-latte aco)
  (cond [(drip-coffee? (first aco)) (check-order (rest aco))]
        [(latte? (first aco)) (latte-maker (first aco))]
        [(cortado? (first aco)) (check-order (rest aco))]))

;; latte-maker : Coffee -> Latte
;; returns the latte that is the first Coffee in the reversed CoffeeOrder
(check-expect (latte-maker LATTE0) (make-latte "medium"))
(check-expect (latte-maker LATTE1) (make-latte "large"))
(check-expect (latte-maker LATTE2) (make-latte "small"))
(define (latte-maker ac)
  (make-latte (latte-size ac)))

; Exercise 6

(define-struct dinner-check [food drink tax tip])
; A DinnerCheck is a (make-dinner-check Number Number Number Number)
; INTERPRETATION: Represents the bill for a table in a restaurant,
; with numbers representing the cost of the food, drink, tax, and tip.
 
; An OrderBook (List-of-DinnerCheck) is one of:
; - '()
; - (cons DinnerCheck OrderBook)

;dinner-check-temp : DinnerCheck -> ?
#;(define (dinner-check-temp adc)
    (... (dinner-check-food adc)
         ... (dinner-check-drink adc)
         ... (dinner-check-tax adc)
         ... (dinner-check-tip adc)))
(define DC1NA (make-dinner-check 75 25 10 35))
(define DC2A (make-dinner-check 75 25 15 35))
(define DC3A (make-dinner-check 75 25 15 65))
(define DC4A (make-dinner-check 75 25 10 65))

;; orderbook-temp : OrderBook -> ?
#;(define (orderbook-temp aob)
    (cond [(empty? ob) ...]
          [(cons? ob) (dinner-check-temp (first ob))...
                      (orderbook-temp (rest ob))...]))
(define OB0 '())
(define OB1 (cons DC1NA OB0))
(define OB2 (cons DC2A OB1))
(define OB3 (cons DC3A OB2))
(define OB4 (cons DC4A OB3))

;; flag-anomalous : OrderBook -> OrderBook
;; produces an OrderBook of only anomalous DinnerChecks from a given OrderBook
(check-expect (flag-anomalous OB0) '())
(check-expect (flag-anomalous OB1) '())
(check-expect (flag-anomalous OB2)
              (cons (make-dinner-check 75 25 15 35) '()))
(check-expect (flag-anomalous OB3)
              (cons (make-dinner-check 75 25 15 65) (cons (make-dinner-check 75 25 15 35) '())))
(define (flag-anomalous ob)
  (cond [(empty? ob) '()]
        [(cons? ob)
         (if (anomalous? (first ob))
             (cons (first ob) (flag-anomalous (rest ob)))
             (flag-anomalous (rest ob)))]))

;; anamolous? : DinnerCheck -> Boolean
;; is the given DinnerCheck anomalous? 
(check-expect (anomalous? DC1NA) #false)
(check-expect (anomalous? DC2A) #true)
(check-expect (anomalous? DC3A) #true)
(check-expect (anomalous? DC4A) #true)
(define (anomalous? ob)
  (or (tax-anomalous ob) (tip-anomalous ob)))

;; tax-anamolous : DinnerCheck -> Boolean
; is the tax portion of the DinnerCheck anomalous?
(check-expect (tax-anomalous DC1NA) #false)
(check-expect (tax-anomalous DC2A) #true)
(check-expect (tax-anomalous DC3A) #true)
(check-expect (tax-anomalous DC4A) #false)
(define (tax-anomalous ob)
  (not (= (dinner-check-tax ob) (tax-math ob))))

;; tax-math : DinnerCheck -> Number
;; calculates 10% of the food and drink portion of the DinnerCheck
(check-expect (tax-math DC1NA) 10)
(check-expect (tax-math DC2A) 10)
(check-expect (tax-math DC3A) 10)
(check-expect (tax-math DC4A) 10)
(define (tax-math ob)
  (/
   (+ (dinner-check-food ob) (dinner-check-drink ob))
   10))

;; tip-anamolous : DinnerCheck -> Boolean
;; is the tip portion of the DinnerCheck anomalous?
(check-expect (tip-anomalous DC1NA) #false)
(check-expect (tip-anomalous DC2A) #false)
(check-expect (tip-anomalous DC3A) #true)
(check-expect (tip-anomalous DC4A) #true)
(define (tip-anomalous ob)
  (< (tip-math ob) (dinner-check-tip ob)))

;; tip-math : DinnerCheck -> Number
;; calculates 50% of the food, drink, and tax portion of the DinnerCheck
(check-expect (tip-math DC1NA) 55)
(check-expect (tip-math DC2A) 57.5)
(check-expect (tip-math DC3A) 57.5)
(check-expect (tip-math DC4A) 55)
(define (tip-math ob)
  (/
   (+ (dinner-check-food ob) (dinner-check-drink ob) (dinner-check-tax ob)) 2))

; Exercise 7

(define-struct seat [number is-empty passenger-name])
; A Seat is a (make-seat String Boolean String)
; INTERPRETATION: A seat assignment on a plane, consisting of a
; seat number ("34B"), whether or not it is empty, and the name of the
; passenger if it is not empty.  If a seat assignment is empty, the
; passenger name should be the empty string ("").
 
; A Flight (List-of-Seat) is one of:
; - '()
; - (cons Seat Flight)
; INTERPRETATION: Represents the seats present in a given flight.
; (The seats are not guaranteed to be ordered by seat number; it's just a
; set of seats.)

;; seat-temp : Seat -> ?
#;(define (seat-temp as)
    (... (seat-number as) ... (seat-is-empty as) ... (seat-passanger-name as)))
(define SEAT0 (make-seat "34B" #true "Ali Schueler"))
(define SEAT1 (make-seat "20A" #false ""))
(define SEAT2 (make-seat "10C" #true "Jacob Livingston"))
  
;; flight-temp : Flight -> ?
#;(define (flight-temp af)
    (cond [(empty? af) ...]
          [(cons? af) (... (seat-temp (first af))
                           ... (flight-temp (rest af)))]))
(define FLIGHT0 '())
(define FLIGHT1 (cons SEAT0 FLIGHT0))
(define FLIGHT2 (cons SEAT1 FLIGHT1))
(define FLIGHT3 (cons SEAT2 FLIGHT2))

;; seat-member : Flight String -> Boolean
;; is the given seat on the given flight?
(check-expect (seat-member? FLIGHT0 "20A") #false)
(check-expect (seat-member? FLIGHT1 "21B") #false)
(check-expect (seat-member? FLIGHT1 "34B") #true)
(check-expect (seat-member? FLIGHT3 "20A") #true)
(define (seat-member? aflight seatnum)
  (cond [(empty? aflight) #false]
        [(cons? aflight) (if (seat-there? (first aflight) seatnum)
                             #true
                             (seat-member? (rest aflight) seatnum))]))

;; seat-there : Seat String -> Boolean
;; is the seat the same as the given seat?
(check-expect (seat-there? SEAT0 "21B") #false)
(check-expect (seat-there? SEAT0 "34B") #true)
(check-expect (seat-there? SEAT2 "10C") #true)
(define (seat-there? aseat seatnum)
  (string=? (seat-number aseat) seatnum))
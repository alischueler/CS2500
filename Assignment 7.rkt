;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |07 Assignment|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise 6

;; alphabetic? : [List-of Strings] -> Boolean
;; determines if every string in a list is alphabetic
(check-expect (alphabetic? (list "abc" "z")) #true)
(check-expect (alphabetic? (list "123" "z")) #false)
(check-expect (alphabetic? (list "123" "??")) #false)
(check-expect (alphabetic? (list )) #true)
(define (alphabetic? alos)
  (andmap string-alphabetic? alos))

; Exercise 7

;; factorial : NatNumber -> NatNumber
;; calculates the factorial of a number
(check-expect (factorial 0) 1) 
(check-expect (factorial 1) 1)
(check-expect (factorial 3) 6)
(check-expect (factorial 8) 40320)
(define (factorial num)
  (foldr * 1 (build-list num add1)))

; Exercise 8

;; at-0 : [List-of [Number -> Number]] -> [List-of Numbers]
;; produces a list of functions at 0
(check-expect (at-0 (list )) '())
(check-expect (at-0 (list add1 sub1 sqr)) (list 1 -1 0))
(check-expect (at-0 (list sub1 sqr add1)) (list -1 0 1))
(define (at-0 lof)
  (map apply-with-0 lof))

;; apply-with-0 : [Number -> Number] -> Number
;; applies 0 to the given function
(check-expect (apply-with-0 sub1) -1)
(check-expect (apply-with-0 add1) 1)
(check-expect (apply-with-0 sqr) 0)
(define (apply-with-0 f)
  (f 0))

; Exercise 9

(define-struct tweet [author text])
(define-struct image-tweet [author text image])
(define-struct retweet [author tweet])
; A Tweet is one of:
; - (make-tweet String String)
; - (make-image-tweet String String Image)
; - (make-retweet String Tweet)
; INTERPRETATION:  A tweet is either a message (make-tweet),
; a message and an image (make-image-tweet), or a retweet
; of another tweet (make-retweet).  All tweets have authors.
 
; A Feed is a [List-of Tweet]
; INTERPREATION:  A list of the Tweets in a user's feed.

;; tweet-temp : Tweet -> ?
#;(define (tweet-temp t)
    (cond [(tweet? t) ... (tweet-author t)
                      ... (tweet-text t)...]
          [(image-tweet? t) ... (image-tweet-author i)
                            ... (image-tweet-text i)
                            ... (image-tweet-image i)...]
          [(retweet? t) ... (retweet-author rt)
                        ... (tweet-temp (retweet-tweet rt)) ...]))

;; feed-temp : Feed -> ?
#;(define (feed-temp f)
    (cond [(empty? f) ...]
          [(cons? f) ... (tweet-temp (first f))
                     ... (feed-temp (rest f))]))

(define FEED0 '())
(define FEED1 (make-tweet "ryan" "abc"))
(define FEED2 (make-image-tweet "bill" "ha" (circle 2 'solid 'red)))
(define FEED3 (make-retweet "george" FEED1))
(define FEED4 (make-retweet "luke" FEED3))
(define FEED5 (list FEED1))
(define FEED6 (list FEED3))
(define FEED7 (list FEED3 FEED1))
(define FEED8 (list FEED4 FEED3))

;; counts-tweets.v1 : Feed -> Number
;; determines how many tweets are in the given feed
(check-expect (counts-tweets.v1 FEED0) 0)
(check-expect (counts-tweets.v1 FEED5) 1)
(check-expect (counts-tweets.v1 FEED6) 2)
(check-expect (counts-tweets.v1 FEED7) 3)
(check-expect (counts-tweets.v1 FEED8) 5)
(define (counts-tweets.v1 f)
  (cond [(empty? f) 0]
        [(cons? f) (+ (how-many (first f)) (counts-tweets.v1 (rest f)))]))

;; how-many : Tweet -> Number
;; how many tweets are embedded in the given tweet
(check-expect (how-many FEED1) 1)
(check-expect (how-many FEED2) 1)
(check-expect (how-many FEED3) 2)
(check-expect (how-many FEED4) 3)
(define (how-many t)
  (cond [(tweet? t) 1]
        [(image-tweet? t) 1]
        [(retweet? t) (add1 (how-many (retweet-tweet t)))]))

; Exercise 10

;; count-tweets.v2 : Feed -> Number
;; determines how many tweets are in the given feed
(check-expect (count-tweets.v2 FEED0) 0)
(check-expect (count-tweets.v2 FEED5) 1)
(check-expect (count-tweets.v2 FEED6) 2)
(check-expect (count-tweets.v2 FEED7) 3)
(check-expect (count-tweets.v2 FEED8) 5)
(define (count-tweets.v2 f)
  (foldr how-many.v2 0 f))

;; how-many.v2 : Tweet Number -> Number
;; how many tweets are embedded in the given tweet
(check-expect (how-many.v2 FEED1 0) 1)
(check-expect (how-many.v2 FEED2 0) 1)
(check-expect (how-many.v2 FEED3 1) 3)
(check-expect (how-many.v2 FEED4 2) 5)
(define (how-many.v2 t result)
  (cond [(tweet? t) (add1 result)]
        [(image-tweet? t) (add1 result)]
        [(retweet? t) (add1 (how-many.v2 (retweet-tweet t) result))]))

; Exercise 11

; An [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])

;; nelist-temp : [NEList-of X] -> ?
#;(define (nelist-temp nel)
    (cond [(empty? (rest nel)) ... (first nel)]
          [(cons? (rest nel)) ... (first nel)
                              (nelist-temp (rest nel))]))

;; absolute-value : [List-of Numbers] -> List-of NatNaumber
;; determines absolute value of every number in a given list-of numbers
(check-expect (absolute-value (list 0)) (list 0))
(check-expect (absolute-value (list 1)) (list 1))
(check-expect (absolute-value (list 1 -3 2)) (list 1 3 2))
(define (absolute-value alon)
  (map abs alon))

; Exercise 12

; An [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])

;; nelist-temp : [NEList-of X] -> ?
#;(define (nelist-temp nel)
    (cond [(empty? (rest nel)) ... (first nel)]
          [(cons? (rest nel)) ... (first nel)
                              (nelist-temp (rest nel))]))

;; largest-num : [List-of Numbers] -> Number
;; determines the largest number in the given list-of numbers
(check-expect (largest-num (list 1 2 3)) 3)
(check-expect (largest-num (list 0)) 0)
(check-expect (largest-num (list 1 12 3)) 12)
(check-expect (largest-num (list 1 34 35)) 35)
(check-expect (largest-num (list -1 3 -2)) 3)
(check-expect (largest-num (list -5 -17 -1)) -1)
(define (largest-num alon)
  (foldr max (first alon) (rest alon)))

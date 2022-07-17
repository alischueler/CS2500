;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A [BT X] is one of:
; - (make-node X [BT X] [BT X])
; - X
(define-struct node (data left right))

;; [BT X] -> ?
#;(define (bt-temp bt)
    (cond [(node? bt) ... (node-data bt)
                      ... (bt-temp (node-left bt))
                      ... (bt-temp (node-right bt))]
          [else ...]))

;; EXERCISE 3
;; largest-n: [BT Number] -> Number
;; determines the largest number in the tree
(check-expect (largest-n 8) 8)
(check-expect (largest-n (make-node 10 8 9)) 10)
(check-expect (largest-n (make-node 10 (make-node 4 3 12) (make-node 7 2 5))) 12)
(define (largest-n btn)
  (cond [(node? btn) (max (node-data btn)
                          (largest-n (node-left btn))
                          (largest-n (node-right btn)))]
        [else btn]))

;; lex: [BT String] -> String
;; determines the first lexicographic string in the tree
(check-expect (lex "hello") "hello")
(check-expect (lex (make-node "hello" "cat" "dog")) "cat")
(check-expect (lex (make-node "hello"
                              (make-node "cat" "dog" "horse")
                              (make-node "butterfly" "rat" "zebra"))) "butterfly")
(define (lex bts)
  (cond [(node? bts) (first-alpha (node-data bts)
                                  (lex (node-left bts))
                                  (lex (node-right bts)))]
        [else bts]))

;; first-alpha: String String String -> String
;; determines the string that comes first in alphabetical order out of the given strings
(check-expect (first-alpha "hello" "sand" "zebra") "hello")
(check-expect (first-alpha "sand" "hello" "zebra") "hello")
(define (first-alpha s bts1 bts2)
  (first (sort (list s bts1 bts2) string-ci<?)))

;; winning-x: [BT X] [X X X -> X] -> X
;; determines the winning x in the given tree
(check-expect (winning-x "hello" first-alpha) "hello")
(check-expect (winning-x
               (make-node "hello"
                          (make-node "cat" "dog" "horse")
                          (make-node "butterfly" "rat" "zebra")) first-alpha) "butterfly")
(check-expect (winning-x 8 max) 8)
(check-expect (winning-x (make-node 10 (make-node 4 3 12) (make-node 7 2 5)) max) 12)
(define (winning-x btx op)
  (cond [(node? btx) (op (node-data btx)
                         (winning-x (node-left btx) op)
                         (winning-x (node-right btx) op))]
        [else btx]))

;; largest-n2: [BT Number] -> Number
;; determines the largest number in the tree
(check-expect (largest-n2 8) 8)
(check-expect (largest-n2 (make-node 10 8 9)) 10)
(check-expect (largest-n2 (make-node 10 (make-node 4 3 12) (make-node 7 2 5))) 12)
(define (largest-n2 btn)
  (winning-x btn max))

;; lex2: [BT String] -> String
;; determines the first lexicographic string in the tree
(check-expect (lex2 "hello") "hello")
(check-expect (lex2 (make-node "hello" "cat" "dog")) "cat")
(check-expect (lex2 (make-node "hello"
                               (make-node "cat" "dog" "horse")
                               (make-node "butterfly" "rat" "zebra"))) "butterfly")
(define (lex2 bts)
  (winning-x bts first-alpha))

;; EXERCISE 4
;; palindrome: [NEList-of 1String] -> [NEList-of 1String]
;; constructs a palindrome of the given list
(check-expect (palindrome (list "a" "b" "c")) (list "a" "b" "c" "b" "a"))
(check-expect (palindrome (list "a" "b" "c" "r")) (list "a" "b" "c" "r" "c" "b" "a"))
(check-expect (palindrome (list "a")) (list "a"))
(check-expect (palindrome (list "a" "b")) (list "a" "b" "a"))
(define (palindrome nel0)
  (local [;; [NEList-of 1String] [NEList-of 1String] -> [NEList-of 1String]
          ;; creates a list of 1Strings that is the reverse of the given list w/out the last item
          ;; accumulator : keeps track of the 1strings added to the [NEList-of 1String]
          (define (rev-acc nel acc)
            (if (empty? (rest nel)) acc
                (rev-acc (rest nel) (cons (first nel) acc))))]
    (if (= (length nel0) 1) nel0 (append nel0 (rev-acc nel0 '())))))

;; EXERCISE 5
;; only-leaves: [BT X] -> [List-of X]
;; produces a list of the leaves in the given tree
(check-expect (only-leaves 5) (list 5))
(check-expect (only-leaves (make-node 10 8 9)) (list 8 9))
(check-expect (only-leaves (make-node 10 (make-node 4 3 12) (make-node 7 2 5)))
              (list 3 12 2 5))
(check-expect (only-leaves "hello") (list "hello"))
(check-expect (only-leaves (make-node "hello" "cat" "dog")) (list "cat" "dog"))
(check-expect (only-leaves (make-node "hello"
                                      (make-node "cat" "dog" "horse")
                                      (make-node "butterfly" "rat" "zebra")))
              (list "dog" "horse" "rat" "zebra"))
(define (only-leaves btx0)
  (local [;; [BT X] [List-of X] -> [List-of X]
          ;; creates a list of leaves of the given tree
          ;; accumulator : keeps track of the leaves on the right side of the tree
          (define (leaves-acc btx acc)
            (cond [(node? btx) (leaves-acc (node-left btx)
                                           (leaves-acc (node-right btx) acc))]
                  [else (cons btx acc)]))]
    (leaves-acc btx0 '())))

;; EXERCISE 6
;; fibonacci: NatNumber -> Number
;; returns the fibonacci number at the given position 
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 2) 1)
(check-expect (fibonacci 3) 2)
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 20) 6765)
(define (fibonacci n)
  (cond [(< n 2) n]
        [else (+ (fibonacci (sub1 n)) (fibonacci (- n 2)))]))

;; fibonacci-a: NatNumber -> Number
;; returns the fibonacci number at the given position
(check-expect (fibonacci-a 0) 0)
(check-expect (fibonacci-a 1) 1)
(check-expect (fibonacci-a 2) 1)
(check-expect (fibonacci-a 3) 2)
(check-expect (fibonacci-a 11) 89)
(check-expect (fibonacci-a 20) 6765)
(define (fibonacci-a n0)
  (local [;; NatNumber NatNumber NatNumber -> NatNumber
          ;; determines the fibonacci number at the given position from the last two fibonacci numbers
          ;; accumulator: keeps track of the last two fibonacci numbers 
          (define (fib-acc n acc1 acc2)
            (cond [(zero? n) acc2]
                  [else (fib-acc (sub1 n) acc2 (+ acc1 acc2))]))]  
    (cond [(< n0 2) n0]
          [else (fib-acc n0 1 0)])))
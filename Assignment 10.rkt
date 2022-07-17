;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct graph [nodes neighbors])
; A Graph is a (make-graph [List-of Symbol] [Symbol -> [List-of Symbol]])
; and represents the nodes and edges in a graph

;; graph-temp : Graph -> ?
#;(define (graph-temp g)
    (... (list-temp (graph-nodes g))
         ((graph-neighbors g) ...) ...))

;; examples 
(define N1 (list 'billy 'claire 'rachel))
(define N2 (list 'john 'rob 'dan 'rachel))
(define G1 (make-graph N1 (λ (x)
                            (cond [(symbol=? x 'billy) '(claire rachel)]
                                  [(symbol=? x 'claire) '(billy rachel)]
                                  [(symbol=? x 'rachel) '(billy)]))))
(define G2 (make-graph N2 (λ (x)
                            (cond [(symbol=? x 'john) '(dan rachel rob)]
                                  [(symbol=? x 'rob) '(dan rachel)]
                                  [(symbol=? x 'dan) '(john)]
                                  [(symbol=? x 'rachel) '(dan)]))))

; Exercise 1

;; neighbors? : Graph Symbol Symbol -> Boolean
;; are the given symbols neighbors?
(check-expect (neighbors? G1 'billy 'claire) #true)
(check-expect (neighbors? G1 'claire 'rachel) #false)
(check-expect (neighbors? G2 'dan 'john) #true)
(check-expect (neighbors? G2 'rob 'rachel) #false)
(define (neighbors? g s1 s2)
  (and (member? s1 ((graph-neighbors g) s2)) (member? s2 ((graph-neighbors g) s1))))

; Exercise 2

;; mutual-neighbors : Graph Symbol Symbol -> [List-of Symbol]
;; creates a list of mutual neighbors between the two symbols in the graph
(check-expect (mutual-neighbors G1 'billy 'claire) '(rachel))
(check-expect (mutual-neighbors G1 'rachel 'claire) '(billy))
(check-expect (mutual-neighbors G2 'john 'rob) '(dan rachel))
(check-expect (mutual-neighbors G2 'dan 'rachel) '())
(define (mutual-neighbors g s1 s2)
  (filter (λ(x) (member? x ((graph-neighbors g) s1))) ((graph-neighbors g) s2)))

; Exercise 3

;; slice: [List-of X] PositiveInteger -> [List-of [List-of X]]
;; slices the given list into segments that are the length of the given integer
;; Termination Argument : Each time the function recurs, the list gets smaller by i items until the
;;                       length of the list is less than or equal to i and the base case is reached.
(check-expect (slice '(a b c d e) 5) '((a b c d e)))
(check-expect (slice '(a b c d e) 2) '((a b) (c d) (e)))
(check-expect (slice '(a b c d e) 3) '((a b c) (d e)))
(check-expect (slice '(1 2 3 4 5) 2) '((1 2) (3 4) (5)))
(check-expect (slice '(1 2 3 4 5) 5) '((1 2 3 4 5)))
(check-expect (slice '("hello" "my" "name" "is") 2) '(("hello" "my") ("name" "is")))
(define (slice alox i)
  (cond [(<= (length alox) i) (list alox)]
        [else (cons (firsti alox i) (slice (restlist alox i) i))]))

;; firsti : [List-of X] PositiveInteger -> [List-of X]
;; creates of the list of the first i elements of the given list
(check-expect (firsti '() 4) '())
(check-expect (firsti '(a b c d e) 0) '())
(check-expect (firsti '("hello" "my" "name" "is") 0) '())
(check-expect (firsti '(a b c d e) 2) '(a b))
(check-expect (firsti '(a b c d e) 3) '(a b c))
(check-expect (firsti '(1 2 3 4 5) 2) '(1 2))
(check-expect (firsti '("hello" "my" "name" "is") 2) '("hello" "my"))
(define (firsti alox i)
  (cond [(or (empty? alox) (= i 0)) '()]
        [(and (cons? alox) (positive? i))
         (cons (first alox) (firsti (rest alox) (- i 1)))]))
      

;; restlist : [List-of X] PositiveInteger -> [List-of X]
;; creates a list of the remaining elements of the list after removng i elements
(check-expect (restlist '() 3) '())
(check-expect (restlist '(a b c d e) 0) '(a b c d e))
(check-expect (restlist '("hello" "my" "name" "is") 0) '("hello" "my" "name" "is"))
(check-expect (restlist '(a b c d e) 2) '(c d e))
(check-expect (restlist '(a b c d e) 3) '(d e))
(check-expect (restlist '(1 2 3 4 5) 2) '(3 4 5))
(check-expect (restlist '("hello" "my" "name" "is") 2) '("name" "is"))
(define (restlist alox i)
  (cond [(or (empty? alox) (= i 0)) alox]
        [(and (cons? alox) (positive? i))
         (restlist (rest alox) (- i 1))]))
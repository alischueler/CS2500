;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])

;; [Tree X] -> ?
#;(define (tree-x-temp t)
    (cond [(leaf? t) ...]
          [(node? t) ... (node-data t)
                     ... (tree-x-temp (node-left t))
                     ... (tree-x-temp (node-right t))]))
 
 
(define-struct person [name yob])
; A Person is a (make-person String Number)
; - where name is the person's name
; - and yob is their year of birth

;; Person -> ?
#;(define (person-temp p)
    ... (person-name p) ... (person-yob p))
 
; An FT is a [Tree Person] and represents a family tree, with the youngest person at the root.

;; FT -> ? 
#;(define (ft-temp ft)
    (cond [(leaf? ft) ...]
          [(node? ft) ... (person-temp (node-data ft))
                      ... (ft-temp (node-left ft))
                      ... (ft-temp (node-right ft))]))

;; person examples
(define PERSON1 (make-person "bob" 2005))
(define PERSON2 (make-person "john" 2000))
(define PERSON3 (make-person "sally" 1995))
(define PERSON4 (make-person "mary" 1970))
(define PERSON5 (make-person "alice" 1990))
;; Tree examples
(define TREE0 (make-leaf))
;; FT examples
(define FT1 (make-node PERSON4 TREE0 TREE0))
(define FT2 (make-node PERSON3 FT1 TREE0))
(define FT3 (make-node PERSON5 TREE0 TREE0))
(define FT4 (make-node PERSON2 FT2 FT3))
(define FT5 (make-node PERSON1 TREE0 FT4))

;; Exercise 1
;; mirror: FT -> FT
;; flips all branches of the tree
(check-expect (mirror TREE0) TREE0)
(check-expect (mirror FT1) FT1)
(check-expect (mirror FT2) (make-node PERSON3 TREE0 FT1))
(check-expect (mirror FT5)
              (make-node PERSON1
                         (make-node PERSON2
                                    FT3
                                    (make-node PERSON3 TREE0 FT1))
                         TREE0))
#;(define (mirror ft)
    (cond [(leaf? ft) ft]
          [(node? ft) (make-node (node-data ft)
                                 (mirror (node-right ft))
                                 (mirror (node-left ft)))]))

;; Exercise 2
;; names: FT -> [List-of String]
;; creates a list of names in a FT
(check-expect (names TREE0) '())
(check-expect (names FT1) (list "mary"))
(check-expect (names FT2) (list "sally" "mary"))
(check-expect (names FT5) (list "bob" "john" "alice" "sally" "mary"))

#;(define (names ft)
    (cond [(leaf? ft) '()]
          [(node? ft) (person-names (node-data ft)
                                    (names (node-right ft))
                                    (names (node-left ft)))]))

;; person-names: Person [List-of String] [List-of String] -> [List-of String]
;; appends lists of names
(check-expect (person-names PERSON1 (list "Jill" "Rob") (list "Sally" "Alice"))
              (list "bob" "Jill" "Rob" "Sally" "Alice"))
(check-expect (person-names PERSON1 '() '()) (list "bob"))
(define (person-names p rlos llos)
  (append (list (person-name p)) rlos llos))

;; Exercise 3
;; foldtree : [X Y Y -> Y] Y [Tree X] -> Y
;; folds the data of nodes in a tree using the given operator
(check-expect (maptree make-node TREE0 FT1) FT1)
(check-expect (maptree make-node TREE0 FT5)
              (make-node PERSON1
                         (make-node PERSON2
                                    FT3
                                    (make-node PERSON3 TREE0 FT1))
                         TREE0))
(check-expect (maptree person-names '() FT1) (list "mary"))
(check-expect (maptree person-names '() FT5) (list "bob" "john" "alice" "sally" "mary"))

(define (maptree op bc ft)
  (cond [(leaf? ft) bc]
        [(node? ft) (op (node-data ft)
                        (maptree op bc (node-right ft))
                        (maptree op bc (node-left ft)))]))


;; mirror: FT -> FT
;; flips all branches of the tree
(check-expect (mirror TREE0) TREE0)
(check-expect (mirror FT1) FT1)
(check-expect (mirror FT2) (make-node PERSON3 TREE0 FT1))
(check-expect (mirror FT5)
              (make-node PERSON1
                         (make-node PERSON2
                                    FT3
                                    (make-node PERSON3 TREE0 FT1))
                         TREE0))
(define (mirror ft)
  (maptree make-node TREE0 ft))

;; names: FT -> [List-of String]
;; creates a list of names in a FT
(check-expect (names TREE0) '())
(check-expect (names FT1) (list "mary"))
(check-expect (names FT2) (list "sally" "mary"))
(check-expect (names FT5) (list "bob" "john" "alice" "sally" "mary"))
(define (names ft)
  (maptree person-names '() ft))


;; EXERCISE 4
; An Sexpr is one of: 
; – Symbol 
; – Lexpr

;; sexpr-temp -> Sexpr -> ?
#;(define (sexpr-temp s)
    (cond [(symbol? s)...]
          [(list? s) ... (lexpr-temp s)]))
 
; A Lexpr is one of: 
; – '()
; – (cons Sexpr Lexpr)


;; lexpr-temp : Lexpr -> ?
#;(define (lexpr-temp l)
    (cond [(empty? l) ...]
          [(cons? l) ... (sexpr-temp (first l))
                     ... (lexpr-temp (rest l))]))


;; examples
(define SEXPR0 '())
(define SEXPR1 (cons 'blue SEXPR0))
(define SEXPR2 (cons 'red SEXPR1))


;; contains-same-symbols?: Sexpr Sexpr -> Boolean
;; do the two Sexprs contain the same symbols?
(check-expect (contains-same-symbols? 'a '(a)) #true)
(check-expect (contains-same-symbols? 'a '(a b)) #false)
(check-expect (contains-same-symbols? SEXPR1 'blue) #true)
(check-expect (contains-same-symbols? SEXPR1 SEXPR2) #false)

(define (contains-same-symbols? s1 s2)
  (and (has-all-symbols? s1 s2) (has-all-symbols? s2 s1)))

;; has-all-symbols?: Sexpr Sexpr -> Boolean
;; does the first Sexpr have all symbols of the second Sexpr?
(check-expect (has-all-symbols? SEXPR0 SEXPR0) #true)
(check-expect (has-all-symbols? SEXPR0 SEXPR1) #false)
(check-expect (has-all-symbols? SEXPR1 SEXPR2) #false)
(check-expect (has-all-symbols? SEXPR1 SEXPR1) #true)
(check-expect (has-all-symbols? '((a (b b) c)) '(c b a)) #true)
(define (has-all-symbols? s1 s2)
  (cond [(symbol? s2) (contains-symbol? s2 s1)]
        [(list? s2) (andmap (λ (a-sexpr) (has-all-symbols? s1 a-sexpr)) s2)]))

;; contains-symbol? : Symbol Sexpr -> Boolean
;; is the given symbol in the Sexpr?
(check-expect (contains-symbol? 'blue '()) #false)
(check-expect (contains-symbol? 'blue SEXPR1) #true)
(check-expect (contains-symbol? 'red SEXPR2) #true)
(check-expect (contains-symbol? 'blue SEXPR2) #true)
(define (contains-symbol? sym s1)
  (cond [(symbol? s1) (symbol=? s1 sym)]
        [(list? s1) (ormap (λ (a-sexpr) (contains-symbol? sym a-sexpr)) s1)]))



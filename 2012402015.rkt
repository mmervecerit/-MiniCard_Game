#lang scheme
; compiling: yes
; complete: yes
; 2012402015



;4.1
;
;(card-color one-card) -> symbol
; one-card -> pair
;
;This function below takes input of one card and returns the color of the card.
;one-card is the input as a pair. Its first element is type of the card, and the second is value. If the type is Hearts or Diamonds, the color is red. Otherwise the color is black.
;Therefore, this function checks if the card type is Hearts of Diamonds, and gives red as output:card-color. Otherwise outputs black.
;If the first element is not H or D or S or C; function gives "Invalid Card Suit" Error.

;Examples:
; > ( card-color '(H . A) )
; =>red
; > ( card-color '(S . 10) ) 
; =>black

(define (card-color one-card)
  (if (or (eqv? (car one-card) 'H)(eqv? (car one-card) 'D))
      'red
      (if (or (eqv? (car one-card) 'S) (eqv? (car one-card) 'C)) 'black "Invalid Card Suit")))



;4.2

;(card-rank one-card) -> number
; one-card -> pair
;
;This function below takes input of one card and returns the value of the card (value is called as rank).
;The value of the card is indicated as the second element of the pair one-card.
;Therefore this function checks the second element and the outputs the value accordingly.
;The rank or value of the A is 11. K,Q,J has the value of 10. And others has the same value with them since they are [2,10] numbers.
;If numbers are beyond the limit of [2,10]; function gives "Invalid Card Rank" Error.
;
;Examples:
; > (card-rank '(H . A))
; =>11
; > (card-rank '(D . 5))
; =>5
                            
(define (card-rank one-card)
  (if (or (eqv? (cdr one-card) 'K) (eqv? (cdr one-card) 'Q) (eqv? (cdr one-card) 'J))
      '10
      (if (eqv? (cdr one-card) 'A)
          '11
          (if (and (> (cdr one-card) 1) (<= (cdr one-card) 10)) (cdr one-card) "Invalid Card Rank"))))

;4.3

; (all-same-color list-of-cards) -> boolean
; list-of-cards -> list
;
;The function below checks if the input list has the cards with all same color. If they have the same color, it outputs #t for true, otherwise it gives #f for false.
;
;Examples:
; >( all-same-color '((H . 3) (H . 2) (H . A) (D . A) (D . Q) (D . J)) )
; => #t
; >(all-same-color'((S.3)(S.2)(S.A)(C.A)(C.Q)(C.J)) )
; => #t
; >(all-same-color'((H.3)(H.2)(H.A)(D.A)(D.Q)(C.J)) )
; => #f

(define (all-same-color list-of-cards)
  (if (null? list-of-cards)
      #t
      (if (eqv? (card-color (car list-of-cards)) (if (= (length list-of-cards) 1) (card-color (car list-of-cards)) (card-color (car (cdr list-of-cards)))))
          (all-same-color (cdr list-of-cards))
          #f)))

;4.4

; (fdraw list-of-cards held-cards) ->list
; (list-of-cards) -> list
; (held-cards) -> list
;
;The function "fdraw" takes two lists: list-of-cards(card-list) and held-cards.
;And makes draw operation which takes the first element of the first list(list-of-cards) and appends that element to the second list(held-cards).
;Note that the first element of the list-of-cards is not removed in this stage. It will be done during the game.
;Also, if the list is empty, it means there is no more card to draw, the function will output GAME OVER.
;To implement the function I first implemented an append function. appendlists function adds the second list(or element) to the end of the first list.
;I also could use the append function of the scheme. It gives the same result.
;
;Examples:
; >( fdraw '((H . 3) (H . 2) (H . A) (D . A) (D . Q) (D . J)) '())
; => ((H . 3))
; >( fdraw '((H . 3) (H . 2) (H . A) (D . A) (D . Q) (D . J)) '((S . 3) (S . 2) (S . A)))
; => ((S . 3) (S . 2) (S . A) (H . 3))

;helper function
(define (appendlists firstlist secondlist)
 (if (null? firstlist)
     secondlist
     (cons (car firstlist) (append (cdr firstlist) secondlist))))

;the function
(define (fdraw list-of-cards held-cards)
  (if (null? list-of-cards)
      "GAME OVER"
      (appendlists held-cards (list (car list-of-cards)))))

;4.5

; (fdiscard list-of-cards list-of-moves goal held-cards) -> list
; (list-of-cards) -> list
; (list-of-moves) -> list
; (goal) -> number
; (held-cards) -> list
;
;The function "fdiscard" takes list-of-cards(card-list), list-of-moves, goal and held-cards. It updates held-cards after removing a card and returns the updated held-cards list.
;As a basic strategy, I discard the first card of the held-cards.
;Also, if the held-cards list is empty, the function output GAME OVER.
;
;Examples:
; >(fdiscard '((C . 3) (C . 2) (C . A) (S . J) (S . Q) (H . J)) '(draw draw draw discard) 66   '((H . 3) (H . 2) (H . A) (D . A) (D . Q) (D . J))))
; => ((H . 2) (H . A) (D . A) (D . Q) (D . J))

(define (fdiscard list-of-cards list-of-moves goal held-cards)
  (if (null? held-cards)
      "GAME OVER"
      (cdr held-cards)))


;4.8
; this is here because I used this function in 4.6.
;(calc-playerpoint list-of-cards) -> number
;(list-of-cards) -> list
;
; This function calculates and returns the playerpoint for the list of cards. A has the value of 11. K,Q,J has the value of 10. And the numbers has the value of own.
; To implement this function i need a function that can apply a function to every element of the list. First I implemented it. Then I used it and accumulated the ranks accordingly.
;
;Examples:  
; > (calc-playerpoint '((H . A) (H . 3) (D . J) (C . 10)))
; => 34

;helper function:
(define (functionoverlist function mylist)
   (if (null? mylist) '() ;if my list null no function applying can be done.
       (if (list? (car mylist))  ; if my list contains lists in it, it should open up it too.
           (cons (functionoverlist function (car mylist)) (functionoverlist function (cdr mylist))) ;recursive calls
           (cons (function (car mylist)) (functionoverlist function (cdr mylist))))))

;the function:
(define (calc-playerpoint list-of-cards) (foldr + 0 (functionoverlist card-rank list-of-cards)))



;4.6 

;(find-steps list-of-cards list-of-moves goal)
; (list-of-cards) -> list
; (list-of-moves) -> list
; (goal) -> number
;
; This function returns a list of steps that is list of pairs of moves and corresponding cards along the game.
; It gives the list of steps until the game ends, therefore it checks the game over conditions too. And, before calling this function held-cards is assumed to be empty.
;
; Examples:
; > ( find-steps '((H . 3) (H . 2) (H . A) (D . J) (D . Q) (C . J)) '(draw draw draw discard) 16)
; => ((draw (H . 3)) (draw (H . 2)) (draw (H . A)) (discard (H . 3)))


(define (find-steps-helper list-of-cards list-of-moves goal  game-over-list held-cards)
  (cond ((null? list-of-moves) game-over-list) ; Game-Over-1.
        ((and (eqv? (car list-of-moves) 'draw) (null? list-of-cards)) game-over-list) ;Game-Over-2.
        ((and (eqv? (car list-of-moves) 'discard) (null? held-cards)) game-over-list); Game-Over-3.
        ((> (calc-playerpoint held-cards) goal) game-over-list); Game-Over-4.
        (else ;if there is no game over.
         (if (eqv? 'draw (car list-of-moves)) ;if the move is draw, we add the card to our held-cards and the game over list. Game-over-list is the final list we have at the time when game ends.
             ;Recursive calls are done for iterating the moves.
             (find-steps-helper (cdr list-of-cards) (cdr list-of-moves) goal (append game-over-list (cons (list 'draw (car list-of-cards)) '())) (append held-cards (cons (car list-of-cards) '())))
             (find-steps-helper list-of-cards (cdr list-of-moves) goal ;if the move is discard we remove the first element from the held-cards and append discarding to the steps list.
                                (append game-over-list (cons (list 'discard (car held-cards)) '())) (remove (car held-cards) held-cards))))))
            
       
(define (find-steps list-of-cards list-of-moves goal)
  (find-steps-helper list-of-cards list-of-moves goal '() '()))



;4.7

;(find-held-cards list-of-steps) -> list
;(list-of-steps) -> list
;
;This function returns the list of held cards after executing the steps in the list-of-steps. It assumes that the initial held-cards list is empty.
;I implemented a helper function to deal with discards.
;
; Examples:
; > ( find-held-cards '((draw (H . 3)) (draw (H . 2)) (draw (H . A)) (discard (H . 3))))
; => ((H . 2) (H . A))

;helper function
(define (find-held-cards-helper intermediarylist list-of-steps count) ;intermediarylist is the list with everything all discards and the draws
  (cond
  ((equal? count 1) (remove (car (cdr (car (filter (lambda (x) (eqv? (car x) 'discard)) list-of-steps)))) intermediarylist))
  ((> count 1 ) (find-held-cards-helper (remove (car (cdr (car (filter (lambda (x) (eqv? (car x) 'discard)) list-of-steps)))) intermediarylist) (remove (car (filter (lambda (x) (eqv? (car x) 'discard)) list-of-steps)) list-of-steps) (- count 1)))))
;extract the elements that has the same card with the discard, from the intermediary list.
 
;the function
(define (find-held-cards list-of-steps)
  (if (null? (filter (lambda (x) (eqv? (car x) 'discard)) list-of-steps)) ;if there is no discard in the list of steps
      (foldr (lambda (x y) (if (eqv? 'draw (car x)) (append (cdr x) y) (filter (lambda (t) (equal? (car (cdr t)) (car (cdr x)))) y))) '() list-of-steps) ;add draws' card to an empty list.
      (find-held-cards-helper
       (foldr (lambda (x y) (if (eqv? 'draw (car x)) (append (cdr x) y) (filter (lambda (t) (equal? (car (cdr t)) (car (cdr x)))) y))) ;else you put everthing to the empty list and call the helper function.
              '() list-of-steps) list-of-steps (length (filter (lambda (x) (eqv? (car x) 'discard)) list-of-steps)))
      )
)


  


;4.9

;(calc-score list-of-cards goal) -> number
;(list-of-cards) -> list
;(goal) -> number
;
; This function calculates and returns finalscore by looking at list-of-cards and goal.
; at first, a prescore is calculated. This prescore is calculated in 2 different ways.
; If playerpoint is greater than goal, 5*(playerpoint - goal) will be the prescore.
; Otherwise goal - playerpoint will be the prescore.
; The finalscore is prescore or prescore/2 depending on the card colors in the list-of-cards.
;
;Examples:
; > (calc-score '((H . 3) (H . 2) (H . A) (D . J) (D . Q) (C . J)) 50)
; => 4


(define (calc-score list-of-cards goal)

  (if (> (calc-playerpoint list-of-cards) goal)
      (if (all-same-color list-of-cards) (floor (/ (* (- (calc-playerpoint list-of-cards) goal) 5) 2)) (* (- (calc-playerpoint list-of-cards) goal) 5))
      (if (all-same-color list-of-cards) (floor (/ (- goal (calc-playerpoint list-of-cards)) 2)) (- goal (calc-playerpoint list-of-cards))))
  )



;4.10

;(play list-of-cards list-of-moves goal) -> number
;(list-of-cards) -> list
;(list-of-moves) -> list
;(goal) -> number
;
;This function returns the finalscore until the game ends(Game over conditions are important, which are checked by find-steps).
;
;Examples:
;
; > ( play '((H . 3) (H . 2) (H . A) (D . J) (D . Q) (C . J)) '(draw draw draw discard) 16)
; => 1


(define (play list-of-cards list-of-moves goal) (calc-score (find-held-cards (find-steps list-of-cards list-of-moves goal)) goal))






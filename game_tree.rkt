#lang racket

;; generating game tree in tic tac toe
;; https://claude.ai/share/bbbb58f6-4cb1-490a-bb42-994c9f376e90

;; Data Definitions
(struct ttt (board moves))
;; A GameTree is a (ttt Board [List-of Move])
;; Interpretation: represents a game state and all possible next moves

(struct action (player position))
;; An Action is a (action Player Position)
;; Interpretation: player taking a specific position on the board

;; A Move is a (list Action GameTree)
;; Interpretation: an action paired with the resulting game tree

;; A Player is one of:
;; - 'X
;; - 'O

;; A Board is a [List-of [List-of Square]]
;; where the list has exactly 3 elements (rows)
;; and each row has exactly 3 elements (columns)
;; Interpretation: represents a 3x3 tic-tac-toe board

;; A Square is one of:
;; - 'X
;; - 'O
;; - '_
;; Interpretation: 
;; - 'X represents X player's mark
;; - 'O represents O player's mark
;; - '_ represents an empty square

;; Examples of Boards:
(define EMPTY-BOARD
  '((_ _ _)
    (_ _ _)
    (_ _ _)))

(define BOARD-1
  '((X _ _)
    (_ O _)
    (_ _ _)))

(define BOARD-X-WINS
  '((X X X)
    (O O _)
    (_ _ _)))

(define BOARD-FULL-DRAW
  '((X O X)
    (O X X)
    (O X O)))

;; A Position is a Natural number representing a board location


;; ============================================================
;; generate-ttt-tree : Player Player -> GameTree
;; Purpose: Generate complete game tree starting from empty board
;; Given: player1 - the first player to move
;;        player2 - the second player
;; Produces: a complete game tree of all possible games

;; Examples/Tests:
;; (Assuming helper functions exist)

;; Example 1: Game over immediately (board full or won)
;; (generate-ttt-tree 'X 'O) where board shows X won
;; => (ttt winning-board '())

;; Example 2: One move left
;; (generate-ttt-tree 'X 'O) with almost-full board
;; => (ttt almost-full-board 
;;         (list (list (action 'X last-position)
;;                     (ttt full-board '()))))

;; Template: structural recursion + generative recursion
;; The function generates a tree by exploring all possible moves

(define (generate-ttt-tree player1 player2)
  (define (generate-tree board player opponent)
    (ttt board (generate-moves board player opponent)))
  (define (generate-moves board0 player opponent)
    (define free-fields (board-find-free-fields board0))
    (for/list ((f free-fields))
      (define actnow (action player f))
      (define board1 (board-take-field board0 player f))
      (list actnow (generate-tree board1 opponent player)))))

;; -- start here --
(generate-tree the-empty-board player1 player2)

;; how to improve the tree operations efficiency

;; this version of tic tac toe is absolutely incomplete. There is a lot of handwaving.
;; But it just intends to demonstrate a way to create game trees which we'll use for our next game
#lang racket

;; A playable tic-tac-toe built on the game tree sketched in game_tree_ttt.rkt.
;; The point is not the game -- it is watching the tree shrink as you descend it.
;; Run with:  racket ttt.rkt      (or (play-game) at the REPL)

(provide (all-defined-out))

;; ============================================================
;; Data Definitions
;; ============================================================

(struct ttt (board moves) #:transparent)
;; A GameTree is a (ttt Board [List-of Move])
;; Interpretation: a game state and all possible next moves

(struct action (player position) #:transparent)
;; An Action is a (action Player Position)

;; A Move is a (list Action GameTree)

;; A Player is one of: 'X | 'O
;; A Square is one of: 'X | 'O | '_
;; A Board is a [List-of [List-of Square]], 3 rows of 3
;; A Position is a Natural in [0, 9), numbering the board left-to-right, top-to-bottom

(define EMPTY-BOARD
  '((_ _ _)
    (_ _ _)
    (_ _ _)))

;; ============================================================
;; Board operations
;; ============================================================

;; board-ref : Board Position -> Square
(define (board-ref board pos)
  (list-ref (list-ref board (quotient pos 3)) (remainder pos 3)))

;; board-find-free-fields : Board -> [List-of Position]
;; Purpose: every position still holding '_
(define (board-find-free-fields board)
  (for/list ([pos (in-range 9)]
             #:when (eq? '_ (board-ref board pos)))
    pos))

;; board-take-field : Board Player Position -> Board
;; Purpose: a new board with player's mark written at pos
(define (board-take-field board player pos)
  (define r (quotient pos 3))
  (define c (remainder pos 3))
  (for/list ([row board] [i (in-naturals)])
    (if (= i r)
        (for/list ([square row] [j (in-naturals)])
          (if (= j c) player square))
        row)))

(define LINES
  '((0 1 2) (3 4 5) (6 7 8)     ; rows
    (0 3 6) (1 4 7) (2 5 8)     ; columns
    (0 4 8) (2 4 6)))           ; diagonals

;; board-winner : Board -> [Maybe Player]
;; Purpose: the player owning a complete line, or #f
(define (board-winner board)
  (for/or ([line LINES])
    (define a (board-ref board (first line)))
    (and (not (eq? a '_))
         (eq? a (board-ref board (second line)))
         (eq? a (board-ref board (third line)))
         a)))

;; ============================================================
;; Tree generation
;; ============================================================

;; generate-ttt-tree : Player Player -> GameTree
;; Purpose: the complete game tree with player1 to move on an empty board
(define (generate-ttt-tree player1 player2)
  (generate-tree EMPTY-BOARD player1 player2))

;; generate-tree : Board Player Player -> GameTree
(define (generate-tree board player opponent)
  (ttt board (generate-moves board player opponent)))

;; generate-moves : Board Player Player -> [List-of Move]
;; Purpose: one move per free field, unless the game is already decided.
;; Note: the sketch in game_tree_ttt.rkt stopped only when the board filled up,
;; so play continued past a win. The board-winner test below is the fix.
(define (generate-moves board0 player opponent)
  (cond
    [(board-winner board0) '()]
    [else
     (for/list ([f (board-find-free-fields board0)])
       (list (action player f)
             (generate-tree (board-take-field board0 player f) opponent player)))]))

;; tree-player : GameTree -> [Maybe Player]
;; Purpose: whose turn it is, or #f when the game is over
(define (tree-player tree)
  (and (pair? (ttt-moves tree))
       (action-player (first (first (ttt-moves tree))))))

;; tree-move : GameTree Position -> [Maybe GameTree]
;; Purpose: descend into the subtree reached by playing pos
(define (tree-move tree pos)
  (for/or ([m (ttt-moves tree)])
    (and (= pos (action-position (first m))) (second m))))

;; ============================================================
;; Tree inspector
;; ============================================================

(struct stats (nodes depth games x-wins o-wins draws) #:transparent)
;; Stats about a subtree:
;;   nodes - total GameTree nodes, this one included
;;   depth - plies remaining along the longest line
;;   games - distinct complete games reachable from here
;;   x-wins / o-wins / draws - how those games end

(define stats-cache (make-weak-hasheq))

;; tree-stats : GameTree -> Stats
;; Purpose: summarize a subtree in one pass, memoized per node
(define (tree-stats tree)
  (hash-ref! stats-cache tree (lambda () (compute-stats tree))))

(define (compute-stats tree)
  (define moves (ttt-moves tree))
  (cond
    [(empty? moves)
     (define w (board-winner (ttt-board tree)))
     (stats 1 0 1 (if (eq? w 'X) 1 0) (if (eq? w 'O) 1 0) (if w 0 1))]
    [else
     (for/fold ([acc (stats 1 0 0 0 0 0)]) ([m moves])
       (define s (tree-stats (second m)))
       (stats (+ (stats-nodes acc) (stats-nodes s))
              (max (stats-depth acc) (add1 (stats-depth s)))
              (+ (stats-games acc) (stats-games s))
              (+ (stats-x-wins acc) (stats-x-wins s))
              (+ (stats-o-wins acc) (stats-o-wins s))
              (+ (stats-draws acc) (stats-draws s))))]))

;; ============================================================
;; Rendering
;; ============================================================

;; commas : Natural -> String
(define (commas n) (~r n #:groups '(3) #:group-sep ","))

;; square->string : Square Position -> String
;; Purpose: a mark, or the position number so you know what to type
(define (square->string square pos)
  (if (eq? square '_) (format "~a" pos) (format "~a" square)))

;; board->string : Board -> String
(define (board->string board)
  (string-join
   (for/list ([r (in-range 3)])
     (string-join
      (for/list ([c (in-range 3)])
        (define pos (+ (* r 3) c))
        (format " ~a " (square->string (board-ref board pos) pos)))
      "|"))
   "\n---+---+---\n"))

;; print-position : GameTree -> Void
;; Purpose: the board plus a one-line summary of the subtree hanging off it
(define (print-position tree)
  (printf "\n~a\n\n" (board->string (ttt-board tree)))
  (define s (tree-stats tree))
  (printf "subtree: ~a nodes, ~a plies deep, ~a games reachable  (X ~a / O ~a / draw ~a)\n"
          (commas (stats-nodes s))
          (stats-depth s)
          (commas (stats-games s))
          (commas (stats-x-wins s))
          (commas (stats-o-wins s))
          (commas (stats-draws s))))

;; print-branches : GameTree -> Void
;; Purpose: what each legal move costs you, in tree terms
(define (print-branches tree)
  (printf "\n  move   nodes      games     X wins    O wins     draws\n")
  (for ([m (ttt-moves tree)])
    (define s (tree-stats (second m)))
    (printf "  ~a    ~a  ~a  ~a  ~a  ~a\n"
            (action-position (first m))
            (~a (commas (stats-nodes s)) #:min-width 8 #:align 'right)
            (~a (commas (stats-games s)) #:min-width 9 #:align 'right)
            (~a (commas (stats-x-wins s)) #:min-width 9 #:align 'right)
            (~a (commas (stats-o-wins s)) #:min-width 9 #:align 'right)
            (~a (commas (stats-draws s)) #:min-width 9 #:align 'right))))

;; announce : GameTree -> Void
(define (announce tree)
  (define w (board-winner (ttt-board tree)))
  (if w
      (printf "\n~a wins.\n" w)
      (printf "\nDraw.\n")))

;; ============================================================
;; Playing
;; ============================================================

;; play-game : -> Void
;; Purpose: hot-seat tic-tac-toe; both sides are played by you.
;; At the prompt: a position number moves, u undoes, i shows the branch table,
;; q quits.
(define (play-game)
  (printf "Generating the full game tree...\n")
  (define start (current-inexact-milliseconds))
  (define root (generate-ttt-tree 'X 'O))
  (void (tree-stats root))
  (printf "done in ~a ms\n" (round (- (current-inexact-milliseconds) start)))
  (let loop ([tree root] [history '()])
    (print-position tree)
    (define player (tree-player tree))
    (cond
      [(not player)
       (announce tree)
       (when (pair? history)
         (printf "\n(u to undo, q to quit)\n")
         (define line (prompt "> "))
         (cond
           [(equal? line "u") (loop (first history) (rest history))]
           [(equal? line "q") (void)]
           [else (loop tree history)]))]
      [else
       (printf "~a to move, free: ~a\n"
               player
               (string-join (map number->string (board-find-free-fields (ttt-board tree))) " "))
       (define line (prompt (format "~a> " player)))
       (cond
         [(or (eof-object? line) (equal? line "q")) (void)]
         [(equal? line "i") (print-branches tree) (loop tree history)]
         [(equal? line "u")
          (if (pair? history)
              (loop (first history) (rest history))
              (begin (printf "nothing to undo\n") (loop tree history)))]
         [else
          (define pos (string->number (string-trim line)))
          (define next (and (exact-nonnegative-integer? pos) (tree-move tree pos)))
          (cond
            [next
             (printf "\n~a plays ~a -- subtree drops from ~a to ~a nodes\n"
                     player pos
                     (commas (stats-nodes (tree-stats tree)))
                     (commas (stats-nodes (tree-stats next))))
             (loop next (cons tree history))]
            [else
             (printf "not a legal move here\n")
             (loop tree history)])])])))

;; prompt : String -> [Maybe String]
(define (prompt text)
  (display text)
  (flush-output)
  (define line (read-line))
  (if (eof-object? line) line (string-trim line)))

(module+ main
  (play-game))

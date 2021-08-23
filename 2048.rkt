#lang racket

;; Implements the 2048 game

(require 2htdp/universe
         2htdp/image
         (only-in "2048-obj.rkt"
                  get-moves-text
                  new-world)
         (only-in utils/2htdp/image
                  scale-image
                  crop-image))

(define BOARD-ORDER 4)
(define SQUARE-SIZE 80)
(define FRAME-RATIO 10/11)
(define SQUARE-BORDER-SIZE (round (* SQUARE-SIZE (/ FRAME-RATIO))))
(define SQUARE-FRAME-WIDTH (round (* SQUARE-SIZE FRAME-RATIO)))
(define SQUARE-FRAME-HEIGHT (quotient SQUARE-SIZE 2))

(define TEXT-SIZE SQUARE-FRAME-HEIGHT)

(define HEADER-BORDER-WIDTH (* BOARD-ORDER SQUARE-BORDER-SIZE))
(define HEADER-BORDER-HEIGHT (round (* 1/2 BOARD-ORDER SQUARE-BORDER-SIZE)))

(define MT-COLOR 'white)
(define MT (empty-scene (* BOARD-ORDER SQUARE-BORDER-SIZE)
                        (* BOARD-ORDER SQUARE-BORDER-SIZE)
                        MT-COLOR))

(define WORLD-STATE-FILE "2048-World-State")

(define (render ws)
  (let ([board (send ws get-board)])
    (overlay (above
              (draw-header ws)
              (draw-board board))
             MT)))

(define (draw-header ws)
  (let ([border (rectangle HEADER-BORDER-WIDTH 
                           HEADER-BORDER-HEIGHT 
                           'solid 
                           (color-chooser 0 'border-color))]
        [score (draw-score-box ws SQUARE-SIZE SQUARE-SIZE)]
        [instr (draw-instr-box TEXT-SIZE 
                               'black 
                               (* (sub1 BOARD-ORDER) SQUARE-SIZE) 
                               SQUARE-SIZE
                               (* (sub1 BOARD-ORDER) SQUARE-SIZE 10/11)
                               SQUARE-SIZE)]
        [moves (draw-moves-box ws 
                               TEXT-SIZE 
                               'black
                               (* BOARD-ORDER SQUARE-SIZE) 
                               (* 1/3 SQUARE-SIZE)
                               (* BOARD-ORDER SQUARE-SIZE FRAME-RATIO)
                               (* 1/4 SQUARE-SIZE FRAME-RATIO))]
        [spacer (rectangle (* BOARD-ORDER SQUARE-SIZE) 
                           (* 1/3 SQUARE-SIZE)
                           'solid
                           'transparent)])
    (apply overlay (list (above (beside score instr) spacer moves spacer) border))))

(define (draw-score-box ws frame-width frame-height)
  (draw-board-square (send ws get-board-tot)
                     #:text-color 'black
                     #:square-color (color-chooser 0 'square-color)
                     #:border-size SQUARE-SIZE
                     #:border-color 'transparent))

(define (draw-instr-box text-size 
                        text-color 
                        box-width
                        box-height
                        frame-width
                        frame-height)
  (overlay/align "middle" "middle" 
                 (scale-image 
                  frame-width
                  frame-height
                  (above (text "Arrow keys to merge cells" text-size text-color)
                         (rectangle frame-width 3 'solid 'transparent)
                         (text "Page Up/Down to rotate" text-size text-color)
                         (rectangle frame-width 3 'solid 'transparent)
                         (text "Space to undo move" text-size text-color)
                         (rectangle frame-width 3 'solid 'transparent)
                         (text "'R' key to restart" text-size text-color)))
                 (rectangle box-width box-height 'solid 'transparent)))

(define (draw-moves-box ws 
                        text-size 
                        text-color 
                        box-width
                        box-height
                        frame-width
                        frame-height)
  (let* ([moves (get-moves-text ws)]
         [undo-text (car moves)]
         [moves-text (caadr moves)]
         [moves-result (cadadr moves)]
         [x-posn (if (zero? moves-result) "middle" "left")]
         [text-color (if (zero? moves-result) 'darkred text-color)]
         [image (if undo-text 
                    (beside (text undo-text text-size text-color) 
                            (text moves-text text-size text-color)) 
                    (text moves-text text-size text-color))])
    (overlay/align x-posn "middle" 
                   (scale-image frame-width frame-height image)
                   (rectangle box-width box-height 'solid 'transparent))))

(define (draw-board board)
  (let loop ([board board] [acc empty])
    (cond
      [(null? board) (apply above (reverse acc))]
      [else (loop (cdr board) (cons (draw-board-row (car board)) acc))])))

(define (draw-board-row row)
  (let loop ([row row] [acc empty])
    (cond
      [(null? row) (apply beside (reverse acc))]
      [else (loop (cdr row) (cons (draw-board-square (car row)) 
                                  acc))])))

(define (draw-board-square n
                           #:text-size (text-size TEXT-SIZE)
                           #:text-color (text-color (color-chooser n 'text-color))
                           #:frame-width (frame-width SQUARE-FRAME-WIDTH)
                           #:frame-height (frame-height SQUARE-FRAME-HEIGHT)
                           #:square-size (square-size SQUARE-SIZE)
                           #:square-color (square-color 
                                           (color-chooser n 'square-color))
                           #:border-size (border-size SQUARE-BORDER-SIZE)
                           #:border-color (border-color 
                                           (color-chooser n 'border-color)))
  (let* ([memo empty]
         [lookup (or (zero? n) (integer? (/ (log n) (log 2))))]
         [ans (assoc n memo)]
         [image (cond
                  [(and lookup ans) (cdr ans)]
                  [else (apply overlay 
                               (list 
                                (draw-text (number->string n)
                                           text-size 
                                           text-color
                                           #:frame-width frame-width
                                           #:frame-height frame-height)
                                (square square-size 'solid square-color)
                                (square border-size 'solid border-color)))])])
    (when lookup
      (set! memo (cons (cons n image) memo)))
    image))

(define (draw-text string
                   text-size
                   text-color
                   #:frame-width (frame-width text-size)
                   #:frame-height (frame-height text-size))
  (scale-image frame-width 
               frame-height 
               (crop-image (text string text-size text-color))))

(define (color-chooser n (c 'text-color))
  (cond
    [(pen? c) c]
    [(not (or (eq? c 'text-color) 
              (eq? c 'square-color) 
              (eq? c 'border-color))) c]
    [else (let ([posn (cond
                        [(eq? c 'text-color)   0]
                        [(eq? c 'square-color) 1]
                        [(eq? c 'border-color) 2]
                        [else (error "Color-chooser: invalid color-type")])])
            (list-ref (match (modulo n 32768)
                        [0    '(lightgray lightgray darkgray)]
                        [2    '(black white darkgray)]
                        [4    '(black orange darkgray)]
                        [8    '(black yellow darkgray)]
                        [16   '(white purple darkgray)]
                        [32   '(black pink darkgray)]
                        [64   '(white red darkgray)]
                        [128  '(black aqua darkgray)]
                        [256  '(white blue darkgray)]
                        [512  '(white darkblue darkgray)]
                        [1024 '(black lightgreen darkgray)]
                        [2048 '(white darkgreen darkgray)]
                        [4096 '(white darkgreen darkgray)]
                        [_    '(lightgray lightgray darkgray)])
                      posn))]))

(define (move ws ke)
  (send ws make-move ke))

(define (main)
  (send (big-bang 
         (send (new-world BOARD-ORDER) load-world-state WORLD-STATE-FILE)
         (to-draw render)
         (on-key move)
         (name "2048"))
        save-world-state WORLD-STATE-FILE))

(main)

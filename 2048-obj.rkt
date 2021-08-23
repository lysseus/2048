#lang racket

;; The 2048 object

(provide world%
         new-world
         get-moves-text)

(require (only-in utils/matrix
                  matrix
                  matrix-row
                  matrix-rotate)
         (only-in utils/list
                  values->list
                  values-ref
                  set-choices-to-value))

(define world%
  (class object%
    (super-new)
    
    (init-field board-order)
    
    (field [board-tot 0])
    (define/public (get-board-tot) board-tot)
    
    (field [undo-requested #f])
    (define/public (get-undo-requested) undo-requested)
    
    (field [undo-response 'n/a])
    (define/public (get-undo-response) undo-response)
    
    (field [board-max 0])
    (define/public (get-board-max) board-max)
    
    (field (board empty))
    (define/public (get-board) board)
    
    (field [move-stack (list board-tot board-max board)])
    
    (define/public (get-state) move-stack)
    
    (define/public (set-state! state) 
      (let* ([move (car state)]
             [moves (cdr state)]
             [tot (car move)]
             [max (cadr move)]
             [b (caddr move)])        
        (set! board-tot tot)
        (set! board-max max)
        (set! move-stack moves)
        (set! board b)))
    
    (define (shift-row-left row (scoref log-score))
      (let ([len (length row)]
            [row (filter (λ (v) (not (zero? v))) row)])
        (let loop ([row row] [acc '()])
          (cond
            [(null? row) 
             (reverse
              (if (> len (length acc))
                  (append (build-list (- len (length acc)) (λ (v) 0)) acc)
                  acc))]
            [(zero? (car row)) (loop (cdr row) acc)]
            [(null? (cdr row)) (loop (cdr row) (cons (car row) acc))]
            [(equal? (car row) (cadr row)) 
             (loop (cddr row) (cons (scoref (car row) (cadr row)) acc))]
            [else (loop (cdr row) (cons (car row) acc))]))))
    
    (define (shift-row-right row (scoref log-score))
      (reverse (shift-row-left (reverse row) scoref)))
    
    (define (score-merge v1 v2)
      (+ v1 v2))
    
    (define (log-score v1 v2)
      (let ([tot (score-merge v1 v2)])
        (when (> tot board-max)
          (set! board-max tot))
        (set! board-tot (+ board-tot tot))
        tot))
    
    (define (shift-board dir (scoref log-score))
      (let ([b (if (or (string=? dir "up")
                       (string=? dir "down"))
                   (matrix-rotate board -1)
                   board)])
        (let loop ([n (length b)] [acc '()])
          (cond
            [(zero? n)
             (if (or (string=? dir "up")
                     (string=? dir "down"))
                 (matrix-rotate acc 1)
                 acc)]
            [else (loop (sub1 n)
                        (cons ((cond
                                 [(string=? dir "left")  shift-row-left]
                                 [(string=? dir "right") shift-row-right]
                                 [(string=? dir "up")    shift-row-left]
                                 [(string=? dir "down")  shift-row-right]
                                 [else                   (λ (row f) row)])
                               (matrix-row b (sub1 n)) scoref)
                              acc))]))))
    
    (define/public (more-moves?)
      (let ([l0 (flatten board)] [flg 0])
        (unless (equal? l0 (flatten (shift-board "left" score-merge)))
          (set! flg (bitwise-ior flg 1)))
        (unless (equal? l0 (flatten (shift-board "right" score-merge)))
          (set! flg (bitwise-ior flg 2)))
        (unless (equal? l0 (flatten (shift-board "up" score-merge)))
          (set! flg (bitwise-ior flg 4)))
        (unless (equal? l0 (flatten (shift-board "down" score-merge)))
          (set! flg (bitwise-ior flg 8)))
        flg))
    
    (define/public (new-board)
      (set! board-max 0)
      (set! board-tot 0)
      (set! board (matrix (values-ref 
                           0
                           (set-choices-to-value (make-list 16 0)
                                                 zero?
                                                 (pick-val board-max)
                                                 #:iter 2))
                          #:order board-order))
      (set! move-stack (cons (list board-tot board-max board) '()))
      board)
    
    (define (next-board b)
      (matrix (values-ref 
               0
               (set-choices-to-value (flatten b)
                                     zero?
                                     (pick-val board-max)))
              #:order (length b)))
    
    (define/public (make-move dir)
      (let ([dir (string-downcase dir)])
        (cond 
          [(or (string=? dir " ") (string=? dir "r"))
           (set!-values (board undo-response) (undo-move dir))]
          [(string=? dir "prior")
           (set! board (matrix-rotate board -1))]
          [(string=? dir "next")
           (set! board (matrix-rotate board 1))]
          [(or (string=? dir "left")
               (string=? dir "right")
               (string=? dir "up")
               (string=? dir "down"))
           (set! undo-requested #f)
           (set! undo-response 'n/a)
           (let ([btot board-tot] [bmax board-max] [shifted (shift-board dir)])
             (unless (equal? shifted board)
               (set! move-stack (cons (list btot bmax board) move-stack))
               (set! board (next-board shifted))))])
        this))
    
    (define (undo-move ke)
      (set! undo-requested #t)
      (cond
        [(string=? ke " ")
         (let ([move (car move-stack)])
           (set! board-tot (car move))
           (set! board-max (cadr move))
           (cond
             [(null? (cdr move-stack)) 
              (values (caddr move) 'undo-fail)]
             [else 
              (set! move-stack (cdr move-stack)) 
              (values (caddr move) 'undo)]))]
        [(string=? ke "r")
         (values (new-board) 'restart)]
        [else (error "Unhandled key in undo-move.")]))
    
    (define/public (save-world-state file-name)
      (set! move-stack (cons (list board-tot board-max board) move-stack))
      (with-output-to-file file-name
        (λ () (write (get-state)))
        #:exists 'replace))
    
    (define/public (load-world-state file-name)
      (with-handlers ([exn:fail:filesystem? (λ (e) this)])
        (set-state! (with-input-from-file file-name
                      (λ () (read))))
        this))))

(define (new-world order)
  (let ([w (new world% [board-order order])])
    (send w new-board)
    w))

(define (pick-val val)
  (let ([r (random 100)])
    (cond
      [(and (>= val 8) (>= r 98)) 8]
      [(and (>= val 4) (>= r 80)) 4]
      [else 2])))

(define (undo-move-text world)
  (let ([undo-requested (send world get-undo-requested)]
        [undo-response (send world get-undo-response)])
    (cond
      [(not undo-requested) #f]
      [(eq? undo-response 'undo) "Previous board. "]
      [(eq? undo-response 'restart) "Game restarted. "]
      [else "No previous board. "])))

(define (avail-moves-text world)
  (let ([result (send world more-moves?)]
        [string ""])
    (if (zero? result)
        (set! string "GAME OVER" )
        (begin
          (set! string "Moves:")
          (when (bitwise-bit-set? result 0)
            (set! string (string-append string " left")))
          (when (bitwise-bit-set? result 1)
            {set! string (string-append string " right")})
          (when (bitwise-bit-set? result 2)
            (set! string (string-append string " up")))
          (when (bitwise-bit-set? result 3)
            (set! string (string-append string " down")))
          (set! string (string-append string "."))))
    (values string result)))

(define (get-moves-text world)
  (list (undo-move-text world) (values->list (avail-moves-text world))))
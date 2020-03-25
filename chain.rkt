#lang racket
(require "block.rkt"
         "chain-verifier.rkt")
         
(define chain%
  (class object%

    (super-new)
    
    (define blocks (list))

    (define (make-new-block index timestamp data previous-hash)      
      (let ([block (new block%)])
        (send block initialize index timestamp data previous-hash)
        block))
      
    (define (initialize-chain)
      (let ([genesis-block (make-new-block
                            0
                            (current-inexact-milliseconds)
                             "genesis block"
                             "")])  
        (set! blocks (append blocks (list genesis-block)))))

    (define/public (set-entries c)
      (set! blocks c))

    (define/public (add-entry data)
      (begin
        (when (empty? blocks) (initialize-chain))
        (let* ([last-block (last blocks)]
               [previous-hash (send last-block get-hash)]
               [previous-index (send last-block get-index)]
               [new-index (+ previous-index 1)]
               [new-block (make-new-block
                           new-index
                           (current-inexact-milliseconds)
                            data
                            previous-hash)])
          (set! blocks (append blocks (list new-block))))))

    (define/public (verify)
      (let ([verifier (new chain-verifier%)])
        (send verifier verify blocks)))

    (define/public (get-blocks)
      blocks)))

(provide chain%)
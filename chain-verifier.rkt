#lang racket
(require predicates)

(define chain-verifier%
  (class object%
    (super-new)

    (define/public (verify the-chain)                    
      (andmap true?
              (map (λ (x)
                     (let* ([idx (send x get-index)]
                            [previous-block (if (eq? 0 idx) null (list-ref the-chain (- idx 1)))]
                            [previous-hash (if (null? previous-block) null (send previous-block get-hash))]
                            [current-hash (send x get-previous-hash)])
                       (or (eq? null previous-hash) (string=? previous-hash current-hash))))
                   the-chain)))))

(provide chain-verifier%)
#lang racket

;Copyright 2020 Dave Wilson
;
;Licensed under the Apache License, Version 2.0 (the "License");
;you may not use this file except in compliance with the License.
;You may obtain a copy of the License at
;
;http://www.apache.org/licenses/LICENSE-2.0
;
;Unless required by applicable law or agreed to in writing, software
;distributed under the License is distributed on an "AS IS" BASIS,
;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;See the License for the specific language governing permissions and
;limitations under the License.

(require predicates)
(require "block.rkt")
         
(define chain%
  (class object%

    (super-new)
    
    (define blocks (list))
    
    (define/public (set-blocks c)
      (set! blocks c))

    (define/public (get-blocks)
      blocks)

    ;; Make and return a new block with the given index, timestamp, data and
    ;; previous hash
    (define (make-new-block index timestamp data previous-hash)      
      (let ([block (new block%)])
        (send block initialize index timestamp data previous-hash)
        block))

    ;; Initialise this chain by giving it one block: the "Genesis" block
    (define/public (initialize-chain)
      (let ([genesis-block (make-new-block
                            0
                            (current-inexact-milliseconds)
                             "genesis block"
                             "")])  
        (set! blocks (list genesis-block))))



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

    ;; Verify the integrity of the chain.
    ;; We do this by cycling through the blocks in the chain and for each,
    ;; checking that the value of its "previous-hash" attribute is the same as
    ;; the freshly-calculated hash of the previous block in the chain.
    (define/public (verify)
      (andmap true?
              (map (λ (x)
                     (let* ([idx (send x get-index)]
                            [previous-block (if (eq? 0 idx) null (list-ref blocks (- idx 1)))]
                            [previous-hash (if (null? previous-block) null (send previous-block
                                                                                 get-hash))]
                            [current-hash (send x get-previous-hash)])
                       (or (eq? null previous-hash) (string=? previous-hash current-hash))))
                   blocks)))))

(provide chain%)
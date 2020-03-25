#lang racket
(require racket/serialize)
(require sha)

(define-serializable-class* block% object% (externalizable<%>)
  (super-new)
  
  (define/public (initialize index timestamp data previous-hash)
    (begin
      (set-index index)
      (set-timestamp timestamp)
      (set-data data)
      (set-previous-hash previous-hash)))

  (define block-index 0)
  (define/public (get-index) block-index)
  (define (set-index idx) (set! block-index idx))
    
  (define block-timestamp (current-inexact-milliseconds))
  (define/public (get-timestamp) block-timestamp)
  (define (set-timestamp t) (set! block-timestamp t))
    
  (define block-data "")
  (define/public (get-data) block-data)    
  (define (set-data new-data) (set! block-data new-data))

  (define block-previous-hash "")
  (define/public (get-previous-hash) block-previous-hash)
  (define (set-previous-hash h) (set! block-previous-hash h))

  (define/public (externalize)
    (let ([h (make-hash)])
      (hash-set! h "index" (get-index))
      (hash-set! h "timestamp" (get-timestamp))
      (hash-set! h "data" (get-data))
      (hash-set! h "previous-hash" (get-previous-hash))                 
      h))

  (define/public (internalize in)    
    (let ([definitions (list
                        (list "index" set-index)
                        (list "timestamp" set-timestamp)
                        (list "data" set-data)
                        (list "previous-hash" set-previous-hash))])
      (for ([d definitions])
        ((second d) (hash-ref in(first d))))))
  
  (define/public (get-hash)
    (bytes->hex-string
     (sha256 (string->bytes/utf-8 (string-append
                                   (number->string block-index)
                                   (number->string block-timestamp)
                                   block-data
                                   block-previous-hash))))))

(provide block%)
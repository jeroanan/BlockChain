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

(require racket/serialize)
(require sha)
(require "member.rkt")

(require (for-syntax racket/base))

(define-serializable-class* block% object% (externalizable<%>)
  (super-new)

  ;; Each block contains four attributes:
  ;;   1. Index (default to zero; increment from there)
  ;;   2. Timestamp (default to current-inextact-milliseconds)
  ;;   3. Data (the data the block stores; default to empty string)
  ;;   4. The hash of the previous block in the chain. This is empty if it's the
  ;;      first block.
  (define/public (initialize index timestamp data previous-hash)
    (begin
      (set-index index)
      (set-timestamp timestamp)
      (set-data data)
      (set-previous-hash previous-hash)))

  (public-attribute index 0)
  (public-attribute timestamp (current-inexact-milliseconds))
  (public-attribute data "")
  (public-attribute previous-hash "")
  
  ;; Used for serialization; take the members of this class and write them to a
  ;; hash table.
  (define/public (externalize)
    (let ([h (make-hash)])
      (hash-set! h "index" (get-index))
      (hash-set! h "timestamp" (get-timestamp))
      (hash-set! h "data" (get-data))
      (hash-set! h "previous-hash" (get-previous-hash))                 
      h))

  ;; Used for deserialization; read hash table in and assign its values to class
  ;; members.
  (define/public (internalize in)    
    (let ([definitions (list
                        (list "index" (λ (x) (set-index x)))
                        (list "timestamp" (λ (x) (set-timestamp x)))
                        (list "data" (λ (x) (set-data x)))
                        (list "previous-hash" (λ (x) (set-previous-hash x))))])
      (for ([d definitions])
        ((second d) (hash-ref in (first d))))))

  ;; Make representation of this object by concatenating its members and running
  ;; through SHA-256
  (define/public (get-hash)
    (bytes->hex-string
     (sha256 (string->bytes/utf-8 (string-append
                                   (number->string (get-index))
                                   (number->string (get-timestamp))
                                   (get-data)
                                   (get-previous-hash)))))))

(provide block%)
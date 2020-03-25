#lang racket
(require racket/serialize)
(require "block.rkt"
         "chain.rkt"
         "gui/main-window.rkt")

(define chain (new chain%))

(define b (send chain get-blocks))
(define s (serialize b))
(define b2 (deserialize s))

(define window (new main-window% [chain chain]))
(send window show #t)
#lang racket
(require racket/serialize)
(require "block.rkt"
         "chain.rkt"
         "gui/main-window.rkt")

(define chain (new chain%))

(define window (new main-window% [chain chain]))
(send window show #t)
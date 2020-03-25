#lang racket/gui
(require framework
         racket/serialize)

(define main-window%
  (class frame%

    (super-new [label "blockchain test"]
               [width 600]
               [height 600])
    
    (init chain)
    (define the-chain chain)
    (define file-name null)

    (define (read-chain-from-file file-name)
      (let* ([f-port (open-input-file file-name)]
             [f-content (read f-port)]
             [the-blocks (deserialize f-content)])
        (send the-chain set-blocks the-blocks)))       
      
    (define menu (new menu-bar% [parent this]))
    (define file-menu (new menu%
                           [label "&File"]
                           [parent menu]))

    (define (open-menu-clicked)
      (let ([f (get-file)])
        (unless (false? f)
          (begin
            (read-chain-from-file f)
            (set! file-name f)
            (update-listbox)))))
      
    (define file-menu-open (new menu-item%
                                [parent file-menu]
                                [label "&Open"]
                                [callback (λ (me ce) (open-menu-clicked))]))


    (define (save-menu-clicked)
      (let ([f (get-file)])
        (unless (false? f)
          #f)))
    
    (define file-menu-save (new menu-item%
                                [parent file-menu]
                                [label "&Save"]
                                [callback (λ (me ce) (save-menu-clicked))]))
    
    (define hp (new horizontal-panel% [parent this]))
    
    (define listbox (new list-box%
                         [parent hp]
                         [label ""]
                         [choices (list)]))
    
    (define vp (new vertical-panel% [parent hp]))
    (define text-data (new text-field%
                           [label "Data"]
                           [parent vp]
                           [init-value ""]))

    (define (update-listbox)
      (let* ([blocks (send chain get-blocks)]
             [block-data (map (λ (b)
                                (send b get-data))
                              blocks)])
        (send listbox set block-data)))
    
    (define (add-button-clicked)
      (let ([the-text (send text-data get-value)])
        (begin
          (send the-chain add-entry the-text)
          (update-listbox))))
    
    (define add-button (new button%
                            [label "Add"]
                            [parent vp]
                            [callback (λ (me ce) (add-button-clicked))]))))

(provide main-window%)
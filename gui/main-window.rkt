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
      
    (define menu (new menu-bar% [parent this]))
    (define file-menu (new menu%
                           [label "&File"]
                           [parent menu]))
      
    (define file-menu-open (new menu-item%
                                [parent file-menu]
                                [label "&Open"]
                                [callback (λ (me ce) (open-menu-clicked))]))

    (define file-menu-save (new menu-item%
                                [parent file-menu]
                                [label "&Save"]
                                [callback (λ (me ce) (save-menu-clicked))]))

    (define file-menu-save-as (new menu-item%
                                   [parent file-menu]
                                   [label "Save &As"]
                                   [callback (λ (me ce) (save-as-menu-clicked))]))
    
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

    (define button-panel (new horizontal-panel% [parent vp]))
    
    (define add-button (new button%
                            [label "Add"]
                            [parent button-panel]
                            [callback (λ (me ce) (add-button-clicked))]))
    
    (define verify-button (new button%
                               [label "Verify"]
                               [parent button-panel]
                               [callback (λ (me ce) (verify-button-clicked))]))

    (define (open-menu-clicked)
      (let ([f (get-file)])
        (unless (false? f)
          (begin
            (read-chain-from-file f)
            (set! file-name f)            
            (update-listbox)))))

    (define (save-menu-clicked)      
      (if (null? file-name)
          (save-as-menu-clicked)        
          (save-chain-to-file)))

    (define (save-as-menu-clicked)
      (let ([f (put-file)])
        (unless (false? f)
          (begin
            (set! file-name f)
            (save-chain-to-file)))))
    
    (define (add-button-clicked)
      (let ([the-text (send text-data get-value)])
        (begin
          (send the-chain add-entry the-text)
          (update-listbox))))
    
    (define (verify-button-clicked)
      (let* ([success? (send chain verify)]
             [msgbox-style (if success? (list 'ok 'no-icon) (list 'ok 'caution))]
             [msgbox-text (if success? "Verification successful" "Verification failed")])
        (message-box "Verification result" msgbox-text this msgbox-style)))
    
    (define (update-listbox)
      (let* ([blocks (send chain get-blocks)]
             [block-data (map (λ (b)
                                (send b get-data))
                              blocks)])
        (send listbox set block-data)))

    (define (read-chain-from-file file-name)
      (let* ([f-port (open-input-file file-name)]
             [f-content (read f-port)]
             [the-blocks (deserialize f-content)])
        (send the-chain set-entries the-blocks)))

    (define (save-chain-to-file)
      (let* ([blocks (send the-chain get-blocks)]
             [serialized (serialize blocks)]
             [f-port (open-output-file file-name #:exists 'replace)])
        (begin 
          (write serialized f-port)
          (close-output-port f-port))))))

(provide main-window%)
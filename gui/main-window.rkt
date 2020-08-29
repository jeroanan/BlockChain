#lang racket/gui

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

    ;; Set when changes are made. Unset when the chain is saved.
    (define dirty? false)

    ;; -- Handlers for menu items
    (define (new-menu-clicked)
      (when (confirm-before-close)
        (send chain initialize-chain)
        (update-listbox)
        (set! dirty? false)))
    
    (define (open-menu-clicked)
      (when (confirm-before-close)
        (let ([f (get-file)])
           (unless (false? f)
             (begin
               (read-chain-from-file f)
               (set! file-name f)            
               (update-listbox))))))

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
    
    (define menu (new menu-bar% [parent this]))

    ;; -- File Menu --
    (define file-menu (new menu%
                           [label "&File"]
                           [parent menu]))

    (define (add-file-menu-item label callback)
      (new menu-item%
           [parent file-menu]
           [label label]
           [callback (λ (me ce) (callback))]))

    (add-file-menu-item "&New" new-menu-clicked)
    (add-file-menu-item "&Open" open-menu-clicked)
    (add-file-menu-item "&Save" save-menu-clicked)
    (add-file-menu-item "Save &As" save-as-menu-clicked)
    
    ;; -- Main window body --
    (define hp (new horizontal-panel% [parent this]))

    ;; The list of blocks in the chain
    (define listbox (new list-box%
                         [parent hp]
                         [label ""]
                         [choices (list)]))
    
    (define vp (new vertical-panel% [parent hp]))

    ;; The data to be put into a new block in the chain
    (define text-data (new text-field%
                           [label "Data"]
                           [parent vp]
                           [init-value ""]))
    ;; Button handlers
    (define (add-button-clicked)
      (let ([the-text (send text-data get-value)])
        (begin
          (send the-chain add-entry the-text)
          (update-listbox)
          (set! dirty? true))))
    
    (define (verify-button-clicked)
      (let* ([success? (send chain verify)]
             [msgbox-style (if success? (list 'ok 'no-icon) (list 'ok 'caution))]
             [msgbox-text (if success? "Verification successful" "Verification failed")])
        (message-box "Verification result" msgbox-text this msgbox-style)))
    
    ;; Place for buttons
    (define button-panel (new horizontal-panel% [parent vp]))

    (define (add-button-to-panel label callback)
      (new button%
           [label label]
           [parent button-panel]
           [callback (λ (me ce) (callback))]))

    (add-button-to-panel "Add" add-button-clicked)
    (add-button-to-panel "Verify" verify-button-clicked)
    
    ;; Refresh the list of blocks with the contents fo the chain
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
        (send the-chain set-blocks the-blocks)
        (set! dirty? false)))

    (define (save-chain-to-file)
      (let* ([blocks (send the-chain get-blocks)]
             [serialized (serialize blocks)]
             [f-port (open-output-file file-name #:exists 'replace)])        
        (write serialized f-port)
        (close-output-port f-port)
        (set! dirty? false)))

    ;; If the chain has unsaved changes, confirm before closing
    (define (confirm-before-close)
      (if dirty?
          (if (eq? (message-box "Discard changes?" "Discard changes?" this (list 'yes-no)) 'yes)
              #t
              #f)
          #t))

    (send chain initialize-chain)
    (update-listbox)))

(provide main-window%)
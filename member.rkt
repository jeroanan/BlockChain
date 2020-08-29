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

(define-syntax (public-attribute stx)
  (syntax-case stx ()
    [(_ field-name default-value)
     (with-syntax ([getter-name
                    (datum->syntax #'field-name
                                   (string->symbol (format "get-~a"
                                                           (syntax->datum #'field-name))))]
                   [setter-name
                    (datum->syntax #'field-name
                                   (string->symbol (format "set-~a"
                                                           (syntax->datum #'field-name))))])
       #'(begin
           (field [field-name default-value])
           
           (define/public (getter-name) field-name)

           (define/public (setter-name x) (set! field-name x))))]))

(provide public-attribute)
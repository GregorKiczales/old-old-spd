#lang racket/base


(provide @TAGS
         PRIMITIVE-TYPES
         DD-TEMPLATE-RULES
         NON-TYPE-TEMPLATE-ORIGINS)

(define @TAGS
  '(@assignment @cwl
                @problem
                @htdw @htdd
                @htdf @signature @dd-template-rules @template-origin @template))


(define PRIMITIVE-TYPES
  '(Number Integer Natural String Boolean Image Color Scene
           1String
           KeyEvent MouseEvent))

(define DD-TEMPLATE-RULES
  '(atomic-non-distinct atomic-distinct one-of compound self-ref ref))
#;
(define TEMPLATE-ORIGINS
  '(;<TypeName>      decomposition and possible structural recursion
  
    htdw-main    
    fn-composition   ;use once for composition of 2 or more fns
    try-catch
    2-one-of
    encapsulated
    use-abstract-fn
    genrec
    bin-tree         ;must be used w/ genrec
    arb-tree         ;must be used w/ genrec
    accumulator))

(define NON-TYPE-TEMPLATE-ORIGINS 
  '(htdw-main    
    fn-composition   ;use once for composition of 2 or more fns
    try-catch
    2-one-of
    encapsulated
    use-abstract-fn
    genrec
    bin-tree         ;must be used w/ genrec
    arb-tree         ;must be used w/ genrec
    accumulator))
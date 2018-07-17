# spd

This is a repository containing source code for Systematic Program Design (SPD), also known as CPSC 110: Computation, Programs, and Programming, at the University of British Columbia, Vancouver.

## Index

1. [spd/tags](spd/tags)
2. [Glossary](Glossary)

## spd/tags

spd/tags is a package for allowing the annotation of `.rkt` files with metadata related to HtDD, HtDF, and HtDW. This is a utility which allows the extension of auto-grading capabilities for student submissions and a variety of other tools in use in the course. 

These metadata tags used in SPD are based on metadata annotations found in other utilities, such as
* `Java`: `@Override`, `@Entity`, `@SuppressWarnings`
* `AspectJ`: `@Pointcut`, `@AspectJ`

You can click [this](https://en.wikipedia.org/wiki/Java_annotation) link to read more about annotations, specfically with regards to the Java programming language. 

The following tags are currently in use:

* `(@Problem n)]`
* `(@HtDF fn-name-1 fn-name-2..fn-name-n)`
* `(@HtDD TypeName1 TypeName2..TypenameN)`
* `(@HtDW TypeName)`
* `(@dd-template-rules r1 r2..rN)]`
* `(@template s1 s2..sN)`

### `(@Problem n)`

**Restrictions:** n>=1, first `(@Problem ...)` tag in a file must have arity 1. Other follow in sequence.

Indicates that the contents of the `.rkt` file from one `@Problem` tag to the next immediate one comprise the solutions to problem n. E.g. Contents of file in between `(@Problem n)` and `(@Problem n+1)` are the student's solutions to problem n.

### (@HtDF fn-name-1 fn-name-2...fn-name-n)

**Restrictions:** n>=1, line immediately following this tag should be a function signature. Having multiple function names inside a `@HtDF` tag is permissible starting from the mutual-reference module; this indicates that a group of mutually-recursive functions are grouped within. 

Usage:
```Racket
(@HtDF pluralize)
;; String -> String
```
### (@HtDD TypeName1 TypeName2...TypeNameN)

**Restrictions:** n>=1, type definition must immediately follow the tag, grouping is permissible, like `@HtDF` starting from the module on mutual-reference.

Usage
```
(@HtDD StreetLight)
;; StreetLight is one of:
;; !!!

(@HtDD Student)
(define-struct student(id name major year))
;; Student is (make-student Natural String String Natural[1,5])
;; !!!
```
### (@HtDW TypeName)

This tag indicates that a HtDW design immediately follows it. `TypeName` should be the same as the WorldState type.

### (@dd-template-rules r1 r2...rN)

**Restrictions:** r1, r2, ... , rN may only be one of the following:
* `atomic-non-distinct`
* `atomic-distinct`
* `one-of`
* `compound`
* `self-ref`
* `ref`

This tag **must** proceed the template to which it refers to

`@dd-template-rules` serves as a replacement for the `;; Template rules used:` notation found in earlier offerings of the course. 

Usage:
```Racket

(define (fn-for-player-health ph)
    (cond [(number? ph)(... ph)]
          [else (...)]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic non distinct: Natural
;; - atomic distinct: false

;; the above should now be written as:

(@-dd-template-rules
    one-of                  ; 2 cases
    atomic-non-distinct     ; Natural
    atomic-distinct)        ; false

(define (fn-for-player-health ph)
    (cond [(number? ph)(... ph)]
          [else (...)]))
```


### (@template s1 s2...sN)

**Restrictions:** s1, s2, ... , sN can only be one of the following types:
* `TypeName`: Where we previously said "type", we now put the actual name of the type the template is based on.
* `add-param`: the template has an additional parameter, treated as atomic data
* `htdw-main`: templated as a main function in the HtDW recipe, with a call to big-bang
* `fn-composition`: templated as a composition of calls to two or more helper functions
* `backtracking`: template includes backtracking search
* `2-one-of`: templated based on a cross-product table, with possible case reduction
* `encapsulated`: encapsulation of two or more functions, usually mutually recursive
* `use-abstract-fn`: templated as a call to one or more abstract functions, either built-in or user defined
* `genrec`: template includes generative recursion
* `bin-tree, arb-tree`: requires use of genrec, and indicates that template is a traversal of a generated binary or arbitrary-arity tree.
* `accumulator`: template includes one or more accumulators
* `for-each`: template includes a call to `for-each`

## Glossary

1. Metadata: *"data about data"*. Specfically with regards to SPD, it describes the information about a xSL program, which, is in itself - data.



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

* [(@Problem n)](@Problem-n)
* [(@HtDF fn-name-1 fn-name-2..)](@HtDF-fn-name-1-fn-name-2...)
* [(@HtDD TypeName1 TypeName2..)](@HtDD-TypeName1-TypeName2..)
* [(@HtDW TypeName)](@HtDW-TypeName)
* [(@dd-template-rules r1 r2..)]((@dd-template-rules-r1-r2..))
* [(@template s1 s2..)]((@template-s1-s2..))

### (@Problem n)

**Restrictions:** n>=1, first `(@Problem ...)` tag in a file must have arity 1. Other follow in sequence.

### (@HtDF fn-name-1 fn-name-2...fn-name-n)

### (@HtDD TypeName1 TypeName2...TypeNameN)

### (@HtDW TypeName)

### (@dd-template-rules r1 r2...rN)

### (@template s1 s2...sN)



## Glossary

1. Metadata: *"data about data"*. Specfically with regards to SPD, it describes the information about a xSL program, which, is in itself - data.



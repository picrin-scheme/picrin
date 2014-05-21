Language
========

The language provided by picrin.

Libraries
---------

- `(scheme base)`
- `(scheme write)`
- `(scheme cxr)`
- `(scheme file)`
- `(scheme inexact)`
- `(scheme time)`
- `(scheme process-context)`
- `(scheme load)`
- `(scheme lazy)`
- `(picrin macro)`

  - `define-macro`
  - `gensym`
  - `macroexpand`

  Old-fashioned macro.

  - `make-syntactic-closure`
  - `identifier?`
  - `identifier=?`

  Syntactic closures.

  - `er-macro-transformer`
  - `ir-macro-transformer`

  Explicit renaming macro family.

- `(picrin regexp)`

  - `(regexp? obj)`
  - `(regexp ptrn [flags])`

  Compiles pattern string into a regexp object. A string `flags` may contain any of #\g, #\i, #\m.

  - `(regexp-match re input)`

  Returns two values: a list of match strings, and a list of match indeces.

  - `(regexp-replace re input txt)`
  - `(regexp-split re input)`

- `(picrin control)`

  - `(reset h)`
  - `(shift k)`

  Delimited control operators.

- `(picrin user)`

  When you start the REPL, you are dropped into here.

- `(srfi 1)`

  List manipulation library.

- `(srfi 26)`

  Cut/cute macros.

- `(srfi 95)`

  Sorting and Marging.

The REPL
--------

At the REPL start-up time, some usuful built-in libraries listed below will be automatically imported.

- `(scheme base)`
- `(scheme load)`
- `(scheme process-context)`
- `(scheme write)`
- `(scheme file)`
- `(scheme inexact)`
- `(scheme cxr)`
- `(scheme lazy)`
- `(scheme time)`

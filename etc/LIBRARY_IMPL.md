# How to implement `define-library`?

* define-library can be nested
* expressions inside define-library are compiled and evaluated in order sequentially
* import declarations inside define-library and on the top level are semantically the same
* each define-library creates one syntactic-env
* and the body is evaluated as if it's on the top level
* so each `toplevel definitions`' results are registered to the global table
* but their renamed symbols are known only to who imported the library.

## export table

* import syntax destructively registers renamed symbols taken from export table of the specified library to syntactic env of the library
* export syntax registers correspoindings of original and renamed symbols to export table of the current library
* therefore, we need some kind of `forward declaration` support, because export syntax is usually placed at the beginning of source code.

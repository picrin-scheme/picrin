Language
========

Picrin's core language is the R7RS scheme with some powerful extensions. Please visit http://r7rs.org/ for the information of R7RS's design and underlying thoughts.

The REPL
--------

At the REPL start-up time, some usuful built-in libraries listed below will be automatically imported.

- ``(scheme base)``
- ``(scheme load)``
- ``(scheme process-context)``
- ``(scheme write)``
- ``(scheme file)``
- ``(scheme inexact)``
- ``(scheme cxr)``
- ``(scheme lazy)``
- ``(scheme time)``
- ``(scheme case-lambda)``
- ``(scheme read)``
- ``(scheme eval)``

Compliance with R7RS
---------------------

================================================ ========== ==========================================================================================================================
section                                          status     comments
================================================ ========== ==========================================================================================================================
2.2 Whitespace and comments                      yes
2.3 Other notations                              incomplete #e #i #b #o #d #x
2.4 Datum labels                                 yes
3.1 Variables, syntactic keywords, and regions
3.2 Disjointness of types                        yes
3.3 External representations
3.4 Storage model                                yes
3.5 Proper tail recursion                        yes        As the report specifies, ``apply``, ``call/cc``, and ``call-with-values`` perform tail calls
4.1.1 Variable references                        yes
4.1.2 Literal expressions                        yes
4.1.3 Procedure calls                            yes        In picrin ``()`` is self-evaluating
4.1.4 Procedures                                 yes
4.1.5 Conditionals                               yes        In picrin ``(if #f #f)`` returns ``#f``
4.1.6 Assignments                                yes
4.1.7 Inclusion                                  incomplete ``include-ci``
4.2.1 Conditionals                               yes
4.2.2 Binding constructs                         yes
4.2.3 Sequencing                                 yes
4.2.4 Iteration                                  yes
4.2.5 Delayed evaluation                         yes
4.2.6 Dynamic bindings                           yes
4.2.7 Exception handling                         yes        ``guard`` syntax.
4.2.8 Quasiquotation                             yes        can be safely nested. TODO: multiple argument for unquote
4.2.9 Case-lambda                                yes
4.3.1 Bindings constructs for syntactic keywords yes [#]_
4.3.2 Pattern language                           yes        ``syntax-rules``
4.3.3 Signaling errors in macro transformers     yes
5.1 Programs                                     yes
5.2 Import declarations                          yes
5.3.1 Top level definitions                      yes
5.3.2 Internal definitions                       yes
5.3.3 Multiple-value definitions                 yes
5.4 Syntax definitions                           yes
5.5 Recored-type definitions                     yes
5.6.1 Library Syntax                             yes        In picrin, libraries can be reopend and can be nested.
5.6.2 Library example                            N/A
5.7 The REPL                                     yes
6.1 Equivalence predicates                       yes
6.2.1 Numerical types                            yes        picrin has only two types of internal representation of numbers: fixnum and double float. It still comforms the R7RS spec.
6.2.2 Exactness                                  yes
6.2.3 Implementation restrictions                yes
6.2.4 Implementation extensions                  yes
6.2.5 Syntax of numerical constants              yes
6.2.6 Numerical operations                       yes        ``denominator``, ``numerator``, and ``rationalize`` are not supported for now. Also, picrin does not provide complex library procedures.
6.2.7 Numerical input and output                 yes
6.3 Booleans                                     yes
6.4 Pairs and lists                              yes        ``list?`` is safe for using against circular list.
6.5 Symbols                                      yes
6.6 Characters                                   yes
6.7 Strings                                      yes
6.8 Vectors                                      yes
6.9 Bytevectors                                  yes
6.10  Control features                           yes
6.11 Exceptions                                  yes
6.12 Environments and evaluation                 yes
6.13.1 Ports                                     yes
6.13.2 Input                                     yes
6.13.3 Output                                    yes
6.14 System interface                            yes
================================================ ========== ==========================================================================================================================

.. [#] Picrin provides hygienic macros in addition to so-called legacy macro (``define-macro``), such as syntactic closure, explicit renaming macro, and implicit renaming macro.

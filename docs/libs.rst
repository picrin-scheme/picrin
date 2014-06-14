Libraries
=========

Picrin's all built-in libraries are described below.

Scheme standard libraries
-------------------------

- (scheme write)
- (scheme cxr)
- (scheme file)
- (scheme inexact)
- (scheme time)
- (scheme process-context)
- (scheme load)
- (scheme lazy)

SRFI libraries
--------------

- (srfi 1)

  List manipulation library.

- (srfi 26)

  Cut/cute macros.

- (srfi 95)

  Sorting and Marging.

(picrin macro)
--------------

Utility functions and syntaces for macro definition.

- define-macro
- gensym
- macroexpand expr

Old-fashioned macro.

- make-syntactic-closure
- identifier?
- identifier=?

Syntactic closures.

- er-macro-transformer
- ir-macro-transformer

Explicit renaming macro family.

(picrin regexp)
---------------

- *(regexp ptrn [flags])*

  Compiles pattern string into a regexp object. A string flags may contain any of #\g, #\i, #\m.

- *(regexp? obj)*

  Judges if obj is a regexp object or not.

- *(regexp-match re input)*

  Returns two values: a list of match strings, and a list of match indeces.

- *(regexp-replace re input txt)*
- *(regexp-split re input)*


(picrin control)
----------------

Delimited control operators.

- *(reset h)*
- *(shift k)*


(picrin dictionary)
-------------------

Symbol to Object table. Internally it is implemented on hash-table.

Note that dictionary is not a weak map; if you are going to make a highly memory-consuming program with dictionaries, you should know that dictionaries keep their bound objects and never let them free until you explicitly deletes bindings.

- *(dictionary)*

  Returns a newly allocated empty dictionary. In the future, it is planned to extend this function to take optional arguments for initial key/values.

- *(dictionary? obj)*

  Returns #t if obj is a dictionary.

- *(dictionary-ref dict key)*

  Look up dictionary dict for a value associated with symbol key. If no object is associated with key, it will raise an error.

- *(dictionary-set! dict key obj)*

  If there is no value already associated with key, this function newly creates a binding of key with obj. Otherwise, updates the existing binding with given obj.

- *(dictionary-delete dict key)*

  Deletes the binding associated with key from dict. If no binding on dict is associated with key, an error will be raised.

- *(dictionary-size dict)*

  Returns the number of registered elements in dict.


(picrin user)
-------------

When you start the REPL, you are dropped into here.


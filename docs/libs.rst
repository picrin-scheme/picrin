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

  List library.

- (srfi 8)

  ``receive`` macro.

- (srfi 26)

  Cut/cute macros.

- (srfi 43)

  Vector library.

- (srfi 60)

  Bitwise operations.

- (srfi 95)

  Sorting and Marging.

(picrin macro)
--------------

Utility functions and syntaces for macro definition.

- define-macro
- gensym
- macroexpand

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

- **(regexp ptrn [flags])**

  Compiles pattern string into a regexp object. A string flags may contain any of #\g, #\i, #\m.

- **(regexp? obj)**

  Judges if obj is a regexp object or not.

- **(regexp-match re input)**

  Returns two values: a list of match strings, and a list of match indeces.

- **(regexp-replace re input txt)**
- **(regexp-split re input)**


(picrin control)
----------------

Delimited control operators.

- **(reset h)**
- **(shift k)**

(picrin control list)
---------------------

Monadic list operators.

The triple of for/in/yield enables you to write a list operation in a very easy and simple code. One of the best examples is list composition::

  (for (let ((a (in '(1 2 3)))
             (b (in '(2 3 4))))
         (yield (+ a b))))

  ;=> (5 6 7 6 7 8 7 8 9)

All monadic operations are done in *for* macro. In this example, *in* operators choose an element from the given lists, a and b are bound here, then *yielding* the sum of them. Because a and b are values moving around in the list elements, the expression (+ a b) can become every possible result. *yield* operator is a operator that gathers the possibilities into a list, so *for* macro returns a list of 3 * 3 results in total. Since expression inside *for* macro is a normal expression, you can write everything that you can write elsewhere. The code below has perfectly the same effect to above one::

  (for (yield (+ (in '(1 2 3))
                 (in '(4 5 6)))))

The second best exmaple is filtering. In the next case, we show that you can do something depending on the condition of chosen elements::

  (for (let ((x (in (iota 10))))
         (if (even? x)
             (yield x)
             (null))))

  ;=> (0 2 4 6 8)

This expression is equivalent to ``(filter even? (iota 10))`` but it is more procedual and non-magical.

- **(for expr)**

  [Macro] Executes expr in a list monad context.

- **(in list)**

  Choose a value from list. *in* function must only appear in *for* macro. The delimited continuation from the position of *in* function to the outside *for* macro is executed for each element in list. If list contains no values, that is ``(in '())``, the continuation is discarded.

- **(yield value)**

  Yields value from the monad context. The result of *for* will be a list of yielded values.

- **(null . value)**

  Returns ``()`` whatever value is given. The identity element of list composition. This operator corresponds to Haskell's fail method of Monad class.


(picrin array)
--------------

Resizable random-access list.

Technically, picrin's array is implemented as a ring-buffer, effective double-ended queue data structure (deque) that can operate pushing and poping from both of front and back in constant time. In addition to the deque interface, array provides standard sequence interface similar to functions specified by R7RS.

- **(make-array [capacity])**

  Returns a newly allocated array object. If capacity is given, internal data chunk of the array object will be initialized by capacity size.

- **(array . objs)**

  Returns an array initialized with objs.

- **(array? . obj)**

  Returns #t if obj is an array.

- **(array-length ary)**

  Returns the length of ary.

- **(array-ref ary i)**

  Like ``list-ref``, return the object pointed by the index i.

- **(array-set! ary i obj)**

  Like ``list-set!``, substitutes the object pointed by the index i with given obj.

- **(array-push! ary obj)**

  Adds obj to the end of ary.

- **(array-pop! ary)**

  Removes the last element of ary, and returns it.

- **(array-unshift! ary obj)**

  Adds obj to the front of ary.

- **(array-shift! ary)**

  Removes the first element of ary, and returns it.

- **(array-map proc ary)**

  Performs mapping operation on ary.

- **(array-for-each proc ary)**

  Performs mapping operation on ary, but discards the result.

- **(array->list ary)**

  Converts ary into list.

- **(list->array list)**

  Converts list into array.


(picrin dictionary)
-------------------

Symbol to Object table. Internally it is implemented on hash-table.

Note that dictionary is not a weak map; if you are going to make a highly memory-consuming program with dictionaries, you should know that dictionaries keep their bound objects and never let them free until you explicitly deletes bindings.

- **(dictionary . plist)**

  Returns a newly allocated empty dictionary. The dictionary is initialized with the content of plist.

- **(dictionary? obj)**

  Returns #t if obj is a dictionary.

- **(dictionary-ref dict key)**

  Look up dictionary dict for a value associated with symbol key. If no object is associated with key, it will raise an error.

- **(dictionary-set! dict key obj)**

  If there is no value already associated with key, this function newly creates a binding of key with obj. Otherwise, updates the existing binding with given obj.

- **(dictionary-delete dict key)**

  Deletes the binding associated with key from dict. If no binding on dict is associated with key, an error will be raised.

- **(dictionary-size dict)**

  Returns the number of registered elements in dict.

- **(dicitonary-map proc dict)**

  Perform mapping action onto dictionary object. ``proc`` is called by a sequence ``(proc key val)``.

- **(dictionary-for-each proc dict)**

  Similar to ``dictionary-map``, but discards the result.

- **(dictionary->plist dict)**
- **(plist->dictionary plist)**
- **(dictionary->alist dict)**
- **(alist->dictionary alist)**

  Conversion between dictionary and alist/plist.


(picrin pretty-print)
---------------------

Pretty-printer.

- **(pretty-print obj)**

  Prints obj with human-readable indention to current-output-port.


(picrin user)
-------------

When you start the REPL, you are dropped into here.


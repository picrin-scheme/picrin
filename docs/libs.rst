Standard Libraries
==================

Picrin's all built-in libraries are described below.

(picrin macro)
--------------

Utility functions and syntaces for macro definition.

- define-macro
- gensym
- ungensym
- macroexpand
- macroexpand-1

Old-fashioned macro.

- identifier?
- identifier=?

- make-syntactic-closure
- close-syntax
- capture-syntactic-environment

- sc-macro-transformer
- rsc-macro-transformer

Syntactic closures.

- er-macro-transformer
- ir-macro-transformer
- strip-syntax

Explicit renaming macro family.

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

Symbol-to-object hash table.

- **(make-dictionary)**

  Returns a newly allocated empty dictionary.

- **(dictionary . plist)**

  Returns a dictionary initialized with the content of plist.

- **(dictionary? obj)**

  Returns #t if obj is a dictionary.

- **(dictionary-ref dict key)**

  Look up dictionary dict for a value associated with key. If dict has a slot for key `key`, a pair containing the key object and the associated value is returned. Otherwise `#f` is returned.

- **(dictionary-set! dict key obj)**

  If there is no value already associated with key, this function newly creates a binding of key with obj. Otherwise, updates the existing binding with given obj.

  If obj is `#undefined`, this procedure behaves like a deleter: it will remove the key/value slot with the name `key` from the dictionary. When no slot is associated with `key`, it will do nothing.

- **(dictionary-size dict)**

  Returns the number of registered elements in dict.

- **(dicitonary-map proc dict)**

  Perform mapping action onto dictionary object. ``proc`` is called by a sequence ``(proc key1 key2 ...)``.

- **(dictionary-for-each proc dict)**

  Similar to ``dictionary-map``, but discards the result.

- **(dictionary->plist dict)**
- **(plist->dictionary plist)**
- **(dictionary->alist dict)**
- **(alist->dictionary alist)**

  Conversion between dictionary and alist/plist.


(picrin user)
-------------

When you start the REPL, you are dropped into here.


Contrib Libraries (a.k.a nitros)
================================

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

(picrin control)
----------------

Delimited control operators.

- **(reset h)**
- **(shift k)**

(picrin pretty-print)
---------------------

Pretty-printer.

- **(pretty-print obj)**

  Prints obj with human-readable indention to current-output-port.


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


SRFI libraries
--------------

- `(srfi 1)
  <http://srfi.schemers.org/srfi-1/>`_

  List library.

- `(srfi 8)
  <http://srfi.schemers.org/srfi-8/>`_

  ``receive`` macro.

- `(srfi 17)
  <http://srfi.schemers.org/srfi-17/>`_

  Generalized set!

- `(srfi 26)
  <http://srfi.schemers.org/srfi-26/>`_

  Cut/cute macros.

- `(srfi 43)
  <http://srfi.schemers.org/srfi-43/>`_

  Vector library.

- `(srfi 60)
  <http://srfi.schemers.org/srfi-60/>`_

  Bitwise operations.

- `(srfi 95)
  <http://srfi.schemers.org/srfi-95/>`_

  Sorting and Marging.

- `(srfi 111)
  <http://srfi.schemers.org/srfi-111/>`_

  Boxes

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



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



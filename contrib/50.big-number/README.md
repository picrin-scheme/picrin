`big-number` is a library for big integer operations.

## Description
This library provides the following functions:

- `make-bigint` : Creates a bigint. It accepts integers and strings.
- `make-bigint-radix` : Creates a bigint with a string and an integer (radix).
- `bigint-add`, `bigint-sub`, `bigint-mul`, `bigint-div`, `bigint-rem` : Operations. Quotients are truncated toward 0. The signs of remainders are equal to those of dividends.
- `bigint-add!`, `bigint-sub!`, `bigint-mul!` : In-place operations. The results are written to the first arguments.
- `bigint->number` : Converts the argument to a double.
- `bigint->string` : Converts the argument to a string. The second argument (radix) is optional.
- `bigint-underlying` : Returns the underlying vector of the given bigint.

Every pure operation other than `make-bigint` accepts either integers or bigints. The first argument of in-place operations must be a bigint.
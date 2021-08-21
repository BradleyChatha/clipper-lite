# Overview

This is a bare minimum implementation of the Clipper programming language needed to create the sieve of eratosthenes.

Andy better appreciate this >:C

## Modifications

Certain modifications to the language have been made:

* The language transpiles into D, which has certain implications since Clipper is actually a dynamic language.

* The `Array` function assumes that the data type is of `bool`, this is unavoidable due to D being statically typed and Clipper being dynamically typed.

* The `TimeHnsecs` function is exclusive to this implementation, simply because Clipper lacks a function that returns a numerical time stamp required for timing the loop.

* Boolean expressions must be explicit, e.g. `IF cond == .T.` instead of `IF cond`

* Arithmetic expressions are very limited, things like `1 + 2 + 3` (probably) don't work, so must be split up. This is to make the transpiler and the parser simpler.
  * A special case is made for boolean expressions, where a singular `.AND.` or `.OR.` clause is supported since it was easy enough to tack on.

* All numbers are assumed to be `long`, that is, signed 64-bit numbers.

## Implemented

* `FUNCTION` and `PROCEDURE`

* `FOR` in the form of `FOR assignment TO expression STEP expression`

* `DO WHILE`

* `IF` and `ELSE`, but not `ELSEIF` as it complicated the transpiler massively.
  * Technically `ELSEIF` will parse correctly, but it will fail during transpiling.

* `Len()`, `Sqrt()`, `Floor()`, `Array()`, and the special `TimeHnsecs()` functions.

* Assignment in `=` and `:=` forms.

* `LOCAL` assignments which act as the declaration of a variable, I didn't implement `DECLARE` since it seems redundant for transpiling purposes.

* Array indexing and assignment to an array index

* `?` and `??`

* Function calling in the form of `CALL func WITH arg1, arg2, ...` as for some reason the form `func(arg1, ...)` breaks the parser massively
  * I couldn't tell you how relieved I was to see that Clipper had the former syntax :D
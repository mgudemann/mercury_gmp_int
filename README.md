# mercury_gmp_int
Multi precision integers for Mercury based on GMP

This provides a binding to a subset of the functions from the `mpz` data-type of
[GMP](http://gmplib.org).

## Usage

To use, you'll need a compiled version of GMP, both shared and static libraries
should work.

1. compile `gmp_int.c` which interfaces to Mercury's version of the
   Boehm-Demers-Weiser garbage collector. This will provide a linkable object
   file called `gmp_int.o`.

   ```
   $ gcc -c gmp_int.c
   ```

2. call the _impure_ GMP `gmp_initialize/0` predicate in your program,
   preferable as very first predicate in `main/1`. This sets up GMP with the
   Mercury GC interface.

   ```
   :- impure pred main(io::di, io::uo) is det.
      ...
   main(!IO) :-
       impure gmp_initialize,
          ...
   ```

3. compile your program and link to the library and `gmp_int.o`

   ```
   $ mmc --link-object libgmp.{a,so} --link-object gmp_int.o $OPTIONS $PROGRAM_NAME
   ```

## Remarks

This only works in the C grades (hlc / asm_fast) with conservative garbage
collection.

Currently, there is basically no error checking.

# mercury_gmp_int
Multi-precision integers for Mercury based on GMP

This provides a binding to a subset of the functions from the `mpz` data-type of
[GMP](http://gmplib.org).

## Usage

To use, you'll need a compiled version of GMP, both shared and static libraries
should work.

1. ~~compile `gmp_int.c` which interfaces to Mercury's version of the
   Boehm-Demers-Weiser garbage collector. This will provide a linkable object
   file called `gmp_int.o`.~~

   ```
   $ gcc -c gmp_int.c
   ```

   **Update** This is no longer required, the code and the prototype
     declarations are directly in `gmp_int.m` now.

2. ~~call the _impure_ GMP `gmp_initialize/0` predicate in your program,
   preferable as very first predicate in `main/1`. This sets up GMP with the
   Mercury GC interface.~~

   ```
   :- impure pred main(io::di, io::uo) is det.
      ...
   main(!IO) :-
       impure gmp_initialize,
          ...
   ```

   **Update** This is unnecessary, as Mercury allows to declare an explicit
   module initialise call (see
   [language reference](http://mercurylang.org/information/doc-release/mercury_ref/Module-initialisation.html#Module-initialisation))

3. compile your program and link to the library:

   ```
   $ mmc -l gmp $OPTIONS $PROGRAM_NAME
   ```

   or as follows if you want to use static linking (slightly faster):

   ```
   $ mmc --link-object $FULL_PATH_TO_STATIC_GMP_LIB $OPTIONS $PROGRAM_NAME
   ```

## Benchmark

For heavy number crunching code on bignums, first benchmarks show that
Mercury-GMP is 2x faster than SWI and 15% faster than YAP (both using
GMP).

For a Miller-Rabin test, Mercury-GMP was more than two orders of magnitude
faster than using `integer.m` from the standard library. It is between 2 to 6
times faster than Mercury-MP which is based on [libtommath](http://libtom.net).

## Remarks

This only works in the C grades (hlc / asm_fast) with conservative garbage
collection.

Currently, there is basically no error checking.

:- module gmp_int.

:- interface.

:- import_module int.

:- type gmp_int.


:- pred equal(gmp_int::in, gmp_int::in) is semidet.
:- pred cmp(comparison_result::uo, gmp_int::in, gmp_int::in) is det.

:- func gmp_int(int) = gmp_int.

:- func gmp_int + gmp_int = gmp_int.
:- func gmp_int - gmp_int = gmp_int.
:- func gmp_int * gmp_int = gmp_int.
:- func gmp_int // gmp_int = gmp_int.
:- func 'rem'(gmp_int, gmp_int) = gmp_int.
:- func 'div'(gmp_int, gmp_int) = gmp_int.
:- func 'mod'(gmp_int, gmp_int) = gmp_int.

:- pred divide_with_rem(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out) is det.
:- pred divide_with_mod(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out) is det.

:- func gmp_int \/ gmp_int = gmp_int.
:- func gmp_int /\ gmp_int = gmp_int.
:- func 'xor'(gmp_int, gmp_int) = gmp_int.
:- func \ gmp_int = gmp_int.

:- pred '<'(gmp_int::in, gmp_int::in) is semidet.
:- pred '>'(gmp_int::in, gmp_int::in) is semidet.
:- pred '=<'(gmp_int::in, gmp_int::in) is semidet.
:- pred '>='(gmp_int::in, gmp_int::in) is semidet.

:- pred is_even(gmp_int::in) is semidet.
:- pred is_odd(gmp_int::in) is semidet.
:- pred is_negative(gmp_int::in) is semidet.
:- pred is_zero(gmp_int::in) is semidet.
:- pred is_positive(gmp_int::in) is semidet.

:- func gcd(gmp_int, gmp_int) = gmp_int.
:- func lcm(gmp_int, gmp_int) = gmp_int.
:- func abs(gmp_int) = gmp_int.
:- func pow(gmp_int, gmp_int) = gmp_int.

    % powm(Base, Exp, M) = Base^Exp mod M
    %
:- func powm(gmp_int, gmp_int, gmp_int) = gmp_int.

:- func det_from_string(string) = gmp_int.
:- func det_from_base_string(string, int) = gmp_int.
:- func to_string(gmp_int) = string.
:- func to_base_string(gmp_int, int) = string.

:- func negative_one = gmp_int.
:- func zero = gmp_int.
:- func one = gmp_int.
:- func two = gmp_int.
:- func ten = gmp_int.

:- implementation.

:- import_module exception, math, require.

   % Type declaration for foreign type gmp_int*.
   %
:- pragma foreign_type("C", gmp_int, "mpz_t *")
    where equality is equal, comparison is cmp.
:- pragma foreign_decl("C",
"\
#include <stdio.h>\n\
#include <gmp.h>\n\
").

:- initialise gmp_initialize/0.
:- impure pred gmp_initialize is det.

:- pragma foreign_code("C",
"
#include <stdlib.h>

void *
gmp_int_alloc_function(size_t size)
{
  return MR_GC_malloc(size);
}

void *
gmp_int_realloc_function(void* ptr, size_t size, size_t new_size)
{
  return MR_GC_realloc(ptr, new_size);
}

void
gmp_int_free_function(void* ptr, size_t size)
{
  GC_free(ptr);
}
").

:- pragma foreign_decl("C", local,
"
void* gmp_int_alloc_function(size_t);
void* gmp_int_realloc_function(void*, size_t, size_t);
void  gmp_int_free_function(void*, size_t);

static mpz_t constant_negative_one;
static mpz_t constant_zero;
static mpz_t constant_one;
static mpz_t constant_two;
static mpz_t constant_ten;
").

:- pragma foreign_proc("C",
                      gmp_initialize,
                      [will_not_call_mercury, thread_safe],
"
  mp_set_memory_functions(&gmp_int_alloc_function,
                          &gmp_int_realloc_function,
                          &gmp_int_free_function);
  mpz_init_set_si(constant_negative_one, -1);
  mpz_init_set_si(constant_zero, 0);
  mpz_init_set_si(constant_one, 1);
  mpz_init_set_si(constant_two, 2);
  mpz_init_set_si(constant_ten, 10);
").

:- pragma foreign_proc("C",
                      gmp_int(Value::in) = (Gmp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Gmp_Int = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init_set_si(*Gmp_Int, Value);
").

to_string(Value) = to_base_string(Value, 10).

:- pragma foreign_proc("C",
                      to_base_string(A::in, R::in) = (S::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  S = mpz_get_str(NULL, R, *A);
").

det_from_string(Value) = det_from_base_string(Value, 10).

det_from_base_string(Value, Base) = Res :-
    ( ( Base = 10; Base = 16) ->
        gmp_from_base_string(Value, Base, Res)
    ;
        error("could not convert from string, base must be in {10, 16}")
    ).

:- pred gmp_from_base_string(string::in, int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_from_base_string(S::in, Base::in, A::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  A = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*A);
  if (Base == 10)
    gmp_sscanf(S, \"%Zd\", *A);
  else
    gmp_sscanf(S, \"%Zx\", *A);
").

equal(A, B) :- cmp((=), A, B).

:- pragma foreign_export_enum("C", comparison_result/0, [],
                             [
                              (<) - "MR_GMP_LT",
                              (=) - "MR_GMP_EQ",
                              (>) - "MR_GMP_GT"
                             ]).

:- pragma foreign_proc("C",
                      cmp(Result::uo, A::in, B::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  int res;
  res = mpz_cmp(*A, *B);
  switch(res) {
  case -1:
    Result = MR_GMP_LT;
    break;
  case 0:
    Result = MR_GMP_EQ;
    break;
  case 1:
    Result = MR_GMP_GT;
    break;
  }
").

:- pragma foreign_proc("C",
                      +(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_add(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      -(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_sub(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      *(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_mul(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      //(A::in, B::in) = (Q::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_tdiv_q(*Q, *A, *B);
").

:- pragma foreign_proc("C",
                      rem(A::in, B::in) = (Q::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_tdiv_r(*Q, *A, *B);
").

:- pragma foreign_proc("C",
                      divide_with_rem(A::in, B::in, Q::out, R::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Q = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  R  = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Q);
  mpz_init(*R);
  mpz_tdiv_qr(*Q, *R, *A, *B);
").

:- pragma foreign_proc("C",
                      div(A::in, B::in) = (D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_fdiv_q(*D, *A, *B);
").

:- pragma foreign_proc("C",
                      mod(A::in, B::in) = (D::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_fdiv_r(*D, *A, *B);
").

:- pragma foreign_proc("C",
                      divide_with_mod(A::in, B::in, D::out, M::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  D = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  M = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*D);
  mpz_init(*M);
  mpz_fdiv_qr(*D, *M, *A, *B);
").

:- pragma foreign_proc("C",
                      /\(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_and(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      \/(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_ior(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      xor(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_xor(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      \(A::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_com(*C, *A);
").

A < B :- cmp((<), A, B).
A > B :- cmp((>), A, B).
A =< B :- \+ cmp((>), A, B).
A >= B :- \+ cmp((<), A, B).

:- pragma foreign_proc("C",
                      is_even(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mpz_even_p(*A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_odd(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = mpz_odd_p(*A) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_negative(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == -1) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_zero(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == 0) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      is_positive(A::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  SUCCESS_INDICATOR = (mpz_sgn(*A) == 1) ? MR_TRUE : MR_FALSE;
").

:- pragma foreign_proc("C",
                      gcd(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_gcd(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      lcm(A::in, B::in) = (C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  C = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*C);
  mpz_lcm(*C, *A, *B);
").

:- pragma foreign_proc("C",
                      abs(A::in) = (B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  B = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*B);
  mpz_abs(*B, *A);
").

pow(A, N) = Res :-
    ( is_negative(N) ->
        throw(math.domain_error("gmp_int.pow: cannot handle negative exponent"))
    ;
        Res = pow2(A, N)
    ).

:- func pow2(gmp_int, gmp_int) = gmp_int.
pow2(A, N) = Res :-
    ( is_zero(N) ->
        Res = one
    ; is_even(N) ->
        SQ = pow2(A, N // two),
        Res = SQ * SQ
    ;
        Res = A * pow2(A, N - one)
    ).

:- pragma foreign_proc("C",
                      powm(Base::in, Exp::in, Mod::in) = (Res::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*Res);
  mpz_powm(*Res, *Base, *Exp, *Mod);
").

:- pragma foreign_proc("C",
    negative_one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &constant_negative_one;
"
).

:- pragma foreign_proc("C",
    zero = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &constant_zero;
"
).

:- pragma foreign_proc("C",
    one = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &constant_one;
"
).

:- pragma foreign_proc("C",
    two = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &constant_two;
"
).

:- pragma foreign_proc("C",
    ten = (Res::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
  Res = &constant_ten;
"
).

:- end_module gmp_int.

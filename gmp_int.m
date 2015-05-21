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

gmp_int(Value) = Res :- gmp_init(Value, Res).

:- pred gmp_init(int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_init(Value::in, Gmp_Int::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *new_val;
  new_val = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init_set_si(*new_val, Value);
  Gmp_Int = new_val;
").

to_string(Value) = to_base_string(Value, 10).

to_base_string(Value, Base) = Res :-
    gmp_to_base_string(Value, Base, Res).

:- pred gmp_to_base_string(gmp_int::in, int::in, string::out) is det.
:- pragma foreign_proc("C",
                      gmp_to_base_string(A::in, R::in, S::out),
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

cmp(CMP, A, B) :-
    gmp_cmp(Result, A, B),
    ( Result < 0 ->
        CMP = (<)
    ; Result = 0 ->
        CMP = (=)
    ;
        CMP = (>)
    ).

:- pred gmp_cmp(int::out, gmp_int::in, gmp_int::in) is det.
:- pragma foreign_proc("C",
                      gmp_cmp(Result::out, A::in, B::in),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  Result = mpz_cmp(*A, *B);
").

A + B = C :- gmp_add(A, B, C).

:- pred gmp_add(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_add(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *new_val;
  new_val = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*new_val);
  mpz_add(*new_val, *A, *B);
  C = new_val;
").

A - B = C :- gmp_sub(A, B, C).

:- pred gmp_sub(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_sub(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *new_val;
  new_val = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*new_val);
  mpz_sub(*new_val, *A, *B);
  C = new_val;
").

A * B = C :- gmp_mul(A, B, C).

:- pred gmp_mul(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_mul(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *new_val;
  new_val = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*new_val);
  mpz_mul(*new_val, *A, *B);
  C = new_val;
").

A // B = C :- gmp_tdiv(A, B, C, _).
A rem B = C :- gmp_tdiv(A, B, _, C).
divide_with_rem(A, B, Quot, Rem) :- gmp_tdiv(A, B, Quot, Rem).

:- pred gmp_tdiv(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_tdiv(A::in, B::in, Q::out, R::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *quot, *rem;
  quot = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  rem  = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*quot);
  mpz_init(*rem);
  mpz_tdiv_qr(*quot, *rem, *A, *B);
  Q = quot;
  R = rem;
").

A div B = C :- gmp_fdiv(A, B, C, _).
A mod B = C :- gmp_fdiv(A, B, _, C).
divide_with_mod(A, B, Div, Mod) :- gmp_fdiv(A, B, Div, Mod).

:- pred gmp_fdiv(gmp_int::in, gmp_int::in, gmp_int::out, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_fdiv(A::in, B::in, D::out, M::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *div, *mod;
  div = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mod = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*div);
  mpz_init(*mod);
  mpz_fdiv_qr(*div, *mod, *A, *B);
  D = div;
  M = mod;
").

A /\ B = Res :- gmp_and(A, B, Res).

:- pred gmp_and(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_and(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_and(*result, *A, *B);
  C = result;
").

A \/ B = Res :- gmp_or(A, B, Res).

:- pred gmp_or(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_or(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_ior(*result, *A, *B);
  C = result;
").

xor(A, B) = Res :- gmp_xor(A, B, Res).

:- pred gmp_xor(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_xor(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_xor(*result, *A, *B);
  C = result;
").

\ A = Res :- gmp_com(A, Res).

:- pred gmp_com(gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_com(A::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_com(*result, *A);
  C = result;
").

A < B :- cmp((<), A, B).
A > B :- cmp((>), A, B).
A =< B :-
    cmp(CMP, A, B),
    ( CMP = (=); CMP = (<)).
A >= B :-
    cmp(CMP, A, B),
    ( CMP = (=); CMP = (>)).

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

is_negative(A) :- A < zero.

is_zero(A) :- A = zero.

gcd(A, B) = GCD :- gmp_gcd(A, B, GCD).

:- pred gmp_gcd(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_gcd(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_gcd(*result, *A, *B);
  C = result;
").

lcm(A, B) = GCD :- gmp_lcm(A, B, GCD).

:- pred gmp_lcm(gmp_int::in, gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_lcm(A::in, B::in, C::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_lcm(*result, *A, *B);
  C = result;
").

abs(A) = B :- gmp_abs(A, B).

:- pred gmp_abs(gmp_int::in, gmp_int::out) is det.
:- pragma foreign_proc("C",
                      gmp_abs(A::in, B::out),
                      [will_not_call_mercury, promise_pure, thread_safe],
"
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_abs(*result, *A);
  B = result;
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
  mpz_t *result;
  result = MR_GC_NEW_ATTRIB(mpz_t, MR_ALLOC_ID);
  mpz_init(*result);
  mpz_powm(*result, *Base, *Exp, *Mod);
  Res = result;
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

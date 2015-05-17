:- module gmp_int_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module gmp_int, list, string.

main(!IO) :-
    A = gmp_int(-10),
    B = gmp_int(3),
    C = A * B,
    BinaryX = det_from_base_string("deadbeef", 16),
    BinaryY = det_from_base_string("c0ffee", 16),
    BinaryZ_AND = BinaryX /\ BinaryY,
    BinaryZ_OR  = BinaryX \/ BinaryY,
    BinaryZ_XOR = xor(BinaryX, BinaryY),
    BSX = to_base_string(BinaryX, 16),
    BSY = to_base_string(BinaryY, 16),
    BSZ_AND = to_base_string(BinaryZ_AND, 16),
    BSZ_OR = to_base_string(BinaryZ_OR, 16),
    BSZ_XOR = to_base_string(BinaryZ_XOR, 16),
    BinaryZ_COMPL = \ BinaryX,
    BSZ_COMPL = to_base_string(BinaryZ_COMPL, 16),
    io.format("bitwise ops of\n%s\n%s\n/\\:  %s\n\\/:  %s\nXOR: %s\n\\ %s:\n  %s\n",
              [s(BSX), s(BSY), s(BSZ_AND), s(BSZ_OR), s(BSZ_XOR),
               s(BSX), s(BSZ_COMPL)], !IO),
    divide_with_rem(A, B, Quot, Rem),
    Large  = det_from_string("123456789123456789123456789"),
    Larger = det_from_string("123456789123456789123456789123456789"),
    QResult = Larger // Large,
    RResult = Larger rem Large,
    divide_with_rem(Larger, Large, QResult0, RResult0),
    io.format("Large: %s // %s = (%s, %s)\n",
              [s(to_string(Larger)), s(to_string(Large)),
               s(to_string(QResult)), s(to_string(RResult))],
              !IO),
    io.format("divide_with_rem(%s::in, %s::in, %s::out, %s::out)\n",
              [s(to_string(Larger)), s(to_string(Large)),
               s(to_string(QResult0)), s(to_string(RResult0))],
              !IO),
    AS = to_string(A),
    BS = to_string(B),
    CS = to_string(C),
    QuotS = to_string(Quot),
    RemS = to_string(Rem),
    io.format("%s * %s = %s\n%s // %s = (%s, %s)\ncompare(%s, %s) = ",
             [s(AS), s(BS), s(CS),
              s(AS), s(BS), s(QuotS), s(RemS),
              s(AS), s(BS)], !IO),
    ( A < B ->
        io.print("<", !IO)
    ;
        io.print(">=", !IO)
    ),
    io.nl(!IO)
    .
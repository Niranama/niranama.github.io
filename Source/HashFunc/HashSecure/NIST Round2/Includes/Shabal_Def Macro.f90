#define DECODE_BLOCK(MBytes, Offset, MWords)    CALL DecodeLE(MBytes, Offset, MWords)
#define INPUT_BLOCK_ADD(B, M)                   B = B + M
#define INPUT_BLOCK_SUB(C, M)                   C = C - M
#define SWAP_BC(B, C)                           SWAP_ARRAY(B, C, 0, 15)
#define IEOR_3(A, B, C)                         IEOR(IEOR(A, B), C)
#define IEOR_4(A, B, C, D)                      IEOR(IEOR_3(A, B, C), D)
#define XOR_W(A, WLo, WHi) \
    A(0) = IEOR(A(0), WLo); \
    A(1) = IEOR(A(1), WHi);
#define INCR_W(WLo, WHi) \
    WLo = WLo + 1; \
    IF (WLo == 0) WHi = WHi + 1;
#define PERM_ELT_C(A0, A1, B0, B1, B2, B3, C, M) \
    A0 = IEOR_4((IEOR_3(A0, RotateLeft(A1, 15)*5, C)*3), B1, IAND(B2, NOT(B3)), M); \
    B0 = NOT(IEOR(RotateLeft(B0, 1), A0));
#define PERM_ELT_JAVA(A0, A1, B0, B1, B2, B3, C, M) \
    A0 = IEOR_4((IEOR_3(A0, RotateLeft(A1, 15)*5, C)*3), B1, IAND(B2, NOT(B3)), M); \
    B0 = IEOR(NOT(RotateLeft(B0, 1)), A0);
#define PERM_STEP_0(A, B, C, M) \
    PERM_ELT_C(A(0), A(11), B(0), B(13), B(9), B(6), C(8), M(0)); \
    PERM_ELT_C(A(1), A(0), B(1), B(14), B(10), B(7), C(7), M(1)); \
    PERM_ELT_C(A(2), A(1), B(2), B(15), B(11), B(8), C(6), M(2)); \
    PERM_ELT_C(A(3), A(2), B(3), B(0), B(12), B(9), C(5), M(3)); \
    PERM_ELT_C(A(4), A(3), B(4), B(1), B(13), B(10), C(4), M(4)); \
    PERM_ELT_C(A(5), A(4), B(5), B(2), B(14), B(11), C(3), M(5)); \
    PERM_ELT_C(A(6), A(5), B(6), B(3), B(15), B(12), C(2), M(6)); \
    PERM_ELT_C(A(7), A(6), B(7), B(4), B(0), B(13), C(1), M(7)); \
    PERM_ELT_C(A(8), A(7), B(8), B(5), B(1), B(14), C(0), M(8)); \
    PERM_ELT_C(A(9), A(8), B(9), B(6), B(2), B(15), C(15), M(9)); \
    PERM_ELT_C(A(10), A(9), B(10), B(7), B(3), B(0), C(14), M(10)); \
    PERM_ELT_C(A(11), A(10), B(11), B(8), B(4), B(1), C(13), M(11)); \
    PERM_ELT_C(A(0), A(11), B(12), B(9), B(5), B(2), C(12), M(12)); \
    PERM_ELT_C(A(1), A(0), B(13), B(10), B(6), B(3), C(11), M(13)); \
    PERM_ELT_C(A(2), A(1), B(14), B(11), B(7), B(4), C(10), M(14)); \
    PERM_ELT_C(A(3), A(2), B(15), B(12), B(8), B(5), C(9), M(15));
#define PERM_STEP_1(A, B, C, M) \
    PERM_ELT_C(A(4), A(3), B(0), B(13), B(9), B(6), C(8), M(0)); \
    PERM_ELT_C(A(5), A(4), B(1), B(14), B(10), B(7), C(7), M(1)); \
    PERM_ELT_C(A(6), A(5), B(2), B(15), B(11), B(8), C(6), M(2)); \
    PERM_ELT_C(A(7), A(6), B(3), B(0), B(12), B(9), C(5), M(3)); \
    PERM_ELT_C(A(8), A(7), B(4), B(1), B(13), B(10), C(4), M(4)); \
    PERM_ELT_C(A(9), A(8), B(5), B(2), B(14), B(11), C(3), M(5)); \
    PERM_ELT_C(A(10), A(9), B(6), B(3), B(15), B(12), C(2), M(6)); \
    PERM_ELT_C(A(11), A(10), B(7), B(4), B(0), B(13), C(1), M(7)); \
    PERM_ELT_C(A(0), A(11), B(8), B(5), B(1), B(14), C(0), M(8)); \
    PERM_ELT_C(A(1), A(0), B(9), B(6), B(2), B(15), C(15), M(9)); \
    PERM_ELT_C(A(2), A(1), B(10), B(7), B(3), B(0), C(14), M(10)); \
    PERM_ELT_C(A(3), A(2), B(11), B(8), B(4), B(1), C(13), M(11)); \
    PERM_ELT_C(A(4), A(3), B(12), B(9), B(5), B(2), C(12), M(12)); \
    PERM_ELT_C(A(5), A(4), B(13), B(10), B(6), B(3), C(11), M(13)); \
    PERM_ELT_C(A(6), A(5), B(14), B(11), B(7), B(4), C(10), M(14)); \
    PERM_ELT_C(A(7), A(6), B(15), B(12), B(8), B(5), C(9), M(15));
#define PERM_STEP_2(A, B, C, M) \
    PERM_ELT_C(A(8), A(7), B(0), B(13), B(9), B(6), C(8), M(0)); \
    PERM_ELT_C(A(9), A(8), B(1), B(14), B(10), B(7), C(7), M(1)); \
    PERM_ELT_C(A(10), A(9), B(2), B(15), B(11), B(8), C(6), M(2)); \
    PERM_ELT_C(A(11), A(10), B(3), B(0), B(12), B(9), C(5), M(3)); \
    PERM_ELT_C(A(0), A(11), B(4), B(1), B(13), B(10), C(4), M(4)); \
    PERM_ELT_C(A(1), A(0), B(5), B(2), B(14), B(11), C(3), M(5)); \
    PERM_ELT_C(A(2), A(1), B(6), B(3), B(15), B(12), C(2), M(6)); \
    PERM_ELT_C(A(3), A(2), B(7), B(4), B(0), B(13), C(1), M(7)); \
    PERM_ELT_C(A(4), A(3), B(8), B(5), B(1), B(14), C(0), M(8)); \
    PERM_ELT_C(A(5), A(4), B(9), B(6), B(2), B(15), C(15), M(9)); \
    PERM_ELT_C(A(6), A(5), B(10), B(7), B(3), B(0), C(14), M(10)); \
    PERM_ELT_C(A(7), A(6), B(11), B(8), B(4), B(1), C(13), M(11)); \
    PERM_ELT_C(A(8), A(7), B(12), B(9), B(5), B(2), C(12), M(12)); \
    PERM_ELT_C(A(9), A(8), B(13), B(10), B(6), B(3), C(11), M(13)); \
    PERM_ELT_C(A(10), A(9), B(14), B(11), B(7), B(4), C(10), M(14)); \
    PERM_ELT_C(A(11), A(10), B(15), B(12), B(8), B(5), C(9), M(15));
#define STATE_ROTL(B) \
    DO I = 0, 15; \
        B(I) = RotateLeft(B(I), 17); \
    END DO;
#define STATE_ADD(A, C) \
    A(11) = A(11) + C(6); \
    A(10) = A(10) + C(5); \
    A(9) = A(9) + C(4); \
    A(8) = A(8) + C(3); \
    A(7) = A(7) + C(2); \
    A(6) = A(6) + C(1); \
    A(5) = A(5) + C(0); \
    A(4) = A(4) + C(15); \
    A(3) = A(3) + C(14); \
    A(2) = A(2) + C(13); \
    A(1) = A(1) + C(12); \
    A(0) = A(0) + C(11); \
    A(11) = A(11) + C(10); \
    A(10) = A(10) + C(9); \
    A(9) = A(9) + C(8); \
    A(8) = A(8) + C(7); \
    A(7) = A(7) + C(6); \
    A(6) = A(6) + C(5); \
    A(5) = A(5) + C(4); \
    A(4) = A(4) + C(3); \
    A(3) = A(3) + C(2); \
    A(2) = A(2) + C(1); \
    A(1) = A(1) + C(0); \
    A(0) = A(0) + C(15); \
    A(11) = A(11) + C(14); \
    A(10) = A(10) + C(13); \
    A(9) = A(9) + C(12); \
    A(8) = A(8) + C(11); \
    A(7) = A(7) + C(10); \
    A(6) = A(6) + C(9); \
    A(5) = A(5) + C(8); \
    A(4) = A(4) + C(7); \
    A(3) = A(3) + C(6); \
    A(2) = A(2) + C(5); \
    A(1) = A(1) + C(4); \
    A(0) = A(0) + C(3);

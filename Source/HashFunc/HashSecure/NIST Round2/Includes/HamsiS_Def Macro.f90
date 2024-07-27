#define S0                      VM(0)
#define S1                      VM(1)
#define S2                      VC(0)
#define S3                      VC(1)
#define S4                      VC(2)
#define S5                      VC(3)
#define S6                      VM(2)
#define S7                      VM(3)
#define S8                      VM(4)
#define S9                      VM(5)
#define SA                      VC(4)
#define SB                      VC(5)
#define SC                      VC(6)
#define SD                      VC(7)
#define SE                      VM(6)
#define SF                      VM(7)
#define READ_STATE_SMALL(C)     C(0:7) = MD%State(0:7)
#define WRITE_STATE_SMALL(C)    MD%State(0:7) = C(0:7)
#define SBOX(A, B, C, D)    \
        T = A; \
        A = IAND(A, C); \
        A = IEOR(A, D); \
        C = IEOR(C, B); \
        C = IEOR(C, A); \
        D = IOR(D, T); \
        D = IEOR(D, B); \
        T = IEOR(T, C); \
        B = D; \
        D = IOR(D, T); \
        D = IEOR(D, A); \
        A = IAND(A, B); \
        T = IEOR(T, A); \
        B = IEOR(B, D); \
        B = IEOR(B, T); \
        A = C; \
        C = B; \
        B = D; \
        D = NOT(T);
#define LBOX(A, B, C, D)    \
        A = RotateLeft(A, 13); \
        C = RotateLeft(C, 3); \
        B = IEOR(B, IEOR(A, C)); \
        D = IEOR(D, IEOR(C, SHIFTL(A, 3))); \
        B = RotateLeft(B, 1); \
        D = RotateLeft(D, 7); \
        A = IEOR(A, IEOR(B, D)); \
        C = IEOR(C, IEOR(D, SHIFTL(B, 7))); \
        A = RotateLeft(A, 5); \
        C = RotateLeft(C, 22);
#define INPUT_SMALL(M, Indx)   \
    M(0:7) = T256_0(0:7, Indx(0)); \
    DO I = 0, 7; \
        M(I) = IEOR(M(I), T256_8(I, Indx(1))); \
    END DO; \
    DO I = 0, 7; \
        M(I) = IEOR(M(I), T256_16(I, Indx(2))); \
    END DO; \
    DO I = 0, 7; \
        M(I) = IEOR(M(I), T256_24(I, Indx(3))); \
    END DO;
#define ROUND_SMALL(RC, Alpha)  \
    S0 = IEOR(S0, Alpha(0)); \
    S1 = IEOR(S1, IEOR(Alpha(1), RC)); \
    S2 = IEOR(S2, Alpha(2)); \
    S3 = IEOR(S3, Alpha(3)); \
    S4 = IEOR(S4, Alpha(8)); \
    S5 = IEOR(S5, Alpha(9)); \
    S6 = IEOR(S6, Alpha(10)); \
    S7 = IEOR(S7, Alpha(11)); \
    S8 = IEOR(S8, Alpha(16)); \
    S9 = IEOR(S9, Alpha(17)); \
    SA = IEOR(SA, Alpha(18)); \
    SB = IEOR(SB, Alpha(19)); \
    SC = IEOR(SC, Alpha(24)); \
    SD = IEOR(SD, Alpha(25)); \
    SE = IEOR(SE, Alpha(26)); \
    SF = IEOR(SF, Alpha(27)); \
    SBOX(S0, S4, S8, SC); \
    SBOX(S1, S5, S9, SD); \
    SBOX(S2, S6, SA, SE); \
    SBOX(S3, S7, SB, SF); \
    LBOX(S0, S5, SA, SF); \
    LBOX(S1, S6, SB, SC); \
    LBOX(S2, S7, S8, SD); \
    LBOX(S3, S4, S9, SE);
! order is important
#define T_SMALL(C)  \
    MD%State(7) = IEOR(MD%State(7), SB); \
    MD%State(6) = IEOR(MD%State(6), SA); \
    MD%State(5) = IEOR(MD%State(5), S9); \
    MD%State(4) = IEOR(MD%State(4), S8); \
    MD%State(3) = IEOR(MD%State(3), S3); \
    MD%State(2) = IEOR(MD%State(2), S2); \
    MD%State(1) = IEOR(MD%State(1), S1); \
    MD%State(0) = IEOR(MD%State(0), S0); \
    C(0:7) = MD%State(0:7);
#define GetIndex(X)     IAND(ToInteger(X), ToInteger(Z'000000FF'))

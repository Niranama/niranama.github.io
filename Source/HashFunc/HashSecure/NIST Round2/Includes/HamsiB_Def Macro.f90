#define S00                     VM(0)
#define S01                     VM(1)
#define S02                     VC(0)
#define S03                     VC(1)
#define S04                     VM(2)
#define S05                     VM(3)
#define S06                     VC(2)
#define S07                     VC(3)
#define S08                     VC(4)
#define S09                     VC(5)
#define S0A                     VM(4)
#define S0B                     VM(5)
#define S0C                     VC(6)
#define S0D                     VC(7)
#define S0E                     VM(6)
#define S0F                     VM(7)
#define S10                     VM(8)
#define S11                     VM(9)
#define S12                     VC(8)
#define S13                     VC(9)
#define S14                     VM(10)
#define S15                     VM(11)
#define S16                     VC(10)
#define S17                     VC(11)
#define S18                     VC(12)
#define S19                     VC(13)
#define S1A                     VM(12)
#define S1B                     VM(13)
#define S1C                     VC(14)
#define S1D                     VC(15)
#define S1E                     VM(14)
#define S1F                     VM(15)
#define READ_STATE_BIG(C)       C(0:15) = MD%State(0:15)
#define WRITE_STATE_BIG(C)      MD%State(0:15) = C(0:15)
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
#define INPUT_BIG(M, Indx)   \
    M(0:15) = T512_0(0:15, Indx(0)); \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_8(I, Indx(1))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_16(I, Indx(2))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_24(I, Indx(3))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_32(I, Indx(4))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_40(I, Indx(5))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_48(I, Indx(6))); \
    END DO; \
    DO I = 0, 15; \
        M(I) = IEOR(M(I), T512_56(I, Indx(7))); \
    END DO;
#define ROUND_BIG_INIT(RC, Alpha)  \
    S00 = IEOR(S00, Alpha(0)); \
    S01 = IEOR(S01, IEOR(Alpha(1), RC)); \
    S02 = IEOR(S02, Alpha(2)); \
    S03 = IEOR(S03, Alpha(3)); \
    S04 = IEOR(S04, Alpha(4)); \
    S05 = IEOR(S05, Alpha(5)); \
    S06 = IEOR(S06, Alpha(6)); \
    S07 = IEOR(S07, Alpha(7)); \
    S08 = IEOR(S08, Alpha(8)); \
    S09 = IEOR(S09, Alpha(9)); \
    S0A = IEOR(S0A, Alpha(10)); \
    S0B = IEOR(S0B, Alpha(11)); \
    S0C = IEOR(S0C, Alpha(12)); \
    S0D = IEOR(S0D, Alpha(13)); \
    S0E = IEOR(S0E, Alpha(14)); \
    S0F = IEOR(S0F, Alpha(15)); \
    S10 = IEOR(S10, Alpha(16)); \
    S11 = IEOR(S11, Alpha(17)); \
    S12 = IEOR(S12, Alpha(18)); \
    S13 = IEOR(S13, Alpha(19)); \
    S14 = IEOR(S14, Alpha(20)); \
    S15 = IEOR(S15, Alpha(21)); \
    S16 = IEOR(S16, Alpha(22)); \
    S17 = IEOR(S17, Alpha(23)); \
    S18 = IEOR(S18, Alpha(24)); \
    S19 = IEOR(S19, Alpha(25)); \
    S1A = IEOR(S1A, Alpha(26)); \
    S1B = IEOR(S1B, Alpha(27)); \
    S1C = IEOR(S1C, Alpha(28)); \
    S1D = IEOR(S1D, Alpha(29)); \
    S1E = IEOR(S1E, Alpha(30)); \
    S1F = IEOR(S1F, Alpha(31));
#define ROUND_BIG_SBOX        \
    SBOX(S00, S08, S10, S18); \
    SBOX(S01, S09, S11, S19); \
    SBOX(S02, S0A, S12, S1A); \
    SBOX(S03, S0B, S13, S1B); \
    SBOX(S04, S0C, S14, S1C); \
    SBOX(S05, S0D, S15, S1D); \
    SBOX(S06, S0E, S16, S1E); \
    SBOX(S07, S0F, S17, S1F);
#define ROUND_BIG_LBOX        \
    LBOX(S00, S09, S12, S1B); \
    LBOX(S01, S0A, S13, S1C); \
    LBOX(S02, S0B, S14, S1D); \
    LBOX(S03, S0C, S15, S1E); \
    LBOX(S04, S0D, S16, S1F); \
    LBOX(S05, S0E, S17, S18); \
    LBOX(S06, S0F, S10, S19); \
    LBOX(S07, S08, S11, S1A); \
    LBOX(S00, S02, S05, S07); \
    LBOX(S10, S13, S15, S16); \
    LBOX(S09, S0B, S0C, S0E); \
    LBOX(S19, S1A, S1C, S1F);
! order is important
#define T_BIG(C)  \
    MD%State(15) = IEOR(MD%State(15), S17); \
    MD%State(14) = IEOR(MD%State(14), S16); \
    MD%State(13) = IEOR(MD%State(13), S15); \
    MD%State(12) = IEOR(MD%State(12), S14); \
    MD%State(11) = IEOR(MD%State(11), S13); \
    MD%State(10) = IEOR(MD%State(10), S12); \
    MD%State(9)  = IEOR(MD%State(9),  S11); \
    MD%State(8)  = IEOR(MD%State(8),  S10); \
    MD%State(7)  = IEOR(MD%State(7),  S07); \
    MD%State(6)  = IEOR(MD%State(6),  S06); \
    MD%State(5)  = IEOR(MD%State(5),  S05); \
    MD%State(4)  = IEOR(MD%State(4),  S04); \
    MD%State(3)  = IEOR(MD%State(3),  S03); \
    MD%State(2)  = IEOR(MD%State(2),  S02); \
    MD%State(1)  = IEOR(MD%State(1),  S01); \
    MD%State(0)  = IEOR(MD%State(0),  S00); \
    C(0:15) = MD%State(0:15);
#define GetIndex(X)     IAND(ToInteger(X), ToInteger(Z'000000FF'))
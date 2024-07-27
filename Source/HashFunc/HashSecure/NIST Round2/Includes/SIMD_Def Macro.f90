! Ranges:
!   REDS1: from -32768..98302 to -383..383
!   REDS2: from -2^31..2^31-1 to -32768..98302
#define REDS1(X)        IAND(X, MaskI8) - SHIFTA(X, 8)
#define REDS2(X)        IAND(X, MaskI16) + SHIFTA(X, 16)
! If, upon entry, the values of q[] are all in the -N..N range (where
! N >= 98302) then the new values of q[] are in the -2N..2N range.
!
! Since alpha_tab[v] <= 256, maximum allowed range is for N = 8388608.
#define FFT_LOOP(RB, HK, AS) \
    BLOCK; \
        tIndex      :: I, J; \
        tInteger    :: M, N, T; \
        M = Q(RB); \
        N = Q(RB + HK); \
        Q(RB) = M + N; \
        Q(RB + HK) = M - N; \
        I = 0; \
        J = 0; \
        DO WHILE (I < HK); \
            IF (I /= 0) THEN; \
                M = Q(RB + I + 0); \
                N = Q(RB + I + 0 + HK); \
                T = REDS2(N * AlphaTab(J + 0 * AS)); \
                Q(RB + I + 0) = M + T; \
                Q(RB + I + 0 + HK) = M - T; \
            END IF; \
            M = Q(RB + I + 1); \
            N = Q(RB + I + 1 + HK); \
            T = REDS2(N * AlphaTab(J + 1 * AS)); \
            Q(RB + I + 1) = M + T; \
            Q(RB + I + 1 + HK) = M - T; \
            M = Q(RB + I + 2); \
            N = Q(RB + I + 2 + HK); \
            T = REDS2(N * AlphaTab(J + 2 * AS)); \
            Q(RB + I + 2) = M + T; \
            Q(RB + I + 2 + HK) = M - T; \
            M = Q(RB + I + 3); \
            N = Q(RB + I + 3 + HK); \
            T = REDS2(N * AlphaTab(J + 3 * AS)); \
            Q(RB + I + 3) = M + T; \
            Q(RB + I + 3 + HK) = M - T; \
            I = I + 4; \
            J = J + 4*AS; \
        END DO; \
    END BLOCK;
! * Output ranges:
!   d0:   min=    0   max= 1020
!   d1:   min=  -67   max= 4587
!   d2:   min=-4335   max= 4335
!   d3:   min=-4147   max=  507
!   d4:   min= -510   max=  510
!   d5:   min= -252   max= 4402
!   d6:   min=-4335   max= 4335
!   d7:   min=-4332   max=  322
#define FFT8(XB, XS, D) \
    BLOCK; \
        tInteger    :: X0, X1, X2, X3; \
        tInteger    :: A0, A1, A2, A3; \
        tInteger    :: B0, B1, B2, B3; \
        X0 = IAND(ToInteger(X(XB)), MaskI8); \
        X1 = IAND(ToInteger(X(XB + XS)), MaskI8); \
        X2 = IAND(ToInteger(X(XB + 2 * XS)), MaskI8); \
        X3 = IAND(ToInteger(X(XB + 3 * XS)), MaskI8); \
        A0 = X0 + X2; \
        A1 = X0 + SHIFTL(X2, 4); \
        A2 = X0 - X2; \
        A3 = X0 - SHIFTL(X2, 4); \
        B0 = X1 + X3; \
        B1 = REDS1(SHIFTL(X1, 2) + SHIFTL(X3, 6)); \
        B2 = SHIFTL(X1, 4) - SHIFTL(X3, 4); \
        B3 = REDS1(SHIFTL(X1, 6) + SHIFTL(X3, 2)); \
        D(0) = A0 + B0; \
        D(1) = A1 + B1; \
        D(2) = A2 + B2; \
        D(3) = A3 + B3; \
        D(4) = A0 - B0; \
        D(5) = A1 - B1; \
        D(6) = A2 - B2; \
        D(7) = A3 - B3; \
    END BLOCK;
! When k=16, we have alpha=2. Multiplication by alpha^i is then reduced
! to some shifting.
!
! Output: within -591471..591723
#define FFT16(XB, XS, RB) \
    BLOCK; \
        tInteger    :: D1(0:7), D2(0:7); \
        FFT8(XB, SHIFTL(XS, 1), D1); \
        FFT8((XB + XS), SHIFTL(XS, 1), D2); \
        Q(RB +  0) = D1(0) + D2(0); \
        Q(RB +  1) = D1(1) + SHIFTL(D2(1), 1); \
        Q(RB +  2) = D1(2) + SHIFTL(D2(2), 2); \
        Q(RB +  3) = D1(3) + SHIFTL(D2(3), 3); \
        Q(RB +  4) = D1(4) + SHIFTL(D2(4), 4); \
        Q(RB +  5) = D1(5) + SHIFTL(D2(5), 5); \
        Q(RB +  6) = D1(6) + SHIFTL(D2(6), 6); \
        Q(RB +  7) = D1(7) + SHIFTL(D2(7), 7); \
        Q(RB +  8) = D1(0) - D2(0); \
        Q(RB +  9) = D1(1) - SHIFTL(D2(1), 1); \
        Q(RB + 10) = D1(2) - SHIFTL(D2(2), 2); \
        Q(RB + 11) = D1(3) - SHIFTL(D2(3), 3); \
        Q(RB + 12) = D1(4) - SHIFTL(D2(4), 4); \
        Q(RB + 13) = D1(5) - SHIFTL(D2(5), 5); \
        Q(RB + 14) = D1(6) - SHIFTL(D2(6), 6); \
        Q(RB + 15) = D1(7) - SHIFTL(D2(7), 7); \
    END BLOCK;
#define INNER(L, H, M)  IAND(L*M, MaskI16) + SHIFTL(H*M, 16)
#define WSREAD(SB, O1, O2, M) \
    BLOCK; \
        tIndex      :: I, J; \
        DO I = 0, 31, 4; \
            J = Wsp(SHIFTA(I, 2) + SB); \
            W(I)     = INNER(Q(J     + O1), Q(J     + O2), M); \
            W(I + 1) = INNER(Q(J + 2 + O1), Q(J + 2 + O2), M); \
            W(I + 2) = INNER(Q(J + 4 + O1), Q(J + 4 + O2), M); \
            W(I + 3) = INNER(Q(J + 6 + O1), Q(J + 6 + O2), M); \
        END DO; \
    END BLOCK;
#define WBREAD(SB, O1, O2, M) \
    BLOCK; \
        tIndex      :: I, J; \
        DO I = 0, 63, 8; \
            J = Wbp(SHIFTA(I, 3) + SB); \
            W(I)     = INNER(Q(J      + O1), Q(J      + O2), M); \
            W(I + 1) = INNER(Q(J +  2 + O1), Q(J +  2 + O2), M); \
            W(I + 2) = INNER(Q(J +  4 + O1), Q(J +  4 + O2), M); \
            W(I + 3) = INNER(Q(J +  6 + O1), Q(J +  6 + O2), M); \
            W(I + 4) = INNER(Q(J +  8 + O1), Q(J +  8 + O2), M); \
            W(I + 5) = INNER(Q(J + 10 + O1), Q(J + 10 + O2), M); \
            W(I + 6) = INNER(Q(J + 12 + O1), Q(J + 12 + O2), M); \
            W(I + 7) = INNER(Q(J + 14 + O1), Q(J + 14 + O2), M); \
        END DO; \
    END BLOCK;
#define IFF(X, Y, Z)    IEOR(IAND(IEOR(Y, Z), X), Z)
#define MAJ(X, Y, Z)    IOR(IAND(X, Y), IAND(IOR(X, Y), Z))
#define STEP_ELT(N, W, Func, S, PPB) \
    Tmp = D(N) + W + Func(A(N), B(N), C(N)); \
    A(N) = RotateLeft(Tmp, S) + tA(IEOR(PPB, N)); \
    D(N) = C(N); \
    C(N) = B(N); \
    B(N) = tA(N);
#define STEP_SMALL(W0, W1, W2, W3, Func, R, S, PP4B) \
    tA(0) = RotateLeft(A(0), R); \
    tA(1) = RotateLeft(A(1), R); \
    tA(2) = RotateLeft(A(2), R); \
    tA(3) = RotateLeft(A(3), R); \
    STEP_ELT(0, W0, Func, S, PP4B); \
    STEP_ELT(1, W1, Func, S, PP4B); \
    STEP_ELT(2, W2, Func, S, PP4B); \
    STEP_ELT(3, W3, Func, S, PP4B);
#define STEP_BIG(W0, W1, W2, W3, W4, W5, W6, W7, Func, R, S, PP8B) \
    tA(0) = RotateLeft(A(0), R); \
    tA(1) = RotateLeft(A(1), R); \
    tA(2) = RotateLeft(A(2), R); \
    tA(3) = RotateLeft(A(3), R); \
    tA(4) = RotateLeft(A(4), R); \
    tA(5) = RotateLeft(A(5), R); \
    tA(6) = RotateLeft(A(6), R); \
    tA(7) = RotateLeft(A(7), R); \
    STEP_ELT(0, W0, Func, S, PP8B); \
    STEP_ELT(1, W1, Func, S, PP8B); \
    STEP_ELT(2, W2, Func, S, PP8B); \
    STEP_ELT(3, W3, Func, S, PP8B); \
    STEP_ELT(4, W4, Func, S, PP8B); \
    STEP_ELT(5, W5, Func, S, PP8B); \
    STEP_ELT(6, W6, Func, S, PP8B); \
    STEP_ELT(7, W7, Func, S, PP8B);
#define STEP_ELT_II(N, W, Func, S, PPB) \
    Tmp = D(N) + W + Func(A(N), B(N), C(N)); \
    A(N) = RotateLeft(Tmp, S) + tA(PPB(N)); \
    D(N) = C(N); \
    C(N) = B(N); \
    B(N) = tA(N);
#define STEP_SMALL_II(W0, W1, W2, W3, Func, R, S, PP4B) \
    tA(0) = RotateLeft(A(0), R); \
    tA(1) = RotateLeft(A(1), R); \
    tA(2) = RotateLeft(A(2), R); \
    tA(3) = RotateLeft(A(3), R); \
    STEP_ELT_II(0, W0, Func, S, PP4B); \
    STEP_ELT_II(1, W1, Func, S, PP4B); \
    STEP_ELT_II(2, W2, Func, S, PP4B); \
    STEP_ELT_II(3, W3, Func, S, PP4B);
#define STEP_BIG_II(W0, W1, W2, W3, W4, W5, W6, W7, Func, R, S, PP8B) \
    tA(0) = RotateLeft(A(0), R); \
    tA(1) = RotateLeft(A(1), R); \
    tA(2) = RotateLeft(A(2), R); \
    tA(3) = RotateLeft(A(3), R); \
    tA(4) = RotateLeft(A(4), R); \
    tA(5) = RotateLeft(A(5), R); \
    tA(6) = RotateLeft(A(6), R); \
    tA(7) = RotateLeft(A(7), R); \
    STEP_ELT_II(0, W0, Func, S, PP8B); \
    STEP_ELT_II(1, W1, Func, S, PP8B); \
    STEP_ELT_II(2, W2, Func, S, PP8B); \
    STEP_ELT_II(3, W3, Func, S, PP8B); \
    STEP_ELT_II(4, W4, Func, S, PP8B); \
    STEP_ELT_II(5, W5, Func, S, PP8B); \
    STEP_ELT_II(6, W6, Func, S, PP8B); \
    STEP_ELT_II(7, W7, Func, S, PP8B);

#define TIX2(Q, X00, X01, X08, X10, X24)    \
        X10 = IEOR(X10, X00); \
        X00 = Q; \
        X08 = IEOR(X08, X00); \
        X01 = IEOR(X01, X24);
#define TIX3(Q, X00, X01, X04, X08, X16, X27, X30)  \
        X16 = IEOR(X16, X00); \
        X00 = Q; \
        X08 = IEOR(X08, X00); \
        X01 = IEOR(X01, X27); \
        X04 = IEOR(X04, X30);
#define TIX4(Q, X00, X01, X04, X07, X08, X22, X24, X27, X30)    \
        X22 = IEOR(X22, X00); \
        X00 = Q; \
        X08 = IEOR(X08, X00); \
        X01 = IEOR(X01, X24); \
        X04 = IEOR(X04, X27); \
        X07 = IEOR(X07, X30);
#define CMIX30(X00, X01, X02, X04, X05, X06, X15, X16, X17) \
        X00 = IEOR(X00, X04); \
        X01 = IEOR(X01, X05); \
        X02 = IEOR(X02, X06); \
        X15 = IEOR(X15, X04); \
        X16 = IEOR(X16, X05); \
        X17 = IEOR(X17, X06);
#define CMIX36(X00, X01, X02, X04, X05, X06, X18, X19, X20) \
        X00 = IEOR(X00, X04); \
        X01 = IEOR(X01, X05); \
        X02 = IEOR(X02, X06); \
        X18 = IEOR(X18, X04); \
        X19 = IEOR(X19, X05); \
        X20 = IEOR(X20, X06);
#define MIndex(X, Pos)          IAND(SHIFTR(X, Pos), ToInteger(Z'000000FF'))
#define SMIX(X0, X1, X2, X3)    \
        C0 = 0; C1 = 0; C2 = 0; C3 = 0; \
        R0 = 0; R1 = 0; R2 = 0; R3 = 0; \
        Tmp = MiXTab0(SHIFTR(X0, 24)); \
        C0 = IEOR(C0, Tmp); \
        Tmp = MiXTab1(MIndex(X0, 16)); \
        C0 = IEOR(C0, Tmp); \
        R1 = IEOR(R1, Tmp); \
        Tmp = MiXTab2(MIndex(X0, 8)); \
        C0 = IEOR(C0, Tmp); \
        R2 = IEOR(R2, Tmp); \
        Tmp = MiXTab3(MIndex(X0, 0)); \
        C0 = IEOR(C0, Tmp); \
        R3 = IEOR(R3, Tmp); \
        Tmp = MiXTab0(SHIFTR(X1, 24)); \
        C1 = IEOR(C1, Tmp); \
        R0 = IEOR(R0, Tmp); \
        Tmp = MiXTab1(MIndex(X1, 16)); \
        C1 = IEOR(C1, Tmp); \
        Tmp = MiXTab2(MIndex(X1, 8)); \
        C1 = IEOR(C1, Tmp); \
        R2 = IEOR(R2, Tmp); \
        Tmp = MiXTab3(MIndex(X1, 0)); \
        C1 = IEOR(C1, Tmp); \
        R3 = IEOR(R3, Tmp); \
        Tmp = MiXTab0(SHIFTR(X2, 24)); \
        C2 = IEOR(C2, Tmp); \
        R0 = IEOR(R0, Tmp); \
        Tmp = MiXTab1(MIndex(X2, 16)); \
        C2 = IEOR(C2, Tmp); \
        R1 = IEOR(R1, Tmp); \
        Tmp = MiXTab2(MIndex(X2, 8)); \
        C2 = IEOR(C2, Tmp); \
        Tmp = MiXTab3(MIndex(X2, 0)); \
        C2 = IEOR(C2, Tmp); \
        R3 = IEOR(R3, Tmp); \
        Tmp = MiXTab0(SHIFTR(X3, 24)); \
        C3 = IEOR(C3, Tmp); \
        R0 = IEOR(R0, Tmp); \
        Tmp = MiXTab1(MIndex(X3, 16)); \
        C3 = IEOR(C3, Tmp); \
        R1 = IEOR(R1, Tmp); \
        Tmp = MiXTab2(MIndex(X3, 8)); \
        C3 = IEOR(C3, Tmp); \
        R2 = IEOR(R2, Tmp); \
        Tmp = MiXTab3(MIndex(X3, 0)); \
        C3 = IEOR(C3, Tmp); \
        X0 = IOR(IOR(IOR(IAND(IEOR(C0, R0), MC0),  \
                         IAND(IEOR(C1, R1), MC1)), \
                         IAND(IEOR(C2, R2), MC2)), \
                         IAND(IEOR(C3, R3), MC3));   \
        X1 = IOR(IOR(IOR(IAND(IEOR(C1, SHIFTL(R0, 8)),  MC0),  \
                         IAND(IEOR(C2, SHIFTL(R1, 8)),  MC1)), \
                         IAND(IEOR(C3, SHIFTL(R2, 8)),  MC2)), \
                         IAND(IEOR(C0, SHIFTR(R3, 24)), MC3));   \
        X2 = IOR(IOR(IOR(IAND(IEOR(C2, SHIFTL(R0, 16)), MC0),  \
                         IAND(IEOR(C3, SHIFTL(R1, 16)), MC1)), \
                         IAND(IEOR(C0, SHIFTR(R2, 16)), MC2)), \
                         IAND(IEOR(C1, SHIFTR(R3, 16)), MC3));   \
        X3 = IOR(IOR(IOR(IAND(IEOR(C3, SHIFTL(R0, 24)), MC0),  \
                         IAND(IEOR(C0, SHIFTR(R1, 8)),  MC1)), \
                         IAND(IEOR(C1, SHIFTR(R2, 8)),  MC2)), \
                         IAND(IEOR(C2, SHIFTR(R3, 8)),  MC3));
#define S00                     MD%State( 0)
#define S01                     MD%State( 1)
#define S02                     MD%State( 2)
#define S03                     MD%State( 3)
#define S04                     MD%State( 4)
#define S05                     MD%State( 5)
#define S06                     MD%State( 6)
#define S07                     MD%State( 7)
#define S08                     MD%State( 8)
#define S09                     MD%State( 9)
#define S10                     MD%State(10)
#define S11                     MD%State(11)
#define S12                     MD%State(12)
#define S13                     MD%State(13)
#define S14                     MD%State(14)
#define S15                     MD%State(15)
#define S16                     MD%State(16)
#define S17                     MD%State(17)
#define S18                     MD%State(18)
#define S19                     MD%State(19)
#define S20                     MD%State(20)
#define S21                     MD%State(21)
#define S22                     MD%State(22)
#define S23                     MD%State(23)
#define S24                     MD%State(24)
#define S25                     MD%State(25)
#define S26                     MD%State(26)
#define S27                     MD%State(27)
#define S28                     MD%State(28)
#define S29                     MD%State(29)
#define S30                     MD%State(30)
#define S31                     MD%State(31)
#define S32                     MD%State(32)
#define S33                     MD%State(33)
#define S34                     MD%State(34)
#define S35                     MD%State(35)
#define CORE_ENTRY(Input, Length) \
    MD%BitCount = MD%BitCount + ToLong(SHIFTL(Length, 3)); \
    P = MD%Partial; \
    PLen = MD%PartialLen; \
    CurID = 0; \
    IF (PLen < 4) THEN; \
        Count = 4 - PLen; \
        IF (Length < Count) Count = Length; \
        PLen = PLen + Count; \
        DO WHILE (Count > 0); \
            Count = Count - 1; \
            P = IOR(SHIFTL(P, 8), IAND(ToInteger(Input(CurID)), Z'000000FF')); \
            CurID = CurID + 1; \
            Length = Length - 1; \
        END DO; \
        IF (Length == 0) THEN; \
            MD%Partial = P; \
            MD%PartialLen = PLen; \
            RETURN; \
        END IF; \
    END IF;
#define CORE_EXIT(Input, Length) \
    P = 0; \
    MD%PartialLen = Length; \
    DO WHILE (Length > 0); \
        Length = Length - 1; \
        P = IOR(SHIFTL(P, 8), IAND(ToInteger(Input(CurID)), Z'000000FF')); \
        CurID = CurID + 1; \
    END DO; \
    MD%Partial = P; \
    MD%RoundShift = RShift;
! Not in a do..while: the 'break' must exit the outer loop.
#define NEXT(RC, Input, Length) \
    IF (Length <= 4) THEN; \
        RShift = RC; \
        EXIT; \
    END IF; \
    CALL BytePackBE(Input, CurID, P); \
    CurID = CurID + 4; \
    Length = Length - 4;
#define CLOSE_ENTRY(Ns, Rcm, FCore) \
    PLen = MD%PartialLen; \
    TmpBuf = -52; \
    CALL ByteUnpackBE(MD%BitCount+NBits, TmpBuf, 4_kIndex); \
    IF ((PLen == 0).AND.(NBits == 0)) THEN; \
        PLen = 4; \
    ELSEIF ((PLen < 4).OR.(NBits /= 0)) THEN; \
        IF (PLen == 4) PLen = 0; \
        TmpBuf(PLen) = IAND(LastByte, NOT(SHIFTR(ToByte(Z'FF'), NBits))); \
        TmpBuf(PLen+1:3) = 0; \
    END IF; \
    CALL FCore(MD, TmpBuf(PLen:15)); \
    Rms = MD%RoundShift*Rcm; \
    S(0:Rms-1)  = MD%State(Ns-Rms:Ns-1); \
    S(Rms:Ns-1) = MD%State(0:(Ns-Rms)-1);
#define ROR(Nn, Ns) \
    TmpS(0:Nn-1) = S(Ns-Nn:Ns-1); \
    S(Nn:Ns-1)   = S(0:Ns-Nn-1); \
    S(0:Nn-1)    = TmpS(0:Nn-1);

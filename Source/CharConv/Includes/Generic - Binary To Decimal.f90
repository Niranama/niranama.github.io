
SUBROUTINE Bin2Dec_DragonBox(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a binary floating point number into the shortest and correctly
    ! rounded decimal representation based on the DragonBox algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    tSInt32,   INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    tUIntType, INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    tSInt32,   INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    tUIntType, INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    tSInt32,   INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
    tUInt64     :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
    tUInt128    :: Pow10
#else
    tUInt64     :: Pow10
#endif
#endif
    tLogical    :: Include_Left_Endpoint, Include_Right_Endpoint
    tSInt32     :: Minus_K, Beta
    tUInt32     :: DeltaI, R, Dist
    tUIntType   :: Two_Fl, Two_Fc
    tUIntType   :: ZMul_Val
    tLogical    :: ZMul_IsInteger
    tLogical    :: XMul_IsInteger, XMul_Parity
    tLogical    :: YMul_IsInteger, Approx_Y_Parity
    tLogical    :: Divisible_By_Small_Divisor

!** FLOW:

    ! Step 1: integer promotion & Schubfach multiplier calculation.

    ! Check if normal.
    IF ((ExpRaw /= 0).AND.(SigRaw == ZeroUInt)) THEN
        CALL Shorter_Interval_Case(ExpBin, SigDec, ExpDec)
        RETURN
    END IF

    Include_Left_Endpoint  = (IAND(SigBin, OneUInt) == ZeroUInt)
    Include_Right_Endpoint = Include_Left_Endpoint

    ! Compute K and Beta as well as get cached data
    Minus_K = Floor_Log10_Pow2(ExpBin) - Kappa
    Beta    = ExpBin + Floor_Log2_Pow10(-Minus_K)
#ifdef  tFloat_is_tQuad
    Pow10 = Get_Pow10_256Bits(-Minus_K)
    IF (Minus_K > 0) Pow10(0) = Pow10(0) + 1_kInt64
#else
#ifdef  tFloat_is_tDouble
    Pow10 = Get_Pow10_128Bits(-Minus_K)
    IF (Minus_K > 0) Pow10%Low = Pow10%Low + 1_kInt64
#else
    Pow10 = Get_Pow10_64Bits(-Minus_K)
    IF (Minus_K > 0) Pow10 = Pow10 + 1_kInt64
#endif
#endif

    ! Compute Zi and Deltai.
    ! 10**Kappa <= Deltai < 10**(Kappa + 1)
    DeltaI = Compute_Delta(Pow10, Beta)
    Two_Fc = SHIFTL(SigBin, 1)

    ! For the case of binary32, the result of integer check is not correct for
    ! 29711844 * 2^-82
    ! = 6.1442653300000000008655037797566933477355632930994033813476... * 10^-18
    ! and 29711844 * 2^-81
    ! = 1.2288530660000000001731007559513386695471126586198806762695... * 10^-17,
    ! and they are the unique counterexamples. However, since 29711844 is even,
    ! this does not cause any problem for the endpoints calculations; it can only
    ! cause a problem when we need to perform integer check for the center.
    ! Fortunately, with these inputs, that branch is never executed, so we are
    ! fine.
    CALL Compute_Mul(SHIFTL(IOR(Two_Fc, OneUInt), Beta), Pow10, ZMul_Val, ZMul_IsInteger)

    ! Step 2: Try larger divisor; remove trailing zeros if necessary.

    ! Using an upper bound on zi, we might be able to optimize the division
    ! better than the compiler; we are computing zi / big_divisor here.
    SigDec = Divide_By_10_To_Kappa_Plus_1(ZMul_Val)
    R = ZMul_Val - Big_Divisor*SigDec   ! implicit conversion if necessary

    IF (R .ULT. DeltaI) THEN
        ! Exclude the right endpoint if necessary.
        IF ((R == 0).AND.ZMul_IsInteger.AND.(.NOT.Include_Right_Endpoint)) THEN
            SigDec = SigDec - OneUInt
            R = Big_Divisor
            ! must perform Step 3
        ELSE
            ExpDec = Minus_K + Kappa + 1
            RETURN
        END IF
    ELSEIF (R == DeltaI) THEN
        ! r == deltai; compare fractional parts.
        Two_Fl = Two_Fc - OneUInt
        XMul_Parity = Compute_Mul_Parity(Two_Fl, Pow10, Beta, XMul_IsInteger)
        IF ((.NOT.Include_Left_Endpoint).OR.(ExpBin < Case_Fc_Pm_Half_Lower_Threshold).OR. &
            (ExpBin > Divisibility_Check_By_5_Threshold)) THEN
            ! If the left endpoint is not included, the condition for
            ! success is z^(f) < delta^(f) (odd parity).
            ! Otherwise, the inequalities on exponent ensure that
            ! x is not an integer, so if z^(f) >= delta^(f) (even parity), we in fact
            ! have strict inequality.
            IF (XMul_Parity) THEN
                ExpDec = Minus_K + Kappa + 1
                RETURN
            END IF
            ! must perform Step 3
        ELSE
            IF (XMul_Parity.OR.XMul_IsInteger) THEN
                ExpDec = Minus_K + Kappa + 1
                RETURN
            END IF
            ! must perform Step 3
        END IF
    ELSE
        ! must perform Step 3
    END IF

    ! Step 3: Find the significand with the smaller divisor
    SigDec = SigDec*TenUInt
    ExpDec = Minus_K + Kappa

    Dist = R - SHIFTR(DeltaI, 1) + Half_Small_Divisor
    Approx_Y_Parity = IAND(IEOR(Dist, Half_Small_Divisor), 1) /= 0

    Divisible_By_Small_Divisor = Is_Divisible_By_Pow10(Dist)

    ! Add dist / 10^kappa to the significand.
    SigDec = SigDec + Dist

    ! Is dist divisible by 10^kappa?
    IF (Divisible_By_Small_Divisor) THEN
        ! Check z^(f) >= epsilon^(f).
        ! We have either yi == zi - epsiloni or yi == (zi - epsiloni) - 1,
        ! where yi == zi - epsiloni if and only if z^(f) >= epsilon^(f)
        ! Since there are only 2 possibilities, we only need to care about the
        ! parity. Also, zi and r should have the same parity since the divisor
        ! is an even number.
        IF (Compute_Mul_Parity(Two_Fc, Pow10, Beta, YMul_IsInteger) .NEQV. Approx_Y_Parity) THEN
            SigDec = SigDec - OneUInt
        ELSE
            ! If z^(f) >= epsilon^(f), we might have a tie
            ! when z^(f) == epsilon^(f), or equivalently, when y is an integer
            IF (YMul_IsInteger) THEN
                IF (IAND(SigDec, OneUInt) /= ZeroUInt) SigDec = SigDec - OneUInt
            END IF
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Compute_Mul(U, Pow10, ResHi, IsInteger)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the multiplication of U and Pow10

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: U
#ifdef  tFloat_is_tQuad
        tUInt64,   INTENT(IN)   :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,   INTENT(IN)   :: Pow10
#endif
#endif
        tUIntType, INTENT(OUT)  :: ResHi
        tLogical,  INTENT(OUT)  :: IsInteger

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUIntType   :: ResLo
#ifdef tFloat_is_tSingle
        tUInt64     :: Output
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        CALL UMul384_Upper256(U, Pow10, ResHi, ResLo)
        IsInteger = ResLo == ZeroU128
#else
#ifdef  tFloat_is_tDouble
        CALL UMul192_Upper128(U, Pow10%High, Pow10%Low, ResHi, ResLo)
        IsInteger = ResLo == 0_kInt64
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        Output = UMul96_Upper64(U, Pow10)
        ResHi = ToU32(SHIFTR(Output, 32))
        IsInteger = ToU32(Output) == 0
#undef  ToU32
#endif
#endif

        RETURN

    END SUBROUTINE Compute_Mul

    !**************************************************************************

    FUNCTION Compute_Mul_Parity(Two_F, Pow10, Beta, IsInteger) RESULT(Parity)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check multiplication parity

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: Two_F
#ifdef  tFloat_is_tQuad
        tUInt64,   INTENT(IN)   :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,   INTENT(IN)   :: Pow10
#endif
#endif
        tSInt32,   INTENT(IN)   :: Beta
        tLogical,  INTENT(OUT)  :: IsInteger
        tLogical                :: Parity

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt128    :: U256Hi, U256Lo
#else
#ifdef  tFloat_is_tDouble
        tUInt64     :: U128Hi, U128Lo
#else
        tUInt64     :: Output
#endif
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        CALL UMul384_Lower256(Two_F, Pow10, U256Hi, U256Lo)
        Parity = IAND(SHIFTR(U256Hi, (128 - Beta)), OneU128) /= ZeroU128
        IsInteger = IOR(SHIFTL(U256Hi, Beta), SHIFTR(U256Lo, (128 - Beta))) == ZeroU128
#else
#ifdef  tFloat_is_tDouble
        CALL UMul192_Lower128(Two_F, Pow10%High, Pow10%Low, U128Hi, U128Lo)
        Parity = IAND(SHIFTR(U128Hi, (64 - Beta)), 1_kInt64) /= 0_kInt64
        IsInteger = IOR(SHIFTL(U128Hi, Beta), SHIFTR(U128Lo, (64 - Beta))) == 0_kInt64
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        Output = UMul96_Lower64(Two_F, Pow10)
        Parity = IAND(SHIFTR(Output, (64 - Beta)), 1_kInt64) /= 0_kInt64
        IsInteger = ToU32(SHIFTR(Output, (32 - Beta))) == 0
#undef  ToU32
#endif
#endif

        RETURN

    END FUNCTION Compute_Mul_Parity

    !**************************************************************************

    FUNCTION Is_Divisible_By_Pow10(N) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To replace N by Floor(N / Pow(10, M)) returning true if and only if N is
        ! divisible by pow(10, M).
        ! Precondition: N <= Pow(10, M + 1).
        ! Note: M = Kappa

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: N
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! The numbers below are chosen such that:
        !   1. floor(n/d) = floor(nm / 2^k) where d=10 or d=100,
        !   2. nm mod 2^k < m if and only if n is divisible by d,
        ! where m is magic_number, k is shift_amount
        ! and d is divisor.
        !
        ! Item 1 is a common technique of replacing division by a constant with
        ! multiplication, see e.g. "Division by Invariant Integers Using
        ! Multiplication" by Granlund and Montgomery (1994). magic_number (m) is set
        ! to ceil(2^k/d) for large enough k.
        ! The idea for item 2 originates from Schubfach.

        N = N * Magic_Number
        Flag = IAND(N, Comparison_Mask) .ULT. Magic_Number
        N = SHIFTR(N, Info_Shift_Amount)

        RETURN

    END FUNCTION Is_Divisible_By_Pow10

    !**************************************************************************

    FUNCTION Divide_By_10_To_Kappa_Plus_1(N) RESULT(M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute M = Floor(N / 10**(Kappa + 1))

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: N
        tUIntType               :: M

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

#ifdef  tFloat_is_tQuad
        M = SHIFTR(UMul256_Upper128(N, DivM), DivS)
#else
#ifdef  tFloat_is_tDouble
        M = SHIFTR(UMul128_Upper64(N, DivM), DivS)
#else
        M = ToInt32(SHIFTR(ToInt64(N)*DivM, DivS))
#endif
#endif

        RETURN

    END FUNCTION Divide_By_10_To_Kappa_Plus_1

    !**************************************************************************

    FUNCTION Compute_Delta(Pow10, Beta) RESULT(Delta)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute Delta

    !** SUBROUTINE ARGUMENT DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,  INTENT(IN)    :: Pow10
#endif
#endif
        tSInt32,  INTENT(IN)    :: Beta
        tSInt32                 :: Delta

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))

#ifdef  tFloat_is_tQuad
        Delta = ToU32(SHIFTR(Pow10(3), (64 - 1 - Beta)))
#else
#ifdef  tFloat_is_tDouble
        Delta = ToU32(SHIFTR(Pow10%High, (64 - 1 - Beta)))
#else
        Delta = ToU32(SHIFTR(Pow10, (64 - 1 - Beta)))
#endif
#endif

#undef  ToU32
        RETURN

    END FUNCTION Compute_Delta

    !**************************************************************************

    SUBROUTINE Shorter_Interval_Case(Exponent, SigDec, ExpDec)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a binary floating point number into the decimal representation
        ! for shorter interval case.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)   :: Exponent ! The decoded value of exponent in binary
        tUIntType, INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
        tSInt32,   INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Minus_K, Beta
        tUIntType   :: Xi, Zi
#ifdef  tFloat_is_tQuad
        tUInt64     :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128    :: Pow10
#else
        tUInt64     :: Pow10
#endif
#endif

    !** FLOW:

        ! Compute k and beta
        Minus_K = Floor_Log10_ThreeQuartersPow2(Exponent)
        Beta = Exponent + Floor_Log2_Pow10(-Minus_K)

        ! Compute Xi and Zi
#ifdef  tFloat_is_tQuad
        Pow10 = Get_Pow10_256Bits(-Minus_K)
        IF (Minus_K > 0) Pow10(0) = Pow10(0) + 1_kInt64
#else
#ifdef  tFloat_is_tDouble
        Pow10 = Get_Pow10_128Bits(-Minus_K)
        IF (Minus_K > 0) Pow10%Low = Pow10%Low + 1_kInt64
#else
        Pow10 = Get_Pow10_64Bits(-Minus_K)
        IF (Minus_K > 0) Pow10 = Pow10 + 1_kInt64
#endif
#endif
        Xi = Compute_Left_Endpoint(Pow10, Beta)
        Zi = Compute_Right_Endpoint(Pow10, Beta)

        ! If the left endpoint is not an integer, increase it
        IF (.NOT.Is_Left_Endpoint_Integer(Exponent)) Xi = Xi + OneUInt

        ! Try bigger divisor
        SigDec = Zi .UDIV. TenUInt

        ! If succeed, remove trailing zeros if necessary and return
        IF (SigDec * TenUInt .UGE. Xi) THEN
            ExpDec = Minus_K + 1
            ExpDec = ExpDec
            RETURN
        END IF

        ! Otherwise, compute the round-up of y
        SigDec = Compute_Round_Up(Pow10, Beta)
        ExpDec = Minus_K

        ! When tie occurs, choose one of them according to the rule
        IF (Exponent >= Shorter_Interval_Tie_Lower_Threshold .AND. &
            Exponent <= Shorter_Interval_Tie_Upper_Threshold) THEN
            IF (IAND(SigDec, OneUInt) /= ZeroUInt) SigDec = SigDec - OneUInt    ! Round to even.
        ELSEIF (SigDec .ULT. Xi) THEN
            SigDec = SigDec + OneUInt
        END IF

        RETURN

    END SUBROUTINE Shorter_Interval_Case

    !**************************************************************************

    FUNCTION Compute_Left_Endpoint(Pow10, Beta) RESULT(X)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the left end point (Xi) for the shorter interval case

    !** SUBROUTINE ARGUMENT DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,  INTENT(IN)    :: Pow10
#endif
#endif
        tSInt32,  INTENT(IN)    :: Beta
        tUIntType               :: X

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt128    :: Pow10Hi
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        Pow10Hi = UInt128(Pow10(3), Pow10(2))
        X = SHIFTR((Pow10Hi - SHIFTR(Pow10Hi, (SignificandBits + 2))), &
                   (TotalBits - SignificandBits - 1 - Beta))
#else
#ifdef  tFloat_is_tDouble
        X = SHIFTR((Pow10%High - SHIFTR(Pow10%High, (SignificandBits + 2))), &
                   (TotalBits - SignificandBits - 1 - Beta))
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        X = ToU32(SHIFTR((Pow10 - SHIFTR(Pow10, (SignificandBits + 2))), (TotalBits*2 - SignificandBits - 1 - Beta)))
#undef  ToU32
#endif
#endif

        RETURN

    END FUNCTION Compute_Left_Endpoint

    !**************************************************************************

    FUNCTION Compute_Right_Endpoint(Pow10, Beta) RESULT(Z)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the right end point (Zi) for the shorter interval case

    !** SUBROUTINE ARGUMENT DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,  INTENT(IN)    :: Pow10
#endif
#endif
        tSInt32,  INTENT(IN)    :: Beta
        tUIntType               :: Z

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt128    :: Pow10Hi
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        Pow10Hi = UInt128(Pow10(3), Pow10(2))
        Z = SHIFTR((Pow10Hi + SHIFTR(Pow10Hi, (SignificandBits + 1))), &
                   (TotalBits - SignificandBits - 1 - Beta))
#else
#ifdef  tFloat_is_tDouble
        Z = SHIFTR((Pow10%High + SHIFTR(Pow10%High, (SignificandBits + 1))), &
                   (TotalBits - SignificandBits - 1 - Beta))
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        Z = ToU32(SHIFTR((Pow10 + SHIFTR(Pow10, (SignificandBits + 1))), (TotalBits*2 - SignificandBits - 1 - Beta)))
#undef  ToU32
#endif
#endif

        RETURN

    END FUNCTION Compute_Right_Endpoint

    !**************************************************************************

    FUNCTION Compute_Round_Up(Pow10, Beta) RESULT(Y)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the rounded-up value of Yi for the shorter interval case

    !** SUBROUTINE ARGUMENT DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
        tUInt128,  INTENT(IN)   :: Pow10
#else
        tUInt64,  INTENT(IN)    :: Pow10
#endif
#endif
        tSInt32,  INTENT(IN)    :: Beta
        tUIntType               :: Y

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt128    :: Pow10Hi
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        Pow10Hi = UInt128(Pow10(3), Pow10(2))
        Y = SHIFTR(SHIFTR(Pow10Hi, (TotalBits - SignificandBits - 2 - Beta)) + OneU128, 1)
#else
#ifdef  tFloat_is_tDouble
        Y = SHIFTR(SHIFTR(Pow10%High, (TotalBits - SignificandBits - 2 - Beta)) + 1_kInt64, 1)
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        Y = SHIFTR(ToU32(SHIFTR(Pow10, (TotalBits*2 - SignificandBits - 2 - Beta))) + 1, 1)
#undef  ToU32
#endif
#endif

        RETURN

    END FUNCTION Compute_Round_Up

    !**************************************************************************

    FUNCTION Is_Left_Endpoint_Integer(E) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the left end point (Xi) is an integer for the shorter interval case

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: E
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = ((E >= Case_Shorter_Interval_Left_Endpoint_Lower_Threshold) .AND. &
                (E <= Case_Shorter_Interval_Left_Endpoint_Upper_Threshold))

        RETURN

    END FUNCTION Is_Left_Endpoint_Integer

    !**************************************************************************

#ifdef  tFloat_is_tQuad

    SUBROUTINE UMul384_Upper256(X128, Y64, U256Hi, U256Lo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute upper 256 bits of multiplication of a 128-bit unsigned integer and
        ! a 256-bit unsigned integer.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: X128              ! a 128-bit unsigned integer
        tUInt64,  INTENT(IN)   :: Y64(0:3)          ! a 256-bit unsigned integer in little-endian order
        tUInt128, INTENT(OUT)  :: U256Hi, U256Lo    ! upper and lower parts of the upper 256 bits of the result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: X64(0:1)
        tUInt64     :: Z64(0:5)

    !** FLOW

        ! get input
        X64(0) = X128%Low
        X64(1) = X128%High

        ! perform multiplication
        CALL MultiplyBasic(X64, 2, Y64, 4, Z64)

        ! set output
        U256Hi = UInt128(Z64(5), Z64(4))
        U256Lo = UInt128(Z64(3), Z64(2))

        RETURN

    END SUBROUTINE UMul384_Upper256

    !**************************************************************************

    SUBROUTINE UMul384_Lower256(X128, Y64, U256Hi, U256Lo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute lower 256 bits of multiplication of a 128-bit unsigned integer and
        ! a 256-bit unsigned integer.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: X128              ! a 128-bit unsigned integer
        tUInt64,  INTENT(IN)   :: Y64(0:3)          ! a 256-bit unsigned integer in little-endian order
        tUInt128, INTENT(OUT)  :: U256Hi, U256Lo    ! upper and lower parts of the lower 256 bits of the result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: X64(0:1)
        tUInt64     :: Z64(0:5)

    !** FLOW

        ! get input
        X64(0) = X128%Low
        X64(1) = X128%High

        ! perform multiplication
        CALL MultiplyBasic(X64, 2, Y64, 4, Z64)

        ! set output
        U256Hi = UInt128(Z64(3), Z64(2))
        U256Lo = UInt128(Z64(1), Z64(0))

        RETURN

    END SUBROUTINE UMul384_Lower256

    !**************************************************************************

#endif

END SUBROUTINE Bin2Dec_DragonBox

!******************************************************************************

SUBROUTINE Bin2Dec_Ryu(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a binary floating point number into the shortest and correctly
    ! rounded decimal representation based on the Ryu algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    tSInt32,   INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    tUIntType, INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    tSInt32,   INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    tUIntType, INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    tSInt32,   INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, PARAMETER  :: Log2Base5 = LOG(2.0_kDP)/LOG(5.0_kDP)
    tSInt32, PARAMETER  :: QLimit    = FLOOR(Log2Base5*BinaryPrecision)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: E2
    tUIntType   :: M2
    tLogical    :: Even, AcceptBounds
    tUIntType   :: MV, MP, MM
    tUIntType   :: Vr, Vp, Vm
    tUIntType   :: VrDiv10, VpDiv10, VmDiv10
    tUIntType   :: VrMod10, VmMod10
    tSInt32     :: E10, Q
    tLogical    :: VmIsTrailingZeros, VrIsTrailingZeros, DecrementVp
    tUInt32     :: Removed, LastRemovedDigit

!** FLOW

    ! We subtract 2 in all cases so that the bounds computation has 2 additional bits.
    E2 = ExpBin - 2
    M2 = SigBin

    Even = IAND(M2, OneUInt) == ZeroUInt
    AcceptBounds = Even

    ! Step 2: Determine the interval of legal decimal representations.
    ! Implicit bool -> int conversion. True is 1, false is 0.
    MV = M2 * FourUInt
    MP = MV + TwoUInt
    ! check whether Cb is closer to the lower bound
    IF ((SigRaw == ZeroUInt).AND.(ExpRaw > 1)) THEN
        ! closer to the lower bound; irregular spacing
        MM = MV - OneUInt
    ELSE
        ! not closer to the lower bound; regular spacing
        MM = MV - TwoUInt
    END IF

    ! Step 3: Convert to a decimal power base using 128-bit arithmetic.
    VmIsTrailingZeros = FalseVal
    VrIsTrailingZeros = FalseVal
    DecrementVp = FalseVal

    IF (E2 >= 0) THEN
        ! We need (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2
        ! and we need to remove at least Q' = LOG10(2**E2) digits from the
        ! scaled values Vm, Vr, Vp, i.e. we want to compute
        !  (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**Q'
        !               = (MM, MV, MP) * 2**E2 / 10**(E10)
        !               = (MM, MV, MP) * 5**(-E10) / 2**(E10 - E2)
        ! However, to correctly round the result we need to know the value of
        ! the last removed digit.  We therefore remove only Q = Q' - 1 digits in
        ! the first step and make sure that we execute the loop below at least
        ! once and determine the correct value of the last removed digit.
        Q = Floor_Log10_Pow2(E2)
#ifndef  tFloat_is_tSingle
        IF (E2 > 3) Q = Q - 1       ! == MAX(0, Q' - 1)
#endif
        E10 = Q

        ! Determine whether all the removed digits are 0.
        !
        ! Z(X, E2, Q) = MOD((X * 2**E2), 10**Q) == 0
        !             = P10(X * 2**E2) >= Q
        !             = MIN(P2(X) + P2(E2), P5(X)) >= Q
        !             = P2(X) + E2 >= Q and P5(x) >= Q
        !             = P5(X) >= Q
        !             = MOD(X, 5**Q) == 0

        ! QLimit = FLOOR(LOG5(2**BinaryPrecision))
        IF (Q <= QLimit) THEN
            ! Only one of MP, MV, and MM can be a multiple of 5, if any.
            IF (UMOD(MV, FiveUInt) == ZeroUInt) THEN
                VrIsTrailingZeros = Is_Multiple_Of_Pow5(MV, Q - 1)
            ELSEIF (AcceptBounds) THEN
                ! Same as min(E2 + (~MM & 1), Pow5Factor(MM)) >= Q
                ! <=> E2 + (~MM & 1) >= Q && Pow5Factor(MM) >= Q
                ! <=> true && Pow5Factor(MM) >= Q, since E2 >= Q.
                VmIsTrailingZeros = Is_Multiple_Of_Pow5(MM, Q)
            ELSE
                ! Same as min(E2 + 1, Pow5Factor(MP)) >= Q.
                ! Vp -= Is_Multiple_Of_Pow5(MP, Q)
                DecrementVp = Is_Multiple_Of_Pow5(MP, Q)
            END IF
        END IF
    ELSE
        ! We need (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**E2
        ! and we need to remove at least Q' = LOG10(5**-E2) digits from the
        ! scaled values Vm, Vr, Vp, i.e. we want to compute
        !  (Vm, Vr, Vp) = (MM, MV, MP) * 2**E2 / 10**(E2 + Q')
        !               = (MM, MV, MP) * 2**E2 / 10**(E10),
        !               = (MM, MV, MP) * 5**(-E10) / 2**(E10 - E2)
        Q = Floor_Log10_Pow5(-E2)
#ifndef  tFloat_is_tSingle
        IF (-E2 > 1) Q = Q - 1      ! == MAX(0, Q' - 1)
#endif
        E10 = Q + E2

        ! Determine whether all the removed digits are 0.
        !
        ! Z(X, E2, Q) = MOD((X * 5**-E2), 10**Q) == 0
        !             = MIN(P2(X), P5(X) - E2) >= Q
        !             = P2(X) >= Q and P5(X) - E2 >= Q
        !             = P2(X) >= Q
        !             = MOD(X, 2**Q) == 0
        IF (Q <= 1) THEN
            ! {Vr,Vp,Vm} is trailing zeros if {MV,MP,MM} has at least Q trailing 0 bits.
            ! MV = 4 M2, so it always has at least two trailing 0 bits.
            VrIsTrailingZeros = TrueVal
            IF (AcceptBounds) THEN
                ! MM = MV - 1 - MMShift, so it has 1 trailing 0 bit iff MMShift == 1.
                VmIsTrailingZeros = (MM == (MV - TwoUInt))
            ELSE
                ! MP = MV + 2, so it always has at least one trailing 0 bit.
                DecrementVp = TrueVal
            END IF
        ELSEIF (Q < (TotalBits-1)) THEN ! TODO(ulfjack): Use a tighter bound here.
            ! We need to compute min(ntz(MV), Pow5Factor(MV) - E2) >= Q-1
            ! <=> ntz(MV) >= Q-1  &&  Pow5Factor(MV) - E2 >= Q-1
            ! <=> ntz(MV) >= Q-1    (E2 is negative and -E2 >= Q)
            ! <=> (MV & ((1 << (Q-1)) - 1)) == 0
            ! We also need to make sure that the left shift does not overflow.
#ifndef  tFloat_is_tDouble
            VrIsTrailingZeros = Is_Multiple_Of_Pow2(MV, Q - 1)
#else
            VrIsTrailingZeros = Is_Multiple_Of_Pow2(MV, Q)
#endif
        END IF
    END IF

    CALL MulPow5DivPow2(MM, MV, MP, -E10, E10-E2, Vm, Vr, Vp)
    IF (DecrementVp) Vp = Vp - OneUInt

    ! Step 4: Find the shortest decimal representation in the interval of legal representations.
    Removed = 0
    LastRemovedDigit = 0

    VpDiv10 = Divide_By_Pow10Factor(Vp, 10)
    CALL DivMod_By_Pow10Factor(Vm, 10, VmDiv10, VmMod10)
    DO WHILE (VpDiv10 .UGT. VmDiv10)
        VmIsTrailingZeros = VmIsTrailingZeros .AND. (VmMod10 == ZeroUInt)
        VrIsTrailingZeros = VrIsTrailingZeros .AND. (LastRemovedDigit == 0)
        CALL DivMod_By_Pow10Factor(Vr, 10, VrDiv10, VrMod10)
        LastRemovedDigit = VrMod10
        Vr = VrDiv10
        Vp = VpDiv10
        VpDiv10 = Divide_By_Pow10Factor(Vp, 10)
        Vm = VmDiv10
        CALL DivMod_By_Pow10Factor(Vm, 10, VmDiv10, VmMod10)
        Removed = Removed + 1
    END DO

    IF (VmIsTrailingZeros) THEN
        DO WHILE (Mod_By_Pow10Factor(Vm, 10) == ZeroUInt)
            VrIsTrailingZeros = VrIsTrailingZeros .AND. (LastRemovedDigit == 0)
            CALL DivMod_By_Pow10Factor(Vr, 10, VrDiv10, VrMod10)
            LastRemovedDigit = VrMod10
            Vr = VrDiv10
            Vp = Divide_By_Pow10Factor(Vp, 10)
            Vm = Divide_By_Pow10Factor(Vm, 10)
            Removed = Removed + 1
        END DO
    END IF

    IF (VrIsTrailingZeros.AND.(LastRemovedDigit == 5).AND.(Mod_By_Pow10Factor(Vr, 2) == ZeroUInt)) THEN
        ! Round even if the exact numbers is .....50..0.
        LastRemovedDigit = 4
    END IF

    ! We need to take Vr+1 if Vr is outside bounds or we need to round up.
    SigDec = Vr

    IF (((Vr == Vm).AND.((.NOT.AcceptBounds).OR.(.NOT.VmIsTrailingZeros))) &
        .OR.(LastRemovedDigit >= 5)) THEN
        SigDec = SigDec + OneUInt
    END IF
    ExpDec = E10 + Removed

    RETURN

    CONTAINS

    SUBROUTINE MulPow5DivPow2(U, V, W, E5, E2, A, B, C)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply by power of 5 and divide by power of 2

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: U, V, W
        tSInt32,   INTENT(IN)   :: E5, E2
        tUIntType, INTENT(OUT)  :: A, B, C

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Shift
#ifdef  tFloat_is_tQuad
        tUInt64     :: U64(0:1), V64(0:1), W64(0:1)
        tUInt64     :: Pow5(0:3)    ! in little endian order; most significant byte is 3
        tUInt64     :: A64(0:5), B64(0:5), C64(0:5)
#else
#ifdef  tFloat_is_tDouble
        tUInt64     :: U64(0:0), V64(0:0), W64(0:0)
        tUInt64     :: Pow5(0:1)    ! in little endian order; most significant byte is 1
        tUInt128    :: Pow5_128
        tUInt64     :: A64(0:2), B64(0:2), C64(0:2)
#else
        tUInt64     :: Pow5
#endif
#endif

    !** FLOW

        Shift = E2 - (Floor_Log2_Pow5(E5) + 1 - BitsPerPow5)

#ifdef  tFloat_is_tQuad
        Pow5 = Get_Pow10_256Bits(E5)
        IF ((E5 < Pow10_Min_Exact_Exp).OR.(E5 > Pow10_Max_Exact_Exp)) Pow5(0) = Pow5(0) + 1_kInt64
        U64(0) = U%Low
        U64(1) = U%High
        CALL Multiply_N_ShiftRight(U64, 2, Pow5, 4, Shift, A64)
        A = UInt128(A64(1), A64(0))
        V64(0) = V%Low
        V64(1) = V%High
        CALL Multiply_N_ShiftRight(V64, 2, Pow5, 4, Shift, B64)
        B = UInt128(B64(1), B64(0))
        W64(0) = W%Low
        W64(1) = W%High
        CALL Multiply_N_ShiftRight(W64, 2, Pow5, 4, Shift, C64)
        C = UInt128(C64(1), C64(0))
#else
#ifdef  tFloat_is_tDouble
        Pow5_128 = Get_Pow10_128Bits(E5)
        Pow5(0) = Pow5_128%Low
        Pow5(1) = Pow5_128%High
        IF ((E5 < Pow10_Min_Exact_Exp).OR.(E5 > Pow10_Max_Exact_Exp)) Pow5(0) = Pow5(0) + 1_kInt64
        U64(0) = U
        CALL Multiply_N_ShiftRight(U64, 1, Pow5, 2, Shift, A64)
        A = A64(0)
        V64(0) = V
        CALL Multiply_N_ShiftRight(V64, 1, Pow5, 2, Shift, B64)
        B = B64(0)
        W64(0) = W
        CALL Multiply_N_ShiftRight(W64, 1, Pow5, 2, Shift, C64)
        C = C64(0)
#else
#define ToU32(X)    ToInt32(IAND(X, ToInt64(Z'00000000FFFFFFFF')))
        Pow5 = Get_Pow10_64Bits(E5)
        IF ((E5 < Pow10_Min_Exact_Exp).OR.(E5 > Pow10_Max_Exact_Exp)) Pow5 = Pow5 + 1_kInt64
        A = MulShift32(U, Pow5, Shift)
        B = MulShift32(V, Pow5, Shift)
        C = MulShift32(W, Pow5, Shift)
#undef  ToU32
#endif
#endif

        RETURN

    END SUBROUTINE MulPow5DivPow2

    !**************************************************************************

#ifdef  tFloat_is_tSingle

    FUNCTION MulShift32(X, Y, J) RESULT(Z)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication of X and Y and then shift the product by J

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN) :: X
        tUInt64, INTENT(IN) :: Y
        tSInt32, INTENT(IN) :: J
        tUInt32             :: Z

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: A, B, Sum
        tSInt32     :: Shift

    !** FLOW

#define LoU32(X)    IAND(X, ToInt64(Z'00000000FFFFFFFF'))
#define HiU32(X)    SHIFTR(X, 32)

        A = ToUnsignedLong(X)*LoU32(Y)
        B = ToUnsignedLong(X)*HiU32(Y)
        Sum = B + HiU32(A)
        Shift = J - 32
        Z = LoU32(SHIFTR(Sum, Shift))

        RETURN

#undef  LoU32
#undef  HiU32

    END FUNCTION MulShift32
#endif

    !**************************************************************************

    FUNCTION Is_Multiple_Of_Pow5(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given value is divisible by 5**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: Value
        tUInt32,   INTENT(IN)   :: Exp
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

#ifdef  tFloat_is_tQuad
        IF (Exp <= MaxExp_ModInv5) THEN
            Flag = IsMultipleOfPow5_128Bits(Value, Exp)
        ELSE
            Flag = Pow5Factor_128Bits(Value) .UGE. Exp
        END IF
#else
#ifdef  tFloat_is_tDouble
        IF (Exp <= MaxExp_ModInv5) THEN
            Flag = IsMultipleOfPow5_64Bits(Value, Exp)
        ELSE
            Flag = Pow5Factor_64Bits(Value) .UGE. Exp
        END IF
#else
        IF (Exp <= MaxExp_ModInv5) THEN
            Flag = IsMultipleOfPow5_32Bits(Value, Exp)
        ELSE
            Flag = Pow5Factor_32Bits(Value) .UGE. Exp
        END IF
#endif
#endif

        RETURN

    END FUNCTION Is_Multiple_Of_Pow5

    !**************************************************************************

    FUNCTION Is_Multiple_Of_Pow2(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given value is divisible by 2**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: Value
        tUInt32,   INTENT(IN)   :: Exp
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = IAND(Value, SHIFTL(OneUInt, Exp) - OneUInt) == ZeroUInt

        RETURN

    END FUNCTION Is_Multiple_Of_Pow2

    !**************************************************************************

#ifdef  tFloat_is_tQuad

    SUBROUTINE DivMod_By_Pow10Factor(X, Y, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division (X .UDIV. Y) by 5 or 10, and modulus (UMOD(X, Y)) of 5 or 10 (i.e. Y is 5 or 10).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: X    ! dividend
        tUInt32,  INTENT(IN)    :: Y    ! divisor; must be 5 or 10
        tUInt128, INTENT(OUT)   :: Q    ! quotient
        tUInt128, INTENT(OUT)   :: R    ! remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: CST     = ToInt64(Z'3333333333333333')
        tUInt32, PARAMETER  :: LookUp1 = ToInt32(B'111100000')          ! 480
        tUInt64, PARAMETER  :: LookUp2 = ToInt64(O'321043210')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: AddendQ
        tUInt64     :: QHi, QLo
        tUInt64     :: R64, RHi, RLo, AddendR

    !** FLOW

        ! Ryu performs 128-bit division only by 5 and 10, so that's what we
        ! implement.  The strategy here is to relate division of x with that of
        ! x.hi and x.lo separately.
        ! assert(y == 5 || y == 10);
        ! The following implements division by 5 and 10.  In either case, we
        ! first compute division by 5:
        !   x/5 = (x.hi*2^64 + x.lo)/5
        !       = (x.hi*(2^64-1) + x.hi + x.lo)/5
        !       = x.hi*((2^64-1)/5) + (x.hi + x.lo)/5 since CST=(2^64-1)/5 is exact
        !       = x.hi*CST + x.hi/5 + x.lo/5 + ((x.lo%5) + (x.hi%5) >= 5)
        ! We go a step further and replace the last adjustment term with a
        ! lookup table, which we encode as a binary literal.  This seems to
        ! yield smaller code on x86 at least.
        CALL UMul128(X%High, CST, Q%High, Q%Low)
        CALL UDivMod(X%High, 5_kInt64, QHi, RHi)
        CALL UDivMod(X%Low,  5_kInt64, QLo, RLo)
        CALL Add(Q, QHi + QLo)
        AddendQ = IAND(SHIFTR(LookUp1, RHi + RLo), 1)
        CALL Add(Q, AddendQ)
        ! The following implements modulus by 5 and 10.  In either case,
        ! we first compute modulus by 5:
        !   x (mod 5) = x.hi*2^64 + x.lo (mod 5)
        !             = x.hi + x.lo (mod 5) since 2^64 == 1 (mod 5)
        ! So the straightforward implementation would be
        !   ((x.hi % 5) + (x.lo % 5)) % 5
        ! But we go a step further and replace the outermost % with a
        ! lookup table:
        !             = {0,1,2,3,4,0,1,2,3}[(x.hi % 5) + (x.lo % 5)] (mod 5)
        ! which we encode as an octal literal.
        R64 = IAND(SHIFTR(Lookup2, 3_kInt64*(RHi + RLo)), 7_kInt64)
        R = UInt128(0_kInt64, R64)
        IF (Y == 10) THEN
            ! x % 10 = (x % 5)      if x / 5 is even
            !          (x % 5) + 5  if x / 5 is odd
            ! The compiler should be able to CSE the below computation of x/5 and
            ! the above modulus operations with a nearby inlined computation of x/10.
            AddendR = 5_kInt64 * IAND(Q%Low, 1_kInt64)
            CALL Add(R, AddendR)
            Q = ShiftROnce(Q)
        END IF

        RETURN

    END SUBROUTINE DivMod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Divide_By_Pow10Factor(X, Y) RESULT(Q)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division (X .UDIV. Y) by 5 or 10 (i.e. Y is 5 or 10).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: X
        tUInt32,  INTENT(IN)    :: Y    ! must be 5 or 10
        tUInt128                :: Q

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: CST    = ToInt64(Z'3333333333333333') ! = MaxU64 .UDIV. 5_kInt64
        tUInt32, PARAMETER  :: LookUp = ToInt32(B'111100000')     ! = 480

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: Addend
        tUInt64     :: QHi, QLo, RHi, RLo

    !** FLOW

        ! compute division by 5:
        !   x/5 = (x.hi*2^64 + x.lo)/5
        !       = (x.hi*(2^64-1) + x.hi + x.lo)/5
        !       = x.hi*((2^64-1)/5) + (x.hi + x.lo)/5 since CST=(2^64-1)/5 is exact
        !       = x.hi*CST + x.hi/5 + x.lo/5 + ((x.lo%5) + (x.hi%5) >= 5)
        ! We go a step further and replace the last adjustment term with a
        ! lookup table, which we encode as a binary literal.
        CALL UMul128(X%High, CST, Q%High, Q%Low)
        CALL UDivMod(X%High, 5_kInt64, QHi, RHi)
        CALL UDivMod(X%Low,  5_kInt64, QLo, RLo)
        CALL Add(Q, QHi + QLo)
        Addend = IAND(SHIFTR(LookUp, RHi + RLo), 1)
        CALL Add(Q, Addend)
        IF (Y == 10) Q = ShiftROnce(Q)

        RETURN

    END FUNCTION Divide_By_Pow10Factor

    !**************************************************************************

    FUNCTION Mod_By_Pow10Factor(X, Y) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulus (UMOD(X, Y)) of 2, 5 or 10 (i.e. Y is 2 or 5 or 10.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: X
        tUInt32,  INTENT(IN)    :: Y    ! must be 2, 5 or 10
        tUInt128                :: R

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: CST     = ToInt64(Z'3333333333333333')
        tUInt32, PARAMETER  :: LookUp1 = ToInt32(B'111100000') ! 480
        tUInt64, PARAMETER  :: LookUp2 = ToInt64(O'321043210')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32         :: AddendQ
        tUInt64         :: QHi, QLo
        tUInt64         :: R64, RHi, RLo, AddendR
        tUInt128        :: Q

    !** FLOW

        ! Ryu performs 128-bit modulus only by 2, 5 and 10, so that's what we
        ! implement.  The strategy here is to relate modulus of x with that of
        ! x.hi and x.lo separately.
        IF (Y == 2) THEN
            R = IAND(X, OneU128)
            RETURN
        END IF
        ! assert(y == 5 || y == 10);
        ! The following implements modulus by 5 and 10.  In either case,
        ! we first compute modulus by 5:
        !   x (mod 5) = x.hi*2^64 + x.lo (mod 5)
        !             = x.hi + x.lo (mod 5) since 2^64 == 1 (mod 5)
        ! So the straightforward implementation would be
        !   ((x.hi % 5) + (x.lo % 5)) % 5
        ! But we go a step further and replace the outermost % with a
        ! lookup table:
        !             = {0,1,2,3,4,0,1,2,3}[(x.hi % 5) + (x.lo % 5)] (mod 5)
        ! which we encode as an octal literal.
        IF (Y == 5) THEN
            R64 = IAND(SHIFTR(Lookup2, 3_kInt64*(UMOD(X%High, 5_kInt64) + UMOD(X%Low, 5_kInt64))), 7_kInt64)
            R = UInt128(0_kInt64, R64)
        ELSE    ! Y == 10
            ! compute division by 5:
            !   x/5 = (x.hi*2^64 + x.lo)/5
            !       = (x.hi*(2^64-1) + x.hi + x.lo)/5
            !       = x.hi*((2^64-1)/5) + (x.hi + x.lo)/5 since CST=(2^64-1)/5 is exact
            !       = x.hi*CST + x.hi/5 + x.lo/5 + ((x.lo%5) + (x.hi%5) >= 5)
            ! We go a step further and replace the last adjustment term with a
            ! lookup table, which we encode as a binary literal.
            CALL UMul128(X%High, CST, Q%High, Q%Low)
            CALL UDivMod(X%High, 5_kInt64, QHi, RHi)
            CALL UDivMod(X%Low,  5_kInt64, QLo, RLo)
            CALL Add(Q, QHi + QLo)
            AddendQ = IAND(SHIFTR(LookUp1, RHi + RLo), 1)
            CALL Add(Q, AddendQ)
            !--------------------------
            R64 = IAND(SHIFTR(Lookup2, 3_kInt64*(RHi + RLo)), 7_kInt64)
            R = UInt128(0_kInt64, R64)
            ! x % 10 = (x % 5)      if x / 5 is even
            !          (x % 5) + 5  if x / 5 is odd
            ! The compiler should be able to CSE the below computation of x/5 and
            ! the above modulus operations with a nearby inlined computation of x/10.
            AddendR = 5_kInt64 * IAND(Q%Low, 1_kInt64)
            CALL Add(R, AddendR)
        END IF

        RETURN

    END FUNCTION Mod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Pow5Factor_128Bits(Value) RESULT(Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the factor of power of 5

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: Value
        tUInt32                 :: Count

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128    :: Dividend
        tUInt128    :: Quotient, Remainder

    !** FLOW

        Count = 0
        Dividend = Value
        DO WHILE (Dividend /= ZeroU128)
            CALL DivMod_By_Pow10Factor(Dividend, 5, Quotient, Remainder)
            IF (Remainder /=  ZeroU128) RETURN
            Dividend = Quotient
            Count = Count + 1
        END DO
        Count = 0

        RETURN

    END FUNCTION Pow5Factor_128Bits

    !**************************************************************************

    FUNCTION IsMultipleOfPow5_128Bits(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given value is divisible by 5**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: Value
        tUInt32,  INTENT(IN)    :: Exp
        tLogical                :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64,   PARAMETER    :: ModInv5(0:3,0:MaxExp_ModInv5) = RESHAPE([  &
             ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000000000'),  &
              ToInt64(Z'FFFFFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFF'), &
             ToInt64(Z'CCCCCCCCCCCCCCCD'), ToInt64(Z'CCCCCCCCCCCCCCCC'),  &
              ToInt64(Z'3333333333333333'), ToInt64(Z'3333333333333333'), &
             ToInt64(Z'8F5C28F5C28F5C29'), ToInt64(Z'28F5C28F5C28F5C2'),  &
              ToInt64(Z'A3D70A3D70A3D70A'), ToInt64(Z'0A3D70A3D70A3D70'), &
             ToInt64(Z'1CAC083126E978D5'), ToInt64(Z'6E978D4FDF3B645A'),  &
              ToInt64(Z'ED916872B020C49B'), ToInt64(Z'020C49BA5E353F7C'), &
             ToInt64(Z'D288CE703AFB7E91'), ToInt64(Z'495182A9930BE0DE'),  &
              ToInt64(Z'95E9E1B089A02752'), ToInt64(Z'0068DB8BAC710CB2'), &
             ToInt64(Z'5D4E8FB00BCBE61D'), ToInt64(Z'DB76B3BB83CF2CF9'),  &
              ToInt64(Z'8461F9F01B866E43'), ToInt64(Z'0014F8B588E368F0'), &
             ToInt64(Z'790FB65668C26139'), ToInt64(Z'C57E23F24D8FD5CB'),  &
              ToInt64(Z'4DAD31FCD24E160D'), ToInt64(Z'000431BDE82D7B63'), &
             ToInt64(Z'E5032477AE8D46A5'), ToInt64(Z'C1193A63A91CC45B'),  &
              ToInt64(Z'42BC3D3290760469'), ToInt64(Z'0000D6BF94D5E57A'), &
             ToInt64(Z'C767074B22E90E21'), ToInt64(Z'F36B7213EE9F5A78'),  &
              ToInt64(Z'73BF3F70834ACDAE'), ToInt64(Z'00002AF31DC46118'), &
             ToInt64(Z'8E47CE423A2E9C6D'), ToInt64(Z'97157D372FB9787E'),  &
              ToInt64(Z'4A59731680A88F89'), ToInt64(Z'0000089705F4136B'), &
             ToInt64(Z'4FA7F60D3ED61F49'), ToInt64(Z'516AB2A4A3251819'),  &
              ToInt64(Z'DBAB7D6AE6881CB5'), ToInt64(Z'000001B7CDFD9D7B'), &
             ToInt64(Z'0FEE64690C913975'), ToInt64(Z'76AEF08753D43805'),  &
              ToInt64(Z'92557F7BC7B4D28A'), ToInt64(Z'00000057F5FF85E5'), &
             ToInt64(Z'3662E0E1CF503EB1'), ToInt64(Z'B156301B10C40B34'),  &
              ToInt64(Z'EA11197F27F0F6E8'), ToInt64(Z'000000119799812D'), &
             ToInt64(Z'A47A2CF9F6433FBD'), ToInt64(Z'2377A3389CF4023D'),  &
              ToInt64(Z'2ED0384CA19697C8'), ToInt64(Z'0000000384B84D09'), &
             ToInt64(Z'54186F653140A659'), ToInt64(Z'0717ED71B8FD9A0C'),  &
              ToInt64(Z'095CD80F538484C1'), ToInt64(Z'00000000B424DC35'), &
             ToInt64(Z'7738164770402145'), ToInt64(Z'CE37FC49F1CC5202'),  &
              ToInt64(Z'CEAC2B3643E74DC0'), ToInt64(Z'0000000024075F3D'), &
             ToInt64(Z'E4A4D1417CD9A041'), ToInt64(Z'F60B3275305C1066'),  &
              ToInt64(Z'F6226F0ADA6175F3'), ToInt64(Z'000000000734ACA5'), &
             ToInt64(Z'C75429D9E5C5200D'), ToInt64(Z'6468A3B109AC0347'),  &
              ToInt64(Z'646D496892137DFD'), ToInt64(Z'000000000170EF54'), &
             ToInt64(Z'C1773B91FAC10669'), ToInt64(Z'E0E1BA569B88CD74'),  &
              ToInt64(Z'47490EAE839D7F99'), ToInt64(Z'000000000049C977'), &
             ToInt64(Z'26B172506559CE15'), ToInt64(Z'93605877B8B4F5E4'),  &
              ToInt64(Z'A7DB69561A52B31E'), ToInt64(Z'00000000000EC1E4'), &
             ToInt64(Z'D489E3A9ADDEC2D1'), ToInt64(Z'83E011B18B576460'),  &
              ToInt64(Z'219248446BAA23D2'), ToInt64(Z'000000000002F394'), &
             ToInt64(Z'90E860BB892C8D5D'), ToInt64(Z'4D9336BD1BDE4746'),  &
              ToInt64(Z'A05074DA7BEED3F6'), ToInt64(Z'000000000000971D'), &
             ToInt64(Z'502E79BF1B6F4F79'), ToInt64(Z'DC50A48C38C60E41'),  &
              ToInt64(Z'2010175EE5962A64'), ToInt64(Z'0000000000001E39'), &
             ToInt64(Z'DCD618596BE30FE5'), ToInt64(Z'9276874F3E8E02D9'),  &
              ToInt64(Z'6CD004AC94513BAD'), ToInt64(Z'000000000000060B'), &
             ToInt64(Z'2C2AD1AB7BFA3661'), ToInt64(Z'EA17B4A972E933C5'),  &
              ToInt64(Z'7C299A88EA76A589'), ToInt64(Z'0000000000000135'), &
             ToInt64(Z'08D55D224BFED7AD'), ToInt64(Z'FB9E575516FB70C1'),  &
              ToInt64(Z'E5A1EBB4FBB1544E'), ToInt64(Z'000000000000003D'), &
             ToInt64(Z'01C445D3A8CC9189'), ToInt64(Z'658611776AFF168D'),  &
              ToInt64(Z'612062576589DDA9'), ToInt64(Z'000000000000000C'), &
             ToInt64(Z'CD27412A54F5B6B5'), ToInt64(Z'E11AD04B156637B5'),  &
              ToInt64(Z'79D346DE4781F921'), ToInt64(Z'0000000000000002'), &
             ToInt64(Z'8F6E403BAA978AF1'), ToInt64(Z'F9D229A89DE13E57'),  &
              ToInt64(Z'7EC3DAF941806506'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'E97C733F221E4EFD'), ToInt64(Z'31F6D521B92D0C77'),  &
              ToInt64(Z'195A5EFEA6B34767'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'2EB27D7306D2DC99'), ToInt64(Z'A397C439F1D5CF4B'),  &
              ToInt64(Z'051212FFBAF0A7E1'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'6FBD4C4A34909285'), ToInt64(Z'ED84C0D863912975'),  &
              ToInt64(Z'01039D66589687F9'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'16590F420A835081'), ToInt64(Z'62B42691AD836EB1'),  &
              ToInt64(Z'0033EC47AB514E65'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'9E11CFDA021A434D'), ToInt64(Z'46F0D483891A4956'),  &
              ToInt64(Z'000A6274BBDD0FAD'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'B936C32B9A0540A9'), ToInt64(Z'A7C9C41A4E9EDB77'),  &
              ToInt64(Z'000213B0F25F6989'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'583E2708B8677355'), ToInt64(Z'87F52738761FC57E'),  &
              ToInt64(Z'00006A5696DFE1E8'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'44D93B01BE7B1711'), ToInt64(Z'E7FDD4A4E46CC119'),  &
              ToInt64(Z'0000154484932D2E'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'742B72338C7F049D'), ToInt64(Z'C7FF90EDC748F36B'),  &
              ToInt64(Z'00000440E750A2A2'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'B0D57D3D827FCDB9'), ToInt64(Z'8E66502F8E41CA48'),  &
              ToInt64(Z'000000D9C7DCED53'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'BCF77F72B3B32925'), ToInt64(Z'4FAE100982D9F541'),  &
              ToInt64(Z'0000002B8E5F62AA'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'BF64B316F0BD6EA1'), ToInt64(Z'A98936684D5ECAA6'),  &
              ToInt64(Z'00000008B61313BB'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'BFE0F09E3025E2ED'), ToInt64(Z'21E83E14DC462887'),  &
              ToInt64(Z'00000001BE03D0BF'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'F32CFCEC700793C9'), ToInt64(Z'6D2E72D0F8DAD4E7'),  &
              ToInt64(Z'000000005933F68C'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'CA3C3295B00183F5'), ToInt64(Z'7C3C7D5CFE922A94'),  &
              ToInt64(Z'0000000011D7314F'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'5BA5A3B78999E731'), ToInt64(Z'18D8E5DF661D3BB7'),  &
              ToInt64(Z'0000000003917043'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'ABEDED8B1B852E3D'), ToInt64(Z'9E91C793146C3F24'),  &
              ToInt64(Z'0000000000B6B00D'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'EF2F95E89F1AA2D9'), ToInt64(Z'B9505B1D6A7C0CA0'),  &
              ToInt64(Z'000000000024899C'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'FCA31DFB530553C5'), ToInt64(Z'8B76789F7BB268EC'),  &
              ToInt64(Z'0000000000074EB8'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'98ED6C65770110C1'), ToInt64(Z'1BE47E864BF07B62'),  &
              ToInt64(Z'0000000000017624'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'B82F7C144B00368D'), ToInt64(Z'6BFA7FB475967F13'),  &
              ToInt64(Z'0000000000004AD4'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'24D64C040F000AE9'), ToInt64(Z'AF32198A7DEAE637'),  &
              ToInt64(Z'0000000000000EF7'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'6DC4759A69666895'), ToInt64(Z'230A051BB2C89471'),  &
              ToInt64(Z'00000000000002FE'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'E2C0E45215147B51'), ToInt64(Z'A09B9A9F23C1B749'),  &
              ToInt64(Z'0000000000000099'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'93C02DAA04374BDD'), ToInt64(Z'201F1EECA0C057DB'),  &
              ToInt64(Z'000000000000001E'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'50C0092200D7DBF9'), ToInt64(Z'D3396C95B9C01192'),  &
              ToInt64(Z'0000000000000006'), ToInt64(Z'0000000000000000'), &
             ToInt64(Z'768CCEA066919265'), ToInt64(Z'90A515B78B8CD050'),  &
              ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000000000')], [4, 56])

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Cache(0:3)
        tUInt128    :: ModInverse, MaxQuotient

    !** FLOW

        Cache = ModInv5(:,Exp)
        ModInverse  = UInt128(Cache(1), Cache(0))
        MaxQuotient = UInt128(Cache(3), Cache(2))
        Flag = Value*ModInverse .ULE. MaxQuotient

        RETURN

    END FUNCTION IsMultipleOfPow5_128Bits

    !**************************************************************************

#endif

#ifdef  tFloat_is_tDouble

    SUBROUTINE DivMod_By_Pow10Factor(X, Y, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division by 5 or 10 (i.e. Y is 5 or 10).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: X
        tUInt32, INTENT(IN)     :: Y    ! must be 5 or 10
        tUInt64, INTENT(OUT)    :: Quotient
        tUInt64, INTENT(OUT)    :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: Long_BitSize = BIT_SIZE(1_kInt64) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Q, R

    !** FLOW

        ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Y)), (Long_BitSize-1))
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Y), (Long_BitSize-1)), ToInt64(Y))

        RETURN

    END SUBROUTINE DivMod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Divide_By_Pow10Factor(X, Y) RESULT(Quotient)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division by 5 or 10.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: X
        tUInt32, INTENT(IN) :: Y    ! must be 5 or 10
        tUInt64             :: Quotient

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: Long_BitSize = BIT_SIZE(1_kInt64) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Q, R

    !** FLOW

        ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Y)), (Long_BitSize-1))

        RETURN

    END FUNCTION Divide_By_Pow10Factor

    !**************************************************************************

    FUNCTION Mod_By_Pow10Factor(X, Y) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulus of 2, 5 or 10 (i.e. Y is 2 or 5 or 10.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: X
        tUInt32, INTENT(IN) :: Y    ! must be 2, 5 or 10
        tUInt64             :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: Long_BitSize = BIT_SIZE(1_kInt64) ! 64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Q, R

    !** FLOW

        ! The following algorithm actually can be used for all positive divisor (Y)
        Q = SHIFTL(SHIFTR(X, 1)/Y, 1)
        R = X - Q*Y
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Y), (Long_BitSize-1)), ToInt64(Y))

        RETURN

    END FUNCTION Mod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Pow5Factor_64Bits(Value) RESULT(Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the factor of power of 5

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: Value
        tUInt32                 :: Count

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: MInv5 = ToInt64(Z'CCCCCCCCCCCCCCCD')  ! 14757395258967641293_kInt64
        tUInt64, PARAMETER  :: NDiv5 = ToInt64(Z'3333333333333333')  ! 3689348814741910323_kInt64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Dividend

    !** FLOW

        Count = 0
        IF (Value == 0_kInt64) RETURN
        Dividend = Value
        DO
            Dividend = Dividend * MInv5
            IF (Dividend .UGT. NDiv5) RETURN
            Count = Count + 1
        END DO

        RETURN

    END FUNCTION Pow5Factor_64Bits

    !**************************************************************************

    FUNCTION IsMultipleOfPow5_64Bits(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given value is divisible by 5**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: Value
        tUInt32, INTENT(IN) :: Exp
        tLogical            :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: ModInv5(0:1,0:MaxExp_ModInv5) = RESHAPE([ &
             ToInt64(Z'0000000000000001'), ToInt64(Z'FFFFFFFFFFFFFFFF'), &
             ToInt64(Z'CCCCCCCCCCCCCCCD'), ToInt64(Z'3333333333333333'), &
             ToInt64(Z'8F5C28F5C28F5C29'), ToInt64(Z'0A3D70A3D70A3D70'), &
             ToInt64(Z'1CAC083126E978D5'), ToInt64(Z'020C49BA5E353F7C'), &
             ToInt64(Z'D288CE703AFB7E91'), ToInt64(Z'0068DB8BAC710CB2'), &
             ToInt64(Z'5D4E8FB00BCBE61D'), ToInt64(Z'0014F8B588E368F0'), &
             ToInt64(Z'790FB65668C26139'), ToInt64(Z'000431BDE82D7B63'), &
             ToInt64(Z'E5032477AE8D46A5'), ToInt64(Z'0000D6BF94D5E57A'), &
             ToInt64(Z'C767074B22E90E21'), ToInt64(Z'00002AF31DC46118'), &
             ToInt64(Z'8E47CE423A2E9C6D'), ToInt64(Z'0000089705F4136B'), &
             ToInt64(Z'4FA7F60D3ED61F49'), ToInt64(Z'000001B7CDFD9D7B'), &
             ToInt64(Z'0FEE64690C913975'), ToInt64(Z'00000057F5FF85E5'), &
             ToInt64(Z'3662E0E1CF503EB1'), ToInt64(Z'000000119799812D'), &
             ToInt64(Z'A47A2CF9F6433FBD'), ToInt64(Z'0000000384B84D09'), &
             ToInt64(Z'54186F653140A659'), ToInt64(Z'00000000B424DC35'), &
             ToInt64(Z'7738164770402145'), ToInt64(Z'0000000024075F3D'), &
             ToInt64(Z'E4A4D1417CD9A041'), ToInt64(Z'000000000734ACA5'), &
             ToInt64(Z'C75429D9E5C5200D'), ToInt64(Z'000000000170EF54'), &
             ToInt64(Z'C1773B91FAC10669'), ToInt64(Z'000000000049C977'), &
             ToInt64(Z'26B172506559CE15'), ToInt64(Z'00000000000EC1E4'), &
             ToInt64(Z'D489E3A9ADDEC2D1'), ToInt64(Z'000000000002F394'), &
             ToInt64(Z'90E860BB892C8D5D'), ToInt64(Z'000000000000971D'), &
             ToInt64(Z'502E79BF1B6F4F79'), ToInt64(Z'0000000000001E39'), &
             ToInt64(Z'DCD618596BE30FE5'), ToInt64(Z'000000000000060B'), &
             ToInt64(Z'2C2AD1AB7BFA3661'), ToInt64(Z'0000000000000135'), &
             ToInt64(Z'08D55D224BFED7AD'), ToInt64(Z'000000000000003D'), &
             ToInt64(Z'01C445D3A8CC9189'), ToInt64(Z'000000000000000C'), &
             ToInt64(Z'CD27412A54F5B6B5'), ToInt64(Z'0000000000000002')], [2,28])

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Cache(0:1)
        tUInt64     :: ModInverse, MaxQuotient

    !** FLOW

        Cache = ModInv5(:,Exp)
        ModInverse  = Cache(0)
        MaxQuotient = Cache(1)
        Flag = Value*ModInverse .ULE. MaxQuotient

        RETURN

    END FUNCTION IsMultipleOfPow5_64Bits

    !**************************************************************************

#endif

#ifdef  tFloat_is_tSingle

    SUBROUTINE DivMod_By_Pow10Factor(X, Y, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division by 5 or 10 (i.e. Y is 5 or 10).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN)     :: X
        tUInt32, INTENT(IN)     :: Y    ! must be 5 or 10
        tUInt32, INTENT(OUT)    :: Quotient
        tUInt32, INTENT(OUT)    :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! The following algorithm actually can be used for all divisor (Y)
        Quotient  = ToInt32(ToUnsignedLong(X)/ToUnsignedLong(Y))
        Remainder = X - Quotient*Y

        RETURN

    END SUBROUTINE DivMod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Divide_By_Pow10Factor(X, Y) RESULT(Quotient)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division by 5 or 10.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN) :: X
        tUInt32, INTENT(IN) :: Y    ! must be 5 or 10
        tUInt32             :: Quotient

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! The following algorithm actually can be used for all divisor (Y)
        Quotient = ToInt32(ToUnsignedLong(X)/ToUnsignedLong(Y))

        RETURN

    END FUNCTION Divide_By_Pow10Factor

    !**************************************************************************

    FUNCTION Mod_By_Pow10Factor(X, Y) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulus of 2, 5 or 10 (i.e. Y is 2 or 5 or 10.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN) :: X
        tUInt32, INTENT(IN) :: Y    ! must be 2, 5 or 10
        tUInt32             :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! The following algorithm actually can be used for all divisor (Y)
        Remainder = ToInt32(MOD(ToUnsignedLong(X), ToUnsignedLong(Y)))

        RETURN

    END FUNCTION Mod_By_Pow10Factor

    !**************************************************************************

    FUNCTION Pow5Factor_32Bits(Value) RESULT(Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute the factor of power of 5

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN) :: Value
        tUInt32             :: Count

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: Dividend
        tUInt32     :: Quotient, Remainder

    !** FLOW

        Count = 0
        Dividend = Value
        DO WHILE (Dividend /= 0)
            CALL UDivMod(Dividend, 5, Quotient, Remainder)
            IF (Remainder /=  0) RETURN
            Dividend = Quotient
            Count = Count + 1
        END DO
        Count = 0

        RETURN

    END FUNCTION Pow5Factor_32Bits

    !**************************************************************************

    FUNCTION IsMultipleOfPow5_32Bits(Value, Exp) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given value is divisible by 5**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN) :: Value
        tUInt32, INTENT(IN) :: Exp
        tLogical            :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER  :: ModInv5(0:1,0:MaxExp_ModInv5) = RESHAPE([ &
                     ToInt32(Z'00000001'), ToInt32(Z'FFFFFFFF'), &
                     ToInt32(Z'CCCCCCCD'), ToInt32(Z'33333333'), &
                     ToInt32(Z'C28F5C29'), ToInt32(Z'0A3D70A3'), &
                     ToInt32(Z'26E978D5'), ToInt32(Z'020C49BA'), &
                     ToInt32(Z'3AFB7E91'), ToInt32(Z'0068DB8B'), &
                     ToInt32(Z'0BCBE61D'), ToInt32(Z'0014F8B5'), &
                     ToInt32(Z'68C26139'), ToInt32(Z'000431BD'), &
                     ToInt32(Z'AE8D46A5'), ToInt32(Z'0000D6BF'), &
                     ToInt32(Z'22E90E21'), ToInt32(Z'00002AF3'), &
                     ToInt32(Z'3A2E9C6D'), ToInt32(Z'00000897'), &
                     ToInt32(Z'3ED61F49'), ToInt32(Z'000001B7'), &
                     ToInt32(Z'0C913975'), ToInt32(Z'00000057'), &
                     ToInt32(Z'CF503EB1'), ToInt32(Z'00000011'), &
                     ToInt32(Z'F6433FBD'), ToInt32(Z'00000003')], [2, 14])

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: Cache(0:1)
        tUInt32     :: ModInverse, MaxQuotient

    !** FLOW

        Cache = ModInv5(:,Exp)
        ModInverse  = Cache(0)
        MaxQuotient = Cache(1)
        Flag = Value*ModInverse .ULE. MaxQuotient

        RETURN

    END FUNCTION IsMultipleOfPow5_32Bits

    !**************************************************************************

#endif

END SUBROUTINE Bin2Dec_Ryu

!******************************************************************************

SUBROUTINE Bin2Dec_Schubfach(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a binary floating point number into the shortest and correctly
    ! rounded decimal representation based on the Schubfach algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: SigRaw   ! The raw value of significand in IEEE 754 format
    tSInt32,   INTENT(IN)   :: ExpRaw   ! The raw value of exponent in IEEE 754 format
    tUIntType, INTENT(IN)   :: SigBin   ! The decoded value of significand in binary
    tSInt32,   INTENT(IN)   :: ExpBin   ! The decoded value of exponent in binary
    tUIntType, INTENT(OUT)  :: SigDec   ! The output value of significand in decimal
    tSInt32,   INTENT(OUT)  :: ExpDec   ! The output value of exponent in decimal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
    tUInt64     :: Pow10(0:3)   ! in little endian order; most significant byte is 3
#else
#ifdef  tFloat_is_tDouble
    tUInt128    :: Pow10
#else
    tUInt64     :: Pow10
#endif
#endif
    tUIntType   :: Cb, Cbl, Cbr, Vb, Vbl, Vbr
    tSInt32     :: kExp, hExp, Exp10
    tLogical    :: uInside, wInside
    tUIntType   :: Sx, Sx4, Sp
    tUIntType   :: Upper, Lower, Middle

!** FLOW:

    Cb  = SHIFTL(SigBin, 2)
    Cbr = Cb + TwoUInt

    ! check whether Cb is closer to the lower bound
    IF ((SigRaw == ZeroUInt).AND.(ExpRaw > 1)) THEN
        ! closer to the lower bound; irregular spacing
        Cbl = Cb - OneUInt
        kExp = Floor_Log10_ThreeQuartersPow2(ExpBin)
    ELSE
        ! not closer to the lower bound; regular spacing
        Cbl = Cb - TwoUInt
        kExp = Floor_Log10_Pow2(ExpBin)
    END IF

    ! compute Exp10 and shift
    Exp10 = -kExp
    hExp  = ExpBin + Floor_Log2_Pow10(Exp10) + 1

    ! get the cached pow10 value from Pow10_Sig_Table or compute it
#ifdef  tFloat_is_tQuad
    Pow10 = Get_Pow10_256Bits(Exp10)
    IF ((Exp10 < Pow10_Min_Exact_Exp).OR.(Exp10 > Pow10_Max_Exact_Exp)) THEN
        Pow10(0) = Pow10(0) + 1_kInt64
    END IF
#else
#ifdef  tFloat_is_tDouble
    Pow10 = Get_Pow10_128Bits(Exp10)
    IF ((Exp10 < Pow10_Min_Exact_Exp).OR.(Exp10 > Pow10_Max_Exact_Exp)) THEN
        Pow10%Low = Pow10%Low + 1_kInt64
    END IF
#else
    Pow10 = Get_Pow10_64Bits(Exp10)
    IF ((Exp10 < Pow10_Min_Exact_Exp).OR.(Exp10 > Pow10_Max_Exact_Exp)) THEN
        Pow10 = Pow10 + 1_kInt64
    END IF
#endif
#endif

    ! To perform integer multiplications and get upper bits of rounded values
    Vbl = Round2Odd(Pow10, SHIFTL(Cbl,  hExp))
    Vb  = Round2Odd(Pow10, SHIFTL(Cb,   hExp))
    Vbr = Round2Odd(Pow10, SHIFTL(Cbr,  hExp))

    IF (IAND(SigBin, OneUInt) == ZeroUInt) THEN
        Lower = Vbl
        Upper = Vbr
    ELSE
        Lower = Vbl + OneUInt
        Upper = Vbr - OneUInt
    END IF

    Sx = SHIFTR(Vb, 2)  ! Sx = Vb / 4
    IF (Sx .UGE. TenUInt) THEN
        ! Vb >= 40
        Sp  = Sx .UDIV. TenUInt     ! Vb / 40
        Sx4 = FortyUInt * Sp
        uInside = (Lower .ULE. Sx4)
        wInside = (Upper .UGE. (Sx4 + FortyUInt))
        IF (uInside .NEQV. wInside) THEN
            IF (wInside) THEN
                SigDec = Sp + OneUInt
            ELSE
                SigDec = Sp
            END IF
            ExpDec = kExp + 1
            RETURN
        END IF
    END IF

    Sx4 = SHIFTL(Sx, 2)
    uInside = (Lower .ULE. Sx4)
    wInside = (Upper .UGE. (Sx4 + FourUInt))

    ExpDec  = kExp
    SigDec  = Sx
    IF (uInside .NEQV. wInside) THEN
        IF (wInside) SigDec = SigDec + OneUInt
        RETURN
    END IF

    Middle  = Sx4 + TwoUInt
    IF ((Vb .UGT. Middle).OR.((Vb == Middle).AND.(IAND(Sx, OneUInt) /= ZeroUInt))) THEN
        SigDec = SigDec + OneUInt
    END IF

    RETURN

    CONTAINS

#ifdef  tFloat_is_tQuad

    FUNCTION Round2Odd(G, Cx) RESULT(Vx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform the rounding of input

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64,  INTENT(IN)    :: G(0:3)   ! in little-endian order
        tUInt128, INTENT(IN)    :: Cx
        tUInt128                :: Vx

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128    :: X_Hi, Y_Lo

    !** FLOW

        ! perform Cp * G%Lo and get the upper 64 bits of the result
        X_Hi = UMul256_Upper128(Cx, UInt128(G(1), G(0)))

        ! perform Cp * G%Hi + X_Hi and return Vx as the upper 64 bits of the result
        CALL UMul256_N_AddU128(Cx, UInt128(G(3), G(2)), X_Hi, Vx, Y_Lo)
        IF (Y_Lo .UGT. OneU128) Vx = IOR(Vx, OneU128)

        RETURN

    END FUNCTION Round2Odd

    !**************************************************************************

    SUBROUTINE UMul256(X, Y, ZHi, ZLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute multiplication of two 128-bit unsigned integers and
        ! return the 256-bit unsigned result.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: X, Y
        tUInt128, INTENT(OUT)   :: ZHi, ZLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: X64(0:1), Y64(0:1), Z64(0:3)

    !** FLOW

        ! get input
        X64(0) = X%Low
        X64(1) = X%High
        Y64(0) = Y%Low
        Y64(1) = Y%High

        ! perform multiplication
        CALL MultiplyBasic(X64, 2, Y64, 2, Z64)

        ! set output
        ZLo = UInt128(Z64(1), Z64(0))
        ZHi = UInt128(Z64(3), Z64(2))

        RETURN

    END SUBROUTINE UMul256

    !**************************************************************************

    SUBROUTINE UMul256_N_AddU128(A, B, C, U256Hi, U256Lo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply two 128-bit unsigned integers and add a 128-bit unsigned integer
        ! (A*B + C), and then return the 256-bit result as U256Hi, U256Lo.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: A, B, C
        tUInt128, INTENT(OUT)   :: U256Hi, U256Lo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128    :: H, L, Sum, Carry

    !** FLOW

        ! multiply A and B
        CALL UMul256(A, B, H, L)

        ! add C
        U256Lo = L + C
        U256Hi = H
        IF (U256Lo .ULT. L) CALL Increment(U256Hi)

        RETURN

    END SUBROUTINE UMul256_N_AddU128

    !**************************************************************************

#else
#ifdef  tFloat_is_tDouble

    FUNCTION Round2Odd(G, Cx) RESULT(Vx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform the rounding of input

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: G
        tUInt64,  INTENT(IN)    :: Cx
        tUInt64                 :: Vx

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64 :: X_Hi, Y_Lo

    !** FLOW

        ! perform Cp * G%Lo and get the upper 64 bits of the result
        X_Hi = UMul128_Upper64(Cx, G%Low)

        ! perform Cp * G%Hi + X_Hi and return Vx as the upper 64 bits of the result
        CALL UMul128_N_AddU64(Cx, G%High, X_Hi, Vx, Y_Lo)
        IF (Y_Lo .UGT. 1_kInt64)  Vx = IOR(Vx, 1_kInt64)

        RETURN

    END FUNCTION Round2Odd

    !**************************************************************************

    SUBROUTINE UMul128_N_AddU64(A, B, C, U128Hi, U128Lo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply two 64-bit unsigned integers and add a 64-bit unsigned integer
        ! (A*B + C), and then return the 128-bit result as U128Hi, U128Lo.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: A, B, C
        tUInt64, INTENT(OUT)    :: U128Hi, U128Lo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: H, L, Carry

    !** FLOW

        ! multiply A and B
        CALL UMul128(A, B, H, L)

        ! add carry
        Carry = 0_kInt64
        CALL AddU64_WithCarry(L, C, Carry, U128Lo)
        U128Hi = H + Carry

        RETURN

    END SUBROUTINE UMul128_N_AddU64

    !**************************************************************************

    SUBROUTINE AddU64_WithCarry(X, Y, Carry, Sum)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.
        ! The carry input must be 0 or 1; otherwise the behavior is undefined.
        ! The carry output is guaranteed to be 0 or 1.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: X, Y
        tUInt64, INTENT(INOUT)  :: Carry
        tUInt64, INTENT(OUT)    :: Sum

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Sum = X + Y + Carry
        ! The sum will overflow if both top bits are set (x & y) or if one of them
        ! is (x | y), and a carry from the lower place happened. If such a carry
        ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
        Carry = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)

        RETURN

    END SUBROUTINE AddU64_WithCarry

    !**************************************************************************

#else

    FUNCTION Round2Odd(G, Cx) RESULT(Vx)

        !** PURPOSE OF THIS SUBROUTINE:
        ! To perform the rounding of input

        !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: G
        tUInt32, INTENT(IN) :: Cx
        tUInt32             :: Vx

        !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64    :: X, Y

        !** FLOW
#define LoU32(X)    IAND(X, ToInt64(Z'00000000FFFFFFFF'))
#define HiU32(X)    SHIFTR(X, 32)

        Y = ToUnsignedLong(Cx)
        X = Y * LoU32(G)
        Y = Y * HiU32(G) + HiU32(X)

        Vx = ToInt32(HiU32(Y))
        IF (ToInt32(LoU32(Y)) .UGT. 1) Vx = IOR(Vx, 1)

        RETURN

#undef  LoU32
#undef  HiU32
    END FUNCTION Round2Odd

    !**************************************************************************

#endif
#endif

END SUBROUTINE Bin2Dec_Schubfach

!******************************************************************************

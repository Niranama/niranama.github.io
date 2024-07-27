
FUNCTION Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert decimal floating point representation into its exact
    ! binary floating point representation using the Clinger algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: SigDec   ! significand in base 10
    tSInt32,   INTENT(IN)   :: ExpDec   ! exponent in base 10
    tUIntType, INTENT(OUT)  :: SigBin   ! significand in base 2
    tUInt32,   INTENT(OUT)  :: ExpBin   ! exponent in base 2
    tLogical                :: Valid    ! true if conversion can be done

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType   :: RawFP
    tSInt32     :: Exp10
    tFloat      :: FloatMantissa

!** FLOW

    IF (SHIFTR(SigDec, SignificandBits) /= ZeroUInt) THEN
        Valid = FalseVal
        RETURN
    END IF

    FloatMantissa = ToFloat(SigDec)
    Exp10 = ExpDec

    IF (Exp10 == 0) THEN
        RawFP = RawFP_FromFloat(FloatMantissa)
    END IF
    IF (Exp10 > 0) THEN
        IF (Exp10 > Num_Exact_Pow10 + Num_Mantissa_Digits) THEN
            Valid = FalseVal
            RETURN
        END IF
        IF (Exp10 > Num_Exact_Pow10) THEN
            FloatMantissa = FloatMantissa * Powers_Of_Ten(Exp10 - Num_Exact_Pow10)
            Exp10 = Num_Exact_Pow10
        END IF
        IF (FloatMantissa > Max_Exact_Integer) THEN
            Valid = FalseVal
            RETURN
        END  IF
        RawFP = RawFP_FromFloat(FloatMantissa * Powers_Of_Ten(Exp10))
    ELSEIF (Exp10 < 0) THEN
        IF (-Exp10 > Num_Exact_Pow10) THEN
            Valid = FalseVal
            RETURN
        END IF
        RawFP = RawFP_FromFloat(FloatMantissa / Powers_Of_Ten(-Exp10))
    END IF

    SigBin = RawFP_Significand(RawFP)
    ExpBin = RawFP_BiasedExponent(RawFP)
    Valid = TrueVal

    RETURN

END FUNCTION Dec2Bin_Clinger

!******************************************************************************

SUBROUTINE Dec2Bin_LibC(SigDec, ExpDec, cStr, Start, Truncated, SigBin, ExpBin)

!** PURPOSE OF THIS SUBROUTINE:
    ! To use LibC algorithm to convert string to real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
#ifndef tFloat_is_tQuad
    tUIntType, INTENT(IN)   :: SigDec
#else
    tUInt128,  INTENT(IN)   :: SigDec
#endif
    tSInt32,   INTENT(IN)   :: ExpDec
    tCharStar, INTENT(IN)   :: cStr
    tSInt32,   INTENT(IN)   :: Start
    tLogical,  INTENT(IN)   :: Truncated
#ifndef tFloat_is_tQuad
    tUIntType, INTENT(OUT)  :: SigBin
#else
    tUInt128,  INTENT(OUT)  :: SigBin
#endif
    tSInt32,   INTENT(OUT)  :: ExpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType   :: FirstSigBin
    tUInt32     :: FirstExpBin

!** FLOW

    ! try the Eisel-Lemire's algorithm
    IF (Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin)) THEN
        ! the Eisel-Lemire's algorithm is possibly valid
        IF (.NOT.Truncated) RETURN
        ! If the mantissa is truncated, then the result may be off by the LSB, so
        ! check if rounding the mantissa up changes the result. If not, then it's
        ! safe, else use the fallback.
        FirstSigBin = SigBin
        FirstExpBin = ExpBin
        IF (Eisel_Lemire(SigDec + OneUInt, ExpDec, SigBin, ExpBin)) THEN
            ! check if the Eisel-Lemire's algorithm is definitely valid
            IF ((SigBin == FirstSigBin).AND.(ExpBin == FirstExpBin)) RETURN
        END IF
    END IF

    ! use the slow Simple Decimal Conversion algorithm
    CALL Simple_Decimal_Conversion(cStr, Start, LEN_TRIM(cStr), SigBin, ExpBin)

    RETURN

    CONTAINS

#ifdef tFloat_is_tQuad

    FUNCTION Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

        ! To convert decimal floating point representation into its closest
        ! binary floating point representation using the Eisel-Lemire algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: SigDec   ! significand in base 10
        tSInt32,  INTENT(IN)    :: ExpDec   ! exponent in base 10
        tUInt128, INTENT(OUT)   :: SigBin   ! significand in base 2
        tUInt32,  INTENT(OUT)   :: ExpBin   ! exponent in base 2
        tLogical                :: Valid    ! true if conversion can be done

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128        :: MantU128
        tSInt32         :: Exp2
        tSInt32         :: CLZ
        tUInt128        :: FinalApproxUpper, FinalApproxLower
        tUInt128        :: SecondProductHi,  SecondProductLo
        tUInt64         :: Mantissa(0:1)
        tUInt64         :: Pow10(0:3)
        tUInt64         :: FirstProduct(0:3)
        tUInt64         :: SecondProduct(0:3)
        tUInt128        :: FinalMantissa
        tSInt32         :: MSB

    !** FLOW

        ! normalization
        CLZ = LEADZ(SigDec)
        MantU128 = SHIFTL(SigDec, CLZ)

        Exp2 = Floor_Log2_Pow10(ExpDec) + TotalBits + ExponentBias - CLZ

        ! multiplication
        Pow10 = Get_Pow10_256Bits(ExpDec)

        ! For small values of Q, e.g., Q in [0,55], the product is always exact.
        Mantissa(0) = MantU128%Low
        Mantissa(1) = MantU128%High
        CALL MultiplyBasic(Mantissa, 2, Pow10(2:3), 2, FirstProduct)
        FinalApproxLower = UInt128(FirstProduct(1), FirstProduct(0))
        FinalApproxUpper = UInt128(FirstProduct(3), FirstProduct(2))

        ! Wider Approximation
        IF ((IAND(FinalApproxUpper, HalfWay) == HalfWay).AND.(FinalApproxLower + MantU128 .ULT. MantU128)) THEN
            CALL MultiplyBasic(Mantissa, 2, Pow10(0:1), 2, SecondProduct)
            SecondProductLo = UInt128(SecondProduct(1), SecondProduct(0))
            SecondProductHi = UInt128(SecondProduct(3), SecondProduct(2))
            FinalApproxLower = FinalApproxLower + SecondProductHi
            IF (SecondProductHi .UGT. FinalApproxLower) CALL Increment(FinalApproxUpper)
            IF ((IAND(SecondProductHi, HalfWay) == HalfWay).AND. &
                (SecondProductLo + MantU128 .ULT. MantU128)) THEN
                Valid = FalseVal
                RETURN
            END IF
        END IF

        ! Shifting to 113 bits
        MSB = ToI32(SHIFTR(FinalApproxUpper, TotalBits - 1))
        FinalMantissa = SHIFTR(FinalApproxUpper, (MSB + TotalBits - (SignificandBits + 3)))
        Exp2 = Exp2 - IEOR(1, MSB)  ! same as NOT(MSB)

        ! Half-way ambiguity
        IF ((FinalApproxLower == ZeroU128).AND.(IAND(FinalApproxUpper, HalfWay) == &
             ZeroU128).AND.(IAND(FinalMantissa, UInt128(0_kInt64, 3_kInt64)) == OneU128)) THEN
            Valid = FalseVal
            RETURN
        END IF

        ! From 113 to 112 bits
        FinalMantissa = FinalMantissa + IAND(FinalMantissa, OneU128)
        FinalMantissa = SHIFTR(FinalMantissa, 1)
        IF (SHIFTR(FinalMantissa, (SignificandBits + 1)) /= ZeroU128) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1
        END IF

        ! check exponent validity
        IF ((Exp2 < 1).OR.(Exp2 > (MaxExponent-1))) THEN
            Valid = FalseVal
            RETURN
        END IF

        SigBin = FinalMantissa
        ExpBin = Exp2
        Valid = TrueVal

        RETURN

    END FUNCTION Eisel_Lemire

    !******************************************************************************

#else
#ifdef tFloat_is_tDouble

    FUNCTION Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

        ! To convert decimal floating point representation into its closest
        ! binary floating point representation using the Eisel-Lemire algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: SigDec   ! significand in base 10
        tSInt32, INTENT(IN)     :: ExpDec   ! exponent in base 10
        tUInt64, INTENT(OUT)    :: SigBin   ! significand in base 2
        tUInt32, INTENT(OUT)    :: ExpBin   ! exponent in base 2
        tLogical                :: Valid    ! true if conversion can be done

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Exp2
        tSInt32     :: CLZ
        tUInt64     :: HighU64, Mantissa
        tUInt128    :: PowTen
        tUInt128    :: FirstApprox, FinalApprox, LowBits, SecondApprox
        tUInt64     :: FinalMantissa
        tSInt32     :: MSB

    !** FLOW

        ! normalization
        CLZ = LEADZ(SigDec)
        Mantissa = SHIFTL(SigDec, CLZ)

        Exp2 = Floor_Log2_Pow10(ExpDec) + TotalBits + ExponentBias - CLZ

        ! multiplication
        PowTen = Get_Pow10_128Bits(ExpDec)
        FirstApprox = UInt128(0_kInt64, Mantissa) * PowTen%High

        ! Wider Approximation
        IF ((IAND(FirstApprox%High, HalfWay) == HalfWay).AND. &
            (FirstApprox%Low + Mantissa .ULT. Mantissa)) THEN
            LowBits = UInt128(0_kInt64, Mantissa) * PowTen%Low
            SecondApprox = FirstApprox + UInt128(0_kInt64, LowBits%High)
            IF ((IAND(SecondApprox%High, HalfWay) == HalfWay).AND. &
                (SecondApprox%Low + 1_kInt64 == 0_kInt64).AND. &
                (LowBits%Low + Mantissa .ULT. Mantissa)) THEN
                Valid = FalseVal
                RETURN
            END IF
            FinalApprox = SecondApprox
        ELSE
            FinalApprox = FirstApprox
        END IF

        ! Shifting to 54 bits for doubles
        HighU64 = FinalApprox%High
        MSB = ToInt32(SHIFTR(HighU64, TotalBits - 1))
        FinalMantissa = SHIFTR(HighU64, (MSB + TotalBits - (SignificandBits + 3)))
        Exp2 = Exp2 - IEOR(1, MSB)  ! same as NOT(MSB)

        ! Half-way ambiguity
        IF ((FinalApprox%Low == 0_kInt64).AND.(IAND(HighU64, HalfWay) == 0_kInt64) &
             .AND.(IAND(FinalMantissa, 3_kInt64) == 1_kInt64)) THEN
            Valid = FalseVal
            RETURN
        END IF

        ! From 54 to 53 bits for doubles
        FinalMantissa = FinalMantissa + IAND(FinalMantissa, 1_kInt64)
        FinalMantissa = SHIFTR(FinalMantissa, 1)
        IF (SHIFTR(FinalMantissa, (SignificandBits + 1)) /= 0_kInt64) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1
        END IF

        ! check exponent validity
        IF ((Exp2 < 1).OR.(Exp2 > (MaxExponent-1))) THEN
            Valid = FalseVal
            RETURN
        END IF

        SigBin = FinalMantissa      ! implicit conversion if type is mismatch
        ExpBin = Exp2
        Valid = TrueVal

        RETURN

    END FUNCTION Eisel_Lemire

    !******************************************************************************
#else

    FUNCTION Eisel_Lemire(SigDec, ExpDec, SigBin, ExpBin) RESULT(Valid)

        ! To convert decimal floating point representation into its closest
        ! binary floating point representation using the Eisel-Lemire algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(IN)     :: SigDec   ! significand in base 10
        tSInt32, INTENT(IN)     :: ExpDec   ! exponent in base 10
        tUInt32, INTENT(OUT)    :: SigBin   ! significand in base 2
        tUInt32, INTENT(OUT)    :: ExpBin   ! exponent in base 2
        tLogical                :: Valid    ! true if conversion can be done

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Exp2
        tSInt32     :: CLZ
        tUInt32     :: Mantissa, HighU32
        tUInt64     :: PowTen
        tUInt64     :: FirstApprox, FinalApprox, LowBits, SecondApprox
        tUInt32     :: FinalMantissa
        tSInt32     :: MSB

    !** FLOW

        ! normalization
        CLZ = LEADZ(SigDec)
        Mantissa = SHIFTL(SigDec, CLZ)

        Exp2 = Floor_Log2_Pow10(ExpDec) + TotalBits + ExponentBias - CLZ

        ! multiplication
        PowTen = Get_Pow10_64Bits(ExpDec)
        FirstApprox = ToUnsignedLong(Mantissa)*SHIFTR(PowTen, 32)

        ! Wider Approximation
        IF ((IAND(ToInt32(SHIFTR(FirstApprox, 32)), HalfWay) == HalfWay).AND. &
            (ToInt32(IAND(FirstApprox, ToInt64(Z'00000000FFFFFFFF'))) + Mantissa .ULT. Mantissa)) THEN
            BLOCK
                ! --- declaration ---
                tUInt32     :: Product_Low, Product_High
                tUInt32     :: Product_Middle, Product_Middle1, Product_Middle2
                ! --- execution ---
                LowBits = ToUnsignedLong(Mantissa)*IAND(PowTen, ToInt64(Z'00000000FFFFFFFF'))
                Product_Low     = ToInt32(IAND(LowBits, ToInt64(Z'00000000FFFFFFFF')))
                Product_Middle2 = ToInt32(SHIFTR(LowBits, 32))
                Product_Middle1 = ToInt32(IAND(FirstApprox, ToInt64(Z'00000000FFFFFFFF')))
                Product_High    = ToInt32(SHIFTR(FirstApprox, 32))
                Product_Middle  = Product_Middle1 + Product_Middle2
                ! overflow carry
                IF (Product_Middle .ULT. Product_Middle1) Product_High = Product_High + 1
                IF ((IAND(Product_High, HalfWay) == HalfWay).AND.(Product_Middle + 1 == 0).AND. &
                    (Product_Low + Mantissa .ULT. Mantissa)) THEN
                    Valid = FalseVal
                    RETURN
                END IF
                SecondApprox =  IOR(SHIFTL(ToUnsignedLong(Product_High), 32), ToUnsignedLong(Product_Middle))
            END BLOCK
            FinalApprox = SecondApprox
        ELSE
            FinalApprox = FirstApprox
        END IF

        ! Shifting to 25 bits for singles
        HighU32 = ToInt32(SHIFTR(FinalApprox, 32))
        MSB = ToInt32(SHIFTR(HighU32, TotalBits - 1))
        FinalMantissa = SHIFTR(HighU32, (MSB + TotalBits - (SignificandBits + 3)))
        Exp2 = Exp2 - IEOR(1, MSB)  ! same as NOT(MSB)

        ! Half-way ambiguity
        IF ((ToInt32(IAND(FinalApprox, ToInt64(Z'00000000FFFFFFFF'))) == 0).AND.(IAND(HighU32, HalfWay) == 0) &
             .AND.(IAND(FinalMantissa, 3) == 1)) THEN
            Valid = FalseVal
            RETURN
        END IF

        ! From 25 to 24 bits for singles
        FinalMantissa = FinalMantissa + IAND(FinalMantissa, 1)
        FinalMantissa = SHIFTR(FinalMantissa, 1)
        IF (SHIFTR(FinalMantissa, (SignificandBits + 1)) /= 0) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1
        END IF

        ! check exponent validity
        IF ((Exp2 < 1).OR.(Exp2 > (MaxExponent-1))) THEN
            Valid = FalseVal
            RETURN
        END IF

        SigBin = FinalMantissa      ! implicit conversion if type is mismatch
        ExpBin = Exp2
        Valid = TrueVal

        RETURN

    END FUNCTION Eisel_Lemire

    !******************************************************************************

#endif
#endif

    SUBROUTINE Simple_Decimal_Conversion(cStr, Start, Finish, SigBin, ExpBin)

        ! To convert decimal string into its closest floating point binary representation
        ! using the Simple Decimal Conversion algorithm.
        ! The routine assumes that cStr is a 'VALID' floating point string and
        ! Start is less than Finish where
        !   - Start is the index of the first valid numeric character, and
        !   - Finish is the index of the last valid character (== length of the input
        !     string excluding trailing space(s))

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr
        tSInt32,   INTENT(IN)   :: Start
        tSInt32,   INTENT(IN)   :: Finish
        tUIntType, INTENT(OUT)  :: SigBin   ! significand in base 2
        tUInt32,   INTENT(OUT)  :: ExpBin   ! exponent in base 2

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: A0 = IACHAR('0')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(HPDecimal) :: HP
        tUIntType       :: FinalMantissa
        tSInt32         :: Exp2
        tSInt32         :: ShiftAmount

    !** FLOW

        ! initialize
        Exp2 = 0

        ! construct HPDecimal object
        CALL HP%Construct(cStr, Start, Finish)

        IF (HP%NumDigits == 0) THEN
            SigBin = ZeroUInt
            ExpBin = 0
            RETURN
        END IF

        ! If the exponent is too large and can't be represented in this size of
        ! float, return inf.
        IF ((HP%DecimalPoint > 0).AND.(Floor_Log2_Pow10(HP%DecimalPoint-1) > ExponentBias)) THEN
            SigBin = ZeroUInt
            ExpBin = MaxExponent
            RETURN
        END IF

        ! If the exponent is too small even for a subnormal, return 0.
        IF ((HP%DecimalPoint < 0).AND. &
            (Floor_Log2_Pow10(-HP%DecimalPoint) > (ExponentBias + SignificandBits))) THEN
            SigBin = ZeroUInt
            ExpBin = 0
            RETURN
        END IF

        ! Right shift until the number is smaller than 1.
        DO WHILE (HP%DecimalPoint > 0)
            ShiftAmount = 0
            IF (HP%DecimalPoint >= Num_Powers_Of_Two) THEN
                ShiftAmount = 60
            ELSE
                ShiftAmount = Powers_Of_Two(HP%DecimalPoint)
            END IF
            Exp2 = Exp2 + ShiftAmount
            CALL HP%Shift(-ShiftAmount)
        END DO

        ! Left shift until the number is between 1/2 and 1
        DO WHILE ((HP%DecimalPoint < 0).OR.((HP%DecimalPoint == 0).AND.(HP%Digits(0) < 5)))
            ShiftAmount = 0
            IF (-HP%DecimalPoint >= Num_Powers_Of_Two) THEN
                ShiftAmount = 60
            ELSEIF (HP%DecimalPoint /= 0) THEN
                ShiftAmount = Powers_Of_Two(-HP%DecimalPoint)
            ELSE
                ! This handles the case of the number being between .1 and .5
                ShiftAmount = 1
            END IF
            Exp2 = Exp2 - ShiftAmount
            CALL HP%Shift(ShiftAmount)
        END DO

        ! Left shift once so that the number is between 1 and 2
        Exp2 = Exp2 - 1
        CALL HP%Shift(1)

        ! Get the biased exponent
        Exp2 = Exp2 + ExponentBias

        ! Handle the exponent being too large (and return inf).
        IF (Exp2 >= MaxExponent) THEN
            SigBin = 0
            ExpBin = MaxExponent
            RETURN
        END IF

        ! Shift left to fill the mantissa
        CALL HP%Shift(SignificandBits)
        CALL HP%RoundToUIntType(FinalMantissa)

        ! Handle subnormals
        IF (Exp2 <= 0) THEN
            ! Shift right until there is a valid exponent
            DO WHILE (Exp2 < 0)
                CALL HP%Shift(-1)
                Exp2 = Exp2 + 1
            END DO
            ! Shift right one more time to compensate for the left shift to get it
            ! between 1 and 2.
            CALL HP%Shift(-1)
            CALL HP%RoundToUIntType(FinalMantissa)

            ! Check if by shifting right we've caused this to round to a normal number.
            IF (SHIFTR(FinalMantissa, SignificandBits) /= ZeroUInt) THEN
                Exp2 = Exp2 + 1
            END IF
        END IF

        ! Check if rounding added a bit, and shift down if that's the case.
        IF (FinalMantissa == SHIFTL(ToUIntType(2), SignificandBits)) THEN
            FinalMantissa = SHIFTR(FinalMantissa, 1)
            Exp2 = Exp2 + 1

            ! Check if this rounding causes Exp2 to go out of range and make the result
            ! INF. If this is the case, then finalMantissa and Exp2 are already the
            ! correct values for an INF result.
            IF (Exp2 >= MaxExponent) THEN
                ! report error if applicable
            END IF
        END IF

        IF (Exp2 == 0) THEN
            ! report error if applicable
        END IF

        SigBin = FinalMantissa
        ExpBin = Exp2

        RETURN

    END SUBROUTINE Simple_Decimal_Conversion

    !******************************************************************************

END SUBROUTINE Dec2Bin_LibC

!******************************************************************************

SUBROUTINE Dec2Bin_FastFloat(SigDec, ExpDec, cStr, SigCut, Indices, SigBin, ExpBin)

!** PURPOSE OF THIS SUBROUTINE:
    ! To use FastFloat algorithm to convert string to real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
#ifndef tFloat_is_tQuad
    tUInt64,   INTENT(IN)   :: SigDec
#else
    tUInt128,  INTENT(IN)   :: SigDec
#endif
    tSInt32,   INTENT(IN)   :: ExpDec
    tCharStar, INTENT(IN)   :: cStr
    tLogical,  INTENT(IN)   :: SigCut
    tSInt32,   INTENT(IN)   :: Indices(4)
#ifndef tFloat_is_tQuad
    tUInt64,   INTENT(OUT)  :: SigBin
#else
    tUInt128,  INTENT(OUT)  :: SigBin
#endif
    tSInt32,   INTENT(OUT)  :: ExpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: EBase
    tSInt32     :: ECmp
#ifndef tFloat_is_tQuad
    tUInt64     :: MBase
    tUInt64     :: MCmp
#else
    tUInt128    :: MBase
    tUInt128    :: MCmp
#endif

!** FLOW

    ! compute float
    CALL Compute_Float(ExpDec, SigDec, EBase, MBase)
    IF (SigCut .AND. EBase >= 0) THEN
        CALL Compute_Float(ExpDec, SigDec+OneMant, ECmp, MCmp)
        IF (Is_AdjustedMantissa_NE(EBase, MBase, ECmp, MCmp)) THEN
            CALL Compute_Error(ExpDec, SigDec, EBase, MBase)
        END IF
    END IF

    ! If we have an invalid power (EBase < 0), then we need to go
    ! the long way around again. This is very uncommon.
    IF (EBase < 0) THEN
        BLOCK
            TYPE(Parsed_Number_Info)  :: NumInfo
            ! set NumInfo
            NumInfo%Exp           = ExpDec
            NumInfo%Sig           = SigDec
            NumInfo%IntegralStart = Indices(1)
            NumInfo%IntegralEnd   = Indices(2)
            NumInfo%FractionStart = Indices(3)
            NumInfo%FractionEnd   = Indices(4)
            ECmp = EBase
            MCmp = MBase
            ! compare digits
            CALL Digit_Comparision(cStr, NumInfo, ECmp, MCmp, EBase, MBase)
        END BLOCK
    END IF

    SigBin = MBase
    ExpBin = EBase

    RETURN

    CONTAINS

#ifndef tFloat_is_tQuad

    SUBROUTINE Compute_Product_Approximation(Q, W, ProductHi, ProductLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute or rather approximate W * 5**Q and return a pair of 64-bit words
        ! approximating the result, with the "high" part corresponding to the most
        ! significant bits and the low part corresponding to the least significant bits.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32, INTENT(IN) :: Q                    ! exponent
        tUInt64, INTENT(IN) :: W                    ! mantissa
        tUInt64             :: ProductHi, ProductLo ! product approximation

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: BitPrecision
        tUInt64     :: PrecisionMask, SecondProductHi
        tUInt128    :: Pow10

    !** FLOW

        ! The required precision is Mantissa_Explicit_Bits + 3 because
        ! 1. We need the implicit bit
        ! 2. We need an extra bit for rounding purposes
        ! 3. We might lose a bit due to the "UpperBit" (result too small, requiring a shift)
        ! BitPrecision = 26 for 32-bit number and 55 for 64-bit number
        BitPrecision = Mantissa_Explicit_Bits + 3

        ! compute precision mask
        PrecisionMask = SHIFTR(MaxMant, BitPrecision)

        ! get 128-bit approximation of power of ten (or power of five)
        Pow10 = Get_Pow10_128Bits(Q)

        ! For small values of Q, e.g., Q in [0,27], the product is always exact.
        CALL UMul128(W, Pow10%High, ProductHi, ProductLo)

        IF (IAND(ProductHi, PrecisionMask) == PrecisionMask) THEN
            ! could further guard with  (ProductLo + W < ProductLo)
            ! regarding the second product, we only need the upper bits of the product.
            SecondProductHi = UMul128_Upper64(W, Pow10%Low)
            ProductLo = ProductLo + SecondProductHi
            IF (SecondProductHi .UGT. ProductLo) ProductHi = ProductHi + 1_kInt64
        END IF

        RETURN

    END SUBROUTINE Compute_Product_Approximation

    !**************************************************************************

#else

    SUBROUTINE Compute_Product_Approximation(Q, W, ProductHi, ProductLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute or rather approximate W * 5**Q and return a pair of 64-bit words
        ! approximating the result, with the "high" part corresponding to the most
        ! significant bits and the low part corresponding to the least significant bits.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: Q                    ! exponent in base 10
        tUInt128, INTENT(IN)    :: W                    ! significand in base 10
        tUInt128                :: ProductHi, ProductLo ! product approximation

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: BitPrecision
        tUInt128    :: PrecisionMask, SecondProductHi
        tUInt64     :: Mantissa(0:1)
        tUInt64     :: Pow10(0:3)
        tUInt64     :: FirstProduct(0:3)
        tUInt64     :: SecondProduct(0:3)

    !** FLOW

        ! The required precision is Mantissa_Explicit_Bits + 3 because
        ! 1. We need the implicit bit
        ! 2. We need an extra bit for rounding purposes
        ! 3. We might lose a bit due to the "UpperBit" (result too small, requiring a shift)
        ! BitPrecision = 26 for 32-bit number and 55 for 64-bit number
        BitPrecision = Mantissa_Explicit_Bits + 3

        ! compute precision mask
        PrecisionMask = SHIFTR(MaxMant, BitPrecision)

        ! get 256-bit approximation of power of ten (or power of five)
        Pow10 = Get_Pow10_256Bits(Q)

        ! For small values of Q, e.g., Q in [0,55], the product is always exact.
        Mantissa(0) = W%Low
        Mantissa(1) = W%High
        CALL MultiplyBasic(Mantissa, 2, Pow10(2:3), 2, FirstProduct)
        ProductLo = UInt128(FirstProduct(1), FirstProduct(0))
        ProductHi = UInt128(FirstProduct(3), FirstProduct(2))

        IF (IAND(ProductHi, PrecisionMask) == PrecisionMask) THEN
            ! could further guard with  (ProductLo + W < ProductLo)
            ! regarding the second product, we only need the upper bits of the product.
            CALL MultiplyBasic(Mantissa, 2, Pow10(0:1), 2, SecondProduct)
            SecondProductHi = UInt128(SecondProduct(3), SecondProduct(2))
            ProductLo = ProductLo + SecondProductHi
            IF (SecondProductHi .UGT. ProductLo) CALL Increment(ProductHi)
        END IF

        RETURN

    END SUBROUTINE Compute_Product_Approximation

    !**************************************************************************

#endif

    FUNCTION Power(Q) RESULT(E)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute power in base 2 based on the power in base 10

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32, INTENT(IN) :: Q    ! power in base 10
        tSInt32             :: E    ! power in base 2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        E = Floor_Log2_Pow10(Q) + MantTotalBits - 1

        RETURN

    END FUNCTION Power

    !**************************************************************************

    SUBROUTINE Compute_Error_Scaled(Q, W, LZ, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To create an adjusted mantissa, biased by the invalid power2
        ! for significant digits already multiplied by 10 ** Q.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: LZ   ! leading zeros in W
        tSInt32,  INTENT(IN)    :: Q    ! exponent in base 10
        tSInt32,  INTENT(OUT)   :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: W    ! significand in base 10
        tUInt64,  INTENT(OUT)   :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(IN)    :: W    ! significand in base 10
        tUInt128, INTENT(OUT)   :: M    ! adjusted significand in base 2
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: HiLZ, Bias

    !** FLOW

        HiLZ = IEOR(ToI32(SHIFTR(W, MantTotalBits - 1)), 1)
        Bias = Mantissa_Explicit_Bits - Minimum_Exponent
        M = SHIFTL(W, HiLZ)
        E = Power(Q) + Bias - HiLZ - LZ - (MantTotalBits-2) + Invalid_AM_Bias

        RETURN

    END SUBROUTINE Compute_Error_Scaled

    !**************************************************************************

    SUBROUTINE Compute_Error(Q, W, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute W * 10 ** Q, without rounding the representation up.
        ! the power2 in the exponent will be adjusted by Invalid_AM_Bias.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: Q    ! exponent in base 10
        tSInt32,  INTENT(OUT)   :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: W    ! significand in base 10
        tUInt64,  INTENT(OUT)   :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(IN)    :: W    ! significand in base 10
        tUInt128, INTENT(OUT)   :: M    ! adjusted significand in base 2
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: LZ
#ifndef tFloat_is_tQuad
        tUInt64     :: LocalW
        tUInt64     :: ProductHi, ProductLo
#else
        tUInt128    :: LocalW
        tUInt128    :: ProductHi, ProductLo
#endif

    !** FLOW

        ! perform normalization
        LZ = LEADZ(W)
        LocalW = SHIFTL(W, LZ)

        ! compute the product approximation
        CALL Compute_Product_Approximation(Q, LocalW, ProductHi, ProductLo)

        ! compute the adjusted mantissa biased by the invalid power2
        CALL Compute_Error_Scaled(Q, ProductHi, LZ, E, M)

        RETURN

    END SUBROUTINE Compute_Error

    !**************************************************************************

    SUBROUTINE Compute_Float(Q, W, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute W * 10 ** Q
        ! The returned value should be a valid ieee64 number that simply need to be packed.
        ! However, in some very rare cases, the computation will fail. In such cases, we
        ! return an adjusted_mantissa with a negative power of 2: the caller should recompute
        ! in such cases.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: Q    ! exponent in base 10
        tSInt32,  INTENT(OUT)   :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: W    ! significand in base 10
        tUInt64,  INTENT(OUT)   :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(IN)    :: W    ! significand in base 10
        tUInt128, INTENT(OUT)   :: M    ! adjusted significand in base 2
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: LZ, UpperBit
#ifndef tFloat_is_tQuad
        tUInt64     :: LocalW
        tUInt64     :: ProductHi, ProductLo
#else
        tUInt128    :: LocalW
        tUInt128    :: ProductHi, ProductLo
#endif

    !** FLOW

        ! check for special cases (may not be needed since it is taken care of in the caller?)
        IF ((W == ZeroUInt) .OR. (Q < Smallest_Power_of_Ten)) THEN
            E = 0
            M = ZeroUInt
            ! result should be zero
            RETURN
        END IF
        IF (Q > Largest_Power_of_Ten) THEN
            ! we want to get infinity:
            E = Infinite_Power
            M = ZeroUInt
            RETURN
        END IF

        ! At this point in time Q is in [Smallest_Power_of_Ten, Largest_Power_of_Ten].

        ! We want the most significant bit of i to be 1. Shift if needed.
        ! (i.e. perform normalization)
        LZ = LEADZ(W)
        LocalW = SHIFTL(W, LZ)

        ! compute the product approximation
        CALL Compute_Product_Approximation(Q, LocalW, ProductHi, ProductLo)
        ! The computed product is always sufficient.
        ! See mathematical proof in the following reference:
        ! Noble Mushtak, Daniel Lemire, Fast Number Parsing Without Fallback,
        !       Software: Practice and Experience 53 (7), 2023.

        ! Shifting to Mantissa_Explicit_Bits + 2 bits
        UpperBit = ToI32(SHIFTR(ProductHi, MantTotalBits - 1))
        M = SHIFTR(ProductHi, (UpperBit + MantTotalBits - Mantissa_Explicit_Bits - 3))
        E = Power(Q) + UpperBit - LZ - Minimum_Exponent

        IF (E <= 0) THEN    ! we have a subnormal?
            ! Here have that E <= 0 so -E >= 0
            IF (-E + 1 >= MantTotalBits) THEN
                ! if we have more than 'MantTotalBits' bits below the minimum exponent, you have a zero for sure.
                E = 0
                M = ZeroUInt
                ! result should be zero
                RETURN
            END IF
            ! next line is safe because -E + 1 < MantTotalBits
            M = SHIFTR(M, -E + 1)
            ! Thankfully, we can't have both "round-to-even" and subnormals because
            ! "round-to-even" only occurs for powers close to 0.
            M = M + IAND(M, OneMant)  ! round up
            M = SHIFTR(M, 1)
            ! There is a weird scenario where we don't have a subnormal but just.
            ! Suppose we start with 2.2250738585072013e-308, we end up
            ! with 0x3fffffffffffff x 2^-1023-53 which is technically subnormal
            ! whereas 0x40000000000000 x 2^-1023-53  is normal. Now, we need to round
            ! up 0x3fffffffffffff x 2^-1023-53  and once we do, we are no longer
            ! subnormal, but we can only know this after rounding.
            ! So we only declare a subnormal if we are smaller than the threshold.
            ! IF (M .ULT. SHIFTL(OneMant, Mantissa_Explicit_Bits)) THEN
            IF (M .ULT. Hidden_Bit_Mask) THEN
                E = 0
            ELSE
                E = 1
            END IF
            RETURN
        END IF

        ! usually, we round *up*, but if we fall right in between and we have an
        ! even basis, we need to round down
        ! We are only concerned with the cases where 5**Q fits in single 64-bit word.
#ifndef tFloat_is_tQuad
        IF ((ProductLo .ULE. OneMant) .AND. (Q >= Min_Exponent_Round_To_Even) .AND. &
            (Q <= Max_Exponent_Round_To_Even) .AND. &
            (IAND(M, ToInt64(ThreeUInt)) == OneMant)) THEN  ! we may fall between two floats!
#else
        IF ((ProductLo .ULE. OneMant) .AND. (Q >= Min_Exponent_Round_To_Even) .AND. &
            (Q <= Max_Exponent_Round_To_Even) .AND. &
            (IAND(M, ThreeUInt) == OneMant)) THEN  ! we may fall between two floats!
#endif
            ! To be in-between two floats we need that in doing
            !   M = ProductHi >> (UpperBit + 64 - Mantissa_Explicit_Bits - 3)
            ! ... we dropped out only zeroes. But if this happened, then we can go back!!!
            IF (SHIFTL(M, (UpperBit + MantTotalBits - Mantissa_Explicit_Bits - 3)) == &
                ProductHi) THEN
                M = IAND(M, NotOneMant)   ! flip it so that we do not round up
            END IF
        END IF

        M = M + IAND(M, OneMant)        ! round up
        M = SHIFTR(M, 1)
        IF (M .UGE. Max_Mantissa_Fast_Path) THEN
            M = SHIFTL(OneMant, Mantissa_Explicit_Bits)
            E = E + 1                   ! undo previous addition
        END IF

        M = IAND(M, NotSigHidBitMask)
        IF (E >= Infinite_Power) THEN   ! infinity
            E = Infinite_Power
            M = ZeroUInt
        END IF

        RETURN

    END SUBROUTINE Compute_Float

    !**************************************************************************

    FUNCTION Scientific_Exponent(Number) RESULT(Exponent)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To calculate the exponent, in scientific notation, of the number.
        ! this algorithm is not even close to optimized, but it has no practical
        ! effect on performance: in order to have a faster algorithm, we'd need
        ! to slow down performance for faster algorithms, and this is still fast.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(Parsed_Number_Info), INTENT(IN)    :: Number
        tSInt32                                 :: Exponent

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef tFloat_is_tSingle
        tUInt32     :: Mantissa
#else
#ifdef tFloat_is_tDouble
        tUInt64     :: Mantissa
#else
        tUInt128    :: Mantissa
#endif
#endif

    !** FLOW

        Mantissa = Number%Sig   ! implicit narrow conversion for 32-bit
        Exponent = Number%Exp
        DO WHILE (Mantissa .UGE. TenThousandUInt)
            Mantissa = DivByPow10(Mantissa, 4)
            Exponent = Exponent + 4
        END DO
        DO WHILE (Mantissa .UGE. HundredUInt)
            Mantissa = DivByPow10(Mantissa, 2)
            Exponent = Exponent + 2
        END DO
        DO WHILE (Mantissa .UGE. TenUInt)
            Mantissa = DivByPow10(Mantissa, 1)
            Exponent = Exponent + 1
        END DO

        RETURN

    END FUNCTION Scientific_Exponent

    !**************************************************************************

    SUBROUTINE Digit_Comparision(cStr, NumInfo, EIn, MIn, EOut, MOut)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse the significant digits as a big integer to unambiguously round the
        ! the significant digits. here, we are trying to determine how to round
        ! an extended float representation close to `b+h`, halfway between `b`
        ! (the float rounded-down) and `b+u`, the next positive float. this
        ! algorithm is always correct, and uses one of two approaches. when
        ! the exponent is positive relative to the significant digits (such as
        ! 1234), we create a big-integer representation, get the high 64-bits,
        ! determine if any lower bits are truncated, and use that to direct
        ! rounding. in case of a negative exponent relative to the significant
        ! digits (such as 1.2345), we create a theoretical representation of
        ! `b` as a big-integer type, scaled to the same binary exponent as
        ! the actual digits. we then compare the big integer representations
        ! of both, and use that to direct rounding.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,                INTENT(IN)    :: cStr
        TYPE(Parsed_Number_Info), INTENT(IN)    :: NumInfo
        tSInt32,                  INTENT(IN)    :: EIn
        tSInt32,                  INTENT(OUT)   :: EOut
#ifndef tFloat_is_tQuad
        tUInt64,                  INTENT(IN)    :: MIn
        tUInt64,                  INTENT(OUT)   :: MOut
#else
        tUInt128,                 INTENT(IN)    :: MIn
        tUInt128,                 INTENT(OUT)   :: MOut
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(BigUInt)   :: Big
        tSInt32         :: Sci_Exp, Digits, Exponent
        tSInt32         :: EIn2

    !** FLOW

        ! remove the invalid exponent bias
        EIn2 = EIn - Invalid_AM_Bias

        Sci_Exp = Scientific_Exponent(NumInfo)
        Digits = 0
        CALL Parse_Mantissa(cStr, Big, NumInfo, Max_Digits, Digits)
        ! can't underflow, since digits is at most max_digits.
        Exponent = Sci_Exp + 1 - Digits
        IF (Exponent >= 0) THEN
            CALL Positive_Digit_Comparision(Big, Exponent, EOut, MOut)
        ELSE
            CALL Negative_Digit_Comparision(Big, EIn2, MIn, Exponent, EOut, MOut)
        END IF

        RETURN

    END SUBROUTINE Digit_Comparision

    !**************************************************************************

    FUNCTION Is_AdjustedMantissa_NE(ELhs, MLhs, ERhs, MRhs) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compare whether LHS /= RHS.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: ELhs
        tSInt32,  INTENT(IN)    :: ERhs
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: MLhs
        tUInt64,  INTENT(IN)    :: MRhs
#else
        tUInt128, INTENT(IN)    :: MLhs
        tUInt128, INTENT(IN)    :: MRhs
#endif
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = (MLhs /= MRhs).OR.(ELhs /= ERhs)

        RETURN

    END FUNCTION Is_AdjustedMantissa_NE

    !**************************************************************************

END SUBROUTINE Dec2Bin_FastFloat

!******************************************************************************

SUBROUTINE Round(E, M, CB)

!** PURPOSE OF THIS SUBROUTINE:
    ! To round an extended-precision float to the nearest machine float.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(INOUT) :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
    tUInt64,  INTENT(INOUT) :: M    ! adjusted significand in base 2
#else
    tUInt128, INTENT(INOUT) :: M    ! adjusted significand in base 2
#endif
    PROCEDURE(CB_Round)     :: CB   ! actual procedure that perform rounding

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: Mantissa_Shift, Shift

!** FLOW

    Mantissa_Shift = MantTotalBits - Mantissa_Explicit_Bits - 1
    IF (-E >= Mantissa_Shift) THEN
        ! have a denormal float
        Shift = -E + 1
        CALL CB(E, M, MIN(Shift, MantTotalBits))
        ! check for round-up: if rounding-nearest carried us to the hidden bit.
        IF (M .ULT. Hidden_Bit_Mask) THEN
            E = 0
        ELSE
            E = 1
        END IF
        RETURN
    END IF

    ! have a normal float, use the default shift.
    CALL CB(E, M, Mantissa_Shift)

    ! check for carry
    IF (M .UGE. Max_Mantissa_Fast_Path) THEN
        M = Hidden_Bit_Mask
        E = E + 1
    END IF

    ! check for infinite: we could have carried to an infinite power
    M = IAND(M, NotSigHidBitMask)
    IF (E >= Infinite_Power) THEN
        E = Infinite_Power
        M = 0_kInt64
    END IF

    RETURN

END SUBROUTINE Round

!******************************************************************************

SUBROUTINE Round_Nearest_Tie_Even(E, M, Shift, CB)

!** PURPOSE OF THIS SUBROUTINE:
    ! To round an extended-precision float to the nearest tie to even.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(INOUT)     :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
    tUInt64,  INTENT(INOUT)     :: M    ! adjusted significand in base 2
#else
    tUInt128, INTENT(INOUT)     :: M    ! adjusted significand in base 2
#endif
    tSInt32, INTENT(IN)         :: Shift
    PROCEDURE(CB_Round_Nearest) :: CB

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifndef tFloat_is_tQuad
    tUInt64     :: Mask, Halfway, Truncated_Bits
#else
    tUInt128    :: Mask, Halfway, Truncated_Bits
#endif
    tLogical    :: Is_Above, Is_Halfway, Is_Odd

!** FLOW

    IF (Shift == MantTotalBits) THEN
        Mask = MaxMant
    ELSE
        Mask = SHIFTL(OneMant, Shift) - OneMant
    END IF
    IF (Shift == 0) THEN
        Halfway = ZeroUInt
    ELSE
        Halfway = SHIFTL(OneMant, (Shift - 1))
    END IF

    Truncated_Bits = IAND(M, Mask)
    Is_Above   = Truncated_Bits .UGT. Halfway
    Is_Halfway = Truncated_Bits == Halfway

    ! shift digits into position
    IF (Shift == MantTotalBits) THEN
        M = ZeroUInt
    ELSE
        M = SHIFTR(M, Shift)
    END IF
    E = E + Shift

    Is_Odd = IAND(M, OneMant) == OneMant
    IF (CB(Is_Odd, Is_Halfway, Is_Above)) M = M + OneMant

    RETURN

END SUBROUTINE Round_Nearest_Tie_Even

!******************************************************************************

SUBROUTINE Parse_Mantissa(cStr, Big, NumInfo, Max_Digits, Digits)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse the significant digits into a BigUInt

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,                INTENT(IN)    :: cStr
    TYPE(BigUInt),            INTENT(INOUT) :: Big
    TYPE(Parsed_Number_Info), INTENT(IN)    :: NumInfo
    tSInt32,                  INTENT(IN)    :: Max_Digits
    tSInt32,                  INTENT(INOUT) :: Digits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Indx, IEnd
    tSInt32     :: Counter, Step
    tUInt64     :: Value
    tLogical    :: Truncated

!** FLOW

    ! try to minimize the number of big integer and scalar multiplication.
    ! therefore, try to parse 8 digits at a time, and multiply by the largest
    ! scalar value (19 digits) for each step.
    Counter = 0
    Digits = 0
    Value = 0_kInt64
    Step = 19

    ! process all integer digits.
    IF (NumInfo%IntegralStart /= 0) THEN
        Indx = NumInfo%IntegralStart
        IEnd = NumInfo%IntegralEnd
        CALL Skip_Zeros(cStr, Indx, IEnd)
        ! process all digits, in increments of step per loop
        DO WHILE (Indx <= IEnd)
            DO WHILE ((Indx+7 <= IEnd).AND.(Step-Counter >= 8).AND.(Max_Digits-Digits >= 8))
                CALL Parse_Eight_Digits(cStr, Indx, Value, Counter, Digits)
            END DO
            DO WHILE ((Counter < Step).AND.(Indx <= IEnd).AND.(Digits < Max_Digits))
                CALL Parse_One_Digit(cStr, Indx, Value, Counter, Digits)
            END DO
            IF (Digits == Max_Digits) THEN
                ! add the temporary value, then check if we've truncated any digits
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Truncated = Is_Truncated(cStr, Indx, IEnd)
                IF (NumInfo%FractionStart /= 0) THEN
                    Truncated = Truncated.OR.Is_Truncated(cStr, NumInfo%FractionStart, NumInfo%FractionEnd)
                END IF
                IF (Truncated) THEN
                    CALL Round_Up_BigUInt(Big, Digits)
                END IF
                RETURN
            ELSE
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Counter = 0
                Value = 0_kInt64
            END IF
        END DO
    END IF

    ! add our fraction digits, if they're available.
    IF (NumInfo%FractionStart /= 0) THEN
        Indx = NumInfo%FractionStart
        IEnd = NumInfo%FractionEnd
        IF (Digits == 0) THEN
            CALL Skip_Zeros(cStr, Indx, IEnd)
        END IF
        ! process all digits, in increments of step per loop
        DO WHILE (Indx <= IEnd)
            DO WHILE ((Indx+7 <= IEnd).AND.(Step-Counter >= 8).AND.(Max_Digits-Digits >= 8))
                CALL Parse_Eight_Digits(cStr, Indx, Value, Counter, Digits)
            END DO
            DO WHILE ((Counter < Step).AND.(Indx <= IEnd).AND.(Digits < Max_Digits))
                CALL Parse_One_Digit(cStr, Indx, Value, Counter, Digits)
            END DO
            IF (Digits == Max_Digits) THEN
                ! add the temporary value, then check if we've truncated any digits
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                IF (Is_Truncated(cStr, Indx, IEnd)) THEN
                    CALL Round_Up_BigUInt(Big, Digits)
                END IF
                RETURN
            ELSE
                CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
                Counter = 0
                Value = 0_kInt64
            END IF
        END DO
    END IF

    IF (Counter /= 0) THEN
        CALL Add_Native(Big, Powers_of_Ten_Uint64(Counter), Value)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Skip_Zeros(cStr, IStart, IEnd)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To find IStart by skipping zeros.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)       :: cStr
        tSInt32,   INTENT(INOUT)    :: IStart
        tSInt32,   INTENT(IN)       :: IEnd

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: MConst = ToInt64(Z'3030303030303030')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(8)     :: wStr
        tUInt64         :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        DO WHILE (IStart + 7 <= IEnd)
            wStr = cStr(IStart:IStart+7)
            IF (wVal /= MConst) EXIT
            IStart = IStart + 8
        END DO
        DO WHILE (IStart <= IEnd)
            IF (cStr(IStart:IStart) /= '0') EXIT
            IStart = IStart + 1
        END DO

        RETURN

    END SUBROUTINE Skip_Zeros

    !**************************************************************************

    FUNCTION Is_Truncated(cStr, IStart, IEnd) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To determine if any non-zero digits were truncated.
        ! all characters must be valid digits.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr
        tSInt32,   INTENT(IN)   :: IStart
        tSInt32,   INTENT(IN)   :: IEnd
        tLogical                :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: MConst = ToInt64(Z'3030303030303030')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Indx
        tCharLen(8)     :: wStr
        tUInt64         :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        ! initialize
        Indx = IStart
        Flag = TrueVal

        ! do 8-bit optimizations, can just compare to 8 literal 0s.
        DO WHILE (Indx + 7 <= IEnd)
            wStr = cStr(Indx:Indx+7)
            IF (wVal /= MConst) RETURN
            Indx = Indx + 8
        END DO
        DO WHILE (Indx <= IEnd)
            IF (cStr(Indx:Indx) /= '0') RETURN
            Indx = Indx + 1
        END DO
        Flag = FalseVal

        RETURN

    END FUNCTION Is_Truncated

    !**************************************************************************

    SUBROUTINE Parse_Eight_Digits(cStr, Indx, Value, Counter, Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse 8 digits immediately.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)       :: cStr
        tSInt32,   INTENT(INOUT)    :: Indx, Counter, Count
        tUInt64,   INTENT(INOUT)    :: Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(8)     :: wStr
        tUInt64         :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        wStr = cStr(Indx:Indx+7)
        Value = Value*100000000_kInt64 + Parse_Eight_Digits_Unrolled(wVal)
        Indx = Indx + 8
        Counter = Counter + 8
        Count = Count + 8

        RETURN

    END SUBROUTINE Parse_Eight_Digits

    !**************************************************************************

    SUBROUTINE Parse_One_Digit(cStr, Indx, Value, Counter, Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse 1 digit

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)       :: cStr
        tSInt32,   INTENT(INOUT)    :: Indx, Counter, Count
        tUInt64,   INTENT(INOUT)    :: Value

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: A0 = IACHAR('0')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Value = Value*10_kInt64 + (IACHAR(cStr(Indx:Indx))-A0)
        Indx = Indx + 1
        Counter = Counter + 1
        Count = Count + 1

        RETURN

    END SUBROUTINE Parse_One_Digit

    !**************************************************************************

    SUBROUTINE Add_Native(Big, Power, Value)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To add value to BigUInt

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BigUInt), INTENT(INOUT)    :: Big
        tUInt64,       INTENT(IN)       :: Power, Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        CALL Big%SmallMul(Power)
        CALL Big%Add(Value)

        RETURN

    END SUBROUTINE Add_Native

    !**************************************************************************

    SUBROUTINE Round_Up_BigUInt(Big, Count)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To round BigUInt up

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(BigUInt), INTENT(INOUT)    :: Big
        tSInt32,       INTENT(INOUT)    :: Count

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! need to round-up the digits, but need to avoid rounding
        ! ....9999 to ...10000, which could cause a false halfway point.
        CALL Add_Native(Big, 10_kInt64, 1_kInt64)
        Count = Count + 1

        RETURN

    END SUBROUTINE Round_Up_BigUInt

    !**************************************************************************

END SUBROUTINE Parse_Mantissa

!******************************************************************************

SUBROUTINE Positive_Digit_Comparision(Big, Exp, E2, M2)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare BigInt for positive exponent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big
    tSInt32,       INTENT(IN)       :: Exp
    tSInt32,       INTENT(OUT)      :: E2    ! exponent in base 2
#ifndef tFloat_is_tQuad
    tUInt64,       INTENT(OUT)      :: M2    ! adjusted significand in base 2
#else
    tUInt128,      INTENT(OUT)      :: M2    ! adjusted significand in base 2
#endif

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: Offset = Mantissa_Explicit_Bits - Minimum_Exponent - MantTotalBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Truncated

!** FLOW

    CALL Big%Pow10(Exp)

#ifndef tFloat_is_tQuad
    M2 = Big%Hi64(Truncated)
#else
    M2 = Big%Hi128(Truncated)
#endif
    E2 = Big%BitLen() + Offset
    CALL Round(E2, M2, Callback_Round)

    RETURN

CONTAINS

    SUBROUTINE Callback_Round(E, M, Shift)
        ! arguments
        tSInt32,  INTENT(INOUT) :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(INOUT) :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(INOUT) :: M    ! adjusted significand in base 2
#endif
        tSInt32,  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Nearest_Tie_Even(E, M, Shift, Callback_Round_Nearest)
        RETURN
    END SUBROUTINE

    !**************************************************************************

    FUNCTION Callback_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
        ! arguments
        tLogical, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
        tLogical                :: Flag
        ! execution
        Flag = IsAbove.OR.(IsHalfway.AND.Truncated).OR.(IsOdd.AND.IsHalfway)
        RETURN
    END FUNCTION

    !**************************************************************************

END SUBROUTINE Positive_Digit_Comparision

!******************************************************************************

SUBROUTINE Negative_Digit_Comparision(Big, EIn, MIn, Exp, EOut, MOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare BigInt for negative exponent.
    !
    ! The scaling here is quite simple: we have, for the real digits `m * 10^e`,
    ! and for the theoretical digits `n * 2^f`. Since `e` is always negative,
    ! to scale them identically, we do `n * 2^f * 5^-f`, so we now have `m * 2^e`.
    ! we then need to scale by `2^(f- e)`, and then the two significant digits
    ! are of the same magnitude.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), TARGET, INTENT(INOUT)    :: Big
    tSInt32,               INTENT(IN)       :: EIn
    tSInt32,               INTENT(IN)       :: Exp
    tSInt32,               INTENT(OUT)      :: EOut
#ifndef tFloat_is_tQuad
    tUInt64,               INTENT(IN)       :: MIn
    tUInt64,               INTENT(OUT)      :: MOut
#else
    tUInt128,              INTENT(IN)       :: MIn
    tUInt128,              INTENT(OUT)      :: MOut
#endif

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(BigUInt), POINTER  :: RealDigits => NULL()
    TYPE(BigUInt)           :: TheoryDigits
    tSInt32                 :: RealExp, TheoryExp
    tSInt32                 :: Pow2_Exp, Pow5_Exp
    tSInt32                 :: Ord
    tFloat                  :: FloatBase
    tSInt32                 :: EBase
    tSInt32                 :: ETheory
#ifndef tFloat_is_tQuad
    tUInt64                 :: MBase
    tUInt64                 :: MTheory
#else
    tUInt128                :: MBase
    tUInt128                :: MTheory
#endif

!** FLOW

    ! set working variables
    RealDigits => Big
    RealExp = Exp

    ! get the value of `b`, rounded down, and get a bigint representation of b+h
    EBase = EIn
    MBase = MIn
    CALL Round(EBase, MBase, CBRound)
    CALL To_Float(FalseVal, EBase, MBase, FloatBase)

    CALL To_Extended_Halfway(FloatBase, ETheory, MTheory)
#ifndef tFloat_is_tQuad
    CALL TheoryDigits%FromU64(MTheory)
#else
    CALL TheoryDigits%FromU128(MTheory)
#endif
    TheoryExp = ETheory

    ! scale real digits and theor digits to be same power.
    Pow2_Exp = TheoryExp - RealExp
    Pow5_Exp = -RealExp
    IF (Pow5_Exp /= 0) THEN
        CALL TheoryDigits%Pow5(Pow5_Exp)
    END IF
    IF (Pow2_Exp > 0) THEN
        CALL TheoryDigits%Pow2(Pow2_Exp)
    ELSEIF (Pow2_Exp < 0) THEN
        CALL RealDigits%Pow2(-Pow2_Exp)
    END IF

    ! compare digits, and use it to director rounding
    Ord = RealDigits%Compare(TheoryDigits)
    EOut = EIn
    MOut = MIn
    CALL Round(EOut, MOut, Callback_Round)

    ! free pointer
    NULLIFY(RealDigits)

    RETURN

CONTAINS

    SUBROUTINE CBRound(E, M, Shift)
        ! arguments
        tSInt32,  INTENT(INOUT) :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(INOUT) :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(INOUT) :: M    ! adjusted significand in base 2
#endif
        tSInt32,  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Down(E, M, Shift)
        RETURN
    END SUBROUTINE

    !**************************************************************************

    SUBROUTINE Callback_Round(E, M, Shift)
        ! arguments
        tSInt32,  INTENT(INOUT) :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(INOUT) :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(INOUT) :: M    ! adjusted significand in base 2
#endif
        tSInt32,  INTENT(IN)    :: Shift
        ! execution
        CALL Round_Nearest_Tie_Even(E, M, Shift, Callback_Round_Nearest)
        RETURN
    END SUBROUTINE

    !**************************************************************************

    FUNCTION Callback_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
        ! arguments
        tLogical, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
        tLogical                :: Flag
        ! execution
        IF (Ord > 0) THEN
            Flag = TrueVal
        ELSEIF (Ord < 0) THEN
            Flag = FalseVal
        ELSE
            Flag = IsOdd
        END IF
        RETURN
    END FUNCTION

    !**************************************************************************

    SUBROUTINE To_Extended(Value, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a native floating-point number to an extended-precision float.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tFloat,   INTENT(IN)    :: Value
        tSInt32,  INTENT(OUT)   :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(OUT)   :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(OUT)   :: M    ! adjusted significand in base 2
#endif

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: Bias = Mantissa_Explicit_Bits - Minimum_Exponent

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifndef tFloat_is_tQuad
        tUInt64     :: Bits
#else
        tUInt128    :: Bits
#endif

    !** FLOW

#ifdef tFloat_is_tSingle
        Bits = ToInt64(RawFP_FromFloat(Value))
#else
        Bits = RawFP_FromFloat(Value)
#endif

        IF (IAND(Bits, Exponent_Mask) == ZeroUInt) THEN
            ! denormal
            E = 1 - Bias
            M = IAND(Bits, Mantissa_Mask)
        ELSE
            ! normal
            E = ToI32(SHIFTR(IAND(Bits, Exponent_Mask), Mantissa_Explicit_Bits)) - Bias
            M = IOR(IAND(Bits, Mantissa_Mask), Hidden_Bit_Mask)
        END IF

        RETURN

    END SUBROUTINE To_Extended

    !**************************************************************************

    SUBROUTINE To_Extended_Halfway(Value, E, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To get the extended precision value of the halfway point between b and b+u.
        ! we are given a native float that represents b, so we need to adjust it
        ! halfway between b and b+u.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tFloat,   INTENT(IN)    :: Value
        tSInt32,  INTENT(OUT)   :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(OUT)   :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(OUT)   :: M    ! adjusted significand in base 2
#endif

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        CALL To_Extended(Value, E, M)
        M = SHIFTL(M, 1) + OneMant
        E = E - 1

        RETURN

    END SUBROUTINE To_Extended_Halfway

    !**************************************************************************

    SUBROUTINE Round_Down(E, M, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To round an extended-precision float down.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(INOUT) :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(INOUT) :: M    ! adjusted significand in base 2
#endif
        tSInt32,  INTENT(IN)    :: Shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (Shift == MantTotalBits) THEN
            M = ZeroUInt
        ELSE
            M = SHIFTR(M, Shift)
        END IF
        E = E + Shift

        RETURN

    END SUBROUTINE Round_Down

    !**************************************************************************

    SUBROUTINE To_Float(Negative, E, M, Value)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert adjusted mantissa to double-precision value.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLogical, INTENT(IN)    :: Negative
        tSInt32,  INTENT(IN)    :: E    ! exponent in base 2
#ifndef tFloat_is_tQuad
        tUInt64,  INTENT(IN)    :: M    ! adjusted significand in base 2
#else
        tUInt128, INTENT(IN)    :: M    ! adjusted significand in base 2
#endif
        tFloat,   INTENT(OUT)   :: Value

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(BinRep)    :: FpBin
        tUIntType       :: Word

    !** FLOW

        ! get input
        FpBin%Negative = Negative
        FpBin%Exponent = E
        FpBin%Significand = M       ! implicit narrowing conversion for 32 bit

        ! compose the component parts into word
        Word = RawFP_Construct(FpBin)

        ! convert word to real number
        Value = RawFP_ToFloat(Word)

        RETURN

    END SUBROUTINE To_Float

    !**************************************************************************

END SUBROUTINE Negative_Digit_Comparision

!******************************************************************************

FUNCTION Dec2Bin_YY(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
    ! To use YY's algorithm to convert from decimal representation
    ! to (raw) binary representation

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType,       INTENT(IN) :: SigDec   ! significand, base 10
    tSInt32,         INTENT(IN) :: ExpDec   ! exponent, base 10
    tLogical,        INTENT(IN) :: Negative ! true if the floating point value is negative
    tCharStar,       INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux), INTENT(IN) :: Aux      ! auxiliary string information
    tUIntType                   :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: MinExpFastPath = -DecimalRange
    tSInt32,  PARAMETER :: MaxExpFastPath = DecimalRange - UIntSafeDigits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType   :: SigBin   ! significand, base 2
    tUInt32     :: ExpBin   ! exponent, base 2

!** FLOW

    ! check whether to use YY's fast path
    IF ((.NOT.Aux%Truncated).AND.(ExpDec > MinExpFastPath).AND.(ExpDec < MaxExpFastPath)) THEN
        IF (D2B_YY_FastPath(SigDec, ExpDec, SigBin, ExpBin)) THEN
            ! YY's fast path is success so set sign bit
            IF (Negative) THEN
                RawFP = SignMask
            ELSE
                RawFP = ZeroUInt
            END IF
            ! then, add exponent bits
            RawFP = IOR(RawFP, SHIFTL(ToUIntType(ExpBin), SignificandBits))
            ! finally, add (both implicit and explicit) significand bits
            RawFP = IOR(RawFP, IAND(SigBin, SignificandMask))
            RETURN
        END IF
    END IF

    ! perform decimal to binary conversion using YY's slow path
    RawFP = D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux)

    RETURN

    CONTAINS

    FUNCTION D2B_YY_FastPath(SigDec, ExpDec, SigBin, ExpBin) RESULT(Success)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To use YY's fast path algorithm to convert from decimal representation
        ! to binary representation

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: SigDec   ! significand in base 10
        tSInt32,   INTENT(IN)   :: ExpDec   ! exponent in base 10
        tUIntType, INTENT(OUT)  :: SigBin   ! significand in base 2
        tUInt32,   INTENT(OUT)  :: ExpBin   ! exponent in base 2
        tLogical                :: Success  ! true if conversion can be handled

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64     :: Pow10(0:3)   ! in little endian order; most significant byte is 3
        tUInt64     :: Mantissa(0:1), Sig2(0:1), Sig2_Ext(0:1)
        tUInt64     :: FirstProduct(0:3)
        tUInt64     :: SecondProduct(0:3)
#else
#ifdef  tFloat_is_tDouble
        tUInt128    :: Pow10
        tUInt64     :: Sig2, Sig2_Ext
#else
        tUInt64     :: Pow10
        tUInt64     :: Sig2, Sig2_Ext
        tUInt64     :: FirstProduct, SecondProduct
#endif
#endif
        tUIntType   :: Hi, Lo, Hi2
        tUIntType   :: Sig1, Add, Bits
        tSInt32     :: Exp2, Lz
        tLogical    :: Exact

    !** FLOW

        ! To keep it simple, we only accept normal number here,
        ! let the slow path to handle subnormal and infinity number.

        ! The result value is exactly equal to (SigDec * 10**ExpDec),
        ! the exponent part (10**ExpDec) can be converted to (Sig2 * 2**Exp2).

        ! The Sig2 can be an infinite length number, only the highest 256 bits
        ! is cached in the Pow10_Sig_Table.
        ! (Quad uses 256 bits, Double uses 128 bits, and Single uses 64 bits)

        ! Now we have these bits:
        ! Sig1 (normalized 128/64/32 bit)   : aaaaaaaaaaaaaaaa
        ! Sig2 (higher 128/64/32 bit)       : bbbbbbbbbbbbbbbb
        ! Sig2_Ext (lower 128/64/32 bit)    : cccccccccccccccc
        ! Sig2_Cut (extra unknown bits)     : dddddddddddddddddddddddd....

        ! And the calculation process is:
        ! -------------------------------------------------------------
        !         aaaaaaaaaaaaaaaa *
        !         bbbbbbbbbbbbbbbbccccccccccccccccdddddddddddd....
        ! -------------------------------------------------------------
        ! abababababababababababababababab +
        !         acacacacacacacacacacacacacacacac +
        !                 adadadadadadadadadadadadadadadadadadadad....
        ! -------------------------------------------------------------
        ! [Hi____][Lo____] +
        !         [Hi2___][Lo2___] +
        !                 [unknown___________....]
        ! -------------------------------------------------------------

        ! The addition with carry may affect higher bits, but if there is a 0
        ! in higher bits, the bits higher than 0 will not be affected.

        ! 'Lo2' + 'unknown' may get a carry bit and may affect 'Hi2', the max value
        ! of 'Hi2' is 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE/0xFFFFFFFFFFFFFFFE/0xFFFFFFFE,
        ! so 'Hi2' will not overflow.

        ! 'Lo' + 'Hi2' may alse get a carry bit and may affect 'Hi', but only
        ! the highest significant 113/53/24 bits of 'Hi' is needed. If there is a 0
        ! in the lower bits of 'Hi', then all the following bits can be dropped.

        ! To convert the result to IEEE-754 double number, we need to perform
        ! correct rounding:
        ! 1. if bit 114/54/25 is 0, round down,
        ! 2. if bit 114/54/25 is 1 and any bit beyond bit 114/54/25 is 1, round up,
        ! 3. if bit 114/54/25 is 1 and all bits beyond bit 114/54/25 are 0, round to even,
        !    as the extra bits is unknown, this case will not be handled here.

        ! initialize
        Exact   = FalseVal
        Success = FalseVal

        ! convert (10*ExpDec) to (Sig2 * 2**Exp2)
#ifdef  tFloat_is_tQuad
        Pow10    = Get_Pow10_256Bits(ExpDec)
        Sig2     = Pow10(2:3)
        Sig2_Ext = Pow10(0:1)
#else
#ifdef  tFloat_is_tDouble
        Pow10    = Get_Pow10_128Bits(ExpDec)
        Sig2     = Pow10%High
        Sig2_Ext = Pow10%Low
#else
        Pow10    = Get_Pow10_64Bits(ExpDec)
        Sig2     = SHIFTR(Pow10, 32)
        Sig2_Ext = IAND(Pow10, ToInt64(Z'00000000FFFFFFFF'))
#endif
#endif
        Exp2     = Floor_Log2_Pow10(ExpDec) - SignBits

        ! normalize and multiply
        Lz   = LEADZ(SigDec)
        Sig1 = SHIFTL(SigDec, Lz)
        Exp2 = Exp2 - Lz
#ifdef  tFloat_is_tQuad
        Mantissa(0) = Sig1%Low
        Mantissa(1) = Sig1%High
        CALL MultiplyBasic(Mantissa, 2, Sig2, 2, FirstProduct)
        Lo = UInt128(FirstProduct(1), FirstProduct(0))
        Hi = UInt128(FirstProduct(3), FirstProduct(2))
#else
#ifdef  tFloat_is_tDouble
        CALL UMul128(Sig1, Sig2, Hi, Lo)
#else
        FirstProduct = ToUnsignedLong(Sig1)*Sig2
        Lo = ToInt32(IAND(FirstProduct, ToInt64(Z'00000000FFFFFFFF')))
        Hi = ToInt32(SHIFTR(FirstProduct, 32))
#endif
#endif

        ! To get normalized value, 'Hi' should be shifted to the left by 0 or 1.

        ! The highest significant 113/53/24 bits is used by IEEE-754 double number,
        ! and the bit 114/54/25 is used to detect rounding direction.

        ! The lowest 13 (= 128 - 114 - 1) / 9 (= 64 - 54 - 1) / 6 (= 32 - 25 - 1) bits (LowBits)
        ! is used to check whether it contains 0.
        ! Note: BitMask = SHIFTL(1, LowBits) - 1
        Bits = IAND(Hi, BitMask)

        IF ((Bits /= ZeroUInt).AND.(Bits /= BitMask)) THEN
            ! The 'Bits' is not zero, so we don't need to check 'round to even' case.
            ! The 'Bits' contains bit '0', so we can drop the extra bits after '0'.
            Exact = TrueVal
        ELSE
            ! The 'Bits' is filled with all '0' or all '1', so we need to check
            ! more lower bits with another multiplication.
#ifdef  tFloat_is_tQuad
            CALL MultiplyBasic(Mantissa, 2, Sig2_Ext, 2, SecondProduct)
            Hi2 = UInt128(SecondProduct(3), SecondProduct(2))
#else
#ifdef  tFloat_is_tDouble
            Hi2 = UMul128_Upper64(Sig1, Sig2_Ext)
#else
            SecondProduct = ToUnsignedLong(Sig1)*Sig2_Ext
            Hi2 = ToInt32(SHIFTR(SecondProduct, 32))
#endif
#endif

            Add = Lo + Hi2
            IF ((Add /= ZeroUInt).AND.(Add /= MaxUInt)) THEN
                ! The 'Add' is not zero, so we don't need to check 'round to even' case.
                ! The 'Add' contains bit '0', so we can drop the extra bits after '0'.
                ! The 'Hi' cannot be MaxUInt, so it will not overflow.
                IF ((Add .ULT. Lo).OR.(Add .ULT. Hi2)) Hi = Hi + OneUInt
                Exact = TrueVal
            END IF
        END IF
        IF (Exact) THEN
            ! normalize
            IF (Hi .ULT. SignMask) THEN
                Hi = SHIFTL(Hi, 1)
                Exp2 = Exp2 - 1
            END IF
            Exp2 = Exp2 + TotalBits

            ! test the bit 114 and get rounding direction
            IF (IAND(Hi, AddRound) /= ZeroUInt) Hi = Hi + AddRound

            ! test overflow
            IF (Hi .ULT. AddRound) THEN
                Hi = SignMask
                Exp2 = Exp2 + 1
            END IF

            ! This is a normal number, convert it to binary representation.
            SigBin = SHIFTR(Hi, ExponentBits)
            ExpBin = Exp2 + (ExponentBits + SignificandBits) + ExponentBias
            Success = TrueVal
        END IF

        RETURN

    END FUNCTION D2B_YY_FastPath

    !**************************************************************************

END FUNCTION Dec2Bin_YY

!******************************************************************************

FUNCTION D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
    ! To use YY's slow path algorithm to convert from decimal representation
    ! to (raw) binary representation

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType,       INTENT(IN) :: SigDec   ! significand
    tSInt32,         INTENT(IN) :: ExpDec   ! exponent
    tLogical,        INTENT(IN) :: Negative ! true if the floating point value is negative
    tCharStar,       INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux), INTENT(IN) :: Aux      ! auxiliary string information
    tUIntType                   :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: ERR_ULP_LOG    = 3
    tSInt32, PARAMETER  :: ERR_ULP        = SHIFTL(1, ERR_ULP_LOG)
    tSInt32, PARAMETER  :: ERR_CACHED_POW = ERR_ULP / 2
    tSInt32, PARAMETER  :: ERR_MUL_FIXED  = ERR_ULP / 2
    tSInt32, PARAMETER  :: DIY_SIG_BITS   = TotalBits
    tSInt32, PARAMETER  :: EXP_BIAS       = ExponentBias + SignificandBits
    tSInt32, PARAMETER  :: EXP_SUBNORMAL  = -EXP_BIAS + 1
    tSInt32, PARAMETER  :: A0             = IACHAR('0')

!** SUBROUTINE DERIVED TYPE DEFINITIONS
    ! Do It Yourself Floating Point" struct
    TYPE Diy_Fp
        tUIntType       :: Sig      ! significand
        tSInt32         :: Exp      ! exponent, base 2
    END TYPE
    ! ----------------------------------------------------------------------------

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType       :: Sign
    tUIntType       :: FpErr, PrecisionBits, HalfWay
    tUInt32         :: Bits
    tSInt32         :: Order_of_Magnitude, Effective_Significand_Size
    tSInt32         :: PrecisionDigitsCount, Cmp, Exp10
    TYPE(Diy_Fp)    :: Fp, FpUpper
    TYPE(BigUInt)   :: BigFull, BigComp

!** FLOW

    ! Slow path: read floating-point number exactly with diyfp.
    ! 1. Use cached Diy_Fp to get an approximation value.
    ! 2. Use BigUInt to check the approximation value if needed.

    ! This algorithm refers to google's double-conversion project:
    ! https://github.com/google/double-conversion

    ! initialize
    IF (Negative) THEN
        Sign = OneUInt
    ELSE
        Sign = ZeroUInt
    END IF

    Fp%Sig = SigDec
    Fp%Exp = 0
    IF (Aux%Truncated) THEN
        FpErr = ToUIntType(ERR_ULP / 2)
        ! round up if the next digit after the cut is more than or equal to 5
        IF ((IACHAR(cStr(Aux%SigCut:Aux%SigCut))-A0) >= 5) Fp%Sig = Fp%Sig + OneUInt
    ELSE
        FpErr = ZeroUInt
    END IF

    ! normalize
    Bits   = LEADZ(Fp%Sig)
    Fp%Sig = SHIFTL(Fp%Sig, Bits)
    Fp%Exp = Fp%Exp - Bits
    FpErr  = SHIFTL(FpErr, Bits)

    ! multiply and add error
    Fp = Diy_Fp_Mul(Fp, Diy_Fp_Get_Cached_Pow10(ExpDec))
    IF (FpErr == ZeroUInt) THEN
        FpErr = FpErr + ToUIntType(ERR_CACHED_POW + ERR_MUL_FIXED)
    ELSE
        FpErr = FpErr + ToUIntType(ERR_CACHED_POW + ERR_MUL_FIXED + 1)
    END IF

    ! normalize
    Bits   = LEADZ(Fp%Sig)
    Fp%Sig = SHIFTL(Fp%Sig, Bits)
    Fp%Exp = Fp%Exp - Bits
    FpErr  = SHIFTL(FpErr, Bits)

    ! effective significand
    Order_of_Magnitude = DIY_SIG_BITS + Fp%Exp
    IF (Order_of_Magnitude >= EXP_SUBNORMAL + BinaryPrecision) THEN
        Effective_Significand_Size = BinaryPrecision
    ELSEIF (Order_of_Magnitude <= EXP_SUBNORMAL) THEN
        Effective_Significand_Size = 0
    ELSE
        Effective_Significand_Size = Order_of_Magnitude - EXP_SUBNORMAL
    END IF

    ! precision digits count
    PrecisionDigitsCount = DIY_SIG_BITS - Effective_Significand_Size
    IF (PrecisionDigitsCount + ERR_ULP_LOG >= DIY_SIG_BITS) THEN
        BLOCK
            tSInt32     :: Shr
            Shr = (PrecisionDigitsCount + ERR_ULP_LOG) - DIY_SIG_BITS + 1
            Fp%Sig = SHIFTR(Fp%Sig, Shr)
            Fp%Exp = Fp%Exp + Shr
            FpErr  = SHIFTR(FpErr, Shr) + ToUIntType(1 + ERR_ULP)
            PrecisionDigitsCount = PrecisionDigitsCount - Shr
        END BLOCK
    END IF

    ! half way
    PrecisionBits = IAND(Fp%Sig, (SHIFTL(OneUInt, PrecisionDigitsCount) - OneUInt))
    PrecisionBits = PrecisionBits*ERR_ULP
    HalfWay = SHIFTL(OneUInt, (PrecisionDigitsCount - 1))
    HalfWay = HalfWay*ERR_ULP

    ! rounding
    Fp%Sig = SHIFTR(Fp%Sig, PrecisionDigitsCount)
    IF (PrecisionBits .UGE. HalfWay + FpErr) Fp%Sig = Fp%Sig + OneUInt
    Fp%Exp = Fp%Exp + PrecisionDigitsCount

    ! get IEEE raw value
    RawFP = Diy_Fp_To_IEEE_Raw(Fp)

    IF (RawFP == FpRawInf) THEN
        RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)
        RETURN
    END IF
    IF ((PrecisionBits .ULE. HalfWay - FpErr).OR.(PrecisionBits .UGE. HalfWay + FpErr)) THEN
        ! number is accurate
        RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)
        RETURN
    END IF

    ! -------------------------------------------------------------------------
    ! now the number is the correct value, or the next lower value
    ! -------------------------------------------------------------------------

    ! upper boundary
    IF (IAND(RawFP, ExponentMask) /= ZeroUInt) THEN
        FpUpper%Sig = IAND(RawFP, SignificandMask) + SHIFTL(OneUInt, SignificandBits)
        FpUpper%Exp = ToI32(SHIFTR(IAND(RawFP, ExponentMask), SignificandBits))
    ELSE
        FpUpper%Sig = IAND(RawFP, SignificandMask)
        FpUpper%Exp = 1
    END IF
    FpUpper%Exp = FpUpper%Exp - (ExponentBias + SignificandBits)
    FpUpper%Sig = SHIFTL(FpUpper%Sig, 1)
    FpUpper%Exp = FpUpper%Exp - 1
    FpUpper%Sig = FpUpper%Sig + 1     ! add half ulp

    ! compare with BigInt
    Exp10 = ExpDec
    CALL BigInt_Set_String(BigFull, SigDec, Exp10, cStr, Aux)
    CALL BigInt_Set_UIntType(BigComp, FpUpper%Sig)
    IF (Exp10 >= 0) THEN
        CALL BigInt_Mul_Pow10(BigFull, +Exp10)
    ELSE
        CALL BigInt_Mul_Pow10(BigComp, -Exp10)
    END IF
    IF (FpUpper%Exp > 0) THEN
        CALL BigInt_Mul_Pow2(BigComp, +FpUpper%Exp)
    ELSE
        CALL BigInt_Mul_Pow2(BigFull, -FpUpper%Exp)
    END IF

    Cmp = BigInt_Compare(BigFull, BigComp)
    IF (Cmp /= 0) THEN
        ! round down or round up
        IF (Cmp > 0) RawFP = RawFP + OneUInt
    ELSE
        ! falls midway, round to even
        IF (IAND(RawFP, OneUInt) /= ZeroUInt) RawFP = RawFP + OneUInt
    END IF
    RawFP = IOR(SHIFTL(Sign, SignBits), RawFP)

    RETURN

    CONTAINS

    FUNCTION Diy_Fp_Get_Cached_Pow10(Exp10) RESULT(Fp)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To get cached rounded diy_fp with pow(10, e). The input value must in range
        ! [POW10_SIG_TABLE_MIN_EXP, POW10_SIG_TABLE_MAX_EXP].

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32, INTENT(IN) :: Exp10    ! an exponent
        TYPE(Diy_Fp)        :: Fp       ! Diy_Fp data

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
        tUIntType   :: Sig_Ext

    !** FLOW

#ifdef  tFloat_is_tQuad
        Pow10   = Get_Pow10_256Bits(Exp10)
        Fp%Sig  = UInt128(Pow10(3), Pow10(2))
        Sig_Ext = UInt128(Pow10(1), Pow10(0))
#else
#ifdef  tFloat_is_tDouble
        Pow10   = Get_Pow10_128Bits(Exp10)
        Fp%Sig  = Pow10%High
        Sig_Ext = Pow10%Low
#else
        Pow10   = Get_Pow10_64Bits(Exp10)
        Fp%Sig  = ToInt32(SHIFTR(Pow10, 32))
        Sig_Ext = ToInt32(IAND(Pow10, ToInt64(Z'00000000FFFFFFFF')))
#endif
#endif

        Fp%Exp = Floor_Log2_Pow10(Exp10) - SignBits
        Fp%Sig = Fp%Sig + SHIFTR(Sig_Ext, SignBits)

        RETURN

    END FUNCTION Diy_Fp_Get_Cached_Pow10

    !**************************************************************************

    FUNCTION Diy_Fp_Mul(Fp1, Fp2) RESULT(Fp)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To evaluate 'fp1 * fp2'.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(Diy_Fp), INTENT(IN)    :: Fp1, Fp2
        TYPE(Diy_Fp)                :: Fp

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUIntType   :: Hi, Lo
#ifdef  tFloat_is_tSingle
        tUInt64     :: Product
#endif

    !** FLOW

#ifdef  tFloat_is_tQuad
        CALL UMul256(Fp1%Sig, Fp2%Sig, Hi, Lo)
#else
#ifdef  tFloat_is_tDouble
        CALL UMul128(Fp1%Sig, Fp2%Sig, Hi, Lo)
#else
        Product = ToUnsignedLong(Fp1%Sig)*ToUnsignedLong(Fp2%Sig)
        Lo = ToInt32(IAND(Product, ToInt64(Z'00000000FFFFFFFF')))
        Hi = ToInt32(SHIFTR(Product, 32))
#endif
#endif

        Fp%Sig = Hi + SHIFTR(Lo, SignBits)
        Fp%Exp = Fp1%Exp + Fp2%Exp + TotalBits

        RETURN

    END FUNCTION Diy_Fp_Mul

    !**************************************************************************

    FUNCTION Diy_Fp_To_IEEE_Raw(Fp) RESULT(Val)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert diy_fp to IEEE-754 raw value.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(Diy_Fp), INTENT(IN)    :: Fp
        tUIntType                   :: Val

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUIntType   :: Sig
        tSInt32     :: Exp
        tSInt32     :: Lz_Bits

    !** FLOW

        ! initialize
        Sig = Fp%Sig
        Exp = Fp%Exp
        Val = ZeroUInt
        IF (Sig == ZeroUInt) RETURN

        ! compute significand and exponent
        Lz_Bits = LEADZ(Sig)
        Sig = SHIFTL(Sig, Lz_Bits)
        Sig = SHIFTR(Sig, ExponentBits)
        Exp = Exp - Lz_Bits + ExponentBits + SignificandBits

        ! check which range the result falls
        IF (Exp >= MaxExpBin) THEN
            ! overflow
            Val = RawFP_SetInfinity(FalseVal)
        ELSEIF (Exp >= (MinExpBin - 1)) THEN
            ! normal
            Exp = Exp + ExponentBias
            Val = IOR(SHIFTL(ToUIntType(Exp), SignificandBits), IAND(Sig, SignificandMask))
        ELSEIF (Exp >= (MinExpBin - BinaryPrecision)) THEN
            ! subnormal
            Val = SHIFTR(Sig, (MinExpBin - Exp - 1))
        ELSE
            ! underflow
            Val = ZeroUInt
        END IF

        RETURN

    END FUNCTION Diy_Fp_To_IEEE_Raw

    !**************************************************************************

END FUNCTION D2B_YY_SlowPath

!******************************************************************************

FUNCTION Dec2Bin_Lemire(SigDec, ExpDec, Negative, cStr, Aux) RESULT(RawFP)

!** PURPOSE OF THIS SUBROUTINE:
    ! To use Lemire's algorithm to convert from decimal representation
    ! to (raw) binary representation

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType,       INTENT(IN) :: SigDec   ! significand, base 10
    tSInt32,         INTENT(IN) :: ExpDec   ! exponent, base 10
    tLogical,        INTENT(IN) :: Negative ! true if the floating point value is negative
    tCharStar,       INTENT(IN) :: cStr     ! floating-point number string
    TYPE(StringAux), INTENT(IN) :: Aux      ! auxiliary string information
    tUIntType                   :: RawFP    ! floating point number as an unsigned integer

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: MinExpFastPath = - DecimalRange - UIntSafeDigits + 1
    tSInt32,  PARAMETER :: MaxExpFastPath = DecimalRange + 2

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType   :: SigBin   ! significand, base 2
    tUInt32     :: ExpBin   ! exponent, base 2

!** FLOW

    ! check whether to use Lemire's fast path
    IF ((.NOT.Aux%Truncated).AND.(ExpDec > MinExpFastPath).AND.(ExpDec < MaxExpFastPath)) THEN
        IF (D2B_Lemire_FastPath(SigDec, ExpDec, SigBin, ExpBin)) THEN
            ! Lemire's fast path is success so set sign bit
            IF (Negative) THEN
                RawFP = SignMask
            ELSE
                RawFP = ZeroUInt
            END IF
            ! then, add exponent bits
            RawFP = IOR(RawFP, SHIFTL(ToUIntType(ExpBin), SignificandBits))
            ! finally, add (both implicit and explicit) significand bits
            RawFP = IOR(RawFP, IAND(SigBin, SignificandMask))
            RETURN
        END IF
    END IF

    ! perform decimal to binary conversion using YY's slow path
    RawFP = D2B_YY_SlowPath(SigDec, ExpDec, Negative, cStr, Aux)

    RETURN

    CONTAINS

    FUNCTION D2B_Lemire_FastPath(SigDec, ExpDec, SigBin, ExpBin) RESULT(Success)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To use Lemire's fast path algorithm to convert from decimal to binary representation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUIntType, INTENT(IN)   :: SigDec   ! significand in base 10
        tSInt32,   INTENT(IN)   :: ExpDec   ! exponent in base 10
        tUIntType, INTENT(OUT)  :: SigBin   ! significand in base 2
        tUInt32,   INTENT(OUT)  :: ExpBin   ! exponent in base 2
        tLogical                :: Success  ! true if conversion can be handled

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef  tFloat_is_tQuad
        tUInt64     :: Pow10(0:3)   ! in little endian order; most significant byte is 3
        tUInt64     :: Mantissa(0:1)
        tUInt64     :: MulProduct(0:3)
#else
#ifdef  tFloat_is_tDouble
        tUInt128    :: Pow10
#else
        tUInt64     :: Pow10
        tUInt64     :: MulProduct
#endif
#endif
        tSInt32     :: LZ
        tSInt32     :: Upperbit
        tUIntType   :: Significand
        tUIntType   :: Lower, Upper
        tUInt32     :: Exponent

    !** FLOW

        ! get 256/128/64-bit approximation of power of 10 (or power of 5)
#ifdef  tFloat_is_tQuad
        Pow10 = Get_Pow10_256Bits(ExpDec)
#else
#ifdef  tFloat_is_tDouble
        Pow10 = Get_Pow10_128Bits(ExpDec)
#else
        Pow10 = Get_Pow10_64Bits(ExpDec)
#endif
#endif

        ! compute the exponent
        Exponent = Floor_Log2_Pow10(ExpDec) + MaxExpBin + SignBits

        ! +++ normalize the significand +++
        ! We want the most significant bit of Significand to be 1. Shift if needed.
        LZ = LEADZ(SigDec)
        Significand = SHIFTL(SigDec, LZ)

        ! +++ perform multiplication +++
        ! We want the most significant 128/64/32 bits of the product. We know this will be non-zero
        ! because the most significant bit of Significand is 1.
#ifdef  tFloat_is_tQuad
        Mantissa(0) = Significand%Low
        Mantissa(1) = Significand%High
        CALL MultiplyBasic(Mantissa, 2, Pow10(2:3), 2, MulProduct)
        Lower = UInt128(MulProduct(1), MulProduct(0))
        Upper = UInt128(MulProduct(3), MulProduct(2))
#else
#ifdef  tFloat_is_tDouble
        CALL UMul128(Significand, Pow10%High, Upper, Lower)
#else
        MulProduct = ToUnsignedLong(Significand)*SHIFTR(Pow10, 32)
        Lower = ToInt32(IAND(MulProduct, ToInt64(Z'00000000FFFFFFFF')))
        Upper = ToInt32(SHIFTR(MulProduct, 32))
#endif
#endif

        ! We know that Upper has at most one leading zero because both Significand and  Pow10 have a leading one.
        ! As long as the first 13/9/6 bits of "upper" are not "1", then we know that we have an exact computed
        ! value for the leading 125/55/26 bits because any imprecision would play out as a +1, in the worst case.
        ! Having 125/55/26 bits is necessary because we need 123/53/24 bits for the mantissa but we have to have
        ! one rounding bit and we can waste a bit if the most significant bit of the product is zero. We expect
        ! this next branch to be rarely taken (say 1% of the time). When (Upper & BitMask) == BitMask,
        ! it can be common for Lower + Significand < Lower to be true (proba. much higher than 1%).
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        IF ((IAND(Upper, BitMask) == BitMask).AND.((Lower + Significand) .ULT. Lower)) THEN
            BLOCK
                ! --- declaration ---
                tUIntType   :: Product_Low, Product_High
                tUIntType   :: Product_Middle, Product_Middle1, Product_Middle2
                ! --- execution ---
                ! perform multiplication
#ifdef  tFloat_is_tQuad
                CALL MultiplyBasic(Mantissa, 2, Pow10(0:1), 2, MulProduct)
                Product_Low     = UInt128(MulProduct(1), MulProduct(0))
                Product_Middle2 = UInt128(MulProduct(3), MulProduct(2))
#else
#ifdef  tFloat_is_tDouble
                CALL UMul128(Significand, Pow10%Low, Product_Middle2, Product_Low)
#else
                MulProduct = ToUnsignedLong(Significand)*IAND(Pow10, ToInt64(Z'00000000FFFFFFFF'))
                Product_Low     = ToInt32(IAND(MulProduct, ToInt64(Z'00000000FFFFFFFF')))
                Product_Middle2 = ToInt32(SHIFTR(MulProduct, 32))
#endif
#endif
                Product_Middle1 = Lower
                Product_High    = Upper
                Product_Middle  = Product_Middle1 + Product_Middle2
                ! overflow carry
                IF (Product_Middle .ULT. Product_Middle1) Product_High = Product_High + OneUInt
                ! we want to check whether Pow10*Significand + Significand would affect our result
                ! This does happen, e.g. with 7.3177701707893310E+15 (for double-precision)
                IF (((Product_Middle + OneUInt == ZeroUInt).AND.(IAND(Product_High, BitMask) == BitMask) &
                    .AND.(Product_Low + Significand .ULT. Product_Low))) THEN
                    ! let us be prudent and bail out.
                    Success = FalseVal
                    RETURN
                END IF
                Lower = Product_Middle
                Upper = Product_High
            END BLOCK
        END IF

        ! The final mantissa should be 123/53/24 (BinaryPrecision) bits with a leading 1.
        ! We shift it so that it occupies 124/54/25 (BinaryPrecision+1) bits with a leading 1.
        Upperbit = ToI32(SHIFTR(Upper, SignBits))
        SigBin   = SHIFTR(Upper, (Upperbit + LowBits))
        LZ = LZ + IEOR(1, Upperbit)

        ! Here we have SigBin < SHIFTL(1, BinaryPrecision+1).

        ! We have to round to even. The "to even" part
        ! is only a problem when we are right in between two floats
        ! which we guard against.
        ! If we have lots of trailing zeros, we may fall right between two
        ! floating-point values.
        IF ((Lower == ZeroUInt).AND.(IAND(Upper, BitMask) == ZeroUInt).AND. &
            (IAND(SigBin, ThreeUInt) == OneUInt)) THEN
            ! if IAND(SigBin, 1) == 1 we might need to round up.
            ! Scenarios:
            ! 1. We are not in the middle. Then we should round up.
            ! 2. We are right in the middle. Whether we round up depends on the last significant
            !    bit: if it is "one" then we round up (round to even) otherwise, we do not.
            ! So if the last significant bit is 1, we can safely round up.  Hence we only need
            ! to bail out if IAND(SigBin, 3) == 1.  Otherwise we may need more accuracy or analysis
            ! to determine whether we are exactly between two floating-point numbers.
            ! It can be triggered with 1E23.
            ! Note: because the factor_mantissa and factor_mantissa_low are almost always rounded
            !       down (except for small positive powers), almost always should round up.
            Success = FalseVal
            RETURN
        END IF
        SigBin = SHIFTR(SigBin + IAND(SigBin, OneUInt), 1)
        ! Here we have SigBin < SHIFTL(1, BinaryPrecision), unless there was an overflow
        IF (SigBin .UGE. MaxMantissa) THEN
            ! This will happen when parsing values such as 7.2057594037927933E+16
            SigBin = SigHidBitMask
            ! undo previous addition
            LZ = LZ - 1
        END IF
        SigBin = IAND(SigBin, NOT(SigHidBitMask))
        ExpBin = Exponent - LZ
        ! we have to check that ExpBin is in range, otherwise we bail out
        IF ((ExpBin < 1).OR.(ExpBin > (MaxExponent-1))) THEN
            Success = FalseVal
        ELSE
            Success = TrueVal
        END IF

        RETURN

    END FUNCTION D2B_Lemire_FastPath

    !**************************************************************************

END FUNCTION Dec2Bin_Lemire

!******************************************************************************

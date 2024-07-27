
FUNCTION RawFP_BiasedExponent(RawVal) RESULT(BiasedExponent)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the biased exponent of the floating point value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tSInt32                 :: BiasedExponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BiasedExponent = ToI32(SHIFTR(IAND(RawVal, ExponentMask), SignificandBits))

    RETURN
    
END FUNCTION RawFP_BiasedExponent

!******************************************************************************

FUNCTION RawFP_UnbiasedExponent(RawVal) RESULT(UnbiasedExponent)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the unbiased exponent of the floating point value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tSInt32                 :: UnbiasedExponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: BiasedExponent

!** FLOW
    
    BiasedExponent   = RawFP_BiasedExponent(RawVal)
    UnbiasedExponent = BiasedExponent - ExponentBias
    IF (BiasedExponent == 0) UnbiasedExponent = UnbiasedExponent + 1

    RETURN
    
END FUNCTION RawFP_UnbiasedExponent

!******************************************************************************

FUNCTION RawFP_Significand(RawVal) RESULT(Significand)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the significand of the floating point value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tUIntType               :: Significand

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Significand = IAND(RawVal, SignificandMask)

    RETURN
    
END FUNCTION RawFP_Significand

!******************************************************************************

FUNCTION RawFP_Fraction(RawVal) RESULT(Fraction)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the fraction part of the floating point value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tUIntType               :: Fraction

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Fraction = RawFP_Significand(RawVal)
    IF (RawFP_BiasedExponent(RawVal) > 0) Fraction = IOR(Fraction, SigHidBitMask)

    RETURN
    
END FUNCTION RawFP_Fraction

!******************************************************************************

FUNCTION RawFP_IsZero(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is zero

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! Remove sign bit by shift
    Flag = SHIFTL(RawVal, 1) == ZeroUInt

    RETURN
    
END FUNCTION RawFP_IsZero

!******************************************************************************

FUNCTION RawFP_IsNaN(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is not a number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = (RawFP_BiasedExponent(RawVal) == MaxExponent).AND. &
           (RawFP_Significand(RawVal) /= ZeroUInt)

    RETURN
    
END FUNCTION RawFP_IsNaN

!******************************************************************************

FUNCTION RawFP_IsQuietNaN(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is a quiet NaN

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = IAND(RawVal, ExpMantMask) == IOR(ExponentMask, QuietNaNMask)

    RETURN
    
END FUNCTION RawFP_IsQuietNaN

!******************************************************************************

FUNCTION RawFP_IsInfinite(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is infinite

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = (RawFP_BiasedExponent(RawVal) == MaxExponent).AND. &
           (RawFP_Significand(RawVal) == ZeroUInt)

    RETURN
    
END FUNCTION RawFP_IsInfinite

!******************************************************************************

FUNCTION RawFP_IsInfOrNaN(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is infinite or NaN

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = IAND(RawVal, ExponentMask) == ExponentMask

    RETURN
    
END FUNCTION RawFP_IsInfOrNaN

!******************************************************************************

FUNCTION RawFP_IsMaximalFiniteMagnitude(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is equal to the maximal finite magnitude

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = (RawFP_BiasedExponent(RawVal) == (MaxExponent - 1)).AND. &
           (RawFP_Significand(RawVal) == SignificandMask)

    RETURN
    
END FUNCTION RawFP_IsMaximalFiniteMagnitude

!******************************************************************************

FUNCTION RawFP_IsNegative(RawVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine whether the input value is negative

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Flag = IAND(RawVal, SignMask) /= ZeroUInt

    RETURN
    
END FUNCTION RawFP_IsNegative

!******************************************************************************

FUNCTION RawFP_Negate(InRaw) RESULT(OutRaw)

!** PURPOSE OF THIS SUBROUTINE:
    ! To negate the input value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: InRaw
    tUIntType               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutRaw = IEOR(InRaw, SignMask)

    RETURN
    
END FUNCTION RawFP_Negate

!******************************************************************************

FUNCTION RawFP_NeighborLow(InRaw) RESULT(OutRaw)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the nearest floating point value that is smaller than the input value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: InRaw
    tUIntType               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutRaw = InRaw - OneUInt

    RETURN
    
END FUNCTION RawFP_NeighborLow

!******************************************************************************

FUNCTION RawFP_NeighborHigh(InRaw) RESULT(OutRaw)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the nearest floating point value that is greater than the input value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: InRaw
    tUIntType               :: OutRaw

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    OutRaw = InRaw + OneUInt
    
    RETURN
    
END FUNCTION RawFP_NeighborHigh

!******************************************************************************

FUNCTION RawFP_Construct(FpBin) RESULT(RawVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct a raw binary floating point number based on
    ! its three parts

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BinRep), INTENT(IN)    :: FpBin
    tUIntType                   :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! set sign bit
    IF (FpBin%Negative) THEN
        RawVal = SignMask
    ELSE
        RawVal = ZeroUInt
    END IF
    ! add exponent bits
    RawVal = IOR(RawVal, SHIFTL(ToUIntType(FpBin%Exponent), SignificandBits))
    ! add (both implicit and explicit) significand bits
    RawVal = IOR(RawVal, IAND(FpBin%Significand, SignificandMask))
    
    RETURN
    
END FUNCTION RawFP_Construct

!******************************************************************************

FUNCTION RawFP_Decompose(RawVal) RESULT(FpBin)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decompose a raw binary floating point number into
    ! its three parts

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    TYPE(BinRep)            :: FpBin

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! set sign
    FpBin%Negative    = IAND(RawVal, SignMask) /= ZeroUInt
    ! set significand
    FpBin%Significand = RawFP_Significand(RawVal)
    ! set exponent
    FpBin%Exponent    = RawFP_BiasedExponent(RawVal)
    
    RETURN
    
END FUNCTION RawFP_Decompose

!******************************************************************************

FUNCTION RawFP_FromFloat(RealVal) RESULT(RawVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct a raw binary floating point number based on
    ! the specified real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tFloat, INTENT(IN)  :: RealVal
    tUIntType           :: RawVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: Little_Endian = (TRANSFER([1_kInt8, 0_kInt8, 0_kInt8, 0_kInt8], &
                                                      0_kInt32) == 1_kInt32)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifndef tFloat_is_tQuad
    tUIntType   :: IntVal
    tFloat      :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)
#else    
    tUInt64     :: IntVal(2)
    tFloat      :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)
#endif

!** FLOW
    
#ifndef tFloat_is_tQuad
    FloatVal = RealVal
    RawVal   = IntVal
#else
    IF (Little_Endian) THEN
        ! little-endian order
        FloatVal = RealVal
        RawVal   = UInt128(IntVal(2), IntVal(1))    ! UInt128(HiVal, LowVal)
    ELSE
        ! big-endian order
        FloatVal = RealVal
        RawVal   = UInt128(IntVal(1), IntVal(2))
    END IF
#endif

    RETURN

END FUNCTION RawFP_FromFloat

!******************************************************************************

FUNCTION RawFP_ToFloat(RawVal) RESULT(RealVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a raw binary floating point number into
    ! its equivalent real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUIntType, INTENT(IN)   :: RawVal
    tFloat                  :: RealVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: Little_Endian = (TRANSFER([1_kInt8, 0_kInt8, 0_kInt8, 0_kInt8], &
                                                      0_kInt32) == 1_kInt32)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifndef tFloat_is_tQuad
    tUIntType   :: IntVal
    tFloat      :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)
#else    
    tUInt64     :: IntVal(2)
    tFloat      :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)
#endif

!** FLOW
    
#ifndef tFloat_is_tQuad
    IntVal  = RawVal
    RealVal = FloatVal
#else
    IF (Little_Endian) THEN
        ! little-endian order
        IntVal(2) = RawVal%High
        IntVal(1) = RawVal%Low
        RealVal   = FloatVal
    ELSE
        ! big-endian order
        IntVal(1) = RawVal%High
        IntVal(2) = RawVal%Low
        RealVal   = FloatVal
    END IF
#endif

    RETURN

END FUNCTION RawFP_ToFloat

!******************************************************************************

FUNCTION RawFP_SetZero(Negative) RESULT(RawVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set value to zero

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Negative
    tUIntType               :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (Negative) THEN
        RawVal = SignMask
    ELSE
        RawVal = ZeroUInt
    END IF

    RETURN

END FUNCTION RawFP_SetZero

!******************************************************************************

FUNCTION RawFP_SetInfinity(Negative) RESULT(RawVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set value to infinity

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Negative
    tUIntType               :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType       :: Exponent

!** FLOW
    
    ! set sign of infinity
    RawVal = RawFP_SetZero(Negative)
    ! set infinity biased exponent
    Exponent = IAND(SHIFTL(ToUIntType(MaxExponent), SignificandBits), ExponentMask)
    RawVal = IAND(RawVal, NOT(ExponentMask))
    RawVal = IOR(RawVal, Exponent)

    RETURN

END FUNCTION RawFP_SetInfinity

!******************************************************************************

FUNCTION RawFP_SetNaN(Quiet) RESULT(RawVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set value to NaN

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Quiet
    tUIntType               :: RawVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType   :: Mantissa

!** FLOW
    
    ! set infinity biased exponent
    RawVal = RawFP_SetInfinity(FalseVal)
    ! set NaN significand
    RawVal = IOR(RawVal, SHIFTL(OneUInt, SignificandBits - 2))
    IF (Quiet) THEN
        Mantissa = IAND(QuietNaNMask, SignificandMask)
        RawVal = IAND(RawVal, NOT(SignificandMask))
        RawVal = IOR(RawVal, Mantissa)
    END IF

    RETURN

END FUNCTION RawFP_SetNaN

!******************************************************************************

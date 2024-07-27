
SUBMODULE (ModBase_CharConv) SubBase_RealSPConv

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversions
!   between a single-precision real (floating-point) number and a string. <br>
!   See references and detailed explanations about usages and technical information
!   in the <a href="../module/subbase_realdpconv.html">SubBase_RealDPConv</a> submodule.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil
    USE ModBase_UIntUtil
    USE ModBase_UInt128
    USE ModBase_LargeTables
    USE, INTRINSIC :: IEEE_ARITHMETIC

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"
#define     tFloat_is_tSingle
! variable types
#define     tUInt128        TYPE(UInt128)
#define     tFloat          tSingle
#define     tUIntType       tUInt32
! common parameters
#define     ZeroUInt        0_kInt32
#define     OneUInt         1_kInt32
#define     ZeroFloat       0.0_kSingle
#define     OneFloat        1.0_kSingle
! type conversions
#define     ToI32(X)        ToInt32(X)
#define     ToUIntType(X)   ToInt32(X)
#define     ToFloat(X)      REAL(X, KIND=kSingle)

!** MODULE PARAMETERS:
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ parameters used to convert bit widths to whole decimal digits +++
    tSInt64, PARAMETER  :: LB2To10_M1 = 301029995664_kInt64     ! LogBaseTenOfTwoTimesTenToThe12th
    tSInt64, PARAMETER  :: LB2To10_M2 = 1000000000000_kInt64    ! TenToThe12th
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ Characteristics of IEEE-754 & related binary floating-point numbers +++
    ! kind
    tSInt32, PARAMETER  :: RealKind         = 4
    ! binary-precision number of bit
    tSInt32, PARAMETER  :: BinaryPrecision  = 24
    tSInt32, PARAMETER  :: TotalBits        = 32
    tSInt32, PARAMETER  :: SignBits         = TotalBits - 1                                             ! 31
    tSInt32, PARAMETER  :: SignificandBits  = BinaryPrecision - 1                                       ! 23
    tSInt32, PARAMETER  :: ExponentBits     = TotalBits - BinaryPrecision                               ! 8
    tSInt32, PARAMETER  :: MaxExponent      = SHIFTL(1, ExponentBits) - 1                               ! 255
    tSInt32, PARAMETER  :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1                           ! 127
    tSInt32, PARAMETER  :: DecimalPrecision = ToInt32((SignificandBits * LB2To10_M1) / LB2To10_M2)      ! 6
    tSInt32, PARAMETER  :: DecimalRange     = ToInt32(((ExponentBias - 1) * LB2To10_M1) / LB2To10_M2)   ! 37
    tSInt32, PARAMETER  :: MaxDecimalConversionDigits = 112
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ masking parameters +++
    tUInt32, PARAMETER  :: SigHidBitMask    = SHIFTL(1, SignificandBits)
    tUInt32, PARAMETER  :: SignificandMask  = SigHidBitMask - 1
    tUInt32, PARAMETER  :: SignMask         = SHIFTL(1, SignBits)
    tUInt32, PARAMETER  :: ExponentMask     = NOT(IOR(SignMask, SignificandMask))
    tUInt32, PARAMETER  :: ExpMantMask      = SignificandMask + ExponentMask        ! = NOT(SignMask)
    tUInt32, PARAMETER  :: QuietNaNMask     = SHIFTL(1, SignificandBits - 1)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Exceptional exponent value for NaN or Infinity
    tSInt32, PARAMETER  :: ExceptionalExponent = ToInt32(Z'7FFFFFFF')
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ maximum and minimum (positive) parameters +++
    tUInt32, PARAMETER  :: MinSubnormal = 1
    tUInt32, PARAMETER  :: MaxSubnormal = SHIFTL(1, SignificandBits) - 1
    tUInt32, PARAMETER  :: MinNormal    = SHIFTL(1, SignificandBits)
    tUInt32, PARAMETER  :: MaxNormal    = IOR(SHIFTL((MaxExponent - 1), SignificandBits), MaxSubnormal)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
    ! 150 is an arbitrary number of digits, but should be large enough for any practical number.
    ! Important note: a number of digits large enough to represent the smallest subnormal
    ! for single-precision number is about 166 (= 54 + 112).
    tUInt32, PARAMETER  :: MAX_NUM_DIGITS = 150
    ! The maximum amount we can shift is the number of bits used in the Accumulator,
    ! minus the number of bits needed to represent the base (in this case 4).
    tUInt32, PARAMETER  :: MAX_SHIFT_AMOUNT = 4
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------------------
    ! -----   parameters for BigUInt of FastFloat algorithm  -----
    ! -----------------------------------------------------------
    ! the number of bits of 'Digit' of BigUInt.
    tSInt32, PARAMETER  :: DigitBits = 64
    ! the total number of bits of a BigUInt that needs to be at least the number of bits
    ! required to store the largest BigUInt, which is Log2(10**(MaxDigits + MaxExp10)), or
    ! Log2(10**(112 + 54))`, or ~551 bits, so we round to 576.
    tSInt32, PARAMETER  :: BigUIntBits = 576
    ! the (fixed) capacity of a BigUInt
    tSInt32, PARAMETER  :: BigCapacity = BigUIntBits / DigitBits   ! = 9
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tUInt32, PARAMETER  :: DivBase      = 10
    tUInt32, PARAMETER  :: MaxDivbyBase = 429496729
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ number parameters +++
    tUInt32, PARAMETER  :: TwoUInt   = 2
    tUInt32, PARAMETER  :: ThreeUInt = 3
    tUInt32, PARAMETER  :: FourUInt  = 4
    tUInt32, PARAMETER  :: FiveUInt  = 5
    tUInt32, PARAMETER  :: TenUInt   = 10
    tUInt32, PARAMETER  :: FortyUInt = 40
    tUInt32, PARAMETER  :: HundredUInt     = 100
    tUInt32, PARAMETER  :: TenThousandUInt = 10000
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ----------------------------------------------------
    ! -----   Simple-Decimal-Algorithm' parameters   -----
    ! ----------------------------------------------------
    ! The nth item in Powers_Of_Two represents the greatest power of two less than
    ! 10^n. This tells us how much we can safely shift without overshooting.
    tUInt8,  PARAMETER  :: Powers_Of_Two(0:18) = [ &
            0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43, 46, 49, 53, 56, 59]
    tSInt32, PARAMETER  :: Num_Powers_Of_Two = SIZE(Powers_Of_Two)                                  ! = 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Eisel-Lemire-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32, PARAMETER  :: LowBits = TotalBits - SignificandBits - 3                                ! = 6
    ! The halfway constant is used to check if the bits that will be shifted away initially are all 1.
    tUInt32, PARAMETER  :: HalfWay = SHIFTL(1, LowBits) - 1                                         ! = 63
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Clinger-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32             :: Idx
    tFloat,  PARAMETER  :: Powers_Of_Ten(0:10)  = [(10.0E0**Idx, Idx = 0, 10)]
    tSInt32, PARAMETER  :: Num_Exact_Pow10 = 10
    tSInt32, PARAMETER  :: Num_Mantissa_Digits = 7
    tFloat,  PARAMETER  :: Max_Exact_Integer = 16777215.0_kSingle
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tSInt32, PARAMETER  :: Exponent_UppBound =  39      ! = 38 + 1
    tSInt32, PARAMETER  :: Exponent_LowBound = -54      ! = (-45) - 9
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Dragonbox-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! parameters for main routine
    tSInt32, PARAMETER  :: Kappa = 1
    tSInt32, PARAMETER  :: Big_Divisor = 10**(Kappa+1)                  ! 100
    tSInt32, PARAMETER  :: Small_Divisor = Big_Divisor / 10             ! 10
    tSInt32, PARAMETER  :: Half_Small_Divisor = Small_Divisor / 2       ! 5
    tSInt32, PARAMETER  :: Divisibility_Check_By_5_Threshold = 39
    tSInt32, PARAMETER  :: Case_Fc_Pm_Half_Lower_Threshold = -1
    ! parameters for short interval case
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Lower_Threshold = 2
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Upper_Threshold = 3
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Lower_Threshold = -35
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Upper_Threshold = -35
    ! parameters for Is_Divisible_By_Pow10 routine
    tSInt32, PARAMETER  :: Info_Shift_Amount = 16
    tSInt32, PARAMETER  :: OneShiftL = SHIFTL(1, Info_Shift_Amount)
    tSInt32, PARAMETER  :: Comparison_Mask = OneShiftL - 1
    tSInt32, PARAMETER  :: Magic_Number = OneShiftL/Small_Divisor + 1
    ! parameters for Divide_By_10_To_Kappa_Plus_1
    tUInt64, PARAMETER  :: DivM = 1374389535_kInt64
    tSInt32, PARAMETER  :: DivS = 37
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Ryu-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: BitsPerPow5 = 64
    tSInt32, PARAMETER  :: MaxExp_ModInv5 = 13
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Schubfach-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: Pow10_Min_Exact_Exp = 0
    tSInt32, PARAMETER  :: Pow10_Max_Exact_Exp = 27
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   FastFloat-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! Bias so we can get the real exponent with an invalid adjusted_mantissa
    tSInt32, PARAMETER  :: Invalid_AM_Bias = -ToInt32(Z'00008000')
    tSInt32, PARAMETER  :: Mantissa_Explicit_Bits     = SignificandBits
    tSInt32, PARAMETER  :: Minimum_Exponent           = -ExponentBias
    tSInt32, PARAMETER  :: Infinite_Power             = MaxExponent
    tSInt32, PARAMETER  :: Sign_Index                 = SignBits
    tSInt32, PARAMETER  :: MantTotalBits              = 64
    ! see section 6 in 'Number Parsing at a Gigabyte per Second' paper for
    ! how the following two numbers can be obtained
    tSInt32, PARAMETER  :: Max_Exponent_Round_To_Even = 10
    tSInt32, PARAMETER  :: Min_Exponent_Round_To_Even = -17
    tSInt32, PARAMETER  :: Largest_Power_of_Ten       = Exponent_UppBound - 1
    tSInt32, PARAMETER  :: Smallest_Power_of_Ten      = Exponent_LowBound + 1
    tSInt32, PARAMETER  :: Max_Digits                 = MaxDecimalConversionDigits + 2
    tUInt64, PARAMETER  :: OneMant                    = 1_kInt64
    tUInt64, PARAMETER  :: Max_Mantissa_Fast_Path     = SHIFTL(2_kInt64, Mantissa_Explicit_Bits)
    tUInt64, PARAMETER  :: Exponent_Mask              = ExponentMask
    tUInt64, PARAMETER  :: Mantissa_Mask              = SignificandMask
    tUInt64, PARAMETER  :: Hidden_Bit_Mask            = SigHidBitMask
    tUInt64, PARAMETER  :: MaxMant                    = MAX_U64
    tUInt64, PARAMETER  :: NotOneMant                 = NOT(1_kInt64)
    tUInt64, PARAMETER  :: NotSigHidBitMask           = NOT(SHIFTL(1_kInt64, SignificandBits))
    tUInt64, PARAMETER  :: Powers_of_Ten_Uint64(0:19) = &
        [0_kInt64, &
         10_kInt64, &
         100_kInt64, &
         1000_kInt64, &
         10000_kInt64, &
         100000_kInt64, &
         1000000_kInt64, &
         10000000_kInt64, &
         100000000_kInt64, &
         1000000000_kInt64, &
         10000000000_kInt64, &
         100000000000_kInt64, &
         1000000000000_kInt64, &
         10000000000000_kInt64, &
         100000000000000_kInt64, &
         1000000000000000_kInt64, &
         10000000000000000_kInt64, &
         100000000000000000_kInt64, &
         1000000000000000000_kInt64, &
         -8446744073709551616_kInt64]
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------
    ! -----   YY/Lemire-Algorithm's parameters   -----
    ! ------------------------------------------------
    tUInt64, PARAMETER  :: MaxU64         = MAX_U64
    tUInt32, PARAMETER  :: BitMask        = SHIFTL(1, LowBits) - 1          ! = Halfway
    tUInt32, PARAMETER  :: BitMaskMinus1  = BitMask - 1
    tUInt32, PARAMETER  :: AddRound       = SHIFTL(1, ExponentBits - 1)
    tUInt32, PARAMETER  :: MaxUInt        = ToInt32(Z'FFFFFFFF')
    tUInt32, PARAMETER  :: FpRawInf       = ToInt32(Z'7F800000')            ! = ExponentMask
    tSInt32, PARAMETER  :: MaxExpBin      = 128
    tSInt32, PARAMETER  :: MinExpBin      = -125
    tSInt32, PARAMETER  :: UIntSafeDigits = 9
    tSInt32, PARAMETER  :: MaxDecDigits   = MaxDecimalConversionDigits + 1
    tUInt32, PARAMETER  :: MaxMantissa    = SHIFTL(1_kInt64, BinaryPrecision)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!** DERIVED TYPE DEFINITIONS
    ! ----------------------------------------------------------------------------
    ! auxiliary string information
    ! ----------------------------------------------------------------------------
    TYPE StringAux
        tSInt32     :: Start        ! starting position that exclude the leading spaces
        tSInt32     :: SigCut       ! position after the string is truncated
                                    ! = zero if Truncated = False; non-zero if Truncated = True
        tSInt32     :: Indices(4)   ! positions of characters representing the 
                                    ! significand in the string
        tLogical    :: Truncated    ! flag indicating whether the computed decimal
                                    ! significand is based on a truncated string
    END TYPE StringAux
    ! ----------------------------------------------------------------------------
    ! -----   derived types for high-precision decimal conversion algorithm  -----
    ! ----------------------------------------------------------------------------
    TYPE HPDecimal
        tUInt32     :: NumDigits = 0
        tSInt32     :: DecimalPoint = 0
        tLogical    :: Truncated = FalseVal
        tUInt8      :: Digits(0:MAX_NUM_DIGITS-1)
    CONTAINS
        PROCEDURE   :: ShouldRoundUp        => HPDec_Should_Round_Up
        PROCEDURE   :: GetNumNewDigits      => HPDec_Get_Num_New_Digits
        PROCEDURE   :: TrimTrailingZeroes   => HPDec_Trim_Trailing_Zeroes
        PROCEDURE   :: RightShift           => HPDec_Right_Shift
        PROCEDURE   :: LeftShift            => HPDec_Left_Shift
        PROCEDURE   :: Construct            => HPDec_Construct
        PROCEDURE   :: Shift                => HPDec_Shift
        PROCEDURE   :: RoundToUIntType      => HPDec_Round_To_UInt
    END TYPE HPDecimal
    ! ----------------------------------------------------------------------------
    ! -----   derived types for FastFloat algorithm                          -----
    ! ----------------------------------------------------------------------------
    ! a multi-precision (fixed capacity) unsigned integer where its representation are:
    ! - Base is 2**64.
    ! - Magnitude as array in little endian order.
    ! - The 'Length' first 'Digit' count as the number.
    ! ----------------------------------------------------------------------------
    TYPE BigUInt
        tUInt64     :: Digit(0:BigCapacity-1)
        tSInt32     :: Length = 0               ! number of digit currently stored
    CONTAINS
        PROCEDURE   :: IsEmpty      => BigUInt_IsEmpty
        PROCEDURE   :: IsNonZero    => BigUInt_IsNonZero
        PROCEDURE   :: Push         => BigUInt_Push
        PROCEDURE   :: Extend       => BigUInt_Extend
        PROCEDURE   :: Normalize    => BigUInt_Normalize
        PROCEDURE   :: FromU64      => BigUInt_From_U64
        PROCEDURE   :: Hi64         => BigUInt_Get_Hi64
        PROCEDURE   :: Compare      => BigUInt_Compare
        PROCEDURE   :: ShiftL       => BigUInt_ShiftL
        PROCEDURE   :: LeadZ        => BigUInt_LeadZ
        PROCEDURE   :: BitLen       => BigUInt_BitLen
        PROCEDURE   :: SmallMul     => BigUInt_SmallMul
        PROCEDURE   :: LongMul      => BigUInt_LongMul
        PROCEDURE   :: Add          => BigUInt_Add
        PROCEDURE   :: Pow2         => BigUInt_Pow2
        PROCEDURE   :: Pow5         => BigUInt_Pow5
        PROCEDURE   :: Pow10        => BigUInt_Pow10
    END TYPE BigUInt
    ! parsed number information
    TYPE Parsed_Number_Info
        tSInt32     :: Exp              ! base-10 exponent
        tUInt32     :: Sig              ! base-10 significand
        tSInt32     :: IntegralStart    ! starting index of integral part of the significand
        tSInt32     :: IntegralEnd      ! ending index of integral part of the significand
        tSInt32     :: FractionStart    ! starting index of fractional part of the significand
        tSInt32     :: FractionEnd      ! ending index of fractional part of the significand
    END TYPE
    ! ----------------------------------------------------------------------------
    ! binary floating-point representation in base 2
    ! --> ((-1)**S) * M * (2**E)
    ! ----------------------------------------------------------------------------
    TYPE BinRep
        tUInt32     :: Significand  ! significand/mantissa (M)
        tUInt32     :: Exponent     ! exponent (E); negative value is invalid
        tLogical    :: Negative     ! negative sign flag; true if the value is negative
    END TYPE BinRep
    ! ----------------------------------------------------------------------------

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    ABSTRACT INTERFACE
        SUBROUTINE CB_Round(E, M, Min)
            IMPORT
            tSInt32, INTENT(INOUT)  :: E
            tUInt64, INTENT(INOUT)  :: M
            tSInt32, INTENT(IN)     :: Min
        END SUBROUTINE
        FUNCTION CB_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
            IMPORT
            tLogical, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
            tLogical                :: Flag
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           COMMON AND GENERIC ROUTINES
!
!------------------------------------------------------------------------------

! include common auxiliary routines
#include    "Includes/Common - Auxiliary.f90"

!------------------------------------------------------------------------------
!
!           (RAW) FLOATING-POINT BINARY REPRESENTATION ROUTINES
!
!------------------------------------------------------------------------------

! include utility routines for binary floating point number
#include    "Includes/Generic - RawBinary FP Number.f90"

!------------------------------------------------------------------------------
!
!               HIGH-PRECISION DECIMAL (HPDECIMAL) ROUTINES
!
!------------------------------------------------------------------------------

! include type-bound routines for HPDecimal type
#include    "Includes/Generic - HPDecimal.f90"

!------------------------------------------------------------------------------
!
!            MULTI-PRECISION UNSIGNED INTEGER (BIGUINT) ROUTINES
!
!------------------------------------------------------------------------------

! include type-bound routines for BigUInt type and related routines
#include    "Includes/Generic - BigUInt.f90"

!------------------------------------------------------------------------------
!
!            PARSING FLOATING-POINT-NUMBER STRING ROUTINES
!
!------------------------------------------------------------------------------

! include routines for parsing floating-poing-number string
#include    "Includes/Generic - Parse FP String.f90"

!------------------------------------------------------------------------------
!
!                       BINARY-TO-DECIMAL CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

! include routines for binary-to-decimal algorithms
#include    "Includes/Generic - Binary To Decimal.f90"

!------------------------------------------------------------------------------
!
!                       DECIMAL-TO-BINARY CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

! include routines for decimal-to-binary algorithms
#include    "Includes/Generic - Decimal To Binary.f90"

!------------------------------------------------------------------------------
!
!                           REAL32 AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION DivByPow10(X, P) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Y = X .UDIV. (10**P)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: X    ! X <= 10**10
    tSInt32, INTENT(IN) :: P    ! 1 <= P <= 8
    tUInt32             :: Y

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Parameters for division by power of 10 applicable for N <= 10 digits
    ! (i.e. used for division of the 'Significand')
    tUInt64, PARAMETER  :: MagicM(1:8) = [                        &
        ToInt64(Z'0000000333333334'), ToInt64(Z'000000028F5C28F6'), &
        ToInt64(Z'00000004189374BD'), ToInt64(Z'0000000346DC5D64'), &
        ToInt64(Z'000000029F16B11D'), ToInt64(Z'0000000431BDE82E'), &
        ToInt64(Z'000000035AFE5358'), ToInt64(Z'00000002AF31DC47')]
    tSInt32, PARAMETER  :: MagicS(1:8) = [37, 40, 44, 47, 50, 54, 57, 60]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Multiplier
    tSInt32     :: Shift

!** FLOW

    Multiplier = MagicM(P)
    Shift      = MagicS(P)
    Y = ToInt32(SHIFTR(ToUnsignedLong(X)*Multiplier, Shift))

    RETURN
            
END FUNCTION DivByPow10

!******************************************************************************

FUNCTION WRITE_RealSP(Fp, Ep, cStr, IsScientific) RESULT(sLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To format the decimal F*10**E

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,            INTENT(IN)      :: Fp           ! significand
    tSInt32,            INTENT(IN)      :: Ep           ! exponent
    tCharStar,          INTENT(INOUT)   :: cStr         ! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific ! format flag
                                                        ! true  if to write the given number in scientific format
                                                        ! false if to write the given number in general format
                                                        ! default is false
    tSInt32                             :: sLen         ! length of string written

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32, PARAMETER  :: H = 9
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt32, PARAMETER  :: S98   = 57
    tSInt64, PARAMETER  :: M98   = 1441151881_kInt64
    tSInt32, PARAMETER  :: S178  = 20                         ! = 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! The first powers of 10. The last entry must be 10^H.
    tSInt32             :: I
    tSInt64, PARAMETER  :: Pow10(0:H) = [(10**I, I = 0, H)]
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: F
    tSInt32     :: E, HF, LF
    tLogical    :: IsGeneral    ! true if to write the given number in general format 

!** FLOW
    
    ! check for special cases
    IF (Ep == ExceptionalExponent) THEN
        ! either NaN or Infinity
        IF (Fp /= ZeroUInt) THEN
            cStr(1:3) = 'NaN'
            sLen = 3
        ELSE
            cStr(1:8) = 'Infinity'
            sLen = 8
        END IF
        RETURN
    END IF
    
    IF (Fp == ZeroUInt) THEN
        ! zero
        cStr(1:3) = '0.0'
        sLen = 3
        RETURN
    END IF

    ! For details not discussed here see section 10 of [3].
    ! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(32 - LEADZ(Fp))
    IF (Fp >= Pow10(sLen)) sLen = sLen + 1
    
    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*Pow10(H - sLen)
    E = Ep + sLen
    
    ! ToChars perform digits extraction using integers,
    ! provided that the arguments are limited to 8 digits.
    ! Therefore, split the H = 9 digits of F into:
    !     HF = the most significant digit of F
    !     LF = the last 8, least significant digits of F
    !
    ! For N = 9, M = 8 the table in section 10 of [2] shows
    !     Floor(F/10**8) = Floor((1,441,151,881*F/2**57)
    !
    HF = ToInt32(SHIFTR(F*M98, S98))
    LF = ToInt32(F - DivE8*HF)
    
    ! set format flag
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    ! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
            ! plain format without leading zeroes
            sLen = ToChar_Plain_Without_LZ(HF, LF, E, cStr)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            sLen = ToChar_Plain_With_LZ(HF, LF, E, cStr)
        ELSE
            ! scientific notation
            sLen = ToChar_Scientific(HF, LF, E, cStr)
        END IF
    ELSE
        ! scientific notation
        sLen = ToChar_Scientific(HF, LF, E, cStr)
    END IF

    RETURN
    
    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        cStr(1:1) = Char1Digit(H)
        Pos = 2
        ! Algorithm 1 in [7] needs computation of floor((a + 1) 2^n / b^k) - 1
        ! with a < 10^8, b = 10, k = 8, n = 28.
        ! Noting that (a + 1) 2^n <= 10^8 2^28 < 10^17
        ! For n = 17, m = 8 the table in section 10 of [3] leads to:
        Y = ToInt32(SHIFTR(UMul128_Upper64(SHIFTL(ToInt64(L+1), 28), M178), S178)) - 1
        I = 1
        DO WHILE (I < E)
            T = 10*Y
            ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append period
        cStr(Pos:Pos) = '.'
        Pos = Pos + 1
        DO WHILE (I <= 8)
            T = 10*Y
            ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        Pos = Pos - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

    !******************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: Y, T, I, Pos

    !** FLOW
    
        ! fill the first 4 characters
        cStr(1:4) = '0.00'
        ! compute Pos
        Pos = 3 - E 
        ! append H
        cStr(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
        ! append L
        Pos = Pos + Write_U32_8_Digits(L, cStr(Pos:)) - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

    !******************************************************************************

    FUNCTION ToChar_Scientific(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        ! append H
        cStr(1:1) = Char1Digit(H)
        ! append period
        cStr(2:2) = '.'
        Pos = 3
        ! append L
        Pos = Pos + Write_U32_8_Digits(L, cStr(Pos:)) - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! append exponent
        Pos = Pos + 1
        cStr(Pos:Pos) = 'E'
        sLen = Pos + Write_I32_Exponent(E-1, cStr(Pos+1:))
    
        RETURN
    
    END FUNCTION ToChar_Scientific

    !******************************************************************************

    FUNCTION Write_U32_8_Digits(Number, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! shift and multiplier parameters (i.e. magic number) for integer division
        tSInt32, PARAMETER  :: Shf78 = 40
        tSInt64, PARAMETER  :: Mul78 = 109951163_kInt64
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW
    
        ! compute NxtNum = PosNum/10000
        NxtNum = ToInt32(SHIFTR(ToInt64(Number)*Mul78, Shf78))
    
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor
    
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
    
        ! convert the rest (NxtNum)
        cStr(1:4) = Char4Digits(NxtNum)
        
        sLen = 8
    
        RETURN

    END FUNCTION Write_U32_8_Digits

    !**************************************************************************

    FUNCTION Write_I32_Exponent(Exp, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write a signed integer in the range -45 to 38

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Exp      ! exponent number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: PosExp

    !** FLOW
        
        IF (Exp < 0) THEN
            cStr(1:1) = '-'
        ELSE
            cStr(1:1) = '+'
        END IF
        PosExp = ABS(Exp)
        IF (PosExp < 10) THEN
            ! 1 digit
            cStr(2:2) = Char1Digit(PosExp)
            sLen = 2
        ELSE
            ! 2 digits
            cStr(2:3) = Char2Digits(PosExp)
            sLen = 3
        END IF
    
        RETURN

    END FUNCTION Write_I32_Exponent

    !**************************************************************************

END FUNCTION WRITE_RealSP

!------------------------------------------------------------------------------
!
!                       REAL32-TO-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION RealSP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a single-precision floating-point value to a character (decimal) string
    ! using the DragonBox algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawBin       ! raw IEEE binary floating point representation
    tUInt32         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: wPos
    tCharLen(48)    :: wStr         ! working string
    tSInt32         :: wLen         ! length of string

!** FLOW
    
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number
    
    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToI32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF
    
    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        CALL Bin2Dec_DragonBox(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + WRITE_RealSP(SigDec, ExpDec, wStr(wPos:), IsScientific)
    
    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealSP_ToString_DragonBox

!******************************************************************************

MODULE FUNCTION RealSP_ToString_Ryu(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a single-precision floating-point value to a character (decimal) string
    ! using the Ryu algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawBin       ! raw IEEE binary floating point representation
    tUInt32         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: wPos
    tCharLen(48)    :: wStr         ! working string
    tSInt32         :: wLen         ! length of string

!** FLOW
    
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number
    
    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToI32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF
    
    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        CALL Bin2Dec_Ryu(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + WRITE_RealSP(SigDec, ExpDec, wStr(wPos:), IsScientific)
    
    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealSP_ToString_Ryu

!******************************************************************************

MODULE FUNCTION RealSP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a single-precision floating-point value to a character (decimal) string
    ! using the Schubfach algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawBin       ! raw IEEE binary floating point representation
    tUInt32         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: wPos
    tCharLen(48)    :: wStr         ! working string
    tSInt32         :: wLen         ! length of string

!** FLOW
    
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number
    
    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToI32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF
    
    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        CALL Bin2Dec_Schubfach(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        wStr(1:1) = '-'
        wPos = 2
    ELSE
        wPos = 1
    END IF
    wLen = (wPos - 1) + WRITE_RealSP(SigDec, ExpDec, wStr(wPos:), IsScientific)
    
    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealSP_ToString_Schubfach

!------------------------------------------------------------------------------
!
!                       REAL64-FROM-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION RealSP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a single-precision floating-point value
    ! using the FastFloat algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tSingle                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigBin64     ! (unbiased) significand in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt32         :: RawVal
    tLogical        :: Valid
    tLogical        :: SlowPath
    tSInt32         :: ParseFormat
    tFloat          :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

    ! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++++ perform decimal to binary conversion +++++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! set flag
        SlowPath = TrueVal
    
        ! If the exponent is too large and can't be represented in this size of
        ! float, return inf. These bounds are relatively loose, but are mostly
        ! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
            ! infinity
            SigBin = ZeroUInt
            ExpBin = MaxExponent
            SlowPath = FalseVal
        ! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
            ! zero
            SigBin = ZeroUInt
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
                ! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF
    
        ! perform decimal to binary conversion using FastFloat algorithm if SlowPath is true
        ! IF (SlowPath) CALL Dec2Bin_FastFloat(SigDec, ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin, ExpBin)
        IF (SlowPath) THEN
            CALL Dec2Bin_FastFloat(ToInt64(SigDec), ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin64, ExpBin)
            SigBin = ToInt32(SigBin64)
        END IF

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++ convert binary representation into real number +++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! construct raw binary representation of floating point number
        ! set sign bit
        IF (Negative) THEN
            RawVal = SignMask
        ELSE
            RawVal = ZeroUInt
        END IF
        ! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToUIntType(ExpBin), SignificandBits))
        ! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        ! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
        ! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF
    
    RETURN

END FUNCTION RealSP_FromString_FastFloat

!******************************************************************************

MODULE FUNCTION RealSP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a single-precision floating-point value
    ! using the LibC algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tSingle                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt32         :: RawVal
    tLogical        :: Valid
    tLogical        :: SlowPath
    tSInt32         :: ParseFormat
    tFloat          :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

    ! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++++ perform decimal to binary conversion +++++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! set flag
        SlowPath = TrueVal
    
        ! If the exponent is too large and can't be represented in this size of
        ! float, return inf. These bounds are relatively loose, but are mostly
        ! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
            ! infinity
            SigBin = ZeroUInt
            ExpBin = MaxExponent
            SlowPath = FalseVal
        ! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
            ! zero
            SigBin = ZeroUInt
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
                ! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF
    
        ! perform decimal to binary conversion using LibC algorithm if SlowPath is true
        IF (SlowPath) CALL Dec2Bin_LibC(SigDec, ExpDec, cStr, Aux%Start, Aux%Truncated, SigBin, ExpBin)

        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++ convert binary representation into real number +++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! construct raw binary representation of floating point number
        ! set sign bit
        IF (Negative) THEN
            RawVal = SignMask
        ELSE
            RawVal = ZeroUInt
        END IF
        ! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToUIntType(ExpBin), SignificandBits))
        ! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        ! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
        ! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF
    
    RETURN

END FUNCTION RealSP_FromString_LibC

!******************************************************************************

MODULE FUNCTION RealSP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a single-precision floating-point value
    ! using the YY algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tSingle                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt32         :: RawVal
    tLogical        :: Valid
    tLogical        :: SlowPath
    tSInt32         :: ParseFormat
    tFloat          :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

    ! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++++ perform decimal to binary conversion +++++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! set flag
        SlowPath = TrueVal
    
        ! If the exponent is too large and can't be represented in this size of
        ! float, return inf. These bounds are relatively loose, but are mostly
        ! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
            ! infinity
            SigBin = ZeroUInt
            ExpBin = MaxExponent
            SlowPath = FalseVal
        ! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
            ! zero
            SigBin = ZeroUInt
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
                ! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF
    
        IF (SlowPath) THEN
            ! +++ perform decimal to binary conversion using YY's algorithm +++
            RawVal = Dec2Bin_YY(SigDec, ExpDec, Negative, cStr, Aux)
        ELSE
            ! +++ construct raw binary representation of floating point number +++
            ! set sign bit
            IF (Negative) THEN
                RawVal = SignMask
            ELSE
                RawVal = ZeroUInt
            END IF
            ! add exponent bits
            RawVal = IOR(RawVal, SHIFTL(ToUIntType(ExpBin), SignificandBits))
            ! add (both implicit and explicit) significand bits
            RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        END IF

        ! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
        ! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF
    
    RETURN

END FUNCTION RealSP_FromString_YY

!******************************************************************************

MODULE FUNCTION RealSP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a single-precision floating-point value
    ! using the Lemire algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tSingle                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt32         :: RawVal
    tLogical        :: Valid
    tLogical        :: SlowPath
    tSInt32         :: ParseFormat
    tFloat          :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! check and set optional input (parsing format)
    ParseFormat = FortNum
    IF (PRESENT(ParseOpt)) THEN
        IF ((ParseOpt >= 1).AND.(ParseOpt <= 3)) ParseFormat = ParseOpt
    END IF

    ! parse floating-point-number string
    SELECT CASE (ParseFormat)
    CASE (FortNum)
        Valid = Parse_Fortran_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (FPlusNum)
        Valid = Parse_FPlus_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    CASE (JsonNum)
        Valid = Parse_JSON_String(cStr, SigDec, ExpDec, Negative, Aux, ErrMsg)
    END SELECT
    IF (PRESENT(ErrFlag)) ErrFlag = .NOT.Valid

    IF (Valid) THEN
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! +++++ perform decimal to binary conversion +++++
        ! ++++++++++++++++++++++++++++++++++++++++++++++++
        ! set flag
        SlowPath = TrueVal
    
        ! If the exponent is too large and can't be represented in this size of
        ! float, return inf. These bounds are relatively loose, but are mostly
        ! serving as a first pass. Some close numbers getting through is okay.
        IF (ExpDec > Exponent_UppBound) THEN
            ! infinity
            SigBin = ZeroUInt
            ExpBin = MaxExponent
            SlowPath = FalseVal
        ! If the exponent is too small even for a subnormal, return 0.
        ELSEIF (ExpDec < Exponent_LowBound) THEN
            ! zero
            SigBin = ZeroUInt
            ExpBin = 0
            SlowPath = FalseVal
        ELSEIF (.NOT.Aux%Truncated) THEN
            IF (Dec2Bin_Clinger(SigDec, ExpDec, SigBin, ExpBin)) THEN
                ! clinger's fast path is valid
                SlowPath = FalseVal
            END IF
        END IF
    
        IF (SlowPath) THEN
            ! +++ perform decimal to binary conversion using Lemire's algorithm +++
            RawVal = Dec2Bin_Lemire(SigDec, ExpDec, Negative, cStr, Aux)
        ELSE
            ! +++ construct raw binary representation of floating point number +++
            ! set sign bit
            IF (Negative) THEN
                RawVal = SignMask
            ELSE
                RawVal = ZeroUInt
            END IF
            ! add exponent bits
            RawVal = IOR(RawVal, SHIFTL(ToUIntType(ExpBin), SignificandBits))
            ! add (both implicit and explicit) significand bits
            RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
        END IF

        ! convert raw binary representation to floating point number (little-endian order)
        Number = FloatVal
    ELSE
        ! handle special cases (infinity or NaN)
        Number = Handle_Invalid_String(cStr, Aux%Start, Negative)
    END IF
    
    RETURN

END FUNCTION RealSP_FromString_Lemire

!******************************************************************************

END SUBMODULE SubBase_RealSPConv

!******************************************************************************

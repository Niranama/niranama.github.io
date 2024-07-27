
SUBMODULE (ModBase_CharConv) SubBase_RealDPConv

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversions
!   between a double-precision real (floating-point) number and a string. <br>
!   <br>
!   **TECHNICAL AND IMPLEMENTATION NOTES**: <br>
!   *On the output to string*: <br>
!   1) Three routines are available to convert a real (floating-point) number into a string. <br>
!    - "RealToString_DragonBox" is based on the Dragonbox binary-to-decimal conversion algorithm [1]
!      and the reference implementation [11, 14, 20]. <br>
!    - "RealToString_Ryu" is based on the Ryu binary-to-decimal conversion algorithm [2]
!      and the reference implementation [12, 14]. <br>
!    - "RealToString_Schubfach" is based on the Schubfach binary-to-decimal conversion algorithm [3]
!      and the reference implementation [13, 14, 15]. <br>
!   2) All three binary-to-decimal conversion algorithms employed here produce the so-called shortest
!      output representation that provide an error-free write-read cycle.  This means that any correct
!      parsers (e.g. RealFromString routines) will read in the output string and return the original
!      real (floating-point) number. <br>
!   3) Although the DragonBox reference implementation provides several modes of rounding, only the
!      round-to-nearest mode is implemented here (the other two algorithms also use this mode). <br>
!   4) Although the Ryu reference implementation provides several conversion output formats (Shortest,
!      Scientific, Fixed), only the shortest representation (as mentioned above) is implemented.
!      Therefore, all three routines will produces the output string in a format similar to "G0" format
!      specification in Fortran. <br>
!   5) Actually, the RealToString routines have an optional "format" argument that we can use to specify
!      whether to output the string in "General (G)" or "Scientific (ES)" format.  However, because they
!      always produce the shortest output, no input argument to the routines is provided to specify
!      the desired number of significant digits as typically done in Fortran format specifications. <br>
!   *On the input from string*: <br>
!   1) Four routines are available to convert a string into a real (floating-point) number.  All four
!      routines utilize the so-call Clinger's fast-path algorithm [4].  Three of them (except "YY") employ
!      the so-call Eisel-Lemire decimal-to-binary conversion algorithm [5, 9] but are based on different
!      reference implementation.  When the Eisel-Lemire (or YY's fast-path) algorithm is NOT valid, three
!      of the routines (except "LibC") use multi-precision (unsigned) integer arithmetic (i.e. BigUInt)
!      whereas "LibC" employs the so-call Simple Decimal Conversion algorithm [10]. <br>
!      - "RealFromString_FastFloat" is based on the reference implementation [16]. <br>
!      - "RealFromString_LibC" is based on the reference implementation [18]. <br>
!      - "RealFromString_YY" is based on the reference implementation [15, 19]. <br>
!      - "RealFromString_Lemire" is based on the reference implementation [17, 19]. <br>
!   2) The RealFromString routines have an optional "parsing" argument that we can use to specify how
!      the routines interpret the input string. <br>
!   3) The "Parse_Fortran_String" routine is called when the optional "parsing" argument is not specified
!      (i.e. the default option) or "FortNum (or 1)" value is supplied as the parsing argument.  The routine
!      will interpret the input string as a valid Fortran real (floating point) number if it has one of
!      the two following forms: <br>
!      <1> A number without exponent part -> [S]N[N...], and <br>
!      <2> A number with exponent part    -> [S]N[N...]E[S]N[N...] <br>
!          where <br>
!          - [ ] indicates an optional field. <br>
!          - S is a sign indicator (required if negative '-', optional if positive '+'). <br>
!          - N is a decimal digit (0 through 9). A decimal point (a period) may appear anywhere
!               after the sign (but before the exponent). <br>
!          - E is an exponent indicator (either 'e' or 'E'). <br>
!      The valid number is similar to "Real" Fortran constant (literal) with some small differences. <br>
!       - A whole number without a decimal point (i.e. "Integer" constant) is considered valid. <br>
!       - The optional kind parameter (e.g. 1.23_DP) is not allowed here. <br>
!      Leading and/or trailing space(s) are allowed.  For example, "  1.23" and "1.23   " are considered
!      valid.  However, no space is allowed inside the supposedly valid number.  For instance, "1 .2 3"
!      is considered NOT valid. Therefore, this routine is not totally compatible with Fortran READ statement
!      where spaces inside the valid number are allowed. However, this can easily be done by adding an
!      optional 'Inside Space' flag that provide an interpretation of the spaces as 'zero' or 'ignored'.
!      Then, the input will be pre-processed according to the flag.  Nonetheless, this routine neglects
!      this optional input because it will make the routine much less efficient due to the fact that
!      we will need to scan the whole string twice and we will also need to copy the input string into
!      a buffer string and working with the buffer instead of directly handling the input string. <br>
!   4) The "Parse_FPlus_String" routine is called when "FPlusNum (or 2)" value is supplied as the parsing
!      argument.  The routine will parse a valid Fortran real (floating point) number with more relaxed
!      rules than those used in "Parse_Fortran_Number" routine. The relaxed rules consider the following
!      numbers as valid: <br>
!      - A number expressed in the scientific format can use 'd', 'D', 'q' and 'Q'
!      in place of 'e' or 'E'. <br>
!      - A number with '+' or '-' after digits (e.g. 1.23-20 or 123+50) is considered to
!      be a valid number expressed in the scientific format where an exponent indicator
!      is omitted. <br>
!      - Digits before any invalid character encountered are treated as a valid number
!      and any characters after the first encounter (including the first invalid one)
!      are neglected.  Therefore, for example, a '12.56ax-300' string is considered to
!      be a valid number with a value of 12.56. <br>
!   5) The "Parse_JSON_String" routine is called when "JsonNum (or 3)" value is supplied
!      as the parsing argument.  The routine will parse a valid JSON floating point number
!      where its differences from Fortran number are as follows: <br>
!      - Leading and trailing spaces are not allowed. <br>
!      - A plus sign as the first character is not allowed. <br>
!      - Leading zero(s) is not allowed (if 0 is the first character, the second one
!      must either be a period or an exponent indicator.) <br>
!      - A period must be followed by at least one digit. <br>
!   <br>
!   **USAGE**: <br>
!   *On the output to string*: <br>
!   => cStr = RealXP_ToString_DragonBox(Number, IsScientific) <br>
!   => cStr = RealXP_ToString_Ryu(Number, IsScientific) <br>
!   => cStr = RealXP_ToString_Schubfach(Number, IsScientific) <br>
!      where <br>
!      "cStr" is an *allocatable* character string representing the output string.  <br>
!      "Number" is a real number representing the floating point value.  <br>
!      "IsScientific" is a logical flag (optional argument) indicating whether
!         the output string is in *General* or *Scientific* format.
!         If present and true, the output string is in *Scientific* format.
!         Otherwise, the output string is in *General* format. <br>
!   *On the input from string*: <br>
!   => Number = RealXP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) <br>
!   => Number = RealXP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) <br>
!   => Number = RealXP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) <br>
!   => Number = RealXP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) <br>
!      where <br>
!      "Number" is a real number representing the floating point value if the input
!         string is valid. <br>
!      "cStr" is a character string representing the floating-point number string. <br>
!      "ParseOpt" is an integer input flag (optional) indicating how to interpret
!         the input string.  The valid value is FortNum (1), FPlusNum (2) or JsonNum (3).
!         If not specified or invalid, the routines will interpret the input string
!         as a Fortran number. <br>
!      "ErrFlag" is a logical output flag (optional) indicating whether there is an
!         error in parsing the input string.  True if the string represents an invalid
!         number.  False, otherwise. <br>
!      "ErrMsg" is an allocatable output character string (optional) that returns a
!         message describing the result of string parsing. <br>
!   *NOTE*: "XP" in the routine names shown above indicates the precision of the
!       real number.  The actual name will either be "RealSP_...", "RealDP_..." or
!       "RealQP_..." for single-precision, double-precision and quadruple-precision
!       number, respectively. <br>
!   <br>
!  **REFERENCE TECHNICAL ARTICLES**: <br>
!   [1] <a href="https://github.com/jk-jeon/dragonbox/blob/master/other_files/Dragonbox.pdf">
!       Junekey Jeon.  "Dragonbox: A New Floating-Point Binary-to-Decimal Conversion Algorithm".</a> <br>
!   [2] <a href="https://dl.acm.org/doi/10.1145/3192366.3192369">
!       Ulf Adams.  "Ryu: Fast Float-to-String Conversion".</a> <br>
!   [3] <a href="https://drive.google.com/open?id=1luHhyQF9zKlM8yJ1nebU0OgVYhfC6CBN">
!       Raffaello Giulietti.  "The Schubfach way to render doubles".</a> <br>
!   [4] <a href="https://doi.org/10.1145/989393.989430">
!       Clinger WD. How to Read Floating Point Numbers Accurately. SIGPLAN Not 2004 Apr;
!       39(4):360-371.</a> <br>
!   [5] <a href="https://arxiv.org/abs/2101.11408">Daniel Lemire.  "Number Parsing at a
!       Gigabyte per Second", Software: Practice and Experience 51 (8), 2021.</a> <br>
!   [6] <a href="https://arxiv.org/abs/2212.06644">Noble Mushtak and Daniel Lemire.
!       "Fast  Number Parsing Without Fallback", Software: Practice and Experience
!       53 (7), 2023.</a> <br>
!   [7] <a href="https://hal.inria.fr/hal-00864293v1/document">Bouvier & Zimmermann.
!       "Division-Free Binary-to-Decimal Conversion".</a> <br>
!   [8] Hacker's Delight, 2nd Edition.
!   [9] <a href="https://nigeltao.github.io/blog/2020/eisel-lemire.html">Nigel Tao.
!       "The Eisel-Lemire ParseNumberF64 Algorithm".</a> <br>
!   [10] <a href="https://nigeltao.github.io/blog/2020/parse-number-f64-simple.html">
!       Nigel Tao.  "ParseNumberF64 by Simple Decimal Conversion".</a> <br>
!   <br>
!   **REFERENCE CODE IMPLEMENTATION**: <br>
!   [11] <a href="https://github.com/jk-jeon/dragonbox">DragonBox: C++ reference
!       implementation.</a> <br>
!   [12] <a href="https://github.com/ulfjack/ryu">Ryu: C reference implementation.</a> <br>
!   [13] <a href="https://github.com/c4f7fcce9cb06515/Schubfach">Schubfach: Java
!       reference implementation.</a> <br>
!   [14] <a href="https://github.com/abolz/Drachennest">Drachennest: Different
!       algorithms for converting binary to decimal floating-point numbers.</a> <br>
!   [15] <a href="https://github.com/ibireme/c_numconv_benchmark">Number Conversion
!       Benchmark in C.</a> <br>
!   [16] <a href="https://github.com/fastfloat/fast_float">Fast_Float Number
!       Parsing Library.</a> <br>
!   [17] <a href="https://github.com/lemire/fast_double_parser">Fast_Double_Parser.</a> <br>
!   [18] <a href="https://github.com/llvm/llvm-project/tree/main/libc/src/__support">
!       The LLVM Project (LibC).</a> <br>
!   [19] <a href="https://github.com/google/double-conversion">Double Conversion:
!       Efficient binary-decimal and decimal-binary conversion routines for IEEE doubles.</a> <br>
!   [20] <a href="https://github.com/fmtlib/fmt">fmt: A modern formatting library.</a> <br>

!** USE STATEMENTS:
    USE ModBase_SIntUtil
    USE ModBase_UIntUtil
    USE ModBase_UInt128
    USE ModBase_LargeTables
    USE, INTRINSIC :: IEEE_ARITHMETIC

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"
#define     tFloat_is_tDouble
! variable types
#define     tUInt128        TYPE(UInt128)
#define     tFloat          tDouble
#define     tUIntType       tUInt64
! common parameters
#define     ZeroUInt        0_kInt64
#define     OneUInt         1_kInt64
#define     ZeroFloat       0.0_kDouble
#define     OneFloat        1.0_kDouble
! type conversions
#define     ToI32(X)        ToInt32(X)
#define     ToUIntType(X)   ToUnsignedLong(X)
#define     ToFloat(X)      REAL(X, KIND=kDouble)

!** MODULE PARAMETERS:
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ parameters used to convert bit widths to whole decimal digits +++
    tSInt64, PARAMETER  :: LB2To10_M1 = 301029995664_kInt64     ! LogBaseTenOfTwoTimesTenToThe12th
    tSInt64, PARAMETER  :: LB2To10_M2 = 1000000000000_kInt64    ! TenToThe12th
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ Characteristics of IEEE-754 & related binary floating-point numbers +++
    tSInt32, PARAMETER  :: RealKind         = 8
    tSInt32, PARAMETER  :: BinaryPrecision  = 53
    tSInt32, PARAMETER  :: TotalBits        = 64
    tSInt32, PARAMETER  :: SignBits         = TotalBits - 1                                             ! 63
    tSInt32, PARAMETER  :: SignificandBits  = BinaryPrecision - 1                                       ! 52
    tSInt32, PARAMETER  :: ExponentBits     = TotalBits - BinaryPrecision                               ! 11
    tSInt32, PARAMETER  :: MaxExponent      = SHIFTL(1, ExponentBits) - 1                               ! 2047
    tSInt32, PARAMETER  :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1                           ! 1023
    tSInt32, PARAMETER  :: DecimalPrecision = ToInt32((SignificandBits * LB2To10_M1) / LB2To10_M2)      ! 15
    tSInt32, PARAMETER  :: DecimalRange     = ToInt32(((ExponentBias - 1) * LB2To10_M1) / LB2To10_M2)   ! 307
    tSInt32, PARAMETER  :: MaxDecimalConversionDigits = 767
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ masking parameters +++
    tUInt64, PARAMETER  :: SigHidBitMask    = SHIFTL(1_kInt64, SignificandBits)
    tUInt64, PARAMETER  :: SignificandMask  = SigHidBitMask - 1_kInt64
    tUInt64, PARAMETER  :: SignMask         = SHIFTL(1_kInt64, SignBits)
    tUInt64, PARAMETER  :: ExponentMask     = NOT(IOR(SignMask, SignificandMask))
    tUInt64, PARAMETER  :: ExpMantMask      = SignificandMask + ExponentMask        ! = NOT(SignMask)
    tUInt64, PARAMETER  :: QuietNaNMask     = SHIFTL(1_kInt64, SignificandBits - 1)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Exceptional exponent value for NaN or Infinity
    tSInt32, PARAMETER  :: ExceptionalExponent = ToInt32(Z'7FFFFFFF')
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ maximum and minimum (positive) parameters +++
    tUInt64, PARAMETER  :: MinSubnormal = 1_kInt64
    tUInt64, PARAMETER  :: MaxSubnormal = SHIFTL(1_kInt64, SignificandBits) - 1_kInt64
    tUInt64, PARAMETER  :: MinNormal    = SHIFTL(1_kInt64, SignificandBits)
    tUInt64, PARAMETER  :: MaxNormal    = IOR(SHIFTL(ToInt64(MaxExponent - 1), SignificandBits), MaxSubnormal)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
    ! 600 is an arbitrary number of digits, but should be large enough for any practical number.
    ! Important note: a number of digits large enough to represent the smallest subnormal
    ! for double-precision number is about 1109 (= 342 + 767).
    tUInt32, PARAMETER  :: MAX_NUM_DIGITS = 600
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
    ! Log2(10**(767 + 342))`, or ~3684 bits, so we round to 3712.
    tSInt32, PARAMETER  :: BigUIntBits = 3712
    ! the (fixed) capacity of a BigUInt
    tSInt32, PARAMETER  :: BigCapacity = BigUIntBits / DigitBits   ! = 58
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tUInt64, PARAMETER  :: DivBase      = 10_kInt64
    tUInt64, PARAMETER  :: MaxDivbyBase = 1844674407370955161_kInt64
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ number parameters +++
    tUInt64, PARAMETER  :: TwoUInt   = 2_kInt64
    tUInt64, PARAMETER  :: ThreeUInt = 3_kInt64
    tUInt64, PARAMETER  :: FourUInt  = 4_kInt64
    tUInt64, PARAMETER  :: FiveUInt  = 5_kInt64
    tUInt64, PARAMETER  :: FortyUInt = 40_kInt64
    tUInt64, PARAMETER  :: TenUInt   = 10_kInt64
    tUInt64, PARAMETER  :: HundredUInt     = 100_kInt64
    tUInt64, PARAMETER  :: TenThousandUInt = 10000_kInt64
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ----------------------------------------------------
    ! -----   Simple-Decimal-Algorithm' parameters   -----
    ! ----------------------------------------------------
    ! The nth item in Powers_Of_Two represents the greatest power of two less than
    ! 10^n. This tells us how much we can safely shift without overshooting.
    tUInt8,  PARAMETER  :: Powers_Of_Two(0:18) = [ &
            0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43, 46, 49, 53, 56, 59]
    tSInt32, PARAMETER  :: Num_Powers_Of_Two = SIZE(Powers_Of_Two)                                      ! = 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Eisel-Lemire-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32, PARAMETER  :: LowBits = TotalBits - SignificandBits - 3                                ! = 9
    ! The halfway constant is used to check if the bits that will be shifted away initially are all 1.
    tUInt64, PARAMETER  :: HalfWay = SHIFTL(1_kInt64, LowBits) - 1_kInt64                           ! = 511
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Clinger-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32             :: Idx
    tFloat,  PARAMETER  :: Powers_Of_Ten(0:22)  = [(10.0D0**Idx, Idx = 0, 22)]
    tSInt32, PARAMETER  :: Num_Exact_Pow10 = 22
    tSInt32, PARAMETER  :: Num_Mantissa_Digits = 15
    tFloat,  PARAMETER  :: Max_Exact_Integer = 9007199254740991.0_kDouble
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tSInt32, PARAMETER  :: Exponent_UppBound =  309     ! = 308 + 1
    tSInt32, PARAMETER  :: Exponent_LowBound = -343     ! = (-324) - 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Dragonbox-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! parameters for main routine
    tSInt32, PARAMETER  :: Kappa = 2
    tSInt32, PARAMETER  :: Big_Divisor = 10**(Kappa+1)              ! 1000
    tSInt32, PARAMETER  :: Small_Divisor = Big_Divisor / 10         ! 100
    tSInt32, PARAMETER  :: Half_Small_Divisor = Small_Divisor / 2   ! 50
    tSInt32, PARAMETER  :: Divisibility_Check_By_5_Threshold = 86
    tSInt32, PARAMETER  :: Case_Fc_Pm_Half_Lower_Threshold = -2
    ! parameters for short interval case
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Lower_Threshold = 2
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Upper_Threshold = 3
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Lower_Threshold = -77
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Upper_Threshold = -77
    ! parameters for Is_Divisible_By_Pow10 routine
    tSInt32, PARAMETER  :: Info_Shift_Amount = 20
    tSInt32, PARAMETER  :: OneShiftL = SHIFTL(1, Info_Shift_Amount)
    tSInt32, PARAMETER  :: Comparison_Mask = OneShiftL - 1
    tSInt32, PARAMETER  :: Magic_Number = OneShiftL/Small_Divisor + 1
    ! parameters for Divide_By_10_To_Kappa_Plus_1
    tUInt64, PARAMETER  :: DivM = 2361183241434822607_kInt64
    tSInt32, PARAMETER  :: DivS = 7     ! 71 - 64
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Ryu-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: BitsPerPow5 = 128
    tSInt32, PARAMETER  :: MaxExp_ModInv5 = 27
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Schubfach-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: Pow10_Min_Exact_Exp = 0
    tSInt32, PARAMETER  :: Pow10_Max_Exact_Exp = 55
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
    tSInt32, PARAMETER  :: Max_Exponent_Round_To_Even = 23
    tSInt32, PARAMETER  :: Min_Exponent_Round_To_Even = -4
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
    tUInt64, PARAMETER  :: BitMask        = SHIFTL(1_kInt64, LowBits) - 1_kInt64    ! = Halfway
    tUInt64, PARAMETER  :: BitMaskMinus1  = BitMask - 1_kInt64
    tUInt64, PARAMETER  :: AddRound       = SHIFTL(1_kInt64, ExponentBits - 1)
    tUInt64, PARAMETER  :: MaxUInt        = MaxU64
    tUInt64, PARAMETER  :: FpRawInf       = ToInt64(Z'7FF0000000000000')            ! = ExponentMask
    tSInt32, PARAMETER  :: MaxExpBin      = 1024
    tSInt32, PARAMETER  :: MinExpBin      = -1021
    tSInt32, PARAMETER  :: UIntSafeDigits = 19
    tSInt32, PARAMETER  :: MaxDecDigits   = MaxDecimalConversionDigits + 1
    tUInt64, PARAMETER  :: MaxMantissa    = SHIFTL(1_kInt64, BinaryPrecision)
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
        tUInt64     :: Sig              ! base-10 significand
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
        tUInt64     :: Significand  ! significand/mantissa (M)
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

! include routines for parsing floating-point-number string
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
!                           REAL64 AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION DivByPow10(X, P) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Y = X .UDIV. (10**P)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: X    ! X <= 10**20
    tSInt32, INTENT(IN) :: P    ! 1 <= P <= 10
    tUInt64             :: Y

!** SUBROUTINE IPARAMETER DECLARATIONS:
    ! Parameters for division by power of 10 applicable for N <= 20 digits
    ! (i.e. used for division of the 'Significand')
    ! Note: elements in the row are in little-endian order
    ! (i.e. element 0 is the least significant byte and element 1 is the most one)
    tUInt64, PARAMETER  :: MagicM(0:1,1:10) = RESHAPE([             &
        ToInt64(Z'6666666666666667'), ToInt64(Z'0000000000000006'), &
        ToInt64(Z'3D70A3D70A3D70A4'), ToInt64(Z'000000000000000A'), &
        ToInt64(Z'3126E978D4FDF3B7'), ToInt64(Z'0000000000000008'), &
        ToInt64(Z'8DB8BAC710CB295F'), ToInt64(Z'0000000000000006'), &
        ToInt64(Z'7C5AC471B4784231'), ToInt64(Z'000000000000000A'), &
        ToInt64(Z'637BD05AF6C69B5B'), ToInt64(Z'0000000000000008'), &
        ToInt64(Z'B5FCA6AF2BD215E2'), ToInt64(Z'0000000000000006'), &
        ToInt64(Z'BCC77118461CEFD0'), ToInt64(Z'000000000000000A'), &
        ToInt64(Z'9705F4136B4A5974'), ToInt64(Z'0000000000000008'), &
        ToInt64(Z'DF37F675EF6EADF6'), ToInt64(Z'0000000000000006')], [2,10])
    tSInt32, PARAMETER  :: MagicS(1:10) = [70, 74, 77, 80, 84, 87, 90, 94, 97, 100]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: MulProduct(0:2)
    tUInt64     :: Input(0:0)
    tUInt64     :: Multiplier(0:1)
    tSInt32     :: Shift

!** FLOW

    Input(0)   = X
    Multiplier = MagicM(:,P)
    Shift      = MagicS(P)
    CALL Multiply_N_ShiftRight(Input, 1, Multiplier, 2, Shift, MulProduct)
    Y = MulProduct(0)

    RETURN

END FUNCTION DivByPow10

!******************************************************************************

FUNCTION Write_RealDP(Fp, Ep, cStr, IsScientific) RESULT(sLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To format the decimal F*10**E

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,            INTENT(IN)      :: Fp           ! significand
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
    tSInt32, PARAMETER  :: H = 17
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt32, PARAMETER  :: S98   = 57
    tSInt64, PARAMETER  :: M98   = 1441151881_kInt64
    tSInt32, PARAMETER  :: S178  = 20                         ! = 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! The first powers of 10. The last entry must be 10^H.
    tSInt32             :: I
    tSInt64, PARAMETER  :: Pow10(0:H) = [(10_kInt64**I, I = 0, H)]
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: F, HM
    tSInt32     :: E, HF, MF, LF
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
    sLen = Floor_Log10_Pow2(64 - LEADZ(Fp))
    IF (Fp >= Pow10(sLen)) sLen = sLen + 1

    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*Pow10(H - sLen)
    E = Ep + sLen

    ! ToChars perform digits extraction using integers,
    ! provided that the arguments are limited to 8 digits.
    ! Therefore, split the H = 17 digits of F into:
    !     HF = the most significant digit of F
    !     MF = the next 8 most significant digits of F
    !     LF = the last 8, least significant digits of F
    !
    ! For N = 17, M = 8 the table in section 10 of [2] shows
    !     Floor(F/10**8) = Floor(193,428,131,138,340,668*F/2**84) =
    !     Floor(Floor(193,428,131,138,340,668*F/2**64) / 2**20)
    ! and for N = 9, M = 8
    !     Floor(HM/10**8) = Floor(1,441,151,881*HM/2**57)
    !
    HM = SHIFTR(UMul128_Upper64(F, M178), S178)
    LF = ToInt32(F - DivE8*HM)
    HF = ToInt32(SHIFTR(HM*M98, S98))
    MF = ToInt32(HM - DivE8*ToInt64(HF))

    ! set format flag
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    ! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
            ! plain format without leading zeroes
            sLen = ToChar_Plain_Without_LZ(HF, MF, LF, E, cStr)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            sLen = ToChar_Plain_With_LZ(HF, MF, LF, E, cStr)
        ELSE
            ! scientific notation
            sLen = ToChar_Scientific(HF, MF, LF, E, cStr)
        END IF
    ELSE
        ! scientific notation
        sLen = ToChar_Scientific(HF, MF, LF, E, cStr)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, M, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
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
        Y = ToInt32(SHIFTR(UMul128_Upper64(SHIFTL(ToInt64(M+1), 28), M178), S178)) - 1
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
        ! append L
        Pos = Pos + Write_I32_8_Digits(L, cStr(Pos:)) - 1
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

    !**************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, M, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW

        ! fill the first 4 characters
        cStr(1:4) = '0.00'
        ! compute Pos
        Pos = 3 - E
        ! append H
        cStr(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
        ! append M and L
        Pos = Pos + Write_2I32_16_Digits(M, L, cStr(Pos:)) - 1
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

    !**************************************************************************

    FUNCTION ToChar_Scientific(H, M, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: Y, T, I, Pos

    !** FLOW

        ! append H
        cStr(1:1) = Char1Digit(H)
        ! append period
        cStr(2:2) = '.'
        Pos = 3
        ! append M and L
        Pos = Pos + Write_2I32_16_Digits(M, L, cStr(Pos:)) - 1
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

    !**************************************************************************

    FUNCTION Write_I32_8_Digits(Number, cStr) RESULT(sLen)

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

    END FUNCTION Write_I32_8_Digits

    !**************************************************************************

    FUNCTION Write_2I32_16_Digits(FirstNum, SecondNum, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two (unsigned) integer numbers with a length of 16 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: FirstNum     ! first number
        tSInt32,   INTENT(IN)       :: SecondNum    ! first number
        tCharStar, INTENT(INOUT)    :: cStr         ! character string
        tSInt32                     :: sLen         ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: DumLen

    !** FLOW

        ! write first number
        DumLen = Write_I32_8_Digits(FirstNum, cStr(1:8))

        ! write second number
        DumLen = Write_I32_8_Digits(SecondNum, cStr(9:16))

        ! set length
        sLen = 16

        RETURN

    END FUNCTION Write_2I32_16_Digits

    !**************************************************************************

    FUNCTION Write_I32_Exponent(Exp, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write a signed integer in the range -324 to 308

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
        IF (PosExp < 100) THEN
            IF (PosExp < 10) THEN
                ! 1 digit
                cStr(2:2) = Char1Digit(PosExp)
                sLen = 2
            ELSE
                ! 2 digits
                cStr(2:3) = Char2Digits(PosExp)
                sLen = 3
            END IF
        ELSE
            ! 3 digits
            cStr(2:4) = Char4Digits(PosExp)(2:4)
            sLen = 4
        END IF

        RETURN

    END FUNCTION Write_I32_Exponent

    !**************************************************************************

END FUNCTION Write_RealDP

!------------------------------------------------------------------------------
!
!                       REAL64-TO-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION RealDP_ToString_DragonBox(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a double-precision floating-point value to a character (decimal) string
    ! using the DragonBox algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawBin       ! raw IEEE binary floating point representation
    tUInt64         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigDec       ! significand in base 10
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
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_DragonBox

!******************************************************************************

MODULE FUNCTION RealDP_ToString_Ryu(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a double-precision floating-point value to a character (decimal) string
    ! using the Ryu algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawBin       ! raw IEEE binary floating point representation
    tUInt64         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigDec       ! significand in base 10
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
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_Ryu

!******************************************************************************

MODULE FUNCTION RealDP_ToString_Schubfach(Number, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a double-precision floating-point value to a character (decimal) string
    ! using the Schubfach algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,            INTENT(IN)  :: Number       ! number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawBin       ! raw IEEE binary floating point representation
    tUInt64         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigDec       ! significand in base 10
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
    wLen = (wPos - 1) + Write_RealDP(SigDec, ExpDec, wStr(wPos:), IsScientific)

    ! set output
    cStr = wStr(1:wLen)

    RETURN

END FUNCTION RealDP_ToString_Schubfach

!------------------------------------------------------------------------------
!
!                       REAL64-FROM-STRING MAIN ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION RealDP_FromString_FastFloat(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a double-precision floating-point value
    ! using the FastFloat algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tDouble                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt64         :: RawVal
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
        IF (SlowPath) CALL Dec2Bin_FastFloat(SigDec, ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin, ExpBin)

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

END FUNCTION RealDP_FromString_FastFloat

!******************************************************************************

MODULE FUNCTION RealDP_FromString_LibC(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a double-precision floating-point value
    ! using the LibC algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tDouble                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt64         :: RawVal
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

END FUNCTION RealDP_FromString_LibC

!******************************************************************************

MODULE FUNCTION RealDP_FromString_YY(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a double-precision floating-point value
    ! using the YY algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tDouble                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt64         :: RawVal
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

END FUNCTION RealDP_FromString_YY

!******************************************************************************

MODULE FUNCTION RealDP_FromString_Lemire(cStr, ParseOpt, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a character (decimal) string to a double-precision floating-point value
    ! using the Lemire algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr
    tSInt32,    OPTIONAL, INTENT(IN)    :: ParseOpt
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
    tDouble                             :: Number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    TYPE(StringAux) :: Aux
    tUInt64         :: RawVal
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

END FUNCTION RealDP_FromString_Lemire

!******************************************************************************

END SUBMODULE SubBase_RealDPConv

!******************************************************************************

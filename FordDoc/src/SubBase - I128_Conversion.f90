
SUBMODULE (ModBase_SInt128) SubBase_I128_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations (including those used in an assignment expression and a structure
!   constructor) of the <a href="../module/modbase_sint128.html">SInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           ASSIGNMENT ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE I128_From_I32(I128, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 32-bit integer number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(OUT)   :: I128
    tSInt32,  INTENT(IN)    :: I32      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(ToInt64(I32), 63)
    I128%Low  = ToInt64(I32)

    RETURN

END SUBROUTINE I128_From_I32

!******************************************************************************

MODULE SUBROUTINE I128_From_I64(I128, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 64-bit integer number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(OUT)   :: I128
    tSInt64,  INTENT(IN)    :: I64      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(I64, 63)
    I128%Low  = I64

    RETURN

END SUBROUTINE I128_From_I64

!******************************************************************************

MODULE SUBROUTINE I128_To_I32(I32, I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a signed 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(OUT)   :: I32      !! number treated as signed
    tSInt128, INTENT(IN)    :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I32 = ToInt32(BitCastToSigned(I128%Low))

    RETURN

END SUBROUTINE I128_To_I32

!******************************************************************************

MODULE SUBROUTINE I128_To_I64(I64, I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a signed 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,  INTENT(OUT)   :: I64      !! number treated as signed
    tSInt128, INTENT(IN)    :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I64 = BitCastToSigned(I128%Low)

    RETURN

END SUBROUTINE I128_To_I64

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I32_To_I128(I32) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 32-bit integer number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: I32      !! number treated as signed
    tSInt128            :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(ToInt64(I32), 63)
    I128%Low  = ToInt64(I32)

    RETURN

END FUNCTION I32_To_I128

!******************************************************************************

MODULE FUNCTION I64_To_I128(I64) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 64-bit integer number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: I64      !! number treated as signed
    tSInt128            :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I128%High = SHIFTA(I64, 63)
    I128%Low  = I64

    RETURN

END FUNCTION I64_To_I128

!******************************************************************************

MODULE FUNCTION U32_To_I128(U32, Negative) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 32-bit integer number to a signed 128-bit integer number
    !  where the sign flag is used to indicate whether the 128-bit integer value is
    !  positive or negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: U32      !! number treated as unsigned
    tLogical, INTENT(IN)    :: Negative
    !^ true if the 128-bit integer value is negative. <br>
    ! otherwise, the 128-bit integer value is positive
    tSInt128                :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: Mask = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: U32L

!** FLOW

    IF (Negative) THEN
        I128%High = MaxU64      ! MaxU64 = NOT(0_kInt64)
        U32L = IAND(ToInt64(U32), Mask)
        IF (U32L == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(U32L) + 1_kInt64
    ELSE
        I128%High = 0_kInt64
        I128%Low  = IAND(ToInt64(U32), Mask)
    END IF

    RETURN

END FUNCTION U32_To_I128

!******************************************************************************

MODULE FUNCTION U64_To_I128(U64, Negative) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 64-bit integer number to a signed 128-bit integer number
    !  where the sign flag is used to indicate whether the 128-bit integer value is
    !  positive or negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: U64      !! number treated as unsigned
    tLogical, INTENT(IN)    :: Negative
    !^ True if the 128-bit integer value is negative. <br>
    !  Otherwise, the 128-bit integer value is positive.
    tSInt128                :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

     IF (Negative) THEN
        I128%High = MaxU64      ! MaxU64 = NOT(0_kInt64)
        IF (U64 == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(U64) + 1_kInt64
    ELSE
        I128%High = 0_kInt64
        I128%Low  = U64
    END IF

    RETURN

END FUNCTION U64_To_I128

!******************************************************************************

MODULE FUNCTION R32_To_I128(R32) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 32-bit floating point number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle, INTENT(IN) :: R32
    tSInt128            :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER  :: Mask = ToInt32(Z'000000FF')    ! 255
    tUInt32, PARAMETER  :: C1   = SHIFTL(1, 23)             ! 2**23
    tUInt32, PARAMETER  :: C2   = C1 - 1                    ! 2**23 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R32 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 is NOT finite.')
        RETURN
    ELSEIF (R32 < -2.0_kSingle**127) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 < I128Min.')
        RETURN
    ELSEIF (R32 >= 2.0_kSingle**127) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 > I128Max.')
        RETURN
    END IF

    !--------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R32 using C_F_POINTER)  +++++!
    !--------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 32-bit integer number equivalent to an absolute value of the 32-bit real number
    fPtr = ABS(R32)

    ! determine exponent bits
    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => I128 = IBits
    ! => I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            I128 = SInt128(0_kInt64, 0_kInt64)
        ELSE
            I128 = SInt128(0_kInt64, SHIFTR(ToInt64(IBits), Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            I128 = SInt128(0_kInt64, 0_kInt64)
        ELSEIF (Exp >= 64) THEN
            I128 = SInt128(SHIFTL(ToInt64(IBits), Exp - 64), 0_kInt64)
        ELSE
            I128 = SInt128(SHIFTR(ToInt64(IBits), 64 - Exp), SHIFTL(ToInt64(IBits), Exp))
        END IF
    END IF
    ! add sign bit
    IF (R32 < 0.0_kSingle) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(I128%Low) + 1_kInt64
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

END FUNCTION R32_To_I128

!******************************************************************************

MODULE FUNCTION R64_To_I128(R64) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit floating point number to a signed 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: R64
    tSInt128            :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: Mask = ToInt64(Z'00000000000007FF')   ! 2047
    tUInt64, PARAMETER  :: C1   = SHIFTL(1_kInt64, 52)           ! 2**52
    tUInt64, PARAMETER  :: C2   = C1 - 1_kInt64                  ! 2**52 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R64 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R64)) THEN
        CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R64 is NOT finite.')
        RETURN
    ELSEIF (R64 < -2.0_kDouble**127) THEN
        CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R64 < I128Min.')
        RETURN
    ELSEIF (R64 >= 2.0_kDouble**127) THEN
        CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R64 > I128Max.')
        RETURN
    END IF

    !--------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R64 using C_F_POINTER)  +++++!
    !--------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 64-bit integer number equivalent to an absolute value of the 64-bit real number
    fPtr = ABS(R64)
    
    ! determine exponent bits
    Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => I128 = SInt128(0_kInt64, IBits)
    ! => I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            I128 = SInt128(0_kInt64, 0_kInt64)
        ELSE
            I128 = SInt128(0_kInt64, SHIFTR(IBits, Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            I128 = SInt128(0_kInt64, 0_kInt64)
        ELSEIF (Exp >= 64) THEN
            I128 = SInt128(SHIFTL(IBits, Exp - 64), 0_kInt64)
        ELSE
            I128 = SInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
        END IF
    END IF
    ! add sign bit
    IF (R64 < 0.0_kDouble) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(I128%Low) + 1_kInt64
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

END FUNCTION R64_To_I128

!******************************************************************************

MODULE FUNCTION R128_To_I128(R128) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 128-bit floating point number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad, INTENT(IN)   :: R128
    tSInt128            :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER  :: Mask  = ToInt32(Z'00007FFF')                 ! 32767
    tUInt64, PARAMETER  :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]  ! 2**112 = SHIFTL(1, 112)
    tUInt64, PARAMETER  :: C2(2) = [-1_kInt64, 281474976710655_kInt64]  ! 2**112 -1 = SHIFTL(1, 112) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    tUInt64             :: ExpL
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R128 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R128 is NOT finite.')
        RETURN
    ELSEIF (R128 < -2.0_kQuad**127) THEN
        CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R128 < I128Min.')
        RETURN
    ELSEIF (R128 >= 2.0_kQuad**127) THEN
        CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R128 > I128Max.')
        RETURN
    END IF

    !---------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R128 using C_F_POINTER)  +++++!
    !---------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 128-bit integer number equivalent to an absolute value of the 128-bit real number
    fPtr = ABS(R128)
    IF (.NOT.IsLittleEndian) THEN
        ! big-endian so swap IBits(1) and IBits(2)
        BLOCK
            tUInt64 :: Tmp
            Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        END BLOCK
    END IF
    
    ! determine exponent bits
    ExpL  = SHIFTR(IBits(2), 48)                ! 48 = 112-64
    Exp = IAND(ToInt32(ExpL), Mask) - 16495   ! 16495 = 16383 + 112
    ! determine significand bits and convert to SInt128
    I128%Low  = IOR(IAND(IBits(1), C2(1)), C1(1))
    I128%High = IOR(IAND(IBits(2), C2(2)), C1(2))
    ! add exponent bits
    ! => I128 = ISHFT(I128, Exp)
    IF (Exp < 0) THEN
        I128 = SHIFTR(I128, -Exp)
    ELSE
        I128 = SHIFTL(I128, Exp)
    END IF
    ! add sign bit
    IF (R128 < 0.0_kQuad) THEN
        !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(I128%Low) + 1_kInt64
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION R128_To_I128

!******************************************************************************

MODULE FUNCTION DecString_To_I128(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a decimal string to a signed 128-bit integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tSInt128                            :: Number   !! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: A0           = IACHAR('0')
    tSInt32,  PARAMETER :: A4           = IACHAR('4')
    tSInt32,  PARAMETER :: A9           = IACHAR('9')
    tSInt32,  PARAMETER :: MaxDigitI32  = 10
    tSInt32,  PARAMETER :: MaxDigitI64  = 19
    tSInt32,  PARAMETER :: MaxDigitI128 = 39
    tSInt128, PARAMETER :: I128Base     = TenI128
    tCharParam          :: MaxStr       = '170141183460469231731687303715884105727'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen, DigitLen
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart, IndxP7
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: NegSign
    tLogical                :: Overflow
    tCharAlloc, TARGET      :: CurStr
    tLogical                :: ErrorFlag  ! true if input is not invalid
    tSInt32                 :: I32Val
    tSInt64                 :: I64Val
    TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
    tUInt64,    POINTER     :: wPtr     ! Fortran pointer to wStr
    tCharLen(8), TARGET     :: wStr

!** FLOW

    ! get valid string length by removing the trailing space(s)
    StrLen = LEN_TRIM(cStr)
    IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

    ! check whether there are spaces in front of the number
    ! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    IF (cStr(Indx:Indx) == ' ') THEN
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= ' ') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
    END IF

    ! check for sign
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
    Number = 0_kInt64
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Number = 0_kInt64
            RETURN
        END IF
    END IF

    ! compute the length of digits
    DigitLen = StrLen - Indx + 1

    ! return quickly if possible
    IF (DigitLen < MaxDigitI32) THEN
        I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg)
        IF (ErrorFlag) THEN
            Number = MinI128
        ELSE
            IF (NegSign) THEN
                Number = -I32Val
            ELSE
                Number = I32Val
            END IF
        END IF
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    ELSEIF (DigitLen < MaxDigitI64) THEN
        I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg)
        IF (ErrorFlag) THEN
            Number = MinI128
        ELSE
            IF (NegSign) THEN
                Number = -I64Val
            ELSE
                Number = I64Val
            END IF
        END IF
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    END IF

    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        ! get a C pointer to IBits
        cPtr = C_LOC(wStr)
        ! associate a Fortran data pointer with the C pointer
        CALL C_F_POINTER(cPtr, wPtr)
        ! initialize
        IStart = Indx
        IndxP7 = Indx + 7
        DO WHILE (IndxP7 <= StrLen)
            wStr = cStr(Indx:IndxP7)
            IF (Is8Digits(wPtr)) THEN
                ! process 8 digits at once
                ! Number = Number*100000000 + Parse8Digits(wPtr)
                CALL Multiply(Number, 100000000)
                CALL Add(Number, Parse8Digits(wPtr))
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                ! nullify pointers
                NULLIFY(wPtr)
                cPtr = C_NULL_PTR
                RETURN
            END IF
            Indx = Indx + 8
            IndxP7 = Indx + 7
        END DO
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL Multiply(Number, 10)
                CALL Add(Number, (IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    ! nullify pointers
                    NULLIFY(wPtr)
                    cPtr = C_NULL_PTR
                    RETURN
                END IF
            END DO
        END IF
        NumDigit = Indx - IStart
        ! nullify pointers
        NULLIFY(wPtr)
        cPtr = C_NULL_PTR
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI128
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI128) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI128) THEN
        ! value might be in the applicable range
        IF (IsNegative(Number)) THEN
            ! overflow likely occurs
            Overflow = TrueVal
            IF ((NegSign).AND.(Number == MinI128)) THEN
                ! actually not overflow
                CurStr = '-' // cStr(IStart:StrLen)
                IF (ToDecString(MinI128) == CurStr) THEN
                    Overflow = FalseVal
                    NegSign = FalseVal
                END IF
            END IF
        ELSE
            ! positive value so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitI128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI128
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI128
        END IF
    ELSE
        IF (NegSign) Number = -Number
    END IF

    RETURN
    CONTAINS

    FUNCTION Parse8Digits(InVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To parse eight digits immediately.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: InVal
        tUInt64             :: OutVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: K1 = ToInt64(Z'0F0F0F0F0F0F0F0F')
        tUInt64, PARAMETER  :: K2 = ToInt64(Z'00FF00FF00FF00FF')
        tUInt64, PARAMETER  :: K3 = ToInt64(Z'0000FFFF0000FFFF')
        tUInt64, PARAMETER  :: M1 = 2561_kInt64
        tUInt64, PARAMETER  :: M2 = 6553601_kInt64
        tUInt64, PARAMETER  :: M3 = 42949672960001_kInt64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

        RETURN

    END FUNCTION Parse8Digits

    !***************************************************************************

    FUNCTION Is8Digits(InVal) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether we can process eight digits immediately.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: InVal
        tLogical            :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: C1 = ToInt64(Z'F0F0F0F0F0F0F0F0')
        tUInt64, PARAMETER  :: C2 = ToInt64(Z'3333333333333333')
        tUInt64, PARAMETER  :: C3 = ToInt64(Z'0606060606060606')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2

        RETURN

    END FUNCTION Is8Digits

    !***************************************************************************

END FUNCTION DecString_To_I128

!------------------------------------------------------------------------------
!
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION U32_From_I128(I128) RESULT(U32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to an unsigned 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tUInt32                 :: U32      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U32 = ToInt32(I128%Low)

    RETURN

END FUNCTION U32_From_I128

!******************************************************************************

MODULE FUNCTION U64_From_I128(I128) RESULT(U64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to an unsigned 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tUInt64                 :: U64      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U64 = I128%Low

    RETURN

END FUNCTION U64_From_I128

!******************************************************************************

MODULE FUNCTION U128_From_I128(I128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer to an unsigned 128-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    TYPE(UInt128)           :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128 = UInt128(I128%High, I128%Low)

    RETURN

END FUNCTION U128_From_I128

!******************************************************************************

MODULE FUNCTION R32_From_I128(I128) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a 32-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tSingle                 :: R32

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64
    tUInt32, PARAMETER  :: TwoPow23 = SHIFTL(1, 23)
    tUInt32, PARAMETER  :: Mask     = ToInt32(Z'000000FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Negative
    tSInt64             :: High
    tUInt64             :: Low
    tSInt32             :: S, Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_kInt64)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) High = High + 1_kInt64
        Low  = NOT(I128%Low) + 1_kInt64
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF

    IF (High == 0_kInt64) THEN
        R32 = U64_To_R32(Low)
        IF (IsNegative(I128)) R32 = -R32
        RETURN
    END IF

    S = LEADZ(High)
    ! Mask out the 24 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 40) THEN
        IBits = IEOR(ToInt32(SHIFTR(High, 40-S)), TwoPow23)
    ELSE
        ! S-40 == additional bits we need
        IBits = IEOR(ToInt32(IOR(SHIFTL(High, S-40), SHIFTR(Low, 104-S))), TwoPow23)
    END IF
    ! get the binary exponent
    Exp = IAND(254-S, Mask)         ! 254 = 64 + 64 + 127 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 23))

    !----------------------------------------------------------------------!
    !+++++  Transfer output (R32 mapped to IBits using C_F_POINTER)   +++++!
    !----------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! get a 32-bit real number equivalent to the 32-bit integer number
    R32 = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    ! check and add sign if needed
    IF (Negative) R32 = -R32

    RETURN

CONTAINS

    FUNCTION U64_To_R32(LongVal) RESULT(SingleVal)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an unsigned 64-bit integer number to a 32-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal      !! integer number treated as unsigned one
        tSingle             :: SingleVal    !! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            SingleVal = REAL(LongVal, KIND=kSingle)
        ELSE
            SingleVal = TwoPow64 + REAL(LongVal, KIND=kSingle)
        END IF

        RETURN

    END FUNCTION U64_To_R32

    !***************************************************************************

END FUNCTION R32_From_I128

!******************************************************************************

MODULE FUNCTION R64_From_I128(I128) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a 64-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tDouble                 :: R64

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64
    tUInt64, PARAMETER  :: TwoPow52 = SHIFTL(1_kInt64, 52)
    tUInt32, PARAMETER  :: Mask     = ToInt32(Z'000007FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Negative
    tSInt64             :: High
    tUInt64             :: Low
    tSInt32             :: S
    tUInt64             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_kInt64)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) High = High + 1_kInt64
        Low  = NOT(I128%Low) + 1_kInt64
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF

    IF (High == 0_kInt64) THEN
        R64 = U64_To_R64(Low)
        IF (Negative) R64 = -R64
        RETURN
    END IF

    S = LEADZ(High)
    ! Mask out the 53 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 11) THEN
        IBits = IEOR(SHIFTR(High, 11-S), TwoPow52)
    ELSE
        ! S-11 == additional bits we need
        IBits = IEOR(IOR(SHIFTL(High, S-11), SHIFTR(Low, 75-S)), TwoPow52)
    END IF
    ! get the binary exponent
    Exp = ToInt64(IAND(1150-S, Mask))        ! 1150 = 64 + 64 + 1023 - 1

    ! Add the exponent
    IBits = IOR(IBits, SHIFTL(Exp, 52))

    !----------------------------------------------------------------------!
    !+++++  Transfer output (R64 mapped to IBits using C_F_POINTER)   +++++!
    !----------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! get a 64-bit real number equivalent to the 64-bit integer number
    R64 = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    ! check and add sign if needed
    IF (Negative) R64 = -R64

    RETURN

CONTAINS

    FUNCTION U64_To_R64(LongVal) RESULT(DoubleVal)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an unsigned 64-bit integer number to a 64-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal      !! integer number treated as unsigned one
        tDouble             :: DoubleVal    !! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            DoubleVal = REAL(LongVal, KIND=kDouble)
        ELSE
            DoubleVal = TwoPow64 + REAL(LongVal, KIND=kDouble)
        END IF

        RETURN

    END FUNCTION U64_To_R64

    !***************************************************************************

END FUNCTION R64_From_I128

!******************************************************************************

MODULE FUNCTION R128_From_I128(I128) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a 128-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tQuad                   :: R128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tQuad,   PARAMETER  :: TwoPow64     = 2.0_kQuad**64
    tUInt64, PARAMETER  :: TwoPow112(2) = [ 0_kInt64, 281474976710656_kInt64] ! SHIFTL(1, 112)
    tUInt32, PARAMETER  :: Mask         = ToInt32(Z'00007FFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Negative
    tSInt64             :: High
    tUInt64             :: Low
    tSInt32             :: S, Shift
    tUInt64             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

!** FLOW

    ! get sign flag and absolute values of components
    Negative = (I128%High < 0_kInt64)
    IF (Negative) THEN
        High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) High = High + 1_kInt64
        Low  = NOT(I128%Low) + 1_kInt64
    ELSE
        High = I128%High
        Low  = I128%Low
    END IF

    IF (High == 0_kInt64) THEN
        R128 = U64_To_R128(Low)
        IF (Negative) R128 = -R128
        RETURN
    END IF

    S = LEADZ(High)
    IF (S >= 15) THEN
        R128 = U64_To_R128(Low) + REAL(High, KIND=kQuad)*TwoPow64
        IF (Negative) R128 = -R128
        RETURN
    END IF

    ! Mask out the 113 MSBits
    Shift = 15 - S
    IBits(2) = SHIFTR(High, Shift)
    IBits(1) = IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift))

    ! get the binary exponent
    Exp = ToInt64(IAND(16510-S, Mask))   ! 16510 = 64 + 64 + 16383 - 1

    ! The leading bit is implicit, cancel it out to get the significand
    ! and also add the exponent
    IBits(1) = IEOR(IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift)), TwoPow112(1))
    IBits(2) = IOR(IEOR(SHIFTR(High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))    ! 48 = 112 - 64

    !-----------------------------------------------------------------------!
    !+++++  Transfer output (R128 mapped to IBits using C_F_POINTER)   +++++!
    !-----------------------------------------------------------------------!
    IF (.NOT.IsLittleEndian) THEN
        ! big-endian so swap IBits(1) and IBits(2)
        BLOCK
            tUInt64 :: Tmp
            Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        END BLOCK
    END IF
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! get a 128-bit real number equivalent to the 128-bit integer number
    R128 = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    ! check and add sign if needed
    IF (Negative) R128 = -R128

    RETURN

CONTAINS

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an unsigned 64-bit integer number to a 128-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal  !! integer number treated as unsigned one
        tQuad               :: QuadVal  !! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            QuadVal = REAL(LongVal, KIND=kQuad)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=kQuad)
        END IF

        RETURN

    END FUNCTION U64_To_R128

    !***************************************************************************

END FUNCTION R128_From_I128

!******************************************************************************

MODULE FUNCTION DecString_From_I128(I128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a decimal string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tCharAlloc              :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: BufLen, Top, I, J
    tCharLen(41)    :: Buffer
    tSInt128        :: Copy
    tSInt64         :: Tmp
    tSInt64         :: Indx
    tLogical        :: Negative

!** FLOW

    IF (I128 == ZeroI128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Negative = IsNegative(I128)
    IF (Negative) THEN
        IF (I128 == MinI128) THEN
            Str  = '-170141183460469231731687303715884105728'
            RETURN
        ELSE
            Copy = -I128
        END IF
    ELSE
        Copy = I128
    END IF
    DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_kInt64)
            Indx = MOD(Tmp, 10_kInt64)
            Buffer(Top:Top) = Char1Digit(Indx)
            Top = Top - 1
            Tmp = Tmp / 10_kInt64
        END DO
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Negative) THEN
        Buffer(Top:Top) = '-'
        Str = Buffer(Top:BufLen)
    ELSE
        Str = Buffer(Top+1:BufLen)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToStringDivide(I128) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To divide the number by 10**13 and return the remainder.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(INOUT) :: I128
        tSInt64                 :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER :: Pow2  = SHIFTL(1, 13)
        tSInt32,  PARAMETER :: Pow5  = 1220703125
        tSInt64,  PARAMETER :: Pow10 = ToInt64(Pow2)*ToInt64(Pow5)
        tLogical, PARAMETER :: Positive = FalseVal
        tLogical, PARAMETER :: AsUnsigned = TrueVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128    :: Rem, Quot, Numer, Pow10_128
        tSInt64     :: Q, R, Mod2

    !** FLOW

        Q = I128%High / Pow5
        R = I128%High - Q*Pow5
        I128%High = SHIFTR(Q, 13)

        Numer%High = R
        Numer%Low  = I128%Low
        Mod2 = IAND(I128%Low, Pow2 - 1_kInt64)

        CALL DivMod(Numer, SInt128(Pow5), Quot, Rem)
        I128%Low = IOR(SHIFTL(Q, 51), SHIFTR(Quot%Low, 13))

        ! Applies the Chinese Rem Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        Pow10_128 = SInt128(0_kInt64, Pow10)
        Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
        IF (Rem%High < 0_kInt64) Rem = Rem + Pow10
        Remainder = Rem%Low

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

    FUNCTION SMOD(Dividend, Divisor) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform division of two SInt128 objects (Dividend / Divisor)
        ! and return the remainder. <br>

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)    :: Dividend
        tSInt128, INTENT(IN)    :: Divisor
        tSInt128                :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128    :: Quotient

    !** FLOW

        CALL DivMod(Dividend, Divisor, Quotient, Remainder)

        RETURN

    END FUNCTION SMOD

    !***************************************************************************

END FUNCTION DecString_From_I128

!******************************************************************************

MODULE FUNCTION HexString_From_I128(I128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 128-bit integer number to a hexadecimal string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tCharAlloc              :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: U128

!** FLOW

    IF (I128 == MinI128) THEN
        Str  = '-80000000000000000000000000000000'
    ELSEIF (IsNegative(I128)) THEN
        U128 = ToU128(-I128)
        Str  = '-' // ToHexString(U128)
    ELSE
        U128 = ToU128(I128)
        Str  = ToHexString(U128)
    END IF

    RETURN

END FUNCTION HexString_From_I128

!******************************************************************************

END SUBMODULE SubBase_I128_Conversion

!******************************************************************************

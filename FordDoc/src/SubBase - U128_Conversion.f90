
SUBMODULE (ModBase_UInt128) SubBase_U128_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations (including those used in an assignment expression and a structure
!   constructor) of the <a href="../module/modbase_uint128.html">UInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tUInt128        TYPE(UInt128)

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

MODULE SUBROUTINE U128_From_U32(U128, U32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 32-bit integer number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(OUT)   :: U128
    tUInt32,  INTENT(IN)    :: U32      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128 = UInt128(MinU64, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE U128_From_U32

!******************************************************************************

MODULE SUBROUTINE U128_From_U64(U128, U64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 64-bit integer number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(OUT)   :: U128
    tUInt64,  INTENT(IN)    :: U64      !! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128 = UInt128(MinU64, U64)

    RETURN

END SUBROUTINE U128_From_U64

!******************************************************************************

MODULE SUBROUTINE U128_To_U32(U32, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to an unsigned 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(OUT)   :: U32      !! number treated as unsigned
    tUInt128, INTENT(IN)    :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U32 = ToInt32(U128%Low)

    RETURN

END SUBROUTINE U128_To_U32

!******************************************************************************

MODULE SUBROUTINE U128_To_U64(U64, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to an unsigned 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(OUT)   :: U64      !! number treated as unsigned
    tUInt128, INTENT(IN)    :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U64 = U128%Low

    RETURN

END SUBROUTINE U128_To_U64

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I32_To_U128(I32, AsUnsigned) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 32-bit integer number to an unsigned 128-bit integer number
    !  or to convert an unsigned 32-bit integer number to an unsigned 128-bit integer
    !  number if the specified flag is present and true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,            INTENT(IN)  :: I32          !! number treated as signed (default)
    tLogical, OPTIONAL, INTENT(IN)  :: AsUnsigned   !! if present and true, number treated as unsigned
    tUInt128                        :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(AsUnsigned)) THEN
        IF (AsUnsigned) THEN
            ! number treated as unsigned
            U128 = UInt128(MinU64, ToUnsignedLong(I32))
            RETURN
        END IF
    END IF
    ! number treated as signed
    IF (I32 < 0) THEN
        U128 = UInt128(MaxU64, ToInt64(I32))
    ELSE
        U128 = UInt128(MinU64, ToInt64(I32))
    END IF

    RETURN

END FUNCTION I32_To_U128

!******************************************************************************

MODULE FUNCTION I64_To_U128(I64, AsUnsigned) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 64-bit integer number to an unsigned 128-bit integer number
    !  or to convert an unsigned 64-bit integer number to an unsigned 128-bit integer
    !  number if the specified flag is present and true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,            INTENT(IN)  :: I64          !! number treated as signed (default)
    tLogical, OPTIONAL, INTENT(IN)  :: AsUnsigned   !! if present and true, number treated as unsigned
    tUInt128                        :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (PRESENT(AsUnsigned)) THEN
        IF (AsUnsigned) THEN
            ! number treated as unsigned
            U128 = UInt128(MinU64, I64)
            RETURN
        END IF
    END IF
    IF (I64 < 0_kInt64) THEN
        U128 = UInt128(MaxU64, I64)
    ELSE
        U128 = UInt128(MinU64, I64)
    END IF

    RETURN

END FUNCTION I64_To_U128

!******************************************************************************

MODULE FUNCTION R32_To_U128(R32) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 32-bit floating point number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle, INTENT(IN) :: R32
    tUInt128            :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER  :: Mask = ToInt32(Z'010000FF')  ! 255
    tUInt32, PARAMETER  :: C1   = SHIFTL(1, 23)         ! 2**23
    tUInt32, PARAMETER  :: C2   = C1 - 1                ! 2**23 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R32 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R32 is NOT finite.')
        RETURN
    ELSEIF (R32 <= -1.0_kSingle) THEN
        CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R32 < U128Min.')
        RETURN
    ELSEIF (R32 >= 2.0_kSingle**128) THEN
        CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R32 > U128Max.')
        RETURN
    ELSEIF (R32 < 0.0_kSingle) THEN
        U128 = ZeroU128
        RETURN
    END IF

    !--------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R32 using C_F_POINTER)  +++++!
    !--------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 32-bit integer number equivalent to a 32-bit real number
    fPtr = R32
    
    ! determine exponent bits
    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => U128 = UInt128(MinU64, IBits)
    ! => U128 = ISHFT(U128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSE
            U128 = UInt128(MinU64, SHIFTR(ToInt64(IBits), Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128 = UInt128(SHIFTL(ToInt64(IBits), Exp - 64), MinU64)
        ELSE
            U128 = UInt128(SHIFTR(ToInt64(IBits), 64 - Exp), SHIFTL(ToInt64(IBits), Exp))
        END IF
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

END FUNCTION R32_To_U128

!******************************************************************************

MODULE FUNCTION R64_To_U128(R64) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 64-bit floating point number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: R64
    tUInt128            :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: Mask = ToInt64(Z'01000000000007FF')  ! 2047
    tUInt64, PARAMETER  :: C1   = SHIFTL(1_kInt64, 52)          ! 2**52
    tUInt64, PARAMETER  :: C2   = C1 - 1_kInt64                 ! 2**52 - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R64 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R64)) THEN
        CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R64 is NOT finite.')
        RETURN
    ELSEIF (R64 <= -1.0_kDouble) THEN
        CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R64 < U128Min.')
        RETURN
    ELSEIF (R64 >= 2.0_kDouble**128) THEN
        CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R64 > U128Max.')
        RETURN
    ELSEIF (R64 < 0.0_kDouble) THEN
        U128 = ZeroU128
        RETURN
    END IF

    !--------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R64 using C_F_POINTER)  +++++!
    !--------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 64-bit integer number equivalent to a 64-bit real number
    fPtr = R64
    
    ! determine exponent bits
    Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert and add exponent bits
    ! => U128 = UInt128(MinU64, IBits)
    ! => U128 = ISHFT(U128, Exp)
    IF (Exp < 0) THEN
        Exp = -Exp
        IF (Exp >= 64) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSE
            U128 = UInt128(MinU64, SHIFTR(IBits, Exp))
        END IF
    ELSE
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128 = UInt128(SHIFTL(IBits, Exp - 64), MinU64)
        ELSE
            U128 = UInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
        END IF
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

END FUNCTION R64_To_U128

!******************************************************************************

MODULE FUNCTION R128_To_U128(R128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 128-bit floating point number to an unsigned 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad, INTENT(IN)   :: R128
    tUInt128            :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER  :: Mask  = ToInt32(Z'01007FFF')                 ! 32767
    tUInt64, PARAMETER  :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]  ! 2**112 = SHIFTL(1, 112)
    tUInt64, PARAMETER  :: C2(2) = [-1_kInt64, 281474976710655_kInt64]  ! 2**112 -1 = SHIFTL(1, 112) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: Exp
    tSInt64             :: ExpL
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

!** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R128 is NaN or cannot fit into U128.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R128 is NOT finite.')
        RETURN
    ELSEIF (R128 <= -1.0_kQuad) THEN
        CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R128 < U128Min.')
        RETURN
    ELSEIF (R128 >= 2.0_kQuad**128) THEN
        CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                          'Undefined behavior: R128 > U128Max.')
        RETURN
    ELSEIF (R128 < 0.0_kQuad) THEN
        U128 = ZeroU128
        RETURN
    END IF

    !---------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R128 using C_F_POINTER)  +++++!
    !---------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 128-bit integer number equivalent to a 128-bit real number
    fPtr = R128
    IF (.NOT.IsLittleEndian) THEN
        ! big-endian so swap IBits(1) and IBits(2)
        BLOCK
            tUInt64 :: Tmp
            Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        END BLOCK
    END IF
    
    ! determine exponent bits
    ExpL = SHIFTR(IBits(2), 48)                 ! 48 = 112-64
    Exp  = IAND(ToInt32(ExpL), Mask) - 16495    ! 16495 = 16383 + 112
    ! convert and add exponent bits
    U128 = UInt128(IOR(IAND(IBits(2), C2(2)), C1(2)), IOR(IAND(IBits(1), C2(1)), C1(1)))
    IF (Exp < 0) THEN
        Exp = -Exp
        ! perform right shift
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128%Low  = SHIFTR(U128%High, Exp - 64)
            U128%High = MinU64
        ELSE
            U128%Low  = IOR(SHIFTR(U128%Low, Exp), SHIFTL(U128%High, 64 - Exp))
            U128%High = SHIFTR(U128%High, Exp)
        END IF
    ELSE
        ! perform left shift
        IF (Exp >= 128) THEN
            U128 = UInt128(MinU64, MinU64)
        ELSEIF (Exp >= 64) THEN
            U128%High = SHIFTL(U128%Low, Exp - 64)
            U128%Low  = MinU64
        ELSE
            U128%High = IOR(SHIFTL(U128%High, Exp), SHIFTR(U128%Low, 64 - Exp))
            U128%Low  = SHIFTL(U128%Low, Exp)
        END IF
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

END FUNCTION R128_To_U128

!******************************************************************************

MODULE FUNCTION DecString_To_U128(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a decimal string to an unsigned 128-bit integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tUInt128                            :: Number   !! unsigned 128-bit integer value

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0           = IACHAR('0')
    tSInt32, PARAMETER  :: A4           = IACHAR('4')
    tSInt32, PARAMETER  :: A9           = IACHAR('9')
    tSInt32, PARAMETER  :: MaxDigitI32  = 10
    tSInt32, PARAMETER  :: MaxDigitI64  = 19
    tSInt32, PARAMETER  :: MaxDigitU128 = 39
    tCharParam          :: MaxStr       = '340282366920938463463374607431768211455'
    tSInt64, PARAMETER  :: Mask32       = MaxU32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen, DigitLen
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart, IndxP7
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow
    tCharAlloc,  TARGET     :: CurStr
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
            Number = MinU128
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
    Number = ZeroU128
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        ! if only zero digits encountered, return
        IF (Indx > StrLen) RETURN
    END IF

    ! compute the length of digits
    DigitLen = StrLen - Indx + 1

    ! return quickly if possible
    IF (DigitLen < MaxDigitI32) THEN
        I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg)
        IF (ErrorFlag) THEN
            Number = MinU128
        ELSE
            Number = UInt128(I32Val)
        END IF
        IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        RETURN
    ELSEIF (DigitLen < MaxDigitI64) THEN
        I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg)
        IF (ErrorFlag) THEN
            Number = MinU128
        ELSE
            Number = UInt128(I64Val)
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
                ! => Number = Number*100000000_kInt64 + Parse8Digits(wPtr)
                CALL MulAddU64(Number, 100000000_kInt64, Parse8Digits(wPtr))
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                ! nullify pointers
                NULLIFY(wPtr)
                cPtr = C_NULL_PTR
                RETURN
            END IF
            Indx   = Indx + 8
            IndxP7 = Indx + 7
        END DO
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! => Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL MulAddU32(Number, 10, (IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
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
        Number = MinU128
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitU128) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitU128) THEN
        ! value might be in the applicable range so check overflow
        CurStr = cStr(IStart:StrLen)
        Overflow = FalseVal
        DO Indx = 1, MaxDigitU128
            CurChr => CurStr(Indx:Indx)
            IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                EXIT
            ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                Overflow = TrueVal
                EXIT
            END IF
        END DO
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
        Number = MaxU128
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
        tUInt64, PARAMETER  :: K2 = ToInt64(Z'01FF00FF00FF00FF')
        tUInt64, PARAMETER  :: K3 = ToInt64(Z'0100FFFF0000FFFF')
        tUInt64, PARAMETER  :: M1 = 2561_kInt64
        tUInt64, PARAMETER  :: M2 = 6553601_kInt64
        tUInt64, PARAMETER  :: M3 = 42949672960001_kInt64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

        RETURN

    END FUNCTION Parse8Digits

    !******************************************************************************

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

    !******************************************************************************

    SUBROUTINE MulAddU64(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To multiply the given number by 'Mul' and then add 'Add' to it.
        !  (i.e. to set  U128 = U128*Mul + Add).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT) :: U128
        tUInt64,  INTENT(IN)    :: Mul
        tUInt64,  INTENT(IN)    :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: X_Lo, Y_Lo, Y_Hi
        tUInt64     :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(Mul, Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinI64) < IEOR(MulLo, MinI64)) U128%High = U128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddU64

    !******************************************************************************

    SUBROUTINE MulAddU32(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To multiply the given number by 'Mul' and then add 'Add' to it.
        !  (i.e. to set  U128 = U128*Mul + Add).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT) :: U128
        tUInt32,  INTENT(IN)    :: Mul
        tUInt32,  INTENT(IN)    :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: X_Lo, Y_Lo, Y_Hi
        tUInt64     :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(ToInt64(Mul), Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinI64) < IEOR(MulLo, MinI64)) U128%High = U128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddU32

    !**************************************************************************

END FUNCTION DecString_To_U128

!------------------------------------------------------------------------------
!
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I32_From_U128(U128) RESULT(I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a signed 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32                 :: I32      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I32 = ToInt32(U128%Low)

    RETURN

END FUNCTION I32_From_U128

!******************************************************************************

MODULE FUNCTION I64_From_U128(U128) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a signed 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt64                 :: I64      !! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I64 = U128%Low

    RETURN

END FUNCTION I64_From_U128

!******************************************************************************

MODULE FUNCTION R32_From_U128(U128) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a 32-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSingle                 :: R32

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64
    tUInt32, PARAMETER  :: TwoPow23 = SHIFTL(1, 23)
    tUInt32, PARAMETER  :: Mask     = ToInt32(Z'010000FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: S, Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    IF (U128%High == 0_kInt64) THEN
        ! convert directly and return quickly
        R32 = U64_To_R32(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    ! Mask out the 24 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 40) THEN
        IBits = IEOR(ToInt32(SHIFTR(U128%High, 40-S)), TwoPow23)
    ELSE
        ! S-40 == additional bits we need
        IBits = IEOR(ToInt32(IOR(SHIFTL(U128%High, S-40), SHIFTR(U128%Low, 104-S))), TwoPow23)
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

    !**************************************************************************

END FUNCTION R32_From_U128

!******************************************************************************

MODULE FUNCTION R64_From_U128(U128) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a 64-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tDouble                 :: R64

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64
    tUInt64, PARAMETER  :: TwoPow52 = SHIFTL(1_kInt64, 52)
    tUInt32, PARAMETER  :: Mask     = ToInt32(Z'010007FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: S
    tSInt64             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    IF (U128%High == 0_kInt64) THEN
        R64 = U64_To_R64(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    ! Mask out the 53 MSBits
    ! Also, the leading bit is implicit so cancel it out to get the significand
    IF (S <= 11) THEN
        IBits = IEOR(SHIFTR(U128%High, 11-S), TwoPow52)
    ELSE
        ! S-11 == additional bits we need
        IBits = IEOR(IOR(SHIFTL(U128%High, S-11), SHIFTR(U128%Low, 75-S)), TwoPow52)
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

    !******************************************************************************

END FUNCTION R64_From_U128

!******************************************************************************

MODULE FUNCTION R128_From_U128(U128) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a 128-bit floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tQuad                   :: R128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tQuad,   PARAMETER  :: TwoPow64     = 2.0_kQuad**64
    tUInt64, PARAMETER  :: TwoPow112(2) = [0_kInt64, 281474976710656_kInt64]  ! SHIFTL(1, 112)
    tUInt32, PARAMETER  :: Mask         = ToInt32(Z'01007FFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32             :: S, Shift
    tSInt64             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

!** FLOW

    IF (U128%High == 0_kInt64) THEN
        R128 = U64_To_R128(U128%Low)
        RETURN
    END IF

    S = LEADZ(U128%High)
    IF (S >= 15) THEN
        R128 = U64_To_R128(U128%Low) + U64_To_R128(U128%High)*TwoPow64
        RETURN
    END IF

    ! Mask out the 113 MSBits
    Shift = 15 - S
    IBits(2) = SHIFTR(U128%High, Shift)
    IBits(1) = IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift))

    ! get the binary exponent
    Exp = ToInt64(IAND(16510-S, Mask))   ! 16510 = 64 + 64 + 16383 - 1

    ! The leading bit is implicit, cancel it out to get the significand
    ! and also add the exponent
    IBits(1) = IEOR(IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift)), TwoPow112(1))
    IBits(2) = IOR(IEOR(SHIFTR(U128%High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))   ! 48 = 112 - 64

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

    !**************************************************************************

END FUNCTION R128_From_U128

!******************************************************************************

MODULE FUNCTION DecString_From_U128(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a decimal string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tCharAlloc              :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: BufLen, Top, I, J
    tCharLen(41)    :: Buffer
    tUInt128        :: Copy
    tSInt64         :: Tmp

!** FLOW

    IF (U128 == ZeroU128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Copy = U128
    DO
        J = Top
        Tmp = DivModBy10Pow18(Copy)
        CALL Write_1_To_18_Digits(Tmp, Buffer, Top)
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 18
        END IF
    END DO
    Str = Buffer(Top+1:BufLen)

    RETURN

    CONTAINS

    SUBROUTINE Write_1_To_18_Digits(Number, cStr, Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an integer number to character string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   !! number
        tCharStar, INTENT(INOUT)    :: cStr     !! character string
        tSInt32,   INTENT(INOUT)    :: Indx     !! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: MaxLen = 19
        tSInt64, PARAMETER  :: Div1E8 = 100000000_kInt64
        ! multiplier and shift for 19 digits and divisor of 1.0E8
        tSInt64, PARAMETER  :: M90 = ToInt64(Z'ABCC77118461CEFD')
        tSInt32, PARAMETER  :: S90 = 90 - 64
        ! multiplier for 11 digits and divisor of 1.0E8
        tSInt64, PARAMETER  :: M64 = ToInt64(Z'0100002AF31DC462')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr
        tSInt64             :: PosNum
        tSInt64             :: NxtNum, RemNum
        tSInt32             :: Start

    !** FLOW

        ! start the conversion
        IF (Number >= 1000000000_kInt64) THEN
            ! compute NxtNum = Number/100000000
            NxtNum = SHIFTR(UMul128_Upper64(Number, M90), S90)
            ! compute RemNum = MOD(Number, 100000000)
            RemNum = Number - NxtNum*Div1E8
            ! convert the remainder to a working string
            CALL Write_8_Digits(ToInt32(RemNum), wStr(12:19))

            PosNum = NxtNum
            IF (PosNum > Div1E8) THEN
                ! compute NxtNum = PosNum/100000000
                NxtNum = UMul128_Upper64(PosNum, M64)
                ! compute RemNum = MOD(PosNum, 100000000)
                RemNum = PosNum - NxtNum*Div1E8
                ! convert the remainder to a working string
                CALL Write_8_Digits(ToInt32(RemNum), wStr(4:11))

                ! convert the rest
                IF (NxtNum < 10) THEN
                    wStr(3:3) = Char2Digits(NxtNum)(2:2)
                    Start = 3
                ELSEIF (NxtNum < 100) THEN
                    wStr(2:3) = Char2Digits(NxtNum)
                    Start = 2
                ELSE
                    wStr(1:3) = Char4Digits(NxtNum)(2:4)
                    Start = 1
                END IF
            ELSE
                ! convert the rest
                Start = 3 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(4:11))
            END IF
            ! transfer to output string
            DO I = MaxLen, Start, -1
                cStr(Indx:Indx) = wStr(I:I)
                Indx = Indx - 1
            END DO
        ELSE
            CALL Write_1_To_9_Digits(ToInt32(Number), cStr, Indx)
        END IF

        RETURN

    END SUBROUTINE Write_1_To_18_Digits

    !******************************************************************************

    SUBROUTINE Write_1_To_9_Digits(Number, cStr, Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an integer number to character string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   !! number
        tCharStar, INTENT(INOUT)    :: cStr     !! character string
        tSInt32,   INTENT(INOUT)    :: Indx     !! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER :: MaxLen = 10
        tSInt32, PARAMETER :: ShiftPos = 45
        tSInt64, PARAMETER :: Multiplier = ToInt64(Z'01000000D1B71759')
        tSInt32, PARAMETER :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr     ! working string
        tSInt32             :: PosNum   ! positive number (working number)
        tSInt32             :: NxtNum   ! next round of positive number
        tSInt32             :: RemNum   ! remainder number
        tSInt32             :: Start, Finish

    !** FLOW

        ! start the conversion
        IF (Number >= 10000) THEN
            ! compute the next round of working number
            NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))   ! NxtNum = Number/10000
            ! compute the remainder
            RemNum = Number - NxtNum*Divisor                        ! RemNum = MOD(Number, 10000)
            ! convert the remainder to a working string
            wStr(7:10) = Char4Digits(RemNum)
            Finish = 10
            PosNum = NxtNum
            IF (PosNum < 10000) THEN
                IF (PosNum < 100) THEN
                    wStr(5:6) = Char2Digits(PosNum)
                    Start  = 5
                    IF (wStr(Start:Start) == '0') Start = 6
                ELSE
                    wStr(3:6) = Char4Digits(PosNum)
                    Start  = 3
                    IF (wStr(Start:Start) == '0') Start = 4
                END IF
            ELSE
                ! compute the next round of working number
                NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))   ! NxtNum = PosNum/10000
                ! compute the remainder
                RemNum = PosNum - NxtNum*Divisor                        ! RemNum = MOD(PosNum, 10000)
                ! convert the remainder to a working string
                wStr(3:6) = Char4Digits(RemNum)
                IF (NxtNum > 0) THEN
                    IF (NxtNum < 10) THEN
                        wStr(2:2) = Char2Digits(NxtNum)(2:2)
                        Start = 2
                    ELSE
                        wStr(1:2) = Char2Digits(NxtNum)
                        Start = 1
                    END IF
                ELSE
                    Start = 3
                END IF
            END IF
        ELSE
            Start  = 1
            IF (Number < 100) THEN
                wStr(1:2) = Char2Digits(Number)
                Finish = 2
                IF (wStr(Start:Start) == '0') Start = 2
            ELSE
                wStr(1:4) = Char4Digits(Number)
                Finish = 4
                IF (wStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        ! transfer to output string
        DO I = Finish, Start, -1
            cStr(Indx:Indx) = wStr(I:I)
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE Write_1_To_9_Digits

    !******************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To write an (unsigned) integer number with a length of 8.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   !! number
        tCharStar, INTENT(INOUT)    :: cStr     !! character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'01000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW

        ! compute NxtNum = PosNum/10000
        NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
        ! convert the rest
        cStr(1:4) = Char4Digits(NxtNum)

        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To write an (unsigned) integer number with a length of 1 to 8.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   !! number
        tCharStar, INTENT(INOUT)    :: cStr     !! character string
        tSInt32                     :: Start

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'01000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW

        IF (Number < 10000) THEN
            IF (Number < 100) THEN
                cStr(7:8) = Char2Digits(Number)
                Start = 7
                IF (cStr(Start:Start) == '0') Start = 8
            ELSE
                cStr(5:8) = Char4Digits(Number)
                Start = 5
                IF (cStr(Start:Start) == '0') Start = 6
            END IF
        ELSE
            ! compute NxtNum = Number/10000
            NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))
            ! compute RemNum = MOD(Number, 10000)
            RemNum = Number - NxtNum*Divisor
            ! convert the remainder to a working string
            cStr(5:8) = Char4Digits(RemNum)
            IF (NxtNum < 100) THEN
                cStr(3:4) = Char2Digits(NxtNum)
                Start = 3
                IF (cStr(Start:Start) == '0') Start = 4
            ELSE
                cStr(1:4) = Char4Digits(NxtNum)
                Start  = 1
                IF (cStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION DivModBy10Pow18(DivQuot) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform division DivQuot / Divisor where the Divisor is equal to 10**18.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        !> on entry, the dividend <br>
        !  on exit, the quotient
        tUInt128, INTENT(INOUT) :: DivQuot
        !% the remainder
        tUInt64                 :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32, PARAMETER  :: LSh   = 4                            ! = LEADZ(Divisor)
        tUInt64, PARAMETER  :: Denom = ToInt64(Z'DE0B6B3A76400000') ! = SHIFTL(Divisor, LSh)
        tUInt64, PARAMETER  :: V     = ToInt64(Z'2725DD1D243ABA0E') ! = Reciprocal_2By1(Denom)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: RSh
        tUInt64         :: NumerHi, NumerLo, DenomHi, DenomLo
        tUInt64         :: NumerEx, RshMask
        tUInt64         :: R1, R2, LHS, RHS

    !** FLOW

        IF (DivQuot%High == 0_kInt64) THEN
            IF ((DivQuot%Low > 0_kInt64).AND.(DivQuot%Low < 1000000000000000000_kInt64)) THEN
                Remainder   = DivQuot%Low
                DivQuot%Low = 0_kInt64
                RETURN
            END IF
        END IF
        RSh = 64 - LSh
        RShMask = -1_kInt64
        NumerLo = SHIFTL(DivQuot%Low, LSh)
        NumerHi = IOR(SHIFTL(DivQuot%High, LSh), IAND(SHIFTR(DivQuot%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(DivQuot%High, RSh), RShMask)

        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, DivQuot%High, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, DivQuot%Low, R2)
        Remainder = SHIFTR(R2, LSh)

        RETURN

    END FUNCTION DivModBy10Pow18

    !**************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform 128-bit integer division by 64-bit integer.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: UHi, ULo, D, V
        tUInt64, INTENT(OUT)    :: Q, R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt64, PARAMETER  :: MinInt64 = ToInt64(Z'8000000000000000')   ! -9223372036854775808

   !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: QHi, QLo, NewLo

    !** FLOW

        ! Q128 = V*UHi
        CALL UMul128(V, UHi, QHi, QLo)

        ! Q128 = Q128 + U128
        NewLo = QLo + ULo
        IF (IEOR(NewLo, MinInt64) < IEOR(QLo, MinInt64)) THEN
            QHi = QHi + UHi + 1_kInt64
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_kInt64

        R = ULo - QHi*D

        IF (IEOR(R, MinInt64) > IEOR(QLo, MinInt64)) THEN
            QHi = QHi - 1_kInt64
            R = R + D
        END IF

        IF (IEOR(R, MinInt64) >= IEOR(D, MinInt64)) THEN
            QHi = QHi + 1_kInt64
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !**************************************************************************

END FUNCTION DecString_From_U128

!******************************************************************************

MODULE FUNCTION HexString_From_U128(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an unsigned 128-bit integer number to a hexadecimal string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tCharAlloc              :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !  na

!** FLOW

    ! for hexadecimal, division and mod of UInt128 is not needed so it is simpler and faster
    IF (U128%High /= 0_kInt64) THEN
        BLOCK
            tCharAlloc      :: LowHex
            tCharLen(16)    :: LowStr
            LowStr = '0100000000000000'
            LowHex = U64_ToHexStr(U128%Low)
            IF (LEN(LowHex) > 0) LowStr(17-LEN(LowHex):16) = LowHex
            Str = U64_ToHexStr(U128%High) // LowStr
        END BLOCK
    ELSE
        Str = U64_ToHexStr(U128%Low)
    END IF

    RETURN

CONTAINS

    FUNCTION I64_ToHexStr(Number) RESULT(cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an integer number to a hexadecimal string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, INTENT(IN) :: Number   ! number
        tCharAlloc          :: cStr     ! hexadecimal string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: MaxLen = 16
        tSInt64, PARAMETER  :: Base   = 16_kInt64
        tSInt32, PARAMETER  :: Shift  = 4
        tChar,   PARAMETER  :: NumStr(0:15) = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                               '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr     ! working string
        tSInt64             :: PosNum   ! positive number (working number)
        tSInt64             :: CurNum   ! current (saved) working number
        tSInt64             :: RemNum   ! remainder number
        tSInt32             :: Indx

    !** FLOW

        ! check whether the number is zero
        IF (Number == 0_kInt64) THEN
            cStr = '0'
            RETURN
        END IF
        IF (Number < 0_kInt64) THEN
            IF (Number == MinI64) THEN
                cStr = '-8000000000000000'
                RETURN
            END IF
            PosNum = ABS(Number)
        ELSE
            PosNum = Number
        END IF
        Indx = MaxLen

        ! start the conversion
        DO
            ! save current number
            CurNum = PosNum
            ! compute the next round of working number
            PosNum = SHIFTR(PosNum, Shift)
            ! compute the remainder
            RemNum = CurNum - SHIFTL(PosNum, Shift)
            ! convert the remainder to a working string
            wStr(Indx:Indx) = NumStr(RemNum)
            Indx = Indx - 1
            IF (PosNum == 0_kInt64) EXIT
        END DO

        ! allocate the resulting string and transfer
        ! characters from the working string
        Indx = Indx + 1
        IF (Number < 0_kInt64) THEN
            cStr = '-' // wStr(Indx:MaxLen)
        ELSE
            cStr = wStr(Indx:MaxLen)
        END IF

        RETURN

    END FUNCTION I64_ToHexStr

    !******************************************************************************

    FUNCTION U64_ToHexStr(Number) RESULT(cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To convert an unsigned integer number to a hexadecimal string.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: Number   ! number treated as unsigned one
        tCharAlloc          :: cStr     ! hexadecimal string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: Shift = 4

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt64     :: Quotient, Remainder
        tCharAlloc  :: QuotStr,  RemStr

    !** FLOW

        IF (Number >= 0_kInt64) THEN
            cStr = I64_ToHexStr(Number)
        ELSE
            Quotient  = SHIFTR(Number, Shift)
            Remainder = Number - SHIFTL(Quotient, Shift)
            QuotStr = I64_ToHexStr(Quotient)
            RemStr  = I64_ToHexStr(Remainder)
            cStr    = QuotStr // RemStr
        END IF

        RETURN

    END FUNCTION U64_ToHexStr

    !**************************************************************************

END FUNCTION HexString_From_U128

!******************************************************************************

END SUBMODULE SubBase_U128_Conversion

!******************************************************************************

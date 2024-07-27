
SUBMODULE (Class_ApInt64 : SubClass_Api64_Auxiliary) SubClass_Api64_Constructor

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.
!   In particular, these routines are used in a structure constructor to convert
!   from other data types to the *ApInt64* type.

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ISO_C_BINDING
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_Constructor'
    tLong,     PARAMETER    :: MinI64   = ToLong(Z'8000000000000000')   ! min signed 64-bit

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION Construct_ApInt64(Sign, Length, Digit) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 64-bit integer
    !  magnitude array and related data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% sign of the number
    tInteger, INTENT(IN)    :: Sign
    !% length of the magnitude array
    tIndex,   INTENT(IN)    :: Length
    !> the magnitude of the number given as a 64-bit integer array where
    !  the first element gives the least significant 64 bits (i.e. little
    !  endian order)
    tLong,    INTENT(IN)    :: Digit(0:Length-1)
    !% the arbitrary-precision integer number
    TYPE(ApInt64)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Big%Sign = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length = Length
    IF (Big%Length < 1) THEN
        CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
        Big%Length = 1_kIndex
        Big%Digit(0) = 0_kLong
    ELSE
        CALL MemAlloc(Big%Digit, Length, StartID=0_kIndex)
        Big%Digit = Digit
    END IF

    RETURN

END FUNCTION Construct_ApInt64

!******************************************************************************

MODULE FUNCTION Bytes_To_ApInt64(Sign, Length, Digit) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 8-bit integer
    !  magnitude array and related data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% sign of the number
    tInteger, INTENT(IN)    :: Sign
    !% length of the magnitude array
    tIndex,   INTENT(IN)    :: Length
    !> the magnitude of the number given as a 8-bit integer array where
    !  the first element gives the least significant 8 bits (i.e. little
    !  endian order)
    tByte,    INTENT(IN)    :: Digit(0:Length-1)
    !% the arbitrary-precision integer number
    TYPE(ApInt64)           :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskU8 = ToLong(Z'00000000000000FF')

!** SUBROUTINE MACRO DECLARATIONS:
#define MaskLong(X)     IAND(ToLong(X), MaskU8)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: BLen, DLen, I, Offset, Remaining

!** FLOW

    BLen = Length
    IF (BLen == 0_kIndex) THEN
        Big = ZeroApInt64()
        RETURN
    END IF
    DO
        IF (BLen == 1_kIndex) EXIT
        IF (Digit(BLen-1_kIndex) /= 0_kLong) EXIT
        BLen = BLen - 1_kIndex
    END DO
    Big%Sign = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length = (BLen + 7_kIndex)/8_kIndex
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)
    DLen = BLen/8_kIndex
    Offset = 0_kIndex
    DO I = 0_kIndex, DLen-1_kIndex
        Big%Digit(I) = PackFull(Digit, Offset)
        Offset = Offset + 8_kIndex
    END DO
    IF (DLen /= Big%Length) THEN
        Remaining = BLen - Offset
        IF (Remaining > 0_kIndex) THEN
            Big%Digit(Big%Length-1) = PackPartial(Digit, Offset, Remaining)
        ELSE
            Big%Length = Big%Length - 1_kIndex
        END IF
    END IF

    RETURN

    CONTAINS

    PURE FUNCTION PackFull(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
        !  in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  !! buffer
        tIndex, INTENT(IN)  :: Off      !! offset
        tLong               :: Res      !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        ! implementation algorithm #1
        Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(       MaskLong(Buf(Off)),          &
                                          SHIFTL(MaskLong(Buf(Off+1)),  8)),  &
                                          SHIFTL(MaskLong(Buf(Off+2)), 16)),  &
                                          SHIFTL(MaskLong(Buf(Off+3)), 24)),  &
                                          SHIFTL(MaskLong(Buf(Off+4)), 32)),  &
                                          SHIFTL(MaskLong(Buf(Off+5)), 40)),  &
                                          SHIFTL(MaskLong(Buf(Off+6)), 48)),  &
                                          SHIFTL(MaskLong(Buf(Off+7)), 56))

        RETURN

    END FUNCTION PackFull

    !**************************************************************************

    PURE FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        !  into the 64-bit word 'Res', in little-endian convention
        !  (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  !! buffer
        tIndex, INTENT(IN)  :: Off      !! offset
        tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 7)
        tLong               :: Res      !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:7)
        tIndex      :: I

    ! FLOW

        ! initialize
        Wrk = 0_kByte

        ! gather available bytes
        DO I = 0, Length-1
            Wrk(I) = Buf(Off+I)
        END DO

        ! pack bytes into word
        Res = PackFull(Wrk, 0)

        RETURN

    END FUNCTION PackPartial

    !**************************************************************************

#undef MaskLong

END FUNCTION Bytes_To_ApInt64

!******************************************************************************

MODULE FUNCTION DecString_To_ApInt64(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified decimal string.

!** TECHNICAL NOTES:
    ! Parse 18 digits at a time using an 8-digit parse algorithm.
    ! Use specialized 'Parse18Digits' routine
    ! Use specialized 'MulAdd' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    TYPE(ApInt64)                       :: Big      !! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP18
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
!            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
!            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP18 = MOD(DigitLen, 18)
    IF (IndxP18 == 0) THEN
        IndxP18 = Indx + 18
    ELSE
        IndxP18 = Indx + IndxP18
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP18, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx = IndxP18
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse_18Digits(Indx, I64Val) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Mul_TenPow18_N_Add(Big, I64Val)
        Indx = Indx + 18
    END DO

    RETURN

CONTAINS

    FUNCTION Parse_18Digits(IStart, Val) RESULT(IErr)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To parse a part of a character string as an unsigned decimal number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)   :: IStart   !! the starting index (inclusive)
        tLong,     INTENT(OUT)  :: Val      !! the parse number
        tInteger                :: IErr     !! 0 - no error; 1 = error

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx
        tCharLen(1), POINTER    :: CurChr
        TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
        tUInt64,    POINTER     :: wPtr     ! Fortran pointer to wStr
        tCharLen(8), TARGET     :: wStr

    !** FLOW

        ! initialize
        IErr = 0
        Indx = IStart

        ! get a C pointer to IBits
        cPtr = C_LOC(wStr)
        ! associate a Fortran data pointer with the C pointer
        CALL C_F_POINTER(cPtr, wPtr)

        Main: BLOCK
            ! process first 8 digits at once
            wStr = cStr(Indx:Indx+7)
            IF (.NOT.Is8Digits(wPtr)) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Parse8Digits(wPtr)
            Indx = Indx + 8

            ! process next 8 digits at once
            wStr = cStr(Indx:Indx+7)
            IF (.NOT.Is8Digits(wPtr)) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*100000000_kLong + Parse8Digits(wPtr)
            Indx = Indx + 8

            ! process the 17th digit
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*Base + ToLong(IACHAR(CurChr) - A0)
            Indx = Indx + 1

            ! process the last digit
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*Base + ToLong(IACHAR(CurChr) - A0)
        END BLOCK Main

        ! check if error occurred
        IF (IErr == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Val = 0_kLong
        END IF

        ! nullify pointers
        NULLIFY(wPtr)
        cPtr = C_NULL_PTR

        RETURN

    END FUNCTION Parse_18Digits

    !**************************************************************************

END FUNCTION DecString_To_ApInt64

!******************************************************************************

MODULE FUNCTION U32_To_ApInt64(Sign, U32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 32-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign     !! the sign of the number
    tInteger, INTENT(IN)    :: U32      !! the magnitude of the number treated as unsigned
    TYPE(ApInt64)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U32)

    RETURN

END FUNCTION U32_To_ApInt64

!******************************************************************************

MODULE FUNCTION U64_To_ApInt64(Sign, U64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 64-bit unsigned integer. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign     !! the sign of the number
    tLong,    INTENT(IN)    :: U64      !! the magnitude of the number treated as unsigned
    TYPE(ApInt64)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U64)

    RETURN

END FUNCTION U64_To_ApInt64

!******************************************************************************

MODULE FUNCTION U128_To_ApInt64(Sign, U128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 128-bit unsigned integer. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: Sign     !! the sign of the number
    TYPE(UInt128), INTENT(IN)   :: U128     !! the magnitude of the number treated as unsigned
    TYPE(ApInt64)               :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U128)

    RETURN

END FUNCTION U128_To_ApInt64

!******************************************************************************

MODULE FUNCTION I32_To_ApInt64(I32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 32-bit signed integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32      !! the 32-bit signed integer
    TYPE(ApInt64)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I32)

    RETURN

END FUNCTION I32_To_ApInt64

!******************************************************************************

MODULE FUNCTION I64_To_ApInt64(I64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 64-bit signed integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64      !! the 64-bit signed integer
    TYPE(ApInt64)       :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I64)

    RETURN

END FUNCTION I64_To_ApInt64

!******************************************************************************

MODULE FUNCTION I128_To_ApInt64(I128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 128-bit signed integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128     !! the 128-bit signed integer
    TYPE(ApInt64)               :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I128)

    RETURN

END FUNCTION I128_To_ApInt64

!******************************************************************************

MODULE FUNCTION R32_To_ApInt64(R32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 32-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle, INTENT(IN) :: R32      !! the 32-bit real number
    TYPE(ApInt64)       :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Mask = ToInteger(Z'000000FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    ! undefined behavior if R32 is NaN.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL Handle_ErrLevel('R32_To_ApInt64', ModName, ErrSevere, &
                          'Undefined behavior: R32 is NOT finite.')
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

    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    IBits = IOR(IAND(IBits, SHIFTL(1_kInteger, 23) - 1_kInteger), SHIFTL(1_kInteger, 23))
    Big = IBits
    Big = ISHFT(Big, Exp)

    IF (R32 > 0.0_kSingle) THEN
        Big%Sign = 1
    ELSE
        Big%Sign = -1
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION R32_To_ApInt64

!******************************************************************************

MODULE FUNCTION R64_To_ApInt64(R64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 64-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: R64      !! the 64-bit real number
    TYPE(ApInt64)       :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: Mask = ToLong(Z'00000000000007FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! undefined behavior if R64 is NaN.
    IF (.NOT.IEEE_IS_FINITE (R64)) THEN
        CALL Handle_ErrLevel('R64_To_ApInt64', ModName, ErrSevere, &
                          'Undefined behavior: R64 is NOT finite.')
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

    Exp = ToInteger(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
    IBits = IOR(IAND(IBits, SHIFTL(1_kLong, 52) - 1_kLong), SHIFTL(1_kLong, 52))
    Big = IBits
    Big = ISHFT(Big, Exp)

    IF (R64 > 0.0_kDouble) THEN
        Big%Sign = 1
    ELSE
        Big%Sign = -1
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION R64_To_ApInt64

!******************************************************************************

MODULE FUNCTION R128_To_ApInt64(R128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct the *ApInt64* object based on the specified 128-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad, INTENT(IN)   :: R128     !! the 128-bit real number
    TYPE(ApInt64)       :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Mask = ToInteger(Z'00007FFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Exp
    TYPE(UInt128)       :: IBits
    TYPE(UInt128)       :: TwoPow112
    tIndex              :: Length
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: LBits(2)

!** FLOW

    ! undefined behavior if R128 is NaN.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL Handle_ErrLevel('R128_To_ApInt64', ModName, ErrSevere, &
                          'Undefined behavior: R128 is NOT finite.')
        RETURN
    END IF

    !---------------------------------------------------------------------!
    !+++++  Transfer input (IBits mapped to R128 using C_F_POINTER)  +++++!
    !---------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(LBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! set a 128-bit integer number equivalent to a 128-bit real number
    fPtr = R128
    IF (IsLittleEndian) THEN
       IBits = UInt128(LBits(2), LBits(1))
    ELSE
       IBits = UInt128(LBits(1), LBits(2))
    END IF

    Exp = ShiftR64Up(IBits, 112)
    Exp = IAND(Exp, Mask) - 16495       ! 16495 = 16383 + 112
    TwoPow112 = ShiftL64Up(OneU128, 112)
    IBits = IOR(IAND(IBits, TwoPow112 - OneU128), TwoPow112)
    LBits(1) = IBits%Low
    LBits(2) = IBits%High
    IF (LBits(2) /= 0) THEN
        Length = 2_kIndex
        Big%Sign = 1
        Big%Length = Length
        CALL MemAlloc(Big%Digit, Length+1_kIndex, StartID=0_kIndex)
        Big%Digit(0:1) = LBits
    ELSE
        Length = 1_kIndex
        Big%Sign = 1
        Big%Length = Length
        CALL MemAlloc(Big%Digit, Length+1_kIndex, StartID=0_kIndex)
        Big%Digit(0:0) = LBits(1:1)
    END IF

    Big = ISHFT(Big, Exp)

    IF (R128 > 0.0_kQuad) THEN
        Big%Sign = 1
    ELSE
        Big%Sign = -1
    END IF

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION R128_To_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Parse18Digits(cStr, IStart, Val, ErrFlag, ErrMsg) RESULT(IErr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse a part of a character string as an unsigned decimal number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tInteger,             INTENT(IN)    :: IStart   ! the starting index (inclusive)
    tLong,                INTENT(OUT)   :: Val      ! the parse number
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tInteger                            :: IErr     ! 0 - no error, 1 = error

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP7, IEnd
!    tCharLen(8)             :: wStr
!    tLong                   :: wVal
!    EQUIVALENCE(wStr, wVal)
    TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
    tUInt64,    POINTER     :: wPtr     ! Fortran pointer to wStr
    tCharLen(8), TARGET     :: wStr

!** FLOW

    ! get a C pointer to IBits
    cPtr = C_LOC(wStr)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, wPtr)

    ! initialize
    IErr = 0
    Indx = IStart
    IndxP7 = Indx + 7

    Main: BLOCK
        ! process first 8 digits at once
        wStr = cStr(Indx:IndxP7)
        IF (Is8Digits(wPtr)) THEN
            Val = Parse8Digits(wPtr)
        ELSE
            IErr = 1
            EXIT Main
        END IF
        Indx = Indx + 8
        IndxP7 = Indx + 7
        ! process next 8 digits at once
        wStr = cStr(Indx:IndxP7)
        IF (Is8Digits(wPtr)) THEN
            Val = Val*100000000_kLong + Parse8Digits(wPtr)
        ELSE
            IErr = 1
            EXIT Main
        END IF
        Indx = Indx + 8
        IEnd = Indx + 2
        ! process last 2 digits at once
        IErr = ParseDigits(cStr, Indx, IEnd, wPtr, ErrFlag, ErrMsg)
        Val = Val*100_kLong + wPtr
    END BLOCK Main

    IF (IErr == 1) THEN
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Val = 0_kLong
    END IF

    ! nullify pointers
    NULLIFY(wPtr)
    cPtr = C_NULL_PTR

    RETURN

END FUNCTION Parse18Digits

!******************************************************************************

FUNCTION ParseDigits(cStr, IStart, IEnd, Val, ErrFlag, ErrMsg) RESULT(IErr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse a part of a character string as an unsigned decimal number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tInteger,             INTENT(IN)    :: IStart   ! the starting index (inclusive)
    tInteger,             INTENT(IN)    :: IEnd     ! the ending index (exclusive)
    tLong,                INTENT(OUT)   :: Val      ! the parse number
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tInteger                            :: IErr     ! 0 - no error, 1 = error

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx
    tCharLen(1), POINTER    :: CurChr

!** FLOW

    ! initialize
    IErr = 0
    Val  = 0_kLong
    Indx = IStart
    DO WHILE (Indx < IEnd)
        CurChr => cStr(Indx:Indx)
        ! check whether current character is valid or not
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IErr = 1
            RETURN
        END IF
        ! update value and index
        Val = Val*Base + ToLong(IACHAR(CurChr) - A0)
        Indx = Indx + 1
    END DO

    RETURN

END FUNCTION ParseDigits

!******************************************************************************

FUNCTION Parse8Digits(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse eight digits immediately.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: InVal
    tLong               :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: K1 = ToLong(Z'0F0F0F0F0F0F0F0F')
    tLong, PARAMETER    :: K2 = ToLong(Z'00FF00FF00FF00FF')
    tLong, PARAMETER    :: K3 = ToLong(Z'0000FFFF0000FFFF')
    tLong, PARAMETER    :: M1 = 2561_kLong
    tLong, PARAMETER    :: M2 = 6553601_kLong
    tLong, PARAMETER    :: M3 = 42949672960001_kLong

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

    RETURN

END FUNCTION Parse8Digits

!******************************************************************************

FUNCTION Is8Digits(InVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether we can process eight digits immediately

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: InVal
    tLogical            :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: C1 = ToLong(Z'F0F0F0F0F0F0F0F0')
    tLong, PARAMETER    :: C2 = ToLong(Z'3333333333333333')
    tLong, PARAMETER    :: C3 = ToLong(Z'0606060606060606')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2

    RETURN

END FUNCTION Is8Digits

!******************************************************************************

SUBROUTINE Mul_TenPow18_N_Add(Big, Add)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply 10**18 and then add 'Add' to it.
    ! (i.e. to set  Big = Big*(10**18) + Add)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: Add      ! Add < 2**63

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, PARAMETER    :: Mask32 = ToLong(Z'00000000FFFFFFFF')
    tLong, PARAMETER    :: Multiplier = 1000000000000000000_kLong
    tLong, PARAMETER    :: Y_Lo = IAND(Multiplier, Mask32)
    tLong, PARAMETER    :: Y_Hi = SHIFTR(Multiplier, 32)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: ProdLo, Carry
    tIndex      :: I
    tLong       :: X_Lo, X_Hi
    tLong       :: Lo_Lo, Hi_Lo, Cross

!** FLOW

    Carry = 0_kLong
    DO I = 0, Big%Length-1
        X_Lo = IAND(Big%Digit(I), Mask32)
        X_Hi = SHIFTR(Big%Digit(I), 32)
        Lo_Lo = X_Lo*Y_Lo
        Hi_Lo = X_Hi*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
        ProdLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        Big%Digit(I) = ProdLo + Carry
        Carry = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
        IF (IEOR(Big%Digit(I), MinI64) < IEOR(ProdLo, MinI64)) Carry = Carry + 1_kLong
    END DO
    IF (Carry /= 0_kLong) THEN
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1_kIndex
    END IF
    Big%Digit(0) = Big%Digit(0) + Add
    IF (IEOR(Big%Digit(0), MinI64) < IEOR(Add, MinI64)) THEN
        I = 1_kIndex
        DO
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0_kLong))) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE Mul_TenPow18_N_Add

!******************************************************************************

SUBROUTINE Multiply_N_Add(Big, Mul, Add)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the given number by 'Mul' and then add 'Add' to it.
    ! (i.e. to set  Big = Big*Mul + Add)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: Mul      ! Mul < 2**63
    tLong,         INTENT(IN)       :: Add      ! Add < 2**63

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: High, Low, Carry
    tIndex      :: I

!** FLOW

    Carry = 0_kLong
    DO I = 0, Big%Length-1
        CALL UMul128(Big%Digit(I), Mul, High, Low)
        Big%Digit(I) = Low + Carry
        IF (IEOR(Big%Digit(I), MinI64) < IEOR(Low, MinI64)) THEN
            Carry = High + 1_kLong
        ELSE
            Carry = High
        END IF
    END DO
    IF (Carry /= 0_kLong) THEN
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1_kIndex
    END IF
    Big%Digit(0) = Big%Digit(0) + Add
    IF (IEOR(Big%Digit(0), MinI64) < IEOR(Add, MinI64)) THEN
        I = 1_kIndex
        DO
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0_kLong))) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE Multiply_N_Add

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                   ALTERNATIVE ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION FromString_P18Simple(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** TECHNICAL NOTES:
    ! Parse 18 digits at a time using simple parse algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP18
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP18 = MOD(DigitLen, 18)
    IF (IndxP18 == 0) THEN
        IndxP18 = Indx + 18
    ELSE
        IndxP18 = Indx + IndxP18
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP18, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx   = IndxP18
    IndxP18 = Indx + 18
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (ParseDigits(cStr, Indx, IndxP18, I64Val, ErrFlag, ErrMsg) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Multiply_N_Add(Big, 1000000000000000000_kLong, I64Val)
        Indx   = IndxP18
        IndxP18 = Indx + 18
    END DO

    RETURN

END FUNCTION FromString_P18Simple

!******************************************************************************

FUNCTION FromString_P16P8(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** TECHNICAL NOTES:
    ! Parse 16 digits at a time using an 8-digit parse algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP16
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP16 = MOD(DigitLen, 16)
    IF (IndxP16 == 0) THEN
        IndxP16 = Indx + 16
    ELSE
        IndxP16 = Indx + IndxP16
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP16, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx = IndxP16
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse16Digits(Indx, I64Val) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Multiply_N_Add(Big, 10000000000000000_kLong, I64Val)
        Indx = Indx + 16
    END DO

    RETURN

CONTAINS

    FUNCTION Parse16Digits(IStart, Val) RESULT(IErr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse a part of a character string as an unsigned decimal number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)   :: IStart   ! the starting index (inclusive)
        tLong,     INTENT(OUT)  :: Val      ! the parse number
        tInteger                :: IErr     ! 0 - no error, 1 = error

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx, IndxP7
        tCharLen(1), POINTER    :: CurChr
        TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
        tUInt64,    POINTER     :: wPtr     ! Fortran pointer to wStr
        tCharLen(8), TARGET     :: wStr

    !** FLOW

        ! get a C pointer to IBits
        cPtr = C_LOC(wStr)
        ! associate a Fortran data pointer with the C pointer
        CALL C_F_POINTER(cPtr, wPtr)

        ! initialize
        IErr = 0
        Indx = IStart
        IndxP7 = Indx + 7

        ! process first 8 digits at once
        wStr = cStr(Indx:IndxP7)
        IF (Is8Digits(wPtr)) THEN
            Val = Parse8Digits(wPtr)
            Indx = Indx + 8
            IndxP7 = Indx + 7
            ! process next 8 digits at once
            wStr = cStr(Indx:IndxP7)
            IF (Is8Digits(wPtr)) THEN
                Val = Val*100000000_kLong + Parse8Digits(wPtr)
            ELSE
                IErr = 1
            END IF
        ELSE
            IErr = 1
        END IF

        IF (IErr == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Val = 0_kLong
        END IF

        ! nullify pointers
        NULLIFY(wPtr)
        cPtr = C_NULL_PTR

        RETURN

    END FUNCTION Parse16Digits

    !**************************************************************************

END FUNCTION FromString_P16P8

!******************************************************************************

FUNCTION FromString_P18P8(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** TECHNICAL NOTES:
    ! Parse 18 digits at a time using an 8-digit parse algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP18
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP18 = MOD(DigitLen, 18)
    IF (IndxP18 == 0) THEN
        IndxP18 = Indx + 18
    ELSE
        IndxP18 = Indx + IndxP18
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP18, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx = IndxP18
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse18Digits(cStr, Indx, I64Val, ErrFlag, ErrMsg) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Multiply_N_Add(Big, 1000000000000000000_kLong, I64Val)
        Indx = Indx + 18
    END DO

    RETURN

END FUNCTION FromString_P18P8

!******************************************************************************

FUNCTION FromString_P18MulSpecial(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** TECHNICAL NOTES:
    ! Parse 18 digits at a time using an 8-digit parse algorithm.
    ! Use specialized 'MulAdd' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP18
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP18 = MOD(DigitLen, 18)
    IF (IndxP18 == 0) THEN
        IndxP18 = Indx + 18
    ELSE
        IndxP18 = Indx + IndxP18
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP18, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx = IndxP18
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse18Digits(cStr, Indx, I64Val, ErrFlag, ErrMsg) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Mul_TenPow18_N_Add(Big, I64Val)
        Indx = Indx + 18
    END DO

    RETURN

END FUNCTION FromString_P18MulSpecial

!******************************************************************************

FUNCTION FromString_P18Special(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** TECHNICAL NOTES:
    ! Parse 18 digits at a time using an 8-digit parse algorithm.
    ! Use specialized 'Parse18Digits' routine
    ! Use specialized 'MulAdd' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tLong,    PARAMETER :: Base = 10_kLong
    tInteger, PARAMETER :: MaxDigitI32 = 10
    tInteger, PARAMETER :: MaxDigitI64 = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP18
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tLong                   :: I64Val

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
            RETURN
        END IF
    END IF

    ! check for sign
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Big%Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
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
            Big = ZeroApInt64()
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < MaxDigitI64) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 64_kIndex, 6)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < MaxDigitI64) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            IF (DigitLen < MaxDigitI32) THEN
                Big%Digit(0) = ToLong(I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg))
            ELSE
                Big%Digit(0) = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            END IF
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP18 = MOD(DigitLen, 18)
    IF (IndxP18 == 0) THEN
        IndxP18 = Indx + 18
    ELSE
        IndxP18 = Indx + IndxP18
    END IF
    ! get value for the first digit
    IF (ParseDigits(cStr, Indx, IndxP18, Big%Digit(0), ErrFlag, ErrMsg) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx = IndxP18
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse_18Digits(Indx, I64Val) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Mul_TenPow18_N_Add(Big, I64Val)
        Indx = Indx + 18
    END DO

    RETURN

CONTAINS

    FUNCTION Parse_18Digits(IStart, Val) RESULT(IErr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse a part of a character string as an unsigned decimal number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)   :: IStart   ! the starting index (inclusive)
        tLong,     INTENT(OUT)  :: Val      ! the parse number
        tInteger                :: IErr     ! 0 - no error, 1 = error

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx
        tCharLen(1), POINTER    :: CurChr
        TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
        tUInt64,    POINTER     :: wPtr     ! Fortran pointer to wStr
        tCharLen(8), TARGET     :: wStr

    !** FLOW

        ! get a C pointer to IBits
        cPtr = C_LOC(wStr)
        ! associate a Fortran data pointer with the C pointer
        CALL C_F_POINTER(cPtr, wPtr)

        ! initialize
        IErr = 0
        Indx = IStart

        Main: BLOCK
            ! process first 8 digits at once
            wStr = cStr(Indx:Indx+7)
            IF (.NOT.Is8Digits(wPtr)) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Parse8Digits(wPtr)
            Indx = Indx + 8

            ! process next 8 digits at once
            wStr = cStr(Indx:Indx+7)
            IF (.NOT.Is8Digits(wPtr)) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*100000000_kLong + Parse8Digits(wPtr)
            Indx = Indx + 8

            ! process the 17th digit
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*Base + ToLong(IACHAR(CurChr) - A0)
            Indx = Indx + 1

            ! process the last digit
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IErr = 1
                EXIT Main
            END IF
            Val = Val*Base + ToLong(IACHAR(CurChr) - A0)
        END BLOCK Main

        IF (IErr == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Val = 0_kLong
        END IF

        ! nullify pointers
        NULLIFY(wPtr)
        cPtr = C_NULL_PTR

        RETURN

    END FUNCTION Parse_18Digits

    !**************************************************************************

END FUNCTION FromString_P18Special

!******************************************************************************

MODULE FUNCTION FromString_Xp(cStr, Algorithm, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tInteger,             INTENT(IN)    :: Algorithm
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt64)                       :: Big      ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        Big = FromString_P18Simple(cStr, ErrFlag, ErrMsg)
    CASE (2)
        Big = FromString_P16P8(cStr, ErrFlag, ErrMsg)
    CASE (3)
        Big = FromString_P18P8(cStr, ErrFlag, ErrMsg)
    CASE (4)
!        Big = FromString_P18Special(cStr, ErrFlag, ErrMsg)
        Big = FromString_P18MulSpecial(cStr, ErrFlag, ErrMsg)
    CASE DEFAULT
        Big = DecString_To_ApInt64(cStr, ErrFlag, ErrMsg)
    END SELECT

    RETURN

END FUNCTION FromString_Xp

!******************************************************************************

END SUBMODULE SubClass_Api64_Constructor

!******************************************************************************

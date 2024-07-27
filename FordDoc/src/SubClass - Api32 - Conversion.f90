
SUBMODULE (Class_ApInt32 : SubClass_Api32_Auxiliary) SubClass_Api32_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations of the <a href="../module/class_apint32.html">ApInt32</a> type.

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ISO_C_BINDING !,          ONLY: C_LOC, C_F_POINTER, C_NULL_PTR
    USE ModBase_Error_Handlers
!    USE ModBase_SIntUtil !,       ONLY: I32_FromChar, I64_FromChar
!    USE ModBase_UIntUtil,       ONLY: ToDecStrUnsigned, ToUnsignedLong
!    USE ModBase_SInt128
!    USE ModBase_UInt128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName = 'SubClass_Api32_Conversion'
    tLogical,  PARAMETER    :: Positive = FalseVal

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
!                           ASSIGNMENT ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE ApInt32_Assign(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'Other' to 'This'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(OUT)  :: This
    TYPE(ApInt32), INTENT(IN)   :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    This%Sign   = Other%Sign
    This%Length = Other%Length
    IF (ALLOCATED(Other%Digit)) THEN
        CALL MemAlloc(This%Digit, SIZE(Other%Digit, KIND=kIndex), StartID=0_kIndex)
        This%Digit = Other%Digit
    END IF

    RETURN

END SUBROUTINE ApInt32_Assign

!******************************************************************************

MODULE SUBROUTINE ApInt32_From_I32(Big, I32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'I32' to 'Big'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(OUT)  :: Big
    tInteger,      INTENT(IN)   :: I32   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I32)

    RETURN

END SUBROUTINE ApInt32_From_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_From_I64(Big, I64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'I64' to 'Big'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(OUT)  :: Big
    tLong,         INTENT(IN)   :: I64   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I64)

    RETURN

END SUBROUTINE ApInt32_From_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_From_I128(Big, I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'I128' to 'Big'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(OUT)  :: Big
    TYPE(SInt128), INTENT(IN)   :: I128   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 4_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I128)

    RETURN

END SUBROUTINE ApInt32_From_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_To_I32(I32, Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'Big' to 'I32'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(OUT)  :: I32      ! value treated as signed
    TYPE(ApInt32), INTENT(IN)   :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: MinI32 = ToInteger(Z'80000000')
    tInteger, PARAMETER :: MaxI32 = ToInteger(Z'7FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        ! rely on that sign always is either 1/-1
        I32 = Big%Sign*IAND(Big%Digit(0), MaxI32)
    ELSE
        I32 = MinI32
    END IF

    RETURN

END SUBROUTINE ApInt32_To_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_To_I64(I64, Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'Big' to 'I64'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(OUT)  :: I64   ! value treated as signed
    TYPE(ApInt32), INTENT(IN)   :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI64 = ToLong(Z'8000000000000000')
    tLong, PARAMETER    :: MaxI32 = ToLong(Z'000000007FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length == 1_kIndex) THEN
            I64 = Big%Sign*ToUnsignedLong(Big%Digit(0))
        ELSE
            I64 = Big%Sign*IOR(SHIFTL(IAND(ToLong(Big%Digit(1)), MaxI32), 32), &
                               ToUnsignedLong(Big%Digit(0)))
        END IF
    ELSE
        I64 = MinI64
    END IF

    RETURN

END SUBROUTINE ApInt32_To_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_To_I128(I128, Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign 'Big' to 'I128'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(OUT)  :: I128
    TYPE(ApInt32), INTENT(IN)   :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: High, Low

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        SELECT CASE (Big%Length)
        CASE (1)
            Low  = ToUnsignedLong(Big%Digit(0))
            I128 = SInt128(0_kLong, Low)
        CASE (2)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            I128 = SInt128(0_kLong, Low)
        CASE (3)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = ToUnsignedLong(Big%Digit(2))
            I128 = SInt128(High, Low)
        CASE DEFAULT
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = IOR(SHIFTL(ToLong(Big%Digit(3)), 32), ToUnsignedLong(Big%Digit(2)))
            I128 = SInt128(High, Low)
        END SELECT
        IF (Big%Sign < 0) I128 = -I128
    ELSE
        I128 = MinI128
    END IF

    RETURN

END SUBROUTINE ApInt32_To_I128

!------------------------------------------------------------------------------
!
!                           CONSTRUCTOR ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION Construct_ApInt32(Sign, Length, Digit) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct ApInt32 object from the specified input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign
    tIndex,   INTENT(IN)    :: Length
    tInteger, INTENT(IN)    :: Digit(0:Length-1)
    TYPE(ApInt32)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Big%Sign   = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length = Length
    IF (Big%Length < 1) THEN
        CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
        Big%Length = 1_kIndex
        Big%Digit(0) = 0
    ELSE
        CALL MemAlloc(Big%Digit, Length, StartID=0_kIndex)
        Big%Digit  = Digit
    END IF

    RETURN

END FUNCTION Construct_ApInt32

!******************************************************************************

MODULE FUNCTION Bytes_To_ApInt32(Sign, Length, Digit) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct ApInt32 object from the specified input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign                 ! sign of the number
    tIndex,   INTENT(IN)    :: Length               ! length of input array
    tByte,    INTENT(IN)    :: Digit(0:Length-1)    ! the magnitude of the number given as a
                                                    ! byte array where the first element gives
                                                    ! the least significant 8 bits
    TYPE(ApInt32)           :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskU8 = ToLong(Z'00000000000000FF')

!** SUBROUTINE MACRO DECLARATIONS:
#define MaskLong(X)     IAND(ToLong(X), MaskU8)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: BLen, DLen, I, Offset, Remaining

!** FLOW

    BLen = Length
    IF (BLen == 0_kIndex) THEN
        Big = ZeroApInt32()
        RETURN
    END IF
    DO
        IF (BLen == 1_kIndex) EXIT
        IF (Digit(BLen-1_kIndex) /= 0_kLong) EXIT
        BLen = BLen - 1_kIndex
    END DO
    Big%Sign = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length = (BLen + 3_kIndex)/4_kIndex
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)
    DLen = BLen/4_kIndex
    Offset = 0_kIndex
    DO I = 0_kIndex, DLen-1_kIndex
        Big%Digit(I) = PackFull(Digit, Offset)
        Offset = Offset + 4_kIndex
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
        ! To pack the array 'Buf' at offset 'Off' into the 32-bit word 'Res',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tInteger            :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        ! implementation algorithm #1
        Res = IOR(IOR(IOR(       MaskLong(Buf(Off)),          &
                          SHIFTL(MaskLong(Buf(Off+1)),  8)),  &
                          SHIFTL(MaskLong(Buf(Off+2)), 16)),  &
                          SHIFTL(MaskLong(Buf(Off+3)), 24))

        RETURN

    END FUNCTION PackFull

    !**************************************************************************

    PURE FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        ! into the 32-bit word 'Res', in little-endian convention
        ! (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 3)
        tInteger            :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:3)
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

END FUNCTION Bytes_To_ApInt32

!******************************************************************************

MODULE FUNCTION DecString_To_ApInt32(cStr, ErrFlag, ErrMsg) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(ApInt32)                       :: Big      ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: A0   = IACHAR('0')
    tInteger, PARAMETER :: Base = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Indx, IndxP9
    tInteger                :: StrLen, DigitLen
    tCharLen(1), POINTER    :: CurChr
    tInteger                :: I32Val

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
            Big%Length = 1_kIndex
            CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)
            Big%Digit(0) = 0
            RETURN
        END IF
    END IF

    ! compute the length of digits and size of magnitude array
    DigitLen = StrLen - Indx + 1
    IF (DigitLen < 10) THEN
        Big%Length = 1_kIndex
    ELSE
        ! 3402 = bits per digit * 1024
        Big%Length = SHIFTR(ToIndex(SHIFTR(DigitLen*3402_kLong, 10)) + 32_kIndex, 5)
    END IF
    CALL MemAlloc(Big%Digit, Big%Length, StartID=0_kIndex)

    ! return quickly if we can
    IF (DigitLen < 10) THEN
        BLOCK
            tLogical    :: ErrorFlag  ! true if input is not invalid
            tCharAlloc  :: ErrorMsg   ! message if input is not invalid
            ! execution
            Big%Digit(0) = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Big%Length = 0_kIndex
                CALL MemFree(Big%Digit)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
        END BLOCK
        RETURN
    END IF

    IndxP9 = MOD(DigitLen, 9)
    IF (IndxP9 == 0) THEN
        IndxP9 = Indx + 9
    ELSE
        IndxP9 = Indx + IndxP9
    END IF
    ! get value for the first digit
    IF (Parse(Indx, IndxP9, Big%Digit(0)) == 1) THEN
        ! return if there is an error
        Big%Length = 0_kIndex
        CALL MemFree(Big%Digit)
        RETURN
    END IF

    Indx   = IndxP9
    IndxP9 = Indx + 9
    Big%Length = 1_kIndex
    DO WHILE (Indx <= StrLen)
        IF (Parse(Indx, IndxP9, I32Val) == 1) THEN
            ! return if there is an error
            Big%Length = 0_kIndex
            CALL MemFree(Big%Digit)
            RETURN
        END IF
        CALL Multiply_N_Add(Big, 1000000000, I32Val)
        Indx   = IndxP9
        IndxP9 = Indx + 9
    END DO

    RETURN

CONTAINS

    FUNCTION Parse(IStart, IEnd, Val) RESULT(IErr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse a part of a character string as an unsigned decimal number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)   :: IStart   ! the starting index (inclusive)
        tInteger,  INTENT(IN)   :: IEnd     ! the ending index (exclusive)
        tInteger,  INTENT(OUT)  :: Val      ! the parse number
        tInteger                :: IErr     ! 0 - no error, 1 = error

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx
        tCharLen(1), POINTER    :: CurChr

    !** FLOW

        ! initialize
        IErr = 0
        Val  = 0
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
            Val = Val*Base + (IACHAR(CurChr) - A0)
            Indx = Indx + 1
        END DO

        RETURN

    END FUNCTION Parse

    !**************************************************************************

    SUBROUTINE Multiply_N_Add(Big, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  Big = Big*Mul + Add)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT) :: Big
        tInteger,     INTENT(IN)    :: Mul      ! Mul < 2**31
        tInteger,     INTENT(IN)    :: Add      ! Add < 2**31

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Carry
        tInteger    :: I

    !** FLOW

        Carry = 0_kLong
        DO I = 0, Big%Length-1
            Carry = ToLong(Mul)*ToUnsignedLong(Big%Digit(I)) + Carry
            Big%Digit(I) = ToInteger(Carry)
            Carry = SHIFTR(Carry, 32)
        END DO
        IF (Carry /= 0_kLong) THEN
            Big%Digit(Big%Length) = ToInteger(Carry)
            Big%Length = Big%Length + 1
        END IF
        Carry = ToUnsignedLong(Big%Digit(0)) + ToLong(Add)
        Big%Digit(0) = ToInteger(Carry)
        IF (SHIFTR(Carry, 32) /= 0_kLong) THEN
            I = 1_kIndex
            DO
                Big%Digit(I) = Big%Digit(I) + 1
                IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0))) EXIT
                I = I + 1_kIndex
            END DO
            IF (I == Big%Length) THEN
                Big%Digit(Big%Length) = 1
                Big%Length = Big%Length + 1_kIndex
            END IF
        END IF

        RETURN

    END SUBROUTINE Multiply_N_Add

    !**************************************************************************

END FUNCTION DecString_To_ApInt32

!******************************************************************************

MODULE FUNCTION U32_To_ApInt32(Sign, U32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign     ! the sign of the number
    tInteger, INTENT(IN)    :: U32      ! the magnitude of the number treated as unsigned
    TYPE(ApInt32)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U32)

    RETURN

END FUNCTION U32_To_ApInt32

!******************************************************************************

MODULE FUNCTION U64_To_ApInt32(Sign, U64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: Sign     ! the sign of the number
    tLong,    INTENT(IN)    :: U64      ! the magnitude of the number treated as unsigned
    TYPE(ApInt32)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U64)

    RETURN

END FUNCTION U64_To_ApInt32

!******************************************************************************

MODULE FUNCTION U128_To_ApInt32(Sign, U128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: Sign     ! the sign of the number
    TYPE(UInt128), INTENT(IN)   :: U128     ! the magnitude of the number treated as unsigned
    TYPE(ApInt32)               :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 4_kIndex, StartID=0_kIndex)
    CALL AssignUnsigned(Big, Sign, U128)

    RETURN

END FUNCTION U128_To_ApInt32

!******************************************************************************

MODULE FUNCTION I32_To_ApInt32(I32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I32      ! the magnitude of the number treated as signed
    TYPE(ApInt32)           :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I32)

    RETURN

END FUNCTION I32_To_ApInt32

!******************************************************************************

MODULE FUNCTION I64_To_ApInt32(I64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: I64      ! the magnitude of the number treated as signed
    TYPE(ApInt32)       :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I64)

    RETURN

END FUNCTION I64_To_ApInt32

!******************************************************************************

MODULE FUNCTION I128_To_ApInt32(I128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128     ! the magnitude of the number treated as signed
    TYPE(ApInt32)               :: Big

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 4_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I128)

    RETURN

END FUNCTION I128_To_ApInt32

!******************************************************************************

MODULE FUNCTION R32_To_ApInt32(R32) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle, INTENT(IN) :: R32
    TYPE(ApInt32)       :: Big

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
        CALL Handle_ErrLevel('R32_To_ApInt32', ModName, ErrSevere, &
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

END FUNCTION R32_To_ApInt32

!******************************************************************************

MODULE FUNCTION R64_To_ApInt32(R64) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: R64
    TYPE(ApInt32)       :: Big

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
        CALL Handle_ErrLevel('R64_To_ApInt32', ModName, ErrSevere, &
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

END FUNCTION R64_To_ApInt32

!******************************************************************************

MODULE FUNCTION R128_To_ApInt32(R128) RESULT(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct the ApInt32 object based on the given number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad, INTENT(IN)   :: R128
    TYPE(ApInt32)       :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Mask = ToInteger(Z'00007FFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Exp
    TYPE(UInt128)       :: IBits
    TYPE(UInt128)       :: TwoPow112
    tIndex              :: Length
    tInteger            :: Digit(0:3)
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: LBits(2)

!** FLOW

    ! undefined behavior if R128 is NaN.
    IF (.NOT.IEEE_IS_FINITE (R128)) THEN
        CALL Handle_ErrLevel('R128_To_ApInt32', ModName, ErrSevere, &
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
    Digit(0) = ToInteger(IBits%Low)
    Digit(1) = ToInteger(SHIFTR(IBits%Low, 32))
    Digit(2) = ToInteger(IBits%High)
    Digit(3) = ToInteger(SHIFTR(IBits%High, 32))
    Length = 4_kIndex
    DO WHILE (Digit(Length-1_kIndex) == 0)
        Length = Length - 1_kIndex
    END DO
    Big%Sign = 1
    Big%Length = Length
    CALL MemAlloc(Big%Digit, Length+1_kIndex, StartID=0_kIndex)
    Big%Digit(0:Length-1) = Digit(0:Length-1)
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

END FUNCTION R128_To_ApInt32

!------------------------------------------------------------------------------
!
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE Bytes_From_ApInt32(Big, Digit, Sign)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct ApInt32 object from the specified input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32),      INTENT(IN)  :: Big
    tByte, ALLOCATABLE, INTENT(OUT) :: Digit(:) ! the magnitude of the number given as a
                                                ! byte array where the first element gives
                                                ! the least significant 8 bits
    tInteger, OPTIONAL, INTENT(OUT) :: Sign     ! sign of the number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskU8 = ToLong(Z'00000000000000FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Offset

!** FLOW

    IF (ALLOCATED(Big%digit)) THEN
        CALL MemAlloc(Digit, Big%Length*4_kIndex, StartID=0_kIndex)
        IF (PRESENT(Sign)) Sign = Big%Sign
        Offset = 0_kIndex
        DO I = 0, Big%Length
            CALL UnpackU32(Big%Digit(I), Digit, Offset)
            Offset = Offset + 4_kIndex
        END DO
    ELSE
        IF (PRESENT(Sign)) Sign = Big%Sign
        ALLOCATE(Digit(0))
    END IF

    RETURN

    CONTAINS

    SUBROUTINE UnpackU32(Val, Buf, Off)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To unpack the 32-bit word 'Val' into the array 'Buf' at offset 'Off',
        ! in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: Val      ! the value to encode
        tByte,    INTENT(INOUT) :: Buf(0:)  ! the destination buffer
        tIndex,   INTENT(IN)    :: Off      ! the destination offset

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        Buf(Off)   = ToByte(Val)
        Buf(Off+1) = ToByte(SHIFTR(Val, 8))
        Buf(Off+2) = ToByte(SHIFTR(Val, 16))
        Buf(Off+3) = ToByte(SHIFTR(Val, 24))

        RETURN

    END SUBROUTINE UnpackU32

    !**************************************************************************

END SUBROUTINE Bytes_From_ApInt32

!******************************************************************************

MODULE FUNCTION DecString_From_ApInt32(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a decimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tChar, PARAMETER :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                    :: BufLen, Top, I, J
    tCharLen(Big%Length*10+1)   :: Buffer
    TYPE(ApInt32)               :: Copy
    tLong                       :: Tmp
    tLong                       :: Q, R

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    ELSEIF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*10 + 1
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top = BufLen
    Copy = MakeCopy(Big)
    DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_kLong)
            Q = Tmp/10
            R = Tmp - Q*10
            Buffer(Top:Top) = NumStr(R)
            Top = Top - 1
            Tmp = Q
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        Buffer(Top:Top) = '-'
        Str = Buffer(Top:BufLen)
    ELSE
        Str = Buffer(Top+1:BufLen)
    END IF

    RETURN

CONTAINS

    FUNCTION ToStringDivide(Big) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tLong                           :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: Pow2  = SHIFTL(1, 13)
        tLong,    PARAMETER :: Pow5  = 1220703125_kLong
        tLong,    PARAMETER :: Pow10 = ToLong(Pow2)*Pow5

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: NextQ, I, Q, Mod2

    !** FLOW

        NextQ = 0
        Remainder = 0_kLong
        DO I = Big%Length-1, 1, -1
            Remainder = SHIFTL(Remainder, 32) + ToUnsignedLong(Big%Digit(I))
            Q = ToInteger(Remainder/Pow5)
            Remainder = MOD(Remainder, Pow5)
            Big%Digit(I) = IOR(NextQ, SHIFTR(Q, 13))
            NextQ = SHIFTL(Q, 19)   ! 19 = 32-13
        END DO
        Remainder = SHIFTL(Remainder, 32) + ToUnsignedLong(Big%Digit(0))
        Mod2 = IAND(Big%Digit(0), Pow2 - 1)
        Big%Digit(0) = IOR(NextQ, ToInteger(SHIFTR(Remainder/Pow5, 13)))
        Remainder = MOD(Remainder, Pow5)
        ! Applies the Chinese Remainder Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        Remainder = MOD((Remainder - MOD(Pow5*(Mod2 - Remainder), Pow10)*67), Pow10)
        IF (Remainder < 0_kLong) Remainder = Remainder + Pow10
        IF ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1)) THEN
            Big%Length = Big%Length - 1
            IF ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1)) THEN
                Big%Length = Big%Length - 1
            END IF
        END IF

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

END FUNCTION DecString_From_ApInt32

!******************************************************************************

MODULE FUNCTION U32_From_ApInt32(Big) RESULT(U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to an unsigned 32-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: U32      ! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        U32 = Big%Digit(0)
    ELSE
        U32 = 0
    END IF

    RETURN

END FUNCTION U32_From_ApInt32

!******************************************************************************

MODULE FUNCTION U64_From_ApInt32(Big) RESULT(U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to an unsigned 64-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLong                       :: U64      ! number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length == 1_kIndex) THEN
            U64 = ToUnsignedLong(Big%Digit(0))
        ELSE
            U64 = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
        END IF
    ELSE
        U64 = 0_kLong
    END IF

    RETURN

END FUNCTION U64_From_ApInt32

!******************************************************************************

MODULE FUNCTION U128_From_ApInt32(Big) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a UInt128 number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(UInt128)               :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Low, High

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        SELECT CASE (Big%Length)
        CASE (1)
            Low  = ToUnsignedLong(Big%Digit(0))
            U128 = UInt128(0_kLong, Low)
        CASE (2)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            U128 = UInt128(0_kLong, Low)
        CASE (3)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = ToUnsignedLong(Big%Digit(2))
            U128 = UInt128(High, Low)
        CASE DEFAULT
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = IOR(SHIFTL(ToLong(Big%Digit(3)), 32), ToUnsignedLong(Big%Digit(2)))
            U128 = UInt128(High, Low)
        END SELECT
    ELSE
        U128 = ZeroU128
    END IF

    RETURN

END FUNCTION U128_From_ApInt32

!******************************************************************************

MODULE FUNCTION I32_From_ApInt32(Big) RESULT(I32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a signed 32-bit integer number
    ! (To return "sign * (magnitude & 0x7FFFFFFFF)")

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: I32      ! number treated as unsigned

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: MinI32 = ToInteger(Z'80000000')
    tInteger, PARAMETER :: MaxI32 = ToInteger(Z'7FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        ! rely on that sign always is either 1/-1
        I32 = Big%Sign*IAND(Big%Digit(0), MaxI32)
    ELSE
        I32 = MinI32
    END IF

    RETURN

END FUNCTION I32_From_ApInt32

!******************************************************************************

MODULE FUNCTION I64_From_ApInt32(Big) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a signed 64-bit integer number
    ! (To return "sign * (magnitude & 0x7FFFFFFFFFFFFFFF)")

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLong                       :: I64      ! number treated as unsigned

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI64 = ToLong(Z'8000000000000000')
    tLong, PARAMETER    :: MaxI32 = ToLong(Z'000000007FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length == 1_kIndex) THEN
            I64 = Big%Sign*ToUnsignedLong(Big%Digit(0))
        ELSE
            I64 = Big%Sign*IOR(SHIFTL(IAND(ToLong(Big%Digit(1)), MaxI32), 32), &
                               ToUnsignedLong(Big%Digit(0)))
        END IF
    ELSE
        I64 = MinI64
    END IF

    RETURN

END FUNCTION I64_From_ApInt32

!******************************************************************************

MODULE FUNCTION I128_From_ApInt32(Big) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a SInt128 number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(SInt128)               :: I128      ! number treated as unsigned

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI128 = ToLong(Z'8000000000000000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: High, Low

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        SELECT CASE (Big%Length)
        CASE (1)
            Low  = ToUnsignedLong(Big%Digit(0))
            I128 = SInt128(0_kLong, Low)
        CASE (2)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            I128 = SInt128(0_kLong, Low)
        CASE (3)
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = ToUnsignedLong(Big%Digit(2))
            I128 = SInt128(High, Low)
        CASE DEFAULT
            Low  = IOR(SHIFTL(ToLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
            High = IOR(SHIFTL(ToLong(Big%Digit(3)), 32), ToUnsignedLong(Big%Digit(2)))
            I128 = SInt128(High, Low)
        END SELECT
        IF (Big%Sign < 0) I128 = -I128
    ELSE
        I128 = MinI128
    END IF

    RETURN

END FUNCTION I128_From_ApInt32

!******************************************************************************

MODULE FUNCTION R32_From_ApInt32(Big) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a 32-bit floating point number
    ! To return the most significant 24 bits in the mantissa (the
    ! highest order bit obviously being implicit), the exponent value
    ! which will be consistent for ApInt32 up to 128 bits (should it
    ! not fit it'll be calculated modulo 256), and the sign bit set
    ! if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tSingle                     :: R32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: S, Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tSingle, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt32, TARGET     :: IBits

!** FLOW

    ! check if the number is valid or not
    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        R32 = -Huge_RSP
        RETURN
    END IF

    ! check leading zeros and return quickly if possible
    S = LEADZ(Big%Digit(Big%Length-1_kIndex))
    IF ((Big%Length == 1_kIndex).AND.(S >= 8)) THEN
        R32 = REAL(Big%Sign, KIND=kSingle)*U64_To_R32(ToUnsignedLong(Big%Digit(0)))
        RETURN
    END IF

    ! Mask out the 24 MSBits
    IBits = Big%Digit(Big%Length-1_kIndex)
    IF (S <= 8) THEN
        IBits = SHIFTR(IBits, 8-S)
    ELSE
        ! S-8 == additional bits we need
        IBits = IOR(SHIFTL(IBits, S-8), SHIFTR(Big%Digit(Big%Length-2_kIndex), 32-(S-8)))
    END IF
    ! The leading bit is implicit, cancel it out.
    IBits = ToInteger(IEOR(IBits, SHIFTL(1_kInteger, 23)))

    Exp = ToInteger(IAND(((32-S + 32_kLong*(Big%Length-1)) - 1 + 127), ToLong(Z'00000000000000FF')))

    ! Add exponent
    IBits = IOR(IBits, SHIFTL(Exp, 23))
    ! Add sign-bit
    IBits = IOR(IBits, IAND(Big%Sign, SHIFTL(1, 31)))

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
        ! To convert an unsigned 64-bit integer number to a 32-bit floating point number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal      ! integer number treated as unsigned one
        tSingle             :: SingleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kLong) THEN
            SingleVal = REAL(LongVal, KIND=kSingle)
        ELSE
            SingleVal = TwoPow64 + REAL(LongVal, KIND=kSingle)
        END IF

        RETURN

    END FUNCTION U64_To_R32

    !******************************************************************************

END FUNCTION R32_From_ApInt32

!******************************************************************************

MODULE FUNCTION R64_From_ApInt32(Big) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a 64-bit floating point number
    ! To return the most significant 53 bits in the mantissa (the
    ! highest order bit obviously being implicit), the exponent value
    ! which will be consistent for ApInt32 up to 1024 bits (should it
    ! not fit it'll be calculated modulo 2048), and the sign bit set
    ! if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tDouble                     :: R64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: S
    tLong               :: Exp, LVal
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! check if the number is valid or not
    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        R64 = -Huge_RDP
        RETURN
    END IF

    ! return quickly if possible
    IF (Big%Length == 1_kIndex) THEN
        R64 = REAL(Big%Sign*ToUnsignedLong(Big%Digit(0)), KIND=kDouble)
        RETURN
    END IF

    ! check leading zeros and return quickly if possible
    S = LEADZ(Big%Digit(Big%Length-1_kIndex))
    IF ((Big%Length == 2_kIndex).AND.(32-S+32 <= 53)) THEN
        LVal = IOR(SHIFTL(ToUnsignedLong(Big%Digit(1)), 32), ToUnsignedLong(Big%Digit(0)))
        R64  = REAL(Big%Sign, KIND=kSingle)*U64_To_R64(LVal)
        RETURN
    END IF

    ! Mask out the 53 MSBits
    IBits = IOR(SHIFTL(ToLong(Big%Digit(Big%Length-1_kIndex)), 32), &
                       ToUnsignedLong(Big%Digit(Big%Length-2_kIndex)))
    IF (S <= 11) THEN
        IBits = SHIFTR(IBits, 11-S)
    ELSE
        ! S-11 == additional bits we need
        IBits = IOR(SHIFTL(IBits, S-11), ToLong(SHIFTR(Big%Digit(Big%Length-3_kIndex), 32-(S-11))))
    END IF
    ! The leading bit is implicit, cancel it out.
    IBits = IEOR(IBits, SHIFTL(1_kLong, 52))

    Exp = IAND(((32-S + 32_kLong*(Big%Length-1)) - 1 + 1023), ToLong(Z'00000000000007FF'))
    ! Add exponent
    IBits = IOR(IBits, SHIFTL(Exp, 52))
    ! Add sign-bit
    IBits = IOR(IBits, IAND(ToLong(Big%Sign), SHIFTL(1_kLong, 63)))

    !----------------------------------------------------------------------!
    !+++++  Transfer output (R32 mapped to IBits using C_F_POINTER)   +++++!
    !----------------------------------------------------------------------!
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! get a 32-bit real number equivalent to the 32-bit integer number
    R64 = fPtr

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR

    RETURN

CONTAINS

    FUNCTION U64_To_R64(LongVal) RESULT(DoubleVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 64-bit floating point number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal      ! integer number treated as unsigned one
        tDouble             :: DoubleVal    ! floating point number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kLong) THEN
            DoubleVal = REAL(LongVal, KIND=kDouble)
        ELSE
            DoubleVal = TwoPow64 + REAL(LongVal, KIND=kDouble)
        END IF

        RETURN

    END FUNCTION U64_To_R64

    !******************************************************************************

END FUNCTION R64_From_ApInt32

!******************************************************************************

MODULE FUNCTION R128_From_ApInt32(Big) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt32 number to a 128-bit floating point number
    ! To return the most significant 113 bits in the mantissa (the
    ! highest order bit obviously being implicit), the exponent value
    ! which will be consistent for ApInt32 up to 16384 bits (should it
    ! not fit it'll be calculated modulo 32768), and the sign bit set
    ! if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tQuad                       :: R128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: S
    TYPE(SInt128)       :: Bits, Exp, I128
    TYPE(UInt128)       :: U128
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tQuad,   POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

!** FLOW

    ! check if the number is valid or not
    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        R128 = -Huge_RQP
        RETURN
    END IF
    ! return quickly if possible
    IF (Big%Length <= 3_kIndex) THEN
        SELECT CASE (Big%Length)
        CASE (1_kIndex)
            R128 = REAL(Big%Sign*ToUnsignedLong(Big%Digit(0)), KIND=kQuad)
        CASE (2_kIndex)
            IBits(1) = IOR(SHIFTL(ToUnsignedLong(Big%Digit(1)), 32), &
                           ToUnsignedLong(Big%Digit(0)))
            R128 = REAL(Big%Sign, KIND=kSingle)*U64_To_R128(IBits(1))
        CASE (3_kIndex)
            IBits(2) = ToUnsignedLong(Big%Digit(2))                 ! HighBits
            IBits(1) = IOR(SHIFTL(ToUnsignedLong(Big%Digit(1)), 32), &
                           ToUnsignedLong(Big%Digit(0)))            ! LowBits
            U128 = UInt128(IBits(2), IBits(1))
            R128 = REAL(Big%Sign, KIND=kQuad)*ToR128(U128)
        END SELECT
        RETURN
    END IF

    ! check leading zeros and return quickly if possible
    S = LEADZ(Big%Digit(Big%Length-1_kIndex))
    IF ((Big%Length == 4_kIndex).AND.(32-S+32*3 <= 113)) THEN
        IBits(2) = IOR(SHIFTL(ToUnsignedLong(Big%Digit(3)), 32), &
                      ToUnsignedLong(Big%Digit(2)))                 ! HighBits
        IBits(1) = IOR(SHIFTL(ToUnsignedLong(Big%Digit(1)), 32), &
                      ToUnsignedLong(Big%Digit(0)))                 ! LowBits
        U128 = UInt128(IBits(2), IBits(1))
        R128 = REAL(Big%Sign, KIND=kQuad)*ToR128(U128)
        RETURN
    END IF

    ! Mask out the 113 MSBits
    IBits(2) = IOR(SHIFTL(ToLong(Big%Digit(Big%Length-1_kIndex)), 32), &
                  ToUnsignedLong(Big%Digit(Big%Length-2_kIndex)))           ! HighBits
    IBits(1) = IOR(SHIFTL(ToUnsignedLong(Big%Digit(Big%Length-3_kIndex)), 32), &
                  ToUnsignedLong(Big%Digit(Big%Length-4_kIndex)))           ! LowBits
    Bits = Sint128(IBits(2), IBits(1))
    IF (S <= 15) THEN
        Bits = SHIFTR(Bits, 15-S)
    ELSE
        ! S-11 == additional bits we need
        I128 = SHIFTR(Big%Digit(Big%Length-5_kIndex), 32-(S-15))
        Bits = IOR(SHIFTL(Bits, S-15), I128)
    END IF
    ! The leading bit is implicit, cancel it out.
    Bits = IEOR(Bits, SHIFTL(OneI128, 112))

    Exp = IAND(((32-S + 32_kLong*(Big%Length-1)) - 1 + 16383), ToLong(Z'0000000000007FFF'))

    ! Add exponent
    Bits = IOR(Bits, SHIFTL(Exp, 112))
    ! Add sign-bit
    I128 = Big%Sign
    Bits = IOR(Bits, IAND(I128, SHIFTL(OneI128, 127)))

    !-----------------------------------------------------------------------!
    !+++++  Transfer output (R128 mapped to IBits using C_F_POINTER)   +++++!
    !-----------------------------------------------------------------------!
    IF (IsLittleEndian) THEN
        IBits(1) = Bits%Low
        IBits(2) = Bits%High
    ELSE
        IBits(1) = Bits%High
        IBits(2) = Bits%Low
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
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal  ! integer number treated as unsigned one
        tQuad               :: QuadVal  ! floating point number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tQuad, PARAMETER  :: TwoPow64 = 2.0_kQuad**64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kLong) THEN
            QuadVal = REAL(LongVal, KIND=kQuad)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=kQuad)
        END IF

        RETURN

    END FUNCTION U64_To_R128

    !******************************************************************************

END FUNCTION R128_From_ApInt32

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE Assign_U32_To_ApInt32(Big, Sign, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     ! the sign of the number
    tInteger,      INTENT(IN)       :: U32      ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    Big%Sign     = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length   = 1_kIndex
    Big%Digit(0) = U32

    RETURN

END SUBROUTINE Assign_U32_To_ApInt32

!******************************************************************************

MODULE SUBROUTINE Assign_U64_To_ApInt32(Big, Sign, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     ! the sign of the number
    tLong,         INTENT(IN)       :: U64      ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    ELSEIF (SIZE(Big%Digit) < 2) THEN
        CALL MemResize(Big%Digit, 2_kIndex)
    END IF
    Big%Sign     = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length   = 2_kIndex
    Big%Digit(0) = ToInteger(IAND(U64, MASK))
    Big%Digit(1) = ToInteger(SHIFTR(U64, 32))
    IF (Big%Digit(1) == 0) Big%Length = Big%Length - 1_kIndex

    RETURN

END SUBROUTINE Assign_U64_To_ApInt32

!******************************************************************************

MODULE SUBROUTINE Assign_U128_To_ApInt32(Big, Sign, U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     ! the sign of the number
    TYPE(UInt128), INTENT(IN)       :: U128     ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: U128Lo, U128Hi

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        CALL MemAlloc(Big%Digit, 4_kIndex, StartID=0_kIndex)
    ELSEIF (SIZE(Big%Digit) < 4) THEN
        CALL MemResize(Big%Digit, 4_kIndex)
    END IF
    Big%Sign     = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length   = 4_kIndex
    U128Lo = U128%Low
    U128Hi = U128%High
    Big%Digit(0) = ToInteger(IAND(U128Lo, MASK))
    Big%Digit(1) = ToInteger(SHIFTR(U128Lo, 32))
    Big%Digit(2) = ToInteger(IAND(U128Hi, MASK))
    Big%Digit(3) = ToInteger(SHIFTR(U128Hi, 32))
    DO WHILE (Big%Digit(Big%Length-1) == 0)
        Big%Length = Big%Length - 1_kIndex
        IF (Big%Length == 1_kIndex) EXIT
    END DO

    RETURN

END SUBROUTINE Assign_U128_To_ApInt32

!******************************************************************************

MODULE SUBROUTINE Assign_U32_To_ApInt32_NoSign(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given non-negative number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32      ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U32)

    RETURN

END SUBROUTINE Assign_U32_To_ApInt32_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_U64_To_ApInt32_NoSign(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given non-negative number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64      ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U64)

    RETURN

END SUBROUTINE Assign_U64_To_ApInt32_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_U128_To_ApInt32_NoSign(Big, U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given non-negative number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(UInt128), INTENT(IN)       :: U128  ! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U128)

    RETURN

END SUBROUTINE Assign_U128_To_ApInt32_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_I32_To_ApInt32(Big, I32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: I32      ! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (I32 < 0) THEN
        CALL AssignUnsigned(Big, -1, -I32)
    ELSE
        CALL AssignUnsigned(Big, 1, I32)
    END IF

    RETURN

END SUBROUTINE Assign_I32_To_ApInt32

!******************************************************************************

MODULE SUBROUTINE Assign_I64_To_ApInt32(Big, I64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: I64      ! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (I64 < 0_kLong) THEN
        CALL AssignUnsigned(Big, -1, -I64)
    ELSE
        CALL AssignUnsigned(Big, 1, I64)
    END IF

    RETURN

END SUBROUTINE Assign_I64_To_ApInt32

!******************************************************************************

MODULE SUBROUTINE Assign_I128_To_ApInt32(Big, I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the given number to the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(SInt128), INTENT(IN)       :: I128      ! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsNegative(I128)) THEN
        CALL AssignUnsigned(Big, -1, ToU128(-I128))
    ELSE
        CALL AssignUnsigned(Big, 1, ToU128(I128))
    END IF

    RETURN

END SUBROUTINE Assign_I128_To_ApInt32

!******************************************************************************

END SUBMODULE SubClass_Api32_Conversion

!******************************************************************************

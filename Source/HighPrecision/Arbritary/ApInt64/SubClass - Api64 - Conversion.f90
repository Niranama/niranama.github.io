
SUBMODULE (Class_ApInt64 : SubClass_Api64_Arithmetic) SubClass_Api64_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.
!   In particular, these routines are used to convert from the *ApInt64* type to
!   other data types.

!** USE STATEMENTS:
    USE ISO_C_BINDING
    USE ModBase_LargeTables,    ONLY: NumStr => Char1Digit, Char2Digits

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_Conversion'

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
!                           CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE Bytes_From_ApInt64(Big, Digit, Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the ApInt64 object to a 8-bit integer magnitude array and its sign.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64),      INTENT(IN)  :: Big
    !> the magnitude of the number given as a 8-bit integer array where
    !  the first element gives the least significant 8 bits (i.e. little
    !  endian order)
    tByte, ALLOCATABLE, INTENT(OUT) :: Digit(:)
    tInteger, OPTIONAL, INTENT(OUT) :: Sign     !! sign of the number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskU8 = ToLong(Z'00000000000000FF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Offset

!** FLOW

    IF (ALLOCATED(Big%digit)) THEN
        CALL MemAlloc(Digit, Big%Length*8_kIndex, StartID=0_kIndex)
        IF (PRESENT(Sign)) Sign = Big%Sign
        Offset = 0_kIndex
        DO I = 0, Big%Length
            CALL UnpackI64(Big%Digit(I), Digit, Offset)
            Offset = Offset + 8_kIndex
        END DO
    ELSE
        IF (PRESENT(Sign)) Sign = Big%Sign
        ALLOCATE(Digit(0))
    END IF

    RETURN

    CONTAINS

    SUBROUTINE UnpackI64(Val, Buf, Off)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To unpack the 64-bit word 'Val' into the array 'Buf' at offset 'Off',
        !  in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(IN)      :: Val      !! the value to encode
        tByte,  INTENT(INOUT)   :: Buf(0:)  !! the destination buffer
        tIndex, INTENT(IN)      :: Off      !! the destination offset

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        Buf(Off)   = ToByte(Val)
        Buf(Off+1) = ToByte(SHIFTR(Val, 8))
        Buf(Off+2) = ToByte(SHIFTR(Val, 16))
        Buf(Off+3) = ToByte(SHIFTR(Val, 24))
        Buf(Off+4) = ToByte(SHIFTR(Val, 32))
        Buf(Off+5) = ToByte(SHIFTR(Val, 40))
        Buf(Off+6) = ToByte(SHIFTR(Val, 48))
        Buf(Off+7) = ToByte(SHIFTR(Val, 56))

        RETURN

    END SUBROUTINE UnpackI64

    !**************************************************************************

END SUBROUTINE Bytes_From_ApInt64

!******************************************************************************

MODULE FUNCTION DecString_From_ApInt64(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an arbitrary-precision signed integer to a decimal string.

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - specialized 'I64_ToChar' routine
    ! - divide Big by 10**18 at a time
    ! ToStringDivide uses
    ! - specialized 'UDivRemBy1' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,       PARAMETER  :: DivBase = 1000000000000000000_kLong
    tInteger,    PARAMETER  :: S = LEADZ(DivBase)
    tLong,       PARAMETER  :: NormBase = SHIFTL(DivBase, S)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp, Reciprocal

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    ELSEIF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    Reciprocal = Reciprocal_2By1(NormBase)
    DO
        J = Top
        Tmp = ToStringDivide(Copy, Reciprocal, S, NormBase)
        CALL I64_ToChar(Tmp, Buffer, Top)
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 18
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

CONTAINS

    SUBROUTINE I64_ToChar(Number, cStr, Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to character string

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(IN)    :: Number   ! number
        tChar,    INTENT(INOUT) :: cStr(:)  ! character array
        tInteger, INTENT(INOUT) :: Indx     ! current index to the array

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger,    PARAMETER  :: MaxLen = 20

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr
        tLong               :: PosNum, TmpNum, HiNum, LoNum, MidNum
        tInteger            :: Start, I

    !** FLOW

        ! set positive number
        PosNum = ABS(Number)

        ! start conversion and store digits in working string
        IF (PosNum < 100000000_kLong) THEN                  ! 1-8 digits
            Start = 12 + Write_1_to_8_Digits(ToInteger(PosNum), wStr(13:20))
        ELSEIF (PosNum < 10000000000000000_kLong) THEN      ! 9-16 digits
            HiNum = PosNum / 100000000_kLong
            LoNum = PosNum - HiNum * 100000000_kLong        ! MOD(PosNum, 100000000)
            CALL Write_8_Digits(ToInteger(LoNum), wStr(13:20))
            Start = 4 + Write_1_to_8_Digits(ToInteger(HiNum), wStr(5:12))
        ELSE                                                ! 17-20 digits
            TmpNum = PosNum / 100000000_kLong
            LoNum = PosNum - TmpNum * 100000000_kLong       ! MOD(PosNum, 100000000)
            HiNum = TmpNum / 10000_kLong
            MidNum = TmpNum - HiNum * 10000_kLong           ! MOD(TmpNum, 10000)
            CALL Write_8_Digits(ToInteger(LoNum), wStr(13:20))
            CALL Write_4_Digits(ToInteger(MidNum), wStr(9:12))
            Start = Write_5_to_8_Digits(ToInteger(HiNum), wStr(1:8))
        END IF

        ! transfer to output string
        DO I = MaxLen, Start, -1
            cStr(Indx) = wStr(I:I)
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE I64_ToChar

    !**************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AA, BB, CC, DD   ! working indices
        tInteger    :: AABB, CCDD       ! working variables

    !** FLOW

        AABB = ToInteger(SHIFTR(ToLong(Number)*109951163_kLong, 40))    ! Number / 10000
        CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
        AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
        BB = AABB - AA*100                                              ! MOD(AABB, 100)
        DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)
        cStr(5:6) = Char2Digits(CC)
        cStr(7:8) = Char2Digits(DD)

        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AA, BB   ! working indices

    !** FLOW

        AA = SHIFTR(Number*5243, 19)            ! Number / 100
        BB = Number - AA*100                    ! MOD(Number, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)

        RETURN

    END SUBROUTINE Write_4_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tInteger                    :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AA, BB, CC, DD   ! working indices
        tInteger    :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 100) THEN              ! 1-2 digits
            AA = Number
            IF (AA < 10) THEN
                cStr(8:8) = Char2Digits(AA)(2:2)
                Start = 8
            ELSE
                cStr(7:8) = Char2Digits(AA)
                Start = 7
            END IF
        ELSEIF (Number < 10000) THEN        ! 3-4 digits
            AA = ToInteger(SHIFTR(ToLong(Number)*5243_kLong, 19))   ! Number / 100
            BB = Number - AA*100                                    ! MOD(Number, 100)
            IF (AA < 10) THEN
                cStr(6:6) = Char2Digits(AA)(2:2)
                cStr(7:8) = Char2Digits(BB)
                Start = 6
            ELSE
                cStr(5:6) = Char2Digits(AA)
                cStr(7:8) = Char2Digits(BB)
                Start = 5
            END IF
        ELSEIF (Number < 1000000) THEN      ! 5-6 digits
            AA = ToInteger(SHIFTR(ToLong(Number)*429497_kLong, 32)) ! Number / 10000
            BBCC = Number - AA*10000                                ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
            CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN    ! 7-8 digits
            AABB = ToInteger(SHIFTR(ToLong(Number)*109951163_kLong, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger,  INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tInteger                    :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: AA, BB, CC, DD   ! working indices
        tInteger    :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 1000000) THEN      ! 5-6 digits
            AA = ToInteger(SHIFTR(ToLong(Number)*429497_kLong, 32)) ! Number / 10000
            BBCC = Number - AA*10000                                ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
            CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN    ! 7-8 digits
            AABB = ToInteger(SHIFTR(ToLong(Number)*109951163_kLong, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_5_to_8_Digits

    !**************************************************************************

END FUNCTION DecString_From_ApInt64

!******************************************************************************

MODULE FUNCTION U32_From_ApInt64(Big) RESULT(U32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an arbitrary-precision signed integer to a 32-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger                    :: U32  !! a 32-bit integer treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        U32 = ToInteger(Big%Digit(0))
    ELSE
        U32 = 0
    END IF

    RETURN

END FUNCTION U32_From_ApInt64

!******************************************************************************

MODULE FUNCTION U64_From_ApInt64(Big) RESULT(U64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an arbitrary-precision signed integer to a 64-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tLong                       :: U64  !! a 64-bit integer treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        U64 = Big%Digit(0)
    ELSE
        U64 = 0_kLong
    END IF

    RETURN

END FUNCTION U64_From_ApInt64

!******************************************************************************

MODULE FUNCTION U128_From_ApInt64(Big) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an arbitrary-precision signed integer to a 128-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(UInt128)               :: U128 !! a 128-bit unsigned integer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length == 1_kIndex) THEN
            U128 = UInt128(0_kLong, Big%Digit(0))
        ELSE
            U128 = UInt128(Big%Digit(1), Big%Digit(0))
        END IF
    ELSE
        U128 = ZeroU128
    END IF

    RETURN

END FUNCTION U128_From_ApInt64

!******************************************************************************

MODULE FUNCTION I32_From_ApInt64(Big) RESULT(I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a signed 32-bit integer number
    !  (To return "sign * (magnitude & 0x7FFFFFFFF)").

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger                    :: I32      !! number treated as signed

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: MinI32 = ToInteger(Z'80000000')
    tInteger, PARAMETER :: MaxI32 = ToInteger(Z'7FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        ! rely on that sign always is either 1/-1
        I32 = Big%Sign*IAND(ToInteger(Big%Digit(0)), MaxI32)
    ELSE
        I32 = MinI32
    END IF

    RETURN

END FUNCTION I32_From_ApInt64

!******************************************************************************

MODULE FUNCTION I64_From_ApInt64(Big) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a signed 64-bit integer number
    !  (To return "sign * (magnitude & 0x7FFFFFFFFFFFFFFF)").

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tLong                       :: I64      !! number treated as signed

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI64 = ToLong(Z'8000000000000000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        I64 = Big%Sign*IAND(Big%Digit(0), MaxI64)
    ELSE
        I64 = MinI64
    END IF

    RETURN

END FUNCTION I64_From_ApInt64

!******************************************************************************

MODULE FUNCTION I128_From_ApInt64(Big) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a SInt128 number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(SInt128)               :: I128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI128 = ToLong(Z'8000000000000000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length == 1_kIndex) THEN
            I128 = SInt128(0_kLong, Big%Digit(0))
        ELSE
            I128 = SInt128(IAND(Big%Digit(1), MaxI64), Big%Digit(0))
        END IF
        IF (Big%Sign < 0) I128 = -I128
    ELSE
        I128 = MinI128
    END IF

    RETURN

END FUNCTION I128_From_ApInt64

!******************************************************************************

MODULE FUNCTION R32_From_ApInt64(Big) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a 32-bit floating point number. <br>
    !  To return the most significant 24 bits in the mantissa (the
    !  highest order bit obviously being implicit), the exponent value
    !  which will be consistent for ApInt64 up to 128 bits (should it
    !  not fit it'll be calculated modulo 256), and the sign bit set
    !  if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
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
    IF ((Big%Length == 1_kIndex).AND.(S >= 40)) THEN
        R32 = REAL(Big%Sign, KIND=kSingle)*U64_To_R32(Big%Digit(0))
        RETURN
    END IF

    ! Mask out the 24 MSBits
    IF (S <= 40) THEN
        IBits = ToInteger(SHIFTR(Big%Digit(Big%Length-1_kIndex), 40-S))
    ELSE
        ! S-40 == additional bits we need
        IBits = ToInteger(IOR(SHIFTL(Big%Digit(Big%Length-1_kIndex), S-40),  SHIFTR(Big%Digit(Big%Length-2_kIndex), 64-(S-40))))
    END IF

    ! The leading bit is implicit, cancel it out.
    IBits = ToInteger(IEOR(IBits, SHIFTL(1_kInteger, 23)))

    Exp = ToInteger(IAND(((64-S + 64_kLong*(Big%Length-1)) - 1 + 127), ToLong(Z'00000000000000FF')))
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
        !^ To convert an unsigned 64-bit integer number to a 32-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal      !! integer number treated as unsigned one
        tSingle             :: SingleVal    !! floating point number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64

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

END FUNCTION R32_From_ApInt64

!******************************************************************************

MODULE FUNCTION R64_From_ApInt64(Big) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a 64-bit floating point number. <br>
    !  To return the most significant 53 bits in the mantissa (the
    !  highest order bit obviously being implicit), the exponent value
    !  which will be consistent for ApInt64 up to 1024 bits (should it
    !  not fit it'll be calculated modulo 2048), and the sign bit set
    !  if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tDouble                     :: R64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: S
    tLong               :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tDouble, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits

!** FLOW

    ! check if the number is valid or not
    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        R64 = -Huge_RDP
        RETURN
    END IF

    ! check leading zeros and return quickly if possible
    S = LEADZ(Big%Digit(Big%Length-1_kIndex))
    IF ((Big%Length == 1_kIndex).AND.(S >= 11)) THEN
        R64 = REAL(Big%Sign, KIND=kDouble)*U64_To_R64(Big%Digit(0))
        RETURN
    END IF

    ! Mask out the 53 MSBits
    IBits = Big%Digit(Big%Length-1_kIndex)
    IF (S <= 11) THEN
        IBits = SHIFTR(IBits, 11-S)
    ELSE
        ! S-11 == additional bits we need
        IBits = IOR(SHIFTL(IBits, S-11), SHIFTR(Big%Digit(Big%Length-2_kIndex), 64-(S-11)))
    END IF
    ! The leading bit is implicit, cancel it out.
    IBits = IEOR(IBits, SHIFTL(1_kLong, 52))

    Exp = IAND(((64-S + 64_kLong*(Big%Length-1)) - 1 + 1023), ToLong(Z'00000000000007FF'))
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
        !^ To convert an unsigned 64-bit integer number to a 64-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal      !! integer number treated as unsigned one
        tDouble             :: DoubleVal    !! floating point number

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

END FUNCTION R64_From_ApInt64

!******************************************************************************

MODULE FUNCTION R128_From_ApInt64(Big) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a ApInt64 number to a 128-bit floating point number. <br>
    !  To return the most significant 113 bits in the mantissa (the
    !  highest order bit obviously being implicit), the exponent value
    !  which will be consistent for ApInt64 up to 16384 bits (should it
    !  not fit it'll be calculated modulo 32768), and the sign bit set
    !  if this number is negative.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
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
    IF (Big%Length == 1_kIndex) THEN
        R128 = REAL(Big%Sign, KIND=kQuad)*U64_To_R128(Big%Digit(0))
        RETURN
    END IF

    ! check leading zeros and return quickly if possible
    S = LEADZ(Big%Digit(Big%Length-1_kIndex))
    IF ((Big%Length == 2_kIndex).AND.(64-S+64 <= 113)) THEN
        IBits(2) = Big%Digit(1)               ! HighBits
        IBits(1) = Big%Digit(0)               ! LowBits
        U128 = UInt128(IBits(2), IBits(1))
        R128 = REAL(Big%Sign, KIND=kQuad)*ToR128(U128)
        RETURN
    END IF

    ! Mask out the 113 MSBits
    IBits(2) = Big%Digit(Big%Length-1_kIndex)  ! HighBits
    IBits(1) = Big%Digit(Big%Length-2_kIndex)  ! LowBits
    Bits = Sint128(IBits(2), IBits(1))
    IF (S <= 15) THEN
        Bits = SHIFTR(Bits, 15-S)
    ELSE
        ! S-11 == additional bits we need
        I128 = SHIFTR(Big%Digit(Big%Length-3_kIndex), 64-(S-15))
        Bits = IOR(SHIFTL(Bits, S-15), I128)
    END IF
    ! The leading bit is implicit, cancel it out.
    Bits = IEOR(Bits, SHIFTL(OneI128, 112))

    Exp = IAND(((64-S + 64_kLong*(Big%Length-1)) - 1 + 16383), ToLong(Z'0000000000007FFF'))

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
        !^ To convert an unsigned 64-bit integer number to a 128-bit floating point number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LongVal  !! integer number treated as unsigned one
        tQuad               :: QuadVal  !! floating point number

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

    !**************************************************************************

END FUNCTION R128_From_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION ToStringDivide(Big, Reciprocal, S, NormBase) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the number by 10**18 and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: Reciprocal   ! reciprocal
    tInteger,      INTENT(IN)       :: S            ! shift
    tLong,         INTENT(IN)       :: NormBase     ! normalized base
    tLong                           :: Remainder    ! remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: U(0:Big%Length+1)    ! working dividend
    tIndex      :: M, I

!** FLOW

    M = Big%Length
    U(0:M-1) = Big%Digit(0:M-1)
    U(M)     = 0_kLong
    IF (S > 0) THEN
        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF
    CALL UDivRem_By1(U, M+1, NormBase, Reciprocal, Remainder)
    Big%Digit(0:M-1) = U(0:M-1)
    Remainder = SHIFTR(Remainder, S)
    IF ((Big%Digit(M-1) == 0_kLong).AND.(M > 1_kIndex)) THEN
        M = M - 1_kIndex
        IF ((Big%Digit(M-1) == 0_kLong).AND.(M > 1_kIndex)) THEN
            M = M - 1_kIndex
        END IF
        Big%Length = M
    END IF

    RETURN

END FUNCTION ToStringDivide

!******************************************************************************

SUBROUTINE UDivRem_By1(U, ULen, Divisor, Reciprocal, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform arbitrary-precision unsigned integer division by 64-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: ULen         ! length of U
    tLong,  INTENT(INOUT)   :: U(0:ULen-1)  ! on entry, the dividend
                                            ! on exit, the quotient
    tLong,  INTENT(IN)      :: Divisor      ! (normalized) divisor
    tLong,  INTENT(IN)      :: Reciprocal   ! the reciprocal of the divisor
    tLong,  INTENT(OUT)     :: R            ! the remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: UHi, ULo
    tIndex      :: I

!** FLOW

    IF (ULen < 2) THEN
        CALL Handle_ErrLevel('UDivRem_By1', SubName, ErrSevere, 'ULen must be at least 2.')
        RETURN
    END IF

    UHi = U(ULen - 1)       ! Set the top word as remainder.
    U(ULen - 1) = 0_kLong   ! Reset the word being a part of the resulting quotient.

    I = ULen - 2
    DO
        ULo = U(I)
        CALL UDivRem_2By1(UHi, ULo, Divisor, Reciprocal, U(I), R)
        UHi = R
        IF (I == 0) EXIT
        I = I - 1
    END DO

    RETURN

END SUBROUTINE UDivRem_By1

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                   ALTERNATIVE ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION ToString_CRT13_SInt128(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! This routine is a naively-adapted version of that of ApInt32
    ! Division of Big uses
    ! - separate divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - the Chinese Remainder Theorem
    ! - SInt128 type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp
    tLong           :: Indx

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_kLong)
            Indx = MOD(Tmp, 10_kLong)
            Buffer(Top) = NumStr(Indx)
            Top = Top - 1
            Tmp = Tmp / 10_kLong
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

CONTAINS

    FUNCTION ToStringDivide(Big) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong                           :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: Pow2  = SHIFTL(1, 13)
        tInteger, PARAMETER :: Pow5  = 1220703125
        tLong,    PARAMETER :: Pow10 = ToLong(Pow2)*ToLong(Pow5)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)   :: D128, R128, Q128
        tLong           :: NextQ, I, Q, Mod2

    !** FLOW

        NextQ = 0_kLong
        R128 = ZeroI128
        DO I = Big%Length-1, 1, -1
            D128 = SInt128(R128%Low, Big%Digit(I))
            CALL DivMod(D128, Pow5, Q128, R128)
            Q = Q128%Low
            Big%Digit(I) = IOR(NextQ, SHIFTR(Q, 13))
            NextQ = SHIFTL(Q, 51)   ! 51 = 64-13
        END DO
        D128  = SInt128(R128%Low, Big%Digit(I))
        Mod2 = IAND(Big%Digit(0), Pow2 - 1_kLong)
        CALL DivMod(D128, Pow5, Q128, R128)
        Q = Q128%Low
        Big%Digit(0) = IOR(NextQ, SHIFTR(Q, 13))
        ! Applies the Chinese Remainder Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        R128 = MOD((R128 - MOD(Pow5*(Mod2 - R128), Pow10)*67), Pow10)
        IF (R128 < ZeroI128) R128 = R128 + Pow10
        IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
            IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                Big%Length = Big%Length - 1_kIndex
            END IF
        END IF
        Remainder = R128

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

END FUNCTION ToString_CRT13_SInt128

!******************************************************************************

FUNCTION ToString_CRT13_UInt128(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - separate divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - the Chinese Remainder Theorem
    ! - UInt128 type and SMOD (a signed version of modulation operation of UInt128).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp
    tLong           :: Indx

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    DO
        J = Top
        Tmp = ToStrDiv_UInt128(Copy)
        DO WHILE (Tmp > 0_kLong)
            Indx = MOD(Tmp, 10_kLong)
            Buffer(Top) = NumStr(Indx)
            Top = Top - 1
            Tmp = Tmp / 10_kLong
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

END FUNCTION ToString_CRT13_UInt128

!******************************************************************************

FUNCTION ToString_CDM13_UInt128(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - combined divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - the Chinese Remainder Theorem
    ! - UInt128 type and SMOD (a signed version of modulation operation of UInt128).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    DO
        J = Top
        Tmp = ToStrDiv_UInt128(Copy)
        DO WHILE (Tmp > 0_kLong)
            Buffer(Top) = NumStr(DivModBy10(Tmp))
            Top = Top - 1
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

END FUNCTION ToString_CDM13_UInt128

!******************************************************************************

FUNCTION ToString_CDM13_BaseDivCRT(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - combined divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - the Chinese Remainder Theorem for Big with length of 1
    ! - basic 'Divide' routine (from arithmetic submodule) for Big with length of 2 and more.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: DivBase = 10000000000000_kLong

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_kLong)
            Buffer(Top) = NumStr(DivModBy10(Tmp))
            Top = Top - 1
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

CONTAINS

    FUNCTION ToStringDivide(Big) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong                           :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: Pow2  = SHIFTL(1, 13)
        tLong,    PARAMETER :: Pow5  = 1220703125_kLong
        tLong,    PARAMETER :: Pow10 = Pow2*Pow5

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:

    !** FLOW

        IF (Big%Length == 1) THEN
            BLOCK
                TYPE(UInt128)   :: Rem, Quot, Numer, Pow10_128, Pow5_128
                tLong           :: Q, NextQ, R, Mod2
                tIndex          :: I
                ! execution
                Mod2 = IAND(Big%Digit(0), Pow2 - 1_kLong)
                CALL UDivMod(Big%Digit(0), Pow5, Q, R)
                Big%Digit(Big%Length-1) = SHIFTR(Q, 13)
                Rem = UInt128(0_kLong, R)
                ! Applies the Chinese Rem Theorem.
                ! -67*5^13 + 9983778*2^13 = 1
                Pow10_128 = UInt128(0_kLong, Pow10)
                Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
                IF (Rem%High < 0_kLong) Rem = Rem + Pow10
                IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                    Big%Length = Big%Length - 1_kIndex
                    IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                        Big%Length = Big%Length - 1_kIndex
                    END IF
                END IF
                Remainder = Rem%Low
            END BLOCK
        ELSE
            CALL Big%Divide(DivBase, Remainder)
        END IF

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

END FUNCTION ToString_CDM13_BaseDivCRT

!******************************************************************************

FUNCTION ToString_CDM13_DivRemCRT(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - combined divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - the Chinese Remainder Theorem for Big with length of 1
    ! - specialized 'UDivRemBy1' routine for Big with length of 2 and more.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,    PARAMETER :: DivBase = 10000000000000_kLong
    tInteger, PARAMETER :: S = LEADZ(DivBase)
    tLong,    PARAMETER :: NormBase = SHIFTL(DivBase, S)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp, Reciprocal

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    IF (Copy%Length > 1) THEN
        Reciprocal = Reciprocal_2By1(NormBase)
    ELSE
        Reciprocal = 1_kLong
    END IF
    DO
        J = Top
        Tmp = ToStringDivide(Copy, Reciprocal)
        DO WHILE (Tmp > 0_kLong)
            Buffer(Top) = NumStr(DivModBy10(Tmp))
            Top = Top - 1
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

CONTAINS

    FUNCTION ToStringDivide(Big, Reciprocal) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong,         INTENT(IN)       :: Reciprocal
        tLong                           :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: Pow2  = SHIFTL(1, 13)
        tLong,    PARAMETER :: Pow5  = 1220703125_kLong
        tLong,    PARAMETER :: Pow10 = Pow2*Pow5

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: U(0:Big%Length+1)    ! working dividend
        tIndex      :: M

    !** FLOW

        IF (Big%Length == 1) THEN
            BLOCK
                TYPE(UInt128)   :: Rem, Quot, Numer, Pow10_128, Pow5_128
                tLong           :: Q, NextQ, R, Mod2
                tIndex          :: I
                ! execution
                Mod2 = IAND(Big%Digit(0), Pow2 - 1_kLong)
                CALL UDivMod(Big%Digit(0), Pow5, Q, R)
                Big%Digit(Big%Length-1) = SHIFTR(Q, 13)
                Rem = UInt128(0_kLong, R)
                ! Applies the Chinese Rem Theorem.
                ! -67*5^13 + 9983778*2^13 = 1
                Pow10_128 = UInt128(0_kLong, Pow10)
                Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
                IF (Rem%High < 0_kLong) Rem = Rem + Pow10
                IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                    Big%Length = Big%Length - 1_kIndex
                    IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                        Big%Length = Big%Length - 1_kIndex
                    END IF
                END IF
                Remainder = Rem%Low
            END BLOCK
        ELSE
            M = Big%Length
            U(0:M-1) = Big%Digit(0:M-1)
            U(M)     = 0_kLong
            IF (S > 0) THEN
                U(M) = SHIFTR(U(M-1), 64-S)
                DO I = M-1, 1, -1
                    U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
                END DO
                U(0) = SHIFTL(U(0), S)
            END IF
            CALL UDivRem_By1(U, M+1, NormBase, Reciprocal, Remainder)
            Big%Digit(0:M-1) = U(0:M-1)
            Remainder = SHIFTR(Remainder, S)
            IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                Big%Length = Big%Length - 1_kIndex
                IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
                    Big%Length = Big%Length - 1_kIndex
                END IF
            END IF
        END IF

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

END FUNCTION ToString_CDM13_DivRemCRT

!******************************************************************************

FUNCTION ToString_CDM13_DivRemBy1(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - combined divide and mod operations
    ! - divide Big by 10**13 at a time
    ! ToStringDivide uses
    ! - specialized 'UDivRemBy1' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,    PARAMETER :: DivBase = 10000000000000_kLong
    tInteger, PARAMETER :: S = LEADZ(DivBase)
    tLong,    PARAMETER :: NormBase = SHIFTL(DivBase, S)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp, Reciprocal

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    Reciprocal = Reciprocal_2By1(NormBase)
    DO
        J = Top
        Tmp = ToStringDivide(Copy, Reciprocal, S, NormBase)
        DO WHILE (Tmp > 0_kLong)
            Buffer(Top) = NumStr(DivModBy10(Tmp))
            Top = Top - 1
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

END FUNCTION ToString_CDM13_DivRemBy1

!******************************************************************************

FUNCTION ToString_CDM18_DivRemBy1(Big) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** TECHNICAL NOTES:
    ! Division of Big uses
    ! - combined divide and mod operations
    ! - divide Big by 10**18 at a time
    ! ToStringDivide uses
    ! - specialized 'UDivRemBy1' routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tCharAlloc                  :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong,    PARAMETER :: DivBase = 1000000000000000000_kLong
    tInteger, PARAMETER :: S = LEADZ(DivBase)
    tLong,    PARAMETER :: NormBase = SHIFTL(DivBase, S)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: BufLen, Top, I, J
    tChar           :: Buffer(Big%Length*20+1)
    TYPE(ApInt64)   :: Copy
    tLong           :: Tmp, Reciprocal

!** FLOW

    IF (IsZero(Big)) THEN
        Str = '0'
        RETURN
    ELSEIF (.NOT.ALLOCATED(Big%Digit)) THEN
        Str = ''
        RETURN
    END IF

    BufLen = ToInteger(Big%Length)*20 + 1
    FORALL (I=1:BufLen) Buffer(I) = '0'
    Top = BufLen
    Copy = ABS(Big)
    Reciprocal = Reciprocal_2By1(NormBase)
    DO
        J = Top
        Tmp = ToStringDivide(Copy, Reciprocal, S, NormBase)
        DO WHILE (Tmp > 0_kLong)
            Buffer(Top) = NumStr(DivModBy10(Tmp))
            Top = Top - 1
        END DO
        IF ((Copy%Length == 1_kIndex).AND.(Copy%Digit(0) == 0)) THEN
            EXIT
        ELSE
            Top = J - 18
        END IF
    END DO
    IF (Big%Sign < 0) THEN
        ALLOCATE(CHARACTER(LEN=BufLen-Top+1) :: Str)
        Str(1:1) = '-'
        Top = Top - 1
        J = 2
    ELSE
        ALLOCATE(CHARACTER(LEN=BufLen-Top) :: Str)
        J = 1
    END IF
    DO I = J, LEN(Str)
        Str(I:I) = Buffer(Top+I)
    END DO

    RETURN

END FUNCTION ToString_CDM18_DivRemBy1

!******************************************************************************

FUNCTION ToStrDiv_UInt128(Big) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the number by 10**13 and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong                           :: Remainder

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Pow2  = SHIFTL(1, 13)
    tLong,    PARAMETER :: Pow5  = 1220703125_kLong
    tLong,    PARAMETER :: Pow10 = Pow2*Pow5
    tLogical, PARAMETER :: Positive = FalseVal
    tLogical, PARAMETER :: AsUnsigned = TrueVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: Rem, Quot, Numer, Pow10_128, Pow5_128
    tLong           :: Q, NextQ, R, Mod2
    tIndex          :: I

!** FLOW

    IF (Big%Length == 1) THEN
        Mod2 = IAND(Big%Digit(0), Pow2 - 1_kLong)
        CALL UDivMod(Big%Digit(0), Pow5, Q, R)
        Big%Digit(Big%Length-1) = SHIFTR(Q, 13)
        Rem = UInt128(0_kLong, R)
    ELSE
        CALL UDivMod(Big%Digit(Big%Length-1), Pow5, Q, R)
        Big%Digit(Big%Length-1) = SHIFTR(Q, 13)
        I = Big%Length-2
        Pow5_128 = UInt128(Pow5, AsUnsigned)
        DO
            NextQ = SHIFTL(Q, 51)   ! 51 = 64-13
            IF (I < 1) EXIT
            Numer = UInt128(R, Big%Digit(I))
            CALL UDivMod(Numer, Pow5_128, Quot, Rem)
            Q = Quot%Low
            R = Rem%Low
            Big%Digit(I) = IOR(NextQ, SHIFTR(Q, 13))
            I = I - 1
        END DO
        Numer = UInt128(R, Big%Digit(0))
        Mod2 = IAND(Big%Digit(0), Pow2 - 1_kLong)
        CALL UDivMod(Numer, Pow5_128, Quot, Rem)
        Big%Digit(0) = IOR(SHIFTL(Q, 51), SHIFTR(Quot%Low, 13))
    END IF
    ! Applies the Chinese Rem Theorem.
    ! -67*5^13 + 9983778*2^13 = 1
    Pow10_128 = UInt128(0_kLong, Pow10)
    Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
    IF (Rem%High < 0_kLong) Rem = Rem + Pow10
    IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
        Big%Length = Big%Length - 1_kIndex
        IF ((Big%Digit(Big%Length-1) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
        END IF
    END IF
    Remainder = Rem%Low

    RETURN

END FUNCTION ToStrDiv_UInt128

!******************************************************************************

FUNCTION SMOD(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two UInt128 objects (Dividend / Divisor)
    ! and return the remainder
    ! note: although the input and output objects are of UInt128 type,
    !       they are all treated as if they are signed 128-bit integers

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: Dividend
    TYPE(UInt128), INTENT(IN)   :: Divisor
    TYPE(UInt128)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: PosNHi, PosNLo, PosDHi, PosDLo
    TYPE(UInt128)   :: PosN, PosD
    TYPE(UInt128)   :: PosR, PosQ
    tLogical        :: NegDividend

!** FLOW

    ! get unsigned absolute of SInt128
    NegDividend = Dividend%High < 0_kLong
    IF (NegDividend) THEN
        PosNHi = NOT(Dividend%High)
        IF (Dividend%Low == 0_kLong) PosNHi = PosNHi + 1_kLong
        PosNLo = NOT(Dividend%Low) + 1_kLong
        PosN = UInt128(PosNHi, PosNLo)
    ELSE
        PosN = Dividend
    END IF
    IF (Divisor%High < 0_kLong) THEN
        PosDHi = NOT(Divisor%High)
        IF (Divisor%Low == 0_kLong) PosDHi = PosDHi + 1_kLong
        PosDLo = NOT(Divisor%Low) + 1_kLong
        PosD = UInt128(PosDHi, PosDLo)
    ELSE
        PosD = Divisor
    END IF

    ! perform unsigned division
    CALL UDivMod(PosN, PosD, PosQ, PosR)

    IF (NegDividend) THEN
        ! Remainder = -PosR
        IF (PosR%Low == 0_kLong) THEN
            Remainder = UInt128(NOT(PosR%High) + 1_kLong, NOT(PosR%Low)  + 1_kLong)
        ELSE
            Remainder = UInt128(NOT(PosR%High), NOT(PosR%Low) + 1_kLong)
        END IF
    ELSE
        Remainder = PosR
    END IF

    RETURN

END FUNCTION SMOD

!******************************************************************************

FUNCTION DivModBy10(DivQuot) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division by 10

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: DivQuot
    tLong                   :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Numer

!** FLOW

    Numer = DivQuot
    DivQuot = Numer / 10_kLong
    Rem = Numer - DivQuot * 10_kLong

    RETURN

END FUNCTION DivModBy10

!******************************************************************************

MODULE FUNCTION ToString_Xp(Big, Algorithm) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a ApInt64 number to a decimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: Algorithm
    tCharAlloc                  :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        Str = ToString_CRT13_SInt128(Big)
    CASE (2)
        Str = ToString_CRT13_UInt128(Big)
    CASE (3)
        Str = ToString_CDM13_UInt128(Big)
    CASE (4)
        Str = ToString_CDM13_BaseDivCRT(Big)
    CASE (5)
        Str = ToString_CDM13_DivRemCRT(Big)
    CASE (6)
        Str = ToString_CDM13_DivRemBy1(Big)
    CASE (7)
        Str = ToString_CDM18_DivRemBy1(Big)
    CASE DEFAULT
        Str = DecString_From_ApInt64(Big)
    END SELECT

    RETURN

END FUNCTION ToString_Xp

!******************************************************************************

END SUBMODULE SubClass_Api64_Conversion

!******************************************************************************

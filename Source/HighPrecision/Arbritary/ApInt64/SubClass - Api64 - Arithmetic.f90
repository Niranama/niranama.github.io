
SUBMODULE (Class_ApInt64 : SubClass_Api64_Auxiliary) SubClass_Api64_Arithmetic

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to arithmetic
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_LargeTables,    ONLY: RecTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_Arithmetic'
    tLogical,  PARAMETER    :: Positive = FalseVal
    tLong,     PARAMETER    :: MaxU64   = ToLong(Z'FFFFFFFFFFFFFFFF')   ! max unsigned 64-bit
    tLong,     PARAMETER    :: MinU64   = ToLong(Z'0000000000000000')   ! min unsigned 64-bit
    tLong,     PARAMETER    :: MaxI64   = ToLong(Z'7FFFFFFFFFFFFFFF')   ! max signed 64-bit
    tLong,     PARAMETER    :: MinI64   = ToLong(Z'8000000000000000')   ! min signed 64-bit

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** GENERIC DECLARATIONS:
    INTERFACE  UAddMag
        MODULE PROCEDURE UAddMag_U32, UAddMag_U64
    END INTERFACE
    INTERFACE  USubMag
        MODULE PROCEDURE USubMag_U32, USubMag_U64
    END INTERFACE
    INTERFACE  UAdd
        MODULE PROCEDURE UAdd_U32, UAdd_U64
    END INTERFACE
    INTERFACE  USub
        MODULE PROCEDURE USub_U32, USub_U64
    END INTERFACE
    INTERFACE  UMul
        MODULE PROCEDURE UMul_U32, UMul_U64
    END INTERFACE
    INTERFACE  UDiv
        MODULE PROCEDURE UDiv_U32, UDiv_U64
    END INTERFACE
    INTERFACE  URem
        MODULE PROCEDURE URem_U32, URem_U64
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 ARITHMETIC OPERATIONS                    ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt64_Modulo(Dividend, Divisor) RESULT(Modulo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the modulo of the arguments

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)    :: Dividend
    TYPE(ApInt64), INTENT(IN)    :: Divisor
    TYPE(ApInt64)                :: Modulo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Modulo = MOD(Dividend, Divisor)
    IF (Dividend%Sign*Divisor%Sign < 0) Modulo = Modulo + Divisor

    RETURN

END FUNCTION ApInt64_Modulo

!******************************************************************************

MODULE FUNCTION ApInt64_Power(BigIn, Exp) RESULT(BigOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return BigOut = BigIn**Exp

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: BigIn
    tInteger,      INTENT(IN)   :: Exp      ! must be nonnegative
    TYPE(ApInt64)               :: BigOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Exp <= 1) THEN
        IF (Exp == 1) THEN
            BigOut = MakeCopy(BigIn)
        ELSEIF (Exp == 0) THEN
            BigOut = OneApInt64()
        ELSE
            BigOut = ZeroApInt64()
        END IF
        RETURN
    END IF

    IF (BigIn%Length <= 64) THEN
        ! small number
        IF (IsZero(BigIn)) THEN
            BigOut = ZeroApInt64()
        ELSE
            BigOut = ApInt_Power_Recur(BigIn, Exp)
        END IF
    ELSE
        IF (Exp <= 6) THEN
            ! large number with small exponent
            BigOut = ApInt_Power_Recur(BigIn, Exp)
        ELSE
            ! large number with large exponent
            BigOut = ApInt64_Power_Java(BigIn, Exp)
        END IF
    END IF

    RETURN

    CONTAINS

    FUNCTION ApInt_Power_Recur(BigIn, Exp) RESULT(BigOut)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return BigOut = BigIn**Exp

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: BigIn
        tInteger,      INTENT(IN)   :: Exp      ! must be nonnegative
        TYPE(ApInt64)               :: BigOut

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        CALL MemAlloc(BigOut%Digit, BigIn%Length*Exp, StartID=0_kIndex)
        BigOut%Digit(0) = 1_kLong
        BigOut%Length   = 1_kIndex
        BigOut%Sign     = 1

        CALL Power_Recur(BigIn, Exp, BigOut)

        RETURN

    END FUNCTION ApInt_Power_Recur

    !**************************************************************************

    FUNCTION ApInt64_Power_Java(BigIn, Exp) RESULT(BigOut)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return BigOut = BigIn**Exp

        ! this USE statement needed by GFORTRAN (but not by IFORT nor IFX)
        USE Class_ApInt64,  ONLY: SHIFTL, SHIFTR, TRAILZ

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: BigIn
        tInteger,      INTENT(IN)   :: Exp      ! must be nonnegative
        TYPE(ApInt64)               :: BigOut

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, PARAMETER    :: MaxI32 = ToLong(Z'000000007FFFFFFF') ! max signed 32-bit

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(ApInt64)   :: Part2SQ, Answer
        tInteger        :: PowTwo, Bits2Shift, RemBits, NewSign, WrkExp
        tLong           :: Bits2ShiftLong

    !** FLOW

        Part2SQ = ABS(BigIn)

        ! Factor out powers of two from the base, as the exponentiation of
        ! these can be done by left shifts only.
        ! The remaining part can then be exponentiated faster.  The
        ! powers of two will be multiplied back at the end.
        PowTwo = TRAILZ(Part2SQ)    ! the index of the rightmost (lowest-order) one bit
        Bits2ShiftLong = ToLong(PowTwo) * Exp

        IF (Bits2ShiftLong > MaxI32) THEN
            ! report overflow
            BigOut = ZeroApInt64()
            CALL Handle_ErrLevel('ApInt64_Power_Java', SubName, ErrSevere, &
                              'ApInt64 would overflow the supported range.')
            RETURN
        END IF
        Bits2Shift = ToInteger(Bits2ShiftLong)

        ! Factor the powers of two out quickly by shifting right, if needed.
        IF (PowTwo > 0) THEN
            Part2SQ = SHIFTR(Part2SQ, PowTwo)
            RemBits = BitLength(Part2SQ)
            IF (RemBits == 1) THEN      ! Nothing left but +/- 1?
                IF ((BigIn%Sign < 0).AND.(IAND(Exp, 1) == 1)) THEN
                    BigOut = SHIFTL(-OneApInt64(), Bits2Shift)
                ELSE
                    BigOut = SHIFTL(OneApInt64(), Bits2Shift)
                END IF
                RETURN
            END IF
        ELSE
            RemBits = BitLength(Part2SQ)
            IF (RemBits == 1) THEN  ! Nothing left but +/- 1?
                IF ((BigIn%Sign < 0).AND.(IAND(Exp, 1) == 1)) THEN
                    BigOut = -OneApInt64()
                ELSE
                    BigOut = OneApInt64()
                END IF
                RETURN
            END IF
        END IF

        ! Large number algorithm.  This is basically identical to
        ! the algorithm above, but calls multiply() and square()
        ! which may use more efficient algorithms for large numbers.
        Answer = OneApInt64()
        WrkExp = Exp

        ! Perform exponentiation using repeated squaring trick
        DO WHILE (WrkExp /= 0)
            IF (IAND(WrkExp, 1) == 1) THEN
                Answer = Answer*Part2SQ
            END IF

            WrkExp = SHIFTR(WrkExp, 1)
            IF (WrkExp /= 0) THEN
                Part2SQ = SQR(Part2SQ)
            END IF
        END DO

        ! Multiply back the (exponentiated) powers of two (quickly, by shifting left)
        IF (PowTwo > 0) THEN
            BigOut = SHIFTL(Answer, Bits2Shift)
        ELSE
            BigOut = MakeCopy(Answer)
        END IF

        IF ((BigIn%Sign < 0).AND.(IAND(Exp, 1) == 1)) BigOut%Sign = -BigOut%Sign

        RETURN

    END FUNCTION ApInt64_Power_Java

    !**************************************************************************

    RECURSIVE SUBROUTINE Power_Recur(Base, Exponent, Output)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)       :: Base
        tInteger,      INTENT(IN)       :: Exponent
        TYPE(ApInt64), INTENT(INOUT)    :: Output

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:

        IF (Exponent == 0) THEN
            Output%Digit(0) = 1_kLong
            Output%Length   = 1_kIndex
            Output%Sign     = 1
            RETURN
        ELSEIF (Exponent == 1) THEN
            Output%Digit(0:Base%Length-1) = Base%Digit(0:Base%Length-1)
            Output%Length = Base%Length
            Output%Sign   = Base%Sign
            RETURN
        END IF

        CALL Power_Recur(Base, SHIFTR(Exponent, 1), Output)

        CALL Output%Square()
        IF (MOD(Exponent, 2) /= 0) CALL Output%Multiply(Base)

        RETURN

    END SUBROUTINE Power_Recur

    !**************************************************************************

    FUNCTION BitLength(InVal) RESULT(Length)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return the number of bits in the minimal two's-complement
        ! representation of ApInt64

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: InVal
        tInteger                    :: Length

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Pow2
        tInteger    :: I

    !** FLOW

        IF (InVal%Length == 0) THEN
            Length = 0
            RETURN
        END IF
        ! Calculate the bit length of the magnitude
        Length = SHIFTL(InVal%Length - 1, 6) + (64-LEADZ(InVal%Digit(InVal%Length - 1)))
        IF (InVal%Sign < 0) THEN
            Pow2 = POPCNT(InVal%Digit(InVal%Length - 1)) == 1
            DO I = InVal%Length - 2, 0, -1
                IF (.NOT.Pow2) EXIT
                Pow2 = InVal%Digit(I) == 0_kLong
            END DO
            IF (Pow2) Length = Length - 1
        END IF

        RETURN

    END FUNCTION BitLength

    !**************************************************************************

END FUNCTION ApInt64_Power

!******************************************************************************

MODULE FUNCTION ApInt64_Square(BigIn) RESULT(BigOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return BigOut = BigIn*BigIn

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: BigIn
    TYPE(ApInt64)               :: BigOut

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and returun quickly if possible
    IF (IsZero(BigIn)) THEN
        BigOut = ZeroApInt64()
    ELSEIF (IsOne(BigIn)) THEN
        BigOut = OneApInt64()
    ELSEIF (BigIn%Length <= Karatsuba_Threshold) THEN
        CALL Square_Basic_Alloc(BigIn%Digit, BigIn%Length, BigOut%Digit)
        BigOut%Length = SIZE(BigOut%Digit, KIND=kIndex)
        DO WHILE (BigOut%Digit(BigOut%Length-1) == 0)
            BigOut%Length = BigOut%Length - 1_kIndex
        END DO
        BigOut%Sign = 1
    ELSE
        CALL Square_Karatsuba_Alloc(BigIn%Digit, 0_kIndex, BigIn%Length, BigOut%Digit)
        BigOut%Length = SIZE(BigOut%Digit, KIND=kIndex)
        DO WHILE (BigOut%Digit(BigOut%Length-1) == 0)
            BigOut%Length = BigOut%Length - 1_kIndex
        END DO
        BigOut%Sign = 1
    END IF

    RETURN

END FUNCTION ApInt64_Square

!******************************************************************************

MODULE SUBROUTINE ApInt64_SquareSub(This)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply This by This and return This.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and returun quickly if possible
    IF (This%IsZero()) THEN
        RETURN
    ELSEIF (IsOne(This)) THEN
        RETURN
    ELSEIF (This%Length <= Karatsuba_Threshold) THEN
        CALL Square_Small(This)
    ELSE
        CALL Square_Big(This)
    END IF

    RETURN
    CONTAINS

    SUBROUTINE Square_Small(This)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform squaring of ApInt64 using a quadratic algorithm
        ! which is often suitable for smaller numbers.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: This

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: ThisDig(0:This%Length-1)
        tIndex      :: ThisLen, OutLen

    !** FLOW

        ThisDig = This%Digit
        ThisLen = This%Length
        OutLen = SHIFTL(ThisLen, 1)
        IF (SIZE(This%Digit, KIND=kIndex) < OutLen) THEN
            CALL MemAlloc(This%Digit, OutLen, StartID=0_kIndex)
        END IF
        CALL Square_Basic_NoAlloc(ThisDig, ThisLen, This%Digit)
        This%Length = OutLen
        DO WHILE (This%Digit(This%Length-1) == 0)
            This%Length = This%Length - 1_kIndex
        END DO
        This%Sign = 1

        RETURN

    END SUBROUTINE Square_Small

    !**************************************************************************

    SUBROUTINE Square_Big(This)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform squaring of ApInt32 using a quadratic algorithm
        ! which is often suitable for smaller numbers.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT) :: This

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: ThisDig(0:This%Length-1)
        tIndex      :: ThisLen, OutLen

    !** FLOW

        ThisDig = This%Digit
        ThisLen = This%Length
        OutLen = SHIFTL(This%Length, 1)
        IF (SIZE(This%Digit, KIND=kIndex) < OutLen) THEN
            CALL MemAlloc(This%Digit, OutLen, StartID=0_kIndex)
        END IF
        CALL Square_Karatsuba_NoAlloc(ThisDig, 0_kIndex, ThisLen, This%Digit)
        This%Length = OutLen
        DO WHILE (This%Digit(This%Length-1) == 0)
            This%Length = This%Length - 1_kIndex
        END DO
        This%Sign = 1

        RETURN

    END SUBROUTINE Square_Big

    !**************************************************************************

END SUBROUTINE ApInt64_SquareSub

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             ADDITION SUPPORTING ROUTINES                 ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE UAddMag_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount of the increase (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL UAddMag(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE UAddMag_U32

!******************************************************************************

SUBROUTINE UAddMag_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount of the increase (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Sum, Carry

!** FLOW

    Sum = Big%Digit(0) + U64
    Carry = SHIFTR(IOR(IAND(Big%Digit(0), U64), IAND(IOR(Big%Digit(0), U64), NOT(Sum))), 63)
    Big%Digit(0) = Sum
    IF (Carry /= 0_kLong) THEN
        BLOCK
            tIndex    :: I
            I = 1_kIndex
            DO
                Big%Digit(I) = Big%Digit(I) + 1_kLong
                IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0))) EXIT
                I = I + 1_kIndex
            END DO
            IF (I == Big%Length) THEN
                IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
                    CALL MemResize(Big%Digit, Big%Length*2_kIndex)
                END IF
                Big%Digit(Big%Length) = 1_kLong
                Big%Length = Big%Length + 1
            END IF
        END BLOCK
    END IF

    RETURN

END SUBROUTINE UAddMag_U64

!******************************************************************************

SUBROUTINE UAdd_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the specified integer to the ApInt64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount to be added (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

   CALL UAdd(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE UAdd_U32

!******************************************************************************

SUBROUTINE UAdd_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the specified integer to the ApInt64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount to be added (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Big%Sign < 0) THEN
        IF ((Big%Length > 1_kIndex).OR.(Big%Digit(0).UGT.U64)) THEN
            CALL USubMag(Big, U64)
            RETURN
        END IF
        Big%Sign = 1
        Big%Digit(0) = U64 - Big%Digit(0)
        RETURN
    END IF
    CALL UAddMag(Big, U64)

    RETURN

END SUBROUTINE UAdd_U64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++           SUBTRACTION SUPPORTING ROUTINES                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE USubMag_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of ApInt64 by the specified integer.
    ! If U32 > the magnitude of ApInt64, behavior is undefined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount of the decrease (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL USubMag(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE USubMag_U32

!******************************************************************************

SUBROUTINE USubMag_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of ApInt64 by the specified integer
    ! If U64 > the magnitude of ApInt64, behavior is undefined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount of the decrease (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: Diff, Borrow

!** FLOW

    Diff = Big%Digit(0) - U64
    Borrow = SHIFTR(IOR(IAND(NOT(Big%Digit(0)), U64), &
                        IAND(NOT(IEOR(Big%Digit(0), U64)), Diff)), 63)
    Big%Digit(0) = Diff
    IF (Borrow /= 0_kLong) THEN
        BLOCK
            tIndex    :: I
            I = 1_kIndex
            DO WHILE (Big%Digit(I) == 0)
                Big%Digit(I) = Big%Digit(I) - 1
                I = I + 1_kIndex
            END DO
            Big%Digit(I) = Big%Digit(I) - 1
            IF ((Big%Digit(I) == 0).AND.(I+1_kIndex == Big%Length)) THEN
                Big%Length = Big%Length - 1
            END IF
        END BLOCK
    END IF

    RETURN

END SUBROUTINE USubMag_U64

!******************************************************************************

SUBROUTINE USub_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To subtract the specified integer to the ApInt64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount to be subtracted (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL USub(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE USub_U32

!******************************************************************************

SUBROUTINE USub_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To subtract the specified integer to the ApInt64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount to be subtracted (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Big%Sign < 0) THEN
        CALL UAddMag(Big, U64)
        RETURN
    END IF
    IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0).ULT.U64)) THEN
        Big%Sign = -1
        Big%Digit(0) = U64 - Big%Digit(0)
        RETURN
    END IF
    CALL USubMag(Big, U64)

    RETURN

END SUBROUTINE USub_U64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++          MULTIPLICATION SUPPORTING ROUTINES              ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE UMul_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL UMul(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE UMul_U32

!******************************************************************************

SUBROUTINE UMul_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: Mask32   = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: High, Low
    tLong       :: Carry
    tIndex      :: I
    tLong       :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tLong       :: Lo_Lo, Hi_Lo, Cross

!** FLOW

    IF (U64 == 0) THEN
        Big = ZeroApInt64()
        RETURN
    END IF

    Carry = 0_kLong
    DO I = 0_kIndex, Big%Length-1_kIndex
        ! => CALL UMul128(Big%Digit(I), U64, High, Low)
        X_Lo = IAND(Big%Digit(I), Mask32)
        X_Hi = SHIFTR(Big%Digit(I), 32)
        Y_Lo = IAND(U64, Mask32)
        Y_Hi = SHIFTR(U64, 32)
        Lo_Lo = X_Lo*Y_Lo
        Hi_Lo = X_Hi*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
        High = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
        Low = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

        Big%Digit(I) = Low + Carry
        IF (Big%Digit(I) .ULT. Low) THEN
            Carry = High + 1_kLong
        ELSE
            Carry = High
        END IF
    END DO
    IF (Carry /= 0_kLong) THEN
        IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
            CALL MemResize(Big%Digit, Big%Length*2_kIndex)
        END IF
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1_kIndex
    END IF

    RETURN

END SUBROUTINE UMul_U64

!******************************************************************************

SUBROUTINE Multiply_Basic(X, XLen, Y, YLen, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: XLen             ! The length of the first array
    tLong,  INTENT(IN)  :: X(0:XLen-1)      ! The first magnitude array
    tIndex, INTENT(IN)  :: YLen             ! The length of the second array
    tLong,  INTENT(IN)  :: Y(0:YLen-1)      ! The second magnitude array
    tLong,  INTENT(OUT) :: Z(0:XLen+YLen-1) ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: Mask32   = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry64, ProductHi, ProductLo, Sum
    tIndex      :: I, J
    tLong       :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tLong       :: Lo_Lo, Hi_Lo, Cross

!** FLOW

    Carry64 = 0_kLong
    DO J = 0, YLen-1
        ! compute 128-bit result of multiplication of two 64-bit unsigned integers
        ! => CALL UMul128(X(0), Y(J), ProductHi, ProductLo)
        X_Lo = IAND(X(0), Mask32)
        X_Hi = SHIFTR(X(0), 32)
        Y_Lo = IAND(Y(J), Mask32)
        Y_Hi = SHIFTR(Y(J), 32)
        Lo_Lo = X_Lo*Y_Lo
        Hi_Lo = X_Hi*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
        ProductHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
        ProductLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

        Z(J) = ProductLo + Carry64
        IF (IEOR(Z(J), MinI64) < IEOR(ProductLo, MinI64)) THEN
            Carry64 = ProductHi + 1_kLong
        ELSE
            Carry64 = ProductHi
        END IF
    END DO
    Z(YLen) = Carry64
    DO I = 1, XLen-1
        Carry64 = 0_kLong
        DO J = 0, YLen-1
            ! compute 128-bit result of multiplication of two 64-bit unsigned integers
            ! => CALL UMul128(X(I), Y(J), ProductHi, ProductLo)
            X_Lo = IAND(X(I), Mask32)
            X_Hi = SHIFTR(X(I), 32)
            Y_Lo = IAND(Y(J), Mask32)
            Y_Hi = SHIFTR(Y(J), 32)
            Lo_Lo = X_Lo*Y_Lo
            Hi_Lo = X_Hi*Y_Lo
            Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
            ProductHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
            ProductLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

            Sum = ProductLo + Z(I+J)
            IF (IEOR(Sum, MinI64) < IEOR(ProductLo, MinI64)) ProductHi = ProductHi + 1_kLong
            Z(I+J) = Sum + Carry64
            IF (IEOR(Z(I+J), MinI64) < IEOR(Sum, MinI64)) THEN
                Carry64 = ProductHi + 1_kLong
            ELSE
                Carry64 = ProductHi
            END IF
        END DO
        Z(I+YLen) = Carry64
    END DO

    RETURN

END SUBROUTINE Multiply_Basic

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++        DIVISION/MODULATION SUPPORTING ROUTINES           ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION UDiv_U32(Big, U32) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt64 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The divisor (treated as unsigned)
    tInteger                        :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Rem = ToInteger(UDiv(Big, ToUnsignedLong(U32)))

    RETURN

END FUNCTION UDiv_U32

!******************************************************************************

SUBROUTINE URem_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt64 with the specified integer
    ! (i.e. set Big to MOD(Big, U32)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The divisor or the amount to modulo
                                            ! (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL URem(Big, ToUnsignedLong(U32))

    RETURN

END SUBROUTINE URem_U32

!******************************************************************************

FUNCTION UDiv_U64(Big, U64) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt64 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tLong                           :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: U(0:Big%Length+1)    ! working dividend
    tLong       :: V
    tInteger    :: S, I
    tIndex      :: M

!** FLOW

    M = Big%Length
    U(0:M-1) = Big%Digit(0:M-1)
    U(M)     = 0_kLong
    S = LEADZ(U64)
    V = SHIFTL(U64, S)
    IF (S > 0) THEN
        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF
    CALL UDivRemBy1(U, M+1, V, Rem)
    Big%Digit(0:M-1) = U(0:M-1)
    Rem = SHIFTR(Rem, S)
    IF ((Big%Digit(M-1) == 0_kLong).AND.(M > 1_kIndex)) THEN
        M = M - 1_kIndex
        IF ((Big%Digit(M-1) == 0_kLong).AND.(M > 1_kIndex)) THEN
            M = M - 1_kIndex
        END IF
        Big%Length = M
    END IF

    RETURN

END FUNCTION UDiv_U64

!******************************************************************************

SUBROUTINE URem_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt64 with the specified integer
    ! (i.e. set Big to MOD(Big, U64)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Remainder

!** FLOW

    Remainder = UDiv(Big, U64)
    Big%Length = 1_kIndex
    Big%Digit(0) = Remainder
    IF (Big%Digit(0) == 0_kLong) Big%Sign = 1

    RETURN

END SUBROUTINE URem_U64

!******************************************************************************

FUNCTION Reciprocal_2By1(D) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
    ! based on Algorithm 2 from "Improved division by invariant integers".

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: D
    tLong               :: R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: D0, D9, D40, D63, E, T
    tLong       :: PHi, PLo, V0, V1, V2, V3

!** FLOW

    D9  = SHIFTR(D, 55)
    V0  = ToLong(RecTable(ToInteger(D9) - 256))

    D40 = SHIFTR(D, 24) + 1
    V1  = SHIFTL(V0, 11) - SHIFTR((V0 * V0) * D40, 40) - 1_kLong

    V2  = SHIFTL(V1, 13) + SHIFTR(V1 * (ToLong(Z'1000000000000000') - V1 * D40), 47)

    D0  = IAND(D, 1_kLong)
    D63 = SHIFTR(D, 1) + D0     ! ceil(D/2)
    E   = IAND(SHIFTR(V2, 1), (0_kLong - D0)) - V2 * D63
    CALL UMul128(V2, E, PHi, PLo)
    V3  = SHIFTR(PHi, 1) + SHIFTL(V2, 31)

    CALL UMul128(V3, D, PHi, PLo)
    T = PLo + D
    IF (IEOR(T, MinI64) < IEOR(PLo, MinI64)) PHi = PHi + 1
    R = V3 - PHi - D

    RETURN

END FUNCTION Reciprocal_2By1

!******************************************************************************

SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform 128-bit integer division by 64-bit integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: UHi, ULo, D, V
    tLong, INTENT(OUT)  :: Q, R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: QHi, QLo, NewLo

!** FLOW

    ! Q128 = V*UHi
    CALL UMul128(V, UHi, QHi, QLo)

    ! Q128 = Q128 + U128
    NewLo = QLo + ULo
    IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
        QHi = QHi + UHi + 1_kLong
    ELSE
        QHi = QHi + UHi
    END IF
    QLo = NewLo

    QHi = QHi + 1_kLong

    R = ULo - QHi*D

    IF (IEOR(R, MinI64) > IEOR(QLo, MinI64)) THEN
        QHi = QHi - 1_kLong
        R = R + D
    END IF

    IF (IEOR(R, MinI64) >= IEOR(D, MinI64)) THEN
        QHi = QHi + 1_kLong
        R = R - D
    END IF
    Q = QHi

    RETURN

END SUBROUTINE UDivRem_2By1

!******************************************************************************

SUBROUTINE UDivRemBy1(U, ULen, D, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform arbitrary-precision unsigned integer division by 64-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: ULen         ! length of U
    tLong,  INTENT(INOUT)   :: U(0:ULen-1)  ! on entry, the dividend
                                            ! on exit, the quotient
    tLong,  INTENT(IN)      :: D            ! the divisor
    tLong,  INTENT(OUT)     :: R            ! the remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Reciprocal
    tLong       :: UHi, ULo
    tIndex      :: I

!** FLOW

    IF (ULen < 2) THEN
        CALL Handle_ErrLevel('UDivRemBy1', SubName, ErrSevere, 'ULen must be at least 2.')
        RETURN
    END IF

    Reciprocal = Reciprocal_2By1(D)

    UHi = U(ULen - 1)       ! Set the top word as remainder.
    U(ULen - 1) = 0_kLong   ! Reset the word being a part of the resulting quotient.

    I = ULen - 2
    DO
        ULo = U(I)
        CALL UDivRem_2By1(UHi, ULo, D, Reciprocal, U(I), R)
        UHi = R
        IF (I == 0) EXIT
        I = I - 1
    END DO

    RETURN

END SUBROUTINE UDivRemBy1

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 SQUARE SUPPORTING ROUTINES               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Square_Basic_Alloc(X, XLen, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring of the input array (i.e. multiply the input array by itself)
    !
    ! The algorithm used here is adapted from Colin Plumb's C library.
    ! Technique: Consider the partial products in the multiplication
    ! of "abcde" by itself:
    !
    !               a  b  c  d  e
    !            *  a  b  c  d  e
    !          ==================
    !              ae be ce de ee
    !           ad bd cd dd de
    !        ac bc cc cd ce
    !     ab bb bc bd be
    !  aa ab ac ad ae
    !
    ! Note that everything above the main diagonal:
    !              ae be ce de = (abcd) * e
    !           ad bd cd       = (abc) * d
    !        ac bc             = (ab) * c
    !     ab                   = (a) * b
    !
    ! is a copy of everything below the main diagonal:
    !                       de
    !                 cd ce
    !           bc bd be
    !     ab ac ad ae
    !
    ! Thus, the sum is 2 * (off the diagonal) + diagonal.
    !
    ! This is accumulated beginning with the diagonal (which
    ! consist of the squares of the digits of the input), which is then
    ! divided by two, the off-diagonal added, and multiplied by two
    ! again.  The low bit is simply a copy of the low bit of the
    ! input, so it doesn't need special care.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: XLen         ! The length of the array
    tLong,              INTENT(IN)  :: X(0:XLen-1)  ! The input array
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)         ! The output array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use 'Multiplication' algorithm
    tIndex, PARAMETER   :: Multiply_Threshold = 16

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: LastProductLowWord, K, Carry
    tLong           :: ProdHi, ProdLo
    tIndex          :: I, J, Offset, ZLen
    tLong           :: Y(0:XLen-1)

!** FLOW

    ! use multiplication algorithm for small number
    IF (XLen <= Multiply_Threshold) THEN
        CALL MemAlloc(Z, SHIFTL(XLen, 1), StartID=0_kIndex)
        CALL Multiply_Basic(X, XLen, X, XLen, Z)
        RETURN
    END IF

    Y = X
    CALL Reverse_Order(Y, 0_kIndex, XLen-1_kIndex)

    ! allocate storage of the output and set all elements to zero
    ZLen = SHIFTL(XLen, 1)
    CALL MemAlloc(Z, ZLen, StartID=0_kIndex)
    Z = 0_kLong

    ! Store the squares, right shifted one bit (i.e., divided by 2)
    LastProductLowWord = 0_kLong
    I = 0
    DO J = 0, XLen - 1
        CALL Sqr_U64(Y(J), ProdHi, ProdLo)
        Z(I) = IOR(SHIFTL(LastProductLowWord, 63), SHIFTR(ProdHi, 1))
        I = I + 1
        Z(I) = IOR(SHIFTR(ProdLo, 1), SHIFTL(SHIFTL(ProdHi, 1), 62))
        I = I + 1
        LastProductLowWord = ProdLo
    END DO

    ! Add in off-diagonal sums
    Offset = 1
    DO I = XLen, 1, -1
        K = Y(I-1)
        CALL MultiplyOneWord(Z, Y, Offset, I-1, K, Carry)
        CALL AddOneWord(Z, Offset-1, I, Carry)
        Offset = Offset + 2
    END DO
    CALL Reverse_Order(Z, 0_kIndex, ZLen-1_kIndex)

    ! Shift back up and set low bit
    CALL ShiftLeft(Z, ZLen, 1)
    Z(0) = IOR(Z(0), IAND(X(0), 1_kLong))

    RETURN

    CONTAINS

    SUBROUTINE ShiftLeft(X, XLen, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt64 number left by the given amount (less than 32) starting
        ! at the given digit, i.e. the first (<len) digits are left untouched.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, ALLOCATABLE, INTENT(INOUT)   :: X(:)
        tIndex,             INTENT(INOUT)   :: XLen
        tInteger,           INTENT(IN)      :: Shift  ! The amount to shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: YLen
        tLong       :: Y(0:XLen-1)
        tLong       :: Nxt
        tIndex      :: I

    !** FLOW

        ! get input information
        YLen = XLen
        Y = X
        IF (XLen > YLen) THEN
            Nxt = 0
        ELSE
            Nxt = Y(XLen-1)
        END IF
        DO I = XLen-1, 1, -1
            X(I) = IOR(SHIFTL(Nxt, Shift), SHIFTR(Y(I-1), 64-Shift))
            Nxt = Y(I-1)
        END DO
        X(0) = SHIFTL(Nxt, Shift)

        RETURN

    END SUBROUTINE ShiftLeft

    !**************************************************************************

    SUBROUTINE Reverse_Order(A, IStart, IEnd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To reverse order of a segment of an array in place

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: A(0:)    ! array to be reverse-ordered
        tIndex, INTENT(IN)      :: IStart   ! starting index (inclusive)
        tIndex, INTENT(IN)      :: IEnd     ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Temp
        tIndex      :: Lo
        tIndex      :: Hi

    !** FLOW:

        Lo = IStart
        Hi = IEnd
        DO WHILE (Lo < Hi)
            Temp = A(Lo)
            A(Lo) = A(Hi)
            A(Hi) = Temp
            Lo = Lo + 1
            Hi = Hi - 1
        END DO

        RETURN

    END SUBROUTINE Reverse_Order

    !**************************************************************************

    SUBROUTINE Sqr_U64(InVal, OutHi, OutLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication of two SInt128 objects (Lhs * Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: InVal
        tLong, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: Mask32 = ToLong(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: X_Lo, X_Hi
        tLong       :: Hi_Lo, Cross

    !** FLOW

        OutLo = InVal * InVal
        X_Lo = IAND(InVal, Mask32)
        X_Hi = SHIFTR(InVal, 32)
        Hi_Lo = X_Hi*X_Lo
        Cross = SHIFTR(X_Lo*X_Lo, 32) + IAND(Hi_Lo, Mask32) + Hi_Lo
        OutHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*X_Hi

        RETURN

    END SUBROUTINE Sqr_U64

    !**************************************************************************

    SUBROUTINE Mul_U64byU64(LhsVal, RhsVal, OutHi, OutLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication of two SInt128 objects (Lhs * Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LhsVal, RhsVal
        tLong, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsVal * RhsVal
        OutHi = UMul128_Upper64(LhsVal, RhsVal)

        RETURN

    END SUBROUTINE Mul_U64byU64

    !**************************************************************************

    SUBROUTINE MultiplyOneWord(OutVal, InVal, Offset, InLen, K, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply an array by one word K and add to result, return the carry

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(OUT) :: OutVal(0:)   ! output array
        tLong,  INTENT(IN)  :: InVal(0:)    ! input array
        tIndex, INTENT(IN)  :: Offset       ! offset to the output array
        tIndex, INTENT(IN)  :: InLen        ! length of the input array
        tLong,  INTENT(IN)  :: K            ! multiplier
        tLong,  INTENT(OUT) :: Carry        ! carry

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong           :: PHi, PLo
        tIndex          :: I, Indx

    !** FLOW

        Carry = 0_kLong
        Indx = SIZE(OutVal, KIND=kIndex) - Offset - 1
        DO I = InLen-1, 0, -1
            CALL Mul_U64byU64(InVal(I), K, PHi, PLo)
            CALL Add_I128byU64(PHi, PLo, OutVal(Indx))
            CALL Add_I128byU64(PHi, PLo, Carry)
            OutVal(Indx) = PLo
            Carry = PHi
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE MultiplyOneWord

    !**************************************************************************

    SUBROUTINE AddOneWord(Val, Offset, InLen, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To add one word to an array of mlen words

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: Val(0:)      ! input/output array
        tIndex, INTENT(IN)      :: Offset       ! offset to the array
        tIndex, INTENT(IN)      :: InLen        ! offset length to the array
        tLong,  INTENT(IN)      :: Add          ! one word to be added


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Indx, MLen
        tLong       :: Sum, Carry

    !** FLOW

        Indx = SIZE(Val, KIND=kIndex) - 1_kIndex - InLen - Offset
        MLen = InLen
        Carry = 0_kLong
        CALL AddU64(Val(Indx), Add, Carry, Sum)
        Val(Indx) = Sum
        IF (Carry == 0_kLong) RETURN
        DO WHILE (MLen > 0_kIndex)
            MLen = MLen - 1_kIndex
            Indx = Indx - 1_kIndex
            IF (Indx < 0_kIndex) EXIT
            Val(Indx) = Val(Indx) + 1_kLong
            IF (Val(Indx) /= 0_kLong) EXIT
        END DO

        RETURN

    END SUBROUTINE AddOneWord

    !**************************************************************************

END SUBROUTINE Square_Basic_Alloc

!******************************************************************************

RECURSIVE SUBROUTINE Square_Karatsuba_Alloc(X, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring of a partial magnitude array X(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 48

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: B, C
    tLong, ALLOCATABLE  :: X2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN
        ! Basecase
        CALL Square_Basic_Alloc(X(Off:), N, Z)
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Square_Karatsuba_Alloc(X, Off+B, N-B, Z2)
    CALL Square_Karatsuba_Alloc(X, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    ! set X2
    CALL AddHiLoParts(X, N, B, Off, X2)

    C = 0_kIndex
    IF (X2(N-B) /= 0_kLong) C = 1_kIndex
    CALL Square_Karatsuba_Alloc(X2, 0_kIndex, N-B+C, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Square_Karatsuba_Alloc

!******************************************************************************

SUBROUTINE Square_Basic_NoAlloc(X, XLen, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring of the input array (i.e. multiply the input array by itself)
    !
    ! The algorithm used here is adapted from Colin Plumb's C library.
    ! Technique: Consider the partial products in the multiplication
    ! of "abcde" by itself:
    !
    !               a  b  c  d  e
    !            *  a  b  c  d  e
    !          ==================
    !              ae be ce de ee
    !           ad bd cd dd de
    !        ac bc cc cd ce
    !     ab bb bc bd be
    !  aa ab ac ad ae
    !
    ! Note that everything above the main diagonal:
    !              ae be ce de = (abcd) * e
    !           ad bd cd       = (abc) * d
    !        ac bc             = (ab) * c
    !     ab                   = (a) * b
    !
    ! is a copy of everything below the main diagonal:
    !                       de
    !                 cd ce
    !           bc bd be
    !     ab ac ad ae
    !
    ! Thus, the sum is 2 * (off the diagonal) + diagonal.
    !
    ! This is accumulated beginning with the diagonal (which
    ! consist of the squares of the digits of the input), which is then
    ! divided by two, the off-diagonal added, and multiplied by two
    ! again.  The low bit is simply a copy of the low bit of the
    ! input, so it doesn't need special care.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: XLen         ! The length of the array
    tLong,  INTENT(IN)  :: X(0:XLen-1)  ! The input array
    tLong,  INTENT(OUT) :: Z(0:)        ! The output array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use 'Multiplication' algorithm
    tIndex, PARAMETER   :: Multiply_Threshold = 16

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: LastProductLowWord, K, Carry
    tLong           :: ProdHi, ProdLo
    tIndex          :: I, J, Offset, ZLen
    tLong           :: Y(0:XLen-1)

!** FLOW

    ! use multiplication algorithm for small number
    IF (XLen <= Multiply_Threshold) THEN
        CALL Multiply_Basic(X, XLen, X, XLen, Z)
        RETURN
    END IF

    Y = X
    CALL Reverse_Order(Y, 0_kIndex, XLen-1_kIndex)

    ! allocate storage of the output and set all elements to zero
    ZLen = 2*XLen
    Z = 0_kLong

    ! Store the squares, right shifted one bit (i.e., divided by 2)
    LastProductLowWord = 0_kLong
    I = 0
    DO J = 0, XLen - 1
        CALL Sqr_U64(Y(J), ProdHi, ProdLo)
        Z(I) = IOR(SHIFTL(LastProductLowWord, 63), SHIFTR(ProdHi, 1))
        I = I + 1
        Z(I) = IOR(SHIFTR(ProdLo, 1), SHIFTL(SHIFTL(ProdHi, 1), 62))
        I = I + 1
        LastProductLowWord = ProdLo
    END DO

    ! Add in off-diagonal sums
    Offset = 1
    DO I = XLen, 1, -1
        K = Y(I-1)
        CALL MultiplyOneWord(Z, ZLen, Y, Offset, I-1, K, Carry)
        CALL AddOneWord(Z, ZLen, Offset-1, I, Carry)
        Offset = Offset + 2
    END DO
    CALL Reverse_Order(Z, 0_kIndex, ZLen-1_kIndex)

    ! Shift back up and set low bit
    CALL ShiftLeft(Z, ZLen, 1)
    Z(0) = IOR(Z(0), IAND(X(0), 1_kLong))

    RETURN

    CONTAINS

    SUBROUTINE ShiftLeft(X, XLen, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt64 number left by the given amount (less than 32) starting
        ! at the given digit, i.e. the first (<len) digits are left untouched.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: X(0:)
        tIndex,   INTENT(IN)    :: XLen
        tInteger, INTENT(IN)    :: Shift  ! The amount to shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I

    !** FLOW

        DO I = XLen-1, 1, -1
            X(I) = IOR(SHIFTL(X(I), Shift), SHIFTR(X(I-1), 64-Shift))
        END DO
        X(0) = SHIFTL(X(0), Shift)

        RETURN

    END SUBROUTINE ShiftLeft

    !**************************************************************************

    SUBROUTINE Reverse_Order(A, IStart, IEnd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To reverse order of a segment of an array in place

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: A(0:)    ! array to be reverse-ordered
        tIndex, INTENT(IN)      :: IStart   ! starting index (inclusive)
        tIndex, INTENT(IN)      :: IEnd     ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Temp
        tIndex      :: Lo
        tIndex      :: Hi

    !** FLOW:

        Lo = IStart
        Hi = IEnd
        DO WHILE (Lo < Hi)
            Temp = A(Lo)
            A(Lo) = A(Hi)
            A(Hi) = Temp
            Lo = Lo + 1
            Hi = Hi - 1
        END DO

        RETURN

    END SUBROUTINE Reverse_Order

    !**************************************************************************

    SUBROUTINE Sqr_U64(InVal, OutHi, OutLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication of two SInt128 objects (Lhs * Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: InVal
        tLong, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: Mask32 = ToLong(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: X_Lo, X_Hi
        tLong       :: Hi_Lo, Cross

    !** FLOW

        OutLo = InVal * InVal
        X_Lo = IAND(InVal, Mask32)
        X_Hi = SHIFTR(InVal, 32)
        Hi_Lo = X_Hi*X_Lo
        Cross = SHIFTR(X_Lo*X_Lo, 32) + IAND(Hi_Lo, Mask32) + Hi_Lo
        OutHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*X_Hi

        RETURN

    END SUBROUTINE Sqr_U64

    !**************************************************************************

    SUBROUTINE Mul_U64byU64(LhsVal, RhsVal, OutHi, OutLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication of two SInt128 objects (Lhs * Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LhsVal, RhsVal
        tLong, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsVal * RhsVal
        OutHi = UMul128_Upper64(LhsVal, RhsVal)

        RETURN

    END SUBROUTINE Mul_U64byU64

    !**************************************************************************

    SUBROUTINE MultiplyOneWord(OutVal, OutLen, InVal, Offset, InLen, K, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply an array by one word K and add to result, return the carry

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(OUT) :: OutVal(0:)   ! output array
        tIndex, INTENT(IN)  :: OutLen       ! length of the output array
        tLong,  INTENT(IN)  :: InVal(0:)    ! input array
        tIndex, INTENT(IN)  :: Offset       ! offset to the output array
        tIndex, INTENT(IN)  :: InLen        ! length of the input array
        tLong,  INTENT(IN)  :: K            ! multiplier
        tLong,  INTENT(OUT) :: Carry        ! carry

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong           :: PHi, PLo
        tIndex          :: I, Indx

    !** FLOW

        Carry = 0_kLong
        Indx = OutLen - Offset - 1
        DO I = InLen-1, 0, -1
            CALL Mul_U64byU64(InVal(I), K, PHi, PLo)
            CALL Add_I128byU64(PHi, PLo, OutVal(Indx))
            CALL Add_I128byU64(PHi, PLo, Carry)
            OutVal(Indx) = PLo
            Carry = PHi
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE MultiplyOneWord

    !**************************************************************************

    SUBROUTINE AddOneWord(Val, OutLen, Offset, InLen, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To add one word to an array of mlen words

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: Val(0:)      ! input/output array
        tIndex, INTENT(IN)      :: OutLen       ! length of the output array
        tIndex, INTENT(IN)      :: Offset       ! offset to the array
        tIndex, INTENT(IN)      :: InLen        ! offset length to the array
        tLong,  INTENT(IN)      :: Add          ! one word to be added


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Indx, MLen
        tLong       :: Sum, Carry

    !** FLOW

        Indx = OutLen - 1_kIndex - InLen - Offset
        MLen = InLen
        Carry = 0_kLong
        CALL AddU64(Val(Indx), Add, Carry, Sum)
        Val(Indx) = Sum
        IF (Carry == 0_kLong) RETURN
        DO WHILE (MLen > 0_kIndex)
            MLen = MLen - 1_kIndex
            Indx = Indx - 1_kIndex
            IF (Indx < 0_kIndex) EXIT
            Val(Indx) = Val(Indx) + 1_kLong
            IF (Val(Indx) /= 0_kLong) EXIT
        END DO

        RETURN

    END SUBROUTINE AddOneWord

    !**************************************************************************

END SUBROUTINE Square_Basic_NoAlloc

!******************************************************************************

SUBROUTINE Square_Karatsuba_NoAlloc(X, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring of a partial magnitude array X(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: X(0:)    ! The first magnitude array
    tIndex, INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex, INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tLong,  INTENT(OUT) :: Z(0:)    ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 48

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: B, C
    tLong, ALLOCATABLE  :: X2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN
        ! Basecase
        CALL Square_Basic_NoAlloc(X(Off:), N, Z)
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Square_Karatsuba_Alloc(X, Off+B, N-B, Z2)
    CALL Square_Karatsuba_Alloc(X, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    ! set X2
    CALL AddHiLoParts(X, N, B, Off, X2)

    C = 0_kIndex
    IF (X2(N-B) /= 0_kLong) C = 1_kIndex
    CALL Square_Karatsuba_Alloc(X2, 0_kIndex, N-B+C, Z1)

    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Square_Karatsuba_NoAlloc

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             KARATSUBA SUPPORTING ROUTINES                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AddHiLoParts(X1, L, K, Offset, X2)

!DIR$ ATTRIBUTES INLINE :: AddHiLoParts

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the high and low parts of the input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: X1(0:)
    tIndex, INTENT(IN)  :: L
    tIndex, INTENT(IN)  :: K
    tIndex, INTENT(IN)  :: Offset
    tLong,  INTENT(OUT) :: X2(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: CHi, CLo
    tIndex      :: I, OffK

!** FLOW

    OffK = Offset+K
    X2(0) = X1(OffK) + X1(Offset)
    CHi = SHIFTR(IOR(IAND(X1(OffK), X1(Offset)), IAND(IOR(X1(OffK), X1(Offset)), NOT(X2(0)))), 63)
    CLo = CHi
    CHi = 0_kLong
    DO I = 1, K-1
        CALL Add_U64byU64(CHi, CLo, X1(OffK+I))
        CALL Add_I128byU64(CHi, CLo, X1(Offset+I))
        X2(I) = CLo
        CLo = CHi
        CHi = 0_kLong
    END DO
    IF (IAND(L, 1_kIndex) /= 0_kIndex) X2(K) = X1(OffK+K)
    IF (CLo /= 0_kLong) THEN
        X2(K) = X2(K) + 1_kLong
        IF (X2(K) == 0_kLong) X2(K+1) = X2(K+1) + 1_kLong
    END IF

    RETURN
    CONTAINS

    SUBROUTINE Add_U64byU64(LhsHi, LhsLo, RhsVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition of 64-bit integers resulting in a 128-bit integer
        ! => (LhsHi, LhsLo) = LhsLo + RhsVal

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: LhsLo
        tLong, INTENT(OUT)      :: LhsHi
        tLong, INTENT(IN)       :: RhsVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: OutLo

    !** FLOW

        OutLo = LhsLo + RhsVal
        LhsHi = SHIFTR(IOR(IAND(LhsLo, RhsVal), IAND(IOR(LhsLo, RhsVal), NOT(OutLo))), 63)
        LhsLo = OutLo

        RETURN

    END SUBROUTINE Add_U64byU64

    !**************************************************************************

END SUBROUTINE AddHiLoParts

!******************************************************************************

SUBROUTINE AddThreeParts(Z0, Z1, Z2, N, B, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: Z0(0:)
    tLong,  INTENT(IN)  :: Z1(0:)
    tLong,  INTENT(IN)  :: Z2(0:)
    tIndex, INTENT(IN)  :: N
    tIndex, INTENT(IN)  :: B
    tLong,  INTENT(OUT) :: Z(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: CHi, CLo
    tIndex      :: I, C

!** FLOW

    C = 2*B
    ! Add Z0
    Z(0:C-1)   = Z0(0:C-1)
    ! Add Z2
    Z(C:2*N-1) = Z2(0:2*(N-B)-1)
    ! Add Z1
    CLo = Z(B) + Z1(0)
    CHi = SHIFTR(IOR(IAND(Z(B), Z1(0)), IAND(IOR(Z(B), Z1(0)), NOT(CLo))), 63)
    CALL Subtract_I128byU64(CHi, CLo, Z2(0))
    CALL Subtract_I128byU64(CHi, CLo, Z0(0))
    Z(B) = CLo
    CLo = CHi
    CHi = SHIFTA(CHi, 63)
    I = 1
    DO WHILE (I < C)
        CALL Add_I128byU64(CHi, CLo, Z(I+B))
        CALL Add_I128byU64(CHi, CLo, Z1(I))
        CALL Subtract_I128byU64(CHi, CLo, Z2(I))
        CALL Subtract_I128byU64(CHi, CLo, Z0(I))
        Z(I+B) = CLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < 2*(N-B))
        CALL Add_I128byU64(CHi, CLo, Z(I+B))
        CALL Add_I128byU64(CHi, CLo, Z1(I))
        CALL Subtract_I128byU64(CHi, CLo, Z2(I))
        Z(I+B) = CLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < SIZE(Z1, KIND=kIndex))
        CALL Add_I128byU64(CHi, CLo, Z(I+B))
        CALL Add_I128byU64(CHi, CLo, Z1(I))
        Z(I+B) = CLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    IF ((CHi /= 0_kLong).OR.(CLo /= 0_kLong)) THEN
        Z(I+B) = Z(I+B) + 1_kLong
        DO WHILE (Z(I+B) == 0_kLong)
            I = I + 1
            Z(I+B) = Z(I+B) + 1_kLong
        END DO
    END IF

    RETURN

END SUBROUTINE AddThreeParts

!******************************************************************************

SUBROUTINE Add_I128byU64(LhsHi, LhsLo, RhsVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform and addition of a 64-bit integer to a 128-bit integer
    ! => (LhsHi, LhsLo) = (LhsHi, LhsLo) + (0_kLong, RhsVal)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: LhsHi, LhsLo
    tLong, INTENT(IN)       :: RhsVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: OutLo

!** FLOW

    OutLo = LhsLo + RhsVal
    LhsHi = LhsHi + SHIFTR(IOR(IAND(LhsLo, RhsVal), IAND(IOR(LhsLo, RhsVal), NOT(OutLo))), 63)
    LhsLo = OutLo

    RETURN

END SUBROUTINE Add_I128byU64

!******************************************************************************

SUBROUTINE Subtract_I128byU64(LhsHi, LhsLo, RhsVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction of 64-bit integer from 128-bit integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: LhsHi, LhsLo
    tLong, INTENT(IN)       :: RhsVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: OutLo

!** FLOW

    OutLo = LhsLo - RhsVal
    LhsHi = LhsHi - SHIFTR(IOR(IAND(NOT(LhsLo), RhsVal), IAND(NOT(IEOR(LhsLo, RhsVal)), OutLo)), 63)
    LhsLo = OutLo

    RETURN

END SUBROUTINE Subtract_I128byU64

!******************************************************************************

END SUBMODULE SubClass_Api64_Arithmetic

!******************************************************************************

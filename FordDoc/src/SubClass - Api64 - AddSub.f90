
SUBMODULE (Class_ApInt64 : SubClass_Api64_Arithmetic) SubClass_Api64_AddSub

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to addition and
!   subtraction operations of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_AddSub'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** GENERIC DECLARATIONS:
    INTERFACE  AddMag
        MODULE PROCEDURE AddMag_ApInt64
    END INTERFACE
    INTERFACE  SubMag
        MODULE PROCEDURE SubMag_ApInt64
    END INTERFACE
    INTERFACE  AddOrSub
        MODULE PROCEDURE AddOrSub_ApInt64
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt64_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return result of the unary plus sign of the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: InVal
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(InVal)

    RETURN

END FUNCTION ApInt64_UnaryPlus

!******************************************************************************

MODULE SUBROUTINE ApInt64_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase value of the input by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%IsZero()) THEN
        Val%Digit(0) = 1_kLong
        Val%Sign = 1
    ELSE
        CALL UAdd(Val, 1)
    END IF

    RETURN

END SUBROUTINE ApInt64_Increment

!******************************************************************************

MODULE SUBROUTINE ApInt64_Add_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tInteger,       INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) THEN
        CALL AssignSigned(This, Other)
    ELSE
        IF (Other < 0) THEN
            CALL USub(This, -Other)
        ELSE
            CALL UAdd(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Add_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_Add_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tLong,          INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) THEN
        CALL AssignSigned(This, Other)
    ELSE
        IF (Other < 0_kLong) THEN
            CALL USub(This, -Other)
        ELSE
            CALL UAdd(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Add_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_Add_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Add(ApInt64(Other))

    RETURN

END SUBROUTINE ApInt64_Add_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_Add_ApInt64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(ApInt64),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either this or other is zero
    IF (This%IsZero()) THEN
        ! This = Other
        IF (ALLOCATED(Other%Digit)) THEN
            This%Sign   = Other%Sign
            This%Length = Other%Length
            IF (This%Length > 1_kIndex) CALL MemAlloc(This%Digit, This%Length, StartID=0_kIndex)
            This%Digit(0:This%Length-1) = Other%Digit(0:This%Length-1)
        END IF
        RETURN
    ELSEIF (IsZero(Other)) THEN
        ! simply return
        RETURN
    END IF

    IF (This%Sign == Other%Sign) THEN
        CALL AddMag(This, Other%Digit, Other%Length)
        RETURN
    END IF
    IF (CompareAbs(This, Other) >= 0) THEN
        CALL SubMag(This, Other%Digit, Other%Length)
        RETURN
    END IF
    CALL AddOrSub(This, Other)

    RETURN

END SUBROUTINE ApInt64_Add_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Plus_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I32)

    RETURN

END FUNCTION ApInt64_Plus_I32

!******************************************************************************

MODULE FUNCTION I32_Plus_ApInt64(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I32 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I32)

    RETURN

END FUNCTION I32_Plus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Plus_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I64)

    RETURN

END FUNCTION ApInt64_Plus_I64

!******************************************************************************

MODULE FUNCTION I64_Plus_ApInt64(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I64 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I64)

    RETURN

END FUNCTION I64_Plus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Plus_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I128)

    RETURN

END FUNCTION ApInt64_Plus_I128

!******************************************************************************

MODULE FUNCTION I128_Plus_ApInt64(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I128 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I128)

    RETURN

END FUNCTION I128_Plus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Plus_ApInt64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition: OutVal = LhsVal + RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: OutSize

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt64()
        ELSE
            ! LhsVal is zero
            OutVal = RhsVal
        END IF
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        ! RhsVal is zero
        OutVal = LhsVal
        RETURN
    END IF

    IF (LhsVal%Length < RhsVal%Length) THEN
        OutSize = RhsVal%Length + 1
    ELSE
        OutSize = LhsVal%Length + 1
    END IF
    OutVal%Sign   = LhsVal%Sign
    OutVal%Length = LhsVal%Length
    CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
    OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
    IF (OutVal%Sign == RhsVal%Sign) THEN
        CALL AddMag(OutVal, RhsVal%Digit, RhsVal%Length)
        RETURN
    END IF
    IF (CompareAbs(OutVal, RhsVal) >= 0) THEN
        CALL SubMag(OutVal, RhsVal%Digit, RhsVal%Length)
        RETURN
    END IF
    CALL AddOrSub(OutVal, RhsVal)

    RETURN

END FUNCTION ApInt64_Plus_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt64_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the negation of the input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: InVal
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(InVal)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION ApInt64_Negate

!******************************************************************************

MODULE SUBROUTINE ApInt64_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease value of the input by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%IsZero()) THEN
        Val%Digit(0) = 1_kLong
        Val%Sign = -1
    ELSE
        CALL USub(Val, 1)
    END IF

    RETURN

END SUBROUTINE ApInt64_Decrement

!******************************************************************************

MODULE SUBROUTINE ApInt64_Subtract_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tInteger,       INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) THEN
        CALL AssignSigned(This, -Other)
    ELSE
        IF (Other < 0) THEN
            CALL UAdd(This, -Other)
        ELSE
            CALL USub(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Subtract_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_Subtract_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tLong,          INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) THEN
        CALL AssignSigned(This, -Other)
    ELSE
        IF (Other < 0_kLong) THEN
            CALL UAdd(This, -Other)
        ELSE
            CALL USub(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Subtract_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_Subtract_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Subtract(ApInt64(Other))

    RETURN

END SUBROUTINE ApInt64_Subtract_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_Subtract_ApInt64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(ApInt64),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either this or other is zero
    IF (This%IsZero()) THEN
        ! This = -Other
        IF (ALLOCATED(Other%Digit)) THEN
            This%Sign   = -Other%Sign
            This%Length = Other%Length
            IF (This%Length > 1_kIndex) CALL MemAlloc(This%Digit, This%Length, StartID=0_kIndex)
            This%Digit(0:This%Length-1) = Other%Digit(0:This%Length-1)
        END IF
        RETURN
    ELSEIF (IsZero(Other)) THEN
        ! simply return
        RETURN
    END IF

    IF (This%Sign /= Other%Sign) THEN
        CALL AddMag(This, Other%Digit, Other%Length)
        RETURN
    END IF
    IF (CompareAbs(This, Other) >= 0) THEN
        CALL SubMag(This, Other%Digit, Other%Length)
        RETURN
    END IF
    CALL AddOrSub(This, Other)

    RETURN

END SUBROUTINE ApInt64_Subtract_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Minus_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I32)

    RETURN

END FUNCTION ApInt64_Minus_I32

!******************************************************************************

MODULE FUNCTION I32_Minus_ApInt64(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I32 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I32)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I32_Minus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Minus_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I64)

    RETURN

END FUNCTION ApInt64_Minus_I64

!******************************************************************************

MODULE FUNCTION I64_Minus_ApInt64(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I64 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I64)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I64_Minus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Minus_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I128)

    RETURN

END FUNCTION ApInt64_Minus_I128

!******************************************************************************

MODULE FUNCTION I128_Minus_ApInt64(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I128 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I128)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I128_Minus_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Minus_ApInt64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction: OutVal = LhsVal - RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: OutSize

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt64()
        ELSE
            ! LhsVal is zero
            OutVal = -RhsVal
        END IF
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        ! RhsVal is zero
        OutVal = LhsVal
        RETURN
    END IF

    IF (LhsVal%Length < RhsVal%Length) THEN
        OutSize = RhsVal%Length + 1
    ELSE
        OutSize = LhsVal%Length + 1
    END IF
    OutVal%Sign   = LhsVal%Sign
    OutVal%Length = LhsVal%Length
    CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
    OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
    IF (OutVal%Sign /= RhsVal%Sign) THEN
        CALL AddMag(OutVal, RhsVal%Digit, RhsVal%Length)
        RETURN
    END IF
    IF (CompareAbs(OutVal, RhsVal) >= 0) THEN
        CALL SubMag(OutVal, RhsVal%Digit, RhsVal%Length)
        RETURN
    END IF
    CALL AddOrSub(OutVal, RhsVal)

    RETURN

END FUNCTION ApInt64_Minus_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AddOrSub_ApInt64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: This
    TYPE(ApInt64), INTENT(IN)       :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tLong       :: Borrow
    tLong       :: Diff

!** FLOW

    IF (SIZE(This%Digit, KIND=kIndex) < Other%Length) THEN
        CALL MemResize(This%Digit, Other%Length + 1_kIndex)
    END IF
    This%Sign = -This%Sign
    Borrow = 0_kLong
    I = 0
    DO WHILE (I < This%Length)
        CALL SubU64(Other%Digit(I), This%Digit(I), Borrow, Diff)
        This%Digit(I) = Diff
        I = I + 1
    END DO
    IF (Other%Length > This%Length)THEN
        This%Digit(This%Length:Other%Length-1) = Other%Digit(This%Length:Other%Length-1)
        This%Length = Other%Length
    END IF
    IF (Borrow /= 0_kLong) THEN
        DO WHILE ((I < Other%Length).AND.(This%Digit(I) == 0_kLong))
            This%Digit(I) = This%Digit(I) - 1_kLong
            I = I + 1
        END DO
        This%Digit(I) = This%Digit(I) - 1_kLong
        IF ((This%Digit(I) == 0_kLong).AND.(I+1_kIndex == This%Length)) THEN
            This%Length = This%Length - 1_kIndex
        END IF
    END IF
    ! IF (I == Other%Length) should be impossible

    RETURN

END SUBROUTINE AddOrSub_ApInt64

!******************************************************************************

SUBROUTINE AddMag_ApInt64(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ULen, VLen
    tLong       :: Carry, Sum

!** FLOW

    ! allocate and assign U and V
    IF (InpLen < Big%Length) THEN
        ULen = InpLen
        VLen = Big%Length
    ELSE
        ULen = Big%Length
        VLen = InpLen
    END IF
    IF (VLen > SIZE(Big%Digit, KIND=kIndex)) THEN
        CALL MemResize(Big%Digit, VLen+1_kIndex)
    END IF
    Carry = 0_kLong
    I = 0
    DO WHILE (I < ULen)
        CALL AddU64(Big%Digit(I), Inp(I), Carry, Sum)
        Big%Digit(I) = Sum
        I = I + 1
    END DO
    IF (VLen > Big%Length)THEN
        Big%Digit(Big%Length:VLen-1) = Inp(Big%Length:VLen-1)
        Big%Length = VLen
    END IF
    IF (Carry /= 0_kLong) THEN            ! Carry == 1
        DO WHILE (I < Big%Length)
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (Big%Digit(I) /= 0_kLong) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN   ! VLen == Big%Length
            IF (Big%Length==SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Big%Length*2_kIndex)
            END IF
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE AddMag_ApInt64

!******************************************************************************

SUBROUTINE SubMag_ApInt64(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tLong       :: Borrow
    tLong       :: Diff

!** FLOW

    Borrow = 0_kLong
    I = 0_kIndex
    DO WHILE (I < InpLen)
        CALL SubU64(Big%Digit(I), Inp(I), Borrow, Diff)
        Big%Digit(I) = Diff
        I = I + 1
    END DO
    IF (Borrow /= 0_kLong) THEN
        DO WHILE (Big%Digit(I) == 0_kLong)
            Big%Digit(I) = Big%Digit(I) - 1_kLong
            I = I + 1
        END DO
        Big%Digit(I) = Big%Digit(I) - 1_kLong
        IF ((Big%Digit(I) == 0_kLong).AND.(I+1_kIndex == Big%Length)) Big%Length = I
    END IF
    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1_kIndex) == 0_kLong))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE SubMag_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                   ALTERNATIVE ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AddOrSub_SInt128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: This
    TYPE(ApInt64), INTENT(IN)       :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    TYPE(SInt128)   :: Dif

!** FLOW

    IF (SIZE(This%Digit, KIND=kIndex) < Other%Length) THEN
        CALL MemResize(This%Digit, Other%Length + 1_kIndex)
    END IF
    This%Sign = -This%Sign
    Dif = ZeroI128
    I = 0
    DO WHILE (I < This%Length)
        Dif = SInt128(Other%Digit(I), Positive) - SInt128(This%Digit(I), Positive) + Dif
        This%Digit(I) = ToU64(Dif)
        Dif = ShiftA64(Dif)
        I = I + 1
    END DO
    IF (Other%Length > This%Length)THEN
        This%Digit(This%Length:Other%Length-1) = Other%Digit(This%Length:Other%Length-1)
        This%Length = Other%Length
    END IF
    IF (Dif /= ZeroI128) THEN
        DO WHILE ((I < Other%Length).AND.(This%Digit(I) == 0_kLong))
            This%Digit(I) = This%Digit(I) - 1_kLong
            I = I + 1
        END DO
        This%Digit(I) = This%Digit(I) - 1_kLong
        IF ((This%Digit(I) == 0_kLong).AND.(I+1_kIndex == This%Length)) THEN
            This%Length = This%Length - 1_kIndex
        END IF
    END IF
    ! IF (I == Other%Length) should be impossible

    RETURN

END SUBROUTINE AddOrSub_SInt128

!******************************************************************************

SUBROUTINE AddMag_SInt128(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: I, ULen, VLen
    TYPE(SInt128)       :: Carry

!** FLOW

    ! allocate and assign U and V
    IF (InpLen < Big%Length) THEN
        ULen = InpLen
        VLen = Big%Length
    ELSE
        ULen = Big%Length
        VLen = InpLen
    END IF
    IF (VLen > SIZE(Big%Digit, KIND=kIndex)) THEN
        CALL MemResize(Big%Digit, VLen+1_kIndex)
    END IF
    Carry = ZeroI128
    I = 0
    DO WHILE (I < ULen)
        Carry = SInt128(Big%Digit(I), Positive) + SInt128(Inp(I), Positive) + Carry
        Big%Digit(I) = ToU64(Carry)
        Carry = ShiftR64(Carry)
        I = I + 1
    END DO
    IF (VLen > Big%Length)THEN
        Big%Digit(Big%Length:VLen-1) = Inp(Big%Length:VLen-1)
        Big%Length = VLen
    END IF
    IF (Carry /= ZeroI128) THEN             ! Carry == 1
        DO WHILE (I < Big%Length)
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (Big%Digit(I) /= 0_kLong) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN   ! VLen == Big%Length
            IF (Big%Length==SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Big%Length*2_kIndex)
            END IF
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE AddMag_SInt128

!******************************************************************************

SUBROUTINE SubMag_SInt128(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    TYPE(SInt128)   :: Dif

!** FLOW

    Dif = ZeroI128
    I = 0
    DO WHILE (I < InpLen)
        Dif = SInt128(Big%Digit(I), Positive) - SInt128(Inp(I), Positive) + Dif
        Big%Digit(I) = ToU64(Dif)
        Dif = ShiftA64(Dif)
        I = I + 1
    END DO
    IF (Dif /= ZeroI128) THEN
        DO WHILE (Big%Digit(I) == 0_kLong)
            Big%Digit(I) = Big%Digit(I) - 1_kLong
            I = I + 1
        END DO
        Big%Digit(I) = Big%Digit(I) - 1_kLong
        IF ((Big%Digit(I) == 0_kLong).AND.(I+1_kIndex == Big%Length)) Big%Length = I
    END IF
    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1_kIndex) == 0_kLong))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE SubMag_SInt128

!******************************************************************************

SUBROUTINE AddOrSub_UInt128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: This
    TYPE(ApInt64), INTENT(IN)       :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    TYPE(UInt128)   :: Dif

!** FLOW

    IF (SIZE(This%Digit, KIND=kIndex) < Other%Length) THEN
        CALL MemResize(This%Digit, Other%Length + 2_kIndex)
    END IF
    This%Sign = -This%Sign
    Dif = ZeroU128
    I = 0
    DO WHILE (I < This%Length)
        Dif = UInt128(0_kLong, Other%Digit(I)) - UInt128(0_kLong, This%Digit(I)) - Dif
        This%Digit(I) = Dif%Low
        Dif = UInt128(0_kLong, IAND(Dif%High, 1_kLong))  ! IAND(SHIFTR(Dif, 64), 1)
        I = I + 1
    END DO
    IF (Other%Length > This%Length)THEN
        This%Digit(This%Length:Other%Length-1) = Other%Digit(This%Length:Other%Length-1)
        This%Length = Other%Length
    END IF
    IF (Dif /= ZeroU128) THEN
        DO WHILE ((I < Other%Length).AND.(This%Digit(I) == 0_kLong))
            This%Digit(I) = This%Digit(I) - 1_kLong
            I = I + 1
        END DO
        This%Digit(I) = This%Digit(I) - 1_kLong
        IF ((This%Digit(I) == 0_kLong).AND.(I+1_kIndex == This%Length)) THEN
            This%Length = This%Length - 1_kIndex
        END IF
    END IF
    ! IF (I == Other%Length) should be impossible

    RETURN

END SUBROUTINE AddOrSub_UInt128

!******************************************************************************

SUBROUTINE AddMag_UInt128(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: I, ULen, VLen
    TYPE(UInt128)       :: Carry

!** FLOW

    ! allocate and assign U and V
    IF (InpLen < Big%Length) THEN
        ULen = InpLen
        VLen = Big%Length
    ELSE
        ULen = Big%Length
        VLen = InpLen
    END IF
    IF (VLen > SIZE(Big%Digit, KIND=kIndex)) THEN
        CALL MemResize(Big%Digit, VLen+2_kIndex)
    END IF
    Carry = ZeroU128
    I = 0
    DO WHILE (I < ULen)
        Carry = UInt128(0_kLong, Big%Digit(I)) + UInt128(0_kLong, Inp(I)) + Carry
        Big%Digit(I) = Carry%Low
        Carry = ShiftR64(Carry)
        I = I + 1
    END DO
    IF (VLen > Big%Length)THEN
        Big%Digit(Big%Length:VLen-1) = Inp(Big%Length:VLen-1)
        Big%Length = VLen
    END IF
    IF (Carry /= ZeroU128) THEN            ! Carry == 1
        DO WHILE (I < Big%Length)
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (Big%Digit(I) /= 0_kLong) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN   ! VLen == Big%Length
            IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Big%Length+2_kIndex)
            END IF
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE AddMag_UInt128

!******************************************************************************

SUBROUTINE SubMag_UInt128(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    TYPE(UInt128)   :: Dif

!** FLOW

    Dif = ZeroU128
    I = 0
    DO WHILE (I < InpLen)
        Dif = UInt128(0_kLong, Big%Digit(I)) - UInt128(0_kLong, Inp(I)) - Dif
        Big%Digit(I) = Dif%Low
        Dif = UInt128(0_kLong, IAND(Dif%High, 1_kLong))  ! IAND(SHIFTR(Dif, 64), 1)
        I = I + 1
    END DO
    IF (Dif /= ZeroU128) THEN
        DO WHILE (Big%Digit(I) == 0_kLong)
            Big%Digit(I) = Big%Digit(I) - 1_kLong
            I = I + 1
        END DO
        Big%Digit(I) = Big%Digit(I) - 1_kLong
        IF ((Big%Digit(I) == 0_kLong).AND.(I+1_kIndex == Big%Length)) Big%Length = I
    END IF
    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1_kIndex) == 0_kLong))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE SubMag_UInt128

!******************************************************************************

SUBROUTINE AddOrSub_IEOR(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: This
    TYPE(ApInt64), INTENT(IN)       :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    tLogical        :: Borrow
    tLong           :: Digit

!** FLOW

    IF (SIZE(This%Digit, KIND=kIndex) < Other%Length) THEN
        CALL MemResize(This%Digit, Other%Length + 1_kIndex)
    END IF
    This%Sign = -This%Sign
    Digit = Other%Digit(0) - This%Digit(0)
    Borrow = (IEOR(Other%Digit(0), MinI64) < IEOR(This%Digit(0), MinI64))
    This%Digit(0) = Digit
    I = 1
    DO WHILE (I < This%Length)
        IF (Borrow) THEN
            ! addition with carry
            Digit = Other%Digit(I) - This%Digit(I) - 1_kLong
        ELSE
            ! addition without carry
            Digit = Other%Digit(I) - This%Digit(I)
        END IF
        Borrow = (IEOR(Other%Digit(I), MinI64) < IEOR(This%Digit(I), MinI64))
        This%Digit(I) = Digit
        I = I + 1
    END DO
    IF (Other%Length > This%Length)THEN
        This%Digit(This%Length:Other%Length-1) = Other%Digit(This%Length:Other%Length-1)
        This%Length = Other%Length
    END IF
    IF (Borrow) THEN
        DO WHILE ((I < Other%Length).AND.(This%Digit(I) == 0_kLong))
            This%Digit(I) = -1_kLong
            I = I + 1
        END DO
        This%Digit(I) = This%Digit(I) - 1_kLong
        IF ((This%Digit(I) == 0_kLong).AND.(I+1_kIndex == This%Length)) THEN
            This%Length = This%Length - 1_kIndex
        END IF
    END IF
    ! IF (I == Other%Length) should be impossible

    RETURN

END SUBROUTINE AddOrSub_IEOR

!******************************************************************************

SUBROUTINE AddMag_IEOR(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ULen, VLen
    tLogical    :: Carry
    tLong       :: Sum

!** FLOW

    ! allocate and assign U and V
    IF (InpLen < Big%Length) THEN
        ULen = InpLen
        VLen = Big%Length
    ELSE
        ULen = Big%Length
        VLen = InpLen
    END IF
    IF (VLen > SIZE(Big%Digit, KIND=kIndex)) THEN
        CALL MemResize(Big%Digit, VLen+1_kIndex)
    END IF
    Carry = FalseVal
    I = 0
    DO WHILE (I < ULen)
        IF (Carry) THEN
            ! addition with carry
            Sum = Big%Digit(I) + Inp(I) + 1_kLong
        ELSE
            ! addition without carry
            Sum = Big%Digit(I) + Inp(I)
        END IF
        Carry = (IEOR(Sum, MinI64) < IEOR(Big%Digit(I), MinI64))
        Big%Digit(I) = Sum
        I = I + 1
    END DO
    IF (VLen > Big%Length)THEN
        Big%Digit(Big%Length:VLen-1) = Inp(Big%Length:VLen-1)
        Big%Length = VLen
    END IF
    IF (Carry) THEN             ! Carry == 1
        DO WHILE (I < Big%Length)
            Big%Digit(I) = Big%Digit(I) + 1_kLong
            IF (Big%Digit(I) /= 0_kLong) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN   ! VLen == Big%Length
            IF (Big%Length==SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Big%Length*2_kIndex)
            END IF
            Big%Digit(Big%Length) = 1_kLong
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE AddMag_IEOR

!******************************************************************************

SUBROUTINE SubMag_IEOR(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    tLogical        :: Borrow
    tLong           :: Digit

!** FLOW

    Digit = Big%Digit(0) - Inp(0)
    Borrow = (IEOR(Big%Digit(0), MinI64) < IEOR(Inp(0), MinI64))
    Big%Digit(0) = Digit
    I = 1
    DO WHILE (I < InpLen)
        IF (Borrow) THEN
            ! addition with carry
            Digit = Big%Digit(I) - Inp(I) - 1_kLong
        ELSE
            ! addition without carry
            Digit = Big%Digit(I) - Inp(I)
        END IF
        Borrow = (IEOR(Big%Digit(I), MinI64) < IEOR(Inp(I), MinI64))
        Big%Digit(I) = Digit
        I = I + 1
    END DO
    IF (Borrow) THEN
        DO WHILE (Big%Digit(I) == 0_kLong)
            Big%Digit(I) = Big%Digit(I) - 1_kLong
            I = I + 1
        END DO
        Big%Digit(I) = Big%Digit(I) - 1_kLong
        IF ((Big%Digit(I) == 0_kLong).AND.(I+1_kIndex == Big%Length)) Big%Length = I
    END IF
    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1_kIndex) == 0_kLong))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE SubMag_IEOR

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  EXPERIMENTAL ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AddOrSub_Xp(This, Other, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: This
    TYPE(ApInt64), INTENT(IN)       :: Other
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL AddOrSub_SInt128(This, Other)
    CASE (2)
        CALL AddOrSub_UInt128(This, Other)
    CASE (3)
        CALL AddOrSub_IEOR(This, Other)
    CASE DEFAULT
        CALL AddOrSub_ApInt64(This, Other)
    END SELECT

    RETURN

END SUBROUTINE AddOrSub_Xp

!******************************************************************************

SUBROUTINE AddMag_Xp(Big, Inp, InpLen, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL AddMag_SInt128(Big, Inp, InpLen)
    CASE (2)
        CALL AddMag_UInt128(Big, Inp, InpLen)
    CASE (3)
        CALL AddMag_IEOR(Big, Inp, InpLen)
    CASE DEFAULT
        CALL AddMag_ApInt64(Big, Inp, InpLen)
    END SELECT

    RETURN

END SUBROUTINE AddMag_Xp

!******************************************************************************

SUBROUTINE SubMag_Xp(Big, Inp, InpLen, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tLong,         INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL SubMag_SInt128(Big, Inp, InpLen)
    CASE (2)
        CALL SubMag_UInt128(Big, Inp, InpLen)
    CASE (3)
        CALL SubMag_IEOR(Big, Inp, InpLen)
    CASE DEFAULT
        CALL SubMag_ApInt64(Big, Inp, InpLen)
    END SELECT

    RETURN

END SUBROUTINE SubMag_Xp

!******************************************************************************

MODULE SUBROUTINE ApInt64_Add_Xp(This, Other, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(ApInt64),  INTENT(IN)      :: Other
    tInteger,       INTENT(IN)      :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either this or other is zero
    IF (This%IsZero()) THEN
        ! This = Other
        IF (ALLOCATED(Other%Digit)) THEN
            This%Sign   = Other%Sign
            This%Length = Other%Length
            IF (This%Length > 1_kIndex) CALL MemAlloc(This%Digit, This%Length, StartID=0_kIndex)
            This%Digit(0:This%Length-1) = Other%Digit(0:This%Length-1)
        END IF
        RETURN
    ELSEIF (IsZero(Other)) THEN
        ! simply return
        RETURN
    END IF

    IF (This%Sign == Other%Sign) THEN
        CALL AddMag_Xp(This, Other%Digit, Other%Length, Algorithm)
        RETURN
    END IF
    IF (CompareAbs(This, Other) >= 0) THEN
        CALL SubMag_Xp(This, Other%Digit, Other%Length, Algorithm)
        RETURN
    END IF
    CALL AddOrSub_Xp(This, Other, Algorithm)

    RETURN

END SUBROUTINE ApInt64_Add_Xp

!******************************************************************************

MODULE FUNCTION ApInt64_Plus_Xp(LhsVal, RhsVal, Algorithm) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition: OutVal = LhsVal + RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal
    tInteger,      INTENT(IN)   :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: OutSize

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt64()
        ELSE
            ! LhsVal is zero
            OutVal = RhsVal
        END IF
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        ! RhsVal is zero
        OutVal = LhsVal
        RETURN
    END IF

    IF (LhsVal%Length < RhsVal%Length) THEN
        OutSize = RhsVal%Length + 1
    ELSE
        OutSize = LhsVal%Length + 1
    END IF
    OutVal%Sign   = LhsVal%Sign
    OutVal%Length = LhsVal%Length
    CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
    OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
    IF (OutVal%Sign == RhsVal%Sign) THEN
        CALL AddMag_Xp(OutVal, RhsVal%Digit, RhsVal%Length, Algorithm)
        RETURN
    END IF
    IF (CompareAbs(OutVal, RhsVal) >= 0) THEN
        CALL SubMag_Xp(OutVal, RhsVal%Digit, RhsVal%Length, Algorithm)
        RETURN
    END IF
    CALL AddOrSub_Xp(OutVal, RhsVal, Algorithm)

    RETURN

END FUNCTION ApInt64_Plus_Xp

!******************************************************************************

MODULE SUBROUTINE ApInt64_Subtract_Xp(This, Other, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(ApInt64),  INTENT(IN)      :: Other
    tInteger,       INTENT(IN)      :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either this or other is zero
    IF (This%IsZero()) THEN
        ! This = -Other
        IF (ALLOCATED(Other%Digit)) THEN
            This%Sign   = -Other%Sign
            This%Length = Other%Length
            IF (This%Length > 1_kIndex) CALL MemAlloc(This%Digit, This%Length, StartID=0_kIndex)
            This%Digit(0:This%Length-1) = Other%Digit(0:This%Length-1)
        END IF
        RETURN
    ELSEIF (IsZero(Other)) THEN
        ! simply return
        RETURN
    END IF

    IF (This%Sign /= Other%Sign) THEN
        CALL AddMag_Xp(This, Other%Digit, Other%Length, Algorithm)
        RETURN
    END IF
    IF (CompareAbs(This, Other) >= 0) THEN
        CALL SubMag_Xp(This, Other%Digit, Other%Length, Algorithm)
        RETURN
    END IF
    CALL AddOrSub_Xp(This, Other, Algorithm)

    RETURN

END SUBROUTINE ApInt64_Subtract_Xp

!******************************************************************************

MODULE FUNCTION ApInt64_Minus_Xp(LhsVal, RhsVal, Algorithm) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction: OutVal = LhsVal - RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal
    tInteger,      INTENT(IN)   :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: OutSize

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt64()
        ELSE
            ! LhsVal is zero
            OutVal = -RhsVal
        END IF
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        ! RhsVal is zero
        OutVal = LhsVal
        RETURN
    END IF

    IF (LhsVal%Length < RhsVal%Length) THEN
        OutSize = RhsVal%Length + 1
    ELSE
        OutSize = LhsVal%Length + 1
    END IF
    OutVal%Sign   = LhsVal%Sign
    OutVal%Length = LhsVal%Length
    CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
    OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
    IF (OutVal%Sign /= RhsVal%Sign) THEN
        CALL AddMag_Xp(OutVal, RhsVal%Digit, RhsVal%Length, Algorithm)
        RETURN
    END IF
    IF (CompareAbs(OutVal, RhsVal) >= 0) THEN
        CALL SubMag_Xp(OutVal, RhsVal%Digit, RhsVal%Length, Algorithm)
        RETURN
    END IF
    CALL AddOrSub_Xp(OutVal, RhsVal, Algorithm)

    RETURN

END FUNCTION ApInt64_Minus_Xp

!******************************************************************************

END SUBMODULE SubClass_Api64_AddSub

!******************************************************************************

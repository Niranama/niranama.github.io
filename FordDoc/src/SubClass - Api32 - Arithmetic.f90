
SUBMODULE (Class_ApInt32 : SubClass_Api32_Auxiliary) SubClass_Api32_Arithmetic

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to arithmetic
!   operations of the <a href="../module/class_apint32.html">ApInt32</a> type.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! name of the module
    tCharStar, PARAMETER    :: SubName = 'SubClass_Api32_Arithmetic'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
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
    INTERFACE  AddMag
        MODULE PROCEDURE AddMag_ApInt32
    END INTERFACE
    INTERFACE  SubMag
        MODULE PROCEDURE SubMag_ApInt32
    END INTERFACE
    INTERFACE  AddOrSub
        MODULE PROCEDURE AddOrSub_ApInt32
    END INTERFACE

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt32_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return result of the unary plus sign of the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(InVal)

    RETURN

END FUNCTION ApInt32_UnaryPlus

!******************************************************************************

MODULE SUBROUTINE ApInt32_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase value of the input by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%IsZero()) THEN
        Val%Digit(0) = 1
        Val%Sign = 1
    ELSE
        CALL UAdd(Val, 1)
    END IF

    RETURN

END SUBROUTINE ApInt32_Increment

!******************************************************************************

MODULE SUBROUTINE ApInt32_Add_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
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

END SUBROUTINE ApInt32_Add_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_Add_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
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

END SUBROUTINE ApInt32_Add_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_Add_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Add(ApInt32(Other))

    RETURN

END SUBROUTINE ApInt32_Add_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_Add_ApInt32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  This = This + Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(ApInt32),  INTENT(IN)      :: Other

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

END SUBROUTINE ApInt32_Add_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Plus_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I32)

    RETURN

END FUNCTION ApInt32_Plus_I32

!******************************************************************************

MODULE FUNCTION I32_Plus_ApInt32(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I32 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I32)

    RETURN

END FUNCTION I32_Plus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Plus_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I64)

    RETURN

END FUNCTION ApInt32_Plus_I64

!******************************************************************************

MODULE FUNCTION I64_Plus_ApInt32(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I64 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I64)

    RETURN

END FUNCTION I64_Plus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Plus_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = Big + I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I128)

    RETURN

END FUNCTION ApInt32_Plus_I128

!******************************************************************************

MODULE FUNCTION I128_Plus_ApInt32(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = I128 + Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Add(I128)

    RETURN

END FUNCTION I128_Plus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Plus_ApInt32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition: OutVal = LhsVal + RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt32()
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
        OutVal = MakeCopy(RhsVal, RhsVal%Length+1_kIndex)
        CALL OutVal%Add(LhsVal)
    ELSE
        OutVal = MakeCopy(LhsVal, LhsVal%Length+1_kIndex)
        CALL OutVal%Add(RhsVal)
    END IF

    RETURN

END FUNCTION ApInt32_Plus_ApInt32

!******************************************************************************

SUBROUTINE UAddMag_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of ApInt32 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount of the increase (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Tmp

!** FLOW

    Tmp = ToUnsignedLong(Big%Digit(0)) + ToUnsignedLong(U32)
    Big%Digit(0) = ToInteger(Tmp)
    IF (SHIFTR(Tmp, 32) /= 0_kLong) THEN
        BLOCK
            tIndex    :: I
            I = 1_kIndex
            DO
                Big%Digit(I) = Big%Digit(I) + 1
                IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0))) EXIT
                I = I + 1_kIndex
            END DO
            IF (I == Big%Length) THEN
                IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
                    CALL MemResize(Big%Digit, Big%Length*2_kIndex)
                END IF
                Big%Digit(Big%Length) = 1
                Big%Length = Big%Length + 1
            END IF
        END BLOCK
    END IF

    RETURN

END SUBROUTINE UAddMag_U32

!******************************************************************************

SUBROUTINE UAddMag_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of ApInt32 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount of the increase (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry, U64Lo, U64Hi

!** FLOW

    IF (SIZE(Big%Digit) <= 2) THEN
        CALL MemResize(Big%Digit, 3_kIndex)
        Big%Length = 2
    END IF

    U64Hi = SHIFTR(U64, 32)
    U64Lo = IAND(U64, MASK)
    Carry = ToUnsignedLong(Big%Digit(0)) + U64Lo
    Big%Digit(0) = ToInteger(Carry)
    Carry = SHIFTR(Carry, 32)
    Carry = ToUnsignedLong(Big%Digit(1)) + U64Hi + Carry
    Big%Digit(1) = ToInteger(Carry)
    IF ((Big%Length == 1_kIndex).AND.(Big%Digit(1) /= 0)) Big%Length = 2
    IF (SHIFTA(Carry, 32) /= 0) THEN
        BLOCK
            tIndex    :: I
            I = 2_kIndex
            DO
                Big%Digit(I) = Big%Digit(I) + 1
                IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0))) EXIT
                I = I + 1_kIndex
            END DO
            IF (I == Big%Length) THEN
                IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
                    CALL MemResize(Big%Digit, Big%Length*2_kIndex)
                END IF
                Big%Digit(Big%Length) = 1
                Big%Length = Big%Length + 1
            END IF
        END BLOCK
    ELSEIF ((Big%Length == 2_kIndex).AND.(Big%Digit(1) == 0)) THEN
        Big%Length = Big%Length - 1_kIndex
    END IF

    RETURN

END SUBROUTINE UAddMag_U64

!******************************************************************************

SUBROUTINE UAdd_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the specified integer to the ApInt32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount to be added (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Big%Sign < 0) THEN
        IF ((Big%Length > 1_kIndex).OR.(ToUnsignedLong(Big%Digit(0)) > ToUnsignedLong(U32))) THEN
            CALL USubMag(Big, U32)
            RETURN
        END IF
        Big%Sign = 1
        Big%Digit(0) = U32 - Big%Digit(0)
        RETURN
    END IF
    CALL UAddMag(Big, U32)

    RETURN

END SUBROUTINE UAdd_U32

!******************************************************************************

SUBROUTINE UAdd_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the specified integer to the ApInt32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount to be added (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Dif, U64Lo, U64Hi
    tLong       :: Digit0, Digit1

!** FLOW


    IF (Big%Sign < 0) THEN
        U64Hi = SHIFTR(U64, 32)
        U64Lo = IAND(U64, MASK)
        Digit0 = ToUnsignedLong(Big%Digit(0))
        Digit1 = ToUnsignedLong(Big%Digit(1))
        IF ((Big%Length > 2).OR.((Big%Length == 2).AND.((Digit1 > U64Hi).OR.((Digit1 == U64Hi) &
            .AND.(Digit0 >= U64Lo)))).OR.((U64Hi == 0).AND.(Digit0 >= U64Lo))) THEN
            CALL USubMag(Big, U64)
            RETURN
        END IF
        IF (SIZE(Big%Digit) == 1) CALL MemResize(Big%Digit, 2_kIndex)
        IF (Big%Length == 1) THEN
            Big%Digit(Big%Length) = 0
            Big%Length = Big%Length + 1_kIndex
        END IF
        Dif = U64Lo - Digit0
        Big%Digit(0) = ToInteger(Dif)
        Dif = SHIFTA(Dif, 32)
        Dif = U64Hi - Digit1 + Dif
        Big%Digit(1) = ToInteger(Dif)
        ! Dif ,  32 /= 0 should be impossible
        IF (Dif == 0_kLong) Big%Length = Big%Length - 1_kIndex
        Big%Sign = 1
    ELSE
        CALL UAddMag(Big, U64)
    END IF

    RETURN

END SUBROUTINE UAdd_U64

!******************************************************************************

SUBROUTINE AddOrSub_ApInt32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To either add 'Other' to 'This' or subtract 'Other' from 'This'
    ! depending on their signs.
    ! If same signs, perform subtraction. Otherwise, perform addition.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: This
    TYPE(ApInt32), INTENT(IN)       :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    tLong           :: Dif

!** FLOW

    IF (SIZE(This%Digit, KIND=kIndex) < Other%Length) THEN
        CALL MemResize(This%Digit, Other%Length + 1_kIndex)
    END IF
    This%Sign = -This%Sign
    Dif = 0_kLong
    I = 0
    DO WHILE (I < This%Length)
        Dif = ToUnsignedLong(Other%Digit(I)) - ToUnsignedLong(This%Digit(I)) + Dif
        This%Digit(I) = ToInteger(Dif)
        Dif = SHIFTA(Dif, 32)
        I = I + 1
    END DO
    IF (Other%Length > This%Length)THEN
        This%Digit(This%Length:Other%Length-1) = Other%Digit(This%Length:Other%Length-1)
        This%Length = Other%Length
    END IF
    IF (Dif /= 0_kLong) THEN
        DO WHILE ((I < Other%Length).AND.(This%Digit(I) == 0))
            This%Digit(I) = This%Digit(I) - 1
            I = I + 1
        END DO
        This%Digit(I) = This%Digit(I) - 1
        IF ((This%Digit(I) == 0).AND.(I+1_kIndex == This%Length)) THEN
            This%Length = This%Length - 1_kIndex
        END IF
    END IF
    ! IF (I == Other%Length) should be impossible

    RETURN

END SUBROUTINE AddOrSub_ApInt32

!******************************************************************************

SUBROUTINE AddMag_ApInt32(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase the magnitude of Big by the given magnitude array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tInteger,      INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ULen, VLen
    tLong       :: Carry

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
        Carry = ToUnsignedLong(Big%Digit(I)) + ToUnsignedLong(Inp(I)) + Carry
        Big%Digit(I) = ToInteger(Carry)
        Carry = SHIFTR(Carry, 32)
        I = I + 1
    END DO
    IF (VLen > Big%Length)THEN
        Big%Digit(Big%Length:VLen-1) = Inp(Big%Length:VLen-1)
        Big%Length = VLen
    END IF
    IF (Carry /= 0) THEN            ! Carry == 1
        DO WHILE (I < Big%Length)
            Big%Digit(I) = Big%Digit(I) + 1
            IF (Big%Digit(I) /= 0) EXIT
            I = I + 1_kIndex
        END DO
        IF (I == Big%Length) THEN   ! VLen == Big%Length
            IF (Big%Length==SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Big%Length*2_kIndex)
            END IF
            Big%Digit(Big%Length) = 1
            Big%Length = Big%Length + 1_kIndex
        END IF
    END IF

    RETURN

END SUBROUTINE AddMag_ApInt32

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt32_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the negation of the input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(InVal)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION ApInt32_Negate

!******************************************************************************

MODULE SUBROUTINE ApInt32_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease value of the input by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%IsZero()) THEN
        Val%Digit(0) = 1
        Val%Sign = -1
    ELSE
        CALL USub(Val, 1)
    END IF

    RETURN

END SUBROUTINE ApInt32_Decrement

!******************************************************************************

MODULE SUBROUTINE ApInt32_Subtract_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
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

END SUBROUTINE ApInt32_Subtract_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_Subtract_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
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

END SUBROUTINE ApInt32_Subtract_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_Subtract_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Subtract(ApInt32(Other))

    RETURN

END SUBROUTINE ApInt32_Subtract_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_Subtract_ApInt32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  This = This - Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(ApInt32),  INTENT(IN)      :: Other

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

END SUBROUTINE ApInt32_Subtract_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Minus_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I32)

    RETURN

END FUNCTION ApInt32_Minus_I32

!******************************************************************************

MODULE FUNCTION I32_Minus_ApInt32(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I32 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I32)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I32_Minus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Minus_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I64)

    RETURN

END FUNCTION ApInt32_Minus_I64

!******************************************************************************

MODULE FUNCTION I64_Minus_ApInt32(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I64 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I64)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I64_Minus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Minus_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = Big - I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I128)

    RETURN

END FUNCTION ApInt32_Minus_I128

!******************************************************************************

MODULE FUNCTION I128_Minus_ApInt32(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = I128 - Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Subtract(I128)
    OutVal%Sign = -OutVal%Sign

    RETURN

END FUNCTION I128_Minus_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Minus_ApInt32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction: OutVal = LhsVal - RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check for special cases where either LhsVal or RhsVal is zero
    IF (IsZero(LhsVal)) THEN
        IF (IsZero(RhsVal)) THEN
            ! both are zero
            OutVal = ZeroApInt32()
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

    OutVal%Sign   = LhsVal%Sign
    OutVal%Length = LhsVal%Length
    IF (LhsVal%Length < RhsVal%Length) THEN
        CALL MemAlloc(OutVal%Digit, RhsVal%Length+1_kIndex, StartID=0_kIndex)
    ELSE
        CALL MemAlloc(OutVal%Digit, LhsVal%Length+1_kIndex, StartID=0_kIndex)
    END IF
    OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
    CALL OutVal%Subtract(RhsVal)

    RETURN

END FUNCTION ApInt32_Minus_ApInt32

!******************************************************************************

SUBROUTINE USubMag_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of ApInt32 by the specified integer.
    ! If U32 > the magnitude of ApInt32, behavior is undefined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount of the decrease (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Dif

!** FLOW

    Dif = ToUnsignedLong(Big%Digit(0)) - ToUnsignedLong(U32)
    Big%Digit(0) = ToInteger(Dif)
    IF (SHIFTA(Dif, 32) /= 0_kLong) THEN
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

END SUBROUTINE USubMag_U32

!******************************************************************************

SUBROUTINE USubMag_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of ApInt32 by the specified integer
    ! If U64 > the magnitude of ApInt32, behavior is undefined.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount of the decrease (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Dif, U64Lo, U64Hi

!** FLOW


    U64Hi = SHIFTR(U64, 32)
    U64Lo = IAND(U64, MASK)
    Dif = ToUnsignedLong(Big%Digit(0)) - U64Lo
    Big%Digit(0) = ToInteger(Dif)
    Dif = SHIFTA(Dif, 32)
    Dif = ToUnsignedLong(Big%Digit(1)) - U64Hi + Dif
    Big%Digit(1) = ToInteger(Dif)
    IF (SHIFTA(Dif, 32) /= 0) THEN
        BLOCK
            tIndex    :: I
            I = 2_kIndex
            DO WHILE (Big%Digit(I) == 0)
                Big%Digit(I) = Big%Digit(I) - 1
                I = I - 1_kIndex
            END DO
            Big%Digit(I) = Big%Digit(I) - 1
            IF ((Big%Digit(I) == 0).AND.(I+1_kIndex == Big%Length)) THEN
                Big%Length = Big%Length - 1
            END IF
        END BLOCK
    END IF
    IF ((Big%Length == 2_kIndex).AND.(Big%Digit(1) == 0)) THEN
        Big%Length = Big%Length - 1_kIndex
    END IF

    RETURN

END SUBROUTINE USubMag_U64

!******************************************************************************

SUBROUTINE USub_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To subtract the specified integer to the ApInt32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The amount to be subtracted (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Big%Sign < 0) THEN
        CALL UAddMag(Big, U32)
        RETURN
    END IF
    IF ((Big%Length == 1_kIndex).AND.(ToUnsignedLong(Big%Digit(0)) .ULT. ToUnsignedLong(U32))) THEN
        Big%Sign = -1
        Big%Digit(0) = U32 - Big%Digit(0)
        RETURN
    END IF
    CALL USubMag(Big, U32)

    RETURN

END SUBROUTINE USub_U32

!******************************************************************************

SUBROUTINE USub_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To subtract the specified integer to the ApInt32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The amount to be subtracted (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Dif, U64Lo, U64Hi
    tLong       :: Digit0, Digit1

!** FLOW


    IF (Big%Sign > 0) THEN
        U64Hi = SHIFTR(U64, 32)
        U64Lo = IAND(U64, MASK)
        Digit0 = ToUnsignedLong(Big%Digit(0))
        Digit1 = ToUnsignedLong(Big%Digit(1))
        IF ((Big%Length > 2).OR.((Big%Length == 2).AND.((Digit1 > U64Hi).OR.((Digit1 == U64Hi) &
            .AND.(Digit0 >= U64Lo)))).OR.((U64Hi == 0).AND.(Digit0 >= U64Lo))) THEN
            CALL USubMag(Big, U64)
            RETURN
        END IF
        IF (SIZE(Big%Digit) == 1) CALL MemResize(Big%Digit, 2_kIndex)
        IF (Big%Length == 1) THEN
            Big%Digit(Big%Length) = 0
            Big%Length = Big%Length + 1_kIndex
        END IF
        Dif = U64Lo - Digit0
        Big%Digit(0) = ToInteger(Dif)
        Dif = SHIFTA(Dif, 32)
        Dif = U64Hi - Digit1 + Dif
        Big%Digit(1) = ToInteger(Dif)
        ! Dif ,  32 /= 0 should be impossible
        IF (Dif == 0_kLong) Big%Length = Big%Length - 1_kIndex
        Big%Sign = -1
    ELSE
        CALL UAddMag(Big, U64)
    END IF

    RETURN

END SUBROUTINE USub_U64

!******************************************************************************

SUBROUTINE SubMag_ApInt32(Big, Inp, InpLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrease the magnitude of Big by the given magnitude array
    ! Behavior is undefined if Inp > Big%Digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tIndex,        INTENT(IN)       :: InpLen           ! length of the magnitude array
    tInteger,      INTENT(IN)       :: Inp(0:InpLen-1)  ! the magnitude array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    tLong           :: Dif

!** FLOW

    Dif = 0_kLong
    I = 0
    DO WHILE (I < InpLen)
        Dif = ToUnsignedLong(Big%Digit(I)) - ToUnsignedLong(Inp(I)) + Dif
        Big%Digit(I) = ToInteger(Dif)
        Dif = SHIFTA(Dif, 32)
        I = I + 1
    END DO
    IF (Dif /= 0_kLong) THEN
        DO WHILE (Big%Digit(I) == 0)
            Big%Digit(I) = Big%Digit(I) - 1
            I = I + 1
        END DO
        Big%Digit(I) = Big%Digit(I) - 1
        IF ((Big%Digit(I) == 0).AND.(I+1_kIndex == Big%Length)) Big%Length = I
    END IF
    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1_kIndex) == 0))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE SubMag_ApInt32

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE ApInt32_Times_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    tInteger,       INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) RETURN

    IF (Other < 0) THEN
        This%Sign = -This%Sign
        CALL UMul(This, -Other)
    ELSE
        CALL UMul(This, Other)
    END IF

    RETURN

END SUBROUTINE ApInt32_Times_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_Times_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    tLong,          INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) RETURN

    IF (Other < 0) THEN
        This%Sign = -This%Sign
        CALL UMul(This, -Other)
    ELSE
        CALL UMul(This, Other)
    END IF

    RETURN

END SUBROUTINE ApInt32_Times_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_Times_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Multiply(ApInt32(Other))

    RETURN

END SUBROUTINE ApInt32_Times_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_Times_ApInt32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This
    TYPE(ApInt32),  INTENT(IN)      :: Other

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Sign, DigI32
    tLong       :: DigI64

!** FLOW

    ! check and returun quickly if possible
    IF (This%IsZero()) THEN
        RETURN
    ELSEIF (IsZero(Other)) THEN
        This = ZeroApInt32()
        RETURN
    ELSEIF ((This%Length <= 2_kIndex).OR.(Other%Length <= 2_kIndex)) THEN
        IF (Other%Length == 1) THEN
            This%Sign = This%Sign*Other%Sign
            CALL UMul(This, Other%Digit(0))
        ELSEIF (This%Length == 1) THEN
            Sign = This%Sign*Other%Sign
            DigI32 = This%Digit(0)
            This = MakeCopy(Other, Other%Length+1_kIndex)
            This%Sign = Sign
            CALL UMul(This, DigI32)
        ELSEIF (Other%Length == 2) THEN
            This%Sign = This%Sign*Other%Sign
            CALL UMul(This, IOR(SHIFTL(ToLong(Other%Digit(1)), 32), &
                                ToUnsignedLong(Other%Digit(0))))
        ELSE
            Sign = This%Sign*Other%Sign
            DigI64 = IOR(SHIFTL(ToLong(This%Digit(1)), 32), ToUnsignedLong(This%Digit(0)))
            This = MakeCopy(Other, Other%Length+1_kIndex)
            This%Sign = Sign
            CALL UMul(This, DigI64)
        END IF
    ELSEIF ((This%Length <= Karatsuba_Threshold).OR.(Other%Length <= Karatsuba_Threshold)) THEN
        ! perform multiplication using grade-school algorithm
        CALL Multiply_Small(This, Other)
    ELSE
        ! perform multiplication using Karatsuba algorithm
        CALL Multiply_Big(This, Other)
    END IF

    RETURN
    CONTAINS

    SUBROUTINE Multiply_Big(This, Other)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply This by Other using the Karatsuba algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32),         INTENT(INOUT)    :: This
        TYPE(ApInt32), TARGET, INTENT(IN)       :: Other

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger, ALLOCATABLE   :: XDigit(:)
        tInteger, POINTER       :: YDigit(:)
        tIndex                  :: I, MaxLen
        tLogical                :: FreeY

    !** FLOW

        ! process input
        MaxLen = MAX(This%Length, Other%Length)
        CALL MemAlloc(XDigit, MaxLen, StartID=0_kIndex)
        XDigit(0:This%Length-1) = This%Digit(0:This%Length-1)
        IF (SIZE(Other%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(YDigit, MaxLen, StartID=0_kIndex)
            YDigit(0:Other%Length-1) = Other%Digit(0:Other%Length-1)
            FreeY = TrueVal
        ELSE
            YDigit => Other%Digit
            FreeY = FalseVal
        END IF
        IF (This%Length  < MaxLen) XDigit(This%Length:MaxLen - 1)  = 0
        IF (Other%Length < MaxLen) YDigit(Other%Length:MaxLen - 1) = 0
        ! perform multiplication
        IF (SIZE(This%Digit, KIND=kIndex) < SHIFTL(MaxLen, 1)) THEN
            CALL MemAlloc(This%Digit, SHIFTL(MaxLen, 1), StartID=0_kIndex)
        END IF
        CALL Multiply_Karatsuba_NoAlloc(XDigit, YDigit, 0_kIndex, MaxLen, This%Digit)
        ! process result
        This%Length = This%Length + Other%Length
        DO WHILE (This%Digit(This%Length-1_kIndex) == 0)
            This%Length = This%Length - 1_kIndex
        END DO
        This%Sign = This%Sign*Other%Sign
        ! free working variables
        CALL MemFree(XDigit)
        IF (FreeY) THEN
            CALL MemFree(YDigit)
        ELSE
            NULLIFY(YDigit)
        END IF

        RETURN

    END SUBROUTINE Multiply_Big

    !******************************************************************************

END SUBROUTINE ApInt32_Times_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Multiply_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I32)

    RETURN

END FUNCTION ApInt32_Multiply_I32

!******************************************************************************

MODULE FUNCTION I32_Multiply_ApInt32(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I32 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I32)

    RETURN

END FUNCTION I32_Multiply_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Multiply_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+2_kIndex)
    CALL OutVal%Multiply(I64)

    RETURN

END FUNCTION ApInt32_Multiply_I64

!******************************************************************************

MODULE FUNCTION I64_Multiply_ApInt32(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I64 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+2_kIndex)
    CALL OutVal%Multiply(I64)

    RETURN

END FUNCTION I64_Multiply_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Multiply_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+4_kIndex)
    CALL OutVal%Multiply(I128)

    RETURN

END FUNCTION ApInt32_Multiply_I128

!******************************************************************************

MODULE FUNCTION I128_Multiply_ApInt32(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I128 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt32), INTENT(IN)   :: Big
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+4_kIndex)
    CALL OutVal%Multiply(I128)

    RETURN

END FUNCTION I128_Multiply_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Multiply_ApInt32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication: OutVal = LhsVal * RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and returun quickly if possible
    IF (IsZero(LhsVal)) THEN
        OutVal = MakeCopy(LhsVal)
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = ZeroApInt32()
        RETURN
    END IF

    IF ((LhsVal%Length <= 2_kIndex).OR.(RhsVal%Length <= 2_kIndex)) THEN
        IF (RhsVal%Length == 1) THEN
            OutVal = MakeCopy(LhsVal, LhsVal%Length+RhsVal%Length)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, RhsVal%Digit(0))
        ELSEIF (LhsVal%Length == 1) THEN
            OutVal = MakeCopy(RhsVal, LhsVal%Length+RhsVal%Length)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, LhsVal%Digit(0))
        ELSEIF (RhsVal%Length == 2) THEN
            OutVal = MakeCopy(LhsVal, LhsVal%Length+RhsVal%Length)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, IOR(SHIFTL(ToLong(RhsVal%Digit(1)), 32), &
                                  ToUnsignedLong(RhsVal%Digit(0))))
        ELSE
            OutVal = MakeCopy(RhsVal, LhsVal%Length+RhsVal%Length)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, IOR(SHIFTL(ToLong(LhsVal%Digit(1)), 32), &
                                  ToUnsignedLong(LhsVal%Digit(0))))
        END IF
    ELSEIF ((LhsVal%Length <= Karatsuba_Threshold).OR.(RhsVal%Length <= Karatsuba_Threshold)) THEN
        ! perform multiplication using grade-school algorithm
        OutVal = MakeCopy(LhsVal, LhsVal%Length+RhsVal%Length)
        CALL Multiply_Small(OutVal, RhsVal)
    ELSE
        ! perform multiplication using Karatsuba algorithm
        OutVal = Multiply_Big(LhsVal, RhsVal)
    END IF

    RETURN
    CONTAINS

    FUNCTION Multiply_Big(LhsVal, RhsVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using Karatsuba algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), TARGET, INTENT(IN)   :: LhsVal
        TYPE(ApInt32), TARGET, INTENT(IN)   :: RhsVal
        TYPE(ApInt32)                       :: OutVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger, POINTER   :: XDigit(:)
        tInteger, POINTER   :: YDigit(:)
        tIndex              :: MaxLen
        tLogical            :: FreeX, FreeY

    !** FLOW

        ! process input
        MaxLen = MAX(LhsVal%Length, RhsVal%Length)
        IF (SIZE(LhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(XDigit, MaxLen, StartID=0_kIndex)
            XDigit(0:LhsVal%Length-1) = LhsVal%Digit(0:LhsVal%Length-1)
            FreeX = TrueVal
        ELSE
            XDigit => LhsVal%Digit
            FreeX = FalseVal
        END IF
        IF (SIZE(RhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(YDigit, MaxLen, StartID=0_kIndex)
            YDigit(0:RhsVal%Length-1) = RhsVal%Digit(0:RhsVal%Length-1)
            FreeY = TrueVal
        ELSE
            YDigit => RhsVal%Digit
            FreeY = FalseVal
        END IF
        IF (LhsVal%Length < MaxLen) XDigit(LhsVal%Length:MaxLen - 1) = 0
        IF (RhsVal%Length < MaxLen) YDigit(RhsVal%Length:MaxLen - 1) = 0
        ! perform multiplication
        CALL Multiply_Karatsuba_Alloc(XDigit, YDigit, 0_kIndex, MaxLen, OutVal%Digit)
        ! process result
        OutVal%Length = LhsVal%Length + RhsVal%Length
        DO WHILE (OutVal%Digit(OutVal%Length-1_kIndex) == 0)
            OutVal%Length = OutVal%Length - 1_kIndex
        END DO
        OutVal%Sign = LhsVal%Sign*RhsVal%Sign
        ! free pointers
        IF (FreeX) THEN
            CALL MemFree(XDigit)
        ELSE
            NULLIFY(XDigit)
        END IF
        IF (FreeY) THEN
            CALL MemFree(YDigit)
        ELSE
            NULLIFY(YDigit)
        END IF

        RETURN

    END FUNCTION Multiply_Big

    !**************************************************************************

END FUNCTION ApInt32_Multiply_ApInt32

!******************************************************************************

SUBROUTINE UMul_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt32 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry, M
    tIndex      :: I

!** FLOW

    IF (U32 == 0) THEN
        Big = ZeroApInt32()
        RETURN
    END IF

    Carry = 0_kLong
    M = ToUnsignedLong(U32)
    DO I = 0_kIndex, Big%Length-1_kIndex
        Carry = ToUnsignedLong(Big%Digit(I))*M + Carry
        Big%Digit(I) = ToInteger(Carry)
        Carry = SHIFTR(Carry, 32)
    END DO
    IF (Carry /= 0) THEN
        IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
            CALL MemResize(Big%Digit, Big%Length*2_kIndex)
        END IF
        Big%Digit(Big%Length) = ToInteger(Carry)
        Big%Length = Big%Length + 1_kIndex
    END IF

    RETURN

END SUBROUTINE UMul_U32

!******************************************************************************

SUBROUTINE UMul_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt32 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry, MHi, MLo, Next, Tmp
    tIndex      :: I

!** FLOW

    IF (U64 == 0) THEN
        Big = ZeroApInt32()
        RETURN
    END IF
    IF (Big%Length + 2_kIndex >= SIZE(Big%Digit, KIND=kIndex)) THEN
        CALL MemResize(Big%Digit, 2_kIndex*Big%Length + 1_kIndex)
    END IF

    MHi = SHIFTR(U64, 32)
    MLo = IAND(U64, MASK)
    Carry = 0_kLong
    Next = 0_kLong
    DO I = 0_kIndex, Big%Length-1_kIndex
        Carry = Carry + Next    ! Could this overflow?
        Tmp = ToUnsignedLong(Big%Digit(I))*MLo
        Next = ToUnsignedLong(Big%Digit(I))*MHi
        Big%Digit(I) = ToInteger(Tmp + Carry)
        Carry = SHIFTR(Tmp, 32) + SHIFTR(Carry, 32) + &
                SHIFTR(IAND(Tmp, MASK) + IAND(Carry, MASK), 32)
    END DO
    Carry = Carry + Next
    Big%Digit(Big%Length) = ToInteger(Carry)
    Big%Length = Big%Length + 1_kIndex
    Big%Digit(Big%Length) = ToInteger(SHIFTR(Carry, 32))
    Big%Length = Big%Length + 1_kIndex

    DO WHILE ((Big%Length > 1_kIndex).AND.(Big%Digit(Big%Length-1) == 0))
        Big%Length = Big%Length - 1_kIndex
    END DO

    RETURN

END SUBROUTINE UMul_U64

!******************************************************************************

SUBROUTINE Multiply_Small(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply This by Other using a quadratic algorithm
    ! which is often suitable for smaller numbers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: This
    TYPE(ApInt32), INTENT(IN)       :: Other

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold and This == Other, use 'Squaring' algorithm
    tIndex, PARAMETER   :: Square_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: ThisDig(0:This%Length-1)
    tIndex      :: ThisLen, OutLen

!** FLOW

    ! check equality
    IF (This == Other) THEN
        This%Sign = 1
        ThisLen   = This%Length
        ThisDig(0:ThisLen-1) = This%Digit(0:ThisLen-1)
        OutLen = SHIFTL(ThisLen, 1)
        IF (SIZE(This%Digit, KIND=kIndex) < OutLen) THEN
            CALL MemAlloc(This%Digit, OutLen, StartID=0_kIndex)
        END IF
        ! perform multiplication
        IF (ThisLen <= Square_Threshold) THEN
            CALL Multiply_Basic(ThisDig, ThisLen, ThisDig, ThisLen, This%Digit)
        ELSE
            CALL Square_Basic_NoAlloc(ThisDig, ThisLen, This%Digit)
        END IF
        This%Length = OutLen
        IF (This%Digit(This%Length-1) == 0) This%Length = This%Length - 1_kIndex
    ELSE
        This%Sign = This%Sign*Other%Sign
        ThisLen   = This%Length
        ThisDig(0:ThisLen-1) = This%Digit(0:ThisLen-1)
        OutLen = ThisLen+Other%Length
        IF (SIZE(This%Digit, KIND=kIndex) < ThisLen+Other%Length) THEN
            CALL MemAlloc(This%Digit, OutLen, StartID=0_kIndex)
        END IF
        ! perform multiplication
        IF (ThisLen < Other%Length) THEN
            CALL Multiply_Basic(ThisDig, ThisLen, Other%Digit, Other%Length, This%Digit)
        ELSE
            CALL Multiply_Basic(Other%Digit, Other%Length, ThisDig, ThisLen, This%Digit)
        END IF
        This%Length = OutLen
        IF (This%Digit(This%Length-1) == 0) This%Length = This%Length - 1_kIndex
    END IF

    RETURN

END SUBROUTINE Multiply_Small

!******************************************************************************

SUBROUTINE Multiply_Basic(U, ULen, V, VLen, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: ULen             ! The length of the first array
    tInteger, INTENT(IN)    :: U(0:ULen-1)      ! The first magnitude array
    tIndex,   INTENT(IN)    :: VLen             ! The length of the second array
    tInteger, INTENT(IN)    :: V(0:VLen-1)      ! The second magnitude array
    tInteger, INTENT(OUT)   :: R(0:ULen+VLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry, Tmp, UI
    tIndex      :: I, J

!** FLOW

    Carry = 0_kLong
    UI = ToUnsignedLong(U(0))
    DO J = 0, VLen-1
        Tmp = UI*ToUnsignedLong(V(J)) + Carry
        R(J) = ToInteger(Tmp)
        Carry = SHIFTR(Tmp, 32)
    END DO
    R(VLen) = ToInteger(Carry)
    DO I = 1, ULen-1
        Carry = 0_kLong
        UI = ToUnsignedLong(U(I))
        DO J = 0, VLen-1
            Tmp = UI*ToUnsignedLong(V(J)) + ToUnsignedLong(R(I+J)) + Carry
            R(I+J) = ToInteger(Tmp)
            Carry = SHIFTR(Tmp, 32)
        END DO
        R(I+VLen) = ToInteger(Carry)
    END DO

    RETURN

END SUBROUTINE Multiply_Basic

!******************************************************************************

RECURSIVE SUBROUTINE Multiply_Karatsuba_Alloc(X, Y, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,              INTENT(IN)   :: X(0:)    ! The first magnitude array
    tInteger,              INTENT(IN)   :: Y(0:)    ! The second magnitude array
    tIndex,                INTENT(IN)   :: Off      ! The offset, where the first element is residing
    tIndex,                INTENT(IN)   :: N        ! The length of each of the two partial arrays
    tInteger, ALLOCATABLE, INTENT(OUT)  :: Z(:)     ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: B, C
    tInteger, ALLOCATABLE   :: X2(:), Y2(:)
    tInteger, ALLOCATABLE   :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN  ! Basecase
        CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off+B, N-B, Z2)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0
    Y2 = 0
    ! set X2 and Y2
    CALL AddHiLoParts(X, N, B, Off, X2)
    CALL AddHiLoParts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0).OR.(Y2(N-B) /= 0)) C = 1_kIndex
    CALL Multiply_Karatsuba_Alloc(X2, Y2, 0_kIndex, N-B+C, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Multiply_Karatsuba_Alloc

!******************************************************************************

SUBROUTINE Multiply_Karatsuba_NoAlloc(X, Y, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: X(0:)    ! The first magnitude array
    tInteger, INTENT(IN)    :: Y(0:)    ! The second magnitude array
    tIndex,   INTENT(IN)    :: Off      ! The offset, where the first element is residing
    tIndex,   INTENT(IN)    :: N        ! The length of each of the two partial arrays
    tInteger, INTENT(OUT)   :: Z(0:)    ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: B, C
    tInteger, ALLOCATABLE   :: X2(:), Y2(:)
    tInteger, ALLOCATABLE   :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN  ! Basecase
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off+B, N-B, Z2)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0
    Y2 = 0
    ! set X2 and Y2
    CALL AddHiLoParts(X, N, B, Off, X2)
    CALL AddHiLoParts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0).OR.(Y2(N-B) /= 0)) C = 1_kIndex
    CALL Multiply_Karatsuba_Alloc(X2, Y2, 0_kIndex, N-B+C, Z1)

    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Multiply_Karatsuba_NoAlloc

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    DIVISION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE ApInt32_Over_I32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32),     INTENT(INOUT)   :: This
    tInteger,           INTENT(IN)      :: Other
    tInteger, OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Rem

!** FLOW

    IF (IsZero(This)) THEN
        IF (PRESENT(Remainder)) Remainder = 0
        RETURN
    ELSEIF (Other == 0) THEN
        This = ZeroApInt32()
        IF (PRESENT(Remainder)) Remainder = 0
        CALL Handle_ErrLevel('ApInt32_Over_I32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    IF (PRESENT(Remainder)) THEN
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Remainder = -This%Sign*UDiv(This, -Other)
        ELSE
            Remainder = This%Sign*UDiv(This, Other)
        END IF
    ELSE
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Rem = -This%Sign*UDiv(This, -Other)
        ELSE
            Rem = This%Sign*UDiv(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt32_Over_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_Over_I64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32),  INTENT(INOUT)  :: This
    tLong,           INTENT(IN)     :: Other
    tLong, OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Rem

!** FLOW

    IF (IsZero(This)) THEN
        IF (PRESENT(Remainder)) Remainder = 0_kLong
        RETURN
    ELSEIF (Other == 0_kLong) THEN
        This = ZeroApInt32()
        IF (PRESENT(Remainder)) Remainder = 0_kLong
        CALL Handle_ErrLevel('ApInt32_Over_I64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    IF (PRESENT(Remainder)) THEN
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Remainder = -This%Sign*UDiv(This, -Other)
        ELSE
            Remainder = This%Sign*UDiv(This, Other)
        END IF
    ELSE
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Rem = -This%Sign*UDiv(This, -Other)
        ELSE
            Rem = This%Sign*UDiv(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt32_Over_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_Over_I128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32),          INTENT(INOUT)  :: This
    TYPE(SInt128),           INTENT(IN)     :: Other
    TYPE(SInt128), OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(ApInt32)   :: Rem

!** FLOW

    IF (PRESENT(Remainder)) THEN
        CALL This%Divide(ApInt32(Other), Rem)
        Remainder = ToI128(Rem)
    ELSE
        CALL This%Divide(ApInt32(Other))
    END IF

    RETURN

END SUBROUTINE ApInt32_Over_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_Over_ApInt32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32),          INTENT(INOUT)  :: This
    TYPE(ApInt32),           INTENT(IN)     :: Other
    TYPE(ApInt32), OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: Tmp, Rem
    tInteger, ALLOCATABLE   :: Dividend(:)

!** FLOW

    IF (This%IsZero()) THEN
        IF (PRESENT(Remainder)) Remainder = ZeroApInt32()
        RETURN
    ELSEIF (IsZero(Other)) THEN
        This = ZeroApInt32()
        IF (PRESENT(Remainder)) Remainder = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_Over_ApInt32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Other%Length == 1) THEN
        This%Sign = This%Sign*Other%Sign
        Rem = UDiv(This, Other%Digit(0))
        IF (PRESENT(Remainder)) Remainder = Rem
        RETURN
    END IF
    Tmp = CompareAbs(This, Other)
    IF (Tmp < 0) THEN
        IF (PRESENT(Remainder)) Remainder = This
        This = ZeroApInt32()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(This, This%Sign*Other%Sign, 1)
        IF (PRESENT(Remainder)) Remainder = ZeroApInt32()
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Dividend, This%Length, StartID=0_kIndex)
    Dividend(0:This%Length-1) = This%Digit(0:This%Length-1)
    CALL MemAlloc(This%Digit, This%Length-Other%Length+1_kIndex, StartID=0_kIndex)
    IF (PRESENT(Remainder)) THEN
        CALL MemAlloc(Remainder%Digit, Other%Length, StartID=0_kIndex)

        ! perform division
        CALL DivCore(Dividend, This%Length, Other%Digit, Other%Length, This%Digit, &
                     Remainder%Digit)
        ! postprocess remainder
        Remainder%Sign = This%Sign
        Remainder%Length = Other%Length
        DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
            Remainder%Length = Remainder%Length - 1_kIndex
        END DO
    ELSE
        ! perform division
        CALL DivCore(Dividend, This%Length, Other%Digit, Other%Length, This%Digit)
    END IF

    ! postprocess quotient
    This%Length = SIZE(This%Digit, KIND=kIndex)
    DO WHILE ((This%Length > 1_kIndex).AND.(This%Digit(This%Length-1_kIndex) == 0))
        This%Length = This%Length - 1_kIndex
    END DO
    This%Sign = This%Sign*Other%Sign

    RETURN

END SUBROUTINE ApInt32_Over_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Divide_I32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    tInteger,      INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt32_Divide_I32

!******************************************************************************

MODULE FUNCTION ApInt32_Divide_I64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    tLong,         INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt32_Divide_I64

!******************************************************************************

MODULE FUNCTION ApInt32_Divide_I128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    TYPE(SInt128), INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt32_Divide_I128

!******************************************************************************

MODULE FUNCTION ApInt32_Divide_ApInt32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division: Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    TYPE(ApInt32), INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp, Rem

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient = ZeroApInt32()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_Divide_ApInt32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Rem = UDiv(Quotient, Divisor%Digit(0))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Quotient = ZeroApInt32()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)

    ! perform division
    CALL DivCore(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, Quotient%Digit)

    ! postprocess
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END FUNCTION ApInt32_Divide_ApInt32

!******************************************************************************

MODULE FUNCTION ApInt32_Mod_I32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    tInteger,      INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: Rem
    TYPE(ApInt32)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt32_Mod_I32

!******************************************************************************

MODULE FUNCTION ApInt32_Mod_I64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    tLong,         INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Rem
    TYPE(ApInt32)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt32_Mod_I64

!******************************************************************************

MODULE FUNCTION ApInt32_Mod_I128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    TYPE(SInt128), INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: Rem
    TYPE(ApInt32)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt32_Mod_I128

!******************************************************************************

MODULE FUNCTION ApInt32_Mod_ApInt32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation: Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    TYPE(ApInt32), INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Remainder = ZeroApInt32()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Remainder = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_Mod_ApInt32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Remainder = MakeCopy(Dividend)
        CALL URem(Remainder, Divisor%Digit(0))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = MakeCopy(Dividend)
        RETURN
    END IF
    IF (Tmp == 0) THEN
        Remainder = ZeroApInt32()
        RETURN
    END IF

    ! allocate storage
    CALL MemAlloc(Remainder%Digit, Divisor%Length, StartID=0_kIndex)

    ! perform division
    CALL DivCore(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                 R=Remainder%Digit)

    ! postprocess
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    RETURN

END FUNCTION ApInt32_Mod_ApInt32

!******************************************************************************

MODULE SUBROUTINE ApInt32_DivMod_I32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(IN)  :: Dividend
    tInteger,       INTENT(IN)  :: Divisor
    TYPE(ApInt32),  INTENT(OUT) :: Quotient
    TYPE(ApInt32),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        RETURN
    ELSEIF (Divisor == 0) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_DivMod_I32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    Quotient = MakeCopy(Dividend)
    IF (Divisor < 0) THEN
        Quotient%Sign = -Quotient%Sign
        Remainder = -Quotient%Sign*UDiv(Quotient, -Divisor)
    ELSE
        Remainder = Quotient%Sign*UDiv(Quotient, Divisor)
    END IF

    RETURN

END SUBROUTINE ApInt32_DivMod_I32

!******************************************************************************

MODULE SUBROUTINE ApInt32_DivMod_I64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor and
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(IN)  :: Dividend
    tLong,          INTENT(IN)  :: Divisor
    TYPE(ApInt32),  INTENT(OUT) :: Quotient
    TYPE(ApInt32),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        RETURN
    ELSEIF (Divisor == 0_kLong) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_DivMod_I64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    Quotient = MakeCopy(Dividend)
    IF (Divisor < 0) THEN
        Quotient%Sign = -Quotient%Sign
        Remainder = -Quotient%Sign*UDiv(Quotient, -Divisor)
    ELSE
        Remainder = Quotient%Sign*UDiv(Quotient, Divisor)
    END IF

    RETURN

END SUBROUTINE ApInt32_DivMod_I64

!******************************************************************************

MODULE SUBROUTINE ApInt32_DivMod_I128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor and
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(IN)  :: Dividend
    TYPE(SInt128),  INTENT(IN)  :: Divisor
    TYPE(ApInt32),  INTENT(OUT) :: Quotient
    TYPE(ApInt32),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Dividend%DivMod(ApInt32(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE ApInt32_DivMod_I128

!******************************************************************************

MODULE SUBROUTINE ApInt32_DivMod_ApInt32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two ApInt32 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(IN)  :: Dividend
    TYPE(ApInt32),  INTENT(IN)  :: Divisor
    TYPE(ApInt32),  INTENT(OUT) :: Quotient
    TYPE(ApInt32),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient  = ZeroApInt32()
        Remainder = ZeroApInt32()
        CALL Handle_ErrLevel('ApInt32_DivMod_ApInt32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Remainder = UDiv(Quotient, Divisor%Digit(0))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = Dividend
        Quotient  = ZeroApInt32()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        Remainder = ZeroApInt32()
        RETURN
    END IF

    ! allocate storages
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Remainder%Digit, Dividend%Length+1_kIndex, StartID=0_kIndex)

    ! perform division
    CALL DivCore(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                 Quotient%Digit, Remainder%Digit)

    ! postprocess remainder
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    ! postprocess quotient
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END SUBROUTINE ApInt32_DivMod_ApInt32

!******************************************************************************

FUNCTION UDiv_U32(Big, U32) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt32 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The divisor (treated as unsigned)
    tInteger                        :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U32 < 0) THEN
        Rem = UDiv_Safe(Big, U32)
    ELSE
        Rem = UDiv_Unsafe(Big, U32)
    END IF

    RETURN

    CONTAINS

    FUNCTION UDiv_Unsafe(Big, Div) RESULT(Rem)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the ApInt32 by the specified integer and return the remainder
        ! Assume the divisor is greater than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: Div  ! The divisor (treated as unsigned)
        tInteger                        :: Rem  ! The remainder (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: DL, RL
        tIndex      :: I

    !** FLOW

        DL = ToUnsignedLong(Div)
        RL = 0_kLong
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            RL = SHIFTL(RL, 32) + ToUnsignedLong(Big%Digit(I))
            Big%Digit(I) = ToInteger(RL/DL)
            RL = MOD(RL, DL)
        END DO
        IF ((Big%Digit(Big%Length-1_kIndex) == 0).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
        END IF
        IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0)) Big%Sign = 1
        Rem = ToInteger(RL)

        RETURN

    END FUNCTION UDiv_Unsafe

    !**************************************************************************

    FUNCTION UDiv_Safe(Big, Div) RESULT(Rem)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the ApInt32 by the specified integer and return the remainder
        ! Assume the divisor is less than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: Div  ! The divisor (treated as unsigned)
        tInteger                        :: Rem  ! The remainder (treated as unsigned)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: HBit = ToLong(Z'8000000000000000')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: RL, DL, HQ, HRL, Q, RLS
        tIndex      :: I

    !** FLOW

        DL = ToUnsignedLong(Div)
        HQ = (HBit - 1_kLong)/DL
        IF ((HQ*DL + DL) == HBit) HQ = HQ + 1_kLong
        HRL = HBit - HQ*DL
        RL = 0_kLong
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            RL = SHIFTL(RL, 32) + ToUnsignedLong(Big%Digit(I))
            RLS = SHIFTA(RL, 63)
            Q = IAND(HQ, RLS) + (IAND(RL, HBit-1_kLong) + IAND(HRL, RLS))/DL
            RL = RL - Q*DL
            Big%Digit(I) = ToInteger(Q)
        END DO
        IF ((Big%Digit(Big%Length-1_kIndex) == 0).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
        END IF
        IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0)) Big%Sign = 1
        Rem = ToInteger(RL)

        RETURN

    END FUNCTION UDiv_Safe

    !**************************************************************************

END FUNCTION UDiv_U32

!******************************************************************************

SUBROUTINE URem_U32(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt32 with the specified integer
    ! (i.e. set Big to MOD(Big, U32)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32  ! The divisor or the amount to modulo
                                            ! (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U32 < 0) THEN
        CALL URem_Safe(Big, U32)
    ELSE
        CALL URem_Unsafe(Big, U32)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE URem_Unsafe(Big, Div)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulo of the ApInt32 with the specified integer
        ! Assume the divisor is greater than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: Div  ! The divisor (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: DL, RL
        tIndex      :: I

    !** FLOW

        DL = ToUnsignedLong(Div)
        RL = 0_kLong
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            RL = MOD(SHIFTL(RL, 32) + ToUnsignedLong(Big%Digit(I)), DL)
        END DO
        Big%Length = 1_kIndex
        Big%Digit(0) = ToInteger(RL)
        IF (Big%Digit(0) == 0) Big%Sign = 1

        RETURN

    END SUBROUTINE URem_Unsafe

    !**************************************************************************

    SUBROUTINE URem_Safe(Big, Div)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulo of the ApInt32 with the specified integer
        ! Assume the divisor is less than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: Div  ! The divisor (treated as unsigned)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: HBit = ToLong(Z'8000000000000000')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: DL, HRem, Rem
        tIndex      :: I

    !** FLOW

        DL = ToUnsignedLong(Div)
        ! Precompute hrem = (1 << 63) % d
        ! I.e. the remainder caused by the highest bit.
        HRem = MOD(HBit-1, DL)
        HRem = HRem + 1_kLong
        IF (HRem == DL) HRem = 0_kLong
        Rem = 0_kLong
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            Rem = SHIFTL(Rem, 32) + ToUnsignedLong(Big%Digit(I))
            ! Calculate rem %= d.
            ! Do this by calculating the lower 63 bits and highest bit separately.
            ! The highest bit remainder only gets added if it's set.
            Rem = MOD(IAND(Rem, HBit-1) + IAND(HRem, SHIFTA(Rem, 63)), DL)
            ! The addition is safe and cannot overflow.
            ! Because hrem < 2^32 and there's at least one zero bit in [62,32] if bit 63 is set.
        END DO
        Big%Length = 1_kIndex
        Big%Digit(0) = ToInteger(Rem)
        IF (Big%Digit(0) == 0) Big%Sign = 1

        RETURN

    END SUBROUTINE URem_Safe

    !**************************************************************************

END SUBROUTINE URem_U32

!******************************************************************************

FUNCTION UDiv_U64(Big, U64) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt32 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tLong                           :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Tmp, DH, DL, U0, U1, U2
    tLong       :: K, P, T, QHat, RHat
    tInteger    :: S
    tIndex      :: J

!** FLOW

    IF (U64 == IAND(U64, MASK)) THEN
        Rem = ToUnsignedLong(UDiv(Big, ToInteger(U64)))
        RETURN
    END IF
    IF (Big%Length == 1_kIndex) THEN
        Rem = ToUnsignedLong(Big%Digit(0))
        Big = ZeroApInt32()
        RETURN
    END IF

    S = LEADZ(ToInteger(SHIFTR(U64, 32)))
    DH = SHIFTR(U64, 32-S)
    DL = IAND(SHIFTL(U64, S), MASK)

    U2 = 0_kLong
    U1 = ToLong(SHIFTR(Big%Digit(Big%Length-1), 32-S))
    U0 = ToUnsignedLong(IOR(SHIFTL(Big%Digit(Big%Length-1), S), SHIFTR(Big%Digit(Big%Length-2), 32-S)))
    IF (S == 0) THEN
        U1 = 0_kLong
        U0 = ToUnsignedLong(Big%Digit(Big%Length-1))
    END IF

    DO J = Big%Length-2_kIndex, 0_kIndex, -1_kIndex
        U2 = U1
        U1 = U0
        IF ((S > 0).AND.(J > 0)) THEN
            U0 = ToUnsignedLong(IOR(SHIFTL(Big%Digit(J), S), SHIFTR(Big%Digit(J-1), 32-S)))
        ELSE
            U0 = ToUnsignedLong(SHIFTL(Big%Digit(J), S))
        END IF

        K = SHIFTL(U2, 32) + U1
        QHat = SHIFTL(SHIFTR(K,  1)/DH,  1)
        T = K - QHat*DH
        IF (T .UGE. DH) QHat = QHat + 1
        RHat = K - QHat*DH

        DO WHILE((QHat .UGE. SHIFTL(1_kLong, 32)).OR. &
                 (QHat*DL .UGT. SHIFTL(RHat, 32)+U0))   ! Unsigned comparison.
            QHat = QHat - 1
            RHat = RHat + DH
            IF (RHat .UGE. SHIFTL(1_kLong, 32)) EXIT
        END DO

        ! Multiply and subtract. Unfolded loop.
        P = QHat*DL
        T = U0 - IAND(P, MASK)
        U0 = IAND(T, MASK)
        K = SHIFTR(P, 32) - SHIFTA(T, 32)
        P = QHat*DH
        T = U1 - K - IAND(P, MASK)
        U1 = IAND(T, MASK)
        K = SHIFTR(P, 32) - SHIFTA(T, 32)
        T = U2 - K
        U2 = IAND(T, MASK)

        ! Store quotient digit. If we subtracted too much, add back.
        Big%Digit(J) = ToInteger(QHat)
        IF (T < 0) THEN
            ! Unfolded loop.
            Big%Digit(J) = Big%Digit(J) - 1
            T = U0 + DL
            U0 = IAND(T, MASK)
               T = SHIFTR(T, 32)
            T = U1 + DH + T
            U1 = IAND(T, MASK)
            T = SHIFTR(T, 32)
            U2 = U2 + IAND(T, MASK)
        END IF
    END DO

    Big%Length = Big%Length - 1
    Big%Digit(Big%Length) = 0
    IF ((Big%Length > 1).AND.(Big%Digit(Big%Length-1) == 0)) THEN
        Big%Length = Big%Length - 1
    END IF

    Tmp = IOR(SHIFTL(U1, 32-S), SHIFTR(U0, S))
    IF (S == 0) THEN
        Rem = Tmp
    ELSE
        Rem = IOR(SHIFTL(U2, 64-S), Tmp)
    END IF

    RETURN

END FUNCTION UDiv_U64

!******************************************************************************

SUBROUTINE URem_U64(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt32 with the specified integer
    ! (i.e. set Big to MOD(Big, U64)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Rem

!** FLOW

    Rem = UDiv(Big, U64)
    Big%Length = 2_kIndex
    Big%Digit(0) = ToInteger(Rem)
    IF (Rem == IAND(Rem, MASK)) THEN
        Big%Length = Big%Length - 1_kIndex
        RETURN
    END IF
    Big%Digit(1) = ToInteger(SHIFTR(Rem, 32))

    RETURN

END SUBROUTINE URem_U64

!******************************************************************************

SUBROUTINE DivCore(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: M            ! The length of the first array
    tInteger,           INTENT(IN)  :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,             INTENT(IN)  :: N            ! The length of the second array
    tInteger,           INTENT(IN)  :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tInteger, OPTIONAL, INTENT(OUT) :: Q(0:M-N+1)   ! The quotient array
    tInteger, OPTIONAL, INTENT(OUT) :: R(0:)        ! The remainder array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: B = SHIFTL(1_kLong, 32)      ! Number base (32 bits)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: U(0:M)   ! working dividend and remainder
    tInteger    :: V(0:N-1) ! working divisor
    tLong       :: QHat     ! estimated quotient digit
    tLong       :: RHat     ! a remainder
    tLong       :: P        ! product of two digits
    tIndex      :: I, J
    tInteger    :: S
    tLong       :: T, K, DH, DL

!** FLOW

    ! *** Hacker's Delight's implementation of Knuth's Algorithm D ***

    ! Initialize
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0
    V(0:N-1) = Dvs(0:N-1)

    ! Normalize by shifting v left just enough so that
    ! its high-order bit is on, and shift u left the
    ! same amount.  We may have to append a high-order
    ! digit on the dividend; we do that unconditionally.

    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 32-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 32-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 32-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    DH = ToUnsignedLong(V(N-1))
    DL = ToUnsignedLong(V(N-2))

    ! Main loop
    DO J = M-N, 0, -1

        ! Compute estimate QHat of Q(J).
        K = U(J+N)*B + ToUnsignedLong(U(J+N-1))
        QHat = SHIFTL(SHIFTR(K, 1)/DH, 1)
        T = K - QHat*DH
        IF (T .UGE. DH) QHat = QHat + 1
        RHat = K - QHat*DH

        ! Unsigned comparison.
        DO WHILE ((QHat .UGE. B).OR. (QHat*DL .UGT. B*RHat+ToUnsignedLong(U(J+N-2))))
            QHat = QHat - 1
            RHat = RHat + DH
            IF (RHat .UGE. B) EXIT
        END DO

        ! Multiply and subtract.
        K = 0_kLong
        DO I = 0, N-1
            P = QHat*ToUnsignedLong(V(I))
            T = ToUnsignedLong(U(I+J)) - K - IAND(P, MASK)
            U(I+J) = ToInteger(T)
            K = SHIFTR(P, 32) - SHIFTA(T, 32)
        END DO
        T = ToUnsignedLong(U(J+N)) - K
        U(J+N) = ToInteger(T)

        ! Store quotient digit. If we subtracted too much, add back.
        IF (PRESENT(Q)) Q(J) = ToInteger(QHat)
        IF (T < 0_kLong) THEN
            IF (PRESENT(Q)) Q(J) = Q(J) - 1
            K = 0_kLong
            DO I = 0, N-1
                T = ToUnsignedLong(U(I+J)) + ToUnsignedLong(V(I)) + K
                U(I+J) = ToInteger(T)
                K = SHIFTR(T, 32)
            END DO
            U(J+N) = U(J+N) + ToInteger(K)
        END IF
    END DO

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            ! Unnormalize U().
            DO I = 0, M-1
                U(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 32-S))
            END DO
            U(M) = SHIFTR(U(M), S)
        END IF
        R(0:SIZE(R)-1) = U(0:SIZE(R)-1)
    END IF

    RETURN

END SUBROUTINE DivCore

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                      OTHER OPERATIONS                    ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION ApInt32_Modulo(Dividend, Divisor) RESULT(Modulo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the modulo of the arguments

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Dividend
    TYPE(ApInt32), INTENT(IN)   :: Divisor
    TYPE(ApInt32)               :: Modulo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Modulo = MOD(Dividend, Divisor)
    IF (Dividend%Sign*Divisor%Sign < 0) Modulo = Modulo + Divisor

    RETURN

END FUNCTION ApInt32_Modulo

!******************************************************************************

MODULE FUNCTION ApInt32_Power(BigIn, Exp) RESULT(BigOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return BigOut = BigIn**Exp

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: BigIn
    tInteger,      INTENT(IN)   :: Exp      ! must be nonnegative
    TYPE(ApInt32)               :: BigOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Exp <= 1) THEN
        IF (Exp == 1) THEN
            BigOut = MakeCopy(BigIn)
        ELSEIF (Exp == 0) THEN
            BigOut = OneApInt32()
        ELSE
            BigOut = ZeroApInt32()
        END IF
        RETURN
    ELSEIF (IsZero(BigIn)) THEN
        BigOut = ZeroApInt32()
        RETURN
    END IF

    CALL MemAlloc(BigOut%Digit, BigIn%Length*Exp, StartID=0_kIndex)
    BigOut%Digit(0) = 1_kLong
    BigOut%Length   = 1_kIndex
    BigOut%Sign     = 1

    CALL ApIntPower(BigIn, Exp, BigOut)

    RETURN
    CONTAINS

    RECURSIVE SUBROUTINE ApIntPower(Base, Exponent, Output)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(IN)       :: Base
        tInteger,      INTENT(IN)       :: Exponent
        TYPE(ApInt32), INTENT(INOUT)    :: Output

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

        CALL ApIntPower(Base, SHIFTR(Exponent, 1), Output)

        CALL Output%Square()
        IF (MOD(Exponent, 2) /= 0) CALL Output%Multiply(Base)

        RETURN

    END SUBROUTINE ApIntPower

    !******************************************************************************

END FUNCTION ApInt32_Power

!******************************************************************************

MODULE FUNCTION ApInt32_Square(BigIn) RESULT(BigOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return BigOut = BigIn*BigIn

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: BigIn
    TYPE(ApInt32)               :: BigOut

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and returun quickly if possible
    IF (IsZero(BigIn)) THEN
        BigOut = ZeroApInt32()
    ELSEIF (IsOne(BigIn)) THEN
        BigOut = OneApInt32()
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

END FUNCTION ApInt32_Square

!******************************************************************************

MODULE SUBROUTINE ApInt32_SquareSub(This)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply This by Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: This

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 128

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
        ! To perform squaring of ApInt32 using a quadratic algorithm
        ! which is often suitable for smaller numbers.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT) :: This

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: ThisDig(0:This%Length-1)
        tIndex      :: ThisLen

    !** FLOW

        ThisDig = This%Digit
        ThisLen = This%Length
        CALL Square_Basic_Alloc(ThisDig, ThisLen, This%Digit)
        This%Length = SIZE(This%Digit, KIND=kIndex)
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
        TYPE(ApInt32), INTENT(INOUT) :: This

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: ThisDig(0:This%Length-1)
        tIndex      :: ThisLen

    !** FLOW

        ThisDig = This%Digit
        ThisLen = This%Length
        CALL Square_Karatsuba_Alloc(ThisDig, 0_kIndex, ThisLen, This%Digit)
        This%Length = SIZE(This%Digit, KIND=kIndex)
        DO WHILE (This%Digit(This%Length-1) == 0)
            This%Length = This%Length - 1_kIndex
        END DO
        This%Sign = 1

        RETURN

    END SUBROUTINE Square_Big

    !**************************************************************************

END SUBROUTINE ApInt32_SquareSub

!******************************************************************************

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
    tIndex,                INTENT(IN)   :: XLen         ! The length of the array
    tInteger,              INTENT(IN)   :: X(0:XLen-1)  ! The input array
    tInteger, ALLOCATABLE, INTENT(OUT)  :: Z(:)         ! The output array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use 'Multiplication' algorithm
    tIndex, PARAMETER   :: Multiply_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LastProductLowWord, K, Carry
    tLong       :: Piece, Product
    tIndex      :: I, J, Offset, ZLen
    tInteger    :: Y(0:XLen-1)

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
    ZLen = 2*XLen
    CALL MemAlloc(Z, ZLen, StartID=0_kIndex)
    Z = 0

    ! Store the squares, right shifted one bit (i.e., divided by 2)
    LastProductLowWord = 0
    I = 0
    DO J = 0, XLen - 1
        Piece = ToUnsignedLong(Y(J))
        Product = Piece * Piece
        Z(I) = IOR(SHIFTL(LastProductLowWord, 31), ToInteger(SHIFTR(Product, 33)))
        I = I + 1
        Z(I) = ToInteger(SHIFTR(Product, 1))
        I = I + 1
        LastProductLowWord = ToInteger(Product)
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
    Z(0) = IOR(Z(0), IAND(X(0), 1))

    RETURN

    CONTAINS

    SUBROUTINE ShiftLeft(X, XLen, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt32 number left by the given amount (less than 32) starting
        ! at the given digit, i.e. the first (<len) digits are left untouched.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, ALLOCATABLE, INTENT(INOUT)    :: X(:)
        tIndex,                INTENT(INOUT)    :: XLen
        tInteger,              INTENT(IN)       :: Shift  ! The amount to shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: YLen
        tInteger        :: Y(0:XLen-1)
        tInteger        :: Nxt
        tIndex          :: I

    !** FLOW

        ! get input information
        YLen = XLen
        Y = X

        ! check if X needs to be resized
        IF (SHIFTR(SHIFTL(Y(XLen-1), Shift), Shift) /= Y(XLen-1)) THEN
            ! Overflow?
            XLen = XLen + 1_kIndex
            CALL MemResize(X, XLen + 1_kIndex)
        END IF
        IF (XLen > YLen) THEN
            Nxt = 0
        ELSE
            Nxt = Y(XLen-1)
        END IF
        DO I = XLen-1, 1, -1
            X(I) = IOR(SHIFTL(Nxt, Shift), SHIFTR(Y(I-1), 32-Shift))
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
        tInteger, INTENT(INOUT) :: A(0:)    ! array to be reverse-ordered
        tIndex,   INTENT(IN)    :: IStart   ! starting index (inclusive)
        tIndex,   INTENT(IN)    :: IEnd     ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger        :: Temp
        tIndex          :: Lo
        tIndex          :: Hi

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

    SUBROUTINE MultiplyOneWord(OutVal, InVal, Offset, InLen, K, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply an array by one word K and add to result, return the carry

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(OUT)   :: OutVal(0:)   ! output array
        tInteger, INTENT(IN)    :: InVal(0:)    ! input array
        tIndex,   INTENT(IN)    :: Offset       ! offset to the output array
        tIndex,   INTENT(IN)    :: InLen        ! length of the input array
        tInteger, INTENT(IN)    :: K            ! multiplier
        tInteger, INTENT(OUT)   :: Carry        ! carry


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Product, KLong, CLong
        tIndex      :: I, Indx

    !** FLOW

        CLong = 0
        Indx = SIZE(OutVal, KIND=kIndex) - Offset - 1
        KLong = ToUnsignedLong(K)
        DO I = InLen-1, 0, -1
            Product = ToUnsignedLong(InVal(I))*KLong + ToUnsignedLong(OutVal(Indx)) + CLong
            OutVal(Indx) = ToInteger(Product)
            Indx = Indx - 1
            CLong = SHIFTR(Product, 32)
        END DO
        Carry = ToInteger(CLong)

        RETURN

    END SUBROUTINE MultiplyOneWord

    !**************************************************************************

    SUBROUTINE AddOneWord(Val, Offset, InLen, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To add one word to an array of mlen words

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Val(0:)      ! input/output array
        tIndex,   INTENT(IN)    :: Offset       ! offset to the array
        tIndex,   INTENT(IN)    :: InLen        ! offset length to the array
        tInteger, INTENT(IN)    :: Carry        ! carry


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Indx, MLen
        tLong       :: T

    !** FLOW

        Indx = SIZE(Val, KIND=kIndex) - 1_kIndex - InLen - Offset
        MLen = InLen
        T = ToUnsignedLong(Val(Indx)) + ToUnsignedLong(Carry)
        Val(Indx) = ToInteger(T)
        IF (SHIFTR(T, 32) == 0_kLong) RETURN
        DO WHILE (MLen > 0_kIndex)
            MLen = MLen - 1_kIndex
            Indx = Indx - 1_kIndex
            IF (Indx < 0_kIndex) EXIT
            Val(Indx) = Val(Indx) + 1
            IF (Val(Indx) /= 0) EXIT
        END DO

        RETURN

    END SUBROUTINE AddOneWord

    !**************************************************************************

END SUBROUTINE Square_Basic_Alloc

!******************************************************************************

RECURSIVE SUBROUTINE Square_Karatsuba_Alloc(X, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring using Karatsuba algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,              INTENT(IN)   :: X(0:)    ! The magnitude array
    tIndex,                INTENT(IN)   :: Off      ! The offset, where the first element is residing
    tIndex,                INTENT(IN)   :: N        ! The length of the partial arrays
    tInteger, ALLOCATABLE, INTENT(OUT)  :: Z(:)     ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 96

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: B, C
    tInteger, ALLOCATABLE   :: X2(:)
    tInteger, ALLOCATABLE   :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN
        ! Basecase
        CALL Square_Basic_Alloc(X(Off:), N, Z)
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Square_Karatsuba_Alloc(X, Off+B, N-B, Z2)
    CALL Square_Karatsuba_Alloc(X, Off,   B,   Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0
    ! set X2
    CALL AddHiLoParts(X, N, B, Off, X2)

    C = 0_kIndex
    IF (X2(N-B) /= 0) C = 1_kIndex
    CALL Square_Karatsuba_Alloc(X2, 0_kIndex, N-B+C, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
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
    tIndex,   INTENT(IN)    :: XLen         ! The length of the array
    tInteger, INTENT(IN)    :: X(0:XLen-1)  ! The input array
    tInteger, INTENT(OUT)   :: Z(0:)        ! The output array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use 'Multiplication' algorithm
    tIndex, PARAMETER   :: Multiply_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LastProductLowWord, K, Carry
    tLong       :: Piece, Product
    tIndex      :: I, J, Offset, ZLen
    tInteger    :: Y(0:XLen-1)

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
    Z = 0

    ! Store the squares, right shifted one bit (i.e., divided by 2)
    LastProductLowWord = 0
    I = 0
    DO J = 0, XLen - 1
        Piece = ToUnsignedLong(Y(J))
        Product = Piece * Piece
        Z(I) = IOR(SHIFTL(LastProductLowWord, 31), ToInteger(SHIFTR(Product, 33)))
        I = I + 1
        Z(I) = ToInteger(SHIFTR(Product, 1))
        I = I + 1
        LastProductLowWord = ToInteger(Product)
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
    Z(0) = IOR(Z(0), IAND(X(0), 1))

    RETURN

    CONTAINS

    SUBROUTINE ShiftLeft(X, XLen, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt32 number left by the given amount (less than 32) starting
        ! at the given digit, i.e. the first (<len) digits are left untouched.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: X(0:)
        tIndex,   INTENT(IN)    :: XLen
        tInteger, INTENT(IN)    :: Shift  ! The amount to shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I

    !** FLOW

        DO I = XLen-1, 1, -1
            X(I) = IOR(SHIFTL(X(I), Shift), SHIFTR(X(I-1), 32-Shift))
        END DO
        X(0) = SHIFTL(X(0), Shift)

        RETURN

    END SUBROUTINE ShiftLeft

    !**************************************************************************

    SUBROUTINE Reverse_Order(A, IStart, IEnd)

    !** PURPOSE OF THIS SUBROUTINE
        ! To reverse order of a segment of an array in place

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: A(0:)    ! array to be reverse-ordered
        tIndex,   INTENT(IN)    :: IStart   ! starting index (inclusive)
        tIndex,   INTENT(IN)    :: IEnd     ! ending index (inclusive)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger        :: Temp
        tIndex          :: Lo
        tIndex          :: Hi

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

    SUBROUTINE MultiplyOneWord(OutVal, OutLen, InVal, Offset, InLen, K, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply an array by one word K and add to result, return the carry

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(OUT)   :: OutVal(0:)   ! output array
        tIndex,   INTENT(IN)    :: OutLen       ! length of the output array
        tInteger, INTENT(IN)    :: InVal(0:)    ! input array
        tIndex,   INTENT(IN)    :: Offset       ! offset to the output array
        tIndex,   INTENT(IN)    :: InLen        ! length of the input array
        tInteger, INTENT(IN)    :: K            ! multiplier
        tInteger, INTENT(OUT)   :: Carry        ! carry


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Product, KLong, CLong
        tIndex      :: I, Indx

    !** FLOW

        CLong = 0
        Indx = OutLen - Offset - 1
        KLong = ToUnsignedLong(K)
        DO I = InLen-1, 0, -1
            Product = ToUnsignedLong(InVal(I))*KLong + ToUnsignedLong(OutVal(Indx)) + CLong
            OutVal(Indx) = ToInteger(Product)
            Indx = Indx - 1
            CLong = SHIFTR(Product, 32)
        END DO
        Carry = ToInteger(CLong)

        RETURN

    END SUBROUTINE MultiplyOneWord

    !**************************************************************************

    SUBROUTINE AddOneWord(Val, OutLen, Offset, InLen, Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To add one word to an array of mlen words

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: Val(0:)      ! input/output array
        tIndex,   INTENT(IN)    :: OutLen       ! length of the output array
        tIndex,   INTENT(IN)    :: Offset       ! offset to the array
        tIndex,   INTENT(IN)    :: InLen        ! offset length to the array
        tInteger, INTENT(IN)    :: Carry        ! carry


    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Indx, MLen
        tLong       :: T

    !** FLOW

        Indx = OutLen - 1_kIndex - InLen - Offset
        MLen = InLen
        T = ToUnsignedLong(Val(Indx)) + ToUnsignedLong(Carry)
        Val(Indx) = ToInteger(T)
        IF (SHIFTR(T, 32) == 0_kLong) RETURN
        DO WHILE (MLen > 0_kIndex)
            MLen = MLen - 1_kIndex
            Indx = Indx - 1_kIndex
            IF (Indx < 0_kIndex) EXIT
            Val(Indx) = Val(Indx) + 1
            IF (Val(Indx) /= 0) EXIT
        END DO

        RETURN

    END SUBROUTINE AddOneWord

    !**************************************************************************

END SUBROUTINE Square_Basic_NoAlloc

!******************************************************************************

SUBROUTINE Square_Karatsuba_NoAlloc(X, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform squaring using Karatsuba algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)   :: X(0:)    ! The magnitude array
    tIndex,   INTENT(IN)   :: Off      ! The offset, where the first element is residing
    tIndex,   INTENT(IN)   :: N        ! The length of the partial arrays
    tInteger, INTENT(OUT)  :: Z(0:)    ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 96

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: B, C
    tInteger, ALLOCATABLE   :: X2(:)
    tInteger, ALLOCATABLE   :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN
        ! Base case
        CALL Square_Basic_NoAlloc(X(Off:), N, Z)
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Square_Karatsuba_Alloc(X, Off+B, N-B, Z2)
    CALL Square_Karatsuba_Alloc(X, Off,   B,   Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0
    ! set X2
    CALL AddHiLoParts(X, N, B, Off, X2)

    C = 0_kIndex
    IF (X2(N-B) /= 0) C = 1_kIndex
    CALL Square_Karatsuba_Alloc(X2, 0_kIndex, N-B+C, Z1)

    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Square_Karatsuba_NoAlloc

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE AddHiLoParts(X1, L, K, Offset, X2)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the high and low parts of the input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: X1(0:)
    tIndex,   INTENT(IN)    :: L
    tIndex,   INTENT(IN)    :: K
    tIndex,   INTENT(IN)    :: Offset
    tInteger, INTENT(OUT)   :: X2(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry
    tIndex      :: I

!** FLOW

    Carry = 0_kLong
    DO I = 0, K-1
        Carry = ToUnsignedLong(X1(Offset+K+I)) + ToUnsignedLong(X1(Offset+I)) + Carry
        X2(I) = ToInteger(Carry)
        Carry = SHIFTR(Carry, 32)
    END DO
    IF (IAND(L, 1_kIndex) /= 0_kIndex) X2(K) = X1(Offset+K+K)
    IF (Carry /= 0_kLong) THEN
        X2(K) = X2(K) + 1
        IF (X2(K) == 0) X2(K+1) = X2(K+1) + 1
    END IF

    RETURN

END SUBROUTINE AddHiLoParts

!**************************************************************************

SUBROUTINE AddThreeParts(Z0, Z1, Z2, N, B, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,  INTENT(IN)  :: Z0(0:)
    tInteger,  INTENT(IN)  :: Z1(0:)
    tInteger,  INTENT(IN)  :: Z2(0:)
    tIndex,    INTENT(IN)  :: N
    tIndex,    INTENT(IN)  :: B
    tInteger,  INTENT(OUT) :: Z(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry
    tIndex      :: I, C

!** FLOW

    C = 2*B
    ! Add Z0
    Z(0:C-1)   = Z0(0:C-1)
    ! Add Z2
    Z(C:2*N-1) = Z2(0:2*(N-B)-1)
    ! Add Z1
    Carry = 0_kLong
    I = 0
    DO WHILE (I < C)
        Carry = ToUnsignedLong(Z(I+B)) + ToUnsignedLong(Z1(I)) - ToUnsignedLong(Z2(I)) - ToUnsignedLong(Z0(I)) + Carry
        Z(I+B) = ToInteger(Carry)
        Carry = SHIFTA(Carry, 32)
        I = I + 1
    END DO
    DO WHILE (I < 2*(N-B))
        Carry = ToUnsignedLong(Z(I+B)) + ToUnsignedLong(Z1(I)) - ToUnsignedLong(Z2(I)) + Carry
        Z(I+B) = ToInteger(Carry)
        Carry = SHIFTA(Carry, 32)
        I = I + 1
    END DO
    DO WHILE (I < SIZE(Z1, KIND=kIndex))
        Carry = ToUnsignedLong(Z(I+B)) + ToUnsignedLong(Z1(I)) + Carry
        Z(I+B) = ToInteger(Carry)
        Carry = SHIFTA(Carry, 32)
        I = I + 1
    END DO
    IF (Carry /= 0_kLong) THEN
        Z(I+B) = Z(I+B) + 1
        DO WHILE (Z(I+B) == 0)
            I = I + 1
            Z(I+B) = Z(I+B) + 1
        END DO
    END IF

    RETURN

END SUBROUTINE AddThreeParts

!******************************************************************************

END SUBMODULE SubClass_Api32_Arithmetic

!******************************************************************************

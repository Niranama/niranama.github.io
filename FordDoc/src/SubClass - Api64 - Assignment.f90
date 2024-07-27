
SUBMODULE (Class_ApInt64 : SubClass_Api64_Auxiliary) SubClass_Api64_Assignment

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to conversion
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.
!   In particular, these routines are used in an assignment expression to convert
!   between the *ApInt64* type and other signed integer types.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_Assignment'
    tLong,     PARAMETER    :: MaxI64   = ToLong(Z'7FFFFFFFFFFFFFFF')   ! max signed 64-bit

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

MODULE SUBROUTINE ApInt64_Assign(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the *ApInt64* object via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(OUT)  :: This
    TYPE(ApInt64), INTENT(IN)   :: Other

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

END SUBROUTINE ApInt64_Assign

!******************************************************************************

MODULE SUBROUTINE ApInt64_From_I32(Big, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 32-bit signed integer to an arbitrary-precision integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(OUT)  :: Big
    tInteger,      INTENT(IN)   :: I32   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I32)

    RETURN

END SUBROUTINE ApInt64_From_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_From_I64(Big, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 64-bit signed integer to an arbitrary-precision integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(OUT)  :: Big
    tLong,         INTENT(IN)   :: I64   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I64)

    RETURN

END SUBROUTINE ApInt64_From_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_From_I128(Big, I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from a 128-bit signed integer to an arbitrary-precision integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(OUT)  :: Big
    TYPE(SInt128), INTENT(IN)   :: I128   ! value treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    CALL AssignSigned(Big, I128)

    RETURN

END SUBROUTINE ApInt64_From_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_To_I32(I32, Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an arbitrary-precision integer to a 32-bit signed integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(OUT)  :: I32      ! value treated as signed
    TYPE(ApInt64), INTENT(IN)   :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: MinI32 = ToInteger(Z'80000000')
    tLong,    PARAMETER :: MaxI32 = ToLong(Z'000000007FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        ! rely on that sign always is either 1/-1
        I32 = Big%Sign*ToInteger(IAND(Big%Digit(0), MaxI32))
    ELSE
        I32 = MinI32
    END IF

    RETURN

END SUBROUTINE ApInt64_To_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_To_I64(I64, Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an arbitrary-precision integer to a 64-bit signed integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(OUT)  :: I64   ! value treated as signed
    TYPE(ApInt64), INTENT(IN)   :: Big

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MinI64 = ToLong(Z'8000000000000000')
    tLong, PARAMETER    :: MaxI32 = ToLong(Z'000000007FFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        I64 = Big%Sign*IAND(Big%Digit(0), MaxI64)
    ELSE
        I64 = MinI64
    END IF

    RETURN

END SUBROUTINE ApInt64_To_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_To_I128(I128, Big)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert from an arbitrary-precision integer to a 128-bit signed integer
    !  via an assignment expression.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(OUT)  :: I128
    TYPE(ApInt64), INTENT(IN)   :: Big

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

END SUBROUTINE ApInt64_To_I128

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

MODULE SUBROUTINE Assign_U32_To_ApInt64(Big, Sign, U32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     !! the sign of the number
    tInteger,      INTENT(IN)       :: U32      !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    Big%Sign     = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length   = 1_kIndex
    Big%Digit(0) = ToUnsignedLong(U32)

    RETURN

END SUBROUTINE Assign_U32_To_ApInt64

!******************************************************************************

MODULE SUBROUTINE Assign_U64_To_ApInt64(Big, Sign, U64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     !! the sign of the number
    tLong,         INTENT(IN)       :: U64      !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    ELSEIF (SIZE(Big%Digit) < 1) THEN
        CALL MemAlloc(Big%Digit, 1_kIndex, StartID=0_kIndex)
    END IF
    Big%Sign     = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length   = 1_kIndex
    Big%Digit(0) = U64

    RETURN

END SUBROUTINE Assign_U64_To_ApInt64

!******************************************************************************

MODULE SUBROUTINE Assign_U128_To_ApInt64(Big, Sign, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Sign     !! the sign of the number
    TYPE(UInt128), INTENT(IN)       :: U128     !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    ELSEIF (SIZE(Big%Digit) < 2) THEN
        CALL MemAlloc(Big%Digit, 2_kIndex, StartID=0_kIndex)
    END IF
    Big%Sign = Sign
    IF (Big%Sign == 0) Big%Sign = 1
    Big%Length = 2_kIndex
    Big%Digit(0) = U128%Low
    Big%Digit(1) = U128%High
    IF (Big%Digit(1) == 0) Big%Length = 1_kIndex

    RETURN

END SUBROUTINE Assign_U128_To_ApInt64

!******************************************************************************

MODULE SUBROUTINE Assign_U32_To_ApInt64_NoSign(Big, U32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given non-negative number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: U32      !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U32)

    RETURN

END SUBROUTINE Assign_U32_To_ApInt64_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_U64_To_ApInt64_NoSign(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given non-negative number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64      !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U64)

    RETURN

END SUBROUTINE Assign_U64_To_ApInt64_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_U128_To_ApInt64_NoSign(Big, U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given non-negative number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    TYPE(UInt128), INTENT(IN)       :: U128  !! the magnitude of the number treated as unsigned

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL AssignUnsigned(Big, 1, U128)

    RETURN

END SUBROUTINE Assign_U128_To_ApInt64_NoSign

!******************************************************************************

MODULE SUBROUTINE Assign_I32_To_ApInt64(Big, I32)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: I32      !! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (I32 < 0) THEN
        CALL AssignUnsigned(Big, -1, -I32)
    ELSE
        CALL AssignUnsigned(Big, 1, I32)
    END IF

    RETURN

END SUBROUTINE Assign_I32_To_ApInt64

!******************************************************************************

MODULE SUBROUTINE Assign_I64_To_ApInt64(Big, I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: I64      !! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (I64 < 0_kLong) THEN
        CALL AssignUnsigned(Big, -1, -I64)
    ELSE
        CALL AssignUnsigned(Big, 1, I64)
    END IF

    RETURN

END SUBROUTINE Assign_I64_To_ApInt64

!******************************************************************************

MODULE SUBROUTINE Assign_I128_To_ApInt64(Big, I128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign the given number to the ApInt64 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    TYPE(SInt128), INTENT(IN)       :: I128      !! the magnitude of the number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsNegative(I128)) THEN
        CALL AssignUnsigned(Big, -1, ToU128(-I128))
    ELSE
        CALL AssignUnsigned(Big, 1, ToU128(I128))
    END IF

    RETURN

END SUBROUTINE Assign_I128_To_ApInt64

!******************************************************************************

END SUBMODULE SubClass_Api64_Assignment

!******************************************************************************

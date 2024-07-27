
SUBMODULE (Class_ApInt32) SubClass_Api32_Comparison

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to comparison
!   operations of the <a href="../module/class_apint32.html">ApInt32</a> type.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! name of the module
    tCharStar, PARAMETER    :: SubName = 'SubClass_Api32_Comparison'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION ApInt32_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To test equality of two ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) == 0)

    RETURN

END FUNCTION ApInt32_Equal

!******************************************************************************

MODULE FUNCTION ApInt32_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether two ApInt32 objects are NOT equal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) /= 0)

    RETURN

END FUNCTION ApInt32_NotEqual

!******************************************************************************

MODULE FUNCTION ApInt32_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS ApInt32 object is less than the RHS ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) < 0)

    RETURN

END FUNCTION ApInt32_LessThan

!******************************************************************************

MODULE FUNCTION ApInt32_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS ApInt32 object is less than or equal to the RHS ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) <= 0)

    RETURN

END FUNCTION ApInt32_LessEqual

!******************************************************************************

MODULE FUNCTION ApInt32_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS ApInt32 object is greater than the RHS ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) > 0)

    RETURN

END FUNCTION ApInt32_GreaterThan

!******************************************************************************

MODULE FUNCTION ApInt32_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS ApInt32 object is greater than or equal to the RHS ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) >= 0)

    RETURN

END FUNCTION ApInt32_GreaterEqual

!******************************************************************************

MODULE FUNCTION ApInt32_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare LHS and RHS.
    ! - return -1 if LHS <  RHS
    ! - return  0 if LHS == RHS
    ! - return +1 if LHS >  RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(IN)  :: LHS
    TYPE(ApInt32),  INTENT(IN)  :: RHS
    tInteger                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(LHS)) THEN
        IF (IsZero(RHS)) THEN
            Flag = 0
        ELSE
            IF (RHS%Sign > 0) THEN
                Flag = -1
            ELSE
                Flag = 1
            END IF
        END IF
    ELSEIF (IsZero(RHS)) THEN
        IF (IsZero(LHS)) THEN
            Flag = 0
        ELSE
            IF (LHS%Sign > 0) THEN
                Flag = 1
            ELSE
                Flag = -1
            END IF
        END IF
    ELSE
        ! both are NOT zero
        IF (LHS%Sign == RHS%Sign) THEN
            ! both are either positive or negative
            Flag = CompareAbs(LHS, RHS)
        ELSE
            ! both have different sign
            IF (LHS%Sign > 0) THEN
                ! positive LHS and negative RHS
                Flag = 1
            ELSE
                ! negative LHS and positive RHS
                Flag = -1
            END IF
        END IF
    END IF

    RETURN

END FUNCTION ApInt32_Compare

!******************************************************************************

MODULE FUNCTION ApInt32_CompareAbs(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare the absolute value of LHS and RHS.
    ! - return -1 if ABS(LHS) <  ABS(RHS)
    ! - return  0 if ABS(LHS) == ABS(RHS)
    ! - return +1 if ABS(LHS) >  ABS(RHS)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LHS
    TYPE(ApInt32), INTENT(IN)   :: RHS
    tInteger                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    IF (LHS%Length > RHS%Length) THEN
        Flag = 1
        RETURN
    ELSEIF (LHS%Length < RHS%Length) THEN
        Flag = -1
        RETURN
    END IF
    IF ((.NOT.ALLOCATED(LHS%Digit)).OR.(.NOT.ALLOCATED(RHS%Digit))) THEN
        IF (.NOT.ALLOCATED(LHS%Digit)) THEN
            CALL Handle_ErrLevel('ApInt32_CompareAbs', SubName, ErrSevere, &
                       'LHS%Digit has not yet been allocated.')
        ELSE
            CALL Handle_ErrLevel('ApInt32_CompareAbs', SubName, ErrSevere, &
                       'RHS%Digit has not yet been allocated.')
        END IF
        Flag = 0
        RETURN
    END IF
    DO I = LHS%Length-1, 0, -1
        IF (LHS%Digit(I) /= RHS%Digit(I)) THEN
            IF (ToUnsignedLong(LHS%Digit(I)) > ToUnsignedLong(RHS%Digit(I))) THEN
                Flag = 1
            ELSE
                Flag = -1
            END IF
            RETURN
        END IF
    END DO
    Flag = 0

    RETURN

END FUNCTION ApInt32_CompareAbs

!******************************************************************************

END SUBMODULE SubClass_Api32_Comparison

!******************************************************************************

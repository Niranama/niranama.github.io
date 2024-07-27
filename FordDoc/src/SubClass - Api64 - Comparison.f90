
SUBMODULE (Class_ApInt64) SubClass_Api64_Comparison

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to comparison
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName = 'SubClass_Api64_Comparison'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE FUNCTION ApInt64_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
   ! To check whether two ApInt64 numbers are equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) == 0)

    RETURN

END FUNCTION ApInt64_Equal

!******************************************************************************

MODULE FUNCTION ApInt64_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether two ApInt64 numbers are NOT equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) /= 0)

    RETURN

END FUNCTION ApInt64_NotEqual

!******************************************************************************

MODULE FUNCTION ApInt64_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS number is less than the RHS number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) < 0)

    RETURN

END FUNCTION ApInt64_LessThan

!******************************************************************************

MODULE FUNCTION ApInt64_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS number is less than or equal to the RHS number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) <= 0)

    RETURN

END FUNCTION ApInt64_LessEqual

!******************************************************************************

MODULE FUNCTION ApInt64_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS number is greater than the RHS number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) > 0)

    RETURN

END FUNCTION ApInt64_GreaterThan

!******************************************************************************

MODULE FUNCTION ApInt64_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS number is greater than or equal to the RHS number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (Compare(LHS, RHS) >= 0)

    RETURN

END FUNCTION ApInt64_GreaterEqual

!******************************************************************************

MODULE FUNCTION ApInt64_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS objects. <br>
    ! - Return -1 if LHS < RHS. <br>
    ! - Return  0 if LHS == RHS. <br>
    ! - Return +1 if LHS > RHS.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: LHS
    TYPE(ApInt64),  INTENT(IN)  :: RHS
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

END FUNCTION ApInt64_Compare

!******************************************************************************

MODULE FUNCTION ApInt64_CompareAbs(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare the absolute value of LHS and RHS. <br>
    ! - Return -1 if ABS(LHS) <  ABS(RHS). <br>
    ! - Return  0 if ABS(LHS) == ABS(RHS). <br>
    ! - Return +1 if ABS(LHS) >  ABS(RHS).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LHS
    TYPE(ApInt64), INTENT(IN)   :: RHS
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
        IF ((.NOT.ALLOCATED(LHS%Digit)).AND.(.NOT.ALLOCATED(RHS%Digit))) THEN
            CALL Handle_ErrLevel('ApInt64_CompareAbs', SubName, ErrSevere, &
                       'Digits of both LHS and RHS have not yet been allocated.')
        ELSEIF (.NOT.ALLOCATED(LHS%Digit)) THEN
            CALL Handle_ErrLevel('ApInt64_CompareAbs', SubName, ErrSevere, &
                       'LHS%Digit has not yet been allocated.')
        ELSE
            CALL Handle_ErrLevel('ApInt64_CompareAbs', SubName, ErrSevere, &
                       'RHS%Digit has not yet been allocated.')
        END IF
        Flag = 0
        RETURN
    END IF
    DO I = LHS%Length-1, 0, -1
        IF (LHS%Digit(I) /= RHS%Digit(I)) THEN
            IF (LHS%Digit(I) .UGT. RHS%Digit(I)) THEN
                Flag = 1
            ELSE
                Flag = -1
            END IF
            RETURN
        END IF
    END DO
    Flag = 0

    RETURN

END FUNCTION ApInt64_CompareAbs

!******************************************************************************

END SUBMODULE SubClass_Api64_Comparison

!******************************************************************************


SUBMODULE (ModBase_SInt128) SubBase_I128_Comparison

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to comparison
!   operations of the <a href="../module/modbase_sint128.html">SInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tSInt128        TYPE(SInt128)

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

MODULE FUNCTION I128_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether two SInt128 objects are equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High == RHS%High).AND.(LHS%Low == RHS%Low)

    RETURN

END FUNCTION I128_Equal

!******************************************************************************

MODULE FUNCTION I128_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether two SInt128 objects are NOT equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High /= RHS%High).OR.(LHS%Low /= RHS%Low)

    RETURN

END FUNCTION I128_NotEqual

!******************************************************************************

MODULE FUNCTION I128_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS SInt128 object is less than the RHS SInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        ! Flag = (LHS%Low .ULT. RHS%Low)
        Flag = (IEOR(LHS%Low, MinI64) < IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High < RHS%High)
    END IF

    RETURN

END FUNCTION I128_LessThan

!******************************************************************************

MODULE FUNCTION I128_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS SInt128 object is less than or equal to the RHS SInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Flag = .NOT.(LHS > RHS)
    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) <= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High <= RHS%High)
    END IF

    RETURN

END FUNCTION I128_LessEqual

!******************************************************************************

MODULE FUNCTION I128_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS SInt128 object is greater than the RHS SInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        ! Flag = (LHS%Low .UGT. RHS%Low)
        Flag = (IEOR(LHS%Low, MinI64) > IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High > RHS%High)
    END IF

    RETURN

END FUNCTION I128_GreaterThan

!******************************************************************************

MODULE FUNCTION I128_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS SInt128 object is greater than or equal to the RHS SInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Flag = .NOT.(LHS < RHS)
    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) >= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (LHS%High >= RHS%High)
    END IF

    RETURN

END FUNCTION I128_GreaterEqual

!******************************************************************************

MODULE FUNCTION I128_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS objects. <br>
    ! - Return -1 if LHS < RHS. <br>
    ! - Return  0 if LHS == RHS. <br>
    ! - Return +1 if LHS > RHS.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LHS
    tSInt128, INTENT(IN)    :: RHS
    tSInt32                 :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: ULHS, URHS

!** FLOW

    IF (LHS%High < RHS%High) THEN
        Flag = -1
    ELSEIF (LHS%High > RHS%High) THEN
        Flag = +1
    ELSE
        ULHS = IEOR(LHS%Low, MinI64)
        URHS = IEOR(RHS%Low, MinI64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF
    END IF

    RETURN

END FUNCTION I128_Compare

!******************************************************************************

END SUBMODULE SubBase_I128_Comparison

!******************************************************************************

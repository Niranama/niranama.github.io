
SUBMODULE (ModBase_UInt128) SubBase_U128_Comparison

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to comparison
!   operations of the <a href="../module/modbase_uint128.html">UInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tUInt128        TYPE(UInt128)

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

MODULE FUNCTION U128_Equal(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether two Uint128 objects are equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High == RHS%High).AND.(LHS%Low == RHS%Low)

    RETURN

END FUNCTION U128_Equal

!******************************************************************************

MODULE FUNCTION U128_NotEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether two Uint128 objects are NOT equal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (LHS%High /= RHS%High).OR.(LHS%Low /= RHS%Low)

    RETURN

END FUNCTION U128_NotEqual

!******************************************************************************

MODULE FUNCTION U128_LessThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS Uint128 object is less than the RHS Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) < IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) < IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_LessThan

!******************************************************************************

MODULE FUNCTION U128_LessEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS Uint128 object is less than or equal to the RHS Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) <= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) <= IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_LessEqual

!******************************************************************************

MODULE FUNCTION U128_GreaterThan(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS Uint128 object is greater than the RHS Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) > IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) > IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_GreaterThan

!******************************************************************************

MODULE FUNCTION U128_GreaterEqual(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the LHS Uint128 object is greater than or equal to the RHS Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS%High == RHS%High) THEN
        Flag = (IEOR(LHS%Low, MinI64) >= IEOR(RHS%Low, MinI64))
    ELSE
        Flag = (IEOR(LHS%High, MinI64) >= IEOR(RHS%High, MinI64))
    END IF

    RETURN

END FUNCTION U128_GreaterEqual

!******************************************************************************

MODULE FUNCTION U128_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS objects. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LHS
    tUInt128, INTENT(IN)    :: RHS
    tSInt32                 :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: ULHS, URHS

!** FLOW

    ULHS = IEOR(LHS%High, MinI64)
    URHS = IEOR(RHS%High, MinI64)
    IF (ULHS < URHS) THEN
        Flag = -1
    ELSEIF (ULHS > URHS) THEN
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

END FUNCTION U128_Compare

!******************************************************************************

END SUBMODULE SubBase_U128_Comparison

!******************************************************************************

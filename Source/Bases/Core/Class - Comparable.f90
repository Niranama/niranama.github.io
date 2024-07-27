
MODULE Class_Comparable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Comparable* type and its related routines.
!   The *Comparable* type is an abstract data type representing an object
!   that can be compared to itself and/or other object in the *Comparable*
!   class.  The comparison provides a total ordering on the *Comparable*
!   objects.  This ordering is referred to as the class's natural ordering,
!   and the class's *CompareTo* method is referred to as its natural
!   comparison method. <br>
!   The *Comparable* type defines an application programming interface (API)
!   for typical relational comparisons.  A *concrete* derived type that extends
!   the *Comparable* type must implement the *CompareTo* deferred procedure.
!   Because the *Comparable* type is a subtype of the *Assignable* type, all
!   extended types must also implement those deferred procedures required by
!   a subtype of the *Assignable* type.

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Assignable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Comparable
    PUBLIC :: ASSIGNMENT(=)     ! inherited from 'Class_Assignable' module

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_Comparable'

!** DERIVED TYPE 
    !> A *Comparable* type is an abstract data type that can be compared to
    !   itself.  When two *Comparable* objects are compared, we can specify
    !   whether they are less than, greater than or equal to one another or not.
    !   This data type can also be called a *Sortable* type because it can be
    !   used with a sorting algorithm.
    TYPE, ABSTRACT, EXTENDS(Assignable) :: Comparable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CompareTo* is a binding name of the *CompareObj* deferred procedure.
        !  Use relational operators provided in place of the *CompareTo* method
        !  to compare two *Comparable* objects.
        PROCEDURE(CompareObj), DEFERRED  :: CompareTo
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Comparable_EqualTo
        PROCEDURE, PRIVATE  :: Comparable_NotEqualTo
        PROCEDURE, PRIVATE  :: Comparable_LessThan
        PROCEDURE, PRIVATE  :: Comparable_GreaterThan
        PROCEDURE, PRIVATE  :: Comparable_LessEqual
        PROCEDURE, PRIVATE  :: Comparable_GreaterEqual
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if the LHS value is equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS == RHS) DoSomething
        GENERIC     :: OPERATOR(==) => Comparable_EqualTo
        !> **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if the LHS value is NOT equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS /= RHS) DoSomething
        GENERIC     :: OPERATOR(/=) => Comparable_NotEqualTo
        !> **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS < RHS) DoSomething
        GENERIC     :: OPERATOR(<)  => Comparable_LessThan
        !> **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS > RHS) DoSomething
        GENERIC     :: OPERATOR(>)  => Comparable_GreaterThan
        !> **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS <= RHS) DoSomething
        GENERIC     :: OPERATOR(<=) => Comparable_LessEqual
        !> **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS >= RHS) DoSomething
        GENERIC     :: OPERATOR(>=) => Comparable_GreaterEqual
    END TYPE Comparable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *CompareObj* is a deferred procedure to compare two *Comparable*
        !  objects where the output flag should be set to the following value: <br>
        !   1 (or positive value) if A is greater than B, <br>
        !   0 if A is equal to B, <br>
        !  -1 (or negative value) if A is less than B. <br>
        !  It is important to note that the implementation of the *CompareObj*
        !  procedure should be consistent with the *EqualObj* procedure* such
        !  that the expressions (A == B) and (A%CompareTo(B) == 0) should always
        !  return the same value.
        FUNCTION CompareObj(A,B) RESULT(Flag)
            IMPORT
            CLASS(Comparable), INTENT(IN)   :: A
            CLASS(Comparable), INTENT(IN)   :: B
            tInteger                        :: Flag
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Comparable_EqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) == 0)

    RETURN

END FUNCTION Comparable_EqualTo

!******************************************************************************

FUNCTION Comparable_NotEqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is NOT equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) /= 0)

    RETURN

END FUNCTION Comparable_NotEqualTo

!******************************************************************************

FUNCTION Comparable_LessThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) < 0)

    RETURN

END FUNCTION Comparable_LessThan

!******************************************************************************

FUNCTION Comparable_GreaterThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) > 0)

    RETURN

END FUNCTION Comparable_GreaterThan

!******************************************************************************

FUNCTION Comparable_LessEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) <= 0)

    RETURN

END FUNCTION Comparable_LessEqual

!******************************************************************************

FUNCTION Comparable_GreaterEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Comparable), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) >= 0)

    RETURN

END FUNCTION Comparable_GreaterEqual

!******************************************************************************

END MODULE Class_Comparable

!******************************************************************************

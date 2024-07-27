
MODULE Class_Hashable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Hashable* type and its related routines.
!   The *Hashable* type is an abstract data type representing an object
!   that can be converted to a hash value.  The *Hashable* type and its
!   concrete extended types are intended to be used with hash tables. <br>
!   A *concrete* extended type that extends the *Hashable* type must
!   implement the *ComputeHashValue* deferred procedure.  Because the 
!   the *Hashable* type is a subtype of the *Comparable* type, all
!   extended types must also implement those deferred procedures required
!   by a subtype of the *Comparable* type.

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Hashable
    PUBLIC :: ASSIGNMENT(=)     ! inherited from 'Class_Comparable' module

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash       tIndex

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_Hashable'

!** DERIVED TYPE 
    !> A *Hashable* type is an abstract data type that can be converted to a
    !   hash value.
    TYPE, ABSTRACT, EXTENDS(Comparable) :: Hashable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *ComputeHashValue* is a binding name of the *HashFunc* deferred procedure. <br>
        !  Use the *HashCode* method in place of the *ComputeHashValue* method to
        !  compute hash code of a *Hashable* object.
        PROCEDURE(HashFunc), DEFERRED   :: ComputeHashValue
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: HashCode <br>
        !  **Purpose**:  To compute hash code of the given key (a *Hashable* object). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Key%HashCode()
        GENERIC     :: HashCode => ComputeHashValue
    END TYPE Hashable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *HashFunc* is a deferred procedure to compute hash code
        ! for the given key.
        FUNCTION HashFunc(Key) RESULT(Code)
            IMPORT
            CLASS(Hashable), INTENT(IN) :: Key
            tHash                       :: Code
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

END MODULE Class_Hashable

!******************************************************************************

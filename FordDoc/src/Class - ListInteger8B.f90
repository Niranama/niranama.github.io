!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

MODULE Class_ListInteger8B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ListInteger8B* type and related routines.
!   The *ListInteger8B* type is a container with *INTEGER(KIND=kI8B)* as the type
!   of its stored items.  It employs a conventional doubly-linked list
!   implementation. <br>
!   See the <a href="../module/modbase_doublylinkedlists.html">ModBase_DoublyLinkedLists</a>
!   module for an overview and usage notes of a *doubly-linked-list-based* type.
!   A user may use the *ModBase_DoublyLinkedLists* module instead of using this
!   module directly. <br>
!   See the <a href="../module/class_linkedlists.html">Class_LinkedLists</a>
!   module for doubly-linked-list-based types of containers that are functionally
!   similar to the *ListInteger8B* type but utilizes a different implementation.
!   Also, unlike the *ListInteger8B* type, these container types are designed as
!   generic containers that can be used to store various data types providing that
!   the size (in bytes) of the data to be stored is known at compile time.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ListInteger8B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#define IS_EQUAL(A, B)      (A == B)
#define DblLnkList          ListInteger8B
#define DLLNode             DLLNodeInteger8B
#define ItemTypeA           INTEGER(KIND=kI8B)
#define ItemTypeB           INTEGER(KIND=kI8B)
#define ItemTypeC           INTEGER(KIND=kI8B)
#define TypeOfItem          INTEGER(KIND=kI8B)

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_ListInteger8B'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic List - Declaraction.f90"

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic List - Implementation.f90"

!** UNDEFINE MACROS **
#undef DblLnkList
#undef DLLNode
#undef ItemTypeA
#undef ItemTypeB
#undef ItemTypeC
#undef TypeOfItem

END MODULE Class_ListInteger8B

!******************************************************************************
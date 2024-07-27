
MODULE ModBase_MemHandlers_Hashable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains memory management routines for a *Hashable* class.

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Hashable
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"
#include    "../MacroDef/Macro - Util Definitions.f90"
#include    "Includes/Macro - MemHandling.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Base_MemHandlers_Hashable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    !> **Subroutine Interface**: MemFree <br>
    !  **Purpose**:  To free memory of an array. <br>
    !  **Usage**: <br>
    !   --->    CALL MemFree(Arr1D) <br>
    !   --->    CALL MemFree(Arr2D) <br>
    !   --->    CALL MemFree(Arr3D)
    INTERFACE MemFree
        ! +++ allocatable arrays +++
        MODULE PROCEDURE Allocatable_Hashable_MemFree_1D
        MODULE PROCEDURE Allocatable_Hashable_MemFree_2D
        MODULE PROCEDURE Allocatable_Hashable_MemFree_3D
        ! +++ pointer arrays +++
        MODULE PROCEDURE Pointer_Hashable_MemFree_1D
        MODULE PROCEDURE Pointer_Hashable_MemFree_2D
        MODULE PROCEDURE Pointer_Hashable_MemFree_3D
    END INTERFACE

    !> **Subroutine Interface**: MemAlloc <br>
    !  **Purpose**:  To allocate memory of an array. <br>
    !  **Note**:  For optional starting ID(s), the default value is 1. <br>
    !  **Usage**: <br>
    !   --->    CALL MemAlloc(Arr1D, 10, ArrMold) <br>
    !   --->    CALL MemAlloc(Arr1D, 100, ArrMold, StartID=11) <br>
    !   --->    CALL MemAlloc(Arr2D, 20, 10, ArrMold) <br>
    !   --->    CALL MemAlloc(Arr2D, 100, 50, ArrMold, StartID1=21, StartID2=-10) <br>
    !   --->    CALL MemAlloc(Arr3D, 50, 50, 50, ArrMold) <br>
    !   --->    CALL MemAlloc(Arr3D, 20, 30, 40, ArrMold, StartID2=-10) <br>
    !   --->    CALL MemAlloc(Arr3D, 10, 10, 10, ArrMold, 0, 0, 0)
    INTERFACE MemAlloc
        ! +++ allocatable arrays +++
        MODULE PROCEDURE Allocatable_Hashable_MemAlloc_1D
        MODULE PROCEDURE Allocatable_Hashable_MemAlloc_2D
        MODULE PROCEDURE Allocatable_Hashable_MemAlloc_3D
        ! +++ pointer arrays +++
        MODULE PROCEDURE Pointer_Hashable_MemAlloc_1D
        MODULE PROCEDURE Pointer_Hashable_MemAlloc_2D
        MODULE PROCEDURE Pointer_Hashable_MemAlloc_3D
    END INTERFACE

    !> **Subroutine Interface**: MemResize <br>
    !  **Purpose**:  To reallocate memory for an array and preserve its data. <br>
    !  **Usage**: <br>
    !   --->    CALL MemResize(Arr1D, 100) <br>
    !   --->    CALL MemResize(Arr2D, 20, 30) <br>
    !   --->    CALL MemResize(Arr3D, 50, 20, 100)
    INTERFACE MemResize
        ! +++ allocatable arrays +++
        MODULE PROCEDURE Allocatable_Hashable_MemResize_1D
        MODULE PROCEDURE Allocatable_Hashable_MemResize_2D
        MODULE PROCEDURE Allocatable_Hashable_MemResize_3D
        ! +++ pointer arrays +++
        MODULE PROCEDURE Pointer_Hashable_MemResize_1D
        MODULE PROCEDURE Pointer_Hashable_MemResize_2D
        MODULE PROCEDURE Pointer_Hashable_MemResize_3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           FREE-UP MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName       ALLOCATABLE
#define ArrayStatusTest(A)  ALLOCATED(A)
! Hashable
#define TypeName            CLASS(Hashable)
#define Generic_MemFree_1D  Allocatable_Hashable_MemFree_1D
#define Generic_MemFree_2D  Allocatable_Hashable_MemFree_2D
#define Generic_MemFree_3D  Allocatable_Hashable_MemFree_3D
#include    "Includes/Assignable MemFree - Implementation.f90"
#undef TypeName
#undef Generic_MemFree_1D
#undef Generic_MemFree_2D
#undef Generic_MemFree_3D
! undefine for allocatable arrays
#undef AttributeName
#undef ArrayStatusTest
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName       POINTER
#define ArrayStatusTest(A)  ASSOCIATED(A)
! Hashable
#define TypeName            CLASS(Hashable)
#define Generic_MemFree_1D  Pointer_Hashable_MemFree_1D
#define Generic_MemFree_2D  Pointer_Hashable_MemFree_2D
#define Generic_MemFree_3D  Pointer_Hashable_MemFree_3D
#include    "Includes/Assignable MemFree - Implementation.f90"
#undef TypeName
#undef Generic_MemFree_1D
#undef Generic_MemFree_2D
#undef Generic_MemFree_3D
! undefine for pointer arrays
#undef AttributeName
#undef ArrayStatusTest

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName       ALLOCATABLE
! Hashable
#define TypeName            CLASS(Hashable)
#define Generic_MemAlloc_1D Allocatable_Hashable_MemAlloc_1D
#define Generic_MemAlloc_2D Allocatable_Hashable_MemAlloc_2D
#define Generic_MemAlloc_3D Allocatable_Hashable_MemAlloc_3D
#include    "Includes/Assignable MemAlloc - Implementation.f90"
#undef TypeName
#undef Generic_MemAlloc_1D
#undef Generic_MemAlloc_2D
#undef Generic_MemAlloc_3D
! undefine for allocatable arrays
#undef AttributeName
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName       POINTER
! Hashable
#define TypeName            CLASS(Hashable)
#define Generic_MemAlloc_1D Pointer_Hashable_MemAlloc_1D
#define Generic_MemAlloc_2D Pointer_Hashable_MemAlloc_2D
#define Generic_MemAlloc_3D Pointer_Hashable_MemAlloc_3D
#include    "Includes/Assignable MemAlloc - Implementation.f90"
#undef TypeName
#undef Generic_MemAlloc_1D
#undef Generic_MemAlloc_2D
#undef Generic_MemAlloc_3D
! undefine for pointer arrays
#undef AttributeName

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                       RE-ALLOCATE MEMORY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! ----------------- ALLOCATABLE ARRAYS -------------------
#define AttributeName           ALLOCATABLE
#define ArrayStatusTest(A)      ALLOCATED(A)
#define MEM_MOVE(A,B)           MOVE_ALLOCATABLE_ARRAY(A, B)
! Hashable
#define TypeName                CLASS(Hashable)
#define Generic_MemResize_1D    Allocatable_Hashable_MemResize_1D
#define Generic_MemResize_2D    Allocatable_Hashable_MemResize_2D
#define Generic_MemResize_3D    Allocatable_Hashable_MemResize_3D
#include    "Includes/Assignable MemResize - Implementation.f90"
#undef TypeName
#undef Generic_MemResize_1D
#undef Generic_MemResize_2D
#undef Generic_MemResize_3D
! undefine for allocatable arrays
#undef AttributeName
#undef ArrayStatusTest
#undef MEM_MOVE
!
! ----------------- POINTER ARRAYS -------------------
#define AttributeName           POINTER
#define ArrayStatusTest(A)      ASSOCIATED(A)
#define MEM_MOVE(A,B)           MOVE_POINTER_ARRAY(A, B)
! Hashable
#define TypeName                CLASS(Hashable)
#define Generic_MemResize_1D    Pointer_Hashable_MemResize_1D
#define Generic_MemResize_2D    Pointer_Hashable_MemResize_2D
#define Generic_MemResize_3D    Pointer_Hashable_MemResize_3D
#include    "Includes/Assignable MemResize - Implementation.f90"
#undef TypeName
#undef Generic_MemResize_1D
#undef Generic_MemResize_2D
#undef Generic_MemResize_3D
! undefine for pointer arrays
#undef AttributeName
#undef ArrayStatusTest
#undef MEM_MOVE

!**************************************************************************************

END MODULE ModBase_MemHandlers_Hashable

!******************************************************************************

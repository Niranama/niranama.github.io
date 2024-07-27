
MODULE ModBase_MemHandlers_Misc

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains memory management routines for special derived types
!   declared in *ModBase_Common* module.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_MemHandlers_Integer
    USE ModBase_MemHandlers_Real
    USE ModBase_MemHandlers_Character
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: MemFree
    PUBLIC :: MemAlloc
    PUBLIC :: MemResize

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Base_MemHandlers_Misc'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    !> **Subroutine Interface**: MemFree <br>
    !  **Purpose**:  To free memory of allocatable components. <br>
    !  **Usage**: <br>
    !   --->    CALL MemFree(UPar) <br>
    !   --->    CALL MemFree(EqText) <br>
    !   --->    CALL MemFree(WkSpace)
    INTERFACE MemFree
        MODULE PROCEDURE DestroyUserParam
        MODULE PROCEDURE DestroyEquation
        MODULE PROCEDURE DestroyWorkSpace
    END INTERFACE

    !> **Subroutine Interface**: MemAlloc <br>
    !  **Purpose**:  To allocate memory for allocatable components. <br>
    !  **Usage**: <br>
    !   --->    CALL MemAlloc(UPar, 5, 2) <br>
    !   --->    CALL MemAlloc(EqText, 4, 5) <br>
    !   --->    CALL MemAlloc(WkSpace, 10, 10)
    INTERFACE MemAlloc
        MODULE PROCEDURE CreateUserParam
        MODULE PROCEDURE CreateEquation
        MODULE PROCEDURE CreateWorkSpace
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                         CONSTRUCTOR AND DESTRUCTOR ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE CreateUserParam(UPar,NR,NI)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create UserParam object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UserParam), INTENT(INOUT)  :: UPar
    tIndex,          INTENT(IN)     :: NR
    tIndex,          INTENT(IN)     :: NI
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((NR <= 0).OR.(NI <= 0)) THEN
        CALL Handle_ErrLevel('CreateUserParam', ModName, ErrSevere, &
                             'Both "NR" and "NI" must be positive integer.')
    END IF

    ! create UserParam object
    UPar%NR = NR
    UPar%NI = NI
    CALL MemAlloc(UPar%RPar, UPar%NR)
    CALL MemAlloc(UPar%IPar, UPar%NI)

    RETURN

END SUBROUTINE CreateUserParam

!**************************************************************************************

SUBROUTINE DestroyUserParam(UPar)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UserParam), INTENT(INOUT)    :: UPar
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    UPar%NR = 0
    UPar%NI = 0
    CALL MemFree(UPar%IPar)
    CALL MemFree(UPar%RPar)

    RETURN

END SUBROUTINE DestroyUserParam

!**************************************************************************************

SUBROUTINE CreateWorkSpace(Work,LRW,LIW)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create WorkSpace object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(WorkSpace), INTENT(INOUT)  :: Work
    tIndex,          INTENT(IN)     :: LRW
    tIndex,          INTENT(IN)     :: LIW
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((LRW <= 0).OR.(LIW <= 0)) THEN
        CALL Handle_ErrLevel('CreateWorkSpace', ModName, ErrSevere, &
                             'Both "LRW" and "LIW" must be positive integer.')
    END IF

    ! create WorkSpace object
    Work%LRW = LRW
    Work%LIW = LIW
    CALL MemAlloc(Work%RVar, Work%LRW)
    CALL MemAlloc(Work%IVar, Work%LIW)

    RETURN

END SUBROUTINE CreateWorkSpace

!**************************************************************************************

SUBROUTINE DestroyWorkSpace(Work)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(WorkSpace), INTENT(INOUT)    :: Work
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Work%LRW = 0
    Work%LIW = 0
    CALL MemFree(Work%IVar)
    CALL MemFree(Work%RVar)

    RETURN

END SUBROUTINE DestroyWorkSpace

!**************************************************************************************

SUBROUTINE CreateEquation(EQ,NEQ,NVR)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create Equation object and allocate memory for allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Equation), INTENT(INOUT)   :: EQ
    tIndex,         INTENT(IN)      :: NEQ
    tIndex,         INTENT(IN)      :: NVR
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check validity for input data
    IF ((NEQ <= 0).OR.(NVR <= 0)) THEN
        CALL Handle_ErrLevel('CreateEquation', ModName, ErrSevere, &
                             'Both "NEQ" and "NVR" must be positive integer.')
    END IF

    ! create Equation object
    EQ%NEQ = NEQ
    EQ%NVR = NVR
    CALL MemAlloc(256_kIndex, EQ%EQText,  NEQ)
    CALL MemAlloc(30_kIndex,  EQ%VarText, NVR)
    CALL MemAlloc(EQ%Values,  NVR)

    RETURN

END SUBROUTINE CreateEquation

!**************************************************************************************

SUBROUTINE DestroyEquation(EQ)

!** PURPOSE OF THIS SUBROUTINE:
    !! To reset components and free memory of allocatable components.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Equation), INTENT(INOUT)    :: EQ
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    EQ%NEQ = 0
    EQ%NVR = 0
    CALL MemFree(EQ%EQText)
    CALL MemFree(EQ%VarText)
    CALL MemFree(EQ%Values)

    RETURN

END SUBROUTINE DestroyEquation

!**************************************************************************************

END MODULE ModBase_MemHandlers_Misc

!******************************************************************************

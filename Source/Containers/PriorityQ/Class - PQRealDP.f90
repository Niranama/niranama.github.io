
MODULE Class_PQRealDP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PQRealDP* type and its related routines.
!   The *PQRealDP* type is a priority-queue container with *REAL(KIND=kDP)*
!   as the type of its stored keys.  It employs a binary heap implementation
!   to order its stored keys. <br>
!   The *PQRealDP* type can represent either the max-priority queue or the
!   min-priority queue.  By default, it represents the max-priority queue but
!   a user can specify the *MinPQ* argument to true so that it represents
!   the min-priority queue instead. <br>
!   See the <a href="../module/modbase_priorityqueues.html">ModBase_PriorityQueues</a>
!   module for an overview of a *priority-queue-based* type. A user may use the
!   *ModBase_PriorityQueues* module instead of using this module directly. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PQRealDP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define     KeyType     REAL(KIND=kDP)
#define     PQHeap      PQRealDP

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'Class_PQRealDP'

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic PQHeap - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic PQHeap - Implementation.f90"

END MODULE Class_PQRealDP

!******************************************************************************
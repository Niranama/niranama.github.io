
SUBMODULE (Class_LinkedLists) SubClass_StackLinked

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *StackLinked* type that represents
!   a last-in-first-out (LIFO) stack with a doubly-linked list data structure
!   based on an intrusive linked list implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_StackLinked'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

MODULE FUNCTION StackLinked_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the StackLinked type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackLinked(*)), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'StackLinked'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION StackLinked_GetTypeName

! ---------------------------------------------------------------------
! -----                     Stack Procedures                      -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE StackLinked_Push(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(StackLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be added to the collection
    TYPE(*),               INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode(Collection%ValSize)), POINTER   :: NewNode
    tInteger                                        :: AllocStat
    tCharLen(MsgLen)                                :: AllocMsg

! FLOW
        
    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('StackLinked_Push', ModName, AllocMsg, AllocStat)
        
    ! set item to the new node
    CALL NewNode%Store%Set(Item)
        
    ! append the new node to the tail
    CALL Collection%WrkLst%AddLast(NewNode)
        
    ! free up pointer
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE StackLinked_Push

!**************************************************************************************

MODULE FUNCTION StackLinked_Pop(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the top (last) item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackLinked object
    CLASS(StackLinked(*)), INTENT(INOUT) :: Collection
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: LastNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg

! FLOW
        
    ! check whether we can get and remove the last node successfully or not
    IF (Collection%WrkLst%RemoveLast(LastNode)) THEN
        ! set flag
        Flag = TrueVal

        ! get the item stored in the node
        SELECT TYPE (LastNode)
        TYPE IS (LinkedNode(*))
            CALL LastNode%Store%Get(Item)
        END SELECT

        ! nullify pointer components of the node
        CALL LastNode%FreePointers()
    
        ! deallocate the node
        DEALLOCATE(LastNode, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('StackLinked_Pop', ModName, AllocMsg, AllocStat)
    ELSE
        ! set flag
        Flag = FalseVal
    END IF

    NULLIFY(LastNode)

    RETURN

END FUNCTION StackLinked_Pop

!**************************************************************************************

MODULE FUNCTION StackLinked_PeekTop(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the top (last) item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackLinked object
    CLASS(StackLinked(*)), INTENT(IN)    :: Collection
    !% the item to be retrieved from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER   :: LastNode

! FLOW

    ! get the last node
    LastNode => Collection%WrkLst%GetTail()
    
    IF (ASSOCIATED(LastNode)) THEN
        ! get the item stored in the node
        SELECT TYPE (LastNode)
        TYPE IS (LinkedNode(*))
            CALL LastNode%Store%Get(Item)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    NULLIFY(LastNode)

    RETURN

END FUNCTION StackLinked_PeekTop

!**************************************************************************************

END SUBMODULE SubClass_StackLinked

!******************************************************************************

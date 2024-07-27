
SUBMODULE (Class_LinkedLists) SubClass_DequeLinked

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *DequeLinked* type that represents
!   a double-ended queue (deque) with a doubly-linked list data structure
!   based on an intrusive linked list implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_DequeLinked'
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

MODULE FUNCTION DequeLinked_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the DequeLinked type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DequeLinked(*)), INTENT(IN)   :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'DequeLinked'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION DequeLinked_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION DequeLinked_Move2LastElm(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last element in the collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the last element as output if requested (and available)
    TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
    !> a flag indicating whether the collection contains no element or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the last element is available.
    tLogical                                :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER    :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkLst%StartLast(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (LinkedNode(*))
                    CALL CurrNode%Store%Get(Item)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkLst%StartLast()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = -1

    RETURN

END FUNCTION DequeLinked_Move2LastElm

!**************************************************************************************

MODULE FUNCTION DequeLinked_Move2PrevElm(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous element in the collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the previous element as output if requested (and available)
    TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
    !> a flag indicating whether the move to the end of the collection occurs or not <br>
    ! - true if previous element is NOT available. <br>
    ! - otherwise previous element is available.
    tLogical                                :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER    :: CurrNode
            ! move to the next iteration
            IsTheEnd = Collection%WrkLst%MoveBackward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (LinkedNode(*))
                    CALL CurrNode%Store%Get(Item)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to the next iteration
        IsTheEnd = Collection%WrkLst%MoveBackward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = -1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION DequeLinked_Move2PrevElm

! ---------------------------------------------------------------------
! -----                     Deque Procedures                      -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE DequeLinked_AddLast(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be added to the collection
    TYPE(*),               INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode(Collection%ValSize)), POINTER   :: NewNode
    tInteger                                        :: AllocStat
    tCharLen(MsgLen)                                :: AllocMsg

! FLOW
        
    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('DequeLinked_AddLast', ModName, AllocMsg, AllocStat)
        
    ! set item to the new node
    CALL NewNode%Store%Set(Item)
        
    ! append the new node to the tail
    CALL Collection%WrkLst%AddLast(NewNode)
        
    ! free up pointer
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE DequeLinked_AddLast

!**************************************************************************************

MODULE SUBROUTINE DequeLinked_AddFirst(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!! To insert the specified item at the front of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be added to the collection
    TYPE(*),               INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode(Collection%ValSize)), POINTER   :: NewNode
    tInteger                                        :: AllocStat
    tCharLen(MsgLen)                                :: AllocMsg

! FLOW
 
    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('DequeLinked_AddFirst', ModName, AllocMsg, AllocStat)
        
    ! set item to the new node
    CALL NewNode%Store%Set(Item)
        
    ! append the new node to the head
    CALL Collection%WrkLst%AddFirst(NewNode)
        
    ! free up pointer
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE DequeLinked_AddFirst

!**************************************************************************************

MODULE FUNCTION DequeLinked_RemoveLast(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the last item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be removed from the collection
    TYPE(*),               INTENT(INOUT)    :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                                :: Flag

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
        CALL Handle_ErrDealloc('DequeLinked_RemoveLast', ModName, AllocMsg, AllocStat)
    ELSE
        ! set flag
        Flag = FalseVal
    END IF

    NULLIFY(LastNode)

    RETURN

END FUNCTION DequeLinked_RemoveLast

!**************************************************************************************

MODULE FUNCTION DequeLinked_RemoveFirst(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the first item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be removed from the collection
    TYPE(*),               INTENT(INOUT)    :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: FirstNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg

! FLOW
    ! check whether we can get and remove the first node successfully or not
    IF (Collection%WrkLst%RemoveFirst(FirstNode)) THEN
        ! set flag
        Flag = TrueVal
        
        ! get the item stored in the node
        SELECT TYPE (FirstNode)
        TYPE IS (LinkedNode(*))
            CALL FirstNode%Store%Get(Item)
        END SELECT

        ! nullify pointer components of the node
        CALL FirstNode%FreePointers()
    
        ! deallocate the node
        DEALLOCATE(FirstNode, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('DequeLinked_Dequeue', ModName, AllocMsg, AllocStat)
    ELSE
        ! set flag
        Flag = FalseVal
    END IF

    NULLIFY(FirstNode)

    RETURN

END FUNCTION DequeLinked_RemoveFirst

!**************************************************************************************

MODULE FUNCTION DequeLinked_PeekLast(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the last item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeLinked object
    CLASS(DequeLinked(*)), INTENT(IN)       :: Collection
    !% the item to be retrieved from the collection
    TYPE(*),               INTENT(INOUT)    :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                                :: Flag

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

END FUNCTION DequeLinked_PeekLast

!**************************************************************************************

END SUBMODULE SubClass_DequeLinked

!******************************************************************************

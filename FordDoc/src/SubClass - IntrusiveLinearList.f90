
SUBMODULE (Class_IntrusiveLinkedLists) SubClass_IntrusiveLinearList

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains the actual implementation of the *IntrusiveLinearList* type,
!   which is an *intrusive doubly-linked-list* container that stores objects (or nodes)
!   in the *DoublyLinkedNode* class (i.e. objects/nodes that are subtypes of the
!   *DoublyLinkedNode* type). <br>
!   Since the *IntrusiveLinearList* type is a subtype of the *IntrusiveRingList* type,
!   several methods inherited from the *IntrusiveRingList* type can be used without
!   any modifications by the *IntrusiveLinearList* type.  Therefore, this submodule only
!   provides overridden routines for those methods that requires different implementation.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----             Adding and Removing Procedures                -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE LinearList_Finalizer(List)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(IntrusiveLinearList), INTENT(INOUT)    :: List !! IntrusiveLinearList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL List%Clear()
       
    RETURN

END SUBROUTINE LinearList_Finalizer

!******************************************************************************

MODULE SUBROUTINE LinearList_AddFirst(List, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To add a new node to the front of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List     !! IntrusiveLinearList object
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode  !! node to be added to the list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        List%Head => NewNode
        List%Tail => NewNode
    ELSE
        NewNode%Next   => List%Head
        List%Head%Prev => NewNode
        List%Head      => NewNode
    END IF
        
    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE LinearList_AddFirst

!******************************************************************************

MODULE SUBROUTINE LinearList_AddLast(List, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !! To add a new node to the back of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List     !! IntrusiveLinearList object
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode  !! node to be added to the list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        List%Head => NewNode
        List%Tail => NewNode
    ELSE
        NewNode%Prev   => List%Tail
        List%Tail%Next => NewNode
        List%Tail      => NewNode
    END IF
        
    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE LinearList_AddLast

!******************************************************************************

MODULE FUNCTION LinearList_AddAt(List, NewNode, N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node at the Nth position of the list where N must be
    !  between 1 and the list size.  Also, return a flag indicating whether
    !  the node is successfully added.  If the list is empty, just add the
    !  new node to the list where N is simply ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
    !% node to be added to the list
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
    !% one-based index indicating position to add the node
    tIndex,                          INTENT(IN)     :: N
    !> flag indicating whether the node is successfully added. <br>
    ! - true if the node is successfully added. <br>
    ! - false if the node is NOT successfully added.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and return quickly if possible
    IF (List%IsEmpty()) THEN
        ! just add the new node
        CALL List%AddFirst(NewNode)
        Flag = TrueVal
        RETURN
    ELSEIF (.NOT.IsIndexValid(List%Size, N)) THEN
        Flag = FalseVal
        RETURN
    END IF
    
    IF (N == 1) THEN
        ! add node to the front
        CALL List%AddFirst(NewNode)
    ELSE
        BLOCK
            ! block variables
            CLASS(DoublyLinkedNode), POINTER   :: NthNode
            CLASS(DoublyLinkedNode), POINTER   :: PrvNode
            ! get Nth node
            IF (N == List%Size) THEN
                NthNode => List%Tail
            ELSE
                NthNode => List%GetAt(N)
            END IF
            ! get previous node
            PrvNode => NthNode%Prev
            ! add new node
            PrvNode%Next => NewNode
            NthNode%Prev => NewNode
            NewNode%Prev => PrvNode
            NewNode%Next => NthNode
            ! free block variables
            NULLIFY(NthNode, PrvNode)
        END BLOCK            
        ! set list length
        List%Size =  List%Size + 1
    END IF
    Flag = TrueVal

    RETURN

END FUNCTION LinearList_AddAt

!******************************************************************************

MODULE FUNCTION LinearList_AddAfter(List, NewNode, ThisNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node into the list after the specified node (*ThisNode*).
    !  Also, return a flag indicating whether the node is successfully added.
    !  If the list is empty or the *ThisNode* node does not exist in the list,
    !  return false.  Otherwise, return true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
    !% node to be added to the list
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
    !% node that the NewNode is inserted just after it
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: ThisNode
    !> flag indicating whether the node is successfully added. <br>
    ! - true if the node is successfully added. <br>
    ! - false if the node is NOT successfully added.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and return quickly if possible
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSEIF (.NOT.List%Contain(ThisNode)) THEN
        Flag = FalseVal
        RETURN
    END IF
    
    IF (ASSOCIATED(List%Tail, ThisNode)) THEN
        ! add node to the back
        CALL List%AddLast(NewNode)
    ELSE
        ASSOCIATE (NextNode => ThisNode%Next)
            ! add new node
            NextNode%Prev => NewNode
            ThisNode%Next => NewNode
            NewNode%Prev => ThisNode
            NewNode%Next => NextNode
        END ASSOCIATE
        ! set list length
        List%Size =  List%Size + 1
    END IF
    Flag = TrueVal

    RETURN

END FUNCTION LinearList_AddAfter

!******************************************************************************

MODULE FUNCTION LinearList_RemoveFirst(List, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the first (head) node from the list. Also, return
    !  a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% pointer to the first node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed. <br>
    ! - false if the node is NOT successfully removed.
    tLogical                                                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(NodeOut)) NodeOut => List%Head
        
    ! check whether the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF
    
    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%FreePointers()
        NULLIFY(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ASSOCIATE (FirstNode => List%Head)
            ! reset the head node to its next node
            List%Head => FirstNode%Next
            List%Head%Prev => NULL()
            ! remove the first node
            CALL FirstNode%FreePointers()
        END ASSOCIATE
        ! set size
        List%Size = List%Size - 1
    END IF
        
    RETURN
        
END FUNCTION LinearList_RemoveFirst

!******************************************************************************

MODULE FUNCTION LinearList_RemoveLast(List, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the last (tail) node from the list. Also, return
    !  a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% pointer to the last node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed. <br>
    ! - false if the node is NOT successfully removed.
    tLogical                                                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(NodeOut)) NodeOut => List%Tail
        
    ! check whether the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF
    
    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%FreePointers()
        NULLIFY(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ASSOCIATE (LastNode => List%Tail)
            ! reset the tail node to its previous node
            List%Tail => LastNode%Prev
            List%Tail%Next => NULL()
            ! remove the last node
            CALL LastNode%FreePointers()
        END ASSOCIATE
        ! set size
        List%Size = List%Size - 1
    END IF
        
    RETURN
        
END FUNCTION LinearList_RemoveLast

!******************************************************************************

MODULE FUNCTION LinearList_RemoveNode(List, CurrNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a node associated with the specified node from the list.
    ! Also, return a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
    !% the node to be removed
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: CurrNode
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed. <br>
    ! - false if the node is NOT successfully removed.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (List%IsEmpty()) THEN
        ! the given node does not exist in the empty list
        Flag = FalseVal
    ELSE
        ! check whether there is only one node or not
        IF (List%Size == 1) THEN
            ! ++ the list has only one node ++
            ! check to make sure that the supplied node is really the only one
            IF (ASSOCIATED(List%Head, CurrNode).AND.ASSOCIATED(List%Tail, CurrNode)) THEN
                ! reset the list
                List%Head => NULL()
                List%Tail => NULL()
                List%Cursor => NULL()
                List%Size = 0
                Flag = TrueVal
            ELSE
                ! the given node does not exist in the list
                Flag = FalseVal
            END IF
        ELSE
            ! ++ the list has two or more nodes ++
            ! check where the supplied node is
            IF (ASSOCIATED(List%Head, CurrNode)) THEN
                ! the node is the head
                List%Head      => CurrNode%Next
                List%Head%Prev => NULL()
                Flag = TrueVal
            ELSEIF (ASSOCIATED(List%Tail, CurrNode)) THEN
                ! the node is the tail
                List%Tail      => CurrNode%Prev
                List%Tail%Next => NULL()
                Flag = TrueVal
            ELSE
                IF (List%Contain(CurrNode)) THEN
                    ! the node is in the middle
                    ASSOCIATE (PrevNode => CurrNode%Prev, NextNode => CurrNode%Next)
                        PrevNode%Next => NextNode
                        NextNode%Prev => PrevNode
                    END ASSOCIATE
                    Flag = TrueVal
                ELSE
                    ! the node does not exist
                    Flag = FalseVal
                END IF
            END IF
            ! set size
            IF (Flag) List%Size = List%Size - 1
        END IF
        ! destroy the current node
        IF (Flag) CALL CurrNode%FreePointers()
    END IF

    RETURN
 
END FUNCTION LinearList_RemoveNode

!******************************************************************************

MODULE FUNCTION LinearList_RemoveAt(List, N, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the Nth node from the list where N must be between 1 and the list size.
    !  Also, return a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% (one-based) index indicating the node to be removed
    tIndex,                                     INTENT(IN)      :: N
    !% pointer to the Nth node (null pointer if N is not valid)
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed. <br>
    ! - false if the node is NOT successfully removed.
    tLogical                                                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER   :: NthNode

!** FLOW

    ! get the Nth node
    NthNode => List%GetAt(N)
    
    ! if request Nth node as output, set pointer to Nth node
    IF (PRESENT(NodeOut)) NodeOut => NthNode
    
    ! remove the Nth node
    IF (ASSOCIATED(NthNode)) THEN
        IF (List%Size == 1) THEN
            ! the list has only one node so reset the list
            List%Head => NULL()
            List%Tail => NULL()
            List%Cursor => NULL()
            List%Size = 0
        ELSE
            ! the list has two or more nodes so check where the supplied node is
            IF (ASSOCIATED(List%Head, NthNode)) THEN
                ! the node is the head
                List%Head      => NthNode%Next
                List%Head%Prev => NULL()
            ELSEIF (ASSOCIATED(List%Tail, NthNode)) THEN
                ! the node is the tail
                List%Tail      => NthNode%Prev
                List%Tail%Next => NULL()
            ELSE
                ! the node is in the middle
                ASSOCIATE (PrevNode => NthNode%Prev, NextNode => NthNode%Next)
                    PrevNode%Next => NextNode
                    NextNode%Prev => PrevNode
                END ASSOCIATE
            END IF
        END IF
        CALL NthNode%FreePointers()
        List%Size = List%Size - 1
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(NthNode)
    
    RETURN

END FUNCTION LinearList_RemoveAt
    
!******************************************************************************

MODULE FUNCTION LinearList_ReplaceNode(List, OldNode, NewNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To replace a node associated with the specified old node from the list
    !  with the specified new node. Also, return a flag indicating whether
    !  the old node is successfully replaced.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),      INTENT(INOUT)  :: List
    !% the old node to be replaced
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: OldNode
    !% the node to be removed
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
    !> flag indicating whether the old node is successfully replaced. <br>
    ! - true if the node is successfully replaced. <br>
    ! - false if the node is NOT successfully replaced.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: NextNode
    CLASS(DoublyLinkedNode), POINTER    :: PrevNode

!** FLOW
    
    ! check whether the old node exists in the list or not
    IF (List%Contain(OldNode)) THEN
        ! get adjacent nodes of old node
        NextNode => OldNode%Next
        PrevNode => OldNode%Prev
        ! set adjacent nodes for new one
        NewNode%Next => NextNode
        NewNode%Prev => PrevNode
        ! free old node and local pointers
        CALL OldNode%FreePointers()
        NULLIFY(NextNode, PrevNode)
        ! set returned flag
        Flag = TrueVal
    ELSE
        ! set returned flag
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION LinearList_ReplaceNode

! ---------------------------------------------------------------------
! -----                     Iteration Procedures                  -----
! ---------------------------------------------------------------------

MODULE FUNCTION LinearList_StartLast(List, NodeOut) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To start a backward iteration by setting the cursor pointer to the tail node
    !  of the list and return a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% pointer to the starting node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !% true if the list is empty
    tLogical                                                    :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set cursor
    List%Cursor => List%Tail
    
    ! if request the current node as output, set pointer to the cursor
    IF (PRESENT(NodeOut)) NodeOut => List%Cursor
       
    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
        
    RETURN

END FUNCTION LinearList_StartLast

!******************************************************************************

MODULE FUNCTION LinearList_Move2Next(List, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node of the forward iteration and return a flag
    !  indicating whether the cursor has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% pointer to the next node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !% true if the cursor pointer has reached the end of the list
    tLogical                                                    :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set cursor
    List%Cursor => List%Cursor%Next
    
    ! if request the current node as output, set pointer to the cursor
    IF (PRESENT(NodeOut)) NodeOut => List%Cursor
       
    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)
        
    RETURN

END FUNCTION LinearList_Move2Next

!******************************************************************************

MODULE FUNCTION LinearList_Move2Prev(List, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node of the backward iteration and return a flag
    !  indicating whether the cursor has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveLinearList object
    CLASS(IntrusiveLinearList),                 INTENT(INOUT)   :: List
    !% pointer to the next node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !% true if the cursor pointer has reached the end of the list
    tLogical                                                    :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set cursor
    List%Cursor => List%Cursor%Prev
    
    ! if request the current node as output, set pointer to the cursor
    IF (PRESENT(NodeOut)) NodeOut => List%Cursor
       
    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)
        
    RETURN

END FUNCTION LinearList_Move2Prev

! ---------------------------------------------------------------------
! -----                     Retrieving Procedures                 -----
! ---------------------------------------------------------------------

MODULE FUNCTION LinearList_GetTail(List) RESULT(Tail)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the tail (last node) of the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveLinearList), INTENT(IN)  :: List !! IntrusiveLinearList object
    CLASS(DoublyLinkedNode),    POINTER     :: Tail !! pointer to the tail

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Tail => List%Tail

    RETURN

END FUNCTION LinearList_GetTail

!******************************************************************************

END SUBMODULE SubClass_IntrusiveLinearList

!******************************************************************************

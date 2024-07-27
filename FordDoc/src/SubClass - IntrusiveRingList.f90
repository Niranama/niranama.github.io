
SUBMODULE (Class_IntrusiveLinkedLists) SubClass_IntrusiveRingList

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains the actual implementation of the *IntrusiveRingList* type,
!   which is an *intrusive circularly-linked-list* container that stores objects (or nodes)
!   in the *DoublyLinkedNode* class (i.e. objects/nodes that are subtypes of the
!   *DoublyLinkedNode* type).

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

MODULE SUBROUTINE RingList_Clear(List)

!** PURPOSE OF THIS SUBROUTINE:
    !! To remove all nodes from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(INOUT) :: List !! IntrusiveRingList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Success

! FLOW

    ! remove all nodes starting from the last one.
    DO WHILE (.NOT.List%IsEmpty())
        Success = List%RemoveLast()
    END DO

    RETURN

END SUBROUTINE RingList_Clear

!******************************************************************************

MODULE SUBROUTINE RingList_Finalizer(List)

!** PURPOSE OF THIS SUBROUTINE:
    !! To perform finalization of the object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(IntrusiveRingList), INTENT(INOUT)  :: List !! IntrusiveRingList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL List%Clear()
       
    RETURN

END SUBROUTINE RingList_Finalizer

!******************************************************************************

MODULE SUBROUTINE RingList_AddFirst(List, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node to the front of the list, which is between the first
    !  node inserted and the last one inserted.  Also, set the *Head* pointer
    !  to the new node

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List     !! IntrusiveRingList object
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode  !! node to be added to the list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        List%Head    => NewNode
        NewNode%Prev => List%Head
        NewNode%Next => List%Head
    ELSE
        ! insert the new node between the first and last nodes
        ASSOCIATE(LastNode => List%Head%Prev, FirstNode => List%Head)
            LastNode%Next  => NewNode
            FirstNode%Prev => NewNode
            NewNode%Prev   => LastNode
            NewNode%Next   => FirstNode
        END ASSOCIATE
        ! set the Head pointer
        List%Head => NewNode
    END IF
        
    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE RingList_AddFirst

!******************************************************************************

MODULE SUBROUTINE RingList_AddLast(List, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node to the back of the list, which is between the first
    !  node inserted and the last one inserted.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List     !! IntrusiveRingList object
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode  !! node to be added to the list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        List%Head    => NewNode
        NewNode%Prev => List%Head
        NewNode%Next => List%Head
    ELSE
        ! insert the new node between the first and last nodes
        ASSOCIATE(LastNode => List%Head%Prev, FirstNode => List%Head)
            LastNode%Next  => NewNode
            FirstNode%Prev => NewNode
            NewNode%Prev   => LastNode
            NewNode%Next   => FirstNode
        END ASSOCIATE
    END IF
        
    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE RingList_AddLast

!******************************************************************************

MODULE FUNCTION RingList_AddAt(List, NewNode, N) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node at the Nth position of the list where N must be
    !  between 1 and the list size.  Also, return a flag indicating whether
    !  the node is successfully added.  If the list is empty, just add the
    !  new node to the list where N is simply ignored.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
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
        CALL List%AddLast(NewNode)
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
                NthNode => List%Head%Prev
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
    END IF
    Flag = TrueVal

    RETURN

END FUNCTION RingList_AddAt

!******************************************************************************

MODULE FUNCTION RingList_AddBefore(List, NewNode, ThisNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node into the list before the specified node (*ThisNode*).
    !  Also, return a flag indicating whether the node is successfully added.
    !  If the list is empty or the *ThisNode* node does not exist in the list,
    !  return false.  Otherwise, return true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
    !% node to be added to the list
    CLASS(DoublyLinkedNode), TARGET, INTENT(INOUT)  :: NewNode
    !% node that the NewNode is inserted just before it
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
    
    IF (ASSOCIATED(List%Head, ThisNode)) THEN
        ! add node to the front
        CALL List%AddFirst(NewNode)
    ELSE
        ASSOCIATE (PrvNode => ThisNode%Prev)
            ! add new node
            PrvNode%Next  => NewNode
            ThisNode%Prev => NewNode
            NewNode%Prev  => PrvNode
            NewNode%Next  => ThisNode
        END ASSOCIATE         
    END IF
    Flag = TrueVal

    RETURN

END FUNCTION RingList_AddBefore

!******************************************************************************

MODULE FUNCTION RingList_AddAfter(List, NewNode, ThisNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a new node into the list after the specified node (*ThisNode*).
    !  Also, return a flag indicating whether the node is successfully added.
    !  If the list is empty or the *ThisNode* node does not exist in the list,
    !  return false.  Otherwise, return true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
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
    
    IF (ASSOCIATED(List%Head%Prev, ThisNode)) THEN
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
    END IF
    Flag = TrueVal

    RETURN

END FUNCTION RingList_AddAfter

!******************************************************************************

MODULE FUNCTION RingList_RemoveFirst(List, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the first (head) node from the list. Also, return
    !  a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
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
    IF (List%Size == 1) THEN
        ! the list has only one node so reset the list
        CALL List%Head%FreePointers()
        NULLIFY(List%Head)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ASSOCIATE (FirstNode => List%Head, PrevNode => List%Head%Prev, &
                   NextNode => List%Head%Next)
            ! connect NextNode and PrevNode
            PrevNode%Next => NextNode
            NextNode%Prev => PrevNode
            List%Head => NextNode
            CALL FirstNode%FreePointers()
        END ASSOCIATE
        ! set size
        List%Size = List%Size - 1
    END IF
        
    RETURN
        
END FUNCTION RingList_RemoveFirst

!******************************************************************************

MODULE FUNCTION RingList_RemoveLast(List, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the last (tail) node from the list. Also, return
    !  a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
    !% pointer to the last node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed. <br>
    ! - false if the node is NOT successfully removed.
    tLogical                                                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(NodeOut)) NodeOut => List%Head%Prev
        
    ! check whether the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF
    
    ! check whether there is only one node or not
    IF (List%Size == 1) THEN
        ! the list has only one node so reset the list
        CALL List%Head%FreePointers()
        NULLIFY(List%Head)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ASSOCIATE (LastNode => List%Head%Prev, NextNode => List%Head, &
                   PrevNode => List%Head%Prev%Prev)
            ! connect NextNode and PrevNode
            PrevNode%Next => NextNode
            NextNode%Prev => PrevNode
            CALL LastNode%FreePointers()
        END ASSOCIATE
        ! set size
        List%Size = List%Size - 1
    END IF
        
    RETURN
        
END FUNCTION RingList_RemoveLast

!******************************************************************************

MODULE FUNCTION RingList_RemoveNode(List, CurrNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a node associated with the specified node from the list.
    ! Also, return a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
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
        IF (ASSOCIATED(List%Head, CurrNode)) THEN
            ! the node is the head
            Flag = List%RemoveFirst()
        ELSEIF (ASSOCIATED(List%Head%Prev, CurrNode)) THEN
            ! the node is the tail
            Flag = List%RemoveLast()
        ELSE
            ! the node may be in the middle
            IF (List%Contain(CurrNode)) THEN
                ! the node is certainly in the middle
                ASSOCIATE (PrevNode => CurrNode%Prev, NextNode => CurrNode%Next)
                    PrevNode%Next => NextNode
                    NextNode%Prev => PrevNode
                END ASSOCIATE
                Flag = TrueVal
            ELSE
                ! the node is not in the list
                Flag = FalseVal
            END IF
            IF (Flag) THEN
                List%Size = List%Size - 1
                CALL CurrNode%FreePointers()
            END IF
        END IF
    END IF

    RETURN
 
END FUNCTION RingList_RemoveNode

!******************************************************************************

MODULE FUNCTION RingList_RemoveAt(List, N, NodeOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the Nth node from the list where N must be between 1 and the list size.
    !  Also, return a flag indicating whether the node is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
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
        IF (ASSOCIATED(List%Head, NthNode)) THEN
            ! the node is the head
            Flag = List%RemoveFirst()
        ELSEIF (ASSOCIATED(List%Head%Prev, NthNode)) THEN
            ! the node is the tail
            Flag = List%RemoveLast()
        ELSE
            ! the node is certainly in the middle
            ASSOCIATE (PrevNode => NthNode%Prev, NextNode => NthNode%Next)
                PrevNode%Next => NextNode
                NextNode%Prev => PrevNode
            END ASSOCIATE
            List%Size = List%Size - 1
            CALL NthNode%FreePointers()
            Flag = TrueVal
        END IF
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(NthNode)
    
    RETURN

END FUNCTION RingList_RemoveAt

! ---------------------------------------------------------------------
! -----                     Iteration Procedures                  -----
! ---------------------------------------------------------------------

MODULE FUNCTION RingList_StartFirst(List, NodeOut) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To start a forward iteration by setting the cursor pointer to the head node
    !  of the list and return a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
    !% pointer to the starting node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !% true if the list is empty
    tLogical                                                    :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set cursor
    List%Cursor => List%Head
    
    ! if request the current node as output, set pointer to the cursor
    IF (PRESENT(NodeOut)) NodeOut => List%Cursor
       
    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
        
    RETURN

END FUNCTION RingList_StartFirst

!******************************************************************************

MODULE FUNCTION RingList_StartLast(List, NodeOut) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To start a backward iteration by setting the cursor pointer to the last node
    !  of the list and return a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
    !% pointer to the starting node
    CLASS(DoublyLinkedNode), OPTIONAL, POINTER, INTENT(OUT)     :: NodeOut
    !% true if the list is empty
    tLogical                                                    :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set cursor
    List%Cursor => List%Head%Prev
    
    ! if request the current node as output, set pointer to the cursor
    IF (PRESENT(NodeOut)) NodeOut => List%Cursor
       
    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
        
    RETURN

END FUNCTION RingList_StartLast

!******************************************************************************

MODULE FUNCTION RingList_Move2Next(List, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node of the forward iteration and return a flag
    !  indicating whether the cursor has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
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
    IsTheEnd = ASSOCIATED(List%Cursor, List%Head)
        
    RETURN

END FUNCTION RingList_Move2Next

!******************************************************************************

MODULE FUNCTION RingList_Move2Prev(List, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node of the backward iteration and return a flag
    !  indicating whether the cursor has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),                   INTENT(INOUT)   :: List
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
    IsTheEnd = ASSOCIATED(List%Cursor, List%Head%Prev)
        
    RETURN

END FUNCTION RingList_Move2Prev

! ---------------------------------------------------------------------
! -----                     Inquiry Procedures                    -----
! ---------------------------------------------------------------------

MODULE FUNCTION RingList_IsEmpty(List) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the list is empty or not

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(IN)    :: List !! IntrusiveRingList object
    tLogical                                :: Flag !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = .NOT.ASSOCIATED(List%Head)

    RETURN

END FUNCTION RingList_IsEmpty

!******************************************************************************

MODULE FUNCTION RingList_GetSize(List) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get size of the list (a number of nodes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(IN)    :: List     !! IntrusiveRingList object
    tIndex                                  :: Size     !! list size (number of nodes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = List%Size

    RETURN

END FUNCTION RingList_GetSize

! ---------------------------------------------------------------------
! -----                     Retrieving Procedures                 -----
! ---------------------------------------------------------------------

MODULE FUNCTION RingList_GetHead(List) RESULT(Head)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the head (first node) of the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(IN)    :: List !! IntrusiveRingList object
    CLASS(DoublyLinkedNode),  POINTER       :: Head !! pointer to the head

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Head => List%Head

    RETURN

END FUNCTION RingList_GetHead

!******************************************************************************

MODULE FUNCTION RingList_GetTail(List) RESULT(Tail)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the tail (last node) of the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(IN)    :: List !! IntrusiveRingList object
    CLASS(DoublyLinkedNode),  POINTER       :: Tail !! pointer to the tail

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Tail => List%Head%Prev

    RETURN

END FUNCTION RingList_GetTail

!******************************************************************************

MODULE FUNCTION RingList_GetCursor(List) RESULT(Cursor)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the cursor node of the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(IN)    :: List     !! IntrusiveRingList object
    CLASS(DoublyLinkedNode),  POINTER       :: Cursor   !! pointer to the cursor node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Cursor => List%Cursor

    RETURN

END FUNCTION RingList_GetCursor

!******************************************************************************

MODULE FUNCTION RingList_GetAt(List, N) RESULT(NodeOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the Nth node from the list where N is between 1 and the list size.
    !  If the list is empty or N is not in a valid range, return Null pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList), INTENT(INOUT) :: List     !! IntrusiveRingList object
    tIndex,                   INTENT(IN)    :: N        !! (one-based) index indicating the node
    CLASS(DoublyLinkedNode),  POINTER       :: NodeOut  !! pointer to the Nth node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurrIndex
    tLogical    :: EndOfList

!** FLOW
    
    ! check and return quickly if possible
    IF (List%IsEmpty()) THEN
        NodeOut => NULL()
        RETURN
    ELSEIF (.NOT.IsIndexValid(List%Size, N)) THEN
        NodeOut => NULL()
        RETURN
    END IF

    ! check which direction to traverse the list
    IF (N <= (List%Size-N+1)) THEN
        ! perform forward iteration
        CurrIndex = 1
        ! restart the iteration
        EndOfList = List%StartFirst()
        DO WHILE (.NOT.EndOfList)
            ! check if the node is found
            IF (CurrIndex == N) EXIT
            ! move to the next iteration
            EndOfList = List%MoveForward()
            ! set index
            CurrIndex = CurrIndex + 1
        END DO
    ELSE
        ! perform backward iteration
        CurrIndex = List%Size
        ! restart the iteration
        EndOfList = List%StartLast()
        DO WHILE (.NOT.EndOfList)
            ! check if the node is found
            IF (CurrIndex == N) EXIT
            ! move to the next iteration
            EndOfList = List%MoveBackward()
            ! set index
            CurrIndex = CurrIndex - 1
        END DO
    END IF
    
    ! set pointer to the Nth node
    NodeOut => List%Cursor
    
    RETURN

END FUNCTION RingList_GetAt

! ---------------------------------------------------------------------
! -----            Searching and Sorting Procedures               -----
! ---------------------------------------------------------------------

MODULE FUNCTION RingList_Contain(List, NodeIn) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified node is currently stored in the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List     !! IntrusiveRingList object
    CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NodeIn   !! input node
    tLogical                                        :: Flag     !! true if the node is stored in the list

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd

! FLOW
    
    Flag = FalseVal
    IsTheEnd = List%StartFirst(CurrNode)
    DO WHILE (.NOT.IsTheEnd)
        IF (ASSOCIATED(CurrNode, NodeIn)) THEN
            ! node found
            Flag = TrueVal
            EXIT
        END IF
        IsTheEnd = List%MoveForward(CurrNode)
    END DO
    NULLIFY(CurrNode)
        
    RETURN
 
END FUNCTION RingList_Contain

!******************************************************************************

MODULE FUNCTION RingList_IndexOf(List, NodeIn) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To determine the index indicating the position where the specified node
    !  is stored in the list.  Return the one-based index of the first node found
    !  or return zero if the given node is not stored in the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
    !% input node
    CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NodeIn
    !% one-based index indicating the position of the first node found
    tInteger                                        :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tInteger                            :: I

! FLOW
    
    Index = 0_kIndex
    IsTheEnd = List%StartFirst(CurrNode)
    I = 1_kIndex
    DO WHILE (.NOT.IsTheEnd)
        IF (ASSOCIATED(CurrNode, NodeIn)) THEN
            ! node found
            Index = I
            EXIT
        END IF
        IsTheEnd = List%MoveForward(CurrNode)
        I = I + 1_kIndex
    END DO
    NULLIFY(CurrNode)
        
    RETURN
 
END FUNCTION RingList_IndexOf

!******************************************************************************

MODULE FUNCTION RingList_LastIndexOf(List, NodeIn) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To determine the index indicating the position where the specified node
    !  is stored in the list.  Return the one-based index of the last node found
    !  (i.e. the first node when searching backward from the tail node) or return
    !  the list size plus one if the given node is not stored in the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList),        INTENT(INOUT)  :: List
    !% input node
    CLASS(DoublyLinkedNode), TARGET, INTENT(IN)     :: NodeIn
    !% one-based index indicating the position of the last node found
    tInteger                                        :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tInteger                            :: I

! FLOW
    
    Index = List%Size + 1_kIndex
    IsTheEnd = List%StartLast(CurrNode)
    I = List%Size
    DO WHILE (.NOT.IsTheEnd)
        IF (ASSOCIATED(CurrNode, NodeIn)) THEN
            ! node found
            Index = I
            EXIT
        END IF
        IsTheEnd = List%MoveBackward(CurrNode)
        I = I - 1_kIndex
    END DO
    NULLIFY(CurrNode)
        
    RETURN
 
END FUNCTION RingList_LastIndexOf

!******************************************************************************

MODULE FUNCTION RingList_FindFirstEqual(List, NodeIn, IsEqualTo) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To determine whether there is a node in the list that is equal to the
    !  specified node.  Return the one-based index indicating the position of
    !  the first node found or return zero if none of the nodes is equal to
    !  the given node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList), INTENT(INOUT) :: List
    !% the node to be found
    CLASS(DoublyLinkedNode),  INTENT(IN)    :: NodeIn
    !% procedure to check whether two objects (nodes) are equal to one another or not
    PROCEDURE(EqualNodes)                   :: IsEqualTo
    !% one-based index indicating the position of the first node found
    tInteger                                :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tInteger                            :: I

! FLOW
    
    Index = 0_kIndex
    IsTheEnd = List%StartFirst(CurrNode)
    I = 1_kIndex
    DO WHILE (.NOT.IsTheEnd)
        IF (IsEqualTo(CurrNode, NodeIn)) THEN
            ! node found
            Index = I
            EXIT
        END IF
        IsTheEnd = List%MoveForward(CurrNode)
        I = I + 1_kIndex
    END DO
    NULLIFY(CurrNode)

    RETURN
 
END FUNCTION RingList_FindFirstEqual

!******************************************************************************

MODULE FUNCTION RingList_FindLastEqual(List, NodeIn, IsEqualTo) RESULT(Index)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To determine whether there is a node in the list that is equal to the given
    !  node.  Return the one-based index indicating the position of the last node
    !  found (i.e. the first node when searching backward from the tail node) or
    !  return the list size plus one if none of the nodes is equal to the given node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList), INTENT(INOUT) :: List
    !% the node to be found
    CLASS(DoublyLinkedNode),  INTENT(IN)    :: NodeIn
    !% procedure to check whether two objects (nodes) are equal to one another or not
    PROCEDURE(EqualNodes)                   :: IsEqualTo
    !% one-based index indicating the position of the last node found
    tInteger                                :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tInteger                            :: I

! FLOW
    
    Index = List%Size + 1_kIndex
    IsTheEnd = List%StartLast(CurrNode)
    I = List%Size
    DO WHILE (.NOT.IsTheEnd)
        IF (IsEqualTo(CurrNode, NodeIn)) THEN
            ! node found
            Index = I
            EXIT
        END IF
        IsTheEnd = List%MoveBackward(CurrNode)
        I = I - 1_kIndex
    END DO
    NULLIFY(CurrNode)
    
    RETURN
 
END FUNCTION RingList_FindLastEqual

!******************************************************************************

MODULE RECURSIVE SUBROUTINE RingList_SortAscend(List, Compare)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To sort nodes in the list in ascending order.  The top-down
    !  merge sort algorithm is employed here.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRingList object
    CLASS(IntrusiveRingList), INTENT(INOUT) :: List
    !% procedure to compare two objects (nodes)
    PROCEDURE(CompareNodes)                 :: Compare

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveRingList)             :: Left, Right
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tIndex                              :: I, Half
    tLogical                            :: Success

! FLOW
    
    ! Base case. A list of zero or one node is sorted, by definition.
    IF (List%Size <= 1) RETURN

    ! Recursive case. First, divide the list into equal-sized sublists
    ! consisting of the first half and second half of the list.
    ! This assumes lists start at index 0.
    I = 0
    Half = List%Size/2
    DO WHILE (.NOT.List%IsEmpty())
        Success = List%RemoveFirst(CurrNode)
        I = I + 1
        IF (I < Half) THEN
            CALL Left%AddLast(CurrNode)
        ELSE
            CALL Right%AddLast(CurrNode)
        END IF
    END DO
    NULLIFY(CurrNode)

    ! Recursively sort both sublists.
    CALL Left%SortAscend(Compare)
    CALL Right%SortAscend(Compare)

    ! Then merge the now-sorted sublists.
    CALL MergeLists(Left, Right, List, Compare)

    RETURN
 
END SUBROUTINE RingList_SortAscend

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

SUBROUTINE MergeLists(Left, Right, OutList, Compare)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To merge two sublists.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% two lists to be merged
    CLASS(IntrusiveRingList), INTENT(INOUT)     :: Left
    CLASS(IntrusiveRingList), INTENT(INOUT)     :: Right
    !% output list
    CLASS(IntrusiveRingList), INTENT(INOUT)     :: OutList
    !% procedure to compare two objects (nodes)
    PROCEDURE(CompareNodes)                     :: Compare

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: FirstLeft
    CLASS(DoublyLinkedNode), POINTER    :: FirstRight
    tLogical                            :: Success

! FLOW

    DO WHILE ((.NOT.Left%IsEmpty()).AND.(.NOT.Right%IsEmpty()))
        Success = Left%RemoveFirst(FirstLeft)
        Success = Right%RemoveFirst(FirstRight)
        IF (Compare(FirstLeft, FirstRight) <= 0) THEN
            CALL OutList%AddLast(FirstLeft)
        ELSE
            CALL OutList%AddLast(FirstRight)
        END IF
    END DO

    ! Either left or right may have elements left; consume them.
    ! (Only one of the following loops will actually be entered.)
    DO WHILE (.NOT.Left%IsEmpty())
        Success = Left%RemoveFirst(FirstLeft)
        CALL OutList%AddLast(FirstLeft)
    END DO
    DO WHILE (.NOT.Right%IsEmpty())
        Success = Right%RemoveFirst(FirstRight)
        CALL OutList%AddLast(FirstRight)
    END DO

    NULLIFY(FirstLeft, FirstRight)

    RETURN
 
END SUBROUTINE MergeLists

!**************************************************************************************

END SUBMODULE SubClass_IntrusiveRingList

!******************************************************************************

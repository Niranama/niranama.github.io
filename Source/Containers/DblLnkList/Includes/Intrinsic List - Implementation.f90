
! ---------------------------------------------------------------------
! -----             DLLNode PROCEDURES                            -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedNode_Deallocate(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To deallocate DLLNode pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DLLNode), POINTER, INTENT(INOUT)   :: Node !! DLLNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW

    IF (ASSOCIATED(Node)) THEN
        DEALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('LinkedNode_Deallocate', ModName, AllocMsg, AllocStat)
    END IF

    RETURN

END SUBROUTINE LinkedNode_Deallocate

!******************************************************************************

SUBROUTINE LinkedNode_Destructor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct DLLNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DLLNode), INTENT(INOUT)   :: Node !! DLLNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(Node%Next)
    NULLIFY(Node%Prev)

    RETURN

END SUBROUTINE LinkedNode_Destructor

! ---------------------------------------------------------------------
! -----             DblLnkList PROCEDURES                         -----
! ---------------------------------------------------------------------
! -----             PRIVATE PROCEDURES                            -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_SetNewNode(List, NewNode, AppendAtTail)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set pointers for newly created node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),      INTENT(INOUT)   :: List         !! DblLnkList object
    TYPE(DLLNode), POINTER, INTENT(IN)      :: NewNode      !! new node to be added to the list
    tLogical,               INTENT(IN)      :: AppendAtTail !! true if append to the tail

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if list is empty or not
    IF (List%IsEmpty()) THEN
        ! the list is EMPTY so the new node is added to the beginning of the list
        List%Head => NewNode
        List%Tail => NewNode
    ELSE
        ! the list is NOT empty so add NewNode according to AppendAtTail flag
        IF (AppendAtTail) THEN
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            !+++ Append the NewNode to the list at the tail +++
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            NewNode%Prev   => List%Tail
            List%Tail%Next => NewNode
            List%Tail      => NewNode
        ELSE
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            !+++ Append the NewNode to the list at the head +++
            !++++++++++++++++++++++++++++++++++++++++++++++++++
            NewNode%Next   => List%Head
            List%Head%Prev => NewNode
            List%Head      => NewNode
        END IF
    END IF

    ! set list length
    List%Size =  List%Size + 1

    RETURN

END SUBROUTINE LinkedList_SetNewNode

!******************************************************************************

FUNCTION LinkedList_RemoveNode(List, CurrNode, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified node from the list.  Also, return a flag indicating
    !  whether the node is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),       INTENT(INOUT)  :: List     !! DblLnkList object
    TYPE(DLLNode), POINTER,  INTENT(INOUT)  :: CurrNode !! the node to be removed
    ItemTypeA,     OPTIONAL, INTENT(OUT)    :: Item     !! item of the removed node if requested
    !> flag indicating whether the node is successfully removed. <br>
    ! - true if the node is successfully removed.
    ! - false if the node is NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check input data and their possibly-related errors
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSEIF (.NOT.ASSOCIATED(CurrNode)) THEN
        Flag = FalseVal
        RETURN
    ELSE
        ! set flag
        Flag = TrueVal
        ! +++ no error in required input arguments +++
        ! check whether requesting the item of the node to be removed
        IF (PRESENT(Item)) Item = CurrNode%Item
        ! check whether there is only one node or not
        IF (List%Size.EQ.1) THEN
            ! ++ the list has only one node ++
            ! check to make sure that the supplied node is really the only one
            IF (ASSOCIATED(CurrNode, List%Head).AND.ASSOCIATED(CurrNode, List%Tail)) THEN
                ! reset the list
                List%Head => NULL()
                List%Tail => NULL()
                List%Cursor => NULL()
                List%Size = 0
            ELSE
                ! The list contains only one node but it is NOT associated with
                ! the specified node so no node is removed.
                Flag = FalseVal
                RETURN
            END IF
        ELSE
            ! ++ the list has two or more nodes ++
            ! check where the supplied node is
            IF (ASSOCIATED(CurrNode,List%Head)) THEN
                ! the node is the head
                List%Head      => CurrNode%Next
                List%Head%Prev => NULL()
            ELSEIF (ASSOCIATED(CurrNode,List%Tail)) THEN
                ! the node is the tail
                List%Tail      => CurrNode%Prev
                List%Tail%Next => NULL()
            ELSE
                ! the node is in the middle
                CurrNode%Prev%Next => CurrNode%Next
                CurrNode%Next%Prev => CurrNode%Prev
            END IF
            ! set length
            List%Size = List%Size - 1
        END IF
        ! destroy the current node
        CALL CurrNode%Destruct()
        CALL LinkedNode_Deallocate(CurrNode)
    END IF

    RETURN

END FUNCTION LinkedList_RemoveNode

!******************************************************************************

SUBROUTINE LinkedList_Traverse(List, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To traverse the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List     !! DblLnkList object
    PROCEDURE(IterFuncItem)             :: IterFunc !! iterator function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: CurrNode
    ItemTypeA               :: CurrItem
    tLogical                :: ErrStat
    tLogical                :: Done

! FLOW

    ! set defaults
    Done    = FalseVal   ! traverse to all nodes
    ErrStat = FalseVal

    ! initialize current node
    CurrNode => List%Head

    ! loop over all nodes of the list
    DO WHILE (ASSOCIATED(CurrNode))

        ! get current item
        CurrItem = CurrNode%Item

        ! call iterator function
        ErrStat = IterFunc(CurrItem, Done)

        ! report error if necessary
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('LinkedList_Traverse', ModName, ErrSevere, &
                                 'An error occurred during call to iterator function.')
            RETURN
        END IF

        ! exit the loop if the user want to stop the traversing
        IF (Done) EXIT

        ! set current node
        CurrNode => CurrNode%Next

    END DO

    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE LinkedList_Traverse

!******************************************************************************

FUNCTION IsIndexValid(Size, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified index is between 1
    !  and the list size or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: Size     !! the list size
    tIndex, INTENT(IN)  :: Index    !! the specified index
    tLogical            :: Flag     !! true if the index is valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    IF (Index < 1) RETURN
    IF (Index > Size) RETURN
    Flag = TrueVal

    RETURN

END FUNCTION IsIndexValid

!******************************************************************************

FUNCTION LinkedList_GetNodeAt(List, N) RESULT(NodeOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the Nth node from the list where N is between 1 and the list size.
    !  If the list is empty or N is not in a valid range, return Null pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List     !! DblLnkList object
    tIndex,            INTENT(IN)       :: N        !! (one-based) index indicating the node
    TYPE(DLLNode),     POINTER          :: NodeOut  !! pointer to the Nth node

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

END FUNCTION LinkedList_GetNodeAt

! ---------------------------------------------------------------------
! -----             PUBLIC PROCEDURES                             -----
! ---------------------------------------------------------------------
! -----             Constructor and Destructor Procedures         -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_CreateByArray(List, N, Items)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a list from an array of item.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List     !! DblLnkList object
    tIndex,            INTENT(IN)       :: N        !! number of items
    ItemTypeB,         INTENT(IN)       :: Items(N) !! an array of items

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! simply return if N is less than 1
    IF (N <= 0) RETURN

    ! built list of input items
    DO I = 1, N
        CALL List%AddLast(Items(I))
    END DO

    RETURN

END SUBROUTINE LinkedList_CreateByArray

!******************************************************************************

SUBROUTINE LinkedList_Destructor(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct DblLnkList object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List !! DblLnkList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! destroy all nodes and free up memory
    CALL List%Clear()

    RETURN

END SUBROUTINE LinkedList_Destructor

! ---------------------------------------------------------------------
! -----             Insertion and Removal Procedures              -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_AddFirst(List, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a item at the head of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List     !! DblLnkList object
    ItemTypeB,         INTENT(IN)       :: Item     !! item to be added to the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, PARAMETER :: AppendAtTail = FalseVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tInteger                :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW

    ! allocate new node and set the new node's item
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddFirst', ModName, AllocMsg, AllocStat)
    NewNode%Item = Item

    ! set pointers to new node
    CALL List%SetNewNode(NewNode, AppendAtTail)

    ! free up memory
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE LinkedList_AddFirst

!******************************************************************************

SUBROUTINE LinkedList_AddLast(List, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a item at the tail of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List     !! DblLnkList object
    ItemTypeB,         INTENT(IN)       :: Item     !! item to be added to the list

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, PARAMETER :: AppendAtTail = TrueVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tInteger                :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW

    ! allocate new node and set the new node's item
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddLast', ModName, AllocMsg, AllocStat)
    NewNode%Item = Item

    ! set pointers to new node
    CALL List%SetNewNode(NewNode, AppendAtTail)

    ! free up memory
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE LinkedList_AddLast

!******************************************************************************

FUNCTION LinkedList_AddAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the given item at the specified index where the index must be
    !  between 1 and the list size.  Also, return a flag indicating whether the
    !  item is successfully added.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,            INTENT(IN)       :: Index
    !% the item to be added to the list
    ItemTypeB,         INTENT(IN)       :: Item
    !> flag indicating whether the item is successfully added. <br>
    ! - true if the item is successfully added.
    ! - false if the item is NOT successfully added.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: NewNode
    tInteger                :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg

! FLOW
    
    ! check the validity of the specified index
    Flag = FalseVal
    IF (.NOT.IsIndexValid(List%Size, Index)) RETURN
 
    ! allocate new node and set the new node's item
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('LinkedList_AddAt', ModName, AllocMsg, AllocStat)
    NewNode%Item = Item
        
    ! set flag
    Flag = TrueVal
    
    IF (Index == 1_kIndex) THEN
        ! add node to the front
        CALL List%SetNewNode(NewNode, AppendAtTail=FalseVal)
    ELSE
        BLOCK
            ! block variables
            TYPE(DLLNode), POINTER  :: NthNode
            TYPE(DLLNode), POINTER  :: PrvNode
            ! get node at the index
            IF (Index == List%Size) THEN
                NthNode => List%Tail
            ELSE
                NthNode => List%GetNodeAt(Index)
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
        
    ! free up pointer
    NULLIFY(NewNode)
    
    RETURN

END FUNCTION LinkedList_AddAt

!******************************************************************************

SUBROUTINE LinkedList_Remove(List, First, Item)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove a node from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),   INTENT(INOUT)  :: List     !! DblLnkList object
    tLogical,  OPTIONAL, INTENT(IN)     :: First
    !^ location flag where the node is removed <br>
    ! - true (by default) if want to remove the first node <br>
    ! - false if want to remove the last node
    ItemTypeA, OPTIONAL, INTENT(OUT)    :: Item     !! item of the removed node if requested

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: RemoveFirstNode
    tLogical    :: Success

! FLOW

    ! first, check whether the list is empty or not
    IF (List%IsEmpty()) THEN
        ! set routine name
        SubName = 'LinkedList_Remove'
        ! set error message
        ErrMsg = 'The list is EMPTY.'
        ! report error
        CALL Handle_ErrLevel(SubName, ModName, ErrWarning, ErrMsg)
        RETURN
    END IF

    ! set default and check optional input
    SET_OPTION(RemoveFirstNode, TrueVal, First)

    ! check whether requesting the item of the node to be removed
    IF (PRESENT(Item)) THEN
        IF (RemoveFirstNode) THEN
            Success = List%PeekFirst(Item)
        ELSE
            Success = List%PeekLast(Item)
        END IF
    END IF

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        ! the list has two or more nodes so check which node to be removed
        IF (RemoveFirstNode) THEN
            ! reset the head
            List%Cursor => List%Head
            List%Head => List%Cursor%Next
            List%Head%Prev => NULL()
        ELSE
            ! reset the tail
            List%Cursor => List%Tail
            List%Tail => List%Cursor%Prev
            List%Tail%Next => NULL()
        END IF
        ! remove the node
        CALL List%Cursor%Destruct()
        CALL LinkedNode_Deallocate(List%Cursor)
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END SUBROUTINE LinkedList_Remove

!******************************************************************************

FUNCTION LinkedList_RemoveFirst(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get and remove the first item of the list.  Also, return
    !  a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(INOUT)    :: List
    !% the item to be removed from the list
    ItemTypeA,         INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! return quickly if the list is empty
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the node to be removed
    Flag = List%PeekFirst(Item)

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        BLOCK
            ! block variable
            TYPE(DLLNode), POINTER  :: DelNode => NULL()
            ! reset the head
            DelNode   => List%Head
            List%Head => DelNode%Next
            List%Head%Prev => NULL()
            ! remove the node
            CALL DelNode%Destruct()
            CALL LinkedNode_Deallocate(DelNode)
            ! free pointer
            NULLIFY(DelNode)
        END BLOCK
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END FUNCTION LinkedList_RemoveFirst

!******************************************************************************

FUNCTION LinkedList_RemoveLast(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get and remove the last item of the list.  Also, return
    !  a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(INOUT)    :: List
    !% the item to be removed from the list
    ItemTypeA,         INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! return quickly if the list is empty
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the node to be removed
    Flag = List%PeekLast(Item)

    ! check whether there is only one node or not
    IF (ASSOCIATED(List%Head, List%Tail)) THEN
        ! the list has only one node so reset the list
        CALL List%Head%Destruct()
        CALL LinkedNode_Deallocate(List%Head)
        NULLIFY(List%Tail)
        NULLIFY(List%Cursor)
        List%Size = 0
    ELSE
        BLOCK
            ! block variable
            TYPE(DLLNode), POINTER  :: DelNode
            ! reset the tail
            DelNode => List%Tail
            List%Tail => DelNode%Prev
            List%Tail%Next => NULL()
            ! remove the node
            CALL DelNode%Destruct()
            CALL LinkedNode_Deallocate(DelNode)
            ! free pointer
            NULLIFY(DelNode)
        END BLOCK
        ! set length
        List%Size = List%Size - 1
    END IF

    RETURN

END FUNCTION LinkedList_RemoveLast

!******************************************************************************

FUNCTION LinkedList_RemoveAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the item at the specified index where the index must be
    !  between 1 and the list size.   Also, return a flag indicating whether
    !  the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,            INTENT(IN)       :: Index
    !% the item to be removed from the list
    ItemTypeA,         INTENT(OUT)      :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the item is successfully removed.
    ! - false if the item is NOT successfully removed.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: DelNode

! FLOW
    
    ! get node at the index
    DelNode => List%GetNodeAt(Index)

    IF (ASSOCIATED(DelNode)) THEN
        ! get item and remove node
        Flag = List%RemoveNode(DelNode, Item)
    ELSE
        Flag = FalseVal
    END IF

    NULLIFY(DelNode)

    RETURN

END FUNCTION LinkedList_RemoveAt

!******************************************************************************

SUBROUTINE LinkedList_Delete(List)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from the list.  This procedure is intended to be used
    !  in conjunction with the *StartFirst* and *MoveForward* methods (or the
    !  *StartLast* and *MoveBackward*).  Therefore, after the call to one of
    !  these methods and then calling this procedure will result in a removal
    !  of the current item of the iteration (i.e. the same item that can be
    !  retrieved via those methods). <br>
    !  If the cursor pointer is not associated, nothing happens.  This usually
    !  means that the list is empty or this procedure is called before those
    !  iteration methods. <br>
    !  This procedure provides a way to remove items in the middle of the list
    !  without knowing specific locations of the items.  The user would perform
    !  an iteration over the list by calling those iteration methods.  While in
    !  the middle of the iteration, if the interested items are found, they can
    !  be removed from the list by this procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List !! DblLnkList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: DelNode
    tLogical                :: Flag

! FLOW

    ! get the node to be deleted
    DelNode => List%Cursor
    
    IF (ASSOCIATED(DelNode)) THEN
        ! reset cursor
        IF (List%Dir == 1) THEN
            ! forward iteration so move cursor backward
            List%Cursor => List%Cursor%Prev
        ELSE
            ! backward iteration so move cursor forward
            List%Cursor => List%Cursor%Next
        END IF
        Flag = List%RemoveNode(DelNode)
    END IF

    NULLIFY(DelNode)

    RETURN

END SUBROUTINE LinkedList_Delete

!**************************************************************************************

SUBROUTINE LinkedList_ClearItems(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free up memory of the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List !! DblLnkList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! traverse the list and destroy all nodes
    DO WHILE (.NOT.List%IsEmpty())
        CALL List%Remove()
    END DO

    ! free up pointers
    NULLIFY(List%Head)
    NULLIFY(List%Tail)
    NULLIFY(List%Cursor)

    ! reset components
    List%Size = 0
    List%Dir = 0

    RETURN

END SUBROUTINE LinkedList_ClearItems

!******************************************************************************

FUNCTION LinkedList_ToArray(List, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the list.  Also, return
    !  a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList),      INTENT(INOUT)   :: List
    !% the item to be removed from the list
    ItemTypeC, ALLOCATABLE, INTENT(INOUT)   :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! get items
    Flag = List%GetAll(Items)

    ! destroy all nodes and free up memory
    CALL List%Clear()

END FUNCTION LinkedList_ToArray

!******************************************************************************

SUBROUTINE LinkedList_RemoveDuplicates(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove nodes with duplicated items from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(INOUT)    :: List    !! DblLnkList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: CurrNode
    TYPE(DLLNode), POINTER  :: NextNode
    ItemTypeA               :: CurrItem
    ItemTypeA               :: IterItem
    tLogical                :: Repeat
    tLogical                :: IsTheEnd
    tLogical                :: Success

!** FLOW

    ! start the list
    CurrNode => List%Head
    NextNode => CurrNode%Next

    ! traverse the list
    DO WHILE (ASSOCIATED(NextNode))

        ! set flag
        Repeat = FalseVal

        ! get current item
        CurrItem = CurrNode%Item

        ! start the iteration at the next node
        List%Cursor => NextNode
        IsTheEnd = FalseVal
        IterItem = List%Cursor%Item

        DO WHILE (.NOT.IsTheEnd)

            ! check whether the items are the same
            IF (IS_EQUAL(CurrItem, IterItem)) THEN
                Repeat = TrueVal
                EXIT
            END IF

            ! move to next iteration and get its item
            IsTheEnd = List%MoveForward(IterItem)

        END DO

        ! remove node if the current item has a duplicated one
        IF (Repeat) Success = List%RemoveNode(CurrNode)

        ! move to next node
        CurrNode => NextNode

        ! set NextNode
        NextNode => CurrNode%Next

    END DO

    ! free up memory
    NULLIFY(CurrNode)
    NULLIFY(NextNode)

    RETURN

END SUBROUTINE LinkedList_RemoveDuplicates

! ---------------------------------------------------------------------
! -----                 Iteration Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION LinkedList_Move2FirstElm(List, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the head node of the list and return
    !  a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),   INTENT(INOUT)  :: List     !! DblLnkList object
    ItemTypeA, OPTIONAL, INTENT(OUT)    :: Item     !! first item
    tLogical                            :: IsEmpty  !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Head

    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
    
    ! set direction
    IF (.NOT.IsEmpty) List%Dir = 1

    IF (PRESENT(Item)) THEN
        IF (.NOT.IsEmpty) Item = List%Cursor%Item
    END IF

    RETURN

END FUNCTION LinkedList_Move2FirstElm

!******************************************************************************

FUNCTION LinkedList_Move2LastElm(List, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the tail node of the list and return
    !  a flag indicating whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),   INTENT(INOUT)  :: List     !! DblLnkList object
    ItemTypeA, OPTIONAL, INTENT(OUT)    :: Item     !! last item
    tLogical                            :: IsEmpty  !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Tail

    ! set flag
    IsEmpty = .NOT.ASSOCIATED(List%Cursor)
    
    ! set direction
    IF (.NOT.IsEmpty) List%Dir = -1

    IF (PRESENT(Item)) THEN
        IF (.NOT.IsEmpty) Item = List%Cursor%Item
    END IF

    RETURN

END FUNCTION LinkedList_Move2LastElm

!******************************************************************************

FUNCTION LinkedList_Move2NextElm(List, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move (forward) to the next node in the list and return a flag indicating
    !  whether the cursor pointer has reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),   INTENT(INOUT)  :: List     !! DblLnkList object
    ItemTypeA, OPTIONAL, INTENT(OUT)    :: Item     !! item of the next node
    tLogical                            :: IsTheEnd !! true if the cursor has reached the end of the list
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Cursor%Next

    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        List%Dir = 1
    ELSE
        List%Dir = 0
    END IF

    IF (PRESENT(Item)) THEN
        IF (.NOT.IsTheEnd) Item = List%Cursor%Item
    END IF

    RETURN

END FUNCTION LinkedList_Move2NextElm

!******************************************************************************

FUNCTION LinkedList_Move2PrevElm(List, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move backward to the next node (i.e. to the so-called previous node)
    !  in the list and return a flag indicating whether the cursor pointer has
    !  reached the end of the list or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),   INTENT(INOUT)  :: List     !! DblLnkList object
    ItemTypeA, OPTIONAL, INTENT(OUT)    :: Item     !! item of the previous node
    tLogical                            :: IsTheEnd !! true if the cursor has reached the end of the list
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor
    List%Cursor => List%Cursor%Prev

    ! set flag
    IsTheEnd = .NOT.ASSOCIATED(List%Cursor)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        List%Dir = -1
    ELSE
        List%Dir = 0
    END IF

    IF (PRESENT(Item)) THEN
        IF (.NOT.IsTheEnd) Item = List%Cursor%Item
    END IF

    RETURN

END FUNCTION LinkedList_Move2PrevElm

! ---------------------------------------------------------------------
! -----                 Inquiry Procedures                       ------
! ---------------------------------------------------------------------

FUNCTION LinkedList_IsEmpty(List) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the list is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(IN)   :: List !! DblLnkList object
    tLogical                        :: Flag !! true if the list is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = .NOT.ASSOCIATED(List%Head)

    RETURN

END FUNCTION LinkedList_IsEmpty

!******************************************************************************

FUNCTION LinkedList_GetSize(List) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get size of the list (a number of nodes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList), INTENT(IN)   :: List     !! DblLnkList object
    tIndex                          :: Size     !! list size (number of nodes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = List%Size

    RETURN

END FUNCTION LinkedList_GetSize

! ---------------------------------------------------------------------
! -----                 Retrieval Procedures                     ------
! ---------------------------------------------------------------------

FUNCTION LinkedList_PeekFirst(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the item stored at the first node without removing it from the list.
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(IN)   :: List
    !% the item to be retrieved from the list
    ItemTypeA,         INTENT(OUT)  :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the first node and set the flag
    Item = List%Head%Item
    Flag = TrueVal

    RETURN

END FUNCTION LinkedList_PeekFirst

!******************************************************************************

FUNCTION LinkedList_PeekLast(List, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the item stored at the last node without removing it from the list.
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(IN)   :: List
    !% the item to be retrieved from the list
    ItemTypeA,         INTENT(OUT)  :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the list is NOT empty.
    ! - false if the list is empty.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! get the item of the last node and set the flag
    Item = List%Tail%Item
    Flag = TrueVal

    RETURN

END FUNCTION LinkedList_PeekLast

!******************************************************************************

FUNCTION LinkedList_PeekAt(List, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the item (without removing it from the list) at the specified index
    !  where the index must be between 1 and the list size.  Also, return
    !  a flag indicating whether the item is available or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DblLnkList object
    CLASS(DblLnkList), INTENT(INOUT)    :: List
    !% the one-based index into the list's items
    tIndex,            INTENT(IN)       :: Index
    !% the item to be retrieved from the list
    ItemTypeA,         INTENT(OUT)      :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the item is available.
    ! - false if the item is NOT available.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(DLLNode), POINTER  :: CurrNode

! FLOW

    ! get the specified node
    CurrNode => List%GetNodeAt(Index)
    
    IF (ASSOCIATED(CurrNode)) THEN
        ! set output flag
        Flag = TrueVal
        ! get the item stored in the node
        Item = CurrNode%Item
    ELSE
        ! set output flag
        Flag = FalseVal
    END IF
    
    NULLIFY(CurrNode)

    RETURN

END FUNCTION LinkedList_PeekAt

!**************************************************************************************

FUNCTION LinkedList_GetAllItems(List, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get all items (without removing them) from the list.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DblLnkList),      INTENT(INOUT)   :: List     !! DblLnkList object
    ItemTypeC, ALLOCATABLE, INTENT(OUT)     :: Items(:) !! an allocatable array of items
    !> flag indicating whether the items are successfully retrieved. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
#ifdef ItemType_Is_Character
    tIndex      :: MaxLength
#endif
    tIndex      :: I, N

! FLOW

    ! check if the list is empty or not
    IF (List%IsEmpty()) THEN
        ! set output
        Flag = FalseVal
        ! report error
        CALL Handle_ErrLevel('LinkedList_GetAllItems', ModName, ErrWarning, 'The list is EMPTY.')
        RETURN
    END IF

    ! get number of items
    N = List%Size

#ifdef ItemType_Is_Character
    ! determine maximum length
    MaxLength = 0
    CALL List%Traverse(MaxLengthIterator)
    ! allocate storage for output
    CALL MemAlloc(MaxLength, Items, N)
#else
    ! allocate storage for output
    CALL MemAlloc(Items, N)
#endif

    ! get items
    I = 0
    CALL List%Traverse(ItemIterator)

    ! set flag
    Flag = TrueVal

    RETURN

CONTAINS

#ifdef ItemType_Is_Character

    FUNCTION MaxLengthIterator(Item,Done) RESULT(ErrStat)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)       :: Item     ! item
        tLogical,  INTENT(INOUT)    :: Done     ! on input, Done is set to FalseVal
                                                ! on exit, set it to TrueVal if user
                                                !   want to stop the stack traversing.
        tLogical                    :: ErrStat  ! true if error occurred in the user routine

    !** FLOW:

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
        ! determine maximum length
        IF (MaxLength < LEN(Item)) MaxLength = LEN(Item)
        RETURN
    END FUNCTION MaxLengthIterator

#endif
    !**************************************************************************

    FUNCTION ItemIterator(Item,Done) RESULT(ErrStat)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ItemTypeB, INTENT(IN)       :: Item     ! item
        tLogical,  INTENT(INOUT)    :: Done     ! on input, Done is set to FalseVal
                                                ! on exit, set it to TrueVal if user
                                                !   want to stop the stack traversing.
        tLogical                    :: ErrStat  ! true if error occurred in the user routine

    !** FLOW:

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
        ! get item
        I = I + 1
        Items(I) = Item
        RETURN
    END FUNCTION ItemIterator

    !**************************************************************************

END FUNCTION LinkedList_GetAllItems

! ---------------------------------------------------------------------
! -----             Final Procedure                               -----
! ---------------------------------------------------------------------

SUBROUTINE LinkedList_Finalizer(List)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DblLnkList), INTENT(INOUT) :: List  !! DblLnkList object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! destroy all nodes and free up memory
    CALL List%Clear()

    RETURN

END SUBROUTINE LinkedList_Finalizer

!******************************************************************************

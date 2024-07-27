!** DERIVED TYPE DEFINITIONS
    !> *DLLNode* is a node type that consists of an item and two pointers of the node type.
    !   As such, the node can point to its adjacent nodes in two directions (next node and
    !   previous node) allowing a doubly-linked list to be formed. <br>
    !   The type of the item stored in this node is *TypeOfItem*.  The  *DLLNode* type is
    !   a private type.
    TYPE DLLNode
        PRIVATE
        !> *Item* is an item (or value) stored in the node where its type can
        !   be any Fortran intrinsic type (character, integer, etc.).
        ItemTypeA               :: Item
        !% pointer to the next node
        TYPE(DLLNode), POINTER  :: Next   => NULL()
        !% pointer to previous node
        TYPE(DLLNode), POINTER  :: Prev   => NULL()
    CONTAINS
        ! destructor procedure
        PROCEDURE, PRIVATE  :: Destruct => LinkedNode_Destructor
    END TYPE DLLNode
    !> *DblLnkList* is a container type that employs a doubly-linked list implementation to
    !   provide common operations for a list container.  It can also represent other forms
    !   of containers including a LIFO stack, a FIFO queue and a double-ended queue (deque).
    !   The type of items stored in this container is *TypeOfItem*.
    TYPE DblLnkList
        PRIVATE
        !% size of the list container (i.e. number of nodes in the list)
        tIndex                  :: Size = 0
        !% pointer to the first node (or the head node) of the list
        TYPE(DLLNode), POINTER  :: Head   => NULL()
        !% pointer to the last node (or the tail node) of the list
        TYPE(DLLNode), POINTER  :: Tail   => NULL()
        !% pointer to the current node used for iteration purpose
        TYPE(DLLNode), POINTER  :: Cursor => NULL()
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration
        !  - zero     -> iteration not yet start
        tInteger                :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             PRIVATE PROCEDURES                            -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: SetNewNode   => LinkedList_SetNewNode
        PROCEDURE, PRIVATE  :: RemoveNode   => LinkedList_RemoveNode
        PROCEDURE, PRIVATE  :: GetNodeAt    => LinkedList_GetNodeAt
        PROCEDURE, PRIVATE  :: Traverse     => LinkedList_Traverse
        ! ---------------------------------------------------------------------
        ! -----             PUBLIC PROCEDURES                             -----
        ! ---------------------------------------------------------------------
        ! -----             Constructor and Destructor Procedures         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a list from an array of items. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Construct(10, Arr)    ! create a list from an array of 10 items
        PROCEDURE   :: Construct    => LinkedList_CreateByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        ! **Purpose**:  To destruct a list and get its items if requested. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Destruct()    ! destruct the list <br>
        !  **Note**: This method is equivalent to the *Clear* method.
        PROCEDURE   :: Destruct     => LinkedList_Destructor
        ! ---------------------------------------------------------------------
        ! -----             Insertion and Removal Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddFirst(Item)
        PROCEDURE   :: AddFirst         => LinkedList_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%AddLast(Item)
        PROCEDURE   :: AddLast          => LinkedList_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the list size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = List%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.List%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt            => LinkedList_AddAt
        !> **Type-Bound Subroutine**: Remove <br>
        ! **Purpose**:  To remove an item from the list.  The first item is removed
        !               by default.  If specified, the last item can be removed instead. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Remove()          ! remove the first item <br>
        !   --->    CALL List%Remove(.FALSE.)   ! remove the last item <br>
        !   --->    CALL List%Remove(Item=Item) ! retrieve and the remove the first item
        PROCEDURE   :: Remove           => LinkedList_Remove
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the list.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.List%RemoveFirst(Item)) DoSomething
        PROCEDURE   :: RemoveFirst      => LinkedList_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the list.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveLast(Item) <br>
        !   --->    IF (.NOT.List%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast       => LinkedList_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the list size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.List%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt         => LinkedList_RemoveAt
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !   *StartFirst* and *MoveForward* methods (or the *StartLast* and
        !   *MoveBackward* methods).  Therefore, after the call to one of those
        !   methods and then calling this one will result in a removal of the
        !   current item of the iteration (i.e. the same item that can be retrieved
        !   via those iteration methods).
        PROCEDURE   :: Delete           => LinkedList_Delete
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%Clear()
        PROCEDURE   :: Clear            => LinkedList_ClearItems
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the list.  Also, return a flag
        !                indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = List%ToArray(Items) <br>
        !   --->    IF (.NOT.List%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray          => LinkedList_ToArray
        !> **Type-Bound Subroutine**: RemoveDuplicates <br>
        !  **Purpose**:  To remove nodes with duplicated items from the list. <br>
        !  **Usage**: <br>
        !   --->    CALL List%RemoveDuplicates()
        PROCEDURE   :: RemoveDuplicates => LinkedList_RemoveDuplicates
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the list is empty or not. <br>
        !  **Usage**: see *MoveForward* procedure.
        PROCEDURE   :: StartFirst       => LinkedList_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move *forward* to the next iteration and return a flag
        !                indicating whether the cursor pointer has reached the end
        !                of the list or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the list.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsEmpty = List%StartFirst()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveForward()
        !       ! check whether we reach the end of the list or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the list.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsTheEnd = List%StartFirst(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveForward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveForward      => LinkedList_Move2NextElm
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the list is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast        => LinkedList_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move *backward* to the next iteration and return a flag
        !                indicating whether the cursor pointer has reached the end
        !                of the list or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the list in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = List%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveBackward()
        !       ! check whether we reach the end of the list or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the list in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = List%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = List%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveBackward     => LinkedList_Move2PrevElm
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                       ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the list is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%IsEmpty() <br>
        !   --->    IF (.NOT.List%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => LinkedList_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the list. <br>
        !  **Usage**: <br>
        !   --->    ListSize = List%GetSize()
        PROCEDURE   :: GetSize          => LinkedList_GetSize
        ! ---------------------------------------------------------------------
        ! -----                 Retrieval Procedures                     ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the first item (without removing it from the list).
        !                Also, return a flag indicating whether the item is available
        !                or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekFirst(Item) <br>
        !   --->    IF (.NOT.List%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst        => LinkedList_PeekFirst
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the list).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekLast(Item) <br>
        !   --->    IF (.NOT.List%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast        => LinkedList_PeekLast
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the list) at
        !                the specified index where the index must be between 1 and the
        !                list size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = List%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.List%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt           => LinkedList_PeekAt
        !> **Type-Bound Subroutine**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the list. Also,
        !                return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = List%GetAll(Items) <br>
        !   --->    IF (.NOT.List%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll           => LinkedList_GetAllItems
        ! ---------------------------------------------------------------------
        ! -----                 Queue Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: EnQueue <br>
        ! **Purpose**:  To add a new item to the end of the queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Queue%EnQueue(NewItem) <br>
        !  **Note**: *EnQueue* is an alias of *AddLast*.
        PROCEDURE   :: EnQueue  => LinkedList_AddLast
        !> **Type-Bound Function**: DeQueue <br>
        !  **Purpose**:  To get and remove the front (first) item of the queue.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Queue%DeQueue(Item) <br>
        !   --->    IF (.NOT.Queue%DeQueue(Item)) DoSomething <br>
        !  **Note**: *DeQueue* is an alias of *RemoveFirst*.
        PROCEDURE   :: DeQueue  => LinkedList_RemoveFirst
        ! ---------------------------------------------------------------------
        ! -----                 Stack Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new item to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        !  **Note**: *Push* is an alias of *AddLast*.
        PROCEDURE   :: Push     => LinkedList_AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Stack%Pop(Item) <br>
        !   --->    IF (.NOT.Stack%Pop(Item)) DoSomething <br>
        !  **Note**: *Pop* is an alias of *RemoveLast*.
        PROCEDURE   :: Pop      => LinkedList_RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekTop(Item) <br>
        !   --->    IF (.NOT.Container%PeekTop(Item)) DoSomething <br>
        !  **Note**: *PeekTop* is an alias of *PeekLast*.
        PROCEDURE   :: PeekTop  => LinkedList_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the container.
        FINAL       :: LinkedList_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE DblLnkList

!** INTERFACE DEFINITIONS:
    ! abstract interfaces
    ABSTRACT INTERFACE
        ! IterFuncItem is a procedure supplied to a *Traverse* procedure that
        ! can be used to get the list's item.
        FUNCTION IterFuncItem(Item,Done) RESULT(ErrStat)
            IMPORT
            ItemTypeB, INTENT(IN)       :: Item     ! item
            tLogical,  INTENT(INOUT)    :: Done     ! on input, Done is set to .FALSE.
                                                    ! on exit, set it to .TRUE. if user
                                                    !   want to stop the queue traversing.
            tLogical                    :: ErrStat  ! true if error occurred in the user routine
        END FUNCTION IterFuncItem
    END INTERFACE

    !> *DynArr* is a container type that employs a dynamic-array implementation to provide
    !   common operations for a list container.  It can also represent other forms of
    !   containers including a LIFO stack, a FIFO queue and a double-ended queue (deque).
    !   The type of items stored in this container is *TypeOfItem*.
    TYPE DynArr
        PRIVATE
        !> incremental size of the container when the container is full.
        !  Its value will be reset to 0 if the optional input is NOT
        !  specified during construction
        tIndex      :: IncSize = 16
        !% flag to shrink the container capacity
        tLogical    :: Shrink = .FALSE.
        !% pointer to first item of the queue
        tIndex      :: First  = 1_kIndex
        !% pointer to next to last item of the queue (i.e. the next available slot)
        tIndex      :: Last   = 1_kIndex
        !% size of the container (number of items)
        tIndex      :: Size   = 0_kIndex
        !% pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tInteger    :: Dir = 0
        !% items stored in the container.
        TypeAlloc   :: Items(:)
#ifdef ItemType_Is_Character
        ! length of character string
        tIndex      :: CharLen = 80_kIndex
#endif
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----             PRIVATE PROCEDURES                            -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Offset       => DynArr_GetFirst
        PROCEDURE, PRIVATE  :: Resize       => DynArr_MemResize
        PROCEDURE, PRIVATE  :: Growing      => DynArr_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => DynArr_Shrinking
        PROCEDURE, PRIVATE  :: DynArr_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----             PUBLIC PROCEDURES                             -----
        ! ---------------------------------------------------------------------
        ! -----             Constructor and Destructor Procedures         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty container. <br>
        !  **Usage**: <br>
#ifdef ItemType_Is_Character
        !   ! create an empty container with specified character length and initial capacity <br>
        !   --->    CALL Container%CreateEmpty(80, 25) <br>
        !   ! create a container and specify the optional incremental size <br>
        !   --->    CALL Container%CreateEmpty(80, 25, IncSize=16) <br>
        !   ! create a container and specify the optional shrink flag <br>
        !   --->    CALL Container%CreateEmpty(80, 25, Shrink=.TRUE.)
#else
        !   ! create an empty container with specified initial capacity <br>
        !   --->    CALL Container%CreateEmpty(25) <br>
        !   ! create a container and specify the optional incremental size <br>
        !   --->    CALL Container%CreateEmpty(25, IncSize=16) <br>
        !   ! create a container and specify the optional shrink flag <br>
        !   --->    CALL Container%CreateEmpty(25, Shrink=.TRUE.)
#endif
        PROCEDURE   :: CreateEmpty  => DynArr_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new container from an array of items. <br>
        !  **Usage**: <br>
        !           ! create a container from an array of 25 items <br>
        !   --->    CALL Container%Construct(25, Arr) <br>
        !           ! create a container and specify the optional incremental size <br>
        !   --->    CALL Container%Construct(25, Arr, IncSize=16) <br>
        !           ! create a container and specify the optional shrink flag <br>
        !   --->    CALL Container%Construct(25, Arr, Shrink=.TRUE.)
        GENERIC     :: Construct    => DynArr_CreateByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the container and free memory
        !                of items stored in the container.<br>
        !  **Usage**: <br>
        !   --->    CALL Container%Destruct() <br>
        !  **Important Note**:  For the *DynArr* class, this method is not equivalent
        !   to the *Clear* method.  Therefore, after calling the *Destruct* method,
        !   the user should reconstruct the container (by calling a *Construction*
        !   method again) before using other operations once more.  Otherwise, the
        !   container's behavior may not be as expected.
        PROCEDURE   :: Destruct     => DynArr_Destroy
        ! ---------------------------------------------------------------------
        ! -----             Insertion and Removal Procedures              -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DynArr_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%AddLast(Item)
        PROCEDURE   :: AddLast      => DynArr_AddLast
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the container size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => DynArr_AddAt
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the container.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Container%RemoveFirst(Item)) DoSomething
        PROCEDURE   :: RemoveFirst  => DynArr_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the container.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Container%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DynArr_RemoveLast
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the container size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => DynArr_RemoveAt
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with
        !   the *StartFirst* and *MoveForward* methods.  Therefore, after
        !   the call to one of those methods and then calling this one
        !   will result in a removal of the current item of the iteration
        !   (i.e. the same item that can be retrieved via the *StartFirst*
        !   and *MoveForward* methods).
        PROCEDURE   :: Delete       => DynArr_Delete
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the container. <br>
        !  **Usage**: <br>
        !   --->    CALL Container%Clear()
        PROCEDURE   :: Clear        => DynArr_ClearItems
        !> **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the container.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%ToArray(Items) <br>
        !   --->    IF (.NOT.Container%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => DynArr_ToArray
        ! ---------------------------------------------------------------------
        ! -----                 Iteration Procedures                      -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the container is empty or not. <br>
        !  **Usage**: see *MoveForward* procedure.
        PROCEDURE   :: StartFirst   => DynArr_Move2FirstElm
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the container or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the container.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsEmpty = Container%StartFirst()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveForward()
        !       ! check whether we reach the end of the container or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the container.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start forward iteration (from the first item)
        !   IsTheEnd = Container%StartFirst(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveForward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveForward  => DynArr_Move2NextElm
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the container is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast    => DynArr_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the next iteration (in reverse order) and return
        !                a flag indicating whether the cursor pointer has reached the
        !                end of the container or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the container in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = Container%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveBackward()
        !       ! check whether we reach the end of the container or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the container in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = Container%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Container%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveBackward => DynArr_Move2PrevElm
        ! ---------------------------------------------------------------------
        ! -----                 Inquiry Procedures                       ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the container. <br>
        !  **Usage**: <br>
        !   --->    Size = Container%GetSize()
        PROCEDURE   :: GetSize      => DynArr_GetSize
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the container is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Container%IsEmpty() <br>
        !   --->    IF (.NOT.Container%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => DynArr_IsEmpty
        ! ---------------------------------------------------------------------
        ! -----                 Retrieval Procedures                     ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Container%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst    => DynArr_PeekFirst
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekLast(Item) <br>
        !   --->    IF (.NOT.Container%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DynArr_PeekLast
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the container) at
        !                the specified index where the index must be between 1 and the
        !                container size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Container%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => DynArr_PeekAt
        !> *GetAll* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the container.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%GetAll(Items) <br>
        !   --->    IF (.NOT.Container%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => DynArr_GetAll
        ! ---------------------------------------------------------------------
        ! -----                 Queue Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: EnQueue <br>
        ! **Purpose**:  To add a new item to the end of the queue. <br>
        !  **Usage**: <br>
        !   --->    CALL Queue%EnQueue(NewItem) <br>
        !  **Note**: *EnQueue* is an alias of *AddLast*.
        PROCEDURE   :: EnQueue      => DynArr_AddLast
        !> **Type-Bound Function**: DeQueue <br>
        !  **Purpose**:  To get and remove the front (first) item of the queue.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Queue%DeQueue(Item) <br>
        !   --->    IF (.NOT.Queue%DeQueue(Item)) DoSomething <br>
        !  **Note**: *DeQueue* is an alias of *RemoveFirst*.
        PROCEDURE   :: DeQueue      => DynArr_RemoveFirst
        ! ---------------------------------------------------------------------
        ! -----                 Stack Procedures                         ------
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        ! **Purpose**:  To add a new item to the top of the stack. <br>
        !  **Usage**: <br>
        !   --->    CALL Stack%Push(NewItem) <br>
        !  **Note**: *Push* is an alias of *AddLast*.
        PROCEDURE   :: Push         => DynArr_AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top item of the stack.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Stack%Pop(Item) <br>
        !   --->    IF (.NOT.Stack%Pop(Item)) DoSomething <br>
        !  **Note**: *Pop* is an alias of *RemoveLast*.
        PROCEDURE   :: Pop          => DynArr_RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last item (without removing it from the container).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Container%PeekTop(Item) <br>
        !   --->    IF (.NOT.Container%PeekTop(Item)) DoSomething <br>
        !  **Note**: *PeekTop* is an alias of *PeekLast*.
        PROCEDURE   :: PeekTop      => DynArr_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the container.
        FINAL       :: DynArr_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE DynArr


MODULE Class_LinkedLists

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *linked-list-based* types and their related routines.
!   A *linked-list-based* type is a collection type employing a linked list
!   implementation.  <br>
!   Available collection types based on the linked list concept include:  <br>
!   - the *StackLinked* type that represents a last-in-first-out (LIFO) stack,  <br>
!   - the *QueueLinked* type that represents a first-in-first-out (FIFO) queue,  <br>
!   - the *DequeLinked* type that represents a double-ended queue (deque).  <br>
!   - the *ListLinked* type that represents a list where an item can be added,
!     removed or retrieved at the (valid) specified index.  <br>
!   **Usage Notes**:  <br>
!   - Unlike *dynamic-array-based* types, *linked-list-based* types commonly do not
!     require an explicit construction.  Items can be added via the *Construction*
!     method or an insert method.  Therefore, the *CreateEmpty* method used to
!     construct an empty collection is deemed unnecessary and thus NOT provided.  <br>
!   - Other than that, all operations provided are the same for both groups of
!     collections.  <br>
!   **Implementation Note**:  <br>
!   - Unlike conventional implementation, all *linked-list-based* types provided
!     in this module employ the <a href="../module/class_intrusivelinkedlists.html#type-intrusivelinearlist">IntrusiveLinearList</a>
!     type, which is an intrusive doubly-linked list container type that performs
!     common linked-list operations without a memory management task.  <br>
!   - As a result, the *linked-list-based* types mostly perform the memory management
!     task while common operations of a linked list are relegated to the
!     *IntrusiveLinearList* type, which is declared as a private component of
!     the *linked-list-based* types.  <br>
!   - The implementation in this module is intended to illustrate the usage of
!     an intrusive container type.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE Class_IntrusiveLinkedLists, ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE Class_Assignable
    USE Class_BaseCollection
    USE Class_BaseIterable
    USE Class_GenStore
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DequeLinked
    PUBLIC :: ListLinked
    PUBLIC :: QueueLinked
    PUBLIC :: StackLinked

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_LinkedLists'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *LinkedNode* is a linked node type with the *GenStore* type as a storage
    !  for the collection's item.  The *LinkedNode* type is a subtype of the
    !  *DoublyLinkedNode* type and is intended to be used with a collection
    !  type that utilizes the *IntrusiveLinearList* type. <br>
    !  **Note**: This is a private type.
    TYPE, EXTENDS(DoublyLinkedNode) :: LinkedNode(ValSize)
        !% size of data content (a value of an element) in bytes
        tInteger,                LEN        :: ValSize
        !% storage of item (or value).
        TYPE(GenStore(ValSize)), PRIVATE    :: Store
    END TYPE LinkedNode
    !> **Description**: <br>
    !   The *QueueLinked* type is a collection type that employs a linked-list
    !   implementation to provide common operations for a FIFO queue. <br>
    !  **Usage Overview**: <br>
    !   The *QueueLinked* type is a *queue* collection type that provides common
    !   operations of a FIFO queue.  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *Enqueue* method - method to insert an item to the end of the collection, <br>
    !   (2.2) *Dequeue* method - method to get and remove the first item of the collection, <br>
    !   (2.3) *Clear* method - method to remove all items from the collection. <br>
    !   (2.4) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekFirst* method - method to retrieve the first item of the collection, <br>
    !   (3.2) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.3) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.4) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item.
    TYPE, EXTENDS(BaseIterable) :: QueueLinked
        PRIVATE
        ! a working doubly-linked list
        TYPE(IntrusiveLinearList)   :: WrkLst
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tInteger                    :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: QueueLinked_CreateByArray
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => QueueLinked_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => QueueLinked_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => QueueLinked_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => QueueLinked_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => QueueLinked_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => QueueLinked_CopyCollection
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  For the linked-list-based types, this method is equivalent
        !             to the *Clear* method.
        PROCEDURE   :: Destruct     => QueueLinked_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => QueueLinked_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseIterable Type        -----
        ! ---------------------------------------------------------------------
        !> *StartFirst* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE   :: StartFirst   => QueueLinked_Move2FirstElm
        !> *MoveForward* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem) <br>
        !  **Note**: See the <a href="../module/class_baseiterable.html">BaseIterable</a> type
        !            for illustrations of an iteration over a collection using the *StartFirst*
        !            and *MoveForward* methods.<br>
        PROCEDURE   :: MoveForward  => QueueLinked_Move2NextElm
        !> *Insert* is a procedure deferred by the *BaseIterable* type. <br>
        !  Use the *Enqueue* method in place of the *Insert* method to add an item
        !  to the collection.
        PROCEDURE   :: Insert       => QueueLinked_AddElm
        !> *Delete* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete an item from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with
        !   the *StartFirst* and *MoveForward* methods.  Therefore, after
        !   the call to one of those methods and then calling this one
        !   will result in a removal of the current item of the iteration
        !   (i.e. the same item that can be retrieved via the *StartFirst*
        !   and *MoveForward* methods).
        PROCEDURE   :: Delete       => QueueLinked_DelElm
        !> *ToArray* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => QueueLinked_ToArray
        !> *GetAll* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => QueueLinked_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or
        !                from another collection. <br>
        !  **Usage**: <br>
        !           ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !           ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => QueueLinked_CreateByArray
        !> **Type-Bound Subroutine**: Enqueue <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Enqueue(Item)
        GENERIC     :: Enqueue      => Insert
        !> **Type-Bound Function**: Dequeue <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Dequeue(Item) <br>
        !   --->    IF (.NOT.Collection%Dequeue(Item)) DoSomething
        PROCEDURE   :: Dequeue      => QueueLinked_Dequeue
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Collection%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst    => QueueLinked_PeekFirst
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: QueueLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE QueueLinked
    !> **Description**: <br>
    !   The *StackLinked* type is a collection type that employs a linked-list
    !   implementation to provide common operations for a LIFO stack. <br>
    !  **Usage Overview**: <br>
    !   The *StackLinked* type is a *stack* collection type that provides common
    !   operations of a LIFO stack.  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *Push* method - method to insert an item at the top (to the end) of the collection, <br>
    !   (2.2) *Pop* method - method to get and remove the top (last) item of the collection, <br>
    !   (2.3) *Clear* method - method to remove all items from the collection. <br>
    !   (2.4) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekTop* method - method to retrieve the top (last) item of the collection, <br>
    !   (3.2) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.3) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.4) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item. <br>
    !  **Note**: Since the *StackLinked* type is a subtype of the *QueueLinked* type,
    !            it can also be used as a FIFO queue.
    TYPE, EXTENDS(QueueLinked) :: StackLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => StackLinked_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the end (top) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        PROCEDURE   :: Push         => StackLinked_Push
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the last (top) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        PROCEDURE   :: Pop          => StackLinked_Pop
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last (top) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        PROCEDURE   :: PeekTop      => StackLinked_PeekTop
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: StackLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE StackLinked
    !> **Description**: <br>
    !   The *DequeLinked* type is a collection type that employs a linked-list
    !   implementation to provide common operations for a double-ended queue (deque).
    !   It can be used as a FIFO queue or a LIFO stack as well. <br>
    !  **Usage Overview**: <br>
    !   The *DequeLinked* type is a *deque* collection type that provides common
    !   operations of a double-ended queue (deque).  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *Destruct* method - method to destruct the collection. <br>
    !   (2) Insert and remove.  Methods for these operations include <br>
    !   (2.1) *AddFirst* method - method to insert an item to the front of the collection, <br>
    !   (2.2) *AddLast* method - method to insert an item to the end of the collection, <br>
    !   (2.3) *RemoveFirst* method - method to get and remove the first item of the collection, <br>
    !   (2.4) *RemoveLast* method - method to get and remove the last item of the collection, <br>
    !   (2.5) *Clear* method - method to remove all items from the collection. <br>
    !   (2.6) *ToArray* method - method to remove and retrieve all items from the collection. <br>
    !   (3) Inquiry.  Methods for these operations include <br>
    !   (3.1) *PeekFirst* method - method to retrieve the first item of the collection, <br>
    !   (3.2) *PeekLast* method - method to retrieve the last item of the collection, <br>
    !   (3.3) *GetSize* method - method to get the collection size (number of items stored), <br>
    !   (3.4) *IsEmpty* method - method to check whether the collection is empty or not. <br>
    !   (3.5) *GetAll* method - method to retrieve all items from the collection. <br>
    !   (4) Iteration.  Methods for these operations include <br>
    !   (4.1) *StartFirst* method - method to start a forward iteration over items, <br>
    !   (4.2) *MoveForward* method - method to move forward to the next item, <br>
    !   (4.3) *StartLast* method - method to start a backward iteration over items, <br>
    !   (4.4) *MoveBackward* method - method to move backward to the previous item. <br>
    !   The *DequeArray* type also supports the usual *Enqueue*, *Dequeue* and *PeekFirst*
    !   operations of a FIFO queue as well as the usual *Push*, *Pop* and *PeekTop* operations
    !   of a LIFO stack.  Therefore, when using as a queue or a stack, these operations can be
    !   used in place of *insert*, *remove*, *peek* operations of a deque as desired.
    TYPE, EXTENDS(QueueLinked) :: DequeLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => DequeLinked_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast    => DequeLinked_Move2LastElm
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the next iteration (in reverse order) and return
        !                a flag indicating whether the cursor pointer has reached the
        !                end of the collection or not. <br>
        !  **Usage**: <br>
        !   The following code snippet illustrates how to typically traverse the collection in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsEmpty = Collection%StartLast()
        !   IF (.NOT.IsEmpty) DoSomeThing...
        !   DO
        !       ! move to the next iteration
        !       IsTheEnd = Collection%MoveBackward()
        !       ! check whether we reach the end of the collection or not
        !       IF (IsTheEnd) EXIT
        !       ! if not, do the task we need
        !       DoSomeThing...
        !   END DO
        !   </Code></Pre> <br>
        !   The following code snippet shows another way to iterate over the collection in reverse order.
        !   <Pre><Code style="color:MidnightBlue;">
        !   ! start backward iteration (from the last item)
        !   IsTheEnd = Collection%StartLast(CurrItem)
        !   DO WHILE (.NOT.IsTheEnd)
        !       DoSomeThing_With_CurrItem...
        !       ! move to the next iteration
        !       IsTheEnd = Collection%MoveBackward(CurrItem)
        !   END DO
        !   </Code></Pre>
        PROCEDURE   :: MoveBackward => DequeLinked_Move2PrevElm
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DequeLinked_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddLast(Item)
        PROCEDURE   :: AddLast      => DequeLinked_AddLast
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveFirst(Item)) DoSomething
        PROCEDURE   :: RemoveFirst  => DequeLinked_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DequeLinked_RemoveLast
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekLast(Item) <br>
        !   --->    IF (.NOT.Collection%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DequeLinked_PeekLast
        ! ---------------------------------------------------------------------
        ! -----             Redefined Stack Operations                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the end (top) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push         => AddLast
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the last (top) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        GENERIC     :: Pop          => RemoveLast
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the last (top) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        GENERIC     :: PeekTop      => PeekLast
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: DequeLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE DequeLinked
    !> **Description**: <br>
    !   The *ListLinked* type is a collection type that employs a linked-list
    !   implementation to provide common operations for a list. <br>
    !  **Usage Overview**: <br>
    !   The *ListLinked* type provides insert, remove and peek operations at a
    !   specified index where the index must be between 1 and the collection size.
    !   The *ListLinked* type is a subtype of the *DequeLinked* type; therefore, 
    !   all operations available for the *DequeLinked* type are also available for
    !   the *ListLinked* type.  Furthermore, it thus can be used as a deque, a FIFO
    !   queue or a LIFO stack.  <br>
    TYPE, EXTENDS(DequeLinked) :: ListLinked
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetTypeName <br>
        !  **Purpose**:  To get the name of the collection type. <br>
        !  **Usage**: <br>
        !   --->    TypeName = Collection%GetTypeName()
        PROCEDURE   :: GetTypeName  => ListLinked_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListLinked Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the collection size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => ListLinked_AddAt
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the collection size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => ListLinked_RemoveAt
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the collection) at
        !                the specified index where the index must be between 1 and the
        !                collection size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => ListLinked_PeekAt
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: ListLinked_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ListLinked

!** INTERFACE DEFINITIONS:
    ! interfaces for QueueLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_CopyAssign(DstObj, SrcObj)
            CLASS(QueueLinked(*)), INTENT(OUT)  :: DstObj
            CLASS(Assignable),     INTENT(IN)   :: SrcObj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_Clone(SrcObj, DstObj)
            CLASS(QueueLinked(*)),          INTENT(IN)  :: SrcObj
            CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            CLASS(QueueLinked(*)), INTENT(IN)   :: LhsObj
            CLASS(Assignable),     INTENT(IN)   :: RhsObj
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_Free(Obj)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Obj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_GetTypeName(Obj) RESULT(Name)
            CLASS(QueueLinked(*)), INTENT(IN)   :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_CopyCollection(This, Other)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: This
            CLASS(BaseCollection), INTENT(INOUT)    :: Other
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_ClearItems(Collection)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_Destroy(Collection)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_GetSize(Collection) RESULT(Size)
            CLASS(QueueLinked(*)), INTENT(IN)   :: Collection
            tIndex                              :: Size
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Move2FirstElm(Collection, Item) RESULT(IsEmpty)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
            tLogical                                :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Move2NextElm(Collection, Item) RESULT(IsTheEnd)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
            tLogical                                :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_AddElm(Collection, Item)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_DelElm(Collection)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueLinked_CreateByArray(Collection, N, Items)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            tIndex,                INTENT(IN)       :: N
            TYPE(*),               INTENT(IN)       :: Items(:)
        END SUBROUTINE QueueLinked_CreateByArray
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_Dequeue(Collection, Item) RESULT(Flag)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_PeekFirst(Collection, Item) RESULT(Flag)
            CLASS(QueueLinked(*)), INTENT(IN)       :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_ToArray(Collection, Items) RESULT(Flag)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Items(:)
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueLinked_GetAll(Collection, Items) RESULT(Flag)
            CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Items(:)
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for StackLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_GetTypeName(Obj) RESULT(Name)
            CLASS(StackLinked(*)), INTENT(IN)   :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackLinked_Push(Collection, Item)
            CLASS(StackLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_Pop(Collection, Item) RESULT(Flag)
            CLASS(StackLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackLinked_PeekTop(Collection, Item) RESULT(Flag)
            CLASS(StackLinked(*)), INTENT(IN)       :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for DequeLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_GetTypeName(Obj) RESULT(Name)
            CLASS(DequeLinked(*)), INTENT(IN)   :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_Move2LastElm(Collection, Item) RESULT(IsEmpty)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
            tLogical                                :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_Move2PrevElm(Collection, Item) RESULT(IsTheEnd)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
            tLogical                                :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE DequeLinked_AddFirst(Collection, Item)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE DequeLinked_AddLast(Collection, Item)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(IN)       :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_RemoveFirst(Collection, Item) RESULT(Flag)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_RemoveLast(Collection, Item) RESULT(Flag)
            CLASS(DequeLinked(*)), INTENT(INOUT)    :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeLinked_PeekLast(Collection, Item) RESULT(Flag)
            CLASS(DequeLinked(*)), INTENT(IN)       :: Collection
            TYPE(*),               INTENT(INOUT)    :: Item
            tLogical                                :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for ListLinked
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_GetTypeName(Obj) RESULT(Name)
            CLASS(ListLinked(*)), INTENT(IN)    :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListLinked Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_AddAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListLinked(*)), INTENT(INOUT) :: Collection
            tIndex,               INTENT(IN)    :: Index
            TYPE(*),              INTENT(IN)    :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_RemoveAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListLinked(*)), INTENT(INOUT) :: Collection
            tIndex,               INTENT(IN)    :: Index
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListLinked_PeekAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListLinked(*)), INTENT(INOUT) :: Collection
            tIndex,               INTENT(IN)    :: Index
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE DequeLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DequeLinked(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE DequeLinked_Finalize

!******************************************************************************

SUBROUTINE ListLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListLinked(*)), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE ListLinked_Finalize

!******************************************************************************

SUBROUTINE QueueLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(QueueLinked(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE QueueLinked_Finalize

!******************************************************************************

SUBROUTINE StackLinked_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StackLinked(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE StackLinked_Finalize

!******************************************************************************

END MODULE Class_LinkedLists

!******************************************************************************

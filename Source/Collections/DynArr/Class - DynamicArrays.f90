
MODULE Class_DynamicArrays

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains *dynamic-array-based* types and their related routines.
!   A *dynamic-array-based* type is a collection type employing a resizable
!   array (or the so-called dynamic array) that can increase or decrease its
!   size depending on the current collection size (number of items contained
!   in the collection) relative to the current collection capacity.  <br>
!   Available collection types based on the dynamic array concept include:  <br>
!   - the *StackArray* type that represents a last-in-first-out (LIFO) stack,  <br>
!   - the *QueueArray* type that represents a first-in-first-out (FIFO) queue,  <br>
!   - the *DequeArray* type that represents a double-ended queue (deque).  <br>
!   - the *ListArray* type that represents a list where an item can be added,
!     removed or retrieved at the (valid) specified index.  <br>
!   <br>
!   **Usage Notes**:  <br>
!   - All *dynamic-array-based* types commonly require an explicit construction
!     before using other provided operations.  There are two methods provided to
!     create a collection.  First, the *CreateEmpty* method constructs an empty
!     collection with the specified initial capacity.  Second, the *Construction*
!     method constructs a collection based on the specified input (either from an
!     array of items or from another collection).  <br>
!   - All available collection types are subtypes of the *BaseDynArr* abstract
!     collection type, which is a private type that is only available in this module.
!     The *BaseDynArr* type provides the *Growing* (private) method to expand the
!     collection's capacity and the *Shrinking* (also private) method to reduce
!     the capacity. <br>
!     When the collection is full (its current size is equal to its current capacity),
!     the collection's capacity is doubled by default.  However, if the *IncSize*
!     argument is specified during a construction of the collection, the collection's
!     capacity increases by the amount specified (the specified value must be positive)
!     instead of doubling.  <br>
!     When the collection's size is reducing due to removing items from the collection,
!     its capacity stays the same by default.  Nevertheless, if the *Shrink* flag is
!     specified and its value is true during a construction of the collection, the
!     collection's capacity is halved when its current size is one quarter of
!     its capacity.  <br>
!   - Instead of using as a deque, the *DequeArray* type can also be used as a
!     FIFO queue or a LIFO stack.  <br>
!   - Instead of using as a list, the *ListArray* type can also be used as a
!     deque, a FIFO queue or a LIFO stack.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_SIntUtil,       ONLY: MAX_I32, MAX_I64
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_BaseCollection
    USE Class_BaseIterable
    USE Class_GenStore
    USE ModBase_MemHandlers_GenStore
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: DequeArray
    PUBLIC :: ListArray
    PUBLIC :: QueueArray
    PUBLIC :: StackArray

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_DynamicArrays'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128
    ! maximum capacity of a dynamic-array-based collection
#ifdef Indx64Bits
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I64
#else
    tIndex,    PARAMETER    :: MaxCapacity = MAX_I32
#endif

!** DERIVED TYPE DEFINITIONS
    !> The *BaseDynArr* type is an abstract dynamic-array type that 
    !  provides resizing operations to grow or shrink the collection
    !  capacity depending on its current size (number of items contained). <br>
    !  **Note**: This is a private type.
    TYPE, ABSTRACT, EXTENDS(BaseIterable) :: BaseDynArr
        PRIVATE
        !> incremental size of the collection when the collection is full.
        !  Its value will be reset to 0 if the optional input is NOT
        !  specified during construction
        tIndex                                  :: IncSize = 16
        !% flag to shrink the collection capacity
        tLogical                                :: Shrink = .FALSE.
        !% items stored in the collection.
        TYPE(GenStore(ValSize)), ALLOCATABLE    :: Items(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *Offset* is a binding name of the *GetFirst* deferred procedure.
        !  This procedure is intended for internal uses only.
        PROCEDURE(GetFirst), DEFERRED   :: Offset
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: BaseDynArr_CreateByArray
        PROCEDURE, PRIVATE  :: Resize       => BaseDynArr_MemResize
        PROCEDURE, PRIVATE  :: Growing      => BaseDynArr_Growing
        PROCEDURE, PRIVATE  :: Shrinking    => BaseDynArr_Shrinking
        PROCEDURE, PRIVATE  :: CopyMembers  => BaseDynArr_CopyMembers
        ! ---------------------------------------------------------------------
        ! -----                     public procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty collection. <br>
        !  **Usage**: <br>
        !           ! create an empty collection with specified initial capacity <br>
        !   --->    CALL Collection%CreateEmpty(25) <br>
        !           ! create a collection and specify the optional incremental size <br>
        !   --->    CALL Collection%CreateEmpty(25, IncSize=16) <br>
        !           ! create a collection and specify the optional shrink flag <br>
        !   --->    CALL Collection%CreateEmpty(25, Shrink=.TRUE.)
        PROCEDURE   :: CreateEmpty  => BaseDynArr_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of items or
        !                from another collection. <br>
        !  **Usage**: <br>
        !           ! create a collection from an array of 25 items <br>
        !   --->    CALL Collection%Construct(25, Arr) <br>
        !           ! create a collection and specify the optional incremental size <br>
        !   --->    CALL Collection%Construct(25, Arr, IncSize=16) <br>
        !           ! create a collection and specify the optional shrink flag <br>
        !   --->    CALL Collection%Construct(25, Arr, Shrink=.TRUE.) <br>
        !           ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => BaseDynArr_CreateByArray
    END TYPE BaseDynArr
    !> **Description**: <br>
    !   The *StackArray* type is a collection type that employs a resizable-array
    !   implementation to provide common operations for a LIFO stack. <br>
    !  **Usage Overview**: <br>
    !   The *StackArray* type is a *stack* collection type that provides common
    !   operations of a LIFO stack.  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
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
    !   (4.2) *MoveForward* method - method to move forward to the next item.
    TYPE, EXTENDS(BaseDynArr) :: StackArray
        PRIVATE
        !% pointer to top (last) item of the stack
        tIndex      :: Top    = 0_kIndex
        !% pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => StackArray_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => StackArray_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => StackArray_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => StackArray_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => StackArray_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => StackArray_CopyCollection
        !> *Clear* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear        => StackArray_ClearItems
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Important Note**:  For the *StackArray* type, this method is not equivalent
        !                to the *Clear* method.  Therefore, after calling the *Destruct*
        !                method, the user should reconstruct the collection (by calling
        !                a *Construction* method again) before using other operations
        !                once more.  Otherwise, the collection's behavior may not be
        !                as expected.
        PROCEDURE   :: Destruct     => StackArray_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => StackArray_GetSize
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
        PROCEDURE   :: StartFirst   => StackArray_Move2FirstElm
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
        PROCEDURE   :: MoveForward  => StackArray_Move2NextElm
        !> *Insert* is a procedure deferred by the *BaseIterable* type. <br>
        !  Use the *Push* method in place of the *Insert* method to add an item
        !  to the collection.
        PROCEDURE   :: Insert       => StackArray_AddElm
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
        PROCEDURE   :: Delete       => StackArray_DelElm
        !> *ToArray* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray  => StackArray_ToArray
        !> *GetAll* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll   => StackArray_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseDynArr Type          -----
        ! ---------------------------------------------------------------------
        !% This procedure is intended for internal uses only.
        PROCEDURE   :: Offset       => StackArray_GetFirst
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Push <br>
        !  **Purpose**:  To insert the specified item at the top (end) of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Push(Item)
        GENERIC     :: Push     => Insert
        !> **Type-Bound Function**: Pop <br>
        !  **Purpose**:  To get and remove the top (last) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%Pop(Item) <br>
        !   --->    IF (.NOT.Collection%Pop(Item)) DoSomething
        PROCEDURE   :: Pop      => StackArray_Pop
        !> **Type-Bound Function**: PeekTop <br>
        !  **Purpose**:  To get the top (last) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekTop(Item) <br>
        !   --->    IF (.NOT.Collection%PeekTop(Item)) DoSomething
        PROCEDURE   :: PeekTop  => StackArray_PeekTop
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: StackArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE StackArray
    !> **Description**: <br>
    !   The *QueueArray* type is a collection type that employs a resizable-array
    !   implementation to provide common operations for a FIFO queue. <br>
    !  **Usage Overview**: <br>
    !   The *QueueArray* type is a *queue* collection type that provides common
    !   operations of a FIFO queue.  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
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
    TYPE, EXTENDS(BaseDynArr) :: QueueArray
        PRIVATE
        ! pointer to first item of the queue
        tIndex      :: First  = 1_kIndex
        ! pointer to next to last item of the queue (i.e. the next available slot)
        tIndex      :: Last   = 1_kIndex
        ! size of the collection (number of items)
        tIndex      :: Size   = 0_kIndex
        !% pointer to current item of the iteration
        tIndex      :: Cursor = 0_kIndex
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration (only applicable for deque and list)
        !  - zero     -> iteration not yet start
        tInteger    :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => QueueArray_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => QueueArray_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => QueueArray_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => QueueArray_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => QueueArray_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => QueueArray_CopyCollection
        !> *Clear* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear        => QueueArray_ClearItems
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Important Note**:  For the *QueueArray* class, this method is not equivalent
        !                to the *Clear* method.  Therefore, after calling the *Destruct*
        !                method, the user should reconstruct the collection (by calling
        !                a *Construction* method again) before using other operations
        !                once more.  Otherwise, the collection's behavior may not be
        !                as expected.
        PROCEDURE   :: Destruct     => QueueArray_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => QueueArray_GetSize
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
        PROCEDURE   :: StartFirst   => QueueArray_Move2FirstElm
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
        PROCEDURE   :: MoveForward  => QueueArray_Move2NextElm
        !> *Insert* is a procedure deferred by the *BaseIterable* type. <br>
        !  Use the *Enqueue* method in place of the *Insert* method to add an item
        !  to the collection.
        PROCEDURE   :: Insert       => QueueArray_AddElm
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
        PROCEDURE   :: Delete       => QueueArray_DelElm
        !> *ToArray* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE   :: ToArray      => QueueArray_ToArray
        !> *GetAll* is a procedure deferred by the *BaseIterable* type. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE   :: GetAll       => QueueArray_GetAll
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseDynArr Type          -----
        ! ---------------------------------------------------------------------
        !% This procedure is intended for internal uses only.
        PROCEDURE   :: Offset       => QueueArray_GetFirst
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueArray Type            -----
        ! ---------------------------------------------------------------------
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
        PROCEDURE   :: Dequeue      => QueueArray_Dequeue
        !> **Type-Bound Function**: PeekFirst <br>
        !  **Purpose**:  To get the front (first) item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekFirst(Item) <br>
        !   --->    IF (.NOT.Collection%PeekFirst(Item)) DoSomething
        PROCEDURE   :: PeekFirst    => QueueArray_PeekFirst
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: QueueArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE QueueArray
    !> **Description**: <br>
    !   The *DequeArray* type is a collection type that employs a resizable-array
    !   implementation to provide common operations for a double-ended queue (deque).
    !   It can be used as a FIFO queue or a LIFO stack as well. <br>
    !  **Usage Overview**: <br>
    !   The *DequeArray* type is a *deque* collection type that provides common
    !   operations of a double-ended queue (deque).  Their operations can be categorized
    !   as follows: <br>
    !   (1) Construction and destruction.  Methods for these operations include <br>
    !   (1.1) *Construct* method - method to construct the collection either from
    !       another collection or from an array of items, <br>
    !   (1.2) *CreateEmpty* method - method to construct an empty collection, <br>
    !   (1.3) *Destruct* method - method to destruct the collection. <br>
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
    TYPE, EXTENDS(QueueArray) :: DequeArray
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => DequeArray_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start the *backward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: see *MoveBackward* procedure.
        PROCEDURE   :: StartLast    => DequeArray_Move2LastElm
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
        PROCEDURE   :: MoveBackward => DequeArray_Move2PrevElm
        !> **Type-Bound Subroutine**: AddFirst <br>
        !  **Purpose**:  To insert the specified item at the front of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddFirst(Item)
        PROCEDURE   :: AddFirst     => DequeArray_AddFirst
        !> **Type-Bound Subroutine**: AddLast <br>
        !  **Purpose**:  To insert the specified item at the end of the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%AddLast(Item)
        PROCEDURE   :: AddLast      => DequeArray_AddLast
        !> **Type-Bound Function**: RemoveFirst <br>
        !  **Purpose**:  To get and remove the front (first) item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveFirst(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveFirst(Item)) DoSomething
        PROCEDURE   :: RemoveFirst  => DequeArray_RemoveFirst
        !> **Type-Bound Function**: RemoveLast <br>
        !  **Purpose**:  To get and remove the last item of the collection.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveLast(Item) <br>
        !   --->    IF (.NOT.Collection%RemoveLast(Item)) DoSomething
        PROCEDURE   :: RemoveLast   => DequeArray_RemoveLast
        !> **Type-Bound Function**: PeekLast <br>
        !  **Purpose**:  To get the last item (without removing it from the collection).
        !                Also, return a flag indicating whether the item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekLast(Item) <br>
        !   --->    IF (.NOT.Collection%PeekLast(Item)) DoSomething
        PROCEDURE   :: PeekLast     => DequeArray_PeekLast
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
        FINAL       :: DequeArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE DequeArray
    !> **Description**: <br>
    !   The *ListArray* type is a collection type that employs a resizable-array
    !   implementation to provide common operations for a list. <br>
    !  **Usage Overview**: <br>
    !   The *ListArray* type provides insert, remove and peek operations at a
    !   specified index where the index must be between 1 and the collection size.
    !   The *ListArray* type is a subtype of the *DequeArray* type; therefore, 
    !   all operations available for the *DequeArray* type are also available for
    !   the *ListArray* type.  Furthermore, it thus can be used as a deque, a FIFO
    !   queue or a LIFO stack.  <br>
    TYPE, EXTENDS(DequeArray) :: ListArray
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetTypeName <br>
        !  **Purpose**:  To get the name of the collection type. <br>
        !  **Usage**: <br>
        !   --->    TypeName = Collection%GetTypeName()
        PROCEDURE   :: GetTypeName  => ListArray_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: AddAt <br>
        !  **Purpose**:  To insert the specified item at the specified index where
        !                the index must be between 1 and the collection size.
        !                Also, return a flag indicating whether the item is
        !                successfully added. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%AddAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%AddAt(Index, Item)) DoSomething
        PROCEDURE   :: AddAt      => ListArray_AddAt
        !> **Type-Bound Function**: RemoveAt <br>
        !  **Purpose**:  To get and remove the item at the specified index where
        !                the index must be between 1 and the collection size.
        !                Also, return a flag indicating whether the item is
        !                successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%RemoveAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%RemoveAt(Index, Item)) DoSomething
        PROCEDURE   :: RemoveAt   => ListArray_RemoveAt
        !> **Type-Bound Function**: PeekAt <br>
        !  **Purpose**:  To get the item (without removing it from the collection) at
        !                the specified index where the index must be between 1 and the
        !                collection size.  Also, return a flag indicating whether the
        !                item is available or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%PeekAt(Index, Item) <br>
        !   --->    IF (.NOT.Collection%PeekAt(Index, Item)) DoSomething
        PROCEDURE   :: PeekAt     => ListArray_PeekAt
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: ListArray_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ListArray

!** INTERFACE DEFINITIONS:
    ! abstract interface for BaseDynArr
    ABSTRACT INTERFACE
        !> GetFirst is a deferred procedure to get an index
        !  pointing to the first item in the collection.
        FUNCTION GetFirst(Collection) RESULT(First)
            IMPORT
            !% the collection object
            CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection
            !% an index pointing to the first item
            tIndex                              :: First
        END FUNCTION
    END INTERFACE
    ! interfaces for StackArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_CopyAssign(DstObj, SrcObj)
            !^ To copy the source object to the destination object. <br>
            !   This is a deferred procedure inherited from the *Assignable* type.
            CLASS(StackArray(*)), INTENT(OUT)   :: DstObj   !! destination object
            CLASS(Assignable),    INTENT(IN)    :: SrcObj   !! source object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_Clone(SrcObj, DstObj)
            !^ To clone the StackArray object. <br>
            !  This is a deferred procedure inherited from the *Assignable* type.
            CLASS(StackArray(*)),           INTENT(IN)  :: SrcObj   !! source object
            CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            !^ To check whether LhsObj and RhsObj are equal or not. <br>
            !  This is a deferred procedure inherited from the *Assignable* type. <br>
            !  **Important Note**:  The *SetEQProc* procedure must be called before
            !  using this procedure. Otherwise, the procedure always returns false.
            CLASS(StackArray(*)), INTENT(IN)    :: LhsObj   !! an object
            CLASS(Assignable),    INTENT(IN)    :: RhsObj   !! another object
            tLogical                            :: Flag     !! true if both objects are equal
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_Free(Obj)
            !^ To free memory of the StackArray object. <br>
            !  This is a deferred procedure inherited from the *Assignable* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Obj  !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetTypeName(Obj) RESULT(Name)
            !^ To get the name of the StackArray type. <br>
            !  This is a deferred procedure inherited from the *Assignable* type.
            CLASS(StackArray(*)), INTENT(IN)    :: Obj      !! collection object
            tCharAlloc                          :: Name     !! type name of the object
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_CopyCollection(This, Other)
	        !^ To creates a new collection (This) with the same items
            !  as the given collection (Other). <br>
            !  This is a deferred procedure by the *BaseCollection* type.
            CLASS(StackArray(*)),  INTENT(INOUT)    :: This     !! collection object to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: Other    !! collection object to be copied
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_ClearItems(Collection)
	        !^ To remove all of the items from the collection. <br>
            ! This is a deferred procedure by the *BaseCollection* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_Destroy(Collection)
	        !^ To destruct the collection. <br>
            ! This is a deferred procedure by the *BaseCollection* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetSize(Collection) RESULT(Size)
            !^ To get the collection size (number of items in the collection). <br>
            !  This is a deferred procedure inherited from the *BaseCollection* type.
            CLASS(StackArray(*)), INTENT(IN)    :: Collection   !! collection object
            tIndex                              :: Size         !! number of items
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Move2FirstElm(Collection, Item) RESULT(IsEmpty)
	        !^ To move to the top (last) element in the collection. <br>
            ! This is a deferred procedure by the *BaseIterable* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the first element as output if requested (and available)
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            !> a flag indicating whether the collection contains no element or not <br>
            ! - true if the collection is empty. <br>
            ! - otherwise the first element is available.
            tLogical                            :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Move2NextElm(Collection, Item) RESULT(IsTheEnd)
	        !^ To move (backward) to the next element in the collection. <br>
            !  This is a deferred procedure by the *BaseIterable* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the next element as output if requested (and available)
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            !> a flag indicating whether the move to the end of the collection occurs or not <br>
            ! - true if next element is NOT available. <br>
            ! - otherwise next element is available.
            tLogical                            :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_AddElm(Collection, Item)
	        !^ To insert the specified item at the top (end) of the collection. <br>
            !  This is a deferred procedure by the *BaseIterable* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the item to be added to the collection
            TYPE(*),              INTENT(IN)    :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE StackArray_DelElm(Collection)
	        !^ To delete an item from a collection.  This procedure is intended to be
            !  used in conjunction with the *StartFirst* and *MoveForward* methods.
            !  Therefore, after the call to one of the methods and then calling this
            !  procedure will result in a removal of the current item of the iteration
            !  (i.e. the same item that can be retrieved via those methods). <br>
            !  This is a deferred procedure by the *BaseIterable* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseDynArr Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetFirst(Collection) RESULT(First)
            !^ To get an index pointing to the first item.
            !  This is a deferred procedure by the *BaseDynArr* type.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            tIndex                              :: First        !! first index
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by StackArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_Pop(Collection, Item) RESULT(Flag)
	        !^ To get and remove the top (last) item of the collection.  Also, return
            ! a flag indicating whether the item is successfully removed.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the item to be removed from the collection
            TYPE(*),              INTENT(INOUT) :: Item
            !> flag indicating whether the item is successfully removed. <br>
            ! - true if the collection is NOT empty.
            ! - false if the collection is empty.
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_PeekTop(Collection, Item) RESULT(Flag)
	        !^ To get the top (last) item (without removing it from the collection).
            !  Also, return a flag indicating whether the item is available.
            CLASS(StackArray(*)), INTENT(IN)    :: Collection   !! collection object
            !% the item to be retrieved from the collection
            TYPE(*),              INTENT(INOUT) :: Item
            !> flag indicating whether the item is available. <br>
            ! - true if the collection is NOT empty.
            ! - false if the collection is empty.
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_ToArray(Collection, Items) RESULT(Flag)
	        !^ To get and remove all items from the collection.  Also, return
            ! a flag indicating whether the items are successfully removed.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the items to be retrieved and removed from the collection
            TYPE(*),              INTENT(INOUT) :: Items(:)
            !> flag indicating whether the items are successfully removed. <br>
            ! - true if the items are successfully removed.
            ! - false if the items are NOT successfully removed.
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION StackArray_GetAll(Collection, Items) RESULT(Flag)
	        !^ To get all items (without removing them) from the collection.  Also,
            ! return a flag indicating whether the items are available.
            CLASS(StackArray(*)), INTENT(INOUT) :: Collection   !! collection object
            !% the items to be retrieved from the collection
            TYPE(*),              INTENT(INOUT) :: Items(:)
            !> flag indicating whether the items are successfully retrieved. <br>
            ! - true if the items are available.
            ! - false if the items are NOT available.
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for QueueArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_CopyAssign(DstObj, SrcObj)
            CLASS(QueueArray(*)), INTENT(OUT)   :: DstObj
            CLASS(Assignable),    INTENT(IN)    :: SrcObj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_Clone(SrcObj, DstObj)
            CLASS(QueueArray(*)),           INTENT(IN)  :: SrcObj
            CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)
            CLASS(QueueArray(*)), INTENT(IN)    :: LhsObj
            CLASS(Assignable),    INTENT(IN)    :: RhsObj
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_Free(Obj)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Obj
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetTypeName(Obj) RESULT(Name)
            CLASS(QueueArray(*)), INTENT(IN)    :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_CopyCollection(This, Other)
            CLASS(QueueArray(*)),  INTENT(INOUT)    :: This
            CLASS(BaseCollection), INTENT(INOUT)    :: Other
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_ClearItems(Collection)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_Destroy(Collection)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetSize(Collection) RESULT(Size)
            CLASS(QueueArray(*)), INTENT(IN)    :: Collection
            tIndex                              :: Size
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Move2FirstElm(Collection, Item) RESULT(IsEmpty)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            tLogical                            :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Move2NextElm(Collection, Item) RESULT(IsTheEnd)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            tLogical                            :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_AddElm(Collection, Item)
            CLASS(QueueArray(*)), INTENT(INOUT)   :: Collection
            TYPE(*),              INTENT(IN)      :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE QueueArray_DelElm(Collection)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseDynArr Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetFirst(Collection) RESULT(First)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            tIndex                              :: First
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by QueueArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_Dequeue(Collection, Item) RESULT(Flag)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_PeekFirst(Collection, Item) RESULT(Flag)
            CLASS(QueueArray(*)), INTENT(IN)    :: Collection
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_ToArray(Collection, Items) RESULT(Flag)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            TYPE(*),              INTENT(INOUT) :: Items(:)
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION QueueArray_GetAll(Collection, Items) RESULT(Flag)
            CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
            TYPE(*),              INTENT(INOUT) :: Items(:)
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for DequeArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_GetTypeName(Obj) RESULT(Name)
            CLASS(DequeArray(*)), INTENT(IN)    :: Obj
            tCharAlloc                          :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----        Deferred Procedures from BaseIterable Type         -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_Move2LastElm(Collection, Item) RESULT(IsEmpty)
            CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            tLogical                            :: IsEmpty
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_Move2PrevElm(Collection, Item) RESULT(IsTheEnd)
            CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
            TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
            tLogical                            :: IsTheEnd
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by DequeArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE DequeArray_AddFirst(Collection, Item)
            CLASS(DequeArray(*)), INTENT(INOUT)   :: Collection
            TYPE(*),              INTENT(IN)      :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE SUBROUTINE DequeArray_AddLast(Collection, Item)
            CLASS(DequeArray(*)), INTENT(INOUT)   :: Collection
            TYPE(*),              INTENT(IN)      :: Item
        END SUBROUTINE
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_RemoveFirst(Collection, Item) RESULT(Flag)
            CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_RemoveLast(Collection, Item) RESULT(Flag)
            CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION DequeArray_PeekLast(Collection, Item) RESULT(Flag)
            CLASS(DequeArray(*)), INTENT(IN)    :: Collection
            TYPE(*),              INTENT(INOUT) :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE
    ! interfaces for ListArray
    INTERFACE
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_GetTypeName(Obj) RESULT(Name)
            CLASS(ListArray(*)), INTENT(IN) :: Obj
            tCharAlloc                      :: Name
        END FUNCTION
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by ListArray Type            -----
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_AddAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
            tIndex,              INTENT(IN)     :: Index
            TYPE(*),             INTENT(IN)     :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_RemoveAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
            tIndex,              INTENT(IN)     :: Index
            TYPE(*),             INTENT(INOUT)  :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
        MODULE FUNCTION ListArray_PeekAt(Collection, Index, Item) RESULT(Flag)
            CLASS(ListArray(*)), INTENT(IN)     :: Collection
            tIndex,              INTENT(IN)     :: Index
            TYPE(*),             INTENT(INOUT)  :: Item
            tLogical                            :: Flag
        END FUNCTION
        ! ---------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseDynArr_CreateEmpty(Collection, InitCap, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !! To create an empty collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection   !! BaseDynArr object
    tIndex,               INTENT(IN)    :: InitCap      !! initial size of the collection
    tIndex,   OPTIONAL,   INTENT(IN)    :: IncSize      !! incremental size of the collection when it is full
    tLogical, OPTIONAL,   INTENT(IN)    :: Shrink
    !^ flag to shrink the collection capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

! FLOW
        
    ! first, check required input data
    IF (InitCap < 1) THEN
        CALL Handle_ErrLevel('BaseDynArr_CreateEmpty', ModName, ErrWarning, &
                             'Invalid InitCap (< 1).  Set the initial capacity to 16.')
        Capacity = Collection%IncSize
    ELSE
        Capacity = InitCap
    END IF
        
    ! then, allocate space for the items in the collection
    CALL MemAlloc(Collection%Items, Capacity)
        
    ! finally, check optional input data
    Collection%IncSize = 0
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0) Collection%IncSize = IncSize
    END IF
    IF (PRESENT(Shrink)) Collection%Shrink  =  Shrink
        
    RETURN

END SUBROUTINE BaseDynArr_CreateEmpty

!******************************************************************************

SUBROUTINE BaseDynArr_CreateByArray(Collection, N, Items, IncSize, Shrink)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection   !! BaseDynArr object
    tIndex,               INTENT(IN)    :: N            !! number of items
    TYPE(*),              INTENT(IN)    :: Items(:)     !! the items to be added to the collection
    tIndex,   OPTIONAL,   INTENT(IN)    :: IncSize      !! incremental size of the collection when it is full
    tLogical, OPTIONAL,   INTENT(IN)    :: Shrink
    !^ flag to shrink the collection capacity <br>
    ! - true if want to reduce capacity when the size is less than a quarter of the capacity. <br>
    ! - otherwise, the capacity stays the same. <br>
    ! - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, InitCap

! FLOW

    ! create empty stack
    InitCap = N*2   ! by default, doubling its capacity
    IF (PRESENT(IncSize)) THEN
        IF (IncSize > 0) InitCap = N + IncSize
    END IF
    CALL Collection%CreateEmpty(InitCap, IncSize, Shrink)
        
    ! add items to the collection
    DO I = 0, N-1
        CALL Collection%Insert(Items(I))
    END DO
       
    RETURN

END SUBROUTINE BaseDynArr_CreateByArray

!******************************************************************************

SUBROUTINE BaseDynArr_MemResize(Collection, NewSize)

!** PURPOSE OF THIS SUBROUTINE:
	!! To re-allocate the array of items of the collection and preserve its data

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection   !! BaseDynArr object
    tIndex,               INTENT(IN)    :: NewSize      !! new size of array
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                                          :: Offset
    tIndex                                          :: OldSize  ! original size of array
    tIndex                                          :: PSize    ! size of preserved data
    TYPE(GenStore(Collection%ValSize)), ALLOCATABLE :: Temp(:)  ! temporary buffer

!** FLOW:
    
    ! determine the original size
    OldSize = SIZE(Collection%Items)

    ! first, allocate the temporary array
    CALL MemAlloc(Temp, NewSize)

    ! determine the preserving size
    IF (NewSize >= OldSize) THEN
        PSize = OldSize
    ELSE
        PSize = NewSize
    END IF
    
    ! get offset
    Offset = Collection%Offset()
    
    ! *** copy items to the temporary buffer ***
    IF (Offset == 1_kIndex) THEN
        ! use whole array expression (typical for a stack)
        Temp(1:PSize) = Collection%Items(1:PSize)
    ELSE
        ! use do loop (typical for a deque or a queue)
        BLOCK
            tIndex  :: I, J
            ! get offset to the first item
            J = Offset
            DO I = 1_kIndex, PSize
                ! copy an item to the buffer
                Temp(I) = Collection%Items(J)
                ! update J and wrap around if necessary
                J = J + 1_kIndex
                IF (J > OldSize) J = 1_kIndex
            END DO
        END BLOCK
    END IF
        
    ! move data from the temporary array back to the array
    ! (this operation includes deallocate the array, reallocate it to
    !  the new size and copy data back)
    CALL MOVE_ALLOC(Temp, Collection%Items)

    RETURN

END SUBROUTINE BaseDynArr_MemResize

!**************************************************************************************

SUBROUTINE BaseDynArr_Growing(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To increase the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection   !! BaseDynArr object
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Capacity

!** FLOW:
    
    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! the collection has not yet been constructed.
        Capacity = 16
        ! allocate storage for the collections' items
        CALL MemAlloc(Collection%Items, Capacity)
    ELSE
        Capacity = SIZE(Collection%Items)
        IF (Collection%GetSize() == Capacity) THEN
            ! increase the collection's capacity
            IF (Collection%IncSize > 0) THEN
                Capacity = Capacity + Collection%IncSize
            ELSE
                Capacity = Capacity*2
            END IF
            ! check integer overflow
            IF (Capacity <= 0) Capacity = MaxCapacity
            ! resize the collections' items
            CALL Collection%Resize(Capacity)
        END IF
    END IF

    RETURN

END SUBROUTINE BaseDynArr_Growing

!**************************************************************************************

SUBROUTINE BaseDynArr_Shrinking(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!! To decrease the collection's capacity if needed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: Collection   !! BaseDynArr object
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: CurCap, CurSize

!** FLOW:
    
    IF (.NOT.ALLOCATED(Collection%Items)) THEN
        ! the collection has not yet been constructed so simply return.
        RETURN
    END IF
    IF (Collection%Shrink) THEN
        CurCap  = SIZE(Collection%Items)
        CurSize = Collection%GetSize()
        IF ((CurSize >= 0).AND.(CurSize <= CurCap/4)) THEN
            ! halves the collection's capacity
            CurCap = CurCap/2
            ! check if the capacity is zero or not
            IF (CurCap <= 0) CurCap = 1
            ! resize the collections' items
            CALL Collection%Resize(CurCap)
        END IF
    END IF

    RETURN

END SUBROUTINE BaseDynArr_Shrinking

!**************************************************************************************

SUBROUTINE BaseDynArr_CopyMembers(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy components from Other to This.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDynArr(*)), INTENT(INOUT) :: This     ! a destination
    CLASS(BaseDynArr(*)), INTENT(IN)    :: Other    ! a source

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Cap

! FLOW

    IF (This%ValSize == Other%ValSize) THEN
        This%IncSize = Other%IncSize
        This%Shrink  = Other%Shrink
        IF (ALLOCATED(Other%Items)) THEN
            Cap = SIZE(Other%Items)
            CALL MemAlloc(This%Items, Cap)
            This%Items(1:Cap) = Other%Items(1:Cap)
        END IF
    ELSE
        CALL Handle_ErrLevel('BaseDynArr_CopyMembers', ModName, ErrSevere, &
                             'The "ValSize" components must have the same value.')
    END IF

    RETURN

END SUBROUTINE BaseDynArr_CopyMembers

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE DequeArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(DequeArray(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE DequeArray_Finalize

!******************************************************************************
SUBROUTINE ListArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListArray(*)), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE ListArray_Finalize

!******************************************************************************

SUBROUTINE QueueArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(QueueArray(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE QueueArray_Finalize

!******************************************************************************

SUBROUTINE StackArray_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(StackArray(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE StackArray_Finalize

!******************************************************************************

END MODULE Class_DynamicArrays

!******************************************************************************

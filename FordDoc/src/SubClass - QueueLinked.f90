
SUBMODULE (Class_LinkedLists) SubClass_QueueLinked

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *QueueLinked* type that represents
!   a first-in-first-out (FIFO) queue with a doubly-linked list data structure
!   based on an intrusive linked list implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF, C_NULL_PTR
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_QueueLinked'
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

MODULE SUBROUTINE QueueLinked_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !   This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(OUT)  :: DstObj   !! destination object
    CLASS(Assignable),     INTENT(IN)   :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    CLASS IS (QueueLinked(*))
        IF (DstObj%GetTypeName() /= SrcObj%GetTypeName()) THEN
            CALL Handle_ErrLevel('QueueLinked_CopyAssign', ModName, ErrSevere, &
                                 'Types of source and destination objects must be the same.')
        ELSEIF (DstObj%ValSize /= SrcObj%ValSize) THEN
            CALL Handle_ErrLevel('QueueLinked_CopyAssign', ModName, ErrSevere, &
                                 'The "ValSize" components must have the same value.')
        ELSE
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the source object.
            BLOCK
                ! block variables
                CLASS(DoublyLinkedNode), POINTER    :: CurrNode
                CLASS(DoublyLinkedNode), POINTER    :: NextNode
                ! start iteration from the head (first node) of the source
                CurrNode => SrcObj%WrkLst%GetHead()
                DO WHILE (ASSOCIATED(CurrNode))
                    ! append the current node to the tail of the destination
                    CALL DstObj%WrkLst%AddLast(CurrNode)
                    ! move to the next iteration of the source
                    NextNode => CurrNode%GetNext()
                    CurrNode => NextNode
                END DO
                NULLIFY(CurrNode, NextNode)
            END BLOCK
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('QueueLinked_CopyAssign', ModName, ErrSevere, &
                             'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE QueueLinked_CopyAssign

!**************************************************************************************

MODULE SUBROUTINE QueueLinked_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the QueueLinked object.   This is a deferred
    !   procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)),          INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the QueueLinked object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    CLASS IS (QueueLinked(*))
        ! implementation note:  we cannot use the iteration methods here
        !   due to the intent of the source object.
        BLOCK
            ! block variables
            CLASS(DoublyLinkedNode), POINTER    :: CurrNode
            CLASS(DoublyLinkedNode), POINTER    :: NextNode
            CLASS(DoublyLinkedNode), POINTER    :: NewNode
            ! start iteration from the head (first node) of the source
            CurrNode => SrcObj%WrkLst%GetHead()
            DO WHILE (ASSOCIATED(CurrNode))
                ALLOCATE(NewNode, SOURCE=CurrNode)
                ! append the new node to the tail of the destination
                CALL DstObj%WrkLst%AddLast(NewNode)
                NULLIFY(NewNode)
                ! move to the next iteration of the source
                NextNode => CurrNode%GetNext()
                CurrNode => NextNode
            END DO
            NULLIFY(CurrNode, NextNode)
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE QueueLinked_Clone

!**************************************************************************************

MODULE FUNCTION QueueLinked_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  **Important Note**:  The *SetEQProc* procedure must be called before
    !   using this procedure. Otherwise, the procedure always returns false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(IN)   :: LhsObj       !! an object
    CLASS(Assignable),     INTENT(IN)   :: RhsObj       !! another object
    tLogical                            :: Flag         !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    CLASS IS (QueueLinked(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%GetTypeName() /= RhsObj%GetTypeName()) RETURN
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (ASSOCIATED(LhsObj%EqualTo).AND.(.NOT.LhsObj%IsEmpty())) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                CLASS(DoublyLinkedNode), POINTER    :: LhsNode, LhsNext
                CLASS(DoublyLinkedNode), POINTER    :: RhsNode, RhsNext
                tLogical                            :: ReturnNow
                ReturnNow = FalseVal
                ! start iteration
                LhsNode => LhsObj%WrkLst%GetHead()
                RhsNode => RhsObj%WrkLst%GetHead()
                Loop: DO WHILE (ASSOCIATED(LhsNode).AND.ASSOCIATED(RhsNode))
                    LhsNext => LhsNode%GetNext()
                    RhsNext => RhsNode%GetNext()
                    ! check item equality
                    SELECT TYPE (LhsNode)
                    TYPE IS (LinkedNode(*))
                        SELECT TYPE (RhsNode)
                        TYPE IS (LinkedNode(*))
                            IF (.NOT.LhsObj%EqualTo(LhsNode%Store, RhsNode%Store)) THEN
                                ReturnNow = TrueVal
                                EXIT Loop
                            END IF
                        END SELECT
                    END SELECT
                    ! move to the next iteration
                    LhsNode => LhsNext
                    RhsNode => RhsNext
                END DO Loop
                NULLIFY(LhsNode, RhsNode)
                NULLIFY(LhsNext, RhsNext)
                IF (ReturnNow) RETURN
            END BLOCK
        ELSE
            RETURN
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION QueueLinked_IsEqualTo

!******************************************************************************

MODULE SUBROUTINE QueueLinked_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the QueueLinked object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Clear()

    RETURN

END SUBROUTINE QueueLinked_Free

!******************************************************************************

MODULE FUNCTION QueueLinked_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the QueueLinked type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(IN)   :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'QueueLinked'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION QueueLinked_GetTypeName

! ---------------------------------------------------------------------
! -----      Deferred Procedures from BaseCollection Type         -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE QueueLinked_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    ! This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    CLASS IS (QueueLinked(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            BLOCK
                ! block variables
                tLogical                            :: IsTheEnd
                CLASS(DoublyLinkedNode), POINTER    :: CurrNode
                CLASS(DoublyLinkedNode), POINTER    :: NewNode
                ! start iteration from the head (first node) of the source
                IsTheEnd = Other%WrkLst%StartFirst(CurrNode)
                DO WHILE (.NOT.IsTheEnd)
                    ALLOCATE(NewNode, SOURCE=CurrNode)
                    ! append the new node to the tail of the destination
                    CALL This%WrkLst%AddLast(NewNode)
                    NULLIFY(NewNode)
                    ! move to the next iteration of the source
                    IsTheEnd = Other%WrkLst%MoveForward(CurrNode)
                END DO
                NULLIFY(CurrNode)
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('QueueLinked_CopyCollection', ModName, ErrSevere, &
                                 'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS IS (BaseIterable(*))
        ! different types of collection
        IF (This%ValSize == Other%ValSize) THEN
            BLOCK
                ! block variables
                tLogical                :: IsTheEnd
                tByte, TARGET           :: Item(This%ValSize)
                TYPE(C_PTR)             :: ItemPtr
                ! set pointer
                ItemPtr = C_LOC(Item)
                ! loop through the other collection
                IsTheEnd = Other%StartFirst(ItemPtr)
                DO WHILE (.NOT.IsTheEnd)
                    ! add an item to this collection
                    CALL This%Enqueue(ItemPtr)
                    IsTheEnd = Other%MoveForward(ItemPtr)
                END DO
                ItemPtr = C_NULL_PTR
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('QueueLinked_CopyCollection', ModName, ErrSevere, &
                                 'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('QueueLinked_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE QueueLinked_CopyCollection

!**************************************************************************************

MODULE SUBROUTINE QueueLinked_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg
    tLogical                            :: Success

! FLOW
    
    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN
    
    ! start removing from the tail node
    DO WHILE (.NOT.Collection%IsEmpty())
        ! remove the currently last node
        Success = Collection%WrkLst%RemoveLast(CurrNode)
        IF (ASSOCIATED(CurrNode)) THEN
            ! nullify pointer components of the node
            CALL CurrNode%FreePointers()
            ! deallocate the node
            DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('QueueLinked_ClearItems', ModName, AllocMsg, AllocStat)
        END IF
    END DO
    
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE QueueLinked_ClearItems

!**************************************************************************************

MODULE SUBROUTINE QueueLinked_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    CALL Collection%Clear()

    RETURN

END SUBROUTINE QueueLinked_Destroy

!**************************************************************************************

MODULE FUNCTION QueueLinked_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(IN)   :: Collection
    tIndex                              :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkLst%GetSize()

    RETURN

END FUNCTION QueueLinked_GetSize

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION QueueLinked_Move2FirstElm(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the front (first) element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
    !% the first element as output if requested (and available)
    TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
    !> a flag indicating whether the collection contains no element or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the first element is available.
    tLogical                                :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkLst%StartFirst(CurrNode)
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
        IsEmpty = Collection%WrkLst%StartFirst()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = 1

    RETURN

END FUNCTION QueueLinked_Move2FirstElm

!**************************************************************************************

MODULE FUNCTION QueueLinked_Move2NextElm(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move (forward) to the next element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
    !% the next element as output if requested (and available)
    TYPE(*), OPTIONAL,     INTENT(INOUT)    :: Item
    !> a flag indicating whether the move to the end of the collection occurs or not <br>
    ! - true if next element is NOT available. <br>
    ! - otherwise next element is available.
    tLogical                                :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Item)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! move to the next iteration
            IsTheEnd = Collection%WrkLst%MoveForward(CurrNode)
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
        IsTheEnd = Collection%WrkLst%MoveForward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = 1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION QueueLinked_Move2NextElm

!**************************************************************************************

MODULE SUBROUTINE QueueLinked_AddElm(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the collection.
    !  This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be added to the collection
    TYPE(*),               INTENT(IN)       :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode(Collection%ValSize)), POINTER   :: NewNode
    tInteger                                        :: AllocStat
    tCharLen(MsgLen)                                :: AllocMsg

! FLOW
        
    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('QueueLinked_AddElm', ModName, AllocMsg, AllocStat)
        
    ! set item to the new node
    CALL NewNode%Store%Set(Item)
        
    ! append the new node to the tail
    CALL Collection%WrkLst%AddLast(NewNode)
        
    ! free up pointer
    NULLIFY(NewNode)

    RETURN

END SUBROUTINE QueueLinked_AddElm

!**************************************************************************************

MODULE SUBROUTINE QueueLinked_DelElm(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from a collection.  This procedure is intended
    !   to be used in conjunction with the *StartFirst* and *MoveForward*
    !   methods.  Therefore, after the call to one of the methods and then
    !   calling this procedure will result in a removal of the current item
    !   of the iteration (i.e. the same item that can be retrieved via those
    !   methods).
    !  This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg
    tLogical                            :: IsTheEnd

! FLOW

    ! get the cursor node
    CurrNode => Collection%WrkLst%GetCursor()
    
    ! check if the node is associated
    IF (ASSOCIATED(CurrNode)) THEN
        ! reset cursor
        IF (Collection%Dir == 1) THEN
            ! forward iteration so move cursor backward
            IsTheEnd = Collection%WrkLst%MoveBackward()
        ELSE
            ! backward iteration so move cursor forward
            IsTheEnd = Collection%WrkLst%MoveForward()
        END IF
        ! check if remove the node successfully or not
        IF (Collection%WrkLst%RemoveNode(CurrNode)) THEN
            ! nullify pointer components of the node
            CALL CurrNode%FreePointers()
            ! deallocate the node
            DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('QueueLinked_DelElm', ModName, AllocMsg, AllocStat)
        END IF
    END IF
    
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE QueueLinked_DelElm

! ---------------------------------------------------------------------
! -----                     Queue Procedures                      -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE QueueLinked_CreateByArray(Collection, N, Items)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of items.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection   !! QueueLinked object
    tIndex,                INTENT(IN)       :: N            !! number of items
    TYPE(*),               INTENT(IN)       :: Items(:)     !! the items to be added to the collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    ! add items to the collection
    DO I = 0, N-1
        CALL Collection%Enqueue(Items(I))
    END DO
       
    RETURN

END SUBROUTINE QueueLinked_CreateByArray

!******************************************************************************

MODULE FUNCTION QueueLinked_Dequeue(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the front (first) item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
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
        CALL Handle_ErrDealloc('QueueLinked_Dequeue', ModName, AllocMsg, AllocStat)
    ELSE
        ! set flag
        Flag = FalseVal
    END IF

    NULLIFY(FirstNode)

    RETURN

END FUNCTION QueueLinked_Dequeue

!**************************************************************************************

MODULE FUNCTION QueueLinked_PeekFirst(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the front (first) item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(IN)       :: Collection
    !% the item to be retrieved from the collection
    TYPE(*),               INTENT(INOUT)    :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER   :: FirstNode

! FLOW
        
    ! get the first node
    FirstNode => Collection%WrkLst%GetHead()
    
    IF (ASSOCIATED(FirstNode)) THEN
        ! get the item stored in the node
        SELECT TYPE (FirstNode)
        TYPE IS (LinkedNode(*))
            CALL FirstNode%Store%Get(Item)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    NULLIFY(FirstNode)

    RETURN

END FUNCTION QueueLinked_PeekFirst

!**************************************************************************************

MODULE FUNCTION QueueLinked_ToArray(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the collection.  Also, return
    ! a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be removed from the collection
    TYPE(*),               INTENT(INOUT)    :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Flag = Collection%GetAll(Items)
    
    ! remove all items
    CALL Collection%Clear()

    RETURN

END FUNCTION QueueLinked_ToArray

!**************************************************************************************

MODULE FUNCTION QueueLinked_GetAll(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the collection.  Also,
    ! return a flag indicating whether the items are available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueLinked object
    CLASS(QueueLinked(*)), INTENT(INOUT)    :: Collection
    !% the item to be removed from the collection
    TYPE(*),               INTENT(INOUT)    :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ArrSize
    tLogical    :: IsTheEnd

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! initialize local variables
    ArrSize = SIZE(Items, KIND=kIndex)
    I = 1
    
    ! start forward iteration (from the first item)
    IsTheEnd = Collection%StartFirst(Items(I))
    DO WHILE ((I < ArrSize).AND.(.NOT.IsTheEnd))
        ! update I
        I = I + 1_kIndex
        ! move to the next iteration
        IsTheEnd = Collection%MoveForward(Items(I))
    END DO

    RETURN

END FUNCTION QueueLinked_GetAll

!**************************************************************************************

END SUBMODULE SubClass_QueueLinked

!******************************************************************************

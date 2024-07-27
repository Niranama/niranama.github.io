
SUBMODULE (Class_DynamicArrays) SubClass_QueueArray

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *QueueArray* type that represents
!   a first-in-first-out (FIFO) queue with a resizable-array implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_QueueArray'
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

MODULE SUBROUTINE QueueArray_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !   This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(OUT)   :: DstObj   !! destination object
    CLASS(Assignable),    INTENT(IN)    :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    CLASS IS (QueueArray(*))
        IF (DstObj%GetTypeName() /= SrcObj%GetTypeName()) THEN
            CALL Handle_ErrLevel('QueueArray_CopyAssign', ModName, ErrSevere, &
                                 'Types of source and destination objects must be the same.')
        ELSEIF (DstObj%ValSize /= SrcObj%ValSize) THEN
            CALL Handle_ErrLevel('QueueArray_CopyAssign', ModName, ErrSevere, &
                                 'The "ValSize" components must have the same value.')
        ELSE
            CALL DstObj%CopyMembers(SrcObj)
            DstObj%First  = SrcObj%First
            DstObj%Last   = SrcObj%Last
            DstObj%Cursor = SrcObj%Cursor
            DstObj%Size   = SrcObj%Size
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('QueueArray_CopyAssign', ModName, ErrSevere, &
                             'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE QueueArray_CopyAssign

!**************************************************************************************

MODULE SUBROUTINE QueueArray_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the QueueArray object.   This is a deferred
    !   procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)),           INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the QueueArray object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    CLASS IS (QueueArray(*))
        CALL DstObj%CopyMembers(SrcObj)
        DstObj%First  = SrcObj%First
        DstObj%Last   = SrcObj%Last
        DstObj%Cursor = SrcObj%Cursor
        DstObj%Size   = SrcObj%Size
    END SELECT

    RETURN

END SUBROUTINE QueueArray_Clone

!**************************************************************************************

MODULE FUNCTION QueueArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  **Important Note**:  The *SetEQProc* procedure must be called before
    !   using this procedure. Otherwise, the procedure always returns false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(IN)    :: LhsObj       !! an object
    CLASS(Assignable),    INTENT(IN)    :: RhsObj       !! another object
    tLogical                            :: Flag         !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    CLASS IS (QueueArray(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%GetTypeName() /= RhsObj%GetTypeName()) RETURN
        IF (LhsObj%IncSize /= RhsObj%IncSize) RETURN
        IF (LhsObj%Shrink .NEQV. RhsObj%Shrink) RETURN
        IF (LhsObj%First /= RhsObj%First) RETURN
        IF (LhsObj%Last /= RhsObj%Last) RETURN
        IF (LhsObj%Size /= RhsObj%Size) RETURN
        ! IF (LhsObj%Cursor /= RhsObj%Cursor) RETURN
        IF (ASSOCIATED(LhsObj%EqualTo).AND.(.NOT.LhsObj%IsEmpty())) THEN
            BLOCK
                tIndex  :: I, Capacity
                I = LhsObj%First
                Capacity = SIZE(LhsObj%Items, KIND=kIndex)
                DO
                    IF (LhsObj%EqualTo(LhsObj%Items(I), RhsObj%Items(I))) RETURN
                    I = I + 1_kIndex
                    IF (I > Capacity) I = 1_kIndex
                    IF (I == LhsObj%Last) EXIT
                END DO
            END BLOCK
        ELSE
            RETURN
        END IF
        Flag = TrueVal
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION QueueArray_IsEqualTo

!******************************************************************************

MODULE SUBROUTINE QueueArray_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the QueueArray object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%Items))    DEALLOCATE(Obj%Items)
    IF (ASSOCIATED(Obj%EqualTo)) NULLIFY(Obj%EqualTo)

    RETURN

END SUBROUTINE QueueArray_Free

!******************************************************************************

MODULE FUNCTION QueueArray_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the QueueArray type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'QueueArray'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION QueueArray_GetTypeName

! ---------------------------------------------------------------------
! -----      Deferred Procedures from BaseCollection Type         -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE QueueArray_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    ! This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(QueueArray(*)),  INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    CLASS IS (QueueArray(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            CALL This%CopyMembers(Other)
            This%First  = Other%First
            This%Last   = Other%Last
            This%Cursor = Other%Cursor
            This%Size   = Other%Size
        ELSE
            CALL Handle_ErrLevel('QueueArray_CopyCollection', ModName, ErrSevere, &
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
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('QueueArray_CopyCollection', ModName, ErrSevere, &
                                 'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('QueueArray_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE QueueArray_CopyCollection

!**************************************************************************************

MODULE SUBROUTINE QueueArray_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! reset pointer indices
    Collection%First  = 1_kIndex
    Collection%Last   = 1_kIndex
    Collection%Cursor = 0_kIndex
    Collection%Size   = 0_kIndex
    Collection%Dir    = 0

    ! shrink the collection
    CALL Collection%Shrinking()

    RETURN

END SUBROUTINE QueueArray_ClearItems

!**************************************************************************************

MODULE SUBROUTINE QueueArray_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! remove all items
    CALL Collection%Clear()
    ! free storage memory
    CALL Collection%MemFree()

    RETURN

END SUBROUTINE QueueArray_Destroy

!**************************************************************************************

MODULE FUNCTION QueueArray_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(IN)    :: Collection
    tIndex                              :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Size

    RETURN

END FUNCTION QueueArray_GetSize

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION QueueArray_Move2FirstElm(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the front (first) element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the first element as output if requested (and available)
    TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
    !> a flag indicating whether the collection contains no element or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the first element is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor pointer
    Collection%Cursor = Collection%First

    ! set return flag
    IsEmpty = (Collection%Size == 0_kIndex)

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = 1

    IF ((.NOT.IsEmpty).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION QueueArray_Move2FirstElm

!**************************************************************************************

MODULE FUNCTION QueueArray_Move2NextElm(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move (backward) to the next element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the next element as output if requested (and available)
    TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
    !> a flag indicating whether the move to the end of the collection occurs or not <br>
    ! - true if next element is NOT available. <br>
    ! - otherwise next element is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if empty
    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! set cursor pointer and wrap around if necessary
    Collection%Cursor = Collection%Cursor + 1_kIndex
    IF (Collection%Cursor > SIZE(Collection%Items, KIND=kIndex)) Collection%Cursor = 1_kIndex

    ! set return flag
    IsTheEnd = (Collection%Cursor == Collection%Last)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = 1
    ELSE
        Collection%Dir = 0
    END IF

    IF ((.NOT.IsTheEnd).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION QueueArray_Move2NextElm

!**************************************************************************************

MODULE SUBROUTINE QueueArray_AddElm(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the collection.
    !  This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the item to be added to the collection
    TYPE(*),              INTENT(IN)    :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()
    
    ! then, add new item to the collection
    CALL Collection%Items(Collection%Last)%Set(Item)

    ! next, update pointer (wrap around if necessary) and size
    Collection%Last = Collection%Last + 1_kIndex
    IF (Collection%Last > SIZE(Collection%Items, KIND=kIndex)) Collection%Last = 1_kIndex
    Collection%Size = Collection%Size + 1_kIndex

    RETURN

END SUBROUTINE QueueArray_AddElm

!**************************************************************************************

MODULE SUBROUTINE QueueArray_DelElm(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from a collection.  This procedure is intended
    !   to be used in conjunction with the *StartFirst* and *MoveForward*
    !   methods.  Therefore, after the call to one of the methods and then
    !   calling this procedure will result in a removal of the current item
    !   of the iteration (i.e. the same item that can be retrieved via those
    !   methods).
    !  This is a deferred procedure by the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! return immediately if the collection is empty
    IF (Collection%IsEmpty()) RETURN
    
    ! return immediately if the cursor is not in a valid position
    IF (Collection%Cursor == 0_kIndex) RETURN
    IF (Collection%First < Collection%Last) THEN
        IF ((Collection%Cursor < Collection%First).OR. &
            (Collection%Cursor >= Collection%Last)) RETURN
    ELSE
        IF ((Collection%Cursor < Collection%First).AND. &
            (Collection%Cursor >= Collection%Last)) RETURN
    END IF
    
    ! the cursor is in a valid position
    Delete_Block: BLOCK
        ! block variables
        tIndex  :: I, Cap, Last
        ! initialize block variables
        I = Collection%Cursor
        Cap = SIZE(Collection%Items, KIND=kIndex)
        Last = Collection%Last - 1_kIndex
        IF (Last == 0_kIndex) Last = Cap
        ! move items as necessary
        DO
            IF (I == Last) EXIT
            IF (I == Cap) THEN
                Collection%Items(Cap) = Collection%Items(1_kIndex)
                I = 1_kIndex
            ELSE
                Collection%Items(I) = Collection%Items(I+1_kIndex)
                I = I + 1_kIndex
            END IF
        END DO
        ! update the last pointer and size
        Collection%Last = Last
        Collection%Size = Collection%Size - 1_kIndex
        ! shrink the collection if necessary
        CALL Collection%Shrinking()
        ! reset cursor
        Cap = SIZE(Collection%Items, KIND=kIndex)
        ! reset cursor
        IF (Collection%Dir == 1) THEN
            ! forward iteration so move cursor backward
            Collection%Cursor = Collection%Cursor - 1_kIndex
            IF (Collection%Cursor == 0_kIndex) Collection%Cursor = Cap
        ELSE
            ! backward iteration so move cursor forward
            Collection%Cursor = Collection%Cursor + 1_kIndex
            IF (Collection%Cursor == Cap) Collection%Cursor = 0_kIndex
        END IF
    END BLOCK Delete_Block

    RETURN

END SUBROUTINE QueueArray_DelElm

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseDynArr Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION QueueArray_GetFirst(Collection) RESULT(First)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get an index pointing to the first item.
    !  This is a deferred procedure by the *BaseDynArr* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    tIndex                              :: First

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set index to the first item
    First = Collection%First
    
    ! reset index pointers of the collections
    Collection%First = 1_kIndex
    Collection%Last  = Collection%Size + 1_kIndex

    RETURN

END FUNCTION QueueArray_GetFirst

! ---------------------------------------------------------------------
! -----                     Queue Procedures                      -----
! ---------------------------------------------------------------------

MODULE FUNCTION QueueArray_Dequeue(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the front (first) item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get the first item
    CALL Collection%Items(Collection%First)%Get(Item)

    ! update the first pointer (wrap around if necessary) and size
    Collection%First = Collection%First + 1_kIndex
    IF (Collection%First > SIZE(Collection%Items, KIND=kIndex)) Collection%First = 1_kIndex
    Collection%Size = Collection%Size - 1_kIndex

    ! shrink the collection if necessary
    CALL Collection%Shrinking()

    RETURN

END FUNCTION QueueArray_Dequeue

!**************************************************************************************

MODULE FUNCTION QueueArray_PeekFirst(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the front (first) item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(IN)    :: Collection
    !% the item to be retrieved from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get the top item
    CALL Collection%Items(Collection%First)%Get(Item)

    RETURN

END FUNCTION QueueArray_PeekFirst

!**************************************************************************************

MODULE FUNCTION QueueArray_ToArray(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the collection.  Also, return
    ! a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT) :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! retrieve all items
    Flag = Collection%GetAll(Items)
    
    ! remove all items
    CALL Collection%Clear()

END FUNCTION QueueArray_ToArray

!**************************************************************************************

MODULE FUNCTION QueueArray_GetAll(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the collection.  Also,
    ! return a flag indicating whether the items are available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% QueueArray object
    CLASS(QueueArray(*)), INTENT(INOUT) :: Collection
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT) :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, MinSize

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get items from the collection
    MinSize = MIN(Collection%Size, SIZE(Items, KIND=kIndex))
    J = Collection%First
    DO I = 1, MinSize
        CALL Collection%Items(J)%Get(Items(I))
        J = J + 1_kIndex
        IF (J > SIZE(Collection%Items, KIND=kIndex)) J = 1_kIndex
    END DO

    RETURN

END FUNCTION QueueArray_GetAll

!**************************************************************************************

END SUBMODULE SubClass_QueueArray

!******************************************************************************

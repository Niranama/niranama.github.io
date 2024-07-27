
SUBMODULE (Class_DynamicArrays) SubClass_StackArray

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *StackArray* type that represents
!   a last-in-first-out (LIFO) stack with a resizable-array implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_StackArray'
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

MODULE SUBROUTINE StackArray_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !   This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(OUT)   :: DstObj   !! destination object
    CLASS(Assignable),    INTENT(IN)    :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    TYPE IS (StackArray(*))
        IF (DstObj%ValSize == SrcObj%ValSize) THEN
            CALL DstObj%CopyMembers(SrcObj)
            DstObj%Top    = SrcObj%Top
            DstObj%Cursor = SrcObj%Cursor
        ELSE
            CALL Handle_ErrLevel('StackArray_CopyAssign', ModName, ErrSevere, &
                                 'The "ValSize" components must have the same value.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('StackArray_CopyAssign', ModName, ErrSevere, &
                             'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE StackArray_CopyAssign

!**************************************************************************************

MODULE SUBROUTINE StackArray_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the StackArray object.   This is a deferred
    !   procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)),           INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the StackArray object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    TYPE IS (StackArray(*))
        CALL DstObj%CopyMembers(SrcObj)
        DstObj%Top    = SrcObj%Top
        DstObj%Cursor = SrcObj%Cursor
    END SELECT

    RETURN

END SUBROUTINE StackArray_Clone

!**************************************************************************************

MODULE FUNCTION StackArray_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type. <br>
    !  **Important Note**:  The *SetEQProc* procedure must be called before
    !  using this procedure. Otherwise, the procedure always returns false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(IN)    :: LhsObj   !! an object
    CLASS(Assignable),    INTENT(IN)    :: RhsObj   !! another object
    tLogical                            :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (StackArray(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%IncSize /= RhsObj%IncSize) RETURN
        IF (LhsObj%Shrink .NEQV. RhsObj%Shrink) RETURN
        IF (LhsObj%Top /= RhsObj%Top) RETURN
        ! IF (LhsObj%Cursor /= RhsObj%Cursor) RETURN
        IF (ASSOCIATED(LhsObj%EqualTo).AND.(.NOT.LhsObj%IsEmpty())) THEN
            BLOCK
                tIndex  :: I
                DO I = 1, LhsObj%Top
                    IF (LhsObj%EqualTo(LhsObj%Items(I), RhsObj%Items(I))) RETURN
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

END FUNCTION StackArray_IsEqualTo

!******************************************************************************

MODULE SUBROUTINE StackArray_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the StackArray object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(INOUT) :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%Items))    DEALLOCATE(Obj%Items)
    IF (ASSOCIATED(Obj%EqualTo)) NULLIFY(Obj%EqualTo)

    RETURN

END SUBROUTINE StackArray_Free

!******************************************************************************

MODULE FUNCTION StackArray_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the StackArray type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'StackArray'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION StackArray_GetTypeName

! ---------------------------------------------------------------------
! -----      Deferred Procedures from BaseCollection Type         -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE StackArray_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* type. <br>
    !  *Note*:  Other must be in the *BaseIterable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(StackArray(*)),  INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (StackArray(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            ! simply use the CopyAssign procedure
            This = Other
        ELSE
            CALL Handle_ErrLevel('StackArray_CopyCollection', ModName, ErrSevere, &
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
                    CALL This%Push(ItemPtr)
                    IsTheEnd = Other%MoveForward(ItemPtr)
                END DO
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('StackArray_CopyCollection', ModName, ErrSevere, &
                                 'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('StackArray_CopyCollection', ModName, ErrSevere, &
                             'Type of "Other" must be in the "BaseIterable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE StackArray_CopyCollection

!**************************************************************************************

MODULE SUBROUTINE StackArray_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! reset pointer indices
    Collection%Top    = 0_kIndex
    Collection%Cursor = Collection%Top

    ! shrink the collection
    CALL Collection%Shrinking()

    RETURN

END SUBROUTINE StackArray_ClearItems

!**************************************************************************************

MODULE SUBROUTINE StackArray_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! remove all items
    CALL Collection%Clear()
    ! free storage memory
    CALL Collection%MemFree()

    RETURN

END SUBROUTINE StackArray_Destroy

!**************************************************************************************

MODULE FUNCTION StackArray_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection. <br>
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(IN)    :: Collection
    tIndex                              :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Top
    ASSOCIATE(Dummy => Collection); END ASSOCIATE

    RETURN

END FUNCTION StackArray_GetSize

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION StackArray_Move2FirstElm(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the top (last) element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
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
    Collection%Cursor = Collection%Top

    ! set return flag
    IsEmpty = (Collection%Cursor < 1_kIndex)

    IF ((.NOT.IsEmpty).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION StackArray_Move2FirstElm

!**************************************************************************************

MODULE FUNCTION StackArray_Move2NextElm(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move (backward) to the next element in the collection. <br>
    ! This is a deferred procedure by the *BaseIterable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
    !% the next element as output if requested (and available)
    TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
    !> a flag indicating whether the move to the end of the collection occurs or not <br>
    ! - true if next element is NOT available. <br>
    ! - otherwise next element is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set cursor pointer
    Collection%Cursor = Collection%Cursor - 1_kIndex

    ! set return flag
    IsTheEnd = (Collection%Cursor < 1_kIndex)

    IF ((.NOT.IsTheEnd).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION StackArray_Move2NextElm

!**************************************************************************************

MODULE SUBROUTINE StackArray_AddElm(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the top (end) of the collection. <br>
    !  This is a deferred procedure by the *BaseIterable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
    !% the item to be added to the collection
    TYPE(*),              INTENT(IN)    :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()
    
    ! next, update pointer
    Collection%Top = Collection%Top + 1_kIndex
    
    ! then, add new item to the collection
    CALL Collection%Items(Collection%Top)%Set(Item)

    RETURN

END SUBROUTINE StackArray_AddElm

!**************************************************************************************

MODULE SUBROUTINE StackArray_DelElm(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete an item from a collection.  This procedure is intended to be
    !  used in conjunction with the *StartFirst* and *MoveForward* methods.
    !  Therefore, after the call to one of the methods and then calling this
    !  procedure will result in a removal of the current item of the iteration
    !  (i.e. the same item that can be retrieved via those methods). <br>
    !  This is a deferred procedure by the *BaseIterable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((Collection%Cursor >= 1_kIndex).AND.(Collection%Cursor <= Collection%Top)) THEN
        ! update the top pointer and size
        Collection%Top = Collection%Top - 1_kIndex

        ! move items
        IF ((Collection%Cursor <= Collection%Top)) THEN
            BLOCK
                tIndex  :: I
                DO I = Collection%Cursor, Collection%Top
                    Collection%Items(I) = Collection%Items(I+1)
                END DO
            END BLOCK
        END IF
        
        ! shrink the collection if necessary
        CALL Collection%Shrinking()
    END IF

    RETURN

END SUBROUTINE StackArray_DelElm

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseDynArr Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION StackArray_GetFirst(Collection) RESULT(First)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get an index pointing to the first item.
    !  This is a deferred procedure by the *BaseDynArr* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
    tIndex                              :: First

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    First = 1_kIndex
    ASSOCIATE(Dummy => Collection); END ASSOCIATE

    RETURN

END FUNCTION StackArray_GetFirst

! ---------------------------------------------------------------------
! -----                     Stack Procedures                      -----
! ---------------------------------------------------------------------

MODULE FUNCTION StackArray_Pop(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the top (last) item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
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

    ! get the top item
    CALL Collection%Items(Collection%Top)%Get(Item)

    ! update the top pointer and size
    Collection%Top = Collection%Top - 1_kIndex

    ! shrink the collection if necessary
    CALL Collection%Shrinking()

    RETURN

END FUNCTION StackArray_Pop

!**************************************************************************************

MODULE FUNCTION StackArray_PeekTop(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the top (last) item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(IN)    :: Collection
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
    CALL Collection%Items(Collection%Top)%Get(Item)

    RETURN

END FUNCTION StackArray_PeekTop

!**************************************************************************************

MODULE FUNCTION StackArray_ToArray(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove all items from the collection.  Also, return
    ! a flag indicating whether the items are successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
    !% the items to be retrieved and removed from the collection
    TYPE(*),              INTENT(INOUT) :: Items(:)
    !> flag indicating whether the items are successfully removed. <br>
    ! - true if the items are successfully removed.
    ! - false if the items are NOT successfully removed.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, MinSize

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get items from the collection
    MinSize = MIN(Collection%Top, SIZE(Items, KIND=kIndex))
    DO I = 1, MinSize
        CALL Collection%Items(I)%Get(Items(I))
    END DO
    
    ! remove all items from the collection
    CALL Collection%Clear()

    RETURN

END FUNCTION StackArray_ToArray

!**************************************************************************************

MODULE FUNCTION StackArray_GetAll(Collection, Items) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get all items (without removing them) from the collection.  Also,
    ! return a flag indicating whether the items are available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% StackArray object
    CLASS(StackArray(*)), INTENT(INOUT) :: Collection
    !% the items to be retrieved from the collection
    TYPE(*),              INTENT(INOUT) :: Items(:)
    !> flag indicating whether the items are successfully retrieved. <br>
    ! - true if the items are available.
    ! - false if the items are NOT available.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, MinSize

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! get items from the collection
    MinSize = MIN(Collection%Top, SIZE(Items, KIND=kIndex))
    DO I = 1, MinSize
        CALL Collection%Items(I)%Get(Items(I))
    END DO

    RETURN

END FUNCTION StackArray_GetAll

!**************************************************************************************

END SUBMODULE SubClass_StackArray

!******************************************************************************

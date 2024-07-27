
SUBMODULE (Class_DynamicArrays) SubClass_DequeArray

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *DequeArray* type that represents
!   a double-ended queue (deque) with a resizable-array implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_DequeArray'
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

MODULE FUNCTION DequeArray_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the DequeArray type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(DequeArray(*)), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'DequeArray'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION DequeArray_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseIterable Type         -----
! ---------------------------------------------------------------------

MODULE FUNCTION DequeArray_Move2LastElm(Collection, Item) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last element in the collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
    !% the last element as output if requested (and available)
    TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
    !> a flag indicating whether the collection contains no element or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the last element is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set return flag
    IsEmpty = (Collection%Size == 0_kIndex)

    ! simply return if empty
    IF (IsEmpty) RETURN

    ! set cursor pointer (note: Collection%Last points to the next to last item)
    ! and wrap around if necessary
    Collection%Cursor = Collection%Last - 1_kIndex
    IF (Collection%Cursor == 0_kIndex) Collection%Cursor = SIZE(Collection%Items, KIND=kIndex)

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = -1

    IF ((.NOT.IsEmpty).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION DequeArray_Move2LastElm

!**************************************************************************************

MODULE FUNCTION DequeArray_Move2PrevElm(Collection, Item) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous element in the collection. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
    !% the previous element as output if requested (and available)
    TYPE(*), OPTIONAL,    INTENT(INOUT) :: Item
    !> a flag indicating whether the move to the end of the collection occurs or not <br>
    ! - true if previous element is NOT available. <br>
    ! - otherwise previous element is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! simply return if empty
    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! set return flag
    IsTheEnd = (Collection%Cursor == Collection%First)

    ! set cursor pointer and wrap around if necessary
    Collection%Cursor = Collection%Cursor - 1_kIndex
    IF (Collection%Cursor == 0_kIndex) Collection%Cursor = SIZE(Collection%Items, KIND=kIndex)

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = -1
    ELSE
        Collection%Dir = 0
    END IF

    IF ((.NOT.IsTheEnd).AND.PRESENT(Item)) THEN
        ! get current item
        CALL Collection%Items(Collection%Cursor)%Get(Item)
    END IF

    RETURN

END FUNCTION DequeArray_Move2PrevElm

! ---------------------------------------------------------------------
! -----                     Deque Procedures                      -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE DequeArray_AddLast(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the end of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
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

END SUBROUTINE DequeArray_AddLast

!**************************************************************************************

MODULE SUBROUTINE DequeArray_AddFirst(Collection, Item)

!** PURPOSE OF THIS SUBROUTINE:
	!! To insert the specified item at the front of the collection.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
    !% the item to be added to the collection
    TYPE(*),              INTENT(IN)    :: Item

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()
    
    ! then, update pointer (wrap around if necessary) and size
    Collection%First = Collection%First - 1_kIndex
    IF (Collection%First == 0_kIndex) Collection%First = SIZE(Collection%Items, KIND=kIndex)
    Collection%Size = Collection%Size + 1_kIndex
    
    ! finally, add new item to the collection
    CALL Collection%Items(Collection%First)%Set(Item)

    RETURN

END SUBROUTINE DequeArray_AddFirst

!**************************************************************************************

MODULE FUNCTION DequeArray_RemoveLast(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the last item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Last

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! set index pointer to the last item (wrap around if necessary)
    Last = Collection%Last - 1_kIndex
    IF (Last == 0_kIndex) Last = SIZE(Collection%Items, KIND=kIndex)

    ! get the last item
    CALL Collection%Items(Last)%Get(Item)

    ! update the last pointer (wrap around if necessary) and size
    Collection%Last = Collection%Last - 1_kIndex
    IF (Collection%Last == 0_kIndex) Collection%Last = SIZE(Collection%Items, KIND=kIndex)
    Collection%Size = Collection%Size - 1_kIndex

    ! shrink the collection if necessary
    CALL Collection%Shrinking()

    RETURN

END FUNCTION DequeArray_RemoveLast

!**************************************************************************************

MODULE FUNCTION DequeArray_RemoveFirst(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the first item of the collection.  Also, return
    ! a flag indicating whether the item is successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(INOUT) :: Collection
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

END FUNCTION DequeArray_RemoveFirst

!**************************************************************************************

MODULE FUNCTION DequeArray_PeekLast(Collection, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the last item (without removing it from the collection).
    !  Also, return a flag indicating whether the item is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% DequeArray object
    CLASS(DequeArray(*)), INTENT(IN)    :: Collection
    !% the item to be retrieved from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the collection is NOT empty.
    ! - false if the collection is empty.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Last

! FLOW

    ! check whether the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
        RETURN
    ELSE
        Flag = TrueVal
    END IF

    ! set index pointer to the last item (wrap around if necessary)
    Last = Collection%Last - 1_kIndex
    IF (Last == 0_kIndex) Last = SIZE(Collection%Items, KIND=kIndex)

    ! get the last item
    CALL Collection%Items(Last)%Get(Item)

    RETURN

END FUNCTION DequeArray_PeekLast

!**************************************************************************************

END SUBMODULE SubClass_DequeArray

!******************************************************************************

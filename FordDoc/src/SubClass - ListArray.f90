
SUBMODULE (Class_DynamicArrays) SubClass_ListArray

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *ListArray* type that provides
!   add, remove and retrieve operations at a specified index where the index
!   must be between 1 and the collection size.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_ListArray'
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
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION IsIndexValid(Size, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To check whether the specified index is between 1
    !  and the collection size or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: Size     !! the collection size
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

!**************************************************************************************

FUNCTION ComputeTrueIndex(Collection, Index) RESULT(ID)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To compute the actual index of the collection's items
    !  based on the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(IN) :: Collection
    !% the specified index
    tIndex,              INTENT(IN) :: Index
    !% the actual index of the collection's items
    tIndex                          :: ID

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ID = Index + Collection%First - 1_kIndex
    IF (Collection%First >= Collection%Last) THEN
        BLOCK
            tIndex  :: Capacity
            Capacity = SIZE(Collection%Items, KIND=kIndex)
            IF (ID > Capacity) THEN
                ID = ID - Capacity
            END IF
        END BLOCK
    END IF

    RETURN

END FUNCTION ComputeTrueIndex

!**************************************************************************************

SUBROUTINE MoveItemsRight(Collection, Index)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move the collection's items to the right of 
    !  the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
    !% the one-based index into the collection's items
    tIndex,              INTENT(IN)     :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I, Cap

! FLOW

    ! initialize local variables
    I = Collection%Last - 1_kIndex
    Cap = SIZE(Collection%Items, KIND=kIndex)
    
    ! move items as necessary
    DO
        IF (I < Index) EXIT
        IF (I == 0_kIndex) THEN
            Collection%Items(1_kIndex) = Collection%Items(Cap)
            I = Cap - 1_kIndex
        ELSE
            Collection%Items(I+1_kIndex) = Collection%Items(I)
            I = I - 1_kIndex
        END IF
    END DO

    RETURN

END SUBROUTINE MoveItemsRight

!**************************************************************************************

SUBROUTINE MoveItemsLeft(Collection, Index)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move the collection's items to the left of 
    !  the specified index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
    !% the one-based index into the collection's items
    tIndex,              INTENT(IN)     :: Index

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I, Cap, Last

! FLOW

    ! initialize local variables
    I    = Index
    Cap  = SIZE(Collection%Items, KIND=kIndex)
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

    RETURN

END SUBROUTINE MoveItemsLeft

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

MODULE FUNCTION ListArray_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the ListArray type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListArray(*)), INTENT(IN) :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'ListArray'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION ListArray_GetTypeName

! ---------------------------------------------------------------------
! -----                     List Procedures                      -----
! ---------------------------------------------------------------------

MODULE FUNCTION ListArray_AddAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the specified item at the specified index where
    !  the index must be between 1 and the collection size.
    !  Also, return a flag indicating whether the item is
    !  successfully added.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
    !% the one-based index into the collection's items
    tIndex,              INTENT(IN)     :: Index
    !% the item to be added to the collection
    TYPE(*),             INTENT(IN)     :: Item
    !> flag indicating whether the item is successfully added. <br>
    ! - true if the item is successfully added.
    ! - false if the item is NOT successfully added.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW
    
    ! check the validity of the specified index
    Flag = FalseVal
    IF (.NOT.IsIndexValid(Collection%Size, Index)) RETURN

    ! first, grow the collection capacity if necessary
    CALL Collection%Growing()

    ! compute the actual index of the collection's items
    ID = ComputeTrueIndex(Collection, Index)

    ! move the collection's items to the right of ID
    CALL MoveItemsRight(Collection, ID)
    
    ! then, add new item to the collection
    CALL Collection%Items(ID)%Set(Item)

    ! next, update pointer (wrap around if necessary) and size
    Collection%Last = Collection%Last + 1_kIndex
    IF (Collection%Last > SIZE(Collection%Items, KIND=kIndex)) Collection%Last = 1_kIndex
    Collection%Size = Collection%Size + 1_kIndex

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION ListArray_AddAt

!**************************************************************************************

MODULE FUNCTION ListArray_RemoveAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the item at the specified index where
    !  the index must be between 1 and the collection size.
    !  Also, return a flag indicating whether the item is
    !  successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(INOUT)  :: Collection
    !% the one-based index into the collection's items
    tIndex,              INTENT(IN)     :: Index
    !% the item to be removed from the collection
    TYPE(*),             INTENT(INOUT)  :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the item is successfully removed.
    ! - false if the item is NOT successfully removed.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW

    ! check whether the item is available or not
    Flag = FalseVal
    IF (Collection%IsEmpty()) RETURN
    IF (.NOT.IsIndexValid(Collection%Size, Index)) RETURN

    ! compute the actual index of the collection's items
    ID = ComputeTrueIndex(Collection, Index)

    ! get the item
    CALL Collection%Items(ID)%Get(Item)

    ! move the collection's items to the left of ID
    CALL MoveItemsLeft(Collection, ID)
    
    ! update pointer (wrap around if necessary) and size
    Collection%Last = Collection%Last - 1_kIndex
    IF (Collection%Last == 0_kIndex) Collection%Last = SIZE(Collection%Items, KIND=kIndex)
    Collection%Size = Collection%Size - 1_kIndex

    ! shrink the collection if necessary
    CALL Collection%Shrinking()

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION ListArray_RemoveAt

!**************************************************************************************

MODULE FUNCTION ListArray_PeekAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the item (without removing it from the collection) at
    !  the specified index where the index must be between 1 and the
    !  collection size.  Also, return a flag indicating whether the
    !  item is available or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListArray object
    CLASS(ListArray(*)), INTENT(IN)     :: Collection
    !% the one-based index into the collection's items
    tIndex,              INTENT(IN)     :: Index
    !% the item to be retrieved from the collection
    TYPE(*),             INTENT(INOUT)  :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the item is available.
    ! - false if the item is NOT available.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: ID

! FLOW

    ! check whether the item is available or not
    Flag = FalseVal
    IF (Collection%IsEmpty()) RETURN
    IF (.NOT.IsIndexValid(Collection%Size, Index)) RETURN

    ! compute the actual index of the collection's items
    ID = ComputeTrueIndex(Collection, Index)

    ! get the item
    CALL Collection%Items(ID)%Get(Item)

    ! set output flag
    Flag = TrueVal

    RETURN

END FUNCTION ListArray_PeekAt

!**************************************************************************************

END SUBMODULE SubClass_ListArray

!******************************************************************************

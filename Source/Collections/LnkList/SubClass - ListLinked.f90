
SUBMODULE (Class_LinkedLists) SubClass_ListLinked

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains routines for the *ListLinked* type that provides
!   add, remove and retrieve operations at a specified index where the index
!   must be between 1 and the collection size.  The *ListLinked* type represents
!   a doubly-linked list utilizing an intrusive linked list implementation.

!** USE STATEMENTS:
    USE ISO_C_BINDING, ONLY: C_LOC, C_F_POINTER, C_PTR, C_SIZEOF
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'SubClass_ListLinked'
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

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

MODULE FUNCTION ListLinked_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the ListLinked type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListLinked(*)), INTENT(IN)    :: Obj
    tCharAlloc                          :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'ListLinked'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION ListLinked_GetTypeName

! ---------------------------------------------------------------------
! -----                     List Procedures                      -----
! ---------------------------------------------------------------------

MODULE FUNCTION ListLinked_AddAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To insert the given item at the specified index where
    !  the index must be between 1 and the collection size.
    !  Also, return a flag indicating whether the item is
    !  successfully added.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListLinked object
    CLASS(ListLinked(*)), INTENT(INOUT) :: Collection
    !% the one-based index into the collection's items
    tIndex,               INTENT(IN)    :: Index
    !% the item to be added to the collection
    TYPE(*),              INTENT(IN)    :: Item
    !> flag indicating whether the item is successfully added. <br>
    ! - true if the item is successfully added.
    ! - false if the item is NOT successfully added.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(LinkedNode(Collection%ValSize)), POINTER   :: NewNode
    tInteger                                        :: AllocStat
    tCharLen(MsgLen)                                :: AllocMsg

! FLOW
    
    ! check the validity of the specified index
    Flag = FalseVal
    IF (.NOT.IsIndexValid(Collection%GetSize(), Index)) RETURN
 
    ! allocate new node
    ALLOCATE(NewNode, STAT=AllocStat, ERRMSG=AllocMsg)
    CALL Handle_ErrAlloc('ListLinked_AddAt', ModName, AllocMsg, AllocStat)
        
    ! set item to the new node
    CALL NewNode%Store%Set(Item)
        
    ! append the new node at the specified index
    Flag = Collection%WrkLst%AddAt(NewNode, Index)
        
    ! free up pointer
    NULLIFY(NewNode)
    
    RETURN

END FUNCTION ListLinked_AddAt

!**************************************************************************************

MODULE FUNCTION ListLinked_RemoveAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get and remove the item at the specified index where
    !  the index must be between 1 and the collection size.
    !  Also, return a flag indicating whether the item is
    !  successfully removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListLinked object
    CLASS(ListLinked(*)), INTENT(INOUT)     :: Collection
    !% the one-based index into the collection's items
    tIndex,               INTENT(IN)        :: Index
    !% the item to be removed from the collection
    TYPE(*),              INTENT(INOUT)     :: Item
    !> flag indicating whether the item is successfully removed. <br>
    ! - true if the item is successfully removed.
    ! - false if the item is NOT successfully removed.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg

! FLOW
    
    Flag = Collection%WrkLst%RemoveAt(Index, CurrNode)

    IF (ASSOCIATED(CurrNode).AND.Flag) THEN
        ! get the item stored in the node
        SELECT TYPE (CurrNode)
        TYPE IS (LinkedNode(*))
            CALL CurrNode%Store%Get(Item)
        END SELECT

        ! nullify pointer components of the node
        CALL CurrNode%FreePointers()
    
        ! deallocate the node
        DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrDealloc('DequeLinked_RemoveFirst', ModName, AllocMsg, AllocStat)
    END IF
    
    NULLIFY(CurrNode)

    RETURN

END FUNCTION ListLinked_RemoveAt

!**************************************************************************************

MODULE FUNCTION ListLinked_PeekAt(Collection, Index, Item) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To get the item (without removing it from the collection) at
    !  the specified index where the index must be between 1 and the
    !  collection size.  Also, return a flag indicating whether the
    !  item is available or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListLinked object
    CLASS(ListLinked(*)), INTENT(INOUT) :: Collection
    !% the one-based index into the collection's items
    tIndex,               INTENT(IN)    :: Index
    !% the item to be retrieved from the collection
    TYPE(*),              INTENT(INOUT) :: Item
    !> flag indicating whether the item is available. <br>
    ! - true if the item is available.
    ! - false if the item is NOT available.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode

! FLOW

    ! get the specified node
    CurrNode => Collection%WrkLst%GetAt(Index)
    
    IF (ASSOCIATED(CurrNode)) THEN
        ! set output flag
        Flag = TrueVal
        ! get the item stored in the node
        SELECT TYPE (CurrNode)
        TYPE IS (LinkedNode(*))
            CALL CurrNode%Store%Get(Item)
        END SELECT
    ELSE
        ! set output flag
        Flag = FalseVal
    END IF
    
    NULLIFY(CurrNode)

    RETURN

END FUNCTION ListLinked_PeekAt

!**************************************************************************************

END SUBMODULE SubClass_ListLinked

!******************************************************************************

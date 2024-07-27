
MODULE Class_BaseCollection

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseCollection* type and its related routines.
!   The *BaseCollection* type is an abstract type representing a collection,
!   which is a group of objects, known as its items or elements.  Some
!   collections allow duplicate elements while others do not.  Some are
!   ordered collections and others are unordered collections. <br>
!   The *BaseCollection* type defines an application programming interface
!   (API) for various common operations.  All other collection types should
!   extend from this base type. <br>
!   It is important to note that the *BaseCollection* type is a subtype of
!   the *Assignable* type.  Therefore, it inherits all deferred procedures
!   required by a subtype of the *Assignable* type.

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_Assignable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseCollection

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_BaseCollection'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE 
    !> The *BaseCollection* type is an abstract collection type that defines
    !  an API for various common operations.  Some operations are deferred
    !  while others (with default implementation) can be overridden.
    TYPE, ABSTRACT, EXTENDS(Assignable) :: BaseCollection
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a binding name of the *CopyCol* deferred procedure. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method
        !  to construct a new collection from another collection.
        PROCEDURE(CopyCol),  DEFERRED   :: MakeCopy
        !> *Clear* is a binding name of the *ClearElm* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE(ClearElm), DEFERRED   :: Clear
        !> *Destruct* is a binding name of the *Destroy* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Important Note**:  For some collections, this method is equivalent to
        !                the *Clear* method.  For other collections (e.g. dynamic-
        !                array-based), the two methods are not equivalent to one
        !                another.  For these collections (which require an explicit
        !                construction of the collection), after calling the *Destruct*
        !                method, the user should reconstruct the collection (by calling
        !                a *Construction* method again) before using other operations
        !                once more.  Otherwise, the collection's behavior may not be
        !                as expected.
        PROCEDURE(Destroy),  DEFERRED   :: Destruct
        !> *GetSize* is a binding name of the *ColSize* deferred procedure. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE(ColSize),  DEFERRED   :: GetSize
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a new collection. <br>
        !  **Usage**: <br>
        !           ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => MakeCopy
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%IsEmpty() <br>
        !   --->    IF (.NOT.Collection%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => BaseCollection_IsEmpty
    END TYPE BaseCollection

!** INTERFACE DEFINITIONS:
    ! abstract interface for Collection
    ABSTRACT INTERFACE
        !> CopyCol is a deferred procedure to creates a new collection (This)
        !  with the same items as the given collection (Other).  In essence,
        !  this is a constructor that allows the user to copy items from
        !  any collection. <br>
        !  It should be noted that this procedure is slightly different from
        !  the *CopyObj* procedure inherited from the *Assignable* type such
        !  that types of *This* and *Other* collections can be different
        !  whereas types of *SrcObj* and *DstObj* objects must be the same.
        SUBROUTINE CopyCol(This, Other)
            IMPORT
            !% collection object to be created
            CLASS(BaseCollection), INTENT(INOUT)    :: This
            !% collection object to be copied from
            CLASS(BaseCollection), INTENT(INOUT)    :: Other
        END SUBROUTINE
        !> *ClearElm* is a deferred procedure to remove all of the items
        !  from the collection.
        SUBROUTINE ClearElm(Collection)
            IMPORT
            !% collection object
            CLASS(BaseCollection), INTENT(INOUT)    :: Collection
        END SUBROUTINE
        !> *Destroy* is a deferred procedure to destruct the collection where
        !  all items are removed (this operation is essentially the same as
        !  that of the *ClearElm* procedure) and the storage of those items
        !  are freed.   For the second operation, this is also done by
        !  the *ClearElm* procedure for some collections.  However, for others
        !  (such as dynamic-array-based collections), this must only be done
        !  by this procedure.
        SUBROUTINE Destroy(Collection)
            IMPORT
            !% collection object
            CLASS(BaseCollection), INTENT(INOUT)    :: Collection
        END SUBROUTINE
        !> *ColSize* is a deferred procedure to get the current size
        !   of the collection, which represents the number of items
        !   currently stored in the collection.
        FUNCTION ColSize(Collection) RESULT(Size)
            IMPORT
            !% collection object
            CLASS(BaseCollection), INTENT(IN)   :: Collection
            !% collection size
            tIndex                              :: Size
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION BaseCollection_IsEmpty(Collection) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether the collection is currently empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseCollection), INTENT(IN)   :: Collection   !! collection object
    tLogical                            :: Flag         !! true if the collection currently contains no item.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = (Collection%GetSize() == 0)

    RETURN

END FUNCTION BaseCollection_IsEmpty

!******************************************************************************

END MODULE Class_BaseCollection

!******************************************************************************

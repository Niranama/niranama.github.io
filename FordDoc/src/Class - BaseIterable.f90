
MODULE Class_BaseIterable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseIterable* type and its related routines.
!   The *BaseIterable* type is an abstract collection type extending the
!   *BaseCollection* type to define additional methods for an iteration over
!   items stored in a collection.  By design, the *BaseIterable* type is a
!   parameterized derive type where all its subtypes utilize the *GenStore*
!   type as a generic data storage of an item inserted into a collection.
!   Therefore, an iterable and parameterized collection type should extend
!   from this base type.  <br>
!   <br>
!   **Usage Notes**:  <br>
!   The *BaseIterable* type provides two methods to iterate over a collection.
!   The *StartFirst* method is used to start an iteration and the *MoveForward*
!   method is used to move to the next iteration. <br>
!   The following code snippet illustrates how to typically traverse the collection.
!   <Pre><Code style="color:MidnightBlue;">
!   ! start forward iteration (from the first item)
!   IsEmpty = Collection%StartFirst()
!   IF (.NOT.IsEmpty) DoSomeThing...
!   DO
!       ! move to the next iteration
!       IsTheEnd = Collection%MoveForward()
!       ! check whether we reach the end of the collection or not
!       IF (IsTheEnd) EXIT
!       ! if not, do the task we need
!       DoSomeThing...
!   END DO
!   </Code></Pre>
!   The following code snippet shows another way to iterate over the collection.
!   <Pre><Code style="color:MidnightBlue;">
!   ! start forward iteration (from the first item)
!   IsTheEnd = Collection%StartFirst(CurrItem)
!   DO WHILE (.NOT.IsTheEnd)
!       DoSomeThing_With_CurrItem...
!       ! move to the next iteration
!       IsTheEnd = Collection%MoveForward(CurrItem)
!   END DO
!   </Code></Pre>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseCollection
    USE Class_GenStore,     ONLY: IsEqualGen

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseIterable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_BaseIterable'

!** DERIVED TYPE 
    !> The *BaseIterable* type is an abstract collection type that
    !  defines an API for iterable collections, which are collections
    !  that can iterate over their items. <br>
    !  The *BaseIterable* type and its subclasses are intended to be
    !  used with the <a href="../module/class_genstore.html#type-genstore">GenStore</a>
    !  type, which is a generic data storage type.  The *GenStore* type
    !  can store various data type providing that the size (in bytes) of
    !  the data to be stored is known at compile time. <br>
    !  As a result, the *BaseIterable* type and all its subclasses are
    !  parameterized collection types that requires a known byte size of
    !  the data to be stored in a collection.  This known byte size must
    !  be specified at compile time when a collection type is declared. <br>
    !  The following code snippet shows various ways to declare the
    !  *BaseIterable* type and its subclasses.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! declare an allocatable variable of the <em>BaseIterable</em> type
    !   ! and then allocate it to one of its concrete subtypes
    !   TYPE Point2D
    !       REAL    :: X, Y
    !   END TYPE
    !   INTEGER, PARAMETER :: PointSize = SIZEOF(Point2D(0.0, 0.0)) ! byte size of Point2D
    !   CLASS(BaseIterable(:)), ALLOCATABLE :: Collection
    !   ALLOCATE(StackArray(PointSize) :: Collection)   ! a stack of Point2D items
    !
    !   ! or directly declare variables of its concrete subtypes
    !   INTEGER, PARAMETER :: IntSize = C_SIZEOF(0)     ! byte size of default integer
    !   INTEGER, PARAMETER :: RealSize = C_SIZEOF(0.0)  ! byte size of default real
    !   TYPE(DequeArray(IntSize))   ::  IntDeque        ! a deque of integer items
    !   TYPE(ListArray(RealSize))   ::  RealList        ! a list of real items
    !   </Code></Pre>
    TYPE, ABSTRACT, EXTENDS(BaseCollection) :: BaseIterable(ValSize)
        !% size of data content (a value of an element) in bytes
        tInteger,              LEN              :: ValSize
        !% procedure to compare GenStore items
        PROCEDURE(IsEqualGen), NOPASS, POINTER  :: EqualTo => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *StartFirst* is a binding name of the *Move2FirstElm* deferred procedure. <br>
        !  **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start the *forward* iteration and return a flag indicating
        !                whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstItem)
        PROCEDURE(Move2FirstElm), DEFERRED  :: StartFirst
        !> *MoveForward* is a binding name of the *Move2NextElm* deferred procedure. <br>
        !  **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next iteration and return a flag indicating whether
        !                the cursor pointer has reached the end of the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextItem)
        PROCEDURE(Move2NextElm),  DEFERRED  :: MoveForward
        !> *Insert* is a binding name of the *AddElm* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified item to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Item) <br>
        !  **Note**:  This is a default procedure for adding an item to the collection.
        !       For a particular collection, an alias name (e.g. the *Push* method for
        !       a stack collection) may be used in place of the *Insert* method.
        PROCEDURE(AddElm),        DEFERRED  :: Insert
        !> *Delete* is a binding name of the *DelElm* deferred procedure. <br>
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
        PROCEDURE(DelElm),        DEFERRED  :: Delete
        !> *ToArray* is a binding name of the *DelNGetElm* deferred procedure. <br>
        !  **Type-Bound Function**: ToArray <br>
        !  **Purpose**:  To get and remove all items from the collection.  Also, return
        !                a flag indicating whether the items are successfully removed. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%ToArray(Items) <br>
        !   --->    IF (.NOT.Collection%ToArray(Items)) DoSomething
        PROCEDURE(DelNGetElm),    DEFERRED  :: ToArray
        !> *GetAll* is a binding name of the *GetAllElm* deferred procedure. <br>
        !  **Type-Bound Function**: GetAll <br>
        !  **Purpose**:  To get all items (without removing them) from the collection.
        !                Also, return a flag indicating whether the items are available. <br>
        !  **Usage**: <br>
        !   --->    Success = Collection%GetAll(Items) <br>
        !   --->    IF (.NOT.Collection%GetAll(Items)) DoSomething
        PROCEDURE(GetAllElm),     DEFERRED  :: GetAll
        ! ---------------------------------------------------------------------
        ! -----      Deferred Procedures from BaseCollection Type         -----
        ! ---------------------------------------------------------------------
        !> *Clear* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all items from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear() <br>
        !  **Note**: The *Clear* method provided by the *BaseIterable* type is
        !       a simple implementation.   It can be overridden by a better and
        !       more efficient implementation.
        PROCEDURE   :: Clear        => BaseIterable_ClearItems
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by BaseIterable Type          -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: SetEQProc <br>
        !  **Purpose**:  To set a pointer to the *IsEqualTo* procedure.   This method
        !                must be called before calling the *IsEqualTo* method to check
        !                whether two iterable collections are equal to one another or not.
        !                Otherwise, the *IsEqualTo* method would always return false.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%SetEQProc(IsEqualTo)
        PROCEDURE   :: SetEQProc    => BaseIterable_SetEQProc
    END TYPE BaseIterable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *Move2FirstElm* is a deferred procedure to move to
        !   the first element in an iterable collection.
        FUNCTION Move2FirstElm(Collection, Item) RESULT(IsEmpty)
            IMPORT
            !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
            !% the first element as output if requested (and available)
            TYPE(*), OPTIONAL,      INTENT(INOUT)   :: Item
            !> a flag indicating whether the collection contains no element or not <br>
            ! - true if the collection is empty. <br>
            ! - otherwise the first element is available.
            tLogical                                :: IsEmpty
        END FUNCTION
        !> *Move2NextElm* is a deferred procedure to move to
        !   the next element in an iterable collection.
        FUNCTION Move2NextElm(Collection, Item) RESULT(IsTheEnd)
            IMPORT
            !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
            !% the next element as output if requested (and available)
            TYPE(*), OPTIONAL,      INTENT(INOUT)   :: Item
            !> a flag indicating whether the move to the end of the collection occurs or not <br>
            ! - true if next element is NOT available. <br>
            ! - otherwise next element is available.
            tLogical                                :: IsTheEnd
        END FUNCTION
        !> *AddElm* is a deferred procedure to add an item to a collection.
        !  For a collection with various *add* procedures.  This procedure
        !  should be the default one.
        SUBROUTINE AddElm(Collection, Item)
            IMPORT
            !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
            !% the item to be added to the collection
            TYPE(*),                INTENT(IN)      :: Item
        END SUBROUTINE
        !> *DelElm* is a deferred procedure to delete an item from a collection.
        !   This procedure is intended to be used in conjunction with the
        !   *Move2FirstElm* and *Move2NextElm* procedures.  Therefore, after
        !   the call to either procedure and then calling this procedure will
        !   result in a removal of the current item of the iteration (i.e. the
        !   same item that can be retrieved via those *Move* procedures).
        SUBROUTINE DelElm(Collection)
            IMPORT
            !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
        END SUBROUTINE
        !> *DelNGetElm* is a deferred procedure to get and remove all items
        !   from the collection.  Also, return a flag indicating whether the
        !   items are successfully removed.
        FUNCTION DelNGetElm(Collection, Items) RESULT(Flag)
            IMPORT
	        !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
            !% the items to be retrieved and removed from the collection
            TYPE(*),                INTENT(INOUT)   :: Items(:)
            !> flag indicating whether the items are successfully removed. <br>
            ! - true if the items are successfully removed.
            ! - false if the items are NOT successfully removed.
            tLogical                                :: Flag
        END FUNCTION
        !> *GetAllElm* is a deferred procedure to get all items (without
        !   removing them) from the collection.  Also, return a flag
        !   indicating whether the items are available.
        FUNCTION GetAllElm(Collection, Items) RESULT(Flag)
            IMPORT
	        !% iterable collection object
            CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
            !% the items to be retrieved from the collection
            TYPE(*),                INTENT(INOUT)   :: Items(:)
            !> flag indicating whether the items are successfully retrieved. <br>
            ! - true if the items are available.
            ! - false if the items are NOT available.
            tLogical                                :: Flag
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseIterable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    !  This routine provides a basic implementation of the *Clear*
    !  deferred procedure required by the *BaseCollection* class. <br>
    !  This routine should be overridden if a better implementation
    !  is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: TheEnd

!** FLOW:

    TheEnd = Collection%StartFirst()
    DO WHILE (.NOT.TheEnd)
        CALL Collection%Delete()
        TheEnd = Collection%MoveForward()
    END DO

    RETURN

END SUBROUTINE BaseIterable_ClearItems

!**************************************************************************************

SUBROUTINE BaseIterable_SetEQProc(Collection, IsEqualTo)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set a pointer to the *IsEqualTo* procedure.  This procedure
    !  must be called before calling the *IsEqualTo* method to check
    !  whether two iterable collections are equal to one another or not.
    !  Otherwise, the *IsEqualTo* method would always return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(BaseIterable(*)), INTENT(INOUT)   :: Collection
    !% user-supplied procedure to compare items stored in the collection
    PROCEDURE(IsEqualGen)                   :: IsEqualTo
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%EqualTo => IsEqualTo

    RETURN

END SUBROUTINE BaseIterable_SetEQProc

!**************************************************************************************

END MODULE Class_BaseIterable

!******************************************************************************

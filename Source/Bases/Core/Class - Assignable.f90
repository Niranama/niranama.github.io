
MODULE Class_Assignable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Assignable* type and its related routines.
!   The *Assignable* type is an abstract data type representing an object
!   that can make a copy of itself.  It defines an application programming
!   interface (API) for various common operations shared by most objects.
!   By design, the *Assignable* type is intended to be a base type (superclass)
!   for all other derived types with object-oriented programming (OOP)
!   implementation.  Hence, all other derived types used in the *XpfLib* library
!   should extend from this base type (i.e being its subclasses). <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Assignable
    PUBLIC :: ASSIGNMENT(=)

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_Assignable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE 
    !>  The *Assignable* type is an abstract data type that must be able to
    !   make a copy of itself. <br>
    TYPE, ABSTRACT :: Assignable
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a binding name of the *CopyObj* deferred procedure. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE(CopyObj),  DEFERRED   :: CopyAssign
        !> *MakeClone* is a binding name of the *CloneObj* deferred procedure. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE(CloneObj), DEFERRED   :: MakeClone
        !> *IsEqualTo* is a binding name of the *EqualObj* deferred procedure. <br>
        PROCEDURE(EqualObj), DEFERRED   :: IsEqualTo
        !> *FreeMemory* is a binding name of the *FreeObj* deferred procedure. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE(FreeObj),  DEFERRED   :: FreeMemory
        !> *GetTypeName* is a binding name of the *TypeName* deferred procedure. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE(TypeName), DEFERRED   :: GetTypeName
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Operator Overload**: ASSIGNMENT(=)  <br> 
        !  **Purpose**:  To copy a source object to a destination object. <br>
        !  **Usage**: <br>
        !   --->    DstObj = SrcObj
        GENERIC     :: ASSIGNMENT(=)    => CopyAssign
        !> **Type-Bound Subroutine**: MemFree <br>
        !  **Purpose**:  To free memory of an *Assignable* object. <br>
        !  **Usage**: <br>
        !   --->    CALL Obj%MemFree()
        GENERIC     :: MemFree          => FreeMemory
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To create and return a copy of an *Assignable* object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcObj%GetClone(DstObj)
        GENERIC     :: GetClone         => MakeClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the *concrete* type name of an *Assignable* object. <br>
        !  **Usage**: <br>
        !   --->    TypeName = Obj%GetName()
        GENERIC     :: GetName          => GetTypeName
    END TYPE Assignable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> *CopyObj* is a deferred procedure to copy the source object to the
        ! destination object.  The procedure typically provides a value-copying
        ! task for the object's components.  However, for objects with pointer
        ! and/or allocatable components, the procedure may also provide a storage
        ! allocation task of those pointer and/or allocatable components for the
        ! destination object.  The procedure should also provides a type-guard
        ! statement to check whether the source and destination objects have the
        ! same *concrete* type or not.  If their types are not the same, the
        ! procedure should report an error.  It is important to note that the
        ! *CopyObj* procedure is intended to be used in an assignment expression,
        ! such as "DstObj = SrcObj".
        SUBROUTINE CopyObj(DstObj, SrcObj)
            IMPORT
            CLASS(Assignable), INTENT(OUT)  :: DstObj   !! destination object
            CLASS(Assignable), INTENT(IN)   :: SrcObj   !! source object
        END SUBROUTINE
        !> *CloneObj* is a deferred procedure to create and return a copy of
        ! an *Assignable-class* object.  The difference between the *CopyObj*
        ! and *CloneObj* procedures is that when comparing the source and
        ! destination objects using the *EqualObj* procedure, the comparison
        ! based on using the *CopyObj* procedure should always return true
        ! (i.e. both objects are equal) whereas the comparison based on using
        ! the *CloneObj* procedure may or may not return true depending on
        ! the concrete extended type.  Also, the *CloneObj* procedure does not
        ! need a type-guard statement because it must allocate the destination
        ! based on the actual type of the source object.
        SUBROUTINE CloneObj(SrcObj, DstObj)
            IMPORT
            CLASS(Assignable),              INTENT(IN)  :: SrcObj   !! source object
            CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object
        END SUBROUTINE
        !> *EqualObj* is a deferred procedure to compare whether two objects
        ! are equal to one another or not.  It is important to note that the
        ! *EqualObj* procedure is intended to be used in an equal expression,
        ! such as "LhsObj == RhsObj".  The procedure should implement an
        ! equivalence relation on non-null objects such that: <br>
        ! - It is *reflexive*: for any non-null object X, the expression (X == X)
        !   should return true. <br>
        ! - It is *symmetric*: for any non-null objects X and Y, (X == Y) returns
        !   true if and only if (Y == X) returns true. <br>
        ! - It is *transitive*: for any non-null objects X, Y and Z, if (X == Y)
        !   returns true and (Y == Z)  returns true, then (X == Z) should return
        !   true. <br>
        ! - For a non-null object X and a null object Y, (X == Y) should return
        !   false.
        FUNCTION EqualObj(LhsObj, RhsObj) RESULT(Flag)
            IMPORT
            CLASS(Assignable), INTENT(IN)   :: LhsObj   !! an object
            CLASS(Assignable), INTENT(IN)   :: RhsObj   !! another object
            tLogical                        :: Flag     !! true if both objects are equal
        END FUNCTION
        !> *FreeObj* is a deferred procedure to free storage/memory of an object
        ! with pointer and/or allocatable components.  For an object without
        ! any pointer or allocatable components, the routine would do nothing
        ! (i.e. it is a dummy routine).
        SUBROUTINE FreeObj(Obj)
            IMPORT
            CLASS(Assignable), INTENT(INOUT)    :: Obj
        END SUBROUTINE
        !> *TypeName* is a deferred procedure to get name of a concrete
        !   extended type that is a subtype of the *Assignable* type.
        FUNCTION TypeName(Obj) RESULT(Name)
            IMPORT
            CLASS(Assignable), INTENT(IN)   :: Obj
            tCharAlloc                      :: Name
        END FUNCTION
    END INTERFACE
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To make a copy of an array of the *Assignable* type. <br>
        !  **Usage**: <br>
        !   --->    LHS = RHS
        MODULE PROCEDURE    :: Assignable_CopyAssign1D
        MODULE PROCEDURE    :: Assignable_CopyAssign2D
        MODULE PROCEDURE    :: Assignable_CopyAssign3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Assignable_CopyAssign1D(OutArr,InArr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To make a copy of an array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Assignable), INTENT(OUT)  :: OutArr(:)
    CLASS(Assignable), INTENT(IN)   :: InArr(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: I

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
        ! report error
        CALL Handle_ErrLevel('CopyAssign1D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    ! copy data
    DO I = 1, MIN(SIZE(OutArr), SIZE(InArr))
        OutArr(I) = InArr(I)
    END DO

    RETURN

END SUBROUTINE Assignable_CopyAssign1D

!******************************************************************************

SUBROUTINE Assignable_CopyAssign2D(OutArr,InArr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To make a copy of an array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Assignable), INTENT(OUT)  :: OutArr(:,:)
    CLASS(Assignable), INTENT(IN)   :: InArr(:,:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: I, J

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
        ! report error
        CALL Handle_ErrLevel('CopyAssign2D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    ! copy data
    DO I = 1, MIN(SIZE(OutArr, DIM=1), SIZE(InArr, DIM=1))
        DO J = 1, MIN(SIZE(OutArr, DIM=2), SIZE(InArr, DIM=2))
            OutArr(I,J) = InArr(I,J)
        END DO
    END DO

    RETURN

END SUBROUTINE Assignable_CopyAssign2D

!******************************************************************************

SUBROUTINE Assignable_CopyAssign3D(OutArr,InArr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To make a copy of an array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Assignable), INTENT(OUT)  :: OutArr(:,:,:)
    CLASS(Assignable), INTENT(IN)   :: InArr(:,:,:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: I, J, K

! FLOW
    
    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutArr, InArr)) THEN
        ! report error
        CALL Handle_ErrLevel('CopyAssign3D', ModName, ErrSevere, &
                             'Types of input and output arrays are NOT compatible.')
        RETURN
    END IF
        
    ! copy data
    DO I = 1, MIN(SIZE(OutArr, DIM=1), SIZE(InArr, DIM=1))
        DO J = 1, MIN(SIZE(OutArr, DIM=2), SIZE(InArr, DIM=2))
            DO K = 1, MIN(SIZE(OutArr, DIM=3), SIZE(InArr, DIM=3))
                OutArr(I,J,K) = InArr(I,J,K)
            END DO
        END DO
    END DO

    RETURN

END SUBROUTINE Assignable_CopyAssign3D

!******************************************************************************

END MODULE Class_Assignable

!******************************************************************************


MODULE Class_KeyStore

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KeyStore* type and its related routines.
!   The *KeyStore* type is a data type that can be used to store various
!   key type providing that the key can be compared to itself and/or other
!   key object.  This data type is intended to be used in conjunction with
!   an ordered associative (key-value) container. <br>
!   The comparison between the keys provides a total ordering on the key
!   objects.  This ordering is referred to as the key's natural ordering,
!   and the *CompareTo* method is referred to as its natural comparison
!   method. <br>
!   For the *KeyStore* type implemented here, the valid key types include
!   any Fortran intrinsic types that have the *natural ordering* and a
!   derived type that is in the *Comparable* class.  The valid Fortran
!   intrinsic key types include the CHARACTER type, INTEGER types and
!   REAL types.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! storage type
    PUBLIC :: KeyStore

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
!#include    "../../MacroDef/Macro - Error Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_KeyStore'

!** DERIVED TYPE DEFINITIONS
    !> The *KeyStore* type is a key storage type that can hold
    !  different data types appropriate for used in associative
    !  (key-value) collections.  A key is stored as an unlimited
    !  polymorphic type.
    TYPE, EXTENDS(Assignable)   :: KeyStore
        PRIVATE
        !% stored key
        CLASS(*), ALLOCATABLE   :: Key
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => KeyStore_Copy
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => KeyStore_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !  **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => KeyStore_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => KeyStore_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => KeyStore_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: KeyStore_EqualTo
        PROCEDURE, PRIVATE  :: KeyStore_NotEqualTo
        PROCEDURE, PRIVATE  :: KeyStore_LessThan
        PROCEDURE, PRIVATE  :: KeyStore_GreaterThan
        PROCEDURE, PRIVATE  :: KeyStore_LessEqual
        PROCEDURE, PRIVATE  :: KeyStore_GreaterEqual
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Set <br>
        ! **Purpose**:  To store the key in the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Set(Key)
        PROCEDURE   :: Set          => KeyStore_SetKey
        !> **Type-Bound Subroutine**: Get <br>
        ! **Purpose**:  To retrieve the key from the storage. <br>
        !  **Usage**: <br>
        !   --->    CALL Store%Get(Key)
        PROCEDURE   :: Get          => KeyStore_GetKey
        !> **Type-Bound Function**: CompareTo <br>
        ! **Purpose**: To compare *Key1* and *Key2* and return <br>
        !              1 if *Key1* is greater than *Key2*, <br>
        !              0 if *Key1* is equal to *Key2*, <br>
        !             -1 if *Key1* is less than *Key2*, <br>
        !           -999 if types of input keys are not the same or either key is invalid. <br>
        !  **Usage**: <br>
        !   --->    Flag = KeyA%CompareTo(KeyB)
        PROCEDURE   :: CompareTo    => KeyStore_CompareKey
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if the LHS value is equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS == RHS) DoSomething
        GENERIC     :: OPERATOR(==) => KeyStore_EqualTo
        !> **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if the LHS value is NOT equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS /= RHS) DoSomething
        GENERIC     :: OPERATOR(/=) => KeyStore_NotEqualTo
        !> **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS < RHS) DoSomething
        GENERIC     :: OPERATOR(<)  => KeyStore_LessThan
        !> **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS > RHS) DoSomething
        GENERIC     :: OPERATOR(>)  => KeyStore_GreaterThan
        !> **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS <= RHS) DoSomething
        GENERIC     :: OPERATOR(<=) => KeyStore_LessEqual
        !> **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS >= RHS) DoSomething
        GENERIC     :: OPERATOR(>=) => KeyStore_GreaterEqual
    END TYPE KeyStore

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE KeyStore_Copy(DstObj, SrcObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To copy the KeyStore object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore),   INTENT(OUT)  :: DstObj   !! destination object
    CLASS(Assignable), INTENT(IN)   :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE(SrcObj)
    TYPE IS (KeyStore)
        ALLOCATE(DstObj%Key, SOURCE=SrcObj%Key)
    CLASS DEFAULT
        CALL Handle_ErrLevel('KeyStore_Copy', ModName, ErrSevere, &
                             'Type of the source object is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE KeyStore_Copy

!******************************************************************************

SUBROUTINE KeyStore_Clone(SrcObj, DstObj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To clone the KeyStore object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore),                INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! create the KeyStore object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! create the key
    SELECT TYPE (DstObj)
    TYPE IS (KeyStore)
        ALLOCATE(DstObj%Key, SOURCE=SrcObj%Key)
    END SELECT

    RETURN

END SUBROUTINE KeyStore_Clone

!******************************************************************************

FUNCTION KeyStore_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore),   INTENT(IN)   :: LhsObj   !! an object
    CLASS(Assignable), INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (RhsObj)
    TYPE IS (KeyStore)
        Flag = (LhsObj%CompareTo(RhsObj) == 0)
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION KeyStore_IsEqualTo

!******************************************************************************

SUBROUTINE KeyStore_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !! To free memory of the KeyStore object.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(Obj%Key)) DEALLOCATE(Obj%Key)

    RETURN

END SUBROUTINE KeyStore_Free

!******************************************************************************

FUNCTION KeyStore_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the KeyStore type.  This is a deferred procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: Obj
    tCharAlloc                  :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'KeyStore'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION KeyStore_GetTypeName

!******************************************************************************

SUBROUTINE KeyStore_SetKey(Store, Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the key in the storage.  Write an error message to
    !   the default log file if type of the specified key is NOT valid

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(INOUT)  :: Store    !! the storage
    CLASS(*),        INTENT(IN)     :: Key      !! specified key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Check whether the specified key is valid or not.
    SELECT TYPE(Key)
    CLASS IS (Comparable)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tCharStar)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tLong)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tInteger)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tShort)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tByte)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tSingle)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tDouble)
        ALLOCATE(Store%Key, SOURCE=Key)
    TYPE IS (tQuad)
        ALLOCATE(Store%Key, SOURCE=Key)
    CLASS DEFAULT
        CALL Handle_ErrLevel('KeyStore_SetKey', ModName, ErrSevere, &
                             'Type of the specified key is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE KeyStore_SetKey

!******************************************************************************

SUBROUTINE KeyStore_GetKey(Store, Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the key from the storage.  Write an error message to
    !   the default log file if type of the given key is NOT valid
    !   or is NOT the same as that of the stored key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN)     :: Store    !! the storage
    CLASS(*),        INTENT(OUT)    :: Key      !! given key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check the stored key
    IF (.NOT.ALLOCATED(Store%Key)) THEN
        CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                             'The stored key has not yet been set.')
        RETURN
    END IF
    
    ! get the stored key
    SELECT TYPE(Key)
    CLASS IS (Comparable)
        SELECT TYPE (StoredKey => Store%Key)
        CLASS IS (Comparable)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tCharStar)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tLong)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tInteger)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tShort)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tByte)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tSingle)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tDouble)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (StoredKey => Store%Key)
        TYPE IS (tQuad)
            Key = StoredKey
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                                 'Type of the specified key is NOT the same as that of stored key.')
        END SELECT
    CLASS DEFAULT
        CALL Handle_ErrLevel('KeyStore_GetKey', ModName, ErrSevere, &
                             'Type of the specified key is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE KeyStore_GetKey

!******************************************************************************

FUNCTION KeyStore_CompareKey(Key1, Key2) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *Key1* and *Key2* and return <br>
    !   1 if *Key1* is greater than *Key2*, <br>
    !   0 if *Key1* is equal to *Key2*, <br>
    !  -1 if *Key1* is less than *Key2*, <br>
    !  -999 if types of input keys are not the same or either key is invalid. <br>
    ! Also, write an error message to the default log file if this happens.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: Key1
    TYPE(KeyStore),  INTENT(IN) :: Key2
    tInteger                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((.NOT.ALLOCATED(Key1%Key)).OR.(.NOT.ALLOCATED(Key2%Key))) THEN
        Flag = -999
        IF (.NOT.ALLOCATED(Key1%Key)) THEN
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Key1 has not yet been set.')
        ELSE
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Key2 has not yet been set.')
        END IF
        RETURN
    END IF

    SELECT TYPE (APtr => Key1%Key)
    CLASS IS (Comparable)
        SELECT TYPE (BPtr => Key2%Key)
        CLASS IS (Comparable)
            Flag = APtr%CompareTo(BPtr)
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tCharStar)
            IF (LGT(APtr, BPtr)) THEN
                Flag = 1
            ELSEIF (LLT(APtr, BPtr)) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tLong)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tInteger)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tShort)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tByte)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tSingle)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tDouble)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (BPtr => Key2%Key)
        TYPE IS (tQuad)
            IF (APtr > BPtr) THEN
                Flag = 1
            ELSEIF (APtr < BPtr) THEN
                Flag = -1
            ELSE
                Flag = 0
            END IF
        CLASS DEFAULT
            CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                                 'Types of the specified keys are NOT the same.')
        END SELECT
    CLASS DEFAULT
        CALL Handle_ErrLevel('KeyStore_CompareKey', ModName, ErrSevere, &
                             'Type of the specified Key1 is NOT valid.')
    END SELECT

    RETURN

END FUNCTION KeyStore_CompareKey

!******************************************************************************

FUNCTION KeyStore_EqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) == 0)

    RETURN

END FUNCTION KeyStore_EqualTo

!******************************************************************************

FUNCTION KeyStore_NotEqualTo(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is NOT equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) /= 0)

    RETURN

END FUNCTION KeyStore_NotEqualTo

!******************************************************************************

FUNCTION KeyStore_LessThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) < 0)

    RETURN

END FUNCTION KeyStore_LessThan

!******************************************************************************

FUNCTION KeyStore_GreaterThan(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) > 0)

    RETURN

END FUNCTION KeyStore_GreaterThan

!******************************************************************************

FUNCTION KeyStore_LessEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is less than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) <= 0)

    RETURN

END FUNCTION KeyStore_LessEqual

!******************************************************************************

FUNCTION KeyStore_GreaterEqual(A,B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To check whether A is greater than or equal to B or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KeyStore), INTENT(IN) :: A
    TYPE(KeyStore),  INTENT(IN) :: B
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Flag = (A%CompareTo(B) >= 0)

    RETURN

END FUNCTION KeyStore_GreaterEqual

!******************************************************************************

END MODULE Class_KeyStore

!******************************************************************************

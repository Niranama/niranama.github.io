
MODULE Class_ListTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ListTable* type, the *TabNode* type and their
!   related routines.  The *TabNode* type is a helper and private type used
!   to store a key-value pair.  The *ListTable* type is a collection type
!   that employs a linked-list implementation to provide common operations
!   for an unordered symbol table. <br>
!   As an unordered symbol table, the *ListTable* type allows all Fortran
!   intrinsic types (with the exception of the *LOGICAL* type) to be used
!   as a type of the key to insert a key-value pair into a collection.  If
!   a derived type as a type of the key is preferable, the key type must be
!   a subtype of the *Assignable* type (i.e. in the *Assignable* class) that
!   provides a method to check the key equality.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,              ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR
    USE ModBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE Class_IntrusiveLinkedLists, ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_BaseCollection
    USE Class_BaseSymTable
    USE Class_GenStore
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ListTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_ListTable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *TabNode* is a doubly-linked-list node type containing key and value as
    !  its components.  An unlimited polymorphic type is used as a storage of
    !  the key while the *GenStore* type is used as a storage of the value. <br>
    !  The *TabNode* type is a subtype of the *DoublyLinkedNode* type and is
    !  intended to be used with the *ListTable* type, which is a collection type
    !  that utilizes the *IntrusiveLinearList* type. <br>
    !  **Note**: This is a private type.
    TYPE, EXTENDS(DoublyLinkedNode) :: TabNode(ValSize)
        !% size of value (data content) in bytes
        tInteger,                LEN        :: ValSize
        !% stored key
        CLASS(*), ALLOCATABLE,   PRIVATE    :: Key
        !% stored value
        TYPE(GenStore(ValSize)), PRIVATE    :: Value
    CONTAINS
        !> **Type-Bound Function**: Set <br>
        !  **Purpose**:  To store the key in the Node.  Also, return a flag indicating
        !                whether the specified key is valid or not. <br>
        !  **Usage**: <br>
        !   --->    Valid = Node%SetKey(Key) <br>
        !   --->    IF (.NOT.Node%SetKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: SetKey       => TabNode_SetKey
        !> **Type-Bound Function**: GetKey <br>
        !  **Purpose**:  To retrieve the key from the node.  Also, return a flag
        !                indicating whether the key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Node%GetKey(Key) <br>
        !   --->    IF (.NOT.Node%GetKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: GetKey       => TabNode_GetKey
        !> **Type-Bound Function**: IsKeyEqual <br>
        !  **Purpose**:  To check whether the specified key is equal to the node's key. <br>
        !  **Usage**: <br>
        !   --->    Flag = Node%IsKeyEqual(Key)
        PROCEDURE, PRIVATE  :: IsKeyEqual   => TabNode_IsKeyEqual
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE, PRIVATE  :: IsEqualTo    => TabNode_IsEqualTo
        !> **Type-Bound Subroutine**: SetNew <br>
        !  **Purpose**:  To set new key and value.  Report error if type of the
        !                specified key is invalid. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%SetNew(Key, Value)
        PROCEDURE           :: SetNew       => TabNode_SetNew
        !> **Type-Bound Subroutine**: FreeNode <br>
        !  **Purpose**:  To free components of the specified node.  Optionally,
        !                retrieve the stored key and/or value. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%FreeNode() <br>
        !   --->    CALL Node%FreeNode(Key) <br>
        !   --->    CALL Node%FreeNode(Value=Value) <br>
        !   --->    CALL Node%FreeNode(Key, Value)
        PROCEDURE           :: FreeNode     => TabNode_FreeNode
    END TYPE TabNode
    !> The *ListTable* type is a collection type that employs a linked-list
    !  implementation to provide common operations for an unordered symbol
    !  table.  The *ListTable* type employs the *IntrusiveLinearList* type
    !  as its component to store *TabNode* objects.  As an intrusive container,
    !  the *IntrusiveLinearList* type provides common linked-list operations
    !  without a memory management task.  The memory management task of the
    !  inserted *TabNode* objects is handled by the *ListTable* type.  <br>
    !  As an unordered symbol table, the *ListTable* type directly extends the
    !  *BaseSymTable* type and implements all deferred procedures required by
    !  the *BaseSymTable* type and all its super classes (i.e. the *Assignable*
    !  type and the *BaseCollection* type).  As a symbol table, the *ListTable*
    !  type does not allow duplicated keys; therefore, if an inserted key is
    !  equal to a key stored in the table, an associated value of the stored key
    !  is replaced by an associated value of the inserted key. 
    TYPE, EXTENDS(BaseSymTable) :: ListTable
        PRIVATE
        ! a working doubly-linked list
        TYPE(IntrusiveLinearList)               :: WrkLst
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindKey(Key, KeyNode) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey  => ListTable_FindKey
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => ListTable_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => ListTable_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => ListTable_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => ListTable_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => ListTable_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => ListTable_CopyCollection
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  For the linked-list-based types, this method is equivalent
        !             to the *Clear* method.
        PROCEDURE   :: Destruct     => ListTable_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => ListTable_GetSize
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseSymTable Type        -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey) <br>
        !   --->    IsEmpty = Collection%StartFirst(Value=FirstVal) <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey, FirstVal)
        PROCEDURE   :: StartFirst   => ListTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => ListTable_Move2NextPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => ListTable_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE   :: Delete       => ListTable_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => ListTable_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => ListTable_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => ListTable_GetValue
        !> **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.
        !                The pointer is intended to be used as a mold for
        !                the key (i.e. provides type of the stored key).
        !                Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE   :: GetKeyPtr    => ListTable_GetKeyPtr
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: ListTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ListTable

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 TabNode Procedures                        -----
! ---------------------------------------------------------------------

FUNCTION TabNode_SetKey(Node, Key) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the key in the Node.  Also, return a flag indicating
    !  whether the specified key is valid or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), INTENT(INOUT)    :: Node
    !% the key to be stored
    CLASS(*),          INTENT(IN)       :: Key
    !> flag indicating whether the key is valid or not. <br>
    !  - true if type of specified key is valid. <br>
    !  - false if type of specified key is invalid.
    tLogical                            :: Valid

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Check whether the specified key is valid or not.
    SELECT TYPE(Key)
    CLASS IS (Assignable)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tCharStar)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tLong)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tInteger)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tShort)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tByte)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tSingle)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tDouble)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tQuad)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tCmpxSingle)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tCmpxDouble)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    TYPE IS (tCmpxQuad)
        Valid = TrueVal
        ALLOCATE(Node%Key, SOURCE=Key)
    CLASS DEFAULT
        Valid = FalseVal
    END SELECT

    RETURN

END FUNCTION TabNode_SetKey

!******************************************************************************

FUNCTION TabNode_GetKey(Node, Key) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the key from the node.  Also, return a flag indicating
    !  whether the key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), INTENT(INOUT)    :: Node
    !% the key to be retrieved
    CLASS(*),          INTENT(OUT)      :: Key
    !> flag indicating whether the key is successfully retrieved or not. <br>
    !  - true if successful where types of stored key and output key are the same. <br>
    !  - false if unsuccessful where types of stored key and output key are different.
    tLogical                            :: Success

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check the stored key
    IF (.NOT.ALLOCATED(Node%Key)) THEN
        Success = FalseVal
        RETURN
    END IF
    
    ! get the stored key
    SELECT TYPE(Key)
    CLASS IS (Assignable)
        SELECT TYPE (StoredKey => Node%Key)
        CLASS IS (Assignable)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
!    TYPE IS (tLogical)
!        SELECT TYPE (StoredKey => Node%Key)
!        TYPE IS (tLogical)
!            Key = StoredKey
!            Success = TrueVal
!        CLASS DEFAULT
!            Success = FalseVal
!        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCharStar)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tLong)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tInteger)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tShort)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tByte)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tSingle)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tDouble)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tQuad)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxSingle)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxDouble)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxQuad)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    CLASS DEFAULT
        Success = FalseVal
    END SELECT

    RETURN

END FUNCTION TabNode_GetKey

!******************************************************************************

FUNCTION TabNode_IsKeyEqual(Node, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is equal to the node's key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), INTENT(IN)   :: Node
    !% the key to be retrieved
    CLASS(*),          INTENT(IN)   :: Key
    !% true if the keys are equal; otherwise, false.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check the stored key
    IF (.NOT.ALLOCATED(Node%Key)) THEN
        Flag = FalseVal
        RETURN
    END IF
    
    ! get the stored key
    SELECT TYPE(Key)
    CLASS IS (Assignable)
        SELECT TYPE (StoredKey => Node%Key)
        CLASS IS (Assignable)
            Flag = Key%IsEqualTo(StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
!    TYPE IS (tLogical)
!        SELECT TYPE (StoredKey => Node%Key)
!        TYPE IS (tLogical)
!            Flag = (Key .EQV. StoredKey)
!        CLASS DEFAULT
!            Flag = FalseVal
!        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCharStar)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tLong)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tInteger)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tShort)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tByte)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tSingle)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tDouble)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tQuad)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxSingle)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxDouble)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (StoredKey => Node%Key)
        TYPE IS (tCmpxQuad)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TabNode_IsKeyEqual

!******************************************************************************

FUNCTION TabNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not. <br>
    !  It should be noted that this routine uses both key and value
    !  components of the *TabNode* object to check equality. Therefore,
    !  although (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B))
    !  can return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(IN)   :: LhsObj   !! an object
    CLASS(TabNode(*)), INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    
    ! check key equality
    IF (.NOT.LhsObj%IsKeyEqual(RhsObj%Key)) RETURN
    
    ! check value equality where values may be represented by character strings
    Flag = Character_IsEqual(LhsObj%Value, RhsObj%Value)

    RETURN

END FUNCTION TabNode_IsEqualTo

!******************************************************************************

SUBROUTINE TabNode_SetNew(Node, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set new key and value.  Report error if type of the specified key is invalid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), INTENT(INOUT)    :: Node
    !% the key
    CLASS(*),          INTENT(IN)       :: Key
    !% the associated value
    TYPE(*),           INTENT(IN)       :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Node%SetKey(Key)) THEN
        CALL Node%Value%Set(Value)
    ELSE
        CALL Handle_ErrLevel('TabNode_SetNew', ModName, ErrSevere, &
                   'Type of the specified key is NOT valid.')
    END IF

    RETURN

END SUBROUTINE TabNode_SetNew

!******************************************************************************

SUBROUTINE TabNode_FreeNode(Node, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free components of the specified node.  Optionally, retrieve
    !  the stored key and/or value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)),  INTENT(INOUT)   :: Node
    !% the stored key
    CLASS(*), OPTIONAL, INTENT(OUT)     :: Key
    !% the stored value
    TYPE(*),  OPTIONAL, INTENT(INOUT)   :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! get key if requested
    IF (PRESENT(Key)) THEN
        IF (.NOT.Node%GetKey(Key)) THEN
            CALL Handle_ErrLevel('TabNode_FreeNode', ModName, ErrWarning, &
                       'Type of the specified key is NOT valid.')
        END IF
    END IF
    
    ! get value if requested
    IF (PRESENT(Value)) CALL Node%Value%Get(Value)
    
    ! free node components
    IF (ALLOCATED(Node%Key)) DEALLOCATE(Node%Key)
    CALL Node%FreePointers()

    RETURN

END SUBROUTINE TabNode_FreeNode

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

SUBROUTINE ListTable_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  *Note*:  SrcObj must be in the *ListTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)), INTENT(OUT)    :: DstObj   !! destination object
    CLASS(Assignable),   INTENT(IN)     :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    TYPE IS (ListTable(*))
        IF (DstObj%ValSize == SrcObj%ValSize) THEN
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
        ELSE
            CALL Handle_ErrLevel('ListTable_CopyAssign', ModName, ErrSevere, &
                       'The "ValSize" components must have the same value.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('ListTable_CopyAssign', ModName, ErrSevere, &
                   'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE ListTable_CopyAssign

!******************************************************************************

SUBROUTINE ListTable_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the ListTable object.  This is a deferred
    !  procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)),            INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the ListTable object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    TYPE IS (ListTable(*))
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

END SUBROUTINE ListTable_Clone

!******************************************************************************

FUNCTION ListTable_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)), INTENT(IN) :: LhsObj   !! an object
    CLASS(Assignable),   INTENT(IN) :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (ListTable(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
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
                    ! check key and value equalities
                    SELECT TYPE (LhsNode)
                    TYPE IS (TabNode(*))
                        SELECT TYPE (RhsNode)
                        TYPE IS (TabNode(*))
                            IF (.NOT.LhsNode%IsEqualTo(RhsNode)) THEN
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

END FUNCTION ListTable_IsEqualTo

!******************************************************************************

SUBROUTINE ListTable_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the ListTable object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Clear()

    RETURN

END SUBROUTINE ListTable_Free

!******************************************************************************

FUNCTION ListTable_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the ListTable type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)), INTENT(IN) :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'ListTable'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION ListTable_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE ListTable_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(ListTable(*)),   INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (ListTable(*))
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
            CALL Handle_ErrLevel('ListTable_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS IS (BaseSymTable(*))
        ! different types of collection
        IF (This%ValSize == Other%ValSize) THEN
            BLOCK
                ! block variables
                tLogical                :: IsTheEnd
                CLASS(*), POINTER       :: KeyPtr
                CLASS(*), ALLOCATABLE   :: Key
                tByte, TARGET           :: Value(This%ValSize)
                TYPE(C_PTR)             :: ValPtr
                ! set pointers and allocate key based on KeyPtr
                ValPtr = C_LOC(Value)
                KeyPtr => Other%GetKeyPtr()
                ALLOCATE(Key, MOLD=KeyPtr)
                ! get the first key-value pair
                IsTheEnd = Other%StartFirst(Key, ValPtr)
                DO WHILE (.NOT.IsTheEnd)
                    ! add the current key-value pair to this collection
                    CALL This%Insert(Key, ValPtr)
                    ! move to the next key-value pair
                    IsTheEnd = Other%MoveForward(Key, ValPtr)
                END DO
                NULLIFY(KeyPtr)
                DEALLOCATE(Key)
                ValPtr = C_NULL_PTR
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('ListTable_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('ListTable_CopyCollection', ModName, ErrSevere, &
                   'Type of "Other" must be in the "BaseSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE ListTable_CopyCollection

!******************************************************************************

SUBROUTINE ListTable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListTable object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection

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
            ! free components of the node
            SELECT TYPE (CurrNode)
            TYPE IS (TabNode(*))
                CALL CurrNode%FreeNode()
            END SELECT
            ! deallocate the node
            DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('ListTable_ClearItems', ModName, AllocMsg, AllocStat)
        END IF
    END DO
    
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE ListTable_ClearItems

!******************************************************************************

SUBROUTINE ListTable_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% ListTable object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    CALL Collection%Clear()

    RETURN

END SUBROUTINE ListTable_Destroy

!******************************************************************************

FUNCTION ListTable_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ListTable(*)), INTENT(IN) :: Collection
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkLst%GetSize()

    RETURN

END FUNCTION ListTable_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION ListTable_Move2FirstPair(Collection, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.
    !  For the *ListTable*, which is an unordered symbol table, 
    !  the starting pair is the first pair inserted.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the first key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the first value as output if requested (and available)
    TYPE(*),  OPTIONAL,  INTENT(INOUT)  :: Value
    !> a flag indicating whether the collection contains no pair data or not <br>
    ! - true if the collection is empty. <br>
    ! - otherwise the first pair data is available.
    tLogical                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkLst%StartFirst(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%GetKey(Key)) THEN
                            CALL Handle_ErrLevel('ListTable_Move2FirstPair', ModName, ErrWarning, &
                                       "Type of the specified key does not match the table's key.")
                        END IF
                    END IF
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkLst%StartFirst()
    END IF

    RETURN

END FUNCTION ListTable_Move2FirstPair

!******************************************************************************

FUNCTION ListTable_Move2NextPair(Collection, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.
    !  For the *ListTable*, which is an unordered symbol table, 
    !  the next pair is the pair inserted after the previous one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the next key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the next value as output if requested (and available)
    TYPE(*),  OPTIONAL,  INTENT(INOUT)  :: Value
    !> a flag indicating whether the move to the end of the
    !  collection occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! move to next iteration
            IsTheEnd = Collection%WrkLst%MoveForward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%GetKey(Key)) THEN
                            CALL Handle_ErrLevel('ListTable_Move2FirstPair', ModName, ErrWarning, &
                                       "Type of the specified key does not match the table's key.")
                        END IF
                    END IF
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IsTheEnd = Collection%WrkLst%MoveForward()
    END IF

    RETURN

END FUNCTION ListTable_Move2NextPair

!******************************************************************************

SUBROUTINE ListTable_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key
    !  is already stored in the table, replace the old value with the 
    !  new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be added to the collection
    CLASS(*),            INTENT(IN)     :: Key
    !% the associated value to be added to the collection
    TYPE(*),             INTENT(IN)     :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                                    :: KeyFound
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode

! FLOW
    
    ! check whether the key is already stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        KeyFound = FalseVal
    ELSE
        KeyFound = Collection%FindKey(Key, KeyNode)
    END IF
    
    IF (KeyFound) THEN
        ! replace the current value with the new one
        CALL KeyNode%Value%Set(Value)
    ELSE
        ! new key-value pair
        BLOCK
            ! block variables
            tInteger            :: AllocStat
            tCharLen(MsgLen)    :: AllocMsg
            ! allocate new node
            ALLOCATE(TabNode(Collection%ValSize) :: KeyNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrAlloc('ListTable_Insert', ModName, AllocMsg, AllocStat)
            ! set key and value
            CALL KeyNode%SetNew(Key, Value)
            ! append the new node
            CALL Collection%WrkLst%AddLast(KeyNode)
        END BLOCK
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END SUBROUTINE ListTable_Insert

!******************************************************************************

SUBROUTINE ListTable_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection

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
        ! reset cursor by moving cursor backward
        IsTheEnd = Collection%WrkLst%MoveBackward()
        ! check if remove the node successfully or not
        IF (Collection%WrkLst%RemoveNode(CurrNode)) THEN
            ! free pointer/allocatable components of the node
            SELECT TYPE (CurrNode)
            TYPE IS (TabNode(*))
                CALL CurrNode%FreeNode()
            END SELECT
            ! deallocate the node
            DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('ListTable_Delete', ModName, AllocMsg, AllocStat)
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE ListTable_Delete

!******************************************************************************

FUNCTION ListTable_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be removed from the collection
    CLASS(*),            INTENT(IN)     :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode
    tInteger                                    :: AllocStat
    tCharLen(MsgLen)                            :: AllocMsg

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode)
    END IF
    
    IF (Flag) THEN
        ! check if remove the node successfully or not
        IF (Collection%WrkLst%RemoveNode(KeyNode)) THEN
            ! free pointer/allocatable components of the node
            CALL KeyNode%FreeNode()
            ! deallocate the node
            DEALLOCATE(KeyNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('ListTable_Remove', ModName, AllocMsg, AllocStat)
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION ListTable_Remove

!******************************************************************************

FUNCTION ListTable_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be looked for in the collection
    CLASS(*),            INTENT(IN)     :: Key
    !% flag indicating whether the specified key is found or not.
    tLogical                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Collection%IsEmpty()) THEN
        Found = FalseVal
    ELSE
        Found = Collection%FindKey(Key)
    END IF

    RETURN

END FUNCTION ListTable_Contain

!******************************************************************************

FUNCTION ListTable_GetValue(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be looked for in the collection
    CLASS(*),            INTENT(IN)     :: Key
    !% the value associated with the specified key
    TYPE(*),             INTENT(INOUT)  :: Value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode)
    END IF
    
    ! get value if key is found
    IF (Flag) CALL KeyNode%Value%Get(Value)
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION ListTable_GetValue

!******************************************************************************

FUNCTION ListTable_GetKeyPtr(Collection) RESULT(Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a key stored in a symbol table.  The pointer
    !  is intended to be used as a mold for the key (i.e. provides type
    !  of the stored key).  Return null pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)), TARGET, INTENT(IN) :: Collection
    !% pointer to a stored key
    CLASS(*),            POINTER            :: Key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: Head

! FLOW
    
    ! get head
    Head => Collection%WrkLst%GetHead()
    IF (ASSOCIATED(Head)) THEN
        SELECT TYPE (Head)
        TYPE IS (TabNode(*))
            Key => Head%Key
        END SELECT
    ELSE
        Key => NULL()
    END IF
    
    ! free working pointer
    NULLIFY(Head)

    RETURN

END FUNCTION ListTable_GetKeyPtr

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION ListTable_FindKey(Collection, Key, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(ListTable(*)),                  INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),                             INTENT(IN)    :: Key
    !% the node containing the specified key; null pointer if the key is not found
    CLASS(TabNode(*)), OPTIONAL, POINTER, INTENT(OUT)   :: KeyNode
    !% flag indicating whether the specified key is found or not.
    tLogical                                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd

! FLOW
    
    ! initialize
    Found = FalseVal
    IF (PRESENT(KeyNode)) KeyNode => NULL()
    IsTheEnd = Collection%WrkLst%StartFirst(CurrNode)
    
    ! iterate over the collection to find key
    DO WHILE (.NOT.IsTheEnd)
        SELECT TYPE (CurrNode)
        TYPE IS (TabNode(*))
            IF (CurrNode%IsKeyEqual(Key)) THEN
                ! key found
                Found = TrueVal
                IF (PRESENT(KeyNode)) KeyNode => CurrNode
                EXIT
            END IF
        END SELECT
        IsTheEnd = Collection%WrkLst%MoveForward(CurrNode)
    END DO
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END FUNCTION ListTable_FindKey

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE ListTable_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ListTable(*)), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE ListTable_Finalize

!******************************************************************************

END MODULE Class_ListTable

!******************************************************************************


MODULE Class_HashTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HashTable* type, the *TabNode* type and their
!   related routines.  The *TabNode* type is a helper and private type used
!   to store a key-value pair.  The *HashTable* type is a collection type
!   that employs a separate-chaining hash table implementation to provide
!   common operations for an unordered symbol table. <br>
!   Unlike the *ListTable* and *TreeTable* types, which can be used instantly
!   by inserting objects into a collection, the *HashTable* type requires
!   an explicit construction before using other provided operations.  There
!   are two methods provided to create the collection.  The *CreateEmpty*
!   method constructs an empty collection with the specified initial capacity
!   while the *Construct* method constructs a collection based on the specified
!   input (either from an array of key-value pairs or from another collection). <br>
!   As an unordered symbol table, the *HashTable* type allows all Fortran
!   intrinsic types (with the exception of the *LOGICAL* type) to be used
!   as a type of the key to insert a key-value pair into a collection.  If
!   a derived type as a type of the key is desirable, the key type must be
!   a subtype of the *Hashable* type (i.e. in the *Hashable* class) that
!   provides a method to compute the key's hash code.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,                  ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR, C_F_POINTER
    USE, INTRINSIC  :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
    USE ModBase_SIntUtil,               ONLY: ToChar => ToDecStrSigned
    USE Class_IntrusiveLinkedLists,     ONLY: DoublyLinkedNode, IntrusiveLinearList
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_Hashable
    USE Class_BaseCollection
    USE Class_BaseSymTable
    USE Class_GenStore

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash               tIndex
#define     RotateLeft(V,P)     ISHFTC(V,  P)

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_HashTable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128
    ! The number of bits used by 8-bit integer
    tInteger,  PARAMETER    :: Bits_kByte = BIT_SIZE(0_kByte)    ! should be  8 bits
    ! The numbers of bits/bytes used by a character
    tInteger,  PARAMETER    :: Bits_Char  = CHARACTER_STORAGE_SIZE
    tInteger,  PARAMETER    :: Bytes_Char = Bits_Char/Bits_kByte
    ! The maximum (positive) number of hash code
#ifdef Indx32Bits
    tInteger,  PARAMETER    :: MaxHash = ToInteger(Z'7FFFFFFF')
#else
    tLong,     PARAMETER    :: MaxHash  = ToLong(Z'7FFFFFFFFFFFFFFF')
#endif

!** DERIVED TYPE DEFINITIONS
    !> *TabNode* is a doubly-linked-list node type containing key and value as
    !  its components.  An unlimited polymorphic type is used as a storage of
    !  the key while the *GenStore* type is used as a storage of the value. <br>
    !  The *TabNode* type is a subtype of the *DoublyLinkedNode* type and is
    !  intended to be used with the *HashTable* type, which is a collection type
    !  that utilizes the *IntrusiveLinearList* type. <br>
    !  **Note**: This is a private type.
    TYPE, EXTENDS(DoublyLinkedNode) :: TabNode(ValSize)
        !% size of value (data content) in bytes
        tInteger,                LEN        :: ValSize
        !% stored key
        CLASS(*), ALLOCATABLE,   PRIVATE    :: Key
        !% stored value
        TYPE(GenStore(ValSize)), PRIVATE    :: Value
        !% hash code of the stored key
        tHash,                   PRIVATE    :: KeyHash
    CONTAINS
        !> **Type-Bound Subroutine**: SetKeyNHash <br>
        !  **Purpose**:  To store the key and its hash code in the node where
        !                the type of the specified key must be valid. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%SetKeyNHash(Key, HashCode)
        PROCEDURE, PRIVATE  :: SetKeyNHash  => TabNode_SetKeyNHash
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
        !  **Purpose**:  To set new key and its associated hash code and value where
        !                the type of the specified key must be valid. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%SetNew(Key, HashCode, Value)
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
    !> The *HashTable* type is a collection type that employs a separate-chaining
    !  hash table implementation to provide common operations for an unordered
    !  symbol table.  The separate-chaining technique combines a linked list with
    !  a hash table to solve the collision problem.  The *HashTable* type employs
    !  an array of the *IntrusiveLinearList* type as its component (i.e. using
    !  an array of linked lists as its buckets) to store *TabNode* objects.  As an
    !  intrusive container, the *IntrusiveLinearList* type only provides a linking/
    !  unlinking task for common linked-list operations, the memory management task
    !  of the inserted *TabNode* objects is directly handled by the *HashTable* type. <br>
    !  As an unordered symbol table, the *HashTable* type extends the *BaseSymTable*
    !  type directly and implements all deferred procedures required by all its super
    !  classes (including the *Assignable* type and the *BaseCollection* type).  As a
    !  symbol table, the *HashTable* type does not allow duplicated keys; therefore,
    !  if an inserted key is equal to a key stored in the table, an associated value
    !  of the stored key is replaced by an associated value of the inserted key.  <br>
    !  In order to avoid long linked lists (due to many keys hashed to the same
    !  indices), the *HashTable* type employs the dynamic-array technique to resize
    !  the linked-list array.  As a result, the *HashTable* type requires an explicit
    !  construction via either the *CreateEmpty* or the *Construct* method before
    !  using other provided operations.  When the *MemFree* or the *Destruct* method
    !  is called, the hash table also needs to be re-construct because those methods
    !  essentially deallocate the working array.
    TYPE, EXTENDS(BaseSymTable) :: HashTable
        PRIVATE
        !% initial capacity of the hash table
        tIndex                                  :: InitCap = 16_kIndex
        !% current size (number of key-value pairs) of the hash table
        tIndex                                  :: Size = 0_kIndex
        !% current capacity of the hash table
        tIndex                                  :: Capacity = 0_kIndex
        !% working doubly-linked lists
        TYPE(IntrusiveLinearList), ALLOCATABLE  :: WrkLst(:)
        !% current index into the working lists (used for iteration purpose)
        tIndex                                  :: Indx = 0_kIndex
        !% pointer to a hash function
        PROCEDURE(HashFunc), NOPASS, POINTER    :: HashCalc => NULL()
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
        PROCEDURE, PRIVATE  :: FindKey  => HashTable_FindKey
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the collection to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Resize(64)
        PROCEDURE, PRIVATE  :: Resize   => HashTable_Resize
        ! ---------------------------------------------------------------------
        ! -----                   Overridden Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *SymbolTable_CreateByArray* is a procedure to create the collection
        !   from an array of key-value pairs. See the *Construct* method for usage.
        PROCEDURE   :: SymbolTable_CreateByArray
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear        => HashTable_ClearItems
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => HashTable_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => HashTable_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashTable_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => HashTable_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => HashTable_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => HashTable_CopyCollection
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method because the
        !             *WrkLst* component is deallocated.
        PROCEDURE   :: Destruct     => HashTable_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => HashTable_GetSize
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
        PROCEDURE   :: StartFirst   => HashTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => HashTable_Move2NextPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => HashTable_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE   :: Delete       => HashTable_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => HashTable_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => HashTable_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => HashTable_GetValue
        !> **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.
        !                The pointer is intended to be used as a mold for
        !                the key (i.e. provides type of the stored key).
        !                Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE   :: GetKeyPtr    => HashTable_GetKeyPtr
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by HashTable Type             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%CreateEmpty() <br>
        !   --->    CALL Collection%CreateEmpty(InitCap) <br>
        !   --->    CALL Collection%CreateEmpty(HashCal=HashFunction) <br>
        !   --->    CALL Collection%CreateEmpty(InitCap, HashFunction) <br>
        !  **Note**: Any suitable hash function routine from the *ModBase_SimpleHash32*, 
        !       *ModBase_SimpleHash64*, *ModBase_ReferenceHash32*, *ModBase_ReferenceHash64*
        !       *ModBase_OptimalHash32*, and *ModBase_OptimalHash64* modules can be used to
        !       specify the *HashCal* argument.  The term *suitable* means that any routine
        !       that has exactly the same interface as the *HashFunc* abstract function
        !       is the suitable one.  <br>
        PROCEDURE   :: CreateEmpty  => HashTable_CreateEmpty
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: HashTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashTable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !----------------------------------------------------------------------
        !^ *HashFunc* is a generic interface for a procedure to compute the hash value.
        FUNCTION HashFunc(Input, InpSize, StartHash, RemoveSign) RESULT(HashCode)
            IMPORT
            TYPE(*), DIMENSION(..), INTENT(IN)  :: Input        !! input (any type and rank)
            tIndex,                 INTENT(IN)  :: InpSize      !! size of the input (in bytes)
            tHash,    OPTIONAL,     INTENT(IN)  :: StartHash    !! optional starting hash for continued hashing
            tLogical, OPTIONAL,     INTENT(IN)  :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tHash                               :: HashCode     !! hash code
        END FUNCTION HashFunc
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 TabNode Procedures                        -----
! ---------------------------------------------------------------------

SUBROUTINE TabNode_SetKeyNHash(Node, Key, HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the key and its hash code in the Node.
    !  The key must be a valid one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), TARGET, INTENT(INOUT)    :: Node
    !% the key to be stored
    CLASS(*),                  INTENT(IN)       :: Key
    !% the key hash code
    tHash,                     INTENT(IN)       :: HashCode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(Node%Key, SOURCE=Key)
    Node%KeyHash = HashCode

    RETURN

END SUBROUTINE TabNode_SetKeyNHash

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
    CLASS IS (Hashable)
        SELECT TYPE (StoredKey => Node%Key)
        CLASS IS (Hashable)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
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
    CLASS IS (Hashable)
        SELECT TYPE (StoredKey => Node%Key)
        CLASS IS (Hashable)
            Flag = Key%IsEqualTo(StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
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

SUBROUTINE TabNode_SetNew(Node, Key, HashCode, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the new key and its associated hash code and value.
    !  The key must be a valid one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabNode object
    CLASS(TabNode(*)), INTENT(INOUT)    :: Node
    !% the key
    CLASS(*),          INTENT(IN)       :: Key
    !% the key hash code
    tHash,             INTENT(IN)       :: HashCode
    !% the associated value
    TYPE(*),           INTENT(IN)       :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Node%SetKeyNHash(Key, HashCode)
    CALL Node%Value%Set(Value)

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

SUBROUTINE HashTable_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  *Note*:  SrcObj must be in the *HashTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)), INTENT(OUT)    :: DstObj   !! destination object
    CLASS(Assignable),   INTENT(IN)     :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    TYPE IS (HashTable(*))
        IF (DstObj%ValSize == SrcObj%ValSize) THEN
            ! copy scalar components
            DstObj%InitCap = SrcObj%InitCap
            DstObj%Size = SrcObj%Size
            DstObj%Capacity = SrcObj%Capacity
            DstObj%Indx = SrcObj%Indx
            ALLOCATE(DstObj%WrkLst(0_kIndex:DstObj%Capacity-1_kIndex))
            ! add objects stored in the source
            ! -- implementation note:  we cannot use the iteration methods here --
            ! -- due to the intent of the source object.                        --
            BLOCK
                ! block variables
                CLASS(DoublyLinkedNode), POINTER    :: CurrNode
                CLASS(DoublyLinkedNode), POINTER    :: NextNode
                tIndex                              :: I
                ! loop over all working lists
                DO I = 0_kIndex, SrcObj%Capacity-1_kIndex
                    ! start iteration from the head (first node) of the source
                    CurrNode => SrcObj%WrkLst(I)%GetHead()
                    DO WHILE (ASSOCIATED(CurrNode))
                        ! append the current node to the tail of the destination
                        CALL DstObj%WrkLst(I)%AddLast(CurrNode)
                        ! move to the next iteration of the source
                        NextNode => CurrNode%GetNext()
                        CurrNode => NextNode
                    END DO
                END DO
                NULLIFY(CurrNode, NextNode)
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('HashTable_CopyAssign', ModName, ErrSevere, &
                       'The "ValSize" components must have the same value.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashTable_CopyAssign', ModName, ErrSevere, &
                   'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashTable_CopyAssign

!******************************************************************************

SUBROUTINE HashTable_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the HashTable object.  This is a deferred
    !  procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)),            INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the HashTable object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    TYPE IS (HashTable(*))
        ! copy scalar components
        DstObj%InitCap = SrcObj%InitCap
        DstObj%Size = SrcObj%Size
        DstObj%Capacity = SrcObj%Capacity
        DstObj%Indx = SrcObj%Indx
        ALLOCATE(DstObj%WrkLst(0_kIndex:DstObj%Capacity-1_kIndex))
        ! add objects stored in the source
        ! -- implementation note:  we cannot use the iteration methods here --
        ! -- due to the intent of the source object.                        --
        BLOCK
            ! block variables
            CLASS(DoublyLinkedNode), POINTER    :: CurrNode
            CLASS(DoublyLinkedNode), POINTER    :: NextNode
            tIndex                              :: I
            ! loop over all working lists
            DO I = 0_kIndex, SrcObj%Capacity-1_kIndex
                ! start iteration from the head (first node) of the source
                CurrNode => SrcObj%WrkLst(I)%GetHead()
                DO WHILE (ASSOCIATED(CurrNode))
                    ! append the current node to the tail of the destination
                    CALL DstObj%WrkLst(I)%AddLast(CurrNode)
                    ! move to the next iteration of the source
                    NextNode => CurrNode%GetNext()
                    CurrNode => NextNode
                END DO
            END DO
            NULLIFY(CurrNode, NextNode)
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE HashTable_Clone

!******************************************************************************

FUNCTION HashTable_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)), INTENT(IN) :: LhsObj   !! an object
    CLASS(Assignable),   INTENT(IN) :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (HashTable(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%InitCap /= RhsObj%InitCap) RETURN
        IF (LhsObj%Size /= RhsObj%Size) RETURN
        IF (LhsObj%Capacity /= RhsObj%Capacity) RETURN
        IF (LhsObj%Indx /= RhsObj%Indx) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                CLASS(DoublyLinkedNode), POINTER    :: LhsNode, LhsNext
                CLASS(DoublyLinkedNode), POINTER    :: RhsNode, RhsNext
                tIndex                              :: I
                tLogical                            :: ReturnNow
                ReturnNow = FalseVal
                ! loop over all working lists
                OutLoop: DO I = 0_kIndex, LhsObj%Capacity-1_kIndex
                    IF (LhsObj%WrkLst(I)%GetSize() /= RhsObj%WrkLst(I)%GetSize()) THEN
                        ReturnNow = TrueVal
                        EXIT OutLoop
                    END IF
                    ! start iteration for the current working list
                    LhsNode => LhsObj%WrkLst(I)%GetHead()
                    RhsNode => RhsObj%WrkLst(I)%GetHead()
                    InLoop: DO WHILE (ASSOCIATED(LhsNode).AND.ASSOCIATED(RhsNode))
                        LhsNext => LhsNode%GetNext()
                        RhsNext => RhsNode%GetNext()
                        ! check key and value equalities
                        SELECT TYPE (LhsNode)
                        TYPE IS (TabNode(*))
                            SELECT TYPE (RhsNode)
                            TYPE IS (TabNode(*))
                                IF (.NOT.LhsNode%IsEqualTo(RhsNode)) THEN
                                    ReturnNow = TrueVal
                                    EXIT OutLoop
                                END IF
                            END SELECT
                        END SELECT
                        ! move to the next iteration
                        LhsNode => LhsNext
                        RhsNode => RhsNext
                    END DO InLoop
                END DO OutLoop
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

END FUNCTION HashTable_IsEqualTo

!******************************************************************************

SUBROUTINE HashTable_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the HashTable object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Destruct()

    RETURN

END SUBROUTINE HashTable_Free

!******************************************************************************

FUNCTION HashTable_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the HashTable type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)), INTENT(IN) :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'HashTable'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION HashTable_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(HashTable(*)),   INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (HashTable(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            ! copy scalar components
            This%InitCap = Other%InitCap
            This%Size = Other%Size
            This%Capacity = Other%Capacity
            This%Indx = Other%Indx
            IF (ALLOCATED(This%WrkLst)) DEALLOCATE(This%WrkLst)
            ALLOCATE(This%WrkLst(0_kIndex:This%Capacity-1_kIndex))
            BLOCK
                ! block variables
                tLogical                            :: IsTheEnd
                CLASS(DoublyLinkedNode), POINTER    :: CurrNode
                CLASS(DoublyLinkedNode), POINTER    :: NewNode
                tIndex                              :: I
                ! loop over all working lists
                DO I = 0_kIndex, Other%Capacity-1_kIndex
                    ! start iteration from the head (first node) of the source
                    IsTheEnd = Other%WrkLst(I)%StartFirst(CurrNode)
                    DO WHILE (.NOT.IsTheEnd)
                        ALLOCATE(NewNode, SOURCE=CurrNode)
                        ! append the new node to the tail of the destination
                        CALL This%WrkLst(I)%AddLast(NewNode)
                        NULLIFY(NewNode)
                        ! move to the next iteration of the source
                        IsTheEnd = Other%WrkLst(I)%MoveForward(CurrNode)
                    END DO
                END DO
                NULLIFY(CurrNode)
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('HashTable_CopyCollection', ModName, ErrSevere, &
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
                ! explicitly construct the hash table
                CALL This%CreateEmpty(Other%GetSize()/4_kIndex)
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
            CALL Handle_ErrLevel('HashTable_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashTable_CopyCollection', ModName, ErrSevere, &
                   'Type of "Other" must be in the "BaseSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE HashTable_CopyCollection

!******************************************************************************

SUBROUTINE HashTable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg
    tLogical                            :: Success
    tIndex                              :: I

! FLOW
    
    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN
    
    ! loop over all working lists
    DO I = 0_kIndex, Collection%Capacity-1_kIndex
        ! start removing from the tail node of the current work list
        DO WHILE (.NOT.Collection%WrkLst(I)%IsEmpty())
            ! remove the currently last node
            Success = Collection%WrkLst(I)%RemoveLast(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! free components of the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    CALL CurrNode%FreeNode()
                END SELECT
                ! deallocate the node
                DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
                CALL Handle_ErrDealloc('HashTable_ClearItems', ModName, AllocMsg, AllocStat)
            END IF
        END DO
    END DO

    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE HashTable_ClearItems

!******************************************************************************

SUBROUTINE HashTable_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! remove all stored objects
    CALL Collection%Clear()
    
    ! set all components to initial states
    Collection%InitCap = 16_kIndex
    Collection%Size = 0_kIndex
    Collection%Capacity = 0_kIndex
    Collection%Indx = 0_kIndex
    DEALLOCATE(Collection%WrkLst)
    NULLIFY(Collection%HashCalc)

    RETURN

END SUBROUTINE HashTable_Destroy

!******************************************************************************

FUNCTION HashTable_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashTable(*)), INTENT(IN) :: Collection
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Size

    RETURN

END FUNCTION HashTable_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION HashTable_Move2FirstPair(Collection, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.
    !  For the *HashTable*, which is an unordered symbol table, 
    !  the starting pair is the first pair inserted into the first non-empty
    !  work list (the first work list is the list with index = 0) .  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
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

    IF (Collection%IsEmpty()) THEN
        IsEmpty = TrueVal
        RETURN
    ELSE
        IsEmpty = FalseVal
    END IF
    
    Collection%Indx = 0_kIndex
    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! start iteration by looking for the first non-empty working list
            DO WHILE (Collection%WrkLst(Collection%Indx)%StartFirst(CurrNode))
                Collection%Indx = Collection%Indx + 1_kIndex
            END DO
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%GetKey(Key)) THEN
                            CALL Handle_ErrLevel('HashTable_Move2FirstPair', ModName, ErrWarning, &
                                       "Type of the specified key does not match the table's key.")
                        END IF
                    END IF
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration by looking for the first non-empty working list
        DO WHILE (Collection%WrkLst(Collection%Indx)%StartFirst())
            Collection%Indx = Collection%Indx + 1_kIndex
        END DO
    END IF

    RETURN

END FUNCTION HashTable_Move2FirstPair

!******************************************************************************

FUNCTION HashTable_Move2NextPair(Collection, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.
    !  For the *HashTable*, which is an unordered symbol table, 
    !  the next pair is the pair inserted after the previous one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% the next key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the next value as output if requested (and available)
    TYPE(*),  OPTIONAL,  INTENT(INOUT)  :: Value
    !> a flag indicating whether the move to the end of the
    !  collection occurs or not <br>
    ! - true if next pair data is NOT available. <br>
    ! - otherwise next pair data is available.
    tLogical                                    :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    IF (PRESENT(Key).OR.PRESENT(Value)) THEN
        BLOCK
            ! block variable
            CLASS(DoublyLinkedNode), POINTER   :: CurrNode
            ! move to next iteration
            IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
                ! the last working list
                IsTheEnd = Collection%WrkLst(Collection%Indx)%MoveForward(CurrNode)
            ELSE
                ! check if the end of the current working list
                IF (Collection%WrkLst(Collection%Indx)%MoveForward(CurrNode)) THEN
                    ! move to next working list
                    Collection%Indx = Collection%Indx + 1_kIndex
                    IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
                        ! the last working list
                        IsTheEnd = Collection%WrkLst(Collection%Indx)%StartFirst(CurrNode)
                    ELSE
                        ! look for the next non-empty working list
                        IsTheEnd = FalseVal
                        DO WHILE (Collection%WrkLst(Collection%Indx)%StartFirst(CurrNode))
                            Collection%Indx = Collection%Indx + 1_kIndex
                            IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
                                ! the last working list
                                IsTheEnd = Collection%WrkLst(Collection%Indx)%StartFirst(CurrNode)
                                EXIT
                            END IF
                        END DO
                    END IF
                ELSE
                    IsTheEnd = FalseVal
                END IF
            END IF
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key)) THEN
                        IF (.NOT.CurrNode%GetKey(Key)) THEN
                            CALL Handle_ErrLevel('HashTable_Move2FirstPair', ModName, ErrWarning, &
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
        IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
            ! the last working list
            IsTheEnd = Collection%WrkLst(Collection%Indx)%MoveForward()
        ELSE
            ! check if the end of the current working list
            IF (Collection%WrkLst(Collection%Indx)%MoveForward()) THEN
                ! move to next working list
                Collection%Indx = Collection%Indx + 1_kIndex
                IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
                    ! the last working list
                    IsTheEnd = Collection%WrkLst(Collection%Indx)%StartFirst()
                ELSE
                    ! look for the next non-empty working list
                    IsTheEnd = FalseVal
                    DO WHILE (Collection%WrkLst(Collection%Indx)%StartFirst())
                        Collection%Indx = Collection%Indx + 1_kIndex
                        IF (Collection%Indx == (Collection%Capacity-1_kIndex)) THEN
                            ! the last working list
                            IsTheEnd = Collection%WrkLst(Collection%Indx)%StartFirst()
                            EXIT
                        END IF
                    END DO
                END IF
            ELSE
                IsTheEnd = FalseVal
            END IF
        END IF
    END IF

    RETURN

END FUNCTION HashTable_Move2NextPair

!******************************************************************************

SUBROUTINE HashTable_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key
    !  is already stored in the table, replace the old value with the 
    !  new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be added to the collection
    CLASS(*),            INTENT(IN)     :: Key
    !% the associated value to be added to the collection
    TYPE(*),             INTENT(IN)     :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical                                    :: KeyFound
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode
    tHash                                       :: HashCode
    tIndex                                      :: Indx
    tInteger                                    :: AllocStat
    tCharLen(MsgLen)                            :: AllocMsg

! FLOW
    
    ! check if the collection is empty or not
    IF (Collection%IsEmpty()) THEN
        ! no key to be found
        KeyFound = FalseVal
        IF (.NOT.CheckKey_N_ComputeHash(Collection, Key, HashCode)) THEN
            ! key is invalid
            Indx = -1_kIndex
        ELSE
            ! compute the index
            Indx = ComputeIndex(HashCode, Collection%Capacity)
        END IF
    ELSE
        ! check whether the key is already stored in the collection or not
        ! also, get key node and compute key's hash code and index
        KeyFound = Collection%FindKey(Key, KeyNode, HashCode, Indx)
    END IF
    
    IF (KeyFound) THEN
        ! +++ existing key +++
        ! replace the current value with the new one
        CALL KeyNode%Value%Set(Value)
    ELSEIF (Indx /= -1_kIndex) THEN
        ! +++ new key-value pair +++
        ! double the hash table capacity if average size of the lists >= 10
        IF (Collection%Size >= 10_kIndex*Collection%Capacity) THEN
            CALL Collection%Resize(2_kIndex*Collection%Capacity)
        END IF
        ! allocate new node
        ALLOCATE(TabNode(Collection%ValSize) :: KeyNode, STAT=AllocStat, ERRMSG=AllocMsg)
        CALL Handle_ErrAlloc('HashTable_Insert', ModName, AllocMsg, AllocStat)
        ! set key and its associated hash code and value
        CALL KeyNode%SetNew(Key, HashCode, Value)
        ! append the new node
        CALL Collection%WrkLst(Indx)%AddLast(KeyNode)
        ! update collection size
        Collection%Size = Collection%Size + 1_kIndex
    ELSE
        ! +++ invalid type of key +++
        CALL Handle_ErrLevel('HashTable_Insert', ModName, ErrSevere, &
                   'Type of the specified key is NOT valid.')
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END SUBROUTINE HashTable_Insert

!******************************************************************************

SUBROUTINE HashTable_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tInteger                            :: AllocStat
    tCharLen(MsgLen)                    :: AllocMsg
    tLogical                            :: IsTheEnd

! FLOW
    
    ASSOCIATE(Indx => Collection%Indx)
        ! get the cursor node
        CurrNode => Collection%WrkLst(Indx)%GetCursor()
        ! check if the node is associated
        IF (ASSOCIATED(CurrNode)) THEN
            ! reset cursor by moving cursor backward
            IF (Collection%Indx == 0_kIndex) THEN
                ! the first working list
                IsTheEnd = Collection%WrkLst(Collection%Indx)%MoveBackward()
            ELSE
                ! check if the end of the current working list
                IF (Collection%WrkLst(Collection%Indx)%MoveBackward()) THEN
                    ! move to previous working list
                    Collection%Indx = Collection%Indx - 1_kIndex
                    IF (Collection%Indx == 0_kIndex) THEN
                        ! the first working list
                        IsTheEnd = Collection%WrkLst(Collection%Indx)%StartLast()
                    ELSE
                        ! look for the next non-empty working list
                        IsTheEnd = FalseVal
                        DO WHILE (Collection%WrkLst(Collection%Indx)%StartLast())
                            Collection%Indx = Collection%Indx - 1_kIndex
                            IF (Collection%Indx == 0_kIndex) THEN
                                ! the first working list
                                IsTheEnd = Collection%WrkLst(Collection%Indx)%StartLast()
                                EXIT
                            END IF
                        END DO
                    END IF
                END IF
            END IF
            ! check if remove the node successfully or not
            IF (Collection%WrkLst(Indx)%RemoveNode(CurrNode)) THEN
                ! free pointer/allocatable components of the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    CALL CurrNode%FreeNode()
                END SELECT
                ! deallocate the node
                DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
                CALL Handle_ErrDealloc('HashTable_Delete', ModName, AllocMsg, AllocStat)
                ! update collection size
                Collection%Size = Collection%Size - 1_kIndex
                ! halve the hash table capacity if average size of the lists <= 2
                IF ((Collection%Capacity > Collection%InitCap).AND. &
                    (Collection%Size <= 2_kIndex*Collection%Capacity)) THEN
                    CALL Collection%Resize(Collection%Capacity/2_kIndex)
                END IF
            END IF
        END IF
    END ASSOCIATE
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE HashTable_Delete

!******************************************************************************

FUNCTION HashTable_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be removed from the collection
    CLASS(*),            INTENT(IN)     :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode
    tIndex                                      :: Indx
    tInteger                                    :: AllocStat
    tCharLen(MsgLen)                            :: AllocMsg

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, KeyNode, TabIndx=Indx)
    END IF
    
    IF (Flag) THEN
        ! check if remove the node successfully or not
        IF (Collection%WrkLst(Indx)%RemoveNode(KeyNode)) THEN
            ! free pointer/allocatable components of the node
            CALL KeyNode%FreeNode()
            ! deallocate the node
            DEALLOCATE(KeyNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('HashTable_Remove', ModName, AllocMsg, AllocStat)
            ! update collection size
            Collection%Size = Collection%Size - 1_kIndex
            ! halve the hash table capacity if average size of the lists <= 2
            IF ((Collection%Capacity > Collection%InitCap).AND. &
                (Collection%Size <= 2_kIndex*Collection%Capacity)) THEN
                CALL Collection%Resize(Collection%Capacity/2_kIndex)
            END IF
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION HashTable_Remove

!******************************************************************************

FUNCTION HashTable_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
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

END FUNCTION HashTable_Contain

!******************************************************************************

FUNCTION HashTable_GetValue(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
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

END FUNCTION HashTable_GetValue

!******************************************************************************

FUNCTION HashTable_GetKeyPtr(Collection) RESULT(Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a key stored in a symbol table.  The pointer
    !  is intended to be used as a mold for the key (i.e. provides type
    !  of the stored key).  Return null pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), TARGET, INTENT(IN) :: Collection
    !% pointer to a stored key
    CLASS(*),            POINTER            :: Key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: Head

! FLOW
    
    ! get head
    Head => Collection%WrkLst(0)%GetHead()
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

END FUNCTION HashTable_GetKeyPtr

! ---------------------------------------------------------------------
! -----     Specific Procedures for HashTable Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_CreateEmpty(Collection, InitCap, HashCalc)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table with a *power of 2* capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% initial capacity of the hash table (must be a power of 2)
    tIndex, OPTIONAL,    INTENT(IN)     :: InitCap
    !> hash function to compute the hash value of the key;
    !  if not present, use default hash function.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 4_kIndex) THEN
            Collection%InitCap = 4_kIndex
        ELSE
            Collection%InitCap = PowerOfTwo(InitCap)
        END IF
    END IF
    Collection%Capacity = Collection%InitCap
    ALLOCATE(Collection%WrkLst(0:Collection%Capacity-1_kIndex))
    IF (PRESENT(HashCalc)) Collection%HashCalc => HashCalc

    RETURN

CONTAINS

    FUNCTION PowerOfTwo(M) RESULT(N)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To return the smallest power of two greater than or equal to M.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: M    ! specified input
        tIndex              :: N    ! smallest power of two

    !** SUBROUTINE PARAMETER DECLARATIONS:
#ifdef Indx32Bits
        tInteger, PARAMETER :: MaxN = SHIFTL(1_kInteger, 30)
#else
        tLong,    PARAMETER :: MaxN = SHIFTL(1_kLong, 62)
#endif
    
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
    
        N = SHIFTR(-1_kIndex, LEADZ(M-1_kIndex))
        IF (N >= 0) THEN
            IF (N < MaxN) THEN
                N = N + 1
            ELSE
                N = MaxN
            END IF
        ELSE
            N = 1
        END IF
        ! alternative implementation
!        IF (M.AND.(.NOT.(IAND(M, (M - 1_kIndex)) /= 0_kIndex))) THEN
!            N = M
!        ELSE
!            N = 1_kIndex
!            DO WHILE (N < M)
!                N = SHIFTL(N, 1)
!            END DO
!        END IF

        RETURN

    END FUNCTION PowerOfTwo

    !**************************************************************************

END SUBROUTINE HashTable_CreateEmpty

!******************************************************************************

SUBROUTINE SymbolTable_CreateByArray(Collection, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% number of key-value pairs
    tIndex,              INTENT(IN)     :: N
    !% the keys to be added to the collection
    CLASS(*),            INTENT(IN)     :: Keys(:)
    !% the associated values to be added to the collection
    TYPE(*),             INTENT(IN)     :: Values(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
    
    ! create empty symbol table with capacity twice of the key size
    CALL Collection%CreateEmpty(N*2_kIndex)

    ! add key-value pairs to the collection
    DO I = 0, N-1
        CALL Collection%Insert(Keys(I), Values(I))
    END DO
       
    RETURN

END SUBROUTINE SymbolTable_CreateByArray

!******************************************************************************

SUBROUTINE HashTable_Resize(Collection, NewCap)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To resize the capacity of the hash table to the *NewCap* value.
    !  Also, redistribute the stored objects to new working lists
    !  according to their new indices. <br>
    !  Unlike conventional implementation, the key's hash code
    !  is only computed once and stored in the object.  When resizing,
    !  an index into the working lists where the object will be stored
    !  is re-computed based on the stored hash code and new capacity of
    !  the symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashTable object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% new capacity of the hash table
    tIndex,              INTENT(IN)     :: NewCap
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(IntrusiveLinearList), TARGET, ALLOCATABLE  :: TmpLst(:)    ! working variable
    CLASS(DoublyLinkedNode),           POINTER      :: CurrNode
    tIndex                                          :: OldCap, I, J

!** FLOW:
    
    ! move currently stored objects to temporary lists
    OldCap = Collection%Capacity
    CALL MOVE_ALLOC(Collection%WrkLst, TmpLst)
    
    ! allocate working lists to new capacity
    Collection%Capacity = NewCap
    ALLOCATE(Collection%WrkLst(0:Collection%Capacity-1_kIndex))

    ! loop over the temporary lists to move stored objects (nodes)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        ! remove nodes from the current temporary list
        DO WHILE (.NOT.TmpLst(I)%IsEmpty())
            IF (.NOT.TmpLst(I)%RemoveFirst(CurrNode)) EXIT
            ! recompute the index into the working lists
            SELECT TYPE (CurrNode)
            TYPE IS (TabNode(*))
                J = ComputeIndex(CurrNode%KeyHash, Collection%Capacity)
            END SELECT
            ! add the current node to a working list
            CALL Collection%WrkLst(J)%AddLast(CurrNode)
        END DO
    END DO
    
    ! free working variables
    NULLIFY(CurrNode)
    DEALLOCATE(TmpLst)

    RETURN

END SUBROUTINE HashTable_Resize

!******************************************************************************

FUNCTION HashTable_FindKey(Collection, Key, KeyNode, KeyHash, TabIndx) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)),                  INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),                             INTENT(IN)    :: Key
    !% the node containing the specified key; null pointer if the key is not found
    CLASS(TabNode(*)), OPTIONAL, POINTER, INTENT(OUT)   :: KeyNode
    !% hash code of the specified key
    tHash,             OPTIONAL,          INTENT(OUT)   :: KeyHash
    !% index of the working list
    tIndex,            OPTIONAL,          INTENT(OUT)   :: TabIndx
    !% flag indicating whether the specified key is found or not.
    tLogical                                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(DoublyLinkedNode), POINTER    :: CurrNode
    tLogical                            :: IsTheEnd
    tHash                               :: HashCode
    tIndex                              :: Indx

! FLOW
    
    ! initialize
    Found = FalseVal
    IF (PRESENT(KeyNode)) KeyNode => NULL()
    IF (PRESENT(KeyHash)) KeyHash = 0_kIndex
    IF (PRESENT(TabIndx)) TabIndx = -1_kIndex
    
    ! return quickly if type of the given key is valid
    IF (.NOT.CheckKey_N_ComputeHash(Collection, Key, HashCode)) RETURN
    
    ! compute index into the working lists
    Indx = ComputeIndex(HashCode, Collection%Capacity)
    
    ! get optional output
    IF (PRESENT(KeyHash)) KeyHash = HashCode
    IF (PRESENT(TabIndx)) TabIndx = Indx
    
    IsTheEnd = Collection%WrkLst(Indx)%StartFirst(CurrNode)
    
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
        IsTheEnd = Collection%WrkLst(Indx)%MoveForward(CurrNode)
    END DO
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END FUNCTION HashTable_FindKey

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION Murmur3_Hash32(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the Murmur3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    INTENT(IN)    :: Input(0:)    !! input bytes
    tInteger, INTENT(IN)    :: Seed         !! seed
    tInteger                :: HashCode     !! single hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: C1 = ToInteger(Z'CC9E2D51')
    tInteger, PARAMETER :: C2 = ToInteger(Z'1B873593')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H1, K1
    tIndex      :: Length, Remaining, Offset

!** FLOW
        
    ! initialize
    H1 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Offset = 0
        
    ! perform hashing
    DO WHILE (Remaining >= 4)
        ! get input
        K1 = PackFull(Input, Offset)
        ! mixing input with constants
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 13) 
        H1 = H1*5 + ToInteger(Z'E6546B64')
        ! update indices
        Remaining = Remaining - 4
        Offset = Offset + 4
    END DO
        
    ! handle the tail
    IF (Remaining > 0) THEN
        K1 = PackPartial(Input, Offset, Remaining)
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
    END IF
        
    ! finalization
    H1 = IEOR(H1, Length)
    HashCode = FinalMixing(H1)

    RETURN

CONTAINS

    FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To pack tree or fewer bytes of the array 'Buf' at offset 'Off' into the 32-bit
        !  word 'Res', in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  !! buffer
        tIndex, INTENT(IN)  :: Off      !! offset
        tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 3)
        tInteger            :: Res      !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:3)
        tIndex      :: I

    ! FLOW
        
        Wrk(0:Length-1) = Buf(Off:Off+Length-1)
        Wrk(Length:3)   = 0_kByte
        Res = PackFull(Wrk, 0)

        RETURN

    END FUNCTION PackPartial

    !**************************************************************************

    FUNCTION PackFull(Buf, Off) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To pack four bytes of the array 'Buf' at offset 'Off' into the 32-bit word
        !  'Res', in little-endian convention (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  !! buffer
        tIndex, INTENT(IN)  :: Off      !! offset
        tInteger            :: Res      !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: Wrk(0:3)
        tIndex      :: I

    ! FLOW

#define MaskInteger(X)          IAND(ToInteger(X), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(MaskInteger(Val(Off)), SHIFTL(MaskInteger(Val(Off+1)), 8))

        Res = IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))

#undef MaskInteger
#undef UnsignedShort

        RETURN

    END FUNCTION PackFull

    !**************************************************************************

    FUNCTION FinalMixing(Hin) RESULT(HOut)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform final mixing.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(IN)    :: HIn
        tInteger                :: HOut

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
        
        HOut = IEOR(HIn, SHIFTR(HIn, 16))
        HOut = HOut*ToInteger(Z'85EBCA6B')
        HOut = IEOR(HOut, SHIFTR(HOut, 13))
        HOut = HOut*ToInteger(Z'C2B2AE35')
        HOut = IEOR(HOut, SHIFTR(HOut, 16))

        RETURN

    END FUNCTION FinalMixing

    !**************************************************************************

END FUNCTION Murmur3_Hash32

!******************************************************************************

FUNCTION Murmur3_Hash64(Input, Seed) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute hash code using the MurmurHash3 hash algorithm by Austin Appleby.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, TARGET,   INTENT(IN)     :: Input(0:)    !! input bytes
    tLong,           INTENT(IN)     :: Seed         !! seed
    tLong                           :: HashCode     !! hash code

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: C1 = ToLong(Z'87C37B91114253D5')
    tLong, PARAMETER    :: C2 = ToLong(Z'4CF5AD432745937F')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong, POINTER  :: LongVal(:) => NULL()
    tLong           :: H1, H2, K1, K2
    tIndex          :: Length, Remaining, Offset, Index

!** FLOW

! define macros for better performance than using internal procedures
#define K1_Mixing(K) \
    K = K*C1; \
    K = RotateLeft(K, 31); \
    K = K*C2;
#define K2_Mixing(K) \
    K = K*C2; \
    K = RotateLeft(K, 33); \
    K = K*C1;
#define FinalMixing(H) \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToLong(Z'FF51AFD7ED558CCD'); \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToLong(Z'C4CEB9FE1A85EC53'); \
    H = IEOR(H, SHIFTR(H, 33));
    
    ! initialize
    H1 = Seed
    H2 = Seed
    Length = SIZE(Input)
    Remaining = Length
    Index = 1
    CALL C_F_POINTER(C_LOC(Input(0)), LongVal, SHAPE=[Length/8])
        
    ! perform hashing by mixing 16 bytes at a time into the hash
    DO WHILE (Remaining >= 16)
            
        K1 = LongVal(Index)
        K2 = LongVal(Index+1)
        Index = Index + 2
        Remaining = Remaining - 16
            
        K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 27)
        H1 = H1 + H2
        H1 = H1*5_kLong + ToLong(Z'0000000052DCE729')
            
        K2_Mixing(K2)
        H2 = IEOR(H2, K2)
        H2 = RotateLeft(H2, 31)
        H2 = H2 + H1
        H2 = H2*5_kLong + ToLong(Z'0000000038495AB5')
            
    END DO
        
    ! compute offset
    Offset = (Index-1)*8
        
    IF (Remaining > 0) THEN
        K1 = 0_kLong
        K2 = 0_kLong
        SELECT CASE (Remaining)
        CASE (8:15)
            K1 = LongVal(Index)
            K2 = PackPartial(Input, Offset+8, Remaining-8)
        CASE (1:7)
            K1 = PackPartial(Input, Offset, Remaining)
        END SELECT
        K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        K2_Mixing(K2)
        H2 = IEOR(H2, K2)
    END IF
        
    ! Do a few final mixes of the hash to ensure the last few bytes are well-incorporated
    H1 = IEOR(H1, ToLong(Length))
    H2 = IEOR(H2, ToLong(Length))
    H1 = H1 + H2
    H2 = H2 + H1
    FinalMixing(H1)
    FinalMixing(H2)
        
    HashCode = H1 + H2
        
    NULLIFY(LongVal)

! undefine macros
#undef Mixing_K1
#undef Mixing_K2
#undef FinalMixing

    RETURN

CONTAINS

    FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To pack seven or fewer bytes of the array 'Buf' at offset 'Off' into the 64-bit
        !  word 'Res', in little-endian convention (least significant byte first).
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,  INTENT(IN)  :: Buf(0:)  !! buffer
        tIndex, INTENT(IN)  :: Off      !! offset
        tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 7)
        tLong               :: Res      !! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte           :: Wrk(0:7)

    ! FLOW
        
        ! initialize
        Wrk = 0_kByte
        
        ! gather available bytes
        Wrk(0:Length-1) = Buf(Off:Off+Length-1)
        
#define MaskLong(X)     IAND(ToLong(X), Z'00000000000000FF')
        Res =        MaskLong(Wrk(0))      + SHIFTL(MaskLong(Wrk(1)),  8) + &
              SHIFTL(MaskLong(Wrk(2)), 16) + SHIFTL(MaskLong(Wrk(3)), 24) + &
              SHIFTL(MaskLong(Wrk(4)), 32) + SHIFTL(MaskLong(Wrk(5)), 40) + &
              SHIFTL(MaskLong(Wrk(6)), 48) + SHIFTL(MaskLong(Wrk(7)), 56)
#undef MaskLong

        RETURN

    END FUNCTION PackPartial

    !**************************************************************************

END FUNCTION Murmur3_Hash64

!******************************************************************************

FUNCTION CheckKey_N_ComputeHash(Collection, Key, HashCode) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether type of the specified key type is valid or not.
    !  Also, compute the hash code of the key if it is valid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashTable(*)), INTENT(INOUT)  :: Collection
    !% the specified key
    CLASS(*), TARGET,    INTENT(IN)     :: Key
    !% the key hash code
    tHash,               INTENT(OUT)    :: HashCode
    !> flag indicating whether the key is valid or not.
    tLogical                            :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! seed for computation of hash code
#ifdef Indx32Bits
    tInteger, PARAMETER :: Seed = 313131_kInteger
#else
    tLong,    PARAMETER :: Seed = 313131_kLong
#endif

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: KeyBytes(:) => NULL()
    TYPE(C_PTR)     :: KeyPtr
    tInteger        :: ByteSize

! FLOW

    ! Check whether the specified key is valid or not.
    ! If so, set KeyPtr and compute the byte size of the key
    SELECT TYPE(Key)
    CLASS IS (Hashable)
        ! special case #1
        Valid = TrueVal
        HashCode = Key%ComputeHashValue()
        RETURN
    TYPE IS (tCharStar)
        KeyPtr = C_LOC(Key)
        ByteSize = Bytes_Char*LEN(Key)
    TYPE IS (tLong)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tInteger)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tShort)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tByte)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tSingle)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tDouble)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tQuad)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tCmpxSingle)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tCmpxDouble)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    TYPE IS (tCmpxQuad)
        KeyPtr = C_LOC(Key)
        ByteSize = C_SIZEOF(Key)
    CLASS DEFAULT
        ! special case #2
        Valid = FalseVal
        RETURN
    END SELECT
    ! convert the specified key to byte arrays
    CALL C_F_POINTER(KeyPtr, KeyBytes, SHAPE=[ByteSize])
    ! compute hash code
    IF (ASSOCIATED(Collection%HashCalc)) THEN
        HashCode = Collection%HashCalc(KeyBytes, ByteSize, Seed)
    ELSE
#ifdef Indx32Bits
        HashCode = Murmur3_Hash32(KeyBytes, Seed)
#else
        HashCode = Murmur3_Hash64(KeyBytes, Seed)
#endif
    END IF
    HashCode = IEOR(IEOR(IEOR(IEOR(HashCode, (SHIFTR(HashCode, 20))), (SHIFTR(HashCode, 12))), &
                        (SHIFTR(HashCode, 7))),  (SHIFTR(HashCode, 4)))
    ! free pointer
    KeyPtr = C_NULL_PTR
    ! set returned flag
    Valid = TrueVal

    RETURN

END FUNCTION CheckKey_N_ComputeHash

!******************************************************************************

FUNCTION ComputeIndex(HashCode, Capacity) RESULT(Indx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the index of working lists of the hash table for
    !  the specified hash code.  Returns value between 0 and
    !  Capacity-1 (assumes Capacity is a power of 2).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tHash,  INTENT(IN)  :: HashCode
    tIndex, INTENT(IN)  :: Capacity
    tIndex              :: Indx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! remove sign and set the index in the applicable range
    Indx = IAND(IAND(HashCode, MaxHash), Capacity-1_kIndex)

    RETURN

END FUNCTION ComputeIndex

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE HashTable_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashTable(*)), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE HashTable_Finalize

!******************************************************************************

END MODULE Class_HashTable

!******************************************************************************

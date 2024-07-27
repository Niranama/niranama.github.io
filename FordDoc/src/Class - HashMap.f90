
MODULE Class_HashMap

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HashMap* type, the *TabItem* type and their
!   related routines.  The *TabItem* type is a helper and private type used
!   to store a key-value pair.  The *HashMap* type is a collection type
!   that employs a linear-probing hash table implementation to provide
!   common operations for an unordered symbol table. <br>
!   Unlike the *ListTable* and *TreeTable* types, which can be used instantly
!   by inserting objects into a collection, the *HashMap* type requires
!   an explicit construction before using other provided operations.  There
!   are two methods provided to create the collection.  The *CreateEmpty*
!   method constructs an empty collection with the specified initial capacity
!   while the *Construct* method constructs a collection based on the specified
!   input (either from an array of key-value pairs or from another collection). <br>
!   As an unordered symbol table, the *HashMap* type allows all Fortran
!   intrinsic types (with the exception of the *LOGICAL* type) to be used
!   as a type of the key to insert a key-value pair into a collection.  If
!   a derived type as a type of the key is desirable, the key type must be
!   a subtype of the *Hashable* type (i.e. in the *Hashable* class) that
!   provides a method to compute the key's hash code.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,                  ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR, C_F_POINTER
    USE, INTRINSIC  :: ISO_FORTRAN_ENV, ONLY: CHARACTER_STORAGE_SIZE
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_Hashable
    USE Class_BaseCollection
    USE Class_BaseSymTable
    USE Class_GenStore

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HashMap

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash               tIndex
#define     RotateLeft(V,P)     ISHFTC(V,  P)

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_HashMap'
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
    !> *TabItem* is a key-value pair data type containing key and value as
    !  its components.  An unlimited polymorphic type is used as a storage of
    !  the key while the *GenStore* type is used as a storage of the value. <br>
    !  **Note**: This is a private type.
    TYPE :: TabItem(ValSize)
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
        !  **Purpose**:  To store the key and its hash code in the Item where
        !                the type of the specified key must be valid. <br>
        !  **Usage**: <br>
        !   --->    CALL Item%SetKeyNHash(Key, HashCode)
        PROCEDURE, PRIVATE  :: SetKeyNHash  => TabItem_SetKeyNHash
        !> **Type-Bound Function**: GetKey <br>
        !  **Purpose**:  To retrieve the key from the Item.  Also, return a flag
        !                indicating whether the key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Success = Item%GetKey(Key) <br>
        !   --->    IF (.NOT.Item%GetKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: GetKey       => TabItem_GetKey
        !> **Type-Bound Function**: IsKeyEqual <br>
        !  **Purpose**:  To check whether the specified key is equal to the Item's key. <br>
        !  **Usage**: <br>
        !   --->    Flag = Item%IsKeyEqual(Key)
        PROCEDURE, PRIVATE  :: IsKeyEqual   => TabItem_IsKeyEqual
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE, PRIVATE  :: IsEqualTo    => TabItem_IsEqualTo
        !> This is a private procedure. <br>
        !  Use the assignment expression to make a copy of a *TabItem* object.
        PROCEDURE, PRIVATE  :: TabItem_CopyAssign
        !> **Operator Overload**: ASSIGNMENT(=)  <br> 
        !  **Purpose**:  To copy a source object to a destination object. <br>
        !  **Usage**: <br>
        !   --->    DstObj = SrcObj
        GENERIC             :: ASSIGNMENT(=)    => TabItem_CopyAssign
        !> **Type-Bound Subroutine**: SetNew <br>
        !  **Purpose**:  To set new key and its associated hash code and value where
        !                the type of the specified key must be valid. <br>
        !  **Usage**: <br>
        !   --->    CALL Item%SetNew(Key, HashCode, Value)
        PROCEDURE           :: SetNew           => TabItem_SetNew
        !> **Type-Bound Subroutine**: FreeItem <br>
        !  **Purpose**:  To free components of the specified Item.  Optionally,
        !                retrieve the stored key and/or value. <br>
        !  **Usage**: <br>
        !   --->    CALL Item%FreeItem() <br>
        !   --->    CALL Item%FreeItem(Key) <br>
        !   --->    CALL Item%FreeItem(Value=Value) <br>
        !   --->    CALL Item%FreeItem(Key, Value)
        PROCEDURE           :: FreeItem         => TabItem_FreeItem
    END TYPE TabItem
    !> The *HashMap* type is a collection type that employs a linear-probing
    !  hash table implementation to provide common operations for an unordered
    !  symbol table.  The linear-probing is a collision handling technique used
    !  in hashing, where the algorithm looks for the next available slot in the
    !  hash table to store the collided key.  The *HashMap* type employs an array
    !  of the *TabItem* type as its component (i.e. table items as its Items)
    !  to store a key-value pair. <br>
    !  As an unordered symbol table, the *HashMap* type extends the *BaseSymTable*
    !  type directly and implements all deferred procedures required by all its super
    !  classes (including the *Assignable* type and the *BaseCollection* type).  As a
    !  symbol table, the *HashMap* type does not allow duplicated keys; therefore,
    !  if an inserted key is equal to a key stored in the table, an associated value
    !  of the stored key is replaced by an associated value of the inserted key.  <br>
    !  The *HashMap* type employs the dynamic-array technique to resize the *TabItem*
    !  array when the load factor is greater than or equal to 0.5 for an insertion
    !  operation or less than or equal to 0.125 for a removal operation.  As a result,
    !  the *HashMap* type requires an explicit construction via either the *CreateEmpty*
    !  or the *Construct* method before using other provided operations.  When the
    !  *MemFree* or the *Destruct* method is called, the hash table also needs to be
    !  re-construct because those methods essentially deallocate the working array.
    TYPE, EXTENDS(BaseSymTable) :: HashMap
        PRIVATE
        !% current size (number of key-value pairs) of the hash table
        tIndex                                  :: Size = 0_kIndex
        !% current capacity of the hash table
        tIndex                                  :: Capacity = 0_kIndex
        !% modification count
        tIndex                                  :: ModCount = 0_kIndex
        !% working table items
        TYPE(TabItem(ValSize)), ALLOCATABLE     :: Items(:)
        !% current index into the working items (used for iteration purpose)
        tIndex                                  :: Indx = 0_kIndex
        !% the number of keys not yet visited (used for iteration purpose)
        tIndex                                  :: KeyLeft = 0_kIndex
        !% current modification count (used for iteration purpose)
        tIndex                                  :: IterModCount = 0_kIndex
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
        !   --->    Flag = Collection%FindKey(Key, KeyItem) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => HashMap_FindKey
        !> **Type-Bound Function**: FindIndex <br>
        !  **Purpose**:  To find the index of the available slot in the symbol table. <br>
        !  **Usage**: <br>
        !   --->    Indx = Collection%FindIndex(InitIndx)
        PROCEDURE, PRIVATE  :: FindIndex    => HashMap_FindIndex
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the collection to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Resize(64)
        PROCEDURE, PRIVATE  :: Resize       => HashMap_Resize
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
        PROCEDURE   :: Clear        => HashMap_ClearItems
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => HashMap_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => HashMap_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => HashMap_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => HashMap_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => HashMap_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => HashMap_CopyCollection
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method because the
        !             *Items* component is deallocated.
        PROCEDURE   :: Destruct     => HashMap_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => HashMap_GetSize
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
        PROCEDURE   :: StartFirst   => HashMap_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => HashMap_Move2NextPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => HashMap_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE   :: Delete       => HashMap_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => HashMap_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => HashMap_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => HashMap_GetValue
        !> **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.
        !                The pointer is intended to be used as a mold for
        !                the key (i.e. provides type of the stored key).
        !                Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE   :: GetKeyPtr    => HashMap_GetKeyPtr
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by HashMap Type             -----
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
        PROCEDURE   :: CreateEmpty  => HashMap_CreateEmpty
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: HashMap_Finalize
        ! ---------------------------------------------------------------------
    END TYPE HashMap

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
! -----                 TabItem Procedures                        -----
! ---------------------------------------------------------------------

SUBROUTINE TabItem_SetKeyNHash(Item, Key, HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To store the key and its hash code in the Item.
    !  The key must be a valid one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabItem object
    CLASS(TabItem(*)), TARGET, INTENT(INOUT)    :: Item
    !% the key to be stored
    CLASS(*),                  INTENT(IN)       :: Key
    !% the key hash code
    tHash,                     INTENT(IN)       :: HashCode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(Item%Key, SOURCE=Key)
    Item%KeyHash = HashCode

    RETURN

END SUBROUTINE TabItem_SetKeyNHash

!******************************************************************************

FUNCTION TabItem_GetKey(Item, Key) RESULT(Success)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve the key from the Item.  Also, return a flag indicating
    !  whether the key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabItem object
    CLASS(TabItem(*)), INTENT(INOUT)    :: Item
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
    IF (.NOT.ALLOCATED(Item%Key)) THEN
        Success = FalseVal
        RETURN
    END IF
    
    ! get the stored key
    SELECT TYPE(Key)
    CLASS IS (Hashable)
        SELECT TYPE (StoredKey => Item%Key)
        CLASS IS (Hashable)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCharStar)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tLong)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tInteger)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tShort)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tByte)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tSingle)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tDouble)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tQuad)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCmpxSingle)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCmpxDouble)
            Key = StoredKey
            Success = TrueVal
        CLASS DEFAULT
            Success = FalseVal
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (StoredKey => Item%Key)
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

END FUNCTION TabItem_GetKey

!******************************************************************************

FUNCTION TabItem_IsKeyEqual(Item, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is equal to the Item's key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabItem object
    CLASS(TabItem(*)), INTENT(IN)   :: Item
    !% the key to be retrieved
    CLASS(*),          INTENT(IN)   :: Key
    !% true if the keys are equal; otherwise, false.
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check the stored key
    IF (.NOT.ALLOCATED(Item%Key)) THEN
        Flag = FalseVal
        RETURN
    END IF
    
    ! get the stored key
    SELECT TYPE(Key)
    CLASS IS (Hashable)
        SELECT TYPE (StoredKey => Item%Key)
        CLASS IS (Hashable)
            Flag = Key%IsEqualTo(StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCharStar)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tLong)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tInteger)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tShort)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tByte)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tSingle)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tDouble)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tQuad)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCmpxSingle)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCmpxDouble)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (StoredKey => Item%Key)
        TYPE IS (tCmpxQuad)
            Flag = (Key == StoredKey)
        CLASS DEFAULT
            Flag = FalseVal
        END SELECT
    CLASS DEFAULT
        Flag = FalseVal
    END SELECT

    RETURN

END FUNCTION TabItem_IsKeyEqual

!******************************************************************************

FUNCTION TabItem_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not. <br>
    !  It should be noted that this routine uses both key and value
    !  components of the *TabItem* object to check equality. Therefore,
    !  although (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B))
    !  can return false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabItem(*)), INTENT(IN)   :: LhsObj   !! an object
    CLASS(TabItem(*)), INTENT(IN)   :: RhsObj   !! another object
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

END FUNCTION TabItem_IsEqualTo

!******************************************************************************

SUBROUTINE TabItem_SetNew(Item, Key, HashCode, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the new key and its associated hash code and value.
    !  The key must be a valid one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabItem object
    CLASS(TabItem(*)), INTENT(INOUT)    :: Item
    !% the key
    CLASS(*),          INTENT(IN)       :: Key
    !% the key hash code
    tHash,             INTENT(IN)       :: HashCode
    !% the associated value
    TYPE(*),           INTENT(IN)       :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL Item%SetKeyNHash(Key, HashCode)
    CALL Item%Value%Set(Value)

    RETURN

END SUBROUTINE TabItem_SetNew

!******************************************************************************

SUBROUTINE TabItem_FreeItem(Item, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free components of the specified Item.  Optionally, retrieve
    !  the stored key and/or value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TabItem object
    CLASS(TabItem(*)),  INTENT(INOUT)   :: Item
    !% the stored key
    CLASS(*), OPTIONAL, INTENT(OUT)     :: Key
    !% the stored value
    TYPE(*),  OPTIONAL, INTENT(INOUT)   :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! get key if requested
    IF (PRESENT(Key)) THEN
        IF (.NOT.Item%GetKey(Key)) THEN
            CALL Handle_ErrLevel('TabItem_FreeItem', ModName, ErrWarning, &
                       'Type of the specified key is NOT valid.')
        END IF
    END IF
    
    ! get value if requested
    IF (PRESENT(Value)) CALL Item%Value%Get(Value)
    
    ! free Item components
    IF (ALLOCATED(Item%Key)) DEALLOCATE(Item%Key)

    RETURN

END SUBROUTINE TabItem_FreeItem

!******************************************************************************

SUBROUTINE TabItem_CopyAssign(DstItem, SrcItem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% destination item
    CLASS(TabItem(*)), INTENT(OUT)  :: DstItem
    !% destination item
    TYPE(TabItem(*)),  INTENT(IN)   :: SrcItem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(DstItem%Key, SOURCE=SrcItem%Key)
    DstItem%Value   = SrcItem%Value
    DstItem%KeyHash = SrcItem%KeyHash

    RETURN

END SUBROUTINE TabItem_CopyAssign

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

SUBROUTINE HashMap_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  *Note*:  SrcObj must be in the *HashMap* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)), INTENT(OUT)  :: DstObj   !! destination object
    CLASS(Assignable), INTENT(IN)   :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    TYPE IS (HashMap(*))
        IF (DstObj%ValSize == SrcObj%ValSize) THEN
            ! copy scalar components
            DstObj%Size = SrcObj%Size
            DstObj%Capacity = SrcObj%Capacity
            DstObj%Indx = SrcObj%Indx
            ALLOCATE(DstObj%Items(0_kIndex:DstObj%Capacity-1_kIndex))
            ! copy items
            BLOCK
                ! block variables
                tIndex  :: I
                ! loop over all working items
                DO I = 0_kIndex, SrcObj%Capacity-1_kIndex
                    ! copy only items that currently contain a key
                    IF (ALLOCATED(SrcObj%Items(I)%Key)) THEN
                        DstObj%Items(I) = SrcObj%Items(I)
                    END IF
                END DO
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('HashMap_CopyAssign', ModName, ErrSevere, &
                       'The "ValSize" components must have the same value.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashMap_CopyAssign', ModName, ErrSevere, &
                   'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE HashMap_CopyAssign

!******************************************************************************

SUBROUTINE HashMap_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the HashMap object.  This is a deferred
    !  procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)),              INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the HashMap object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    TYPE IS (HashMap(*))
        ! copy scalar components
        DstObj%Size = SrcObj%Size
        DstObj%Capacity = SrcObj%Capacity
        DstObj%Indx = SrcObj%Indx
        ALLOCATE(DstObj%Items(0_kIndex:DstObj%Capacity-1_kIndex))
        ! copy items
        BLOCK
            ! block variables
            tIndex  :: I
            ! loop over all working items
            DO I = 0_kIndex, SrcObj%Capacity-1_kIndex
                ! copy only items that currently contain a key
                IF (ALLOCATED(SrcObj%Items(I)%Key)) THEN
                    DstObj%Items(I) = SrcObj%Items(I)
                END IF
            END DO
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE HashMap_Clone

!******************************************************************************

FUNCTION HashMap_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Assignable), INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (HashMap(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%Size /= RhsObj%Size) RETURN
        IF (LhsObj%Capacity /= RhsObj%Capacity) RETURN
        IF (LhsObj%Indx /= RhsObj%Indx) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            BLOCK
                tIndex      :: I
                tLogical    :: ReturnNow
                ReturnNow = FalseVal
                ! loop over all working items
                Loop: DO I = 0_kIndex, LhsObj%Capacity-1_kIndex
                    IF ((.NOT.ALLOCATED(LhsObj%Items(I)%Key)).AND. &
                        (ALLOCATED(RhsObj%Items(I)%Key))) THEN
                        ReturnNow = TrueVal
                        EXIT Loop
                    ELSEIF ((ALLOCATED(LhsObj%Items(I)%Key)).AND. &
                            (.NOT.ALLOCATED(RhsObj%Items(I)%Key))) THEN
                        ReturnNow = TrueVal
                        EXIT Loop
                    ELSEIF ((ALLOCATED(LhsObj%Items(I)%Key)).AND. &
                            (ALLOCATED(RhsObj%Items(I)%Key))) THEN
                        ! check item equality
                        IF (.NOT.LhsObj%Items(I)%IsEqualTo(RhsObj%Items(I))) THEN
                            ReturnNow = TrueVal
                            EXIT Loop
                        END IF
                    END IF
                END DO Loop
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

END FUNCTION HashMap_IsEqualTo

!******************************************************************************

SUBROUTINE HashMap_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the HashMap object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Destruct()

    RETURN

END SUBROUTINE HashMap_Free

!******************************************************************************

FUNCTION HashMap_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the HashMap type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)), INTENT(IN)   :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'HashMap'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION HashMap_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashMap_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(HashMap(*)),     INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (HashMap(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            ! copy scalar components
            This%Size = Other%Size
            This%Capacity = Other%Capacity
            This%Indx = Other%Indx
            IF (ALLOCATED(This%Items)) DEALLOCATE(This%Items)
            ALLOCATE(This%Items(0_kIndex:This%Capacity-1_kIndex))
            BLOCK
                ! block variables
                tIndex  :: I
                ! loop over all working items
                DO I = 0_kIndex, Other%Capacity-1_kIndex
                    ! copy only items that currently contain a key
                    IF (ALLOCATED(Other%Items(I)%Key)) THEN
                        This%Items(I) = Other%Items(I)
                    END IF
                END DO
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('HashMap_CopyCollection', ModName, ErrSevere, &
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
                CALL This%CreateEmpty(Other%GetSize()*2_kIndex)
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
            CALL Handle_ErrLevel('HashMap_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('HashMap_CopyCollection', ModName, ErrSevere, &
                   'Type of "Other" must be in the "BaseSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE HashMap_CopyCollection

!******************************************************************************

SUBROUTINE HashMap_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                              :: I

! FLOW
    
    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN
    
    ! free all items
    DO I = 0_kIndex, Collection%Capacity-1_kIndex
        CALL Collection%Items(I)%FreeItem()
    END DO
    Collection%Size = 0_kIndex
    Collection%ModCount = Collection%ModCount + 1_kIndex

    RETURN

END SUBROUTINE HashMap_ClearItems

!******************************************************************************

SUBROUTINE HashMap_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    ! remove all stored objects
    CALL Collection%Clear()
    
    ! set all components to initial states
    Collection%Size = 0_kIndex
    Collection%Capacity = 0_kIndex
    Collection%ModCount = 0_kIndex
    Collection%Indx = 0_kIndex
    Collection%KeyLeft = 0_kIndex
    Collection%IterModCount = 0_kIndex
    DEALLOCATE(Collection%Items)
    NULLIFY(Collection%HashCalc)

    RETURN

END SUBROUTINE HashMap_Destroy

!******************************************************************************

FUNCTION HashMap_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HashMap(*)), INTENT(IN)   :: Collection
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%Size

    RETURN

END FUNCTION HashMap_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION HashMap_Move2FirstPair(Collection, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.  For the *HashMap*,
    !  which is an unordered symbol table, the starting pair is a pair inserted into
    !  the first non-empty item of its' *Items* component.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)),   INTENT(INOUT)  :: Collection
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
    Collection%KeyLeft = Collection%Size
    Collection%IterModCount = Collection%ModCount
    ! start iteration by looking for the first non-empty slot
    DO WHILE (.NOT.ALLOCATED(Collection%Items(Collection%Indx)%Key))
        Collection%Indx = Collection%Indx + 1_kIndex
    END DO
    Collection%KeyLeft = Collection%KeyLeft - 1_kIndex
    
    ! get key if requested
    IF (PRESENT(Key)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%GetKey(Key)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                        "Type of the specified key does not match the table's key.")
        END IF
    END IF
    
    ! get value if requested
    IF (PRESENT(Value)) CALL Collection%Items(Collection%Indx)%Value%Get(Value)

    RETURN

END FUNCTION HashMap_Move2FirstPair

!******************************************************************************

FUNCTION HashMap_Move2NextPair(Collection, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.  For the *HashMap*,
    !  which is an unordered symbol table, the next pair is a pair inserted
    !  in the first non-empty item after the previous one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)),   INTENT(INOUT)  :: Collection
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

    IF (Collection%IterModCount /= Collection%ModCount) THEN
        CALL Handle_ErrLevel('HashMap_Move2NextPair', ModName, ErrWarning, &
                 "Must re-start the iteration because the stored items have been altered.")
        RETURN
    END IF
    
    ! check for empty table
    IF (Collection%IsEmpty()) THEN
        IsTheEnd = TrueVal
        RETURN
    ELSEIF (Collection%KeyLeft == 0_kIndex) THEN
        IsTheEnd = TrueVal
        RETURN
    END IF

    ! update index and check if we reach the end of the table
    Collection%Indx = Collection%Indx + 1
    IsTheEnd = FalseVal
    
    ! start iteration by looking for the first non-empty slot
    DO WHILE (.NOT.ALLOCATED(Collection%Items(Collection%Indx)%Key))
        Collection%Indx = Collection%Indx + 1_kIndex
    END DO
    Collection%KeyLeft = Collection%KeyLeft - 1_kIndex
    
    ! get key if requested
    IF (PRESENT(Key)) THEN
        IF (.NOT.Collection%Items(Collection%Indx)%GetKey(Key)) THEN
            CALL Handle_ErrLevel('HashMap_Move2FirstPair', ModName, ErrWarning, &
                        "Type of the specified key does not match the table's key.")
        END IF
    END IF
    
    ! get value if requested
    IF (PRESENT(Value)) CALL Collection%Items(Collection%Indx)%Value%Get(Value)

    RETURN

END FUNCTION HashMap_Move2NextPair

!******************************************************************************

SUBROUTINE HashMap_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key
    !  is already stored in the table, replace the old value with the 
    !  new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the key to be added to the collection
    CLASS(*),          INTENT(IN)       :: Key
    !% the associated value to be added to the collection
    TYPE(*),           INTENT(IN)       :: Value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: KeyFound
    tHash       :: HashCode
    tIndex      :: Indx

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
        ! also, compute key's hash code and index
        KeyFound = Collection%FindKey(Key, HashCode, Indx)
    END IF
    
    IF (KeyFound) THEN
        ! +++ existing key +++
        ! replace the current value with the new one
        CALL Collection%Items(Indx)%Value%Set(Value)
        Collection%ModCount = Collection%ModCount + 1_kIndex
    ELSEIF (Indx /= -1_kIndex) THEN
        ! +++ new key-value pair +++
        ! double the hash table capacity if 50% full
        IF (Collection%Size >= Collection%Capacity/2_kIndex) THEN
            CALL Collection%Resize(2_kIndex*Collection%Capacity)
        END IF
        ! find available slot
        Indx = Collection%FindIndex(Indx)
        ! set key and its associated hash code and value
        CALL Collection%Items(Indx)%SetNew(Key, HashCode, Value)
        ! update collection size
        Collection%Size = Collection%Size + 1_kIndex
        Collection%ModCount = Collection%ModCount + 1_kIndex
    ELSE
        ! +++ invalid type of key +++
        CALL Handle_ErrLevel('HashMap_Insert', ModName, ErrSevere, &
                   'Type of the specified key is NOT valid.')
    END IF
    
    RETURN

END SUBROUTINE HashMap_Insert

!******************************************************************************

SUBROUTINE HashMap_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Success

! FLOW
    
    ASSOCIATE(Indx => Collection%Indx)
        IF (ALLOCATED(Collection%Items(Indx)%Key)) THEN
            ! remove the key-value pair
            Success = Collection%Remove(Collection%Items(Indx)%Key)
        END IF
    END ASSOCIATE
    ! reset by moving the index backward by 1
    Collection%Indx = Collection%Indx - 1_kIndex

    RETURN

END SUBROUTINE HashMap_Delete

!******************************************************************************

FUNCTION HashMap_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the key to be removed from the collection
    CLASS(*),          INTENT(IN)       :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabItem(Collection%ValSize))   :: CurrItem ! working variable
    tIndex                              :: Indx, NewID

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, TabIndx=Indx)
    END IF
    
    IF (Flag) THEN
        ! remove the key-value pair
        CALL Collection%Items(Indx)%FreeItem()
        ! rehash all keys in same cluster
        Indx = Indx + 1_kIndex
        IF (Indx == Collection%Capacity) Indx = 0_kIndex
        DO WHILE (ALLOCATED(Collection%Items(Indx)%Key))
            ! remove current item and then reinsert it
            CurrItem = Collection%Items(Indx)
            CALL Collection%Items(Indx)%FreeItem()
            ! find available slot
            NewID = Collection%FindIndex(ComputeIndex(CurrItem%KeyHash, Collection%Capacity))
            ! insert current item back to the hash table
            Collection%Items(NewID) = CurrItem
            CALL CurrItem%FreeItem()
            ! update index
            Indx = Indx + 1_kIndex
            IF (Indx == Collection%Capacity) Indx = 0_kIndex
        END DO
        ! update collection size
        Collection%Size = Collection%Size - 1_kIndex
        Collection%ModCount = Collection%ModCount + 1_kIndex
        ! halve the hash table capacity if it is 12.5% full or less
        IF ((Collection%Size > 0_kIndex).AND.(Collection%Size <= Collection%Capacity/8)) THEN
            CALL Collection%Resize(Collection%Capacity/2_kIndex)
        END IF
    END IF

    RETURN

END FUNCTION HashMap_Remove

!******************************************************************************

FUNCTION HashMap_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the key to be looked for in the collection
    CLASS(*),          INTENT(IN)       :: Key
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

END FUNCTION HashMap_Contain

!******************************************************************************

FUNCTION HashMap_GetValue(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the key to be looked for in the collection
    CLASS(*),          INTENT(IN)       :: Key
    !% the value associated with the specified key
    TYPE(*),           INTENT(INOUT)    :: Value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: Indx

! FLOW
    
    ! check whether the key is stored in the collection or not
    IF (Collection%IsEmpty()) THEN
        Flag = FalseVal
    ELSE
        Flag = Collection%FindKey(Key, TabIndx=Indx)
    END IF
    
    ! get value if key is found
    IF (Flag) CALL Collection%Items(Indx)%Value%Get(Value)

    RETURN

END FUNCTION HashMap_GetValue

!******************************************************************************

FUNCTION HashMap_GetKeyPtr(Collection) RESULT(Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a key stored in a symbol table.  The pointer
    !  is intended to be used as a mold for the key (i.e. provides type
    !  of the stored key).  Return null pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), TARGET, INTENT(IN)   :: Collection
    !% pointer to a stored key
    CLASS(*),          POINTER              :: Key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
    
    ! find the first key stored
    DO I = 0_kIndex, Collection%Capacity-1_kIndex
        IF (ALLOCATED(Collection%Items(I)%Key)) THEN
            Key => Collection%Items(I)%Key
            EXIT
        END IF
    END DO

    RETURN

END FUNCTION HashMap_GetKeyPtr

! ---------------------------------------------------------------------
! -----     Specific Procedures for HashMap Type       -----
! ---------------------------------------------------------------------

SUBROUTINE HashMap_CreateEmpty(Collection, InitCap, HashCalc)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To create an empty hash table with a *power of 2* capacity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% initial capacity of the hash table (must be a power of 2)
    tIndex, OPTIONAL,  INTENT(IN)       :: InitCap
    !> hash function to compute the hash value of the key;
    !  if not present, use default hash function.
    PROCEDURE(HashFunc), OPTIONAL       :: HashCalc
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    IF (PRESENT(InitCap)) THEN
        IF (InitCap < 4_kIndex) THEN
            Collection%Capacity = 4_kIndex
        ELSE
            Collection%Capacity = PowerOfTwo(InitCap)
        END IF
    ELSE
        Collection%Capacity = 16_kIndex
    END IF
    ALLOCATE(Collection%Items(0:Collection%Capacity-1_kIndex))
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

END SUBROUTINE HashMap_CreateEmpty

!******************************************************************************

SUBROUTINE SymbolTable_CreateByArray(Collection, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% HashMap object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% number of key-value pairs
    tIndex,            INTENT(IN)       :: N
    !% the keys to be added to the collection
    CLASS(*),          INTENT(IN)       :: Keys(:)
    !% the associated values to be added to the collection
    TYPE(*),           INTENT(IN)       :: Values(:)

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

SUBROUTINE HashMap_Resize(Collection, NewCap)

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
    !% HashMap object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% new capacity of the hash table
    tIndex,            INTENT(IN)       :: NewCap
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabItem(Collection%ValSize)), TARGET, ALLOCATABLE  :: TmpItems(:)    ! temporary variable
    tIndex                                                  :: OldCap, I, J

!** FLOW:
    
    ! move currently stored objects to temporary variable
    OldCap = Collection%Capacity
    CALL MOVE_ALLOC(Collection%Items, TmpItems)
    
    ! allocate working items to new capacity
    Collection%Capacity = NewCap
    ALLOCATE(Collection%Items(0:Collection%Capacity-1_kIndex))

    ! loop over the temporary lists to move stored objects (Items)
    ! back to the working lists of the hash table
    DO I = 0_kIndex, OldCap-1_kIndex
        IF (ALLOCATED(TmpItems(I)%Key)) THEN
            ! find available slot
            J = Collection%FindIndex(ComputeIndex(TmpItems(I)%KeyHash, Collection%Capacity))
            ! insert key and value back to the hash table
            Collection%Items(J) = TmpItems(I)
            ! free current item of the temporary variable
            CALL TmpItems(I)%FreeItem()
        END IF
    END DO
    
    ! free temporary variable
    DEALLOCATE(TmpItems)

    RETURN

END SUBROUTINE HashMap_Resize

!******************************************************************************

FUNCTION HashMap_FindKey(Collection, Key, KeyHash, TabIndx) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the key to be looked for in the collection
    CLASS(*),          INTENT(IN)       :: Key
    !% hash code of the specified key
    tHash,  OPTIONAL,  INTENT(OUT)      :: KeyHash
    !> If found, index of the Items where the key is stored.
    !  If not found, the index of available slot
    !  If key invalid, return negative value (i.e. -1)
    tIndex, OPTIONAL,  INTENT(OUT)      :: TabIndx
    !% flag indicating whether the specified key is found or not.
    tLogical                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tHash       :: HashCode
    tIndex      :: Indx

! FLOW
    
    ! initialize
    Found = FalseVal
    IF (PRESENT(KeyHash)) KeyHash = 0_kIndex
    IF (PRESENT(TabIndx)) TabIndx = -1_kIndex
    
    ! return quickly if type of the given key is valid
    IF (.NOT.CheckKey_N_ComputeHash(Collection, Key, HashCode)) RETURN
    
    ! compute the initial index
    Indx = ComputeIndex(HashCode, Collection%Capacity)

    ! check whether key is stored in the table or not
    DO WHILE (ALLOCATED(Collection%Items(Indx)%Key))
        IF (Collection%Items(Indx)%IsKeyEqual(Key)) THEN
            Found = TrueVal
            EXIT
        END IF
        ! update index
        Indx = Indx + 1_kIndex
        ! wrap around if necessary
        IF (Indx == Collection%Capacity) Indx = 0_kIndex
    END DO
    
    ! get optional output
    IF (PRESENT(KeyHash)) KeyHash = HashCode
    IF (PRESENT(TabIndx)) TabIndx = Indx

    RETURN

END FUNCTION HashMap_FindKey

!******************************************************************************

FUNCTION HashMap_FindIndex(Collection, Indx) RESULT(TabIndx)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the index of an available slot in a symbol table.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the initial index
    tIndex,            INTENT(IN)       :: Indx
    !% the index of an available slot
    tIndex                              :: TabIndx

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set initial index
    TabIndx = Indx
    DO
        ! if the available slot found, exit the loop
        IF (.NOT.ALLOCATED(Collection%Items(TabIndx)%Key)) EXIT
        ! update index
        TabIndx = TabIndx + 1_kIndex
        ! wrap around if necessary
        IF (TabIndx == Collection%Capacity) TabIndx = 0_kIndex
    END DO

    RETURN

END FUNCTION HashMap_FindIndex

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
    CLASS(HashMap(*)), INTENT(INOUT)    :: Collection
    !% the specified key
    CLASS(*), TARGET,  INTENT(IN)       :: Key
    !% the key hash code
    tHash,             INTENT(OUT)      :: HashCode
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
    tIndex          :: ByteSize

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
    !^ To compute the index of working items of the hash table for
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

SUBROUTINE HashMap_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(HashMap(*)), INTENT(INOUT) :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE HashMap_Finalize

!******************************************************************************

END MODULE Class_HashMap

!******************************************************************************

    ! default capacity
    tIndex,    PARAMETER    :: DefaultCapacity   = 7
    ! default load factor
    tDouble,   PARAMETER    :: DefaultLoadFactor = 0.65
    tInteger,  PARAMETER    :: LinearProbing     = 1
    tInteger,  PARAMETER    :: QuadraticProbing  = 2
    tInteger,  PARAMETER    :: DoubleHashing     = 3
    ! This is the linear constant used in the linear probing, it can be
    ! any positive number. The table capacity will be adjusted so that
    ! the GCD(capacity, LinearConstant) = 1 so that all buckets can be probed.
    tIndex,    PARAMETER    :: LinearConstant    = 17_kIndex
    ! seed for computation of hash code
#ifdef Indx32Bits
    tInteger,  PARAMETER    :: HashSeed = 313131_kInteger
#else
    tLong,     PARAMETER    :: HashSeed = 313131_kLong
#endif
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *TabItem* is a key-value pair data type containing key and value as
    !  its components.
    TYPE TabItem
        PRIVATE
        KeyTypeA                :: Key
        CLASS(*), ALLOCATABLE   :: Value
    END TYPE TabItem
    !> The *HashTable* type is a container type that employs an open-addressing hash
    !  table implementation to provide common operations for an unordered symbol table.
    TYPE HashTable
        PRIVATE
        !% current capacity of the hash table
        tIndex                      :: Capacity = DefaultCapacity
        !% working table items used to store key-value pairs
        TYPE(TabItem), ALLOCATABLE  :: Items(:)
        !% current index into the working items (used for iteration purpose)
        tIndex                      :: Indx = 0_kIndex
        !% the number of keys not yet visited (used for iteration purpose)
        tIndex                      :: KeyLeft = 0_kIndex
        !% current modification count (used for iteration purpose)
        tIndex                      :: IterModCount = 0_kIndex
        !% pointer to a hash function
        PROCEDURE(HashFunc), NOPASS, POINTER    :: HashCalc => NULL()
        !% load factor
        tDouble                     :: LoadFactor = DefaultLoadFactor
        !% threshold for resizing
        tIndex                      :: Threshold = 0_kIndex
        !% modification count
        tIndex                      :: ModCount = 0_kIndex
        !% the total number of used buckets inside the hash table (including cells marked as deleted).
        tIndex                      :: UsedBuckets = 0_kIndex
        !% the total number of unique keys currently inside the hash table.
        tIndex                      :: KeyCount = 0_kIndex
        !% probing algorithm
        tInteger                    :: ProbAlgo = LinearProbing
        !% index for double hashing
        tHash                       :: HashIndx = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To find the specified key in the table.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%FindKey(Key, Value) <br>
        !   --->    IF (.NOT.Table%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey      => HashTable_FindKey
        !> **Type-Bound Subroutine**: Resize <br>
        !  **Purpose**:  To resize the table to the specified capacity. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Resize(64)
        PROCEDURE, PRIVATE  :: Resize       => HashTable_Resize
        ! ---------------------------------------------------------------------
        ! -----                      Public Procedures                    -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateEmpty <br>
        !  **Purpose**:  To construct an empty table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%CreateEmpty()                           ! use default options  <br>
        !   --->    CALL Table%CreateEmpty(InitCap=25)                 ! specify initial capacity <br>
        !   --->    CALL Table%CreateEmpty(LoadFactor=0.5)             ! specify load factor <br>
        !   --->    CALL Table%CreateEmpty(ProbAlgo=2)                 ! specify probing algorithm <br>
        !   --->    CALL Table%CreateEmpty(HashCal=Murmur3_Hash32_Opt) ! specify hash function <br>
        !   --->    CALL Table%CreateEmpty(30, 0.75, 3, XX_Hash64_Opt) ! specify all options <br>
        !  **Note**: Any suitable hash function routine from the *ModBase_SimpleHash32*, 
        !       *ModBase_SimpleHash64*, *ModBase_ReferenceHash32*, *ModBase_ReferenceHash64*
        !       *ModBase_OptimalHash32*, and *ModBase_OptimalHash64* modules can be used to
        !       specify the *HashCal* argument.  The term *suitable* means that any routine
        !       that has exactly the same interface as the *HashFunc* abstract function
        !       is the suitable one.  <br>
        !  **Note2**: Depending on a type of indices defined in the '*Macro - Basic Definitions.f90*'
        !       file, a 32-bit hash-function routine is a suitable one for 32-bit integer indices
        !       while a 64-bit hash-function routine is a suitable one for 64-bit integer indices.
        !       This is a compile-time choice.  <br>
        PROCEDURE   :: CreateEmpty  => HashTable_CreateEmpty
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a hash table from the specified key and value arrays. <br>
        !  **Usage**: <br>
        !   ! use default options  <br>
        !   --->    CALL Table%Construct(40, KeyArr, ValArr) <br>
        !   ! specify all options (initial capacity is twice of the size of the given arrays) <br>
        !   --->    CALL Table%Construct(20, KeyArr, ValArr, LoadFactor, ProbAlgo, HashFunc) <br>
        PROCEDURE   :: Construct    => HashTable_CreateByArray
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To free components of the items from the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Clear()
        PROCEDURE   :: Clear        => HashTable_ClearItems
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To free all memory currently used by the table.<br>
        !  **Usage**: <br>
        !   --->    CALL Table%Destruct() <br>
        !  **Note**:  This method is NOT equivalent to the *Clear* method.  After the *Clear* method
        !       is called, other methods (the *Insert* method in particular) can be immediately used.
        !       However, after the *Destruct* method is called, the *Construct* method must be called
        !       again before other methods can be used. <br>
        PROCEDURE   :: Destruct     => HashTable_Destroy
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the table. <br>
        !  **Usage**: <br>
        !   --->    CALL Table%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => HashTable_Insert
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from the table.
        !                Also, return a flag indicating whether the key-value pair is successfully
        !                removed or not.  Optionally, retrieve the associated value if the key
        !                exists in the table.  <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Remove(Key, Value) <br>
        !   --->    IF (.NOT.Table%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => HashTable_Remove
        ! -------------------------------------------------------
        ! -----           tree-traversing procedures        -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration and return a flag indicating whether
        !                the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Table%StartFirst() <br>
        !   --->    IsEmpty = Table%StartFirst(FirstKey) <br>
        !   --->    IsEmpty = Table%StartFirst(Value=FirstVal) <br>
        !   --->    IsEmpty = Table%StartFirst(FirstKey, FirstVal)
        PROCEDURE   :: StartFirst   => HashTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the table or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Table%MoveForward() <br>
        !   --->    IsTheEnd = Table%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Table%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Table%MoveForward(NextKey, NextVal) <br>
        !  **Important Note**: <br>
        !   After the start of the current iteration (i.e. a call to the *StartFirst* method),
        !   a user should not insert or remove any key-value pair.  Otherwise, the *MoveForward*
        !   method is not valid for the current iteration and the user must re-start the iteration
        !   in order to use the *MoveForward* method.
        PROCEDURE   :: MoveForward  => HashTable_Move2NextPair
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the table is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%IsEmpty() <br>
        !   --->    IF (.NOT.Table%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty      => HashTable_IsEmpty
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size (number of keys and their associated values) of the table. <br>
        !  **Usage**: <br>
        !   --->    Size = Table%GetSize()
        PROCEDURE   :: GetSize      => HashTable_GetSize
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the table.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%Contain(Key) <br>
        !   --->    IF (.NOT.Table%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => HashTable_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the table.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Table%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Table%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => HashTable_GetValue
        !> **Type-Bound Subroutine**: GetKeys <br>
        !  **Purpose**:  To return a queue of all keys and optionally a queue of all associated values. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%GetKeys(KeyQueue) <br>
        !   --->    CALL Tree%GetKeys(KeyQueue, ValueQueue) <br>
        PROCEDURE   :: GetKeys      => HashTable_GetAllKeys
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the hash table.
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

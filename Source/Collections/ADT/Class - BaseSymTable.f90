
MODULE Class_BaseSymTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseSymTable* type and its related routines.
!   The *BaseSymTable* type is an abstract collection type representing a
!   symbol table, which is a collection that associates a *value* with a
!   *key*.  The user can insert key-value pairs into the symbol table with
!   the expectation of later being able to search for the value associated
!   with a given key. <br>
!   The *BaseSymTable* type extends the *BaseCollection* type to define
!   additional methods for various common operations of a symbol table.  All
!   other symbol table types (unordered or ordered ones) should extend from
!   this base type.  By design, the *BaseSymTable* type is a parameterized
!   derive type where all its subtypes utilize the *GenStore* type as a
!   generic data storage of a value for a key-value pair inserted into a
!   collection.  However, the type of a storage of a key varies depending
!   on the type of the symbol table. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE Class_BaseCollection

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC  :: BaseSymTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_BaseSymTable'

!** DERIVED TYPE 
    !> The *BaseSymTable* type is an abstract collection type that
    !  defines an API for a symbol table, which is a collection
    !  that associates a *value* with a *key*. <br>
    !  The *BaseSymTable* type and its subclasses are intended to be
    !  used with the <a href="../module/class_genstore.html#type-genstore">GenStore</a>
    !  type, which is a generic data storage type.  The *GenStore* type
    !  used to store a value of an inserted key-value pair can store
    !  various data type providing that the size (in bytes) of the data
    !  to be stored is known at compile time. <br>
    !  As a result, the *BaseSymTable* type and all its subclasses are
    !  parameterized collection types that requires a known byte size of
    !  the value to be stored in a collection.  This known byte size must
    !  be specified at compile time when a collection type is declared. <br>
    !  The following code snippet shows various ways to declare the
    !  *BaseSymTable* type and its subclasses.
    !   <Pre><Code style="color:MidnightBlue;">
    !   ! declare an allocatable variable of the <em>BaseSymTable</em> type
    !   ! and then allocate it to one of its concrete subtypes
    !   TYPE Point2D
    !       REAL    :: X, Y
    !   END TYPE
    !   INTEGER, PARAMETER :: PointSize = SIZEOF(Point2D(0.0, 0.0)) ! byte size of Point2D
    !   CLASS(BaseSymTable(:)), ALLOCATABLE :: Collection
    !   ALLOCATE(HashTable(PointSize) :: Collection)    ! a hash table of key-value pairs where Point2D is the value type
    !
    !   ! or directly declare variables of its concrete subtypes
    !   INTEGER, PARAMETER :: IntSize = C_SIZEOF(0)     ! byte size of default integer
    !   INTEGER, PARAMETER :: RealSize = C_SIZEOF(0.0)  ! byte size of default real
    !   TYPE(TreeTable(IntSize))    ::  IntTree         ! a tree table of key-value pairs where integer is the value type
    !   TYPE(ListTable(RealSize))   ::  RealList        ! a list table of key-value pairs where real is the value type
    !   </Code></Pre>
    TYPE, ABSTRACT, EXTENDS(BaseCollection) :: BaseSymTable(ValSize)
        !% size of data content (value) in bytes
        tInteger, LEN   :: ValSize
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *StartFirst* is a binding name of the *Move2FirstPair* deferred procedure. <br>
        !  **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration and return a flag indicating whether
        !                the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartFirst() <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey) <br>
        !   --->    IsEmpty = Collection%StartFirst(Value=FirstVal) <br>
        !   --->    IsEmpty = Collection%StartFirst(FirstKey, FirstVal)
        PROCEDURE(Move2FirstPair), DEFERRED :: StartFirst
        !> *MoveForward* is a binding name of the *Move2NextPair* deferred procedure. <br>
        !  **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE(Move2NextPair),  DEFERRED :: MoveForward
        !> *Insert* is a binding name of the *AddPair* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE(AddPair),        DEFERRED :: Insert
        !> *Delete* is a binding name of the *DelPair* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE(DelPair),        DEFERRED :: Delete
        !> *Remove* is a binding name of the *DelKey* deferred procedure. <br>
        !  **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE(DelKey),         DEFERRED :: Remove
        !> *Contain* is a binding name of the *FindKey* deferred procedure. <br>
        !  **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE(FindKey),        DEFERRED :: Contain
        !> *GetValue* is a binding name of the *GetVal* deferred procedure. <br>
        !  **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE(GetVal),         DEFERRED :: GetValue
        !> *GetKeyPtr* is a binding name of the *GetKey* deferred procedure. <br>
        !  **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.
        !                The pointer is intended to be used as a mold for
        !                the key (i.e. provides type of the stored key).
        !                Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE(GetKey),         DEFERRED :: GetKeyPtr
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Clear <br>
        !  **Purpose**:  To remove all key-value pairs from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Clear()
        PROCEDURE   :: Clear    => BaseSymTable_RemoveAllPairs
        ! ---------------------------------------------------------------------
        !> *SymbolTable_CreateByArray* is an procedure to create the collection
        !   from an array of key-value pairs. See the *Construct* method for usage.
        PROCEDURE   :: SymbolTable_CreateByArray
        !> **Type-Bound Subroutine**: Construct <br>
        !  **Purpose**:  To construct a new collection from an array of key-value
        !                pairs or from another collection. <br>
        !  **Usage**: <br>
        !           ! create a collection from an array of 25 key-value pairs <br>
        !   --->    CALL Collection%Construct(25, Keys, Values) <br>
        !           ! create a collection from another collection <br>
        !   --->    CALL Collection%Construct(OtherCollection)
        GENERIC     :: Construct    => SymbolTable_CreateByArray
        ! ---------------------------------------------------------------------
    END TYPE BaseSymTable

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> Move2FirstPair is a deferred procedure to move to the first
        !  (starting) pair data in a symbol table.  It is provided for
        !  an iteration over all key-value pairs in a symbol table. <br>
        !  For an ordered symbol table, the first pair data normally
        !  means the key-value pair with the smallest key.  For an
        !  unordered symbol table, the first pair data can be any
        !  key-value pair.
        FUNCTION Move2FirstPair(Collection, Key, Value) RESULT(IsEmpty)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the first key as output if requested (and available)
            CLASS(*), OPTIONAL,     INTENT(OUT)     :: Key
            !% the first value as output if requested (and available)
            TYPE(*),  OPTIONAL,     INTENT(INOUT)   :: Value
            !> a flag indicating whether the collection contains no pair data or not <br>
            ! - true if the collection is empty. <br>
            ! - otherwise the first pair data is available.
            tLogical                                :: IsEmpty
        END FUNCTION
        !> Move2NextPair is a deferred procedure to move to the next pair
        !  data in a symbol table.  It is provided for an iteration over
        !  all key-value pairs in a symbol table. <br>
        !  For an ordered symbol table, the next pair data normally
        !  indicates the key-value pair with the so-called successor key.
        !  For an unordered symbol table, the next pair data may be any
        !  key-value pair.
        FUNCTION Move2NextPair(Collection, Key, Value) RESULT(IsTheEnd)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the next key as output if requested (and available)
            CLASS(*), OPTIONAL,     INTENT(OUT)     :: Key
            !% the next value as output if requested (and available)
            TYPE(*),  OPTIONAL,     INTENT(INOUT)   :: Value
            !> a flag indicating whether the move to the end of the
            !  collection occurs or not <br>
            ! - true if next pair data is NOT available. <br>
            ! - otherwise next pair data is available.
            tLogical                                :: IsTheEnd
        END FUNCTION
        !> *AddPair* is a deferred procedure to add a key-value pair
        !  into a symbol table.  When implementing this procedure,
        !  a symbol table should only allow one value being associated
        !  with each key (i.e. no duplicate keys in the table).  This
        !  means that when a user puts a key-value pair into a table
        !  already containing that key (and an associated value), the
        !  new value should then replace the old one.  These conventions
        !  define the associative array abstraction, where we can think
        !  of a symbol table as being just like an array, where keys are
        !  indices and values are array entries.
        SUBROUTINE AddPair(Collection, Key, Value)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the key to be added to the collection
            CLASS(*),               INTENT(IN)      :: Key
            !% the value to be added to the collection
            TYPE(*),                INTENT(IN)      :: Value
        END SUBROUTINE
        !> *DelPair* is a deferred procedure to delete a key-value pair
        !  from a symbol table.  This procedure is intended to be used
        !  in conjunction with the *Move2FirstPair* and *Move2NextPair*
        !  procedures.  Therefore, after the call to either procedure
        !  and then calling this procedure will result in a removal of
        !  the current key-value pair of the iteration (i.e. the same
        !  key-value pair that can be retrieved via those *Move* procedures).
        SUBROUTINE DelPair(Collection)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
        END SUBROUTINE
        !> *DelKey* is a deferred procedure to delete the specified key (and
        !  its associated value) from a symbol table.  Also, return a flag
        !  indicating whether the key-value pair is successfully removed or not.
        FUNCTION DelKey(Collection, Key) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the key to be removed from the collection
            CLASS(*),               INTENT(IN)      :: Key
            !> flag indicating whether the specified key and its associated
            !  value are successfully removed or not.
            tLogical                                :: Flag
        END FUNCTION DelKey
        !> *FindKey* is a deferred procedure to find the specified key in a symbol
        !  table.  Return true if the key is found.  Otherwise, return false.
        FUNCTION FindKey(Collection, Key) RESULT(Found)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the key to be looked for in the collection
            CLASS(*),               INTENT(IN)      :: Key
            !% flag indicating whether the specified key is found or not.
            tLogical                                :: Found
        END FUNCTION FindKey
        !> *GetVal* is a deferred procedure to get a value associated with the
        !  specified key in a symbol table.  Also, return a flag indicating
        !  whether the key-value pair is successfully found or not.
        FUNCTION GetVal(Collection, Key, Value) RESULT(Flag)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
            !% the key to be looked for in the collection
            CLASS(*),               INTENT(IN)      :: Key
            !% the value associated with the specified key
            TYPE(*),                INTENT(INOUT)   :: Value
            !% flag indicating whether the key-value pair is found or not.
            tLogical                                :: Flag
        END FUNCTION GetVal
        !> *GetKey* is a deferred procedure to get a pointer to a key stored
        !  in a symbol table.  The pointer is intended to be used as a mold
        !  for the key (i.e. provides type of the stored key).  Return null
        !  pointer if the table is empty.
        FUNCTION GetKey(Collection) RESULT(Key)
            IMPORT
            !% symbol-table collection object
            CLASS(BaseSymTable(*)), TARGET, INTENT(IN)  :: Collection
            !% pointer to a stored key
            CLASS(*),               POINTER             :: Key
        END FUNCTION GetKey
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    !  na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BaseSymTable_RemoveAllPairs(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the pair data items from the collection. <br>
    !  This routine provides a basic implementation of the *Clear*
    !  deferred procedure required by the *BaseCollection* class.
    !  This routine should be overridden if a better implementation
    !  is available.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: IsTheEnd

!** FLOW:
    
    IsTheEnd = Collection%StartFirst()
    DO WHILE (.NOT.IsTheEnd)
        CALL Collection%Delete()
        IsTheEnd = Collection%MoveForward()
    END DO

    RETURN

END SUBROUTINE BaseSymTable_RemoveAllPairs

! ---------------------------------------------------------------------
! -----         Specific Procedures for BaseSymTable Type         -----
! ---------------------------------------------------------------------

SUBROUTINE SymbolTable_CreateByArray(Collection, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a collection from an array of key-value pairs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% BaseSymTable object
    CLASS(BaseSymTable(*)), INTENT(INOUT)   :: Collection
    !% number of key-value pairs
    tIndex,                 INTENT(IN)      :: N
    !% the keys to be added to the collection
    CLASS(*),               INTENT(IN)      :: Keys(:)
    !% the associated values to be added to the collection
    TYPE(*),                INTENT(IN)      :: Values(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
    
    ! add key-value pairs to the collection
    DO I = 0, N-1
        CALL Collection%Insert(Keys(I), Values(I))
    END DO
       
    RETURN

END SUBROUTINE SymbolTable_CreateByArray

!**************************************************************************************

END MODULE Class_BaseSymTable

!******************************************************************************

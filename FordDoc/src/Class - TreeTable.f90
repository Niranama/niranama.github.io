
MODULE Class_TreeTable

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeTable* type, the *TabNode* type and their
!   related routines.  The *TabNode* type is a helper and private type used
!   to store a key-value pair.  The *TreeTable* type is a collection type
!   that employs a balanced binary-search-tree (BST) implementation to provide
!   common operations for an ordered symbol table. <br>
!   As an ordered symbol table, the *TreeTable* type only allows three Fortran
!   intrinsic types (including the *CHARACTER*, *INTEGER* and *REAL* types) to
!   be used as a type of the key to insert a key-value pair into a collection.
!   If a derived type as a type of the key is essential, the key type must be
!   a subtype of the *Comparable* type (i.e. in the *Comparable* class) that
!   provides a method to determine a total ordering on all the stored keys.

!** USE STATEMENTS:
    USE ModBase_Common
    USE ISO_C_BINDING,          ONLY: C_SIZEOF, C_LOC, C_NULL_PTR, C_PTR
    USE ModBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned
    USE Class_IntrusiveBSTrees
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_Comparable
    USE Class_KeyStore
    USE Class_GenStore
    USE Class_BaseCollection
    USE Class_BaseSymTable
    USE Class_OrderedSymTable
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeTable

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_TreeTable'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    !> *TabNode* is a binary-search-tree node type containing key and value as
    !  its components.  The *KeyStore* type is used as a storage for the key and
    !  the *GenStore* type is used as a storage for the value. <br>
    !  The *TabNode* type is a subtype of the *BSTNode* type and is intended to be
    !  used with the *TreeTable* type, which is a collection type that utilizes
    !  the *IntrusiveRBTree* type. <br>
    !  **Note**: This is a private type.
    TYPE, EXTENDS(BSTNode)  :: TabNode(ValSize)
        !% size of data content (a value of an element) in bytes
        tInteger,                LEN        :: ValSize
        !% stored key
        TYPE(KeyStore),          PRIVATE    :: Key
        !% stored value
        TYPE(GenStore(ValSize)), PRIVATE    :: Value
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => TabNode_Copy
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => TabNode_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (.NOT.ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TabNode_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => TabNode_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => TabNode_GetTypeName
        !> *CompareTo* is a procedure deferred by the *Comparable* type. <br>
        !  Use logical expression(s) in place of the *CompareTo* method to compare
        !  two *Comparable* objects.
        PROCEDURE   :: CompareTo    => TabNode_CompareKey
        ! ---------------------------------------------------------------------
        ! -----                     TabNode Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: SetKeyNVal <br>
        !  **Purpose**:  To set new key and value. <br>
        !  **Usage**: <br>
        !   --->    CALL Node%SetKeyNVal(Key, Value)
        PROCEDURE   :: SetKeyNVal   => TabNode_SetKeyNVal
        ! ---------------------------------------------------------------------
    END TYPE TabNode
    !> The *TreeTable* type is a collection type that utilizes a balanced BST
    !  implementation to provide common operations for an ordered symbol table.
    !  The *TreeTable* type uses the *IntrusiveRBTree* type as its component
    !  to store *TabNode* objects.  As an intrusive BST container, the
    !  *IntrusiveRBTree* type provides common binary-search-tree operations
    !  without a memory management task.  The memory management task of the
    !  inserted *TabNode* objects is handled by the *TreeTable* type.  <br>
    !  As an ordered symbol table, the *TreeTable* type is a subtype of the
    !  *OrderedSymTable* type.  Thus, it implements all deferred procedures
    !  required by the *OrderedSymTable* type and all its super classes (i.e.
    !  the *Assignable* type, the *BaseCollection* type and the *BaseSymTable*
    !  type).  As a symbol table, the *TreeTable* type does not allow duplicated
    !  keys; therefore, if an inserted key is equal to a key stored in the table,
    !  an associated value of the stored key is replaced by an associated value
    !  of the inserted key. <br>
    !  Because the *IntrusiveRBTree* type is a subtype of the *IntrusiveAVLTree*
    !  type, the *WrkTree* component can be employed as a red-black tree or an
    !  AVL tree.  Therefore, the *TreeTable* type allows a user to specify which
    !  type of binary-search tree implementation to be used.  By default, the
    !  red-black tree implementation is used.  The user can call the *UseAVLTree*
    !  method to change to AVL tree implementation.  The *UseAVLTree* method must
    !  be called before inserting an object into the symbol table (i.e when the
    !  table is empty).  Otherwise, the red-black tree implementation is employed.
    TYPE, EXTENDS(OrderedSymTable) :: TreeTable
        PRIVATE
        !> a flag indicating whether to use the red-black tree implementation
        !  or the AVL tree implementation
        tLogical                :: IsRBTree = TrueVal
        !% a working binary-search tree
        TYPE(IntrusiveRBTree)   :: WrkTree
        !> direction of the iteration <br>
        !  - positive -> forward iteration
        !  - negative -> backward iteration
        !  - zero     -> iteration not yet start
        tInteger                :: Dir = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: FindKey <br>
        !  **Purpose**:  To check whether the specified key is stored in the
        !                collection or not.  Optionally, return a stored node
        !                containing a key equal to the specified key. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%FindKey(Key, KeyNode) <br>
        !   --->    IF (.NOT.Collection%FindKey(Key)) DoSomething
        PROCEDURE, PRIVATE  :: FindKey  => TreeTable_FindKey
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from Assignable Type          -----
        ! ---------------------------------------------------------------------
        !> *CopyAssign* is a procedure deferred by the *Assignable* type. <br>
        !  Use the assignment expression in place of the *CopyAssign* method
        !  to make a copy of an *Assignable* object.
        PROCEDURE   :: CopyAssign   => TreeTable_CopyAssign
        !> *MakeClone* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetClone* method in place of the *MakeClone* method
        !  to create and return a copy of an *Assignable* object.
        PROCEDURE   :: MakeClone    => TreeTable_Clone
        !> *IsEqualTo* is a procedure deferred by the *Assignable* type. <br>
        !> **Type-Bound Function**: IsEqualTo <br>
        !  **Purpose**: To check whether two objects are equal to one another or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ObjA%IsEqualTo(ObjB) <br>
        !   --->    IF (ObjA%IsEqualTo(ObjB)) DoSomething
        PROCEDURE   :: IsEqualTo    => TreeTable_IsEqualTo
        !> *FreeMemory* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *MemFree* method in place of the *FreeMemory* method to free
        !  memory the *Assignable* object if the object contains allocatable and/or
        !  pointer component(s).
        PROCEDURE   :: FreeMemory   => TreeTable_Free
        !> *GetTypeName* is a procedure deferred by the *Assignable* type. <br>
        !  Use the *GetName* method in place of the *GetTypeName* method to get
        !  the name of the *concrete* type of an *Assignable* object (i.e. name
        !  of a concrete subtype of an *Assignable* abstract type).
        PROCEDURE   :: GetTypeName  => TreeTable_GetTypeName
        ! ---------------------------------------------------------------------
        ! -----         Deferred Procedures from BaseCollection Type      -----
        ! ---------------------------------------------------------------------
        !> *MakeCopy* is a procedure deferred by the *BaseCollection* type. <br>
        !  Use the *Construct* method in place of the *MakeCopy* method to
        !  create a collection from another collection.
        PROCEDURE   :: MakeCopy     => TreeTable_CopyCollection
        !> *Destruct* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all items from the collection and free memory
        !                of items stored in the collection.<br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Destruct() <br>
        !  **Note**:  For the balanced-tree-based types, this method is equivalent
        !             to the *Clear* method.
        PROCEDURE   :: Destruct     => TreeTable_Destroy
        !> *GetSize* is a procedure deferred by the *BaseCollection* type. <br>
        !  **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get the current size of the collection. <br>
        !  **Usage**: <br>
        !   --->    Size = Collection%GetSize()
        PROCEDURE   :: GetSize      => TreeTable_GetSize
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
        PROCEDURE   :: StartFirst   => TreeTable_Move2FirstPair
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveForward() <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Collection%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Collection%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward  => TreeTable_Move2NextPair
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start an iteration in a reversed order and return a flag
        !                indicating whether the collection is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Collection%StartLast() <br>
        !   --->    IsEmpty = Collection%StartLast(LastKey) <br>
        !   --->    IsEmpty = Collection%StartLast(Value=LastVal) <br>
        !   --->    IsEmpty = Collection%StartLast(LastKey, LastVal)
        PROCEDURE   :: StartLast    => TreeTable_Move2LastPair
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the previous key-value pair and return a flag
        !                indicating whether the cursor pointer has reached the end of
        !                the collection or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Collection%MoveBackward() <br>
        !   --->    IsTheEnd = Collection%MoveBackward(PrevKey) <br>
        !   --->    IsTheEnd = Collection%MoveBackward(Value=PrevVal) <br>
        !   --->    IsTheEnd = Collection%MoveBackward(PrevKey, PrevVal)
        PROCEDURE   :: MoveBackward => TreeTable_Move2PrevPair
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Insert(Key, Value) <br>
        PROCEDURE   :: Insert       => TreeTable_Insert
        !> **Type-Bound Subroutine**: Delete <br>
        !  **Purpose**:  To delete the current key-value pair from the collection. <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%Delete() <br>
        !  **Note**: This method is intended to be used in conjunction with the
        !  *StartFirst* and *MoveForward* methods.  Therefore, after the call to
        !  one of those methods and then calling this one will result in a removal
        !  of the current pair data of the iteration (i.e. the same key-value pair
        !  that can be retrieved via the *StartFirst* and *MoveForward* methods).
        PROCEDURE   :: Delete       => TreeTable_Delete
        !> **Type-Bound Function**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from
        !                the collection.  Also, return a flag indicating whether the
        !                key-value pair is successfully removed or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Remove(Key) <br>
        !   --->    IF (.NOT.Collection%Remove(Key)) DoSomething
        PROCEDURE   :: Remove       => TreeTable_Remove
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the collection.  Return true if
        !                the specified key is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Contain(Key) <br>
        !   --->    IF (.NOT.Collection%Contain(Key)) DoSomething
        PROCEDURE   :: Contain      => TreeTable_Contain
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get a value associated with the specified key in the collection.
        !                Also, return a flag indicating whether the key-value pair is
        !                successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Collection%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue     => TreeTable_GetValue
        !> **Type-Bound Function**: GetKeyPtr <br>
        !  **Purpose**:  To get a pointer to a key stored in a symbol table.
        !                The pointer is intended to be used as a mold for
        !                the key (i.e. provides type of the stored key).
        !                Return null pointer if the table is empty. <br>
        !  **Usage**: <br>
        !   --->    KeyPtr => Collection%GetKeyPtr()
        PROCEDURE   :: GetKeyPtr    => TreeTable_GetKeyPtr
        ! ---------------------------------------------------------------------
        ! -----     Deferred Procedures from OrderedSymTable Type         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetMinKey <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMinKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMinKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMinKey    => TreeTable_GetMinKey
        !> **Type-Bound Function**: GetMaxKey <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table.  Also, return a flag indicating
        !                whether the key is successfully retrieved or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%GetMaxKey(Key) <br>
        !   --->    IF (.NOT.Collection%GetMaxKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMaxKey    => TreeTable_GetMaxKey
        !> **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated
        !                with it) in a symbol table less than or equal to the given
        !                key.  Also, return a flag indicating whether the floor key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Floor(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Collection%Floor(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Floor        => TreeTable_Floor
        !> **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated
        !                with it) in a symbol table greater than or equal to the given
        !                key.  Also, return a flag indicating whether the ceiling key
        !                is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%Ceiling(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Collection%Ceiling(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Ceiling      => TreeTable_Ceiling
        !> **Type-Bound Function**: GetRank <br>
        !  **Purpose**:  To return the number of keys in the symbol table strictly
        !                less than the given key. <br>
        !  **Usage**: <br>
        !   --->    KeyRank = Collection%GetRank(Key)
        PROCEDURE   :: GetRank      => TreeTable_GetRank
        !> **Type-Bound Subroutine**: Select <br>
        !  **Purpose**:  To get the key (and optionally its associated value) of the
        !                specified rank where the applicable range of rank is between
        !                0 and TableSize-1. Also, return a flag indicating whether the
        !                ranked key is successfully retrieved or not. <br>
        !   --->    Flag = Collection%Select(Rank, Key) <br>
        !   --->    IF (.NOT.Collection%Select(Rank, Key, Value)) DoSomething
        PROCEDURE   :: Select       => TreeTable_Select
        !> **Type-Bound Function**: RemoveMin <br>
        !  **Purpose**:  To remove the smallest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMin() <br>
        !   --->    Flag = Collection%RemoveMin(MinKey) <br>
        !   --->    Flag = Collection%RemoveMin(Value=MinVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMin(MinKey, MinVal)) DoSomething
        PROCEDURE   :: RemoveMin    => TreeTable_RemoveMin
        !> **Type-Bound Function**: RemoveMax <br>
        !  **Purpose**:  To remove the largest key (and a value associated with it)
        !                from a symbol table.  Also, return a flag indicating
        !                whether the key is successfully removed or not.  If the
        !                table is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Collection%RemoveMax() <br>
        !   --->    Flag = Collection%RemoveMax(MaxKey) <br>
        !   --->    Flag = Collection%RemoveMax(Value=MaxVal) <br>
        !   --->    IF (.NOT.Collection%RemoveMax(MaxKey, MaxVal)) DoSomething
        PROCEDURE   :: RemoveMax    => TreeTable_RemoveMax
        !> **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To return the number of keys between *KeyLo* (inclusive)
        !                and *KeyHi* (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Collection%GetRangeSize(KeyLo, KeyHi)
        PROCEDURE   :: GetRangeSize => TreeTable_RangeSize
        ! ---------------------------------------------------------------------
        ! -----         Specific Procedures by TreeTable Type             -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: UseAVLTree <br>
        !  **Purpose**:  To set the working tree component to work as an AVL tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseAVLTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseAVLTree   => TreeTable_UseAVLTree
        !> **Type-Bound Subroutine**: UseRBTree <br>
        !  **Purpose**:  To set the working tree component to work as an red-black RB tree.  <br>
        !  **Usage**: <br>
        !   --->    CALL Collection%UseRBTree() <br>
        !  *Note*: The collection must be empty when calling this method.
        PROCEDURE   :: UseRBTree   => TreeTable_UseRBTree
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check        => TreeTable_CheckIntegrity
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the object.
        FINAL       :: TreeTable_Finalize
        ! ---------------------------------------------------------------------
    END TYPE TreeTable

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

! ---------------------------------------------------------------------
! -----                 TabNode Procedures                        -----
! ---------------------------------------------------------------------

SUBROUTINE TabNode_Copy(DstObj, SrcObj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the TabNode object.
    !  This is a deferred procedure by an *Assignable* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(OUT)  :: DstObj   !! destination object
    CLASS(Assignable), INTENT(IN)   :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! copy Key and Value components
    SELECT TYPE (SrcObj)
    TYPE IS (TabNode(*))
        DstObj%Key   = SrcObj%Key
        DstObj%Value = SrcObj%Value
    CLASS DEFAULT
        CALL Handle_ErrLevel('TabNode_Copy', ModName, ErrSevere, &
                             'Type of the source object must be "TabNode" only.')
        RETURN
    END SELECT
    
    ! copy "BSTNode" components
    SELECT TYPE (SrcObj)
    CLASS IS (BSTNode)
        CALL DstObj%CopyMembers(SrcObj)
    CLASS DEFAULT
        CALL Handle_ErrLevel('TabNode_Copy', ModName, ErrSevere, &
                             'The source object must be in the "BSTNode" class.')
    END SELECT

    RETURN

END SUBROUTINE TabNode_Copy

!******************************************************************************

SUBROUTINE TabNode_Clone(SrcObj, DstObj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the TabNode object.
    !  This is a deferred procedure by an *Assignable* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)),              INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! create the TabNode object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! make a copy of the TabNode object
    SELECT TYPE (DstObj)
    TYPE IS (TabNode(*))
        DstObj = SrcObj
    END SELECT

    RETURN

END SUBROUTINE TabNode_Clone

!******************************************************************************

FUNCTION TabNode_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure by an *Assignable* object. <br>
    !  It should be noted that this routine uses all components of
    !  the *TabNode* object to check equality. Therefore, although
    !  (A%CompareTo(B) == 0) returns true, (A%IsEqualTo(B)) can return
    !  false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(IN)   :: LhsObj   !! an object
    CLASS(Assignable), INTENT(IN)   :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    
    ! check key and value equalities
    SELECT TYPE (RhsObj)
    TYPE IS (TabNode(*))
        ! check key equality
        IF (LhsObj%Key /= RhsObj%Key) RETURN
        ! check value equality where values may be represented by character strings
        IF (.NOT.Character_IsEqual(LhsObj%Value, RhsObj%Value)) RETURN
    CLASS DEFAULT
        RETURN
    END SELECT

    ! check BSTNode-component equalities
    SELECT TYPE (RhsObj)
    CLASS IS (BSTNode)
        Flag = LhsObj%EqualTo(RhsObj)
    CLASS DEFAULT
        RETURN
    END SELECT

    RETURN

END FUNCTION TabNode_IsEqualTo

!******************************************************************************

SUBROUTINE TabNode_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the TabNode object.
    !  This is a deferred procedure by an *Assignable* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(INOUT)    :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! free the key component
    CALL Obj%Key%MemFree()
    
    ! free the "BSTNode" components
    CALL Obj%Reset()

    RETURN

END SUBROUTINE TabNode_Free

!******************************************************************************

FUNCTION TabNode_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the TabNode type.
    !  This is a deferred procedure by an *Assignable* object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(IN)   :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'TabNode'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION TabNode_GetTypeName

!******************************************************************************

FUNCTION TabNode_CompareKey(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare *A* and *B* and return <br>
    !   1 if *A* is greater than *B*, <br>
    !   0 if *A* is equal to *B*, <br>
    !  -1 if *A* is less than *B*, <br>
    !  -999 if type of *B* is invalid. <br>
    !  Also, write an error message to the default log file if this happens. <br>
    !  This is a deferred procedure by an *Comparable* object. <br>
    !  It is important to note that this routine only uses the key component
    !  of the *TabNode* object.  Thus, even though (A%CompareTo(B) == 0)
    !  is true, A%IsEqualTo(B) may be false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TabNode(*)), INTENT(IN)   :: A
    CLASS(Comparable), INTENT(IN)   :: B
    tInteger                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT TYPE (B)
    TYPE IS (TabNode(*))
        Flag = A%Key%CompareTo(B%Key)
    CLASS DEFAULT
        Flag = -999
        CALL Handle_ErrLevel('TabNode_CompareKey', ModName, ErrSevere, 'Type of B is valid.')
    END SELECT

    RETURN

END FUNCTION TabNode_CompareKey

!******************************************************************************

SUBROUTINE TabNode_SetKeyNVal(Node, Key, Value)

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
    
    CALL Node%Key%Set(Key)
    CALL Node%Value%Set(Value)

    RETURN

END SUBROUTINE TabNode_SetKeyNVal

! ---------------------------------------------------------------------
! -----         Deferred Procedures from Assignable Type          -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_CopyAssign(DstObj, SrcObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To copy the source object to the destination object.
    !  This is a deferred procedure inherited from the *Assignable* type.
    !  *Note*:  SrcObj must be in the *TreeTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)), INTENT(OUT)    :: DstObj   !! destination object
    CLASS(Assignable),   INTENT(IN)     :: SrcObj   !! source object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (SrcObj)
    TYPE IS (TreeTable(*))
        IF (DstObj%ValSize /= SrcObj%ValSize) THEN
            CALL Handle_ErrLevel('TreeTable_CopyAssign', ModName, ErrSevere, &
                       'The "ValSize" components must have the same value.')
        ELSE
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the source object.
            BLOCK
                ! block variables
                CLASS(BSTNode), POINTER :: CurrNode
                CLASS(BSTNode), POINTER :: NextNode
                CLASS(BSTNode), POINTER :: RootNode
                ! start iteration from the minimum node (with smallest key) of the source
                CurrNode => SrcObj%WrkTree%GetMinNode()
                RootNode => SrcObj%WrkTree%GetRoot()
                DO WHILE (ASSOCIATED(CurrNode))
                    ! insert the current node to the destination
                    CALL DstObj%WrkTree%Insert(CurrNode)
                    ! move to the next iteration of the source (i.e. successor node)
                    CALL Find_Inorder_Successor(RootNode, CurrNode, NextNode)
                    CurrNode => NextNode
                END DO
                NULLIFY(CurrNode, NextNode, RootNode)
            END BLOCK
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('TreeTable_CopyAssign', ModName, ErrSevere, &
                   'Type of the SrcObj collection is NOT valid.')
    END SELECT

    RETURN

END SUBROUTINE TreeTable_CopyAssign

!******************************************************************************

SUBROUTINE TreeTable_Clone(SrcObj, DstObj)

!** PURPOSE OF DstObj SUBROUTINE:
    !^ To clone the TreeTable object.  This is a deferred
    !  procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)),            INTENT(IN)  :: SrcObj   !! source object
    CLASS(Assignable), ALLOCATABLE, INTENT(OUT) :: DstObj   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! create the TreeTable object
    ALLOCATE(DstObj, MOLD=SrcObj)
    
    ! copy values from the source object's components
    ! note: these are the same as those of 'CopyAssign' procedure
    !       but without checking type validity.
    SELECT TYPE (DstObj)
    TYPE IS (TreeTable(*))
        ! implementation note:  we cannot use the iteration methods here
        !   due to the intent of the source object.
        BLOCK
            ! block variables
            CLASS(BSTNode), POINTER :: CurrNode
            CLASS(BSTNode), POINTER :: NextNode
            CLASS(BSTNode), POINTER :: NewNode
            CLASS(BSTNode), POINTER :: RootNode
            ! start iteration from the minimum node (with smallest key) of the source
            CurrNode => SrcObj%WrkTree%GetMinNode()
            RootNode => SrcObj%WrkTree%GetRoot()
            DO WHILE (ASSOCIATED(CurrNode))
                ALLOCATE(NewNode, SOURCE=CurrNode)
                ! insert the current node to the destination
                CALL DstObj%WrkTree%Insert(NewNode)
                NULLIFY(NewNode)
                ! move to the next iteration of the source (i.e. successor node)
                CALL Find_Inorder_Successor(RootNode, CurrNode, NextNode)
                CurrNode => NextNode
            END DO
            NULLIFY(CurrNode, NextNode, RootNode)
        END BLOCK
    END SELECT

    RETURN

END SUBROUTINE TreeTable_Clone

!******************************************************************************

FUNCTION TreeTable_IsEqualTo(LhsObj, RhsObj) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether LhsObj and RhsObj are equal or not.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)), INTENT(IN) :: LhsObj   !! an object
    CLASS(Assignable),   INTENT(IN) :: RhsObj   !! another object
    tLogical                        :: Flag     !! true if both objects are equal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (RhsObj)
    TYPE IS (TreeTable(*))
        Flag = FalseVal
        IF (LhsObj%ValSize /= RhsObj%ValSize) RETURN
        IF (LhsObj%GetSize() /= RhsObj%GetSize()) RETURN
        IF (.NOT.LhsObj%IsEmpty()) THEN
            ! implementation note:  we cannot use the iteration methods here
            !   due to the intent of the input data.
            BLOCK
                CLASS(BSTNode), POINTER :: LhsNode, LhsNext, LhsRoot
                CLASS(BSTNode), POINTER :: RhsNode, RhsNext, RhsRoot
                tLogical                :: ReturnNow
                ReturnNow = FalseVal
                ! start iteration
                LhsNode => LhsObj%WrkTree%GetMinNode()
                RhsNode => RhsObj%WrkTree%GetMinNode()
                LhsRoot => LhsObj%WrkTree%GetRoot()
                RhsRoot => RhsObj%WrkTree%GetRoot()
                Loop: DO WHILE (ASSOCIATED(LhsNode).AND.ASSOCIATED(RhsNode))
                    ! get successor nodes
                    CALL Find_Inorder_Successor(LhsRoot, LhsNode, LhsNext)
                    CALL Find_Inorder_Successor(RhsRoot, RhsNode, RhsNext)
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
                NULLIFY(LhsRoot, RhsRoot)
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

END FUNCTION TreeTable_IsEqualTo

!******************************************************************************

SUBROUTINE TreeTable_Free(Obj)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free memory of the TreeTable object.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Obj

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Obj%Clear()

    RETURN

END SUBROUTINE TreeTable_Free

!******************************************************************************

FUNCTION TreeTable_GetTypeName(Obj) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the name of the TreeTable type.
    !  This is a deferred procedure inherited from the *Assignable* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)), INTENT(IN) :: Obj
    tCharAlloc                      :: Name

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'TreeTable'
    ASSOCIATE(Dummy => Obj); END ASSOCIATE

    RETURN

END FUNCTION TreeTable_GetTypeName

! ---------------------------------------------------------------------
! -----        Deferred Procedures from BaseCollection Type       -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_CopyCollection(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To creates a new collection (This) with the same items
    !  as the given collection (Other). <br>
    !  This is a deferred procedure by the *BaseCollection* class. <br>
    !  *Note*:  Other must be in the *OrderedSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object to be created
    CLASS(TreeTable(*)),   INTENT(INOUT)    :: This
    !% collection object to be copied
    CLASS(BaseCollection), INTENT(INOUT)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    SELECT TYPE (Other)
    TYPE IS (TreeTable(*))
        ! same type of collection
        IF (This%ValSize == Other%ValSize) THEN
            BLOCK
                ! block variables
                tLogical                :: IsTheEnd
                CLASS(BSTNode), POINTER :: CurrNode
                CLASS(BSTNode), POINTER :: NewNode
                ! start iteration from the minimum node (with smallest key) of the source
                IsTheEnd = Other%WrkTree%StartMin(CurrNode)
                DO WHILE (.NOT.IsTheEnd)
                    ALLOCATE(NewNode, SOURCE=CurrNode)
                    ! insert the new node to the destination
                    CALL This%WrkTree%Insert(NewNode)
                    NULLIFY(NewNode)
                    ! move to the next iteration of the source
                    IsTheEnd = Other%WrkTree%MoveForward(CurrNode)
                END DO
                NULLIFY(CurrNode)
            END BLOCK
        ELSE
            CALL Handle_ErrLevel('TreeTable_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS IS (OrderedSymTable(*))
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
            CALL Handle_ErrLevel('TreeTable_CopyCollection', ModName, ErrSevere, &
                       'Cannot copy items because the "ValSize" components are NOT the same.')
        END IF
    CLASS DEFAULT
        CALL Handle_ErrLevel('TreeTable_CopyCollection', ModName, ErrSevere, &
                   'Type of "Other" must be in the "OrderedSymTable" class.')
    END SELECT
    
    RETURN

END SUBROUTINE TreeTable_CopyCollection

!******************************************************************************

SUBROUTINE TreeTable_ClearItems(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To remove all of the items from the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Success

! FLOW
    
    ! simply return if the collection is empty
    IF (Collection%IsEmpty()) RETURN
    
    ! remove all nodes starting from the node with the largest key
    DO
        Success = Collection%RemoveMax()
        IF (.NOT.Success) EXIT
    END DO

    RETURN

END SUBROUTINE TreeTable_ClearItems

!******************************************************************************

SUBROUTINE TreeTable_Destroy(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To destruct the collection. <br>
    ! This is a deferred procedure by the *BaseCollection* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% TreeTable object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:
    
    CALL Collection%Clear()

    RETURN

END SUBROUTINE TreeTable_Destroy

!******************************************************************************

FUNCTION TreeTable_GetSize(Collection) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of items in the collection.
    !  This is a deferred procedure inherited from the *BaseCollection* type.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(TreeTable(*)), INTENT(IN) :: Collection
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = Collection%WrkTree%GetSize()

    RETURN

END FUNCTION TreeTable_GetSize

! ---------------------------------------------------------------------
! -----         Deferred Procedures from BaseSymTable Type        -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_Move2FirstPair(Collection, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the first (starting) pair data in a symbol table.
    !  For the *TreeTable*, which is an ordered symbol table, 
    !  the starting pair is the pair with smallest key.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the smallest key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the associated value as output if requested (and available)
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMin(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL CurrNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMin()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = 1

    RETURN

END FUNCTION TreeTable_Move2FirstPair

!******************************************************************************

FUNCTION TreeTable_Move2NextPair(Collection, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the next pair data in a symbol table.
    !  For the *TreeTable*, which is an ordered symbol table, 
    !  the next pair is the so-called successor pair.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! move to next iteration
            IsTheEnd = Collection%WrkTree%MoveForward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL CurrNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! move to next iteration
        IsTheEnd = Collection%WrkTree%MoveForward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = 1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeTable_Move2NextPair

!******************************************************************************

FUNCTION TreeTable_Move2LastPair(Collection, Key, Value) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the last (starting in a reversed order) pair data in a symbol table.
    !  For the *TreeTable*, which is an ordered symbol table, 
    !  the starting pair in a reversed order is the pair with greatest key.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the greatest key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the associated value as output if requested (and available)
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsEmpty = Collection%WrkTree%StartMax(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL CurrNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsEmpty = Collection%WrkTree%StartMax()
    END IF

    ! set direction
    IF (.NOT.IsEmpty) Collection%Dir = -1

    RETURN

END FUNCTION TreeTable_Move2LastPair

!******************************************************************************

FUNCTION TreeTable_Move2PrevPair(Collection, Key, Value) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To move to the previous pair data in a symbol table.
    !  For the *TreeTable*, which is an ordered symbol table, 
    !  the previous pair is the so-called predecessor pair.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the previous key as output if requested (and available)
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the previous value as output if requested (and available)
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
            CLASS(BSTNode), POINTER :: CurrNode
            ! start iteration
            IsTheEnd = Collection%WrkTree%MoveBackward(CurrNode)
            IF (ASSOCIATED(CurrNode)) THEN
                ! get the item stored in the node
                SELECT TYPE (CurrNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL CurrNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL CurrNode%Value%Get(Value)
                END SELECT
            END IF
            NULLIFY(CurrNode)
        END BLOCK
    ELSE
        ! start iteration
        IsTheEnd = Collection%WrkTree%MoveBackward()
    END IF

    ! set direction
    IF (.NOT.IsTheEnd) THEN
        Collection%Dir = -1
    ELSE
        Collection%Dir = 0
    END IF

    RETURN

END FUNCTION TreeTable_Move2PrevPair

!******************************************************************************

SUBROUTINE TreeTable_Insert(Collection, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add a key-value pair into a symbol table.  If the specified key
    !  is already stored in the table, replace the old value with the 
    !  new one.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
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
            CALL Handle_ErrAlloc('TreeTable_Insert', ModName, AllocMsg, AllocStat)
            ! set key and value
            CALL KeyNode%SetKeyNVal(Key, Value)
            ! insert the new node
            IF (Collection%IsRBTree) THEN
                ! WrkTree is a red-black tree.
                CALL Collection%WrkTree%Insert(KeyNode)
            ELSE
                ! WrkTree is an AVL tree.
                CALL Collection%WrkTree%IntrusiveAVLTree%Insert(KeyNode)
            END IF
        END BLOCK
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END SUBROUTINE TreeTable_Insert

!******************************************************************************

SUBROUTINE TreeTable_Delete(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To delete a key-value pair from a symbol table.  This procedure is intended
    !  to be used in conjunction with the *StartFirst* and *MoveForward* procedures.
    !  Therefore, after the call to either procedure and then calling this procedure
    !  will result in a removal of the current key-value pair of the iteration (i.e.
    !  the same key-value pair that can be retrieved via those *Move* procedures). <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: CurrNode
    tLogical                :: Success
    tInteger                :: AllocStat
    tCharLen(MsgLen)        :: AllocMsg
    tLogical                :: IsTheEnd

! FLOW

    ! get the cursor node
    CurrNode => Collection%WrkTree%GetCursor()
    
    ! check if the node is associated
    IF (ASSOCIATED(CurrNode)) THEN
        ! reset cursor
        IF (Collection%Dir == 1) THEN
            ! forward iteration so move cursor backward
            IsTheEnd = Collection%WrkTree%MoveBackward()
        ELSE
            ! backward iteration so move cursor forward
            IsTheEnd = Collection%WrkTree%MoveForward()
        END IF
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(CurrNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(CurrNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! free pointer/allocatable components of the node
            SELECT TYPE (CurrNode)
            TYPE IS (TabNode(*))
                CALL CurrNode%FreeMemory()
            END SELECT
            ! deallocate the node
            DEALLOCATE(CurrNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('TreeTable_Delete', ModName, AllocMsg, AllocStat)
        ELSE
            CALL Handle_ErrLevel('TreeTable_Delete', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(CurrNode)

    RETURN

END SUBROUTINE TreeTable_Delete

!******************************************************************************

FUNCTION TreeTable_Remove(Collection, Key) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To delete the specified key (and its associated value) from a symbol
    !  table.  Also, return a flag indicating whether the key-value pair is
    !  successfully removed or not.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the key to be removed from the collection
    CLASS(*),            INTENT(IN)     :: Key
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(TabNode(Collection%ValSize)), POINTER :: KeyNode
    tLogical                                    :: Success
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
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree.
            Success = Collection%WrkTree%Remove(KeyNode)
        ELSE
            ! WrkTree is an AVL tree.
            Success = Collection%WrkTree%IntrusiveAVLTree%Remove(KeyNode)
        END IF
        ! check if remove the node successfully or not
        IF (Success) THEN
            ! free pointer/allocatable components of the node
            CALL KeyNode%FreeMemory()
            ! deallocate the node
            DEALLOCATE(KeyNode, STAT=AllocStat, ERRMSG=AllocMsg)
            CALL Handle_ErrDealloc('TreeTable_Remove', ModName, AllocMsg, AllocStat)
        ELSE
            CALL Handle_ErrLevel('TreeTable_Remove', ModName, ErrWarning, &
                       'Check the Remove method of the intrusive tree for possible bug(s).')
        END IF
    END IF
    
    ! free working pointer
    NULLIFY(KeyNode)

    RETURN

END FUNCTION TreeTable_Remove

!******************************************************************************

FUNCTION TreeTable_Contain(Collection, Key) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified key is currently stored in a symbol table.  <br>
    !  This is a deferred procedure by the *BaseSymTable* class.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
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

END FUNCTION TreeTable_Contain

!******************************************************************************

FUNCTION TreeTable_GetValue(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a value associated with the specified key in a symbol table.
    !  Also, return a flag indicating whether the key-value pair is successfully
    !  found or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
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

END FUNCTION TreeTable_GetValue

!******************************************************************************

FUNCTION TreeTable_GetKeyPtr(Collection) RESULT(Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to a key stored in a symbol table.  The pointer
    !  is intended to be used as a mold for the key (i.e. provides type
    !  of the stored key).  Return null pointer if the table is empty.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), TARGET, INTENT(IN) :: Collection
    !% pointer to a stored key
    CLASS(*),            POINTER            :: Key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: MinNode

! FLOW
    
    ! get node with smallest key
    MinNode => Collection%WrkTree%GetMinNode()
    IF (ASSOCIATED(MinNode)) THEN
        SELECT TYPE (MinNode)
        TYPE IS (TabNode(*))
            Key => MinNode%Key
        END SELECT
    ELSE
        Key => NULL()
    END IF
    
    ! free working pointer
    NULLIFY(MinNode)

    RETURN

END FUNCTION TreeTable_GetKeyPtr

! ---------------------------------------------------------------------
! -----         Deferred Procedures from OrderedSymTable Type     -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_GetMinKey(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the smallest key (and optionally a value associated with it)
    !  in a symbol table.  Also, return a flag indicating whether the key
    !  is successfully retrieved or not.  If the table is empty, the flag
    !  is typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the smallest key to be retrieved from the collection
    CLASS(*),            INTENT(OUT)    :: Key
    !% the value associated with the smallest key
    TYPE(*), OPTIONAL,   INTENT(INOUT)  :: Value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: MinNode

! FLOW
    
    ! get node with smallest key
    MinNode => Collection%WrkTree%GetMinNode()
    IF (ASSOCIATED(MinNode)) THEN
        SELECT TYPE (MinNode)
        TYPE IS (TabNode(*))
            CALL MinNode%Key%Get(Key)
            IF (PRESENT(Value)) CALL MinNode%Value%Get(Value)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(MinNode)

    RETURN

END FUNCTION TreeTable_GetMinKey

!******************************************************************************

FUNCTION TreeTable_GetMaxKey(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the largest key (and optionally a value associated with it)
    !  in a symbol table.  Also, return a flag indicating whether the key
    !  is successfully retrieved or not.  If the table is empty, the flag
    !  is typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the largest key to be retrieved from the collection
    CLASS(*),            INTENT(OUT)    :: Key
    !% the value associated with the largest key
    TYPE(*), OPTIONAL,   INTENT(INOUT)  :: Value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: MaxNode

! FLOW
    
    ! get node with largest key
    MaxNode => Collection%WrkTree%GetMaxNode()
    IF (ASSOCIATED(MaxNode)) THEN
        SELECT TYPE (MaxNode)
        TYPE IS (TabNode(*))
            CALL MaxNode%Key%Get(Key)
            IF (PRESENT(Value)) CALL MaxNode%Value%Get(Value)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(MaxNode)

    RETURN

END FUNCTION TreeTable_GetMaxKey

!******************************************************************************

FUNCTION TreeTable_Floor(Collection, KeyIn, KeyOut, ValOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To the largest key (and optionally a value associated with the key) in
    !  a collection less than or equal to the given key.  Also, return a flag
    !  indicating whether the floor key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the specified key
    CLASS(*),            INTENT(IN)     :: KeyIn
    !% the largest key in the table less than or equal to the given key
    CLASS(*),            INTENT(OUT)    :: KeyOut
    !% the value associated with the largest key
    TYPE(*), OPTIONAL,   INTENT(INOUT)  :: ValOut
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode(Collection%ValSize))   :: KeyNode
    CLASS(BSTNode), POINTER             :: FloorNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(KeyIn)
    
    ! get floor node
    FloorNode => Collection%WrkTree%Floor(KeyNode)
    IF (ASSOCIATED(FloorNode)) THEN
        SELECT TYPE (FloorNode)
        TYPE IS (TabNode(*))
            CALL FloorNode%Key%Get(KeyOut)
            IF (PRESENT(ValOut)) CALL FloorNode%Value%Get(ValOut)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(FloorNode)

    RETURN

END FUNCTION TreeTable_Floor

!******************************************************************************

FUNCTION TreeTable_Ceiling(Collection, KeyIn, KeyOut, ValOut) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To the smallest key (and optionally a value associated with the key) in
    !  a collection greater than or equal to the given key.  Also, return a flag
    !  indicating whether the floor key is successfully retrieved or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the specified key
    CLASS(*),            INTENT(IN)     :: KeyIn
    !% the smallest key in the table greater than or equal to the given key
    CLASS(*),            INTENT(OUT)    :: KeyOut
    !% the value associated with the smallest key
    TYPE(*), OPTIONAL,   INTENT(INOUT)  :: ValOut
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode(Collection%ValSize))   :: KeyNode
    CLASS(BSTNode), POINTER             :: CeilingNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(KeyIn)
    
    ! get floor node
    CeilingNode => Collection%WrkTree%Ceiling(KeyNode)
    IF (ASSOCIATED(CeilingNode)) THEN
        SELECT TYPE (CeilingNode)
        TYPE IS (TabNode(*))
            CALL CeilingNode%Key%Get(KeyOut)
            IF (PRESENT(ValOut)) CALL CeilingNode%Value%Get(ValOut)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(CeilingNode)

    RETURN

END FUNCTION TreeTable_Ceiling

!******************************************************************************

FUNCTION TreeTable_GetRank(Collection, Key) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the symbol table strictly less than the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the specified key
    CLASS(*),            INTENT(IN)     :: Key
    !% the number of keys less than the given key.
    tIndex                              :: Rank

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode(Collection%ValSize))   :: KeyNode

! FLOW
    
    ! set key
    CALL KeyNode%Key%Set(Key)
    
    ! get rank
    Rank = Collection%WrkTree%Rank(KeyNode)

    RETURN

END FUNCTION TreeTable_GetRank

!******************************************************************************

FUNCTION TreeTable_Select(Collection, Rank, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the key (and optionally its associated value) of the given rank.
    !  Also, return a flag indicating whether the ranked key is successfully
    !  retrieved or not. <br>
    !  This ranked key has the property such that there are keys in the symbol
    !  table that are smaller.  In other words, this key is the (rank+1)st smallest
    !  key in the table. <br>
    !  The applicable range of rank is between 0 and TableSize-1 where the rank number
    !  is zero-based.  If the specified rank is out of range or the table is empty,
    !  the returned flag is false.  Otherwise, the returned flag is true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the specified rank.
    tIndex,              INTENT(IN)     :: Rank
    !% the key of the specified rank
    CLASS(*),            INTENT(OUT)    :: Key
    !% the value associated with the ranked key
    TYPE(*), OPTIONAL,  INTENT(INOUT)   :: Value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: RankNode

! FLOW
    
    ! get rank node
    RankNode => Collection%WrkTree%Select(Rank)

    ! get key and optionally its associated value
    IF (ASSOCIATED(RankNode)) THEN
        SELECT TYPE (RankNode)
        TYPE IS (TabNode(*))
            CALL RankNode%Key%Get(Key)
            IF (PRESENT(Value)) CALL RankNode%Value%Get(Value)
        END SELECT
        Flag = TrueVal
    ELSE
        Flag = FalseVal
    END IF
    
    ! free working pointer
    NULLIFY(RankNode)

    RETURN

END FUNCTION TreeTable_Select

!******************************************************************************

FUNCTION TreeTable_RemoveMin(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and a value associated with the key from
    !  a symbol table.  Also, return a flag indicating whether the key is
    !  successfully removed or not.  If the table is empty, the flag is
    !  typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the smallest key
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the value associated with the smallest key
    TYPE(*),  OPTIONAL,  INTENT(INOUT)  :: Value
    !% flag indicating whether the key is successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((PRESENT(Key)).OR.(PRESENT(Value))) THEN
        BLOCK
            ! local variable
            CLASS(BSTNode), POINTER :: MinNode
            ! remove node with smallest key
            IF (Collection%IsRBTree) THEN
                ! WrkTree is a red-black tree
                Flag = Collection%WrkTree%RemoveMin(MinNode)
            ELSE
                ! WrkTree is an AVL tree
                Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMin(MinNode)
            END IF
            ! optionally get key and its associated value
            IF (ASSOCIATED(MinNode)) THEN
                SELECT TYPE (MinNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL MinNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL MinNode%Value%Get(Value)
                END SELECT
            END IF
            ! free working pointer
            NULLIFY(MinNode)
        END BLOCK
    ELSE
        ! remove node with smallest key
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree
            Flag = Collection%WrkTree%RemoveMin()
        ELSE
            ! WrkTree is an AVL tree
            Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMin()
        END IF
    END IF

    RETURN

END FUNCTION TreeTable_RemoveMin

!******************************************************************************

FUNCTION TreeTable_RemoveMax(Collection, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and a value associated with the key from
    !  a symbol table.  Also, return a flag indicating whether the key is
    !  successfully removed or not.  If the table is empty, the flag is
    !  typically false.  Otherwise, the flag is always true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the largest key
    CLASS(*), OPTIONAL,  INTENT(OUT)    :: Key
    !% the value associated with the largest key
    TYPE(*),  OPTIONAL,  INTENT(INOUT)  :: Value
    !% flag indicating whether the key is successfully removed or not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF ((PRESENT(Key)).OR.(PRESENT(Value))) THEN
        BLOCK
            ! local variable
            CLASS(BSTNode), POINTER :: MaxNode
            ! remove node with largest key
            IF (Collection%IsRBTree) THEN
                ! WrkTree is a red-black tree
                Flag = Collection%WrkTree%RemoveMax(MaxNode)
            ELSE
                ! WrkTree is an AVL tree
                Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMax(MaxNode)
            END IF
            ! optionally get key and its associated value
            IF (ASSOCIATED(MaxNode)) THEN
                SELECT TYPE (MaxNode)
                TYPE IS (TabNode(*))
                    IF (PRESENT(Key))   CALL MaxNode%Key%Get(Key)
                    IF (PRESENT(Value)) CALL MaxNode%Value%Get(Value)
                END SELECT
            END IF
            ! free working pointer
            NULLIFY(MaxNode)
        END BLOCK
    ELSE
        ! remove node with largest key
        IF (Collection%IsRBTree) THEN
            ! WrkTree is a red-black tree
            Flag = Collection%WrkTree%RemoveMax()
        ELSE
            ! WrkTree is an AVL tree
            Flag = Collection%WrkTree%IntrusiveAVLTree%RemoveMax()
        END IF
    END IF

    RETURN

END FUNCTION TreeTable_RemoveMax

!******************************************************************************

FUNCTION TreeTable_RangeSize(Collection, KeyLo, KeyHi) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the given range (between KeyLo and KeyHi).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
    !% the minimum key (inclusive)
    CLASS(*),            INTENT(IN)     :: KeyLo
    !% the maximum key (inclusive)
    CLASS(*),            INTENT(IN)     :: KeyHi
    !% the number of keys in the given range.
    tIndex                              :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode(Collection%ValSize))   :: NodeLo
    TYPE(TabNode(Collection%ValSize))   :: NodeHi

! FLOW
    
    ! set keys
    CALL NodeLo%Key%Set(KeyLo)
    CALL NodeHi%Key%Set(KeyHi)
    
    ! get range size
    Size = Collection%WrkTree%GetRangeSize(NodeLo, NodeHi)

    RETURN

END FUNCTION TreeTable_RangeSize

! ---------------------------------------------------------------------
! -----         Specific Procedures for TreeTable Type            -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_UseAVLTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to false.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = FalseVal

    RETURN

END SUBROUTINE TreeTable_UseAVLTree

!******************************************************************************

SUBROUTINE TreeTable_UseRBTree(Collection)

!** PURPOSE OF THIS SUBROUTINE:
	!^ To set the *IsRBTree* component to true.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% collection object
    CLASS(TreeTable(*)), INTENT(INOUT)  :: Collection
 
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    Collection%IsRBTree = TrueVal

    RETURN

END SUBROUTINE TreeTable_UseRBTree

!******************************************************************************

FUNCTION TreeTable_CheckIntegrity(Collection, Message) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of the binary-search-tree (BST) data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)),  INTENT(INOUT) :: Collection
    !> message indicating the reason why the tree did not pass the
    !  integrity test
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: Message
    !> flag for integrity <br>
    ! - true if the tree passed the integrity test.
    ! - false if the tree did not.
    tLogical                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (Collection%IsRBTree) THEN
        ! WrkTree is a red-black tree
        Flag = Collection%WrkTree%Check(Message)
    ELSE
        ! WrkTree is an AVL tree
        Flag = Collection%WrkTree%IntrusiveAVLTree%Check(Message)
    END IF

    RETURN

END FUNCTION TreeTable_CheckIntegrity

! ---------------------------------------------------------------------
! -----                 Auxiliary Procedures                      -----
! ---------------------------------------------------------------------

FUNCTION TreeTable_FindKey(Collection, Key, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key in a symbol table.  Optionally, return
    !  a stored node containing a key equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% symbol-table collection object
    CLASS(TreeTable(*)),                  INTENT(INOUT) :: Collection
    !% the key to be looked for in the collection
    CLASS(*),                             INTENT(IN)    :: Key
    !% the node containing the specified key; null pointer if the key is not found
    CLASS(TabNode(*)), OPTIONAL, POINTER, INTENT(OUT)   :: KeyNode
    !% flag indicating whether the specified key is found or not.
    tLogical                                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(TabNode(Collection%ValSize))   :: InNode
    CLASS(BSTNode), POINTER             :: StoredNode

! FLOW
    
    ! initialize
    CALL InNode%Key%Set(Key)
    IF (PRESENT(KeyNode)) KeyNode => NULL()
    
    ! find the stored node equal to input node
    Found = Collection%WrkTree%Contain(InNode, StoredNode)
    IF (PRESENT(KeyNode)) THEN
        SELECT TYPE (StoredNode)
        TYPE IS (TabNode(*))
            KeyNode => StoredNode
        END SELECT
    END IF
    
    ! free working pointer
    NULLIFY(StoredNode)

    RETURN

END FUNCTION TreeTable_FindKey

! ---------------------------------------------------------------------
! -----             Final Procedures                              -----
! ---------------------------------------------------------------------

SUBROUTINE TreeTable_Finalize(Collection)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(TreeTable(*)), INTENT(INOUT)   :: Collection

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Collection%Destruct()

    RETURN

END SUBROUTINE TreeTable_Finalize

!******************************************************************************

END MODULE Class_TreeTable

!******************************************************************************

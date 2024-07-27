    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger, PARAMETER     :: MsgLen = 128
    ! traverse flags
    tInteger, PARAMETER     :: Inorder   = 1
    tInteger, PARAMETER     :: Preorder  = 2
    tInteger, PARAMETER     :: Postorder = 3
    ! color flags
    tLogical, PARAMETER     :: Red   = TrueVal
    tLogical, PARAMETER     :: Black = FalseVal

!** DERIVED TYPE DEFINITIONS
    !> *RedBlackNode* is a red-black tree node type that consists of a key, a value and two
    !   pointers of the node type.  The  *RedBlackNode* type is a private type.
   TYPE RedBlackNode
        PRIVATE
        ! 'Key' is the key in a key-value pair stored in the symbol table.
        ! 'KeyType' can be an intrinsic type (integer, real or character).
        KeyTypeA                    :: Key
        ! 'Value' is the value associated with a give key stored in the symbol table.
        ! 'ValueType' can be any intrinsic or derived type.
        CLASS(*),       ALLOCATABLE :: Value
        ! pointer to the left node (or subtree)
        TYPE(RedBlackNode), POINTER :: Left  => NULL()
        ! pointer to the right node (or subtree)
        TYPE(RedBlackNode), POINTER :: Right => NULL()
        ! number of nodes in subtree rooted by this node
        tIndex                      :: Size = 0
        ! color of parent node
        tLogical                    :: Color = Black
    CONTAINS
        PRIVATE
        PROCEDURE   :: SetNew       => BSTNode_SetNewKeyNValue
        PROCEDURE   :: Destruct     => BSTNode_Destructor
    END TYPE RedBlackNode
    !> *RedBlackTree* is a container type that employs a left-leaning red-black (RB) tree
    !   implementation to provide common operations for an ordered symbol table.
    TYPE RedBlackTree
        PRIVATE
        ! pointer to the root node (topmost item) of the tree
        TYPE(RedBlackNode), POINTER :: Root   => NULL()
        ! pointer to the current item (or node) used for iteration purpose
        TYPE(RedBlackNode), POINTER :: Cursor => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: BSTree_Destructor_I
        PROCEDURE, PRIVATE  :: BSTree_Destructor_II
        PROCEDURE, PRIVATE  :: BSTree_Traverse_I
        PROCEDURE, PRIVATE  :: BSTree_Traverse_II
        PROCEDURE, PRIVATE  :: BSTree_GetKeys_Range
        PROCEDURE, PRIVATE  :: BSTree_GetKeys_All
        PROCEDURE, PRIVATE  :: IsRankConsistent => BSTree_IsRankConsistent
        PROCEDURE, PRIVATE  :: IsSizeConsistent => BSTree_IsSizeConsistent
        PROCEDURE, PRIVATE  :: IsBSTree         => BSTree_IsBinarySearchTree
        PROCEDURE, PRIVATE  :: Is23Tree         => BSTree_Is23Tree
        PROCEDURE, PRIVATE  :: IsBalanced       => BSTree_IsBalanced
        GENERIC,   PRIVATE  :: Traverse         => BSTree_Traverse_I, BSTree_Traverse_II
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        ! -----     constructor and destructor procedures   -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Construct <br>
        ! **Purpose**:  To construct a tree from arrays of keys and values.  <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Construct(10, KeyArr, ValArr)
        PROCEDURE   :: Construct        => BSTree_ConstructByArray
        !> **Type-Bound Subroutine**: Destruct <br>
        !  **Purpose**:  To remove all key-value pairs from the tree and optionally to
        !                retrieve stored keys and values. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Destruct() <br>
        !   --->    CALL Tree%Destruct(KeyQueue, ValQueue) <br>
        GENERIC     :: Destruct         => BSTree_Destructor_I,  BSTree_Destructor_II
        ! -------------------------------------------------------
        ! -----         adding and removing procedures      -----
        ! -------------------------------------------------------
        !> **Type-Bound Subroutine**: Insert <br>
        !  **Purpose**:  To insert the specified key-value pair to the tree. <br>
        !  **Usage**: <br>
        !   --->    CALL Tree%Insert(Key, Value) <br>
        PROCEDURE   :: Insert           => BSTree_Insert
        !> **Type-Bound Subroutine**: Remove <br>
        !  **Purpose**:  To remove the specified key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not.  Optionally, the associated value of the specified key can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Remove(Key) <br>
        !   --->    IF (.NOT.Tree%Remove(Key, Value)) DoSomething <br>
        PROCEDURE   :: Remove           => BSTree_Remove
        !> **Type-Bound Subroutine**: RemoveMin <br>
        !  **Purpose**:  To remove the smallest key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not.  Optionally, the smallest key and/or its associated value can be retrieved. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%RemoveMin() <br>
        !   --->    Flag = Tree%RemoveMin(Key) <br>
        !   --->    IF (.NOT.Tree%RemoveMin(Value=Value)) DoSomething <br>
        !   --->    IF (.NOT.Tree%RemoveMin(Key, Value)) DoSomething <br>
        PROCEDURE   :: RemoveMin        => BSTree_RemoveMin
        !> **Type-Bound Subroutine**: RemoveMax <br>
        !  **Purpose**:  To remove the largest key (and its associated value) from the tree
        !       and return a flag indicating whether the key-value pair is successfully removed
        !       or not.  Optionally, the largest key and/or its associated valuecan be retrieved. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%RemoveMax() <br>
        !   --->    Flag = Tree%RemoveMax(Key) <br>
        !   --->    IF (.NOT.Tree%RemoveMax(Value=Value)) DoSomething <br>
        !   --->    IF (.NOT.Tree%RemoveMax(Key, Value)) DoSomething <br>
        PROCEDURE   :: RemoveMax        => BSTree_RemoveMax
        ! -------------------------------------------------------
        ! -----           tree-traversing procedures        -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: StartFirst <br>
        !  **Purpose**:  To start an iteration (at a node with the smallest key) and return a flag
        !                indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartFirst() <br>
        !   --->    IsEmpty = Tree%StartFirst(FirstKey) <br>
        !   --->    IsEmpty = Tree%StartFirst(Value=FirstVal) <br>
        !   --->    IsEmpty = Tree%StartFirst(FirstKey, FirstVal)
        PROCEDURE   :: StartFirst       => BSTree_Move2First
        !> **Type-Bound Function**: MoveForward <br>
        !  **Purpose**:  To move to the next key-value pair and return a flag indicating whether
        !                the cursor pointer has reached the end of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveForward() <br>
        !   --->    IsTheEnd = Tree%MoveForward(NextKey) <br>
        !   --->    IsTheEnd = Tree%MoveForward(Value=NextVal) <br>
        !   --->    IsTheEnd = Tree%MoveForward(NextKey, NextVal)
        PROCEDURE   :: MoveForward      => BSTree_Move2NextPair
        !> **Type-Bound Function**: StartLast <br>
        !  **Purpose**:  To start an iteration in a reversed order (at a node with the largest key)
        !                and return a flag indicating whether the tree is empty or not. <br>
        !  **Usage**: <br>
        !   --->    IsEmpty = Tree%StartLast() <br>
        !   --->    IsEmpty = Tree%StartLast(LastKey) <br>
        !   --->    IsEmpty = Tree%StartLast(Value=LastVal) <br>
        !   --->    IsEmpty = Tree%StartLast(LastKey, LastVal)
        PROCEDURE   :: StartLast        => BSTree_Move2Last
        !> **Type-Bound Function**: MoveBackward <br>
        !  **Purpose**:  To move to the previous key-value pair and return a flag indicating whether
        !                the cursor pointer has reached the end of the tree or not. <br>
        !  **Usage**: <br>
        !   --->    IsTheEnd = Tree%MoveBackward() <br>
        !   --->    IsTheEnd = Tree%MoveBackward(PrevKey) <br>
        !   --->    IsTheEnd = Tree%MoveBackward(Value=PrevVal) <br>
        !   --->    IsTheEnd = Tree%MoveBackward(PrevKey, PrevVal)
        PROCEDURE   :: MoveBackward     => BSTree_Move2PrevPair
        ! -------------------------------------------------------
        ! -----               inquiry procedures            -----
        ! -------------------------------------------------------
        !> **Type-Bound Function**: IsEmpty <br>
        !  **Purpose**:  To check whether the three is empty or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%IsEmpty() <br>
        !   --->    IF (.NOT.Tree%IsEmpty()) DoSomeThing
        PROCEDURE   :: IsEmpty          => BSTree_IsEmpty
        !> **Type-Bound Function**: Contain <br>
        !  **Purpose**:  To find the specified key in the tree.  Return true if the specified key
        !                is found.  Otherwise, return false. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Contain(Key) <br>
        !   --->    IF (.NOT.Tree%Contain(Key)) DoSomething
        PROCEDURE   :: Contain          => BSTree_FindKey
        !> **Type-Bound Function**: GetSize <br>
        !  **Purpose**:  To get size of the tree (the number of key-value pair stored in the tree). <br>
        !  **Usage**: <br>
        !   --->    TreeSize = Tree%GetSize()
        PROCEDURE   :: GetSize          => BSTree_GetSize
        !> **Type-Bound Function**: GetRangeSize <br>
        !  **Purpose**:  To return the number of keys between *KeyLo* (inclusive)
        !                and *KeyHi* (inclusive). <br>
        !  **Usage**: <br>
        !   --->    RangeSize = Tree%GetRangeSize(KeyLo, KeyHi)
        PROCEDURE   :: GetRangeSize     => BSTree_GetSize_Range
        !> **Type-Bound Function**: GetValue <br>
        !  **Purpose**:  To get the value associated with the specified key in the tree.  Also, return
        !                a flag indicating whether the key-value pair is successfully found or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%GetValue(Key, Value) <br>
        !   --->    IF (.NOT.Tree%GetValue(Key, Value)) DoSomething
        PROCEDURE   :: GetValue       => BSTree_GetValue
        !> **Type-Bound Function**: GetMinKey <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated with it) in the
        !                tree.  Also, return a flag indicating whether the key is successfully retrieved
        !                or not.  If the tree is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%GetMinKey(Key) <br>
        !   --->    IF (.NOT.Tree%GetMinKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMinKey        => BSTree_GetSmallestKey
        !> **Type-Bound Function**: GetMaxKey <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated with it) in the
        !                tree.  Also, return a flag indicating whether the key is successfully retrieved
        !                or not.  If the tree is not empty, the returned flag is always true. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%GetMaxKey(Key) <br>
        !   --->    IF (.NOT.Tree%GetMaxKey(Key, Value)) DoSomething
        PROCEDURE   :: GetMaxKey        => BSTree_GetLargestKey
        !> **Type-Bound Subroutine**: GetKeys <br>
        !  **Purpose**:  To return all keys (in the tree or in the specified range) and
        !                optionally all associated values. <br>
        !  **Usage**: <br>
        !   ! return all keys in the tree <br>
        !   --->    CALL Tree%GetKeys(Keys) <br>
        !   --->    CALL Tree%GetKeys(Keys, Values) <br>
        !   ! return all keys in the specified range <br>
        !   --->    CALL Tree%GetKeys(LoKey, HiKey, Keys) <br>
        !   --->    CALL Tree%GetKeys(LoKey, HiKey, Keys, Values) <br>
        GENERIC     :: GetKeys          => BSTree_GetKeys_Range, BSTree_GetKeys_All
        !> **Type-Bound Function**: Floor <br>
        !  **Purpose**:  To get the largest key (and optionally a value associated with it) in the
        !                tree less than or equal to the given key.  Also, return a flag indicating
        !                whether the floor key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Floor(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Tree%Floor(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Floor            => BSTree_Floor
        !> **Type-Bound Function**: Ceiling <br>
        !  **Purpose**:  To get the smallest key (and optionally a value associated with it) in the
        !                tree greater than or equal to the given key.  Also, return a flag indicating
        !                whether the ceiling key is successfully retrieved or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Ceiling(KeyIn, KeyOut) <br>
        !   --->    IF (.NOT.Tree%Ceiling(KeyIn, KeyOut, Value)) DoSomething
        PROCEDURE   :: Ceiling          => BSTree_Ceiling
        !> **Type-Bound Subroutine**: Select <br>
        !  **Purpose**:  To get the key (and optionally its associated value) of the specified rank
        !                where the applicable range of rank is between 0 and TableSize-1. Also, return
        !                a flag indicating whether the ranked key is successfully retrieved or not. <br>
        !   --->    Flag = Tree%Select(Rank, Key) <br>
        !   --->    IF (.NOT.Tree%Select(Rank, Key, Value)) DoSomething
        PROCEDURE   :: Select           => BSTree_Select
        !> **Type-Bound Function**: GetRank <br>
        !  **Purpose**:  To return the number of keys in the tree strictly less than the given key. <br>
        !  **Usage**: <br>
        !   --->    KeyRank = Tree%GetRank(Key)
        PROCEDURE   :: GetRank          => BSTree_Rank
        !> **Type-Bound Function**: Check <br>
        !  **Purpose**:  To check integrity of the BST data structure. <br>
        !  **Usage**: <br>
        !   --->    Flag = Tree%Check() <br>
        !   --->    IF (.NOT.Tree%Check(ErrMsg)) DoSomething
        PROCEDURE   :: Check            => BSTree_CheckBST
        ! ---------------------------------------------------------------------
        ! -----                   Finalize Procedure                      -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the tree.
        FINAL       :: BSTree_Finalizer
        ! ---------------------------------------------------------------------
    END TYPE RedBlackTree
    
!** INTERFACE DEFINITIONS:
    ! abstract interfaces
    ABSTRACT INTERFACE
        !> IteratureFunc is a user-suppled procedure used to traverse the tree
        !  in order to get the tree's keys and values.
        FUNCTION IteratorFunc(Key,Value,Done) RESULT(ErrStat)
            IMPORT
            KeyTypeB, INTENT(IN)    :: Key      !! key
            CLASS(*), INTENT(IN)    :: Value    !! value
            tLogical, INTENT(INOUT) :: Done
            !^ on input, Done is set to .FALSE. <br>
            !  on exit, set it to .TRUE. if user want to stop the tree traversing. <br>
            tLogical                :: ErrStat  ! true if error occurred in the user routine
        END FUNCTION IteratorFunc
        !> IteratorLocal is a procedure used (locally in this module) to traverse
        !  the list in order to get the list's node.
        FUNCTION IteratorLocal(Node,Done) RESULT(ErrStat)
            IMPORT
            TYPE(RedBlackNode), INTENT(IN)      :: Node     !! node
            tLogical,           INTENT(INOUT)   :: Done
            !^ on input, Done is set to .FALSE. <br>
            !  on exit, set it to .TRUE. if user want to stop the tree traversing. <br>
            tLogical                            :: ErrStat  !! true if error occurred in the user routine
        END FUNCTION IteratorLocal
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message
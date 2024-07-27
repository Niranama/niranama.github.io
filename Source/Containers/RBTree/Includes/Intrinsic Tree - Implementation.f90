
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Type-Bound Procedures for RedBlackNode
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE BSTNode_SetNewKeyNValue(Node,Key,Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set new key and value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackNode), INTENT(INOUT)  :: Node     !! RedBlackNode object
    KeyTypeB,            INTENT(IN)     :: Key      !! key
    CLASS(*),            INTENT(IN)     :: Value    !! value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Node%Key = Key
    ALLOCATE(Node%Value, SOURCE=Value)
    Node%Size = 1
    Node%Color = Red
       
    RETURN

END SUBROUTINE BSTNode_SetNewKeyNValue

!******************************************************************************

SUBROUTINE BSTNode_Destructor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct RedBlackNode object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackNode), INTENT(INOUT)  :: Node     !! RedBlackNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT TYPE (Item => Node%Value)
    CLASS IS (Assignable)
        CALL Item%MemFree()
    END SELECT
    Node%Size = 0
    Node%Color = Black
    DEALLOCATE(Node%Value)
    NULLIFY(Node%Left)
    NULLIFY(Node%Right)
       
    RETURN

END SUBROUTINE BSTNode_Destructor

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

RECURSIVE FUNCTION AddKeyNValue(InNode, Key, Value) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the given key-value pair into the tree.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node (root node of (sub)tree)
    KeyTypeB,                    INTENT(IN)     :: Key      !! key
    CLASS(*),                    INTENT(IN)     :: Value    !! value
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check whether the input node is null or not
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! create new node
        CALL CreateNewNode(Key, Value, OutNode)
        RETURN
    END IF
        
    ! compare the specified key with the node's key
    IF (Key < InNode%Key) THEN
                
        ! add the specified key and value to the left subtree
        InNode%Left => AddKeyNValue(InNode%Left, Key, Value)
            
    ELSEIF (Key > InNode%Key) THEN
                
        ! add the specified key and value to the right subtree
        InNode%Right => AddKeyNValue(InNode%Right, Key, Value)
            
    ELSE
                
        ! replace value
        InNode%Value = Value
        
    END IF
            
    ! fix-up any right-leaning links
    IF (IsRed(InNode%Right).AND..NOT.IsRed(InNode%Left)) InNode => LeftRotate(InNode)
    IF (IsRed(InNode%Left)) THEN
        IF (IsRed(InNode%Left%Left)) THEN
            InNode => RightRotate(InNode)
        END IF
    END IF
    IF (IsRed(InNode%Left).AND.IsRed(InNode%Right)) CALL FlipColor(InNode)
            
    ! update number of nodes in subtree rooted by the output node
    InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION AddKeyNValue

!******************************************************************************

SUBROUTINE CreateNewNode(Key, Value, Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a new node and then set its key and value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    KeyTypeB,                    INTENT(IN)     :: Key      !! key to be set to the node
    CLASS(*),                    INTENT(IN)     :: Value    !! value to be set to the node
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: Node     !! new node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
        
    ! allocate new node
    ALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        
    ! check allocation status and report error if necessary
    CALL Handle_ErrAlloc('CreateNewNode', ModName, AllocMsg, AllocStat)
        
    ! set the key and value
    CALL Node%SetNew(Key, Value)
        
    RETURN

END SUBROUTINE CreateNewNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Predecessor(Root, Key, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the previous node in an inorder traversal of the tree
    !  of the specified node given by its key. <br>
    !  Note: The first call of this routine should provide the root of the
    !        tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Root     !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key of the node
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: PrvNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (Key == Root%Key) THEN
        ! the maximum value in the left subtree is predecessor node
        IF (ASSOCIATED(Root%Left)) THEN
            ! iteratively search the rightmost node of the left subtree
            Temp => Root%Left
            DO WHILE (ASSOCIATED(Temp%Right))
                Temp => Temp%Right
            END DO
            PrvNode => Temp
            NULLIFY(Temp)
        END IF
        RETURN
    END IF
        
    IF (Key < Root%Key) THEN
        ! If key is smaller than the key of the root, go to left subtree
        CALL Find_Inorder_Predecessor(Root%Left, Key, PrvNode)
    ELSE
        ! Otherwise, go to right subtree
        PrvNode => Root
        CALL Find_Inorder_Predecessor(Root%Right, Key, PrvNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Predecessor

!******************************************************************************

RECURSIVE SUBROUTINE Find_Inorder_Successor(Root, Key, NxtNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the next node in an inorder traversal of the tree
    !  of the specified node given by its key.
    !  Note: The first call of this routine should provide the root of
    !        the tree as an input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Root     !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key of the node
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: NxtNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: Temp => NULL()

! FLOW
        
    ! base case
    IF (.NOT.ASSOCIATED(Root)) RETURN
        
    ! If key is equal to the key of the root
    IF (Key == Root%Key) THEN
        ! the minimum value in the right subtree is the successor node
        IF (ASSOCIATED(Root%Right)) THEN
            ! iteratively search the leftmost node of the right subtree
            Temp => Root%Right
            DO WHILE (ASSOCIATED(Temp%Left))
                Temp => Temp%Left
            END DO
            NxtNode => Temp
            NULLIFY(Temp)
        END IF
        RETURN
    END IF
        
    IF (Key < Root%Key) THEN
        ! If key is smaller than the key of the root, go to left subtree
        NxtNode => Root
        CALL Find_Inorder_Successor(Root%Left, Key, NxtNode)
    ELSE
        ! Otherwise, go to right subtree
        CALL Find_Inorder_Successor(Root%Right, Key, NxtNode)
    END IF

    RETURN

END SUBROUTINE Find_Inorder_Successor

!******************************************************************************

SUBROUTINE Find_Inorder_PrevNode(Node, Root, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the previous node in an inorder traversal of the tree
    !  of the specified input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Node     !! input node
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Root     !! root node
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: PrvNode  !! previous node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If left subtree of node is not NULL, then PrvNode lies in left subtree.
    ! Thus, go to left subtree and return the node with maximum key value
    ! in the left subtree.
    IF (ASSOCIATED(Node%Left)) THEN
        CALL Find_MaxKeyNode(Node%Left, PrvNode)
        RETURN
    END IF
        
    ! start from root and iteratively search for the previous node down the tree
    PrvNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node%Key < CurRoot%Key) THEN
            ! the previous node should be on the left subtree
            CurRoot => CurRoot%Left
        ELSEIF (Node%Key > CurRoot%Key) THEN
            ! the previous node should be on the right subtree
            PrvNode => CurRoot
            CurRoot => CurRoot%Right
        ELSE
            ! the previous node found
            EXIT
        END IF
    END DO

    RETURN

END SUBROUTINE Find_Inorder_PrevNode

!******************************************************************************

SUBROUTINE Find_Inorder_NextNode(Node, Root, NxtNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the next node in an inorder traversal of the tree
    !  of the specified input node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Node     !! input node
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Root     !! root node
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: NxtNode  !! next node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If right subtree of node is not NULL, then NxtNode lies in right subtree.
    ! Thus, go to right subtree and return the node with minimum key value
    ! in the right subtree.
    IF (ASSOCIATED(Node%Right)) THEN
        CALL Find_MinKeyNode(Node%Right, NxtNode)
        RETURN
    END IF
        
    ! start from root and search for the next node down the tree
    NxtNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node%Key < CurRoot%Key) THEN
            ! the next node should be on the left subtree
            NxtNode => CurRoot
            CurRoot => CurRoot%Left
        ELSEIF (Node%Key > CurRoot%Key) THEN
            ! the next node should be on the right subtree
            CurRoot => CurRoot%Right
        ELSE
            ! the next node found
            EXIT
        END IF
    END DO

    RETURN

END SUBROUTINE Find_Inorder_NextNode

!******************************************************************************

SUBROUTINE Find_MinKeyNode(Node, MinNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the leftmost leaf of the given node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Node     !! input node
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: MinNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set pointer for working/output node
    MinNode => Node
        
    ! loop down to find the leftmost leaf
    DO WHILE (ASSOCIATED(MinNode%Left))
        MinNode => MinNode%Left
    END DO

    RETURN

END SUBROUTINE Find_MinKeyNode

!******************************************************************************

SUBROUTINE Find_MaxKeyNode(Node, MaxNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the rightmost leaf of the given node.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: Node     !! input node
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: MaxNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! set pointer for working/output node
    MaxNode => Node
        
    ! loop down to find the rightmost leaf
    DO WHILE (ASSOCIATED(MaxNode%Right))
        MaxNode => MaxNode%Right
    END DO

    RETURN

END SUBROUTINE Find_MaxKeyNode

!******************************************************************************

RECURSIVE SUBROUTINE FindNode(InNode, Key, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To search for the node having the same key as the specified one.
    !  Return null if the node does not exist.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: InNode   !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key to be looked for
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (Key == InNode%Key) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (Key < InNode%Key) THEN
        ! the node should be on the left subtree
        CALL FindNode(InNode%Left, Key, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL FindNode(InNode%Right, Key, OutNode)
    END IF

    RETURN

END SUBROUTINE FindNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_FloorNode(InNode, Key, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the node containing largest key in the tree
    !  less than or equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: InNode   !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (Key == InNode%Key) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (Key < InNode%Key) THEN
        ! the node should be on the left subtree
        CALL Find_FloorNode(InNode%Left, Key, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL Find_FloorNode(InNode%Right, Key, OutNode)
        ! if output node is null, return the input node instead
        IF (.NOT.ASSOCIATED(OutNode)) OutNode => InNode
    END IF

    RETURN

END SUBROUTINE Find_FloorNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_CeilingNode(InNode, Key, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the node containing smallest key in the tree
    !  greater than or equal to the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: InNode   !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key
    TYPE(RedBlackNode), POINTER, INTENT(OUT)    :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (Key == InNode%Key) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (Key < InNode%Key) THEN
        ! the node should be on the left subtree
        CALL Find_CeilingNode(InNode%Left, Key, OutNode)
        ! if output node is null, return the input node instead
        IF (.NOT.ASSOCIATED(OutNode)) OutNode => InNode
    ELSE
        ! the node should be on the right subtree
        CALL Find_CeilingNode(InNode%Right, Key, OutNode)
    END IF

    RETURN

END SUBROUTINE Find_CeilingNode

!******************************************************************************

RECURSIVE FUNCTION SelectKey(InNode, Rank, Key, Value) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return key in the subtree rooted at InNode of given rank. <br>
    !  Precondition: rank is in legal range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode),     POINTER, INTENT(IN)     :: InNode   !! input node
    tIndex,                          INTENT(IN)     :: Rank     !! rank of the key
    KeyTypeA,                        INTENT(OUT)    :: Key      !! key of the given rank
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    tLogical                                        :: Found    !! true if the key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: LeftSize

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        Found = FalseVal
        RETURN
    END IF
        
    ! determine number of keys in the left subtree
    LeftSize = NodeSize(InNode%Left)
        
    ! find the node with a given rank
    IF (LeftSize > Rank) THEN
        ! the node should be on the left subtree
        Found = SelectKey(InNode%Left, Rank, Key, Value)
    ELSEIF (LeftSize < Rank) THEN
        ! the node should be on the right subtree
        Found = SelectKey(InNode%Right, Rank-LeftSize-1, Key, Value)
    ELSE
        ! the node is found
        Found = TrueVal
        Key   = InNode%Key
        IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=InNode%Value)
    END IF

    RETURN

END FUNCTION SelectKey

!******************************************************************************

RECURSIVE FUNCTION KeyRank(InNode, Key) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the rank of the given key (i.e. the number
    !  of keys in the tree that are less than the given key).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN)     :: InNode   !! input node
    KeyTypeB,                    INTENT(IN)     :: Key      !! key
    tIndex                                      :: Rank     !! rank of the key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        Rank = 0
        RETURN
    END IF
        
    IF (Key < InNode%Key) THEN
        Rank = KeyRank(InNode%Left, Key)
    ELSEIF (Key > InNode%Key) THEN
        Rank = KeyRank(InNode%Right, Key) + NodeSize(InNode%Left) + 1
    ELSE
        Rank = NodeSize(InNode%Left)
    END IF

    RETURN

END FUNCTION KeyRank

!******************************************************************************

RECURSIVE FUNCTION DeleteNode(InNode, Key, Value) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER,     INTENT(INOUT)  :: InNode   !! input node (root node of (sub)tree)
    KeyTypeB,                        INTENT(IN)     :: Key      !! key
    CLASS(*), OPTIONAL, ALLOCATABLE, INTENT(OUT)    :: Value    !! associated value
    TYPE(RedBlackNode), POINTER                     :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: MinNode => NULL()

! FLOW
        
    ! return if the (sub)tree is empty
    IF (.NOT.ASSOCIATED(InNode)) THEN
        OutNode => NULL()
        RETURN
    END IF

    ! find the node to be deleted
    IF (Key < InNode%Key) THEN
            
        ! check whether to move red node to the left
        IF (.NOT.IsRed(InNode%Left)) THEN
            IF (ASSOCIATED(InNode%Left)) THEN
                IF (.NOT.IsRed(InNode%Left%Left)) InNode => MoveRedLeft(InNode)
            END IF
        END IF
            
        ! find the node on the left subtree
        InNode%Left => DeleteNode(InNode%Left, Key, Value)
            
    ELSE
            
        ! check whether to perform right rotation
        IF (IsRed(InNode%Left)) InNode => RightRotate(InNode)

        ! check whether the key is found with the null right child of the key node
        IF ((Key == InNode%Key).AND.(.NOT.ASSOCIATED(InNode%Right))) THEN

            ! get node value if requested
            IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=InNode%Value)

            ! delete node from the tree
            CALL InNode%Destruct()
            CALL FreeTreeNode(InNode)
                
            ! reset the input node
            InNode => NULL()
                
            ! set output node and return
            OutNode => InNode
            RETURN
                
        END IF
        
        ! check whether to move red node to the right
        IF (.NOT.IsRed(InNode%Right)) THEN
            IF (ASSOCIATED(InNode%Right)) THEN
                IF (.NOT.IsRed(InNode%Right%Left)) InNode => MoveRedRight(InNode)
            END IF
        END IF
            
        IF (Key == InNode%Key) THEN
                
            ! get optional output
            IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=InNode%Value)
                    
            ! find inorder successor of InNode
            CALL Find_MinKeyNode(InNode%Right, MinNode)

            ! set the inorder successor data at the position of the node supposedly to be deleted
            InNode%Key = MinNode%Key
            ALLOCATE(InNode%Value, SOURCE=MinNode%Value)
                
            ! actually delete the inorder successor node instead
            InNode%Right => DeleteMinKeyNode(InNode%Right)
                
            ! free working pointers
            NULLIFY(MinNode)
        
        ELSE
                
            ! find the node on the right subtree
            InNode%Right => DeleteNode(InNode%Right, Key, Value)
            
        END IF

    END IF

    ! rebalance
    InNode => Rebalance(InNode)
        
    ! set output node and return
    OutNode => InNode
                
    RETURN

END FUNCTION DeleteNode

!******************************************************************************

RECURSIVE FUNCTION DeleteMinKeyNode(InNode, Key, Value) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER,     INTENT(INOUT)  :: InNode   !! input node (root node of (sub)tree)
    KeyTypeA, OPTIONAL,              INTENT(OUT)    :: Key      !! smallest key
    CLASS(*), OPTIONAL, ALLOCATABLE, INTENT(OUT)    :: Value    !! associated value
    TYPE(RedBlackNode), POINTER                     :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode%Left)) THEN
                
        ! smallest key found so get optional output if requested
        IF (PRESENT(Key))     Key     = InNode%Key
        IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=InNode%Value)
            
        ! delete node from the tree
        CALL InNode%Destruct()
        CALL FreeTreeNode(InNode)
                
        ! reset the input node
        InNode => NULL()
                
    ELSE
            
        ! check whether to move red node to the left
        IF (.NOT.IsRed(InNode%Left)) THEN
            IF (ASSOCIATED(InNode%Left)) THEN
                IF (.NOT.IsRed(InNode%Left%Left)) InNode => MoveRedLeft(InNode)
            END IF
        END IF

        ! find the node on the left subtree
        InNode%Left => DeleteMinKeyNode(InNode%Left, Key, Value)
            
        ! rebalance
        InNode => Rebalance(InNode)
                
    END IF
            
    ! set output node
    OutNode => InNode

    RETURN

END FUNCTION DeleteMinKeyNode

!******************************************************************************

RECURSIVE FUNCTION DeleteMaxKeyNode(InNode, Key, Value) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and its associated value from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER,     INTENT(INOUT)  :: InNode   !! input node (root node of (sub)tree)
    KeyTypeA, OPTIONAL,              INTENT(OUT)    :: Key      !! largest key
    CLASS(*), OPTIONAL, ALLOCATABLE, INTENT(OUT)    :: Value    !! associated value
    TYPE(RedBlackNode), POINTER                     :: OutNode  !! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! check whether to perform right rotation
    IF (IsRed(InNode%Left)) InNode => RightRotate(InNode)

    IF (.NOT.ASSOCIATED(InNode%Right)) THEN
            
        ! largest key found so get optional output if requested
        IF (PRESENT(Key))     Key     = InNode%Key
        IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=InNode%Value)
            
        ! delete node from the tree
        CALL InNode%Destruct()
        CALL FreeTreeNode(InNode)
            
        ! reset the input node
        InNode => NULL()
                
    ELSE
            
        ! check whether to move red node to the right
        IF (.NOT.IsRed(InNode%Right)) THEN
            IF (ASSOCIATED(InNode%Right)) THEN
                IF (.NOT.IsRed(InNode%Right%Left)) InNode => MoveRedRight(InNode)
            END IF
        END IF

        ! find the node on the right subtree
        InNode%Right => DeleteMaxKeyNode(InNode%Right, Key, Value)
            
        ! rebalance
        InNode => Rebalance(InNode)

    END IF
        
    ! set output node
    OutNode => InNode

    RETURN

END FUNCTION DeleteMaxKeyNode

!******************************************************************************

FUNCTION NodeSize(Node) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get number of nodes in subtree rooted by this node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN) :: Node !! RedBlackNode object
    tIndex                                  :: Size !! number of nodes in subtree rooted by this node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        Size = Node%Size
    ELSE
        Size = 0_kIndex
    END IF
       
    RETURN

END FUNCTION NodeSize

!******************************************************************************

FUNCTION IsRed(Node) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the color of this node is red.
    !  Return FalseVal if the node is not associated.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(IN) :: Node !! RedBlackNode object
    tLogical                                :: Flag !! true if the color is red

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        Flag = (Node%Color .EQV. Red)
    ELSE
        Flag = Black
    END IF
       
    RETURN

END FUNCTION IsRed

!******************************************************************************

FUNCTION RightRotate(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a left-leaning link lean to the right.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node (root of input subtree)
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER   :: NodeA => NULL()
    TYPE(RedBlackNode), POINTER   :: NodeB => NULL()
    TYPE(RedBlackNode), POINTER   :: NodeC => NULL()

! FLOW
    
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode).AND.IsRed(InNode%Left), 'RotateRight')
        
    ! set working nodes
    NodeA => InNode
    NodeB => NodeA%Left
    NodeC => NodeB%Right
        
    ! perform right rotation
    NodeB%Right => NodeA
    NodeA%Left  => NodeC
        
    ! update the number of nodes in subtree rooted by NodeA and NodeB
    NodeB%Size = NodeA%Size
    NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
    ! update color
    NodeB%Color = NodeB%Right%Color
    NodeB%Right%Color = Red
        
    ! set output root
    OutNode => NodeB
        
    ! free pointers
    NULLIFY(NodeA)
    NULLIFY(NodeB)
    NULLIFY(NodeC)
        
    RETURN

END FUNCTION RightRotate

!******************************************************************************

FUNCTION LeftRotate(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a right-leaning link lean to the left.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node (root of input subtree)
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER   :: NodeA => NULL()
    TYPE(RedBlackNode), POINTER   :: NodeB => NULL()
    TYPE(RedBlackNode), POINTER   :: NodeC => NULL()

! FLOW
    
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode).AND.IsRed(InNode%Right), 'RotateLeft')
        
    ! set working nodes
    NodeA => InNode
    NodeB => NodeA%Right
    NodeC => NodeB%Left
        
    ! perform left rotation
    NodeB%Left  => NodeA
    NodeA%Right => NodeC
        
    ! update the number of nodes in subtree rooted by NodeA and NodeB
    NodeB%Size = NodeA%Size
    NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
    ! update color
    NodeB%Color = NodeB%Left%Color
    NodeB%Left%Color = Red
        
    ! set output root
    OutNode => NodeB
        
    ! free pointers
    NULLIFY(NodeA)
    NULLIFY(NodeB)
    NULLIFY(NodeC)
        
    RETURN

END FUNCTION LeftRotate

!******************************************************************************

SUBROUTINE FlipColor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To flip the colors of a node and its two children.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Node   !! RedBlackNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%Color       = .NOT.Node%Color
    Node%Left%Color  = .NOT.Node%Left%Color
    Node%Right%Color = .NOT.Node%Right%Color
       
    RETURN

END SUBROUTINE FlipColor

!******************************************************************************

FUNCTION MoveRedLeft(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ Assuming that InNode is red and both InNode%Left and InNode%Left%Left
    !  are black, make InNode%Left or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'MoveRedLeft')
    ASSERT(IsRed(InNode).AND..NOT.IsRed(InNode%Left), 'MoveRedLeft')
#ifdef DebugMode
    IF (ASSOCIATED(InNode%Left)) THEN
        ASSERT(.NOT.IsRed(InNode%Left%Left), 'MoveRedLeft')
    END IF
#endif
     
    ! move the input node to the left
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Right)) THEN
        IF (IsRed(InNode%Right%Left)) THEN
            InNode%Right => RightRotate(InNode%Right)
            InNode => LeftRotate(InNode)
            CALL FlipColor(InNode)
        END IF
    END IF
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION MoveRedLeft

!******************************************************************************

FUNCTION MoveRedRight(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ Assuming that InNode is red and both InNode%Right and InNode%Right%Left
    !  are black, make InNode%Right or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'MoveRedRight')
    ASSERT(IsRed(InNode).AND..NOT.IsRed(InNode%Right), 'MoveRedRight')
#ifdef DebugMode
    IF (ASSOCIATED(InNode%Right)) THEN
        ASSERT(.NOT.IsRed(InNode%Right%Left), 'MoveRedRight')
    END IF
#endif
        
    ! move the input node to the right
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Left)) THEN
        IF (IsRed(InNode%Left%Left)) THEN
            InNode => RightRotate(InNode)
            CALL FlipColor(InNode)
        END IF
    END IF

    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION MoveRedRight

!******************************************************************************

FUNCTION Rebalance(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restore red-black tree invariant.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: InNode   !! input node
    TYPE(RedBlackNode), POINTER                 :: OutNode  !! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! for debugging purpose
    ASSERT(ASSOCIATED(InNode), 'Rebalance')
        
    ! check whether to rotate left or not
    IF (IsRed(InNode%Right).AND..NOT.IsRed(InNode%Left)) InNode => LeftRotate(InNode)
        
    ! check whether to rotate right or not
    IF (IsRed(InNode%Left)) THEN
        ! note: if the left node is red, it must not be null
        IF (IsRed(InNode%Left%Left)) InNode => RightRotate(InNode)
    END IF
        
    ! check whether to flip color or not
    IF (IsRed(InNode%Left).AND.IsRed(InNode%Right)) CALL FlipColor(InNode)
        
    ! set output node size
    InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        
    ! set output node
    OutNode => InNode
        
    RETURN

END FUNCTION Rebalance

!******************************************************************************

SUBROUTINE FreeTreeNode(Node)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To deallocate RedBlackNode pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Node   !! RedBlackNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: AllocStat
    tCharLen(MsgLen)    :: AllocMsg

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        DEALLOCATE(Node, STAT=AllocStat, ERRMSG=AllocMsg)
        NULLIFY(Node)
        CALL Handle_ErrDealloc('FreeTreeNode', ModName, AllocMsg, AllocStat)
    END IF
       
    RETURN

END SUBROUTINE FreeTreeNode

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Type-Bound Procedures for RedBlackTree
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE BSTree_ConstructByArray(Tree, N, Keys, Values)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To construct a tree based on specified arrays.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree         !! tree
    tIndex,              INTENT(IN)     :: N            !! number of keys
    KeyTypeB,            INTENT(IN)     :: Keys(N)      !! an array of keys
    CLASS(*),            INTENT(IN)     :: Values(N)    !! an array of associated values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW
        
    ! built tree from input arrays
    DO I = 1, N
        CALL Tree%Insert(Keys(I), Values(I))
    END DO

    RETURN

END SUBROUTINE BSTree_ConstructByArray

!******************************************************************************

SUBROUTINE BSTree_Destructor_I(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree     !! tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and destroy all nodes
    CALL DestroyNode(Tree%Root)

    ! nullify pointers
    NULLIFY(Tree%Root)
    NULLIFY(Tree%Cursor)
        
    RETURN
        
CONTAINS

    RECURSIVE SUBROUTINE DestroyNode(Node)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To destroy the specified node.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Node !! node
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! return if null
        IF (.NOT.ASSOCIATED(Node)) RETURN
            
        ! recursively remove left and right subtrees
        CALL DestroyNode(Node%Left)
        CALL DestroyNode(Node%Right)
            
        ! remove this node
        CALL Node%Destruct()
        CALL FreeTreeNode(Node)
            
        RETURN
            
    END SUBROUTINE DestroyNode
        
    !**************************************************************************

END SUBROUTINE BSTree_Destructor_I

!******************************************************************************

SUBROUTINE BSTree_Destructor_II(Tree, KeyQ, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To destruct a tree and get its pair data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree     !! tree
    TYPE(QueueKey),      INTENT(OUT)    :: KeyQ     !! a queue of stored keys
    TYPE(QueueVal),      INTENT(OUT)    :: ValueQ   !! a queue of stored values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree to get keys and values
    CALL Tree%Traverse(Inorder, GetData)
        
    ! destroy all nodes
    CALL Tree%Destruct()
        
    RETURN
        
CONTAINS

    FUNCTION GetData(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get key and value of the specified node

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), INTENT(IN)      :: Node     !! node
        tLogical,           INTENT(INOUT)   :: Done     !! on input, Done is set to FalseVal
                                                        !! on exit, set it to TrueVal if user
                                                        !!   want to stop the queue traversing.
        tLogical                            :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key and value to queues
        CALL KeyQ%EnQueue(Node%Key)
        CALL ValueQ%EnQueue(Node%Value)

        RETURN
            
    END FUNCTION GetData
        
    !**************************************************************************
        
END SUBROUTINE BSTree_Destructor_II

!******************************************************************************

SUBROUTINE BSTree_Finalizer(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(RedBlackTree), INTENT(INOUT)   :: Tree !! tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! destroy all nodes and free up memory
    CALL Tree%Destruct()
       
    RETURN

END SUBROUTINE BSTree_Finalizer

!******************************************************************************

SUBROUTINE BSTree_Insert(Tree, Key, Value)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert the given key-value pair into the tree.
    !  If the tree already contains the specified key, the
    !  old value is replaced with the new one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree     !! tree
    KeyTypeB,            INTENT(IN)     :: Key      !! key to be inserted
    CLASS(*),            INTENT(IN)     :: Value    !! value to be inserted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! recursively search for place to insert the key and value
    Tree%Root => AddKeyNValue(Tree%Root, Key, Value)
    Tree%Root%Color = Black

    RETURN

END SUBROUTINE BSTree_Insert

!******************************************************************************

FUNCTION BSTree_Remove(Tree, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified key and its associated value from the tree.
    !  Optionally, to retrieve the associated value of the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeB,                        INTENT(IN)     :: Key      !! key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Remove', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! check if the tree contains the specified key
    IF (.NOT.Tree%Contain(Key)) RETURN
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with the given key
    Tree%Root => DeleteNode(Tree%Root, Key, Value)

    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_Remove

!******************************************************************************

FUNCTION BSTree_RemoveMin(Tree, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the smallest key and its associated value from the tree.
    !  Optionally, to retrieve the smallest key and its the associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key      !! smallest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_RemoveMin', ModName, ErrWarning, &
                            'The binary search tree is empty.')
        RETURN
    END IF
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with smallest key
    Tree%Root => DeleteMinKeyNode(Tree%Root, Key, Value)
        
    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_RemoveMin

!******************************************************************************

FUNCTION BSTree_RemoveMax(Tree, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the largest key and its associated value from the tree.
    !  Optionally, to retrieve the largest key and its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key      !! largest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !> flag indicating whether the specified key and its associated
    !  value are successfully removed or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_RemoveMax', ModName, ErrWarning, &
                            'The binary search tree is empty.')
        RETURN
    END IF
        
    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%Color = Red
    END IF

    ! delete node with largest key
    Tree%Root => DeleteMaxKeyNode(Tree%Root, Key, Value)
        
    ! update color
    IF (.NOT.Tree%IsEmpty()) Tree%Root%Color = Black

    Flag = TrueVal

    RETURN

END FUNCTION BSTree_RemoveMax

!******************************************************************************

SUBROUTINE BSTree_Traverse_I(Tree, TraverseFlg, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To traverse the tree according to traversal flag.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree         !! tree
    tInteger,            INTENT(IN)     :: TraverseFlg
    !^ traversal flag. <br>
    ! = 1 -> inorder. <br>
    ! = 2 -> preorder. <br>
    ! = 3 -> postorder. <br>
    PROCEDURE(IteratorLocal)            :: IterFunc     !! user-supplied routine

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Done

! FLOW
        
    Done = FalseVal
    SELECT CASE (TraverseFlg)
    CASE (Inorder)
        CALL Traverse_Inorder(Tree%Root, IterFunc, Done)
    CASE (Preorder)
        CALL Traverse_Preorder(Tree%Root, IterFunc, Done)
    CASE (Postorder)
        CALL Traverse_Postorder(Tree%Root, IterFunc, Done)
    END SELECT

    RETURN
        
CONTAINS

    RECURSIVE SUBROUTINE Traverse_Inorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform inorder traversal.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                    :: IterFunc !! user-supplied routine
        tLogical,                    INTENT(INOUT)  :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! traverse the left subtree
        IF (.NOT.Done) CALL Traverse_Inorder(Root%Left, IterFunc, Done)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (.NOT.Done) ErrStat = IterFunc(Root, Done)
        
        ! traverse the right subtree
        IF ((.NOT.Done).AND.(.NOT.ErrStat)) CALL Traverse_Inorder(Root%Right, IterFunc, Done)
        
        RETURN

    END SUBROUTINE Traverse_Inorder

    !**************************************************************************

    RECURSIVE SUBROUTINE Traverse_Preorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform preorder traversal.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                    :: IterFunc !! user-supplied routine
        tLogical,                    INTENT(INOUT)  :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! call user-supplied routine to perform user-specified task for the current root
        ErrStat = IterFunc(Root, Done)
        
        IF ((.NOT.Done).AND.(.NOT.ErrStat)) THEN
        
            ! traverse the left subtree
            CALL Traverse_Preorder(Root%Left, IterFunc, Done)
        
            ! traverse the right subtree
            IF (.NOT.Done) CALL Traverse_Preorder(Root%Right, IterFunc, Done)

        END IF
        
        RETURN

    END SUBROUTINE Traverse_Preorder

    !**************************************************************************

    RECURSIVE SUBROUTINE Traverse_Postorder(Root, IterFunc, Done)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform postorder traversal.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(INOUT)  :: Root     !! root node of (sub)tree
        PROCEDURE(IteratorLocal)                    :: IterFunc !! user-supplied routine
        tLogical,                    INTENT(INOUT)  :: Done     !! true if want to stop the traversal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Root)) RETURN
        
        ! traverse the left subtree
        IF (.NOT.Done) CALL Traverse_Postorder(Root%Left, IterFunc, Done)
        
        ! traverse the right subtree
        IF (.NOT.Done) CALL Traverse_Postorder(Root%Right, IterFunc, Done)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (.NOT.Done) ErrStat = IterFunc(Root, Done)
            
        ! set flag to quit if there is an error
        IF (ErrStat) Done = TrueVal
        
        RETURN

    END SUBROUTINE Traverse_Postorder

    !**************************************************************************

END SUBROUTINE BSTree_Traverse_I

!******************************************************************************

SUBROUTINE BSTree_Traverse_II(Tree, IterFunc)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform inorder traversal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree     !! tree
    PROCEDURE(IteratorFunc)             :: IterFunc !! iterator function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    KeyTypeA                :: Key
    CLASS(*), ALLOCATABLE   :: Value
    tLogical                :: EndOfTree
    tLogical                :: ErrStat
    tLogical                :: Done

! FLOW
        
    ! check whether the tree is empty or not
    IF (Tree%IsEmpty()) RETURN
        
    ! set defaults
    Done    = FalseVal   ! traverse to all nodes
    ErrStat = FalseVal

    ! start iteration
    EndOfTree = Tree%StartFirst(Key, Value)
        
    ! loop over all nodes of the list
    DO WHILE (.NOT.EndOfTree)
            
        ! call iterator function
        ErrStat = IterFunc(Key, Value, Done)
            
        ! report error if necessary
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('BSTree_Traverse_II', ModName, ErrSevere, &
                                 'An error occurred during call to iterator function.')
            RETURN
        END IF
            
        ! exit the loop if the user want to stop the traversing
        IF (Done) EXIT

        ! move to next node in the inorder traversal
        EndOfTree = Tree%MoveForward(Key, Value)

    END DO

    RETURN

END SUBROUTINE BSTree_Traverse_II

!******************************************************************************

FUNCTION BSTree_Move2First(Tree, Key, Value) RESULT(EmptyTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restart the iteration at the node with smallest key (and optionally to retrieve
    !  the key and value of that node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree         !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key          !! the smallest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value        !! associated value
    tLogical                                        :: EmptyTree    !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
            
        ! set iteration node to the node with the smallest key
        CALL Find_MinKeyNode(Tree%Root, Tree%Cursor)
            
        ! set flag
        EmptyTree = FalseVal

        ! get key and value if requested
        IF (PRESENT(Key))     Key = Tree%Cursor%Key
        IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=Tree%Cursor%Value)
            
    ELSE
            
        ! set iteration node to null
        Tree%Cursor => NULL()
            
        ! set flag
        EmptyTree = TrueVal
            
    END IF

    RETURN
        
END FUNCTION BSTree_Move2First

!******************************************************************************

FUNCTION BSTree_Move2NextPair(Tree, Key, Value) RESULT(EndOfTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next node in inorder traversal (and optionally to retrieve
    !  the key and value of the next node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key      !! key of the next node
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! value of the next node
    tLogical                                        :: EndOfTree
    !^ true if the current iteration node is  at the end of tree (i.e. the next node does not exist).

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER   :: NxtNode => NULL()

! FLOW

    IF (ASSOCIATED(Tree%Cursor)) THEN
            
        ! find next node
        CALL Find_Inorder_NextNode(Tree%Cursor, Tree%Root, NxtNode)
            
        ! check status of NxtNode
        IF (ASSOCIATED(NxtNode)) THEN
                
            ! next node exists so set iteration node to NxtNode
            Tree%Cursor => NxtNode
                
            ! set flag
            EndOfTree = FalseVal

            ! get key and value if requested
            IF (PRESENT(Key))     Key = Tree%Cursor%Key
            IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=Tree%Cursor%Value)
                
        ELSE
                
            ! next node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            
            ! set flag
            EndOfTree = TrueVal
                
        END IF
            
    ELSE
            
        ! current iteration node is null so set flag to true
        EndOfTree = TrueVal
            
    END IF

    ! nullify working node
    NULLIFY(NxtNode)

    RETURN
        
END FUNCTION BSTree_Move2NextPair

!******************************************************************************

FUNCTION BSTree_Move2Last(Tree, Key, Value) RESULT(EmptyTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To restart the iteration at the node with largest key (and optionally to retrieve
    !  the key and value of that node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree         !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key          !! the largest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value        !! associated value
    tLogical                                        :: EmptyTree    !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
            
        ! set iteration node to the node with the largest key
        CALL Find_MaxKeyNode(Tree%Root, Tree%Cursor)
            
        ! set flag
        EmptyTree = FalseVal

        ! get key and value if requested
        IF (PRESENT(Key))     Key = Tree%Cursor%Key
        IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=Tree%Cursor%Value)
            
    ELSE
            
        ! set iteration node to null
        Tree%Cursor => NULL()
            
        ! set flag
        EmptyTree = TrueVal
            
    END IF

    RETURN
        
END FUNCTION BSTree_Move2Last

!******************************************************************************

FUNCTION BSTree_Move2PrevPair(Tree, Key, Value) RESULT(EndOfTree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the previous node in inorder traversal (and optionally to retrieve
    !  the key and value of the previous node if requested).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA,              OPTIONAL, INTENT(OUT)    :: Key      !! key of the previous node
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! value of the previous node
    tLogical                                        :: EndOfTree
    !^ true if the current iteration node is  at the end of tree (i.e. the previous node does not exist).

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER   :: PrvNode => NULL()

! FLOW

    IF (ASSOCIATED(Tree%Cursor)) THEN
            
        ! find previous node
        CALL Find_Inorder_PrevNode(Tree%Cursor, Tree%Root, PrvNode)
            
        ! check status of PrvNode
        IF (ASSOCIATED(PrvNode)) THEN
                
            ! previous node exists so set iteration node to PrvNode
            Tree%Cursor => PrvNode
                
            ! set flag
            EndOfTree = FalseVal

            ! get key and value if requested
            IF (PRESENT(Key))     Key     = Tree%Cursor%Key
            IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=Tree%Cursor%Value)

        ELSE
                
            ! previous node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            
            ! set flag
            EndOfTree = TrueVal
                
        END IF
            
    ELSE
            
        ! current iteration node is null so set flag to true
        EndOfTree = TrueVal
            
    END IF

    ! nullify working node
    NULLIFY(PrvNode)

    RETURN
        
END FUNCTION BSTree_Move2PrevPair

!******************************************************************************

FUNCTION BSTree_IsEmpty(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(IN) :: Tree     !! tree
    tLogical                        :: Flag     !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = (Tree%GetSize() == 0_kIndex)

    RETURN

END FUNCTION BSTree_IsEmpty

!******************************************************************************

FUNCTION BSTree_FindKey(Tree, Key, KeyNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To find the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),                   INTENT(INOUT)    :: Tree     !! tree
    KeyTypeB,                              INTENT(IN)       :: Key      !! the key to be looked for
    TYPE(RedBlackNode), OPTIONAL, POINTER, INTENT(OUT)      :: KeyNode  !! the node containing the specified key
    tLogical                                                :: Found    !! true if the key found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: OutNode => NULL()

! FLOW
        
    ! recursively search for the node that have the same key
    CALL FindNode(Tree%Root, Key, OutNode)
        
    ! set flag
    Found = ASSOCIATED(OutNode)
        
    ! set optional output
    IF (PRESENT(KeyNode)) KeyNode => OutNode
        
    ! nullify working node
    NULLIFY(OutNode)

    RETURN
        
END FUNCTION BSTree_FindKey

!******************************************************************************

FUNCTION BSTree_GetSize(Tree) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get size of the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(IN) :: Tree     !! tree
    tIndex                          :: Size     !! size of the tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Size = NodeSize(Tree%Root)
        
    RETURN
        
END FUNCTION BSTree_GetSize

!******************************************************************************

FUNCTION BSTree_GetValue(Tree, Key, Value) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To retrieve value associated with the specified key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),   INTENT(INOUT)    :: Tree     !! tree
    KeyTypeB,              INTENT(IN)       :: Key      !! key to be looked for
    CLASS(*), ALLOCATABLE, INTENT(OUT)      :: Value    !! value associated with the key
    tLogical                                :: Found
    !^ true if the key and its associated value found; false if the key is not in the tree.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: KeyNode => NULL()

! FLOW
        
    ! find Key
    Found = Tree%Contain(Key, KeyNode)
        
    ! get value
    IF (Found) ALLOCATE(Value, SOURCE=KeyNode%Value)

    ! nullify working node
    NULLIFY(KeyNode)

    RETURN
        
END FUNCTION BSTree_GetValue

!******************************************************************************

FUNCTION BSTree_GetSmallestKey(Tree, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest key in the tree and optionally its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA ,                       INTENT(OUT)    :: Key      !! smallest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: KeyNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_GetSmallestKey', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find node with smallest key
    CALL Find_MinKeyNode(Tree%Root, KeyNode)
        
    ! get key
    Key = KeyNode%Key
        
    ! get value
    IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=KeyNode%Value)

    ! nullify working node
    NULLIFY(KeyNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_GetSmallestKey

!******************************************************************************

FUNCTION BSTree_GetLargestKey(Tree, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the largest key in the tree and optionally its associated value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeA,                        INTENT(OUT)    :: Key      !! largest key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: KeyNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_GetLargestKey', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find node with largest key
    CALL Find_MaxKeyNode(Tree%Root, KeyNode)
        
    ! get key
    Key = KeyNode%Key
        
    ! get value
    IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=KeyNode%Value)

    ! nullify working node
    NULLIFY(KeyNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_GetLargestKey

!******************************************************************************

FUNCTION BSTree_Floor(Tree, InKey, OutKey, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the largest key in the tree less than or equal to the given key
    !  and optionally its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeB,                        INTENT(IN)     :: InKey    !! input key
    KeyTypeA,                        INTENT(OUT)    :: OutKey   !! output key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: FloorNode => NULL()

! FLOW
    
    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Floor', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find the floor node
    CALL Find_FloorNode(Tree%Root, InKey, FloorNode)
        
    ! check whether the floor node exists or not
    IF (.NOT.ASSOCIATED(FloorNode)) THEN
        CALL Handle_ErrLevel('BSTree_Floor', ModName, ErrWarning, &
                             'The specified key is too small.')
        RETURN
    END IF
        
    ! get key
    OutKey = FloorNode%Key
        
    ! get value
    IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=FloorNode%Value)

    ! nullify working node
    NULLIFY(FloorNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_Floor

!******************************************************************************

FUNCTION BSTree_Ceiling(Tree, InKey, OutKey, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the smallest key in the tree greater than or equal to the given key
    !  and optionally its associated value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    KeyTypeB,                        INTENT(IN)     :: InKey    !! input key
    KeyTypeA,                        INTENT(OUT)    :: OutKey   !! output key
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(RedBlackNode), POINTER :: CeilingNode => NULL()

! FLOW

    Flag = FalseVal

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        CALL Handle_ErrLevel('BSTree_Ceiling', ModName, ErrWarning, &
                             'The binary search tree is empty.')
        RETURN
    END IF
        
    ! find the floor node
    CALL Find_CeilingNode(Tree%Root, InKey, CeilingNode)
        
    ! check whether the floor node exists or not
    IF (.NOT.ASSOCIATED(CeilingNode)) THEN
        CALL Handle_ErrLevel('BSTree_Ceiling', ModName, ErrWarning, &
                             'The specified key is too large.')
        RETURN
    END IF
        
    ! get key
    OutKey = CeilingNode%Key
        
    ! get value
    IF (PRESENT(Value)) ALLOCATE(Value, SOURCE=CeilingNode%Value)

    ! nullify working node
    NULLIFY(CeilingNode)

    Flag = TrueVal

    RETURN
        
END FUNCTION BSTree_Ceiling

!******************************************************************************

FUNCTION BSTree_Select(Tree, Rank, Key, Value) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the key in the tree of a given rank.
    !  This key has the property that there are rank keys in
    !  the tree that are smaller. In other words, this key is the
    !  (rank+1)st smallest key in the tree. <br>
    !  Note: applicable range of rank is between 0 and tree_size-1.
    !       (this rank number is zero-based).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),             INTENT(INOUT)  :: Tree     !! tree
    tIndex,                          INTENT(IN)     :: Rank     !! rank
    KeyTypeA,                        INTENT(OUT)    :: Key      !! key of the given rank
    CLASS(*), ALLOCATABLE, OPTIONAL, INTENT(OUT)    :: Value    !! associated value
    !% flag indicating whether the key-value pair is found or not.
    tLogical                                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Found

! FLOW

    Flag = FalseVal

    ! check input validity
    IF (NOT_IN_RANGE(Rank, 0, Tree%GetSize()-1)) THEN
        CALL Handle_ErrLevel('BSTree_Select', ModName, ErrSevere, &
                             'The specified rank is not in the applicable range.')
        RETURN
    END IF
        
    ! find the floor node
    Found = SelectKey(Tree%Root, Rank, Key, Value)

    ! check if the key is found
    IF (.NOT.Found) THEN
        CALL Handle_ErrLevel('BSTree_Select', ModName, ErrSevere, &
                             'There is a bug in the code.')
    ELSE
        Flag = TrueVal
    END IF
        
    RETURN
        
END FUNCTION BSTree_Select

!******************************************************************************

FUNCTION BSTree_Rank(Tree, Key) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of keys in the tree strictly less than the given key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    KeyTypeB,            INTENT(IN)     :: Key  !! key
    tIndex                              :: Rank !! rank of key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Rank = KeyRank(Tree%Root, Key)
        
    RETURN
        
END FUNCTION BSTree_Rank

!******************************************************************************

SUBROUTINE BSTree_GetKeys_Range(Tree, Low, High, KeyQ, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys in the tree in the given range and optionally also
    !  return all associated values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),       INTENT(INOUT)    :: Tree     !! tree
    KeyTypeB,                  INTENT(IN)       :: Low      !! low key
    KeyTypeB,                  INTENT(IN)       :: High     !! high key
    TYPE(QueueKey),            INTENT(OUT)      :: KeyQ     !! key queue
    TYPE(QueueVal),  OPTIONAL, INTENT(OUT)      :: ValueQ   !! value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and get all keys in the range
    CALL GetKeys_Inorder(Tree%Root, Low, High, KeyQ, ValueQ)
        
    RETURN
        
CONTAINS

    FUNCTION IsKeyInRange(Key, LowLimit, UppLimit) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check if the key is in the specified range.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        KeyTypeB, INTENT(IN)    :: Key
        KeyTypeB, INTENT(IN)    :: LowLimit
        KeyTypeB, INTENT(IN)    :: UppLimit
        tLogical                :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW
    
        IF ((Key < LowLimit).OR.(Key > UppLimit)) THEN
            Flag = FalseVal
        ELSE
            Flag = TrueVal
        END IF

        RETURN

    END FUNCTION IsKeyInRange

    !**************************************************************************

    RECURSIVE SUBROUTINE GetKeys_Inorder(Node, Lo, Hi, QueueK, QueueC)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get inorder key.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER,  INTENT(INOUT) :: Node     !! root node of (sub)tree
        KeyTypeB,                     INTENT(IN)    :: Lo       !! low key
        KeyTypeB,                     INTENT(IN)    :: Hi       !! high key
        TYPE(QueueKey),               INTENT(INOUT) :: QueueK   !! key queue
        TYPE(QueueVal),     OPTIONAL, INTENT(INOUT) :: QueueC   !! value queue

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: ErrStat

    ! FLOW
        
        ! check whether the root is null or not
        IF (.NOT.ASSOCIATED(Node)) RETURN
        
        ! traverse the left subtree
        IF (Lo < Node%Key) CALL GetKeys_Inorder(Node%Left, Lo, Hi, QueueK, QueueC)
        
        ! call user-supplied routine to perform user-specified task for the current root
        IF (IsKeyInRange(Node%Key, Lo, Hi)) THEN
            CALL QueueK%EnQueue(Node%Key)
            IF (PRESENT(QueueC)) CALL QueueC%EnQueue(Node%Value)
        END IF
        
        ! traverse the right subtree
        IF (Hi > Node%Key) CALL GetKeys_Inorder(Node%Right, Lo, Hi, QueueK, QueueC)
        
        RETURN

    END SUBROUTINE GetKeys_Inorder

    !**************************************************************************

END SUBROUTINE BSTree_GetKeys_Range

!******************************************************************************

SUBROUTINE BSTree_GetKeys_All(Tree, KeyQ, ValueQ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return all keys in the tree and optionally all associated values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree),       INTENT(INOUT)    :: Tree     !! tree
    TYPE(QueueKey),            INTENT(OUT)      :: KeyQ     !! key queue
    TYPE(QueueVal),  OPTIONAL, INTENT(OUT)      :: ValueQ   !! value queue

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and get all keys in the range
    CALL Tree%Traverse(Inorder, GetKeys)
        
    RETURN
        
CONTAINS

    FUNCTION GetKeys(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To get key of the specified node

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), INTENT(IN)      :: Node     !! node
        tLogical,           INTENT(INOUT)   :: Done     !! on input, Done is set to FalseVal
                                                        !! on exit, set it to TrueVal if user
                                                        !!   want to stop the queue traversing.
        tLogical                            :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key to queues
        CALL KeyQ%EnQueue(Node%Key)
        IF (PRESENT(ValueQ)) CALL ValueQ%EnQueue(Node%Value)

        RETURN
            
    END FUNCTION GetKeys
        
    !**************************************************************************

END SUBROUTINE BSTree_GetKeys_All

!******************************************************************************

FUNCTION BSTree_GetSize_Range(Tree, Low, High) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of keys in the tree in the given range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree     !! RedBlackTree object
    KeyTypeB,            INTENT(IN)     :: Low      !! low key
    KeyTypeB,            INTENT(IN)     :: High     !! high key
    tIndex                              :: Size     !! size of the tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (Low > High) THEN
        Size = 0
        RETURN
    END IF
        
    IF (Tree%Contain(High)) THEN
        Size = Tree%GetRank(High) - Tree%GetRank(Low) + 1
    ELSE
        Size = Tree%GetRank(High) - Tree%GetRank(Low)
    END IF
        
    RETURN

END FUNCTION BSTree_GetSize_Range

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               Routines for Checking Integrity of BST Data Structure
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION BSTree_IsRankConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check that ranks are consistent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! true if ranks are consistent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tLogical    :: Found
    KeyTypeA    :: Key

! FLOW
        
    ! check rank of select of I
    DO I = 0, Tree%GetSize()-1
        Found = Tree%Select(I, Key)
        IF ((.NOT.Found).OR.(I /= Tree%GetRank(Key))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END DO
        
    Flag = TrueVal
        
    ! check select of rank of key
    CALL Tree%Traverse(Inorder, CompareKey)
        
    RETURN
        
CONTAINS

    FUNCTION CompareKey(Node, Done) RESULT(ErrStat)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To compare key of the specified node.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), INTENT(IN)      :: Node     !! node
        tLogical,           INTENT(INOUT)   :: Done     !! on input, Done is set to FalseVal
                                                        !! on exit, set it to TrueVal if user
                                                        !!   want to stop the queue traversing.
        tLogical                            :: ErrStat  !! true if error occurred in iterator
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Found
        KeyTypeA    :: NodeKey

    !** FLOW

        ! set flag
        ErrStat = FalseVal
        Done = FalseVal
            
        ! add key to queues
        Found = Tree%Select(Tree%GetRank(Node%Key), NodeKey)
        IF ((.NOT.Found).OR.(Node%Key /= NodeKey)) THEN
            Done = TrueVal
            Flag = FalseVal
        END IF

        RETURN
            
    END FUNCTION CompareKey
        
    !**************************************************************************

END FUNCTION BSTree_IsRankConsistent

!******************************************************************************

FUNCTION BSTree_IsSizeConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the size fields are correct or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! true if the size fields are correct

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = IsSizeConsistent(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION IsSizeConsistent(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the size field is correct or not.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(IN) :: Node     !! node
        tLogical                                :: Flag     !! true if the size fields are correct
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = TrueVal
        ELSEIF (Node%Size /= (NodeSize(Node%Left)+NodeSize(Node%Right)+1)) THEN
            Flag = FalseVal
        ELSE
            Flag = (IsSizeConsistent(Node%Left).AND.IsSizeConsistent(Node%Right))
        END IF

        RETURN
            
    END FUNCTION IsSizeConsistent
        
!**************************************************************************

END FUNCTION BSTree_IsSizeConsistent

!******************************************************************************

FUNCTION BSTree_IsBinarySearchTree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the inorder keys in the tree are in ascending order or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! true if the inorder keys are sorted

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(QueueKey)    :: InorderKeyQ

! FLOW
        
    ! get inorder keys
    CALL Tree%GetKeys(InorderKeyQ)
        
    ! verify that the inorder keys are sorted
    Flag = Are_Keys_In_Ascending_Order(InorderKeyQ)
        
    RETURN
        
CONTAINS

    FUNCTION Are_Keys_In_Ascending_Order(KeyQ) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE
        !^ To check whether the given keys are sorted in ascending order.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(QueueKey), INTENT(INOUT)   :: KeyQ !! key queue
        tLogical                        :: Flag !! true if the queue is sorted
 
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        KeyTypeA    :: KeyI, KeyIP1
        tLogical    :: Success
 
    !** FLOW:

        ! initialize
        Flag = TrueVal
    
        ! check if the queue is empty or not
        IF (KeyQ%IsEmpty()) RETURN
    
        ! get KeyI
        Success = KeyQ%DeQueue(KeyI)

        DO WHILE (.NOT.KeyQ%IsEmpty())
            ! get KeyIP1
            Success = KeyQ%DeQueue(KeyIP1)
            ! compare keys
            IF (KeyIP1 <= KeyI) THEN
                Flag = FalseVal
                CALL KeyQ%Destruct()
                EXIT
            END IF
            ! reset KeyI
            KeyI = KeyIP1
        END DO
    
        RETURN
    
    END FUNCTION Are_Keys_In_Ascending_Order

    !**************************************************************************

END FUNCTION BSTree_IsBinarySearchTree

!******************************************************************************

FUNCTION BSTree_Is23Tree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is a 2-3 tree. <br>
    !  => Does the tree have no red right links, and at most one (left)
    !     red links in a row on any path?

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! true if this is a 2-3 tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = Is23Tree(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION Is23Tree(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the subtree is a 2-3 tree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(IN) :: Node     !! node
        tLogical                                :: Flag     !! true if this is a 2-3 tree
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = TrueVal
        ELSEIF (IsRed(Node%Right)) THEN
            Flag = FalseVal
        ELSEIF (.NOT.ASSOCIATED(Node,Tree%Root).AND.IsRed(Node).AND.IsRed(Node%Left)) THEN
            Flag = FalseVal
        ELSE
            Flag = (Is23Tree(Node%Left).AND.Is23Tree(Node%Right))
        END IF

        RETURN
            
    END FUNCTION Is23Tree
        
    !**************************************************************************

END FUNCTION BSTree_Is23Tree

!******************************************************************************

FUNCTION BSTree_IsBalanced(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is balanced. <br>
    !  => Do all paths from root to leaf have
    !     same number of black edges?.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! true if the tree is balanced

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                      :: BlackCount   ! number of black links on path from root to min
    TYPE(RedBlackNode), POINTER :: XNode => NULL()

! FLOW
        
    BlackCount = 0
    XNode => Tree%Root
        
    DO WHILE (ASSOCIATED(XNode))
        IF (.NOT.IsRed(XNode)) BlackCount = BlackCount + 1
        XNode => XNode%Left
    END DO
    NULLIFY(XNode)
        
    Flag = IsBalanced(Tree%Root, BlackCount)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION IsBalanced(Node, BlackCount) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        !^ To check whether the subtree is balanced.
        !  => Does every path from the root to a leaf
        !     have the given number of black links?

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(RedBlackNode), POINTER, INTENT(IN) :: Node         !! node
        tIndex,                      INTENT(IN) :: BlackCount   !! number of black links on path from root to min
        tLogical                                :: Flag         !! true if the tree is balanced
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex          :: CurrBlackCount

    !** FLOW

        CurrBlackCount = BlackCount
        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = (CurrBlackCount == 0)
        ELSE
            IF (.NOT.IsRed(Node)) CurrBlackCount = CurrBlackCount - 1
            Flag = (IsBalanced(Node%Left, CurrBlackCount).AND. &
                    IsBalanced(Node%Right, CurrBlackCount))
        END IF

        RETURN
            
    END FUNCTION IsBalanced
        
    !**************************************************************************

END FUNCTION BSTree_IsBalanced

!******************************************************************************

FUNCTION BSTree_CheckBST(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of BST data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RedBlackTree), INTENT(INOUT)  :: Tree !! tree
    tLogical                            :: Flag !! flag for integrity

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: IsBSTree
    tLogical    :: Is23Tree
    tLogical    :: IsBalanced
    tLogical    :: IsRankConsistent
    tLogical    :: IsSizeConsistent

! FLOW
        
    IsBSTree = Tree%IsBSTree()
    Is23Tree = Tree%Is23Tree()
    IsBalanced = Tree%IsBalanced()
    IsRankConsistent = Tree%IsRankConsistent()
    IsSizeConsistent = Tree%IsSizeConsistent()
    IF (.NOT.IsBSTree) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not in symmetric order.')
    END IF
    IF (.NOT.Is23Tree) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not a 2-3 tree.')
    END IF
    IF (.NOT.IsBalanced) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Not balanced tree.')
    END IF
    IF (.NOT.IsRankConsistent) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Ranks not consistent.')
    END IF
    IF (.NOT.IsSizeConsistent) THEN
        CALL Handle_ErrLevel('BSTree_CheckBST', ModName, ErrWarning, &
                    'Subtree counts not consistent.')
    END IF
    Flag = (IsBSTree.AND.Is23Tree.AND.IsRankConsistent.AND.IsSizeConsistent)
        
    RETURN
        
END FUNCTION BSTree_CheckBST

!******************************************************************************

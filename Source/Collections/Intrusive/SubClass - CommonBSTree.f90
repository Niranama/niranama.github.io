
SUBMODULE (Class_IntrusiveBSTrees) SubClass_CommonBSTree

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains the actual implementation of the *IntrusiveAVLTree* type,
!   which is an *intrusive binary-search-tree* container that stores objects (or nodes)
!   in the *BSTNode* class (i.e. objects/nodes that are subtypes of the
!   *BSTNode* type).

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE BSTree_RemoveAll(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove all nodes from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! traverse the tree and destroy all nodes
    CALL RemoveNode(Tree%Root)

    ! nullify pointer components
    NULLIFY(Tree%Root)
    NULLIFY(Tree%Cursor)
        
    RETURN
        
    CONTAINS

    RECURSIVE SUBROUTINE RemoveNode(Node)
            
    !** PURPOSE OF THIS SUBROUTINE:
        ! To destroy the specified node

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(INOUT)  :: Node ! node
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! return if null
        IF (.NOT.ASSOCIATED(Node)) RETURN
            
        ! recursively remove left and right subtrees
        CALL RemoveNode(Node%Left)
        CALL RemoveNode(Node%Right)
            
        ! remove this node
        CALL Node%Reset()
            
        RETURN
            
    END SUBROUTINE RemoveNode
        
    !**************************************************************************

END SUBROUTINE BSTree_RemoveAll

!******************************************************************************

MODULE FUNCTION BSTree_StartMin(Tree, NodeOut) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To start an iteration by setting the cursor pointer to the minimum node
    !  of the tree and return a flag indicating whether the tree is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% pointer to the starting node
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
    !% true if the tree is empty
    tLogical                                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
        ! set iteration node to the node with the smallest key
        CALL Find_MinNode(Tree%Root, Tree%Cursor)
        ! set flag
        IsEmpty = FalseVal
    ELSE
        ! set iteration node to null
        Tree%Cursor => NULL()
        ! set flag
        IsEmpty = TrueVal
    END IF
    
    ! get the minimum node if requested
    IF (PRESENT(NodeOut)) NodeOut => Tree%Cursor

    RETURN

END FUNCTION BSTree_StartMin

!******************************************************************************

MODULE FUNCTION BSTree_StartMax(Tree, NodeOut) RESULT(IsEmpty)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To start an iteration by setting the cursor pointer to the maximum node
    !  of the tree and return a flag indicating whether the tree is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% pointer to the starting node
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
    !% true if the tree is empty
    tLogical                                            :: IsEmpty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ASSOCIATED(Tree%Root)) THEN
        ! set iteration node to the node with the largest key
        CALL Find_MaxNode(Tree%Root, Tree%Cursor)
        ! set flag
        IsEmpty = FalseVal
    ELSE
        ! set iteration node to null
        Tree%Cursor => NULL()
        ! set flag
        IsEmpty = TrueVal
    END IF
    
    ! get the maximum node if requested
    IF (PRESENT(NodeOut)) NodeOut => Tree%Cursor

    RETURN

END FUNCTION BSTree_StartMax

!******************************************************************************

MODULE FUNCTION BSTree_Move2Next(Tree, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the next (successor) node and return a flag indicating
    !  whether the cursor has reached the end of the tree or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% pointer to the next node
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
    !% true if the cursor pointer has reached the end of the tree
    tLogical                                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: NxtNode

! FLOW

    ! set null pointer
    NxtNode => NULL()
    
    IF (ASSOCIATED(Tree%Cursor)) THEN
        ! find next node
        CALL Find_Inorder_NextNode(Tree%Cursor, Tree%Root, NxtNode)
        ! check status of NxtNode
        IF (ASSOCIATED(NxtNode)) THEN
            ! next node exists so set iteration node to NxtNode
            Tree%Cursor => NxtNode
            ! set flag
            IsTheEnd = FalseVal
        ELSE
            ! next node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            ! set flag
            IsTheEnd = TrueVal
        END IF
    ELSE
        ! current iteration node is null so set flag to true
        IsTheEnd = TrueVal
    END IF

    ! get the output node if requested
    IF (PRESENT(NodeOut)) NodeOut => NxtNode

    ! nullify working node
    NULLIFY(NxtNode)
        
    RETURN

END FUNCTION BSTree_Move2Next

!******************************************************************************

MODULE FUNCTION BSTree_Move2Prev(Tree, NodeOut) RESULT(IsTheEnd)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To move to the previous (predecessor) node and return a flag indicating
    !  whether the cursor has reached the end of the tree or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% pointer to the next node
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: NodeOut
    !% true if the cursor pointer has reached the end of the tree
    tLogical                                            :: IsTheEnd

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: PrvNode

! FLOW

    ! set null pointer
    PrvNode => NULL()
    
    IF (ASSOCIATED(Tree%Cursor)) THEN
        ! find previous node
        CALL Find_Inorder_PrevNode(Tree%Cursor, Tree%Root, PrvNode)
        ! check status of PrvNode
        IF (ASSOCIATED(PrvNode)) THEN
            ! previous node exists so set iteration node to PrvNode
            Tree%Cursor => PrvNode
            ! set flag
            IsTheEnd = FalseVal
        ELSE
            ! previous node does not exist so set iteration node to null
            Tree%Cursor => NULL()
            ! set flag
            IsTheEnd = TrueVal
        END IF
    ELSE
        ! current iteration node is null so set flag to true
        IsTheEnd = TrueVal
    END IF

    ! get the output node if requested
    IF (PRESENT(NodeOut)) NodeOut => PrvNode

    ! nullify working node
    NULLIFY(PrvNode)

    RETURN

END FUNCTION BSTree_Move2Prev

!******************************************************************************

MODULE FUNCTION BSTree_IsEmpty(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the tree is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN)  :: Tree !! tree
    tLogical                            :: Flag !! true if the tree is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = (Tree%GetSize() == 0)
    
    RETURN

END FUNCTION BSTree_IsEmpty

!******************************************************************************

MODULE FUNCTION BSTree_Contain(Tree, InNode, OutNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether there is a node stored in the tree that is equal
    !  to the specified node or not.  Optionally, return a pointer to the
    !  stored node if found (or return null pointer if not found).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% node to be looked for
    CLASS(BSTNode),                    INTENT(IN)       :: InNode
    !% a pointer to a stored node equal to the specified one
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
    !% true if the stored node found
    tLogical                                            :: Found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: StoredNode

! FLOW

    ! recursively search for the node that have the same key
    CALL FindNode(Tree%Root, InNode, StoredNode)
        
    ! set flag
    Found = ASSOCIATED(StoredNode)
    
    ! set pointer
    IF (PRESENT(OutNode)) OutNode => StoredNode
        
    ! nullify working node
    NULLIFY(StoredNode)

    RETURN

END FUNCTION BSTree_Contain

!******************************************************************************

MODULE FUNCTION BSTree_GetSize(Tree) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the tree size (i.e. the number of nodes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree !! IntrusiveAVLTree object
    tIndex                              :: Size !! size of the tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = NodeSize(Tree%Root)
    
    RETURN
    
END FUNCTION BSTree_GetSize

!******************************************************************************

MODULE FUNCTION BSTree_GetSmallestNode(Tree) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to the node with the smallest value (key).
    !  If the tree is empty, return null pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! tree
    CLASS(BSTNode),          POINTER    :: OutNode  !! pointer to smallest node (or null)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        OutNode => NULL()
    ELSE
        ! find node with smallest key
        CALL Find_MinNode(Tree%Root, OutNode)
    END IF
        
    RETURN

END FUNCTION BSTree_GetSmallestNode

!******************************************************************************

MODULE FUNCTION BSTree_GetLargestNode(Tree) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get a pointer to the node with the largest value (key).
    !  If the tree is empty, return null pointer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! tree
    CLASS(BSTNode),          POINTER    :: OutNode  !! pointer to largest node (or null)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        OutNode => NULL()
    ELSE
        ! find node with largest key
        CALL Find_MaxNode(Tree%Root, OutNode)
    END IF
        
    RETURN

END FUNCTION BSTree_GetLargestNode

!******************************************************************************

MODULE FUNCTION BSTree_GetCursor(Tree) RESULT(Cursor)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the cursor node of the tree

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! IntrusiveAVLTree object
    CLASS(BSTNode),          POINTER    :: Cursor   !! pointer to the cursor node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Cursor => Tree%Cursor

    RETURN

END FUNCTION BSTree_GetCursor

!******************************************************************************

MODULE FUNCTION BSTree_GetRoot(Tree) RESULT(Root)

!** PURPOSE OF THIS SUBROUTINE:
    !! To get a pointer to the root node of the tree

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(IN) :: Tree     !! IntrusiveAVLTree object
    CLASS(BSTNode),          POINTER    :: Root   !! pointer to the root node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Root => Tree%Root

    RETURN

END FUNCTION BSTree_GetRoot

!******************************************************************************

MODULE FUNCTION BSTree_Floor(Tree, InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the largest node in the tree less than or
    !  equal to the given node.  Return null pointer if the tree is empty
    !  or such node does not exist.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
    CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
    CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to floor node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        OutNode => NULL()
    ELSE
        ! find the floor node
        CALL Find_FloorNode(Tree%Root, InNode, OutNode)
    END IF
        
    RETURN

END FUNCTION BSTree_Floor

!******************************************************************************

MODULE FUNCTION BSTree_Ceiling(Tree, InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the smallest node in the tree greater than or
    !  equal to the given node.  Return null pointer if the tree is empty or
    !  such node does not exist.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
    CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
    CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to ceiling node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        OutNode => NULL()
    ELSE
        ! find the ceiling node
        CALL Find_CeilingNode(Tree%Root, InNode, OutNode)
    END IF
        
    RETURN

END FUNCTION BSTree_Ceiling

!******************************************************************************

MODULE FUNCTION BSTree_Select(Tree, Rank) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a pointer to the node in the tree of a given rank.  This node
    !  has the property that there are rank nodes in the tree that are smaller.
    !  In other words, this node is the (rank+1)st smallest node in the tree. <br>
    !  Applicable range of rank is between 0 and tree_size-1 where this rank number
    !  is zero-based. <br>
    !  Return null pointer if the tree is empty, such node does not exist or the 
    !  input is invalid.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
    tIndex,                  INTENT(IN)     :: Rank     !! rank
    CLASS(BSTNode),          POINTER        :: OutNode  !! pointer to output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check input validity
    IF ((Rank < 0).OR.(Rank >= Tree%GetSize())) THEN
        OutNode => NULL()
    ELSE
        IF (.NOT.SelectNode(Tree%Root, Rank, OutNode)) THEN
            OutNode => NULL()
        END IF
    END IF

    RETURN

END FUNCTION BSTree_Select

!******************************************************************************

MODULE FUNCTION BSTree_Rank(Tree, InNode) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the number of nodes in the tree strictly less than the given node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
    CLASS(BSTNode),          INTENT(IN)     :: InNode   !! input node
    tIndex                                  :: Rank     !! rank of key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Rank = NodeRank(Tree%Root, InNode)

    RETURN

END FUNCTION BSTree_Rank

!******************************************************************************

MODULE FUNCTION BSTree_GetRangeSize(Tree, Low, High) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of nodes in the tree in the given range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree     !! tree
    CLASS(BSTNode),          INTENT(IN)     :: Low      !! low node
    CLASS(BSTNode),          INTENT(IN)     :: High     !! high node
    tIndex                                  :: Size     !! range size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Low > High) THEN
        Size = 0
        RETURN
    END IF

    IF (Tree%Contain(High)) THEN
        Size = Tree%Rank(High) - Tree%Rank(Low) + 1
    ELSE
        Size = Tree%Rank(High) - Tree%Rank(Low)
    END IF

    RETURN

END FUNCTION BSTree_GetRangeSize

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Find_MaxNode(Node, MaxNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To find the rightmost leaf of the given node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Node     ! input node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: MaxNode  ! output node

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

END SUBROUTINE Find_MaxNode

!******************************************************************************

RECURSIVE SUBROUTINE FindNode(InNode, ThisNode, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To search for the node that is equal to the specified one (ThisNode).
    ! Return null if the node does not exist.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: InNode   ! input node
    CLASS(BSTNode),          INTENT(IN)     :: ThisNode ! the node to be looked for
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (ThisNode == InNode) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (ThisNode < InNode) THEN
        ! the node should be on the left subtree
        CALL FindNode(InNode%Left, ThisNode, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL FindNode(InNode%Right, ThisNode, OutNode)
    END IF

    RETURN

END SUBROUTINE FindNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_FloorNode(InNode, ThisNode, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To find the node with largest value (i.e. key) in the tree
    ! less than or equal to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: InNode   ! input node
    CLASS(BSTNode),          INTENT(IN)     :: ThisNode ! the specified node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (ThisNode == InNode) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (ThisNode < InNode) THEN
        ! the node should be on the left subtree
        CALL Find_FloorNode(InNode%Left, ThisNode, OutNode)
    ELSE
        ! the node should be on the right subtree
        CALL Find_FloorNode(InNode%Right, ThisNode, OutNode)
        IF (.NOT.ASSOCIATED(OutNode)) THEN
            OutNode => InNode
        END IF
    END IF

    RETURN

END SUBROUTINE Find_FloorNode

!******************************************************************************

RECURSIVE SUBROUTINE Find_CeilingNode(InNode, ThisNode, OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To find the node containing smallest value (i.e. key) in the tree
    ! greater than or equal to the specified node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: InNode   ! input node
    CLASS(BSTNode),          INTENT(IN)     :: ThisNode ! the specified node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        ! the node we are looking for does not exist
        OutNode => NULL()
    ELSEIF (ThisNode == InNode) THEN
        ! the node found
        OutNode => InNode
    ELSEIF (ThisNode < InNode) THEN
        ! the node should be on the left subtree
        CALL Find_CeilingNode(InNode%Left, ThisNode, OutNode)
        IF (.NOT.ASSOCIATED(OutNode)) THEN
            OutNode => InNode
        END IF
    ELSE
        ! the node should be on the right subtree
        CALL Find_CeilingNode(InNode%Right, ThisNode, OutNode)
    END IF

    RETURN

END SUBROUTINE Find_CeilingNode

!******************************************************************************

RECURSIVE FUNCTION SelectNode(InNode, Rank, OutNode) RESULT(Found)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the node in the subtree rooted at InNode of given rank.
    ! Precondition: rank is in legal range.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: InNode   ! input node
    tIndex,                  INTENT(IN)     :: Rank     ! rank of the node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: OutNode  ! node of the given rank
    tLogical                                :: Found    ! true if the node found

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
        Found = SelectNode(InNode%Left, Rank, OutNode)
    ELSEIF (LeftSize < Rank) THEN
        ! the node should be on the right subtree
        Found = SelectNode(InNode%Right, Rank-LeftSize-1, OutNode)
    ELSE
        ! the node is found
        Found = TrueVal
        OutNode => InNode
    END IF

    RETURN

END FUNCTION SelectNode

!******************************************************************************

RECURSIVE FUNCTION NodeRank(InNode, ThisNode) RESULT(Rank)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the rank of the given node (i.e. the number
    ! of nodes in the tree that are less than the given node).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: InNode   ! input node
    CLASS(BSTNode),          INTENT(IN) :: ThisNode ! the given node
    tIndex                              :: Rank     ! rank of the node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (.NOT.ASSOCIATED(InNode)) THEN
        Rank = 0
        RETURN
    END IF
        
    IF (ThisNode < InNode) THEN
        Rank = NodeRank(InNode%Left, ThisNode)
    ELSEIF (ThisNode > InNode) THEN
        Rank = NodeRank(InNode%Right, ThisNode) + NodeSize(InNode%Left) + 1
    ELSE
        Rank = NodeSize(InNode%Left)
    END IF

    RETURN

END FUNCTION NodeRank

!******************************************************************************

SUBROUTINE Find_Inorder_PrevNode(Node, Root, PrvNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To search for the previous node in inorder traversal of the tree
    !   of the specified input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Node     ! input node
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Root     ! root node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: PrvNode  ! previous node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If left subtree of node is not NULL, then PrvNode lies in left subtree.
    ! Thus, go to left subtree and return the node with maximum key value
    ! in the left subtree.
    IF (ASSOCIATED(Node%Left)) THEN
        CALL Find_MaxNode(Node%Left, PrvNode)
        RETURN
    END IF
        
    ! start from root and iteratively search for the previous node down the tree
    PrvNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node < CurRoot) THEN
            ! the previous node should be on the left subtree
            CurRoot => CurRoot%Left
        ELSEIF (Node > CurRoot) THEN
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
    ! To search for the next node in inorder traversal of the tree
    !   of the specified input node.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Node     ! input node
    CLASS(BSTNode), POINTER, INTENT(IN)     :: Root     ! root node
    CLASS(BSTNode), POINTER, INTENT(OUT)    :: NxtNode  ! next node of input node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: CurRoot => NULL()

! FLOW
        
    ! If right subtree of node is not NULL, then NxtNode lies in right subtree.
    ! Thus, go to right subtree and return the node with minimum key value
    ! in the right subtree.
    IF (ASSOCIATED(Node%Right)) THEN
        CALL Find_MinNode(Node%Right, NxtNode)
        RETURN
    END IF
        
    ! start from root and search for the next node down the tree
    NxtNode => NULL()
    CurRoot => Root
    DO WHILE (ASSOCIATED(CurRoot))
        IF (Node < CurRoot) THEN
            ! the next node should be on the left subtree
            NxtNode => CurRoot
            CurRoot => CurRoot%Left
        ELSEIF (Node > CurRoot) THEN
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

END SUBMODULE SubClass_CommonBSTree

!******************************************************************************


SUBMODULE (Class_IntrusiveBSTrees) SubClass_IntrusiveAVLTree

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains the actual implementation of the *IntrusiveAVLTree* type,
!   which is an *intrusive AVL-tree* container that stores objects (or nodes)
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

! ---------------------------------------------------------------------
! -----                 Adding/Removing Procedures                -----
! ---------------------------------------------------------------------

MODULE SUBROUTINE AVLTree_Insert(Tree, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert a new node into the tree.  If the tree already contains a node
    !  that is equal to the new node, replace that node with the new one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
    !% a new node to be added to the tree
    CLASS(BSTNode), TARGET,  INTENT(IN)     :: NewNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! recursively search for place to insert the new node
    Tree%Root => AddNew(Tree%Root, NewNode)

    RETURN

    CONTAINS

    RECURSIVE FUNCTION AddNew(InNode, NewNode) RESULT(OutNode)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To add new node into the tree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        !% input node (root node of (sub)tree)
        CLASS(BSTNode), POINTER, INTENT(IN)   :: InNode
        !% new node to be added
        CLASS(BSTNode), TARGET,  INTENT(IN)   :: NewNode
        !% output node (new root node of (sub)tree)
        CLASS(BSTNode), POINTER               :: OutNode

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLogical, PARAMETER     :: IsRBTree = FalseVal
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        OutNode => InNode
        
        IF (.NOT.ASSOCIATED(OutNode)) THEN
            ! set new node
            OutNode => NewNode
            CALL OutNode%SetNew(IsRBTree)
        ELSE
            ! compare the new node with the input node
            IF (NewNode < OutNode) THEN
                ! add the new node to the left subtree
                OutNode%Left => AddNew(OutNode%Left, NewNode)
            ELSEIF (NewNode > OutNode) THEN
                ! add the new node to the right subtree
                OutNode%Right => AddNew(OutNode%Right, NewNode)
            ELSE
                ! replace input node with new node
                OutNode => NewNode
                CALL OutNode%CopyMembers(InNode)
                RETURN
            END IF
            ! update number of nodes in subtree rooted by the output node
            OutNode%Size = NodeSize(OutNode%Left) + NodeSize(OutNode%Right) + 1
            ! rebalance the subtree of the new root node if necessary; note that 
            ! subtrees of all ancestors of the new root node will also be rebalanced
            ! since the recursive call will travel back up and visit all the ancestors
            ! of the node.
            OutNode => Rebalance(OutNode)
        END IF

        RETURN

    END FUNCTION AddNew

    !**************************************************************************

END SUBROUTINE AVLTree_Insert

!******************************************************************************

MODULE FUNCTION AVLTree_Remove(Tree, DelNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified node from the tree.  Return the flag
    !  indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
    !% the node to be removed
    CLASS(BSTNode),          INTENT(IN)     :: DelNode
    !% true if the specified node is removed successfully
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set flag
    Flag = FalseVal
    
    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) RETURN
        
    ! delete the specified node
    Tree%Root => DeleteNode(Tree%Root, DelNode)

    RETURN

    CONTAINS

    RECURSIVE FUNCTION DeleteNode(InNode, DelNode) RESULT(OutNode)
    
    !** PURPOSE OF THIS SUBROUTINE:
        ! To remove the specified node from the tree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(INOUT)  :: InNode   ! input node (root node of (sub)tree)
        CLASS(BSTNode),          INTENT(IN)     :: DelNode  ! node to be removed
        CLASS(BSTNode), POINTER                 :: OutNode  ! output node (new root node of (sub)tree)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
        
        DO
            ! exit loop if the (sub)tree is empty
            IF (.NOT.ASSOCIATED(InNode)) EXIT
            ! find the node to be deleted
            IF (DelNode < InNode) THEN
                ! find the node on the left subtree
                InNode%Left => DeleteNode(InNode%Left, DelNode)
            ELSEIF (DelNode > InNode) THEN
                ! find the node on the right subtree
                InNode%Right => DeleteNode(InNode%Right, DelNode)
            ELSE
                ! the node found
                Flag = TrueVal
                BLOCK
                    ! block variable
                    CLASS(BSTNode), POINTER :: TmpNode
                    ! check deletion cases
                    IF (.NOT.ASSOCIATED(InNode%Left)) THEN
                        ! the node is with only one child or no child so set
                        ! output node (if no child, it would point to null)
                        TmpNode => InNode%Right
                        ! delete node from the tree
                        CALL InNode%Reset()
                        ! reset the input node and exit loop
                        InNode => TmpNode
                        ! free block variable
                        NULLIFY(TmpNode)
                        EXIT
                    ELSEIF (.NOT.ASSOCIATED(InNode%Right)) THEN
                        !  the node is with only one child so set output node
                        TmpNode => InNode%Left
                        ! delete node from the tree
                        CALL InNode%Reset()
                        ! reset the input node and exit loop
                        InNode => TmpNode
                        ! free block variable
                        NULLIFY(TmpNode)
                        EXIT
                    END IF
                    ! the node has two children so find inorder successor of InNode
                    ! and replace the node with the inorder successor node
                    TmpNode => InNode
                    CALL Find_MinNode(TmpNode%Right, InNode)
                    CALL InNode%CopyMembers(TmpNode)
                    ! then delete the inorder successor node
                    InNode%Right => DeleteMinNode(TmpNode%Right)
                    ! free block variable
                    NULLIFY(TmpNode)
                END BLOCK
            END IF
            ! exit loop if the (sub)tree is empty
            IF (.NOT.ASSOCIATED(InNode)) EXIT
            ! update number of nodes in subtree rooted by the (current) input node
            InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
            ! rebalance the node if necessary; note that all ancestors of the node
            ! will also be rebalanced since the recursive call will travel back up
            ! and visit all the ancestors of the deleted node.
            InNode => Rebalance(InNode)
            ! exit loop
            EXIT
        END DO
        ! set output node
        OutNode => InNode
        
        RETURN

    END FUNCTION DeleteNode

    !**************************************************************************

END FUNCTION AVLTree_Remove

!******************************************************************************

MODULE FUNCTION AVLTree_RemoveMin(Tree, OutNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the node with the smallest value from the tree.  Return
    !  the flag indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% the smallest node to be removed
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
    !% true if the specified node is removed successfully
    tLogical                                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        ! set flag
        Flag = FalseVal
        IF (PRESENT(OutNode)) OutNode => NULL()
    ELSE
        ! set flag
        Flag = TrueVal
        ! delete the node with smallest value
        Tree%Root => DeleteMinNode(Tree%Root, OutNode)
    END IF

    RETURN

END FUNCTION AVLTree_RemoveMin

!******************************************************************************

MODULE FUNCTION AVLTree_RemoveMax(Tree, OutNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the node with the largest value from the tree.  Return
    !  the flag indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree),           INTENT(INOUT)    :: Tree
    !% the largest node to be removed
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: OutNode
    !% true if the specified node is removed successfully
    tLogical                                            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) THEN
        ! set flag
        Flag = FalseVal
        IF (PRESENT(OutNode)) OutNode => NULL()
    ELSE
        ! set flag
        Flag = TrueVal
        ! delete the node with largest value
        Tree%Root => DeleteMaxNode(Tree%Root, OutNode)
    END IF

    RETURN

END FUNCTION AVLTree_RemoveMax

!******************************************************************************

MODULE SUBROUTINE AVLTree_Finalizer(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove all nodes from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    TYPE(IntrusiveAVLTree), INTENT(INOUT)   :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Tree%Clear()
    
    RETURN

END SUBROUTINE AVLTree_Finalizer

!******************************************************************************

MODULE FUNCTION AVLTree_CheckIntegrity(Tree, Message) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of the BST data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveAVLTree object
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree
    !> message indicating the reason why the tree did not pass the
    !  integrity test
    tCharAlloc, OPTIONAL,    INTENT(OUT)    :: Message
    !> flag for integrity <br>
    ! - true if the tree passed the integrity test.
    ! - false if the tree did not.
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = FalseVal
    IF (.NOT.BSTree_IsAVLTree(Tree)) THEN
        IF (PRESENT(Message)) Message = 'AVL property is not consistent.'
        RETURN
    END IF
    IF (.NOT.BSTree_IsBinarySearchTree(Tree)) THEN
        IF (PRESENT(Message)) Message = 'The tree is not in symmetric order.'
        RETURN
    END IF
    IF (.NOT.BSTree_IsRankConsistent(Tree)) THEN
        IF (PRESENT(Message)) Message = 'The tree ranks are not consistent.'
        RETURN
    END IF
    IF (.NOT.BSTree_IsSizeConsistent(Tree)) THEN
        IF (PRESENT(Message)) Message = 'Subtree counts are not consistent.'
        RETURN
    END IF
    Flag = TrueVal
    IF (PRESENT(Message)) Message = 'The tree passes all tests.'

    RETURN

END FUNCTION AVLTree_CheckIntegrity

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION NodeHeight(Node) RESULT(Height)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the height of the node (or subtree)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! input node
    tIndex                              :: Height   ! the height of the node (or subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (.NOT.ASSOCIATED(Node)) THEN
        Height = -1_kIndex
    ELSE
        Height = Node%HC
    END IF
       
    RETURN

END FUNCTION NodeHeight

!******************************************************************************

FUNCTION ComputeBalanceFactor(Node) RESULT(Factor)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the balance factor (as the difference between
    ! the heights of the left-subtree and right-subtree nodes).
    ! Therefore, a subtree with a balance factor of -1, 0 or 1 has
    ! the AVL property since the heights of the two child subtrees
    ! differ by at most one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! input node
    tIndex                              :: Factor   ! balance factor

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (.NOT.ASSOCIATED(Node)) THEN
        Factor = 0
    ELSE
        Factor = NodeHeight(Node%Left) - NodeHeight(Node%Right)
    END IF

    RETURN

END FUNCTION ComputeBalanceFactor

!******************************************************************************

FUNCTION RotateRight(InRoot) RESULT(OutRoot)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a right rotation of a subtree where
    ! its root (input) is NodeA.  After the rotation,
    ! as illustrated in the diagram below, NodeB becomes
    ! the root of the output subtree
    !
    !       A                 B
    !      /                 / \
    !     B         =>      D   A
    !    / \                   /
    !   D   C                 C

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: InRoot   ! root of input subtree
    CLASS(BSTNode), POINTER             :: OutRoot  ! root of output subtree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check for null node(s)
    IF ((.NOT.ASSOCIATED(InRoot)).OR.(.NOT.ASSOCIATED(InRoot%Left))) THEN
        OutRoot => InRoot
        RETURN
    END IF
    
    ASSOCIATE (NodeA => InRoot, NodeB => InRoot%Left, NodeC => InRoot%Left%Right)
        ! perform right rotation
        NodeB%Right => NodeA
        NodeA%Left  => NodeC
        
        ! update the number of nodes in subtree rooted by NodeA and NodeB
        NodeB%Size = NodeA%Size
        NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
        ! update the heights of NodeA and NodeB
        NodeA%HC = MAX(NodeHeight(NodeA%Left), NodeHeight(NodeA%Right)) + 1
        NodeB%HC = MAX(NodeHeight(NodeB%Left), NodeHeight(NodeB%Right)) + 1
        
        ! set output root
        OutRoot => NodeB
    END ASSOCIATE
        
    RETURN

END FUNCTION RotateRight

!******************************************************************************

FUNCTION RotateLeft(InRoot) RESULT(OutRoot)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a left rotation of a subtree where
    ! its root (input) is NodeA.  After the rotation,
    ! as illustrated in the diagram below, NodeB becomes
    ! the root of the output subtree
    !
    !   A                   B
    !    \                 / \
    !     B        =>     A   D
    !    / \               \
    !   C   D               C

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: InRoot   ! root of input subtree
    CLASS(BSTNode), POINTER             :: OutRoot  ! root of output subtree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check for null node(s)
    IF ((.NOT.ASSOCIATED(InRoot)).OR.(.NOT.ASSOCIATED(InRoot%Right))) THEN
        OutRoot => InRoot
        RETURN
    END IF

    ASSOCIATE (NodeA => InRoot, NodeB => InRoot%Right, NodeC => InRoot%Right%Left)
        ! perform left rotation
        NodeB%Left  => NodeA
        NodeA%Right => NodeC
        
        ! update the number of nodes in subtree rooted by NodeA and NodeB
        NodeB%Size = NodeA%Size
        NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
        ! update the heights of NodeA and NodeB
        NodeA%HC = MAX(NodeHeight(NodeA%Left), NodeHeight(NodeA%Right)) + 1
        NodeB%HC = MAX(NodeHeight(NodeB%Left), NodeHeight(NodeB%Right)) + 1

        ! set output root
        OutRoot => NodeB
    END ASSOCIATE
        
    RETURN

END FUNCTION RotateLeft

!******************************************************************************

FUNCTION Rebalance(OrgRoot) RESULT(NewRoot)

!** PURPOSE OF THIS SUBROUTINE:
    ! To restore the AVL tree property of the subtree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: OrgRoot  ! original root of the subtree
    CLASS(BSTNode), POINTER             :: NewRoot  ! new root of the subtree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: RootBalance
    tIndex      :: LeftBalance
    tIndex      :: RightBalance

! FLOW
        
    ! initialize
    NewRoot => OrgRoot
    
    ! check for null node
    IF (.NOT.ASSOCIATED(NewRoot)) RETURN
        
    ! update the height of the new root
    NewRoot%HC = 1_kIndex + MAX(NodeHeight(NewRoot%Left), NodeHeight(NewRoot%Right))

    ! update balance factors
    RootBalance  = ComputeBalanceFactor(NewRoot)
    LeftBalance  = ComputeBalanceFactor(NewRoot%Left)
    RightBalance = ComputeBalanceFactor(NewRoot%Right)
        
    ! check balance factors and rebalance if necessary
    IF (RootBalance > 1) THEN
        ! the subtree is unbalance and its left subtree is longer
        IF (LeftBalance >= 0) THEN
            ! perform right rotation only
            NewRoot => RotateRight(NewRoot)
        ELSE
            ! perform left-right rotation
            NewRoot%Left => RotateLeft(NewRoot%Left)
            NewRoot      => RotateRight(NewRoot)
        END IF
    ELSEIF (RootBalance < -1) THEN
        ! the subtree is unbalance and its right subtree is longer
        IF (RightBalance <= 0) THEN
            ! perform left rotation only
            NewRoot => RotateLeft(NewRoot)
        ELSE
            ! perform right-left rotation
            NewRoot%Right => RotateRight(NewRoot%Right)
            NewRoot       => RotateLeft(NewRoot)
        END IF
    END IF
        
    RETURN

END FUNCTION Rebalance

!******************************************************************************

RECURSIVE FUNCTION DeleteMinNode(InNode, DelNode) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    ! To remove the smallest node from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode),           POINTER, INTENT(INOUT)    :: InNode   ! input node (root node of (sub)tree)
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: DelNode  ! smallest node
    CLASS(BSTNode),           POINTER                   :: OutNode  ! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: TmpNode

! FLOW

    ! initialize
    TmpNode => NULL()
    
    IF (.NOT.ASSOCIATED(InNode%Left)) THEN
        ! node with smallest value found so set output node
        TmpNode => InNode%Right
        ! get optional output
        IF (PRESENT(DelNode)) DelNode => InNode
        ! delete node from the tree
        CALL InNode%Reset()
        ! reset the input node
        InNode => TmpNode
    ELSE
        ! find the node on the left subtree
        InNode%Left => DeleteMinNode(InNode%Left, DelNode)
        ! update number of nodes in subtree rooted by the (current) input node
        InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        ! rebalance the current (replacement) node if necessary; note that 
        ! all ancestors of the current node will also be rebalanced since
        ! the recursive call will travel back up and visit all the ancestors
        ! of the deleted node.
        InNode => Rebalance(InNode)
    END IF  
    ! set output node
    OutNode => InNode
    ! nullify working node
    NULLIFY(TmpNode)

    RETURN

END FUNCTION DeleteMinNode

!******************************************************************************

RECURSIVE FUNCTION DeleteMaxNode(InNode, DelNode) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    ! To remove the largest node from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode),           POINTER, INTENT(INOUT)    :: InNode   ! input node (root node of (sub)tree)
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: DelNode  ! largest node
    CLASS(BSTNode),           POINTER                   :: OutNode  ! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(BSTNode), POINTER :: TmpNode

! FLOW

    ! initialize
    TmpNode => NULL()
    
    IF (.NOT.ASSOCIATED(InNode%Right)) THEN
        ! node with largest value found so set output node
        TmpNode => InNode%Left
        ! get optional output
        IF (PRESENT(DelNode)) DelNode => InNode
        ! delete node from the tree
        CALL InNode%Reset()
        ! reset the input node
        InNode => TmpNode
    ELSE
        ! find the node on the right subtree
        InNode%Right => DeleteMaxNode(InNode%Right, DelNode)
        ! update number of nodes in subtree rooted by the (current) input node
        InNode%Size = NodeSize(InNode%Left) + NodeSize(InNode%Right) + 1
        ! rebalance the current (replacement) node if necessary; note that 
        ! all ancestors of the current node will also be rebalanced since
        ! the recursive call will travel back up and visit all the ancestors
        ! of the deleted node.
        InNode => Rebalance(InNode)
    END IF
    ! set output node
    OutNode => InNode
    ! nullify working node
    NULLIFY(TmpNode)
            
    RETURN

END FUNCTION DeleteMaxNode

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               Routines for Checking Integrity of BST Data Structure
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION BSTree_IsRankConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check that ranks are consistent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree ! IntrusiveAVLTree object
    tLogical                                :: Flag ! true if ranks are consistent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    

! FLOW
        
    ! check rank of select of I
    DO I = 0, Tree%GetSize()-1
        IF (I /= Tree%Rank(Tree%Select(I))) THEN
            Flag = FalseVal
            RETURN
        END IF
    END DO
        
    Flag = TrueVal
        
    ! check select of rank of key
    BLOCK
        ! block variables
        CLASS(BSTNode), POINTER :: NodeA, NodeB
        tLogical                :: IsTheEnd
        ! start inorder traversal
        IsTheEnd = Tree%StartMin(NodeA)
        DO WHILE (.NOT.IsTheEnd)
            NodeB => Tree%Select(Tree%Rank(NodeA))
            ! compare current nodes
            IF (NodeA /= NodeB) THEN
                Flag = FalseVal
                EXIT
            END IF
            ! move to successor node
            IsTheEnd = Tree%MoveForward(NodeA)
        END DO
        ! free block variables
        NULLIFY(NodeA, NodeB)
    END BLOCK

    RETURN

END FUNCTION BSTree_IsRankConsistent

!******************************************************************************

FUNCTION BSTree_IsSizeConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the size fields are correct or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree ! IntrusiveAVLTree object
    tLogical                                :: Flag ! true if the size fields are correct

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Flag = IsSizeConsistent(Tree%Root)

    RETURN

CONTAINS

    RECURSIVE FUNCTION IsSizeConsistent(Node) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the size field is correct or not.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! node
        tLogical                            :: Flag     ! true if the size fields are correct
            
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
    ! To check whether the inorder keys in the tree
    !   are in ascending order or not

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree ! IntrusiveAVLTree object
    tLogical                                :: Flag ! true if the tree satisfies symmetric order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize
    Flag = TrueVal

    ! check if we can return quickly
    IF (Tree%GetSize() <= 1) RETURN
    
    BLOCK
        ! block variables
        CLASS(BSTNode), POINTER :: CurrNode, NextNode
        tLogical                :: IsTheEnd
        ! start inorder traversal
        IsTheEnd = Tree%StartMin(CurrNode)
        DO
            ! get successor node
            IsTheEnd = Tree%MoveForward(NextNode)
            ! check if it is the end of the tree
            IF (IsTheEnd) EXIT
            ! compare current and next nodes
            IF (NextNode <= CurrNode) THEN
                Flag = FalseVal
                EXIT
            END IF
            ! reset current node
            CurrNode => NextNode
        END DO
        ! free block variables
        NULLIFY(CurrNode, NextNode)
    END BLOCK

    RETURN

END FUNCTION BSTree_IsBinarySearchTree

!******************************************************************************

FUNCTION BSTree_IsAVLTree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check if AVL property is consistent in the subtree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveAVLTree), INTENT(INOUT)  :: Tree ! IntrusiveAVLTree object
    tLogical                                :: Flag ! true if AVL property is consistent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = IsAVLTree(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION IsAVLTree(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        ! To check if AVL property is consistent in the subtree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! node
        tLogical                            :: Flag     ! true if AVL property is consistent
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (.NOT.ASSOCIATED(Node)) THEN
            Flag = TrueVal
        ELSE
            IF (ABS(ComputeBalanceFactor(Node)) > 1) THEN
                Flag = FalseVal
            ELSE
                Flag = (IsAVLTree(Node%Left).AND.IsAVLTree(Node%Right))
            END IF
        END IF

        RETURN
            
    END FUNCTION IsAVLTree
        
!**************************************************************************

END FUNCTION BSTree_IsAVLTree

!******************************************************************************

END SUBMODULE SubClass_IntrusiveAVLTree

!******************************************************************************

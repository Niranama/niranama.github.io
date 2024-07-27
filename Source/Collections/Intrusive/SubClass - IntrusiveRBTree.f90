
SUBMODULE (Class_IntrusiveBSTrees) SubClass_IntrusiveRBTree

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains the actual implementation of the *IntrusiveRBTree* type,
!   which is an *intrusive red-black-tree* container that stores objects (or nodes)
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

MODULE SUBROUTINE RBTree_Insert(Tree, NewNode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert a new node into the tree.  If the tree already contains a node
    !  that is equal to the new node, replace that node with the new one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree
    !% a new node to be added to the tree
    CLASS(BSTNode), TARGET, INTENT(IN)      :: NewNode

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! recursively search for place to insert the new node
    Tree%Root => AddNew(Tree%Root, NewNode)
    
    ! set root's color
    Tree%Root%HC = Black

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
        tLogical, PARAMETER     :: IsRBTree = TrueVal
        
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
            END IF
            ! fix-up any right-leaning links
            IF (IsRed(OutNode%Right).AND..NOT.IsRed(OutNode%Left)) OutNode => RotateLeft(OutNode)
            IF (IsRed(OutNode%Left)) THEN
                IF (IsRed(OutNode%Left%Left)) THEN
                    OutNode => RotateRight(OutNode)
                END IF
            END IF
            IF (IsRed(OutNode%Left).AND.IsRed(OutNode%Right)) CALL FlipColor(OutNode)
            ! update number of nodes in subtree rooted by the output node
            OutNode%Size = NodeSize(OutNode%Left) + NodeSize(OutNode%Right) + 1
        END IF

        RETURN

    END FUNCTION AddNew

    !**************************************************************************

END SUBROUTINE RBTree_Insert

!******************************************************************************

MODULE FUNCTION RBTree_Remove(Tree, DelNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the specified node from the tree.  Return the flag
    !  indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree
    !% the node to be removed
    CLASS(BSTNode),         INTENT(IN)      :: DelNode
    !% true if the specified node is removed successfully
    tLogical                                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set flag
    Flag = FalseVal
    
    ! check if the tree is empty or not
    IF (Tree%IsEmpty()) RETURN
        
    ! check if the tree contains the specified key
    IF (.NOT.Tree%Contain(DelNode)) RETURN

    ! if both children of root are black, set root to red
    IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
        Tree%Root%HC = Red
    END IF

    ! delete the specified node
    Tree%Root => DeleteNode(Tree%Root, DelNode)

    ! update color
    IF (.NOT.Tree%IsEmpty()) THEN
        Tree%Root%HC = Black
    END IF

    ! set flag
    Flag = TrueVal

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
        
        ! return if the (sub)tree is empty
        IF (.NOT.ASSOCIATED(InNode)) THEN
            OutNode => NULL()
            RETURN
        END IF

        ! find the node to be deleted
        IF (DelNode < InNode) THEN
            ! check whether to move red node to the left
            IF (.NOT.IsRed(InNode%Left)) THEN
                IF (ASSOCIATED(InNode%Left)) THEN
                    IF (.NOT.IsRed(InNode%Left%Left)) InNode => MoveRedLeft(InNode)
                END IF
            END IF
            ! find the node on the left subtree
            InNode%Left => DeleteNode(InNode%Left, DelNode)
        ELSE
            ! check whether to perform right rotation
            IF (IsRed(InNode%Left)) InNode => RotateRight(InNode)
            ! check whether the node is found with the null right child of the node
            IF ((DelNode == InNode).AND.(.NOT.ASSOCIATED(InNode%Right))) THEN
                ! delete node from the tree
                CALL InNode%Reset()
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
            IF (DelNode == InNode) THEN
                BLOCK
                    ! block variable
                    CLASS(BSTNode), POINTER :: OrgNode
                    ! set pointer to InNode
                    OrgNode => InNode
                    ! find inorder successor of InNode and replace
                    ! the node with the inorder successor node
                    CALL Find_MinNode(OrgNode%Right, InNode)
                    CALL InNode%CopyMembers(OrgNode)
                    ! then delete the inorder successor node instead
                    InNode%Right => DeleteMinNode(OrgNode%Right)
                    ! free working pointers
                    NULLIFY(OrgNode)
                END BLOCK
            ELSE
                ! find the node on the right subtree
                InNode%Right => DeleteNode(InNode%Right, DelNode)
            END IF
        END IF
        ! rebalance
        InNode => Rebalance(InNode)
        ! set output node and return
        OutNode => InNode
        
        RETURN

    END FUNCTION DeleteNode

    !**************************************************************************

END FUNCTION RBTree_Remove

!******************************************************************************

MODULE FUNCTION RBTree_RemoveMin(Tree, OutNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the node with the smallest value from the tree.  Return
    !  the flag indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    CLASS(IntrusiveRBTree),            INTENT(INOUT)    :: Tree
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
        ! if both children of root are black, set root to red
        IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
            Tree%Root%HC = Red
        END IF
        ! delete the node with smallest value
        Tree%Root => DeleteMinNode(Tree%Root, OutNode)
        ! update color
        IF (.NOT.Tree%IsEmpty()) THEN
            Tree%Root%HC = Black
        END IF
    END IF

    RETURN

END FUNCTION RBTree_RemoveMin

!******************************************************************************

MODULE FUNCTION RBTree_RemoveMax(Tree, OutNode) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove the node with the largest value from the tree.  Return
    !  the flag indicating whether the mode is removed successfully or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    CLASS(IntrusiveRBTree),            INTENT(INOUT)    :: Tree
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
        ! if both children of root are black, set root to red
        IF (.NOT.IsRed(Tree%Root%Left).AND..NOT.IsRed(Tree%Root%Right)) THEN
            Tree%Root%HC = Red
        END IF
        ! delete the node with largest value
        Tree%Root => DeleteMaxNode(Tree%Root, OutNode)
        ! update color
        IF (.NOT.Tree%IsEmpty()) THEN
            Tree%Root%HC = Black
        END IF
    END IF

    RETURN

END FUNCTION RBTree_RemoveMax

!******************************************************************************

MODULE SUBROUTINE RBTree_Finalizer(Tree)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To remove all nodes from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    TYPE(IntrusiveRBTree), INTENT(INOUT)   :: Tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL Tree%Clear()
    
    RETURN

END SUBROUTINE RBTree_Finalizer

!******************************************************************************

MODULE FUNCTION RBTree_CheckIntegrity(Tree, Message) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check integrity of the BST data structure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% IntrusiveRBTree object
    CLASS(IntrusiveRBTree), INTENT(INOUT)  :: Tree
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
    IF (.NOT.BSTree_IsBinarySearchTree(Tree)) THEN
        IF (PRESENT(Message)) Message = 'The tree is not in symmetric order.'
        RETURN
    END IF
    IF (.NOT.BSTree_Is23Tree(Tree)) THEN
        IF (PRESENT(Message)) Message = 'The tree is not a 2-3 tree.'
        RETURN
    END IF
    IF (.NOT.BSTree_IsBalanced(Tree)) THEN
        IF (PRESENT(Message)) Message = 'The tree is not a balanced tree.'
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

END FUNCTION RBTree_CheckIntegrity

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           Auxiliary Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION IsRed(Node) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the color of this node is red.
    ! Return FalseVal if the node is not associated.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: Node ! BSTNode object
    tLogical                            :: Flag ! true if the color is red

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ASSOCIATED(Node)) THEN
        Flag = (Node%HC == Red)
    ELSE
        Flag = FalseVal
    END IF
       
    RETURN

END FUNCTION IsRed

!******************************************************************************

FUNCTION RotateRight(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a left-leaning link lean to the right.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: InNode   ! input node (root of input subtree)
    CLASS(BSTNode), POINTER             :: OutNode  ! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check for null node(s)
    IF ((.NOT.ASSOCIATED(InNode)).OR.(.NOT.ASSOCIATED(InNode%Left))) THEN
        OutNode => InNode
        RETURN
    END IF
    
    ASSOCIATE (NodeA => InNode, NodeB => InNode%Left, NodeC => InNode%Left%Right)
        ! perform right rotation
        NodeB%Right => NodeA
        NodeA%Left  => NodeC
        
        ! update the number of nodes in subtree rooted by NodeA and NodeB
        NodeB%Size = NodeA%Size
        NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
        ! update color
        NodeB%HC = NodeB%Right%HC
        NodeB%Right%HC = Red
        
        ! set output root
        OutNode => NodeB
    END ASSOCIATE
        
    RETURN

END FUNCTION RotateRight

!******************************************************************************

FUNCTION RotateLeft(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a right-leaning link lean to the left.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(IN) :: InNode   ! input node (root of input subtree)
    CLASS(BSTNode), POINTER             :: OutNode  ! output node (root of output subtree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! check for null node(s)
    IF ((.NOT.ASSOCIATED(InNode)).OR.(.NOT.ASSOCIATED(InNode%Right))) THEN
        OutNode => InNode
        RETURN
    END IF

    ASSOCIATE (NodeA => InNode, NodeB => InNode%Right, NodeC => InNode%Right%Left)
        ! perform left rotation
        NodeB%Left  => NodeA
        NodeA%Right => NodeC
        
        ! update the number of nodes in subtree rooted by NodeA and NodeB
        NodeB%Size = NodeA%Size
        NodeA%Size = NodeSize(NodeA%Left) + NodeSize(NodeA%Right) + 1
        
        ! update color
        NodeB%HC = NodeB%Left%HC
        NodeB%Left%HC = Red
        
        ! set output root
        OutNode => NodeB
    END ASSOCIATE
        
    RETURN

END FUNCTION RotateLeft

!******************************************************************************

SUBROUTINE FlipColor(Node)

!** PURPOSE OF THIS SUBROUTINE:
    ! To flip the colors of a node and its two children

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: Node   ! RedBlackNode object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Node%HC       = -Node%HC
    Node%Left%HC  = -Node%Left%HC
    Node%Right%HC = -Node%Right%HC
       
    RETURN

END SUBROUTINE FlipColor

!******************************************************************************

FUNCTION MoveRedLeft(InNode) RESULT(OutNode)

!** PURPOSE OF THIS SUBROUTINE:
    ! Assuming that InNode is red and both InNode%Left and InNode%Left%Left
    ! are black, make InNode%Left or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: InNode   ! input node
    CLASS(BSTNode), POINTER                 :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! move the input node to the left
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Right)) THEN
        IF (IsRed(InNode%Right%Left)) THEN
            InNode%Right => RotateRight(InNode%Right)
            InNode       => RotateLeft(InNode)
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
    ! Assuming that InNode is red and both InNode%Right and InNode%Right%Left
    ! are black, make InNode%Right or one of its children red.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: InNode   ! input node
    CLASS(BSTNode), POINTER                 :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! move the input node to the right
    CALL FlipColor(InNode)
    IF (ASSOCIATED(InNode%Left)) THEN
        IF (IsRed(InNode%Left%Left)) THEN
            InNode => RotateRight(InNode)
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
    ! To restore the AVL tree property of the subtree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode), POINTER, INTENT(INOUT)  :: InNode   ! input node
    CLASS(BSTNode), POINTER                 :: OutNode  ! output node

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! check whether to rotate left or not
    IF (IsRed(InNode%Right).AND..NOT.IsRed(InNode%Left)) InNode => RotateLeft(InNode)
        
    ! check whether to rotate right or not
    IF (IsRed(InNode%Left)) THEN
        ! note: if the left node is red, it must not be null
        IF (IsRed(InNode%Left%Left)) InNode => RotateRight(InNode)
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

RECURSIVE FUNCTION DeleteMinNode(InNode, DelNode) RESULT(OutNode)
    
!** PURPOSE OF THIS SUBROUTINE:
    ! To remove the smallest node from the tree.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BSTNode),           POINTER, INTENT(INOUT)    :: InNode   ! input node (root node of (sub)tree)
    CLASS(BSTNode), OPTIONAL, POINTER, INTENT(OUT)      :: DelNode  ! smallest node
    CLASS(BSTNode),           POINTER                   :: OutNode  ! output node (new root node of (sub)tree)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (.NOT.ASSOCIATED(InNode%Left)) THEN
        ! get optional output
        IF (PRESENT(DelNode)) DelNode => InNode
        ! delete node from the tree
        CALL InNode%Reset()
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
        InNode%Left => DeleteMinNode(InNode%Left, DelNode)
        ! rebalance
        InNode => Rebalance(InNode)
    END IF  
    ! set output node
    OutNode => InNode

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
    ! na

! FLOW

    ! check whether to perform right rotation
    IF (IsRed(InNode%Left)) InNode => RotateRight(InNode)
    
    IF (.NOT.ASSOCIATED(InNode%Right)) THEN
        ! get optional output
        IF (PRESENT(DelNode)) DelNode => InNode
        ! delete node from the tree
        CALL InNode%Reset()
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
        InNode%Right => DeleteMaxNode(InNode%Right, DelNode)
        ! rebalance
        InNode => Rebalance(InNode)
    END IF
    ! set output node
    OutNode => InNode
            
    RETURN

END FUNCTION DeleteMaxNode

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               Routines for Checking Integrity of BST Data Structure
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION BSTree_IsRankConsistent(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check that ranks are consistent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRBTree), INTENT(INOUT)  :: Tree ! IntrusiveRBTree object
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
    CLASS(IntrusiveRBTree), INTENT(INOUT)  :: Tree ! IntrusiveRBTree object
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
    CLASS(IntrusiveRBTree), INTENT(INOUT)  :: Tree ! IntrusiveRBTree object
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

FUNCTION BSTree_Is23Tree(Tree) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the tree is a 2-3 tree.
    ! => Does the tree have no red right links, and at most one (left)
    !    red links in a row on any path?

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree ! IntrusiveRBTree object
    tLogical                                :: Flag ! true if this is a 2-3 tree

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    Flag = Is23Tree(Tree%Root)
        
    RETURN
        
CONTAINS

    RECURSIVE FUNCTION Is23Tree(Node) RESULT(Flag)
            
    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the subtree is a 2-3 tree.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(IN) :: Node     ! node
        tLogical                            :: Flag     ! true if this is a 2-3 tree
            
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
    ! To check whether the tree is balanced.
    ! => Do all paths from root to leaf have
    !    same number of black edges?.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntrusiveRBTree), INTENT(INOUT)   :: Tree ! IntrusiveRBTree object
    tLogical                                :: Flag ! true if the tree is balanced

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: BlackCount   ! number of black links on path from root to min
    CLASS(BSTNode), POINTER :: XNode => NULL()

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
        ! To check whether the subtree is balanced.
        ! => Does every path from the root to a leaf
        !    have the given number of black links?

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BSTNode), POINTER, INTENT(IN) :: Node         ! node
        tIndex,                  INTENT(IN) :: BlackCount   ! number of black links on path from root to min
        tLogical                            :: Flag         ! true if the tree is balanced
            
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex  :: CurrBlackCount

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

END SUBMODULE SubClass_IntrusiveRBTree

!******************************************************************************

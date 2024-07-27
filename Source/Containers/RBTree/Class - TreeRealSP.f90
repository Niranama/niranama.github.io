
MODULE Class_TreeRealSP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *TreeRealSP* type and its related helper type and routines.
!   The *TreeRealSP* type is a container type representing an ordered symbol table, which
!   is a container that associates a *value* with a *key* where keys are stored in a sorted
!   order.  It employs a balanced binary-search-tree (BST) implementation to provide common
!   operations for an ordered symbol table.  As an ordered symbol table, the *TreeRealSP*
!   type uses the Fortran intrinsic *REAL(KIND=kSP)* type as the type of its stored keys
!   and an unlimited polymorphic type as the type of its stored values. <br>
!   As a symbol table, the *TreeRealSP* type does not allow duplicated keys.  Therefore,
!   if an inserted key is equal to a key stored in the table, an associated value of the
!   stored key is replaced by an associated value of the inserted key.  Technically, the
!   *TreeRealSP* type employs a left-leaning red-black (RB) tree as the balanced BST. <br>
!   See the <a href="../module/modbase_balancedtrees.html">ModBase_BalancedTrees</a> module
!   for an overview of a *balanced-tree-based* type.  A user may use the *ModBase_BalancedTrees*
!   module instead of using this module directly. <br>
!   See the <a href="../module/class_treetable.html">Class_TreeTable</a> module for a balanced
!   tree container type that is functionally similar to the *TreeRealSP* type but utilizes
!   a different implementation.  Also, unlike the *TreeRealSP* type, the *TreeTable* type
!   is designed as a generic ordered symbol table that allows keys with various types to be
!   inserted into the table. <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Memory_Handlers
    USE ModBase_Error_Handlers
    USE ModBase_SIntUtil,       ONLY: ToChar => ToDecStrSigned
    USE ModBase_DoublyLinkedLists
    USE Class_Assignable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: TreeRealSP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define KeyTypeA        REAL(KIND=kSP)
#define KeyTypeB        REAL(KIND=kSP)
#define KeyTypeC        REAL(KIND=kSP)
#define QueueKey        ListRealSP
#define QueueVal        ListAnyType
#define RedBlackTree    TreeRealSP

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'Class_TreeRealSP'

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic Tree - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic Tree - Implementation.f90"

!** UNDEFINE MACROS **
#undef RedBlackTree
#undef KeyTypeA
#undef KeyTypeB
#undef KeyTypeC
#undef QueueKey
#undef QueueVal

END MODULE Class_TreeRealSP

!******************************************************************************

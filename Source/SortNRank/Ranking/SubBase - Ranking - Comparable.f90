
SUBMODULE (ModBase_Ranking) SubBase_Ranking_Comp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of *Comparable* objects
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define TYPE_IS_IN_Comparable_CLASS
#define tArgument                   CLASS(Comparable)
#define tVarScalar                  CLASS(Comparable), ALLOCATABLE
#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)
#define Is_Array_Ranked             IsRanked_Comp
#define Wise_Rank_Unstable          Wise_RankComp
#define Wise_Rank_Stable            WiseStable_RankComp
#define Intro_Rank                  Intro_RankComp
#define Java_Rank                   Java_RankComp
#define PDQ_Rank                    PDQ_RankComp
#define Tim_Rank                    Tim_RankComp
#define Rust_Rank                   Rust_RankComp
#define Quick_Rank_Hoare            QuickHoare_RankComp
#define Quick_Rank_Lomuto           QuickLomuto_RankComp
#define Quick_Rank_Mo3              QuickMo3_RankComp
#define Quick_Rank_3Way             Quick3Way_RankComp
#define Quick_Rank_Vowels           QuickVowels_RankComp
#define Quick_Rank_Stable           QuickStable_RankComp
#define Quick_Rank_Iterative        QuickIterative_RankComp
#define Quick_Rank_Java             QuickJava_RankComp
#define Merge_Rank_TopDown          MergeTopDown_RankComp
#define Merge_Rank_BottomUp         MergeBottomUp_RankComp
#define Merge_Rank_QuadSplit        MergeQuadSplit_RankComp
#define Merge_Rank_HalfCopy         MergeHalfCopy_RankComp
#define Merge_Rank_OrderPack        MergeOrderPack_RankComp

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Rank Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_Ranking_Comp

!******************************************************************************


SUBMODULE (ModBase_Ranking) SubBase_Ranking_R128

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 128-bit real numbers
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tQuad
#define tVarScalar                  tQuad
#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)
#define Is_Array_Ranked             IsRanked_R128
#define Wise_Rank_Unstable          Wise_RankR128
#define Wise_Rank_Stable            WiseStable_RankR128
#define Intro_Rank                  Intro_RankR128
#define Java_Rank                   Java_RankR128
#define PDQ_Rank                    PDQ_RankR128
#define Tim_Rank                    Tim_RankR128
#define Rust_Rank                   Rust_RankR128
#define Quick_Rank_Hoare            QuickHoare_RankR128
#define Quick_Rank_Lomuto           QuickLomuto_RankR128
#define Quick_Rank_Mo3              QuickMo3_RankR128
#define Quick_Rank_3Way             Quick3Way_RankR128
#define Quick_Rank_Vowels           QuickVowels_RankR128
#define Quick_Rank_Stable           QuickStable_RankR128
#define Quick_Rank_Iterative        QuickIterative_RankR128
#define Quick_Rank_Java             QuickJava_RankR128
#define Merge_Rank_TopDown          MergeTopDown_RankR128
#define Merge_Rank_BottomUp         MergeBottomUp_RankR128
#define Merge_Rank_QuadSplit        MergeQuadSplit_RankR128
#define Merge_Rank_HalfCopy         MergeHalfCopy_RankR128
#define Merge_Rank_OrderPack        MergeOrderPack_RankR128

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Rank Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_Ranking_R128

!******************************************************************************

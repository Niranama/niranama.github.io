
SUBMODULE (ModBase_Ranking) SubBase_Ranking_Char

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of character strings
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tCharStar
#define tVarScalar                  tCharLen(LEN(AVal(1)))
#define COMPARE_GLT(A, B)           LLT(A, B)
#define COMPARE_GLE(A, B)           LLE(A, B)
#define Is_Array_Ranked             IsRanked_Char
#define Wise_Rank_Unstable          Wise_RankChar
#define Wise_Rank_Stable            WiseStable_RankChar
#define Intro_Rank                  Intro_RankChar
#define Java_Rank                   Java_RankChar
#define PDQ_Rank                    PDQ_RankChar
#define Tim_Rank                    Tim_RankChar
#define Rust_Rank                   Rust_RankChar
#define Quick_Rank_Hoare            QuickHoare_RankChar
#define Quick_Rank_Lomuto           QuickLomuto_RankChar
#define Quick_Rank_Mo3              QuickMo3_RankChar
#define Quick_Rank_3Way             Quick3Way_RankChar
#define Quick_Rank_Vowels           QuickVowels_RankChar
#define Quick_Rank_Stable           QuickStable_RankChar
#define Quick_Rank_Iterative        QuickIterative_RankChar
#define Quick_Rank_Java             QuickJava_RankChar
#define Merge_Rank_TopDown          MergeTopDown_RankChar
#define Merge_Rank_BottomUp         MergeBottomUp_RankChar
#define Merge_Rank_QuadSplit        MergeQuadSplit_RankChar
#define Merge_Rank_HalfCopy         MergeHalfCopy_RankChar
#define Merge_Rank_OrderPack        MergeOrderPack_RankChar

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Rank Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_Ranking_Char

!******************************************************************************

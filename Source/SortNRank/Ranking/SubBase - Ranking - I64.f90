﻿
SUBMODULE (ModBase_Ranking) SubBase_Ranking_I64

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 64-bit integer numbers
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tSInt64
#define tVarScalar                  tSInt64
#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)
#define Is_Array_Ranked             IsRanked_I64
#define Wise_Rank_Unstable          Wise_RankI64
#define Wise_Rank_Stable            WiseStable_RankI64
#define Intro_Rank                  Intro_RankI64
#define Java_Rank                   Java_RankI64
#define PDQ_Rank                    PDQ_RankI64
#define Tim_Rank                    Tim_RankI64
#define Rust_Rank                   Rust_RankI64
#define Quick_Rank_Hoare            QuickHoare_RankI64
#define Quick_Rank_Lomuto           QuickLomuto_RankI64
#define Quick_Rank_Mo3              QuickMo3_RankI64
#define Quick_Rank_3Way             Quick3Way_RankI64
#define Quick_Rank_Vowels           QuickVowels_RankI64
#define Quick_Rank_Stable           QuickStable_RankI64
#define Quick_Rank_Iterative        QuickIterative_RankI64
#define Quick_Rank_Java             QuickJava_RankI64
#define Merge_Rank_TopDown          MergeTopDown_RankI64
#define Merge_Rank_BottomUp         MergeBottomUp_RankI64
#define Merge_Rank_QuadSplit        MergeQuadSplit_RankI64
#define Merge_Rank_HalfCopy         MergeHalfCopy_RankI64
#define Merge_Rank_OrderPack        MergeOrderPack_RankI64

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Rank Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_Ranking_I64

!******************************************************************************

SUBMODULE (ModBase_SortAscend) SubBase_SortAscend_Char

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
#define tVarLocal                   tCharLen(LEN(A(1)))
#define tVarScalar                  tCharLen(LEN(A(1)))
#define tVarAlloc                   tCharAlloc
#define COMPARE_GLT(A, B)           LLT(A, B)
#define COMPARE_GLE(A, B)           LLE(A, B)
#define Is_Array_Sorted             IsSortedAscend_Char
#define Wise_Sort_Unstable          Wise_SortChar
#define Wise_Sort_Stable            WiseStable_SortChar
#define Intro_Sort                  Intro_SortChar
#define Java_Sort                   Java_SortChar
#define PDQ_Sort                    PDQ_SortChar
#define Tim_Sort                    Tim_SortChar
#define Rust_Sort                   Rust_SortChar
#define Quick_Sort_Hoare            QuickHoare_SortChar
#define Quick_Sort_Lomuto           QuickLomuto_SortChar
#define Quick_Sort_Mo3              QuickMo3_SortChar
#define Quick_Sort_3Way             Quick3Way_SortChar
#define Quick_Sort_Vowels           QuickVowels_SortChar
#define Quick_Sort_Stable           QuickStable_SortChar
#define Quick_Sort_Iterative        QuickIterative_SortChar
#define Quick_Sort_Java             QuickJava_SortChar
#define Merge_Sort_TopDown          MergeTopDown_SortChar
#define Merge_Sort_BottomUp         MergeBottomUp_SortChar
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortChar
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortChar

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortAscend_Char

!******************************************************************************

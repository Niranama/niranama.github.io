
SUBMODULE (ModBase_SortDescend) SubBase_SortDescend_R128

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 128-bit real numbers
!   in an descending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tQuad
#define tVarLocal                   tQuad
#define tVarScalar                  tQuad
#define tVarAlloc                   tQuad, ALLOCATABLE
#define COMPARE_GLT(A, B)           (A > B)
#define COMPARE_GLE(A, B)           (A >= B)
#define Is_Array_Sorted             IsSortedDescend_R128
#define Wise_Sort_Unstable          Wise_SortR128
#define Wise_Sort_Stable            WiseStable_SortR128
#define Intro_Sort                  Intro_SortR128
#define Java_Sort                   Java_SortR128
#define PDQ_Sort                    PDQ_SortR128
#define Tim_Sort                    Tim_SortR128
#define Rust_Sort                   Rust_SortR128
#define Quick_Sort_Hoare            QuickHoare_SortR128
#define Quick_Sort_Lomuto           QuickLomuto_SortR128
#define Quick_Sort_Mo3              QuickMo3_SortR128
#define Quick_Sort_3Way             Quick3Way_SortR128
#define Quick_Sort_Vowels           QuickVowels_SortR128
#define Quick_Sort_Stable           QuickStable_SortR128
#define Quick_Sort_Iterative        QuickIterative_SortR128
#define Quick_Sort_Java             QuickJava_SortR128
#define Merge_Sort_TopDown          MergeTopDown_SortR128
#define Merge_Sort_BottomUp         MergeBottomUp_SortR128
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortR128
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortR128

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortDescend_R128

!******************************************************************************

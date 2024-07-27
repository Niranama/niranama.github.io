
SUBMODULE (ModBase_SortDescend) SubBase_SortDescend_R32

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 32-bit real numbers
!   in an descending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tSingle
#define tVarLocal                   tSingle
#define tVarScalar                  tSingle
#define tVarAlloc                   tSingle, ALLOCATABLE
#define COMPARE_GLT(A, B)           (A > B)
#define COMPARE_GLE(A, B)           (A >= B)
#define Is_Array_Sorted             IsSortedDescend_R32
#define Wise_Sort_Unstable          Wise_SortR32
#define Wise_Sort_Stable            WiseStable_SortR32
#define Intro_Sort                  Intro_SortR32
#define Java_Sort                   Java_SortR32
#define PDQ_Sort                    PDQ_SortR32
#define Tim_Sort                    Tim_SortR32
#define Rust_Sort                   Rust_SortR32
#define Quick_Sort_Hoare            QuickHoare_SortR32
#define Quick_Sort_Lomuto           QuickLomuto_SortR32
#define Quick_Sort_Mo3              QuickMo3_SortR32
#define Quick_Sort_3Way             Quick3Way_SortR32
#define Quick_Sort_Vowels           QuickVowels_SortR32
#define Quick_Sort_Stable           QuickStable_SortR32
#define Quick_Sort_Iterative        QuickIterative_SortR32
#define Quick_Sort_Java             QuickJava_SortR32
#define Merge_Sort_TopDown          MergeTopDown_SortR32
#define Merge_Sort_BottomUp         MergeBottomUp_SortR32
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortR32
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortR32

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortDescend_R32

!******************************************************************************

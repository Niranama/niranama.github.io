
SUBMODULE (ModBase_SortDescend) SubBase_SortDescend_I64

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 64-bit integer numbers
!   in an descending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tSInt64
#define tVarLocal                   tSInt64
#define tVarScalar                  tSInt64
#define tVarAlloc                   tSInt64, ALLOCATABLE
#define COMPARE_GLT(A, B)           (A > B)
#define COMPARE_GLE(A, B)           (A >= B)
#define Is_Array_Sorted             IsSortedDescend_I64
#define Wise_Sort_Unstable          Wise_SortI64
#define Wise_Sort_Stable            WiseStable_SortI64
#define Intro_Sort                  Intro_SortI64
#define Java_Sort                   Java_SortI64
#define PDQ_Sort                    PDQ_SortI64
#define Tim_Sort                    Tim_SortI64
#define Rust_Sort                   Rust_SortI64
#define Quick_Sort_Hoare            QuickHoare_SortI64
#define Quick_Sort_Lomuto           QuickLomuto_SortI64
#define Quick_Sort_Mo3              QuickMo3_SortI64
#define Quick_Sort_3Way             Quick3Way_SortI64
#define Quick_Sort_Vowels           QuickVowels_SortI64
#define Quick_Sort_Stable           QuickStable_SortI64
#define Quick_Sort_Iterative        QuickIterative_SortI64
#define Quick_Sort_Java             QuickJava_SortI64
#define Merge_Sort_TopDown          MergeTopDown_SortI64
#define Merge_Sort_BottomUp         MergeBottomUp_SortI64
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortI64
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortI64

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortDescend_I64

!******************************************************************************

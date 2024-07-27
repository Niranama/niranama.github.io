
SUBMODULE (ModBase_SortAscend) SubBase_SortAscend_I32

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 32-bit integer numbers
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tSInt32
#define tVarLocal                   tSInt32
#define tVarScalar                  tSInt32
#define tVarAlloc                   tSInt32, ALLOCATABLE
#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)
#define Is_Array_Sorted             IsSortedAscend_I32
#define Wise_Sort_Unstable          Wise_SortI32
#define Wise_Sort_Stable            WiseStable_SortI32
#define Intro_Sort                  Intro_SortI32
#define Java_Sort                   Java_SortI32
#define PDQ_Sort                    PDQ_SortI32
#define Tim_Sort                    Tim_SortI32
#define Rust_Sort                   Rust_SortI32
#define Quick_Sort_Hoare            QuickHoare_SortI32
#define Quick_Sort_Lomuto           QuickLomuto_SortI32
#define Quick_Sort_Mo3              QuickMo3_SortI32
#define Quick_Sort_3Way             Quick3Way_SortI32
#define Quick_Sort_Vowels           QuickVowels_SortI32
#define Quick_Sort_Stable           QuickStable_SortI32
#define Quick_Sort_Iterative        QuickIterative_SortI32
#define Quick_Sort_Java             QuickJava_SortI32
#define Merge_Sort_TopDown          MergeTopDown_SortI32
#define Merge_Sort_BottomUp         MergeBottomUp_SortI32
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortI32
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortI32

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortAscend_I32

!******************************************************************************

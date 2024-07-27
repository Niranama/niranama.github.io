
SUBMODULE (ModBase_SortAscend) SubBase_SortAscend_I8

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of 8-bit integer numbers
!   in an ascending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define tArgument                   tSInt8
#define tVarLocal                   tSInt8
#define tVarScalar                  tSInt8
#define tVarAlloc                   tSInt8, ALLOCATABLE
#define COMPARE_GLT(A, B)           (A < B)
#define COMPARE_GLE(A, B)           (A <= B)
#define Is_Array_Sorted             IsSortedAscend_I8
#define Wise_Sort_Unstable          Wise_SortI8
#define Wise_Sort_Stable            WiseStable_SortI8
#define Intro_Sort                  Intro_SortI8
#define Java_Sort                   Java_SortI8
#define PDQ_Sort                    PDQ_SortI8
#define Tim_Sort                    Tim_SortI8
#define Rust_Sort                   Rust_SortI8
#define Quick_Sort_Hoare            QuickHoare_SortI8
#define Quick_Sort_Lomuto           QuickLomuto_SortI8
#define Quick_Sort_Mo3              QuickMo3_SortI8
#define Quick_Sort_3Way             Quick3Way_SortI8
#define Quick_Sort_Vowels           QuickVowels_SortI8
#define Quick_Sort_Stable           QuickStable_SortI8
#define Quick_Sort_Iterative        QuickIterative_SortI8
#define Quick_Sort_Java             QuickJava_SortI8
#define Merge_Sort_TopDown          MergeTopDown_SortI8
#define Merge_Sort_BottomUp         MergeBottomUp_SortI8
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortI8
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortI8

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortAscend_I8

!******************************************************************************


SUBMODULE (ModBase_SortDescend) SubBase_SortDescend_Comp

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains various routines for sorting an array of *Comparable* objects
!   in an descending order.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define TYPE_IS_IN_Comparable_CLASS
#define tArgument                   CLASS(Comparable)
#define tVarLocal                   CLASS(Comparable)
#define tVarScalar                  CLASS(Comparable), ALLOCATABLE
#define tVarAlloc                   CLASS(Comparable), ALLOCATABLE
#define COMPARE_GLT(A, B)           (A > B)
#define COMPARE_GLE(A, B)           (A >= B)
#define Is_Array_Sorted             IsSortedDescend_Comp
#define Wise_Sort_Unstable          Wise_SortComp
#define Wise_Sort_Stable            WiseStable_SortComp
#define Intro_Sort                  Intro_SortComp
#define Java_Sort                   Java_SortComp
#define PDQ_Sort                    PDQ_SortComp
#define Tim_Sort                    Tim_SortComp
#define Rust_Sort                   Rust_SortComp
#define Quick_Sort_Hoare            QuickHoare_SortComp
#define Quick_Sort_Lomuto           QuickLomuto_SortComp
#define Quick_Sort_Mo3              QuickMo3_SortComp
#define Quick_Sort_3Way             Quick3Way_SortComp
#define Quick_Sort_Vowels           QuickVowels_SortComp
#define Quick_Sort_Stable           QuickStable_SortComp
#define Quick_Sort_Iterative        QuickIterative_SortComp
#define Quick_Sort_Java             QuickJava_SortComp
#define Merge_Sort_TopDown          MergeTopDown_SortComp
#define Merge_Sort_BottomUp         MergeBottomUp_SortComp
#define Merge_Sort_QuadSplit        MergeQuadSplit_SortComp
#define Merge_Sort_HalfCopy         MergeHalfCopy_SortComp

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!              GENERIC DECLARATION AND IMPLEMENTATION
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#include "../Includes/Generic Sort Implementation.f90"

!*********************************************************************

END SUBMODULE SubBase_SortDescend_Comp

!******************************************************************************


MODULE ModBase_PolyCommon

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains common routines for unlimited polymorphic entities.
!   For all concrete procedures, the specified polymorphic arguments can have
!   any rank between 0 and 7.  <br>

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! abstract procedure for data copying
    PUBLIC  :: CopyData
    PUBLIC  :: CompareData
    ! concrete procedures for data copying for Fortran intrinsic types
    ! and derived types in the Assignable class
    PUBLIC  :: CopyData_Character
    PUBLIC  :: CopyData_Integer
    PUBLIC  :: CopyData_Real
    PUBLIC  :: CopyData_Complex
    PUBLIC  :: CopyData_Logical
    ! general procedures for unlimited polymorphic entities
    PUBLIC  :: PolyCopyData
    PUBLIC  :: PolySwapData

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModBase_PolyCommon'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tInteger,  PARAMETER    :: MsgLen = 128

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !^ *CopyData* is an interface for a procedure to copy unlimited polymorphic data where
        !  both the source and the destination must have the same type, kind, rank, and size.
        !  If all of those samenesses are met, the routine should set the *ErrStat* to false
        !  and copy data from the source to the destination. Otherwise, the routine should set
        !  the *ErrStat* to true and return immediately. <br>
        FUNCTION CopyData(OutData, InData) RESULT(ErrStat)
            IMPORT
            CLASS(*), INTENT(OUT)   :: OutData(..)  !! output (destination)
            CLASS(*), INTENT(IN)    :: InData(..)   !! input (source)
            tLogical                :: ErrStat      !! true if error occurs while copying data
        END FUNCTION CopyData
        !^ *CompareData* is an interface for a procedure to compare *comparable* data
        !  where the output flag should be set to the following value: <br>
        !   1 if DataA is greater than DataB,  <br>
        !   0 if DataA is equal to DataB, <br>
        !  -1 if DataA is less than DataB.
        FUNCTION CompareData(DataA,DataB) RESULT(Flag)
            IMPORT
            CLASS(*), INTENT(IN)    :: DataA    !! data A
            CLASS(*), INTENT(IN)    :: DataB    !! data B
            tInteger                :: Flag     !! output flag
        END FUNCTION CompareData
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!               DATA-COPYING ROUTINES FOR FORTRAN INTRINSIC TYPES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION CopyData_Character(OutData, InData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments
    !  must have the same sizes, ranks and lengths and their types must be *CHARACTER*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Character', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Character', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
#include    "Includes/CopyData_Character_CodeFracment.f90"
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('CopyData_Character', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION CopyData_Character

!******************************************************************************

FUNCTION CopyData_Integer(OutData, InData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments
    !  must have the same sizes, ranks and kinds and their types must be *INTEGER*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Integer', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Integer', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
#include    "Includes/CopyData_Integer_CodeFracment.f90"
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('CopyData_Integer', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION CopyData_Integer

!******************************************************************************

FUNCTION CopyData_Real(OutData, InData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments
    !  must have the same sizes, ranks and kinds and their types must be *REAL*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Real', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Real', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
#include    "Includes/CopyData_Real_CodeFracment.f90"
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('CopyData_Real', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION CopyData_Real

!******************************************************************************

FUNCTION CopyData_Complex(OutData, InData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments
    !  must have the same sizes, ranks and kinds and their types must be *COMPLEX*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Complex', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Complex', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
#include    "Includes/CopyData_Complex_CodeFracment.f90"
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('CopyData_Complex', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION CopyData_Complex

!******************************************************************************

FUNCTION CopyData_Logical(OutData, InData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments
    !  must have the same sizes and ranks and their types must be *LOGICAL*.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Logical', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('CopyData_Logical', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
#include    "Includes/CopyData_Logical_CodeFracment.f90"
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('CopyData_Logical', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION CopyData_Logical

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           POLYMORPHIC ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION PolyCopyData(OutData, InData, MakeCopy) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To make a copy of the input data.  The *OutData* and *InData* arguments must have the
    !  same sizes, ranks and types.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData(..)  !! output
    CLASS(*), INTENT(IN)    :: InData(..)   !! input
    PROCEDURE(CopyData)     :: MakeCopy     !! A procedure to make a copy of A.
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(OutData) /= RANK(InData)) THEN
        CALL Handle_ErrLevel('PolyCopyData', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(OutData) /= SIZE(InData)) THEN
        CALL Handle_ErrLevel('PolyCopyData', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(OutArr => OutData)
    RANK (0)
        SELECT RANK(InArr => InData)
        RANK (0)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (1)
        SELECT RANK(InArr => InData)
        RANK (1)
           ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (2)
        SELECT RANK(InArr => InData)
        RANK (2)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (3)
        SELECT RANK(InArr => InData)
        RANK (3)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (4)
        SELECT RANK(InArr => InData)
        RANK (4)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (5)
        SELECT RANK(InArr => InData)
        RANK (5)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (6)
        SELECT RANK(InArr => InData)
        RANK (6)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK (7)
        SELECT RANK(InArr => InData)
        RANK (7)
            ErrStat= MakeCopy(OutArr, InArr)
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('PolyCopyData', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION PolyCopyData

!******************************************************************************

FUNCTION PolySwapData(Data1, Data2, MakeCopy) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To swap the input data.  The *Data1* and *Data2* arguments must have the
    !  same sizes, ranks and types.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(INOUT) :: Data1(..)    !! output
    CLASS(*), INTENT(INOUT) :: Data2(..)    !! input
    PROCEDURE(CopyData)     :: MakeCopy     !! A procedure to make a copy of A.
    tLogical                :: ErrStat      !! true if error occurs while copying data

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! set initial flag result
    ErrStat = FalseVal

    ! check for errors
    IF (RANK(Data1) /= RANK(Data2)) THEN
        CALL Handle_ErrLevel('PolySwapData', ModName, ErrSevere, &
            'Ranks of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    ELSEIF (SIZE(Data1) /= SIZE(Data2)) THEN
        CALL Handle_ErrLevel('PolySwapData', ModName, ErrSevere, &
            'Sizes of input and output are NOT the same.')
        ErrStat = TrueVal
        RETURN
    END IF

    SELECT RANK(ArrOne => Data1)
    RANK (0)
        SELECT RANK(ArrTwo => Data2)
        RANK (0)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (1)
        SELECT RANK(ArrTwo => Data2)
        RANK (1)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (2)
        SELECT RANK(ArrTwo => Data2)
        RANK (2)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (3)
        SELECT RANK(ArrTwo => Data2)
        RANK (3)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (4)
        SELECT RANK(ArrTwo => Data2)
        RANK (4)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:,:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (5)
        SELECT RANK(ArrTwo => Data2)
        RANK (5)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:,:,:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (6)
        SELECT RANK(ArrTwo => Data2)
        RANK (6)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:,:,:,:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK (7)
        SELECT RANK(ArrTwo => Data2)
        RANK (7)
            BLOCK
                CLASS(*), ALLOCATABLE   :: TmpArr(:,:,:,:,:,:,:)
                ALLOCATE(TmpArr, SOURCE=ArrOne)
                ErrStat= MakeCopy(ArrOne, ArrTwo)
                IF (ErrStat) RETURN
                ErrStat= MakeCopy(ArrTwo, TmpArr)
                DEALLOCATE(TmpArr)
            END BLOCK
        END SELECT
    RANK DEFAULT
        CALL Handle_ErrLevel('PolySwapData', ModName, ErrSevere, &
            'Currently, only handle an entity with the rank between 0 and 7.')
        ErrStat = TrueVal
    END SELECT

    RETURN

END FUNCTION PolySwapData

!******************************************************************************

END MODULE ModBase_PolyCommon

!******************************************************************************

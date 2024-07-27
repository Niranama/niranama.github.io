
MODULE ModBase_PolyCommon

!** PURPOSE OF THIS MODULE:
    ! contains common routines dealing with polymorphism

!** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Memory_Handlers
    USE ModBase_Error_Handlers
    USE Class_Assignable
    USE Class_Comparable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! interface routines
    PUBLIC  :: CopyData
    PUBLIC  :: CopyArray1D
    PUBLIC  :: CopyArray2D
    PUBLIC  :: CopyArray3D
    PUBLIC  :: CompareData
    ! data-management routines
    PUBLIC  :: PolyCopy
    PUBLIC  :: PolySwap
    ! anytype2byte routines
    PUBLIC  :: AnyType2ChrBytes
    PUBLIC  :: AnyType2IntBytes
    ! intr2byte routines
    PUBLIC  :: Intr2ChrBytes
    PUBLIC  :: Intr2IntBytes

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
    ! abstract interfaces
    ABSTRACT INTERFACE
        ! CopyData is a procedure to copy data where data type is a derived one.
        ! For data with allocatable/pointer components, the procedure
        ! not only makes a copy of the data but also allocates
        ! memory/storage of those allocatable/pointer components.
        FUNCTION CopyData(OutData,InData) RESULT(ErrStat)
            IMPLICIT NONE
            CLASS(*), INTENT(OUT)   :: OutData  ! output
            CLASS(*), INTENT(IN)    :: InData   ! input
            tLogical                :: ErrStat  ! true if error occurs while copying data
        END FUNCTION CopyData
        ! CopyArray1D is similar to CopyData where CopyData is for scalar data
        ! whereas CopyArray1D is for one-dimensional arrays.
        FUNCTION CopyArray1D(OutData,InData,N) RESULT(ErrStat)
            IMPORT
            IMPLICIT NONE
            tIndex,   INTENT(IN)    :: N            ! size of the arrays
            CLASS(*), INTENT(OUT)   :: OutData(N)   ! output
            CLASS(*), INTENT(IN)    :: InData(N)    ! input
            tLogical                :: ErrStat      ! true if error occurs while copying data
        END FUNCTION CopyArray1D
        ! CopyArray2D is similar to CopyData where CopyData is for scalar data
        ! whereas CopyArray2D is for two-dimensional arrays.
        FUNCTION CopyArray2D(OutData,InData,N1,N2) RESULT(ErrStat)
            IMPORT
            IMPLICIT NONE
            tIndex,   INTENT(IN)    :: N1               ! size of the first dimension
            tIndex,   INTENT(IN)    :: N2               ! size of the second dimension
            CLASS(*), INTENT(OUT)   :: OutData(N1,N2)   ! output
            CLASS(*), INTENT(IN)    :: InData(N1,N2)    ! input
            tLogical                :: ErrStat          ! true if error occurs while copying data
        END FUNCTION CopyArray2D
        ! CopyArray3D is similar to CopyData where CopyData is for scalar data
        ! whereas CopyArray3D is for three-dimensional arrays.
        FUNCTION CopyArray3D(OutData,InData,N1,N2,N3) RESULT(ErrStat)
            IMPORT
            IMPLICIT NONE
            tIndex,   INTENT(IN)    :: N1                   ! size of the first dimension
            tIndex,   INTENT(IN)    :: N2                   ! size of the second dimension
            tIndex,   INTENT(IN)    :: N3                   ! size of the third dimension
            CLASS(*), INTENT(OUT)   :: OutData(N1,N2,N3)    ! output
            CLASS(*), INTENT(IN)    :: InData(N1,N2,N3)     ! input
            tLogical                :: ErrStat              ! true if error occurs while copying data
        END FUNCTION CopyArray3D
        ! DataEqaul is a procedure to compare whether two data are considered
        ! to be equal (having same quantities, measures or values)
        FUNCTION DataEqaul(DataA,DataB) RESULT(Flag)
            IMPLICIT NONE
            CLASS(*), INTENT(IN)    :: DataA    ! data A
            CLASS(*), INTENT(IN)    :: DataB    ! data B
            tLogical                :: Flag     ! true if the two data are equal one another
        END FUNCTION DataEqaul
        ! CompareData is a procedure to compare 'comparable' data
        ! where the output flag should be set to the following value:
        !   1 if DataA is greater than DataB
        !   0 if DataA is equal to DataB
        !  -1 if DataA is less than DataB
        FUNCTION CompareData(DataA,DataB) RESULT(Flag)
            IMPORT
            IMPLICIT NONE
            CLASS(*), INTENT(IN)    :: DataA    ! data A
            CLASS(*), INTENT(IN)    :: DataB    ! data B
            tInteger                :: Flag     ! output flag
        END FUNCTION CompareData
    END INTERFACE

!** GENERIC SPECIFICATIONS:
    ! assignment routines
    INTERFACE PolyCopy
        MODULE PROCEDURE Poly_Copy_Data
        MODULE PROCEDURE Poly_Copy_Array1D
        MODULE PROCEDURE Poly_Copy_Array2D
        MODULE PROCEDURE Poly_Copy_Array3D
    END INTERFACE
    INTERFACE PolySwap
        MODULE PROCEDURE Poly_Swap_Scalar
        MODULE PROCEDURE Poly_Swap_Array1D
        MODULE PROCEDURE Poly_Swap_Array2D
        MODULE PROCEDURE Poly_Swap_Array3D
    END INTERFACE
    ! anytype2byte routines
    INTERFACE AnyType2ChrBytes
        MODULE PROCEDURE AnyType_To_ChrBytes_Scalar
        MODULE PROCEDURE AnyType_To_ChrBytes_Array1D
        MODULE PROCEDURE AnyType_To_ChrBytes_Array2D
        MODULE PROCEDURE AnyType_To_ChrBytes_Array3D
    END INTERFACE
    INTERFACE AnyType2IntBytes
        MODULE PROCEDURE AnyType_To_IntBytes_Scalar
        MODULE PROCEDURE AnyType_To_IntBytes_Array1D
        MODULE PROCEDURE AnyType_To_IntBytes_Array2D
        MODULE PROCEDURE AnyType_To_IntBytes_Array3D
    END INTERFACE
    INTERFACE Intr2ChrBytes
        MODULE PROCEDURE Intrinsic_To_ChrBytes_Scalar
        MODULE PROCEDURE Intrinsic_To_ChrBytes_Array1D
        MODULE PROCEDURE Intrinsic_To_ChrBytes_Array2D
        MODULE PROCEDURE Intrinsic_To_ChrBytes_Array3D
    END INTERFACE
    INTERFACE Intr2IntBytes
        MODULE PROCEDURE Intrinsic_To_IntBytes_Scalar
        MODULE PROCEDURE Intrinsic_To_IntBytes_Array1D
        MODULE PROCEDURE Intrinsic_To_IntBytes_Array2D
        MODULE PROCEDURE Intrinsic_To_IntBytes_Array3D
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           COPY ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Poly_Copy_Data(OutData,InData,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a copy of data

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(OUT)   :: OutData          ! output
    CLASS(*), INTENT(IN)    :: InData           ! input
    PROCEDURE(CopyData)     :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutData, InData)) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Data', ModName, ErrSevere, &
                            'Types of input and output data are NOT compatible.')
        RETURN
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! copy using user-supplied routine
        ErrStat = CopyUserData(OutData, InData)
        RETURN
    END IF

    ! then, set ErrStat
    ErrStat = FalseVal

    ! finally, copy data if their types are intrinsic/assignable
    SELECT TYPE (OutData)
    CLASS IS (Assignable)
        SELECT TYPE (InData)
        CLASS IS (Assignable)
            OutData = InData
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (InData)
        TYPE IS (tByte)
            OutData = InData
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (InData)
        TYPE IS (tShort)
            OutData = InData
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (InData)
        TYPE IS (tInteger)
            OutData = InData
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (InData)
        TYPE IS (tLong)
            OutData = InData
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (InData)
        TYPE IS (tSingle)
            OutData = InData
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (InData)
        TYPE IS (tDouble)
            OutData = InData
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (InData)
        TYPE IS (tQuad)
            OutData = InData
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (InData)
        TYPE IS (tCmpxSingle)
            OutData = InData
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (InData)
        TYPE IS (tCmpxDouble)
            OutData = InData
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (InData)
        TYPE IS (tCmpxQuad)
            OutData = InData
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (InData)
        TYPE IS (tLogical)
            OutData = InData
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (InData)
        TYPE IS (tCharStar)
            OutData = InData
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Data', ModName, ErrSevere, &
                    'No user-supplied routine provided but types of data are NOT "assignable/copyable".')
    END SELECT

    RETURN

END FUNCTION Poly_Copy_Data

!******************************************************************************

FUNCTION Poly_Copy_Array1D(OutData,InData,N,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a copy of data

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N            ! size of arrays
    CLASS(*), INTENT(OUT)   :: OutData(N)   ! output
    CLASS(*), INTENT(IN)    :: InData(N)    ! input
    PROCEDURE(CopyArray1D)  :: CopyUserData ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat      ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutData(1), InData(1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array1D', ModName, ErrSevere, &
                            'Types of input and output data are NOT compatible.')
        RETURN
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! copy using user-supplied routine
        ErrStat = CopyUserData(OutData, InData, N)
        RETURN
    END IF

    ! then, set ErrStat
    ErrStat = FalseVal

    ! finally, copy data if their types are intrinsic/assignable
    SELECT TYPE (OutData)
    CLASS IS (Assignable)
        SELECT TYPE (InData)
        CLASS IS (Assignable)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (InData)
        TYPE IS (tByte)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (InData)
        TYPE IS (tShort)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (InData)
        TYPE IS (tInteger)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (InData)
        TYPE IS (tLong)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (InData)
        TYPE IS (tSingle)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (InData)
        TYPE IS (tDouble)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (InData)
        TYPE IS (tQuad)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (InData)
        TYPE IS (tCmpxSingle)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (InData)
        TYPE IS (tCmpxDouble)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (InData)
        TYPE IS (tCmpxQuad)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (InData)
        TYPE IS (tLogical)
            OutData(1:N) = InData(1:N)
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (InData)
        TYPE IS (tCharStar)
            OutData(1:N) = InData(1:N)
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array1D', ModName, ErrSevere, &
                    'No user-supplied routine provided but types of data are NOT "assignable/copyable".')
    END SELECT

    RETURN

END FUNCTION Poly_Copy_Array1D

!******************************************************************************

FUNCTION Poly_Copy_Array2D(OutData,InData,N1,N2,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a copy of data

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N1               ! first dimension of arrays
    tIndex,   INTENT(IN)    :: N2               ! second dimension of arrays
    CLASS(*), INTENT(OUT)   :: OutData(N1,N2)   ! output
    CLASS(*), INTENT(IN)    :: InData(N1,N2)    ! input
    PROCEDURE(CopyArray2D)  :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutData(1,1), InData(1,1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array2D', ModName, ErrSevere, &
                            'Types of input and output data are NOT compatible.')
        RETURN
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! copy using user-supplied routine
        ErrStat = CopyUserData(OutData, InData, N1, N2)
        RETURN
    END IF

    ! then, set ErrStat
    ErrStat = FalseVal

    ! finally, copy data if their types are intrinsic/assignable
    SELECT TYPE (OutData)
    CLASS IS (Assignable)
        SELECT TYPE (InData)
        CLASS IS (Assignable)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (InData)
        TYPE IS (tByte)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (InData)
        TYPE IS (tShort)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (InData)
        TYPE IS (tInteger)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (InData)
        TYPE IS (tLong)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (InData)
        TYPE IS (tSingle)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (InData)
        TYPE IS (tDouble)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (InData)
        TYPE IS (tQuad)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (InData)
        TYPE IS (tCmpxSingle)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (InData)
        TYPE IS (tCmpxDouble)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (InData)
        TYPE IS (tCmpxQuad)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (InData)
        TYPE IS (tLogical)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (InData)
        TYPE IS (tCharStar)
            OutData(1:N1,1:N2) = InData(1:N1,1:N2)
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array2D', ModName, ErrSevere, &
                    'No user-supplied routine provided but types of data are NOT "assignable/copyable".')
    END SELECT

    RETURN

END FUNCTION Poly_Copy_Array2D

!******************************************************************************

FUNCTION Poly_Copy_Array3D(OutData,InData,N1,N2,N3,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To make a copy of data

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N1                   ! first dimension of arrays
    tIndex,   INTENT(IN)    :: N2                   ! second dimension of arrays
    tIndex,   INTENT(IN)    :: N3                   ! third dimension of arrays
    CLASS(*), INTENT(OUT)   :: OutData(N1,N2,N3)    ! output
    CLASS(*), INTENT(IN)    :: InData(N1,N2,N3)     ! input
    PROCEDURE(CopyArray3D)  :: CopyUserData         ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat              ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! first, check input data
    IF (.NOT.SAME_TYPE_AS(OutData(1,1,1), InData(1,1,1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array3D', ModName, ErrSevere, &
                            'Types of input and output data are NOT compatible.')
        RETURN
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! copy using user-supplied routine
        ErrStat = CopyUserData(OutData, InData, N1, N2, N3)
        RETURN
    END IF

    ! then, set ErrStat
    ErrStat = FalseVal

    ! finally, copy data if their types are intrinsic/assignable
    SELECT TYPE (OutData)
    CLASS IS (Assignable)
        SELECT TYPE (InData)
        CLASS IS (Assignable)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (InData)
        TYPE IS (tByte)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (InData)
        TYPE IS (tShort)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (InData)
        TYPE IS (tInteger)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (InData)
        TYPE IS (tLong)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (InData)
        TYPE IS (tSingle)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (InData)
        TYPE IS (tDouble)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (InData)
        TYPE IS (tQuad)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (InData)
        TYPE IS (tCmpxSingle)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (InData)
        TYPE IS (tCmpxDouble)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (InData)
        TYPE IS (tCmpxQuad)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (InData)
        TYPE IS (tLogical)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (InData)
        TYPE IS (tCharStar)
            OutData(1:N1,1:N2,1:N3) = InData(1:N1,1:N2,1:N3)
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Copy_Array3D', ModName, ErrSevere, &
                    'No user-supplied routine provided but types of data are NOT "assignable/copyable".')
    END SELECT

    RETURN

END FUNCTION Poly_Copy_Array3D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           SWAP ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Poly_Swap_Scalar(DataA,DataB,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform swapping operation of two data

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(INOUT) :: DataA            ! data A
    CLASS(*), INTENT(INOUT) :: DataB            ! data B
    PROCEDURE(CopyData)     :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(*), ALLOCATABLE   :: Temp

! FLOW

    ! first, set default
    ErrStat = FalseVal

    ! next, check input data
    IF (.NOT.SAME_TYPE_AS(DataA, DataB)) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Scalar', ModName, ErrSevere, &
                            'Types of input data are NOT compatible.')
        RETURN
    ELSE
        ! allocate Temp and make it a copy of DataA
        ALLOCATE(Temp, SOURCE=DataA)
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! use user-supplied routine to swap values
        ErrStat = CopyUserData(DataA, DataB)
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('Poly_Swap_Scalar', ModName, ErrSevere, &
                        'There is an error in the user-supplied routine.')
        ELSE
            ErrStat = CopyUserData(DataB, Temp)
        END IF
        ! free up memory
        DEALLOCATE(Temp)
        RETURN
    END IF

    ! finally, swap data if their types are intrinsic/assignable
    SELECT TYPE (DataA)
    CLASS IS (Assignable)
        SELECT TYPE (DataB)
        CLASS IS (Assignable)
            SELECT TYPE (Temp)
            CLASS IS (Assignable)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (DataB)
        TYPE IS (tByte)
            SELECT TYPE (Temp)
            TYPE IS (tByte)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (DataB)
        TYPE IS (tShort)
            SELECT TYPE (Temp)
            TYPE IS (tShort)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (DataB)
        TYPE IS (tInteger)
            SELECT TYPE (Temp)
            TYPE IS (tInteger)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (DataB)
        TYPE IS (tLong)
            SELECT TYPE (Temp)
            TYPE IS (tLong)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (DataB)
        TYPE IS (tSingle)
            SELECT TYPE (Temp)
            TYPE IS (tSingle)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (DataB)
        TYPE IS (tDouble)
            SELECT TYPE (Temp)
            TYPE IS (tDouble)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (DataB)
        TYPE IS (tQuad)
            SELECT TYPE (Temp)
            TYPE IS (tQuad)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxSingle)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxSingle)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxDouble)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxDouble)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxQuad)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxQuad)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (DataB)
        TYPE IS (tLogical)
            SELECT TYPE (Temp)
            TYPE IS (tLogical)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (DataB)
        TYPE IS (tCharStar)
            SELECT TYPE (Temp)
            TYPE IS (tCharStar)
                DataA = DataB
                DataB = Temp
            END SELECT
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Scalar', ModName, ErrSevere, &
                    'Types of input data are NOT "assignable/copyable".')
    END SELECT

    ! free up memory
    DEALLOCATE(Temp)

    RETURN

END FUNCTION Poly_Swap_Scalar

!******************************************************************************

FUNCTION Poly_Swap_Array1D(DataA,DataB,N,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform swapping operation of two data arrays

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N                ! size of arrays
    CLASS(*), INTENT(INOUT) :: DataA(N)         ! data A
    CLASS(*), INTENT(INOUT) :: DataB(N)         ! data B
    PROCEDURE(CopyArray1D)  :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(*), ALLOCATABLE   :: Temp(:)
    tIndex                  :: I

! FLOW

    ! first, set default
    ErrStat = FalseVal

    ! next, check input data
    IF (.NOT.SAME_TYPE_AS(DataA(1), DataB(1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array1D', ModName, ErrSevere, &
                            'Types of input data are NOT compatible.')
        RETURN
    ELSE
        ! allocate Temp and make it a copy of DataA
        ALLOCATE(Temp, SOURCE=DataA)
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! use user-supplied routine to swap values
        ErrStat = CopyUserData(DataA, DataB, N)
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('Poly_Swap_Array1D', ModName, ErrSevere, &
                        'There is an error in the user-supplied routine.')
        ELSE
            ErrStat = CopyUserData(DataB, Temp, N)
        END IF
        ! free up memory
        DEALLOCATE(Temp)
        RETURN
    END IF

    ! finally, swap data if their types are intrinsic/assignable
    SELECT TYPE (DataA)
    CLASS IS (Assignable)
        SELECT TYPE (DataB)
        CLASS IS (Assignable)
            SELECT TYPE (Temp)
            CLASS IS (Assignable)
                DO I = 1, N
                    DataA(I) = DataB(I)
                    DataB(I) = Temp(I)
                END DO
            END SELECT
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (DataB)
        TYPE IS (tByte)
            SELECT TYPE (Temp)
            TYPE IS (tByte)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (DataB)
        TYPE IS (tShort)
            SELECT TYPE (Temp)
            TYPE IS (tShort)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (DataB)
        TYPE IS (tInteger)
            SELECT TYPE (Temp)
            TYPE IS (tInteger)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (DataB)
        TYPE IS (tLong)
            SELECT TYPE (Temp)
            TYPE IS (tLong)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (DataB)
        TYPE IS (tSingle)
            SELECT TYPE (Temp)
            TYPE IS (tSingle)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (DataB)
        TYPE IS (tDouble)
            SELECT TYPE (Temp)
            TYPE IS (tDouble)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (DataB)
        TYPE IS (tQuad)
            SELECT TYPE (Temp)
            TYPE IS (tQuad)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxSingle)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxSingle)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxDouble)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxDouble)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxQuad)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxQuad)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (DataB)
        TYPE IS (tLogical)
            SELECT TYPE (Temp)
            TYPE IS (tLogical)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (DataB)
        TYPE IS (tCharStar)
            SELECT TYPE (Temp)
            TYPE IS (tCharStar)
                DataA(1:N) = DataB(1:N)
                DataB(1:N) = Temp(1:N)
            END SELECT
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array1D', ModName, ErrSevere, &
                    'Types of input data are NOT "assignable/copyable".')
    END SELECT

    ! free up memory
    DEALLOCATE(Temp)

    RETURN

END FUNCTION Poly_Swap_Array1D

!******************************************************************************

FUNCTION Poly_Swap_Array2D(DataA,DataB,N1,N2,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform swapping operation of two data arrays

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N1               ! first dimension of arrays
    tIndex,   INTENT(IN)    :: N2               ! second dimension of arrays
    CLASS(*), INTENT(INOUT) :: DataA(N1,N2)     ! data A
    CLASS(*), INTENT(INOUT) :: DataB(N1,N2)     ! data B
    PROCEDURE(CopyArray2D)  :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(*), ALLOCATABLE   :: Temp(:,:)
    tIndex                  :: I, J

! FLOW

    ! first, set default
    ErrStat = FalseVal

    ! next, check input data
    IF (.NOT.SAME_TYPE_AS(DataA(1,1), DataB(1,1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array2D', ModName, ErrSevere, &
                            'Types of input data are NOT compatible.')
        RETURN
    ELSE
        ! allocate Temp and make it a copy of DataA
        ALLOCATE(Temp, SOURCE=DataA)
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! use user-supplied routine to swap values
        ErrStat = CopyUserData(DataA, DataB, N1, N2)
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('Poly_Swap_Array2D', ModName, ErrSevere, &
                        'There is an error in the user-supplied routine.')
        ELSE
            ErrStat = CopyUserData(DataB, Temp, N1, N2)
        END IF
        ! free up memory
        DEALLOCATE(Temp)
        RETURN
    END IF

    ! finally, swap data if their types are intrinsic/assignable
    SELECT TYPE (DataA)
    CLASS IS (Assignable)
        SELECT TYPE (DataB)
        CLASS IS (Assignable)
            SELECT TYPE (Temp)
            CLASS IS (Assignable)
                DO I = 1, N1
                    DO J = 1, N2
                        DataA(I,J) = DataB(I,J)
                        DataB(I,J) = Temp(I,J)
                    END DO
                END DO
            END SELECT
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (DataB)
        TYPE IS (tByte)
            SELECT TYPE (Temp)
            TYPE IS (tByte)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (DataB)
        TYPE IS (tShort)
            SELECT TYPE (Temp)
            TYPE IS (tShort)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (DataB)
        TYPE IS (tInteger)
            SELECT TYPE (Temp)
            TYPE IS (tInteger)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (DataB)
        TYPE IS (tLong)
            SELECT TYPE (Temp)
            TYPE IS (tLong)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (DataB)
        TYPE IS (tSingle)
            SELECT TYPE (Temp)
            TYPE IS (tSingle)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (DataB)
        TYPE IS (tDouble)
            SELECT TYPE (Temp)
            TYPE IS (tDouble)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (DataB)
        TYPE IS (tQuad)
            SELECT TYPE (Temp)
            TYPE IS (tQuad)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxSingle)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxSingle)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxDouble)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxDouble)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxQuad)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxQuad)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (DataB)
        TYPE IS (tLogical)
            SELECT TYPE (Temp)
            TYPE IS (tLogical)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (DataB)
        TYPE IS (tCharStar)
            SELECT TYPE (Temp)
            TYPE IS (tCharStar)
                DataA(1:N1,1:N2) = DataB(1:N1,1:N2)
                DataB(1:N1,1:N2) = Temp(1:N1,1:N2)
            END SELECT
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array2D', ModName, ErrSevere, &
                    'Types of input data are NOT "assignable/copyable".')
    END SELECT

    ! free up memory
    DEALLOCATE(Temp)

    RETURN

END FUNCTION Poly_Swap_Array2D

!******************************************************************************

FUNCTION Poly_Swap_Array3D(DataA,DataB,N1,N2,N3,CopyUserData) RESULT(ErrStat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform swapping operation of two data arrays

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N1               ! first dimension of arrays
    tIndex,   INTENT(IN)    :: N2               ! second dimension of arrays
    tIndex,   INTENT(IN)    :: N3               ! third dimension of arrays
    CLASS(*), INTENT(INOUT) :: DataA(N1,N2,N3)  ! data A
    CLASS(*), INTENT(INOUT) :: DataB(N1,N2,N3)  ! data B
    PROCEDURE(CopyArray3D)  :: CopyUserData     ! user-supplied routine
    OPTIONAL                :: CopyUserData
    tLogical                :: ErrStat          ! error status

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    CLASS(*), ALLOCATABLE   :: Temp(:,:,:)
    tIndex                  :: I, J, K

! FLOW

    ! first, set default
    ErrStat = FalseVal

    ! next, check input data
    IF (.NOT.SAME_TYPE_AS(DataA(1,1,1), DataB(1,1,1))) THEN
        ! report error
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array3D', ModName, ErrSevere, &
                            'Types of input data are NOT compatible.')
        RETURN
    ELSE
        ! allocate Temp and make it a copy of DataA
        ALLOCATE(Temp, SOURCE=DataA)
    END IF

    ! next, check optional input
    IF (PRESENT(CopyUserData)) THEN
        ! use user-supplied routine to swap values
        ErrStat = CopyUserData(DataA, DataB, N1, N2, N3)
        IF (ErrStat) THEN
            CALL Handle_ErrLevel('Poly_Swap_Array3D', ModName, ErrSevere, &
                        'There is an error in the user-supplied routine.')
        ELSE
            ErrStat = CopyUserData(DataB, Temp, N1, N2, N3)
        END IF
        ! free up memory
        DEALLOCATE(Temp)
        RETURN
    END IF

    ! finally, swap data if their types are intrinsic/assignable
    SELECT TYPE (DataA)
    CLASS IS (Assignable)
        SELECT TYPE (DataB)
        CLASS IS (Assignable)
            SELECT TYPE (Temp)
            CLASS IS (Assignable)
                DO I = 1, N1
                    DO J = 1, N2
                        DO K = 1, N3
                            DataA(I,J,K) = DataB(I,J,K)
                            DataB(I,J,K) = Temp(I,J,K)
                        END DO
                    END DO
                END DO
            END SELECT
        END SELECT
    TYPE IS (tByte)
        SELECT TYPE (DataB)
        TYPE IS (tByte)
            SELECT TYPE (Temp)
            TYPE IS (tByte)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tShort)
        SELECT TYPE (DataB)
        TYPE IS (tShort)
            SELECT TYPE (Temp)
            TYPE IS (tShort)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tInteger)
        SELECT TYPE (DataB)
        TYPE IS (tInteger)
            SELECT TYPE (Temp)
            TYPE IS (tInteger)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tLong)
        SELECT TYPE (DataB)
        TYPE IS (tLong)
            SELECT TYPE (Temp)
            TYPE IS (tLong)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tSingle)
        SELECT TYPE (DataB)
        TYPE IS (tSingle)
            SELECT TYPE (Temp)
            TYPE IS (tSingle)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tDouble)
        SELECT TYPE (DataB)
        TYPE IS (tDouble)
            SELECT TYPE (Temp)
            TYPE IS (tDouble)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tQuad)
        SELECT TYPE (DataB)
        TYPE IS (tQuad)
            SELECT TYPE (Temp)
            TYPE IS (tQuad)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tCmpxSingle)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxSingle)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxSingle)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tCmpxDouble)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxDouble)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxDouble)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tCmpxQuad)
        SELECT TYPE (DataB)
        TYPE IS (tCmpxQuad)
            SELECT TYPE (Temp)
            TYPE IS (tCmpxQuad)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tLogical)
        SELECT TYPE (DataB)
        TYPE IS (tLogical)
            SELECT TYPE (Temp)
            TYPE IS (tLogical)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (DataB)
        TYPE IS (tCharStar)
            SELECT TYPE (Temp)
            TYPE IS (tCharStar)
                DataA(1:N1,1:N2,1:N3) = DataB(1:N1,1:N2,1:N3)
                DataB(1:N1,1:N2,1:N3) = Temp(1:N1,1:N2,1:N3)
            END SELECT
        END SELECT
    CLASS DEFAULT
        ErrStat = TrueVal
        CALL Handle_ErrLevel('Poly_Swap_Array3D', ModName, ErrSevere, &
                    'Types of input data are NOT "assignable/copyable".')
    END SELECT

    ! free up memory
    DEALLOCATE(Temp)

    RETURN

END FUNCTION Poly_Swap_Array3D

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                           COMPARE ROUTINES
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Poly_Compare(A,OP,B,CompareUserData) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE
    ! To compare A and B

    IMPLICIT NONE            ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*),    INTENT(IN) :: A                ! A
    tCharLen(2), INTENT(IN) :: OP               ! character specifying relational operator
    CLASS(*),    INTENT(IN) :: B                ! B
    PROCEDURE(CompareData)  :: CompareUserData  ! user-supplied routine
    OPTIONAL                :: CompareUserData
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW:

    ! check whether the input types are the same or not
    IF (.NOT.SAME_TYPE_AS(A, B)) THEN
        Flag = FalseVal
        RETURN
    END IF

! define macro for generic data comparison
#define COMPARE_DATA(Flag,LHS,OP,RHS) \
SELECT CASE (OP); \
CASE ('EQ'); \
    Flag = (LHS == RHS); \
CASE ('NE'); \
    Flag = (LHS /= RHS); \
CASE ('GT'); \
    Flag = (LHS > RHS); \
CASE ('GE'); \
    Flag = (LHS >= RHS); \
CASE ('LT'); \
    Flag = (LHS < RHS); \
CASE ('LE'); \
    Flag = (LHS <= RHS); \
END SELECT;
#define COMPARE_COMPLEX_DATA(Flag,LHS,OP,RHS) \
SELECT CASE (OP); \
CASE ('EQ'); \
    Flag = (A .EQ. B); \
CASE ('NE'); \
    Flag = (A .NE. B); \
CASE ('GT'); \
    Flag = (ABS(A) .GT. ABS(B)); \
CASE ('GE'); \
    Flag = (ABS(A) .GE. ABS(B)); \
CASE ('LT'); \
    Flag = (ABS(A) .LT. ABS(B)); \
CASE ('LE'); \
    Flag = (ABS(A) .LE. ABS(B)); \
END SELECT;

    ! next, check optional input
    IF (PRESENT(CompareUserData)) THEN
        COMPARE_DATA(Flag, CompareUserData(A, B), OP, 0)
        RETURN
    END IF

    ! finally, compare data if their types are intrinsic/comparable
    SELECT TYPE (A)
    CLASS IS (Comparable)
        SELECT TYPE (B)
        CLASS IS (Comparable)
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
!    CLASS IS (INTEGER)
!        SELECT TYPE (B)
!        CLASS IS (INTEGER)
!            COMPARE_DATA(Flag, A, OP, B)
!        END SELECT
    TYPE IS (INTEGER(kI1B))
        SELECT TYPE (B)
        TYPE IS (INTEGER(kI1B))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (INTEGER(kI2B))
        SELECT TYPE (B)
        TYPE IS (INTEGER(kI2B))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (INTEGER(kI4B))
        SELECT TYPE (B)
        TYPE IS (INTEGER(kI4B))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (INTEGER(kI8B))
        SELECT TYPE (B)
        TYPE IS (INTEGER(kI8B))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
!    CLASS IS (REAL)
!        SELECT TYPE (B)
!        CLASS IS (REAL)
!            COMPARE_DATA(Flag, A, OP, B)
!        END SELECT
    TYPE IS (REAL(kSP))
        SELECT TYPE (B)
        TYPE IS (REAL(kSP))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (REAL(kDP))
        SELECT TYPE (B)
        TYPE IS (REAL(kDP))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (REAL(kQP))
        SELECT TYPE (B)
        TYPE IS (REAL(kQP))
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
!    CLASS IS (COMPLEX)
!        SELECT TYPE (B)
!        CLASS IS (COMPLEX)
!            COMPARE_COMPLEX_DATA(Flag, A, OP, B)
!        END SELECT
    TYPE IS (COMPLEX(kSP))
        SELECT TYPE (B)
        TYPE IS (COMPLEX(kSP))
            COMPARE_COMPLEX_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (COMPLEX(kDP))
        SELECT TYPE (B)
        TYPE IS (COMPLEX(kDP))
            COMPARE_COMPLEX_DATA(Flag, A, OP, B)
        END SELECT
    TYPE IS (COMPLEX(kQP))
        SELECT TYPE (B)
        TYPE IS (COMPLEX(kQP))
            COMPARE_COMPLEX_DATA(Flag, A, OP, B)
        END SELECT
!    CLASS IS (tCharStar)
!        SELECT TYPE (B)
!        CLASS IS (tCharStar)
!            COMPARE_DATA(Flag, A, OP, B)
!        END SELECT
    TYPE IS (tCharStar)
        SELECT TYPE (B)
        TYPE IS (tCharStar)
            COMPARE_DATA(Flag, A, OP, B)
        END SELECT
    CLASS DEFAULT
        ! set flag to false for "incomparable" class
        Flag = FalseVal
    END SELECT
#undef COMPARE_DATA
#undef COMPARE_COMPLEX_DATA
    RETURN

END FUNCTION Poly_Compare

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                     ANYTYPE_TO_BYTES FUNCTIONS
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION AnyType_To_ChrBytes_Scalar(Value) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a scalar data of any type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(' ')))
    ALLOCATE(ChrBytes(ByteSize))

    ! transfer data
    ChrBytes = TRANSFER(Value, ChrBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_ChrBytes_Scalar

!******************************************************************************

FUNCTION AnyType_To_ChrBytes_Array1D(Value) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 1-D array of any type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(' ')))
    ALLOCATE(ChrBytes(ByteSize))

    ! transfer data
    ChrBytes = TRANSFER(Value, ChrBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_ChrBytes_Array1D

!******************************************************************************

FUNCTION AnyType_To_ChrBytes_Array2D(Value) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 2-D array of any type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:,:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(' ')))
    ALLOCATE(ChrBytes(ByteSize))

    ! transfer data
    ChrBytes = TRANSFER(Value, ChrBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_ChrBytes_Array2D

!******************************************************************************

FUNCTION AnyType_To_ChrBytes_Array3D(Value) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 3-D array of any type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:,:,:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(' ')))
    ALLOCATE(ChrBytes(ByteSize))

    ! transfer data
    ChrBytes = TRANSFER(Value, ChrBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_ChrBytes_Array3D

!******************************************************************************

FUNCTION AnyType_To_IntBytes_Scalar(Value) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a scalar data of any type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    IntBytes = TRANSFER(Value, IntBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_IntBytes_Scalar

!******************************************************************************

FUNCTION AnyType_To_IntBytes_Array1D(Value) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 1-D array of any type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    IntBytes = TRANSFER(Value, IntBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_IntBytes_Array1D

!******************************************************************************

FUNCTION AnyType_To_IntBytes_Array2D(Value) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 2-D array of any type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:,:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    IntBytes = TRANSFER(Value, IntBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_IntBytes_Array2D

!******************************************************************************

FUNCTION AnyType_To_IntBytes_Array3D(Value) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 3-D array of any type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Value(:,:,:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Value))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    IntBytes = TRANSFER(Value, IntBytes, ByteSize)

    RETURN

END FUNCTION AnyType_To_IntBytes_Array3D


!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   INTRINSIC_TO_BYTES FUNCTIONS                              +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION Intrinsic_To_ChrBytes_Scalar(Val) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a scalar data of any intrinsic type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE(Val)
    TYPE IS (tByte)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tShort)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tInteger)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLong)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCharStar)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLogical)
        ChrBytes = TRANSFER(Val, ChrBytes)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_ChrBytes_Scalar', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_ChrBytes_Scalar

!******************************************************************************

FUNCTION Intrinsic_To_ChrBytes_Array1D(Val) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 1-D array of any intrinsic type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE(Val)
    TYPE IS (tByte)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tShort)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tInteger)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLong)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCharStar)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLogical)
        ChrBytes = TRANSFER(Val, ChrBytes)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_ChrBytes_Array1D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_ChrBytes_Array1D

!******************************************************************************

FUNCTION Intrinsic_To_ChrBytes_Array2D(Val) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 2-D array of any intrinsic type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:,:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE(Val)
    TYPE IS (tByte)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tShort)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tInteger)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLong)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCharStar)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLogical)
        ChrBytes = TRANSFER(Val, ChrBytes)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_ChrBytes_Array2D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_ChrBytes_Array2D

!******************************************************************************

FUNCTION Intrinsic_To_ChrBytes_Array3D(Val) RESULT(ChrBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 3-D array of any intrinsic type into character bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:,:,:)
    tChar, ALLOCATABLE      :: ChrBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT TYPE(Val)
    TYPE IS (tByte)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tShort)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tInteger)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLong)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxSingle)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxDouble)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCmpxQuad)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tCharStar)
        ChrBytes = TRANSFER(Val, ChrBytes)
    TYPE IS (tLogical)
        ChrBytes = TRANSFER(Val, ChrBytes)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_ChrBytes_Array3D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_ChrBytes_Array3D

!******************************************************************************

FUNCTION Intrinsic_To_IntBytes_Scalar(Val) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a scalar data of any intrinsic type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Val))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    SELECT TYPE(Val)
    TYPE IS (tByte)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tShort)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tInteger)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLong)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCharStar)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLogical)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_IntBytes_Scalar', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_IntBytes_Scalar

!******************************************************************************

FUNCTION Intrinsic_To_IntBytes_Array1D(Val) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 1-D array of any intrinsic type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Val))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    SELECT TYPE(Val)
    TYPE IS (tByte)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tShort)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tInteger)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLong)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCharStar)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLogical)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_IntBytes_Array1D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_IntBytes_Array1D

!******************************************************************************

FUNCTION Intrinsic_To_IntBytes_Array2D(Val) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 2-D array of any intrinsic type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:,:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Val))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    SELECT TYPE(Val)
    TYPE IS (tByte)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tShort)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tInteger)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLong)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCharStar)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLogical)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_IntBytes_Array2D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_IntBytes_Array2D

!******************************************************************************

FUNCTION Intrinsic_To_IntBytes_Array3D(Val) RESULT(IntBytes)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 3-D array of any intrinsic type into integer bytes

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(*), INTENT(IN)    :: Val(:,:,:)
    tByte, ALLOCATABLE      :: IntBytes(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: ByteSize

!** FLOW

    ! determine byte size and allocate the output
    ByteSize = CEILING(DBLE(STORAGE_SIZE(Val))/DBLE(STORAGE_SIZE(0_kI1B)))
    CALL MemAlloc(IntBytes, ByteSize)

    ! transfer data
    SELECT TYPE(Val)
    TYPE IS (tByte)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tShort)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tInteger)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLong)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxSingle)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxDouble)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCmpxQuad)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tCharStar)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    TYPE IS (tLogical)
        IntBytes = TRANSFER(Val, IntBytes, ByteSize)
    CLASS DEFAULT
        CALL Handle_ErrLevel('Intrinsic_To_IntBytes_Array3D', ModName, ErrSevere, &
                    'Type of input is NOT valid.')
    END SELECT

    RETURN

END FUNCTION Intrinsic_To_IntBytes_Array3D

!******************************************************************************

END MODULE ModBase_PolyCommon

!******************************************************************************

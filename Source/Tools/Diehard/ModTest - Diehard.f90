
MODULE ModTest_Diehard

    !** PURPOSE OF THIS MODULE:
        ! contains routines to test pseudo random number generators

    !** USE STATEMENTS:
    USE ModBase_Common
    USE ModBase_Ranking
    USE ModBase_SortAscend
    USE Class_BaseRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: Diehard_Tests
    PUBLIC :: Diehard_Tests_Direct
    PUBLIC :: Diehard_Tests_FromFile

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'ModTest_Diehard'

!** DERIVED TYPE DEFINITIONS
    TYPE I8B_Data
        CLASS(BaseRNG), POINTER :: RNG => NULL()
        tInteger                :: NLeft   = 0
        tInteger                :: CurrentSample
    CONTAINS
        PROCEDURE   :: GetSample    => Read_Byte_FromRNG
    END TYPE I8B_Data
    TYPE I8Data
        tInteger    :: FileUnit
        tInteger    :: Sample(4096)
        tInteger    :: Counter = 4097
        tInteger    :: RecNum  = 1
        tInteger    :: NLeft   = 0
        tInteger    :: CurrentSample
    CONTAINS
        PROCEDURE   :: GetSample    => Read_Byte_FromFile
    END TYPE I8Data
    TYPE I32Data
        tInteger    :: FileUnit
        tInteger    :: Sample(4096)
        tInteger    :: Counter = 4097
        tInteger    :: RecNum  = 1
    CONTAINS
        PROCEDURE   :: GetSample    => Read_Integer_FromFile
    END TYPE I32Data

!** INTERFACE DEFINITIONS
    INTERFACE
        MODULE SUBROUTINE FT01_BirthdaySpacing_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT01_BirthdaySpacing_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT02_Overlap5Permute_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT02_Overlap5Permute_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT03_BinaryRank_3132_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT03_BinaryRank_3132_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT04_BinaryRank_6x8_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT04_BinaryRank_6x8_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT05_BitStream_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT05_BitStream_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT06_OPSO_OQSO_DNA_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT06_OPSO_OQSO_DNA_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT07_CountThe1s_Stream_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),  INTENT(IN)       :: InpFileName
            tInteger,      INTENT(IN)       :: OutUnit
            CLASS(I8Data), INTENT(INOUT)    :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT07_CountThe1s_Stream_FromRNG(OutUnit, RNG)
            tInteger,               INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), TARGET, INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT08_CountThe1s_SpecificBytes_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT08_CountThe1s_SpecificBytes_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT09_ParkingLot_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT09_ParkingLot_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT10_MinDistance_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT10_MinDistance_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT11_3DSphere_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT11_3DSphere_FromRNG(OutUnit, RNG)
            tInteger,           INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT12_Squeeze_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT12_Squeeze_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT13_OverlapSum_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT13_OverlapSum_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT14_Runs_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT14_Runs_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE
    INTERFACE
        MODULE SUBROUTINE FT15_Craps_FromFile(InpFileName, OutUnit, InpRec)
            tCharLen(25),   INTENT(IN)      :: InpFileName
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(I32Data), INTENT(INOUT)   :: InpRec
        END SUBROUTINE
        MODULE SUBROUTINE FT15_Craps_FromRNG(OutUnit, RNG)
            tInteger,       INTENT(IN)      :: OutUnit
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
    END INTERFACE

!** GENERIC DECLARATIONS:
    INTERFACE GetSample
        MODULE PROCEDURE Read_Integer_FromRNG
    END INTERFACE
    INTERFACE Uni
        MODULE PROCEDURE Uni_FromRNG
        MODULE PROCEDURE Uni_FromFile
    END INTERFACE
    ! diehard tests
    INTERFACE FT01_BirthdaySpacing
        MODULE PROCEDURE FT01_BirthdaySpacing_FromRNG
        MODULE PROCEDURE FT01_BirthdaySpacing_FromFile
    END INTERFACE
    INTERFACE FT02_Overlap5Permute
        MODULE PROCEDURE FT02_Overlap5Permute_FromRNG
        MODULE PROCEDURE FT02_Overlap5Permute_FromFile
    END INTERFACE
    INTERFACE FT03_BinaryRank_3132
        MODULE PROCEDURE FT03_BinaryRank_3132_FromRNG
        MODULE PROCEDURE FT03_BinaryRank_3132_FromFile
    END INTERFACE
    INTERFACE FT04_BinaryRank_6x8
        MODULE PROCEDURE FT04_BinaryRank_6x8_FromRNG
        MODULE PROCEDURE FT04_BinaryRank_6x8_FromFile
    END INTERFACE
    INTERFACE FT05_BitStream
        MODULE PROCEDURE FT05_BitStream_FromRNG
        MODULE PROCEDURE FT05_BitStream_FromFile
    END INTERFACE
    INTERFACE FT06_OPSO_OQSO_DNA
        MODULE PROCEDURE FT06_OPSO_OQSO_DNA_FromRNG
        MODULE PROCEDURE FT06_OPSO_OQSO_DNA_FromFile
    END INTERFACE
    INTERFACE FT07_CountThe1s_Stream
        MODULE PROCEDURE FT07_CountThe1s_Stream_FromRNG
        MODULE PROCEDURE FT07_CountThe1s_Stream_FromFile
    END INTERFACE
    INTERFACE FT08_CountThe1s_SpecificBytes
        MODULE PROCEDURE FT08_CountThe1s_SpecificBytes_FromRNG
        MODULE PROCEDURE FT08_CountThe1s_SpecificBytes_FromFile
    END INTERFACE
    INTERFACE FT09_ParkingLot
        MODULE PROCEDURE FT09_ParkingLot_FromRNG
        MODULE PROCEDURE FT09_ParkingLot_FromFile
    END INTERFACE
    INTERFACE FT10_MinDistance
        MODULE PROCEDURE FT10_MinDistance_FromRNG
        MODULE PROCEDURE FT10_MinDistance_FromFile
    END INTERFACE
    INTERFACE FT11_3DSphere
        MODULE PROCEDURE FT11_3DSphere_FromRNG
        MODULE PROCEDURE FT11_3DSphere_FromFile
    END INTERFACE
    INTERFACE FT12_Squeeze
        MODULE PROCEDURE FT12_Squeeze_FromRNG
        MODULE PROCEDURE FT12_Squeeze_FromFile
    END INTERFACE
    INTERFACE FT13_OverlapSum
        MODULE PROCEDURE FT13_OverlapSum_FromRNG
        MODULE PROCEDURE FT13_OverlapSum_FromFile
    END INTERFACE
    INTERFACE FT14_Runs
        MODULE PROCEDURE FT14_Runs_FromRNG
        MODULE PROCEDURE FT14_Runs_FromFile
    END INTERFACE
    INTERFACE FT15_Craps
        MODULE PROCEDURE FT15_Craps_FromRNG
        MODULE PROCEDURE FT15_Craps_FromFile
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Main Interface Routine
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Diehard_Tests(RNG, FromFile)

    ! argument
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG
    tLogical, OPTIONAL, INTENT(IN)      :: FromFile

    ! local variables
    tInteger        :: OutUnit
    tCharAlloc      :: FileName
    tLogical        :: ReadDirect

    ! execution

    ! set variable relating optional input
    ReadDirect = TrueVal
    IF (PRESENT(FromFile)) ReadDirect = .NOT.FromFile

    ! open output file
    OutUnit = 11
    FileName = RNG%GetName() // '.txt'
    OPEN (OutUnit, FILE=FileName)

    ! check whether to perform test by using the generator directly
    IF (ReadDirect) THEN
        ! perform tests by using the generator directly
        CALL FT01_BirthdaySpacing(OutUnit, RNG)
        CALL FT02_Overlap5Permute(OutUnit, RNG)
        CALL FT03_BinaryRank_3132(OutUnit, RNG)
        CALL FT04_BinaryRank_6x8(OutUnit, RNG)
        CALL FT05_BitStream(OutUnit, RNG)
        CALL FT06_OPSO_OQSO_DNA(OutUnit, RNG)
        CALL FT07_CountThe1s_Stream(OutUnit, RNG)
        CALL FT08_CountThe1s_SpecificBytes(OutUnit, RNG)
        CALL FT09_ParkingLot(OutUnit, RNG)
        CALL FT10_MinDistance(OutUnit, RNG)
        CALL FT11_3DSphere(OutUnit, RNG)
        CALL FT12_Squeeze(OutUnit, RNG)
        CALL FT13_OverlapSum(OutUnit, RNG)
        CALL FT14_Runs(OutUnit, RNG)
        CALL FT15_Craps(OutUnit, RNG)
    ELSE
        ! generate random numbers and write them to a file
        CALL WRITE_Random_I32(RNG)
        BLOCK
            ! blcok variables
            TYPE(I32Data)   :: InpRec
            TYPE(I8Data)    :: InpRec2
            tCharAlloc      :: InpFileName

            ! open input file
            InpRec%FileUnit  = 12
            InpRec2%FileUnit = 12
            InpFileName = RNG%GetName() // '.dat'
            OPEN (InpRec%FileUnit, FILE=InpFileName, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4096)

            ! perform tests
            CALL FT01_BirthdaySpacing(InpFileName, OutUnit, InpRec)
            CALL FT02_Overlap5Permute(InpFileName, OutUnit, InpRec)
            CALL FT03_BinaryRank_3132(InpFileName, OutUnit, InpRec)
            CALL FT04_BinaryRank_6x8(InpFileName, OutUnit, InpRec)
            CALL FT05_BitStream(InpFileName, OutUnit, InpRec)
            CALL FT06_OPSO_OQSO_DNA(InpFileName, OutUnit, InpRec)
            CALL FT07_CountThe1s_Stream(InpFileName, OutUnit, InpRec2)
            CALL FT08_CountThe1s_SpecificBytes(InpFileName, OutUnit, InpRec)
            CALL FT09_ParkingLot(InpFileName, OutUnit, InpRec)
            CALL FT10_MinDistance(InpFileName, OutUnit, InpRec)
            CALL FT11_3DSphere(InpFileName, OutUnit, InpRec)
            CALL FT12_Squeeze(InpFileName, OutUnit, InpRec)
            CALL FT13_OverlapSum(InpFileName, OutUnit, InpRec)
            CALL FT14_Runs(InpFileName, OutUnit, InpRec)
            CALL FT15_Craps(InpFileName, OutUnit, InpRec)

            ! close input file
            CLOSE (InpRec%FileUnit)
        END BLOCK
    END IF

    ! close output file
    CLOSE (OutUnit)

    RETURN

END SUBROUTINE Diehard_Tests

!******************************************************************************

SUBROUTINE Diehard_Tests_Direct(RNG)

    ! argument
    CLASS(BaseRNG),  INTENT(INOUT)  :: RNG

    ! local variables
    tInteger        :: OutUnit
    tCharAlloc      :: FileName

    ! execution

    ! open output file
    OutUnit = 11
    FileName = RNG%GetName() // '.txt'
    OPEN (OutUnit, FILE=FileName)

    ! perform tests by using the generator directly
    CALL FT01_BirthdaySpacing(OutUnit, RNG)
    CALL FT02_Overlap5Permute(OutUnit, RNG)
    CALL FT03_BinaryRank_3132(OutUnit, RNG)
    CALL FT04_BinaryRank_6x8(OutUnit, RNG)
    CALL FT05_BitStream(OutUnit, RNG)
    CALL FT06_OPSO_OQSO_DNA(OutUnit, RNG)
    CALL FT07_CountThe1s_Stream(OutUnit, RNG)
    CALL FT08_CountThe1s_SpecificBytes(OutUnit, RNG)
    CALL FT09_ParkingLot(OutUnit, RNG)
    CALL FT10_MinDistance(OutUnit, RNG)
    CALL FT11_3DSphere(OutUnit, RNG)
    CALL FT12_Squeeze(OutUnit, RNG)
    CALL FT13_OverlapSum(OutUnit, RNG)
    CALL FT14_Runs(OutUnit, RNG)
    CALL FT15_Craps(OutUnit, RNG)

    ! close output file
    CLOSE (OutUnit)

    RETURN

END SUBROUTINE Diehard_Tests_Direct

!******************************************************************************

SUBROUTINE Diehard_Tests_FromFile(RNG)

    ! argument
    CLASS(BaseRNG),  INTENT(INOUT)  :: RNG

    ! local variables
    tInteger        :: OutUnit
    tCharAlloc      :: FileName
    TYPE(I32Data)   :: InpRec
    TYPE(I8Data)    :: InpRec2
    tCharAlloc      :: InpFileName

    ! execution

    ! open output file
    OutUnit = 11
    FileName = RNG%GetName() // '.txt'
    OPEN (OutUnit, FILE=FileName)

    ! generate random numbers and write them to a file
    CALL WRITE_Random_I32(RNG)

    ! open input file
    InpRec%FileUnit  = 12
    InpRec2%FileUnit = 12
    InpFileName = RNG%GetName() // '.dat'
    OPEN (InpRec%FileUnit, FILE=InpFileName, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4096)

    ! perform tests
    CALL FT01_BirthdaySpacing(InpFileName, OutUnit, InpRec)
    CALL FT02_Overlap5Permute(InpFileName, OutUnit, InpRec)
    CALL FT03_BinaryRank_3132(InpFileName, OutUnit, InpRec)
    CALL FT04_BinaryRank_6x8(InpFileName, OutUnit, InpRec)
    CALL FT05_BitStream(InpFileName, OutUnit, InpRec)
    CALL FT06_OPSO_OQSO_DNA(InpFileName, OutUnit, InpRec)
    CALL FT07_CountThe1s_Stream(InpFileName, OutUnit, InpRec2)
    CALL FT08_CountThe1s_SpecificBytes(InpFileName, OutUnit, InpRec)
    CALL FT09_ParkingLot(InpFileName, OutUnit, InpRec)
    CALL FT10_MinDistance(InpFileName, OutUnit, InpRec)
    CALL FT11_3DSphere(InpFileName, OutUnit, InpRec)
    CALL FT12_Squeeze(InpFileName, OutUnit, InpRec)
    CALL FT13_OverlapSum(InpFileName, OutUnit, InpRec)
    CALL FT14_Runs(InpFileName, OutUnit, InpRec)
    CALL FT15_Craps(InpFileName, OutUnit, InpRec)

    ! close input file
    CLOSE (InpRec%FileUnit)

    ! close output file
    CLOSE (OutUnit)

    RETURN

END SUBROUTINE Diehard_Tests_FromFile

!******************************************************************************

FUNCTION Read_Byte_FromFile(InpRec) RESULT(Sample)

    ! arguments
    CLASS(I8Data), INTENT(INOUT)    :: InpRec
    tInteger                        :: Sample

    ! paameter
    tInteger, PARAMETER :: Mask = ToInteger(Z'000000FF')

    ! execution

    IF (InpRec%Counter > 4096) THEN
        READ (InpRec%FileUnit, REC=InpRec%RecNum) InpRec%Sample(1:4096)
        InpRec%Counter = 1
        InpRec%RecNum  = InpRec%RecNum + 1
    END IF

    IF (InpRec%NLeft == 0) THEN
        InpRec%CurrentSample = InpRec%Sample(InpRec%Counter)
        InpRec%Counter       = InpRec%Counter + 1
        InpRec%NLeft         = 4
    END IF

    Sample = IAND(SHIFTR(InpRec%CurrentSample, 24), Mask)
    InpRec%CurrentSample = SHIFTL(InpRec%CurrentSample, 8)
    InpRec%NLeft = InpRec%NLeft - 1

    RETURN

END FUNCTION Read_Byte_FromFile

!******************************************************************************

FUNCTION Read_Integer_FromFile(InpRec) RESULT(Sample)

    ! arguments
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    tInteger                        :: Sample

    ! execution

    IF (InpRec%Counter > 4096) THEN
      READ (InpRec%FileUnit, REC=InpRec%RecNum) InpRec%Sample(1:4096)
      InpRec%Counter = 1
      InpRec%RecNum = InpRec%RecNum + 1

    END IF
    Sample = InpRec%Sample(InpRec%Counter)
    InpRec%Counter = InpRec%Counter + 1

    RETURN

END FUNCTION Read_Integer_FromFile

!******************************************************************************

FUNCTION Read_Byte_FromRNG(InpRec) RESULT(Sample)

    ! arguments
    CLASS(I8B_Data), INTENT(INOUT)  :: InpRec
    tInteger                        :: Sample

    ! paameter
    tInteger, PARAMETER :: Mask = ToInteger(Z'000000FF')

    ! execution

    IF (InpRec%NLeft == 0) THEN
        InpRec%CurrentSample = InpRec%RNG%NextInteger()
        InpRec%NLeft         = 4
    END IF

    Sample = IAND(SHIFTR(InpRec%CurrentSample, 24), Mask)
    InpRec%CurrentSample = SHIFTL(InpRec%CurrentSample, 8)
    InpRec%NLeft = InpRec%NLeft - 1

    RETURN

END FUNCTION Read_Byte_FromRNG

!******************************************************************************

FUNCTION Read_Integer_FromRNG(RNG) RESULT(Sample)

    ! arguments
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    tInteger                        :: Sample

    ! execution

    Sample = RNG%NextInteger()

    RETURN

END FUNCTION Read_Integer_FromRNG

!******************************************************************************

FUNCTION Uni_FromFile(InpRec) RESULT(ResVal)

    ! function used by 'PARKING LOT' and 'SQUEEZE' tests

    ! arguments
    TYPE(I32Data), INTENT(INOUT)    :: InpRec
    tSingle                         :: ResVal

    ! execution

    ! Put a one-line function here to provide the uni being tested:
    ResVal = 0.5 + InpRec%GetSample() * 0.5 ** 32

    RETURN

END FUNCTION Uni_FromFile

!******************************************************************************

FUNCTION Uni_FromRNG(RNG) RESULT(ResVal)

    ! function used by 'PARKING LOT' and 'SQUEEZE' tests

    ! arguments
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    tSingle                         :: ResVal

    ! execution

    ! Put a one-line function here to provide the uni being tested:
    ResVal = 0.5 + GetSample(RNG) * 0.5 ** 32

    RETURN

END FUNCTION Uni_FromRNG

!******************************************************************************

FUNCTION Phi(X) RESULT(ResVal)

    ! arguments
    tSingle, INTENT(IN) :: X
    tSingle             :: ResVal

    ! parameters
    tDouble, PARAMETER  :: V(0:14) = [  &
        1.2533141373155000D+00, 0.6556795424187985D+00, &
        0.4213692292880545D+00, 0.3045902987101033D+00, &
        0.2366523829135607D+00, 0.1928081047153158D+00, &
        0.1623776608968675D+00, 0.1401041834530502D+00, &
        0.1231319632579329D+00, 0.1097872825783083D+00, &
        0.9902859647173193D-01, 0.9017567550106468D-01, &
        0.8276628650136917D-01, 0.7647576101624850D-01, &
        0.7106958053885211D-01 ]

    ! local variables
    tDouble     :: A, B, CPhi, H, Pwr, Sum, Z
    tInteger    :: I, J

    ! execution

    ResVal = 0.5 + SIGN(0.5, X)
    IF (ABS(X) > 7.0) RETURN
    CPhi = 0.5_kDouble - SIGN(0.5, X)
    J = ABS(X) + 0.5_kDouble
    J = MIN(J,14)
    Z = J
    H = ABS(X) - Z
    A = V(J)
    B = Z * A - 1.0_kDouble
    Pwr = 1.0_kDouble
    Sum = A + H * B
    DO  I = 2, 24 - J, 2
        A = (A + Z*B) / I
        B = (B + Z*A) / (I+1)
        Pwr = Pwr * H ** 2
        Sum = Sum + Pwr * (A + H*B)
    END DO
    CPhi = Sum * EXP(-0.5_kDouble*X*X - 0.918938533204672_kDouble)
    ResVal = 1.0_kDouble - CPhi
    IF (X < 0.0_kDouble) ResVal = CPhi

    RETURN

END FUNCTION Phi

!******************************************************************************

FUNCTION ChiSquare(X, N) RESULT(ResVal)

    ! arguments
    tSingle,  INTENT(IN)    :: X
    tInteger, INTENT(IN)    :: N
    tSingle                 :: ResVal

    ! local variables
    tInteger    :: I, L
    tSingle     :: D, S, T

    ! execution

    ResVal = 0.0
    IF (X <= 0.0) RETURN
    IF (N > 20) THEN
        T = ((X/N)**0.333333 - 1 + (0.222222/N)) / SQRT(0.222222/N)
        ResVal = Phi(MIN(T, 8.0))
        RETURN
    END IF
    L = 4 - MOD(N, 2)
    D = MIN(1, N/3)
    DO  I = L, N, 2
        D = D*X / (I-2)
        ResVal = ResVal + D
    END DO
    IF (L == 3) THEN
        S = SQRT(MIN(0.5*X, 50.0))
        ResVal = Phi(S/0.7071068) - EXP(-MIN(0.5*X,50.0)) * 0.564189 * ResVal / S
        RETURN
    END IF
    ResVal = 1.0 - EXP(-MIN(0.5*X, 50.0)) * (1.0 + ResVal)

    RETURN

END FUNCTION ChiSquare

!******************************************************************************

FUNCTION SpFunc(X, I) RESULT(ResVal)

    ! arguments
    tSingle,  INTENT(IN)    :: X
    tInteger, INTENT(IN)    :: I
    tSingle                 :: ResVal

    ! local variables
    tSingle     :: T

    ! execution

    ResVal = 0.0
    SELECT CASE (I)
    CASE (1:7)
        T = ABS(10.0*X - 0.5 - I)
        IF (T > 1.5) RETURN
        IF (T <= 0.5) THEN
            ResVal = 1.5 - 2.0 * T * T
        ELSE
            ResVal = 2.25 - T * (3.0-T)
        END IF
    CASE (8)
        IF (X <= 0.8 .OR. X >= 1.0) RETURN
        ResVal = 100.0 * (X-0.9) ** 2 - 1.0
    CASE (9)
        IF (X <= 0.0 .OR. X >= 0.05) RETURN
        IF (X <= 0.01) THEN
            ResVal = -100.0 * X
        ELSE
            ResVal = 25.0 * (X-.05)
        END IF
    CASE (10)
        IF (X <= 0.98 .OR. X >= 1.0) RETURN
        ResVal = 0.1 - 10.0 * ABS(X-0.99)
    END SELECT

    RETURN
END FUNCTION SpFunc

!******************************************************************************

SUBROUTINE ASort(List)

    ! arguments
    tSingle,  INTENT(INOUT) :: List(:)

    ! local variables
    ! na

    ! execution

    CALL WiseSort(List)

    RETURN

END SUBROUTINE ASort
!******************************************************************************

SUBROUTINE ISort(List)

    ! arguments
    tInteger, INTENT(INOUT) :: List(:)

    ! local variables
    ! na

    ! execution
    
    CALL WiseSort(List)

    RETURN

END SUBROUTINE ISort
!******************************************************************************

SUBROUTINE KS_Test(Y, N, P)

    !      TO TEST WHETHER A SET OF N REAL NUMBERS IS DRAWN
    !      FROM A UNIFORM DISTRIBUTION (KOLMOROGOV-SMIRNOV METHOD)
    !      THE TEST IS BASED ON THE DISTANCE BETWEEN THE EMPIRICAL
    !      AND THEORETICAL DISTRIBUTION FUNCTIONS
    !       USAGE: CALL KSTEST(Y,N,P)
    !      Y ...   ARRAY OF REAL NUMBERS HYPOTHETICALLY DRAWN
    !              FROM A UNIFORM DISTRIBUTION ON (0,1)
    !      N ...   NUMBER OF ELEMENTS IN 'Y'
    !      P IS THE PROBABILITY ASSOCIATED WITH THE OBSERVED VALUE
    !      OF THE ANDERSON-DARLING STATISTIC: N TIMES THE INTEGRAL
    !      OF (FN(X)-X)**2/(X*(1-X))

    ! arguments
    tSingle,  INTENT(INOUT) :: Y(:)
    tInteger, INTENT(IN)    :: N
    tSingle,  INTENT(OUT)   :: P

    ! parameters
    tInteger, PARAMETER  :: L(8,10) = RESHAPE(                            &
       [40, 46, 37, 34, 27, 24, 20, 20, 88, 59, 43, 37, 29, 27, 20, 22,   &
        92, 63, 48, 41, 30, 30, 25, 24, 82, 59, 42, 37, 26, 28, 26, 22,   &
        62, 48, 33, 30, 23, 23, 22, 18, 49, 34, 22, 20, 16, 17, 17, 12,   &
        17, 17,  7,  8,  4,  7,  5,  1, 40, 18, 19, 14, 16, 13, 10,  9,   &
        59, 20, 10,  4,  1,  1,  0, -1, 41, 43, 36, 112, 15, 95, 32, 58], [8, 10])

    ! local variables
    tInteger    :: I, J, M
    tSingle     :: E, T, Z

    ! execution

    CALL ASort(Y)
    Z = -N * N
    DO  I = 1, N
        T = Y(I) * (1.0 - Y(N+1-I))
        IF (T < 1.0E-20) T = 1.0E-20
        Z = Z - (I+I-1) * LOG(T)
    END DO
    Z = Z / N
    P = 0.0
    IF (Z >= 0.01) THEN
        IF (Z <= 2.0) THEN
            P = 2.0 * EXP(-1.2337/Z) * (1.0+Z/8.0-0.04958*Z*Z/(1.325+Z)) / SQRT(Z)
        ELSE
            IF (Z <= 4.0) THEN
                P = 1.0 - 0.6621361 * EXP(-1.091638*Z) - 0.95059 * EXP(-2.005138*Z)
            ELSE
                P = 1.0 - 0.4938691 * EXP(-1.050321*Z) - 0.5946335 * EXP(-1.527198*Z)
            END IF
        END IF
    END IF
    M = MIN(N-2, 8)
    E = 0.0
    DO  J = 1, 10
        E = E + L(M, J) * SpFunc(P, J) * 0.0001
    END DO
    IF (N > 10) E = 10.0 * E / N

    RETURN

END SUBROUTINE KS_Test

!******************************************************************************

SUBROUTINE WRITE_Random_I32(RNG)

    ! argument
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG

    ! local variables
    tInteger        :: OutUnit
    tCharAlloc      :: FileName
    tInteger        :: RandNum(4096)
    tInteger        :: I, J

    ! execution

    OutUnit = 101
    FileName = RNG%GetName() // '.dat'
    OPEN (OutUnit, FILE=FileName, FORM='UNFORMATTED', ACCESS='DIRECT', RECL=4096)
    DO I = 1, 700
        DO J = 1, 4096
            RandNum(J) = RNG%NextInteger()
        END DO
        WRITE(OutUnit, REC=I) RandNum
    END DO
    CLOSE (OutUnit)

    RETURN

END SUBROUTINE WRITE_Random_I32

!******************************************************************************

END MODULE ModTest_Diehard

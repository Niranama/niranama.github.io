
SUBMODULE (ModTest_Diehard) SubTest_Runs

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Runs" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(13) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::     This is the RUNS test.  It counts runs up, and runs down, ::", &
        "     :: in a sequence of uniform [0,1) variables, obtained by float-  ::", &
        "     :: ing the 32-bit integers in the specified file. This example   ::", &
        "     :: shows how runs are counted:  .123,.357,.789,.425,.224,.416,.95::", &
        "     :: contains an up-run of length 3, a down-run of length 2 and an ::", &
        "     :: up-run of (at least) 2, depending on the next values.  The    ::", &
        "     :: covariance matrices for the runs-up and runs-down are well    ::", &
        "     :: known, leading to chisquare tests for quadratic forms in the  ::", &
        "     :: weak inverses of the covariance matrices.  Runs are counted   ::", &
        "     :: for sequences of length 10,000.  This is done ten times. Then ::", &
        "     :: repeated.                                                     ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE FT14_Runs_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Runs" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec

    ! local variables
    tSingle         :: X(10000), Up(100), Down(100)
    tInteger        :: I, IFault, IJ, IJKN, NS, NXS
    tSingle         :: DV, P, UV

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:13)
    WRITE (OutUnit, 5000) TestDescription(1:13)

    NS = 10
    NXS = 10000
    InpRec%RecNum = 1
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5100)       InpFileName
    DO  IJKN = 1, 2
        DO  IJ = 1, NS
            DO  I = 1, NXS
                X(I) = InpRec%GetSample() * 2.328306E-10
            END DO
            CALL Up_N_Down_Runs(X, NXS, UV, DV, IFault)
            Up(IJ) = Ch(UV)
            Down(IJ) = Ch(DV)
        END DO
        CALL KS_Test(Up, NS, P)
        WRITE (*, 5200)       InpFileName
        WRITE (OutUnit, 5200) InpFileName
        WRITE (OutUnit, 5300) P
        WRITE (*, 5300)       P
        CALL KS_Test(Down, NS, P)
        WRITE (OutUnit, 5400) P
        WRITE (*, 5400)       P
    END DO
    WRITE (*, 5500)
    WRITE (OutUnit, 5500)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('           The RUNS test for file ', A15/   &
             '     Up and down runs in a sample of 10000'/   &
             '_________________________________________________ ')
5200 FORMAT (T16, '  Run test for ', A20, ':')
5300 FORMAT (T5, '   runs up; ks test for 10 p''S:', F8.6)
5400 FORMAT (T5, ' runs down; ks test for 10 p''S:', F8.6)
5500 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT14_Runs_FromFile

!******************************************************************************

MODULE SUBROUTINE FT14_Runs_FromRNG(OutUnit, RNG)

    ! To perform the "Runs" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG

    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: X(10000), Up(100), Down(100)
    tInteger        :: I, IFault, IJ, IJKN, NS, NXS
    tSingle         :: DV, P, UV

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:13)
    WRITE (OutUnit, 5000) TestDescription(1:13)

    NS = 10
    NXS = 10000
    CALL RNG%ReInit()
    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5100)       RngName
    DO  IJKN = 1, 2
        DO  IJ = 1, NS
            DO  I = 1, NXS
                X(I) = GetSample(RNG) * 2.328306E-10
            END DO
            CALL Up_N_Down_Runs(X, NXS, UV, DV, IFault)
            Up(IJ) = Ch(UV)
            Down(IJ) = Ch(DV)
        END DO
        CALL KS_Test(Up, NS, P)
        WRITE (*, 5200)       RngName
        WRITE (OutUnit, 5200) RngName
        WRITE (OutUnit, 5300) P
        WRITE (*, 5300)       P
        CALL KS_Test(Down, NS, P)
        WRITE (OutUnit, 5400) P
        WRITE (*, 5400)       P
    END DO
    WRITE (*, 5500)
    WRITE (OutUnit, 5500)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('           The RUNS test ', A15/   &
             '     Up and down runs in a sample of 10000'/   &
             '_________________________________________________ ')
5200 FORMAT (T16, '  Run test for ', A20, ':')
5300 FORMAT (T5, '   runs up; ks test for 10 p''S:', F8.6)
5400 FORMAT (T5, ' runs down; ks test for 10 p''S:', F8.6)
5500 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT14_Runs_FromRNG

!******************************************************************************

FUNCTION Ch(X) RESULT(ResVal)

    ! function used by 'RUNS' test

    ! arguments
    tSingle, INTENT(IN) :: X
    tSingle             :: ResVal

    ! execution

    !**** up and down runs test******************
    ResVal = 1.0 - EXP(-0.5*X) * (1.0 + 0.5*X + 0.125*X**2)

    RETURN

END FUNCTION Ch

!******************************************************************************

SUBROUTINE Up_N_Down_Runs(X, N, UV, DV, IFault)

    ! Algorithm AS 157 APPl. Statist. (1981) vol. 30, No. 1
    ! The Runs-up and Runs-down test used by 'RUNS' test.

    ! arguments
    tSingle,  INTENT(IN)    :: X(10000)
    tInteger, INTENT(IN)    :: N
    tSingle,  INTENT(OUT)   :: UV
    tSingle,  INTENT(OUT)   :: DV
    tInteger, INTENT(OUT)   :: IFault

    ! parameters
    tSingle, PARAMETER  :: A(6,6) = RESHAPE([                         &
         4529.4,   9044.9,  13568.0,   18091.0,   22615.0,   27892.0, &
         9044.9,  18097.0,  27139.0,   36187.0,   45234.0,   55789.0, &
        13568.0,  27139.0,  40721.0,   54281.0,   67852.0,   83685.0, &
        18091.0,  36187.0,  54281.0,   72414.0,   90470.0,  111580.0, &
        22615.0,  45234.0,  67852.0,   90470.0,  113262.0,  139476.0, &
        27892.0,  55789.0,  83685.0,  111580.0,  139476.0,  172860.0], [6, 6])
    tSingle, PARAMETER  :: B(6) = [                                   &
        1.0/6.0, 5.0/24.0, 11.0/120.0, 19.0/720.0, 29.0/5040.0, 1.0/840.0]

    ! local variables
    tInteger    :: I, J, UCount(6), DCount(6), RU, RD
    tSingle     :: RN

    ! execution

    IFault = 0
    IF (N >= 4000) THEN
        DO  I = 1, 6
            UCount(I) = 0
            DCount(I) = 0
        END DO
        !     The loop that ends at line 300 determines the number of
        !     Runs-up and Runs-down of length i for i = 1(1)5 and the number
        !     of Runs-up and Runs-down of length greater than or equal to 6.
        RU = 1
        RD = 1
        DO  J = 2, N
            IF ((X(J)-X(J-1)) <= 0.0) THEN
                UCount(RU) = UCount(RU) + 1
                RU = 1
                IF (RD < 6) RD = RD + 1
            ELSE
                DCount(RD) = DCount(RD) + 1
                RD = 1
                IF (RU < 6) RU = RU + 1
            END IF
        END DO
        UCount(RU) = UCount(RU) + 1
        DCount(RD) = DCount(RD) + 1
        !      print 21,UCount,DCount

        !     Calculate the test statistics UV and DV.
        UV = 0.0
        DV = 0.0
        RN = N
        DO  I = 1, 6
            DO  J = 1, 6
                UV = UV + (UCount(I) - RN*B(I)) * (UCount(J) - RN*B(J)) * A(I,J)
                DV = DV + (DCount(I) - RN*B(I)) * (DCount(J) - RN*B(J)) * A(I,J)
            END DO
        END DO
        UV = UV / RN
        DV = DV / RN
    ELSE
        IFault = N
    END IF

    RETURN

END SUBROUTINE Up_N_Down_Runs

!******************************************************************************

END SUBMODULE SubTest_Runs

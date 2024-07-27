
SUBMODULE (ModTest_Diehard) SubTest_Squeeze

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Squeeze" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(10) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::      This is the SQUEEZE test                                 ::", &
        "     ::  Random integers are floated to get uniforms on [0,1). Start- ::", &
        "     ::  ing with k=2^31=2147483647, the test finds j, the number of  ::", &
        "     ::  iterations necessary to reduce k to 1, using the reduction   ::", &
        "     ::  k=ceiling(k*U), with U provided by floating integers from    ::", &
        "     ::  the file being tested.  Such j's are found 100,000 times,    ::", &
        "     ::  then counts for the number of times j was <=6,7,...,47,>=48  ::", &
        "     ::  are used to provide a chi-square test for cell frequencies.  ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tSingle,      PARAMETER :: Expected(6:48) = [                         &
        21.03, 57.79, 175.54, 467.32, 1107.83,                      &
        2367.84, 4609.44, 8241.16, 13627.81, 20968.49, 30176.12,    &
        40801.97, 52042.03, 62838.28, 72056.37, 78694.51, 82067.55, &
        81919.35, 78440.08, 72194.12, 63986.79, 54709.31, 45198.52, &
        36136.61, 28000.28, 21055.67, 15386.52, 10940.20, 7577.96,  &
        5119.56, 3377.26, 2177.87, 1374.39, 849.70, 515.18, 306.66, &
        179.39, 103.24, 58.51, 32.69, 18.03, 9.82, 11.21 ]

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

MODULE SUBROUTINE FT12_Squeeze_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Squeeze" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: ChiSqr, Sigma, Observed(6:48)
    tInteger        :: I, J, K

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:10)
    WRITE (OutUnit, 5000) TestDescription(1:10)

    !  SQUEEZE TEST.  How many iterations of k=k*Uni()+1 are required
    !  to squeeze k down to 1, starting with k=2147483647=2^31-1.
    !  The exact distribution of the required j is used, with
    !  a chi-square test based on 100,000 tries.
    !  The mean of j is 23.064779, with variance 23.70971151.

    InpRec%RecNum = 1

    Observed(6:48) = 0
    DO  I = 1, 100000
        J = 0
        K = 2147483647
        Loop20: DO
            K = K * Uni(InpRec) + 1
            J = J + 1
            IF (K <= 1) EXIT Loop20
        END DO Loop20
        J = MIN(MAX(J, 6), 48)
        Observed(J) = Observed(J) + 1.
    END DO
    ChiSqr = 0
    DO  I = 6, 48
        ChiSqr = ChiSqr + (Observed(I)-0.1*Expected(I)) ** 2 / (0.1*Expected(I))
    END DO
    Sigma = SQRT(84.)
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5100)       InpFileName
    WRITE (OutUnit, 5200)
    WRITE (*, 5200)
    WRITE (OutUnit, 5300) ((Observed(I)-0.1*Expected(I))/SQRT(0.1*Expected(I)), I = 6,48)
    WRITE (*, 5300)       ((Observed(I)-0.1*Expected(I))/SQRT(0.1*Expected(I)), I = 6,48)
    WRITE (OutUnit, 5400) ChiSqr
    WRITE (*, 5400)       ChiSqr
    WRITE (OutUnit, 5500) (ChiSqr-42.0) / Sigma, ChiSquare(ChiSqr, 42)
    WRITE (*, 5500)       (ChiSqr-42.0) / Sigma, ChiSquare(ChiSqr, 42)
    WRITE (OutUnit, 5600)
    WRITE (*, 5600)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('            RESULTS OF SQUEEZE TEST FOR ', A15)
5200 FORMAT ('         Observed of standardized frequency counts'/   &
             '     ( (obs-exp)/sqrt(exp) )^2'/   &
             '        for j taking values <=6,7,8,...,47,>=48:')
5300 FORMAT (6F8.1)
5400 FORMAT (T9, '   Chi-square with 42 degrees of freedom:', F7.3)
5500 FORMAT (T9, '      z-score=', F7.3, '  p-value=', F8.6/   &
             '______________________________________________________________')
5600 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT12_Squeeze_FromFile

!******************************************************************************

MODULE SUBROUTINE FT12_Squeeze_FromRNG(OutUnit, RNG)

    ! To perform the "Squeeze" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: ChiSqr, Sigma, Observed(6:48)
    tInteger        :: I, J, K

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:10)
    WRITE (OutUnit, 5000) TestDescription(1:10)

    !  SQUEEZE TEST.  How many iterations of k=k*Uni()+1 are required
    !  to squeeze k down to 1, starting with k=2147483647=2^31-1.
    !  The exact distribution of the required j is used, with
    !  a chi-square test based on 100,000 tries.
    !  The mean of j is 23.064779, with variance 23.70971151.

    CALL RNG%ReInit()

    Observed(6:48) = 0
    DO  I = 1, 100000
        J = 0
        K = 2147483647
        Loop20: DO
            K = K * Uni(RNG) + 1
            J = J + 1
            IF (K <= 1) EXIT Loop20
        END DO Loop20
        J = MIN(MAX(J, 6), 48)
        Observed(J) = Observed(J) + 1.
    END DO
    ChiSqr = 0
    DO  I = 6, 48
        ChiSqr = ChiSqr + (Observed(I)-0.1*Expected(I)) ** 2 / (0.1*Expected(I))
    END DO
    Sigma = SQRT(84.)
    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5100)       RngName
    WRITE (OutUnit, 5200)
    WRITE (*, 5200)
    WRITE (OutUnit, 5300) ((Observed(I)-0.1*Expected(I))/SQRT(0.1*Expected(I)), I = 6,48)
    WRITE (*, 5300)       ((Observed(I)-0.1*Expected(I))/SQRT(0.1*Expected(I)), I = 6,48)
    WRITE (OutUnit, 5400) ChiSqr
    WRITE (*, 5400)       ChiSqr
    WRITE (OutUnit, 5500) (ChiSqr-42.0) / Sigma, ChiSquare(ChiSqr, 42)
    WRITE (*, 5500)       (ChiSqr-42.0) / Sigma, ChiSquare(ChiSqr, 42)
    WRITE (OutUnit, 5600)
    WRITE (*, 5600)

    RETURN

5000 FORMAT (A78)
5100 FORMAT ('            RESULTS OF SQUEEZE TEST FOR ', A15)
5200 FORMAT ('         Observed of standardized frequency counts'/   &
             '     ( (obs-exp)/sqrt(exp) )^2'/   &
             '        for j taking values <=6,7,8,...,47,>=48:')
5300 FORMAT (6F8.1)
5400 FORMAT (T9, '   Chi-square with 42 degrees of freedom:', F7.3)
5500 FORMAT (T9, '      z-score=', F7.3, '  p-value=', F8.6/   &
             '______________________________________________________________')
5600 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT12_Squeeze_FromRNG

!******************************************************************************

END SUBMODULE SubTest_Squeeze

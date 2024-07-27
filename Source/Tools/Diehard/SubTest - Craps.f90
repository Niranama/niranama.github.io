
SUBMODULE (ModTest_Diehard) SubTest_Craps

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "Craps" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(12) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     :: This is the CRAPS TEST. It plays 200,000 games of craps, finds::", &
        "     :: the number of wins and the number of throws necessary to end  ::", &
        "     :: each game.  The number of wins should be (very close to) a    ::", &
        "     :: normal with mean 200000p and variance 200000p(1-p), with      ::", &
        "     :: p=244/495.  Throws necessary to complete the game can vary    ::", &
        "     :: from 1 to infinity, but counts for all>21 are lumped with 21. ::", &
        "     :: A chi-square test is made on the no.-of-throws cell counts.   ::", &
        "     :: Each 32-bit integer from the test file provides the value for ::", &
        "     :: the throw of a die, by floating to [0,1), multiplying by 6    ::", &
        "     :: and taking 1 plus the integer part of the result.             ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    INTERFACE KThrow
        MODULE PROCEDURE KThrow_FromRNG, KThrow_FromFile
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

MODULE SUBROUTINE FT15_Craps_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "Craps" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec

    ! local variables
    tSingle         :: Avg, E(21), Expected, PThrows, PWins, StdDev, Sum, T, ChiSqr
    tInteger        :: I, IWin, K, LP, M, NG, NT(21), NThrows, NWins
    tLogical        :: SkipLoop30

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:12)
    WRITE (OutUnit, 5000) TestDescription(1:12)

    InpRec%RecNum = 1

    E(1) = 1.0 / 3.0
    Sum = E(1)
    DO  K = 2, 20
        E(K) = (27.0*(27.0/36.0)**(K-2) + 40.0*(26.0/36.0)**(K-2) + &
                55.0*(25.0/36.0)**(K-2)) / 648.0
        Sum = Sum + E(K)
    END DO
    E(21) = 1.0 - Sum
    NG = 200000
    NWins = 0
    NT(1:21) = 0
    DO  I = 1, NG
        LP = KThrow(InpRec)
        NThrows = 1
        SkipLoop30 = FalseVal
        IF (LP == 7 .OR. LP == 11) THEN
            IWin = 1
            SkipLoop30 = TrueVal
        END IF
        IF (LP == 2 .OR. LP == 3 .OR. LP == 12) THEN
            IWin = 0
            SkipLoop30 = TrueVal
        END IF
        IF (.NOT.SkipLoop30) THEN
            Loop30: DO
                K = KThrow(InpRec)
                NThrows = NThrows + 1
                IF (K == 7) THEN
                    IWin = 0
                    EXIT Loop30
                END IF
                IF (K == LP) THEN
                    IWin = 1
                    EXIT Loop30
                END IF
            END DO Loop30
        END IF
        M = MIN(21, NThrows)
        NT(M) = NT(M) + 1
        NWins = NWins + IWin
    END DO
    Avg = 244.0 * NG / 495.0
    StdDev = SQRT(Avg*251.0/495.0)
    T = (NWins-Avg) / StdDev
    WRITE (*, 5100)       InpFileName
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5200)       NWins, Avg
    WRITE (OutUnit, 5200) NWins, Avg
    PWins = Phi(T)
    WRITE (*, 5300)       NWins, T, PWins
    WRITE (OutUnit, 5300) NWins, T, PWins
    Sum = 0.
    DO  I = 1, 21
        Expected = NG * E(I)
        Sum = Sum + (NT(I)-Expected) ** 2 / Expected
    END DO
    PThrows = ChiSquare(Sum,20)
    WRITE (OutUnit, 5400) Sum, PThrows
    WRITE (*, 5400)       Sum, PThrows
    Sum = 0
    DO  I = 1, 21
        Expected = NG * E(I)
        ChiSqr = (NT(I)-Expected) ** 2 / Expected
        Sum = Sum + ChiSqr
        WRITE (OutUnit, 5500) I, NT(I), Expected, ChiSqr, Sum
        WRITE (*, 5500)       I, NT(I), Expected, ChiSqr, Sum
    END DO
    WRITE (OutUnit, 5600) InpFileName
    WRITE (*, 5600)       InpFileName
    WRITE (OutUnit, 5700) PWins
    WRITE (*, 5700)       PWins
    WRITE (OutUnit, 5800) PThrows
    WRITE (*, 5800)       PThrows
    WRITE (*, 5900)
    WRITE (OutUnit, 5900)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (T16, ' Results of craps test for ', A20/   &
             '  No. of wins:  Observed Expected')
5200 FORMAT (T16, '          ',I12, F12.2)
5300 FORMAT (T16, I8, '= No. of wins, z-score=', F6.3, ' pvalue=', F7.5/   &
             '   Analysis of Throws-per-Game:')
5400 FORMAT (' Chisq=', F7.2, ' for 20 degrees of freedom, p= ', F8.5/   &
             T16, 'Throws Observed Expected  Chisq     Sum')
5500 FORMAT (I19, I9, F11.1, F8.3, F9.3)
5600 FORMAT ('            SUMMARY  FOR ', A15)
5700 FORMAT (T16, ' p-value for no. of wins:', F8.6)
5800 FORMAT (T16, ' p-value for throws/game:', F8.6)
5900 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT15_Craps_FromFile

!******************************************************************************

MODULE SUBROUTINE FT15_Craps_FromRNG(OutUnit, RNG)

    ! To perform the "Craps" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG

    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: Avg, E(21), Expected, PThrows, PWins, StdDev, Sum, T, ChiSqr
    tInteger        :: I, IWin, K, LP, M, NG, NT(21), NThrows, NWins
    tLogical        :: SkipLoop30

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:12)
    WRITE (OutUnit, 5000) TestDescription(1:12)

    CALL RNG%ReInit()

    E(1) = 1.0 / 3.0
    Sum = E(1)
    DO  K = 2, 20
        E(K) = (27.0*(27.0/36.0)**(K-2) + 40.0*(26.0/36.0)**(K-2) + &
                55.0*(25.0/36.0)**(K-2)) / 648.0
        Sum = Sum + E(K)
    END DO
    E(21) = 1.0 - Sum
    NG = 200000
    NWins = 0
    NT(1:21) = 0
    DO  I = 1, NG
        LP = KThrow(RNG)
        NThrows = 1
        SkipLoop30 = FalseVal
        IF (LP == 7 .OR. LP == 11) THEN
            IWin = 1
            SkipLoop30 = TrueVal
        END IF
        IF (LP == 2 .OR. LP == 3 .OR. LP == 12) THEN
            IWin = 0
            SkipLoop30 = TrueVal
        END IF
        IF (.NOT.SkipLoop30) THEN
            Loop30: DO
                K = KThrow(RNG)
                NThrows = NThrows + 1
                IF (K == 7) THEN
                    IWin = 0
                    EXIT Loop30
                END IF
                IF (K == LP) THEN
                    IWin = 1
                    EXIT Loop30
                END IF
            END DO Loop30
        END IF
        M = MIN(21, NThrows)
        NT(M) = NT(M) + 1
        NWins = NWins + IWin
    END DO
    Avg = 244.0 * NG / 495.0
    StdDev = SQRT(Avg*251.0/495.0)
    T = (NWins-Avg) / StdDev
    WRITE (*, 5100)       RngName
    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5200)       NWins, Avg
    WRITE (OutUnit, 5200) NWins, Avg
    PWins = Phi(T)
    WRITE (*, 5300)       NWins, T, PWins
    WRITE (OutUnit, 5300) NWins, T, PWins
    Sum = 0.
    DO  I = 1, 21
        Expected = NG * E(I)
        Sum = Sum + (NT(I)-Expected) ** 2 / Expected
    END DO
    PThrows = ChiSquare(Sum,20)
    WRITE (OutUnit, 5400) Sum, PThrows
    WRITE (*, 5400)       Sum, PThrows
    Sum = 0
    DO  I = 1, 21
        Expected = NG * E(I)
        ChiSqr = (NT(I)-Expected) ** 2 / Expected
        Sum = Sum + ChiSqr
        WRITE (OutUnit, 5500) I, NT(I), Expected, ChiSqr, Sum
        WRITE (*, 5500)       I, NT(I), Expected, ChiSqr, Sum
    END DO
    WRITE (OutUnit, 5600) RngName
    WRITE (*, 5600)       RngName
    WRITE (OutUnit, 5700) PWins
    WRITE (*, 5700)       PWins
    WRITE (OutUnit, 5800) PThrows
    WRITE (*, 5800)       PThrows
    WRITE (*, 5900)
    WRITE (OutUnit, 5900)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (T16, ' Results of craps test for ', A20/   &
             '  No. of wins:  Observed Expected')
5200 FORMAT (T16, '          ',I12, F12.2)
5300 FORMAT (T16, I8, '= No. of wins, z-score=', F6.3, ' pvalue=', F7.5/   &
             '   Analysis of Throws-per-Game:')
5400 FORMAT (' Chisq=', F7.2, ' for 20 degrees of freedom, p= ', F8.5/   &
             T16, 'Throws Observed Expected  Chisq     Sum')
5500 FORMAT (I19, I9, F11.1, F8.3, F9.3)
5600 FORMAT ('            SUMMARY  FOR ', A15)
5700 FORMAT (T16, ' p-value for no. of wins:', F8.6)
5800 FORMAT (T16, ' p-value for throws/game:', F8.6)
5900 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT15_Craps_FromRNG

!******************************************************************************

FUNCTION KThrow_FromFile(InpRec) RESULT(ResVal)

    ! function used by 'CRAPS' test

    ! arguments
    TYPE(I32Data), INTENT(INOUT)    :: InpRec
    tInteger                        :: ResVal

    ! execution
    tSingle, PARAMETER  :: CC = 6.0*0.5**32

    ! execution

    ResVal = 2 + ToInteger(CC*InpRec%GetSample()+3.0) + ToInteger(CC*InpRec%GetSample()+3.0)

    RETURN

END FUNCTION KThrow_FromFile

!******************************************************************************

FUNCTION KThrow_FromRNG(RNG) RESULT(ResVal)

    ! function used by 'CRAPS' test

    ! arguments
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    tInteger                            :: ResVal

    ! execution
    tSingle, PARAMETER  :: CC = 6.0*0.5**32

    ! execution

    ResVal = 2 + ToInteger(CC*GetSample(RNG)+3.0) + ToInteger(CC*GetSample(RNG)+3.0)

    RETURN

END FUNCTION KThrow_FromRNG

!******************************************************************************

END SUBMODULE SubTest_Craps

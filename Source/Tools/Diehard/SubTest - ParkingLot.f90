
SUBMODULE (ModTest_Diehard) SubTest_ParkingLot

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "ParkingLot" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(22) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::               THIS IS A PARKING LOT TEST                      ::", &
        "     :: In a square of side 100, randomly 'park' a car---a circle of  ::", &
        "     :: radius 1.   Then try to park a 2nd, a 3rd, and so on, each    ::", &
        "     :: time parking 'by ear'.  That is, if an attempt to park a car  ::", &
        "     :: causes a crash with one already parked, try again at a new    ::", &
        "     :: random location. (To avoid path problems, consider parking    ::", &
        "     :: helicopters rather than cars.)   Each attempt leads to either ::", &
        "     :: a crash or a success, the latter followed by an increment to  ::", &
        "     :: the list of cars already parked. If we plot n:  the number of ::", &
        "     :: attempts, versus k::  the number successfully parked, we get a::", &
        "     :: curve that should be similar to those provided by a perfect   ::", &
        "     :: random number generator.  Theory for the behavior of such a   ::", &
        "     :: random curve seems beyond reach, and as graphics displays are ::", &
        "     :: not available for this battery of tests, a simple characteriz ::", &
        "     :: ation of the random experiment is used: k, the number of cars ::", &
        "     :: successfully parked after n=12,000 attempts. Simulation shows ::", &
        "     :: that k should average 3523 with sigma 21.9 and is very close  ::", &
        "     :: to normally distributed.  Thus (k-3523)/21.9 should be a st-  ::", &
        "     :: andard normal variable, which, converted to a uniform varia-  ::", &
        "     :: ble, provides input to a KSTEST based on a sample of 10.      ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]
    tInteger,     PARAMETER :: NTries = 12000
    tSingle,      PARAMETER :: SQ = 100.0
    tInteger,     PARAMETER :: NT = 10

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

MODULE SUBROUTINE FT09_ParkingLot_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "ParkingLot" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: X(4000), Y(4000), G(10)
    tInteger        :: I, IJ, K, N
    tSingle         :: Avg, PP, S, Sigma, SS, W, Z

    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:22)
    WRITE (OutUnit, 5000) TestDescription(1:22)

    InpRec%RecNum = 1
    S = 0.0
    SS = 0.0

    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5100)       InpFileName

    DO  IJ = 1, NT
        X(1) = SQ * Uni(InpRec)
        Y(1) = SQ * Uni(InpRec)
        K = 1
        Loop20:  DO  N = 1, NTries
            Z = SQ * Uni(InpRec)
            W = SQ * Uni(InpRec)
            DO I= 1, K
                IF (ABS(X(I)-Z) <= 1.0 .AND. ABS(Y(I)-W) <= 1.0) CYCLE Loop20
            END DO
            K = K + 1
            X(K) = Z
            Y(K) = W
        END DO Loop20
        S = S + K
        SS = SS + K * K
        Z = (K-3523.0) / 21.9
        G(IJ) = Phi(Z)
        WRITE (*, 5200)       K, Z, G(IJ)
        WRITE (OutUnit, 5200) K, Z, G(IJ)
    END DO
    Avg = S / NT
    Sigma = SS / NT - Avg ** 2
    WRITE (OutUnit, *)
    WRITE (*, 5300)       SQ, Avg, SQRT(Sigma)
    WRITE (OutUnit, 5300) SQ, Avg, SQRT(Sigma)
    CALL KS_Test(G, 10, PP)
    WRITE (*, 5400)       PP
    WRITE (OutUnit, 5400) PP
    WRITE (*, 5500)
    WRITE (OutUnit, 5500)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (T11, ' CDPARK: result of ten tests on file ', A15/    &
             T11, '  Of 12,000 tries, the average no. of successes'/   &
             T16, '  should be 3523 with sigma=21.9')
5200 FORMAT (T11, '  Successes:',i5,'    z-score:', F7.3, ' p-value: ', F8.6 )
5300 FORMAT (T11, ' square size   avg. no.  parked   sample sigma'/   &
             T11, F7.0, F20.3, F13.3)
5400 FORMAT ('            KSTEST for the above 10: p= ', F8.6)
5500 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )

END SUBROUTINE FT09_ParkingLot_FromFile

!******************************************************************************

MODULE SUBROUTINE FT09_ParkingLot_FromRNG(OutUnit, RNG)

    ! To perform the "ParkingLot" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: X(4000), Y(4000), G(10)
    tInteger        :: I, IJ, K, N
    tSingle         :: Avg, PP, S, Sigma, SS, W, Z

    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:22)
    WRITE (OutUnit, 5000) TestDescription(1:22)

    CALL RNG%ReInit()
    S = 0.0
    SS = 0.0

    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5100)       RngName

    DO  IJ = 1, NT
        X(1) = SQ * Uni(RNG)
        Y(1) = SQ * Uni(RNG)
        K = 1
        Loop20:  DO  N = 1, NTries
            Z = SQ * Uni(RNG)
            W = SQ * Uni(RNG)
            DO I= 1, K
                IF (ABS(X(I)-Z) <= 1.0 .AND. ABS(Y(I)-W) <= 1.0) CYCLE Loop20
            END DO
            K = K + 1
            X(K) = Z
            Y(K) = W
        END DO Loop20
        S = S + K
        SS = SS + K * K
        Z = (K-3523.0) / 21.9
        G(IJ) = Phi(Z)
        WRITE (*, 5200)       K, Z, G(IJ)
        WRITE (OutUnit, 5200) K, Z, G(IJ)
    END DO
    Avg = S / NT
    Sigma = SS / NT - Avg ** 2
    WRITE (OutUnit, *)
    WRITE (*, 5300)       SQ, Avg, SQRT(Sigma)
    WRITE (OutUnit, 5300) SQ, Avg, SQRT(Sigma)
    CALL KS_Test(G, 10, PP)
    WRITE (*, 5400)       PP
    WRITE (OutUnit, 5400) PP
    WRITE (*, 5500)
    WRITE (OutUnit, 5500)

    RETURN

5000 FORMAT (A78)
5100 FORMAT (T11, ' CDPARK: result of ten tests on file ', A15/    &
             T11, '  Of 12,000 tries, the average no. of successes'/   &
             T16, '  should be 3523 with sigma=21.9')
5200 FORMAT (T11, '  Successes:',i5,'    z-score:', F7.3, ' p-value: ', F8.6 )
5300 FORMAT (T11, ' square size   avg. no.  parked   sample sigma'/   &
             T11, F7.0, F20.3, F13.3)
5400 FORMAT ('            KSTEST for the above 10: p= ', F8.6)
5500 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
     
END SUBROUTINE FT09_ParkingLot_FromRNG

!******************************************************************************

END SUBMODULE SubTest_ParkingLot

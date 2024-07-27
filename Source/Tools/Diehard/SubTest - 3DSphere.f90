
SUBMODULE (ModTest_Diehard) SubTest_3DSphere

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform "3DSpheres" test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(12) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::              THE 3DSPHERES TEST                               ::", &
        "     :: Choose  4000 random points in a cube of edge 1000.  At each   ::", &
        "     :: point, center a sphere large enough to reach the next closest ::", &
        "     :: point. Then the volume of the smallest such sphere is (very   ::", &
        "     :: close to) exponentially distributed with mean 120pi/3.  Thus  ::", &
        "     :: the radius cubed is exponential with mean 30. (The mean is    ::", &
        "     :: obtained by extensive simulation).  The 3DSPHERES test gener- ::", &
        "     :: ates 4000 such spheres 20 times.  Each min radius cubed leads ::", &
        "     :: to a uniform variable by means of 1-exp(-r^3/30.), then a     ::", &
        "     ::  KSTEST is done on the 20 p-values.                           ::", &
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

MODULE SUBROUTINE FT11_3DSphere_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the "3DSpheres" test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tSingle         :: X(4000), Y(4000), Z(4000), P(20)
    tInteger        :: I, IJ, J, N
    tSingle         :: D, DMin, PV, R3, U, V, W
    
    ! execution

    ! write test description
    WRITE (*, 5000)       TestDescription(1:12)
    WRITE (OutUnit, 5000) TestDescription(1:12)

    N = 4000

    InpRec%RecNum = 1
    WRITE (OutUnit, 5100) InpFileName
    WRITE (*, 5100)       InpFileName
    DO  IJ = 1, 20
        DMin = 10000000.0
        DO  I = 1, N
            X(I) = 500.0 + 0.2328306E-6 * InpRec%GetSample()
        END DO
        CALL ASort(X(1:N))
        DO  I = 1, N
            Y(I) = 500.0 + 0.2328306E-6 * InpRec%GetSample()
            Z(I) = 500.0 + 0.2328306E-6 * InpRec%GetSample()
        END DO
        Loop40:  DO  I = 1, N
            U = X(I)
            V = Y(I)
            W = Z(I)
            DO  J = I + 1, N
                D = (U-X(J)) ** 2
                IF (D >= DMiN) CYCLE Loop40
                D = D + (V-Y(J)) ** 2 + (W-Z(J)) ** 2
                IF (D < DMin) DMin = D
            END DO
        END DO Loop40
        R3 = DMin * SQRT(DMin)
        P(IJ) = 1 - EXP(-R3/30.0)
        WRITE (OutUnit, 5200) IJ, R3, P(IJ)
        WRITE (*, 5200)       IJ, R3, P(IJ)
    END DO
    WRITE (OutUnit, 5300)
    WRITE (*, 5300)
    CALL KS_Test(P, 20, PV)
    WRITE (*, 5400)       InpFileName, PV
    WRITE (OutUnit, 5400) InpFileName, PV
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('               The 3DSPHERES test for ', A15)
5200 FORMAT (' sample no: ', I2, '     r^3= ', F7.3, '     p-value= ', F7.5)
5300 FORMAT ('  A KS test is applied to those 20 p-values.'/   &
             '---------------------------------------------------------')
5400 FORMAT ('       3DSPHERES test for ', A15, '      p-value= ', F8.6/  &
             '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
     
END SUBROUTINE FT11_3DSphere_FromFile

!******************************************************************************

MODULE SUBROUTINE FT11_3DSphere_FromRNG(OutUnit, RNG)

    ! To perform the "3DSpheres" test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tCharAlloc      :: RngName
    tSingle         :: X(4000), Y(4000), Z(4000), P(20)
    tInteger        :: I, IJ, J, N
    tSingle         :: D, DMin, PV, R3, U, V, W
    
    ! execution

    RngName = RNG%GetName()

    ! write test description
    WRITE (*, 5000)       TestDescription(1:12)
    WRITE (OutUnit, 5000) TestDescription(1:12)

    N = 4000

    CALL RNG%ReInit()
    WRITE (OutUnit, 5100) RngName
    WRITE (*, 5100)       RngName
    DO  IJ = 1, 20
        DMin = 10000000.0
        DO  I = 1, N
            X(I) = 500.0 + 0.2328306E-6 * GetSample(RNG)
        END DO
        CALL ASort(X(1:N))
        DO  I = 1, N
            Y(I) = 500.0 + 0.2328306E-6 * GetSample(RNG)
            Z(I) = 500.0 + 0.2328306E-6 * GetSample(RNG)
        END DO
        Loop40:  DO  I = 1, N
            U = X(I)
            V = Y(I)
            W = Z(I)
            DO  J = I + 1, N
                D = (U-X(J)) ** 2
                IF (D >= DMiN) CYCLE Loop40
                D = D + (V-Y(J)) ** 2 + (W-Z(J)) ** 2
                IF (D < DMin) DMin = D
            END DO
        END DO Loop40
        R3 = DMin * SQRT(DMin)
        P(IJ) = 1 - EXP(-R3/30.0)
        WRITE (OutUnit, 5200) IJ, R3, P(IJ)
        WRITE (*, 5200)       IJ, R3, P(IJ)
    END DO
    WRITE (OutUnit, 5300)
    WRITE (*, 5300)
    CALL KS_Test(P, 20, PV)
    WRITE (*, 5400)       RngName, PV
    WRITE (OutUnit, 5400) RngName, PV
    
    RETURN

5000 FORMAT (A78)
5100 FORMAT ('               The 3DSPHERES test for ', A15)
5200 FORMAT (' sample no: ', I2, '     r^3= ', F7.3, '     p-value= ', F7.5)
5300 FORMAT ('  A KS test is applied to those 20 p-values.'/   &
             '---------------------------------------------------------')
5400 FORMAT ('       3DSPHERES test for ', A15, '      p-value= ', F8.6/  &
             '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/ )
          
END SUBROUTINE FT11_3DSphere_FromRNG

!******************************************************************************

END SUBMODULE SubTest_3DSphere


SUBMODULE (ModTest_Diehard) SubTest_Operm5

!** PURPOSE OF THIS MODULE:
    ! contains routines to perform 'Overlapping 5-Permutation' test.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tCharLen(80), PARAMETER :: TestDescription(15) = [                              &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", &
        "     ::            THE OVERLAPPING 5-PERMUTATION TEST                 ::", &
        "     :: This is the OPERM5 test.  It looks at a sequence of one mill- ::", &
        "     :: ion 32-bit random integers.  Each set of five consecutive     ::", &
        "     :: integers can be in one of 120 states, for the 5! possible or- ::", &
        "     :: derings of five numbers.  Thus the 5th, 6th, 7th,...numbers   ::", &
        "     :: each provide a state. As many thousands of state transitions  ::", &
        "     :: are observed,  cumulative counts are made of the number of    ::", &
        "     :: occurences of each state.  Then the quadratic form in the     ::", &
        "     :: weak inverse of the 120x120 covariance matrix yields a test   ::", &
        "     :: equivalent to the likelihood ratio test that the 120 cell     ::", &
        "     :: counts came from the specified (asymptotically) normal dis-   ::", &
        "     :: tribution with the specified 120x120 covariance matrix (with  ::", &
        "     :: rank 99).  This version uses 1,000,000 integers, twice.       ::", &
        "     :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::"]

#include "Includes/OPerm5_Param.f90"

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

MODULE SUBROUTINE FT02_Overlap5Permute_FromFile(InpFileName, OutUnit, InpRec)

    ! To perform the 'Overlapping 5-Permutation' Test based on the specified input file

    ! arguments
    tCharLen(25),   INTENT(IN)      :: InpFileName
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(I32Data), INTENT(INOUT)   :: InpRec
    
    ! local variables
    tInteger        :: T(120)
    tInteger        :: U(1005)
    tInteger        :: I, IJKM, J, K, N
    tSingle         :: AV, ChiSqr, X, Y
    
    ! execution

    !*** overlapping 5-permutations.  Uses 120x120 weak inverse *******
    !*** of covariance matrix (in 60x60 blocks).
    !***  69069 passes, Randu fails, Weyl fails, SR(15,17),SR(13,18) fail.
    !***  F(2,1,*) and F(3,1,*) pass

    ! write test description
    WRITE (*, 5100)       TestDescription(1:15)
    WRITE (OutUnit, 5100) TestDescription(1:15)

    !**** divide r and s elements by (200000*n) for proper cov. inverse
    !****    the Rank is 99=50+49.

    DO IJKM = 1, 2
        !**********************get counts T(1),...,T(120)******************
        InpRec%RecNum = 1
        N = 1000
        T(1:120) = 0
        DO  I = 1001, 1005
            U(I) = InpRec%GetSample()
        END DO
        DO  I = 1, N
            DO  J = 1, 5
                U(J) = U(1000+J)
            END DO
            DO  J = 1, 1000
                K = Kp(U(J)) + 1
                T(K) = T(K) + 1
                U(J+5) = InpRec%GetSample()
            END DO
        END DO
        !*********************evalute quadratic form in weak inverse*******
        ChiSqr = 0.0
        AV = N * 2000.0 / 120.0
        DO  I = 1, 60
            X = T(I) + T(I+60) - AV
            Y = T(I) - T(I+60)
            DO  J = 1, 60
              ChiSqr = ChiSqr + X*R(I, J)*(T(J) + T(J+60) - AV) + Y*S(I,J)*(T(J) - T(J+60))
            END DO
        END DO
        ChiSqr = ChiSqr / (2.0E08*N)
        WRITE (OutUnit,5200) InpFileName
        WRITE (*, 5200)      InpFileName
        WRITE (OutUnit,5300) ChiSqr, ChiSquare(ChiSqr,99)
        WRITE (*, 5300)      ChiSqr, ChiSquare(ChiSqr,99)
    END DO
    WRITE (*, 5700)
    WRITE (OutUnit,5700)
    
    RETURN

5000 FORMAT (8I10)
5100 FORMAT (A78)
5200 FORMAT ('           OPERM5 test for ', A20/   &
             '     For a sample of 1,000,000 consecutive 5-tuples,')
5300 FORMAT (' ChiSquare for 99 degrees of freedom = ', F7.3,'; p-value = ', F8.6)
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT02_Overlap5Permute_FromFile

!******************************************************************************

MODULE SUBROUTINE FT02_Overlap5Permute_FromRNG(OutUnit, RNG)

    ! To perform the 'Overlapping 5-Permutation' Test based on the specified generator

    ! arguments
    tInteger,       INTENT(IN)      :: OutUnit
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG
    
    ! local variables
    tInteger        :: T(120)
    tInteger        :: U(1005)
    tInteger        :: I, IJKM, J, K, N
    tSingle         :: AV, ChiSqr, X, Y
    tCharAlloc      :: RngName
    
    ! execution
    
    RngName = RNG%GetName()
    
    !*** overlapping 5-permutations.  Uses 120x120 weak inverse *******
    !*** of covariance matrix (in 60x60 blocks).
    !***  69069 passes, Randu fails, Weyl fails, SR(15,17),SR(13,18) fail.
    !***  F(2,1,*) and F(3,1,*) pass

    ! write test description
    WRITE (*, 5100)       TestDescription(1:15)
    WRITE (OutUnit, 5100) TestDescription(1:15)

    !**** divide r and s elements by (200000*n) for proper cov. inverse
    !****    the Rank is 99=50+49.

    DO IJKM = 1, 2
        !**********************get counts T(1),...,T(120)******************
        CALL RNG%ReInit()
        N = 1000
        T(1:120) = 0
        DO  I = 1001, 1005
            U(I) = GetSample(RNG)
        END DO
        DO  I = 1, N
            DO  J = 1, 5
                U(J) = U(1000+J)
            END DO
            DO  J = 1, 1000
                K = Kp(U(J)) + 1
                T(K) = T(K) + 1
                U(J+5) = GetSample(RNG)
            END DO
        END DO
        !*********************evalute quadratic form in weak inverse*******
        ChiSqr = 0.0
        AV = N * 2000.0 / 120.0
        DO  I = 1, 60
            X = T(I) + T(I+60) - AV
            Y = T(I) - T(I+60)
            DO  J = 1, 60
              ChiSqr = ChiSqr + X*R(I, J)*(T(J) + T(J+60) - AV) + Y*S(I,J)*(T(J) - T(J+60))
            END DO
        END DO
        ChiSqr = ChiSqr / (2.0E08*N)
        WRITE (OutUnit,5200) RngName
        WRITE (*, 5200)      RngName
        WRITE (OutUnit,5300) ChiSqr, ChiSquare(ChiSqr,99)
        WRITE (*, 5300)      ChiSqr, ChiSquare(ChiSqr,99)
    END DO
    WRITE (*, 5700)
    WRITE (OutUnit,5700)
    
    RETURN

5000 FORMAT (8I10)
5100 FORMAT (A78)
5200 FORMAT ('           OPERM5 test for ', A20/   &
             '     For a sample of 1,000,000 consecutive 5-tuples,')
5300 FORMAT (' ChiSquare for 99 degrees of freedom = ', F7.3,'; p-value = ', F8.6)
5700 FORMAT (/ '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/)

END SUBROUTINE FT02_Overlap5Permute_FromRNG

!******************************************************************************

FUNCTION Kp(C) RESULT(ResVal)

    ! routine used by 'OVERLAPPING 5-PERMUTATION' test

    ! arguments
    tInteger, INTENT(IN)    :: C(5)
    tInteger                :: ResVal
    
    ! parameters
    tInteger, PARAMETER  :: Map(0:59) = [                               &
        39, 38, 37, 36, 41, 40, 54, 55, 56, 57, 58, 59, 49, 48,         &
        52, 53, 50, 51, 42, 43, 44, 45, 46, 47, 33, 32, 31, 30, 35,     &
        34, 12, 13, 14, 15, 16, 17, 29, 28, 24, 25, 27, 26, 21, 20,     &
        19, 18, 23, 22,  2,  3,  5,  4,  1,  0, 10, 11,  9,  8,  6, 7]

    ! local variables
    tInteger  :: B(5), I, J, L, S, T

    ! execution

    B(1:5) = C(1:5)
    ResVal = 0
    DO  I = 5, 2, -1
        T = B(1)
        L = 1
        DO  J = 2, I
            IF (B(J) >= T) THEN
                T = B(J)
                L = J
            END IF
        END DO
        ResVal = I * ResVal + L - 1
        S = B(I)
        B(I) = B(L)
        B(L) = S
    END DO
    IF (ResVal < 60) ResVal = Map(ResVal)

    RETURN
    
END FUNCTION Kp

!******************************************************************************

END SUBMODULE SubTest_Operm5

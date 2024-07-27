
SUBMODULE (ModBase_ProbDist) SubBase_FBar

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule is similar to the submodule "SubBase_FDist", except that it provides
    ! procedures to compute or approximate the complementary distribution function of X,
    ! which we define as FBar(x) = P[X >= x], instead of F(x) = P[X <= x].  Note that 
    ! with our definition of FBar, one has FBar(x) = 1 - F(x) for continuous distributions
    ! and FBar(x) = 1 - F(x-1) for discrete distributions over the integers.  This is
    ! non-standard but we find it convenient.
    ! For more details about the specific distributions, see the submodule "SubBase_FDist".
    ! When F(x) is very close to 1, these procedures generally provide much more precise
    ! values of FBar(x) than using 1 - F(x) where F(x) is computed by a procedure from
    ! the submodule "SubBase_FDist".

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubBase_FBar'
    ! Compute IMAX extra terms in the tails of discrete distributions
    tIndex,    PARAMETER    :: IMAX = 20

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

MODULE FUNCTION fbar_Unif(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return 1 - X for X in [0, 1], return 1 for X < 0, and return 0 for X > 1.
    ! This is the complementary uniform distribution function over [0, 1].

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= ONE_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = ONE_DBL - X
    END IF

    RETURN

END FUNCTION fbar_Unif

!******************************************************************************
MODULE FUNCTION fbar_Expon(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary exponential distribution: FBar(X) = EXP(-X)
    ! for X > 0, and 1 for X <= 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = EXP(-X)
    END IF

    RETURN

END FUNCTION fbar_Expon

!******************************************************************************

MODULE FUNCTION fbar_Weibull(Alpha, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary standard Weibull distribution function 
    ! with shape parameter Alpha, defined by FBar(X) = EXP(-X**Alpha) for
    ! X > 0, and 1 for X <= 0.  Restriction:  Alpha > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Alpha, X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y

! FLOW

    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_Weibull', SubModName, ErrWarning, 'Alpha <= 0.')
        RETURN
    END IF
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF ((X >= DBL_MAX_EXP * FLT_RADIX).AND.(Alpha >= ONE_DBL)) THEN
        ResVal = ZERO_DBL
    ELSE
        Y = Alpha*LOG(X)
        IF (Y >= DBL_MAX_EXP * num_Ln2) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = EXP(-EXP(Y))
        END IF
    END IF

    RETURN

END FUNCTION fbar_Weibull

!******************************************************************************

MODULE FUNCTION fbar_Logistic(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary standard logistic distribution
    ! function evaluated at x.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= -fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = EXP(-X)
    ELSE
        ResVal = ONE_DBL / (ONE_DBL + EXP(X))
    END IF

    RETURN

END FUNCTION fbar_Logistic

!******************************************************************************

MODULE FUNCTION fbar_Pareto(C, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return F(X) = 1/X**C for X >= 1, and 1 for X < 1, which is the
    ! complementary standard Pareto distribution function.  Restriction:  C > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: C, X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (C <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_Pareto', SubModName, ErrWarning, 'C <= 0.')
        RETURN
    END IF
    
    IF (X <= ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = X**(-C)
    END IF

    RETURN

END FUNCTION fbar_Pareto

!******************************************************************************

MODULE FUNCTION fbar_Normal1(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of 1 - Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses a Chebyshev series
    ! giving 16 decimal digits of precision.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: A(0:24) = [                      &
        6.10143081923200418D-1,  -4.34841272712577472D-1,   &
        1.76351193643605501D-1,  -6.07107956092494149D-2,   &
        1.77120689956941145D-2,  -4.32111938556729382D-3,   &
        8.54216676887098679D-4,  -1.27155090609162743D-4,   &
        1.12481672436711895D-5,   3.13063885421820973D-7,   &
       -2.70988068537762022D-7,   3.07376227014076884D-8,   &
        2.51562038481762294D-9,  -1.02892992132031913D-9,   &
        2.99440521199499394D-11,  2.60517896872669363D-11,  &
       -2.63483992417196939D-12, -6.43404509890636443D-13,  &
        1.12457401801663447D-13,  1.7281533389986098D-14,   &
       -4.2641016949424D-15,     -5.4537197788D-16,         &
        1.5869760776D-16,         2.08998378D-17,           &
       -0.5900D-17]
    tDouble, PARAMETER  :: KK = 5.30330085889910643300_kDouble      ! 3.75 Sqrt(2)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y, T, X2
    tLogical    :: Negative

! FLOW

    ! Returns P[X >= x] = 1 - F(x) where F is the normal distribution by
    ! computing the complementary distribution directly; it is thus more
    ! precise in the tail.

    IF (X >= fdist_XBIG) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X <= -fdist_XBIG) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    IF (X >= ZERO_DBL) THEN
        Negative = FalseVal
        X2 = X
    ELSE
        Negative = TrueVal
        X2 = -X
    END IF

    T = (X2 - KK) / (X2 + KK)
    Y = num2_EvalCheby (A, 24, T)
    Y = Y * EXP (-X2 * X2 / TWO_DBL) / TWO_DBL

    IF (Negative) THEN
        ResVal = ONE_DBL - Y
    ELSE
        ResVal = Y
    END IF

    RETURN

END FUNCTION fbar_Normal1

!******************************************************************************

MODULE FUNCTION fbar_Normal2(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of 1 - Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses Marsaglia’s fast
    ! method [1] with tables lookup.  Returns 15 decimal digits of precision.
    ! This function is approximately 1.3 times faster than "fbar_Normal1".

!** REFERENCE:
    ! [1] G. Marsaglia, A. Zaman, and J. C. W. Marsaglia. Rapid evaluation of
    !     the inverse normal distribution function. Statistics and Probability
    !     Letters, 19:259–266, 1994.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: V(0:120) = [                                      &
            1.2533141373155,      1.137490921203605,      1.037824575853727, &
          0.951527192071207,     0.8763644564536924,     0.8105337152790306, &
         0.7525711790634081,     0.7012808218544303,     0.6556795424187987, &
           0.61495459615093,     0.5784303460476312,     0.5455421356582171, &
         0.5158156382179634,     0.4888504415275737,     0.4643069280394423, &
         0.4418957328326002,     0.4213692292880546,     0.4025146181296722, &
         0.3851482907984348,     0.3691112106902635,     0.3542651113297938, &
         0.3404893532870847,     0.3276783146905521,       0.31573921586941, &
         0.3045902987101033,     0.2941592970402893,      0.284382146748493, &
         0.2752018941576065,     0.2665677689682238,     0.2584343943120386, &
         0.2507611114439651,      0.243511400615456,     0.2366523829135607, &
          0.230154390478801,     0.2239905946538289,     0.2181366833614714, &
         0.2125705804420318,     0.2072722008565011,     0.2022232366330547, &
         0.1974069692375194,     0.1928081047153158,     0.1884126285076003, &
         0.1842076773079702,     0.1801814257143918,     0.1763229857571027, &
         0.1726223176578506,     0.1690701504076941,     0.1656579109468773, &
         0.1623776608968675,     0.1592220399363674,     0.1561842150339759, &
          0.153257834853479,     0.1504369887362691,     0.1477161697413935, &
          0.145090241289131,     0.1425544070104023,     0.1401041834530503, &
         0.1377353753382303,     0.1354440530967635,     0.1332265324471292, &
         0.1310793558044918,     0.1289992753343376,      0.126983237485437, &
         0.1250283688553504,     0.1231319632579323,     0.1212914698765462, &
          0.119504482399253,     0.1177687290432979,     0.1160820633859823, &
         0.1144424559276431,      0.112847986320103,     0.1112968362007359, &
         0.1097872825783083,     0.1083176917221132,     0.1068865135106745, &
         0.1054922762005562,     0.1041335815795983,     0.1028091004723001, &
         0.1015175685681028,     0.1002577825460485,    0.09902859647173194, &
        0.09782891844465691,    0.09665770747608191,    0.09551397057921558, &
        0.09439676005522439,    0.09330517095996169,    0.09223833873763035, &
        0.09119543700877471,    0.09017567550106469,    0.08917829811230435, &
        0.08820258109597616,    0.08724783136042988,    0.08631338487354936, &
        0.08539860516539227,    0.08450288192189578,    0.08362562966329139, &
        0.08276628650136918,    0.08192431297018954,    0.08109919092525536, &
        0.08029042250654048,    0.07949752916111721,    0.07872005072144664, &
        0.07795754453568722,    0.07720958464664668,    0.07647576101624852, &
        0.07575567879261112,    0.07504895761704659,    0.07435523096847724, &
        0.07367414554294564,    0.07300536066605566,    0.07234854773633338, &
        0.07170338969763433,    0.07106958053885212,    0.07044682481930167, &
        0.06983483721825942,    0.06923334210724434,    0.06864207314371742, &
        0.06806077288496332,     0.0674891924209997,    0.06692709102543307, &
        0.06637423582325017]
    tDouble, PARAMETER  :: KK = 5.30330085889910643300_kDouble      ! 3.75 Sqrt(2)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T, U, Z, H, X2
    tDouble     :: R, R1, R2, R3, R4, R5, R6, R7, R8
    tLogical    :: Negative
    tInteger    :: J

! FLOW

    IF (X >= fdist_XBIG) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X <= -fdist_XBIG) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    IF (X >= ZERO_DBL) THEN
        Negative = FalseVal
        X2 = X
    ELSE
        Negative = TrueVal
        X2 = -X
    END IF

    J = ToInteger(8.0_kDouble * X2 + HALF_DBL)
    IF (J > 120) J = 120
    Z = 0.125_kDouble * J
    H = X2 - Z
    R = V(J)
    R1 = R * Z - ONE_DBL
    R2 = HALF_DBL * (R + Z * R1)
    R3 = (R1 + Z * R2) / 3.0
    R4 = 0.25_kDouble * (R2 + Z * R3)
    R5 = 0.2_kDouble * (R3 + Z * R4)
    R6 = (R4 + Z * R5) / 6.0_kDouble
    R7 = (R5 + Z * R6) / 7.0_kDouble
    R8 = 0.125 * (R6 + Z * R7)
    T = R + H * (R1 + H * (R2 + H * (R3 + H * (R4 + H * (R5 + H * (R6 + H * (R7 + H * R8)))))))
    U = T * EXP (-HALF_DBL * X2 * X2 - 0.9189385332046727_kDouble)
    IF (Negative) THEN
        ResVal = ONE_DBL - U
    ELSE
        ResVal = U
    END IF

    RETURN

END FUNCTION fbar_Normal2

!******************************************************************************

MODULE FUNCTION fbar_Normal3(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of 1 - Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses the FORTRAN
    ! intrinsic ERFC function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = HALF_DBL * ERFC (X * num_1Rac2)

    RETURN

END FUNCTION fbar_Normal3

!******************************************************************************

MODULE FUNCTION fbar_BiNormal1(X, Y, Rho, NDig) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value u of the upper standard bivariate normal distribution,
    ! given by Eq (38) in [1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: X, Y, Rho
    tInteger, INTENT(IN)    :: NDig
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = fdist_BiNormal1(-X, -Y, Rho, NDig)

    RETURN

END FUNCTION fbar_BiNormal1

!******************************************************************************

MODULE FUNCTION fbar_BiNormal2(X, Y, Rho) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value u of the upper standard bivariate normal distribution,
    ! given by Eq (38) in [1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: X, Y, Rho
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = fdist_BiNormal2(-X, -Y, Rho)

    RETURN

END FUNCTION fbar_BiNormal2

!******************************************************************************

MODULE FUNCTION fbar_LogNormal(Mu, Sigma, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary lognormal distribution function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Mu, Sigma, X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Sigma <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_LogNormal', SubModName, ErrWarning, 'Sigma <= 0.')
        RETURN
    END IF
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = fbar_Normal1 ((LOG (X) - Mu) / Sigma)
    END IF

    RETURN

END FUNCTION fbar_LogNormal

!******************************************************************************

MODULE FUNCTION fbar_JohnsonSB(Alpha, Beta, A, B, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Johnson JSB distribution function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Alpha, Beta, A, B, X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_JohnsonSB', SubModName, ErrWarning, 'Beta <= 0.')
        RETURN
    ELSEIF (B <= A) THEN
        CALL Handle_ErrLevel('fbar_JohnsonSB', SubModName, ErrWarning, 'B <= A.')
        RETURN
    END IF
    
    IF (X <= A) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= B) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = fbar_Normal1 (Alpha + Beta * LOG ((X - A) / (B - X)))
    END IF

    RETURN

END FUNCTION fbar_JohnsonSB

!******************************************************************************

MODULE FUNCTION fbar_JohnsonSU(Alpha, Beta, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Johnson JSU distribution function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Alpha, Beta, X
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: XLimit = 1.0D+10
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: R, X2
    tLogical    :: Negative

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_JohnsonSB', SubModName, ErrWarning, 'Beta <= 0.')
        RETURN
    END IF
    
    IF (X < ZERO_DBL) THEN
        X2 = -X
        Negative = TrueVal
    ELSE
        X2 = X
        Negative = FalseVal
    END IF
    
    ! compute r = x + sqrt (x * x + 1)
    IF (X2 < XLimit) THEN
        R = X2 + SQRT (X2 * X2 + ONE_DBL)
    ELSE
        R = TWO_DBL * X2
    END IF
    IF (Negative) R = ONE_DBL / R

    IF (R > 0.0) THEN
        ResVal = fbar_Normal1 (Alpha + Beta * LOG (R))
    ELSE
        ResVal = ONE_DBL
    END IF

    RETURN

END FUNCTION fbar_JohnsonSU

!******************************************************************************

MODULE FUNCTION fbar_ChiSquare1(K, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary chi-square distribution function with K degrees of
    ! freedom.  Uses the approximation given in [1, p.116] for K <= 1000, and the normal
    ! approximation for K > 1000.  Gives no more than 4 decimals of precision for K > 1000.

!** REFERENCE:
    ! [1] W. J. Kennedy Jr. and J. E. Gentle. Statistical Computing.
    !     Dekker, New York, NY, 1980.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: K
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: XBIG_CHI = 2000.0_kDouble
    tDouble, PARAMETER  :: Tiers = 0.33333333333333333_kDouble
    tDouble, PARAMETER  :: Pt2 = 0.22222222222222222_kDouble
    tDouble, PARAMETER  :: MoinsHuit = -8.3_kDouble
    tDouble, PARAMETER  :: Gamma = 0.8862269254527579825931_kDouble
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: H, E, HalfX, Term, Summation, Y, X2
    tIndex      :: I

! FLOW

    ! Returns an approximation of the complementary Chi square cdf (N degrees
    ! of freedom). Similar to p:116 of W.J.Kennedy Jr and J.E.Gentle.
    ! Statistical computing, Dekker, New York, 1980. More precise in the
    ! tail than simply returning  1 - fdist_ChiSquare.

    IF (K <= 0) THEN
        CALL Handle_ErrLevel('fbar_ChiSquare1', SubModName, ErrWarning, 'K <= 0')
        RETURN
    END IF
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF (K >= 150) THEN
        IF (X >= K * fdist_XBIG) THEN
            ResVal = ZERO_DBL
            RETURN
        END IF
    ELSE
        IF (X >= XBIG_CHI) THEN
            ResVal = ZERO_DBL
            RETURN
        END IF
    END IF

    IF (K > 1000) THEN
        IF (X < TWO_DBL) THEN
            ResVal = ONE_DBL
            RETURN
        END IF
        X2 = (((X / K)**Tiers) - (ONE_DBL - Pt2 / K)) / SQRT (Pt2 / K)
        IF (X2 > 35.0_kDouble) THEN
            ResVal = ZERO_DBL
        ELSEIF (X2 <= MoinsHuit) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = fbar_Normal1 (X2)
        END IF
        RETURN
    END IF

    HalfX = X / TWO_DBL
    IF (NOT(IAND(K, 1)) /= 0) THEN  ! even K
        Term = EXP (-HalfX)
        Summation = Term
        DO I = 1, K / 2 - 1
            Term = Term * HalfX / I
            Summation = Summation + Term
        END DO
        Y = Summation
    ELSE
        H = TWO_DBL * fbar_Normal1 (SQRT (X))
        IF (K == 1) THEN
            ResVal = H
            RETURN
        END IF
        E = EXP (-HalfX)
        Term = SQRT (HalfX) * E / Gamma
        DO I = 3, K-1, 2
            H = H + Term
            Term = Term * HalfX * TWO_DBL / I
        END DO
        Y = H + Term
    END IF

    IF (Y > ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSE 
        ResVal = Y
    END IF

    RETURN

END FUNCTION fbar_ChiSquare1

!******************************************************************************

MODULE FUNCTION fbar_ChiSquare2(K, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary chi-square distribution function with K degrees of
    ! freedom, by calling fbar_Gamma (K/2, D, X/2). The function will do its best to
    ! return D decimals digits of precision.  Restrictions: K > 0 and 0 < D <= 15.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: K
    tInteger, INTENT(IN)    :: D
    tDouble,  INTENT(IN)    :: X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (K <= 0) THEN
        CALL Handle_ErrLevel('fbar_ChiSquare2', SubModName, ErrWarning, 'K <= 0')
        RETURN
    END IF
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = fbar_Gamma (K / TWO_DBL, D, X / TWO_DBL)
    END IF

    RETURN

END FUNCTION fbar_ChiSquare2

!******************************************************************************

MODULE FUNCTION fbar_Gamma(Alpha, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the complementary gamma distribution function
    ! with parameter Alpha.  The function tries to return D decimals digits of
    ! precision.  For Alpha not too large (e.g. Alpha <= 1000), D gives a good idea
    ! of the precision attained.  Restrictions: Alpha > 0 and 0 < D <= 15.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: D
    tDouble,  INTENT(IN)    :: Alpha, X
    tDouble                 :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: AlphaLimit = 1.0D+5
    tDouble, PARAMETER  :: ReNorm     = 1.0D+100

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: VArr(0:5)
    tDouble     :: V, A, B, R, Term, Diff, Eps, Res
    tInteger    :: I

! FLOW

    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_Gamma', SubModName, ErrWarning, 'Alpha <= 0')
        RETURN
    ELSEIF (D <= 0) THEN
        CALL Handle_ErrLevel('fbar_Gamma', SubModName, ErrWarning, 'D <= 0')
        RETURN
    ELSEIF (D > 15) THEN
        CALL Handle_ErrLevel('fbar_Gamma', SubModName, ErrWarning, 'D > 15')
        RETURN
    END IF
    
    Eps = EpsArray(D)
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (Alpha == ONE_DBL) THEN
        ResVal = fbar_Expon (X)
        RETURN
    END IF

    IF (Alpha >= 70.0_kDouble) THEN
        IF (X >= Alpha * fdist_XBIG) THEN
            ResVal = ZERO_DBL
            RETURN
        END IF
    ELSE
        IF (X >= fdist_XBIGM) THEN
            ResVal = ZERO_DBL
            RETURN
        END IF
    END IF

    IF (Alpha >= AlphaLimit) THEN
        BLOCK
            tDouble     :: D2, S, Z 
            D2 = X + ONE_DBL/3.0_kDouble - Alpha - 0.02_kDouble/Alpha
            S = Alpha - HALF_DBL
            Z = D2 * SQRT((ONE_DBL + fdist_belog(S/X))/X)
            ResVal = fbar_Normal1 (z)
            RETURN
        END BLOCK
    END IF

    IF ((X <= ONE_DBL).OR.(X < Alpha)) THEN
        ResVal = ONE_DBL - fdist_Gamma (Alpha, D, X)
        RETURN
    END IF

    V = EXP (Alpha * LOG (X) - X - num2_LnGamma (Alpha))

    A = ONE_DBL - Alpha
    B = A + X + ONE_DBL
    Term = ZERO_DBL
    VArr(0) = ONE_DBL
    VArr(1) = X
    VArr(2) = X + ONE_DBL
    VArr(3) = X * B
    Res = VArr(2) / VArr(3)

    DO
        A = A + ONE_DBL
        B = B + TWO_DBL
        Term = Term + ONE_DBL
        VArr(4) = B * VArr(2) - A * Term * VArr(0)
        VArr(5) = B * VArr(3) - A * Term * VArr(1)
        IF (VArr(5) /= ZERO_DBL) THEN
            R = VArr(4) / VArr(5)
            Diff = ABS (Res - R)
            IF (Diff <= Eps * R) THEN
                ResVal = (V * Res)
                RETURN
            END IF
            Res = R
        END IF
        DO I = 0, 3
            VArr(I) = VArr(I + 2)
        END DO
        IF (ABS (VArr(4)) >= ReNorm) THEN
            DO I = 0, 3
                VArr(I) = VArr(I) / ReNorm
            END DO
        END IF
    END DO

    ! to eliminate a warning from the compiler never reached
    ResVal = ZERO_DBL

    RETURN

END FUNCTION fbar_Gamma

!******************************************************************************

FUNCTION KSPlusbarAsymp(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the probability of the KSPlus distribution using an asymptotic formula.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: T, Z, V

! FLOW

    T = (6.0_kDouble*N*X + 1)
    Z = T*T/(18.0_kDouble*N)
    V = ONE_DBL - (TWO_DBL*Z*Z - 4.0_kDouble*Z - ONE_DBL)/(18.0_kDouble*N)
    IF (V <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        V = V*EXP(-Z)
        IF (V >= ONE_DBL) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = V
        END IF
    END IF
    
    RETURN

END FUNCTION KSPlusbarAsymp

!******************************************************************************

FUNCTION KSPlusbarUpper(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the probability of the KSPlus distribution in the upper
    ! tail using Smirnov's stable formula.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: Eps = 1.0D-10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Q, Sum, Term, T, LogCom, LogJMax
    tInteger    :: J, JMax

! FLOW

#undef  ToDouble
#define ToDouble(A)     REAL(A, KIND=kDouble)

    Sum = ZERO_DBL
    JMax = ToInteger(N - N*X)

    ! We must avoid log(0) for J = JMax and Q ~ 1.0
    IF ((ONE_DBL - X - ToDouble(JMax) / N) <= ZERO_DBL) THEN
        JMax = JMax - 1
    END IF

    J = JMax/2
    LogCom = num2_LnFactorial(ToInteger(N)) - num2_LnFactorial(J) - &
                num2_LnFactorial(ToInteger(N-J))
    LogJMax = LogCom

    DO WHILE (J > 0)
        Q = ToDouble(J) / N + X
        Term = LogCom + (J - 1)*LOG (Q) + (N - J)*num2_LOG1P (-Q)
        T = EXP (Term)
        Sum = Sum + T
        LogCom = LogCom + LOG (ToDouble(J) / ToDouble(N - J + 1))
        IF (T <= Sum*Eps) EXIT
        J = J - 1
    END DO

    J = JMax/2
    LogCom = LogJMax + LOG (ToDouble(N - J) / ToDouble(J + 1))
    J = J + 1

    DO WHILE (J <= JMax)
        Q = ToDouble(J) / N + X
        Term = LogCom + (J - 1)*LOG(Q) + (N - J)*num2_LOG1P(-Q)
        T = EXP (Term)
        Sum = Sum + T
        LogCom = LogCom + LOG (ToDouble(N - J) / ToDouble(J + 1))
        IF (T <= Sum*Eps) EXIT
        J = J + 1
    END DO

    Sum = Sum * X
    ! add the term J = 0
    ResVal = Sum + EXP (N*num2_LOG1P (-X))
        
    RETURN

#undef ToDouble

END FUNCTION KSPlusbarUpper

!******************************************************************************

MODULE FUNCTION fbar_KSPlus(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Kolmogorov-Smirnov+ distribution FBar(x) = P[D+n >= x]
    ! in a form that is more precise in the upper tail.  It should return at least 8 decimal
    ! digits of precision everywhere.  Restrictions: n > 0 and 0 <= x <= 1.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: NxParam = 6.5_kDouble    ! frontier: alternating series
    tIndex,  PARAMETER  :: NParam  = 4000           ! frontier: non-alternating series
    tIndex,  PARAMETER  :: NAsymp  = 200000         ! frontier: asymptotic

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fbar_KSPlus', SubModName, ErrWarning, 'N <= 0')
        RETURN
    END IF
    
    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF ((X >= ONE_DBL).OR.(N*X*X >= 370.0_kDouble)) THEN
        ResVal = ZERO_DBL
    ELSEIF (N == 1) THEN
        ResVal = ONE_DBL - X
    ELSEIF (N * X <= NxParam) THEN
        ResVal = ONE_DBL - fdist_KSPlus (N, X)
    ELSEIF (N >= NAsymp) THEN
        ResVal = KSPlusbarAsymp (N, X)
    ELSEIF ((N <= NParam).OR.(N*X*X > ONE_DBL)) THEN
        ResVal = KSPlusbarUpper(N, X)
    ELSE
        !   return (1.0 - 2.0*X/3.0)*exp(-2.0*N*X*X)
        ResVal = KSPlusbarAsymp (N, X)
    END IF
    
    RETURN
    
END FUNCTION fbar_KSPlus

!******************************************************************************

FUNCTION KSSpecial(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! ***.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, PARAMETER :: NLIM = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: W, T

! FLOW

    IF ((N * X * X >= 370.0_kDouble).OR.(X >= ONE_DBL)) THEN
        ResVal = ZERO_DBL
    ELSEIF (X <= HALF_DBL / N) THEN
        ResVal = ONE_DBL
    ELSEIF (N == 1) THEN
        ResVal = TWO_DBL - TWO_DBL * X
    ELSEIF (X <= ONE_DBL / N) THEN
        T = TWO_DBL * X - ONE_DBL / N
        IF (N <= NLIM) THEN
            W = num2_Factorial (ToInteger(N))
            ResVal = ONE_DBL - W * (T**N)
        ELSE
            W = num2_LnFactorial (ToInteger(N)) + N * LOG (T)
            ResVal = ONE_DBL - EXP (W)
        END IF
    ELSEIF (X >= ONE_DBL - ONE_DBL / N) THEN
        ResVal = TWO_DBL * ((ONE_DBL - X)**N)
    ELSE
        ResVal = -ONE_DBL
    END IF

    RETURN

END FUNCTION KSSpecial

!******************************************************************************

MODULE FUNCTION fbar_KS1(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Kolmogorov-Smirnov distribution FBar(x) = P[Dn >= x]
    ! in a form that is more precise in the upper tail.  It returns at least 10 decimal
    ! digits of precision for all n <= 400, at least 6 decimal digits of precision for
    ! 400 < n <= 200000, and a few correct digits (1 to 5) for n > 200000.
    ! Restrictions: n > 0 and 0 <= x <= 1.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Z

! FLOW

    ResVal = KSSpecial(N, X)
    IF (ResVal >= ZERO_DBL) RETURN

    Z = N * X * X
    IF (N <= 400) THEN
        IF (Z < 4.0_kDouble) THEN
            ResVal = ONE_DBL - fdist_KS1(N, X)
        ELSE 
            ResVal = TWO_DBL * KSPlusbarUpper(N, X)
        END IF
    ELSE
        IF (Z >= 2.2_kDouble) THEN
            IF (N <= 200000) THEN
                ResVal = TWO_DBL * KSPlusbarUpper(N, X)
            ELSE
                ResVal = TWO_DBL * KSPlusbarAsymp (N, X)
            END IF
        ELSE
            ResVal = ONE_DBL - fdist_KS1(N, X)
        END IF
    END IF
    
    RETURN
    
END FUNCTION fbar_KS1

!******************************************************************************

MODULE FUNCTION fbar_CramerMises(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary CramerMises distribution function.
    ! See "SubBase_FDist" for the description of the function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = ONE_DBL - fdist_CramerMises (N, X)
    
    RETURN
    
END FUNCTION fbar_CramerMises

!******************************************************************************

MODULE FUNCTION fbar_WatsonG(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary WatsonG distribution function.
    ! See "SubBase_FDist" for the description of the function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = ONE_DBL - fdist_WatsonG (N, X)
    
    RETURN
    
END FUNCTION fbar_WatsonG

!******************************************************************************

MODULE FUNCTION fbar_WatsonU(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary WatsonU distribution function.
    ! See "SubBase_FDist" for the description of the function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: JMAX = 10
    tDouble,  PARAMETER :: xSepare = 0.15

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Only the asymptotic form has been implemented. In the trivial case
    ! N = 1, we simply return 0.5

    IF (X <= ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ZERO_DBL
    ELSEIF (N == 1) THEN
        ! N = 1, degenerate case
        ResVal = HALF_DBL
    ELSEIF (X > xSepare) THEN
        ! this series converges rapidly for X > 0.15
        BLOCK
            tInteger    :: J
            tDouble     :: Sign, V, Term, Sum
            V = EXP (-(X * TWO_DBL * num_Pi * num_Pi))
            Sign = ONE_DBL
            Sum = ZERO_DBL
            J = 1
            DO
                Term = V**(J * J)
                Sum = Sum + Sign * Term
                Sign = -Sign
                J = J + 1
                IF ((Term < DBL_EPSILON).OR.(J > JMAX)) EXIT
            END DO
            IF (J > JMAX) THEN
                CALL Handle_ErrLevel('fbar_WatsonU', SubModName, ErrWarning, 'Sum has not converged.')
            END IF
            V = TWO_DBL * Sum
            IF (V <= ZERO_DBL) THEN
                ResVal = ZERO_DBL
            ELSE
                ResVal = V
            END IF
        END BLOCK
    ELSE
        ResVal = ONE_DBL - fdist_WatsonU (N, X)
    END IF
    
    RETURN
    
END FUNCTION fbar_WatsonU

!******************************************************************************

MODULE FUNCTION fbar_AndersonDarling(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Anderson-Darling distribution function.
    ! See "SubBase_FDist" for the description of the function.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: H = 0.05_kDouble         ! the step of the interpolation table

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Tables for the approximation of the Anderson-Darling distribution
    tDouble     :: F2AD(0:102)
    tDouble     :: CoAD(0:102)
    tDouble     :: Q, Cor
    tInteger    :: I

! FLOW
    
    IF (N == 1) THEN
        IF (X <= 0.38629436111989_kDouble) THEN
            ResVal = ONE_DBL
        ELSEIF (X >= fdist_XBIGM) THEN
            ResVal = ZERO_DBL
        ELSEIF (X < 6.0_kDouble) THEN
            Q = ONE_DBL - 4.0_kDouble * EXP(-X - ONE_DBL)
            ResVal = ONE_DBL - SQRT (Q)
        ELSE
            Q = 4.0_kDouble * EXP(-X - ONE_DBL)
            ResVal = HALF_DBL*Q*(ONE_DBL + 0.25_kDouble*Q*(ONE_DBL + &
                     HALF_DBL*Q*(ONE_DBL + 0.125_kDouble*Q*(5.0_kDouble + 3.5_kDouble*Q))))
        END IF
    ELSEIF (N <= 0) THEN
        ResVal = -ONE_DBL
        CALL Handle_ErrLevel('fbar_AndersonDarling', SubModName, ErrWarning, 'N <= 0.')
    ELSEIF (X > 10.0) THEN
        ! Sinclair-Spurr upper tail approximation (3.5)
        ResVal = 1.732_kDouble * EXP(-X) / SQRT(num_Pi * X)
    ELSEIF (X > 5.0_kDouble) THEN
        ! asymptotic X:  our empirical fit
        ResVal = EXP (-0.56_kDouble - 1.06_kDouble * X)
        Q = EXP (-1.03_kDouble - 1.06_kDouble * X)          ! Empirical correction in 1/N
        ResVal = ResVal + Q / N
    ELSEIF (X <= 0.2_kDouble) THEN
        ResVal = ONE_DBL - fdist_AndersonDarling (N, X)
    ELSE
        CALL AndersonDarlingInit ()

        I = 1 + ToInteger(X / H)
        Q = X / H - I

        ! Newton backwards quadratic interpolation
        ResVal = (F2AD(I - 2) - TWO_DBL * F2AD(I - 1) + F2AD(I)) * Q * (Q + ONE_DBL) / TWO_DBL &
               + (F2AD(I) - F2AD(I - 1)) * Q + F2AD(I)

        ! Empirical correction in 1/N
        Cor = (CoAD(I) * (Q + ONE_DBL) - CoAD(I - 1) * Q) / N

        ResVal = ONE_DBL - ResVal - Cor
        IF (ResVal >= ONE_DBL) THEN
            ResVal = ONE_DBL
        ELSEIF (ResVal <= ZERO_DBL) THEN
            ResVal = ZERO_DBL
        END IF
    END IF

    RETURN
    
    CONTAINS

    SUBROUTINE AndersonDarlingInit()

    !** PURPOSE OF THIS SUBROUTINE:
        ! Initialization procedure for fbar_AndersonDarling.

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        F2AD(0) = 0.0_kDouble;        F2AD(1) = 1.7315D-10;
        F2AD(2) = 2.80781D-5;         F2AD(3) = 1.40856D-3;
        F2AD(4) = 9.58772D-3;         F2AD(5) = 2.960552D-2;
        F2AD(6) = 6.185146D-2;        F2AD(7) = 1.0357152D-1;
        F2AD(8) = 1.5127241D-1;       F2AD(9) = 2.0190317D-1;
        F2AD(10) = 2.5318023D-1;      F2AD(11) = 3.0354278D-1;
        F2AD(12) = 3.5200015D-1;      F2AD(13) = 3.9797537D-1;
        F2AD(14) = 4.4117692D-1;      F2AD(15) = 4.8150305D-1;
        F2AD(16) = 5.1897375D-1;      F2AD(17) = 5.5368396D-1;
        F2AD(18) = 5.8577199D-1;      F2AD(19) = 6.1539864D-1;
        F2AD(20) = 6.4273362D-1;      F2AD(21) = 6.6794694D-1;
        F2AD(22) = 6.9120359D-1;      F2AD(23) = 7.126605D-1;
        F2AD(24) = 7.3246483D-1;      F2AD(25) = 7.507533D-1;
        F2AD(26) = 7.6765207D-1;      F2AD(27) = 7.8327703D-1;
        F2AD(28) = 7.9773426D-1;      F2AD(29) = 8.1112067D-1;
        F2AD(30) = 8.2352466D-1;      F2AD(31) = 8.3502676D-1;
        F2AD(32) = 8.4570037D-1;      F2AD(33) = 8.5561231D-1;
        F2AD(34) = 8.6482346D-1;      F2AD(35) = 8.7338931D-1;
        F2AD(36) = 8.8136046D-1;      F2AD(37) = 8.8878306D-1;
        F2AD(38) = 8.9569925D-1;      F2AD(39) = 9.0214757D-1;
        F2AD(40) = 9.081653D-1;       F2AD(41) = 9.1378043D-1;
        F2AD(42) = 9.1902284D-1;      F2AD(43) = 9.2392345D-1;
        F2AD(44) = 9.2850516D-1;      F2AD(45) = 9.3279084D-1;
        F2AD(46) = 9.3680149D-1;      F2AD(47) = 9.4055647D-1;
        F2AD(48) = 9.440736D-1;       F2AD(49) = 9.4736933D-1;
        F2AD(50) = 9.5045883D-1;      F2AD(51) = 9.5335611D-1;
        F2AD(52) = 9.5607414D-1;      F2AD(53) = 9.586249D-1;
        F2AD(54) = 9.6101951D-1;      F2AD(55) = 9.6326825D-1;
        F2AD(56) = 9.6538067D-1;      F2AD(57) = 9.6736563D-1;
        F2AD(58) = 9.6923135D-1;      F2AD(59) = 9.7098548D-1;
        F2AD(60) = 9.7263514D-1;      F2AD(61) = 9.7418694D-1;
        F2AD(62) = 9.7564704D-1;      F2AD(63) = 9.7702119D-1;
        F2AD(64) = 9.7831473D-1;      F2AD(65) = 9.7953267D-1;
        F2AD(66) = 9.8067966D-1;      F2AD(67) = 9.8176005D-1;
        F2AD(68) = 9.827779D-1;       F2AD(69) = 9.8373702D-1;
        F2AD(70) = 9.8464096D-1;      F2AD(71) = 9.8549304D-1;
        F2AD(72) = 9.8629637D-1;      F2AD(73) = 9.8705386D-1;
        F2AD(74) = 9.8776824D-1;      F2AD(75) = 9.8844206D-1;
        F2AD(76) = 9.8907773D-1;      F2AD(77) = 9.8967747D-1;
        F2AD(78) = 9.9024341D-1;      F2AD(79) = 9.9077752D-1;
        F2AD(80) = 9.9128164D-1;      F2AD(81) = 9.9175753D-1;
        F2AD(82) = 9.9220682D-1;      F2AD(83) = 9.9263105D-1;
        F2AD(84) = 9.9303165D-1;      F2AD(85) = 9.9340998D-1;
        F2AD(86) = 9.9376733D-1;      F2AD(87) = 9.9410488D-1;
        F2AD(88) = 9.9442377D-1;      F2AD(89) = 9.9472506D-1;
        F2AD(90) = 9.9500974D-1;      F2AD(91) = 9.9527876D-1;
        F2AD(92) = 9.95533D-1;        F2AD(93) = 9.9577329D-1;
        F2AD(94) = 9.9600042D-1;      F2AD(95) = 9.9621513D-1;
        F2AD(96) = 9.964181D-1;       F2AD(97) = 0.99661_kDouble;
        F2AD(98) = 9.9679145D-1;      F2AD(99) = 9.9696303D-1;
        F2AD(100) = 9.9712528D-1;     F2AD(101) = 9.9727872D-1;
        F2AD(102) = 9.9742384D-1;

        CoAD(0) = 0.0_kDouble;
        CoAD(1) = 0.0_kDouble;         CoAD(2) = 0.0_kDouble;            
        CoAD(3) = 0.0_kDouble;         CoAD(4) = 0.0_kDouble;            
        CoAD(5) = -1.87D-3;            CoAD(6) = 0.00898_kDouble;        
        CoAD(7) = 0.0209_kDouble;      CoAD(8) = 0.03087_kDouble;        
        CoAD(9) = 0.0377_kDouble;      CoAD(10) = 0.0414_kDouble;        
        CoAD(11) = 0.04386_kDouble;    CoAD(12) = 0.043_kDouble;         
        CoAD(13) = 0.0419_kDouble;     CoAD(14) = 0.0403_kDouble;        
        CoAD(15) = 0.038_kDouble;      CoAD(16) = 3.54804D-2;    
        CoAD(17) = 0.032_kDouble;      CoAD(18) = 0.0293_kDouble;        
        CoAD(19) = 2.61949D-2;         CoAD(20) = 0.0228_kDouble;        
        CoAD(21) = 0.0192_kDouble;     CoAD(22) = 1.59865D-2;    
        CoAD(23) = 0.0129_kDouble;     CoAD(24) = 0.0107_kDouble;        
        CoAD(25) = 8.2464D-3;          CoAD(26) = 0.00611_kDouble;       
        CoAD(27) = 0.00363_kDouble;    CoAD(28) = 1.32272D-3;    
        CoAD(29) = -5.87D-4;           CoAD(30) = -2.75D-3;      
        CoAD(31) = -3.95248D-3;        CoAD(32) = -5.34D-3;      
        CoAD(33) = -6.892D-3;          CoAD(34) = -8.10208D-3;   
        CoAD(35) = -8.93D-3;           CoAD(36) = -9.552D-3;     
        CoAD(37) = -1.04605D-2;        CoAD(38) = -0.0112_kDouble;       
        CoAD(39) = -1.175D-2;          CoAD(40) = -1.20216D-2;   
        CoAD(41) = -0.0124_kDouble;    CoAD(42) = -1.253D-2;     
        CoAD(43) = -1.27076D-2;        CoAD(44) = -0.0129_kDouble;       
        CoAD(45) = -1.267D-2;          CoAD(46) = -1.22015D-2;   
        CoAD(47) = -0.0122_kDouble;    CoAD(48) = -1.186D-2;     
        CoAD(49) = -1.17218D-2;        CoAD(50) = -0.0114_kDouble;       
        CoAD(51) = -1.113D-2;          CoAD(52) = -1.08459D-2;   
        CoAD(53) = -0.0104_kDouble;    CoAD(54) = -9.93D-3;      
        CoAD(55) = -9.5252D-3;         CoAD(56) = -9.24D-3;      
        CoAD(57) = -9.16D-3;           CoAD(58) = -8.8004D-3;    
        CoAD(59) = -8.63D-3;           CoAD(60) = -8.336D-3;     
        CoAD(61) = -8.10512D-3;        CoAD(62) = -7.94D-3;      
        CoAD(63) = -7.71D-3;           CoAD(64) = -7.55064D-3;   
        CoAD(65) = -7.25D-3;           CoAD(66) = -7.11D-3;      
        CoAD(67) = -6.834D-3;          CoAD(68) = -0.0065_kDouble;       
        CoAD(69) = -6.28D-3;           CoAD(70) = -6.11008D-3;   
        CoAD(71) = -5.86D-3;           CoAD(72) = -5.673D-3;     
        CoAD(73) = -5.35008D-3;        CoAD(74) = -5.11D-3;      
        CoAD(75) = -4.786D-3;          CoAD(76) = -4.59144D-3;   
        CoAD(77) = -4.38D-3;           CoAD(78) = -4.15D-3;      
        CoAD(79) = -4.07696D-3;        CoAD(80) = -3.93D-3;      
        CoAD(81) = -3.83D-3;           CoAD(82) = -3.74656D-3;   
        CoAD(83) = -3.49D-3;           CoAD(84) = -3.33D-3;      
        CoAD(85) = -3.20064D-3;        CoAD(86) = -3.09D-3;      
        CoAD(87) = -2.93D-3;           CoAD(88) = -2.78136D-3;   
        CoAD(89) = -2.72D-3;           CoAD(90) = -2.66D-3;      
        CoAD(91) = -2.56208D-3;        CoAD(92) = -2.43D-3;      
        CoAD(93) = -2.28D-3;           CoAD(94) = -2.13536D-3;   
        CoAD(95) = -2.083D-3;          CoAD(96) = -1.94D-3;      
        CoAD(97) = -1.82D-3;           CoAD(98) = -1.77D-3;      
        CoAD(99) = -1.72D-3;           CoAD(100) = -1.71104D-3;  
        CoAD(101) = -1.741D-3;         CoAD(102) = -0.0016_kDouble;

        RETURN

    END SUBROUTINE AndersonDarlingInit

    !**************************************************************************

END FUNCTION fbar_AndersonDarling

!******************************************************************************
MODULE FUNCTION fbar_Geometric(P, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary distribution function of a geometric random
    ! variable X with parameter p, FBar(s) = P[X >= s] = (1-p)**s for s >= 0.
    ! Restriction: 0 <= p <= 1.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: P
    tIndex,  INTENT(IN) :: S
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fbar_Geometric', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    END IF

    IF (S <= 0) THEN
        ResVal = ONE_DBL
    ELSEIF (P >= ONE_DBL) THEN
        ! In fact, P == 1
        ResVal = ZERO_DBL
    ELSEIF (P <= ZERO_DBL) THEN
        ! In fact, P == 0
        ResVal = ONE_DBL
    ELSE
        ResVal = (ONE_DBL - P)**S
    END IF

    RETURN
    
END FUNCTION fbar_Geometric

!******************************************************************************
MODULE FUNCTION fbar_Poisson1(Lambda, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Poisson distribution function with parameter Lambda,
    ! evaluated at s.  Computes and adds the non-negligible terms in the tail.
    ! Restriction: Lambda > 0.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Lambda
    tIndex,  INTENT(IN) :: S
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: Lambda_Limit = 150.0_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Term, Sum
    tInteger    :: I

! FLOW

    IF (Lambda < ZERO_DBL) THEN
        CALL Handle_ErrLevel('fbar_Poisson1', SubModName, ErrWarning, 'Lambda < 0.')
        RETURN
    END IF

    IF (S <= 0) THEN
        ResVal = ONE_DBL
    ELSEIF (Lambda > Lambda_Limit) THEN
        ! If Lambda > Lambda_Limit, we use the Chi2 distribution according to the exact
        !  relation, with 2S + 2 degrees of freedom fdist_Poisson (Lambda, S) = 1 -
        !  fdist_ChiSquare (2S + 2, 2*Lambda) which also equals 1 - fdist_Gamma (S +
        !  1, Lambda)
        ResVal = fdist_Gamma (REAL(S, KIND=kDouble), 15, Lambda)
    ELSEIF (S <= Lambda) THEN
        ResVal = ONE_DBL - fdist_Poisson1 (Lambda, S - 1)
    ELSE
        ! Sum at least IMAX prob. terms from i = s to i = oo
        Term = fmass_PoissonTerm1 (Lambda, S)
        Sum = Term
        I = S + 1
        DO WHILE ((Term > fmass_Epsilon).OR.(I <= S + IMAX))
            Term = Term * (Lambda / I)
            Sum = Sum + Term
            I = I + 1
        END DO
        ResVal = Sum
    END IF

    RETURN
    
END FUNCTION fbar_Poisson1

!******************************************************************************
MODULE FUNCTION fbar_Poisson2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary Poisson distribution function from the structure W,
    ! which must have been created previously by calling "fmass_CreatePoisson" with
    ! the desired Lambda.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Lambda

! FLOW
    
    ! fbar_Poisson (lam, s) = 1 - fdist_Poisson (lam, s - 1)

    IF (.NOT.ALLOCATED(W%ParamR)) THEN
        CALL Handle_ErrLevel('fbar_Poisson2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF
    
    Lambda = W%ParamR(0)
    
    IF (S <= 0) THEN
        ResVal = ONE_DBL
    ELSEIF (.NOT.ALLOCATED(W%CDF)) THEN
        ! For large Lambda, we use the Chi2 distribution according to the exact
        !   relation, with 2S + 2 degrees of freedom
        ! fdist_Poisson (Lambda, S) = 1 - fdist_ChiSquare (2S + 2, 2*Lambda)
        ! fdist_Poisson (Lambda, S) = 1 - fdist_Gamma (S + 1, Lambda)
        ResVal = fdist_Gamma (REAL(S, KIND=kDouble), 15, Lambda)
    ELSEIF (S > W%SMax) THEN
        ResVal = fbar_Poisson1 (Lambda, S)
    ELSEIF (S < W%SMin) THEN
        ResVal = ONE_DBL
    ELSEIF (S > W%SMed) THEN
        ! We keep the complementary distribution in the upper part of cdf
        ResVal = W%CDF(S - W%SMin)
    ELSE
        ResVal = ONE_DBL - W%CDF(S - 1 - W%SMin)
    END IF

    RETURN
    
END FUNCTION fbar_Poisson2

!******************************************************************************
MODULE FUNCTION fbar_Binomial2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary distribution function for a binomial random variable
    ! from the structure W, which must have been created previously by calling
    ! "fmass_CreateBinomial" with values of N and P.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P
    tIndex      :: N

! FLOW
    
    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fbar_Binomial2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF
    
    N = W%ParamI(0)
    P = W%ParamR(0)
    
    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fbar_Binomial2', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    END IF

    IF (N == 0) THEN
        ResVal = ONE_DBL
    ELSEIF (S < 1) THEN
        ResVal = ONE_DBL
    ELSEIF (S > N) THEN
        ResVal = ZERO_DBL
    ELSEIF (P == ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (P == ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (ALLOCATED(W%CDF)) THEN
        IF (S >= W%SMax) THEN
            ! Add IMAX dominant terms to get a few decimals in the tail
            BLOCK
                tDouble     :: Q, Z, Sum, Term
                tIndex      :: I
                Q = ONE_DBL - P
                Term = fmass_BinomialTerm3 (N, P, S)
                Sum = Term
                IF (ABS (Q) > ZERO_DBL) THEN
                    Z = P / Q
                ELSE
                    Z = ZERO_DBL
                    CALL Handle_ErrLevel('fbar_Binomial2', SubModName, ErrWarning, 'P / Q = infinite.')
                END IF
                I = S
                DO WHILE ((I < N).AND.(I < S + IMAX))
                    Term = Term * Z * (N - I) / (I + 1)
                    Sum = Sum + Term
                    I = I + 1
                END DO
                ResVal = Sum
            END BLOCK
        ELSEIF (S <= W%SMin) THEN
            ResVal = ONE_DBL
        ELSEIF (S > W%SMed) THEN
            ! We keep the complementary distribution in the upper part of cdf
            ResVal = W%CDF(S - W%SMin)
        ELSE
            ResVal = ONE_DBL - W%CDF(S - 1 - W%SMin)
        END IF
    ELSE
        ResVal = ONE_DBL - fdist_Binomial1 (N, P, S - 1)
    END IF

    RETURN
    
END FUNCTION fbar_Binomial2

!******************************************************************************
MODULE FUNCTION fbar_NegaBin2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the complementary distribution function for a negative binomial 
    ! random variable from the structure W, which must have been created previously
    ! by calling "fmass_CreateNegaBin" with values of N and P.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P
    tIndex      :: N

! FLOW
    
    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fbar_NegaBin2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF
    
    N = W%ParamI(0)
    P = W%ParamR(0)
    
    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fbar_NegaBin2', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    END IF

    IF (S < 1) THEN
        ResVal = ONE_DBL
    ELSEIF (P >= ONE_DBL) THEN
        ! In fact, p == 1
        ResVal = ZERO_DBL
    ELSEIF (P <= ZERO_DBL) THEN
        ! In fact, p == 0
        ResVal = ONE_DBL
    ELSEIF (.NOT.ALLOCATED(W%CDF)) THEN
        ResVal = fdist_Binomial1 (S - 1 + N, P, N - 1)
    ELSEIF (S >= W%SMax) THEN
        ResVal = fdist_Binomial1 (S - 1 + N, P, N - 1)
    ELSEIF (S <= W%SMin) THEN
        ResVal = ONE_DBL
    ELSEIF (S > W%SMed) THEN
        ! We keep the complementary distribution in the upper part of cdf
        ResVal = W%CDF(S - W%SMin)
    ELSE
        ResVal = ONE_DBL - W%CDF(S - 1 - W%SMin)
    END IF

    RETURN
    
END FUNCTION fbar_NegaBin2

!******************************************************************************
MODULE FUNCTION fbar_Scan(N, D, M) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return P[Sn(d) >= m], where Sn(d) is the scan statistic defined by Eq. (39)
    ! in [1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: D
    tIndex,  INTENT(IN) :: N, M
    tDouble             :: ResVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, PARAMETER  :: EpsilonScan = 1.0D-7

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Mu, Prob

! FLOW

    IF (N <  2) THEN
        CALL Handle_ErrLevel('fbar_Scan', SubModName, ErrWarning, 'N <  2.')
        RETURN
    ELSEIF ((D <= ZERO_DBL).OR.(D >= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fbar_Scan', SubModName, ErrWarning, 'D Not in (0, 1).')
        RETURN
    END IF

    IF (M > N) THEN
        ResVal = ZERO_DBL
    ELSEIF (M <= 1) THEN
        ResVal = ONE_DBL
    ELSEIF (M <= 2) THEN
        IF ((N - 1) * D >= ONE_DBL) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = ONE_DBL - (ONE_DBL - (N - 1) * D)**N
        END IF
    ELSEIF ((D >= HALF_DBL).AND.(M <= (N + 1) / TWO_DBL)) THEN
        ResVal = ONE_DBL
    ELSEIF (D > HALF_DBL) THEN
        ResVal = -ONE_DBL       ! Error
    ELSE
        ! mean of a binomial
        Mu = N * D
        IF (M <= Mu + D) THEN
            ResVal = ONE_DBL
        ELSEIF (Mu <= 10.0_kDouble) THEN
            ResVal = ScanGlaz (N, D, M)
        ELSE
            IF (((D >= 0.3_kDouble).AND.(N >= 50.0_kDouble)).OR. &
                ((N * D * D >= 250.0_kDouble).AND.(D < 0.3_kDouble))) THEN
                Prob = ScanAsympt (N, D, M)
                IF (Prob <= 0.4_kDouble) THEN
                    ResVal = Prob
                    RETURN
                END IF
            END IF
            Prob = ScanWNeff (N, D, M)
            IF (Prob <= 0.4_kDouble) THEN
                ResVal = Prob
            ELSE
                Prob = ScanGlaz (N, D, M)
                IF ((Prob > 0.4_kDouble).AND.(Prob <= ONE_DBL)) THEN
                    ResVal = Prob
                ELSE
                    ResVal = ONE_DBL
                END IF
            END IF
        END IF
    END IF

    RETURN

    CONTAINS

    FUNCTION ScanGlaz(N, D, M) RESULT(ResVal)

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: D
        tIndex,  INTENT(IN) :: N, M
        tDouble             :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: J, JMax
        tDouble     :: Temp
        tDouble     :: Jr, Jm1r, Nr
        tDouble     :: Sign
        tDouble     :: Q
        tDouble     :: Q4, Q3, Q2, Q1
        tDouble     :: Bin, BinMax

    ! FLOW

        Nr = N
        Q = ONE_DBL - D

        JMax = ToLong((N + 1) * D)    ! max term of the Binomial
        IF (JMax < M - 1) JMax = M - 1

        !---------------------------------------------------------
        ! Compute Q1: formula (2.5) in Glaz (1989)               
        ! Compute Q2: formula (A.6) in Berman and Eagleson (1985)
        ! Compute Q3, Q4 : Theorem (3.2) in Glaz (1989)          
        !---------------------------------------------------------

        ! compute the probability of term J = JMax
        Q1 = ZERO_DBL
        DO J = 1, JMax
            Jr = J
            Q1 = Q1 + LOG (Nr - Jr + ONE_DBL) - LOG (Jr)
        END DO
        Q1 = Q1 + JMax * LOG (D) + (Nr - JMax) * LOG (Q)
        BinMax = EXP (Q1)
        Q1 = BinMax
        Jm1r = JMax - M + 1
        IF (IAND((JMax - M + 1), 1) /= 0) THEN
            Sign = -ONE_DBL
        ELSE
            Sign = ONE_DBL
        END IF
        Q2 = Sign * BinMax
        Q3 = Sign * BinMax * (TWO_DBL - Jm1r * Jm1r + Jm1r)
        Q4 = Sign * BinMax * (Jm1r + ONE_DBL) * (Jm1r + TWO_DBL) * &
             (6.0_kDouble + Jm1r * Jm1r - 5.0_kDouble * Jm1r)

        ! compute the probability of terms J > JMax
        IF (IAND((JMax - M + 1), 1) /= 0) THEN
            Sign = -ONE_DBL
        ELSE
            Sign = ONE_DBL
        END IF

        Jm1r = JMax - M + 1
        Bin = BinMax
        DO J = JMax + 1, N
            Jr = J
            Jm1r = Jm1r + ONE_DBL
            Sign = -Sign
            Bin = (Bin * (Nr - Jr + ONE_DBL) * D) / (Jr * Q)
            IF (Bin < EpsilonScan) EXIT
            Q1 = Q1 + Bin
            Q2 = Q2 + Sign * Bin
            Q3 = Q3 + Sign * Bin * (TWO_DBL - Jm1r * Jm1r + Jm1r)
            Q4 = Q4 + Sign * Bin * (Jm1r + ONE_DBL) * (Jm1r + TWO_DBL) * &
                      (6.0_kDouble + Jm1r * Jm1r - 5.0_kDouble * Jm1r)
        END DO

        Q1 = ONE_DBL - Q1
        Q3 = Q3 / TWO_DBL
        Q4 = Q4 / 12.0_kDouble
        IF (M == 3) THEN
            ! Problem with this formula I do not get the same results as Glaz
            Q4 = ((Nr * (Nr - ONE_DBL) * D * D * (Q**(Nr - TWO_DBL))) / 8.0_kDouble &
                 + Nr * D * TWO_DBL * ((ONE_DBL - TWO_DBL * D)**(Nr - ONE_DBL))) &
                 - 4.0_kDouble * ((ONE_DBL - TWO_DBL * D)**Nr)
            IF (D < ONE_DBL / 3.0_kDouble) THEN
                Q4 = Q4 + (Nr * D * TWO_DBL * ((ONE_DBL - 3.0_kDouble * D)**(Nr - ONE_DBL)) &
                      + 4.0_kDouble * ((ONE_DBL - 3.0_kDouble * D)**Nr))
            END IF
        END IF
        ! compute probability: Glaz, equations (3.2) and (3.3)
        Q3 = Q1 - Q2 - Q3
        Q4 = Q3 - Q4
        ! when the approximation is bad, avoid overflow
        Temp = LOG (Q3) + (Nr - M - TWO_DBL) * LOG (Q4 / Q3)
        IF (Temp >= ZERO_DBL) THEN
            ResVal = ZERO_DBL
        ELSEIF (Temp < (-30.0)) THEN
            ResVal = ONE_DBL
        ELSE
            Q4 = EXP (Temp)
            ResVal = ONE_DBL - Q4
        END IF

        RETURN
    
    END FUNCTION ScanGlaz

    !******************************************************************************
    FUNCTION ScanWNeff(N, D, M) RESULT(ResVal)

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: D
        tIndex,  INTENT(IN) :: N, M
        tDouble             :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: J
        tDouble     :: Temp, Bin, Sum, Q

    ! FLOW

        Q = ONE_DBL - D
        
        !--------------------------------------
        ! Anderson-Titterington: equation (4) 
        !--------------------------------------

        ! compute the probability of term J = M
        Sum = ZERO_DBL
        DO J = 1, M
            Sum = Sum + LOG (REAL(N - J + 1, KIND=kDouble)) - LOG (REAL(J, KIND=kDouble))
        END DO
        Sum = Sum + M * LOG (D) + (N - M) * LOG (Q)
        Bin = EXP (Sum)
        Temp = (M / D - N - ONE_DBL) * Bin
        Sum = Bin

        ! compute the probability of terms J > M
        DO J = M + 1, N
            Bin = Bin * ((N - J + 1) * D / (J * Q))
            IF (Bin < EpsilonScan) EXIT
            Sum = Sum + Bin
        END DO
        Sum = TWO_DBL * Sum + Temp
        ResVal = Sum

        RETURN
    
    END FUNCTION ScanWNeff

    !******************************************************************************
    FUNCTION ScanAsympt(N, D, M) RESULT(ResVal)

        IMPLICIT NONE
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: D
        tIndex,  INTENT(IN) :: N, M
        tDouble             :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: Kappa, Temp, Theta, Sum

    ! FLOW

        !--------------------------------------------------------------
        ! Anderson-Titterington: asymptotic formula after equation (4)
        !--------------------------------------------------------------

        Theta = SQRT (D / (ONE_DBL - D))
        Temp = SQRT (REAL(N, KIND=kDouble))
        Kappa = M / (D * Temp) - Temp
        Temp = Theta * Kappa
        Temp = Temp * Temp / TWO_DBL
        Sum = TWO_DBL * fbar_Normal1 (Theta * Kappa) + (Kappa * Theta * EXP (-Temp)) / &
                                                       (D * SQRT (TWO_DBL * num_Pi))
        ResVal = Sum

        RETURN
    
    END FUNCTION ScanAsympt

    !******************************************************************************
END FUNCTION fbar_Scan

!******************************************************************************
END SUBMODULE SubBase_FBar

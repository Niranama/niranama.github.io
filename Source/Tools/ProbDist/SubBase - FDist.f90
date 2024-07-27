
SUBMODULE (ModBase_ProbDist) SubBase_FDist

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule provides procedures to compute (or approximate) the distribution
    ! functions of several standard types of random variables and of certain goodness
    ! -of-fit test statistics.  Recall that the distribution function of a continuous
    ! random variable X with density f is
    !                           _x
    !                          |
    !   F(x) = P[X <= x] =     | f(x)dx
    !                     -inf_|
    !
    ! while that of a discrete random variable X with mass function f over the set of
    ! integers is
    !
    !   F(x) = P[X <= x] = SUM(f(x)) for s = -inf to x
    !
    ! All the procedures in this module return F(x) for some probability distribution.
    !
    ! Most distributions are implemented only in standardized form here, i.e., with the
    ! location parameter set to 0 and the scale parameter set to 1.  To shift the
    ! distribution by x0 and rescale by c, it suffices to replace x by (x-x0)/c in the
    ! argument when calling the function.
    !
    ! For some of the discrete distributions, the value of F(x) can be simply recovered
    ! from a table that would have been previously constructed; see the submodule
    ! "SubBase_FMass" for the details.  This permits one to avoid recomputing the sums.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubBase_FDist1'
    tDouble,   PARAMETER    :: X_EPSILON  = 1.0D-3                       ! For x --> 0
    tDouble,   PARAMETER    :: EPSTOL     = 1.0D-15                     ! Tolerance
    tDouble,   PARAMETER    :: EPSBETA    = 0.5D-10                     ! < 0.75 sqrt(DBL_EPSILON)
    tDouble,   PARAMETER    :: ALPHALIM   = 100000.0_kDouble            ! Limiting alpha for normal approx.
    tInteger,  PARAMETER    :: MAXJ       = 2000                        ! Max number of terms in series
    tDouble,   PARAMETER    :: INV2PI     = 0.6366197723675813_kDouble  ! 2 / PI
    tDouble,   PARAMETER    :: LOG4       = 1.38629436111989062_kDouble ! Ln(4)
    tDouble,   PARAMETER    :: OneRac2    = 0.70710678118654752_kDouble ! 1/sqrt(2)
    tDouble,   PARAMETER    :: SQPI_2     = 0.88622692545275801_kDouble ! Sqrt(Pi) / 2
    tDouble,   PARAMETER    :: LOG_SQPI_2 = -0.1207822376352453_kDouble ! Ln(Sqrt(Pi) / 2)

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

MODULE RECURSIVE FUNCTION fdist_belog(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! This is the function   (1 - x*x + 2*x*log(x)) / ((1 - x)*(1 - x)).

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: EPS = 1.0D-12

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X > ONE_DBL) THEN
        ResVal = -fdist_belog(ONE_DBL/X)
    ELSEIF (X < 1.0D-20) THEN
        ResVal = ONE_DBL
    ELSEIF (X < 0.9_kDouble) THEN
        ResVal = (ONE_DBL - X * X + TWO_DBL * X * LOG(X)) &
               / ((ONE_DBL - X) * (ONE_DBL - X))
    ELSEIF (X == ONE_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        ! For X near 1, use a series expansion to avoid loss of precision.
        BLOCK
            tDouble     :: Term, Y, YPow, Sum
            tInteger    :: J
            Y = ONE_DBL - X
            YPow = ONE_DBL
            Sum = ZERO_DBL
            J = 2
            DO
                YPow = YPow * Y
                Term = YPow / (J * (J + 1))
                Sum = Sum + Term
                J = J + 1
                IF (ABS(Term/Sum) <= EPS) EXIT
            END DO

            ResVal = TWO_DBL * Sum
        END BLOCK
    END IF

    RETURN

END FUNCTION fdist_belog

!******************************************************************************

MODULE SUBROUTINE fdist_CalcB4(Alpha, PB, PLogB, PC, PLogC)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Beta(alpha, alpha) and Beta(alpha, alpha)*4^(alpha-1).

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha
    tDouble,  INTENT(INOUT) :: PB, PLogB, PC, PLogC

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Temp

! FLOW

    IF (Alpha <= EPSBETA) THEN
        ! For a -> 0, B(a,a) = (2/a)*(1 - 1.645*a^2 + O(a^3))
        PB = TWO_DBL / Alpha
        PC = PB / (4.0_kDouble*(ONE_DBL - Alpha*LOG4))
    ELSEIF (Alpha <= ONE_DBL) THEN
        PLogB = TWO_DBL * num2_LnGamma (Alpha) - num2_LnGamma (TWO_DBL*Alpha)
        PLogC = PLogB + (Alpha - ONE_DBL)*LOG4
        PC = EXP(PLogC)
        PB = EXP(PLogB)
    ELSEIF (Alpha <= 10.0_kDouble) THEN
        PLogC = num2_LnGamma (Alpha) - num2_LnGamma (HALF_DBL + Alpha) + LOG_SQPI_2
        PLogB = PLogC - (Alpha - ONE_DBL)*LOG4
    ELSEIF (Alpha <= 200.0_kDouble) THEN
        ! Convergent Series for Gamma(x + 0.5) / Gamma(x)
        BLOCK
            tDouble     :: Term, Sum
            tInteger    :: I
            Term = ONE_DBL
            Sum = ONE_DBL
            I = 1
            DO WHILE (Term > EPSTOL*Sum)
                Term = Term * ((I - 1.5_kDouble)*(I - 1.5_kDouble) / &
                                (I*(Alpha + I - 1.5_kDouble)))
                Sum  = Sum + Term
                I = I + 1
            END DO
            Temp = SQPI_2 / SQRT ((Alpha - HALF_DBL)*Sum)
            PLogC = LOG(Temp)
            PLogB = PLogC - (Alpha - ONE_DBL)*LOG4
        END BLOCK
    ELSE
        ! Asymptotic Series for Gamma(a + 0.5) / (Gamma(a) * Sqrt(a))
        BLOCK
            tDouble     :: U
            U = ONE_DBL / (8.0_kDouble*Alpha)
            Temp = ONE_DBL + U*(-ONE_DBL + U*(HALF_DBL + U*(2.5_kDouble - &
                                U*(2.625_kDouble + 49.875_kDouble*U))))
            ! This is 4^(Alpha - 1)*B(Alpha, Alpha)
            Temp = SQPI_2 / (SQRT(Alpha) * Temp)
            PLogC = LOG(Temp)
            PLogB = PLogC - (Alpha - ONE_DBL)*LOG4
        END BLOCK
    END IF

    RETURN

END SUBROUTINE fdist_CalcB4

!******************************************************************************

MODULE FUNCTION fdist_Unif(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return X for X in [0, 1], return 0 for X < 0, and return 1 for X > 1.
    ! This is the uniform distribution function over [0, 1].

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = X
    END IF

    RETURN

END FUNCTION fdist_Unif

!******************************************************************************
MODULE FUNCTION fdist_Expon(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return F(X) = 1 - EXP(-X) for X > 0, and 0 for X < 0.
    ! This is the standard exponential distribution with mean 1.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSEIF (X > X_EPSILON) THEN
        ResVal = ONE_DBL - EXP(-X)
    ELSE
        ! Avoid loss of precision for small X
        ResVal = X*(ONE_DBL - X*(0.5_kDouble - X* &
                   (ONE_DBL/6.0_kDouble - X/24.0_kDouble)))
    END IF

    RETURN

END FUNCTION fdist_Expon

!******************************************************************************

MODULE FUNCTION fdist_Weibull(Alpha, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return F(X) = 1 - EXP(-X**Alpha) for X > 0, and 0 for X <= 0.
    ! This is the standard Weibull distribution function with shape
    ! parameter Alpha.  Restriction:  Alpha > 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: Alpha, X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y

! FLOW

    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_Weibull', SubModName, ErrWarning, &
                          'Invalid Alpha: Alpha must be greater than zero.')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF ((X >= fdist_XBIG).AND.(Alpha >= ONE_DBL)) THEN
        ResVal = ONE_DBL
    ELSE
        Y = Alpha*LOG(X)
        IF (Y >= 5.0_kDouble) THEN
            ResVal = ONE_DBL
        ELSE
            Y = EXP(Y)
            IF (Y > X_EPSILON) THEN
                ResVal = (ONE_DBL - EXP(-Y))
            ELSE
                ! Avoid loss of precision for small Y
                ResVal = Y*(ONE_DBL - Y*(0.5_kDouble - Y* &
                           (ONE_DBL/6.0_kDouble - Y/24.0_kDouble)))
            END IF
        END IF
    END IF

    RETURN

END FUNCTION fdist_Weibull

!******************************************************************************

MODULE FUNCTION fdist_ExtremeValue(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return F(X) = EXP(-EXP(-X)), the standard extreme value distribution function.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= -10.0_kDouble) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = EXP(-EXP(-X))
    END IF

    RETURN

END FUNCTION fdist_ExtremeValue

!******************************************************************************

MODULE FUNCTION fdist_Logistic(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the standard logistic distribution function.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (X <= -fdist_XBIG) THEN
        ResVal = EXP(X)
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = ONE_DBL / (ONE_DBL + EXP(-X))
    END IF

    RETURN

END FUNCTION fdist_Logistic

!******************************************************************************

MODULE FUNCTION fdist_Pareto(C, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return F(X) = 1 - (1/X**C) for X >= 1, and 0 for X < 1.
    ! This is the standard Pareto distribution function.  Restriction:  C > 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: C, X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y

! FLOW

    IF (C <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_Pareto', SubModName, ErrWarning, &
                          'Invalid C: C must be greater than zero.')
        RETURN
    END IF

    IF (X <= ONE_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    Y = C*LOG(X)
    IF (Y >= 50.0_kDouble) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = ONE_DBL - ONE_DBL/EXP(Y)
    END IF

    RETURN

END FUNCTION fdist_Pareto

!******************************************************************************

MODULE RECURSIVE FUNCTION fdist_Normal1(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses the approximation
    ! given in [1].  This distribution is less precise than "fdist_Normal2" in
    ! the lower tail, as it will not compute probabilities smaller than DBL_EPSILON.

!** REFERENCE:
    ! [1] W. J. Kennedy Jr. and J. E. Gentle. Statistical Computing.
    !     Dekker, New York, NY,1980.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: Racinedeux    = 1.4142135623730950488_kDouble
    tDouble, PARAMETER  :: Racineunsurpi = 0.56418958354775628694_kDouble
    tDouble, PARAMETER  :: P10 = 2.4266795523053175D+02
    tDouble, PARAMETER  :: P11 = 2.1979261618294152D+01
    tDouble, PARAMETER  :: P12 = 6.9963834886191355_kDouble
    tDouble, PARAMETER  :: P13 = -3.5609843701815385D-02
    tDouble, PARAMETER  :: P20 = 3.004592610201616005D+02
    tDouble, PARAMETER  :: P21 = 4.519189537118729422D+02
    tDouble, PARAMETER  :: P22 = 3.393208167343436870D+02
    tDouble, PARAMETER  :: P23 = 1.529892850469404039D+02
    tDouble, PARAMETER  :: P24 = 4.316222722205673530D+01
    tDouble, PARAMETER  :: P25 = 7.211758250883093659D+00
    tDouble, PARAMETER  :: P26 = 5.641955174789739711D-01
    tDouble, PARAMETER  :: P27 = -1.368648573827167067D-07
    tDouble, PARAMETER  :: P30 = -2.99610707703542174D-03
    tDouble, PARAMETER  :: P31 = -4.94730910623250734D-02
    tDouble, PARAMETER  :: P32 = -2.26956593539686930D-01
    tDouble, PARAMETER  :: P33 = -2.78661308609647788D-01
    tDouble, PARAMETER  :: P34 = -2.23192459734184686D-02
    tDouble, PARAMETER  :: Q10 = 2.1505887586986120D+02
    tDouble, PARAMETER  :: Q11 = 9.1164905404514901D+01
    tDouble, PARAMETER  :: Q12 = 1.5082797630407787D+01
    tDouble, PARAMETER  :: Q13 = ONE_DBL
    tDouble, PARAMETER  :: Q20 = 3.004592609569832933D+02
    tDouble, PARAMETER  :: Q21 = 7.909509253278980272D+02
    tDouble, PARAMETER  :: Q22 = 9.313540948506096211D+02
    tDouble, PARAMETER  :: Q23 = 6.389802644656311665D+02
    tDouble, PARAMETER  :: Q24 = 2.775854447439876434D+02
    tDouble, PARAMETER  :: Q25 = 7.700015293522947295D+01
    tDouble, PARAMETER  :: Q26 = 1.278272731962942351D+01
    tDouble, PARAMETER  :: Q27 = ONE_DBL
    tDouble, PARAMETER  :: Q30 = 1.06209230528467918D-02
    tDouble, PARAMETER  :: Q31 = 1.91308926107829841D-01
    tDouble, PARAMETER  :: Q32 = 1.05167510706793207D+00
    tDouble, PARAMETER  :: Q33 = 1.98733201817135256D+00
    tDouble, PARAMETER  :: Q34 = ONE_DBL
    tDouble, PARAMETER  :: XASymp = 40.0_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Ycarre, UnsurY2, Y, R, Erf, Temp

! FLOW

    ! Returns P[X <= x] for the normal distribution.
    ! As in p:90 of W.J.Kennedy Jr and J.E.Gentle. Statistical computing.
    ! Dekker, New York, 1980.

    IF (X < -XASymp) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (X > XASymp) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    IF (X < ZERO_DBL) THEN
        ResVal = ONE_DBL - fdist_Normal1(-X)
        RETURN
    END IF

    Y = X / Racinedeux
    Ycarre = X * X / TWO_DBL
    IF (Y < 0.447_kDouble) THEN
        R = (P10 + Ycarre * (P11 + Ycarre * (P12 + Ycarre * P13))) / &
            (Q10 + Ycarre * (Q11 + Ycarre * (Q12 + Ycarre * Q13)))
        Erf = Y * R
    ELSE
        IF (Y <= 4.0_kDouble) THEN
            R = (P20 + Y * (P21 + Y * (P22 + Y * (P23 + Y * (P24 + Y * (P25 + &
                       Y * (P26 + Y * P27))))))) / (Q20 + Y * (Q21 +          &
                       Y * (Q22 + Y * (Q23 + Y * (Q24 + Y * (Q25 + Y * (Q26 + &
                       Y * Q27)))))))
            IF (-Ycarre < DBL_MIN_EXP * num_Ln2) THEN
                Erf = ONE_DBL
            ELSE
                Erf = ONE_DBL - EXP(-Ycarre) * R
            END IF
        ELSE
            UnsurY2 = ONE_DBL / Ycarre
            R = (P30 + UnsurY2 * (P31 + UnsurY2 * (P32 + UnsurY2 *  &
                 (P33 + UnsurY2 * P34)))) / (Q30 + UnsurY2 *        &
                 (Q31 + UnsurY2 * (Q32 + UnsurY2 * (Q33 + UnsurY2 * Q34))))
            IF (-Ycarre < DBL_MIN_EXP * num_Ln2) THEN
                Temp = ZERO_DBL
            ELSE
                Temp = EXP(-Ycarre)
            END IF
            Erf = ONE_DBL - (Temp / Y) * (Racineunsurpi + R / Ycarre)
        END IF
    END IF
    ResVal = ((ONE_DBL + Erf) / TWO_DBL)

    RETURN

END FUNCTION fdist_Normal1

!******************************************************************************

MODULE FUNCTION fdist_Normal2(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses the Chebyshev
    ! approximation proposed in [1], which gives 15 decimals of precision nearly
    ! everywhere.   In the paper, the author gives the coefficients with 30 decimals
    ! of precision.  This function is 1.5 times slower than "fdist_Normal1".

!** REFERENCE:
    ! [1] J. L. Schonfelder. Chebyshev expansions for the error and related functions.
    !     Mathematics of Computation, 32:1232–1240, 1978.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! The precision of double is 16 decimals; we shall thus use COEFFMAX = 24
    ! coefficients. But the approximation is good to 30 decimals of precision
    ! with 44 coefficients.
    tInteger, PARAMETER :: COEFFMAX = 24
    tDouble,  PARAMETER :: Normal2_A(0:43) = [ &
       6.10143081923200417926465815756D-1, &
       -4.34841272712577471828182820888D-1, &
       1.76351193643605501125840298123D-1, &
       -6.0710795609249414860051215825D-2, &
       1.7712068995694114486147141191D-2, &
       -4.321119385567293818599864968D-3, &
       8.54216676887098678819832055D-4, &
       -1.27155090609162742628893940D-4, &
       1.1248167243671189468847072D-5, &
       3.13063885421820972630152D-7, &
       -2.70988068537762022009086D-7, &
       3.0737622701407688440959D-8, &
       2.515620384817622937314D-9, &
       -1.028929921320319127590D-9, &
       2.9944052119949939363D-11, &
       2.6051789687266936290D-11, &
       -2.634839924171969386D-12, &
       -6.43404509890636443D-13, &
       1.12457401801663447D-13, &
       1.7281533389986098D-14, &
       -4.264101694942375D-15, &
       -5.45371977880191D-16, &
       1.58697607761671D-16, &
       2.0899837844334D-17, &
       -5.900526869409D-18, &
       -9.41893387554D-19, &
       2.14977356470D-19, &
       4.6660985008D-20, &
       -7.243011862D-21, &
       -2.387966824D-21, &
       1.91177535D-22, &
       1.20482568D-22, &
       -6.72377D-25, &
       -5.747997D-24, &
       -4.28493D-25, &
       2.44856D-25, &
       4.3793D-26, &
       -8.151D-27, &
       -3.089D-27, &
       9.3D-29, &
       1.74D-28, &
       1.6D-29, &
       -8.0D-30, &
       -2.0D-30]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y, T

! FLOW

    ! Returns P[X < x] for the normal distribution.
    ! As in J. L. Schonfelder, Math. of Computation, Vol. 32,
    ! pp 1232--1240, (1978).

    Y = X
    IF (Y <= -fdist_XBIG) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (Y >= fdist_XBIG) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    Y = -Y / num_Rac2
    IF (Y < ZERO_DBL) THEN
        Y = -Y
        T = (Y - 3.75_kDouble) / (Y + 3.75_kDouble)
        ResVal = ONE_DBL - 0.5_kDouble * EXP(-Y * Y) * num2_EvalCheby(Normal2_A, COEFFMAX, T)
    ELSE
        T = (Y - 3.75_kDouble) / (Y + 3.75_kDouble)
        ResVal = 0.5_kDouble * EXP(-Y * Y) * num2_EvalCheby(Normal2_A, COEFFMAX, T)
    END IF

    RETURN

END FUNCTION fdist_Normal2

!******************************************************************************

MODULE FUNCTION fdist_Normal3(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses the FORTRAN
    ! intrinsic ERFC function.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = HALF_DBL * ERFC(-X * num_1Rac2)

    RETURN

END FUNCTION fdist_Normal3

!******************************************************************************

MODULE FUNCTION fdist_Normal4(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of Phi(X), where Phi is the standard normal
    ! distribution function, with mean 0 and variance 1.  Uses Marsaglia’s
    ! fast method [1] with tables lookup. Returns 15 decimal digits of precision.
    ! This function is as fast as "fdist_Normal1" (no more no less).

!** REFERENCE:
    ! [1] G. Marsaglia, A. Zaman, and J. C. W. Marsaglia. Rapid evaluation of
    !     the inverse normal distribution function. Statistics and Probability
    !     Letters, 19:259–266, 1994.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: V(0:120) = [                                                      &
      1.2533141373155_kDouble,     1.137490921203605_kDouble,   1.037824575853727_kDouble,   &
      0.951527192071207_kDouble,   0.8763644564536924_kDouble,  0.8105337152790306_kDouble,  &
      0.7525711790634081_kDouble,  0.7012808218544303_kDouble,  0.6556795424187987_kDouble,  &
      0.61495459615093_kDouble,    0.5784303460476312_kDouble,  0.5455421356582171_kDouble,  &
      0.5158156382179634_kDouble,  0.4888504415275737_kDouble,  0.4643069280394423_kDouble,  &
      0.4418957328326002_kDouble,  0.4213692292880546_kDouble,  0.4025146181296722_kDouble,  &
      0.3851482907984348_kDouble,  0.3691112106902635_kDouble,  0.3542651113297938_kDouble,  &
      0.3404893532870847_kDouble,  0.3276783146905521_kDouble,  0.31573921586941_kDouble,    &
      0.3045902987101033_kDouble,  0.2941592970402893_kDouble,  0.284382146748493_kDouble,   &
      0.2752018941576065_kDouble,  0.2665677689682238_kDouble,  0.2584343943120386_kDouble,  &
      0.2507611114439651_kDouble,  0.243511400615456_kDouble,   0.2366523829135607_kDouble,  &
      0.230154390478801_kDouble,   0.2239905946538289_kDouble,  0.2181366833614714_kDouble,  &
      0.2125705804420318_kDouble,  0.2072722008565011_kDouble,  0.2022232366330547_kDouble,  &
      0.1974069692375194_kDouble,  0.1928081047153158_kDouble,  0.1884126285076003_kDouble,  &
      0.1842076773079702_kDouble,  0.1801814257143918_kDouble,  0.1763229857571027_kDouble,  &
      0.1726223176578506_kDouble,  0.1690701504076941_kDouble,  0.1656579109468773_kDouble,  &
      0.1623776608968675_kDouble,  0.1592220399363674_kDouble,  0.1561842150339759_kDouble,  &
      0.153257834853479_kDouble,   0.1504369887362691_kDouble,  0.1477161697413935_kDouble,  &
      0.145090241289131_kDouble,   0.1425544070104023_kDouble,  0.1401041834530503_kDouble,  &
      0.1377353753382303_kDouble,  0.1354440530967635_kDouble,  0.1332265324471292_kDouble,  &
      0.1310793558044918_kDouble,  0.1289992753343376_kDouble,  0.126983237485437_kDouble,   &
      0.1250283688553504_kDouble,  0.1231319632579323_kDouble,  0.1212914698765462_kDouble,  &
      0.119504482399253_kDouble,   0.1177687290432979_kDouble,  0.1160820633859823_kDouble,  &
      0.1144424559276431_kDouble,  0.112847986320103_kDouble,   0.1112968362007359_kDouble,  &
      0.1097872825783083_kDouble,  0.1083176917221132_kDouble,  0.1068865135106745_kDouble,  &
      0.1054922762005562_kDouble,  0.1041335815795983_kDouble,  0.1028091004723001_kDouble,  &
      0.1015175685681028_kDouble,  0.1002577825460485_kDouble,  0.09902859647173194_kDouble, &
      0.09782891844465691_kDouble, 0.09665770747608191_kDouble, 0.09551397057921558_kDouble, &
      0.09439676005522439_kDouble, 0.09330517095996169_kDouble, 0.09223833873763035_kDouble, &
      0.09119543700877471_kDouble, 0.09017567550106469_kDouble, 0.08917829811230435_kDouble, &
      0.08820258109597616_kDouble, 0.08724783136042988_kDouble, 0.08631338487354936_kDouble, &
      0.08539860516539227_kDouble, 0.08450288192189578_kDouble, 0.08362562966329139_kDouble, &
      0.08276628650136918_kDouble, 0.08192431297018954_kDouble, 0.08109919092525536_kDouble, &
      0.08029042250654048_kDouble, 0.07949752916111721_kDouble, 0.07872005072144664_kDouble, &
      0.07795754453568722_kDouble, 0.07720958464664668_kDouble, 0.07647576101624852_kDouble, &
      0.07575567879261112_kDouble, 0.07504895761704659_kDouble, 0.07435523096847724_kDouble, &
      0.07367414554294564_kDouble, 0.07300536066605566_kDouble, 0.07234854773633338_kDouble, &
      0.07170338969763433_kDouble, 0.07106958053885212_kDouble, 0.07044682481930167_kDouble, &
      0.06983483721825942_kDouble, 0.06923334210724434_kDouble, 0.06864207314371742_kDouble, &
      0.06806077288496332_kDouble, 0.0674891924209997_kDouble,  0.06692709102543307_kDouble, &
      0.06637423582325017_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: J
    tLogical    :: Negative
    tDouble     :: T, U, Z, H, Y
    tDouble     :: R, R1, R2, R3, R4, R5, R6, R7, R8

! FLOW

    IF (X <= -fdist_XBIG) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF (X < ZERO_DBL)  THEN
        Negative = TrueVal
        Y = -X
    ELSE
        Negative = FalseVal
        Y = X
    END IF
    J = ToInteger(8.0_kDouble * Y + 0.5_kDouble)
    IF (J > 120) J = 120
    Z = 0.125_kDouble * J
    H = Y - Z
    R = V(J)
    R1 = R * Z - ONE_DBL
    R2 = 0.5_kDouble * (R + Z * R1)
    R3 = (R1 + Z * R2) / 3.0_kDouble
    R4 = 0.25_kDouble * (R2 + Z * R3)
    R5 = 0.2_kDouble * (R3 + Z * R4)
    R6 = (R4 + Z * R5) / 6.0_kDouble
    R7 = (R5 + Z * R6) / 7.0_kDouble
    R8 = 0.125_kDouble * (R6 + Z * R7)
    T = R + H * (R1 + H * (R2 + H * (R3 + H * (R4 + H * (R5 + H * (R6 + H * (R7 + H * R8)))))))
    U = T * EXP(-0.5_kDouble * Y * Y - 0.9189385332046727_kDouble)
    IF (Negative) THEN
        ResVal = U
    ELSE
        ResVal = ONE_DBL - U
    END IF

    RETURN

END FUNCTION fdist_Normal4

!******************************************************************************

FUNCTION InitBiNormal(X, Y, Rho) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform initialization for "fdist_BiNormal1" and "fdist_BiNormal2".

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X, Y, Rho
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! The special cases of the BiNormal
    IF (ABS(Rho) > ONE_DBL) THEN
        CALL Handle_ErrLevel('InitBiNormal', SubModName, ErrWarning, '|Rho| > 1')
        ResVal = -ONE_DBL
    ELSEIF ((X == ZERO_DBL).AND.(Y == ZERO_DBL)) THEN
        ResVal = 0.25_kDouble + ASIN(Rho)/TWOPI
    ELSEIF (Rho == ONE_DBL) THEN
        BLOCK
            tDouble     :: Z
            Z = MIN(X, Y)
            ResVal = fdist_Normal2 (Z)
        END BLOCK
    ELSEIF (Rho == ZERO_DBL) THEN
        ResVal = fdist_Normal2 (X) * fdist_Normal2 (Y)
    ELSEIF (Rho == -ONE_DBL) THEN
        IF (Y <= -X) THEN
            ResVal = ZERO_DBL
        ELSE
            ResVal = fdist_Normal2 (X) - fdist_Normal2 (-Y)
        END IF
    ELSEIF ((X <= -fdist_XBIG).OR.(Y <= -fdist_XBIG)) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = fdist_Normal2 (Y)
    ELSEIF (Y >= fdist_XBIG) THEN
        ResVal = fdist_Normal2 (X)
    ELSE
        ResVal = -TWO_DBL
    END IF

    RETURN

END FUNCTION InitBiNormal

!******************************************************************************

MODULE FUNCTION fdist_BiNormal1(X, Y, Rho, NDig) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value u of the standard bivariate normal distribution, given by
    ! Eq. (13) in [1] where rho = is the correlation between x and y, and ndig is
    ! the number of decimal digits of accuracy.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: X, Y, Rho
    tInteger, INTENT(IN)    :: NDig
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: AH, AK, Con, Eps
    tDouble     :: A2, AP, B, CN, Conex, EX
    tDouble     :: G2, GH, GK, GW, H2, H4
    tDouble     :: RR, S1, S2, Sign, SN, SP, Sqr
    tDouble     :: T, W2, WH, WK
    tInteger    :: IS, Flag

! FLOW

    IS = -1
    Flag = 1
    AH = -X
    AK = -Y
    Con = num_Pi * num_TENNEGPOW(NDig)
    Eps = 0.5_kDouble * num_TENNEGPOW(NDig)

    IF (NDig > 15) THEN
        CALL Handle_ErrLevel('fdist_BiNormal1', SubModName, ErrWarning, 'NDig > 15')
        RETURN
    END IF

    B = InitBiNormal (X, Y, Rho)
    IF (B >= ZERO_DBL) THEN
        ResVal = B
        RETURN
    END IF

    GH = fdist_Normal2 (-AH) / TWO_DBL
    GK = fdist_Normal2 (-AK) / TWO_DBL

    B = ZERO_DBL
    RR = (ONE_DBL - Rho) * (ONE_DBL + Rho)
    Sqr = SQRT (RR)
    Flag = 1
    IF (AH /= ZERO_DBL) THEN
        B = GH
        IF (AH * AK < ZERO_DBL) THEN
            B = B - 0.5_kDouble
        ELSE IF (AH * AK == ZERO_DBL) THEN
            Flag = 0
        END IF
    ELSEIF (AK == ZERO_DBL) THEN
        ResVal = ASIN (Rho) / TWOPI + 0.25_kDouble
        RETURN
    END IF
    IF (Flag /= 0) B = B + GK
    IF (AH /= ZERO_DBL) THEN
        Flag = 0
        WH = -AH
        WK = (AK / AH - Rho) / Sqr
        GW = TWO_DBL * GH
        IS = -1
    END IF

    OuterLoop: DO
        IF (Flag /= 0) THEN
             WH = -AK
             WK = (AH / AK - Rho) / Sqr
             GW = TWO_DBL * GK
             IS = 1
        END IF
        Flag = 1
        Sign = -ONE_DBL
        T = ZERO_DBL
        IF (WK /= ZERO_DBL) THEN
            IF (ABS (WK) >= ONE_DBL) THEN
                IF (ABS (WK) == ONE_DBL) THEN
                    T = WK * GW * (ONE_DBL - GW) / TWO_DBL
                    B = B + Sign * T
                    IF (IS >= 0) THEN
                        EXIT OuterLoop
                    ELSE
                        CYCLE OuterLoop
                    END IF
                ELSE
                    Sign = -Sign
                    WH = WH * WK
                    G2 = fdist_Normal2 (WH)
                    WK = ONE_DBL / WK
                    IF (WK < ZERO_DBL) B = B + 0.5_kDouble
                    B = B - (GW + G2) / TWO_DBL + GW * G2
                END IF
            END IF
            H2 = WH * WH
            A2 = WK * WK
            H4 = H2 * 0.5_kDouble
            EX = ZERO_DBL
            IF (H4 < 150.0_kDouble) EX = EXP (-H4)
            W2 = H4 * EX
            AP = ONE_DBL
            S2 = AP - EX
            SP = AP
            S1 = ZERO_DBL
            SN = S1
            Conex = ABS (Con / WK)
            InnerLoop: DO
                CN = AP * S2 / (SN + SP)
                S1 = S1 + CN
                IF (ABS (CN) <= Conex) EXIT InnerLoop
                SN = SP
                SP = SP + ONE_DBL
                S2 = S2 - W2
                W2 = W2 * H4 / SP
                AP = -AP * A2
            END DO InnerLoop
            T = (ATAN (WK) - WK * S1) / TWOPI
            B = B + Sign * T
        END IF
        IF (IS >= 0) EXIT OuterLoop
        IF (AK == ZERO_DBL) EXIT OuterLoop
    END DO OuterLoop

    IF (B < Eps)     B = ZERO_DBL
    IF (B > ONE_DBL) B = ONE_DBL

    ResVal = B

    RETURN

END FUNCTION fdist_BiNormal1

!******************************************************************************

MODULE FUNCTION fdist_BiNormal2(DH, DK, Rho) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value u of the standard bivariate normal distribution as in
    ! "fdist_BiNormal".  The algorithm described in [2] is a modified form of
    ! the algorithm proposed in [1].

!** REFERENCE:
    ! [1] Z. Drezner and G. O. Wesolowsky. On the computation of the bivariate
    !     normal integral.  Journal of Statistical Computation and Simulation,
    !     35:101–107, 1989.
    ! [2] A. Genz. Numerical computation of rectangular bivariate and trivariate
    !     normal and t probabilities.  Statistics and Computing, 14:151–160, 2004.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: DH, DK, Rho
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, ALLOCATABLE    :: W(:), X(:)
    tDouble                 :: H, K, HK, BVN, HS, ASR, SN, AS
    tDouble                 :: A, B, C, D, SP, RS, EP, BS, XS
    tInteger                :: I, IS
    tIndex                  :: LG

! FLOW

    BVN = InitBiNormal (DH, DK, Rho)
    IF (BVN >= ZERO_DBL) THEN
        ResVal = BVN
        RETURN
    END IF

    IF (ABS(Rho) < 0.3_kDouble) THEN
        LG = 3
        CALL Get_GaussLegendre_Set(LG, W, X)
    ELSEIF (ABS(Rho) < 0.75_kDouble) THEN
        LG = 6
        CALL Get_GaussLegendre_Set(LG, W, X)
    ELSE
        LG = 10
        CALL Get_GaussLegendre_Set(LG, W, X)
    END IF

    H = -DH
    K = -DK
    HK = H * K
    BVN = ZERO_DBL
    IF (ABS(Rho) < 0.925_kDouble) THEN
        HS = (H * H + K * K) / TWO_DBL
        ASR = ASIN(Rho)
        DO I = 1, LG
            SN = SIN (ASR * (ONE_DBL - X(I)) / TWO_DBL)
            BVN = BVN + W(I) * EXP((SN * HK - HS) / (ONE_DBL - SN * SN))
            SN = SIN (ASR * (ONE_DBL + X(I)) / TWO_DBL)
            BVN = BVN + W(I) * EXP((SN * HK - HS) / (ONE_DBL - SN * SN))
        END DO
        BVN = BVN * ASR / (4.0_kDouble * num_Pi) + fdist_Normal2 (-H) * fdist_Normal2 (-K)
    ELSE
        IF (Rho < ZERO_DBL) THEN
            K = -K
            HK = -HK
        END IF
        IF (ABS(Rho) < ONE_DBL) THEN
            AS = (ONE_DBL - Rho) * (ONE_DBL + Rho)
            A = SQRT(AS)
            BS = (H - K) * (H - K)
            C = (4.0_kDouble - HK) / 8.0_kDouble
            D = (12.0_kDouble - HK) / 16.0_kDouble
            ASR = -(BS / AS + HK) / TWO_DBL
            IF (ASR > -100.0_kDouble) THEN
                BVN = A * EXP(ASR) * (ONE_DBL - C * (BS - AS) * (ONE_DBL - &
                      D * BS / 5.0_kDouble) / 3.0_kDouble + C * D * AS * AS / 5.0_kDouble)
            END IF

            IF (-HK < 100.0_kDouble) THEN
                B = SQRT(BS)
                SP = SQRT(TWOPI) * fdist_Normal2 (-B / A)
                BVN = BVN - EXP(-HK / TWO_DBL) * SP * B * (ONE_DBL - C * BS * (ONE_DBL - &
                      D * BS / 5.0_kDouble) / 3.0_kDouble)
            END IF
            A = A / TWO_DBL
            DO I = 1, LG
                DO IS = -1, 1, 2
                   XS = (A * (IS * X(I) + ONE_DBL))
                   XS = XS * XS
                   RS = SQRT(ONE_DBL - XS)
                   ASR = -(BS / XS + HK) / TWO_DBL
                   IF (ASR > -100.0_kDouble) THEN
                      SP = (ONE_DBL + C * XS * (ONE_DBL + D * XS))
                      EP = EXP(-HK * (ONE_DBL - RS) / (TWO_DBL * (ONE_DBL + RS))) / RS
                      BVN = BVN + A * W(I) * EXP(ASR) * (EP - SP)
                   END IF
                END DO
            END DO
            BVN = -BVN / TWOPI
        END IF
        IF (Rho > ZERO_DBL) THEN
            IF (K > H) H = K
            BVN = BVN + fdist_Normal2 (-H)
        END IF
        IF (Rho < ZERO_DBL) THEN
            XS = fdist_Normal2 (-H) - fdist_Normal2 (-K)
            IF (XS < ZERO_DBL) XS = ZERO_DBL
            BVN = -BVN + XS
        END IF
    END IF
    IF (BVN <= ZERO_DBL) BVN = ZERO_DBL
    IF (BVN >= ONE_DBL) BVN = ONE_DBL
    ResVal = BVN

    CALL MemFree(W)
    CALL MemFree(X)

    RETURN

    CONTAINS

    SUBROUTINE Get_GaussLegendre_Set(N, W, X)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To get Gauss (Guass-Legendre) points and weights.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,               INTENT(IN)    :: N
        tDouble, ALLOCATABLE, INTENT(OUT)   :: W(:), X(:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        CALL MemAlloc(W, N)
        CALL MemAlloc(X, N)

        SELECT CASE (N)
        CASE (3)
            ! Gauss Legendre points and weights, n = 6
            W(1) = 0.1713244923791705_kDouble
            W(2) = 0.3607615730481384_kDouble
            W(3) = 0.4679139345726904_kDouble

            X(1) = 0.9324695142031522_kDouble
            X(2) = 0.6612093864662647_kDouble
            X(3) = 0.2386191860831970_kDouble
        CASE (6)
            ! Gauss Legendre points and weights, n = 12
            W(1) = 0.4717533638651177D-1
            W(2) = 0.1069393259953183_kDouble
            W(3) = 0.1600783285433464_kDouble
            W(4) = 0.2031674267230659_kDouble
            W(5) = 0.2334925365383547_kDouble
            W(6) = 0.2491470458134029_kDouble

            X(1) = 0.9815606342467191_kDouble
            X(2) = 0.9041172563704750_kDouble
            X(3) = 0.7699026741943050_kDouble
            X(4) = 0.5873179542866171_kDouble
            X(5) = 0.3678314989981802_kDouble
            X(6) = 0.1252334085114692_kDouble
        CASE (10)
            ! Gauss Legendre points and weights, n = 20
            W(1) = 0.1761400713915212D-1
            W(2) = 0.4060142980038694D-1
            W(3) = 0.6267204833410906D-1
            W(4) = 0.8327674157670475D-1
            W(5) = 0.1019301198172404_kDouble
            W(6) = 0.1181945319615184_kDouble
            W(7) = 0.1316886384491766_kDouble
            W(8) = 0.1420961093183821_kDouble
            W(9) = 0.1491729864726037_kDouble
            W(10) = 0.1527533871307259_kDouble

            X(1) = 0.9931285991850949_kDouble
            X(2) = 0.9639719272779138_kDouble
            X(3) = 0.9122344282513259_kDouble
            X(4) = 0.8391169718222188_kDouble
            X(5) = 0.7463319064601508_kDouble
            X(6) = 0.6360536807265150_kDouble
            X(7) = 0.5108670019508271_kDouble
            X(8) = 0.3737060887154196_kDouble
            X(9) = 0.2277858511416451_kDouble
            X(10) = 0.7652652113349733D-1
        END SELECT

        RETURN

    END SUBROUTINE Get_GaussLegendre_Set

    !**************************************************************************

END FUNCTION fdist_BiNormal2

!******************************************************************************

MODULE FUNCTION fdist_LogNormal(Mu, Sigma, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the lognormal distribution function, defined by Eq. (14) in [1]
    ! for x > 0 and 0 for x <= 0, where Phi is the standard normal distribution.
    ! Restriction: sigma > 0.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Mu, Sigma, X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Sigma <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_LogNormal', SubModName, ErrWarning, 'Sigma <= 0')
        RETURN
    END IF
    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = fdist_Normal2 ((LOG(X) - Mu) / Sigma)
    END IF

    RETURN

END FUNCTION fdist_LogNormal

!******************************************************************************

MODULE FUNCTION fdist_JohnsonSB(Alpha, Beta, A, B, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the Johnson JSB distribution function, defined by Eq. (15) in [1]
    ! where Phi is the standard normal distribution.  Restriction: beta > 0,
    ! a < b, and a <= x <= b.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha, Beta, A, B, X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_JohnsonSB', SubModName, ErrWarning, 'Beta <= 0')
        RETURN
    ELSEIF (B <= A) THEN
        CALL Handle_ErrLevel('fdist_JohnsonSB', SubModName, ErrWarning, 'B <= A')
        RETURN
    END IF
    IF (X <= A) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= B) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = fdist_Normal2 (Alpha + Beta * LOG((X - A) / (B - X)))
    END IF

    RETURN

END FUNCTION fdist_JohnsonSB

!******************************************************************************

MODULE FUNCTION fdist_JohnsonSU(Alpha, Beta, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the Johnson JSU distribution function, defined by Eq. (16) in [1]
    ! where Phi is the standard normal distribution.  Restriction: beta > 0.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha, Beta, X
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: XLIM = 1.0D+10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: R, XX
    tLogical    :: Negative

! FLOW

    IF (Beta <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_JohnsonSU', SubModName, ErrWarning, 'Beta <= 0')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        Negative = TrueVal
        XX = -X
    ELSE
        Negative = FalseVal
        XX = X
    END IF

    ! compute r = x + sqrt (x * x + 1)
    IF (XX < XLIM) THEN
        R = XX + SQRT(XX*XX + ONE_DBL)
    ELSE
        R = TWO_DBL * XX
    END IF
    IF (Negative) R = ONE_DBL / R

    IF (R > ZERO_DBL) THEN
        ResVal = fdist_Normal2 (Alpha + Beta * LOG(R))
    ELSE
        ResVal = ZERO_DBL
    END IF

    RETURN

END FUNCTION fdist_JohnsonSU

!******************************************************************************

MODULE FUNCTION fdist_ChiSquare1(K, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the chi-square distribution function with
    ! k degrees of freedom, which is a special case of the gamma distribution,
    ! with shape parameter k/2 and scale parameter 1/2.  Uses the approximation
    ! given in [1, p.116] for k <= 1000, and the normal approximation for k > 1000.
    ! Gives no more than 4 decimals of precision for k > 1000.  Restrictions: k > 0.

!** REFERENCE:
    ! [1] W. J. Kennedy Jr. and J. E. Gentle. Statistical Computing.
    !     Dekker, New York, NY, 1980.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: K
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: Tiers = 0.33333333333333333_kDouble
    tDouble, PARAMETER  :: Pt2 = 0.22222222222222222_kDouble
    tDouble, PARAMETER  :: MoinsDixHuit = -18.8055_kDouble
    tDouble, PARAMETER  :: Gamma = 0.8862269254527579825931_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: H, H2, E, HalfX, Term, Summation, Y, XX
    tIndex      :: I

! FLOW

    Y = ZERO_DBL

    IF (K <= 0) THEN
        CALL Handle_ErrLevel('fdist_ChiSquare1', SubModName, ErrWarning, 'K <= 0')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X >= fdist_XBIG * K) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    IF (K > 1000) THEN
        IF (X < TWO_DBL) THEN
            ResVal = ZERO_DBL
        ELSE
            XX = (((X / K)**Tiers) - (ONE_DBL - Pt2 / K)) / SQRT (Pt2 / K)
            IF (XX > 5.0_kDouble) THEN
                ResVal = ONE_DBL
            ELSEIF (XX < MoinsDixHuit) THEN
                ResVal = ZERO_DBL
            ELSE
                ResVal = fdist_Normal2 (XX)
            END IF
        END IF
        RETURN
    ELSE
        HalfX = X / TWO_DBL
        IF (NOT(IAND(K, 1)) /= 0) THEN  ! even K
            IF (-HalfX < DBL_MIN_EXP * num_Ln2) THEN
                Term = ZERO_DBL
            ELSE
                Term = EXP (-HalfX)
            END IF
            Summation = Term
            DO I = 1, K/2 - 1
                Term = Term * HalfX / REAL(I, KIND=kDouble)
                Summation = Summation + Term
            END DO
            Y = ONE_DBL - Summation
        ELSE
            H2 = -ONE_DBL + TWO_DBL * fdist_Normal2 (SQRT (X))
            IF (K == 1) ResVal = H2
            IF (-HalfX < DBL_MIN_EXP * num_Ln2) THEN
                E = ZERO_DBL
            ELSE
                E = EXP (-HalfX)
            END IF
            Term = SQRT (HalfX) * E / Gamma
            H = H2
            DO I = 3, K-1, 2
                H = H - Term
                Term = Term * HalfX * TWO_DBL / REAL(I, KIND=kDouble)
            END DO
            Y = H - Term
        END IF
    END IF
    IF (Y < ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSE
        ResVal = Y
    END IF

    RETURN

END FUNCTION fdist_ChiSquare1

!******************************************************************************

MODULE FUNCTION fdist_ChiSquare2(K, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the chi-square distribution function with
    ! k degrees of freedom, by calling "fdist_Gamma (k/2, d, x/2)."  The function
    ! will do its best to return d decimals digits of precision (but there is no
    ! guarantee).  For k not too large (e.g., k <= 1000), d gives a good idea of
    ! the precision attained.  Restrictions:  k > 0 and 0 < d <= 15.

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
        CALL Handle_ErrLevel('fdist_ChiSquare2', SubModName, ErrWarning, 'K <= 0')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG * K) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = fdist_Gamma (K / TWO_DBL, D, X / TWO_DBL)
    END IF

    RETURN

END FUNCTION fdist_ChiSquare2

!******************************************************************************

MODULE FUNCTION fdist_Student1(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the Student-t distribution function with
    ! n degrees of freedom, whose density is given by Eq. (17) in [1].
    ! Gives at least 12 decimals of precision for n >= 103, and at least 10
    ! decimals for 103 < n <= 105.  Restriction: n > 0.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: Student_N1 = 20
    tDouble,  PARAMETER :: Student_X1 = 8.01_kDouble
    tInteger, PARAMETER :: Student_KMax = 200
    tDouble,  PARAMETER :: Student_Eps = 0.5D-16

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: A, U, B, Y, Z, Z2, Precision
    tIndex      :: K

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_Student1', SubModName, ErrWarning, 'N <= 0')
        RETURN
    END IF

    IF (N == 1) THEN
        IF (X < -HALF_DBL) THEN
            ResVal = ATAN (-ONE_DBL/X) / num_Pi
        ELSE
            ResVal = HALF_DBL + (ATAN (X)) / num_Pi
        END IF
        RETURN
    END IF

    IF (N == 2) THEN
        Z = ONE_DBL + X * X / TWO_DBL
        IF (X >= ZERO_DBL) THEN
            ResVal = HALF_DBL + X / (TWO_DBL * SQRT (Z) * num_Rac2)
        ELSE
            ResVal = 0.25_kDouble / (Z * (HALF_DBL - X /(TWO_DBL*SQRT(Z)*num_Rac2)))
        END IF
        RETURN
    END IF

    ! first case: small N and small X
    IF ((N <= Student_N1).AND.(X <= Student_X1)) THEN
        B = ONE_DBL + X * X / N
        Y = X / SQRT (REAL(N, KIND=kDouble))
        Z = ONE_DBL
        DO K = N - 2, 2, -2
            Z = ONE_DBL + Z * (K - ONE_DBL) / (K * B)
        END DO
        IF (MOD(N, 2) == 0) THEN
            U = (ONE_DBL + Z * Y / SQRT (B)) / TWO_DBL
            IF (U >= ZERO_DBL) THEN
                ResVal = U
            ELSE
                ResVal = ZERO_DBL
            END IF
        ELSE
            IF (Y > -ONE_DBL) THEN
                ResVal = (HALF_DBL + (ATAN (Y) + Z * Y / B) / num_Pi)
            ELSE
                U = (ATAN (-ONE_DBL / Y) + Z * Y / B) / num_Pi
                IF (U >= ZERO_DBL) THEN
                    ResVal = U
                ELSE
                    ResVal = ZERO_DBL
                END IF
            END IF
        END IF

    ! second case: large N and small X
    ELSEIF (X < Student_X1) THEN
        A = N - HALF_DBL
        B = 48.0_kDouble * A * A
        Z2 = A * num2_LOG1P (X * X / N)
        Z = SQRT (Z2)
        Y = (((((64.0_kDouble * Z2 + 788.0_kDouble) * Z2 + 9801.0_kDouble) * Z2 +        &
               89775.0_kDouble) * Z2 + 543375.0_kDouble) * Z2 + 1788885.0_kDouble) * Z / &
            (210.0_kDouble * B * B * B)
        Y = Y - ((((4.0_kDouble * Z2 + 33.0_kDouble) * Z2 + 240.0_kDouble) * Z2 + &
                  855.0_kDouble) * Z / (10.0_kDouble * B * B))
        Y = Y + Z + (Z2 + 3.0_kDouble) * Z / B
        IF (X >= ZERO_DBL) THEN
            ResVal = fbar_Normal1 (-Y)
        ELSE
            ResVal = fbar_Normal1 (Y)
        END IF

    ! third case: large X
    ELSE
        ! Compute the Student probability density
        B = ONE_DBL + X * X / N
        ! to avoid overflow with the 2 Gamma functions, use their logarithm.
        ! However, for large N, there will be some loss of precision
        Y = num2_LnGamma ((N + 1) / TWO_DBL) - num2_LnGamma (N / TWO_DBL)
        Y = EXP (Y)
        Y = Y * ((B**(-(N + 1) / TWO_DBL)) / SQRT (num_Pi * N))
        Y = Y * (TWO_DBL * SQRT (N * B))
        Z = Y / N
        K = 2
        Precision = 10.0_kDouble
        Z2 = Precision
        DO WHILE ((K < Student_KMax).AND.(Precision > Student_Eps))
            Y = Y * ((K - 1) / (K * B))
            Z = Z + Y / (N + K)
            Precision = ABS (Z - Z2)
            Z2 = Z
            K = K + 2
        END DO
        IF (K >= Student_KMax) THEN
            CALL Handle_ErrLevel('fdist_Student1', SubModName, ErrWarning, 'K >= Student_KMax')
        END IF
        IF (X >= ZERO_DBL) THEN
            ResVal = ONE_DBL - Z / TWO_DBL
        ELSE
            ResVal = Z / TWO_DBL
        END IF
    END IF

    RETURN

END FUNCTION fdist_Student1

!******************************************************************************

MODULE FUNCTION fdist_Student2(N, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the Student-t distribution function with
    ! n degrees of freedom, whose density is given by Eq. (17) in [1].
    ! Uses the relationship given by Eq. (18) in [1] where Ip,q is the distribution
    ! function with parameters p and q (also called the incomplete beta ratio)
    ! defined in (2), which is approximated by calling "fdist_Beta".  The function
    ! tries to return d decimals digits of precision (but there is no guarantee).
    ! Restriction: n > 0 and 0 < d <= 15.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.
    ! [2] W. J. Kennedy Jr. and J. E. Gentle. Statistical Computing.
    !     Dekker, New York, NY, 1980.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: N
    tInteger, INTENT(IN)    :: D
    tDouble,  INTENT(IN)    :: X
    tDouble                 :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_Student1', SubModName, ErrWarning, 'N <= 0')
        RETURN
    END IF
    IF (D <= 0) THEN
        CALL Handle_ErrLevel('fdist_Student1', SubModName, ErrWarning, 'D <= 0')
        RETURN
    END IF
    IF (D > 15) THEN
        CALL Handle_ErrLevel('fdist_Student1', SubModName, ErrWarning, 'D > 15')
        RETURN
    END IF

    IF (X <= -fdist_XBIG) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSEIF (X >= ZERO_DBL) THEN
        ResVal = HALF_DBL*(ONE_DBL + fdist_Beta(HALF_DBL, HALF_DBL * N, D, X * X / (N + X * X)))
    else
        ResVal = HALF_DBL*(fdist_Beta(HALF_DBL * N, HALF_DBL, D, N / (N + X * X)))
    END IF

    RETURN

END FUNCTION fdist_Student2

!******************************************************************************

MODULE FUNCTION fdist_Gamma(Alpha, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation, based on [2], of the gamma distribution function
    ! with parameter alpha, whose density is given by Eq. (19) in [1] for x >= 0,
    ! where gamma is the gamma function defined by Eq. (20) in [1].  The function
    ! tries to return d decimals digits of precision.  For alpha not too large
    ! (e.g., alpha <=  1000), d gives a good idea of the precision attained.
    ! For alpha >= 100000, uses a normal approximation given in [3].
    ! Restriction: alpha > 0 and 0 < d <= 15.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.
    ! [2] G. P. Bhattacharjee. The incomplete gamma integral. Applied Statistics,
    !     19:285–287, 1970. AS32.
    ! [3] D. B. Peizer and J. W. Pratt. A normal approximation for binomial,
    !     F, beta, and other common related tail probabilities.  Journal of the
    !     American Statistical Association, 63:1416–1456, 1968.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: Alpha, X
    tInteger, INTENT(IN)    :: D
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: ALIM = 1.0D5

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (Alpha <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_Gamma', SubModName, ErrWarning, 'Alpha <= 0')
        RETURN
    END IF
    IF (D <= 0) THEN
        CALL Handle_ErrLevel('fdist_Gamma', SubModName, ErrWarning, 'D <= 0')
        RETURN
    END IF
    IF (D > 15) THEN
        CALL Handle_ErrLevel('fdist_Gamma', SubModName, ErrWarning, 'D > 15')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (ONE_DBL == Alpha) THEN
        ResVal = fdist_Expon (X)
        RETURN
    END IF

    IF (Alpha >= ALIM) THEN
        BLOCK
            tDouble    :: D2, S, Z
            D2 = X + ONE_DBL/3.0_kDouble - Alpha - 0.02_kDouble/Alpha
            S = Alpha - HALF_DBL
            Z = D2 * SQRT((ONE_DBL + fdist_belog(S/X))/X)
            ResVal = fdist_Normal2 (Z)
        END BLOCK
        RETURN
    END IF

    IF ((X <= ONE_DBL).OR.(X < Alpha)) THEN
        BLOCK
            tDouble     :: EPS, V, Z, AN, Term
            EPS = EpsArray(D)
            V = EXP (Alpha * LOG(X) - X - num2_LnGamma (Alpha))
            Z = ONE_DBL
            Term = ONE_DBL
            AN = Alpha
            DO
                AN = AN + ONE_DBL
                Term = Term * (X / AN)
                Z = Z + Term
                IF (Term < EPS * Z) EXIT
            END DO
            ResVal = Z * V / Alpha
        END BLOCK
    ELSE
        ResVal = ONE_DBL - fbar_Gamma (Alpha, D, X)
    END IF

    RETURN

END FUNCTION fdist_Gamma

!******************************************************************************

MODULE RECURSIVE FUNCTION fdist_Beta(PIn, QIn, D, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the beta distribution function given by Eq. (21)
    ! in [1] with parameters p and q, evaluated at x in [0, 1], where B(p, q) is the
    ! beta function defined by Eq. (22) in [1], where Gamma(x) is the Gamma function
    ! defined by Eq. (20) in [1].  For max(p, q) <= 1000, use a recurrence relation
    ! in p and q for "fdist_Beta", given in [2] and [3].  If min(p, q) <= 30, use
    ! an approximation due to Bol'shev [4].  Otherwise, use a normal approximation [5].
    ! The function tries to return d decimals digits of precision.  For d <= 13, when
    ! the normal approximation is not used, d gives a good idea of the precision
    ! attained.  Restrictions:  p > 0, q > 0, x in [0, 1] and 0 < d <= 15.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.
    ! [2] W. Gautschi. Algorithm 222: Incomplete beta function ratios. Communications
    !     of the ACM, 7(3):143–144, 1964.
    ! [3] W. Gautschi. Certification of algorithm 222: Incomplete beta function ratios.
    !     Communications of the ACM, 7(3):244, 1964.
    ! [4] K. V. Mardia and P. J. Zemroch. Tables of the F and Related Distributions with
    !     Algorithms. Academic Press, London, 1978.
    ! [5] D. B. Peizer and J. W. Pratt. A normal approximation for binomial, F, beta,
    !     and othercommon related tail probabilities. Journal of the American Statistical
    !     Association, 63:1416–1456, 1968.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: PIn, QIn, X
    tInteger, INTENT(IN)    :: D
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: RENORM = 1.0D+300
    tDouble, PARAMETER  :: PQMax  = 1000.0_kDouble
    tDouble, PARAMETER  :: PQLim  = 30.0_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: N
    tLogical                :: Flag
    tDouble                 :: P, Q, P0, Q0, U, Y
    tDouble                 :: Temp, YD, Gamma, H1, H3
    tDouble, ALLOCATABLE    :: II(:)

! FLOW

    ! The exact section of fdist_Beta below is very slow for large parameters.
    ! It is an old algorithm of Gautschi of 1964. There is an algorithm
    ! for fdist_Beta (1994) that is recent and is supposed to be very fast.
    !
    ! II[j] will contain either the values of fdist_Beta (p0 + j, q, d, x),
    ! where 0 < p0 <= 1, for j = 0, 1, 2, ..., n,  with p = p0 + n; or the
    ! values of fdist_Beta (p, q0 + j, d, x), where 0 < q0 <= 1, for j = 0,
    ! 1, 2, ..., n, with q = q0 + n.

    P = PIn
    Q = QIn

    IF (P <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_Beta', SubModName, ErrWarning, 'P <= 0.')
        RETURN
    ELSEIF (Q <= ZERO_DBL) THEN
        CALL Handle_ErrLevel('fdist_Beta', SubModName, ErrWarning, 'Q <= 0.')
        RETURN
    ELSEIF (D <= 0) THEN
        CALL Handle_ErrLevel('fdist_Beta', SubModName, ErrWarning, 'D <= 0.')
        RETURN
    ELSEIF (D > 15) THEN
        CALL Handle_ErrLevel('fdist_Beta', SubModName, ErrWarning, 'D > 15.')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X >= ONE_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    IF (MAX(P, Q) <= PQMax) THEN
        IF (P < Q) THEN
            ! integer part of P
            N = ToInteger(P)
            ! fractional part of P
            P0 = P - N
            IF (P0 <= ZERO_DBL) THEN
                ! P0 == 0 not allowed
                P0 = ONE_DBL
                N = N - 1
            END IF
            CALL MemAlloc(II, N + 1_kIndex, StartID=0_kIndex)
            CALL Beta_Q_Fixed (P0, Q, X, D, N, II)
            U = II(N)
            CALL MemFree (II)
            ! There may be numerical errors far in the tails giving very small
            ! negative values instead of 0.
            IF (U <= ZERO_DBL) THEN
                ResVal = ZERO_DBL
            ELSEIF (U <= ONE_DBL) THEN
                ResVal = U
            ELSE
                ResVal = ONE_DBL
            END IF
        ELSE
            ! integer part of Q
            N = ToInteger(Q)
            ! fractional part of Q
            Q0 = Q - N
            IF (Q0 <= ZERO_DBL) THEN
                ! Q0 == 0 not allowed
                Q0 = ONE_DBL
                N = N - 1
            END IF
            CALL MemAlloc(II, N + 1_kIndex, StartID=0_kIndex)
            CALL Beta_P_Fixed (P, Q0, X, D, N, II)
            U = II(N)
            CALL MemFree (II)
            ! There may be numerical errors far in the tails giving very small
            ! negative values instead of 0.
            IF (U <= ZERO_DBL) THEN
                ResVal = ZERO_DBL
            ELSEIF (U <= ONE_DBL) THEN
                ResVal = U
            ELSE
                ResVal = ONE_DBL
            END IF
        END IF
        RETURN
    END IF

    IF (((P > PQMax).AND.(Q < PQLim)).OR.((Q > PQMax).AND.(P < PQLim))) THEN
        ! Bol'shev approximation for large max(P, Q) and small min(P, Q)
        IF (X > HALF_DBL) THEN
            ResVal = ONE_DBL - fdist_Beta (Q, P, D, ONE_DBL - X)
            RETURN
        END IF

        IF (P < Q) THEN
            U = P
            P = Q
            Q = U
            Flag = FalseVal
        ELSE
            Flag = TrueVal
        END IF
        U = P + HALF_DBL * Q - HALF_DBL
        IF (.NOT.Flag) THEN
            Temp = X / (TWO_DBL - X)
        ELSE
            Temp = (ONE_DBL - X) / (ONE_DBL + X)
        END IF
        YD = TWO_DBL * U * Temp
        Gamma = (EXP(Q * LOG(YD) - YD - num2_LnGamma (Q)) *                      &
                 (TWO_DBL * YD * YD - (Q - ONE_DBL) * YD - (Q * Q - ONE_DBL))) / &
                 (24.0_kDouble * U * U)
        IF (Flag) THEN
            YD = fbar_Gamma (Q, D, YD)
            ResVal = YD - Gamma
        ELSE
            YD = fdist_Gamma (Q, D, YD)
            ResVal = YD + Gamma
        END IF
        RETURN
    END IF

    ! Normal approximation of Peizer and Pratt
    H1 = P + Q - ONE_DBL
    Y = ONE_DBL - X
    H3 = SQRT ((ONE_DBL + Y * fdist_belog ((P - HALF_DBL) / (H1 * X))   &
              + X * fdist_belog ((Q - HALF_DBL) / (H1 * Y)))            &
              / ((H1 + ONE_DBL / 6.0_kDouble) * X * Y))                 &
       * ((H1 + ONE_DBL / 3.0_kDouble + 0.02_kDouble *                  &
          (ONE_DBL / P + ONE_DBL / Q + ONE_DBL / (P + Q))) * X - P +    &
           ONE_DBL / 3.0_kDouble - 0.02_kDouble / P - 0.01_kDouble / (P + Q))
    ResVal = fdist_Normal2 (H3)

    RETURN

    CONTAINS

    FUNCTION Isubx_PQ_Small(P, Q, X, D) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To evaluate fdist_Beta (p, q, d, x) when 0 < p <= 1 and 0 < q <= 2 to
        ! a precision of d = -log10 (2 epsilon) decimal digits. Uses a Series
        ! expansion in powers of x.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X
        tInteger, INTENT(IN)    :: D
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: K
        tDouble     :: S, U, V, Eps

    ! FLOW

        IF ((P <= ZERO_DBL).AND.(P > ONE_DBL)) THEN
            CALL Handle_ErrLevel('Isubx_PQ_Small', SubModName, ErrWarning, 'P Not in (0, 1].')
            RETURN
        ELSEIF ((Q <= ZERO_DBL).AND.(Q > TWO_DBL)) THEN
            CALL Handle_ErrLevel('Isubx_PQ_Small', SubModName, ErrWarning, 'Q Not in (0, 2].')
            RETURN
        END IF

        K = 0
        Eps = EpsArray(D)
        U = X**P
        S = U / P
        DO
            U = ((K + 1) - Q) * X * U / (K + 1)
            V = U / ((K + 1) + P)
            S = S + V
            K = K + 1
            IF ((ABS (V) / S) <= Eps) EXIT
        END DO

        V = num2_LnGamma (P + Q) - num2_LnGamma (P) - num2_LnGamma (Q)
        ResVal = S * EXP (V)

        RETURN

    END FUNCTION Isubx_PQ_Small

    !**************************************************************************

    SUBROUTINE Forward(P, Q, X, I0, I1, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Given I0 = fdist_Beta (p, q, x) and I1 = fdist_Beta (p, q + 1, x),
        ! generates fdist_Beta (p, q + n, x) for n = 0, 1, 2, ..., nmax, and
        ! stores the result in II.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X, I0, I1
        tInteger, INTENT(IN)    :: NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: N

    ! FLOW

        II(0) = I0
        IF (NMaX > 0) II(1) = I1
        DO N = 1, NMax-1
            II(N + 1) = (1 + ((N - 1) + P + Q) * (ONE_DBL - X) / (N + Q)) * II(N) &
                           - ((N - 1) + P + Q) * (ONE_DBL - X) * II(N - 1) / (N + Q)
        END DO

        RETURN

    END SUBROUTINE Forward

    !**************************************************************************

    SUBROUTINE Backward(P, Q, X, I0, D, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Given I0 = fdist_Beta (p, q, x), generates fdist_Beta (p + n, q, x)
        ! for n = 0, 1, 2,..., nmax to d significant digits, using a variant of
        ! J.C.P. Miller's backward recurrence algorithm. Stores the result in II.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X, I0
        tInteger, INTENT(IN)    :: D, NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex                  :: NTab
        tInteger                :: N, Nu, M, Again
        tDouble, ALLOCATABLE    :: ITemp(:), IApprox(:), Rr(:)
        tDouble                 :: Eps, R

    ! FLOW

        II(0) = I0
        IF (NMax == 0) RETURN

        Eps = EpsArray(D)
        Nu = 2 * NMax + 5
        NTab = 64
        DO WHILE (NTab <= Nu)
            NTab = NTab * 2
        END DO

        CALL MemAlloc(Rr,      NTab, StartID=0_kIndex)
        CALL MemAlloc(IApprox, NTab, StartID=0_kIndex)
        CALL MemAlloc(ITemp,   NTab, StartID=0_kIndex)

        IApprox(1:NMax) = ZERO_DBL
        ITemp(0:NMax)   = II(0:NMax)

        DO
            N = Nu
            R = ZERO_DBL
            DO
                R = ((N - 1) + P + Q) * X / (N + P + ((N - 1) + P + Q) * X - (N + P) * R)
                IF (N <= NMax) Rr(N - 1) = R
                N = N -1
                IF (N < 1) EXIT
            END DO

            DO N = 0, NMax
                ITemp(N + 1) = Rr(N) * ITemp(N)
            END DO

            Again = 0
            DO N = 1, NMax
                IF (ABS ((ITemp(N) - IApprox(N))/ITemp(N)) > Eps) THEN
                    Again = Again + 1
                    IApprox(1:NMax) = ITemp(1:NMax)
                    Nu = Nu + 5
                    IF (NTab <= Nu) THEN
                        NTab = NTab * 2
                        CALL MemResize(Rr,      NTab)
                        CALL MemResize(IApprox, NTab)
                        CALL MemResize(ITemp,   NTab)
                    END IF
                    EXIT
                END IF
            END DO
            IF (Again == 0) EXIT
        END DO

        II(0:NMax) = ITemp(0:NMax)
        CALL MemFree (Rr)
        CALL MemFree (IApprox)
        CALL MemFree (ITemp)

        RETURN

    END SUBROUTINE Backward

    !**************************************************************************

    SUBROUTINE Isubx_Q_Fixed(P, Q, X, D, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Generates fdist_Beta (p + n, q, x), 0 < p <= 1, for n = 0, 1, 2,...,
        ! nmax to d significant digits, using procedure backward. First reduces
        ! q modulo 1 to q0, where 0 < q0 <= 1.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X
        tInteger, INTENT(IN)    :: D, NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex                  :: MMax
        tInteger                :: M
        tDouble                 :: S, Q0, IQ0, IQ1
        tDouble, ALLOCATABLE    :: IQ(:)

    ! FLOW

        IF ((P <= ZERO_DBL).AND.(P > ONE_DBL)) THEN
            CALL Handle_ErrLevel('Isubx_Q_Fixed', SubModName, ErrWarning, 'P Not in (0, 1].')
            RETURN
        END IF

        ! integer part of q
        M = ToInteger(Q)
        ! fractional part of q
        S = Q - M

        IF (S > ZERO_DBL) THEN
            Q0 = S
            MMax = M
        ELSE
            Q0 = S + 1
            MMax = M - 1
        END IF
        IQ0 = RENORM * Isubx_PQ_Small (P, Q0, X, D)
        IF (MMax > 0_kIndex) IQ1 = RENORM * Isubx_PQ_Small (P, Q0 + ONE_DBL, X, D)

        CALL MemAlloc(IQ,  MMax + 1_kIndex, StartID=0_kIndex)
        CALL Forward (P, Q0, X, IQ0, IQ1, ToInteger(MMax), IQ)
        CALL Backward (P, Q, X, IQ(MMax), D, NMax, II)
        II(0:NMax) = II(0:NMax) / RENORM
        CALL MemFree (IQ)

        RETURN

    END SUBROUTINE Isubx_Q_Fixed

    !**************************************************************************

    SUBROUTINE Isubx_P_Fixed(P, Q, X, D, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Generates fdist_Beta (p, q + n, x), 0 < q <= 1, for n = 0, 1, 2,...,
        ! nmax to d significant digits, using procedure forward.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X
        tInteger, INTENT(IN)    :: D, NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex                  :: MMax
        tInteger                :: M
        tDouble                 :: S, P0, I0, I1, IQ0, IQ1
        tDouble, ALLOCATABLE    :: IP(:)

    ! FLOW

        IF ((Q <= ZERO_DBL).AND.(Q > ONE_DBL)) THEN
            CALL Handle_ErrLevel('Isubx_P_Fixed', SubModName, ErrWarning, 'Q Not in (0, 1].')
            RETURN
        END IF

        ! integer part of p
        M = ToInteger(P)
        ! fractionnal part of p
        S = P - M
        IF (S > ZERO_DBL) THEN
            P0 = S
            MMax = M
        ELSE
            P0 = S + 1
            MMax = M - 1
        END IF
        I0 = RENORM * Isubx_PQ_Small (P0, Q, X, D)
        I1 = RENORM * Isubx_PQ_Small (P0, Q + ONE_DBL, X, D)

        CALL MemAlloc(IP,  MMax + 1_kIndex, StartID=0_kIndex)
        CALL Backward (P0, Q, X, I0, D, ToInteger(MMax), IP)
        IQ0 = IP(MMax)
        CALL Backward (P0, Q + ONE_DBL, X, I1, D, ToInteger(MMax), IP)
        IQ1 = IP(MMax)
        CALL Forward (P, Q, X, IQ0, IQ1, NMax, II)
        II(0:NMax) = II(0:NMax) / RENORM
        CALL MemFree (IP)

        RETURN

    END SUBROUTINE Isubx_P_Fixed

    !**************************************************************************

    SUBROUTINE Beta_Q_Fixed(P, Q, X, D, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! ....

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X
        tInteger, INTENT(IN)    :: D, NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: N

    ! FLOW

        IF ((P <= ZERO_DBL).AND.(P > ONE_DBL)) THEN
            CALL Handle_ErrLevel('Beta_Q_Fixed', SubModName, ErrWarning, 'P Not in (0, 1].')
            RETURN
        ELSEIF (Q <= ZERO_DBL) THEN
            CALL Handle_ErrLevel('Beta_Q_Fixed', SubModName, ErrWarning, 'Q <= 0.')
            RETURN
        ELSEIF (NMax < 0) THEN
            CALL Handle_ErrLevel('Beta_Q_Fixed', SubModName, ErrWarning, 'NMax < 0.')
            RETURN
        END IF

        IF ((X == ZERO_DBL).OR.(X == ONE_DBL)) THEN
            II(0:NMax) = X
            RETURN
        END IF
        IF (X <= HALF_DBL) THEN
            CALL Isubx_Q_Fixed (P, Q, X, D, NMax, II)
        ELSE
            CALL Isubx_P_Fixed (Q, P, ONE_DBL - X, D, NMax, II)
            II(0:NMax) = ONE_DBL - II(0:NMax)
        END IF

        RETURN

    END SUBROUTINE Beta_Q_Fixed

    !**************************************************************************

    SUBROUTINE Beta_P_Fixed(P, Q, X, D, NMax, II)

    !** PURPOSE OF THIS SUBROUTINE:
        ! ....

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: P, Q, X
        tInteger, INTENT(IN)    :: D, NMax
        tDouble,  INTENT(INOUT) :: II(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: N

    ! FLOW

        IF ((Q <= ZERO_DBL).AND.(Q > ONE_DBL)) THEN
            CALL Handle_ErrLevel('Beta_P_Fixed', SubModName, ErrWarning, 'Q Not in (0, 1].')
            RETURN
        ELSEIF (P <= ZERO_DBL) THEN
            CALL Handle_ErrLevel('Beta_P_Fixed', SubModName, ErrWarning, 'P <= 0.')
            RETURN
        ELSEIF (NMax < 0) THEN
            CALL Handle_ErrLevel('Beta_P_Fixed', SubModName, ErrWarning, 'NMax < 0.')
            RETURN
        END IF

        IF ((X == ZERO_DBL).OR.(X == ONE_DBL)) THEN
            II(0:NMax) = X
            RETURN
        END IF
        IF (X <= HALF_DBL) THEN
            CALL Isubx_P_Fixed (P, Q, X, D, NMax, II)
        ELSE
            CALL Isubx_Q_Fixed (Q, P, ONE_DBL - X, D, NMax, II)
            II(0:NMax) = ONE_DBL - II(0:NMax)
        END IF

        RETURN

    END SUBROUTINE Beta_P_Fixed

    !**************************************************************************

END FUNCTION fdist_Beta

!******************************************************************************
MODULE FUNCTION fdist_BetaSymmetric(P, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of the symmetrical beta distribution function F(x)
    ! with parameters p = q as defined by Eq (21) in [1].  Uses four different
    ! hypergeometric series (for the four cases x close to 0 and p <= 1, x close to 0
    ! and p > 1, x close to 1/2 and p <= 1, and x close to 1/2 and p > 1) to compute
    ! F(x).  For p > 100000, uses a normal approximation given in [2]. Restrictions: p > 0
    ! and x in [0, 1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.
    ! [2] D. B. Peizer and J. W. Pratt. A normal approximation for binomial, F, beta,
    !     and othercommon related tail probabilities. Journal of the American Statistical
    !     Association, 63:1416–1456, 1968.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,  INTENT(IN)    :: P, X
    tDouble                 :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: EPSBETA    = 0.5D-10             ! < 0.75 sqrt(DBL_EPSILON)
    tDouble,  PARAMETER :: ALPHALIM   = 100000.0_kDouble    ! Limiting alpha for normal approx.
    tInteger, PARAMETER :: MAXJ       = 2000                ! Max number of terms in Series
    tDouble,  PARAMETER :: INV2PI     = 0.6366197723675813_kDouble  ! 2 / PI
    tDouble,  PARAMETER :: LOG4       = 1.38629436111989062_kDouble ! Ln(4)
    tDouble,  PARAMETER :: OneRac2    = 0.70710678118654752_kDouble ! 1/sqrt(2)
    tDouble,  PARAMETER :: SQPI_2     = 0.88622692545275801_kDouble ! Sqrt(Pi) / 2
    tDouble,  PARAMETER :: LOG_SQPI_2 = -0.1207822376352453_kDouble ! Ln(Sqrt(Pi) / 2)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Alpha, Temp, U, LogB, LogC
    tLogical    :: IsUpper              ! True if X > 0.5
    tDouble     :: B, C, X0, Y, Z       ! B => Beta(alpha, alpha)

! FLOW

    ! Compute the cumulative probability of the symmetrical beta distribution.
    ! Returns a negative value on error, otherwise returns U in [0, 1].

    ! initialize
    Alpha = P
    Z     = X
    B     = ZERO_DBL

    IF (Alpha <= ZERO_DBL) THEN
        ResVal = -ONE_DBL
        CALL Handle_ErrLevel('fdist_BetaSymmetric', SubModName, ErrWarning, 'P <= 0.')
        RETURN
    END IF

    IF (Z <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (Z >= ONE_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (Z == HALF_DBL) THEN
        ResVal = HALF_DBL
        RETURN
    ELSEIF (Alpha == ONE_DBL) THEN
        ! alpha = 1 is the uniform law
        ResVal = Z
        RETURN
    ELSEIF (Alpha == HALF_DBL) THEN
        ! alpha = 1/2 is the arcsin law
        ResVal = INV2PI * ASIN(SQRT(Z))
        RETURN
    ELSEIF (Alpha > ALPHALIM) THEN
        ResVal = Peizer(Alpha, Z)
        RETURN
    END IF

    IF (Z > HALF_DBL) THEN
        Z = ONE_DBL - Z
        IsUpper = TrueVal
    ELSE
        IsUpper = FalseVal
    END IF

    CALL fdist_CalcB4 (Alpha, B, LogB, C, LogC)

    IF (Alpha <= ONE_DBL) THEN
        ! For Z = X0, both series use the same number of terms to get the
        ! required precision
        IF (Z > 0.25_kDouble) THEN
            Temp = -LOG (Alpha)
            IF (Alpha >= 1.0D-6) THEN
                X0 = 0.25_kDouble + 0.005_kDouble * Temp
            ELSE
                X0 = 0.13863_kDouble + 0.01235_kDouble * Temp
            END IF
        ELSE
            X0 = 0.25_kDouble
        END IF
        IF (Z <= X0) THEN
            U = (Series1 (Alpha, Z)) / B
        ELSE
            U = HALF_DBL - (Series2 (Alpha, HALF_DBL - Z)) / C
        END IF
    ELSE
        ! 1 < Alpha < ALPHALIM
        IF (Alpha < 400.0_kDouble) THEN
            X0 = HALF_DBL - 0.45_kDouble / SQRT(Alpha)
        ELSE
            X0 = HALF_DBL - ONE_DBL / SQRT(Alpha)
        END IF
        IF (X0 < 0.25_kDouble) X0 = 0.25_kDouble
        IF (Z <= X0) THEN
            Temp = (Alpha - ONE_DBL) * LOG (Z * (ONE_DBL - Z)) - LogB
            U = Series3 (Alpha, Z) * EXP(Temp) / Alpha
        ELSE
            Y = HALF_DBL - Z
            Temp = num2_LOG1P(-4.0_kDouble*Y*Y)
            Temp = Alpha * Temp - LogC
            U = HALF_DBL - (Series4 (Alpha, Y)) * EXP(Temp)
        END IF
    END IF

    IF (IsUpper) THEN
        ResVal = ONE_DBL - U
    ELSE
        ResVal = U
    END IF

    RETURN

    CONTAINS

    FUNCTION Series1(Alpha, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Compute the hypergeometric series for F(x).
        ! This Series is used for alpha < 1 and x close to 0.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, X
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: Sum, Term, Poc

    ! FLOW

        Poc = ONE_DBL
        Sum = ONE_DBL / Alpha
        J = 1
        DO
            Poc = Poc * (X * (J - Alpha) / J)
            Term = Poc / (J + Alpha)
            Sum = Sum + Term
            J = J + 1
            IF (.NOT.((Term > Sum * EPSTOL).AND.(J < MAXJ))) EXIT
        END DO

        ResVal = Sum * (X**Alpha)

        RETURN

    END FUNCTION Series1

    !**************************************************************************

    FUNCTION Series2(Alpha, Y) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Compute the hypergeometric for G(y).   y = 0.5 - x.
        ! This Series is used for alpha < 1 and x close to 1/2.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, Y
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: Sum, Term, Poc, Z

    ! FLOW

        Z = 4.0_kDouble * Y * Y

        ! Compute the Series for G(y)
        Poc = ONE_DBL
        Sum = ONE_DBL
        J = 1
        DO
            Poc = Poc * (Z * (J - Alpha) / J)
            Term = Poc / (2 * J + 1)
            Sum = Sum + Term
            J = J + 1
            IF (.NOT.((Term > Sum * EPSTOL).AND.(J < MAXJ))) EXIT
        END DO

        ResVal = Sum * Y

        RETURN

    END FUNCTION Series2

    !**************************************************************************

    FUNCTION Series3(Alpha, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Compute the hypergeometric series for F(x).
        ! This Series is used for alpha > 1 and x close to 0.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, X
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: Sum, Term, Z

    ! FLOW

        Z = -X / (ONE_DBL - X)
        Sum = ONE_DBL
        Term = ONE_DBL
        J = 1
        DO
            Term = Term * (Z * (J - Alpha) / (J + Alpha))
            Sum = Sum + Term
            J = J + 1
            IF (.NOT.((ABS(Term) > Sum * EPSTOL).AND.(J < MAXJ))) EXIT
        END DO

        ResVal = Sum * X

        RETURN

    END FUNCTION Series3

    !**************************************************************************

    FUNCTION Series4(Alpha, Y) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Compute the hypergeometric for G(y).   y = 0.5 - x.
        ! This Series is used for alpha > 1 and x close to 1/2.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, Y
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: Sum, Term, Z

    ! FLOW

        Z = 4.0_kDouble * Y * Y

        ! Compute the Series for G(y)
        Term = ONE_DBL
        Sum  = ONE_DBL
        J = 1
        DO
            Term = Term * (Z * (J + Alpha - HALF_DBL) / (HALF_DBL + J))
            Sum = Sum + Term
            J = J + 1
            IF (.NOT.((Term > Sum * EPSTOL).AND.(J < MAXJ))) EXIT
        END DO

        ResVal = Sum * Y

        RETURN

    END FUNCTION Series4

    !**************************************************************************

    FUNCTION Peizer(Alpha, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute Normal approximation of Peizer and Pratt.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Alpha, X
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: Y, Z

    ! FLOW

        Y = ONE_DBL - Z
        Z = SQRT((ONE_DBL - Y * fdist_belog (TWO_DBL * X) - X * fdist_belog (TWO_DBL * Y)) &
                /((TWO_DBL*Alpha - 5.0_kDouble / 6.0_kDouble) * X * Y)) *                  &
                (TWO_DBL*X - ONE_DBL) * (Alpha - ONE_DBL / 3.0_kDouble + 0.025_kDouble / Alpha)
        ResVal = fdist_Normal2 (Z)

        RETURN

    END FUNCTION Peizer

    !**************************************************************************

END FUNCTION fdist_BetaSymmetric

!******************************************************************************
MODULE FUNCTION fdist_KSPlus(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return p = P(Dn+ <= x) where Dn+ (given by Eq (23) in [1]) is the positive
    ! Kolmogorov-Smirnov statistic for a sample of size n.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: NxParam = 6.5_kDouble    ! frontier: alternating series
    tIndex,   PARAMETER :: NParam  = 4000           ! frontier: non-alternating series

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Q, Sum, Term

! FLOW

    IF (N < 1) THEN
        CALL Handle_ErrLevel('fdist_KSPlus', SubModName, ErrWarning, 'N < 1.')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF ((X >= ONE_DBL).OR.(N*X*X >= 25.0_kDouble)) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (N == 1) THEN
        ResVal = X
        RETURN
    END IF

    !--------------------------------------------------------------!
    ! the alternating series is stable and fast for N*x very small !
    !--------------------------------------------------------------!
    IF (N * X <= NxParam) THEN
        BLOCK
            tDouble     :: Eps, LogCom, JReal, NJReal
            tInteger    :: Sign
            tIndex      :: J, JMax
            ! execution
            Eps = 1.0D-300
            LogCom = LOG(REAL(N, KIND=kDouble))
            Sign = -1
            JMax = ToLong(N * X)
            Sum = ZERO_DBL
            DO J = 1, JMax
                JReal = J
                NJReal = N - J
                Q = JReal / N - X
                ! we must avoid log(0.0) for J = JMax and N*X near an integer
                IF (-Q > Eps) THEN
                    Term = LogCom + JReal * LOG (-Q) + (NJReal - ONE_DBL) * num2_LOG1P (-Q)
                    Sum = Sum + Sign * EXP (Term)
                END IF
                Sign = -Sign
                LogCom = LogCom + LOG (NJReal / (J + 1))
            END DO
        END BLOCK
        ! add the term j = 0
        Sum = Sum + EXP ((N - 1) * num2_LOG1P (X))
        IF (Sum >= ZERO_DBL) THEN
            ResVal = Sum * X
        ELSE
            ResVal = ZERO_DBL
        END IF
        RETURN
    END IF

    IF (N <= NParam) THEN
        BLOCK
            tDouble     :: LogCom, JReal, NJReal
            tIndex      :: J, JMax
            ! execution
            LogCom = LOG(REAL(N, KIND=kDouble))
            Sum = ZERO_DBL
            JMax = ToLong(N * (ONE_DBL - X))
            IF ((ONE_DBL - X - REAL(JMax/N, KIND=kDouble)) <= ZERO_DBL) JMax = JMax - 1
            DO J = 1, JMax
                JReal = J
                NJReal = N - J
                Q = JReal / N + X
                Term = LogCom + (JReal - ONE_DBL) * LOG (Q) + NJReal * num2_LOG1P(-Q)
                Sum = Sum + EXP (Term)
                LogCom = LogCom + LOG (NJReal / (JReal + ONE_DBL))
            END DO
        END BLOCK
        Sum = Sum * X

        ! add the term j = 0 avoid log(0.0)
        IF (ONE_DBL > X) Sum = Sum + EXP (N * num2_LOG1P(-X))
        Sum = ONE_DBL - Sum
        IF (Sum >= ZERO_DBL) THEN
            ResVal = Sum
        ELSE
            ResVal = ZERO_DBL
        END IF
        RETURN
    END IF

    !---------------------------!
    ! Use an asymptotic formula !
    !---------------------------!

    Term = TWO_DBL / 3.0_kDouble
    Q = X * X * N
    Sum = ONE_DBL - EXP (-TWO_DBL * Q) * (ONE_DBL - Term * X * (ONE_DBL - X * (ONE_DBL - Term * Q) &
                  - Term / N * (0.2_kDouble - 19.0_kDouble / 15.0_kDouble * Q + Term * Q * Q)))
    IF (Sum >= ZERO_DBL) THEN
        ResVal = Sum
    ELSE
        ResVal = ZERO_DBL
    END IF

    RETURN

END FUNCTION fdist_KSPlus

!******************************************************************************
MODULE FUNCTION fdist_KS1(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return p = P(Dn <= x) where Dn = max(Dn+, Dn-) is the two-sided
    ! Kolmogorov-Smirnov statistic for a sample of size n, and Dn+ and Dn-
    ! are given by Eq (23) and (24) in [1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: NSEP  = 400
    tInteger, PARAMETER :: NSEP2 = 4000
    tDouble,  PARAMETER :: ZSEP  = 4.0_kDouble
    tDouble,  PARAMETER :: ZSEP2 = 0.2_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: U

! FLOW

    U = KSSpecial(N, X)
    IF (U >= ZERO_DBL) THEN
        ResVal = U
        RETURN
    END IF

    IF (N <= NSEP) THEN
        IF (N*X*X < ZSEP) THEN
            ResVal = Pomeranz (N, X)
        ELSE
            ResVal = ONE_DBL - fbar_KS1(N, X)
        END IF
        RETURN
    END IF

    IF ((N*X*X <= ZSEP2).AND.(N <= NSEP2)) THEN
        ResVal = Pomeranz (N, X)
    ELSE
        ResVal = Pelz (N, X)
    END IF

    RETURN

    CONTAINS

    FUNCTION KSSpecial(N, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For nx^2 > 18, fbar_KS(n, x) is smaller than DBL_EPSILON

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,  INTENT(IN) :: N
        tDouble, INTENT(IN) :: X
        tDouble             :: ResVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: NLIM = 20

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: T, W

    ! FLOW

        IF ((N*X*X >= 18.0_kDouble).OR.(X >= ONE_DBL)) THEN
            ResVal = ONE_DBL
        ELSEIF (X <= HALF_DBL / N) THEN
            ResVal = ZERO_DBL
        ELSEIF (N == 1) THEN
            ResVal = TWO_DBL * X - ONE_DBL
        ELSEIF (X <= ONE_DBL / N) THEN
            T = TWO_DBL * X - ONE_DBL / N
            IF (N <= NLIM) THEN
                W = num2_Factorial (ToInteger(N))
                ResVal = W * (T**N)
            ELSE
                W = num2_LnFactorial (ToInteger(N)) + N * LOG (T)
                ResVal = EXP (W)
            END IF
        ELSEIF (X >= ONE_DBL - ONE_DBL / N) THEN
            ResVal = ONE_DBL - TWO_DBL * ((ONE_DBL - X)**N)
        ELSE
            ResVal = -ONE_DBL
        END IF

        RETURN

    END FUNCTION KSSpecial

    !**************************************************************************
    FUNCTION Pelz(N, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute Kolmogorov-Smirnov statistic using the Pelz-Good asymptotic expansion

    !** REFERENCE:
        ! [1] Wolfgang Pelz and I. J. Good, Journal of the Royal Statistical Society,
        !     Series B. Vol. 38, No. 2 (1976), pp. 152-156

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,  INTENT(IN) :: N
        tDouble, INTENT(IN) :: X
        tDouble             :: ResVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: JMAX = 20
        tDouble,  PARAMETER :: EPS = 1.0D-10
        tDouble,  PARAMETER :: C = 2.506628274631001_kDouble    ! sqrt(2*Pi)
        tDouble,  PARAMETER :: C2 = 1.2533141373155001_kDouble  ! sqrt(Pi/2)
        tDouble,  PARAMETER :: PI2 = num_Pi * num_Pi
        tDouble,  PARAMETER :: PI4 = PI2 * PI2

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: RACN, Z, Z2, Z4, Z6, W
        tDouble     :: TI, Term, Tom, Sum
        tInteger    :: J

    ! FLOW

        ! initialize
        RACN = SQRT(REAL(N, KIND=kDouble))
        Z = RACN * X
        Z2 = Z * Z
        Z4 = Z2 * Z2
        Z6 = Z4 * Z2
        W = PI2 / (TWO_DBL * Z*Z)
        Term = ONE_DBL
        J = 0
        Sum = ZERO_DBL

        DO WHILE ((J <= JMAX).AND.(Term > EPS * Sum))
            TI = J + HALF_DBL
            Term = EXP (-TI * TI * W)
            Sum = Sum + Term
            J = J + 1
        END DO
        Sum = Sum * (C / Z)

        Term = ONE_DBL
        Tom = ZERO_DBL
        J = 0
        DO WHILE ((J <= JMAX).AND.(ABS(Term) > EPS * ABS(Tom)))
            TI = J + HALF_DBL
            Term = (PI2 * TI * TI - Z2) * EXP (-TI * TI * W)
            Tom = Tom + Term
            J = J + 1
        END DO
        Sum = Sum + Tom * C2 / (RACN * 3.0_kDouble * Z4)

        Term = ONE_DBL
        Tom = ZERO_DBL
        J = 0
        DO WHILE ((J <= JMAX).AND.(ABS(Term) > EPS * ABS(Tom)))
            TI = J + HALF_DBL
            Term = 6.0_kDouble*Z6 + TWO_DBL*Z4 + PI2*(TWO_DBL*Z4 - 5.0_kDouble*Z2)*TI*TI &
                 + PI4*(ONE_DBL - TWO_DBL*Z2)*TI*TI*TI*TI
            Term = Term * EXP (-TI * TI * W)
            Tom = Tom + Term
            J = J + 1
        END DO
        Sum = Sum + Tom * C2 / (N * 36.0_kDouble * Z * Z6)

        Term = ONE_DBL
        Tom = ZERO_DBL
        J = 1
        DO WHILE ((J <= JMAX).AND.(Term > EPS * Tom))
            TI = J
            Term = PI2 * TI * TI * EXP (-TI * TI * W)
            Tom = Tom + Term
            J = J + 1
        END DO
        Sum = Sum - Tom * C2 / (N * 18.0_kDouble * Z * Z2)

        Term = ONE_DBL
        Tom = ZERO_DBL
        J = 0
        DO WHILE ((J <= JMAX).AND.(ABS(Term) > EPS * ABS(Tom)))
            TI = J + HALF_DBL
            TI = TI * TI
            Term = -30.0_kDouble*Z6 -90.0_kDouble*Z6*Z2             &
                 + PI2*(135.0_kDouble*Z4 - 96.0_kDouble*Z6)*TI      &
                 + PI4*(212.0_kDouble*Z4 - 60.0_kDouble*Z2)*TI*TI   &
                 + PI2*PI4*TI*TI*TI*(5.0_kDouble - 30.0_kDouble*Z2)
            Term = Term * EXP (-TI * W)
            Tom = Tom + Term
            J = J + 1
        END DO
        Sum = Sum + Tom * C2 / (RACN * N * 3240.0_kDouble * Z4 * Z6)

        Term = ONE_DBL
        Tom = ZERO_DBL
        J = 1
        DO WHILE ((J <= JMAX).AND.(ABS(Term) > EPS * ABS(Tom)))
            TI = J*J
            Term = (3.0_kDouble*PI2 * TI * Z2 - PI4*TI*TI) * EXP (-TI * W)
            Tom = Tom + Term
            J = J + 1
        END DO
        Sum = Sum + Tom * C2 / (RACN * N * 108.0_kDouble * Z6)

        ResVal = Sum

        RETURN

    END FUNCTION Pelz

    !**************************************************************************
    FUNCTION Pomeranz(N, X) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compute Kolmogorov-Smirnov statistic using Pomeranz’s recursion formula

    !** REFERENCE:
        ! [1] J. R. Brown and M. E. Harvey. Rational arithmetic Mathematica functions
        !     to evaluate the two-sided one sample K-S cumulative sample distribution.
        !     Journal of Statistical Software, 26(2):1–40, 2008.
        ! [2] J. Pomeranz. Exact cumulative distribution of the Kolmogorov-Smirnov
        !     statistic for small samples (algorithm 487). Communications of the ACM,
        !     17(12):703–704, 1974.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex,  INTENT(IN) :: N
        tDouble, INTENT(IN) :: X
        tDouble             :: ResVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tDouble,  PARAMETER :: EPS = 5.0D-13            ! for floors and ceilings
        tInteger, PARAMETER :: ENO = 350
        tDouble,  PARAMETER :: RENO = TWO_DBL**ENO      ! for renormalization of V
        tDouble,  PARAMETER :: IRENO = ONE_DBL/RENO

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble, ALLOCATABLE    :: A(:)
        tDouble, ALLOCATABLE    :: V(:,:)       ! work variables = pow(w, j-k)
        tDouble, ALLOCATABLE    :: H(:,:)       ! work variables = Factorial(j-k)
        tDouble                 :: T, W, Z
        tDouble                 :: Sum, MaxSum
        tInteger                :: Count        ! counter: how many renormalizations
        tInteger                :: R1, R2       ! Indices i and i-1 for V(i,..)
        tInteger                :: I, J, K, S
        tInteger                :: JLow, JUpp, KLow, KUpp, KUpp0

    ! FLOW

        CALL MemAlloc(A, 2_kIndex*N + 3_kIndex, StartID=0_kIndex)
        CALL MemAlloc(V, N + 2_kIndex, 2_kIndex, StartID1=0_kIndex, StartID2=0_kIndex)
        CALL MemAlloc(H, N + 1_kIndex, 4_kIndex, StartID1=0_kIndex, StartID2=0_kIndex)
        T = N*X

        A(0) = ZERO_DBL
        A(1) = ZERO_DBL
        Z = T - FLOOR(T)
        W = CEILING(T) - T
        IF (W < Z) Z = W
        A(2) = Z
        A(3) = ONE_DBL - A(2)
        DO I = 4, 2*N + 1
            A(I) = A(I-2) + ONE_DBL
        END DO
        A(2*N + 2) = N

        V(1:N+1, 0) = ZERO_DBL
        V(2:N+1, 1) = ZERO_DBL
        V(1, 1) = RENO
        Count = 1

        ! Precompute H(..,..) = (A(j) - A(j-1)^(j-k) / (j-k)! for speed
        H(0, 0) = ONE_DBL
        W = TWO_DBL * A(2) / N
        DO J = 1, N
            H(J, 0) = W * H(J - 1, 0) / J
        END DO
        H(0, 1) = ONE_DBL
        W = (ONE_DBL - TWO_DBL*A(2))/N
        DO J = 1, N
            H(J, 1) = W*H(J-1, 1) / J
        END DO
        H(0, 2) = ONE_DBL
        W = A(2)/N
        DO J = 1, N
            H(J, 2) = W*H(J-1, 2) / J
        END DO
        H(0, 3)   = ONE_DBL
        H(1:N, 3) = ZERO_DBL
        R1 = 0
        R2 = 1
        DO I = 2, 2 * N + 2
            JLow = 2 + FLOOR (A(I) - T + EPS)
            IF (JLow < 1) JLow = 1
            JUpp = CEILING (A(I) + T - EPS)
            IF (JUpp > N + 1) JUpp = N + 1
            KLow = 2 + FLOOR (A(I - 1) - T + EPS)
            IF (KLow < 1) KLow = 1
            KUpp0 = CEILING (A(I - 1) + T - EPS)
            ! Find to which case it corresponds
            W = (A(I) - A(I-1))/N
            S = -1
            DO J = 0, 3
                IF (ABS(W - H(1, J)) <= EPS) THEN
                    S = J
                    EXIT
                END IF
            END DO
            IF (S < 0) THEN
                CALL Handle_ErrLevel('Pomeranz', SubModName, ErrWarning, 'S < 0.')
                RETURN
            END IF

            MaxSum = -ONE_DBL
            R1 = IAND(R1 + 1, 1)    ! i - 1
            R2 = IAND(R2 + 1, 1)    ! i

            DO J = JLow, JUpp
                KUpp = KUpp0
                IF (KUpp > J) KUpp = J
                Sum = ZERO_DBL
                DO K = KUpp, KLow, -1
                    Sum = Sum + V(K, R1) * H(J - K, S)
                END DO
                V(J, R2) = Sum
                IF (Sum > MaxSum) MaxSum = Sum
            END DO

            IF (MaxSum < IRENO) THEN
                ! V is too small: renormalize to avoid underflow of prob
                DO J = JLow, JUpp
                    V(J, R2) = V(J, R2) * RENO
                END DO
                ! keep track of log of RENO
                Count = Count + 1
            END IF
        END DO

        Z = V(N+1, R2)
        CALL MemFree (A)
        CALL MemFree (H)
        CALL MemFree (V)

        W = num2_LnFactorial(ToInteger(N)) - Count*ENO*num_Ln2 + LOG(Z)
        IF (W >= ZERO_DBL) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = EXP(W)
        END IF

        RETURN

    END FUNCTION Pomeranz

    !**************************************************************************
END FUNCTION fdist_KS1

!******************************************************************************
MODULE FUNCTION fdist_KS2(N0, X, NDig) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! Similar to "fdist_KS1" but employs different algorithm.  See [1].

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N0
    tDouble,            INTENT(IN)  :: X
    tInteger, OPTIONAL, INTENT(IN)  :: NDig
    tDouble                         :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble, ALLOCATABLE    :: H(:), Q(:)
    tDouble                 :: D, HS, S, Z
    tInteger                :: K, M, MM1, I, J, G, EH, EQ, N, SigDigit
    tIndex                  :: MxM

! FLOW

    ! initialize
    N = N0
    D = X
    SigDigit = 15
    IF (PRESENT(NDig)) SigDigit = NDig

    IF (SigDigit <= 7) THEN
        ! The following statemetns are for accuracy less than
        ! or equal to 7 digits in the right tail
        S = D * D * N
        IF ((S > 7.24_kDouble).OR.((S > 3.76_kDouble).AND.(N > 99))) THEN
            ResVal = ONE_DBL - TWO_DBL * EXP (-(2.000071_kDouble + 0.331_kDouble / &
                     SQRT (REAL(N, KIND=kDouble)) + 1.409_kDouble / N) * S)
            RETURN
        END IF
    END IF

    K = ToInteger(N * D) + 1
    M = 2 * K - 1
    MM1 = M - 1
    MxM = M * M
    HS = K - N * D
    CALL MemAlloc(H, MxM, StartID=0_kIndex)
    CALL MemAlloc(Q, MxM, StartID=0_kIndex)

    DO I = 0, M-1
        DO J = 0, M-1
            IF (I - J + 1 < 0) THEN
                H(I * M + J) = ZERO_DBL
            ELSE
                H(I * M + J) = ONE_DBL
            END IF
        END DO
    END DO

    DO I = 0, M-1
        J = I * M
        G = (M - 1) * M + I
        H(J) = H(J) - (HS**(I + 1))
        H(G) = H(G) - (HS**(M - I))
    END DO

    Z = TWO_DBL * HS - ONE_DBL
    IF (Z > ZERO_DBL) THEN
        G = (M - 1) * M
        H(G) = H(G) + (Z**M)
    END IF

    DO I = 0, M-1
        DO J = 0, M-1
            IF (I - J + 1 > 0) THEN
                DO G = 1, I - J + 1
                    H(I * M + J) = H(I * M + J) / REAL(G, KIND=kDouble)
                END DO
            END IF
        END DO
    END DO

    EH = 0
    CALL mPower (H, EH, Q, EQ, M, N)
    S = Q((K - 1) * M + K - 1)
    DO I = 1, N
        S = S * I / N
        IF (S < 1.0D-140) THEN
            S = S * 1.0D+140
            EQ = EQ - 140
        END IF
    END DO
    S = S * (10.0_kDouble**EQ)
    CALL MemFree (H)
    CALL MemFree (Q)
    ResVal = S

    RETURN

    CONTAINS

    SUBROUTINE mMultiply(A, B, C, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: A(0:), B(0:)
        tDouble,  INTENT(INOUT) :: C(0:)
        tInteger, INTENT(IN)    :: M

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble     :: S
        tInteger    :: I, J, K

    ! FLOW

        DO I = 0, M-1
            DO J = 0, M-1
                S = ZERO_DBL
                DO K = 0, M-1
                    S = S + A(I*M + K)*B(K*M + J)
                END DO
                C(I*M + J) = S
            END DO
        END DO

        RETURN

    END SUBROUTINE mMultiply

    !**************************************************************************

    RECURSIVE SUBROUTINE mPower(A, EA, V, EV, M, N)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: A(0:)
        tDouble,  INTENT(INOUT) :: V(0:)
        tInteger, INTENT(IN)    :: EA, M, N
        tInteger, INTENT(INOUT) :: EV

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tDouble, ALLOCATABLE    :: B(:)
        tInteger                :: I, EB
        tIndex                  :: MM

    ! FLOW

        MM = M * M
        IF (N == 1) THEN
            V(0:MM-1) = A(0:MM-1)
            EV = EA
            RETURN
        END IF
        CALL mPower (A, EA, V, EV, M, N / 2)
        CALL MemAlloc(B, MM, StartID=0_kIndex)
        CALL mMultiply (V, V, B, M)
        EB = 2 * (EV)

        IF (MOD(N, 2) == 0) THEN
            V(0:MM-1) = B(0:MM-1)
            EV = EB
        ELSE
            CALL mMultiply (A, B, V, M)
            EV = EA + EB
        END IF

        IF (V((M / 2) * M + (M / 2)) > 1.0D+140) THEN
            V(0:MM-1) = V(0:MM-1) * 1.0D-140
            EV = EV + 140
        END IF
        CALL MemFree (B)

        RETURN

    END SUBROUTINE mPower

    !**************************************************************************

END FUNCTION fdist_KS2

!******************************************************************************
MODULE FUNCTION fdist_KSPlusJumpOne(N, A, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! Similar to "fdist_KSPlus" but for the case where the distribution function F
    ! has a jump of size a at a given point x0, is zero at the left of x0, and is
    ! continuous at the right of x0.  Restriction: 0 < a < 1.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: A, X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: NxaParam  = 6.5_kDouble   ! frontier: alternating series
    tDouble,  PARAMETER :: EpsLR = 1.0D-15
    tDouble,  PARAMETER :: Eps   = 1.0D-290

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Sum, Term, LogCom
    tDouble     :: Q, P1, Q1
    tDouble     :: JReal, NJReal
    tInteger    :: Sign
    tIndex      :: J, JMax

! FLOW

    IF (N < 1) THEN
        CALL Handle_ErrLevel('fdist_KSPlusJumpOne', SubModName, ErrWarning, 'N < 1.')
        RETURN
    ELSEIF ((A <= ZERO_DBL).AND.(A >= ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_KSPlusJumpOne', SubModName, ErrWarning, 'A Not In (0, 1).')
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X + A >= ONE_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    LogCom = LOG(REAL(N, KIND=kDouble))
    Sum = ZERO_DBL

    !--------------------------------------------------------------------!
    ! the alternating series is stable and fast for N*(x + a) very small !
    !--------------------------------------------------------------------!
    IF ((N * (X + A) < NxaParam).AND.(A + X < HALF_DBL)) THEN
        JMax = ToLong(N * (X + A))
        DO J = 1, JMax
            JReal = J
            NJReal = N - J
            Q = JReal / N - X
            IF (((Q < ZERO_DBL).AND.(IAND(J,         1) /= 0)).OR. &
                ((Q > ONE_DBL) .AND.(IAND(N - J - 1, 1) /= 0))) THEN
                Sign = -1
            ELSE
                Sign = 1
            END IF
            ! we must avoid log(0.0)
            Q1 = ABS (Q)
            P1 = ABS (ONE_DBL - Q)
            IF ((Q1 > Eps).AND.(P1 > Eps)) THEN
                Term = LogCom + JReal * LOG (Q1) + (NJReal - ONE_DBL) * LOG (P1)
                Sum  = Sum + Sign * EXP (Term)
            END IF
            LogCom = LogCom + LOG (NJReal / (JReal + ONE_DBL))
        END DO
        ! add the term j = 0
        Sum = Sum + EXP ((N - 1) * num2_LOG1P(X))
        ResVal = Sum * X
        RETURN
    END IF

    !--------------------------------------------!
    ! For N(x + a) >= NxaParam or (a + x) > 0.5, !
    ! use the non-alternating series.            !
    !*-------------------------------------------!

    ! EpsLR because the distribution has a jump
    JMax = ToLong(N * (ONE_DBL - A - X - EpsLR))
    DO J = 1, JMax
        JReal = J
        NJReal = N - JReal
        Q = JReal / N + X
        IF (ONE_DBL - Q > Eps) THEN
            Term = LogCom + (JReal - ONE_DBL) * LOG (Q) + NJReal * num2_LOG1P (-Q)
            Sum = Sum + EXP (Term)
        END IF
        LogCom = LogCom + LOG (NJReal / (JReal + ONE_DBL))
    END DO
    Sum = Sum * X

    ! add the term j = 0
    IF (ONE_DBL - X > Eps) Sum = Sum + EXP (N * num2_LOG1P (-X))
    ResVal = ONE_DBL - Sum

    RETURN

END FUNCTION fdist_KSPlusJumpOne

!******************************************************************************
MODULE FUNCTION fdist_CramerMises(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of P(Wn2 <= x) where Wn2 is the Cram'er von Mises
    ! statistic, defined by Eq (46) in [1], for a sample of independent uniforms
    ! over (0, 1).  See [1] for more details.

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: Eps  = DBL_EPSILON
    tInteger, PARAMETER :: JMax = 10
    tDouble,  PARAMETER :: A(0:9) = [                   &
        1.0_kDouble,           1.11803398875_kDouble,   &
        1.125_kDouble,         1.12673477358_kDouble,   &
        1.1274116945_kDouble,  1.12774323743_kDouble,   &
        1.1279296875_kDouble,  1.12804477649_kDouble,   &
        1.12812074678_kDouble, 1.12817350091_kDouble]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Cor, Arg
    tDouble     :: TermX, TermS, TermJ
    tInteger    :: J

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_CramerMises', SubModName, ErrWarning, 'N <= 0.')
        RETURN
    END IF

    IF (N == 1) THEN
        IF (X <= ONE_DBL / 12.0_kDouble) THEN
            ResVal = ZERO_DBL
        ELSEIF (X >= ONE_DBL / 3.0_kDouble) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = TWO_DBL * SQRT (X - ONE_DBL / 12.0_kDouble)
        END IF
        RETURN
    END IF

    IF ((X <= 0.002_kDouble).OR.(X < ONE_DBL / (12.0_kDouble*N))) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF ((X > 3.95_kDouble).OR.(X >= N/3.0_kDouble)) THEN
        ResVal = ONE_DBL
        RETURN
    END IF

    TermX = 0.0625_kDouble / X          ! 1 / (16x)
    ResVal = ZERO_DBL
    J = 0
    DO
        TermJ = 4 * J + 1
        Arg = TermJ * TermJ * TermX
        TermS = A(J) * EXP (-Arg) * num2_BesselK025 (Arg)
        ResVal = ResVal + TermS
        J = J + 1
        IF ((TermS < Eps).OR.(J > JMax)) EXIT
    END DO

    IF (J > JMax) THEN
        CALL Handle_ErrLevel('fdist_CramerMises', SubModName, ErrWarning, 'Iterations have not converged.')
    END IF

    ResVal = ResVal / (num_Pi * SQRT (X))

    ! Empirical correction in 1/N
    IF (X < 0.0092_kDouble) THEN
        Cor = ZERO_DBL
    ELSEIF (X < 0.03_kDouble) THEN
        Cor = -0.0121763_kDouble + X * (2.56672_kDouble - 132.571_kDouble * X)
    ELSEIF (X < 0.06_kDouble) THEN
        Cor = 0.108688_kDouble + X * (-7.14677_kDouble + 58.0662_kDouble * X)
    ELSEIF (X < 0.19_kDouble) THEN
        Cor = -0.0539444_kDouble + X * (-2.22024_kDouble +      &
                X * (25.0407_kDouble - 64.9233_kDouble * X))
    ELSEIF (X < HALF_DBL) THEN
        Cor = -0.251455_kDouble + X * (2.46087_kDouble +        &
                X * (-8.92836_kDouble + X * (14.0988_kDouble -  &
                X * (5.5204_kDouble + 4.61784_kDouble * X))))
    ELSEIF (X <= 1.1_kDouble) THEN
        Cor = 0.0782122_kDouble + X * (-0.519924_kDouble + X * (1.75148_kDouble +   &
                X * (-2.72035_kDouble + X * (1.94487_kDouble - 0.524911_kDouble * X))))
    ELSE
        Cor = EXP (-0.244889_kDouble - 4.26506_kDouble * X)
    END IF

    ResVal = ResVal + (Cor / N)
    ! This empirical correction is not very precise, so ...
    IF (ResVal > ONE_DBL) ResVal = ONE_DBL

    RETURN

END FUNCTION fdist_CramerMises

!******************************************************************************
MODULE FUNCTION fdist_WatsonG(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of P(Gn <= x) where Gn is the Watson statistic,
    ! defined by Eq (44) in [1], for a sample of independent uniforms over (0, 1).

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: MinArg = 0.15_kDouble
    tDouble,  PARAMETER :: MaxArg = 1.5_kDouble
    tDouble,  PARAMETER :: MinTab = 0.1_kDouble
    tDouble,  PARAMETER :: Step   = 0.01_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Tables for a spline approximation of the WatsonG distribution
    tDouble     :: YWA(0:142)
    tDouble     :: MWA(0:142)
    tDouble     :: CoWA(0:142)      ! Empirical correction in 1/sqrt(n)
    tDouble     :: TJ, TI, R, P, H
    tInteger    :: I, J

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_WatsonG', SubModName, ErrWarning, 'N <= 0.')
        RETURN
    ELSEIF (N == 1) THEN
        ! n = 1, degenerate case
        ResVal = HALF_DBL
        RETURN
    END IF

    ! Initialization of the interpolation table
    CALL WatsonGInit()

    IF (X <= MinArg) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF
    IF (X >= 10.0_kDouble) THEN
        ResVal = ONE_DBL
        RETURN
    END IF
    IF (X > MaxArg) THEN
        R = EXP (19.0_kDouble - 20.0_kDouble * X)
        ResVal = ONE_DBL - R
        ! Empirical Correction in 1/sqrt(n)
        R = EXP (13.34_kDouble - 15.26_kDouble * X) / SQRT (REAL(N, KIND=kDouble))
        ResVal = ResVal + R
        ! The correction in 1/sqrt(n) is not always precise
        IF (ResVal >= ONE_DBL) THEN
            ResVal = ONE_DBL
        ELSE
            ResVal = ResVal
        END IF
        RETURN
    END IF

    ! Search of the correct slot in the interpolation table
    I = ToInteger((X - MinTab) / Step) + 1
    TI = MinTab + I * Step
    TJ = TI - Step
    ! Approximation within the slot
    J = I - 1
    H = X - TJ
    R = TI - X
    P = Step * Step / 6.0_kDouble
    ResVal = ((MWA(J) * R * R * R + MWA(I) * H * H * H) / 6.0_kDouble) / Step
    ResVal = ResVal + (((YWA(J) - MWA(J) * P) * R + (YWA(I) - MWA(I) * P) * H) / Step)

    ! Empirical correction in 1/sqrt(n)
    ResVal = ResVal + ((CoWA(I) * H + CoWA(J) * R) / (Step * SQRT (REAL(N, KIND=kDouble))))

    IF (ResVal >= ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = ResVal
    END IF

    RETURN

    CONTAINS

    SUBROUTINE WatsonGInit()

    !** PURPOSE OF THIS SUBROUTINE:
        ! Initialization procedure for fdist_WatsonG.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        YWA(0) = 1.8121832847D-39;      YWA(1) = 2.0503176304D-32;
        YWA(2) = 4.6139577764D-27;      YWA(3) = 6.5869745929D-23;
        YWA(4) = 1.2765816107D-19;      YWA(5) = 5.6251923105D-17;
        YWA(6) = 8.0747150511D-15;      YWA(7) = 4.8819994144D-13;
        YWA(8) = 1.4996052497D-11;      YWA(9) = 2.6903519441D-10;
        YWA(10) = 3.1322929018D-9;      YWA(11) = 2.5659643046D-8;
        YWA(12) = 1.5749759318D-7;      YWA(13) = 7.6105096466D-7;
        YWA(14) = 3.0113293541D-6;      YWA(15) = 1.0070166837D-5;
        YWA(16) = 2.9199826692D-5;      YWA(17) = 7.4970409372D-5;
        YWA(18) = 1.7340586581D-4;      YWA(19) = 3.6654236297D-4;
        YWA(20) = 7.165864865D-4;       YWA(21) = 1.3087767385D-3;
        YWA(22) = 2.2522044209D-3;      YWA(23) = 3.6781862572D-3;
        YWA(24) = 5.7361958631D-3;      YWA(25) = 8.5877444706D-3;
        YWA(26) = 1.23988738D-2;        YWA(27) = 1.73320516D-2;
        YWA(28) = 2.35382479D-2;        YWA(29) = 3.11498548D-2;
        YWA(30) = 4.02749297D-2;        YWA(31) = 5.09930445D-2;
        YWA(32) = 6.33528333D-2;        YWA(33) = 7.73711747D-2;
        YWA(34) = 9.30338324D-2;        YWA(35) = 1.10297306D-1;
        YWA(36) = 1.290916098D-1;       YWA(37) = 1.493236984D-1;
        YWA(38) = 1.708812741D-1;       YWA(39) = 1.936367476D-1;
        YWA(40) = 2.174511609D-1;       YWA(41) = 2.42177928D-1;
        YWA(42) = 2.676662852D-1;       YWA(43) = 2.937643828D-1;
        YWA(44) = 3.203219784D-1;       YWA(45) = 3.471927188D-1;
        YWA(46) = 3.742360163D-1;       YWA(47) = 4.013185392D-1;
        YWA(48) = 4.283153467D-1;       YWA(49) = 4.551107027D-1;
        YWA(50) = 4.815986082D-1;       YWA(51) = 5.076830902D-1;
        YWA(52) = 5.332782852D-1;       YWA(53) = 5.583083531D-1;
        YWA(54) = 5.827072528D-1;       YWA(55) = 6.064184099D-1;
        YWA(56) = 6.293943006D-1;       YWA(57) = 6.515959739D-1;
        YWA(58) = 6.729925313D-1;       YWA(59) = 6.935605784D-1;
        YWA(60) = 7.132836621D-1;       YWA(61) = 7.321517033D-1;
        YWA(62) = 7.501604333D-1;       YWA(63) = 7.673108406D-1;
        YWA(64) = 7.836086337D-1;       YWA(65) = 7.99063723D-1;
        YWA(66) = 8.136897251D-1;       YWA(67) = 8.275034914D-1;
        YWA(68) = 8.405246632D-1;       YWA(69) = 8.527752531D-1;
        YWA(70) = 8.642792535D-1;       YWA(71) = 8.750622738D-1;
        YWA(72) = 8.851512032D-1;       YWA(73) = 8.945739017D-1;
        YWA(74) = 9.033589176D-1;       YWA(75) = 9.115352296D-1;
        YWA(76) = 9.19132015D-1;        YWA(77) = 9.261784413D-1;
        YWA(78) = 9.327034806D-1;       YWA(79) = 9.387357465D-1;
        YWA(80) = 9.44303351D-1;        YWA(81) = 9.494337813D-1;
        YWA(82) = 9.541537951D-1;       YWA(83) = 9.584893325D-1;
        YWA(84) = 9.624654445D-1;       YWA(85) = 9.661062352D-1;
        YWA(86) = 9.694348183D-1;       YWA(87) = 9.724732859D-1;
        YWA(88) = 9.752426872D-1;       YWA(89) = 9.777630186D-1;
        YWA(90) = 9.800532221D-1;       YWA(91) = 9.821311912D-1;
        YWA(92) = 9.840137844D-1;       YWA(93) = 9.85716844D-1;
        YWA(94) = 9.872552203D-1;       YWA(95) = 9.886428002D-1;
        YWA(96) = 9.898925389D-1;       YWA(97) = 9.910164946D-1;
        YWA(98) = 9.920258656D-1;       YWA(99) = 9.929310287D-1;
        YWA(100) = 9.937415788D-1;      YWA(101) = 9.944663692D-1;
        YWA(102) = 9.95113552D-1;       YWA(103) = 9.956906185D-1;
        YWA(104) = 9.962044387D-1;      YWA(105) = 9.966613009D-1;
        YWA(106) = 9.970669496D-1;      YWA(107) = 9.974266225D-1;
        YWA(108) = 9.977450862D-1;      YWA(109) = 9.980266707D-1;
        YWA(110) = 9.982753021D-1;      YWA(111) = 9.984945338D-1;
        YWA(112) = 9.98687576D-1;       YWA(113) = 9.98857324D-1;
        YWA(114) = 9.990063842D-1;      YWA(115) = 9.991370993D-1;
        YWA(116) = 9.992515708D-1;      YWA(117) = 9.99351681D-1;
        YWA(118) = 9.994391129D-1;      YWA(119) = 9.995153688D-1;
        YWA(120) = 9.995817875D-1;      YWA(121) = 9.996395602D-1;
        YWA(122) = 9.996897446D-1;      YWA(123) = 9.997332791D-1;
        YWA(124) = 9.997709943D-1;      YWA(125) = 9.998036243D-1;
        YWA(126) = 9.998318172D-1;      YWA(127) = 9.998561438D-1;
        YWA(128) = 9.998771066D-1;      YWA(129) = 9.998951466D-1;
        YWA(130) = 9.999106508D-1;      YWA(131) = 9.99923958D-1;
        YWA(132) = 9.999353645D-1;      YWA(133) = 9.999451288D-1;
        YWA(134) = 9.999534765D-1;      YWA(135) = 9.999606035D-1;
        YWA(136) = 9.999666805D-1;      YWA(137) = 9.999718553D-1;
        YWA(138) = 9.999762562D-1;      YWA(139) = 9.999799939D-1;
        YWA(140) = 9.999831643D-1;      YWA(141) = 9.999858D-1;
        YWA(142) = 9.999883D-1;

        MWA(0) = ZERO_DBL;           MWA(1) = 6.909D-15;
        MWA(2) = 2.763D-14;          MWA(3) = 1.036D-13;
        MWA(4) = 3.792D-13;          MWA(5) = 4.773D-12;
        MWA(6) = 4.59D-10;           MWA(7) = 2.649D-8;
        MWA(8) = 7.353D-7;           MWA(9) = 1.14D-5;
        MWA(10) = 1.102D-4;          MWA(11) = 7.276D-4;
        MWA(12) = 3.538D-3;          MWA(13) = 0.01342_kDouble;
        MWA(14) = 0.04157_kDouble;   MWA(15) = 0.1088_kDouble;
        MWA(16) = 0.2474_kDouble;    MWA(17) = 0.4999_kDouble;
        MWA(18) = 0.913_kDouble;     MWA(19) = 1.53_kDouble;
        MWA(20) = 2.381_kDouble;     MWA(21) = 3.475_kDouble;
        MWA(22) = 4.795_kDouble;     MWA(23) = 6.3_kDouble;
        MWA(24) = 7.928_kDouble;     MWA(25) = 9.602_kDouble;
        MWA(26) = 11.24_kDouble;     MWA(27) = 12.76_kDouble;
        MWA(28) = 14.1_kDouble;      MWA(29) = 15.18_kDouble;
        MWA(30) = 15.98_kDouble;     MWA(31) = 16.47_kDouble;
        MWA(32) = 16.64_kDouble;     MWA(33) = 16.49_kDouble;
        MWA(34) = 16.05_kDouble;     MWA(35) = 15.35_kDouble;
        MWA(36) = 14.41_kDouble;     MWA(37) = 13.28_kDouble;
        MWA(38) = 12.0_kDouble;      MWA(39) = 10.6_kDouble;
        MWA(40) = 9.13_kDouble;      MWA(41) = 7.618_kDouble;
        MWA(42) = 6.095_kDouble;     MWA(43) = 4.588_kDouble;
        MWA(44) = 3.122_kDouble;     MWA(45) = 1.713_kDouble;
        MWA(46) = 0.3782_kDouble;    MWA(47) = -0.8726_kDouble;
        MWA(48) = -2.031_kDouble;    MWA(49) = -3.091_kDouble;
        MWA(50) = -4.051_kDouble;    MWA(51) = -4.91_kDouble;
        MWA(52) = -5.668_kDouble;    MWA(53) = -6.327_kDouble;
        MWA(54) = -6.893_kDouble;    MWA(55) = -7.367_kDouble;
        MWA(56) = -7.756_kDouble;    MWA(57) = -8.064_kDouble;
        MWA(58) = -8.297_kDouble;    MWA(59) = -8.46_kDouble;
        MWA(60) = -8.56_kDouble;     MWA(61) = -8.602_kDouble;
        MWA(62) = -8.591_kDouble;    MWA(63) = -8.533_kDouble;
        MWA(64) = -8.433_kDouble;    MWA(65) = -8.296_kDouble;
        MWA(66) = -8.127_kDouble;    MWA(67) = -7.93_kDouble;
        MWA(68) = -7.709_kDouble;    MWA(69) = -7.469_kDouble;
        MWA(70) = -7.212_kDouble;    MWA(71) = -6.943_kDouble;
        MWA(72) = -6.663_kDouble;    MWA(73) = -6.378_kDouble;
        MWA(74) = -6.087_kDouble;    MWA(75) = -5.795_kDouble;
        MWA(76) = -5.503_kDouble;    MWA(77) = -5.213_kDouble;
        MWA(78) = -4.927_kDouble;    MWA(79) = -4.646_kDouble;
        MWA(80) = -4.371_kDouble;    MWA(81) = -4.103_kDouble;
        MWA(82) = -3.843_kDouble;    MWA(83) = -3.593_kDouble;
        MWA(84) = -3.352_kDouble;    MWA(85) = -3.12_kDouble;
        MWA(86) = -2.899_kDouble;    MWA(87) = -2.689_kDouble;
        MWA(88) = -2.489_kDouble;    MWA(89) = -2.3_kDouble;
        MWA(90) = -2.121_kDouble;    MWA(91) = -1.952_kDouble;
        MWA(92) = -1.794_kDouble;    MWA(93) = -1.645_kDouble;
        MWA(94) = -1.506_kDouble;    MWA(95) = -1.377_kDouble;
        MWA(96) = -1.256_kDouble;    MWA(97) = -1.144_kDouble;
        MWA(98) = -1.041_kDouble;    MWA(99) = -0.9449_kDouble;
        MWA(100) = -0.8564_kDouble;  MWA(101) = -0.775_kDouble;
        MWA(102) = -0.7001_kDouble;  MWA(103) = -0.6315_kDouble;
        MWA(104) = -0.5687_kDouble;  MWA(105) = -0.5113_kDouble;
        MWA(106) = -0.459_kDouble;   MWA(107) = -0.4114_kDouble;
        MWA(108) = -0.3681_kDouble;  MWA(109) = -0.3289_kDouble;
        MWA(110) = -0.2934_kDouble;  MWA(111) = -0.2614_kDouble;
        MWA(112) = -0.2325_kDouble;  MWA(113) = -0.2064_kDouble;
        MWA(114) = -0.183_kDouble;   MWA(115) = -0.1621_kDouble;
        MWA(116) = -0.1433_kDouble;  MWA(117) = -0.1265_kDouble;
        MWA(118) = -0.1115_kDouble;  MWA(119) = -9.813D-2;
        MWA(120) = -8.624D-2;        MWA(121) = -7.569D-2;
        MWA(122) = -6.632D-2;        MWA(123) = -5.803D-2;
        MWA(124) = -5.071D-2;        MWA(125) = -4.424D-2;
        MWA(126) = -3.855D-2;        MWA(127) = -3.353D-2;
        MWA(128) = -2.914D-2;        MWA(129) = -2.528D-2;
        MWA(130) = -0.0219_kDouble;  MWA(131) = -1.894D-2;
        MWA(132) = -1.637D-2;        MWA(133) = -1.412D-2;
        MWA(134) = -1.217D-2;        MWA(135) = -1.046D-2;
        MWA(136) = -8.988D-3;        MWA(137) = -7.72D-3;
        MWA(138) = -6.567D-3;        MWA(139) = -5.802D-3;
        MWA(140) = -0.0053_kDouble;  MWA(141) = -4.7D-4;
        MWA(142) = -4.3D-4;

        CoWA(0:11) = ZERO_DBL;
        CoWA(12) = 1.25D-5;            CoWA(13) = 3.87D-5;
        CoWA(14) = 1.004D-4;           CoWA(15) = 2.703D-4;
        CoWA(16) = 6.507D-4;           CoWA(17) = 1.3985D-3;
        CoWA(18) = 2.8353D-3;          CoWA(19) = 5.1911D-3;
        CoWA(20) = 8.9486D-3;          CoWA(21) = 1.41773D-2;
        CoWA(22) = 2.16551D-2;         CoWA(23) = 3.1489D-2;
        CoWA(24) = 4.34123D-2;         CoWA(25) = 5.78719D-2;
        CoWA(26) = 7.46921D-2;         CoWA(27) = 9.45265D-2;
        CoWA(28) = 1.165183D-1;        CoWA(29) = 1.406353D-1;
        CoWA(30) = 1.662849D-1;        CoWA(31) = 1.929895D-1;
        CoWA(32) = 2.189347D-1;        CoWA(33) = 2.457772D-1;
        CoWA(34) = 2.704794D-1;        CoWA(35) = 2.947906D-1;
        CoWA(36) = 3.169854D-1;        CoWA(37) = 3.377435D-1;
        CoWA(38) = 3.573555D-1;        CoWA(39) = 3.751205D-1;
        CoWA(40) = 3.906829D-1;        CoWA(41) = 4.039806D-1;
        CoWA(42) = 4.142483D-1;        CoWA(43) = 4.22779D-1;
        CoWA(44) = 4.288013D-1;        CoWA(45) = 4.330353D-1;
        CoWA(46) = 4.34452D-1;         CoWA(47) = 4.338138D-1;
        CoWA(48) = 4.31504D-1;         CoWA(49) = 4.272541D-1;
        CoWA(50) = 4.220568D-1;        CoWA(51) = 4.158229D-1;
        CoWA(52) = 4.083281D-1;        CoWA(53) = 3.981182D-1;
        CoWA(54) = 3.871678D-1;        CoWA(55) = 3.755527D-1;
        CoWA(56) = 3.628823D-1;        CoWA(57) = 3.520135D-1;
        CoWA(58) = 3.400924D-1;        CoWA(59) = 3.280532D-1;
        CoWA(60) = 3.139477D-1;        CoWA(61) = 2.997087D-1;
        CoWA(62) = 2.849179D-1;        CoWA(63) = 2.710475D-1;
        CoWA(64) = 2.576478D-1;        CoWA(65) = 2.449155D-1;
        CoWA(66) = 2.317447D-1;        CoWA(67) = 2.193161D-1;
        CoWA(68) = 2.072622D-1;        CoWA(69) = 1.956955D-1;
        CoWA(70) = 1.846514D-1;        CoWA(71) = 1.734096D-1;
        CoWA(72) = 1.622678D-1;        CoWA(73) = 1.520447D-1;
        CoWA(74) = 1.416351D-1;        CoWA(75) = 1.32136D-1;
        CoWA(76) = 1.231861D-1;        CoWA(77) = 1.150411D-1;
        CoWA(78) = 1.071536D-1;        CoWA(79) = 9.9465D-2;
        CoWA(80) = 9.22347D-2;         CoWA(81) = 8.54394D-2;
        CoWA(82) = 7.87697D-2;         CoWA(83) = 7.23848D-2;
        CoWA(84) = 6.6587D-2;          CoWA(85) = 6.15849D-2;
        CoWA(86) = 5.6573D-2;          CoWA(87) = 5.17893D-2;
        CoWA(88) = 4.70011D-2;         CoWA(89) = 4.2886D-2;
        CoWA(90) = 3.91224D-2;         CoWA(91) = 3.53163D-2;
        CoWA(92) = 3.20884D-2;         CoWA(93) = 2.92264D-2;
        CoWA(94) = 2.66058D-2;         CoWA(95) = 2.37352D-2;
        CoWA(96) = 2.14669D-2;         CoWA(97) = 1.94848D-2;
        CoWA(98) = 1.75591D-2;         CoWA(99) = 1.58232D-2;
        CoWA(100) = 1.40302D-2;        CoWA(101) = 1.24349D-2;
        CoWA(102) = 1.11856D-2;        CoWA(103) = 9.9765D-3;
        CoWA(104) = 8.9492D-3;         CoWA(105) = 8.0063D-3;
        CoWA(106) = 7.1509D-3;         CoWA(107) = 6.3196D-3;
        CoWA(108) = 5.6856D-3;         CoWA(109) = 5.0686D-3;
        CoWA(110) = 4.5085D-3;         CoWA(111) = 3.9895D-3;
        CoWA(112) = 3.4804D-3;         CoWA(113) = 3.0447D-3;
        CoWA(114) = 2.7012D-3;         CoWA(115) = 2.2984D-3;
        CoWA(116) = 2.0283D-3;         CoWA(117) = 1.7399D-3;
        CoWA(118) = 1.5032D-3;         CoWA(119) = 1.3267D-3;
        CoWA(120) = 1.1531D-3;         CoWA(121) = 9.92D-4;
        CoWA(122) = 9.211D-4;          CoWA(123) = 8.296D-4;
        CoWA(124) = 6.991D-4;          CoWA(125) = 5.84D-4;
        CoWA(126) = 5.12D-4;           CoWA(127) = 4.314D-4;
        CoWA(128) = 3.593D-4;          CoWA(129) = 3.014D-4;
        CoWA(130) = 2.401D-4;          CoWA(131) = 2.004D-4;
        CoWA(132) = 1.614D-4;          CoWA(133) = 1.257D-4;
        CoWA(134) = 1.112D-4;          CoWA(135) = 9.22D-5;
        CoWA(136) = 8.77D-5;           CoWA(137) = 6.22D-5;
        CoWA(138) = 4.93D-5;           CoWA(139) = 3.92D-5;
        CoWA(140) = 3.15D-5;           CoWA(141) = 1.03D-5;
        CoWA(142) = 9.6D-6;

        RETURN

    END SUBROUTINE WatsonGInit

    !**************************************************************************

END FUNCTION fdist_WatsonG

!******************************************************************************
MODULE FUNCTION fdist_WatsonU(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return an approximation of P(U2 <= x) where U2 is the Watson statistic,
    ! defined by Eq (46) in [1], for a sample of independent uniforms over (0, 1).

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: JMAX = 10
    tDouble,  PARAMETER :: xSepare = 0.15

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: V, Term, Sum
    tInteger    :: J, IPow

! FLOW

    ! Only the asymptotic form has been implemented. In the trivial case
    ! N = 1, we simply return 0.5

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (N == 1) THEN
        ! n = 1, degenerate case
        ResVal = HALF_DBL
        RETURN
    ELSEIF (X > xSepare) THEN
        ResVal = ONE_DBL - fbar_WatsonU (N, X)
        RETURN
    END IF

    ! this series converges rapidly for x <= 0.15
    V = EXP (-(0.125_kDouble / X))
    Sum = V
    J = 2
    DO
        IPow = 2 * J - 1
        Term = V**(IPow * IPow)
        Sum = Sum + Term
        J = J + 1
        IF ((Term < V * DBL_EPSILON).OR.(J > JMAX)) EXIT
    END DO
    IF (J > JMAX) THEN
        CALL Handle_ErrLevel('fdist_WatsonU', SubModName, ErrWarning, 'Sum has not converged.')
    END IF

    V = TWO_DBL * Sum / SQRT (TWO_DBL * num_Pi * X)
    IF (V >= ONE_DBL) THEN
        ResVal = ONE_DBL
    ELSE
        ResVal = V
    END IF

    RETURN

END FUNCTION fdist_WatsonU

!******************************************************************************
FUNCTION AD_N_1(X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! The Anderson-Darling distribution for N = 1.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: AD_X0 = 0.38629436111989062_kDouble
    tDouble,  PARAMETER :: AD_X1 = 37.816242111357_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Term, Q

! FLOW

    IF (X <= AD_X0) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= AD_X1) THEN
        ResVal = ONE_DBL
    ELSE
        IF (X - AD_X0 >= 1.0D-3) THEN
            Term = ONE_DBL - 4.0_kDouble * EXP (-X - ONE_DBL)
        ELSE
            Q = X - AD_X0
            Term = Q*(ONE_DBL - Q*(HALF_DBL - Q / 6.0_kDouble))
        END IF
        ResVal = SQRT (Term)
    END IF

    RETURN

END FUNCTION AD_N_1

!******************************************************************************

MODULE FUNCTION fdist_AndersonDarling(N, X) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return Fn(x) = P[A2n <= x], where A2n is the Anderson-Darling statistic,
    ! defined by Eq (47) in [1], for a sample of independent uniforms over (0, 1).

!** REFERENCE:
    ! [1] P. L’Ecuyer and R. Simard.  ProbDist: A Software Library of Probability
    !     Distributions and Goodness-of-Fit Statistics in ANSI C.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN) :: N
    tDouble, INTENT(IN) :: X
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Q

! FLOW

    IF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_AndersonDarling', SubModName, ErrWarning, 'N <= 0.')
        RETURN
    ELSEIF (N == 1) THEN
        ! The Anderson-Darling distribution for N = 1
        ResVal = AD_N_1(X)
        RETURN
    END IF

    IF (X <= ZERO_DBL) THEN
        ResVal = ZERO_DBL
    ELSEIF (X >= fdist_XBIG) THEN
        ResVal = ONE_DBL
    ELSEIF (X <= 0.2_kDouble) THEN
        ! Sinclair and Spurr lower tail approximation (3.6)
        Q = 1.784_kDouble + 0.9936_kDouble*X + 0.03287_kDouble/X &
          - (2.018_kDouble + 0.2029_kDouble/X) / SQRT (X)
        IF (Q < -18.0_kDouble) THEN
            ResVal = EXP(Q)
        ELSE
            Q = ONE_DBL + EXP(Q)
            ResVal = ONE_DBL - ONE_DBL / Q
        END IF
    ELSE
        ResVal = ONE_DBL - fbar_AndersonDarling (N, X)
    END IF

    RETURN

END FUNCTION fdist_AndersonDarling

!******************************************************************************
MODULE FUNCTION fdist_AndersonDarling2(N, X, IsExact) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the value of the Anderson-Darling distribution at x for a sample
    ! of n independent uniforms over (0, 1) using Marsaglia’s algorithm [1].

!** REFERENCE:
    ! [1] G. Marsaglia and J. Marsaglia. Evaluating the Anderson-Darling
    !     distribution. Journal of Statistical Software, 9(2):1–5, 2004.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: N
    tDouble,            INTENT(IN)  :: X
    tLogical, OPTIONAL, INTENT(IN)  :: IsExact
    tDouble                         :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Z, C, V
    tLogical    :: IsFastADInf

! FLOW

    IF (N == 1) THEN
        ! The Anderson-Darling distribution for N = 1
        ResVal = AD_N_1(X)
        RETURN
    END IF

    ! check whether to use exact or approximate limiting distribution
    IsFastADInf = TrueVal
    IF (PRESENT(IsExact)) IsFastADInf = .NOT.IsExact

    IF (IsFastADInf) THEN
        Z = ADFInf_Approx(X)
    ELSE
        Z = ADFInf_Exact(X)
    END IF

    ! now Z = adinf(X). Next, get V = errfix(N, Z) and return Z + V;
    IF (Z > 0.8_kDouble) THEN
        V = (-130.2137_kDouble + (745.2337_kDouble - (1705.091_kDouble - (1950.646_kDouble &
             - (1116.360_kDouble - 255.7844_kDouble * Z) * Z) * Z) * Z) * Z) / N
        ResVal = Z + V
    ELSE
        C = 0.01265_kDouble + 0.1757_kDouble / N
        IF (Z < C) THEN
            V = Z / C
            V = SQRT (V) * (ONE_DBL - V) * (49 * V - 102)
            ResVal = Z + V * (0.0037_kDouble / (N * N) + 0.00078_kDouble / N &
                       + 0.00006_kDouble) / N
        ELSE
            V = (Z - C) / (0.8_kDouble - C)
            V = -0.00022633_kDouble + (6.54034_kDouble - (14.6538_kDouble - (14.458_kDouble - &
                (8.259_kDouble - 1.91864_kDouble * V) * V) * V) * V) * V
            ResVal = Z + V * (0.04213_kDouble + 0.01365_kDouble / N) / N
        END IF
    END IF

    RETURN

    CONTAINS

    FUNCTION ADF_Exact(Z, J) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To evaluate the limiting distribution of the Anderson-Darling statistic

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Z
        tInteger, INTENT(IN)    :: J
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: I
        tDouble     :: T, F, FNew, A, B, C, R

    ! FLOW

        T = (4 * J + 1) * (4 * J + 1) * 1.23370055013617_kDouble / Z
        IF (T > 150.0_kDouble) THEN
            ResVal = ZERO_DBL
            RETURN
        END IF
        A = 2.22144146907918_kDouble * EXP (-T) / SQRT (T)
        ! initialization requires cPhi
        ! if you have erfc(), replace 2*cPhi(SQRT(2*t)) with erfc(SQRT(t))
        B = 3.93740248643060_kDouble * TWO_DBL * fbar_Normal2 (SQRT (TWO_DBL * T))
        ! B = 3.93740248643060_kDouble * ERFC (SQRT (T))

        R = Z * 0.125_kDouble
        F = A + B * R
        DO I = 1, 199
            C = ((I - HALF_DBL - T) * B + T * A) / I
            A = B
            B = C
            R = R * Z / (8 * I + 8)
            IF ((ABS (R) < 1.0D-40).OR.(ABS (C) < 1.0D-40)) THEN
                ResVal = F
                RETURN
            END IF
            FNew = F + C * R
            IF (F == FNew) THEN
                ResVal = F
                RETURN
            END IF
            F = FNew
        END DO

        ResVal = F

        RETURN

    END FUNCTION ADF_Exact

    !**************************************************************************

    FUNCTION ADFInf_Exact(Z) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To evaluate the limiting distribution of the Anderson-Darling statistic

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Z
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: AD, ADNew, R

    ! FLOW

        IF (Z < 0.01_kDouble) THEN
            ! avoids exponent limits; ADinf(.01)=.528e-52
            ResVal = ZERO_DBL
            RETURN
        END IF
        R = ONE_DBL / Z
        AD = R * ADF_Exact (Z, 0)
        DO J = 1, 99
            R = R * ((HALF_DBL - J) / J)
            ADNew = AD + (4 * J + 1) * R * ADF_Exact (Z, J)
            IF (AD == ADNew) THEN
                ResVal = AD
                RETURN
            END IF
            AD = ADNew
        END DO

        ResVal = AD

        RETURN

    END FUNCTION ADFInf_Exact

    !**************************************************************************

    FUNCTION ADFInf_Approx(Z) RESULT(ResVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! Short, practical version of full ADFInf(z), z > 0.

        IMPLICIT NONE

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble,  INTENT(IN)    :: Z
        tDouble                 :: ResVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: J
        tDouble     :: AD, ADNew, R

    ! FLOW

        IF (Z < TWO_DBL) THEN
            ! max |error| < 0.000002 for Z < 2, (p = 0.90816...)
            ResVal = EXP (-1.2337141_kDouble / Z) / SQRT (Z) * (2.00012_kDouble     &
                   + (0.247105_kDouble - (0.0649821_kDouble - (0.0347962_kDouble    &
                   - (0.011672_kDouble - 0.00168691_kDouble * Z) * Z) * Z) * Z) * Z)
        ELSE
            ! max |error| < 0.0000008 for 4 < Z < infinity
            ResVal = EXP (-EXP (1.0776_kDouble - (2.30695_kDouble - (0.43424_kDouble &
                   - (0.082433_kDouble - (0.008056_kDouble - 0.0003146_kDouble * Z)  &
                   * Z) * Z) * Z) * Z))
        END IF

        RETURN

    END FUNCTION ADFInf_Approx

    !**************************************************************************

END FUNCTION fdist_AndersonDarling2

!******************************************************************************
MODULE FUNCTION fdist_Geometric(P, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the distribution function of a geometric random variable with
    ! parameter p, evaluated at s.  Restriction: 0 <= p <= 1.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: P
    tIndex,  INTENT(IN) :: S
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_Geometric', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    END IF

    IF (S < 0) THEN
        ResVal = ZERO_DBL
    ELSEIF (P >= ONE_DBL) THEN
        ! In fact, P == 1
        ResVal = ONE_DBL
    ELSEIF (P <= ZERO_DBL) THEN
        ! In fact, P == 0
        ResVal = ZERO_DBL
    ELSE
        ResVal = ONE_DBL - (ONE_DBL - P)**(S + 1)
    END IF

    RETURN

END FUNCTION fdist_Geometric

!******************************************************************************
MODULE FUNCTION fdist_Poisson1(Lambda, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the Poisson distribution function with parameter Lambda,
    ! evaluated at s.  In the cases where the Poisson distribution must be
    ! computed more than once with the same Lambda, it is more efficient to use
    ! "fdist_Poisson2" instead of "fdist_Poisson1".  Restriction: Lambda > 0.

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
        CALL Handle_ErrLevel('fdist_Poisson1', SubModName, ErrWarning, 'Lambda < 0.')
        RETURN
    END IF

    IF (Lambda == ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (S < 0) THEN
        ResVal = ZERO_DBL
    ELSEIF (Lambda > Lambda_Limit) THEN
        ! If Lambda > Lambda_Limit, we use the Chi2 distribution according to the exact
        !  relation, with 2S + 2 degrees of freedom fdist_Poisson (Lambda, S) = 1 -
        !  fdist_ChiSquare (2S + 2, 2*Lambda) which also equals 1 - fdist_Gamma (S +
        !  1, Lambda)
        ResVal = fbar_Gamma (S + ONE_DBL, 15, Lambda)
    ELSE
        ! Naive computation: sum all prob. from I = 0 to I = S
        Term = EXP (-Lambda)
        Sum = Term
        DO I = 1, S
            Term = Term * (Lambda / I)
            Sum = Sum + Term
        END DO
        ResVal = Sum
    END IF

    RETURN

END FUNCTION fdist_Poisson1

!******************************************************************************
MODULE FUNCTION fdist_Poisson2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the Poisson distribution function from the structure W, which must have
    ! been created previously by calling "fmass_CreatePoisson" with the desired Lambda.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: RMAX = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Lambda

! FLOW

    IF (.NOT.ALLOCATED(W%ParamR)) THEN
        CALL Handle_ErrLevel('fdist_Poisson2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    Lambda = W%ParamR(0)

    IF (Lambda == ZERO_DBL) THEN
        ResVal = ONE_DBL
    ELSEIF (S < 0) THEN
        ResVal = ZERO_DBL
    ELSEIF (.NOT.ALLOCATED(W%CDF)) THEN
        ! For large Lambda, we use the Chi2 distribution according to the exact
        !   relation, with 2S + 2 degrees of freedom
        ! fdist_Poisson (Lambda, S) = 1 - fdist_ChiSquare (2S + 2, 2*Lambda)
        ! which equals also 1 - fdist_Gamma (S + 1, Lambda)
        ResVal = fbar_Gamma (S + ONE_DBL, 15, Lambda)
    ELSEIF (S >= W%SMax) THEN
        ResVal = ONE_DBL
    ELSEIF (S < W%SMin) THEN
        ! Sum RMAX dominant terms to get a few decimals in the lower tail. One
        ! could also call fbar_Gamma (S + 1.0, 15, Lambda)
        BLOCK
            tIndex      :: I
            tDouble     :: Term, Sum
            Term = fmass_PoissonTerm1 (Lambda, S)
            Sum = Term
            I = S
            DO WHILE ((I > 0).AND.(I >= S - RMAX))
                Term = Term * I / Lambda
                I = I - 1
                Sum = Sum + Term
            END DO
            ResVal = Sum
        END BLOCK
    ELSEIF (S <= W%SMed) THEN
        ResVal = W%CDF(S - W%SMin)
    ELSE
        ! We keep the complementary distribution in the upper part of cdf
        ResVal = ONE_DBL - W%CDF(S + 1 - W%SMin)
    END IF

    RETURN

END FUNCTION fdist_Poisson2

!******************************************************************************
MODULE FUNCTION fdist_Binomial1(N, PIn, SIn) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the distribution function of a binomial random variable with
    ! parameters n and p, evaluated at s.  When the binomial distribution has
    ! to be computed more than once with the same parameters n and p, it is
    ! more efficient to use "fdist_Binomial2" instead of "fdist_Binomial1",
    ! unless n is very large (e.g., n > 105).
    ! Restrictions: 0 <= p <= 1 and n >= 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: PIn
    tIndex,  INTENT(IN) :: N, SIn
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: VarLimit = 100.0_kDouble
    tDouble,  PARAMETER :: Eps = fmass_Epsilon
    tInteger, PARAMETER :: NLimit1 = 10000
    tIndex,   PARAMETER :: RMAX = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Y, Z, Q, P
    tDouble     :: Sum, Term, TermMid
    tIndex      :: I, Mid, S
    tLogical    :: Flag

! FLOW

    P = PIn
    S = SIn

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_Binomial1', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    ELSEIF (N < 0) THEN
        CALL Handle_ErrLevel('fdist_Binomial1', SubModName, ErrWarning, 'N < 0.')
        RETURN
    END IF

    Q = ONE_DBL - P
    Flag = FalseVal

    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (S < 0) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (S >= N) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P <= ZERO_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P >= ONE_DBL) THEN
        ! For any S < N
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF (N < NLimit1) THEN               ! Exact Binomial
        ! sum RMAX terms to get a few decimals in the lower tail
        Mid = ToLong((N + 1) * P)
        IF (Mid > S) Mid = S
        TermMid = fmass_BinomialTerm3 (N, P, Mid)
        Term = TermMid
        Sum = Term

        Z = Q / P
        I = Mid
        DO WHILE ((Term >= Eps).OR.(I >= Mid - RMAX))
            Term = Term * (Z * I / (N - I + 1))
            Sum = Sum + Term
            I = I - 1
            IF (I == 0) EXIT
        END DO

        Z = P / Q
        Term = TermMid
        DO I = Mid, S - 1
            Term = Term * (Z * (N - I) / (I + 1))
            IF (Term < Eps) EXIT
            Sum = Sum + Term
        END DO
        ResVal = Sum
    ELSE
        IF ((P > HALF_DBL).OR.((P == HALF_DBL).AND.(S > N / 2))) THEN
            ! use F(P, N, S) = 1 - F(Q, N, N-S-1)
            P = Q
            Q = ONE_DBL - P
            Flag = TrueVal
            S = N - S - 1
        END IF
        IF (N * P * Q > VarLimit) THEN      ! Normal approximation
            ! Uses the Camp-Paulson approximation based on the F-distribution.
            ! Its maximum absolute error is smaller than 0.007 / sqrt (npq).
            ! Ref: W. Molenaar Approximations to the Poisson, Binomial,....
            ! QA273.6 M64, p. 93 (1970) */
            Term = ((S + 1) * Q / ((N - S) * P))**(ONE_DBL / 3.0_kDouble)
            Y = Term * (9.0_kDouble - ONE_DBL / (S + 1)) - 9.0_kDouble + ONE_DBL / (N - S)
            Z = 3.0_kDouble * SQRT (Term * Term / (S + 1) + ONE_DBL / (N - S))
            Y = Y / Z
            IF (Flag) THEN
                ResVal = fbar_Normal1 (Y)
            ELSE
                ResVal = fdist_Normal2 (Y)
            END IF
        ELSE                                ! Poisson approximation
            ! Uses a Bol'shev approximation based on the Poisson distribution.
            ! Error is O(1/n^4) as n -> infinity. Ref: W. Molenaar
            ! Approximations to the Poisson, Binomial,.... QA273.6 M64, p. 107,
            ! Table 6.2, Formule lambda_9 (1970).
            Y = (TWO_DBL * N - S) * P / (TWO_DBL - P)
            Z = (TWO_DBL * Y * Y - S * Y - S * S - TWO_DBL * S) / &
                (6.0_kDouble * (TWO_DBL * N - S) * (TWO_DBL * N - S))
            Z = Y / (ONE_DBL - Z)
            IF (Flag) THEN
                ResVal = fbar_Poisson1 (Z, S - 1_kIndex)
            ELSE
                ResVal = fdist_Poisson1 (Z, S)
            END IF
        END IF
    END IF

    RETURN

END FUNCTION fdist_Binomial1

!******************************************************************************
MODULE FUNCTION fdist_Binomial2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the binomial distribution function from the structure W, which must have
    ! been created previously by calling "fmass_CreateBinomial" with the desired values
    ! of n and p.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: RMAX = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P
    tIndex      :: N

! FLOW

    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fdist_Binomial2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    P = W%ParamR(0)
    N = W%ParamI(0)

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_Binomial2', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    ELSEIF (N < 0) THEN
        CALL Handle_ErrLevel('fdist_Binomial2', SubModName, ErrWarning, 'N < 0.')
        RETURN
    END IF

    IF (N == 0) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (S < 0) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (S >= N) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P == ZERO_DBL) THEN
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P == ONE_DBL) THEN
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF (ALLOCATED(W%CDF)) THEN
        IF (S >= W%SMax) THEN
            ResVal = ONE_DBL
        ELSEIF (S < W%SMin) THEN
            ! sum RMAX terms to get a few decimals in the lower tail
            BLOCK
                tIndex      :: I
                tDouble     :: Term, Sum, Z
                Term = fmass_BinomialTerm3 (N, P, S)
                Sum = Term
                Z = (ONE_DBL - P) / P
                I = S
                DO WHILE ((I > 0).AND.(I >= S - RMAX))
                    Term = Term * (Z * I / (N - I + 1))
                    I = I - 1
                    Sum = Sum + Term
                END DO
                ResVal = Sum
            END BLOCK
        ELSEIF (S <= W%SMed) THEN
            ResVal = W%CDF(S - W%SMin)
        ELSE
            ! We keep the complementary distribution in the upper part of cdf
            ResVal = ONE_DBL - W%CDF(S + 1 - W%SMin)
        END IF
    ELSE
        ResVal = fdist_Binomial1 (N, P, S)
    END IF

    RETURN

END FUNCTION fdist_Binomial2

!******************************************************************************
MODULE FUNCTION fdist_NegaBin1(N, P, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the distribution function of a negative binomial random variable
    ! with parameters n and p, evaluated at s.  If this distribution has to be
    ! computed more than once with the same n and p, it is  more efficient to use
    ! "fdist_NegaBin2" instead of "fdist_NegaBin1", unless n is very large.
    ! Restrictions: 0 <= p <= 1 and n > 0.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: P
    tIndex,  INTENT(IN) :: N, S
    tDouble             :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble,  PARAMETER :: Eps = fmass_Epsilon
    tInteger, PARAMETER :: Limit1 = 10000

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: Sum, Term, TermMode
    tIndex      :: I, Mode

! FLOW

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_NegaBin1', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    ELSEIF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_NegaBin1', SubModName, ErrWarning, 'N <= 0.')
        RETURN
    END IF

    IF (S < 0) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (P >= ONE_DBL) THEN
        ! In fact, p == 1
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P <= ZERO_DBL) THEN
        ! In fact, p == 0
        ResVal = ZERO_DBL
        RETURN
    END IF

    ! Compute the maximum term
    Mode = 1_kLong + ToLong((N * (ONE_DBL - P) - ONE_DBL) / P)
    IF (Mode > S) Mode = S

    IF (Mode <= Limit1) THEN
        TermMode = fmass_NegaBinTerm1 (N, P, Mode)
        Term = TermMode
        Sum = Term
        DO I = Mode, 1, -1
            Term = Term * (I / ((ONE_DBL - P) * (N + I - 1)))
            IF (Term < Eps) EXIT
            Sum = Sum + Term
        END DO

        Term = TermMode
        DO I = Mode, S - 1
            Term = Term * ((ONE_DBL - P) * (N + I) / (I + 1))
            IF (Term < Eps) EXIT
            Sum = Sum + Term
        END DO
        IF (Sum <= ONE_DBL) THEN
            ResVal = Sum
        ELSE
            ResVal = ONE_DBL
        END IF

    ELSE
        ResVal = ONE_DBL - fdist_Binomial1 (S + N, P, N - 1)
    END IF

    RETURN

END FUNCTION fdist_NegaBin1

!******************************************************************************
MODULE FUNCTION fdist_NegaBin2(W, S) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the negative binomial distribution function from the structure W,
    ! which must have been created previously by calling "fmass_CreateNegaBin" with
    ! the desired values of n and p.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FMassInfo), INTENT(IN) :: W
    tIndex,          INTENT(IN) :: S
    tDouble                     :: ResVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: RMAX = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: P
    tIndex      :: N

! FLOW

    IF ((.NOT.ALLOCATED(W%ParamI)).OR.(.NOT.ALLOCATED(W%ParamR))) THEN
        CALL Handle_ErrLevel('fdist_NegaBin2', SubModName, ErrWarning, &
                          'FMassInfo has not yet been created.')
        RETURN
    END IF

    P = W%ParamR(0)
    N = W%ParamI(0)

    IF ((P < ZERO_DBL).OR.(P > ONE_DBL)) THEN
        CALL Handle_ErrLevel('fdist_NegaBin2', SubModName, ErrWarning, 'P Not in [0, 1].')
        RETURN
    ELSEIF (N <= 0) THEN
        CALL Handle_ErrLevel('fdist_NegaBin2', SubModName, ErrWarning, 'N <= 0.')
        RETURN
    END IF

    IF (S < 0) THEN
        ResVal = ZERO_DBL
        RETURN
    ELSEIF (P >= ONE_DBL) THEN
        ! In fact, p == 1
        ResVal = ONE_DBL
        RETURN
    ELSEIF (P <= ZERO_DBL) THEN
        ! In fact, p == 0
        ResVal = ZERO_DBL
        RETURN
    END IF

    IF (ALLOCATED(W%CDF)) THEN
        IF (S >= W%SMax) THEN
            ResVal = ONE_DBL
        ELSEIF (S < W%SMin) THEN
            ResVal = fdist_NegaBin1 (N, P, S)
        ELSEIF (S <= W%SMed) THEN
            ResVal = W%CDF(S - W%SMin)
        ELSE
            ! We keep the complementary distribution in the upper part of cdf
            ResVal = ONE_DBL - W%CDF(S + 1 - W%SMin)
        END IF
    ELSE
        ResVal = fdist_NegaBin1 (N, P, S)
    END IF

    RETURN

END FUNCTION fdist_NegaBin2

!******************************************************************************
MODULE FUNCTION fdist_Scan(N, D, M) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the distribution function of the scan statistic with
    ! parameters N and d, evaluated at m.

    IMPLICIT NONE

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN) :: D
    tIndex,  INTENT(IN) :: N, M
    tDouble             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ResVal = ONE_DBL - fbar_Scan (N, D, M)

    RETURN

END FUNCTION fdist_Scan

!******************************************************************************
END SUBMODULE SubBase_FDist

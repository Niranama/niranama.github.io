 
MODULE ModLib_Random

!** PURPOSE OF THIS MODULE:
    ! contains random number generation routines by Alan Miller.
    !
    ! Random number generation can be from the following distributions:
    !
    !     Distribution                    Function/subroutine name
    !
    !     Normal (Gaussian)               random_normal
    !     Gamma                           random_gamma
    !     Chi-squared                     random_chisq
    !     Exponential                     random_exponential
    !     Weibull                         random_Weibull
    !     Beta                            random_beta
    !     t                               random_t
    !     Multivariate normal             random_mvnorm
    !     Generalized inverse Gaussian    random_inv_gauss
    !     Poisson                         random_Poisson
    !     Binomial                        random_binomial1   *
    !                                     random_binomial2   *
    !     Negative binomial               random_neg_binomial
    !     von Mises                       random_von_Mises
    !     Cauchy                          random_Cauchy
    !
    !  Generate a random ordering of the integers 1 .. N
    !                                     random_order
    !     Initialize (seed) the uniform random number generator for ANY compiler
    !                                     seed_random_number
 
    !     Lognormal - see note below.
 
    !  ** Two functions are provided for the binomial distribution.
    !  If the parameter values remain constant, it is recommended that the
    !  first function is used (random_binomial1).   If one or both of the
    !  parameters change, use the second function (random_binomial2).
 
    ! The compilers own random number generator, SUBROUTINE RANDOM_NUMBER(r),
    ! is used to provide a source of uniformly distributed random numbers.
 
    ! N.B. At this stage, only one random number is generated at each call to
    !      one of the functions above.
 
    ! The module uses the following functions which are included here:
    ! bin_prob to calculate a single binomial probability
    ! lngamma  to calculate the logarithm to base e of the gamma function
 
    ! Some of the code is adapted from Dagpunar's book:
    !     Dagpunar, J. 'Principles of random variate generation'
    !     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
    !
    ! In most of Dagpunar's routines, there is a test to see whether the value
    ! of one or two floating-point parameters has changed since the last call.
    ! These tests have been replaced by using a logical variable FIRST.
    ! This should be set to .TRUE. on the first call using new values of the
    ! parameters, and .FALSE. if the parameter values are the same as for the
    ! previous call.
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Lognormal distribution
    ! If X has a lognormal distribution, then log(X) is normally distributed.
    ! Here the logarithm is the natural logarithm, that is to base e, sometimes
    ! denoted as ln.  To generate random variates from this distribution, generate
    ! a random deviate from the normal distribution with mean and variance equal
    ! to the mean and variance of the logarithms of X, then take its exponential.
 
    ! Relationship between the mean & variance of log(X) and the mean & variance
    ! of X, when X has a lognormal distribution.
    ! Let m = mean of log(X), and s^2 = variance of log(X)
    ! Then
    ! mean of X     = exp(m + 0.5s^2)
    ! variance of X = (mean(X))^2.[exp(s^2) - 1]
 
    ! In the reverse direction (rarely used)
    ! variance of log(X) = log[1 + var(X)/(mean(X))^2]
    ! mean of log(X)     = log(mean(X) - 0.5var(log(X))
 
    ! N.B. The above formulae relate to population parameters; they will only be
    !      approximate if applied to sample values.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RANDOM_NORMAL, RANDOM_GAMMA, RANDOM_GAMMA1, RANDOM_GAMMA2
    PUBLIC :: RANDOM_CHISQ, RANDOM_EXPONENTIAL, RANDOM_WEIBULL, RANDOM_BETA
    PUBLIC :: RANDOM_T, RANDOM_MVNORM, RANDOM_INV_GAUSS, RANDOM_POISSON
    PUBLIC :: RANDOM_BINOMIAL1, RANDOM_BINOMIAL2, RANDOM_NEG_BINOMIAL
    PUBLIC :: RANDOM_VON_MISES, RANDOM_CAUCHY, RANDOM_ORDER, SEED_RANDOM_NUMBER
    
    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    REAL,    PARAMETER  :: ZERO = 0.0
    REAL,    PARAMETER  :: HALF = 0.5
    REAL,    PARAMETER  :: ONE = 1.0
    REAL,    PARAMETER  :: TWO = 2.0
    REAL,    PARAMETER  :: VSMALL = TINY (1.0)
    REAL,    PARAMETER  :: VLARGE = HUGE (1.0)
    INTEGER, PARAMETER  :: DP = SELECTED_REAL_KIND (12, 60)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION RANDOM_NORMAL () RESULT (FN_VAL)
 
! Adapted from the following Fortran 77 code
!      ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!      VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.
 
!  The function random_normal() returns a normally distributed pseudo-random
!  number with zero mean and unit variance.
 
!  The algorithm uses the ratio of uniforms method of A.J. Kinderman
!  and J.F. Monahan augmented with quadratic bounding curves.
 
    REAL :: FN_VAL
 
!     Parameters
    REAL, PARAMETER :: S = 0.449871, T = - 0.386595, A = 0.19600, B = 0.25472, &
                       R1 = 0.27597, R2 = 0.27846
        
!     Local variables
    REAL :: U, V, X, Y, Q
 
!     Generate P = (u,v) uniform in rectangle enclosing acceptance region
 
    DO
        CALL RANDOM_NUMBER (U)
        CALL RANDOM_NUMBER (V)
        V = 1.7156 * (V-HALF)
 
!     Evaluate the quadratic form
        X = U - S
        Y = ABS (V) - T
        Q = X ** 2 + Y * (A*Y-B*X)
 
!     Accept P if inside inner ellipse
        IF (Q < R1) EXIT
!     Reject P if outside outer ellipse
        IF (Q > R2) CYCLE
!     Reject P if outside acceptance region
        IF (V**2 <-4.0*LOG(U)*U**2) EXIT
    END DO
 
!     Return ratio of P's coordinates as the normal deviate
    FN_VAL = V / U
    RETURN
 
END FUNCTION RANDOM_NORMAL
 
!******************************************************************************
 
FUNCTION RANDOM_GAMMA (S, FIRST) RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
!     FUNCTION GENERATES A RANDOM GAMMA VARIATE.
!     CALLS EITHER random_gamma1 (S > 1.0)
!     OR random_exponential (S = 1.0)
!     OR random_gamma2 (S < 1.0).
 
!     S = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
 
    REAL, INTENT (IN) :: S
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
    IF (S <= ZERO) THEN
        WRITE (*,*) 'SHAPE PARAMETER VALUE MUST BE POSITIVE'
        STOP
    END IF
 
    IF (S > ONE) THEN
        FN_VAL = RANDOM_GAMMA1 (S, FIRST)
    ELSE IF (S < ONE) THEN
        FN_VAL = RANDOM_GAMMA2 (S, FIRST)
    ELSE
        FN_VAL = RANDOM_EXPONENTIAL ()
    END IF
 
    RETURN
END FUNCTION RANDOM_GAMMA
 
!******************************************************************************
 
FUNCTION RANDOM_GAMMA1 (S, FIRST) RESULT (FN_VAL)
 
! Uses the algorithm in
! Marsaglia, G. and Tsang, W.W. (2000) `A simple method for generating
! gamma variables', Trans. om Math. Software (TOMS), vol.26(3), pp.363-372.
 
! Generates a random gamma deviate for shape parameter s >= 1.
 
    REAL, INTENT (IN) :: S
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
! Local variables
    REAL, SAVE :: C, D
    REAL :: U, V, X
 
    IF (FIRST) THEN
        D = S - ONE / 3.
        C = ONE / SQRT (9.0*D)
    END IF
 
! Start of main loop
    DO
 
! Generate v = (1+cx)^3 where x is random normal; repeat if v <= 0.
 
        DO
            X = RANDOM_NORMAL ()
            V = (ONE+C*X) ** 3
            IF (V > ZERO) EXIT
        END DO
 
! Generate uniform variable U
 
        CALL RANDOM_NUMBER (U)
        IF (U < ONE-0.0331*X**4) THEN
            FN_VAL = D * V
            EXIT
        ELSE IF (LOG(U) < HALF*X**2+D*(ONE-V+LOG(V))) THEN
            FN_VAL = D * V
            EXIT
        END IF
    END DO
 
    RETURN
END FUNCTION RANDOM_GAMMA1
 
!******************************************************************************
 
FUNCTION RANDOM_GAMMA2 (S, FIRST) RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
! GAMMA2**(S-1) * EXP(-GAMMA2),
! USING A SWITCHING METHOD.
 
!    S = SHAPE PARAMETER OF DISTRIBUTION
!          (REAL < 1.0)
 
    REAL, INTENT (IN) :: S
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
!     Local variables
    REAL :: R, X, W
    REAL, SAVE :: A, P, C, UF, VR, D
 
    IF (S <= ZERO .OR. S >=ONE) THEN
        WRITE (*,*) 'SHAPE PARAMETER VALUE OUTSIDE PERMITTED RANGE'
        STOP
    END IF
 
    IF (FIRST) THEN ! Initialization, if necessary
        A = ONE - S
        P = A / (A+S*EXP(-A))
        IF (S < VSMALL) THEN
            WRITE (*,*) 'SHAPE PARAMETER VALUE TOO SMALL'
            STOP
        END IF
        C = ONE / S
        UF = P * (VSMALL/A) ** S
        VR = ONE - VSMALL
        D = A * LOG (A)
    END IF
 
    DO
        CALL RANDOM_NUMBER (R)
        IF (R >=VR) THEN
            CYCLE
        ELSE IF (R > P) THEN
            X = A - LOG ((ONE-R)/(ONE-P))
            W = A * LOG (X) - D
        ELSE IF (R > UF) THEN
            X = A * (R/P) ** C
            W = X
        ELSE
            FN_VAL = ZERO
            RETURN
        END IF
 
        CALL RANDOM_NUMBER (R)
        IF (ONE-R <= W .AND. R > ZERO) THEN
            IF (R*(W+ONE) >=ONE) CYCLE
            IF (-LOG(R) <= W) CYCLE
        END IF
        EXIT
    END DO
 
    FN_VAL = X
    RETURN
 
END FUNCTION RANDOM_GAMMA2
 
!******************************************************************************
 
FUNCTION RANDOM_CHISQ (NDF, FIRST) RESULT (FN_VAL)
 
!     Generates a random variate from the chi-squared distribution with
!     ndf degrees of freedom
 
    INTEGER, INTENT (IN) :: NDF
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
    FN_VAL = TWO * RANDOM_GAMMA (HALF*NDF, FIRST)
    RETURN
 
END FUNCTION RANDOM_CHISQ
 
!******************************************************************************
 
FUNCTION RANDOM_EXPONENTIAL () RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
! A NEGATIVE EXPONENTIAL DlSTRIBUTION WlTH DENSITY PROPORTIONAL
! TO EXP(-random_exponential), USING INVERSION.
 
    REAL :: FN_VAL
 
!     Local variable
    REAL :: R
 
    DO
        CALL RANDOM_NUMBER (R)
        IF (R > ZERO) EXIT
    END DO
 
    FN_VAL = - LOG (R)
    RETURN
 
END FUNCTION RANDOM_EXPONENTIAL
 
!******************************************************************************
 
FUNCTION RANDOM_WEIBULL (A) RESULT (FN_VAL)
 
!     Generates a random variate from the Weibull distribution with
!     probability density:
!                      a
!               a-1  -x
!     f(x) = a.x    e
 
    REAL, INTENT (IN) :: A
    REAL :: FN_VAL
 
!     For speed, there is no checking that a is not zero or very small.
 
    FN_VAL = RANDOM_EXPONENTIAL () ** (ONE/A)
    RETURN
 
END FUNCTION RANDOM_WEIBULL
 
!******************************************************************************
 
FUNCTION RANDOM_BETA (AA, BB, FIRST) RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM VARIATE IN [0,1]
! FROM A BETA DISTRIBUTION WITH DENSITY
! PROPORTIONAL TO BETA**(AA-1) * (1-BETA)**(BB-1).
! USING CHENG'S LOG LOGISTIC METHOD.
 
!     AA = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
!     BB = SHAPE PARAMETER FROM DISTRIBUTION (0 < REAL)
 
    REAL, INTENT (IN) :: AA, BB
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
!     Local variables
    REAL, PARAMETER :: ALN4 = 1.3862944
    REAL :: A, B, G, R, S, X, Y, Z
    REAL, SAVE :: D, F, H, T, C
    LOGICAL, SAVE :: SWAP
 
    IF (AA <= ZERO .OR. BB <= ZERO) THEN
        WRITE (*,*) 'IMPERMISSIBLE SHAPE PARAMETER VALUE(S)'
        STOP
    END IF
 
    IF (FIRST) THEN ! Initialization, if necessary
        A = AA
        B = BB
        SWAP = B > A
        IF (SWAP) THEN
            G = B
            B = A
            A = G
        END IF
        D = A / B
        F = A + B
        IF (B > ONE) THEN
            H = SQRT ((TWO*A*B-F)/(F-TWO))
            T = ONE
        ELSE
            H = B
            T = ONE / (ONE+(A/(VLARGE*B))**B)
        END IF
        C = A + H
    END IF
 
    DO
        CALL RANDOM_NUMBER (R)
        CALL RANDOM_NUMBER (X)
        S = R * R * X
        IF (R < VSMALL .OR. S <= ZERO) CYCLE
        IF (R < T) THEN
            X = LOG (R/(ONE-R)) / H
            Y = D * EXP (X)
            Z = C * X + F * LOG ((ONE+D)/(ONE+Y)) - ALN4
            IF (S-ONE > Z) THEN
                IF (S-S*Z > ONE) CYCLE
                IF (LOG(S) > Z) CYCLE
            END IF
            FN_VAL = Y / (ONE+Y)
        ELSE
            IF (4.0*S > (ONE+ONE/D)**F) CYCLE
            FN_VAL = ONE
        END IF
        EXIT
    END DO
 
    IF (SWAP) FN_VAL = ONE - FN_VAL
    RETURN
END FUNCTION RANDOM_BETA
 
!******************************************************************************
 
FUNCTION RANDOM_T (M) RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM VARIATE FROM A
! T DISTRIBUTION USING KINDERMAN AND MONAHAN'S RATIO METHOD.
 
!     M = DEGREES OF FREEDOM OF DISTRIBUTION
!           (1 <= 1NTEGER)
 
    INTEGER, INTENT (IN) :: M
    REAL :: FN_VAL
 
!     Local variables
    REAL, SAVE :: S, C, A, F, G
    REAL :: R, X, V
 
    REAL, PARAMETER :: THREE = 3.0, FOUR = 4.0, QUART = 0.25, FIVE = 5.0, SIXTEEN = 16.0
    INTEGER :: MM = 0
 
    IF (M < 1) THEN
        WRITE (*,*) 'IMPERMISSIBLE DEGREES OF FREEDOM'
        STOP
    END IF
 
    IF (M /= MM) THEN ! Initialization, if necessary
        S = M
        C = - QUART * (S+ONE)
        A = FOUR / (ONE+ONE/S) ** C
        F = SIXTEEN / A
        IF (M > 1) THEN
            G = S - ONE
            G = ((S+ONE)/G) ** C * SQRT ((S+S)/G)
        ELSE
            G = ONE
        END IF
        MM = M
    END IF
 
    DO
        CALL RANDOM_NUMBER (R)
        IF (R <= ZERO) CYCLE
        CALL RANDOM_NUMBER (V)
        X = (TWO*V-ONE) * G / R
        V = X * X
        IF (V > FIVE-A*R) THEN
            IF (M >=1 .AND. R*(V+THREE) > F) CYCLE
            IF (R > (ONE+V/S)**C) CYCLE
        END IF
        EXIT
    END DO
 
    FN_VAL = X
    RETURN
END FUNCTION RANDOM_T
 
!******************************************************************************
 
SUBROUTINE RANDOM_MVNORM (N, H, D, F, FIRST, X, IER)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! N.B. An extra argument, ier, has been added to Dagpunar's routine
 
!     SUBROUTINE GENERATES AN N VARIATE RANDOM NORMAL
!     VECTOR USING A CHOLESKY DECOMPOSITION.
 
! ARGUMENTS:
!        N = NUMBER OF VARIATES IN VECTOR
!           (INPUT,INTEGER >= 1)
!     H(J) = J'TH ELEMENT OF VECTOR OF MEANS
!           (INPUT,REAL)
!     X(J) = J'TH ELEMENT OF DELIVERED VECTOR
!           (OUTPUT,REAL)
!
!    D(J*(J-1)/2+I) = (I,J)'TH ELEMENT OF VARIANCE MATRIX (J> = I)
!            (INPUT,REAL)
!    F((J-1)*(2*N-J)/2+I) = (I,J)'TH ELEMENT OF LOWER TRIANGULAR
!           DECOMPOSITION OF VARIANCE MATRIX (J <= I)
!            (OUTPUT,REAL)
 
!    FIRST = .TRUE. IF THIS IS THE FIRST CALL OF THE ROUTINE
!    OR IF THE DISTRIBUTION HAS CHANGED SINCE THE LAST CALL OF THE ROUTINE.
!    OTHERWISE SET TO .FALSE.
!            (INPUT,LOGICAL)
 
!    ier = 1 if the input covariance matrix is not +ve definite
!        = 0 otherwise
 
    INTEGER, INTENT (IN) :: N
    REAL, INTENT (IN) :: H (:), D (:)! d(n*(n+1)/2)
    REAL, INTENT (INOUT) :: F (:)! f(n*(n+1)/2)
    REAL, INTENT (OUT) :: X (:)
    LOGICAL, INTENT (IN) :: FIRST
    INTEGER, INTENT (OUT) :: IER
 
!     Local variables
    INTEGER :: J, I, M
    REAL :: Y, V
    INTEGER, SAVE :: N2
 
    IF (N < 1) THEN
        WRITE (*,*) 'SIZE OF VECTOR IS NON POSITIVE'
        STOP
    END IF
 
    IER = 0
    IF (FIRST) THEN ! Initialization, if necessary
        N2 = 2 * N
        IF (D(1) < ZERO) THEN
            IER = 1
            RETURN
        END IF
 
        F (1) = SQRT (D(1))
        Y = ONE / F (1)
        DO J = 2, N
            F (J) = D (1+J*(J-1)/2) * Y
        END DO
 
        DO I = 2, N
            V = D (I*(I-1)/2+I)
            DO M = 1, I - 1
                V = V - F ((M-1)*(N2-M)/2+I) ** 2
            END DO
 
            IF (V < ZERO) THEN
                IER = 1
                RETURN
            END IF
 
            V = SQRT (V)
            Y = ONE / V
            F ((I-1)*(N2-I)/2+I) = V
            DO J = I + 1, N
                V = D (J*(J-1)/2+I)
                DO M = 1, I - 1
                    V = V - F ((M-1)*(N2-M)/2+I) * F ((M-1)*(N2-M)/2+J)
                END DO ! m = 1,i-1
                F ((I-1)*(N2-I)/2+J) = V * Y
            END DO ! j = i+1,n
        END DO ! i = 2,n
    END IF
 
    X (1:N) = H (1:N)
    DO J = 1, N
        Y = RANDOM_NORMAL ()
        DO I = J, N
            X (I) = X (I) + F ((J-1)*(N2-J)/2+I) * Y
        END DO ! i = j,n
    END DO ! j = 1,n
 
    RETURN
END SUBROUTINE RANDOM_MVNORM
 
!******************************************************************************
 
FUNCTION RANDOM_INV_GAUSS (H, B, FIRST) RESULT (FN_VAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY] FROM
! A REPARAMETERISED GENERALISED INVERSE GAUSSIAN (GIG) DISTRIBUTION
! WITH DENSITY PROPORTIONAL TO  GIG**(H-1) * EXP(-0.5*B*(GIG+1/GIG))
! USING A RATIO METHOD.
 
!     H = PARAMETER OF DISTRIBUTION (0 <= REAL)
!     B = PARAMETER OF DISTRIBUTION (0 < REAL)
 
    REAL, INTENT (IN) :: H, B
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
!     Local variables
    REAL :: YM, XM, R, W, R1, R2, X
    REAL, SAVE :: A, C, D, E
    REAL, PARAMETER :: QUART = 0.25
 
    IF (H < ZERO .OR. B <= ZERO) THEN
        WRITE (*,*) 'IMPERMISSIBLE DISTRIBUTION PARAMETER VALUES'
        STOP
    END IF
 
    IF (FIRST) THEN ! Initialization, if necessary
        IF (H > QUART*B*SQRT(VLARGE)) THEN
            WRITE (*,*) 'THE RATIO H:B IS TOO SMALL'
            STOP
        END IF
        E = B * B
        D = H + ONE
        YM = (-D+SQRT(D*D+E)) / B
        IF (YM < VSMALL) THEN
            WRITE (*,*) 'THE VALUE OF B IS TOO SMALL'
            STOP
        END IF
 
        D = H - ONE
        XM = (D+SQRT(D*D+E)) / B
        D = HALF * D
        E = - QUART * B
        R = XM + ONE / XM
        W = XM * YM
        A = W ** (-HALF*H) * SQRT (XM/YM) * EXP (-E*(R-YM-ONE/YM))
        IF (A < VSMALL) THEN
            WRITE (*,*) 'THE VALUE OF H IS TOO LARGE'
            STOP
        END IF
        C = - D * LOG (XM) - E * R
    END IF
 
    DO
        CALL RANDOM_NUMBER (R1)
        IF (R1 <= ZERO) CYCLE
        CALL RANDOM_NUMBER (R2)
        X = A * R2 / R1
        IF (X <= ZERO) CYCLE
        IF (LOG(R1) < D*LOG(X)+E*(X+ONE/X)+C) EXIT
    END DO
 
    FN_VAL = X
 
    RETURN
END FUNCTION RANDOM_INV_GAUSS
 
!******************************************************************************
 
FUNCTION RANDOM_POISSON (MU, FIRST) RESULT (IVAL)
!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:
!                           RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                    Compiled and Written by:
!
!                         Barry W. Brown
!                          James Lovato
!
!             Department of Biomathematics, Box 237
!             The University of Texas, M.D. Anderson Cancer Center
!             1515 Holcombe Boulevard
!             Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.
 
!                    GENerate POIsson random deviate
 
!                            Function
 
! Generates a single random deviate from a Poisson distribution with mean mu.
 
!                            Arguments
 
!     mu --> The mean of the Poisson distribution from which
!            a random deviate is to be generated.
!                              REAL mu
 
!                              Method
 
!     For details see:
 
!               Ahrens, J.H. and Dieter, U.
!               Computer Generation of Poisson Deviates
!               From Modified Normal Distributions.
!               ACM Trans. Math. Software, 8, 2
!               (June 1982),163-179
 
!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
 
!     SEPARATION OF CASES A AND B
 
!     .. Scalar Arguments ..
    REAL, INTENT (IN) :: MU
    LOGICAL, INTENT (IN) :: FIRST
    INTEGER :: IVAL
!     ..
!     .. Local Scalars ..
    REAL :: B1, B2, C, C0, C1, C2, C3, DEL, DIFMUK, E, FK, FX, FY, G, OMEGA, &
            PX, PY, T, U, V, X, XX
    REAL, SAVE :: S, D, P, Q, P0
    INTEGER :: J, K, KFLAG
    LOGICAL, SAVE :: FULL_INIT
    INTEGER, SAVE :: L, M
!     ..
!     .. Local Arrays ..
    REAL, SAVE :: PP (35)
!     ..
!     .. Data statements ..
    REAL, PARAMETER :: A0 = - .5, A1 = .3333333, A2 = - .2500068, A3 = .2000118, &
                       A4 = - .1661269, A5 = .1421878, A6 = - .1384794, A7 = .1250060
 
    REAL, PARAMETER :: FACT (10) = [ 1., 1., 2., 6., 24., 120., 720., 5040., &
                                    40320., 362880. ]
 
!     ..
!     .. Executable Statements ..
    IF (MU > 10.0) THEN
!     C A S E  A. (RECALCULATION OF S, D, L IF MU HAS CHANGED)
 
        IF (FIRST) THEN
            S = SQRT (MU)
            D = 6.0 * MU * MU
 
!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)
!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .
 
            L = MU - 1.1484
            FULL_INIT = .FALSE.
        END IF
 
 
!     STEP N. NORMAL SAMPLE - random_normal() FOR STANDARD NORMAL DEVIATE
 
        G = MU + S * RANDOM_NORMAL ()
        IF (G > 0.0) THEN
            IVAL = G
 
!     STEP I. IMMEDIATE ACCEPTANCE IF ival IS LARGE ENOUGH
 
            IF (IVAL >=L) RETURN
 
!     STEP S. SQUEEZE ACCEPTANCE - SAMPLE U
 
            FK = IVAL
            DIFMUK = MU - FK
            CALL RANDOM_NUMBER (U)
            IF (D*U >=DIFMUK*DIFMUK*DIFMUK) RETURN
        END IF
 
!     STEP P. PREPARATIONS FOR STEPS Q AND H.
!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.
 
        IF ( .NOT. FULL_INIT) THEN
            OMEGA = .3989423 / S
            B1 = .4166667E-1 / MU
            B2 = .3 * B1 * B1
            C3 = .1428571 * B1 * B2
            C2 = B2 - 15. * C3
            C1 = B1 - 6. * B2 + 45. * C3
            C0 = 1. - B1 + 3. * B2 - 15. * C3
            C = .1069 / MU
            FULL_INIT = .TRUE.
        END IF
 
        IF (G < 0.0) GO TO 50
 
!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)
 
        KFLAG = 0
        GO TO 70
 
!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)
 
40      IF (FY-U*FY <= PY*EXP(PX-FX)) RETURN
 
!     STEP E. EXPONENTIAL SAMPLE - random_exponential() FOR STANDARD EXPONENTIAL
!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)
 
50          E = RANDOM_EXPONENTIAL ()
        CALL RANDOM_NUMBER (U)
        U = U + U - ONE
        T = 1.8 + SIGN (E, U)
        IF (T <= (-.6744)) GO TO 50
        IVAL = MU + S * T
        FK = IVAL
        DIFMUK = MU - FK
 
!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)
 
        KFLAG = 1
        GO TO 70
 
!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)
 
60      IF (C*ABS(U) > PY*EXP(PX+E)-FY*EXP(FX+E)) GO TO 50
        RETURN
 
!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX, PY, FX, FY.
!             CASE ival < 10 USES FACTORIALS FROM TABLE FACT
 
70      IF (IVAL >=10) GO TO 80
        PX = - MU
        PY = MU ** IVAL / FACT (IVAL+1)
        GO TO 110
 
!             CASE ival >= 10 USES POLYNOMIAL APPROXIMATION
!             A0-A7 FOR ACCURACY WHEN ADVISABLE
!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)
 
80      DEL = .8333333E-1 / FK
        DEL = DEL - 4.8 * DEL * DEL * DEL
        V = DIFMUK / FK
        IF (ABS(V) > 0.25) THEN
            PX = FK * LOG (ONE+V) - DIFMUK - DEL
        ELSE
            PX = FK * V * V * (((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V+A0) - DEL
        END IF
        PY = .3989423 / SQRT (FK)
110     X = (HALF-DIFMUK) / S
        XX = X * X
        FX = - HALF * XX
        FY = OMEGA * (((C3*XX+C2)*XX+C1)*XX+C0)
        IF (KFLAG <= 0) GO TO 40
        GO TO 60
 
!---------------------------------------------------------------------------
!     C A S E  B.    mu < 10
!     START NEW TABLE AND CALCULATE P0 IF NECESSARY
 
    ELSE
        IF (FIRST) THEN
            M = MAX (1, INT(MU))
            L = 0
            P = EXP (-MU)
            Q = P
            P0 = P
        END IF
 
!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD
 
        DO
            CALL RANDOM_NUMBER (U)
            IVAL = 0
            IF (U <= P0) RETURN
 
!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
!             (0.458=PP(9) FOR MU=10)
 
            IF (L == 0) GO TO 150
            J = 1
            IF (U > 0.458) J = MIN (L, M)
            DO K = J, L
                IF (U <= PP(K)) GO TO 180
            END DO
            IF (L == 35) CYCLE
 
!     STEP C. CREATION OF NEW POISSON PROBABILITIES P
!             AND THEIR CUMULATIVES Q=PP(K)
 
150         L = L + 1
            DO K = L, 35
                P = P * MU / K
                Q = Q + P
                PP (K) = Q
                IF (U <= Q) GO TO 170
            END DO
            L = 35
        END DO
 
170     L = K
180     IVAL = K
        RETURN
    END IF
 
    RETURN
END FUNCTION RANDOM_POISSON
 
!******************************************************************************
 
FUNCTION RANDOM_BINOMIAL1 (N, P, FIRST) RESULT (IVAL)
 
! FUNCTION GENERATES A RANDOM BINOMIAL VARIATE USING C.D.Kemp's method.
! This algorithm is suitable when many random variates are required
! with the SAME parameter values for n & p.
 
!    P = BERNOULLI SUCCESS PROBABILITY
!           (0 <= REAL <= 1)
!    N = NUMBER OF BERNOULLI TRIALS
!           (1 <= INTEGER)
!    FIRST = .TRUE. for the first call using the current parameter values
!          = .FALSE. if the values of (n,p) are unchanged from last call
 
! Reference: Kemp, C.D. (1986). `A modal method for generating binomial
!            variables', Commun. Statist. - Theor. Meth. 15(3), 805-813.
 
    INTEGER, INTENT (IN) :: N
    REAL, INTENT (IN) :: P
    LOGICAL, INTENT (IN) :: FIRST
    INTEGER :: IVAL
 
!     Local variables
 
    INTEGER :: RU, RD
    INTEGER, SAVE :: R0
    REAL :: U, PD, PU
    REAL, SAVE :: ODDS_RATIO, P_R
    REAL, PARAMETER :: ZERO = 0.0, ONE = 1.0
 
    IF (FIRST) THEN
        R0 = (N+1) * P
        P_R = BIN_PROB (N, P, R0)
        ODDS_RATIO = P / (ONE-P)
    END IF
 
    CALL RANDOM_NUMBER (U)
    U = U - P_R
    IF (U < ZERO) THEN
        IVAL = R0
        RETURN
    END IF
 
    PU = P_R
    RU = R0
    PD = P_R
    RD = R0
    DO
        RD = RD - 1
        IF (RD >=0) THEN
            PD = PD * (RD+1) / (ODDS_RATIO*(N-RD))
            U = U - PD
            IF (U < ZERO) THEN
                IVAL = RD
                RETURN
            END IF
        END IF
 
        RU = RU + 1
        IF (RU <= N) THEN
            PU = PU * (N-RU+1) * ODDS_RATIO / RU
            U = U - PU
            IF (U < ZERO) THEN
                IVAL = RU
                RETURN
            END IF
        END IF
    END DO
 
!     This point should not be reached, but just in case:
 
    IVAL = R0
    RETURN
 
END FUNCTION RANDOM_BINOMIAL1
 
!******************************************************************************
 
FUNCTION BIN_PROB (N, P, R) RESULT (FN_VAL)
!     Calculate a binomial probability
 
    INTEGER, INTENT (IN) :: N, R
    REAL, INTENT (IN) :: P
    REAL :: FN_VAL
 
!     Local variable
    REAL :: ONE = 1.0
 
    FN_VAL = EXP (LNGAMMA(DBLE(N+1)) - LNGAMMA(DBLE(R+1)) - LNGAMMA(DBLE(N-R+1)) + &
                  R*LOG(P) + (N-R)*LOG(ONE-P))
    RETURN
 
END FUNCTION BIN_PROB
 
!******************************************************************************
 
FUNCTION LNGAMMA (X) RESULT (FN_VAL)
 
! Logarithm to base e of the gamma function.
!
! Accurate to about 1.e-14.
! Programmer: Alan Miller
 
! Latest revision of Fortran 77 version - 28 February 1988
 
    REAL (DP), INTENT (IN) :: X
    REAL (DP) :: FN_VAL
 
!       Local variables
 
    REAL (DP), PARAMETER :: A1 = - 4.166666666554424D-02, A2 = 2.430554511376954D-03, &
                            A3 = - 7.685928044064347D-04, A4 = 5.660478426014386D-04, &
                            LNRT2PI = 9.189385332046727D-1, PI = 3.141592653589793D0
    REAL (DP) :: TEMP, ARG, PRODUCT
    LOGICAL :: REFLECT
 
!       lngamma is not defined if x = 0 or a negative integer.
 
    IF (X > 0.d0) GO TO 10
    IF (X /= INT(X)) GO TO 10
    FN_VAL = 0.d0
    RETURN
 
!       If x < 0, use the reflection formula:
!               gamma(x) * gamma(1-x) = pi * cosec(pi.x)
 
10  REFLECT = (X < 0.d0)
    IF (REFLECT) THEN
        ARG = 1.d0 - X
    ELSE
        ARG = X
    END IF
 
!       Increase the argument, if necessary, to make it > 10.
 
    PRODUCT = 1.d0
20  IF (ARG <= 10.d0) THEN
        PRODUCT = PRODUCT * ARG
        ARG = ARG + 1.d0
        GO TO 20
    END IF
 
!  Use a polynomial approximation to Stirling's formula.
!  N.B. The real Stirling's formula is used here, not the simpler, but less
!       accurate formula given by De Moivre in a letter to Stirling, which
!       is the one usually quoted.
 
    ARG = ARG - 0.5D0
    TEMP = 1.D0 / ARG ** 2
    FN_VAL = LNRT2PI + ARG * (LOG(ARG)-1.d0+(((A4*TEMP+A3)*TEMP+A2)*TEMP+A1)*TEMP) &
                - LOG (PRODUCT)
    IF (REFLECT) THEN
        TEMP = SIN (PI*X)
        FN_VAL = LOG (PI/TEMP) - FN_VAL
    END IF
    RETURN
END FUNCTION LNGAMMA
 
!******************************************************************************
 
FUNCTION RANDOM_BINOMIAL2 (N, PP, FIRST) RESULT (IVAL)
!**********************************************************************
!     Translated to Fortran 90 by Alan Miller from:
!                              RANLIB
!
!     Library of Fortran Routines for Random Number Generation
!
!                      Compiled and Written by:
!
!                           Barry W. Brown
!                            James Lovato
!
!               Department of Biomathematics, Box 237
!               The University of Texas, M.D. Anderson Cancer Center
!               1515 Holcombe Boulevard
!               Houston, TX      77030
!
! This work was supported by grant CA-16672 from the National Cancer Institute.
 
!                    GENerate BINomial random deviate
 
!                              Function
 
!     Generates a single random deviate from a binomial
!     distribution whose number of trials is N and whose
!     probability of an event in each trial is P.
 
!                              Arguments
 
!     N  --> The number of trials in the binomial distribution
!            from which a random deviate is to be generated.
!                              INTEGER N
 
!     P  --> The probability of an event in each trial of the
!            binomial distribution from which a random deviate
!            is to be generated.
!                              REAL P
 
!     FIRST --> Set FIRST = .TRUE. for the first call to perform initialization
!               the set FIRST = .FALSE. for further calls using the same pair
!               of parameter values (N, P).
!                              LOGICAL FIRST
 
!     random_binomial2 <-- A random deviate yielding the number of events
!                from N independent trials, each of which has
!                a probability of event P.
!                              INTEGER random_binomial
 
!                              Method
 
!     This is algorithm BTPE from:
 
!         Kachitvichyanukul, V. and Schmeiser, B. W.
!         Binomial Random Variate Generation.
!         Communications of the ACM, 31, 2 (February, 1988) 216.
 
!**********************************************************************
 
!*****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY
 
!     ..
!     .. Scalar Arguments ..
    REAL, INTENT (IN) :: PP
    INTEGER, INTENT (IN) :: N
    LOGICAL, INTENT (IN) :: FIRST
    INTEGER :: IVAL
!     ..
!     .. Local Scalars ..
    REAL :: ALV, AMAXP, F, F1, F2, U, V, W, W2, X, X1, X2, YNORM, Z, Z2
    REAL, PARAMETER :: ZERO = 0.0, HALF = 0.5, ONE = 1.0
    INTEGER :: I, IX, IX1, K, MP
    INTEGER, SAVE :: M
    REAL, SAVE :: P, Q, XNP, FFM, FM, XNPQ, P1, XM, XL, XR, C, AL, XLL, XLR, &
                    P2, P3, P4, QN, R, G
 
!     ..
!     .. Executable Statements ..
 
!*****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE
 
    IF (FIRST) THEN
        P = MIN (PP, ONE-PP)
        Q = ONE - P
        XNP = N * P
    END IF
 
    IF (XNP > 30.) THEN
        IF (FIRST) THEN
            FFM = XNP + P
            M = FFM
            FM = M
            XNPQ = XNP * Q
            P1 = INT (2.195*SQRT(XNPQ)-4.6*Q) + HALF
            XM = FM + HALF
            XL = XM - P1
            XR = XM + P1
            C = 0.134 + 20.5 / (15.3+FM)
            AL = (FFM-XL) / (FFM-XL*P)
            XLL = AL * (ONE+HALF*AL)
            AL = (XR-FFM) / (XR*Q)
            XLR = AL * (ONE+HALF*AL)
            P2 = P1 * (ONE+C+C)
            P3 = P2 + C / XLL
            P4 = P3 + C / XLR
        END IF
 
!*****GENERATE VARIATE, Binomial mean at least 30.
 
20      CALL RANDOM_NUMBER (U)
        U = U * P4
        CALL RANDOM_NUMBER (V)
 
!     TRIANGULAR REGION
 
        IF (U <= P1) THEN
            IX = XM - P1 * V + U
            GO TO 110
        END IF
 
!     PARALLELOGRAM REGION
 
        IF (U <= P2) THEN
            X = XL + (U-P1) / C
            V = V * C + ONE - ABS (XM-X) / P1
            IF (V > ONE .OR. V <= ZERO) GO TO 20
            IX = X
        ELSE
 
!     LEFT TAIL
 
            IF (U <= P3) THEN
                IX = XL + LOG (V) / XLL
                IF (IX < 0) GO TO 20
                V = V * (U-P2) * XLL
            ELSE
 
!     RIGHT TAIL
 
                IX = XR - LOG (V) / XLR
                IF (IX > N) GO TO 20
                V = V * (U-P3) * XLR
            END IF
        END IF
 
!*****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST
 
        K = ABS (IX-M)
        IF (K <= 20 .OR. K >=XNPQ/2-1) THEN
 
!     EXPLICIT EVALUATION
 
            F = ONE
            R = P / Q
            G = (N+1) * R
            IF (M < IX) THEN
                MP = M + 1
                DO I = MP, IX
                    F = F * (G/I-R)
                END DO
 
            ELSE IF (M > IX) THEN
                IX1 = IX + 1
                DO I = IX1, M
                    F = F / (G/I-R)
                END DO
            END IF
 
            IF (V > F) THEN
                GO TO 20
            ELSE
                GO TO 110
            END IF
        END IF
 
!     SQUEEZING USING UPPER AND LOWER BOUNDS ON LOG(F(X))
 
        AMAXP = (K/XNPQ) * ((K*(K/3.+.625)+.1666666666666)/XNPQ+HALF)
        YNORM = - K * K / (2.*XNPQ)
        ALV = LOG (V)
        IF (ALV < YNORM-AMAXP) GO TO 110
        IF (ALV > YNORM+AMAXP) GO TO 20
 
!     STIRLING'S (actually de Moivre's) FORMULA TO MACHINE ACCURACY FOR
!     THE FINAL ACCEPTANCE/REJECTION TEST
 
        X1 = IX + 1
        F1 = FM + ONE
        Z = N + 1 - FM
        W = N - IX + ONE
        Z2 = Z * Z
        X2 = X1 * X1
        F2 = F1 * F1
        W2 = W * W
        IF (ALV-(XM*LOG(F1/X1)+(N-M+HALF)*LOG(Z/W)+(IX-M)*LOG(W*P/(X1*Q))+(13860.-(462.-&
        & (132.-(99.-140./F2)/F2)/F2)/F2)/F1/166320.+(13860.-(462.-(132.-(99.-&
        & 140./Z2)/Z2)/Z2)/Z2)/Z/166320.+(13860.-(462.-(132.-(99.-&
        & 140./X2)/X2)/X2)/X2)/X1/166320.+(13860.-(462.-(132.-(99.-&
        & 140./W2)/W2)/W2)/W2)/W/166320.) > ZERO) THEN
            GO TO 20
        ELSE
            GO TO 110
        END IF
 
    ELSE
!     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
        IF (FIRST) THEN
            QN = Q ** N
            R = P / Q
            G = R * (N+1)
        END IF
 
90      IX = 0
        F = QN
        CALL RANDOM_NUMBER (U)
100     IF (U >=F) THEN
            IF (IX > 110) GO TO 90
            U = U - F
            IX = IX + 1
            F = F * (G/IX-R)
            GO TO 100
        END IF
    END IF
 
110 IF (PP > HALF) IX = N - IX
    IVAL = IX
    RETURN
 
END FUNCTION RANDOM_BINOMIAL2
 
!******************************************************************************
 
FUNCTION RANDOM_NEG_BINOMIAL (SK, P) RESULT (IVAL)
 
! Adapted from Fortran 77 code from the book:
!     Dagpunar, J. 'Principles of random variate generation'
!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
 
! FUNCTION GENERATES A RANDOM NEGATIVE BINOMIAL VARIATE USING UNSTORED
! INVERSION AND/OR THE REPRODUCTIVE PROPERTY.
 
!    SK = NUMBER OF FAILURES REQUIRED (Dagpunar's words!)
!       = the `power' parameter of the negative binomial
!           (0 < REAL)
!    P = BERNOULLI SUCCESS PROBABILITY
!           (0 < REAL < 1)
 
! THE PARAMETER H IS SET SO THAT UNSTORED INVERSION ONLY IS USED WHEN P <= H,
! OTHERWISE A COMBINATION OF UNSTORED INVERSION AND
! THE REPRODUCTIVE PROPERTY IS USED.
 
    REAL, INTENT (IN) :: SK, P
    INTEGER :: IVAL
 
!     Local variables
! THE PARAMETER ULN = -LOG(MACHINE'S SMALLEST REAL NUMBER).
 
    REAL, PARAMETER :: H = 0.7
    REAL :: Q, X, ST, ULN, V, R, S, Y, G
    INTEGER :: K, I, N
 
    IF (SK <= ZERO .OR. P <= ZERO .OR. P >=ONE) THEN
        WRITE (*,*) 'IMPERMISSIBLE DISTRIBUTION PARAMETER VALUES'
        STOP
    END IF
 
    Q = ONE - P
    X = ZERO
    ST = SK
    IF (P > H) THEN
        V = ONE / LOG (P)
        K = ST
        DO I = 1, K
            DO
                CALL RANDOM_NUMBER (R)
                IF (R > ZERO) EXIT
            END DO
            N = V * LOG (R)
            X = X + N
        END DO
        ST = ST - K
    END IF
 
    S = ZERO
    ULN = - LOG (VSMALL)
    IF (ST >-ULN/LOG(Q)) THEN
        WRITE (*,*) ' P IS TOO LARGE FOR THIS VALUE OF SK'
        STOP
    END IF
 
    Y = Q ** ST
    G = ST
    CALL RANDOM_NUMBER (R)
    DO
        IF (Y > R) EXIT
        R = R - Y
        S = S + ONE
        Y = Y * P * G / S
        G = G + ONE
    END DO
 
    IVAL = X + S + HALF
    RETURN
END FUNCTION RANDOM_NEG_BINOMIAL
 
!******************************************************************************
 
FUNCTION RANDOM_VON_MISES (K, FIRST) RESULT (FN_VAL)
 
!     Algorithm VMD from:
!     Dagpunar, J.S. (1990) `Sampling from the von Mises distribution via a
!     comparison of random numbers', J. of Appl. Statist., 17, 165-168.
 
!     Fortran 90 code by Alan Miller
!     CSIRO Division of Mathematical & Information Sciences
 
!     Arguments:
!     k (real)        parameter of the von Mises distribution.
!     first (logical) set to .TRUE. the first time that the function
!                     is called, or the first time with a new value
!                     for k.   When first = .TRUE., the function sets
!                     up starting values and may be very much slower.
 
    REAL, INTENT (IN) :: K
    LOGICAL, INTENT (IN) :: FIRST
    REAL :: FN_VAL
 
!     Local variables
 
    INTEGER :: J, N
    INTEGER, SAVE :: NK
    REAL, PARAMETER :: PI = 3.14159265
    REAL, SAVE :: P (20), THETA (0:20)
    REAL :: SUMP, R, TH, LAMBDA, RLAST
    REAL (DP) :: DK
 
    IF (FIRST) THEN ! Initialization, if necessary
        IF (K < ZERO) THEN
            WRITE (*,*) '** Error: argument k for random_von_Mises = ', K
            RETURN
        END IF
 
        NK = K + K + ONE
        IF (NK > 20) THEN
            WRITE (*,*) '** Error: argument k for random_von_Mises = ', K
            RETURN
        END IF
 
        DK = K
        THETA (0) = ZERO
        IF (K > HALF) THEN
 
!     Set up array p of probabilities.
 
            SUMP = ZERO
            DO J = 1, NK
                IF (J < NK) THEN
                    THETA (J) = ACOS (ONE-J/K)
                ELSE
                    THETA (NK) = PI
                END IF
 
!     Numerical integration of e^[k.cos(x)] from theta(j-1) to theta(j)
 
                CALL INTEGRAL (THETA(J-1), THETA(J), P(J), DK)
                SUMP = SUMP + P (J)
            END DO
            P (1:NK) = P (1:NK) / SUMP
        ELSE
            P (1) = ONE
            THETA (1) = PI
        END IF ! if k > 0.5
    END IF ! if first
 
    CALL RANDOM_NUMBER (R)
    DO J = 1, NK
        R = R - P (J)
        IF (R < ZERO) EXIT
    END DO
    R = - R / P (J)
 
    DO
        TH = THETA (J-1) + R * (THETA(J)-THETA(J-1))
        LAMBDA = K - J + ONE - K * COS (TH)
        N = 1
        RLAST = LAMBDA
 
        DO
            CALL RANDOM_NUMBER (R)
            IF (R > RLAST) EXIT
            N = N + 1
            RLAST = R
        END DO
 
        IF (N .NE. 2*(N/2)) EXIT! is n even?
        CALL RANDOM_NUMBER (R)
    END DO
 
    FN_VAL = SIGN (TH, (R-RLAST)/(ONE-RLAST)-HALF)
    RETURN
END FUNCTION RANDOM_VON_MISES
 
!******************************************************************************
 
SUBROUTINE INTEGRAL (A, B, RESULT, DK)
 
!     Gaussian integration of exp(k.cosx) from a to b.
 
    REAL (DP), INTENT (IN) :: DK
    REAL, INTENT (IN) :: A, B
    REAL, INTENT (OUT) :: RESULT
 
!     Local variables
 
    REAL (DP) :: XMID, RANGE, X1, X2, X (3) = (/ 0.238619186083197_DP, &
    & 0.661209386466265_DP, 0.932469514203152_DP /), W (3) = (/ 0.467913934572691_DP, &
    & 0.360761573048139_DP, 0.171324492379170_DP /)
    INTEGER :: I
 
    XMID = (A+B) / 2._DP
    RANGE = (B-A) / 2._DP
 
    RESULT = 0._DP
    DO I = 1, 3
        X1 = XMID + X (I) * RANGE
        X2 = XMID - X (I) * RANGE
        RESULT = RESULT + W (I) * (EXP(DK*COS(X1))+EXP(DK*COS(X2)))
    END DO
 
    RESULT = RESULT * RANGE
    RETURN
END SUBROUTINE INTEGRAL
 
!******************************************************************************
 
FUNCTION RANDOM_CAUCHY () RESULT (FN_VAL)
 
!     Generate a random deviate from the standard Cauchy distribution
 
    REAL :: FN_VAL
 
!     Local variables
    REAL :: V (2)
 
    DO
        CALL RANDOM_NUMBER (V)
        V = TWO * (V-HALF)
        IF (ABS(V(2)) < VSMALL) CYCLE! Test for zero
        IF (V(1)**2+V(2)**2 < ONE) EXIT
    END DO
    FN_VAL = V (1) / V (2)
 
    RETURN
END FUNCTION RANDOM_CAUCHY
 
!******************************************************************************
 
SUBROUTINE RANDOM_ORDER (ORDER, N)
 
!     Generate a random ordering of the integers 1 ... n.
 
    INTEGER, INTENT (IN) :: N
    INTEGER, INTENT (OUT) :: ORDER (N)
 
!     Local variables
 
    INTEGER :: I, J, K
    REAL :: WK
 
    DO I = 1, N
        ORDER (I) = I
    END DO
 
!     Starting at the end, swap the current last indicator with one
!     randomly chosen from those preceeding it.
 
    DO I = N, 2, - 1
        CALL RANDOM_NUMBER (WK)
        J = 1 + I * WK
        IF (J < I) THEN
            K = ORDER (I)
            ORDER (I) = ORDER (J)
            ORDER (J) = K
        END IF
    END DO
 
    RETURN
END SUBROUTINE RANDOM_ORDER
 
!******************************************************************************
 
SUBROUTINE SEED_RANDOM_NUMBER (IOUNIT)
 
    INTEGER, INTENT (IN) :: IOUNIT
 
! Local variables
 
    INTEGER :: K
    INTEGER, ALLOCATABLE :: SEED (:)
 
    CALL RANDOM_SEED (SIZE=K)
    ALLOCATE (SEED(K))
 
    WRITE (*, '(a, i2, a)') ' Enter ', K, ' integers for random no. seeds: '
    READ (*,*) SEED
    WRITE (IOUNIT, '(a, (7i10))') ' Random no. seeds: ', SEED
    CALL RANDOM_SEED (PUT=SEED)
 
    DEALLOCATE (SEED)
 
    RETURN
END SUBROUTINE SEED_RANDOM_NUMBER
 
!******************************************************************************

END MODULE ModLib_Random

!******************************************************************************

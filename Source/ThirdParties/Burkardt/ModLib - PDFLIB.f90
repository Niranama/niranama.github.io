
MODULE ModLib_PDFLib
       
!** PURPOSE OF THIS MODULE:
    ! contains routines that handles various discrete and continuous probability
    ! density functions ("PDF's").  
    ! For a discrete variable X, PDF(X) is the probability that the value X will occur;
    ! for a continuous variable, PDF(X) is the probability density of X, that is, the
    ! probability of a value between X and X+dX is PDF(X) * dX.
    ! The corresponding cumulative density functions or "CDF"'s are also handled.
    ! For a discrete or continuous variable, CDF(X) is the probability that the variable
    ! takes on a value less than or equal to X.

!** REFERENCES:
    ! These routines are from PDFLIB package by John Burkardt

!** USE STATEMENTS:
    USE ModLib_RanLib,  ONLY: R8_UNI_01

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_PDFLib'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
 
FUNCTION I4_BINOMIAL_PDF (N, P, K)
 
!*****************************************************************************80
!
!! I4_BINOMIAL_PDF evaluates the binomial PDF.
!
!  Discussion:
!
!    pdf(n,p,k) = C(n,k) p^k (1-p)^(n-k)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 January 2018
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials.
!    0 < N.
!
!    Input, real ( kind = 8 ) P, the probability of a success in one trial.
!
!    Input, integer ( kind = 4 ) K, the number of successes.
!
!    Output, real ( kind = 8 ) I4_BINOMIAL_PDF, the probability of K successes
!    in N trials with a per-trial success probability of P.
!
    IMPLICIT NONE
 
    REAL (KIND=8) I4_BINOMIAL_PDF
    INTEGER (KIND=4) K
    INTEGER (KIND=4) N
    REAL (KIND=8) P
    REAL (KIND=8) VALUE
 
    IF (K < 0) THEN
        VALUE = 0.0D+00
    ELSE IF (K <= N) THEN
        VALUE = R8_CHOOSE (N, K) * P ** K * (1.0D+00-P) ** (N-K)
    ELSE
        VALUE = 0.0D+00
    END IF
 
    I4_BINOMIAL_PDF = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_BINOMIAL_SAMPLE (N, PP)
 
!*****************************************************************************80
!
!! I4_BINOMIAL_SAMPLE generates a binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a binomial
!    distribution whose number of trials is N and whose
!    probability of an event in each trial is P.
!
!    The previous version of this program relied on the assumption that
!    local memory would be preserved between calls.  It set up data
!    one time to be preserved for use over multiple calls.  In the
!    interests of portability, this assumption has been removed, and
!    the "setup" data is recomputed on every call.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Voratas Kachitvichyanukul, Bruce Schmeiser,
!    Binomial Random Variate Generation,
!    Communications of the ACM,
!    Volume 31, Number 2, February 1988, pages 216-222.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of binomial trials, from which a
!    random deviate will be generated.
!    0 < N.
!
!    Input, real ( kind = 8 ) PP, the probability of an event in each trial of
!    the binomial distribution from which a random deviate is to be generated.
!    0.0 < PP < 1.0.
!
!    Output, integer ( kind = 4 ) I4_BINOMIAL_SAMPLE, a random deviate from the
!    distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) AL
    REAL (KIND=8) ALV
    REAL (KIND=8) AMAXP
    REAL (KIND=8) C
    REAL (KIND=8) F
    REAL (KIND=8) F1
    REAL (KIND=8) F2
    REAL (KIND=8) FFM
    REAL (KIND=8) FM
    REAL (KIND=8) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) I4_BINOMIAL_SAMPLE
    INTEGER (KIND=4) IX
    INTEGER (KIND=4) IX1
    INTEGER (KIND=4) K
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MP
    REAL (KIND=8) PP
    INTEGER (KIND=4) N
    REAL (KIND=8) P
    REAL (KIND=8) P1
    REAL (KIND=8) P2
    REAL (KIND=8) P3
    REAL (KIND=8) P4
    REAL (KIND=8) Q
    REAL (KIND=8) QN
    REAL (KIND=8) R
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) W2
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) XL
    REAL (KIND=8) XLL
    REAL (KIND=8) XLR
    REAL (KIND=8) XM
    REAL (KIND=8) XNP
    REAL (KIND=8) XNPQ
    REAL (KIND=8) XR
    REAL (KIND=8) YNORM
    REAL (KIND=8) Z
    REAL (KIND=8) Z2
 
    IF (PP <= 0.0D+00 .OR. 1.0D+00 <= PP) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_BINOMIAL_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  PP is out of range.'
        RETURN
    END IF
 
    P = MIN (PP, 1.0D+00-PP)
    Q = 1.0D+00 - P
    XNP = REAL (N, KIND=8) * P
 
    IF (XNP < 30.0D+00) THEN
 
        QN = Q ** N
        R = P / Q
        G = R * REAL (N+1, KIND=8)
 
        DO
 
            IX = 0
            F = QN
            U = R8_UNIFORM_01_SAMPLE ()
 
            DO
 
                IF (U < F) THEN
                    IF (0.5D+00 < PP) THEN
                        IX = N - IX
                    END IF
                    I4_BINOMIAL_SAMPLE = IX
                    RETURN
                END IF
 
                IF (110 < IX) THEN
                    EXIT
                END IF
 
                U = U - F
                IX = IX + 1
                F = F * (G/REAL(IX, KIND=8)-R)
 
            END DO
 
        END DO
 
    END IF
 
    FFM = XNP + P
    M = FFM
    FM = M
    XNPQ = XNP * Q
    P1 = INT (2.195D+00*SQRT(XNPQ)-4.6D+00*Q) + 0.5D+00
    XM = FM + 0.5D+00
    XL = XM - P1
    XR = XM + P1
    C = 0.134D+00 + 20.5D+00 / (15.3D+00+FM)
    AL = (FFM-XL) / (FFM-XL*P)
    XLL = AL * (1.0D+00+0.5D+00*AL)
    AL = (XR-FFM) / (XR*Q)
    XLR = AL * (1.0D+00+0.5D+00*AL)
    P2 = P1 * (1.0D+00+C+C)
    P3 = P2 + C / XLL
    P4 = P3 + C / XLR
!
!  Generate a variate.
!
    DO
 
        U = R8_UNIFORM_01_SAMPLE () * P4
        V = R8_UNIFORM_01_SAMPLE ()
!
!  Triangle
!
        IF (U < P1) THEN
            IX = XM - P1 * V + U
            IF (0.5D+00 < PP) THEN
                IX = N - IX
            END IF
            I4_BINOMIAL_SAMPLE = IX
            RETURN
        END IF
!
!  Parallelogram
!
        IF (U <= P2) THEN
 
            X = XL + (U-P1) / C
            V = V * C + 1.0D+00 - ABS (XM-X) / P1
 
            IF (V <= 0.0D+00 .OR. 1.0D+00 < V) THEN
                CYCLE
            END IF
 
            IX = X
 
        ELSE IF (U <= P3) THEN
 
            IX = XL + LOG (V) / XLL
            IF (IX < 0) THEN
                CYCLE
            END IF
            V = V * (U-P2) * XLL
 
        ELSE
 
            IX = XR - LOG (V) / XLR
            IF (N < IX) THEN
                CYCLE
            END IF
            V = V * (U-P3) * XLR
 
        END IF
 
        K = ABS (IX-M)
 
        IF (K <= 20 .OR. XNPQ/2.0D+00-1.0D+00 <= K) THEN
 
            F = 1.0D+00
            R = P / Q
            G = REAL (N+1, KIND=8) * R
 
            IF (M < IX) THEN
                MP = M + 1
                DO I = M + 1, IX
                    F = F * (G/I-R)
                END DO
            ELSE IF (IX < M) THEN
                IX1 = IX + 1
                DO I = IX + 1, M
                    F = F / (G/REAL(I, KIND=8)-R)
                END DO
            END IF
 
            IF (V <= F) THEN
                IF (0.5D+00 < PP) THEN
                    IX = N - IX
                END IF
                I4_BINOMIAL_SAMPLE = IX
                RETURN
            END IF
 
        ELSE
 
            AMAXP = (K/XNPQ) * ((K*(K/3.0D+00+0.625D+00)+0.1666666666666D+00)/XNPQ+0.5D+00)
            YNORM = - REAL (K*K, KIND=8) / (2.0D+00*XNPQ)
            ALV = LOG (V)
 
            IF (ALV < YNORM-AMAXP) THEN
                IF (0.5D+00 < PP) THEN
                    IX = N - IX
                END IF
                I4_BINOMIAL_SAMPLE = IX
                RETURN
            END IF
 
            IF (YNORM+AMAXP < ALV) THEN
                CYCLE
            END IF
 
            X1 = REAL (IX+1, KIND=8)
            F1 = FM + 1.0D+00
            Z = REAL (N+1, KIND=8) - FM
            W = REAL (N-IX+1, KIND=8)
            Z2 = Z * Z
            X2 = X1 * X1
            F2 = F1 * F1
            W2 = W * W
 
            T = XM * LOG (F1/X1) + (N-M+0.5D+00) * LOG (Z/W) + REAL (IX-M, KIND=8) * LOG &
           & (W*P/(X1*Q)) + &
           & (13860.0D+00-(462.0D+00-(132.0D+00-(99.0D+00-140.0D+00/F2)/F2)/F2)/F2) / F1 / &
           & 166320.0D+00 + &
           & (13860.0D+00-(462.0D+00-(132.0D+00-(99.0D+00-140.0D+00/Z2)/Z2)/Z2)/Z2) / Z / &
           & 166320.0D+00 + &
           & (13860.0D+00-(462.0D+00-(132.0D+00-(99.0D+00-140.0D+00/X2)/X2)/X2)/X2) / X1 / &
           & 166320.0D+00 + &
           & (13860.0D+00-(462.0D+00-(132.0D+00-(99.0D+00-140.0D+00/W2)/W2)/W2)/W2) / W / &
           & 166320.0D+00
 
            IF (ALV <= T) THEN
                IF (0.5D+00 < PP) THEN
                    IX = N - IX
                END IF
                I4_BINOMIAL_SAMPLE = IX
                RETURN
            END IF
 
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_UNIFORM_SAMPLE (A, B)
 
!*****************************************************************************80
!
!! I4_UNIFORM_SAMPLE returns a scaled pseudorandom I4 between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) A2
    INTEGER (KIND=4) B
    INTEGER (KIND=4) B2
    INTEGER (KIND=4) I4_UNIFORM_SAMPLE
    REAL (KIND=8) U
    INTEGER (KIND=4) VALUE
!
!  We prefer A < B.
!
    A2 = MIN (A, B)
    B2 = MAX (A, B)
 
    U = R8_UNIFORM_01_SAMPLE ()
!
!  Scale to [A2-0.5,B2+0.5].
!
    U = (1.0D+00-U) * (REAL(A2, KIND=8)-0.5D+00) + U * (REAL(B2, KIND=8)+0.5D+00)
!
!  Round.
!
    VALUE = NINT (U)
!
!  Enforce limits.
!
    VALUE = MAX (VALUE, A2)
    VALUE = MIN (VALUE, B2)
 
    I4_UNIFORM_SAMPLE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4VEC_MULTINOMIAL_PDF (N, P, M, X)
 
!*****************************************************************************80
!
!! I4VEC_MULTINOMIAL_PDF evaluates the multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of trials.
!
!    Input, real ( kind = 8 ) P(M), the probability of each outcome
!    on any single trial.
!
!    Input, integer ( kind = 4 ) M, the number of possible outcomes
!    of a single trial.
!
!    Input, integer ( kind = 4 ) X(M), the results of N trials,
!    with X(I) the number of times outcome I occurred.
!
!    Output, real ( kind = 8 ) I4VEC_MULTINOMIAL_PDF, the probability
!    density function evaluated at X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) BOT
    INTEGER (KIND=4) C
    INTEGER (KIND=4) I
    REAL (KIND=8) I4VEC_MULTINOMIAL_PDF
    INTEGER (KIND=4) J
    REAL (KIND=8) P (M)
    REAL (KIND=8) PDF
    INTEGER (KIND=4) TOP
    INTEGER (KIND=4) X (M)
!
!  The combinatorial coefficient is an integer.
!
    C = 1
    TOP = N
    DO I = 1, M
        BOT = 1
        DO J = 1, X (I)
            C = (C*TOP) / BOT
            TOP = TOP - 1
            BOT = BOT + 1
        END DO
    END DO
 
    PDF = REAL (C, KIND=8)
    DO I = 1, M
        PDF = PDF * P (I) ** X (I)
    END DO
 
    I4VEC_MULTINOMIAL_PDF = PDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_MULTINOMIAL_SAMPLE (N, P, NCAT, IX)
 
!*****************************************************************************80
!
!! I4VEC_MULTINOMIAL_SAMPLE generates a multinomial random deviate.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer, 1986,
!    ISBN: 0387963057,
!    LC: QA274.D48.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of trials.
!
!    Input, real ( kind = 8 ) P(NCAT).  P(I) is the probability that an event
!    will be classified into category I.  Thus, each P(I) must be between
!    0.0 and 1.0, and the P's must sum to 1.
!
!    Input, integer ( kind = 4 ) NCAT, the number of possible outcomes
!    of a single trial.
!
!    Output, integer ( kind = 4 ) IX(NCAT), a random observation from
!    the multinomial distribution.  All IX(i) will be nonnegative and their
!    sum will be N.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
    INTEGER (KIND=4) NCAT
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICAT
    INTEGER (KIND=4) IX (NCAT)
    INTEGER (KIND=4) NTOT
    REAL (KIND=8) P (NCAT)
    REAL (KIND=8) PROB
    REAL (KIND=8) PTOT
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  N < 0'
        RETURN
    END IF
 
    IF (NCAT <= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  NCAT <= 1'
        RETURN
    END IF
 
    DO I = 1, NCAT
 
        IF (P(I) < 0.0D+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
            WRITE (*, '(a)') '  Some P(i) < 0.'
            RETURN
        END IF
 
        IF (1.0D+00 < P(I)) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'I4VEC_MULTINOMIAL_SAMPLE - Fatal error!'
            WRITE (*, '(a)') '  Some 1 < P(i).'
            RETURN
        END IF
 
    END DO
!
!  Initialize variables.
!
    NTOT = N
    PTOT = 1.0D+00
    DO I = 1, NCAT
        IX (I) = 0
    END DO
!
!  Generate the observation.
!
    DO ICAT = 1, NCAT - 1
        PROB = P (ICAT) / PTOT
        IX (ICAT) = I4_BINOMIAL_SAMPLE (NTOT, PROB)
        NTOT = NTOT - IX (ICAT)
        IF (NTOT <= 0) THEN
            RETURN
        END IF
        PTOT = PTOT - P (ICAT)
    END DO
 
    IX (NCAT) = NTOT
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_BETA_PDF (ALPHA, BETA, RVAL)
 
!*****************************************************************************80
!
!! R8_BETA_PDF evaluates the PDF of a beta distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2015
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    This version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, BETA, shape parameters.
!    0.0 < ALPHA, BETA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_BETA_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) BETA
    REAL (KIND=8) R8_BETA_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter ALPHA is not positive.'
        RETURN
    END IF
 
    IF (BETA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter BETA is not positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00 .OR. 1.0D+00 <= RVAL) THEN
 
        R8_BETA_PDF = 0.0D+00
 
    ELSE
 
        TEMP = R8_GAMMA_LOG (ALPHA+BETA) - R8_GAMMA_LOG (ALPHA) - R8_GAMMA_LOG (BETA)
 
        R8_BETA_PDF = EXP (TEMP) * RVAL ** (ALPHA-1.0D+00) * (1.0D+00-RVAL) ** (BETA-1.0D+00)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_BETA_SAMPLE (AA, BB)
 
!*****************************************************************************80
!
!! R8_BETA_SAMPLE generates a beta random deviate.
!
!  Discussion:
!
!    This procedure returns a single random deviate from the beta distribution
!    with parameters A and B.  The density is
!
!      x^(a-1) * (1-x)^(b-1) / Beta(a,b) for 0 < x < 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Russell Cheng,
!    Generating Beta Variates with Nonintegral Shape Parameters,
!    Communications of the ACM,
!    Volume 21, Number 4, April 1978, pages 317-322.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AA, the first parameter of the beta distribution.
!    0.0 < AA.
!
!    Input, real ( kind = 8 ) BB, the second parameter of the beta distribution.
!    0.0 < BB.
!
!    Output, real ( kind = 8 ) R8_BETA_SAMPLE, a beta random variate.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) AA
    REAL (KIND=8) ALPHA
    REAL (KIND=8) B
    REAL (KIND=8) BB
    REAL (KIND=8) BETA
    REAL (KIND=8) DELTA
    REAL (KIND=8) GAMMA
    REAL (KIND=8) K1
    REAL (KIND=8) K2
    REAL (KIND=8), PARAMETER :: LOG4 = 1.3862943611198906188D+00
    REAL (KIND=8), PARAMETER :: LOG5 = 1.6094379124341003746D+00
    REAL (KIND=8) R
    REAL (KIND=8) R8_BETA_SAMPLE
    REAL (KIND=8) S
    REAL (KIND=8) T
    REAL (KIND=8) U1
    REAL (KIND=8) U2
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    IF (AA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  AA <= 0.0'
        RETURN
    END IF
 
    IF (BB <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  BB <= 0.0'
        RETURN
    END IF
!
!  Algorithm BB
!
    IF (1.0D+00 < AA .AND. 1.0D+00 < BB) THEN
 
        A = MIN (AA, BB)
        B = MAX (AA, BB)
        ALPHA = A + B
        BETA = SQRT ((ALPHA-2.0D+00)/(2.0D+00*A*B-ALPHA))
        GAMMA = A + 1.0D+00 / BETA
 
        DO
 
            U1 = R8_UNIFORM_01_SAMPLE ()
            U2 = R8_UNIFORM_01_SAMPLE ()
            V = BETA * LOG (U1/(1.0D+00-U1))
            W = A * EXP (V)
 
            Z = U1 ** 2 * U2
            R = GAMMA * V - LOG4
            S = A + R - W
 
            IF (5.0D+00*Z <= S+1.0D+00+LOG5) THEN
                EXIT
            END IF
 
            T = LOG (Z)
            IF (T <= S) THEN
                EXIT
            END IF
 
            IF (T <= (R+ALPHA*LOG(ALPHA/(B+W)))) THEN
                EXIT
            END IF
 
        END DO
!
!  Algorithm BC
!
    ELSE
 
        A = MAX (AA, BB)
        B = MIN (AA, BB)
        ALPHA = A + B
        BETA = 1.0D+00 / B
        DELTA = 1.0D+00 + A - B
        K1 = DELTA * (1.0D+00/72.0D+00+B/24.0D+00) / (A/B-7.0D+00/9.0D+00)
        K2 = 0.25D+00 + (0.5D+00+0.25D+00/DELTA) * B
 
        DO
 
            U1 = R8_UNIFORM_01_SAMPLE ()
            U2 = R8_UNIFORM_01_SAMPLE ()
 
            IF (U1 < 0.5D+00) THEN
 
                Y = U1 * U2
                Z = U1 * Y
 
                IF (K1 <= 0.25D+00*U2+Z-Y) THEN
                    CYCLE
                END IF
 
            ELSE
 
                Z = U1 ** 2 * U2
 
                IF (Z <= 0.25D+00) THEN
 
                    V = BETA * LOG (U1/(1.0D+00-U1))
                    W = A * EXP (V)
 
                    IF (AA == A) THEN
                        R8_BETA_SAMPLE = W / (B+W)
                    ELSE
                        R8_BETA_SAMPLE = B / (B+W)
                    END IF
 
                    RETURN
 
                END IF
 
                IF (K2 < Z) THEN
                    CYCLE
                END IF
 
            END IF
 
            V = BETA * LOG (U1/(1.0D+00-U1))
            W = A * EXP (V)
 
            IF (LOG(Z) <= ALPHA*(LOG(ALPHA/(B+W))+V)-LOG4) THEN
                EXIT
            END IF
 
        END DO
 
    END IF
 
    IF (AA == A) THEN
        R8_BETA_SAMPLE = W / (B+W)
    ELSE
        R8_BETA_SAMPLE = B / (B+W)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CHI_PDF (DF, RVAL)
 
!*****************************************************************************80
!
!! R8_CHI_PDF evaluates the PDF of a chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_CHI_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) DF
    REAL (KIND=8) R8_CHI_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP1
    REAL (KIND=8) TEMP2
 
    IF (DF <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_CHI_PDF - Fatal error!'
        WRITE (*, '(a)') '  Degrees of freedom must be positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_CHI_PDF = 0.0D+00
 
    ELSE
 
        TEMP2 = DF * 0.5D+00
 
        TEMP1 = (TEMP2-1.0D+00) * LOG (RVAL) - 0.5D+00 * RVAL - TEMP2 * LOG (2.0D+00) - &
       & R8_GAMMA_LOG (TEMP2)
 
        R8_CHI_PDF = EXP (TEMP1)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CHI_SAMPLE (DF)
 
!*****************************************************************************80
!
!! R8_CHI_SAMPLE generates a Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the chi square distribution
!    with DF degrees of freedom random variable.
!
!    The algorithm exploits the relation between chisquare and gamma.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 8 ) R8_CHI_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ARG1
    REAL (KIND=8) ARG2
    REAL (KIND=8) DF
    REAL (KIND=8) R8_CHI_SAMPLE
 
    IF (DF <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_CHI_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  DF <= 0.'
        WRITE (*, '(a,g14.6)') '  Value of DF: ', DF
        RETURN
    END IF
 
    ARG1 = 1.0D+00
    ARG2 = DF / 2.0D+00
 
    R8_CHI_SAMPLE = 2.0D+00 * R8_GAMMA_SAMPLE (ARG1, ARG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CHOOSE (N, K)
 
!*****************************************************************************80
!
!! R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in R8 arithmetic.
!
!    The formula used is:
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2008
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    ML Wolfson, HV Wright,
!    Algorithm 160:
!    Combinatorial of M Things Taken N at a Time,
!    Communications of the ACM,
!    Volume 6, Number 4, April 1963, page 161.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) R8_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) K
    INTEGER (KIND=4) MN
    INTEGER (KIND=4) MX
    INTEGER (KIND=4) N
    REAL (KIND=8) R8_CHOOSE
    REAL (KIND=8) VALUE
 
    MN = MIN (K, N-K)
 
    IF (MN < 0) THEN
 
        VALUE = 0.0D+00
 
    ELSE IF (MN == 0) THEN
 
        VALUE = 1.0D+00
 
    ELSE
 
        MX = MAX (K, N-K)
        VALUE = REAL (MX+1, KIND=8)
 
        DO I = 2, MN
            VALUE = (VALUE*REAL(MX+I, KIND=8)) / REAL (I, KIND=8)
        END DO
 
    END IF
 
    R8_CHOOSE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_EXPONENTIAL_PDF (BETA, RVAL)
 
!*****************************************************************************80
!
!! R8_EXPONENTIAL_PDF evaluates the PDF of an exponential distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the scale value.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) BETA
    REAL (KIND=8) R8_EXPONENTIAL_PDF
    REAL (KIND=8) RVAL
 
    IF (BETA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_EXPONENTIAL_PDF - Fatal error!'
        WRITE (*, '(a)') '  BETA parameter must be positive.'
        RETURN
    END IF
 
    IF (RVAL < 0.0D+00) THEN
        R8_EXPONENTIAL_PDF = 0.0D+00
    ELSE
        R8_EXPONENTIAL_PDF = EXP (-RVAL/BETA) / BETA
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_EXPONENTIAL_SAMPLE (LAMBDA)
 
!*****************************************************************************80
!
!! R8_EXPONENTIAL_SAMPLE samples the exponential PDF.
!
!  Discussion:
!
!    Note that the parameter LAMBDA is a multiplier.  In some formulations,
!    it is used as a divisor instead.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) LAMBDA
    REAL (KIND=8) R
    REAL (KIND=8) R8_EXPONENTIAL_SAMPLE
 
    R = R8_UNIFORM_01_SAMPLE ()
 
    R8_EXPONENTIAL_SAMPLE = - LOG (R) * LAMBDA
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_EXPONENTIAL_01_PDF (RVAL)
 
!*****************************************************************************80
!
!! R8_EXPONENTIAL_01_PDF: PDF of a standard exponential distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_01_PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R8_EXPONENTIAL_01_PDF
    REAL (KIND=8) RVAL
 
    IF (RVAL < 0.0D+00) THEN
        R8_EXPONENTIAL_01_PDF = 0.0D+00
    ELSE
        R8_EXPONENTIAL_01_PDF = EXP (-RVAL)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_EXPONENTIAL_01_SAMPLE ()
 
!*****************************************************************************80
!
!! R8_EXPONENTIAL_01_SAMPLE samples the standard exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_EXPONENTIAL_01_SAMPLE, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R
    REAL (KIND=8) R8_EXPONENTIAL_01_SAMPLE
 
    R = R8_UNIFORM_01_SAMPLE ()
 
    R8_EXPONENTIAL_01_SAMPLE = - LOG (R)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_LOG (X)
 
!*****************************************************************************80
!
!! R8_GAMMA_LOG evaluates the logarithm of the gamma function.
!
!  Discussion:
!
!    This routine calculates the LOG(GAMMA) function for a positive real
!    argument X.  Computation is based on an algorithm outlined in
!    references 1 and 2.  The program uses rational functions that
!    theoretically approximate LOG(GAMMA) to at least 18 significant
!    decimal digits.  The approximation for X > 12 is from reference
!    3, while approximations for X < 12.0 are similar to those in
!    reference 1, but are unpublished.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the
!    Gamma Function,
!    Mathematics of Computation,
!    Volume 21, Number 98, April 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thatcher,
!    Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968,
!    LC: QA297.C64.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!    0.0 < X.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8), DIMENSION (7) :: C = (/ - 1.910444077728D-03, 8.4171387781295D-04, - &
   & 5.952379913043012D-04, 7.93650793500350248D-04, - 2.777777777777681622553D-03, &
   & 8.333333333333333331554247D-02, 5.7083835261D-03 /)
    REAL (KIND=8) CORR
    REAL (KIND=8) :: D1 = - 5.772156649015328605195174D-01
    REAL (KIND=8) :: D2 = 4.227843350984671393993777D-01
    REAL (KIND=8) :: D4 = 1.791759469228055000094023D+00
    REAL (KIND=8), PARAMETER :: FRTBIG = 2.25D+76
    INTEGER (KIND=4) I
    REAL (KIND=8), DIMENSION (8) :: P1 = (/ 4.945235359296727046734888D+00, &
   & 2.018112620856775083915565D+02, 2.290838373831346393026739D+03, &
   & 1.131967205903380828685045D+04, 2.855724635671635335736389D+04, &
   & 3.848496228443793359990269D+04, 2.637748787624195437963534D+04, &
   & 7.225813979700288197698961D+03 /)
    REAL (KIND=8), DIMENSION (8) :: P2 = (/ 4.974607845568932035012064D+00, &
   & 5.424138599891070494101986D+02, 1.550693864978364947665077D+04, &
   & 1.847932904445632425417223D+05, 1.088204769468828767498470D+06, &
   & 3.338152967987029735917223D+06, 5.106661678927352456275255D+06, &
   & 3.074109054850539556250927D+06 /)
    REAL (KIND=8), DIMENSION (8) :: P4 = (/ 1.474502166059939948905062D+04, &
   & 2.426813369486704502836312D+06, 1.214755574045093227939592D+08, &
   & 2.663432449630976949898078D+09, 2.940378956634553899906876D+10, &
   & 1.702665737765398868392998D+11, 4.926125793377430887588120D+11, &
   & 5.606251856223951465078242D+11 /)
    REAL (KIND=8), DIMENSION (8) :: Q1 = (/ 6.748212550303777196073036D+01, &
   & 1.113332393857199323513008D+03, 7.738757056935398733233834D+03, &
   & 2.763987074403340708898585D+04, 5.499310206226157329794414D+04, &
   & 6.161122180066002127833352D+04, 3.635127591501940507276287D+04, &
   & 8.785536302431013170870835D+03 /)
    REAL (KIND=8), DIMENSION (8) :: Q2 = (/ 1.830328399370592604055942D+02, &
   & 7.765049321445005871323047D+03, 1.331903827966074194402448D+05, &
   & 1.136705821321969608938755D+06, 5.267964117437946917577538D+06, &
   & 1.346701454311101692290052D+07, 1.782736530353274213975932D+07, &
   & 9.533095591844353613395747D+06 /)
    REAL (KIND=8), DIMENSION (8) :: Q4 = (/ 2.690530175870899333379843D+03, &
   & 6.393885654300092398984238D+05, 4.135599930241388052042842D+07, &
   & 1.120872109616147941376570D+09, 1.488613728678813811542398D+10, &
   & 1.016803586272438228077304D+11, 3.417476345507377132798597D+11, &
   & 4.463158187419713286462081D+11 /)
    REAL (KIND=8) R8_GAMMA_LOG
    REAL (KIND=8) RES
    REAL (KIND=8), PARAMETER :: SQRTPI = 0.9189385332046727417803297D+00
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XBIG = 2.55D+305
    REAL (KIND=8) XDEN
    REAL (KIND=8), PARAMETER :: XINF = 1.79D+308
    REAL (KIND=8) XM1
    REAL (KIND=8) XM2
    REAL (KIND=8) XM4
    REAL (KIND=8) XNUM
    REAL (KIND=8) Y
    REAL (KIND=8) YSQ
 
    Y = X
 
    IF (0.0D+00 < Y .AND. Y <= XBIG) THEN
 
        IF (Y <= EPSILON(Y)) THEN
 
            RES = - LOG (Y)
!
!  EPS < X <= 1.5.
!
        ELSE IF (Y <= 1.5D+00) THEN
 
            IF (Y < 0.6796875D+00) THEN
                CORR = - LOG (Y)
                XM1 = Y
            ELSE
                CORR = 0.0D+00
                XM1 = (Y-0.5D+00) - 0.5D+00
            END IF
 
            IF (Y <= 0.5D+00 .OR. 0.6796875D+00 <= Y) THEN
 
                XDEN = 1.0D+00
                XNUM = 0.0D+00
                DO I = 1, 8
                    XNUM = XNUM * XM1 + P1 (I)
                    XDEN = XDEN * XM1 + Q1 (I)
                END DO
 
                RES = CORR + (XM1*(D1+XM1*(XNUM/XDEN)))
 
            ELSE
 
                XM2 = (Y-0.5D+00) - 0.5D+00
                XDEN = 1.0D+00
                XNUM = 0.0D+00
                DO I = 1, 8
                    XNUM = XNUM * XM2 + P2 (I)
                    XDEN = XDEN * XM2 + Q2 (I)
                END DO
 
                RES = CORR + XM2 * (D2+XM2*(XNUM/XDEN))
 
            END IF
!
!  1.5 < X <= 4.0.
!
        ELSE IF (Y <= 4.0D+00) THEN
 
            XM2 = Y - 2.0D+00
            XDEN = 1.0D+00
            XNUM = 0.0D+00
            DO I = 1, 8
                XNUM = XNUM * XM2 + P2 (I)
                XDEN = XDEN * XM2 + Q2 (I)
            END DO
 
            RES = XM2 * (D2+XM2*(XNUM/XDEN))
!
!  4.0 < X <= 12.0.
!
        ELSE IF (Y <= 12.0D+00) THEN
 
            XM4 = Y - 4.0D+00
            XDEN = - 1.0D+00
            XNUM = 0.0D+00
            DO I = 1, 8
                XNUM = XNUM * XM4 + P4 (I)
                XDEN = XDEN * XM4 + Q4 (I)
            END DO
 
            RES = D4 + XM4 * (XNUM/XDEN)
!
!  Evaluate for 12 <= argument.
!
        ELSE
 
            RES = 0.0D+00
 
            IF (Y <= FRTBIG) THEN
 
                RES = C (7)
                YSQ = Y * Y
 
                DO I = 1, 6
                    RES = RES / YSQ + C (I)
                END DO
 
            END IF
 
            RES = RES / Y
            CORR = LOG (Y)
            RES = RES + SQRTPI - 0.5D+00 * CORR
            RES = RES + Y * (CORR-1.0D+00)
 
        END IF
!
!  Return for bad arguments.
!
    ELSE
 
        RES = XINF
 
    END IF
!
!  Final adjustments and return.
!
    R8_GAMMA_LOG = RES
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_PDF (BETA, ALPHA, RVAL)
 
!*****************************************************************************80
!
!! R8_GAMMA_PDF evaluates the PDF of a gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_GAMMA_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) BETA
    REAL (KIND=8) R8_GAMMA_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_GAMMA_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter ALPHA is not positive.'
        RETURN
    END IF
 
    IF (BETA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_GAMMA_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter BETA is not positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_GAMMA_PDF = 0.0D+00
 
    ELSE
 
        TEMP = ALPHA * LOG (BETA) + (ALPHA-1.0D+00) * LOG (RVAL) - BETA * RVAL - R8_GAMMA_LOG &
       & (ALPHA)
 
        R8_GAMMA_PDF = EXP (TEMP)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_SAMPLE (A, R)
 
!*****************************************************************************80
!
!! R8_GAMMA_SAMPLE generates a Gamma random deviate.
!
!  Discussion:
!
!    This procedure generates random deviates from the gamma distribution whose
!    density is (A^R)/Gamma(R) * X^(R-1) * Exp(-A*X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions,
!    Computing,
!    Volume 12, Number 3, September 1974, pages 223-246.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the rate parameter.
!    A nonzero.
!
!    Input, real ( kind = 8 ) R, the shape parameter.
!    0.0 < R.
!
!    Output, real ( kind = 8 ) R8_GAMMA_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) R
    REAL (KIND=8) R8_GAMMA_SAMPLE
 
    R8_GAMMA_SAMPLE = R8_GAMMA_01_SAMPLE (R) / A
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_01_PDF (ALPHA, RVAL)
 
!*****************************************************************************80
!
!! R8_GAMMA_01_PDF evaluates the PDF of a standard gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_GAMMA_01_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) R8_GAMMA_01_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_GAMMA_01_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter ALPHA is not positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_GAMMA_01_PDF = 0.0D+00
 
    ELSE
 
        TEMP = (ALPHA-1.0D+00) * LOG (RVAL) - RVAL - R8_GAMMA_LOG (ALPHA)
 
        R8_GAMMA_01_PDF = EXP (TEMP)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_01_SAMPLE (A)
 
!*****************************************************************************80
!
!! R8_GAMMA_01_SAMPLE samples the standard Gamma distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm GD in the reference.
!
!    pdf ( a; x ) = 1/gamma(a) * x^(a-1) * exp ( - x )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47-54.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the shape parameter.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) R8_GAMMA_01_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: A1 = 0.3333333D+00
    REAL (KIND=8), PARAMETER :: A2 = - 0.2500030D+00
    REAL (KIND=8), PARAMETER :: A3 = 0.2000062D+00
    REAL (KIND=8), PARAMETER :: A4 = - 0.1662921D+00
    REAL (KIND=8), PARAMETER :: A5 = 0.1423657D+00
    REAL (KIND=8), PARAMETER :: A6 = - 0.1367177D+00
    REAL (KIND=8), PARAMETER :: A7 = 0.1233795D+00
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) D
    REAL (KIND=8) E
    REAL (KIND=8), PARAMETER :: E1 = 1.0D+00
    REAL (KIND=8), PARAMETER :: E2 = 0.4999897D+00
    REAL (KIND=8), PARAMETER :: E3 = 0.1668290D+00
    REAL (KIND=8), PARAMETER :: E4 = 0.0407753D+00
    REAL (KIND=8), PARAMETER :: E5 = 0.0102930D+00
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) Q0
    REAL (KIND=8), PARAMETER :: Q1 = 0.04166669D+00
    REAL (KIND=8), PARAMETER :: Q2 = 0.02083148D+00
    REAL (KIND=8), PARAMETER :: Q3 = 0.00801191D+00
    REAL (KIND=8), PARAMETER :: Q4 = 0.00144121D+00
    REAL (KIND=8), PARAMETER :: Q5 = - 0.00007388D+00
    REAL (KIND=8), PARAMETER :: Q6 = 0.00024511D+00
    REAL (KIND=8), PARAMETER :: Q7 = 0.00024240D+00
    REAL (KIND=8) R
    REAL (KIND=8) R8_GAMMA_01_SAMPLE
    REAL (KIND=8) S
    REAL (KIND=8) S2
    REAL (KIND=8) SI
    REAL (KIND=8), PARAMETER :: SQRT32 = 5.656854D+00
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) X
 
    IF (1.0D+00 <= A) THEN
 
        S2 = A - 0.5D+00
        S = SQRT (S2)
        D = SQRT32 - 12.0D+00 * S
!
!  Immediate acceptance.
!
        T = R8_NORMAL_01_SAMPLE ()
        X = S + 0.5D+00 * T
        R8_GAMMA_01_SAMPLE = X * X
 
        IF (0.0D+00 <= T) THEN
            RETURN
        END IF
!
!  Squeeze acceptance.
!
        U = R8_UNIFORM_01_SAMPLE ()
        IF (D*U <= T*T*T) THEN
            RETURN
        END IF
 
        R = 1.0D+00 / A
        Q0 = ((((((Q7*R+Q6)*R+Q5)*R+Q4)*R+Q3)*R+Q2)*R+Q1) * R
!
!  Approximation depending on size of parameter A.
!
        IF (13.022D+00 < A) THEN
            B = 1.77D+00
            SI = 0.75D+00
            C = 0.1515D+00 / S
        ELSE IF (3.686D+00 < A) THEN
            B = 1.654D+00 + 0.0076D+00 * S2
            SI = 1.68D+00 / S + 0.275D+00
            C = 0.062D+00 / S + 0.024D+00
        ELSE
            B = 0.463D+00 + S + 0.178D+00 * S2
            SI = 1.235D+00
            C = 0.195D+00 / S - 0.079D+00 + 0.16D+00 * S
        END IF
!
!  Quotient test.
!
        IF (0.0D+00 < X) THEN
 
            V = 0.5D+00 * T / S
 
            IF (0.25D+00 < ABS(V)) THEN
                Q = Q0 - S * T + 0.25D+00 * T * T + 2.0D+00 * S2 * LOG (1.0D+00+V)
            ELSE
                Q = Q0 + 0.5D+00 * T * T * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * V
            END IF
 
            IF (LOG(1.0D+00-U) <= Q) THEN
                RETURN
            END IF
 
        END IF
 
        DO
 
            E = R8_EXPONENTIAL_01_SAMPLE ()
            U = 2.0D+00 * R8_UNIFORM_01_SAMPLE () - 1.0D+00
 
            IF (0.0D+00 <= U) THEN
                T = B + ABS (SI*E)
            ELSE
                T = B - ABS (SI*E)
            END IF
!
!  Possible rejection.
!
            IF (T <-0.7187449D+00) THEN
                CYCLE
            END IF
!
!  Calculate V and quotient Q.
!
            V = 0.5D+00 * T / S
 
            IF (0.25D+00 < ABS(V)) THEN
                Q = Q0 - S * T + 0.25D+00 * T * T + 2.0D+00 * S2 * LOG (1.0D+00+V)
            ELSE
                Q = Q0 + 0.5D+00 * T * T * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * V
            END IF
!
!  Hat acceptance.
!
            IF (Q <= 0.0D+00) THEN
                CYCLE
            END IF
 
            IF (0.5D+00 < Q) THEN
                W = EXP (Q) - 1.0D+00
            ELSE
                W = ((((E5*Q+E4)*Q+E3)*Q+E2)*Q+E1) * Q
            END IF
!
!  May have to sample again.
!
            IF (C*ABS(U) <= W*EXP(E-0.5D+00*T*T)) THEN
                EXIT
            END IF
 
        END DO
 
        X = S + 0.5D+00 * T
        R8_GAMMA_01_SAMPLE = X * X
 
        RETURN
!
!  Method for A < 1.
!
    ELSE
 
        B = 1.0D+00 + 0.3678794D+00 * A
 
        DO
 
            P = B * R8_UNIFORM_01_SAMPLE ()
 
            IF (P < 1.0D+00) THEN
 
                R8_GAMMA_01_SAMPLE = EXP (LOG(P)/A)
 
                IF (R8_GAMMA_01_SAMPLE <= R8_EXPONENTIAL_01_SAMPLE()) THEN
                    RETURN
                END IF
 
                CYCLE
 
            END IF
 
            R8_GAMMA_01_SAMPLE = - LOG ((B-P)/A)
 
            IF ((1.0D+00-A)*LOG(R8_GAMMA_01_SAMPLE) <= R8_EXPONENTIAL_01_SAMPLE()) THEN
                EXIT
            END IF
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_INVCHI_PDF (DF, RVAL)
 
!*****************************************************************************80
!
!! R8_INVCHI_PDF evaluates the PDF of an inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_INVCHI_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) DF
    REAL (KIND=8) R8_INVCHI_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP1
    REAL (KIND=8) TEMP2
 
    IF (DF <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_INVCHI_PDF - Fatal error!'
        WRITE (*, '(a)') '  Degrees of freedom must be positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_INVCHI_PDF = 0.0D+00
 
    ELSE
 
        TEMP2 = DF * 0.5D+00
        TEMP1 = - TEMP2 * LOG (2.0D+00) - (TEMP2+1.0D+00) * LOG (RVAL) - 0.5D+00 / RVAL - &
       & R8_GAMMA_LOG (TEMP2)
 
        R8_INVCHI_PDF = EXP (TEMP1)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_INVCHI_SAMPLE (DF)
 
!*****************************************************************************80
!
!! R8_INVCHI_SAMPLE samples an inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 8 ) R8_INVCHI_SAMPLE, a sample value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) DF
    REAL (KIND=8) R8_INVCHI_SAMPLE
    REAL (KIND=8) VALUE
 
    A = 0.5D+00
    B = 0.5D+00 * DF
 
    VALUE = R8_GAMMA_SAMPLE (A, B)
 
    IF (VALUE /= 0.0D+00) THEN
        VALUE = 1.0D+00 / VALUE
    END IF
 
    R8_INVCHI_SAMPLE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_INVGAM_PDF (BETA, ALPHA, RVAL)
 
!*****************************************************************************80
!
!! R8_INVGAM_PDF evaluates the PDF of an inverse gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_INVGAM_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) BETA
    REAL (KIND=8) R8_INVGAM_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) TEMP
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_INVGAM_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter ALPHA is not positive.'
        RETURN
    END IF
 
    IF (BETA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_INVGAM_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter BETA is not positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_INVGAM_PDF = 0.0D+00
 
    ELSE
 
        TEMP = ALPHA * LOG (BETA) - (ALPHA+1.0D+00) * LOG (RVAL) - BETA / RVAL - R8_GAMMA_LOG &
       & (ALPHA)
 
        R8_INVGAM_PDF = EXP (TEMP)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_INVGAM_SAMPLE (BETA, ALPHA)
 
!*****************************************************************************80
!
!! R8_INVGAM_SAMPLE samples an inverse gamma distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) BETA, the rate parameter.
!    0.0 < BETA.
!
!    Input, real ( kind = 8 ) ALPHA, the shape parameter.
!    0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) R8_INVGAM_SAMPLE, a sample value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) BETA
    REAL (KIND=8) R8_INVGAM_SAMPLE
    REAL (KIND=8) VALUE
 
    VALUE = R8_GAMMA_SAMPLE (BETA, ALPHA)
 
    IF (VALUE /= 0.0D+00) THEN
        VALUE = 1.0D+00 / VALUE
    END IF
 
    R8_INVGAM_SAMPLE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_NORMAL_PDF (AV, SD, RVAL)
 
!*****************************************************************************80
!
!! R8_NORMAL_PDF evaluates the PDF of a normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AV, the mean value.
!
!    Input, real ( kind = 8 ) SD, the standard deviation.
!    0.0 < SD.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_NORMAL_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) AV
    REAL (KIND=8), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=8) R8_NORMAL_PDF
    REAL (KIND=8) RTEMP
    REAL (KIND=8) RVAL
    REAL (KIND=8) SD
 
    IF (SD <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_NORMAL_PDF - Fatal error!'
        WRITE (*, '(a)') '  Standard deviation must be positive.'
        RETURN
    END IF
 
    RTEMP = (RVAL-AV) * (RVAL-AV) * 0.5D+00 / (SD*SD)
 
    R8_NORMAL_PDF = EXP (-RTEMP) / SD / SQRT (2.0D+00*PI)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_NORMAL_SAMPLE (AV, SD)
 
!*****************************************************************************80
!
!! R8_NORMAL_SAMPLE generates a normal random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a normal distribution
!    with mean AV, and standard deviation SD.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) AV, the mean.
!
!    Input, real ( kind = 8 ) SD, the standard deviation.
!
!    Output, real ( kind = 8 ) R8_NORMAL_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) AV
    REAL (KIND=8) R8_NORMAL_SAMPLE
    REAL (KIND=8) SD
 
    R8_NORMAL_SAMPLE = SD * R8_NORMAL_01_SAMPLE () + AV
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_NORMAL_01_PDF (RVAL)
 
!*****************************************************************************80
!
!! R8_NORMAL_01_PDF evaluates the PDF of a standard normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_NORMAL_01_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=8) R8_NORMAL_01_PDF
    REAL (KIND=8) RVAL
 
    R8_NORMAL_01_PDF = EXP (-0.5D+00*RVAL**2) / SQRT (2.0D+00*PI)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_NORMAL_01_SAMPLE ()
 
!*****************************************************************************80
!
!! R8_NORMAL_01_SAMPLE returns a unit pseudonormal R8.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!    Typically, we would use one value and save the other for the next call.
!    However, the fact that this function has saved memory makes it difficult
!    to correctly handle cases where we want to re-initialize the code,
!    or to run in parallel.  Therefore, we will instead use the first value
!    and DISCARD the second.
!
!    EFFICIENCY must defer to SIMPLICITY.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_NORMAL_01_SAMPLE, a sample of the standard
!    normal PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=8) R1
    REAL (KIND=8) R2
    REAL (KIND=8) R8_NORMAL_01_SAMPLE
    REAL (KIND=8) X
 
    R1 = R8_UNIFORM_01_SAMPLE ()
    R2 = R8_UNIFORM_01_SAMPLE ()
 
    X = SQRT (-2.0D+00*LOG(R1)) * COS (2.0D+00*PI*R2)
 
    R8_NORMAL_01_SAMPLE = X
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_SCINVCHI_PDF (DF, S, RVAL)
 
!*****************************************************************************80
!
!! R8_SCINVCHI_PDF: PDF for a scaled inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) S, the scale factor.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_SCINVCHI_PDF, the value of the PDF at RVAL.
!    inverse-chi-square distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) DF
    REAL (KIND=8) R8_SCINVCHI_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) S
    REAL (KIND=8) TEMP1
    REAL (KIND=8) TEMP2
 
    IF (DF <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_SCINVCHI_PDF - Fatal error!'
        WRITE (*, '(a)') '  Degrees of freedom must be positive.'
        RETURN
    END IF
 
    IF (S <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_SCINVCHI_PDF - Fatal error!'
        WRITE (*, '(a)') '  Scale parameter must be positive.'
        RETURN
    END IF
 
    IF (RVAL <= 0.0D+00) THEN
 
        R8_SCINVCHI_PDF = 0.0D+00
 
    ELSE
 
        TEMP2 = DF * 0.5D+00
        TEMP1 = TEMP2 * LOG (TEMP2) + TEMP2 * LOG (S) - (TEMP2*S/RVAL) - (TEMP2+1.0D+00) * LOG &
       & (RVAL) - R8_GAMMA_LOG (TEMP2)
 
        R8_SCINVCHI_PDF = EXP (TEMP1)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_SCINVCHI_SAMPLE (DF, S)
 
!*****************************************************************************80
!
!! R8_SCINVCHI_SAMPLE: sample a scaled inverse chi-squared distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Input, real ( kind = 8 ) S, the scale factor.
!    0.0 < S.
!
!    Input, real ( kind = 8 ) R8_SCINVCHI_SAMPLE, a sample value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) DF
    REAL (KIND=8) R8_SCINVCHI_SAMPLE
    REAL (KIND=8) S
    REAL (KIND=8) VALUE
 
    A = 0.5D+00 * DF * S
    B = 0.5D+00 * DF
 
    VALUE = R8_GAMMA_SAMPLE (A, B)
 
    IF (VALUE /= 0.0D+00) THEN
        VALUE = 1.0D+00 / VALUE
    END IF
 
    R8_SCINVCHI_SAMPLE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNIFORM_PDF (LOWER, UPPER, RVAL)
 
!*****************************************************************************80
!
!! R8_UNIFORM_PDF evaluates the PDF of a uniform distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2013
!
!  Author:
!
!    Original FORTRAN90 version by Guannan Zhang.
!    Modifications by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LOWER, UPPER, the lower and upper range limits.
!    LOWER < UPPER.
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) LOWER
    REAL (KIND=8) R8_UNIFORM_PDF
    REAL (KIND=8) RVAL
    REAL (KIND=8) UPPER
 
    IF (UPPER <= LOWER) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_UNIFORM_PDF - Fatal error!'
        WRITE (*, '(a)') '  For uniform PDF, the lower limit must be '
        WRITE (*, '(a)') '  less than the upper limit!'
        RETURN
    END IF
 
    IF (RVAL < LOWER) THEN
        R8_UNIFORM_PDF = 0.0D+00
    ELSE IF (RVAL <= UPPER) THEN
        R8_UNIFORM_PDF = 1.0D+00 / (UPPER-LOWER)
    ELSE
        R8_UNIFORM_PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNIFORM_SAMPLE (LOW, HIGH)
 
!*****************************************************************************80
!
!! R8_UNIFORM_SAMPLE generates a uniform random deviate.
!
!  Discussion:
!
!    This procedure generates a real deviate uniformly distributed between
!    LOW and HIGH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) HIGH
    REAL (KIND=8) LOW
    REAL (KIND=8) R8_UNIFORM_SAMPLE
 
    R8_UNIFORM_SAMPLE = LOW + (HIGH-LOW) * R8_UNIFORM_01_SAMPLE ()
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNIFORM_01_PDF (RVAL)
 
!*****************************************************************************80
!
!! R8_UNIFORM_01_PDF evaluates the PDF of a standard uniform distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) RVAL, the point where the PDF is evaluated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01_PDF, the value of the PDF at RVAL.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R8_UNIFORM_01_PDF
    REAL (KIND=8) RVAL
 
    IF (RVAL < 0.0D+00) THEN
        R8_UNIFORM_01_PDF = 0.0D+00
    ELSE IF (RVAL <= 1.0D+00) THEN
        R8_UNIFORM_01_PDF = 1.0D+00
    ELSE
        R8_UNIFORM_01_PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNIFORM_01_SAMPLE ()
 
!*****************************************************************************80
!
!! R8_UNIFORM_01_SAMPLE generates a uniform random deviate from [0,1].
!
!  Discussion:
!
!    This function should be the only way that the package accesses random
!    numbers.
!
!    Setting OPTION to 0 accesses the R8_UNI_01() function in RNGLIB,
!    for which there are versions in various languages, which should result
!    in the same values being returned.
!
!    Setting OPTION to 1 in the FORTRAN90 version calls the system
!    RNG "random_number()".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01_SAMPLE, a random deviate
!    from the distribution.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: OPTION = 0
    REAL (KIND=8) R8_UNIFORM_01_SAMPLE
    REAL (KIND=8) VALUE
 
    IF (OPTION == 0) THEN
        VALUE = R8_UNI_01 ()
    ELSE
        CALL RANDOM_NUMBER (HARVEST=VALUE)
    END IF
 
    R8_UNIFORM_01_SAMPLE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8GE_PRINT (M, N, A, TITLE)
 
!*****************************************************************************80
!
!! R8GE_PRINT prints an R8GE matrix.
!
!  Discussion:
!
!    The R8GE storage format is used for a general M by N matrix.  A storage
!    space is made for each entry.  The two dimensional logical
!    array can be thought of as a vector of M*N entries, starting with
!    the M entries in the column 1, then the M entries in column 2
!    and so on.  Considered as a vector, the entry A(I,J) is then stored
!    in vector location I+(J-1)*M.
!
!    R8GE storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(M,N), the R8GE matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    CHARACTER (LEN=*) TITLE
 
    CALL R8GE_PRINT_SOME (M, N, A, 1, 1, M, N, TITLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8GE_PRINT_SOME (M, N, A, ILO, JLO, IHI, JHI, TITLE)
 
!*****************************************************************************80
!
!! R8GE_PRINT_SOME prints some of an R8GE matrix.
!
!  Discussion:
!
!    The R8GE storage format is used for a general M by N matrix.  A storage
!    space is made for each entry.  The two dimensional logical
!    array can be thought of as a vector of M*N entries, starting with
!    the M entries in the column 1, then the M entries in column 2
!    and so on.  Considered as a vector, the entry A(I,J) is then stored
!    in vector location I+(J-1)*M.
!
!    R8GE storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(M,N), the R8GE matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: INCX = 5
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    CHARACTER (LEN=14) CTEMP (INCX)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) I2HI
    INTEGER (KIND=4) I2LO
    INTEGER (KIND=4) IHI
    INTEGER (KIND=4) ILO
    INTEGER (KIND=4) INC
    INTEGER (KIND=4) J
    INTEGER (KIND=4) J2
    INTEGER (KIND=4) J2HI
    INTEGER (KIND=4) J2LO
    INTEGER (KIND=4) JHI
    INTEGER (KIND=4) JLO
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
!
!  Print the columns of the matrix, in strips of 5.
!
    DO J2LO = JLO, JHI, INCX
 
        J2HI = J2LO + INCX - 1
        J2HI = MIN (J2HI, N)
        J2HI = MIN (J2HI, JHI)
 
        INC = J2HI + 1 - J2LO
 
        WRITE (*, '(a)') ' '
 
        DO J = J2LO, J2HI
            J2 = J + 1 - J2LO
            WRITE (CTEMP(J2), '(i7,7x)') J
        END DO
 
        WRITE (*, '(''  Col:  '',5a14)') (CTEMP(J2), J2=1, INC)
        WRITE (*, '(a)') '  Row'
        WRITE (*, '(a)') '  ---'
!
!  Determine the range of the rows in this strip.
!
        I2LO = MAX (ILO, 1)
        I2HI = MIN (IHI, M)
 
        DO I = I2LO, I2HI
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
            DO J2 = 1, INC
 
                J = J2LO - 1 + J2
 
                WRITE (CTEMP(J2), '(g14.6)') A (I, J)
 
            END DO
 
            WRITE (*, '(i5,1x,5a14)') I, (CTEMP(J2), J2=1, INC)
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8GE_TO_R8PO (N, A, B)
 
!*****************************************************************************80
!
!! R8GE_TO_R8PO copies an R8GE matrix to an R8PO matrix.
!
!  Discussion:
!
!    The R8PO format assumes the matrix is square and symmetric; it is also
!    typically assumed that the matrix is positive definite.  These are not
!    required here.  The copied R8PO matrix simply zeros out the lower triangle
!    of the R8GE matrix.
!
!    The R8GE storage format is used for a general M by N matrix.  A storage
!    space is made for each entry.  The two dimensional logical
!    array can be thought of as a vector of M*N entries, starting with
!    the M entries in the column 1, then the M entries in column 2
!    and so on.  Considered as a vector, the entry A(I,J) is then stored
!    in vector location I+(J-1)*M.
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the R8PO matrix.
!
!    Output, real ( kind = 8 ) B(N,N), the R8GE matrix.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    REAL (KIND=8) B (N, N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, N
        DO J = 1, N
            IF (I <= J) THEN
                B (I, J) = A (I, J)
            ELSE
                B (I, J) = 0.0D+00
            END IF
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8MAT_NORM_FRO_AFFINE (M, N, A1, A2)
 
!*****************************************************************************80
!
!! R8MAT_NORM_FRO_AFFINE returns the Frobenius norm of an R8MAT difference.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!    The Frobenius norm is defined as
!
!      R8MAT_NORM_FRO = sqrt (
!        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) A(I,J) * A(I,J) )
!
!    The matrix Frobenius norm is not derived from a vector norm, but
!    is compatible with the vector L2 norm, so that:
!
!      r8vec_norm_l2 ( A * x ) <= r8mat_norm_fro ( A ) * r8vec_norm_l2 ( x ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows.
!
!    Input, integer ( kind = 4 ) N, the number of columns.
!
!    Input, real ( kind = 8 ) A1(M,N), A2(M,N), the matrices for whose
!    difference the Frobenius norm is desired.
!
!    Output, real ( kind = 8 ) R8MAT_NORM_FRO_AFFINE, the Frobenius
!    norm of A1 - A2.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A1 (M, N)
    REAL (KIND=8) A2 (M, N)
    REAL (KIND=8) R8MAT_NORM_FRO_AFFINE
 
    R8MAT_NORM_FRO_AFFINE = SQRT (SUM((A1(1:M, 1:N)-A2(1:M, 1:N))**2))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_DET (N, A_LU, DET)
 
!*****************************************************************************80
!
!! R8PO_DET computes the determinant of a matrix factored by R8PO_FA.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A_LU(N,N), the LU factors from R8PO_FA.
!
!    Output, real ( kind = 8 ) DET, the determinant of A.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A_LU (N, N)
    REAL (KIND=8) DET
    INTEGER (KIND=4) I
 
    DET = 1.0D+00
 
    DO I = 1, N
        DET = DET * A_LU (I, I) ** 2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_FA (N, A, R)
 
!*****************************************************************************80
!
!! R8PO_FA factors an R8PO matrix.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!    The positive definite symmetric matrix A has a Cholesky factorization
!    of the form:
!
!      A = R' * R
!
!    where R is an upper triangular matrix with positive elements on
!    its diagonal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the matrix.
!
!    Output, real ( kind = 8 ) R(N,N), the Cholesky factor R.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) INFO
    INTEGER (KIND=4) J
    INTEGER (KIND=4) K
    REAL (KIND=8) R (N, N)
    REAL (KIND=8) S
 
    R (1:N, 1:N) = A (1:N, 1:N)
 
    DO J = 1, N
 
        DO K = 1, J - 1
            R (K, J) = (R(K, J)-SUM(R(1:K-1, K)*R(1:K-1, J))) / R (K, K)
        END DO
 
        S = R (J, J) - SUM (R(1:J-1, J)**2)
 
        IF (S <= 0.0D+00) THEN
            INFO = J
            WRITE (*, '(a)') ''
            WRITE (*, '(a)') 'R8PO_FA - Fatal error!'
            WRITE (*, '(a,i4)') '  Factorization failed on column ', J
            RETURN
        END IF
 
        R (J, J) = SQRT (S)
 
    END DO
 
    INFO = 0
!
!  Since the Cholesky factor is stored in R8GE format, be sure to
!  zero out the lower triangle.
!
    DO I = 1, N
        DO J = 1, I - 1
            R (I, J) = 0.0D+00
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_INVERSE (N, R, B)
 
!*****************************************************************************80
!
!! R8PO_INVERSE computes the inverse of a matrix factored by R8PO_FA.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) R(N,N), the Cholesky factor..
!
!    Output, real ( kind = 8 ) B(N,N), the inverse matrix, in R8PO storage.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) B (N, N)
    INTEGER (KIND=4) J
    INTEGER (KIND=4) K
    REAL (KIND=8) R (N, N)
    REAL (KIND=8) T
 
    B (1:N, 1:N) = R (1:N, 1:N)
!
!  Compute Inverse ( R ).
!
    DO K = 1, N
 
        B (K, K) = 1.0D+00 / B (K, K)
        B (1:K-1, K) = - B (1:K-1, K) * B (K, K)
 
        DO J = K + 1, N
            T = B (K, J)
            B (K, J) = 0.0D+00
            B (1:K, J) = B (1:K, J) + T * B (1:K, K)
        END DO
 
    END DO
!
!  Compute Inverse ( R ) * ( Inverse ( R ) )'.
!
    DO J = 1, N
 
        DO K = 1, J - 1
            T = B (K, J)
            B (1:K, K) = B (1:K, K) + T * B (1:K, J)
        END DO
 
        B (1:J, J) = B (1:J, J) * B (J, J)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_MV (N, A, X, B)
 
!*****************************************************************************80
!
!! R8PO_MV multiplies an R8PO matrix by an R8VEC.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the R8PO matrix.
!
!    Input, real ( kind = 8 ) X(N), the vector to be multiplied by A.
!
!    Output, real ( kind = 8 ) B(N), the product A * x.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    REAL (KIND=8) B (N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) X (N)
 
    DO I = 1, N
        B (I) = 0.0D+00
        DO J = 1, I - 1
            B (I) = B (I) + A (J, I) * X (J)
        END DO
        DO J = I, N
            B (I) = B (I) + A (I, J) * X (J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! R8PO_PRINT prints an R8PO matrix.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the R8PO matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    CHARACTER (LEN=*) TITLE
 
    CALL R8PO_PRINT_SOME (N, A, 1, 1, N, N, TITLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8PO_PRINT_SOME (N, A, ILO, JLO, IHI, JHI, TITLE)
 
!*****************************************************************************80
!
!! R8PO_PRINT_SOME prints some of an R8PO matrix.
!
!  Discussion:
!
!    The R8PO storage format is used for a symmetric positive definite
!    matrix and its inverse.  (The Cholesky factor of an R8PO matrix is an
!    upper triangular matrix, so it will be in R8GE storage format.)
!
!    Only the diagonal and upper triangle of the square array are used.
!    This same storage scheme is used when the matrix is factored by
!    R8PO_FA, or inverted by R8PO_INVERSE.  For clarity, the lower triangle
!    is set to zero.
!
!    R8PO storage is used by LINPACK and LAPACK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the R8PO matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: INCX = 5
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    REAL (KIND=8) AIJ
    CHARACTER (LEN=14) CTEMP (INCX)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) I2HI
    INTEGER (KIND=4) I2LO
    INTEGER (KIND=4) IHI
    INTEGER (KIND=4) ILO
    INTEGER (KIND=4) INC
    INTEGER (KIND=4) J
    INTEGER (KIND=4) J2
    INTEGER (KIND=4) J2HI
    INTEGER (KIND=4) J2LO
    INTEGER (KIND=4) JHI
    INTEGER (KIND=4) JLO
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
!
!  Print the columns of the matrix, in strips of 5.
!
    DO J2LO = JLO, JHI, INCX
 
        J2HI = J2LO + INCX - 1
        J2HI = MIN (J2HI, N)
        J2HI = MIN (J2HI, JHI)
 
        INC = J2HI + 1 - J2LO
 
        WRITE (*, '(a)') ' '
 
        DO J = J2LO, J2HI
            J2 = J + 1 - J2LO
            WRITE (CTEMP(J2), '(i7,7x)') J
        END DO
 
        WRITE (*, '(''  Col:  '',5a14)') (CTEMP(J2), J2=1, INC)
        WRITE (*, '(a)') '  Row'
        WRITE (*, '(a)') '  ---'
!
!  Determine the range of the rows in this strip.
!
        I2LO = MAX (ILO, 1)
        I2HI = MIN (IHI, N)
 
        DO I = I2LO, I2HI
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
            DO J2 = 1, INC
 
                J = J2LO - 1 + J2
 
                IF (I <= J) THEN
                    AIJ = A (I, J)
                ELSE
                    AIJ = A (J, I)
                END IF
 
                WRITE (CTEMP(J2), '(g14.6)') AIJ
 
            END DO
 
            WRITE (*, '(i5,1x,5a14)') I, (CTEMP(J2), J2=1, INC)
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8UT_PRINT (M, N, A, TITLE)
 
!*****************************************************************************80
!
!! R8UT_PRINT prints an R8UT matrix.
!
!  Discussion:
!
!    The R8UT storage format is used for an M by N upper triangular
!    matrix.  The format stores all M*N entries, even those which are zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(M,N), the R8UT matrix.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    CHARACTER (LEN=*) TITLE
 
    CALL R8UT_PRINT_SOME (M, N, A, 1, 1, M, N, TITLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8UT_PRINT_SOME (M, N, A, ILO, JLO, IHI, JHI, TITLE)
 
!*****************************************************************************80
!
!! R8UT_PRINT_SOME prints some of an R8UT matrix.
!
!  Discussion:
!
!    The R8UT storage format is used for an M by N upper triangular
!    matrix.  The format stores all M*N entries, even those which are zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer ( kind = 4 ) N, the number of columns of the matrix.
!    N must be positive.
!
!    Input, real ( kind = 8 ) A(M,N), the R8UT matrix.
!
!    Input, integer ( kind = 4 ) ILO, JLO, IHI, JHI, the first row and
!    column, and the last row and column to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: INCX = 5
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    CHARACTER (LEN=14) CTEMP (INCX)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) I2HI
    INTEGER (KIND=4) I2LO
    INTEGER (KIND=4) IHI
    INTEGER (KIND=4) ILO
    INTEGER (KIND=4) INC
    INTEGER (KIND=4) J
    INTEGER (KIND=4) J2
    INTEGER (KIND=4) J2HI
    INTEGER (KIND=4) J2LO
    INTEGER (KIND=4) JHI
    INTEGER (KIND=4) JLO
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
!
!  Print the columns of the matrix, in strips of 5.
!
    DO J2LO = JLO, JHI, INCX
 
        J2HI = J2LO + INCX - 1
        J2HI = MIN (J2HI, N)
        J2HI = MIN (J2HI, JHI)
 
        INC = J2HI + 1 - J2LO
 
        WRITE (*, '(a)') ' '
 
        DO J = J2LO, J2HI
            J2 = J + 1 - J2LO
            WRITE (CTEMP(J2), '(i7,7x)') J
        END DO
 
        WRITE (*, '(a,5a14)') '  Col: ', (CTEMP(J2), J2=1, INC)
        WRITE (*, '(a)') '  Row'
        WRITE (*, '(a)') '  ---'
!
!  Determine the range of the rows in this strip.
!
        I2LO = MAX (ILO, 1)
        I2HI = MIN (IHI, M)
        I2HI = MIN (I2HI, J2HI)
 
        DO I = I2LO, I2HI
!
!  Print out (up to) 5 entries in row I, that lie in the current strip.
!
            DO J2 = 1, INC
 
                J = J2LO - 1 + J2
 
                IF (J < I) THEN
                    CTEMP (J2) = '              '
                ELSE
                    WRITE (CTEMP(J2), '(g14.6)') A (I, J)
                END IF
 
            END DO
 
            WRITE (*, '(i5,1x,5a14)') I, (CTEMP(J2), J2=1, INC)
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8UT_SL (N, A, B, X)
 
!*****************************************************************************80
!
!! R8UT_SL solves a linear system A*x=b with A an R8UT matrix.
!
!  Discussion:
!
!    The R8UT storage format is used for an M by N upper triangular
!    matrix.  The format stores all M*N entries, even those which are zero.
!
!    No factorization of the upper triangular matrix is required.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Input, real ( kind = 8 ) A(N,N), the R8UT matrix.
!
!    Input, real ( kind = 8 ) B(N), the right hand side.
!
!    Output, real ( kind = 8 ) X(N), the solution vector.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N, N)
    REAL (KIND=8) B (N)
    INTEGER (KIND=4) J
    REAL (KIND=8) X (N)
 
    X (1:N) = B (1:N)
 
    DO J = N, 1, - 1
        X (J) = X (J) / A (J, J)
        X (1:J-1) = X (1:J-1) - A (1:J-1, J) * X (J)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8UT_ZEROS (M, N, A)
 
!*****************************************************************************80
!
!! R8UT_ZEROS zeroes an R8UT matrix.
!
!  Discussion:
!
!    The R8UT storage format is used for an M by N upper triangular
!    matrix.  The format stores all M*N entries, even those which are zero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of
!    the matrix.  M and N must be positive.
!
!    Output, real ( kind = 8 ) A(M,N), the R8UT matrix.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
 
    A (1:M, 1:N) = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8VEC_MULTINORMAL_PDF (N, MU, R, C_DET, X)
 
!*****************************************************************************80
!
!! R8VEC_MULTINORMAL_PDF evaluates a multivariate normal PDF.
!
!  Discussion:
!
!    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / sqrt ( det ( C ) )
!      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      MU is the mean vector of length N,
!      C is an N by N positive definite symmetric covariance matrix.
!
!    The properties of C guarantee that it has an upper triangular
!    matrix R, the Cholesky factor, such that C = R' * R.  It is the
!    matrix R that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MU(N), the mean vector.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular Cholesky
!    factor of the covariance matrix C.
!
!    Input, real ( kind = 8 ) C_DET, the determinant of the
!    covariance matrix C.
!
!    Input, real ( kind = 8 ) X(N), a sample of the distribution.
!
!    Output, real ( kind = 8 ) R8VEC_MULTINORMAL_PDF, the PDF evaluated
!    at X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) B (N)
    REAL (KIND=8) C_DET
    REAL (KIND=8) MU (N)
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=8) R (N, N)
    REAL (KIND=8) R8VEC_MULTINORMAL_PDF
    REAL (KIND=8) X (N)
    REAL (KIND=8) XCX
    REAL (KIND=8) Y (N)
!
!  Compute:
!    inverse(R')*(x-mu) = y
!  by solving:
!    R'*y = x-mu
!
    B (1:N) = X (1:N) - MU (1:N)
    CALL R8UT_SL (N, R, B, Y)
!
!  Compute:
!    (x-mu)' * inv(C)          * (x-mu)
!  = (x-mu)' * inv(R'*R)       * (x-mu)
!  = (x-mu)' * inv(R) * inv(R) * (x-mu)
!  = y' * y.
!
    XCX = DOT_PRODUCT (Y, Y)
 
    PDF = 1.0D+00 / SQRT ((2.0D+00*PI)**N) * 1.0D+00 / SQRT (C_DET) * EXP (-0.5D+00*XCX)
 
    R8VEC_MULTINORMAL_PDF = PDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_MULTINORMAL_SAMPLE (N, MU, R, X)
 
!*****************************************************************************80
!
!! R8VEC_MULTINORMAL_SAMPLE samples a multivariate normal PDF.
!
!  Discussion:
!
!    PDF ( MU(1:N), C(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( C )
!      * exp ( - ( X - MU )' * inverse ( C ) * ( X - MU ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      MU is the mean vector of length N,
!      C is an N by N positive definite symmetric covariance matrix.
!
!    The properties of C guarantee that it has an upper triangular
!    matrix R, the Cholesky factor, such that C = R' * R.  It is the
!    matrix R that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MU(N), the mean vector.
!
!    Input, real ( kind = 8 ) R(N,N), the upper triangular Cholesky
!    factor of the covariance matrix C.
!
!    Output, real ( kind = 8 ) X(N), a sample of the distribution.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) MU (N)
    REAL (KIND=8) R (N, N)
    REAL (KIND=8) X (N)
    REAL (KIND=8) Z (N)
!
!  Compute X = MU + R' * Z
!  where Z is a vector of standard normal variates.
!
    DO J = 1, N
        Z (J) = R8_NORMAL_01_SAMPLE ()
    END DO
 
    DO I = 1, N
        X (I) = MU (I)
        DO J = 1, I
            X (I) = X (I) + R (J, I) * Z (J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    INTEGER (KIND=4) I
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
    WRITE (*, '(a)') ' '
 
    DO I = 1, N
        WRITE (*, '(2x,i8,a,1x,g16.8)') I, ':', A (I)
    END DO
 
    RETURN
END
 
!******************************************************************************

END MODULE ModLib_PDFLib

!******************************************************************************


MODULE ModLib_RanLib
       
!** PURPOSE OF THIS MODULE:
    ! contains routines that produce random samples from Probability Density Functions (PDF's),
    ! including Beta, Chi-square Exponential, F, Gamma, Multivariate normal, Noncentral
    ! chi-square, Noncentral F, Univariate normal, random permutations, Real uniform, Binomial,
    ! Negative Binomial, Multinomial, Poisson and Integer uniform, by Barry Brown and James
    ! Lovato.
    !
    ! The routines, and the probability density functions they sample, include:
    !    - GENBET, Beta distribution;
    !    - GENCHI, Chi-Square distribution;
    !    - GENEXP, Exponential distribution;
    !    - GENF, F distribution;
    !    - GENGAM, Gamma distribution;
    !    - GENMN, multivariate normal distribution;
    !    - GENMUL, multinomial distribution;
    !    - GENNCH, noncentral Chi-Square distribution;
    !    - GENNF, noncentral F distribution;
    !    - GENNOR, normal distribution;
    !    - GENUNF, uniform distribution on [0,1];
    !    - IGNBIN, binomial distribution;
    !    - IGNNBN, negative binomial distribution.
    !    - IGNPOI, Poisson distribution.
    !    - IGNUIN, uniform distribution on integers in a given range.

!** REFERENCES:
    ! These routines are from RANLIB and RNGLIB packages by John Burkardt

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: GENBET, GENCHI, GENEXP, GENF,   GENGAM
    PUBLIC :: GENMN,  GENMUL, GENNCH, GENNF,  GENNOR
    PUBLIC :: GENUNF, IGNBIN, IGNNBN, IGNPOI, IGNUIN
    PUBLIC :: I4_UNI, R4_UNI_01, R8_UNI_01
    
    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_RanLib'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!------------------------------------------------------------------------------
!
!                               RANLIB
!               General Random Number Generators (RNG's)
!
!------------------------------------------------------------------------------

FUNCTION GENBET (AA, BB)
 
!*****************************************************************************80
!
!! GENBET generates a beta random deviate.
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
!    19 September 2014
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
!    Input, real ( kind = 4 ) AA, the first parameter of the beta distribution.
!    0.0 < AA.
!
!    Input, real ( kind = 4 ) BB, the second parameter of the beta distribution.
!    0.0 < BB.
!
!    Output, real ( kind = 4 ) GENBET, a beta random variate.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    REAL (KIND=4) AA
    REAL (KIND=4) ALPHA
    REAL (KIND=4) B
    REAL (KIND=4) BB
    REAL (KIND=4) BETA
    REAL (KIND=4) DELTA
    REAL (KIND=4) GAMMA
    REAL (KIND=4) GENBET
    REAL (KIND=4) K1
    REAL (KIND=4) K2
    REAL (KIND=4), PARAMETER :: LOG4 = 1.3862943611198906188E+00
    REAL (KIND=4), PARAMETER :: LOG5 = 1.6094379124341003746E+00
    REAL (KIND=4) R
    REAL (KIND=4) S
    REAL (KIND=4) T
    REAL (KIND=4) U1
    REAL (KIND=4) U2
    REAL (KIND=4) V
    REAL (KIND=4) W
    REAL (KIND=4) Y
    REAL (KIND=4) Z
 
    IF (AA <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENBET - Fatal error!'
        WRITE (*, '(a)') '  AA <= 0.0'
        RETURN
    END IF
 
    IF (BB <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENBET - Fatal error!'
        WRITE (*, '(a)') '  BB <= 0.0'
        RETURN
    END IF
!
!  Algorithm BB
!
    IF (1.0E+00 < AA .AND. 1.0E+00 < BB) THEN
 
        A = MIN (AA, BB)
        B = MAX (AA, BB)
        ALPHA = A + B
        BETA = SQRT ((ALPHA-2.0E+00)/(2.0E+00*A*B-ALPHA))
        GAMMA = A + 1.0E+00 / BETA
 
        DO
 
            U1 = R4_UNI_01 ()
            U2 = R4_UNI_01 ()
            V = BETA * LOG (U1/(1.0E+00-U1))
!
!  exp ( v ) replaced by r4_exp ( v )
!
            W = A * R4_EXP (V)
 
            Z = U1 ** 2 * U2
            R = GAMMA * V - LOG4
            S = A + R - W
 
            IF (5.0E+00*Z <= S+1.0E+00+LOG5) THEN
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
        BETA = 1.0E+00 / B
        DELTA = 1.0E+00 + A - B
        K1 = DELTA * (1.0E+00/72.0E+00+B/24.0E+00) / (A/B-7.0E+00/9.0E+00)
        K2 = 0.25E+00 + (0.5E+00+0.25E+00/DELTA) * B
 
        DO
 
            U1 = R4_UNI_01 ()
            U2 = R4_UNI_01 ()
 
            IF (U1 < 0.5E+00) THEN
 
                Y = U1 * U2
                Z = U1 * Y
 
                IF (K1 <= 0.25E+00*U2+Z-Y) THEN
                    CYCLE
                END IF
 
            ELSE
 
                Z = U1 ** 2 * U2
 
                IF (Z <= 0.25E+00) THEN
 
                    V = BETA * LOG (U1/(1.0E+00-U1))
                    W = A * EXP (V)
 
                    IF (AA == A) THEN
                        GENBET = W / (B+W)
                    ELSE
                        GENBET = B / (B+W)
                    END IF
 
                    RETURN
 
                END IF
 
                IF (K2 < Z) THEN
                    CYCLE
                END IF
 
            END IF
 
            V = BETA * LOG (U1/(1.0E+00-U1))
            W = A * EXP (V)
 
            IF (LOG(Z) <= ALPHA*(LOG(ALPHA/(B+W))+V)-LOG4) THEN
                EXIT
            END IF
 
        END DO
 
    END IF
 
    IF (AA == A) THEN
        GENBET = W / (B+W)
    ELSE
        GENBET = B / (B+W)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENCHI (DF)
 
!*****************************************************************************80
!
!! GENCHI generates a Chi-Square random deviate.
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
!    Input, real ( kind = 4 ) DF, the degrees of freedom.
!    0.0 < DF.
!
!    Output, real ( kind = 4 ) GENCHI, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) ARG1
    REAL (KIND=4) ARG2
    REAL (KIND=4) DF
    REAL (KIND=4) GENCHI
 
    IF (DF <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENCHI - Fatal error!'
        WRITE (*, '(a)') '  DF <= 0.'
        WRITE (*, '(a,g14.6)') '  Value of DF: ', DF
        RETURN
    END IF
 
    ARG1 = 1.0E+00
    ARG2 = DF / 2.0E+00
 
    GENCHI = 2.0E+00 * GENGAM (ARG1, ARG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENEXP (AV)
 
!*****************************************************************************80
!
!! GENEXP generates an exponential random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from an exponential
!    distribution with mean AV.
!
!    See also the function R4_EXPONENTIAL_SAMPLE.
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
!    Computer Methods for Sampling From the
!    Exponential and Normal Distributions,
!    Communications of the ACM,
!    Volume 15, Number 10, October 1972, pages 873-882.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean of the exponential distribution
!    from which a random deviate is to be generated.
!
!    Output, real ( kind = 4 ) GENEXP, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) AV
    REAL (KIND=4) GENEXP
 
    GENEXP = SEXPO () * AV
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENF (DFN, DFD)
 
!*****************************************************************************80
!
!! GENF generates an F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the F (variance ratio)
!    distribution with DFN degrees of freedom in the numerator
!    and DFD degrees of freedom in the denominator.
!
!    It directly generates the ratio of chisquare variates
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
!    Input, real ( kind = 4 ) DFN, the numerator degrees of freedom.
!    0.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Output, real ( kind = 4 ) GENF, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) DFD
    REAL (KIND=4) DFN
    REAL (KIND=4) GENF
    REAL (KIND=4) XDEN
    REAL (KIND=4) XNUM
 
    IF (DFN <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENF - Fatal error!'
        WRITE (*, '(a)') '  DFN <= 0.0'
        RETURN
    END IF
 
    IF (DFD <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENF - Fatal error!'
        WRITE (*, '(a)') '  DFD <= 0.0'
        RETURN
    END IF
 
    XNUM = GENCHI (DFN) / DFN
    XDEN = GENCHI (DFD) / DFD
    GENF = XNUM / XDEN
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENGAM (A, R)
 
!*****************************************************************************80
!
!! GENGAM generates a Gamma random deviate.
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
!    Input, real ( kind = 4 ) A, the location parameter.
!
!    Input, real ( kind = 4 ) R, the shape parameter.
!
!    Output, real ( kind = 4 ) GENGAM, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    REAL (KIND=4) GENGAM
    REAL (KIND=4) R
 
    GENGAM = SGAMMA (R) / A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENMN (PARM, X, WORK)
 
!*****************************************************************************80
!
!! GENMN generates a multivariate normal deviate.
!
!  Discussion:
!
!    The method is:
!    1) Generate P independent standard normal deviates - Ei ~ N(0,1)
!    2) Using Cholesky decomposition find A so that A'*A = COVM
!    3) A' * E + MEANV ~ N(MEANV,COVM)
!
!    Note that PARM contains information needed to generate the
!    deviates, and is set up by SETGMN.
!
!    PARM(1) contains the size of the deviates, P
!    PARM(2:P+1) contains the mean vector.
!    PARM(P+2:P*(P+3)/2+1) contains the upper half of the Cholesky
!    decomposition of the covariance matrix.
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
!    Input, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters set by SETGMN.
!
!    Output, real ( kind = 4 ) X(P), a random deviate from the distribution.
!
!    Workspace, real ( kind = 4 ) WORK(P).
!
    IMPLICIT NONE
 
    REAL (KIND=4) AE
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICOUNT
    INTEGER (KIND=4) J
    INTEGER (KIND=4) P
    REAL (KIND=4) PARM (*)
    REAL (KIND=4) WORK (*)
    REAL (KIND=4) X (*)
 
    P = INT (PARM(1))
!
!  Generate P independent normal deviates.
!
    DO I = 1, P
        WORK (I) = SNORM ()
    END DO
!
!  Compute X = MEANV + A' * WORK
!
    DO I = 1, P
        ICOUNT = 0
        AE = 0.0E+00
        DO J = 1, I
            ICOUNT = ICOUNT + J - 1
            AE = AE + PARM (I+(J-1)*P-ICOUNT+P+1) * WORK (J)
        END DO
 
        X (I) = AE + PARM (I+1)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENMUL (N, P, NCAT, IX)
 
!*****************************************************************************80
!
!! GENMUL generates a multinomial random deviate.
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
!    Input, integer ( kind = 4 ) N, the number of events, which will be
!    classified into one of the NCAT categories.
!
!    Input, real ( kind = 4 ) P(NCAT-1).  P(I) is the probability that an event
!    will be classified into category I.  Thus, each P(I) must be between
!    0.0 and 1.0.  Only the first NCAT-1 values of P must be defined since
!    P(NCAT) would be 1.0 minus the sum of the first NCAT-1 P's.
!
!    Input, integer ( kind = 4 ) NCAT, the number of categories.
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
    REAL (KIND=4) P (NCAT-1)
    REAL (KIND=4) PROB
    REAL (KIND=4) PTOT
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENMUL - Fatal error!'
        WRITE (*, '(a)') '  N < 0'
        RETURN
    END IF
 
    IF (NCAT <= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENMUL - Fatal error!'
        WRITE (*, '(a)') '  NCAT <= 1'
        RETURN
    END IF
 
    DO I = 1, NCAT - 1
 
        IF (P(I) < 0.0E+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'GENMUL - Fatal error!'
            WRITE (*, '(a)') '  Some P(i) < 0.'
            RETURN
        END IF
 
        IF (1.0E+00 < P(I)) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'GENMUL - Fatal error!'
            WRITE (*, '(a)') '  Some 1 < P(i).'
            RETURN
        END IF
 
    END DO
 
    PTOT = 0.0E+00
    DO I = 1, NCAT - 1
        PTOT = PTOT + P (I)
    END DO
 
    IF (0.99999E+00 < PTOT) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENMUL - Fatal error!'
        WRITE (*, '(a)') '  1 < Sum of P().'
        RETURN
    END IF
!
!  Initialize variables.
!
    NTOT = N
    PTOT = 1.0E+00
    DO I = 1, NCAT
        IX (I) = 0
    END DO
!
!  Generate the observation.
!
    DO ICAT = 1, NCAT - 1
        PROB = P (ICAT) / PTOT
        IX (ICAT) = IGNBIN (NTOT, PROB)
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
 
FUNCTION GENNCH (DF, XNONC)
 
!*****************************************************************************80
!
!! GENNCH generates a noncentral Chi-Square random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the  distribution of a
!    noncentral chisquare with DF degrees of freedom and noncentrality parameter
!    XNONC.
!
!    It uses the fact that the noncentral chisquare is the sum of a chisquare
!    deviate with DF-1 degrees of freedom plus the square of a normal
!    deviate with mean XNONC and standard deviation 1.
!
!    A subtle ambiguity arises in the original formulation:
!
!      gennch = genchi ( arg1 ) + ( gennor ( arg2, arg3 ) ) ^ 2
!
!    because the compiler is free to invoke either genchi or gennor
!    first, both of which alter the random number generator state,
!    resulting in two distinct possible results.
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
!    Input, real ( kind = 4 ) DF, the degrees of freedom.
!    1.0 < DF.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNCH, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) ARG1
    REAL (KIND=4) ARG2
    REAL (KIND=4) ARG3
    REAL (KIND=4) DF
    REAL (KIND=4) GENNCH
    REAL (KIND=4) T1
    REAL (KIND=4) T2
    REAL (KIND=4) XNONC
 
    IF (DF <= 1.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENNCH - Fatal error!'
        WRITE (*, '(a)') '  DF <= 1.'
        RETURN
    END IF
 
    IF (XNONC < 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENNCH - Fatal error!'
        WRITE (*, '(a)') '  XNONC < 0.0.'
        RETURN
    END IF
 
    ARG1 = DF - 1.0E+00
    ARG2 = SQRT (XNONC)
    ARG3 = 1.0E+00
 
    T1 = GENCHI (ARG1)
    T2 = GENNOR (ARG2, ARG3)
 
    GENNCH = T1 + T2 * T2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENNF (DFN, DFD, XNONC)
 
!*****************************************************************************80
!
!! GENNF generates a noncentral F random deviate.
!
!  Discussion:
!
!    This procedure generates a random deviate from the noncentral F
!    (variance ratio) distribution with DFN degrees of freedom in the
!    numerator, and DFD degrees of freedom in the denominator, and
!    noncentrality parameter XNONC.
!
!    It directly generates the ratio of noncentral numerator chisquare variate
!    to central denominator chisquare variate.
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
!    Input, real ( kind = 4 ) DFN, the numerator degrees of freedom.
!    1.0 < DFN.
!
!    Input, real ( kind = 4 ) DFD, the denominator degrees of freedom.
!    0.0 < DFD.
!
!    Input, real ( kind = 4 ) XNONC, the noncentrality parameter.
!    0.0 <= XNONC.
!
!    Output, real ( kind = 4 ) GENNF, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) DFD
    REAL (KIND=4) DFN
    REAL (KIND=4) GENNF
    REAL (KIND=4) XDEN
    REAL (KIND=4) XNONC
    REAL (KIND=4) XNUM
 
    IF (DFN <= 1.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENNF - Fatal error!'
        WRITE (*, '(a)') '  DFN <= 1.0'
        RETURN
    END IF
 
    IF (DFD <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENNF - Fatal error!'
        WRITE (*, '(a)') '  DFD <= 0.0'
        RETURN
    END IF
 
    IF (XNONC < 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENNF - Fatal error!'
        WRITE (*, '(a)') '  XNONC < 0.0'
        RETURN
    END IF
 
    XNUM = GENNCH (DFN, XNONC) / DFN
    XDEN = GENCHI (DFD) / DFD
 
    GENNF = XNUM / XDEN
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENNOR (AV, SD)
 
!*****************************************************************************80
!
!! GENNOR generates a normal random deviate.
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
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) AV, the mean.
!
!    Input, real ( kind = 4 ) SD, the standard deviation.
!
!    Output, real ( kind = 4 ) GENNOR, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) AV
    REAL (KIND=4) GENNOR
    REAL (KIND=4) SD
 
    GENNOR = SD * SNORM () + AV
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENPRM (IARRAY, N)
 
!*****************************************************************************80
!
!! GENPRM generates and applies a random permutation to an array.
!
!  Discussion:
!
!    To see the permutation explicitly, let the input array be
!    1, 2, ..., N.
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
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) IARRAY(N), an array to be permuted.
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IARRAY (N)
    INTEGER (KIND=4) ITMP
    INTEGER (KIND=4) IWHICH
 
    DO I = 1, N
        IWHICH = IGNUIN (I, N)
        ITMP = IARRAY (IWHICH)
        IARRAY (IWHICH) = IARRAY (I)
        IARRAY (I) = ITMP
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENUNF (LOW, HIGH)
 
!*****************************************************************************80
!
!! GENUNF generates a uniform random deviate.
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
!    Input, real ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, real ( kind = 4 ) GENUNF, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) GENUNF
    REAL (KIND=4) HIGH
    REAL (KIND=4) LOW
 
    GENUNF = LOW + (HIGH-LOW) * R4_UNI_01 ()
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION IGNBIN (N, PP)
 
!*****************************************************************************80
!
!! IGNBIN generates a binomial random deviate.
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
!    31 March 2013
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
!    Input, real ( kind = 4 ) PP, the probability of an event in each trial of
!    the binomial distribution from which a random deviate is to be generated.
!    0.0 < PP < 1.0.
!
!    Output, integer ( kind = 4 ) IGNBIN, a random deviate from the
!    distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) AL
    REAL (KIND=4) ALV
    REAL (KIND=4) AMAXP
    REAL (KIND=4) C
    REAL (KIND=4) F
    REAL (KIND=4) F1
    REAL (KIND=4) F2
    REAL (KIND=4) FFM
    REAL (KIND=4) FM
    REAL (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IGNBIN
    INTEGER (KIND=4) IX
    INTEGER (KIND=4) IX1
    INTEGER (KIND=4) K
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MP
    REAL (KIND=4) PP
    INTEGER (KIND=4) N
    REAL (KIND=4) P
    REAL (KIND=4) P1
    REAL (KIND=4) P2
    REAL (KIND=4) P3
    REAL (KIND=4) P4
    REAL (KIND=4) Q
    REAL (KIND=4) QN
    REAL (KIND=4) R
    REAL (KIND=4) T
    REAL (KIND=4) U
    REAL (KIND=4) V
    REAL (KIND=4) W
    REAL (KIND=4) W2
    REAL (KIND=4) X
    REAL (KIND=4) X1
    REAL (KIND=4) X2
    REAL (KIND=4) XL
    REAL (KIND=4) XLL
    REAL (KIND=4) XLR
    REAL (KIND=4) XM
    REAL (KIND=4) XNP
    REAL (KIND=4) XNPQ
    REAL (KIND=4) XR
    REAL (KIND=4) YNORM
    REAL (KIND=4) Z
    REAL (KIND=4) Z2
 
    IF (PP <= 0.0E+00 .OR. 1.0E+00 <= PP) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNBIN - Fatal error!'
        WRITE (*, '(a)') '  PP is out of range.'
        RETURN
    END IF
 
    P = MIN (PP, 1.0E+00-PP)
    Q = 1.0E+00 - P
    XNP = REAL (N, KIND=4) * P
 
    IF (XNP < 30.0E+00) THEN
 
        QN = Q ** N
        R = P / Q
        G = R * REAL (N+1, KIND=4)
 
        DO
 
            IX = 0
            F = QN
            U = R4_UNI_01 ()
 
            DO
 
                IF (U < F) THEN
                    IF (0.5E+00 < PP) THEN
                        IX = N - IX
                    END IF
                    IGNBIN = IX
                    RETURN
                END IF
 
                IF (110 < IX) THEN
                    EXIT
                END IF
 
                U = U - F
                IX = IX + 1
                F = F * (G/REAL(IX, KIND=4)-R)
 
            END DO
 
        END DO
 
    END IF
 
    FFM = XNP + P
    M = FFM
    FM = M
    XNPQ = XNP * Q
    P1 = INT (2.195E+00*SQRT(XNPQ)-4.6E+00*Q) + 0.5E+00
    XM = FM + 0.5E+00
    XL = XM - P1
    XR = XM + P1
    C = 0.134E+00 + 20.5E+00 / (15.3E+00+FM)
    AL = (FFM-XL) / (FFM-XL*P)
    XLL = AL * (1.0E+00+0.5E+00*AL)
    AL = (XR-FFM) / (XR*Q)
    XLR = AL * (1.0E+00+0.5E+00*AL)
    P2 = P1 * (1.0E+00+C+C)
    P3 = P2 + C / XLL
    P4 = P3 + C / XLR
!
!  Generate a variate.
!
    DO
 
        U = R4_UNI_01 () * P4
        V = R4_UNI_01 ()
!
!  Triangle
!
        IF (U < P1) THEN
            IX = XM - P1 * V + U
            IF (0.5E+00 < PP) THEN
                IX = N - IX
            END IF
            IGNBIN = IX
            RETURN
        END IF
!
!  Parallelogram
!
        IF (U <= P2) THEN
 
            X = XL + (U-P1) / C
            V = V * C + 1.0E+00 - ABS (XM-X) / P1
 
            IF (V <= 0.0E+00 .OR. 1.0E+00 < V) THEN
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
 
        IF (K <= 20 .OR. XNPQ/2.0-1.0 <= K) THEN
 
            F = 1.0E+00
            R = P / Q
            G = (N+1) * R
 
            IF (M < IX) THEN
                MP = M + 1
                DO I = M + 1, IX
                    F = F * (G/I-R)
                END DO
            ELSE IF (IX < M) THEN
                IX1 = IX + 1
                DO I = IX + 1, M
                    F = F / (G/REAL(I, KIND=4)-R)
                END DO
            END IF
 
            IF (V <= F) THEN
                IF (0.5E+00 < PP) THEN
                    IX = N - IX
                END IF
                IGNBIN = IX
                RETURN
            END IF
 
        ELSE
 
            AMAXP = (K/XNPQ) * ((K*(K/3.0E+00+0.625E+00)+0.1666666666666E+00)/XNPQ+0.5E+00)
            YNORM = - REAL (K*K, KIND=4) / (2.0E+00*XNPQ)
            ALV = LOG (V)
 
            IF (ALV < YNORM-AMAXP) THEN
                IF (0.5E+00 < PP) THEN
                    IX = N - IX
                END IF
                IGNBIN = IX
                RETURN
            END IF
 
            IF (YNORM+AMAXP < ALV) THEN
                CYCLE
            END IF
 
            X1 = REAL (IX+1, KIND=4)
            F1 = FM + 1.0E+00
            Z = REAL (N+1, KIND=4) - FM
            W = REAL (N-IX+1, KIND=4)
            Z2 = Z * Z
            X2 = X1 * X1
            F2 = F1 * F1
            W2 = W * W
 
            T = XM * LOG (F1/X1) + (N-M+0.5E+00) * LOG (Z/W) + REAL (IX-M, KIND=4) * LOG &
           & (W*P/(X1*Q)) + &
           & (13860.0E+00-(462.0E+00-(132.0E+00-(99.0E+00-140.0E+00/F2)/F2)/F2)/F2) / F1 / &
           & 166320.0E+00 + &
           & (13860.0E+00-(462.0E+00-(132.0E+00-(99.0E+00-140.0E+00/Z2)/Z2)/Z2)/Z2) / Z / &
           & 166320.0E+00 + &
           & (13860.0E+00-(462.0E+00-(132.0E+00-(99.0E+00-140.0E+00/X2)/X2)/X2)/X2) / X1 / &
           & 166320.0E+00 + &
           & (13860.0E+00-(462.0E+00-(132.0E+00-(99.0E+00-140.0E+00/W2)/W2)/W2)/W2) / W / &
           & 166320.0E+00
 
            IF (ALV <= T) THEN
                IF (0.5E+00 < PP) THEN
                    IX = N - IX
                END IF
                IGNBIN = IX
                RETURN
            END IF
 
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION IGNNBN (N, P)
 
!*****************************************************************************80
!
!! IGNNBN generates a negative binomial random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a negative binomial
!    distribution.
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
!    Input, integer ( kind = 4 ) N, the required number of events.
!    0 <= N.
!
!    Input, real ( kind = 4 ) P, the probability of an event during a
!    Bernoulli trial.  0.0 < P < 1.0.
!
!    Output, integer ( kind = 4 ) IGNNBN, a random deviate from
!    the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    INTEGER (KIND=4) IGNNBN
    INTEGER (KIND=4) N
    REAL (KIND=4) P
    REAL (KIND=4) R
    REAL (KIND=4) Y
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNNBN - Fatal error!'
        WRITE (*, '(a)') '  N < 0.'
        RETURN
    END IF
 
    IF (P <= 0.0E+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNNBN - Fatal error!'
        WRITE (*, '(a)') '  P <= 0.0'
        RETURN
    END IF
 
    IF (1.0E+00 <= P) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNNBN - Fatal error!'
        WRITE (*, '(a)') '  1.0 <= P'
        RETURN
    END IF
!
!  Generate Y, a random gamma (n,(1-p)/p) variable.
!
    R = REAL (N)
    A = P / (1.0E+00-P)
    Y = GENGAM (A, R)
!
!  Generate a random Poisson ( y ) variable.
!
    IGNNBN = IGNPOI (Y)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION IGNPOI (MU)
 
!*****************************************************************************80
!
!! IGNPOI generates a Poisson random deviate.
!
!  Discussion:
!
!    This procedure generates a single random deviate from a Poisson
!    distribution with given mean.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2018
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Generation of Poisson Deviates
!    From Modified Normal Distributions,
!    ACM Transactions on Mathematical Software,
!    Volume 8, Number 2, June 1982, pages 163-179.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) MU, the mean of the Poisson distribution
!    from which a random deviate is to be generated.
!
!    Output, integer ( kind = 4 ) IGNPOI, a random deviate from
!    the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4), PARAMETER :: A0 = - 0.5E+00
    REAL (KIND=4), PARAMETER :: A1 = 0.3333333E+00
    REAL (KIND=4), PARAMETER :: A2 = - 0.2500068E+00
    REAL (KIND=4), PARAMETER :: A3 = 0.2000118E+00
    REAL (KIND=4), PARAMETER :: A4 = - 0.1661269E+00
    REAL (KIND=4), PARAMETER :: A5 = 0.1421878E+00
    REAL (KIND=4), PARAMETER :: A6 = - 0.1384794E+00
    REAL (KIND=4), PARAMETER :: A7 = 0.1250060E+00
    REAL (KIND=4) B1
    REAL (KIND=4) B2
    REAL (KIND=4) C
    REAL (KIND=4) C0
    REAL (KIND=4) C1
    REAL (KIND=4) C2
    REAL (KIND=4) C3
    REAL (KIND=4) D
    REAL (KIND=4) DEL
    REAL (KIND=4) DIFMUK
    REAL (KIND=4) E
    REAL (KIND=4) FACT (10)
    REAL (KIND=4) FK
    REAL (KIND=4) FX
    REAL (KIND=4) FY
    REAL (KIND=4) G
    INTEGER (KIND=4) IGNPOI
    INTEGER (KIND=4) K
    INTEGER (KIND=4) KFLAG
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    REAL (KIND=4) MU
    REAL (KIND=4) OMEGA
    REAL (KIND=4) P
    REAL (KIND=4) P0
    REAL (KIND=4) PX
    REAL (KIND=4) PY
    REAL (KIND=4) Q
    REAL (KIND=4) S
    REAL (KIND=4) T
    REAL (KIND=4) U
    REAL (KIND=4) V
    REAL (KIND=4) X
    REAL (KIND=4) XX
 
    SAVE FACT
 
    DATA FACT / 1.0E+00, 1.0E+00, 2.0E+00, 6.0E+00, 24.0E+00, 120.0E+00, 720.0E+00, 5040.0E+00, &
   & 40320.0E+00, 362880.0E+00 /
!
!  MU < 10
!
    IF (MU < 10.0E+00) THEN
 
        M = MAX (1, INT(MU))
        L = 0
        P = EXP (-MU)
        Q = P
        P0 = P
!
!  Uniform sample for inversion method.
!
        DO
 
            U = R4_UNI_01 ()
            IGNPOI = 0
 
            IF (U <= P0) THEN
                RETURN
            END IF
!
!  Creation of new Poisson probabilities.
!
            DO K = 1, 35
                P = P * MU / REAL (K)
                Q = Q + P
                IF (U <= Q) THEN
                    IGNPOI = K
                    RETURN
                END IF
            END DO
 
        END DO
!
!  10 <= MU
!
    ELSE
 
        S = SQRT (MU)
        D = 6.0E+00 * MU * MU
        L = INT (MU-1.1484E+00)
!
!  Normal sample.
!
        G = MU + S * SNORM ()
 
        IF (0.0E+00 <= G) THEN
 
            IGNPOI = INT (G)
!
!  Immediate acceptance if large enough.
!
            IF (L <= IGNPOI) THEN
                RETURN
            END IF
!
!  Squeeze acceptance.
!
            FK = REAL (IGNPOI)
            DIFMUK = MU - FK
            U = R4_UNI_01 ()
 
            IF (DIFMUK*DIFMUK*DIFMUK <= D*U) THEN
                RETURN
            END IF
 
        END IF
!
!  Preparation for steps P and Q.
!
        OMEGA = 0.3989423E+00 / S
        B1 = 0.04166667E+00 / MU
        B2 = 0.3E+00 * B1 * B1
        C3 = 0.1428571E+00 * B1 * B2
        C2 = B2 - 15.0E+00 * C3
        C1 = B1 - 6.0E+00 * B2 + 45.0E+00 * C3
        C0 = 1.0E+00 - B1 + 3.0E+00 * B2 - 15.0E+00 * C3
        C = 0.1069E+00 / MU
 
        IF (0.0E+00 <= G) THEN
 
            KFLAG = 0
 
            IF (IGNPOI < 10) THEN
 
                PX = - MU
                PY = MU ** IGNPOI / FACT (IGNPOI+1)
 
            ELSE
 
                DEL = 0.8333333E-01 / FK
                DEL = DEL - 4.8E+00 * DEL * DEL * DEL
                V = DIFMUK / FK
 
                IF (0.25E+00 < ABS(V)) THEN
                    PX = FK * LOG (1.0E+00+V) - DIFMUK - DEL
                ELSE
                    PX = FK * V * V * (((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V+A0) - DEL
                END IF
 
                PY = 0.3989423E+00 / SQRT (FK)
 
            END IF
 
            X = (0.5E+00-DIFMUK) / S
            XX = X * X
            FX = - 0.5E+00 * XX
            FY = OMEGA * (((C3*XX+C2)*XX+C1)*XX+C0)
 
            IF (FY-U*FY <= PY*EXP(PX-FX)) THEN
                RETURN
            END IF
 
        END IF
!
!  Exponential sample.
!
        DO
 
            E = SEXPO ()
            U = 2.0E+00 * R4_UNI_01 () - 1.0E+00
            IF (U < 0.0E+00) THEN
                T = 1.8E+00 - ABS (E)
            ELSE
                T = 1.8E+00 + ABS (E)
            END IF
 
            IF (T <=-0.6744E+00) THEN
                CYCLE
            END IF
 
            IGNPOI = INT (MU+S*T)
            FK = REAL (IGNPOI)
            DIFMUK = MU - FK
 
            KFLAG = 1
!
!  Calculation of PX, PY, FX, FY.
!
            IF (IGNPOI < 10) THEN
 
                PX = - MU
                PY = MU ** IGNPOI / FACT (IGNPOI+1)
 
            ELSE
 
                DEL = 0.8333333E-01 / FK
                DEL = DEL - 4.8E+00 * DEL * DEL * DEL
                V = DIFMUK / FK
 
                IF (0.25E+00 < ABS(V)) THEN
                    PX = FK * LOG (1.0E+00+V) - DIFMUK - DEL
                ELSE
                    PX = FK * V * V * (((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1)*V+A0) - DEL
                END IF
 
                PY = 0.3989423E+00 / SQRT (FK)
 
            END IF
 
            X = (0.5E+00-DIFMUK) / S
            XX = X * X
            FX = - 0.5E+00 * XX
            FY = OMEGA * (((C3*XX+C2)*XX+C1)*XX+C0)
 
            IF (KFLAG <= 0) THEN
 
                IF (FY-U*FY <= PY*EXP(PX-FX)) THEN
                    RETURN
                END IF
 
            ELSE
 
                IF (C*ABS(U) <= PY*EXP(PX+E)-FY*EXP(FX+E)) THEN
                    RETURN
                END IF
 
            END IF
 
        END DO
 
    END IF
 
END
 
!******************************************************************************
 
FUNCTION IGNUIN (LOW, HIGH)
 
!*****************************************************************************80
!
!! IGNUIN generates a random integer in a given range.
!
!  Discussion:
!
!    Each deviate K satisfies LOW <= K <= HIGH.
!
!    If (HIGH-LOW) > 2,147,483,561, this procedure prints an error message
!    and stops the program.
!
!    IGNLGI generates integer ( kind = 4 )s between 1 and 2147483562.
!
!    MAXNUM is 1 less than the maximum generatable value.
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
!    Input, integer ( kind = 4 ) LOW, HIGH, the lower and upper bounds.
!
!    Output, integer ( kind = 4 ) IGNUIN, a random deviate from
!    the distribution.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) HIGH
    INTEGER (KIND=4) IGN
    INTEGER (KIND=4) IGNUIN
    INTEGER (KIND=4) LOW
    INTEGER (KIND=4) MAXNOW
    INTEGER (KIND=4) MAXNUM
    PARAMETER (MAXNUM=2147483561)
    INTEGER (KIND=4) RANP1
    INTEGER (KIND=4) WIDTH
 
    IF (HIGH < LOW) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNUIN - Fatal error!'
        WRITE (*, '(a)') '  HIGH < LOW.'
        RETURN
    END IF
 
    WIDTH = HIGH - LOW
 
    IF (MAXNUM < WIDTH) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IGNUIN - Fatal error!'
        WRITE (*, '(a)') '  Range HIGH-LOW is too large.'
        RETURN
    END IF
 
    IF (LOW == HIGH) THEN
        IGNUIN = LOW
        RETURN
    END IF
 
    RANP1 = WIDTH + 1
    MAXNOW = (MAXNUM/RANP1) * RANP1
 
    DO
 
        IGN = I4_UNI () - 1
 
        IF (IGN <= MAXNOW) THEN
            EXIT
        END IF
 
    END DO
 
    IGNUIN = LOW + MOD (IGN, RANP1)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LENNOB (S)
 
!*****************************************************************************80
!
!! LENNOB counts the length of a string, ignoring trailing blanks.
!
!  Discussion:
!
!    This procedure returns the length of a string up to and including
!    the last non-blank character.
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
!    Input, character * ( * ) S, the string.
!
!    Output, integer ( kind = 4 ) LENNOB, the length of the string to the last
!    nonblank.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) LENNOB
    CHARACTER(LEN=*) S
    INTEGER (KIND=4) S_MAX
 
    S_MAX = LEN (S)
 
    DO I = S_MAX, 1, - 1
        IF (S(I:I) /= ' ') THEN
            LENNOB = I
            RETURN
        END IF
    END DO
 
    LENNOB = 0
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PHRTSD (PHRASE, SEED1, SEED2)
 
!*****************************************************************************80
!
!! PHRTST converts a phrase to a pair of random number generator seeds.
!
!  Discussion:
!
!    This procedure uses a character string to generate two seeds for the RGN
!    random number generator.
!
!    Trailing blanks are eliminated before the seeds are generated.
!
!    Generated seed values will fall in the range 1 to 2^30 = 1,073,741,824.
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
!    Input, character * ( * ) PHRASE, a phrase to be used for the
!    random number generation.
!
!    Output, integer ( kind = 4 ) SEED1, SEED2, the two seeds for the
!    random number generator, based on PHRASE.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICHR
    INTEGER (KIND=4) J
    INTEGER (KIND=4) LPHR
    CHARACTER(LEN=*) PHRASE
    INTEGER (KIND=4) SEED1
    INTEGER (KIND=4) SEED2
    INTEGER (KIND=4) SHIFT (0:4)
    CHARACTER(LEN=86) TABLE
    PARAMETER (TABLE='abcdefghijklmnopqrstuvwxyz'//'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//'0123456789'//&
   & '!@#$%^&*()_+[];:''"<>?,./')
    INTEGER (KIND=4) TWOP30
    PARAMETER (TWOP30=1073741824)
    INTEGER (KIND=4) VALUES (5)
 
    SAVE SHIFT
 
    DATA SHIFT / 1, 64, 4096, 262144, 16777216 /
 
    SEED1 = 1234567890
    SEED2 = 123456789
 
    LPHR = LENNOB (PHRASE)
 
    DO I = 1, LPHR
 
        ICHR = INDEX (TABLE, PHRASE(I:I))
!
!  If the character does not occur, ICHR is returned as 0.
!
        ICHR = MOD (ICHR, 64)
 
        IF (ICHR == 0) THEN
            ICHR = 63
        END IF
 
        DO J = 1, 5
            VALUES (J) = ICHR - J
            IF (VALUES(J) < 1) THEN
                VALUES (J) = VALUES (J) + 63
            END IF
        END DO
 
        DO J = 1, 5
            SEED1 = MOD (SEED1+SHIFT(J-1)*VALUES(J), TWOP30)
            SEED2 = MOD (SEED2+SHIFT(J-1)*VALUES(6-J), TWOP30)
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PRCOMP (MAXOBS, P, MEAN, XCOVAR, ANSWER)
 
!*****************************************************************************80
!
!! PRCOMP prints covariance information.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2018
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MAXOBS, the number of observations.
!
!    Input, integer ( kind = 4 ) P, the number of variables.
!
!    Input, real ( kind = 4 ) MEAN(P), the mean for each column.
!
!    Input, real ( kind = 4 ) XCOVAR(P,P), the variance/covariance matrix.
!
!    Input, real ( kind = 4 ) ANSWER(MAXOBS,P), the observed values.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) P
    INTEGER (KIND=4) MAXOBS
 
    REAL (KIND=4) ANSWER (MAXOBS, P)
    REAL (KIND=4) DUM1
    REAL (KIND=4) DUM2
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=4) MEAN (P)
    REAL (KIND=4) RCOVAR (P, P)
    REAL (KIND=4) RMEAN (P)
    REAL (KIND=4) RVAR (P)
    REAL (KIND=4) XCOVAR (P, P)
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'PRCOMP:'
    WRITE (*, '(a)') '  Print and compare covariance information'
    WRITE (*, '(a)') ' '
 
    DO J = 1, P
        CALL STATS (ANSWER(1, J), MAXOBS, RMEAN(J), RVAR(J), DUM1, DUM2)
        WRITE (*, '(a,i4)') '  Variable Number ', J
        WRITE (*, '(a,g14.6,a,g14.6)') '  Mean ', MEAN (J), ' Generated ', RMEAN (J)
        WRITE (*, '(a,g14.6,a,g14.6)') '  Variance ', XCOVAR (J, J), ' Generated ', RVAR (J)
    END DO
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') '  Covariances:'
    WRITE (*, '(a)') ' '
 
    DO I = 1, P
        DO J = 1, I - 1
            WRITE (*, '(a,i4,a,i4)') '  I = ', I, ' J = ', J
            RCOVAR (I, J) = R4VEC_COVAR (MAXOBS, ANSWER(1, I), ANSWER(1, J))
            WRITE (*, '(a,g14.6,a,g14.6)') '  Covariance ', XCOVAR (I, J), ' Generated ', &
           & RCOVAR (I, J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R4_EXP (X)
 
!*****************************************************************************80
!
!! R4_EXP computes the exponential of an R8, avoiding overflow and underflow.
!
!  Discussion:
!
!    For arguments of very large magnitude, the evaluation of the
!    exponential function can cause computational problems.  Some languages
!    and compilers may return an infinite value or a "Not-a-Number".
!    An alternative, when dealing with a wide range of inputs, is simply
!    to truncate the calculation for arguments whose magnitude is too large.
!    Whether this is the right or convenient approach depends on the problem
!    you are dealing with, and whether or not you really need accurate
!    results for large magnitude inputs, or you just want your code to
!    stop crashing.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X, the argument of the exponential function.
!
!    Output, real ( kind = 4 ) R4_EXP, the value of exp ( X ).
!
    IMPLICIT NONE
 
    REAL (KIND=4) R4_EXP
    REAL (KIND=4), PARAMETER :: R4_HUGE = 1.0E+30
    REAL (KIND=4), PARAMETER :: R4_LOG_MAX = + 69.0776E+00
    REAL (KIND=4), PARAMETER :: R4_LOG_MIN = - 69.0776E+00
    REAL (KIND=4) VALUE
    REAL (KIND=4) X
 
    IF (X <= R4_LOG_MIN) THEN
        VALUE = 0.0E+00
    ELSE IF (X < R4_LOG_MAX) THEN
        VALUE = EXP (X)
    ELSE
        VALUE = R4_HUGE
    END IF
 
    R4_EXP = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R4_EXPONENTIAL_SAMPLE (LAMBDA)
 
!*****************************************************************************80
!
!! R4_EXPONENTIAL_SAMPLE samples the exponential PDF.
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
!    Input, real ( kind = 4 ) LAMBDA, the parameter of the PDF.
!
!    Output, real ( kind = 4 ) R4_EXPONENTIAL_SAMPLE, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=4) LAMBDA
    REAL (KIND=4) R
    REAL (KIND=4) R4_EXPONENTIAL_SAMPLE
 
    R = R4_UNI_01 ()
 
    R4_EXPONENTIAL_SAMPLE = - LOG (R) * LAMBDA
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R4VEC_COVAR (N, X, Y)
 
!*****************************************************************************80
!
!! R4VEC_COVAR computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X(N), Y(N), the two vectors.
!
!    Input, integer ( kind = 4 ) N, the dimension of the two vectors.
!
!    Output, real ( kind = 4 ) R4VEC_COVAR, the covariance of the vectors.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    REAL (KIND=4) R4VEC_COVAR
    REAL (KIND=4) VALUE
    REAL (KIND=4) X (N)
    REAL (KIND=4) X_AVERAGE
    REAL (KIND=4) Y (N)
    REAL (KIND=4) Y_AVERAGE
 
    X_AVERAGE = SUM (X(1:N)) / REAL (N, KIND=4)
    Y_AVERAGE = SUM (Y(1:N)) / REAL (N, KIND=4)
 
    VALUE = 0.0E+00
    DO I = 1, N
        VALUE = VALUE + (X(I)-X_AVERAGE) * (Y(I)-Y_AVERAGE)
    END DO
 
    R4VEC_COVAR = VALUE / REAL (N-1, KIND=4)
 
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
!    20 April 2013
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
 
    R = R8_UNI_01 ()
 
    R8_EXPONENTIAL_SAMPLE = - LOG (R) * LAMBDA
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8VEC_COVAR (N, X, Y)
 
!*****************************************************************************80
!
!! R8VEC_COVAR computes the covariance of two vectors.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2013
!
!  Author:
!
!    John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), Y(N), the two vectors.
!
!    Input, integer ( kind = 4 ) N, the dimension of the two vectors.
!
!    Output, real ( kind = 8 ) R4VEC_COVAR, the covariance of the vectors.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    REAL (KIND=8) R8VEC_COVAR
    REAL (KIND=8) VALUE
    REAL (KIND=8) X (N)
    REAL (KIND=8) X_AVERAGE
    REAL (KIND=8) Y (N)
    REAL (KIND=8) Y_AVERAGE
 
    X_AVERAGE = SUM (X(1:N)) / REAL (N, KIND=8)
    Y_AVERAGE = SUM (Y(1:N)) / REAL (N, KIND=8)
 
    VALUE = 0.0D+00
    DO I = 1, N
        VALUE = VALUE + (X(I)-X_AVERAGE) * (Y(I)-Y_AVERAGE)
    END DO
 
    R8VEC_COVAR = VALUE / REAL (N-1, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SDOT (N, SX, INCX, SY, INCY)
 
!*****************************************************************************80
!
!! SDOT forms the dot product of two vectors.
!
!  Discussion:
!
!    This routine uses single precision real ( kind = 4 ) arithmetic.
!
!    This routine uses unrolled loops for increments equal to one.
!
!  Modified:
!
!    07 July 2007
!
!  Author:
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, real ( kind = 4 ) X(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Input, real ( kind = 4 ) Y(*), one of the vectors to be multiplied.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of Y.
!
!    Output, real ( kind = 4 ) SDOT, the dot product of X and Y.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) INCX
    INTEGER (KIND=4) INCY
    INTEGER (KIND=4) IX
    INTEGER (KIND=4) IY
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=4) SDOT
    REAL (KIND=4) STEMP
    REAL (KIND=4) SX (*)
    REAL (KIND=4) SY (*)
 
    SDOT = 0.0E+00
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    STEMP = 0.0E+00
!
!  Code for unequal increments or equal increments not equal to 1.
!
    IF (INCX /= 1 .OR. INCY /= 1) THEN
 
        IF (INCX < 0) THEN
            IX = (-N+1) * INCX + 1
        ELSE
            IX = 1
        END IF
 
        IF (INCY < 0) THEN
            IY = (-N+1) * INCY + 1
        ELSE
            IY = 1
        END IF
 
        DO I = 1, N
            STEMP = STEMP + SX (IX) * SY (IY)
            IX = IX + INCX
            IY = IY + INCY
        END DO
!
!  Code for both increments equal to 1.
!
    ELSE
 
        M = MOD (N, 5)
 
        DO I = 1, M
            STEMP = STEMP + SX (I) * SY (I)
        END DO
 
        DO I = M + 1, N, 5
            STEMP = STEMP + SX (I) * SY (I) + SX (I+1) * SY (I+1) + SX (I+2) * SY (I+2) + SX &
           & (I+3) * SY (I+3) + SX (I+4) * SY (I+4)
        END DO
 
    END IF
 
    SDOT = STEMP
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SETCOV (P, VAR, CORR, COVAR)
 
!*****************************************************************************80
!
!! SETCOV sets a covariance matrix from variance and common correlation.
!
!  Discussion:
!
!    This procedure sets the covariance matrix from the variance and
!    common correlation.
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
!    Input, integer ( kind = 4 ) P, the number of variables.
!
!    Input, real ( kind = 4 ) VAR(P), the variances.
!
!    Input, real ( kind = 4 ) CORR, the common correlaton.
!
!    Output, real ( kind = 4 ) COVAR(P,P), the covariance matrix.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) P
 
    REAL (KIND=4) CORR
    REAL (KIND=4) COVAR (P, P)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=4) VAR (P)
 
    DO I = 1, P
        DO J = 1, P
            IF (I == J) THEN
                COVAR (I, J) = VAR (I)
            ELSE
                COVAR (I, J) = CORR * SQRT (VAR(I)*VAR(J))
            END IF
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SETGMN (MEANV, COVM, P, PARM)
 
!*****************************************************************************80
!
!! SETGMN sets data for the generation of multivariate normal deviates.
!
!  Discussion:
!
!    This procedure places P, MEANV, and the Cholesky factorization of
!    COVM in GENMN.
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
!    Input, real ( kind = 4 ) MEANV(P), the means of the multivariate
!    normal distribution.
!
!    Input/output, real ( kind = 4 ) COVM(P,P).  On input, the covariance
!    matrix of the multivariate distribution.  On output, the information
!    in COVM has been overwritten.
!
!    Input, integer ( kind = 4 ) P, the number of dimensions.
!
!    Output, real ( kind = 4 ) PARM(P*(P+3)/2+1), parameters needed to generate
!    multivariate normal deviates.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) P
 
    REAL (KIND=4) COVM (P, P)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICOUNT
    INTEGER (KIND=4) INFO
    INTEGER (KIND=4) J
    REAL (KIND=4) MEANV (P)
    REAL (KIND=4) PARM (P*(P+3)/2+1)
 
    IF (P <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SETGMN - Fatal error!'
        WRITE (*, '(a)') '  P was not positive.'
        RETURN
    END IF
!
!  Store P.
!
    PARM (1) = P
!
!  Store MEANV.
!
    DO I = 2, P + 1
        PARM (I) = MEANV (I-1)
    END DO
!
!  Compute the Cholesky decomposition.
!
    CALL SPOFA (COVM, P, P, INFO)
 
    IF (INFO /= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SETGMN - Fatal error!'
        WRITE (*, '(a)') '  SPOFA finds COVM not positive definite.'
        RETURN
    END IF
!
!  Store the upper half of the Cholesky factor.
!
    ICOUNT = P + 1
 
    DO I = 1, P
        DO J = I, P
            ICOUNT = ICOUNT + 1
            PARM (ICOUNT) = COVM (I, J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SEXPO ()
 
!*****************************************************************************80
!
!! SEXPO samples the standard exponential distribution.
!
!  Discussion:
!
!   This procedure corresponds to algorithm SA in the reference.
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
!    Computer Methods for Sampling From the
!    Exponential and Normal Distributions,
!    Communications of the ACM,
!    Volume 15, Number 10, October 1972, pages 873-882.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) SEXPO, a random deviate from the standard
!    exponential distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    INTEGER (KIND=4) I
    REAL (KIND=4) Q (8)
    REAL (KIND=4) SEXPO
    REAL (KIND=4) U
    REAL (KIND=4) UMIN
    REAL (KIND=4) USTAR
 
    SAVE Q
 
    DATA Q / 0.6931472E+00, 0.9333737E+00, 0.9888778E+00, 0.9984959E+00, 0.9998293E+00, &
   & 0.9999833E+00, 0.9999986E+00, 0.9999999E+00 /
 
    A = 0.0E+00
    U = R4_UNI_01 ()
 
    DO
 
        U = U + U
 
        IF (1.0E+00 < U) THEN
            EXIT
        END IF
 
        A = A + Q (1)
 
    END DO
 
    U = U - 1.0E+00
 
    IF (U <= Q(1)) THEN
        SEXPO = A + U
        RETURN
    END IF
 
    I = 1
    USTAR = R4_UNI_01 ()
    UMIN = USTAR
 
    DO
 
        USTAR = R4_UNI_01 ()
        UMIN = MIN (UMIN, USTAR)
        I = I + 1
 
        IF (U <= Q(I)) THEN
            EXIT
        END IF
 
    END DO
 
    SEXPO = A + UMIN * Q (1)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SGAMMA (A)
 
!*****************************************************************************80
!
!! SGAMMA samples the standard Gamma distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm GD in the reference.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2013
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
!    Input, real ( kind = 4 ) A, the parameter of the standard gamma
!    distribution.  0.0 < A < 1.0.
!
!    Output, real ( kind = 4 ) SGAMMA, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    REAL (KIND=4), PARAMETER :: A1 = 0.3333333E+00
    REAL (KIND=4), PARAMETER :: A2 = - 0.2500030E+00
    REAL (KIND=4), PARAMETER :: A3 = 0.2000062E+00
    REAL (KIND=4), PARAMETER :: A4 = - 0.1662921E+00
    REAL (KIND=4), PARAMETER :: A5 = 0.1423657E+00
    REAL (KIND=4), PARAMETER :: A6 = - 0.1367177E+00
    REAL (KIND=4), PARAMETER :: A7 = 0.1233795E+00
    REAL (KIND=4) B
    REAL (KIND=4) C
    REAL (KIND=4) D
    REAL (KIND=4) E
    REAL (KIND=4), PARAMETER :: E1 = 1.0E+00
    REAL (KIND=4), PARAMETER :: E2 = 0.4999897E+00
    REAL (KIND=4), PARAMETER :: E3 = 0.1668290E+00
    REAL (KIND=4), PARAMETER :: E4 = 0.0407753E+00
    REAL (KIND=4), PARAMETER :: E5 = 0.0102930E+00
    REAL (KIND=4) P
    REAL (KIND=4) Q
    REAL (KIND=4) Q0
    REAL (KIND=4), PARAMETER :: Q1 = 0.04166669E+00
    REAL (KIND=4), PARAMETER :: Q2 = 0.02083148E+00
    REAL (KIND=4), PARAMETER :: Q3 = 0.00801191E+00
    REAL (KIND=4), PARAMETER :: Q4 = 0.00144121E+00
    REAL (KIND=4), PARAMETER :: Q5 = - 0.00007388E+00
    REAL (KIND=4), PARAMETER :: Q6 = 0.00024511E+00
    REAL (KIND=4), PARAMETER :: Q7 = 0.00024240E+00
    REAL (KIND=4) R
    REAL (KIND=4) S
    REAL (KIND=4) S2
    REAL (KIND=4) SI
    REAL (KIND=4) SGAMMA
    REAL (KIND=4), PARAMETER :: SQRT32 = 5.656854E+00
    REAL (KIND=4) T
    REAL (KIND=4) U
    REAL (KIND=4) V
    REAL (KIND=4) W
    REAL (KIND=4) X
 
    IF (1.0E+00 <= A) THEN
 
        S2 = A - 0.5E+00
        S = SQRT (S2)
        D = SQRT32 - 12.0E+00 * S
!
!  Immediate acceptance.
!
        T = SNORM ()
        X = S + 0.5E+00 * T
        SGAMMA = X * X
 
        IF (0.0E+00 <= T) THEN
            RETURN
        END IF
!
!  Squeeze acceptance.
!
        U = R4_UNI_01 ()
        IF (D*U <= T*T*T) THEN
            RETURN
        END IF
 
        R = 1.0E+00 / A
        Q0 = ((((((Q7*R+Q6)*R+Q5)*R+Q4)*R+Q3)*R+Q2)*R+Q1) * R
!
!  Approximation depending on size of parameter A.
!
        IF (13.022E+00 < A) THEN
            B = 1.77E+00
            SI = 0.75E+00
            C = 0.1515E+00 / S
        ELSE IF (3.686E+00 < A) THEN
            B = 1.654E+00 + 0.0076E+00 * S2
            SI = 1.68E+00 / S + 0.275E+00
            C = 0.062E+00 / S + 0.024E+00
        ELSE
            B = 0.463E+00 + S + 0.178E+00 * S2
            SI = 1.235E+00
            C = 0.195E+00 / S - 0.079E+00 + 0.16E+00 * S
        END IF
!
!  Quotient test.
!
        IF (0.0E+00 < X) THEN
 
            V = 0.5E+00 * T / S
 
            IF (0.25E+00 < ABS(V)) THEN
                Q = Q0 - S * T + 0.25E+00 * T * T + 2.0E+00 * S2 * LOG (1.0E+00+V)
            ELSE
                Q = Q0 + 0.5E+00 * T * T * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * V
            END IF
 
            IF (LOG(1.0E+00-U) <= Q) THEN
                RETURN
            END IF
 
        END IF
 
        DO
 
            E = SEXPO ()
            U = 2.0E+00 * R4_UNI_01 () - 1.0E+00
 
            IF (0.0E+00 <= U) THEN
                T = B + ABS (SI*E)
            ELSE
                T = B - ABS (SI*E)
            END IF
!
!  Possible rejection.
!
            IF (T <-0.7187449E+00) THEN
                CYCLE
            END IF
!
!  Calculate V and quotient Q.
!
            V = 0.5E+00 * T / S
 
            IF (0.25E+00 < ABS(V)) THEN
                Q = Q0 - S * T + 0.25E+00 * T * T + 2.0E+00 * S2 * LOG (1.0E+00+V)
            ELSE
                Q = Q0 + 0.5E+00 * T * T * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * V
            END IF
!
!  Hat acceptance.
!
            IF (Q <= 0.0E+00) THEN
                CYCLE
            END IF
 
            IF (0.5E+00 < Q) THEN
                W = EXP (Q) - 1.0E+00
            ELSE
                W = ((((E5*Q+E4)*Q+E3)*Q+E2)*Q+E1) * Q
            END IF
!
!  May have to sample again.
!
            IF (C*ABS(U) <= W*EXP(E-0.5E+00*T*T)) THEN
                EXIT
            END IF
 
        END DO
 
        X = S + 0.5E+00 * T
        SGAMMA = X * X
 
        RETURN
!
!  Method for A < 1.
!
    ELSE
 
        B = 1.0E+00 + 0.3678794E+00 * A
 
        DO
 
            P = B * R4_UNI_01 ()
 
            IF (P < 1.0E+00) THEN
 
                SGAMMA = EXP (LOG(P)/A)
 
                IF (SGAMMA <= SEXPO()) THEN
                    RETURN
                END IF
 
                CYCLE
 
            END IF
 
            SGAMMA = - LOG ((B-P)/A)
 
            IF ((1.0E+00-A)*LOG(SGAMMA) <= SEXPO()) THEN
                EXIT
            END IF
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SNORM ()
 
!*****************************************************************************80
!
!! SNORM samples the standard normal distribution.
!
!  Discussion:
!
!    This procedure corresponds to algorithm FL, with M = 5, in the reference.
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
!    Extensions of Forsythe's Method for Random
!    Sampling from the Normal Distribution,
!    Mathematics of Computation,
!    Volume 27, Number 124, October 1973, page 927-937.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) SNORM, a random deviate from the distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A (32)
    REAL (KIND=4) AA
    REAL (KIND=4) D (31)
    REAL (KIND=4) H (31)
    INTEGER (KIND=4) I
    REAL (KIND=4) S
    REAL (KIND=4) SNORM
    REAL (KIND=4) T (31)
    REAL (KIND=4) TT
    REAL (KIND=4) U
    REAL (KIND=4) USTAR
    REAL (KIND=4) W
    REAL (KIND=4) Y
 
    SAVE A
    SAVE D
    SAVE H
    SAVE T
 
    DATA A / 0.0000000E+00, 0.3917609E-01, 0.7841241E-01, 0.1177699E+00, 0.1573107E+00, &
   & 0.1970991E+00, 0.2372021E+00, 0.2776904E+00, 0.3186394E+00, 0.3601299E+00, 0.4022501E+00, &
   & 0.4450965E+00, 0.4887764E+00, 0.5334097E+00, 0.5791322E+00, 0.6260990E+00, 0.6744898E+00, &
   & 0.7245144E+00, 0.7764218E+00, 0.8305109E+00, 0.8871466E+00, 0.9467818E+00, 1.009990E+00, &
   & 1.077516E+00, 1.150349E+00, 1.229859E+00, 1.318011E+00, 1.417797E+00, 1.534121E+00, &
   & 1.675940E+00, 1.862732E+00, 2.153875E+00 /
 
    DATA D / 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, 0.0000000E+00, &
   & 0.2636843E+00, 0.2425085E+00, 0.2255674E+00, 0.2116342E+00, 0.1999243E+00, 0.1899108E+00, &
   & 0.1812252E+00, 0.1736014E+00, 0.1668419E+00, 0.1607967E+00, 0.1553497E+00, 0.1504094E+00, &
   & 0.1459026E+00, 0.1417700E+00, 0.1379632E+00, 0.1344418E+00, 0.1311722E+00, 0.1281260E+00, &
   & 0.1252791E+00, 0.1226109E+00, 0.1201036E+00, 0.1177417E+00, 0.1155119E+00, 0.1134023E+00, &
   & 0.1114027E+00, 0.1095039E+00 /
 
    DATA H / 0.3920617E-01, 0.3932705E-01, 0.3950999E-01, 0.3975703E-01, 0.4007093E-01, &
   & 0.4045533E-01, 0.4091481E-01, 0.4145507E-01, 0.4208311E-01, 0.4280748E-01, 0.4363863E-01, &
   & 0.4458932E-01, 0.4567523E-01, 0.4691571E-01, 0.4833487E-01, 0.4996298E-01, 0.5183859E-01, &
   & 0.5401138E-01, 0.5654656E-01, 0.5953130E-01, 0.6308489E-01, 0.6737503E-01, 0.7264544E-01, &
   & 0.7926471E-01, 0.8781922E-01, 0.9930398E-01, 0.1155599E+00, 0.1404344E+00, 0.1836142E+00, &
   & 0.2790016E+00, 0.7010474E+00 /
 
    DATA T / 0.7673828E-03, 0.2306870E-02, 0.3860618E-02, 0.5438454E-02, 0.7050699E-02, &
   & 0.8708396E-02, 0.1042357E-01, 0.1220953E-01, 0.1408125E-01, 0.1605579E-01, 0.1815290E-01, &
   & 0.2039573E-01, 0.2281177E-01, 0.2543407E-01, 0.2830296E-01, 0.3146822E-01, 0.3499233E-01, &
   & 0.3895483E-01, 0.4345878E-01, 0.4864035E-01, 0.5468334E-01, 0.6184222E-01, 0.7047983E-01, &
   & 0.8113195E-01, 0.9462444E-01, 0.1123001E+00, 0.1364980E+00, 0.1716886E+00, 0.2276241E+00, &
   & 0.3304980E+00, 0.5847031E+00 /
 
    U = R4_UNI_01 ()
    IF (U <= 0.5E+00) THEN
        S = 0.0E+00
    ELSE
        S = 1.0E+00
    END IF
    U = 2.0E+00 * U - S
    U = 32.0E+00 * U
    I = INT (U)
    IF (I == 32) THEN
        I = 31
    END IF
!
!  Center
!
    IF (I /= 0) THEN
 
        USTAR = U - REAL (I)
        AA = A (I)
 
        DO
 
            IF (T(I) < USTAR) THEN
 
                W = (USTAR-T(I)) * H (I)
 
                Y = AA + W
 
                IF (S /= 1.0E+00) THEN
                    SNORM = Y
                ELSE
                    SNORM = - Y
                END IF
 
                RETURN
 
            END IF
 
            U = R4_UNI_01 ()
            W = U * (A(I+1)-AA)
            TT = (0.5E+00*W+AA) * W
 
            DO
 
                IF (TT < USTAR) THEN
                    Y = AA + W
                    IF (S /= 1.0E+00) THEN
                        SNORM = Y
                    ELSE
                        SNORM = - Y
                    END IF
                    RETURN
                END IF
 
                U = R4_UNI_01 ()
 
                IF (USTAR < U) THEN
                    EXIT
                END IF
 
                TT = U
                USTAR = R4_UNI_01 ()
 
            END DO
 
            USTAR = R4_UNI_01 ()
 
        END DO
!
!  Tail
!
    ELSE
 
        I = 6
        AA = A (32)
 
        DO
 
            U = U + U
 
            IF (1.0E+00 <= U) THEN
                EXIT
            END IF
 
            AA = AA + D (I)
            I = I + 1
 
        END DO
 
        U = U - 1.0E+00
        W = U * D (I)
        TT = (0.5E+00*W+AA) * W
 
        DO
 
            USTAR = R4_UNI_01 ()
 
            IF (TT < USTAR) THEN
                Y = AA + W
                IF (S /= 1.0E+00) THEN
                    SNORM = Y
                ELSE
                    SNORM = - Y
                END IF
                RETURN
            END IF
 
            U = R4_UNI_01 ()
 
            IF (U <= USTAR) THEN
                TT = U
            ELSE
                U = R4_UNI_01 ()
                W = U * D (I)
                TT = (0.5E+00*W+AA) * W
            END IF
 
        END DO
 
    END IF
 
END
 
!******************************************************************************
 
SUBROUTINE SPOFA (A, LDA, N, INFO)
 
!*****************************************************************************80
!
!! SPOFA factors a real symmetric positive definite matrix.
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
!    Cleve Moler
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) A(LDA,N).  On input, the symmetric matrix
!    to be factored.  Only the diagonal and upper triangle are accessed.
!    On output, the strict lower triangle has not been changed.  The diagonal
!    and upper triangle contain an upper triangular matrix R such that
!    A = R' * R.  If INFO is nonzero, the factorization was not completed.
!
!    Input, integer ( kind = 4 ) LDA, the leading dimension of the array A.
!    N <= LDA.
!
!    Input, integer ( kind = 4 ) N, the order of the matrix.
!
!    Output, integer ( kind = 4 ) INFO, error flag.
!    0, no error was detected.
!    K, the leading minor of order K is not positive definite.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) LDA
    INTEGER (KIND=4) N
 
    REAL (KIND=4) A (LDA, N)
    INTEGER (KIND=4) INFO
    INTEGER (KIND=4) J
    INTEGER (KIND=4) JM1
    INTEGER (KIND=4) K
    REAL (KIND=4) S
    REAL (KIND=4) T
 
    INFO = 0
 
    DO J = 1, N
        INFO = J
        S = 0.0E+00
        JM1 = J - 1
        DO K = 1, JM1
            T = A (K, J) - SDOT (K-1, A(1, K), 1, A(1, J), 1)
            T = T / A (K, K)
            A (K, J) = T
            S = S + T * T
        END DO
        S = A (J, J) - S
        IF (S <= 0.0E+00) THEN
            INFO = J
            RETURN
        END IF
        A (J, J) = SQRT (S)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STATS (X, N, AV, VAR, XMIN, XMAX)
 
!*****************************************************************************80
!
!! STATS computes statistics for a given array.
!
!  Discussion:
!
!    This procedure computes the average and variance of an array.
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
!    Input, real ( kind = 4 ) X(N), the array to be analyzed.
!
!    Input, integer ( kind = 4 ) N, the dimension of the array.
!
!    Output, real ( kind = 4 ) AV, the average value.
!
!    Output, real ( kind = 4 ) VAR, the variance.
!
!    Output, real ( kind = 4 ) XMIN, XMAX, the minimum and maximum entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=4) AV
    INTEGER (KIND=4) I
    REAL (KIND=4) TOTAL
    REAL (KIND=4) VAR
    REAL (KIND=4) X (N)
    REAL (KIND=4) XMAX
    REAL (KIND=4) XMIN
 
    XMIN = X (1)
    XMAX = X (1)
    TOTAL = 0.0E+00
    DO I = 1, N
        TOTAL = TOTAL + X (I)
        XMIN = MIN (XMIN, X(I))
        XMAX = MAX (XMAX, X(I))
    END DO
 
    AV = TOTAL / REAL (N)
 
    TOTAL = 0.0E+00
    DO I = 1, N
        TOTAL = TOTAL + (X(I)-AV) ** 2
    END DO
    VAR = TOTAL / REAL (N-1)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRSTAT (PDF, PARIN, AV, VAR)
 
!*****************************************************************************80
!
!! TRSTAT returns the mean and variance for distributions.
!
!  Discussion:
!
!    This procedure returns the mean and variance for a number of statistical
!    distributions as a function of their parameters.
!
!    The input vector PARIN is used to pass in the parameters necessary
!    to specify the distribution.  The number of these parameters varies
!    per distribution, and it is necessary to specify an ordering for the
!    parameters used to a given distribution.  The ordering chosen here
!    is as follows:
!
!    bet
!      PARIN(1) is A
!      PARIN(2) is B
!    bin
!      PARIN(1) is Number of trials
!      PARIN(2) is Prob Event at Each Trial
!    chi
!      PARIN(1) = df
!    exp
!      PARIN(1) = mu
!    f
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!    gam
!      PARIN(1) is A
!      PARIN(2) is R
!    nbn
!      PARIN(1) is N
!      PARIN(2) is P
!    nch
!      PARIN(1) is df
!      PARIN(2) is noncentrality parameter
!    nf
!      PARIN(1) is df numerator
!      PARIN(2) is df denominator
!      PARIN(3) is noncentrality parameter
!    nor
!      PARIN(1) is mean
!      PARIN(2) is standard deviation
!    poi
!      PARIN(1) is Mean
!    unf
!      PARIN(1) is LOW bound
!      PARIN(2) is HIGH bound
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by Barry Brown, James Lovato.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, character * ( 4 ) PDF, indicates the distribution:
!    'bet'  beta distribution
!    'bin'  binomial
!    'chi'  chisquare
!    'exp'  exponential
!    'f'    F (variance ratio)
!    'gam'  gamma
!    'nbn'  negative binomial
!    'nch'  noncentral chisquare
!    'nf'   noncentral f
!    'nor'  normal
!    'poi'  Poisson
!    'unf'  uniform
!
!    Input, real ( kind = 4 ) PARIN(*), the parameters of the distribution.
!
!    Output, real ( kind = 4 ) AV, the mean of the specified distribution.
!
!    Output, real ( kind = 4 ) VAR, the variance of the specified distribuion.
!
    IMPLICIT NONE
 
    REAL (KIND=4) A
    REAL (KIND=4) AV
    REAL (KIND=4) B
    INTEGER (KIND=4) N
    REAL (KIND=4) P
    REAL (KIND=4) PARIN (*)
    CHARACTER (LEN=4) PDF
    REAL (KIND=4) R
    REAL (KIND=4) VAR
    REAL (KIND=4) WIDTH
 
    IF (PDF == 'bet') THEN
 
        AV = PARIN (1) / (PARIN(1)+PARIN(2))
        VAR = (AV*PARIN(2)) / ((PARIN(1)+PARIN(2))*(PARIN(1)+PARIN(2)+1.0E+00))
 
    ELSE IF (PDF == 'bin') THEN
 
        N = INT (PARIN(1))
        P = PARIN (2)
        AV = REAL (N) * P
        VAR = REAL (N) * P * (1.0E+00-P)
 
    ELSE IF (PDF == 'chi') THEN
 
        AV = PARIN (1)
        VAR = 2.0E+00 * PARIN (1)
 
    ELSE IF (PDF == 'exp') THEN
 
        AV = PARIN (1)
        VAR = AV ** 2
 
    ELSE IF (PDF == 'f') THEN
 
        IF (PARIN(2) <= 2.0001E+00) THEN
            AV = - 1.0E+00
        ELSE
            AV = PARIN (2) / (PARIN(2)-2.0E+00)
        END IF
 
        IF (PARIN(2) <= 4.0001E+00) THEN
            VAR = - 1.0E+00
        ELSE
            VAR = (2.0E+00*PARIN(2)**2*(PARIN(1)+PARIN(2)-2.0E+00)) / &
           & (PARIN(1)*(PARIN(2)-2.0E+00)**2*(PARIN(2)-4.0E+00))
        END IF
 
    ELSE IF (PDF == 'gam') THEN
 
        A = PARIN (1)
        R = PARIN (2)
        AV = R / A
        VAR = R / A ** 2
 
    ELSE IF (PDF == 'nbn') THEN
 
        N = INT (PARIN(1))
        P = PARIN (2)
        AV = N * (1.0E+00-P) / P
        VAR = N * (1.0E+00-P) / P ** 2
 
    ELSE IF (PDF == 'nch') THEN
 
        A = PARIN (1) + PARIN (2)
        B = PARIN (2) / A
        AV = A
        VAR = 2.0E+00 * A * (1.0E+00+B)
 
    ELSE IF (PDF == 'nf') THEN
 
        IF (PARIN(2) <= 2.0001E+00) THEN
            AV = - 1.0E+00
        ELSE
            AV = (PARIN(2)*(PARIN(1)+PARIN(3))) / ((PARIN(2)-2.0E+00)*PARIN(1))
        END IF
 
        IF (PARIN(2) <= 4.0001E+00) THEN
            VAR = - 1.0E+00
        ELSE
            A = (PARIN(1)+PARIN(3)) ** 2 + (PARIN(1)+2.0E+00*PARIN(3)) * (PARIN(2)-2.0E+00)
            B = (PARIN(2)-2.0E+00) ** 2 * (PARIN(2)-4.0E+00)
            VAR = 2.0E+00 * (PARIN(2)/PARIN(1)) ** 2 * (A/B)
        END IF
 
    ELSE IF (PDF == 'nor') THEN
 
        AV = PARIN (1)
        VAR = PARIN (2) ** 2
 
    ELSE IF (PDF == 'poi') THEN
 
        AV = PARIN (1)
        VAR = PARIN (1)
 
    ELSE IF (PDF == 'unf') THEN
 
        WIDTH = PARIN (2) - PARIN (1)
        AV = PARIN (1) + WIDTH / 2.0E+00
        VAR = WIDTH ** 2 / 12.0E+00
 
    ELSE
 
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRSTAT - Fatal error!'
        WRITE (*, '(a)') '  Illegal input value for PDF.'
        RETURN
 
    END IF
 
    RETURN
END

!------------------------------------------------------------------------------
!
!                               RNGLIB
!   A Package of Random Number Generators (RNG's) with Splitting Facilities
!
!------------------------------------------------------------------------------
! RNGLIB is a FORTRAN90 library which implements random number generators (RNG's)
!    which can generate one or more streams of random numbers.
! RNGLIB is a portable set of software tools for uniform random variate generation.
! It provides for multiple generators running simultaneously, and each generator has
! its sequence of numbers partitioned into many long disjoint substreams. Simple
! procedure calls allow the user to make any generator jump ahead to the beginning
! of its next substream.
!------------------------------------------------------------------------------

SUBROUTINE ADVANCE_STATE (K)
 
!*****************************************************************************80
!
!! ADVANCE_STATE advances the state of the current generator.
!
!  Discussion:
!
!    This procedure advances the state of the current generator by 2^K
!    values and resets the initial seed to that value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) K, indicates that the generator is to be
!    advanced by 2^K values.
!    0 <= K.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: A1 = 40014
    INTEGER (KIND=4), PARAMETER :: A2 = 40692
    INTEGER (KIND=4) B1
    INTEGER (KIND=4) B2
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) K
    INTEGER (KIND=4), PARAMETER :: M1 = 2147483563
    INTEGER (KIND=4), PARAMETER :: M2 = 2147483399
 
    IF (K < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ADVANCE_STATE - Fatal error!'
        WRITE (*, '(a)') '  Input exponent K is out of bounds.'
        RETURN
    END IF
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ADVANCE_STATE - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get the current generator index.
!
    G = CGN_GET ()
 
    B1 = A1
    B2 = A2
 
    DO I = 1, K
        B1 = MULTMOD (B1, B1, M1)
        B2 = MULTMOD (B2, B2, M2)
    END DO
 
    CALL CG_GET (G, CG1, CG2)
    CG1 = MULTMOD (B1, CG1, M1)
    CG2 = MULTMOD (B2, CG2, M2)
    CALL CG_SET (G, CG1, CG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ANTITHETIC_GET ()
 
!*****************************************************************************80
!
!! ANTITHETIC_GET queries the antithetic value for a given generator.
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
!    John Burkardt
!
!  Parameters:
!
!    Output, logical ANTITHETIC_GET, is TRUE if generator G is antithetic.
!
    IMPLICIT NONE
 
    LOGICAL ANTITHETIC_GET
    INTEGER (KIND=4) I
    LOGICAL VALUE
 
    I = - 1
    CALL ANTITHETIC_MEMORY (I, VALUE)
 
    ANTITHETIC_GET = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANTITHETIC_MEMORY (I, VALUE)
 
!*****************************************************************************80
!
!! ANTITHETIC_MEMORY stores the antithetic value for all generators.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input/output, logical VALUE.  For I = -1, VALUE is an output
!    quantity, for I = +1, an input quantity.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
 
    LOGICAL A_SAVE (G_MAX)
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    LOGICAL VALUE
 
    SAVE A_SAVE
 
    DATA A_SAVE / 32 * .FALSE. /
 
    IF (I < 0) THEN
        G = CGN_GET ()
        VALUE = A_SAVE (G)
    ELSE IF (I == 0) THEN
        A_SAVE (1:G_MAX) = .FALSE.
    ELSE IF (0 < I) THEN
        G = CGN_GET ()
        A_SAVE (G) = VALUE
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANTITHETIC_SET (VALUE)
 
!*****************************************************************************80
!
!! ANTITHETIC_SET sets the antithetic value for a given generator.
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
!    John Burkardt
!
!  Parameters:
!
!    Input, logical VALUE, is TRUE if generator G is to be antithetic.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    LOGICAL VALUE
 
    I = + 1
    CALL ANTITHETIC_MEMORY (I, VALUE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CG_GET (G, CG1, CG2)
 
!*****************************************************************************80
!
!! CG_GET queries the CG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
 
    I = - 1
    CALL CG_MEMORY (I, G, CG1, CG2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CG_MEMORY (I, G, CG1, CG2)
 
!*****************************************************************************80
!
!! CG_MEMORY stores the CG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) CG1, CG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the CG parameter for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
 
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG1_SAVE (G_MAX)
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) CG2_SAVE (G_MAX)
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
 
    SAVE CG1_SAVE
    SAVE CG2_SAVE
 
    DATA CG1_SAVE / 32 * 0 /
    DATA CG2_SAVE / 32 * 0 /
 
    IF (G < 1 .OR. G_MAX < G) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CG_MEMORY - Fatal error!'
        WRITE (*, '(a)') '  Input generator index G is out of bounds.'
        RETURN
    END IF
 
    IF (I < 0) THEN
        CG1 = CG1_SAVE (G)
        CG2 = CG2_SAVE (G)
    ELSE IF (I == 0) THEN
        CG1_SAVE (1:G_MAX) = 0
        CG2_SAVE (1:G_MAX) = 0
    ELSE IF (0 < I) THEN
        CG1_SAVE (G) = CG1
        CG2_SAVE (G) = CG2
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CG_SET (G, CG1, CG2)
 
!*****************************************************************************80
!
!! CG_SET sets the CG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
 
    I = + 1
    CALL CG_MEMORY (I, G, CG1, CG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CGN_GET ()
 
!*****************************************************************************80
!
!! CGN_GET gets the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CGN_GET, the current generator index.
!    1 <= CGN_GET <= 32.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) CGN_GET
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
 
    I = - 1
    CALL CGN_MEMORY (I, G)
 
    CGN_GET = G
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CGN_MEMORY (I, G)
 
!*****************************************************************************80
!
!! CGN_MEMORY stores the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get the value.
!    0, initialize the value.
!    1, set the value.
!
!    Input/output, integer ( kind = 4 ) G.  For I = -1 or 0,
!    this is output, for I = +1, this is input.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) G_SAVE
    INTEGER (KIND=4) I
 
    SAVE G_SAVE
 
    DATA G_SAVE / 1 /
 
    IF (I < 0) THEN
 
        G = G_SAVE
 
    ELSE IF (I == 0) THEN
 
        G_SAVE = 1
        G = G_SAVE
 
    ELSE IF (0 < I) THEN
 
        IF (G < 1 .OR. G_MAX < G) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CGN_MEMORY - Fatal error!'
            WRITE (*, '(a)') '  Generator index G is out of bounds.'
            RETURN
        END IF
 
        G_SAVE = G
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CGN_SET (G)
 
!*****************************************************************************80
!
!! CGN_SET sets the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
 
    I = + 1
    CALL CGN_MEMORY (I, G)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GET_STATE (CG1, CG2)
 
!*****************************************************************************80
!
!! GET_STATE returns the state of the current generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for the
!    current generator.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GET_STATE - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get the current generator index.
!
    G = CGN_GET ()
!
!  Retrieve the seed values for this generator.
!
    CALL CG_GET (G, CG1, CG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_UNI ()
 
!*****************************************************************************80
!
!! I4_UNI generates a random positive integer.
!
!  Discussion:
!
!    This procedure returns a random integer following a uniform distribution
!    over (1, 2147483562) using the current generator.
!
!    The original name of this function was "random()", but this conflicts
!    with a standard library function name in C.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) I4_UNI, the random integer.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: A1 = 40014
    INTEGER (KIND=4), PARAMETER :: A2 = 40692
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I4_UNI
    INTEGER (KIND=4) K
    INTEGER (KIND=4), PARAMETER :: M1 = 2147483563
    INTEGER (KIND=4), PARAMETER :: M2 = 2147483399
    LOGICAL VALUE
    INTEGER (KIND=4) Z
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_UNI - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get the current generator index.
!
    G = CGN_GET ()
!
!  Retrieve the seeds for the current generator.
!
    CALL CG_GET (G, CG1, CG2)
!
!  Update the seeds.
!
    K = CG1 / 53668
    CG1 = A1 * (CG1-K*53668) - K * 12211
 
    IF (CG1 < 0) THEN
        CG1 = CG1 + M1
    END IF
 
    K = CG2 / 52774
    CG2 = A2 * (CG2-K*52774) - K * 3791
 
    IF (CG2 < 0) THEN
        CG2 = CG2 + M2
    END IF
!
!  Store the updated seeds.
!
    CALL CG_SET (G, CG1, CG2)
!
!  Construct the random integer from the seeds.
!
    Z = CG1 - CG2
 
    IF (Z < 1) THEN
        Z = Z + M1 - 1
    END IF
!
!  If the generator is in antithetic mode, we must reflect the value.
!
    VALUE = ANTITHETIC_GET ()
 
    IF (VALUE) THEN
        Z = M1 - Z
    END IF
 
    I4_UNI = Z
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE IG_GET (G, IG1, IG2)
 
!*****************************************************************************80
!
!! IG_GET queries the IG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) IG1, IG2, the IG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG2
 
    I = - 1
    CALL IG_MEMORY (I, G, IG1, IG2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE IG_MEMORY (I, G, IG1, IG2)
 
!*****************************************************************************80
!
!! IG_MEMORY stores the IG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) IG1, IG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the IG parameter for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG1_SAVE (G_MAX)
    INTEGER (KIND=4) IG2
    INTEGER (KIND=4) IG2_SAVE (G_MAX)
 
    SAVE IG1_SAVE
    SAVE IG2_SAVE
 
    DATA IG1_SAVE / 32 * 0 /
    DATA IG2_SAVE / 32 * 0 /
 
    IF (G < 1 .OR. G_MAX < G) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'IG_MEMORY - Fatal error!'
        WRITE (*, '(a)') '  Input generator index G is out of bounds.'
        RETURN
    END IF
 
    IF (I < 0) THEN
        IG1 = IG1_SAVE (G)
        IG2 = IG2_SAVE (G)
    ELSE IF (I == 0) THEN
        IG1_SAVE (1:G_MAX) = 0
        IG2_SAVE (1:G_MAX) = 0
    ELSE IF (0 < I) THEN
        IG1_SAVE (G) = IG1
        IG2_SAVE (G) = IG2
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE IG_SET (G, IG1, IG2)
 
!*****************************************************************************80
!
!! IG_SET sets the IG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) IG1, IG2, the IG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG2
 
    I = + 1
    CALL IG_MEMORY (I, G, IG1, IG2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INIT_GENERATOR (T)
 
!*****************************************************************************80
!
!! INIT_GENERATOR sets the current generator to initial, last or new seed.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) T, the seed type:
!    0, use the seed chosen at initialization time.
!    1, use the last seed.
!    2, use a new seed set 2^30 values away.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: A1_W = 1033780774
    INTEGER (KIND=4), PARAMETER :: A2_W = 1494757890
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG2
    INTEGER (KIND=4) LG1
    INTEGER (KIND=4) LG2
    INTEGER (KIND=4), PARAMETER :: M1 = 2147483563
    INTEGER (KIND=4), PARAMETER :: M2 = 2147483399
    INTEGER (KIND=4) T
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'INIT_GENERATOR - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get the current generator index.
!
    G = CGN_GET ()
!
!  0: restore the initial seed.
!
    IF (T == 0) THEN
 
        CALL IG_GET (G, IG1, IG2)
        LG1 = IG1
        LG2 = IG2
        CALL LG_SET (G, LG1, LG2)
!
!  1: restore the last seed.
!
    ELSE IF (T == 1) THEN
 
        CALL LG_GET (G, LG1, LG2)
!
!  2: advance to a new seed.
!
    ELSE IF (T == 2) THEN
 
        CALL LG_GET (G, LG1, LG2)
        LG1 = MULTMOD (A1_W, LG1, M1)
        LG2 = MULTMOD (A2_W, LG2, M2)
        CALL LG_SET (G, LG1, LG2)
 
    ELSE
 
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'INIT_GENERATOR - Fatal error!'
        WRITE (*, '(a)') '  Input parameter T out of bounds.'
        RETURN
 
    END IF
!
!  Store the new seed.
!
    CG1 = LG1
    CG2 = LG2
    CALL CG_SET (G, CG1, CG2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INITIALIZE ()
 
!*****************************************************************************80
!
!! INITIALIZE initializes the random number generator library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    None
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG2
    LOGICAL VALUE
!
!  Remember that we have called INITIALIZE().
!
    CALL INITIALIZED_SET ()
!
!  Initialize all generators to have FALSE antithetic value.
!
    VALUE = .FALSE.
    DO G = 1, G_MAX
        CALL CGN_SET (G)
        CALL ANTITHETIC_SET (VALUE)
    END DO
!
!  Set the initial seeds.
!
    IG1 = 1234567890
    IG2 = 123456789
    CALL SET_INITIAL_SEED (IG1, IG2)
!
!  Initialize the current generator index to the first one.
!
    G = 1
    CALL CGN_SET (G)
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'INITIALIZE - Note:'
    WRITE (*, '(a)') '  The RNGLIB package has been initialized.'
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION INITIALIZED_GET ()
 
!*****************************************************************************80
!
!! INITIALIZED_GET queries the INITIALIZED value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, logical INITIALIZED_GET, is TRUE if the package has
!    been initialized.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    LOGICAL INITIALIZED
    LOGICAL INITIALIZED_GET
 
    I = - 1
    CALL INITIALIZED_MEMORY (I, INITIALIZED)
 
    INITIALIZED_GET = INITIALIZED
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INITIALIZED_MEMORY (I, INITIALIZED)
 
!*****************************************************************************80
!
!! INITIALIZED_MEMORY stores the INITIALIZED value for the package.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get the value.
!    0, initialize the value.
!    1, set the value.
!
!    Input/output, logical INITIALIZED.  For I = -1,
!    this is output, for I = +1, this is input, for I = 0,
!    this argument is ignored.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    LOGICAL INITIALIZED
    LOGICAL INITIALIZED_SAVE
 
    SAVE INITIALIZED_SAVE
 
    DATA INITIALIZED_SAVE / .FALSE. /
 
    IF (I < 0) THEN
        INITIALIZED = INITIALIZED_SAVE
    ELSE IF (I == 0) THEN
        INITIALIZED_SAVE = .FALSE.
    ELSE IF (0 < I) THEN
        INITIALIZED_SAVE = INITIALIZED
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INITIALIZED_SET ()
 
!*****************************************************************************80
!
!! INITIALIZED_SET sets the INITIALIZED value true.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    LOGICAL INITIALIZED
 
    I = + 1
    INITIALIZED = .TRUE.
    CALL INITIALIZED_MEMORY (I, INITIALIZED)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LG_GET (G, LG1, LG2)
 
!*****************************************************************************80
!
!! LG_GET queries the LG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Output, integer ( kind = 4 ) LG1, LG2, the LG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) LG1
    INTEGER (KIND=4) LG2
 
    I = - 1
    CALL LG_MEMORY (I, G, LG1, LG2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LG_MEMORY (I, G, LG1, LG2)
 
!*****************************************************************************80
!
!! LG_MEMORY stores the LG values for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) LG1, LG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the LG parameter for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) LG1
    INTEGER (KIND=4) LG1_SAVE (G_MAX)
    INTEGER (KIND=4) LG2
    INTEGER (KIND=4) LG2_SAVE (G_MAX)
 
    SAVE LG1_SAVE
    SAVE LG2_SAVE
 
    DATA LG1_SAVE / 32 * 0 /
    DATA LG2_SAVE / 32 * 0 /
 
    IF (G < 1 .OR. G_MAX < G) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LG_MEMORY - Fatal error!'
        WRITE (*, '(a)') '  Input generator index G is out of bounds.'
        RETURN
    END IF
 
    IF (I < 0) THEN
        LG1 = LG1_SAVE (G)
        LG2 = LG2_SAVE (G)
    ELSE IF (I == 0) THEN
        LG1_SAVE (1:G_MAX) = 0
        LG2_SAVE (1:G_MAX) = 0
    ELSE IF (0 < I) THEN
        LG1_SAVE (G) = LG1
        LG2_SAVE (G) = LG2
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LG_SET (G, LG1, LG2)
 
!*****************************************************************************80
!
!! LG_SET sets the LG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
!    Input, integer ( kind = 4 ) LG1, LG2, the LG values for generator G.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4) LG1
    INTEGER (KIND=4) LG2
 
    I = + 1
    CALL LG_MEMORY (I, G, LG1, LG2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION MULTMOD (A, S, M)
 
!*****************************************************************************80
!
!! MULTMOD carries out modular multiplication.
!
!  Discussion:
!
!    This procedure returns
!
!      ( A * S ) mod M
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, S, M, the arguments.
!
!    Output, integer ( kind = 4 ) MULTMOD, the value of the product of A and S,
!    modulo M.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) A0
    INTEGER (KIND=4) A1
    INTEGER (KIND=4), PARAMETER :: H = 32768
    INTEGER (KIND=4) K
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MULTMOD
    INTEGER (KIND=4) P
    INTEGER (KIND=4) Q
    INTEGER (KIND=4) QH
    INTEGER (KIND=4) RH
    INTEGER (KIND=4) S
 
    IF (A <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTMOD - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        RETURN
    END IF
 
    IF (M <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTMOD - Fatal error!'
        WRITE (*, '(a)') '  M <= A.'
        RETURN
    END IF
 
    IF (S <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTMOD - Fatal error!'
        WRITE (*, '(a)') '  S <= 0.'
        RETURN
    END IF
 
    IF (M <= S) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTMOD - Fatal error!'
        WRITE (*, '(a)') '  M <= S.'
        RETURN
    END IF
 
    IF (A < H) THEN
 
        A0 = A
        P = 0
 
    ELSE
 
        A1 = A / H
        A0 = A - H * A1
        QH = M / H
        RH = M - H * QH
 
        IF (H <= A1) THEN
 
            A1 = A1 - H
            K = S / QH
            P = H * (S-K*QH) - K * RH
 
            DO WHILE (P <  0)
                P = P + M
            END DO
 
        ELSE
 
            P = 0
 
        END IF
 
        IF (A1 /= 0) THEN
 
            Q = M / A1
            K = S / Q
            P = P - K * (M-A1*Q)
 
            IF (0 < P) THEN
                P = P - M
            END IF
 
            P = P + A1 * (S-K*Q)
 
            DO WHILE (P <  0)
                P = P + M
            END DO
 
        END IF
 
        K = P / QH
        P = H * (P-K*QH) - K * RH
 
        DO WHILE (P <  0)
            P = P + M
        END DO
 
    END IF
 
    IF (A0 /= 0) THEN
 
        Q = M / A0
        K = S / Q
        P = P - K * (M-A0*Q)
 
        IF (0 < P) THEN
            P = P - M
        END IF
 
        P = P + A0 * (S-K*Q)
 
        DO WHILE (P <  0)
            P = P + M
        END DO
 
    END IF
 
    MULTMOD = P
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R4_UNI_01 ()
 
!*****************************************************************************80
!
!! R4_UNI_01 returns a uniform random real number in [0,1].
!
!  Discussion:
!
!    This procedure returns a random floating point number from a uniform
!    distribution over (0,1), not including the endpoint values, using the
!    current random number generator.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, real ( kind = 4 ) R4_UNI_01, a uniform random value in [0,1].
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    REAL (KIND=4) R4_UNI_01
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R4_UNI_01 - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get a random positive integer.
!
    I = I4_UNI ()
!
!  Scale it to a random real in [0,1].
!
    R4_UNI_01 = REAL (I, KIND=4) * 4.656613057E-10
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNI_01 ()
 
!*****************************************************************************80
!
!! R8_UNI_01 returns a uniform random double precision number in [0,1].
!
!  Discussion:
!
!    This procedure returns a random floating point number from a uniform
!    distribution over (0,1), not including the endpoint values, using the
!    current random number generator.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) R8_UNI_01, a uniform random value in [0,1].
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    REAL (KIND=8) R8_UNI_01
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_UNI_01 - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Get a random positive integer.
!
    I = I4_UNI ()
!
!  Scale it to a random real in [0,1].
!
    R8_UNI_01 = REAL (I, KIND=8) * 4.656613057D-10
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SET_INITIAL_SEED (IG1, IG2)
 
!*****************************************************************************80
!
!! SET_INITIAL_SEED resets the initial seed and state for all generators.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
!
!  Author:
!
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IG1, IG2, the initial seed values
!    for the first generator.
!    1 <= IG1 < 2147483563
!    1 <= IG2 < 2147483399
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: A1_VW = 2082007225
    INTEGER (KIND=4), PARAMETER :: A2_VW = 784306273
    INTEGER (KIND=4) G
    INTEGER (KIND=4), PARAMETER :: G_MAX = 32
    INTEGER (KIND=4) IG1
    INTEGER (KIND=4) IG2
    INTEGER (KIND=4), PARAMETER :: M1 = 2147483563
    INTEGER (KIND=4), PARAMETER :: M2 = 2147483399
    INTEGER (KIND=4) T
 
    IF (IG1 < 1 .OR. M1 <= IG1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_INITIAL_SEED - Fatal error!'
        WRITE (*, '(a)') '  Input parameter IG1 out of bounds.'
        RETURN
    END IF
 
    IF (IG2 < 1 .OR. M2 <= IG2) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_INITIAL_SEED - Fatal error!'
        WRITE (*, '(a)') '  Input parameter IG2 out of bounds.'
        RETURN
    END IF
!
!  Because INITIALIZE calls SET_INITIAL_SEED, it's not easy to correct
!  the error that arises if SET_INITIAL_SEED is called before INITIALIZE.
!  So don't bother trying.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_INITIAL_SEED - Fatal error!'
        WRITE (*, '(a)') '  The RNGLIB package has not been initialized.'
        RETURN
    END IF
!
!  Set the initial seed, then initialize the first generator.
!
    G = 1
    CALL CGN_SET (G)
 
    CALL IG_SET (G, IG1, IG2)
 
    T = 0
    CALL INIT_GENERATOR (T)
!
!  Now do similar operations for the other generators.
!
    DO G = 2, G_MAX
 
        CALL CGN_SET (G)
        IG1 = MULTMOD (A1_VW, IG1, M1)
        IG2 = MULTMOD (A2_VW, IG2, M2)
        CALL IG_SET (G, IG1, IG2)
        CALL INIT_GENERATOR (T)
 
    END DO
!
!  Now choose the first generator.
!
    G = 1
    CALL CGN_SET (G)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SET_SEED (CG1, CG2)
 
!*****************************************************************************80
!
!! SET_SEED resets the initial seed and state of the current generator.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!    1 <= CG1 < 2147483563
!    1 <= CG2 < 2147483399
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) CG1
    INTEGER (KIND=4) CG2
    INTEGER (KIND=4) G
    INTEGER (KIND=4), PARAMETER :: M1 = 2147483563
    INTEGER (KIND=4), PARAMETER :: M2 = 2147483399
    INTEGER (KIND=4) T
 
    IF (CG1 < 1 .OR. M1 <= CG1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_SEED - Fatal error!'
        WRITE (*, '(a)') '  Input parameter CG1 out of bounds.'
        RETURN
    END IF
 
    IF (CG2 < 1 .OR. M2 <= CG2) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_SEED - Fatal error!'
        WRITE (*, '(a)') '  Input parameter CG2 out of bounds.'
        RETURN
    END IF
!
!  Check whether the package must be initialized.
!
    IF ( .NOT. INITIALIZED_GET()) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SET_SEED - Note:'
        WRITE (*, '(a)') '  Initializing RNGLIB package.'
        CALL INITIALIZE ()
    END IF
!
!  Retrieve the current generator index.
!
    G = CGN_GET ()
!
!  Set the seeds.
!
    CALL CG_SET (G, CG1, CG2)
!
!  Initialize the generator.
!
    T = 0
    CALL INIT_GENERATOR (T)
 
    RETURN
END
 
!******************************************************************************

END MODULE ModLib_RanLib

!******************************************************************************

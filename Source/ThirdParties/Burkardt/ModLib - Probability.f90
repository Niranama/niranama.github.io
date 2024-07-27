
MODULE ModLib_Probability
       
!** PURPOSE OF THIS MODULE:
    ! contains routines that  evaluates Probability Density Functions (PDF's) and produces
    ! random samples from them, including beta, binomial, chi, exponential, gamma, inverse chi,
    ! inverse gamma, multinomial, normal, scaled inverse chi, and uniform.

!** REFERENCES:
    ! These routines are from PROB package by John Burkardt

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_Probability'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
 
SUBROUTINE ANGLE_CDF (X, N, CDF)
 
!*****************************************************************************80
!
!! ANGLE_CDF evaluates the Angle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation and Sensitivity of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!    N must be at least 2.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) N
    REAL (KIND=8) N_REAL
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: ZERO = 0.0D+00
 
    IF (N < 2) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ANGLE_CDF - Fatal error!'
        WRITE (*, '(a)') '  N must be at least 2.'
        WRITE (*, '(a,i8)') '  The input value of N = ', N
        RETURN
    END IF
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (R8_PI <= X) THEN
        CDF = 1.0D+00
    ELSE IF (N == 2) THEN
        CDF = X / R8_PI
    ELSE
        N_REAL = REAL (N, KIND=8)
        CDF = SIN_POWER_INT (ZERO, X, N-2) * GAMMA (N_REAL/2.0D+00) / &
       & (SQRT(R8_PI)*GAMMA((N_REAL-1.0D+00)/2.0D+00))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLE_MEAN (N, MEAN)
 
!*****************************************************************************80
!
!! ANGLE_MEAN returns the mean of the Angle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!    N must be at least 2.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) N
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    MEAN = R8_PI / 2.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLE_PDF (X, N, PDF)
 
!*****************************************************************************80
!
!! ANGLE_PDF evaluates the Angle PDF.
!
!  Discussion:
!
!    X is an angle between 0 and PI, corresponding to the angle
!    made in an N-dimensional space, between a fixed line passing
!    through the origin, and an arbitrary line that also passes
!    through the origin, which is specified by a choosing any point
!    on the N-dimensional sphere with uniform probability.
!
!    The formula is
!
!      PDF(X) = ( sin ( X ) )^(N-2) * Gamma ( N / 2 )
!               / ( sqrt ( PI ) * Gamma ( ( N - 1 ) / 2 ) )
!
!      PDF(X) = 1 / PI if N = 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Reuven Rubinstein,
!    Monte Carlo Optimization, Simulation and Sensitivity of Queueing Networks,
!    Krieger, 1992,
!    ISBN: 0894647644,
!    LC: QA298.R79
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!    N must be at least 2.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (N < 2) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ANGLE_PDF - Fatal error!'
        WRITE (*, '(a)') '  N must be at least 2.'
        WRITE (*, '(a,i8)') '  The input value of N = ', N
        RETURN
    END IF
 
    IF (X < 0.0D+00 .OR. R8_PI < X) THEN
        PDF = 0.0D+00
    ELSE IF (N == 2) THEN
        PDF = 1.0D+00 / R8_PI
    ELSE
        PDF = (SIN(X)) ** (N-2) * GAMMA (REAL(N, KIND=8)/2.0D+00) / &
       & (SQRT(R8_PI)*GAMMA(REAL(N-1, KIND=8)/2.0D+00))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_CDF (X, CDF)
 
!*****************************************************************************80
!
!! ANGLIT_CDF evaluates the Anglit CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <-0.25D+00*R8_PI) THEN
        CDF = 0.0D+00
    ELSE IF (X < 0.25D+00*R8_PI) THEN
        CDF = 0.5D+00 - 0.5D+00 * COS (2.0D+00*X+R8_PI/2.0D+00)
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! ANGLIT_CDF_INV inverts the Anglit CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ANGLIT_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = 0.5D+00 * (ACOS(1.0D+00-2.0D+00*CDF)-R8_PI/2.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_MEAN (MEAN)
 
!*****************************************************************************80
!
!! ANGLIT_MEAN returns the mean of the Anglit PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_PDF (X, PDF)
 
!*****************************************************************************80
!
!! ANGLIT_PDF evaluates the Anglit PDF.
!
!  Discussion:
!
!    PDF(X) = sin ( 2 * X + PI / 2 ) for -PI/4 <= X <= PI/4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <=-0.25D+00*R8_PI .OR. 0.25D+00*R8_PI <= X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = SIN (2.0D+00*X+0.25D+00*R8_PI)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! ANGLIT_SAMPLE samples the Anglit PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL ANGLIT_CDF_INV (CDF, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ANGLIT_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! ANGLIT_VARIANCE returns the variance of the Anglit PDF.
!
!  Discussion:
!
!    Variance =
!      Integral ( -PI/4 <= X <= PI/4 ) X^2 * sin ( 2 * X + PI / 2 )
!
!    Antiderivative =
!      0.5D+00 * X * sin ( 2 * X + PI / 2 )
!      + ( 0.25 - 0.5D+00 * X^2 ) * cos ( 2 * X + PI / 2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 0.0625D+00 * R8_PI * R8_PI - 0.5D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! ARCSIN_CDF evaluates the Arcsin CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <=-A) THEN
        CDF = 0.0D+00
    ELSE IF (X < A) THEN
        CDF = 0.5D+00 + ASIN (X/A) / R8_PI
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! ARCSIN_CDF_INV inverts the Arcsin CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ARCSIN_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A * SIN (R8_PI*(CDF-0.5D+00))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ARCSIN_CHECK (A)
 
!*****************************************************************************80
!
!! ARCSIN_CHECK checks the parameter of the Arcsin CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, logical ARCSIN_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL ARCSIN_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ARCSIN_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        ARCSIN_CHECK = .FALSE.
        RETURN
    END IF
 
    ARCSIN_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! ARCSIN_MEAN returns the mean of the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! ARCSIN_PDF evaluates the Arcsin PDF.
!
!  Discussion:
!
!    The LOGISTIC EQUATION has the form:
!
!      X(N+1) = 4 * LAMBDA * ( 1 - X(N) ).
!
!    where 0 < LAMBDA <= 1.  This nonlinear difference equation maps
!    the unit interval into itself, and is a simple example of a system
!    exhibiting chaotic behavior.  Ulam and von Neumann studied the
!    logistic equation with LAMBDA = 1, and showed that iterates of the
!    function generated a sequence of pseudorandom numbers with
!    the Arcsin probability density function.
!
!    The derived sequence
!
!      Y(N) = ( 2 / PI ) * Arcsin ( SQRT ( X(N) ) )
!
!    is a pseudorandom sequence with the uniform probability density
!    function on [0,1].  For certain starting values, such as X(0) = 0, 0.75,
!    or 1.0D+00, the sequence degenerates into a constant sequence, and for
!    values very near these, the sequence takes a while before becoming
!    chaotic.
!
!    The formula is:
!
!      PDF(X) = 1 / ( pi * sqrt ( A^2 - X^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall/CRC, 2000, pages 114-115.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    -A < X < A.
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ARCSIN_PDF - Fatal error!'
        WRITE (*, '(a)') '  Parameter A must be positive.'
        RETURN
    END IF
 
    IF (X <=-A .OR. A <= X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 1.0D+00 / (R8_PI*SQRT(A*A-X*X))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! ARCSIN_SAMPLE samples the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL ARCSIN_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ARCSIN_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! ARCSIN_VARIANCE returns the variance of the Arcsin PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the CDF.
!    A must be positive.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = A * A / 2.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BENFORD_CDF (X, CDF)
 
!*****************************************************************************80
!
!! BENFORD_CDF returns the Benford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the string of significant digits to be
!    checked.  If X is 1, then we are asking for the Benford probability that
!    a value will have first digit 1.  If X is 123, we are asking for
!    the probability that the first three digits will be 123, and so on.
!
!    Output, real ( kind = 8 ) CDF, the Benford probability that an item taken
!    from a real world distribution will have the initial digit X or less.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X <= 0) THEN
        CDF = 0.0D+00
    ELSE IF (I4_IS_POWER_OF_10(X+1)) THEN
        CDF = 1.0D+00
    ELSE
        CDF = LOG10 (REAL(X+1, KIND=8))
        CDF = MOD (CDF, 1.0D+00)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BENFORD_PDF (X, PDF)
 
!*****************************************************************************80
!
!! BENFORD_PDF returns the Benford PDF.
!
!  Discussion:
!
!    Benford's law is an empirical formula explaining the observed
!    distribution of initial digits in lists culled from newspapers,
!    tax forms, stock market prices, and so on.  It predicts the observed
!    high frequency of the initial digit 1, for instance.
!
!    Note that the probabilities of digits 1 through 9 are guaranteed
!    to add up to 1, since
!      LOG10 ( 2/1 ) + LOG10 ( 3/2) + LOG10 ( 4/3 ) + ... + LOG10 ( 10/9 )
!      = LOG10 ( 2/1 * 3/2 * 4/3 * ... * 10/9 ) = LOG10 ( 10 ) = 1.
!
!    The formula is:
!
!      PDF(X) = LOG10 ( ( X + 1 ) / X ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Frank Benford,
!    The Law of Anomalous Numbers,
!    Proceedings of the American Philosophical Society,
!    Volume 78, pages 551-572, 1938.
!
!    Ted Hill,
!    The First Digit Phenomenon,
!    American Scientist,
!    Volume 86, July/August 1998, pages 358 - 363.
!
!    Ralph Raimi,
!    The Peculiar Distribution of First Digits,
!    Scientific American,
!    December 1969, pages 109-119.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the string of significant digits to be
!    checked.  If X is 1, then we are asking for the Benford probability that
!    a value will have first digit 1.  If X is 123, we are asking for
!    the probability that the first three digits will be 123, and so on.
!
!    Output, real ( kind = 8 ) PDF, the Benford probability that an item taken
!    from a real world distribution will have the initial digits X.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X <= 0) THEN
        PDF = 0.0D+00
    ELSE
        PDF = LOG10 (REAL(X+1, KIND=8)/REAL(X, KIND=8))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BESSEL_IX_VALUES (N_DATA, NU, X, FX)
 
!*****************************************************************************80
!
!! BESSEL_IX_VALUES returns some values of the Ix Bessel function.
!
!  Discussion:
!
!    This set of data considers the less common case in which the
!    index of the Bessel function In is actually not an integer.
!    We may suggest this case by occasionally replacing the symbol
!    "In" by "Ix".
!
!    The modified Bessel functions In(Z) and Kn(Z) are solutions of
!    the differential equation
!
!      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselI[n,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) NU, the order of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 28
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.3592084175833614D+00, &
   & 0.9376748882454876D+00, 2.046236863089055D+00, 3.053093538196718D+00, &
   & 4.614822903407601D+00, 26.47754749755907D+00, 2778.784603874571D+00, &
   & 4.327974627242893D+07, 0.2935253263474798D+00, 1.099473188633110D+00, &
   & 21.18444226479414D+00, 2500.906154942118D+00, 2.866653715931464D+20, &
   & 0.05709890920304825D+00, 0.3970270801393905D+00, 13.76688213868258D+00, &
   & 2028.512757391936D+00, 2.753157630035402D+20, 0.4139416015642352D+00, &
   & 1.340196758982897D+00, 22.85715510364670D+00, 2593.006763432002D+00, &
   & 2.886630075077766D+20, 0.03590910483251082D+00, 0.2931108636266483D+00, &
   & 11.99397010023068D+00, 1894.575731562383D+00, 2.716911375760483D+20 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) NU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: NU_VEC = (/ 0.50D+00, 0.50D+00, 0.50D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 1.50D+00, 1.50D+00, 1.50D+00, 1.50D+00, &
   & 1.50D+00, 2.50D+00, 2.50D+00, 2.50D+00, 2.50D+00, 2.50D+00, 1.25D+00, 1.25D+00, 1.25D+00, &
   & 1.25D+00, 1.25D+00, 2.75D+00, 2.75D+00, 2.75D+00, 2.75D+00, 2.75D+00 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.2D+00, 1.0D+00, 2.0D+00, 2.5D+00, &
   & 3.0D+00, 5.0D+00, 10.0D+00, 20.0D+00, 1.0D+00, 2.0D+00, 5.0D+00, 10.0D+00, 50.0D+00, &
   & 1.0D+00, 2.0D+00, 5.0D+00, 10.0D+00, 50.0D+00, 1.0D+00, 2.0D+00, 5.0D+00, 10.0D+00, &
   & 50.0D+00, 1.0D+00, 2.0D+00, 5.0D+00, 10.0D+00, 50.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        NU = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        NU = NU_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BIRTHDAY_CDF (N, CDF)
 
!*****************************************************************************80
!
!! BIRTHDAY_CDF returns the Birthday Concurrence CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of people whose birthdays have
!    been disclosed.
!
!    Output, real ( kind = 8 ) CDF, the probability that at least
!    two of the N people have matching birthays.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
 
    IF (N < 1) THEN
        CDF = 0.0D+00
        RETURN
    ELSE IF (365 < N) THEN
        CDF = 1.0D+00
        RETURN
    END IF
!
!  Compute the probability that N people have distinct birthdays.
!
    CDF = 1.0D+00
    DO I = 1, N
        CDF = CDF * REAL (365+1-I, KIND=8) / 365.0D+00
    END DO
!
!  Compute the probability that it is NOT the case that N people
!  have distinct birthdays.  This is the cumulative probability
!  that person 2 matches person 1, or person 3 matches 1 or 2,
!  etc.
!
    CDF = 1.0D+00 - CDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BIRTHDAY_CDF_INV (CDF, N)
 
!*****************************************************************************80
!
!! BIRTHDAY_CDF_INV inverts the Birthday Concurrence CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the probability that at least
!    two of the N people have matching birthays.
!
!    Output, integer ( kind = 4 ) N, the corresponding number of people whose
!    birthdays need to be disclosed.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF_NOT
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
 
    IF (CDF <= 0.0D+00) THEN
        N = 1
        RETURN
    ELSE IF (1.0D+00 <= CDF) THEN
        N = 365
        RETURN
    END IF
!
!  Compute the probability that N people have distinct birthdays.
!
    CDF_NOT = 1.0D+00
 
    DO I = 1, 365
        CDF_NOT = CDF_NOT * REAL (365+1-I, KIND=8) / 365.0D+00
        IF (CDF <= 1.0D+00-CDF_NOT) THEN
            N = I
            RETURN
        END IF
    END DO
 
    N = 365
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BIRTHDAY_PDF (N, PDF)
 
!*****************************************************************************80
!
!! BIRTHDAY_PDF returns the Birthday Concurrence PDF.
!
!  Discussion:
!
!    The probability is the probability that the N-th person is the
!    first one to match a birthday with someone earlier.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of people whose birthdays have
!    been disclosed.
!
!    Output, real ( kind = 8 ) PDF, the probability that the N-th person
!    is the first to match a birthday with someone earlier.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
 
    IF (N < 1 .OR. 365 < N) THEN
        PDF = 0.0D+00
        RETURN
    END IF
 
    PDF = 1.0D+00
!
!  Compute the probability that N-1 people have distinct birthdays.
!
    DO I = 1, N - 1
        PDF = PDF * REAL (365+1-I, KIND=8) / 365.0D+00
    END DO
!
!  Compute the probability that person N has one of those N-1 birthdays.
!
    PDF = PDF * REAL (N-1, KIND=8) / 365.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BIRTHDAY_SAMPLE (N, SEED, VALUE)
 
!*****************************************************************************80
!
!! BIRTHDAY_SAMPLE samples the Birthday Concurrence PDF.
!
!  Discussion:
!
!    The probability is the probability that the N-th person is the
!    first one to match a birthday with someone earlier.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of people whose birthdays have been
!    disclosed.
!
!    Input, integer SEED, a seed for the random number generator.
!
!    Output, integer VALUE,
!    * 1 if the first N-1 people had distinct
!      birthdays, but person N had a birthday in common with a previous person,
!    * 0 otherwise.
!
!    Output, integer SEED, a seed for the random number generator.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), ALLOCATABLE :: B (:)
    INTEGER (KIND=4) N
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) U1
    INTEGER (KIND=4) U2
    INTEGER (KIND=4) VALUE
 
    IF (N < 1) THEN
        VALUE = 0
        RETURN
    END IF
!
!  Choose N birthdays at random.
!
    ALLOCATE (B(1:N))
 
    CALL I4VEC_UNIFORM_AB (N, 1, 365, SEED, B)
!
!  Are the first N-1 birthdays unique?
!
    CALL I4VEC_UNIQUE_COUNT (N-1, B(1:N-1), U1)
 
    IF (U1 < N-1) THEN
        VALUE = 0
        RETURN
    END IF
!
!  Does the N-th birthday match an earlier one?
!
    CALL I4VEC_UNIQUE_COUNT (N, B(1:N), U2)
 
    IF (U2 == N-1) THEN
        VALUE = 1
    ELSE
        VALUE = 0
    END IF
 
    DEALLOCATE (B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! BERNOULLI_CDF evaluates the Bernoulli CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of successes on a single trial.
!    X = 0 or 1.
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        CDF = 0.0D+00
    ELSE IF (X == 0) THEN
        CDF = 1.0D+00 - A
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! BERNOULLI_CDF_INV inverts the Bernoulli CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 <= A <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BERNOULLI_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF <= 1.0D+00-A) THEN
        X = 0
    ELSE
        X = 1
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BERNOULLI_CHECK (A)
 
!*****************************************************************************80
!
!! BERNOULLI_CHECK checks the parameter of the Bernoulli CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 <= A <= 1.0.
!
!    Output, logical BERNOULLI_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL BERNOULLI_CHECK
 
    IF (A < 0.0D+00 .OR. 1.0D+00 < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BERNOULLI_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 0 or 1 < A.'
        BERNOULLI_CHECK = .FALSE.
        RETURN
    END IF
 
    BERNOULLI_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! BERNOULLI_MEAN returns the mean of the Bernoulli PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! BERNOULLI_PDF evaluates the Bernoulli PDF.
!
!  Discussion:
!
!    PDF(A;X) = A^X * ( 1 - A )^( X - 1 )
!
!    X = 0 or 1.
!
!    The Bernoulli PDF describes the simple case in which a single trial
!    is carried out, with two possible outcomes, called "success" and
!    "failure"; the probability of success is A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of successes on a single trial.
!    X = 0 or 1.
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        PDF = 0.0D+00
    ELSE IF (X == 0) THEN
        PDF = 1.0D+00 - A
    ELSE IF (X == 1) THEN
        PDF = A
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! BERNOULLI_SAMPLE samples the Bernoulli PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL BERNOULLI_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! BERNOULLI_VARIANCE returns the variance of the Bernoulli PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = A * (1.0D+00-A)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BESSEL_I0 (ARG)
 
!*****************************************************************************80
!
!! BESSEL_I0 evaluates the modified Bessel function I0(X).
!
!  Discussion:
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards, Chalk
!    River (Atomic Energy of Canada Limited) Report AECL-4928,
!    October, 1974.  This transportable program is patterned after
!    the machine dependent FUNPACK packet NATSI0, but cannot match
!    that version for efficiency or accuracy.  This version uses
!    rational functions that theoretically approximate I-SUB-0(X)
!    to at least 18 significant decimal digits.
!
!  Machine dependent constants:
!
!    beta   = Radix for the floating-point system
!    maxexp = Smallest power of beta that overflows
!    XMAX =   Largest argument acceptable to BESI0;  Solution to
!             equation:
!               W(X) * (1+1/(8*X)+9/(128*X^2) = beta^maxexp
!             where  W(X) = EXP(X)/sqrt(2*PI*X)
!
!    Approximate values for some important machines are:
!
!                             beta       maxexp       XMAX
!
!    CRAY-1        (S.P.)       2         8191       5682.810
!    Cyber 180/855
!      under NOS   (S.P.)       2         1070        745.893
!    IEEE (IBM/XT,
!      SUN, etc.)  (S.P.)       2          128         91.900
!    IEEE (IBM/XT,
!      SUN, etc.)  (D.P.)       2         1024        713.986
!    IBM 3033      (D.P.)      16           63        178.182
!    VAX           (S.P.)       2          127         91.203
!    VAX D-Format  (D.P.)       2          127         91.203
!    VAX G-Format  (D.P.)       2         1023        713.293
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.
!
!    Output, real ( kind = 8 ) BESSEL_I0, the value of the modified
!    Bessel function of the first kind.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ARG
    REAL (KIND=8) B
    REAL (KIND=8) BESSEL_I0
    REAL (KIND=8), PARAMETER :: EXP40 = 2.353852668370199854D+17
    INTEGER (KIND=4) I
    REAL (KIND=8), PARAMETER, DIMENSION (15) :: P = (/ - 5.2487866627945699800D-18, - &
   & 1.5982226675653184646D-14, - 2.6843448573468483278D-11, - 3.0517226450451067446D-08, - &
   & 2.5172644670688975051D-05, - 1.5453977791786851041D-02, - 7.0935347449210549190D+00, - &
   & 2.4125195876041896775D+03, - 5.9545626019847898221D+05, - 1.0313066708737980747D+08, - &
   & 1.1912746104985237192D+10, - 8.4925101247114157499D+11, - 3.2940087627407749166D+13, - &
   & 5.5050369673018427753D+14, - 2.2335582639474375249D+15 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: PP = (/ - 3.9843750000000000000D-01, &
   & 2.9205384596336793945D+00, - 2.4708469169133954315D+00, 4.7914889422856814203D-01, - &
   & 3.7384991926068969150D-03, - 2.6801520353328635310D-03, 9.9168777670983678974D-05, - &
   & 2.1877128189032726730D-06 /)
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: Q = (/ - 3.7277560179962773046D+03, &
   & 6.5158506418655165707D+06, - 6.5626560740833869295D+09, 3.7604188704092954661D+12, - &
   & 9.7087946179594019126D+14 /)
    REAL (KIND=8), PARAMETER, DIMENSION (7) :: QQ = (/ - 3.1446690275135491500D+01, &
   & 8.5539563258012929600D+01, - 6.0228002066743340583D+01, 1.3982595353892851542D+01, - &
   & 1.1151759188741312645D+00, 3.2547697594819615062D-02, - 5.5194330231005480228D-04 /)
    REAL (KIND=8), PARAMETER :: REC15 = 6.6666666666666666666D-02
    REAL (KIND=8) SUMP
    REAL (KIND=8) SUMQ
    REAL (KIND=8) VALUE
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XMAX = 91.9D+00
    REAL (KIND=8) XX
 
    X = ABS (ARG)
 
    IF (X < EPSILON(ARG)) THEN
        VALUE = 1.0D+00
    ELSE IF (X < 15.0D+00) THEN
!
!  EPSILON ( ARG ) <= ABS(ARG) < 15.0D+00
!
        XX = X * X
        SUMP = P (1)
        DO I = 2, 15
            SUMP = SUMP * XX + P (I)
        END DO
 
        XX = XX - 225.0D+00
        SUMQ = ((((XX+Q(1))*XX+Q(2))*XX+Q(3))*XX+Q(4)) * XX + Q (5)
 
        VALUE = SUMP / SUMQ
 
    ELSE IF (15.0D+00 <= X) THEN
 
        IF (XMAX < X) THEN
            VALUE = HUGE (VALUE)
        ELSE
!
!  15.0D+00 <= ABS(ARG)
!
            XX = 1.0D+00 / X - REC15
 
            SUMP = ((((((PP(1)*XX+PP(2))*XX+PP(3))*XX+PP(4))*XX+PP(5))*XX+PP(6))*XX+PP(7)) * XX &
           & + PP (8)
 
            SUMQ = ((((((XX+QQ(1))*XX+QQ(2))*XX+QQ(3))*XX+QQ(4))*XX+QQ(5))*XX+QQ(6)) * XX + QQ &
           & (7)
 
            VALUE = SUMP / SUMQ
!
!  Calculation reformulated to avoid premature overflow.
!
            IF (X <= XMAX-15.0D+00) THEN
                A = EXP (X)
                B = 1.0D+00
            ELSE
                A = EXP (X-40.0D+00)
                B = EXP40
            END IF
 
            VALUE = ((VALUE*A-PP(1)*A)/SQRT(X)) * B
 
        END IF
 
    END IF
 
    BESSEL_I0 = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BESSEL_I0_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! BESSEL_I0_VALUES returns some values of the I0 Bessel function.
!
!  Discussion:
!
!    The modified Bessel functions In(Z) and Kn(Z) are solutions of
!    the differential equation
!
!      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
!
!    The modified Bessel function I0(Z) corresponds to N = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselI[0,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 20
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1000000000000000D+01, &
   & 0.1010025027795146D+01, 0.1040401782229341D+01, 0.1092045364317340D+01, &
   & 0.1166514922869803D+01, 0.1266065877752008D+01, 0.1393725584134064D+01, &
   & 0.1553395099731217D+01, 0.1749980639738909D+01, 0.1989559356618051D+01, &
   & 0.2279585302336067D+01, 0.3289839144050123D+01, 0.4880792585865024D+01, &
   & 0.7378203432225480D+01, 0.1130192195213633D+02, 0.1748117185560928D+02, &
   & 0.2723987182360445D+02, 0.6723440697647798D+02, 0.4275641157218048D+03, &
   & 0.2815716628466254D+04 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.00D+00, 0.20D+00, 0.40D+00, &
   & 0.60D+00, 0.80D+00, 0.10D+01, 0.12D+01, 0.14D+01, 0.16D+01, 0.18D+01, 0.20D+01, 0.25D+01, &
   & 0.30D+01, 0.35D+01, 0.40D+01, 0.45D+01, 0.50D+01, 0.60D+01, 0.80D+01, 0.10D+02 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BESSEL_I1 (ARG)
 
!*****************************************************************************80
!
!! BESSEL_I1 evaluates the Bessel I function of order I.
!
!  Discussion:
!
!    The main computation evaluates slightly modified forms of
!    minimax approximations generated by Blair and Edwards.
!    This transportable program is patterned after the machine-dependent
!    FUNPACK packet NATSI1, but cannot match that version for efficiency
!    or accuracy.  This version uses rational functions that theoretically
!    approximate I-SUB-1(X) to at least 18 significant decimal digits.
!    The accuracy achieved depends on the arithmetic system, the compiler,
!    the intrinsic functions, and proper selection of the machine-dependent
!    constants.
!
!  Machine-dependent constants:
!
!    beta   = Radix for the floating-point system.
!    maxexp = Smallest power of beta that overflows.
!    XMAX =   Largest argument acceptable to BESI1;  Solution to
!             equation:
!               EXP(X) * (1-3/(8*X)) / SQRT(2*PI*X) = beta**maxexp
!
!
!    Approximate values for some important machines are:
!
!                            beta       maxexp    XMAX
!
!    CRAY-1        (S.P.)       2         8191    5682.810
!    Cyber 180/855
!      under NOS   (S.P.)       2         1070     745.894
!    IEEE (IBM/XT,
!      SUN, etc.)  (S.P.)       2          128      91.906
!    IEEE (IBM/XT,
!      SUN, etc.)  (D.P.)       2         1024     713.987
!    IBM 3033      (D.P.)      16           63     178.185
!    VAX           (S.P.)       2          127      91.209
!    VAX D-Format  (D.P.)       2          127      91.209
!    VAX G-Format  (D.P.)       2         1023     713.293
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Blair, Edwards,
!    Chalk River Report AECL-4928,
!    Atomic Energy of Canada, Limited,
!    October, 1974.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the argument.
!
!    Output, real ( kind = 8 ) BESSEL_I1, the value of the Bessel
!    I1 function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ARG
    REAL (KIND=8) B
    REAL (KIND=8) BESSEL_I1
    REAL (KIND=8), PARAMETER :: EXP40 = 2.353852668370199854D+17
    REAL (KIND=8), PARAMETER :: FORTY = 40.0D+00
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    INTEGER (KIND=4) J
    REAL (KIND=8), PARAMETER :: ONE = 1.0D+00
    REAL (KIND=8), PARAMETER :: ONE5 = 15.0D+00
    REAL (KIND=8), DIMENSION (15) :: P = (/ - 1.9705291802535139930D-19, - &
   & 6.5245515583151902910D-16, - 1.1928788903603238754D-12, - 1.4831904935994647675D-09, - &
   & 1.3466829827635152875D-06, - 9.1746443287817501309D-04, - 4.7207090827310162436D-01, - &
   & 1.8225946631657315931D+02, - 5.1894091982308017540D+04, - 1.0588550724769347106D+07, - &
   & 1.4828267606612366099D+09, - 1.3357437682275493024D+11, - 6.9876779648010090070D+12, - &
   & 1.7732037840791591320D+14, - 1.4577180278143463643D+15 /)
    REAL (KIND=8) :: PBAR = 3.98437500D-01
    REAL (KIND=8), DIMENSION (8) :: PP = (/ - 6.0437159056137600000D-02, &
   & 4.5748122901933459000D-01, - 4.2843766903304806403D-01, 9.7356000150886612134D-02, - &
   & 3.2457723974465568321D-03, - 3.6395264712121795296D-04, 1.6258661867440836395D-05, - &
   & 3.6347578404608223492D-07 /)
    REAL (KIND=8), DIMENSION (5) :: Q = (/ - 4.0076864679904189921D+03, &
   & 7.4810580356655069138D+06, - 8.0059518998619764991D+09, 4.8544714258273622913D+12, - &
   & 1.3218168307321442305D+15 /)
    REAL (KIND=8), DIMENSION (6) :: QQ = (/ - 3.8806586721556593450D+00, &
   & 3.2593714889036996297D+00, - 8.5017476463217924408D-01, 7.4212010813186530069D-02, - &
   & 2.2835624489492512649D-03, 3.7510433111922824643D-05 /)
    REAL (KIND=8), PARAMETER :: REC15 = 6.6666666666666666666D-02
    REAL (KIND=8) SUMP
    REAL (KIND=8) SUMQ
    REAL (KIND=8), PARAMETER :: TWO25 = 225.0D+00
    REAL (KIND=8) VALUE
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XMAX = 713.987D+00
    REAL (KIND=8) XX
    REAL (KIND=8), PARAMETER :: ZERO = 0.0D+00
 
    X = ABS (ARG)
!
!  ABS(ARG) < EPSILON ( ARG )
!
    IF (X < EPSILON(X)) THEN
 
        VALUE = HALF * X
!
!  EPSILON ( ARG ) <= ABS(ARG) < 15.0
!
    ELSE IF (X < ONE5) THEN
 
        XX = X * X
        SUMP = P (1)
        DO J = 2, 15
            SUMP = SUMP * XX + P (J)
        END DO
 
        XX = XX - TWO25
 
        SUMQ = ((((XX+Q(1))*XX+Q(2))*XX+Q(3))*XX+Q(4)) * XX + Q (5)
 
        VALUE = (SUMP/SUMQ) * X
 
    ELSE IF (XMAX < X) THEN
 
        VALUE = HUGE (X)
!
!  15.0 <= ABS(ARG)
!
    ELSE
 
        XX = ONE / X - REC15
 
        SUMP = ((((((PP(1)*XX+PP(2))*XX+PP(3))*XX+PP(4))*XX+PP(5))*XX+PP(6))*XX+PP(7)) * XX + &
       & PP (8)
 
        SUMQ = (((((XX+QQ(1))*XX+QQ(2))*XX+QQ(3))*XX+QQ(4))*XX+QQ(5)) * XX + QQ (6)
 
        VALUE = SUMP / SUMQ
 
        IF (XMAX-ONE5 < X) THEN
            A = EXP (X-FORTY)
            B = EXP40
        ELSE
            A = EXP (X)
            B = ONE
        END IF
 
        VALUE = ((VALUE*A+PBAR*A)/SQRT(X)) * B
 
    END IF
 
    IF (ARG < ZERO) THEN
        VALUE = - VALUE
    END IF
 
    BESSEL_I1 = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BESSEL_I1_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! BESSEL_I1_VALUES returns some values of the I1 Bessel function.
!
!  Discussion:
!
!    The modified Bessel functions In(Z) and Kn(Z) are solutions of
!    the differential equation
!
!      Z^2 W'' + Z * W' - ( Z^2 + N^2 ) * W = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      BesselI[1,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 20
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.0000000000000000D+00, &
   & 0.1005008340281251D+00, 0.2040267557335706D+00, 0.3137040256049221D+00, &
   & 0.4328648026206398D+00, 0.5651591039924850D+00, 0.7146779415526431D+00, &
   & 0.8860919814143274D+00, 0.1084810635129880D+01, 0.1317167230391899D+01, &
   & 0.1590636854637329D+01, 0.2516716245288698D+01, 0.3953370217402609D+01, &
   & 0.6205834922258365D+01, 0.9759465153704450D+01, 0.1538922275373592D+02, &
   & 0.2433564214245053D+02, 0.6134193677764024D+02, 0.3998731367825601D+03, &
   & 0.2670988303701255D+04 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.00D+00, 0.20D+00, 0.40D+00, &
   & 0.60D+00, 0.80D+00, 0.10D+01, 0.12D+01, 0.14D+01, 0.16D+01, 0.18D+01, 0.20D+01, 0.25D+01, &
   & 0.30D+01, 0.35D+01, 0.40D+01, 0.45D+01, 0.50D+01, 0.60D+01, 0.80D+01, 0.10D+02 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF evaluates the Beta Binomial CDF.
!
!  Discussion:
!
!    A simple summing approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) CDF
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) Y
 
    IF (X < 0) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (X < C) THEN
 
        CDF = 0.0D+00
        DO Y = 0, X
            PDF = R8_BETA (A+REAL(Y, KIND=8), B+REAL(C-Y, KIND=8)) / (REAL(C+1, &
           & KIND=8)*R8_BETA(REAL(Y+1, KIND=8), REAL(C-Y+1, KIND=8))*R8_BETA(A, B))
            CDF = CDF + PDF
        END DO
 
    ELSE IF (C <= X) THEN
 
        CDF = 1.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF_INV inverts the Beta Binomial CDF.
!
!  Discussion:
!
!    A simple discrete approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, integer ( kind = 4 ) X, the smallest X whose cumulative density
!    function is greater than or equal to CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CUM
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) Y
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_BINOMIAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CUM = 0.0D+00
 
    DO Y = 0, C
 
        PDF = R8_BETA (A+REAL(Y, KIND=8), B+REAL(C-Y, KIND=8)) / (REAL(C+1, &
       & KIND=8)*R8_BETA(REAL(Y+1, KIND=8), REAL(C-Y+1, KIND=8))*R8_BETA(A, B))
 
        CUM = CUM + PDF
 
        IF (CDF <= CUM) THEN
            X = Y
            RETURN
        END IF
 
    END DO
 
    X = C
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_BINOMIAL_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_CHECK checks the parameters of the Beta Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, logical BETA_BINOMIAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL BETA_BINOMIAL_CHECK
    INTEGER (KIND=4) C
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        BETA_BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        BETA_BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C < 0.'
        BETA_BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    BETA_BINOMIAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_MEAN returns the mean of the Beta Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= N.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) MEAN
 
    MEAN = REAL (C, KIND=8) * A / (A+B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_PDF evaluates the Beta Binomial PDF.
!
!  Discussion:
!
!    The PDF is defined as:
!
!      PDF(A,B,C;X) = Beta(A+X,B+C-X)
!        / ( (C+1) * Beta(X+1,C-X+1) * Beta(A,B) )  for 0 <= X <= C.
!
!    This PDF can be reformulated as:
!
!      The beta binomial probability density function for X successes
!      out of N trials is
!
!      PDF2(X)( N, MU, THETA ) =
!        C(N,X) * Product ( 0 <= R <= X - 1 ) ( MU + R * THETA )
!               * Product ( 0 <= R <= N - X - 1 ) ( 1 - MU + R * THETA )
!               / Product ( 0 <= R <= N - 1 )  ( 1 + R * THETA )
!
!      where
!
!        C(N,X) is the combinatorial coefficient;
!        MU is the expectation of the underlying Beta distribution;
!        THETA is a shape parameter.
!
!      A THETA value of 0 ( or A+B --> +oo ) results in the binomial
!      distribution:
!
!        PDF2(X) ( N, MU, 0 ) = C(N,X) * MU^X * ( 1 - MU )^(N-X)
!
!    Given A, B, C for PDF, then the equivalent PDF2 has:
!
!      N     = C
!      MU    = A / ( A + B )
!      THETA = 1 / ( A + B )
!
!    Given N, MU, THETA for PDF2, the equivalent PDF has:
!
!      A = MU / THETA
!      B = ( 1 - MU ) / THETA
!      C = N
!
!    BETA_BINOMIAL_PDF(1,1,C;X) = UNIFORM_DISCRETE_PDF(0,C-1;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (X <= C) THEN
 
        PDF = R8_BETA (A+REAL(X, KIND=8), B+REAL(C-X, KIND=8)) / (REAL(C+1, &
       & KIND=8)*R8_BETA(REAL(X+1, KIND=8), REAL(C-X+1, KIND=8))*R8_BETA(A, B))
 
    ELSE IF (C < X) THEN
 
        PDF = 0.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_SAMPLE samples the Beta Binomial CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL BETA_BINOMIAL_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_BINOMIAL_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! BETA_BINOMIAL_VARIANCE returns the variance of the Beta Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (REAL(C, KIND=8)*A*B) * (A+B+REAL(C, KIND=8)) / ((A+B)**2*(A+B+1.0D+00))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! BETA_CDF evaluates the Beta CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (X <= 1.0D+00) THEN
        CDF = BETA_INC (A, B, X)
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_CDF_INV (CDF, P, Q, X)
 
!*****************************************************************************80
!
!! BETA_CDF_INV computes the inverse of the incomplete Beta function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    Original FORTRAN77 version by GW Cran, KJ Martin, GE Thomas.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    GW Cran, KJ Martin, GE Thomas,
!    Remark AS R19 and Algorithm AS 109:
!    A Remark on Algorithms AS 63: The Incomplete Beta Integral
!    and AS 64: Inverse of the Incomplete Beta Integeral,
!    Applied Statistics,
!    Volume 26, Number 1, 1977, pages 111-114.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the Beta CDF.
!    0 <= CDF <= 1.
!
!    Input, real ( kind = 8 ) P, Q, the parameters of the incomplete
!    Beta function.
!
!    Output, real ( kind = 8 ) X, the argument of the incomplete
!    Beta function which produces the value CDF.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) SAE, the most negative decimal exponent
!    which does not cause an underflow.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ACU
    REAL (KIND=8) ADJ
    REAL (KIND=8) BETA_LOG
    REAL (KIND=8) CDF
    REAL (KIND=8) FPU
    REAL (KIND=8) G
    REAL (KIND=8) H
    INTEGER (KIND=4) IEX
    LOGICAL INDX
    REAL (KIND=8) P
    REAL (KIND=8) PP
    REAL (KIND=8) PREV
    REAL (KIND=8) Q
    REAL (KIND=8) QQ
    REAL (KIND=8) R
    REAL (KIND=8) S
    REAL (KIND=8), PARAMETER :: SAE = - 37.0D+00
    REAL (KIND=8) SQ
    REAL (KIND=8) T
    REAL (KIND=8) TX
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) XIN
    REAL (KIND=8) Y
    REAL (KIND=8) YPREV
 
    FPU = 10.0D+00 ** SAE
    BETA_LOG = LOG_GAMMA (P) + LOG_GAMMA (Q) - LOG_GAMMA (P+Q)
!
!  Test for admissibility of parameters.
!
    IF (P <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  P <= 0.0'
        RETURN
    END IF
 
    IF (Q <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  Q <= 0.0'
        RETURN
    END IF
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0.0 or 1.0 < CDF.'
        RETURN
    END IF
!
!  Return immediately if the answer is easy to determine.
!
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
        RETURN
    ELSE IF (CDF == 1.0D+00) THEN
        X = 1.0D+00
        RETURN
    END IF
!
!  Change tail if necessary.
!
    IF (0.5D+00 < CDF) THEN
        A = 1.0D+00 - CDF
        PP = Q
        QQ = P
        INDX = .TRUE.
    ELSE
        A = CDF
        PP = P
        QQ = Q
        INDX = .FALSE.
    END IF
!
!  Calculate the initial approximation.
!
    R = SQRT (-LOG(A*A))
 
    Y = R - (2.30753D+00+0.27061D+00*R) / (1.0D+00+(0.99229D+00+0.04481D+00*R)*R)
 
    IF (1.0D+00 < PP .AND. 1.0D+00 < QQ) THEN
 
        R = (Y*Y-3.0D+00) / 6.0D+00
        S = 1.0D+00 / (PP+PP-1.0D+00)
        T = 1.0D+00 / (QQ+QQ-1.0D+00)
        H = 2.0D+00 / (S+T)
        W = Y * SQRT (H+R) / H - (T-S) * (R+5.0D+00/6.0D+00-2.0D+00/(3.0D+00*H))
        X = PP / (PP+QQ*EXP(W+W))
 
    ELSE
 
        R = QQ + QQ
        T = 1.0D+00 / (9.0D+00*QQ)
        T = R * (1.0D+00-T+Y*SQRT(T)) ** 3
 
        IF (T <= 0.0D+00) THEN
            X = 1.0D+00 - EXP ((LOG((1.0D+00-A)*QQ)+BETA_LOG)/QQ)
        ELSE
 
            T = (4.0D+00*PP+R-2.0D+00) / T
 
            IF (T <= 1.0D+00) THEN
                X = EXP ((LOG(A*PP)+BETA_LOG)/PP)
            ELSE
                X = 1.0D+00 - 2.0D+00 / (T+1.0D+00)
            END IF
 
        END IF
 
    END IF
!
!  Solve for X by a modified Newton-Raphson method.
!
    R = 1.0D+00 - PP
    T = 1.0D+00 - QQ
    YPREV = 0.0D+00
    SQ = 1.0D+00
    PREV = 1.0D+00
 
    IF (X < 0.0001D+00) THEN
        X = 0.0001D+00
    END IF
 
    IF (0.9999D+00 < X) THEN
        X = 0.9999D+00
    END IF
 
    IEX = MAX (-5.0D+00/PP**2-1.0D+00/A**0.2D+00-13.0D+00, SAE)
 
    ACU = 10.0D+00 ** IEX
 
    DO
 
        Y = BETA_INC (PP, QQ, X)
 
        XIN = X
        Y = (Y-A) * EXP (BETA_LOG+R*LOG(XIN)+T*LOG(1.0D+00-XIN))
 
        IF (Y*YPREV <= 0.0D+00) THEN
            PREV = MAX (SQ, FPU)
        END IF
 
        G = 1.0D+00
 
        DO
 
            DO
 
                ADJ = G * Y
                SQ = ADJ * ADJ
 
                IF (SQ < PREV) THEN
 
                    TX = X - ADJ
 
                    IF (0.0D+00 <= TX .AND. TX <= 1.0D+00) THEN
                        EXIT
                    END IF
 
                END IF
 
                G = G / 3.0D+00
 
            END DO
 
            IF (PREV <= ACU) THEN
                IF (INDX) THEN
                    X = 1.0D+00 - X
                END IF
                RETURN
            END IF
 
            IF (Y*Y <= ACU) THEN
                IF (INDX) THEN
                    X = 1.0D+00 - X
                END IF
                RETURN
            END IF
 
            IF (TX /= 0.0D+00 .AND. TX /= 1.0D+00) THEN
                EXIT
            END IF
 
            G = G / 3.0D+00
 
        END DO
 
        IF (TX == X) THEN
            EXIT
        END IF
 
        X = TX
        YPREV = Y
 
    END DO
 
    IF (INDX) THEN
        X = 1.0D+00 - X
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_CDF_INV_OLD (CDF, A, B, X)
 
!*****************************************************************************80
!
!! BETA_CDF_INV_OLD inverts the Beta CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2001
!
!  Author:
!
!    Original FORTRAN77 version by Roger Abernathy, Robert Smith.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Roger Abernathy, Robert Smith,
!    Algorithm 724,
!    Program to Calculate F Percentiles,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 4, December 1993, pages 481-483.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: MAXK = 20
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) BCOEFF
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF_X
    REAL (KIND=8) D (2:MAXK, 0:MAXK-2)
    REAL (KIND=8), PARAMETER :: ERROR = 0.0001D+00
    REAL (KIND=8), PARAMETER :: ERRAPP = 0.01D+00
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) K
    INTEGER (KIND=4) LOOPCT
    REAL (KIND=8) PDF_X
    REAL (KIND=8) Q
    REAL (KIND=8) S1
    REAL (KIND=8) S2
    REAL (KIND=8) SUM2
    REAL (KIND=8) T
    REAL (KIND=8) TAIL
    REAL (KIND=8) X
    REAL (KIND=8) XOLD
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
!
!  Estimate the solution.
!
    X = A / (A+B)
 
    XOLD = 0.0D+00
    LOOPCT = 2
 
    DO WHILE (ERRAPP <= ABS((X-XOLD)/X) .AND. LOOPCT /=  0)
 
        XOLD = X
        LOOPCT = LOOPCT - 1
!
!  CDF_X = PROB { BETA(A,B) <= X }.
!  Q = ( CDF - CDF_X ) / PDF_X.
!
        CALL BETA_CDF (X, A, B, CDF_X)
 
        CALL BETA_PDF (X, A, B, PDF_X)
 
        Q = (CDF-CDF_X) / PDF_X
!
!  D(N,K) = C(N,K) * Q^(N+K-1) / (N-1)!
!
        T = 1.0D+00 - X
        S1 = Q * (B-1.0D+00) / T
        S2 = Q * (1.0D+00-A) / X
        D (2, 0) = S1 + S2
        TAIL = D (2, 0) * Q / 2.0D+00
        X = X + Q + TAIL
 
        K = 3
 
        DO WHILE (ERROR < ABS(TAIL/X) .AND. K <= MAXK)
!
!  Find D(2,K-2).
!
            S1 = Q * (REAL(K, KIND=8)-2.0D+00) * S1 / T
            S2 = Q * (2.0D+00-REAL(K, KIND=8)) * S2 / X
            D (2, K-2) = S1 + S2
!
!  Find D(3,K-3), D(4,K-4), D(5,K-5), ... , D(K-1,1).
!
            DO I = 3, K - 1
                SUM2 = D (2, 0) * D (I-1, K-I)
                BCOEFF = 1.0D+00
                DO J = 1, K - I
                    BCOEFF = (BCOEFF*REAL(K-I-J+1, KIND=8)) / REAL (J, KIND=8)
                    SUM2 = SUM2 + BCOEFF * D (2, J) * D (I-1, K-I-J)
                END DO
                D (I, K-I) = SUM2 + D (I-1, K-I+1) / REAL (I-1, KIND=8)
            END DO
!
!  Compute D(K,0) and use it to expand the series.
!
            D (K, 0) = D (2, 0) * D (K-1, 0) + D (K-1, 1) / REAL (K-1, KIND=8)
            TAIL = D (K, 0) * Q / REAL (K, KIND=8)
            X = X + TAIL
!
!  Check for divergence.
!
            IF (X <= 0.0D+00 .OR. 1.0D+00 <= X) THEN
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'BETA_CDF_INV - Fatal error!'
                WRITE (*, '(a)') '  The series has diverged.'
                WRITE (*, '(a,g14.6)') '  X = ', X
                X = - 1.0D+00
                RETURN
            END IF
 
            K = K + 1
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_CDF_VALUES (N_DATA, A, B, X, FX)
 
!*****************************************************************************80
!
!! BETA_CDF_VALUES returns some values of the Beta CDF.
!
!  Discussion:
!
!    The incomplete Beta function may be written
!
!      BETA_INC(A,B,X) = integral (0 <= t <= X) T^(A-1) * (1-T)^(B-1) dT
!                      / integral (0 <= t <= 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    The incomplete Beta function is also sometimes called the
!    "modified" Beta function, or the "normalized" Beta function
!    or the Beta CDF (cumulative density function).
!
!    In Mathematica, the function can be evaluated by:
!
!      BETA[X,A,B] / BETA[A,B]
!
!    The function can also be evaluated by using the Statistics package:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = BetaDistribution [ a, b ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968,
!    LC: QA351.P38.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, the parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 45
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.5D+00, 10.0D+00, 10.0D+00, 10.0D+00, 10.0D+00, 20.0D+00, &
   & 20.0D+00, 20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, 30.0D+00, 40.0D+00, 1.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, &
   & 1.30625D+00, 1.30625D+00, 1.30625D+00 /)
    REAL (KIND=8) B
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.0D+00, 0.5D+00, 5.0D+00, 5.0D+00, 10.0D+00, 5.0D+00, &
   & 10.0D+00, 10.0D+00, 20.0D+00, 20.0D+00, 10.0D+00, 10.0D+00, 20.0D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 11.7562D+00, 11.7562D+00, 11.7562D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.6376856085851985D-01, &
   & 0.2048327646991335D+00, 0.1000000000000000D+01, 0.0000000000000000D+00, &
   & 0.5012562893380045D-02, 0.5131670194948620D-01, 0.2928932188134525D+00, &
   & 0.5000000000000000D+00, 0.2800000000000000D-01, 0.1040000000000000D+00, &
   & 0.2160000000000000D+00, 0.3520000000000000D+00, 0.5000000000000000D+00, &
   & 0.6480000000000000D+00, 0.7840000000000000D+00, 0.8960000000000000D+00, &
   & 0.9720000000000000D+00, 0.4361908850559777D+00, 0.1516409096347099D+00, &
   & 0.8978271484375000D-01, 0.1000000000000000D+01, 0.5000000000000000D+00, &
   & 0.4598773297575791D+00, 0.2146816102371739D+00, 0.9507364826957875D+00, &
   & 0.5000000000000000D+00, 0.8979413687105918D+00, 0.2241297491808366D+00, &
   & 0.7586405487192086D+00, 0.7001783247477069D+00, 0.5131670194948620D-01, &
   & 0.1055728090000841D+00, 0.1633399734659245D+00, 0.2254033307585166D+00, &
   & 0.3600000000000000D+00, 0.4880000000000000D+00, 0.5904000000000000D+00, &
   & 0.6723200000000000D+00, 0.2160000000000000D+00, 0.8370000000000000D-01, &
   & 0.3078000000000000D-01, 0.1093500000000000D-01, 0.918884684620518D+00, &
   & 0.21052977489419D+00, 0.1824130512500673D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.01D+00, 0.10D+00, 1.00D+00, &
   & 0.00D+00, 0.01D+00, 0.10D+00, 0.50D+00, 0.50D+00, 0.10D+00, 0.20D+00, 0.30D+00, 0.40D+00, &
   & 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.90D+00, 0.50D+00, 0.90D+00, 0.50D+00, 1.00D+00, &
   & 0.50D+00, 0.80D+00, 0.60D+00, 0.80D+00, 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.70D+00, &
   & 0.10D+00, 0.20D+00, 0.30D+00, 0.40D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.30D+00, &
   & 0.30D+00, 0.30D+00, 0.30D+00, 0.225609D+00, 0.0335568D+00, 0.0295222D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_CHECK (A, B)
 
!*****************************************************************************80
!
!! BETA_CHECK checks the parameters of the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical BETA_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL BETA_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        BETA_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        BETA_CHECK = .FALSE.
        RETURN
    END IF
 
    BETA_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_INC (A, B, X)
 
!*****************************************************************************80
!
!! BETA_INC returns the value of the incomplete Beta function.
!
!  Discussion:
!
!    This calculation requires an iteration.  In some cases, the iteration
!    may not converge rapidly, or may become inaccurate.
!
!    The formula is:
!
!      BETA_INC(A,B,X)
!
!        =   Integral ( 0 <= T <= X ) T^(A-1) (1-T)^(B-1) dT
!          / Integral ( 0 <= T <= 1 ) T^(A-1) (1-T)^(B-1) dT
!
!        =   Integral ( 0 <= T <= X ) T^(A-1) (1-T)^(B-1) dT
!          / BETA(A,B)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2004
!
!  Author:
!
!    Original FORTRAN77 version by KL Majumder, GP Bhattacharjee.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    KL Majumder, GP Bhattacharjee,
!    Algorithm AS63,
!    Applied Statistics,
!    1973, volume 22, number 3.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    0.0 < A,
!    0.0 < B.
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!    Normally, 0.0D+00 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) BETA_INC, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) BETA_INC
    REAL (KIND=8) CX
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 1000
    LOGICAL INDX
    INTEGER (KIND=4) NS
    REAL (KIND=8) PP
    REAL (KIND=8) PSQ
    REAL (KIND=8) QQ
    REAL (KIND=8) RX
    REAL (KIND=8) TEMP
    REAL (KIND=8) TERM
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-07
    REAL (KIND=8) X
    REAL (KIND=8) XX
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        RETURN
    END IF
 
    IF (X <= 0.0D+00) THEN
        BETA_INC = 0.0D+00
        RETURN
    ELSE IF (1.0D+00 <= X) THEN
        BETA_INC = 1.0D+00
        RETURN
    END IF
!
!  Change tail if necessary and determine S.
!
    PSQ = A + B
 
    IF (A < (A+B)*X) THEN
        XX = 1.0D+00 - X
        CX = X
        PP = B
        QQ = A
        INDX = .TRUE.
    ELSE
        XX = X
        CX = 1.0D+00 - X
        PP = A
        QQ = B
        INDX = .FALSE.
    END IF
 
    TERM = 1.0D+00
    I = 1
    BETA_INC = 1.0D+00
 
    NS = INT (QQ+CX*(A+B))
!
!  Use Soper's reduction formulas.
!
    RX = XX / CX
 
    TEMP = QQ - REAL (I, KIND=8)
    IF (NS == 0) THEN
        RX = XX
    END IF
 
    IT = 0
 
    DO
 
        IT = IT + 1
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'BETA_INC - Fatal error!'
            WRITE (*, '(a)') '  Maximum number of iterations exceeded!'
            WRITE (*, '(a,i8)') '  IT_MAX = ', IT_MAX
            RETURN
        END IF
 
        TERM = TERM * TEMP * RX / (PP+REAL(I, KIND=8))
        BETA_INC = BETA_INC + TERM
        TEMP = ABS (TERM)
 
        IF (TEMP <= TOL .AND. TEMP <= TOL*BETA_INC) THEN
            EXIT
        END IF
 
        I = I + 1
        NS = NS - 1
 
        IF (0 <= NS) THEN
            TEMP = QQ - REAL (I, KIND=8)
            IF (NS == 0) THEN
                RX = XX
            END IF
        ELSE
            TEMP = PSQ
            PSQ = PSQ + 1.0D+00
        END IF
 
    END DO
!
!  Finish calculation.
!
    BETA_INC = BETA_INC * EXP (PP*LOG(XX)+(QQ-1.0D+00)*LOG(CX)) / (R8_BETA(A, B)*PP)
 
    IF (INDX) THEN
        BETA_INC = 1.0D+00 - BETA_INC
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_INC_VALUES (N_DATA, A, B, X, FX)
 
!*****************************************************************************80
!
!! BETA_INC_VALUES returns some values of the incomplete Beta function.
!
!  Discussion:
!
!    The incomplete Beta function may be written
!
!      BETA_INC(A,B,X) = integral (0 <= t <= X) T^(A-1) * (1-T)^(B-1) dT
!                      / integral (0 <= t <= 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    The incomplete Beta function is also sometimes called the
!    "modified" Beta function, or the "normalized" Beta function
!    or the Beta CDF (cumulative density function).
!
!    In Mathematica, the function can be evaluated by:
!
!      BETA[X,A,B] / BETA[A,B]
!
!    The function can also be evaluated by using the Statistics package:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = BetaDistribution [ a, b ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968,
!    LC: QA351.P38.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, the parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 45
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.5D+00, 10.0D+00, 10.0D+00, 10.0D+00, 10.0D+00, 20.0D+00, &
   & 20.0D+00, 20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, 30.0D+00, 40.0D+00, 1.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, &
   & 1.30625D+00, 1.30625D+00, 1.30625D+00 /)
    REAL (KIND=8) B
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.0D+00, 0.5D+00, 5.0D+00, 5.0D+00, 10.0D+00, 5.0D+00, &
   & 10.0D+00, 10.0D+00, 20.0D+00, 20.0D+00, 10.0D+00, 10.0D+00, 20.0D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 11.7562D+00, 11.7562D+00, 11.7562D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.6376856085851985D-01, &
   & 0.2048327646991335D+00, 0.1000000000000000D+01, 0.0000000000000000D+00, &
   & 0.5012562893380045D-02, 0.5131670194948620D-01, 0.2928932188134525D+00, &
   & 0.5000000000000000D+00, 0.2800000000000000D-01, 0.1040000000000000D+00, &
   & 0.2160000000000000D+00, 0.3520000000000000D+00, 0.5000000000000000D+00, &
   & 0.6480000000000000D+00, 0.7840000000000000D+00, 0.8960000000000000D+00, &
   & 0.9720000000000000D+00, 0.4361908850559777D+00, 0.1516409096347099D+00, &
   & 0.8978271484375000D-01, 0.1000000000000000D+01, 0.5000000000000000D+00, &
   & 0.4598773297575791D+00, 0.2146816102371739D+00, 0.9507364826957875D+00, &
   & 0.5000000000000000D+00, 0.8979413687105918D+00, 0.2241297491808366D+00, &
   & 0.7586405487192086D+00, 0.7001783247477069D+00, 0.5131670194948620D-01, &
   & 0.1055728090000841D+00, 0.1633399734659245D+00, 0.2254033307585166D+00, &
   & 0.3600000000000000D+00, 0.4880000000000000D+00, 0.5904000000000000D+00, &
   & 0.6723200000000000D+00, 0.2160000000000000D+00, 0.8370000000000000D-01, &
   & 0.3078000000000000D-01, 0.1093500000000000D-01, 0.918884684620518D+00, &
   & 0.21052977489419D+00, 0.1824130512500673D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.01D+00, 0.10D+00, 1.00D+00, &
   & 0.00D+00, 0.01D+00, 0.10D+00, 0.50D+00, 0.50D+00, 0.10D+00, 0.20D+00, 0.30D+00, 0.40D+00, &
   & 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.90D+00, 0.50D+00, 0.90D+00, 0.50D+00, 1.00D+00, &
   & 0.50D+00, 0.80D+00, 0.60D+00, 0.80D+00, 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.70D+00, &
   & 0.10D+00, 0.20D+00, 0.30D+00, 0.40D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.30D+00, &
   & 0.30D+00, 0.30D+00, 0.30D+00, 0.225609D+00, 0.0335568D+00, 0.0295222D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! BETA_MEAN returns the mean of the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A / (A+B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! BETA_PDF evaluates the Beta PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!      PDF(A,B;X) = X^(A-1) * (1-X)^(B-1) / BETA(A,B).
!
!    A = B = 1 yields the Uniform distribution on [0,1].
!    A = B = 1/2 yields the Arcsin distribution.
!        B = 1 yields the power function distribution.
!    A = B -> Infinity tends to the Normal distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00 .OR. 1.0D+00 < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = X ** (A-1.0D+00) * (1.0D+00-X) ** (B-1.0D+00) / R8_BETA (A, B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! BETA_SAMPLE samples the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Algorithm BN,
!    Statistical Computing,
!    Dekker, 1980.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MU
    INTEGER (KIND=4) SEED
    REAL (KIND=8) STDEV
    REAL (KIND=8) TEST
    REAL (KIND=8) U
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    MU = (A-1.0D+00) / (A+B-2.0D+00)
    STDEV = 0.5D+00 / SQRT (A+B-2.0D+00)
 
    DO
 
        CALL NORMAL_01_SAMPLE (SEED, Y)
 
        X = MU + STDEV * Y
 
        IF (X < 0.0D+00 .OR. 1.0D+00 < X) THEN
            CYCLE
        END IF
 
        U = R8_UNIFORM_01 (SEED)
 
        TEST = (A-1.0D+00) * LOG (X/(A-1.0D+00)) + (B-1.0D+00) * LOG ((1.0D+00-X)/(B-1.0D+00)) &
       & + (A+B-2.0D+00) * LOG (A+B-2.0D+00) + 0.5D+00 * Y * Y
 
        IF (LOG(U) <= TEST) THEN
            EXIT
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_VALUES (N_DATA, X, Y, FXY)
 
!*****************************************************************************80
!
!! BETA_VALUES returns some values of the Beta function.
!
!  Discussion:
!
!    Beta(X,Y) = ( Gamma(X) * Gamma(Y) ) / Gamma(X+Y)
!
!    Both X and Y must be greater than 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      Beta[X,Y]
!
!  Properties:
!
!    Beta(X,Y) = Beta(Y,X).
!    Beta(X,Y) = integral ( 0 <= T <= 1 ) T^(X-1) (1-T)^(Y-1) dT.
!    Beta(X,Y) = Gamma(X) * Gamma(Y) / Gamma(X+Y)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, Y, the arguments of the function.
!
!    Output, real ( kind = 8 ) FXY, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 17
 
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.5000000000000000D+01, &
   & 0.2500000000000000D+01, 0.1666666666666667D+01, 0.1250000000000000D+01, &
   & 0.5000000000000000D+01, 0.2500000000000000D+01, 0.1000000000000000D+01, &
   & 0.1666666666666667D+00, 0.3333333333333333D-01, 0.7142857142857143D-02, &
   & 0.1587301587301587D-02, 0.2380952380952381D-01, 0.5952380952380952D-02, &
   & 0.1984126984126984D-02, 0.7936507936507937D-03, 0.3607503607503608D-03, &
   & 0.8325008325008325D-04 /)
    REAL (KIND=8) FXY
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.2D+00, 0.4D+00, 0.6D+00, 0.8D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00, 6.0D+00, 6.0D+00, &
   & 6.0D+00, 6.0D+00, 7.0D+00 /)
    REAL (KIND=8) Y
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: Y_VEC = (/ 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   & 0.2D+00, 0.4D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
   & 5.0D+00, 6.0D+00, 7.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        Y = 0.0D+00
        FXY = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        Y = Y_VEC (N_DATA)
        FXY = B_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! BETA_VARIANCE returns the variance of the Beta PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (A*B) / ((A+B)**2*(1.0D+00+A+B))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! BINOMIAL_CDF evaluates the Binomial CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    A sequence of trials with fixed probability of success on
!    any trial is known as a sequence of Bernoulli trials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the desired number of successes.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    INTEGER (KIND=4) CNK
    REAL (KIND=8) CDF
    INTEGER (KIND=4) J
    REAL (KIND=8) PR
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (A <= X) THEN
 
        CDF = 1.0D+00
 
    ELSE IF (B == 0.0D+00) THEN
 
        CDF = 1.0D+00
 
    ELSE IF (B == 1.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        CDF = 0.0D+00
 
        DO J = 0, X
 
            CNK = I4_CHOOSE (A, J)
 
            PR = REAL (CNK, KIND=8) * B ** J * (1.0D+00-B) ** (A-J)
 
            CDF = CDF + PR
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! BINOMIAL_CDF_INV inverts the Binomial CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) X2
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BINOMIAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CDF2 = 0.0D+00
 
    DO X2 = 0, A
 
        CALL BINOMIAL_PDF (X2, A, B, PDF)
 
        CDF2 = CDF2 + PDF
 
        IF (CDF <= CDF2) THEN
            X = X2
            RETURN
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BINOMIAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! BINOMIAL_CHECK checks the parameter of the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, logical BINOMIAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    LOGICAL BINOMIAL_CHECK
 
    IF (A < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 1.'
        BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B < 0.0D+00 .OR. 1.0D+00 < B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < 0 or 1 < B.'
        BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    BINOMIAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! BINOMIAL_MEAN returns the mean of the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the expected value of the number of
!    successes in A trials.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = REAL (A, KIND=8) * B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! BINOMIAL_PDF evaluates the Binomial PDF.
!
!  Discussion:
!
!    PDF(A,B;X) is the probability of exactly X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    The formula is:
!
!      PDF(A,B;X) = C(N,X) * B^X * ( 1.0D+00 - B )^( A - X )
!
!    Binomial_PDF(1,B;X) = Bernoulli_PDF(B;X).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the desired number of successes.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    INTEGER (KIND=4) CNK
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (A < 1) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (X < 0 .OR. A < X) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (B == 0.0D+00) THEN
 
        IF (X == 0) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
 
    ELSE IF (B == 1.0D+00) THEN
 
        IF (X == A) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
 
    ELSE
 
        CNK = I4_CHOOSE (A, X)
 
        PDF = REAL (CNK, KIND=8) * B ** X * (1.0D+00-B) ** (A-X)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! BINOMIAL_SAMPLE samples the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Algorithm BU,
!    Statistical Computing,
!    Dekker, 1980.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    INTEGER (KIND=4) X
 
    X = 0
 
    DO I = 1, A
 
        U = R8_UNIFORM_01 (SEED)
 
        IF (U <= B) THEN
            X = X + 1
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! BINOMIAL_VARIANCE returns the variance of the Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the probability of success on one trial.
!    0.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = REAL (A, KIND=8) * B * (1.0D+00-B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! BRADFORD_CDF evaluates the Bradford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE IF (X <= B) THEN
        CDF = LOG (1.0D+00+C*(X-A)/(B-A)) / LOG (C+1.0D+00)
    ELSE IF (B < X) THEN
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! BRADFORD_CDF_INV inverts the Bradford CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BRADFORD_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF <= 0.0D+00) THEN
        X = A
    ELSE IF (CDF < 1.0D+00) THEN
        X = A + (B-A) * ((C+1.0D+00)**CDF-1.0D+00) / C
    ELSE IF (1.0D+00 <= CDF) THEN
        X = B
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BRADFORD_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! BRADFORD_CHECK checks the parameters of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, logical BRADFORD_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL BRADFORD_CHECK
    REAL (KIND=8) C
 
    IF (B <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BRADFORD_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= A.'
        BRADFORD_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BRADFORD_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C <= 0.'
        BRADFORD_CHECK = .FALSE.
        RETURN
    END IF
 
    BRADFORD_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! BRADFORD_MEAN returns the mean of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = (C*(B-A)+LOG(C+1.0D+00)*(A*(C+1.0D+00)-B)) / (C*LOG(C+1.0D+00))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! BRADFORD_PDF evaluates the Bradford PDF.
!
!  Discussion:
!
!    The formula is:
!
!      PDF(A,B,C;X) =
!        C / ( ( C * ( X - A ) + B - A ) * log ( C + 1 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        PDF = 0.0D+00
    ELSE IF (X <= B) THEN
        PDF = C / ((C*(X-A)+B-A)*LOG(C+1.0D+00))
    ELSE IF (B < X) THEN
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! BRADFORD_SAMPLE samples the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    X = A + (B-A) * ((C+1.0D+00)**CDF-1.0D+00) / C
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BRADFORD_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! BRADFORD_VARIANCE returns the variance of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (B-A) ** 2 * (C*(LOG(C+1.0D+00)-2.0D+00)+2.0D+00*LOG(C+1.0D+00)) / &
   & (2.0D+00*C*(LOG(C+1.0D+00))**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BUFFON_BOX_PDF (A, B, L, PDF)
 
!*****************************************************************************80
!
!! BUFFON_BOX_PDF evaluates the Buffon Box PDF.
!
!  Discussion:
!
!    In the Buffon-Laplace needle experiment, we suppose that the plane has been
!    tiled into a grid of rectangles of width A and height B, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    We may assume that one end, the "eye" of the needle falls at the point
!    (X1,Y1), taken uniformly at random in the cell [0,A]x[0,B].
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2, Y2 <= 0, B <= Y2.
!
!    If L is larger than sqrt ( A*A + B*B ), then the needle will
!    cross every time, and the computation is uninteresting.  However, if
!    L is smaller than this limit, then the probability of a crossing on
!    a single trial is
!
!      P(L,A,B) = ( 2 * L * ( A + B ) - L * L ) / ( PI * A * B )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sudarshan Raghunathan,
!    Making a Supercomputer Do What You Want: High Level Tools for
!    Parallel Programming,
!    Computing in Science and Engineering,
!    Volume 8, Number 5, September/October 2006, pages 70-80.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the horizontal and vertical dimensions
!    of each cell of the grid.  0 <= A, 0 <= B.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!    0 <= L <= min ( A, B ).
!
!    Output, real ( kind = 8 ) PDF, the Buffon-Laplace PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) L
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793238462643D+00
 
    IF (A < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_BOX_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input A < 0.'
        RETURN
    ELSE IF (A == 0.0D+00) THEN
        PDF = 1.0D+00
        RETURN
    END IF
 
    IF (B < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_BOX_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input B < 0.'
        RETURN
    ELSE IF (B == 0.0D+00) THEN
        PDF = 1.0D+00
        RETURN
    END IF
 
    IF (L < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_BOX_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input L < 0.'
        RETURN
    ELSE IF (L == 0.0D+00) THEN
        PDF = 0.0D+00
        RETURN
    ELSE IF (MIN(A, B) < L) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_BOX_PDF - Fatal error!'
        WRITE (*, '(a)') '  min ( A, B ) < L.'
        RETURN
    END IF
 
    PDF = L * (2.0D+00*(A+B)-L) / (R8_PI*A*B)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BUFFON_BOX_SAMPLE (A, B, L, TRIAL_NUM, SEED)
 
!*****************************************************************************80
!
!! BUFFON_BOX_SAMPLE samples the Buffon Box PDF.
!
!  Discussion:
!
!    In the Buffon-Laplace needle experiment, we suppose that the plane has
!    been tiled into a grid of rectangles of width A and height B, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    We may assume that one end, the "eye" of the needle falls at the point
!    (X1,Y1), taken uniformly at random in the cell [0,A]x[0,B].
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2, Y2 <= 0, B <= Y2.
!
!    This routine simulates the tossing of the needle, and returns the number
!    of times that the needle crossed at least one grid line.
!
!    If L is larger than sqrt ( A*A + B*B ), then the needle will
!    cross every time, and the computation is uninteresting.  However, if
!    L is smaller than this limit, then the probability of a crossing on
!    a single trial is
!
!      P(L,A,B) = ( 2 * L * ( A + B ) - L * L ) / ( PI * A * B )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!    (Particularly roundabout, since we actually will use a good value of
!    PI in order to pick the random angles!)
!
!    Note that this routine will try to generate 5 * TRIAL_NUM random
!    double precision values at one time, using automatic arrays.
!    When I tried this with TRIAL_NUM = 1,000,000, the program failed,
!    because of internal system limits on such arrays.
!
!    Such a problem could be avoided by using a DO loop running through
!    each trial individually, but this tend to run much more slowly than
!    necessary.
!
!    Since this routine invokes the FORTRAN90 random number generator,
!    the user should initialize the random number generator, particularly
!    if it is desired to control whether the sequence is to be varied
!    or repeated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Sudarshan Raghunathan,
!    Making a Supercomputer Do What You Want: High Level Tools for
!    Parallel Programming,
!    Computing in Science and Engineering,
!    Volume 8, Number 5, September/October 2006, pages 70-80.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the horizontal and vertical dimensions
!    of each cell of the grid.  0 <= A, 0 <= B.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!    0 <= L <= min ( A, B ).
!
!    Input, integer ( kind = 4 ) TRIAL_NUM, the number of times the needle is
!    to be dropped onto the grid.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) BUFFON_BOX_SAMPLE, the number of times
!    the needle crossed at least one line of the grid of cells.
!
!  Local Parameters:
!
!    Local, integer BATCH_SIZE, specifies the number of trials to be done
!    in a single batch.  Setting BATCH_SIZE to 1 will be very slow.
!    Replacing it by TRIAL_NUM would be fine except that your system
!    may have a limit on the size of automatic arrays.  We have set a default
!    value of 10,000 here which should be large enough to be efficient
!    but small enough not to annoy the system.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: BATCH_SIZE = 10000
    INTEGER (KIND=4) TRIAL_NUM
 
    REAL (KIND=8) A
    REAL (KIND=8) ANGLE (BATCH_SIZE)
    REAL (KIND=8) B
    INTEGER (KIND=4) BATCH
    INTEGER (KIND=4) BUFFON_BOX_SAMPLE
    INTEGER (KIND=4) HITS
    REAL (KIND=8) L
    INTEGER (KIND=4) N
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793238462643D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X1 (BATCH_SIZE)
    REAL (KIND=8) X2 (BATCH_SIZE)
    REAL (KIND=8) Y1 (BATCH_SIZE)
    REAL (KIND=8) Y2 (BATCH_SIZE)
 
    HITS = 0
 
    DO BATCH = 1, TRIAL_NUM, BATCH_SIZE
 
        N = MIN (BATCH_SIZE, TRIAL_NUM+1-BATCH)
!
!  Randomly choose the location of the eye of the needle in [0,0]x[A,B],
!  and the angle the needle makes.
!
        CALL RANDOM_NUMBER (HARVEST=X1(1:N))
        CALL RANDOM_NUMBER (HARVEST=Y1(1:N))
        CALL RANDOM_NUMBER (HARVEST=ANGLE(1:N))
 
        X1 (1:N) = A * X1 (1:N)
        Y1 (1:N) = B * Y1 (1:N)
        ANGLE (1:N) = 2.0D+00 * R8_PI * ANGLE (1:N)
!
!  Compute the location of the point of the needle.
!
        X2 (1:N) = X1 (1:N) + L * COS (ANGLE(1:N))
        Y2 (1:N) = Y1 (1:N) + L * SIN (ANGLE(1:N))
!
!  Count the end locations that lie outside the cell.
!
        HITS = HITS + COUNT (X2(1:N) <= 0.0 .OR. A <= X2(1:N) .OR. Y2(1:N) <= 0.0 .OR. B <= &
       & Y2(1:N))
 
    END DO
 
    BUFFON_BOX_SAMPLE = HITS
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BUFFON_PDF (A, L, PDF)
 
!*****************************************************************************80
!
!! BUFFON_PDF evaluates the Buffon PDF.
!
!  Discussion:
!
!    In the Buffon needle experiment, we suppose that the plane has been
!    ruled by vertical lines with a spacing of A units, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    Because of the various symmetries, we may assume that this eye of
!    this needle lands in the first infinite strip, and we may further
!    assume that its Y coordinate is 0.  Thus, we have
!    the eye as (X1,Y1) with 0 <= X1 <= A and Y1 = 0.
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2.
!
!    The probability of a crossing on a single trial is
!
!      P(A,L) = ( 2 * L ) / ( PI * A )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the horizontal spacing between the
!    vertical grid lines.  0 <= A.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!
!    Output, real ( kind = 8 ) PDF, the Buffon PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) L
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793238462643D+00
 
    IF (A < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input A < 0.'
        RETURN
    ELSE IF (A == 0.0D+00) THEN
        PDF = 1.0D+00
        RETURN
    END IF
 
    IF (L < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BUFFON_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input L < 0.'
        RETURN
    ELSE IF (L == 0.0D+00) THEN
        PDF = 0.0D+00
        RETURN
    END IF
 
    PDF = (2.0D+00*L) / (R8_PI*A)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BUFFON_SAMPLE (A, L, TRIAL_NUM)
 
!*****************************************************************************80
!
!! BUFFON_SAMPLE samples the Buffon PDF.
!
!  Discussion:
!
!    In the Buffon needle experiment, we suppose that the plane has been
!    ruled by vertical lines with a spacing of A units, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    Because of the various symmetries, we may assume that this eye of
!    this needle lands in the first infinite strip, and we may further
!    assume that its Y coordinate is 0.  Thus, we have
!    the eye as (X1,Y1) with 0 <= X1 <= A and Y1 = 0.
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2.
!
!    The probability of a crossing on a single trial is
!
!      P(A,L) = ( 2 * L ) / ( PI * A )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!
!    Note that this routine will try to generate 4 * TRIAL_NUM random
!    double precision values at one time, using automatic arrays.
!    When I tried this with TRIAL_NUM = 1,000,000, the program failed,
!    because of internal system limits on such arrays.
!
!    Such a problem could be avoided by using a DO loop running through
!    each trial individually, but this tend to run much more slowly than
!    necessary.
!
!    Since this routine invokes the FORTRAN90 random number generator,
!    the user should initialize the random number generator, particularly
!    if it is desired to control whether the sequence is to be varied
!    or repeated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the horizontal spacing between the
!    vertical grid lines.  0 <= A.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!
!    Input, integer ( kind = 4 ) TRIAL_NUM, the number of times the needle is
!    to be dropped onto the grid.
!
!    Output, integer ( kind = 4 ) BUFFON_SAMPLE, the number of times the
!    needle crossed at least one line of the grid of cells.
!
!  Local Parameters:
!
!    Local, integer BATCH_SIZE, specifies the number of trials to be done
!    in a single batch.  Setting BATCH_SIZE to 1 will be very slow.
!    Replacing it by TRIAL_NUM would be fine except that your system
!    may have a limit on the size of automatic arrays.  We have set a default
!    value of 10,000 here which should be large enough to be efficient
!    but small enough not to annoy the system.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: BATCH_SIZE = 10000
    INTEGER (KIND=4) TRIAL_NUM
 
    REAL (KIND=8) A
    REAL (KIND=8) ANGLE (BATCH_SIZE)
    INTEGER (KIND=4) BATCH
    INTEGER (KIND=4) BUFFON_SAMPLE
    INTEGER (KIND=4) HITS
    REAL (KIND=8) L
    INTEGER (KIND=4) N
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793238462643D+00
    REAL (KIND=8) X1 (BATCH_SIZE)
    REAL (KIND=8) X2 (BATCH_SIZE)
 
    HITS = 0
 
    DO BATCH = 1, TRIAL_NUM, BATCH_SIZE
 
        N = MIN (BATCH_SIZE, TRIAL_NUM+1-BATCH)
!
!  Randomly choose the location (X1,Y1) of the eye of the needle
!  in [0,0]x[A,0], and the angle the needle makes.
!
        CALL RANDOM_NUMBER (HARVEST=X1(1:N))
        CALL RANDOM_NUMBER (HARVEST=ANGLE(1:N))
 
        X1 (1:N) = A * X1 (1:N)
        ANGLE (1:N) = 2.0D+00 * R8_PI * ANGLE (1:N)
!
!  Compute the location of the point of the needle.
!  We only need to know the value of X2, not Y2!
!
        X2 (1:N) = X1 (1:N) + L * COS (ANGLE(1:N))
!
!  Count the end locations that lie outside the cell.
!
        HITS = HITS + COUNT (X2(1:N) <= 0.0 .OR. A <= X2(1:N))
 
    END DO
 
    BUFFON_SAMPLE = HITS
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_CDF (X, A, B, C, D, CDF)
 
!*****************************************************************************80
!
!! BURR_CDF evaluates the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) D
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        CDF = 1.0D+00 - 1.0D+00 / (1.0D+00+Y**C) ** D
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_CDF_INV (CDF, A, B, C, D, X)
 
!*****************************************************************************80
!
!! BURR_CDF_INV inverts the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) D
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BURR_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    Y = ((1.0D+00/(1.0D+00-CDF))**(1.0D+00/D)-1.0D+00) ** (1.0D+00/C)
 
    X = A + B * Y
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BURR_CHECK (A, B, C, D)
 
!*****************************************************************************80
!
!! BURR_CHECK checks the parameters of the Burr CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, logical BURR_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL BURR_CHECK
    REAL (KIND=8) C
    REAL (KIND=8) D
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BURR_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        BURR_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BURR_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C <= 0.'
        BURR_CHECK = .FALSE.
        RETURN
    END IF
 
    BURR_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_MEAN (A, B, C, D, MEAN)
 
!*****************************************************************************80
!
!! BURR_MEAN returns the mean of the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) D
    REAL (KIND=8) MEAN
    REAL (KIND=8) YMEAN
 
    YMEAN = D * R8_BETA (D-1.0D+00/C, 1.0D+00+1.0D+00/C)
 
    MEAN = A + B * YMEAN
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_PDF (X, A, B, C, D, PDF)
 
!*****************************************************************************80
!
!! BURR_PDF evaluates the Burr PDF.
!
!  Discussion:
!
!    Y = ( X - A ) / B;
!
!    PDF(X)(A,B,C,D) = ( C * D / B ) * Y ^ ( C - 1 ) / ( 1 + Y ^ C ) ^ ( D + 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Mark Johnson,
!    Multivariate Statistical Simulation,
!    Wiley, 1987.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) D
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
        PDF = 0.0D+00
    ELSE
 
        Y = (X-A) / B
 
        PDF = (C*D/B) * Y ** (C-1.0D+00) / (1.0D+00+Y**C) ** (D+1.0D+00)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_SAMPLE (A, B, C, D, SEED, X)
 
!*****************************************************************************80
!
!! BURR_SAMPLE samples the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) D
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL BURR_CDF_INV (CDF, A, B, C, D, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BURR_VARIANCE (A, B, C, D, VARIANCE)
 
!*****************************************************************************80
!
!! BURR_VARIANCE returns the variance of the Burr PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) D
    REAL (KIND=8) MU1
    REAL (KIND=8) MU2
    REAL (KIND=8) VARIANCE
 
    IF (C <= 2.0D+00) THEN
 
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BURR_VARIANCE - Warning!'
        WRITE (*, '(a)') '  Variance undefined for C <= 2.'
        VARIANCE = HUGE (VARIANCE)
 
    ELSE
 
        MU1 = B * D * R8_BETA ((C*D-1.0D+00)/C, (C+1.0D+00)/C)
        MU2 = B * B * D * R8_BETA ((C*D-2.0D+00)/C, (C+2.0D+00)/C)
        VARIANCE = - MU1 * MU1 + MU2
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE C8_NORMAL_01_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! C8_NORMAL_01_SAMPLE samples the complex Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, complex ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) V1
    REAL (KIND=8) V2
    COMPLEX (KIND=8) X
    REAL (KIND=8) X_C
    REAL (KIND=8) X_R
 
    V1 = R8_UNIFORM_01 (SEED)
    V2 = R8_UNIFORM_01 (SEED)
 
    X_R = SQRT (-2.0D+00*LOG(V1)) * COS (2.0D+00*R8_PI*V2)
    X_C = SQRT (-2.0D+00*LOG(V1)) * SIN (2.0D+00*R8_PI*V2)
 
    X = CMPLX (X_R, X_C, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! CARDIOID_CDF evaluates the Cardioid CDF.
!
!  Discussion:
!
!    The angle X is assumed to lie between A - PI and A + PI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <= A-R8_PI) THEN
        CDF = 0.0D+00
    ELSE IF (X < A+R8_PI) THEN
        CDF = (R8_PI+X-A+2.0D+00*B*SIN(X-A)) / (2.0D+00*R8_PI)
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! CARDIOID_CDF_INV inverts the Cardioid CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0 <= CDF <= 1.
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) X, the argument with the given CDF.
!    A - PI <= X <= A + PI.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) FP
    REAL (KIND=8) FX
    INTEGER (KIND=4) IT
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8), PARAMETER :: TOL = 0.000001D+00
    REAL (KIND=8) X
 
    IF (CDF <= 0.0D+00) THEN
 
        X = A - R8_PI
 
    ELSE IF (CDF < 1.0D+00) THEN
 
        X = A
 
        IT = 0
 
        DO
 
            FX = CDF - (R8_PI+X-A+2.0D+00*B*SIN(X-A)) / (2.0D+00*R8_PI)
 
            IF (ABS(FX) < TOL) THEN
                EXIT
            END IF
 
            IF (10 < IT) THEN
                RETURN
            END IF
 
            FP = - (1.0D+00+2.0D+00*B*COS(X-A)) / (2.0D+00*R8_PI)
 
            X = X - FX / FP
            X = MAX (X, A-R8_PI)
            X = MIN (X, A+R8_PI)
 
            IT = IT + 1
 
        END DO
 
    ELSE
 
        X = A + R8_PI
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CARDIOID_CHECK (A, B)
 
!*****************************************************************************80
!
!! CARDIOID_CHECK checks the parameters of the Cardioid CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, logical CARDIOID_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL CARDIOID_CHECK
 
    IF (B <-0.5D+00 .OR. 0.5D+00 < B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CARDIOID_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < -0.5 or 0.5 < B.'
        CARDIOID_CHECK = .FALSE.
        RETURN
    END IF
 
    CARDIOID_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! CARDIOID_MEAN returns the mean of the Cardioid PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! CARDIOID_PDF evaluates the Cardioid PDF.
!
!  Discussion:
!
!    The cardioid PDF can be thought of as being applied to points on
!    a circle.  Compare this distribution with the "Cosine PDF".
!
!    PDF(A,B;X) = ( 1 / ( 2 * PI ) ) * ( 1 + 2 * B * COS ( X - A ) )
!    for  A - PI <= X <= A + PI, -1/2 <= B <= 1/2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Fisher,
!    Statistical Analysis of Circular Data,
!    Cambridge, 1993.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    PDF = (1.0D+00+2.0D+00*B*COS(X-A)) / (2.0D+00*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! CARDIOID_SAMPLE samples the Cardioid PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!    A - PI <= X <= A + PI.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL CARDIOID_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDIOID_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! CARDIOID_VARIANCE returns the variance of the Cardioid PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -0.5 <= B <= 0.5.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! CAUCHY_CDF evaluates the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    CDF = 0.5D+00 + ATAN2 (X-A, B) / R8_PI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! CAUCHY_CDF_INV inverts the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CAUCHY_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A + B * TAN (R8_PI*(CDF-0.5D+00))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_CDF_VALUES (N_DATA, MU, SIGMA, X, FX)
 
!*****************************************************************************80
!
!! CAUCHY_CDF_VALUES returns some values of the Cauchy CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = CauchyDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the variance of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.8524163823495667D+00, 0.9220208696226307D+00, 0.9474315432887466D+00, &
   & 0.6475836176504333D+00, 0.6024163823495667D+00, 0.5779791303773693D+00, &
   & 0.5628329581890012D+00, 0.6475836176504333D+00, 0.5000000000000000D+00, &
   & 0.3524163823495667D+00, 0.2500000000000000D+00 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) SIGMA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: SIGMA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        SIGMA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        SIGMA = SIGMA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CAUCHY_CHECK (A, B)
 
!*****************************************************************************80
!
!! CAUCHY_CHECK checks the parameters of the Cauchy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical CAUCHY_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL CAUCHY_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CAUCHY_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        CAUCHY_CHECK = .FALSE.
        RETURN
    END IF
 
    CAUCHY_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! CAUCHY_MEAN returns the mean of the Cauchy PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! CAUCHY_PDF evaluates the Cauchy PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1 / ( PI * B * ( 1 + ( ( X - A ) / B )^2 ) )
!
!    The Cauchy PDF is also known as the Breit-Wigner PDF.  It
!    has some unusual properties.  In particular, the integrals for the
!    expected value and higher order moments are "singular", in the
!    sense that the limiting values do not exist.  A result can be
!    obtained if the upper and lower limits of integration are set
!    equal to +T and -T, and the limit as T => +oo is taken, but
!    this is a very weak and unreliable sort of limit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    PDF = 1.0D+00 / (R8_PI*B*(1.0D+00+Y*Y))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! CAUCHY_SAMPLE samples the Cauchy PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL CAUCHY_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CAUCHY_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! CAUCHY_VARIANCE returns the variance of the Cauchy PDF.
!
!  Discussion:
!
!    The variance of the Cauchy PDF is not well defined.  This routine
!    is made available for completeness only, and simply returns
!    a "very large" number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = HUGE (VARIANCE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV1_CDF (X, CDF)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_CDF evaluates the Chebyshev1 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <-1.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (1.0D+00 < X) THEN
        CDF = 1.0D+00
    ELSE
        CDF = 0.5 + ASIN (X) / R8_PI
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV1_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_CDF_INV inverts the Chebyshev1 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHEBYSHEV1_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = SIN (R8_PI*(CDF-0.5))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV1_MEAN (MEAN)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_MEAN returns the mean of the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV1_PDF (X, PDF)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_PDF evaluates the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <-1.0D+00 .OR. 1.0D+00 < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 1.0D+00 / R8_PI / SQRT (1.0D+00-X*X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CHEBYSHEV1_SAMPLE (SEED)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_SAMPLE samples the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) CHEBYSHEV1_SAMPLE, a random value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) CHEBYSHEV1_SAMPLE
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL CHEBYSHEV1_CDF_INV (CDF, X)
 
    CHEBYSHEV1_SAMPLE = X
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV1_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! CHEBYSHEV1_VARIANCE returns the variance of the Chebyshev1 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 0.5D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV2_CDF (X, CDF)
 
!*****************************************************************************80
!
!! CHEBYSHEV2_CDF evaluates the Chebyshev2 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <=-1.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (1.0D+00 <= X) THEN
        CDF = 1.0D+00
    ELSE
        CDF = 0.5D+00 + (X*SQRT(1.0D+00-X*X)+ASIN(X)) / R8_PI
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV2_MEAN (MEAN)
 
!*****************************************************************************80
!
!! CHEBYSHEV2_MEAN returns the mean of the Chebyshev2 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV2_PDF (X, PDF)
 
!*****************************************************************************80
!
!! CHEBYSHEV2_PDF evaluates the Chebyshev2 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <-1.0D+00 .OR. 1.0D+00 < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 2.0D+00 * SQRT (1.0D+00-X*X) / R8_PI
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV2_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! CHEBYSHEV2_VARIANCE returns the variance of the Chebyshev2 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = R8_PI / 8.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! CHI_CDF evaluates the Chi CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) P2
    REAL (KIND=8) X
    REAL (KIND=8) X2
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
        X2 = 0.5D+00 * Y * Y
        P2 = 0.5D+00 * C
 
        CDF = R8_GAMMA_INC (P2, X2)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! CHI_CDF_INV inverts the Chi CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = A
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
        RETURN
    END IF
 
    X1 = A
    CDF1 = 0.0D+00
 
    X2 = A + 1.0D+00
 
    DO
 
        CALL CHI_CDF (X2, A, B, C, CDF2)
 
        IF (CDF < CDF2) THEN
            EXIT
        END IF
 
        X2 = A + 2.0D+00 * (X2-A)
 
    END DO
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL CHI_CDF (X3, A, B, C, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            RETURN
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CHI_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CHI_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! CHI_CHECK checks the parameters of the Chi CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, logical CHI_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL CHI_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.0.'
        CHI_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_CHECK - Warning!'
        WRITE (*, '(a)') '  C <= 0.0.'
        CHI_CHECK = .FALSE.
        RETURN
    END IF
 
    CHI_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! CHI_MEAN returns the mean of the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A + SQRT (2.0D+00) * B * GAMMA (0.5D+00*(C+1.0D+00)) / GAMMA (0.5D+00*C)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! CHI_PDF evaluates the Chi PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = EXP ( - 0.5D+00 * ( ( X - A ) / B )^2 )
!      * ( ( X - A ) / B )^( C - 1 ) /
!      ( 2^( 0.5D+00 * C - 1 ) * B * GAMMA ( 0.5D+00 * C ) )
!
!    CHI(A,B,1) is the Half Normal PDF;
!    CHI(0,B,2) is the Rayleigh PDF;
!    CHI(0,B,3) is the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = EXP (-0.5D+00*Y*Y) * Y ** (C-1.0D+00) / &
       & (2.0D+00**(0.5D+00*C-1.0D+00)*B*GAMMA(0.5D+00*C))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! CHI_SAMPLE samples the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CALL CHI_SQUARE_SAMPLE (C, SEED, X)
 
    X = A + B * SQRT (X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! CHI_VARIANCE returns the variance of the Chi PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * (C-2.0D+00*(GAMMA(0.5D+00*(C+1.0D+00))/GAMMA(0.5D+00*C))**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! CHI_SQUARE_CDF evaluates the Chi squared CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value of the random deviate.
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution, usually
!    the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B2
    REAL (KIND=8) C2
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    X2 = 0.5D+00 * X
 
    A2 = 0.0D+00
    B2 = 1.0D+00
    C2 = 0.5D+00 * A
 
    CALL GAMMA_CDF (X2, A2, B2, C2, CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! CHI_SQUARE_CDF_INV inverts the Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2004
!
!  Author:
!
!    Original FORTAN77 version by Donald Best, Roberts.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Donald Best, Roberts,
!    The Percentage Points of the Chi-Squared Distribution,
!    Algorithm AS 91,
!    Applied Statistics,
!    Volume 24, Number ?, pages 385-390, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, a value of the chi-squared cumulative
!    probability density function.
!    0.000002 <= CDF <= 0.999998.
!
!    Input, real ( kind = 8 ) A, the parameter of the chi-squared
!    probability density function.  0 < A.
!
!    Output, real ( kind = 8 ) X, the value of the chi-squared random deviate
!    with the property that the probability that a chi-squared random
!    deviate with parameter A is less than or equal to X is CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8), PARAMETER :: AA = 0.6931471806D+00
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: C1 = 0.01D+00
    REAL (KIND=8), PARAMETER :: C2 = 0.222222D+00
    REAL (KIND=8), PARAMETER :: C3 = 0.32D+00
    REAL (KIND=8), PARAMETER :: C4 = 0.4D+00
    REAL (KIND=8), PARAMETER :: C5 = 1.24D+00
    REAL (KIND=8), PARAMETER :: C6 = 2.2D+00
    REAL (KIND=8), PARAMETER :: C7 = 4.67D+00
    REAL (KIND=8), PARAMETER :: C8 = 6.66D+00
    REAL (KIND=8), PARAMETER :: C9 = 6.73D+00
    REAL (KIND=8), PARAMETER :: C10 = 13.32D+00
    REAL (KIND=8), PARAMETER :: C11 = 60.0D+00
    REAL (KIND=8), PARAMETER :: C12 = 70.0D+00
    REAL (KIND=8), PARAMETER :: C13 = 84.0D+00
    REAL (KIND=8), PARAMETER :: C14 = 105.0D+00
    REAL (KIND=8), PARAMETER :: C15 = 120.0D+00
    REAL (KIND=8), PARAMETER :: C16 = 127.0D+00
    REAL (KIND=8), PARAMETER :: C17 = 140.0D+00
    REAL (KIND=8), PARAMETER :: C18 = 175.0D+00
    REAL (KIND=8), PARAMETER :: C19 = 210.0D+00
    REAL (KIND=8), PARAMETER :: C20 = 252.0D+00
    REAL (KIND=8), PARAMETER :: C21 = 264.0D+00
    REAL (KIND=8), PARAMETER :: C22 = 294.0D+00
    REAL (KIND=8), PARAMETER :: C23 = 346.0D+00
    REAL (KIND=8), PARAMETER :: C24 = 420.0D+00
    REAL (KIND=8), PARAMETER :: C25 = 462.0D+00
    REAL (KIND=8), PARAMETER :: C26 = 606.0D+00
    REAL (KIND=8), PARAMETER :: C27 = 672.0D+00
    REAL (KIND=8), PARAMETER :: C28 = 707.0D+00
    REAL (KIND=8), PARAMETER :: C29 = 735.0D+00
    REAL (KIND=8), PARAMETER :: C30 = 889.0D+00
    REAL (KIND=8), PARAMETER :: C31 = 932.0D+00
    REAL (KIND=8), PARAMETER :: C32 = 966.0D+00
    REAL (KIND=8), PARAMETER :: C33 = 1141.0D+00
    REAL (KIND=8), PARAMETER :: C34 = 1182.0D+00
    REAL (KIND=8), PARAMETER :: C35 = 1278.0D+00
    REAL (KIND=8), PARAMETER :: C36 = 1740.0D+00
    REAL (KIND=8), PARAMETER :: C37 = 2520.0D+00
    REAL (KIND=8), PARAMETER :: C38 = 5040.0D+00
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: CDF_MAX = 0.999998D+00
    REAL (KIND=8), PARAMETER :: CDF_MIN = 0.000002D+00
    REAL (KIND=8) CH
    REAL (KIND=8), PARAMETER :: E = 0.0000005D+00
    REAL (KIND=8) G
    INTEGER (KIND=4) I
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 20
    REAL (KIND=8) P1
    REAL (KIND=8) P2
    REAL (KIND=8) Q
    REAL (KIND=8) S1
    REAL (KIND=8) S2
    REAL (KIND=8) S3
    REAL (KIND=8) S4
    REAL (KIND=8) S5
    REAL (KIND=8) S6
    REAL (KIND=8) T
    REAL (KIND=8) X
    REAL (KIND=8) X2
    REAL (KIND=8) XX
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        WRITE (*, '(a,g14.6)') '  CDF = ', CDF
        RETURN
    END IF
 
    IF (CDF < CDF_MIN) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_CDF_INV - Warning!'
        WRITE (*, '(a)') '  CDF < CDF_MIN.'
        WRITE (*, '(a,g14.6)') '  CDF = ', CDF
        WRITE (*, '(a,g14.6)') '  CDF_MIN = ', CDF_MIN
    END IF
 
    IF (CDF_MAX < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_CDF_INV - Warning!'
        WRITE (*, '(a)') '  CDF_MAX < CDF.'
        WRITE (*, '(a,g14.6)') '  CDF = ', CDF
        WRITE (*, '(a,g14.6)') '  CDF_MAX = ', CDF_MAX
    END IF
 
    XX = 0.5D+00 * A
    C = XX - 1.0D+00
!
!  Compute Log ( Gamma ( A/2 ) ).
!
    G = LOG_GAMMA (A/2.0D+00)
!
!  Starting approximation for small chi-squared.
!
    IF (A <-C5*LOG(CDF)) THEN
 
        CH = (CDF*XX*EXP(G+XX*AA)) ** (1.0D+00/XX)
 
        IF (CH < E) THEN
            X = CH
            RETURN
        END IF
!
!  Starting approximation for A less than or equal to 0.32.
!
    ELSE IF (A <= C3) THEN
 
        CH = C4
        A2 = LOG (1.0D+00-CDF)
 
        DO
 
            Q = CH
            P1 = 1.0D+00 + CH * (C7+CH)
            P2 = CH * (C9+CH*(C8+CH))
 
            T = - 0.5D+00 + (C7+2.0D+00*CH) / P1 - (C9+CH*(C10+3.0D+00*CH)) / P2
 
            CH = CH - (1.0D+00-EXP(A2+G+0.5D+00*CH+C*AA)*P2/P1) / T
 
            IF (ABS(Q/CH-1.0D+00) <= C1) THEN
                EXIT
            END IF
 
        END DO
!
!  Call to algorithm AS 111.
!  Note that P has been tested above.
!  AS 241 could be used as an alternative.
!
    ELSE
 
        CALL NORMAL_01_CDF_INV (CDF, X2)
!
!  Starting approximation using Wilson and Hilferty estimate.
!
        P1 = C2 / A
        CH = A * (X2*SQRT(P1)+1.0D+00-P1) ** 3
!
!  Starting approximation for P tending to 1.
!
        IF (C6*A+6.0D+00 < CH) THEN
            CH = - 2.0D+00 * (LOG(1.0D+00-CDF)-C*LOG(0.5D+00*CH)+G)
        END IF
 
    END IF
!
!  Call to algorithm AS 239 and calculation of seven term Taylor series.
!
    DO I = 1, IT_MAX
 
        Q = CH
        P1 = 0.5D+00 * CH
        P2 = CDF - R8_GAMMA_INC (XX, P1)
        T = P2 * EXP (XX*AA+G+P1-C*LOG(CH))
        B = T / CH
        A2 = 0.5D+00 * T - B * C
 
        S1 = (C19+A2*(C17+A2*(C14+A2*(C13+A2*(C12+A2*C11))))) / C24
 
        S2 = (C24+A2*(C29+A2*(C32+A2*(C33+A2*C35)))) / C37
 
        S3 = (C19+A2*(C25+A2*(C28+A2*C31))) / C37
 
        S4 = (C20+A2*(C27+A2*C34)+C*(C22+A2*(C30+A2*C36))) / C38
 
        S5 = (C13+C21*A2+C*(C18+C26*A2)) / C37
 
        S6 = (C15+C*(C23+C16*C)) / C38
 
        CH = CH + T * (1.0D+00+0.5D+00*T*S1-B*C*(S1-B*(S2-B*(S3-B*(S4-B*(S5-B*S6))))))
 
        IF (E < ABS(Q/CH-1.0D+00)) THEN
            X = CH
            RETURN
        END IF
 
    END DO
 
    X = CH
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'CHI_SQUARE_CDF_INV - Warning!'
    WRITE (*, '(a)') '  Convergence not reached.'
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_CDF_VALUES (N_DATA, A, X, FX)
 
!*****************************************************************************80
!
!! CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = ChiSquareDistribution [ df ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, the parameter of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 21
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 1, 2, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4, &
   & 5, 3, 3, 3, 3, 3, 10, 10, 10 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.7965567455405796D-01, &
   & 0.4987520807317687D-02, 0.1124629160182849D+00, 0.9950166250831946D-02, &
   & 0.4729107431344619D+00, 0.1812692469220181D+00, 0.5975750516063926D-01, &
   & 0.1752309630642177D-01, 0.6826894921370859D+00, 0.3934693402873666D+00, &
   & 0.1987480430987992D+00, 0.9020401043104986D-01, 0.3743422675270363D-01, &
   & 0.4275932955291202D+00, 0.6083748237289110D+00, 0.7385358700508894D+00, &
   & 0.8282028557032669D+00, 0.8883897749052874D+00, 0.1721156299558408D-03, &
   & 0.3659846827343712D-02, 0.1857593622214067D-01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.01D+00, 0.01D+00, 0.02D+00, &
   & 0.02D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
   & 1.00D+00, 2.00D+00, 3.00D+00, 4.00D+00, 5.00D+00, 6.00D+00, 1.00D+00, 2.00D+00, 3.00D+00 &
   & /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CHI_SQUARE_CHECK (A)
 
!*****************************************************************************80
!
!! CHI_SQUARE_CHECK checks the parameter of the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution.
!    1 <= A.
!
!    Output, logical CHI_SQUARE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL CHI_SQUARE_CHECK
 
    IF (A < 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_CHECK - Warning!'
        WRITE (*, '(a)') '  A < 1.0.'
        CHI_SQUARE_CHECK = .FALSE.
        RETURN
    END IF
 
    CHI_SQUARE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! CHI_SQUARE_MEAN returns the mean of the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution.
!    1 <= A.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! CHI_SQUARE_PDF evaluates the central Chi squared PDF.
!
!  Discussion:
!
!    PDF(A;X) =
!      EXP ( - X / 2 ) * X^((A-2)/2) / ( 2^(A/2) * GAMMA ( A/2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1 <= A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        B = A / 2.0D+00
        PDF = EXP (-0.5D+00*X) * X ** (B-1.0D+00) / (2.0D+00**B*GAMMA(B))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! CHI_SQUARE_SAMPLE samples the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1 <= A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B2
    REAL (KIND=8) C2
    INTEGER (KIND=4) I
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    INTEGER (KIND=4) N
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    N = INT (A)
 
    IF (REAL(N, KIND=8) == A .AND. N <= IT_MAX) THEN
 
        X = 0.0D+00
        DO I = 1, N
            CALL NORMAL_01_SAMPLE (SEED, X2)
            X = X + X2 * X2
        END DO
 
    ELSE
 
        A2 = 0.0D+00
        B2 = 1.0D+00
        C2 = A / 2.0D+00
 
        CALL GAMMA_SAMPLE (A2, B2, C2, SEED, X)
 
        X = 2.0D+00 * X
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! CHI_SQUARE_VARIANCE returns the variance of the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the distribution.
!    1 <= A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 2.0D+00 * A
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CHI_SQUARE_NONCENTRAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_CHECK check parameters of noncentral Chi Squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, logical CHI_SQUARE_NONCENTRAL_CHECK, is true if the parameters
!    are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL CHI_SQUARE_NONCENTRAL_CHECK
 
    IF (A < 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_NONCENTRAL_CHECK - Warning!'
        WRITE (*, '(a)') '  A < 1.'
        CHI_SQUARE_NONCENTRAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHI_SQUARE_NONCENTRAL_CHECK - Warning!'
        WRITE (*, '(a)') '  B < 0.'
        CHI_SQUARE_NONCENTRAL_CHECK = .FALSE.
        RETURN
    END IF
 
    CHI_SQUARE_NONCENTRAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_NONCENTRAL_CDF_VALUES (N_DATA, DF, LAMBDA, X, CDF)
 
!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NoncentralChiSquareDistribution [ df, lambda ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) DF, the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) CDF, the noncentral chi CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 28
 
    REAL (KIND=8) CDF
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: CDF_VEC = (/ 0.8399444269398261D+00, &
   & 0.6959060300435139D+00, 0.5350879697078847D+00, 0.7647841496310313D+00, &
   & 0.6206436532195436D+00, 0.4691667375373180D+00, 0.3070884345937569D+00, &
   & 0.2203818092990903D+00, 0.1500251895581519D+00, 0.3071163194335791D-02, &
   & 0.1763982670131894D-02, 0.9816792594625022D-03, 0.1651753140866208D-01, &
   & 0.2023419573950451D-03, 0.4984476352854074D-06, 0.1513252400654827D-01, &
   & 0.2090414910614367D-02, 0.2465021206048452D-03, 0.2636835050342939D-01, &
   & 0.1857983220079215D-01, 0.1305736595486640D-01, 0.5838039534819351D-01, &
   & 0.4249784402463712D-01, 0.3082137716021596D-01, 0.1057878223400849D+00, &
   & 0.7940842984598509D-01, 0.5932010895599639D-01, 0.2110395656918684D+00 /)
    INTEGER (KIND=4) DF
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: DF_VEC = (/ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, &
   & 3, 60, 80, 100, 1, 2, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10, 8 /)
    REAL (KIND=8) LAMBDA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 20.0D+00, 20.0D+00, 20.0D+00, &
   & 30.0D+00, 30.0D+00, 30.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
   & 2.0D+00, 3.0D+00, 4.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 0.5D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 3.000D+00, 3.000D+00, 3.000D+00, &
   & 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, &
   & 3.000D+00, 60.000D+00, 60.000D+00, 60.000D+00, 0.050D+00, 0.050D+00, 0.050D+00, 4.000D+00, &
   & 4.000D+00, 4.000D+00, 5.000D+00, 5.000D+00, 5.000D+00, 6.000D+00, 6.000D+00, 6.000D+00, &
   & 5.000D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        LAMBDA = 0.0D+00
        DF = 0
        CDF = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        LAMBDA = LAMBDA_VEC (N_DATA)
        DF = DF_VEC (N_DATA)
        CDF = CDF_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_NONCENTRAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_MEAN: mean of the noncentral Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A + B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_NONCENTRAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_SAMPLE samples the noncentral Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the parameter of the PDF.
!    1.0D+00 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A1
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
 
    A1 = A - 1.0D+00
 
    CALL CHI_SQUARE_SAMPLE (A1, SEED, X1)
 
    A2 = SQRT (B)
    B2 = 1.0D+00
    CALL NORMAL_SAMPLE (A2, B2, SEED, X2)
 
    X = X1 + X2 * X2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_SQUARE_NONCENTRAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! CHI_SQUARE_NONCENTRAL_VARIANCE: variance of the noncentral Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1 <= A.
!
!    Input, real ( kind = 8 ) B, the noncentrality parameter of the PDF.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 2.0D+00 * (A+2.0D+00*B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_01_MEAN (MEAN)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_MEAN returns the mean of the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN(2), the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN (2)
 
    MEAN (1:2) = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_01_PDF (X, PDF)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_PDF evaluates the Circular Normal 01 PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - 0.5D+00 * ( X(1)^2 + X(2)^2 ) ) / ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(2), the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X (2)
 
    PDF = EXP (-0.5D+00*(X(1)**2+X(2)**2)) / (2.0D+00*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_01_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_SAMPLE samples the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(2), a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) V1
    REAL (KIND=8) V2
    REAL (KIND=8) X (2)
 
    V1 = R8_UNIFORM_01 (SEED)
    V2 = R8_UNIFORM_01 (SEED)
 
    X (1) = SQRT (-2.0D+00*LOG(V1)) * COS (2.0D+00*R8_PI*V2)
    X (2) = SQRT (-2.0D+00*LOG(V1)) * SIN (2.0D+00*R8_PI*V2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_01_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_01_VARIANCE: variance of the Circular Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE(2), the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE (2)
 
    VARIANCE (1) = 1.0D+00
    VARIANCE (2) = 1.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_MEAN returns the mean of the Circular Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Output, real ( kind = 8 ) MEAN(2), the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A (2)
    REAL (KIND=8) B
    REAL (KIND=8) MEAN (2)
 
    MEAN (1:2) = A (1:2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_PDF evaluates the Circular Normal PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - 0.5D+00 * ( ( (X(1)-A(1))^2 + (X(2)-A(2))^2 ) / B^2 )
!      / ( 2 * PI * B^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(2), the argument of the PDF.
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A (2)
    REAL (KIND=8) B
    REAL (KIND=8) D
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X (2)
 
    D = ((X(1)-A(1))**2+(X(2)-A(2))**2) / B ** 2
 
    PDF = EXP (-0.5D+00*D) / (2.0D+00*B**2*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_SAMPLE samples the Circular Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(2), a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A (2)
    REAL (KIND=8) B
    REAL (KIND=8) R
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) V1
    REAL (KIND=8) V2
    REAL (KIND=8) X (2)
 
    V1 = R8_UNIFORM_01 (SEED)
    V2 = R8_UNIFORM_01 (SEED)
 
    R = SQRT (-2.0D+00*LOG(V1))
 
    X (1) = A (1) + B * R * COS (2.0D+00*R8_PI*V2)
    X (2) = A (2) + B * R * SIN (2.0D+00*R8_PI*V2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CIRCULAR_NORMAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! CIRCULAR_NORMAL_VARIANCE returns the variance of the Circular Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(2), a parameter of the PDF, the mean value.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF, the standard deviation.
!
!    Output, real ( kind = 8 ) VARIANCE(2), the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A (2)
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE (2)
 
    VARIANCE (1) = B ** 2
    VARIANCE (2) = B ** 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! COSINE_CDF evaluates the Cosine CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A-R8_PI*B) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (X <= A+R8_PI*B) THEN
 
        Y = (X-A) / B
 
        CDF = 0.5D+00 + (Y+SIN(Y)) / (2.0D+00*R8_PI)
 
    ELSE IF (A+R8_PI*B < X) THEN
 
        CDF = 1.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! COSINE_CDF_INV inverts the Cosine CDF.
!
!  Discussion:
!
!    A simple bisection method is used on the interval
!    [ A - PI * B, A + PI * B ].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COSINE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = A - R8_PI * B
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = A + R8_PI * B
        RETURN
    END IF
 
    X1 = A - R8_PI * B
    CDF1 = 0.0D+00
 
    X2 = A + R8_PI * B
    CDF2 = 1.0D+00
!
!  Now use bisection.
!
    IT = 0
 
    DO IT = 1, IT_MAX
 
        X3 = 0.5D+00 * (X1+X2)
        CALL COSINE_CDF (X3, A, B, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'COSINE_CDF_INV - Fatal error!'
    WRITE (*, '(a)') '  Iteration limit exceeded.'
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION COSINE_CHECK (A, B)
 
!*****************************************************************************80
!
!! COSINE_CHECK checks the parameters of the Cosine CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical COSINE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL COSINE_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COSINE_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.0'
        COSINE_CHECK = .FALSE.
        RETURN
    END IF
 
    COSINE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! COSINE_MEAN returns the mean of the Cosine PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! COSINE_PDF evaluates the Cosine PDF.
!
!  Discussion:
!
!    The cosine PDF can be thought of as being applied to points on
!    a circle.
!
!    PDF(A,B;X) = ( 1 / ( 2 * PI * B ) ) * COS ( ( X - A ) / B )
!    for A - PI * B <= X <= A + PI * B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X < A-R8_PI*B) THEN
        PDF = 0.0D+00
 
    ELSE IF (X <= A+R8_PI*B) THEN
 
        Y = (X-A) / B
 
        PDF = 1.0D+00 / (2.0D+00*R8_PI*B) * COS (Y)
 
    ELSE IF (A+R8_PI*B < X) THEN
 
        PDF = 0.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! COSINE_SAMPLE samples the Cosine PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL COSINE_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COSINE_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! COSINE_VARIANCE returns the variance of the Cosine PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (R8_PI*R8_PI/3.0D+00-2.0D+00) * B * B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COUPON_COMPLETE_PDF (TYPE_NUM, BOX_NUM, PDF)
 
!*****************************************************************************80
!
!! COUPON_COMPLETE_PDF evaluates the Complete Coupon Collection PDF.
!
!  Discussion:
!
!    PDF(TYPE_NUM;BOX_NUM) is the probability that, given an inexhaustible
!    supply of boxes, inside each of which there is one of TYPE_NUM distinct
!    coupons, which are uniformly distributed among the boxes, that it will
!    require opening exactly BOX_NUM boxes to achieve at least one of each
!    kind of coupon.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Herbert Wilf,
!    Some New Aspects of the Coupon Collector's Problem,
!    SIAM Review,
!    Volume 48, Number 3, September 2006, pages 549-565.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) BOX_NUM, the number of boxes that had to be
!    opened in order to just get at least one of each coupon.
!    0 <= BOX_NUM.  If BOX_NUM < TYPE_NUM, then PDF is surely 0.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of distinct coupons.
!    1 <= TYPE_NUM.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) BOX_NUM
    REAL (KIND=8) FACTOR
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    INTEGER (KIND=4) TYPE_NUM
!
!  Nonsense cases.
!
    IF (BOX_NUM < 0) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (TYPE_NUM < 1) THEN
 
        PDF = 0.0D+00
!
!  Degenerate but meaningful case.
!
    ELSE IF (TYPE_NUM == 1) THEN
 
        IF (BOX_NUM == 1) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
!
!  Easy cases.
!
    ELSE IF (BOX_NUM < TYPE_NUM) THEN
 
        PDF = 0.0D+00
!
!  General case.
!
    ELSE
 
        FACTOR = 1.0D+00
        DO I = 1, TYPE_NUM
            FACTOR = FACTOR * REAL (I, KIND=8) / REAL (TYPE_NUM, KIND=8)
        END DO
        DO I = TYPE_NUM + 1, BOX_NUM
            FACTOR = FACTOR / REAL (TYPE_NUM, KIND=8)
        END DO
 
        PDF = FACTOR * REAL (STIRLING2_VALUE(BOX_NUM-1, TYPE_NUM-1), KIND=8)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COUPON_MEAN (J, TYPE_NUM, MEAN)
 
!*****************************************************************************80
!
!! COUPON_MEAN returns the mean of the Coupon PDF.
!
!  Discussion:
!
!    In this version of the coupon collector's problem, we assume
!    that each box contains 1 coupon, that there are TYPE_NUM distinct types
!    of coupon, uniformly distributed among an inexhaustible supply
!    of boxes, and that the collector's goal is to get J distinct
!    types of coupons by opening one box after another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) J, the number of distinct coupons to be
!    collected.  J must be between 1 and TYPE_NUM.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of distinct coupons.
!
!    Output, real ( kind = 8 ) MEAN, the mean number of boxes that
!    must be opened in order to just get J distinct kinds.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) TYPE_NUM
 
    IF (TYPE_NUM < J) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COUPON_MEAN - Fatal error!'
        WRITE (*, '(a)') '  Number of distinct coupons desired must be no more'
        WRITE (*, '(a)') '  than the total number of distinct coupons.'
        RETURN
    END IF
 
    MEAN = 0.0D+00
    DO I = 1, J
        MEAN = MEAN + 1.0D+00 / REAL (TYPE_NUM-I+1, KIND=8)
    END DO
    MEAN = MEAN * REAL (TYPE_NUM, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COUPON_SAMPLE (TYPE_NUM, SEED, COUPON, BOX_NUM)
 
!*****************************************************************************80
!
!! COUPON_SAMPLE simulates the coupon collector's problem.
!
!  Discussion:
!
!    The coupon collector needs to collect one of each of TYPE_NUM
!    coupons.  The collector may draw one coupon (or, in some settings,
!    open one box) on each trial, and takes as many trials as necessary
!    to complete the task.  On each trial, the probability of picking
!    any particular type of coupon is always 1 / TYPE_NUM.
!
!    Interesting questions include;
!
!    * what is the expected number of drawings necessary to complete
!      the collection?
!
!    * How does the expected number of drawings necessary to complete
!      the collection vary as TYPE_NUM increases?
!
!    * What is the distribution of the numbers of each type of coupon
!      in a typical collection when it is just completed?
!
!    As TYPE_NUM increases, the number of coupons necessary to be
!    collected in order to get a complete set in any simulation
!    strongly tends to the value TYPE_NUM * LOG ( TYPE_NUM ).
!
!    If TYPE_NUM is 1, the simulation ends with a single drawing.
!
!    If TYPE_NUM is 2, then we may call the coupon taken on the first drawing
!    a "Head", say, and the process then is similar to the question of the
!    length, plus one, of a run of Heads or Tails in coin flipping.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of types of coupons.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) COUPON(TYPE_NUM), the number of coupons
!    of each type that were collected during the simulation.
!
!    Output, integer ( kind = 4 ) BOX_NUM, the total number of boxes opened.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) TYPE_NUM
 
    INTEGER (KIND=4), PARAMETER :: BOX_MAX = 2000
    INTEGER (KIND=4) BOX_NUM
    INTEGER (KIND=4) COUPON (TYPE_NUM)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) STRAIGHT
 
    COUPON (1:TYPE_NUM) = 0
 
    STRAIGHT = 0
    BOX_NUM = 0
!
!  Draw another coupon.
!
    DO WHILE (BOX_NUM < BOX_MAX)
 
        I = I4_UNIFORM_AB (1, TYPE_NUM, SEED)
!
!  Increment the number of I coupons.
!
        COUPON (I) = COUPON (I) + 1
        BOX_NUM = BOX_NUM + 1
!
!  If I is the next one we needed, increase STRAIGHT by 1.
!
        IF (I == STRAIGHT+1) THEN
 
            DO
 
                STRAIGHT = STRAIGHT + 1
!
!  If STRAIGHT = TYPE_NUM, we have all of them.
!
                IF (TYPE_NUM <= STRAIGHT) THEN
                    RETURN
                END IF
!
!  If the next coupon has not been collected, our straight is over.
!
                IF (COUPON(STRAIGHT+1) <= 0) THEN
                    EXIT
                END IF
 
            END DO
 
        END IF
 
    END DO
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'COUPON_SAMPLE - Fatal error!'
    WRITE (*, '(a)') '  Maximum number of coupons drawn without success.'
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COUPON_VARIANCE (J, TYPE_NUM, VARIANCE)
 
!*****************************************************************************80
!
!! COUPON_VARIANCE returns the variance of the Coupon PDF.
!
!  Discussion:
!
!    In this version of the coupon collector's problem, we assume
!    that each box contains 1 coupon, that there are TYPE_NUM distinct types
!    of coupon, uniformly distributed among an inexhaustible supply
!    of boxes, and that the collector's goal is to get J distinct
!    types of coupons by opening one box after another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) J, the number of distinct coupons to be
!    collected.  1 <= J <= TYPE_NUM.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of types of coupons.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the number of
!    boxes that must be opened in order to just get J distinct kinds
!    of coupons.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) TYPE_NUM
    REAL (KIND=8) VARIANCE
 
    IF (TYPE_NUM < J) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COUPON_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  Number of distinct coupons desired must be no more'
        WRITE (*, '(a)') '  than the total number of distinct coupons.'
        RETURN
    END IF
 
    VARIANCE = 0.0D+00
    DO I = 1, J
        VARIANCE = VARIANCE + REAL (I-1, KIND=8) / REAL (TYPE_NUM-I+1, KIND=8) ** 2
    END DO
    VARIANCE = VARIANCE * REAL (TYPE_NUM, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! DERANGED_CDF evaluates the Deranged CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of items in
!    their correct places.
!    0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) CNK
    INTEGER (KIND=4) DNMK
    INTEGER (KIND=4) SUM2
    INTEGER (KIND=4) X
    INTEGER (KIND=4) X2
 
    IF (X < 0 .OR. A < X) THEN
        CDF = 0.0D+00
    ELSE
        SUM2 = 0
        DO X2 = 0, X
            CNK = I4_CHOOSE (A, X2)
            DNMK = DERANGED_ENUM (A-X2)
            SUM2 = SUM2 + CNK * DNMK
        END DO
        CDF = REAL (SUM2, KIND=8) / R8_FACTORIAL (A)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! DERANGED_CDF_INV inverts the Deranged CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) X2
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DERANGED_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CDF2 = 0.0D+00
 
    DO X2 = 0, A
 
        CALL DERANGED_PDF (X2, A, PDF)
 
        CDF2 = CDF2 + PDF
 
        IF (CDF <= CDF2) THEN
            X = X2
            RETURN
        END IF
 
    END DO
 
    X = A
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DERANGED_CHECK (A)
 
!*****************************************************************************80
!
!! DERANGED_CHECK checks the parameter of the Deranged PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the total number of items.
!    1 <= A.
!
!    Output, logical DERANGED_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    LOGICAL DERANGED_CHECK
 
    IF (A < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DERANGED_CHECK - Warning!'
        WRITE (*, '(a)') '  A < 1.'
        DERANGED_CHECK = .FALSE.
        RETURN
    END IF
 
    DERANGED_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DERANGED_ENUM (N)
 
!*****************************************************************************80
!
!! DERANGED_ENUM returns the number of derangements of N objects.
!
!  Discussion:
!
!    A derangement of N objects is a permutation with no fixed
!    points.  If we symbolize the permutation operation by "P",
!    then for a derangment, P(I) is never equal to I.
!
!      D(0) = 1
!      D(1) = 0
!      D(2) = 1
!      D(N) = (N-1) * ( D(N-1) + D(N-2) )
!
!    or
!
!      D(0) = 1
!      D(1) = 0
!      D(N) = N * D(N-1) + (-1)^N
!
!    D(N) = N! * ( 1 - 1/1! + 1/2! - 1/3! ... 1/N! )
!
!    Based on the inclusion/exclusion law.
!
!    D(N) is the number of ways of placing N non-attacking rooks on
!    an N by N chessboard with one diagonal deleted.
!
!    Limit ( N -> Infinity ) D(N)/N! = 1 / e.
!
!    The number of permutations with exactly K items in the right
!    place is COMB(N,K) * D(N-K).
!
!  First values:
!
!     N         D(N)
!     0           1
!     1           0
!     2           1
!     3           2
!     4           9
!     5          44
!     6         265
!     7        1854
!     8       14833
!     9      133496
!    10     1334961
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    Output, integer ( kind = 4 ) DERANGED_ENUM, the number of derangements
!    of N objects.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) DERANGED_ENUM
    INTEGER (KIND=4) DN
    INTEGER (KIND=4) DNM1
    INTEGER (KIND=4) DNM2
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
 
    IF (N < 0) THEN
 
        DN = 0
 
    ELSE IF (N == 0) THEN
 
        DN = 1
 
    ELSE IF (N == 1) THEN
 
        DN = 0
 
    ELSE IF (N == 2) THEN
 
        DN = 1
 
    ELSE
 
        DNM1 = 0
        DN = 1
 
        DO I = 3, N
            DNM2 = DNM1
            DNM1 = DN
            DN = (I-1) * (DNM1+DNM2)
        END DO
 
    END IF
 
    DERANGED_ENUM = DN
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! DERANGED_MEAN returns the mean of the Deranged CDF.
!
!  Discussion:
!
!    The mean is computed by straightforward summation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) MEAN
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    MEAN = 0.0D+00
    DO X = 0, A
        CALL DERANGED_PDF (X, A, PDF)
        MEAN = MEAN + PDF * X
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! DERANGED_PDF evaluates the Deranged PDF.
!
!  Discussion:
!
!    PDF(A;X) is the probability that exactly X items will occur in
!    their proper place after a random permutation of A items.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of items in their
!    correct places.  0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the total number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) CNK
    INTEGER (KIND=4) DNMK
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0 .OR. A < X) THEN
        PDF = 0.0D+00
    ELSE
        CNK = I4_CHOOSE (A, X)
        DNMK = DERANGED_ENUM (A-X)
        PDF = REAL (CNK*DNMK, KIND=8) / LOG_GAMMA (REAL(A+1, KIND=8))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! DERANGED_SAMPLE samples the Deranged PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL DERANGED_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DERANGED_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! DERANGED_VARIANCE returns the variance of the Deranged CDF.
!
!  Discussion:
!
!    The variance is computed by straightforward summation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) MEAN
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    REAL (KIND=8) VARIANCE
 
    CALL DERANGED_MEAN (A, MEAN)
 
    VARIANCE = 0.0D+00
    DO X = 0, A
        CALL DERANGED_PDF (X, A, PDF)
        VARIANCE = VARIANCE + PDF * (X-MEAN) ** 2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DIGAMMA (X)
 
!*****************************************************************************80
!
!! DIGAMMA calculates the digamma or Psi function.
!
!  Discussion:
!
!    DiGamma ( X ) = d ( log ( Gamma ( X ) ) ) / dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    Original FORTRAN77 version by Jose Bernardo.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Jose Bernardo,
!    Algorithm AS 103:
!    Psi ( Digamma ) Function,
!    Applied Statistics,
!    Volume 25, Number 3, pages 315-317, 1976.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the digamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) DIGAMMA, the value of the digamma function at X.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: C = 8.5D+00
    REAL (KIND=8), PARAMETER :: EULER_MASCHERONI = 0.57721566490153286060D+00
    REAL (KIND=8) DIGAMMA
    REAL (KIND=8) R
    REAL (KIND=8) X
    REAL (KIND=8) X2
!
!  Check the input.
!
    IF (X <= 0.0D+00) THEN
        DIGAMMA = 0.0D+00
        RETURN
    END IF
!
!  Approximation for small argument.
!
    IF (X <= 0.000001D+00) THEN
        DIGAMMA = - EULER_MASCHERONI - 1.0D+00 / X + 1.6449340668482264365D+00 * X
        RETURN
    END IF
!
!  Reduce to DIGAMA(X + N).
!
    DIGAMMA = 0.0D+00
    X2 = X
 
    DO WHILE (X2 < C)
        DIGAMMA = DIGAMMA - 1.0D+00 / X2
        X2 = X2 + 1.0D+00
    END DO
!
!  Use Stirling's (actually de Moivre's) expansion.
!
    R = 1.0D+00 / X2
 
    DIGAMMA = DIGAMMA + LOG (X2) - 0.5D+00 * R
 
    R = R * R
 
    DIGAMMA = DIGAMMA - R*(1.0D+00/12.0D+00  - R*(1.0D+00/120.0D+00 - R*(1.0D+00/252.0D+00 - &
                        R*(1.0D+00/240.0D+00 - R*(1.0D+00/132.0D+00)))))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIPOLE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! DIPOLE_CDF evaluates the Dipole CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!    is interesting, and -1.0D+00 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    CDF = 0.5D+00 + (1.0D+00/R8_PI) * ATAN (X) + B * B * (X*COS(2.0D+00*A)-SIN(2.0D+00*A)) / &
   & (R8_PI*(1.0D+00+X*X))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIPOLE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! DIPOLE_CDF_INV inverts the Dipole CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    -1.0D+00 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DIPOLE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = - HUGE (X)
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
        RETURN
    END IF
!
!  Seek X1 < X < X2.
!
    X1 = - 1.0D+00
 
    DO
 
        CALL DIPOLE_CDF (X1, A, B, CDF1)
 
        IF (CDF1 <= CDF) THEN
            EXIT
        END IF
 
        X1 = 2.0D+00 * X1
 
    END DO
 
    X2 = 1.0D+00
 
    DO
 
        CALL DIPOLE_CDF (X2, A, B, CDF2)
 
        IF (CDF <= CDF2) THEN
            EXIT
        END IF
 
        X2 = 2.0D+00 * X2
 
    END DO
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL DIPOLE_CDF (X3, A, B, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'DIPOLE_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DIPOLE_CHECK (A, B)
 
!*****************************************************************************80
!
!! DIPOLE_CHECK checks the parameters of the Dipole CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!    is interesting, and -1.0 <= B <= 1.0.
!
!    Output, logical DIPOLE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL DIPOLE_CHECK
 
    IF (B <-1.0D+00 .OR. 1.0D+00 < B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DIPOLE_CHECK - Warning!'
        WRITE (*, '(a)') '  -1.0D+00 <= B <= 1.0D+00 is required.'
        WRITE (*, '(a,g14.6)') '  The input B = ', B
        DIPOLE_CHECK = .FALSE.
        RETURN
    END IF
 
    DIPOLE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIPOLE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! DIPOLE_PDF evaluates the Dipole PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!        1 / ( PI * ( 1 + X^2 ) )
!      + B^2 * ( ( 1 - X^2 ) * cos ( 2 * A ) + 2 * X * sin ( 2 * A ) )
!      / ( PI * ( 1 + X^2 )^2 )
!
!    Densities of this kind commonly occur in the analysis of resonant
!    scattering of elementary particles.
!
!    DIPOLE_PDF(A,0;X) = CAUCHY_PDF(A;X)
!
!    A = 0, B = 1 yields the single channel dipole distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Knop,
!    Algorithm 441,
!    Random Deviates from the Dipole Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 16, Number 1, 1973, page 51.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!      is interesting,
!    and -1.0 <= B <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    PDF = 1.0D+00 / (R8_PI*(1.0D+00+X*X)) + B * B * &
   & ((1.0D+00-X*X)*COS(2.0D+00*A)+2.0D+00*X*SIN(2.0D+00*X)) / (R8_PI*(1.0D+00+X*X)**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIPOLE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! DIPOLE_SAMPLE samples the Dipole PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Knop,
!    Algorithm 441,
!    Random Deviates from the Dipole Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 16, Number 1, 1973, page 51.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A is arbitrary, but represents an angle, so only 0 <= A <= 2 * PI
!      is interesting,
!    and -1.0D+00 <= B <= 1.0D+00.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    REAL (KIND=8) C2
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
!
!  Find (X1,X2) at random in a circle.
!
    A2 = B * SIN (A)
    B2 = B * COS (A)
    C2 = 1.0D+00
 
    CALL DISK_SAMPLE (A2, B2, C2, SEED, X1, X2)
!
!  The dipole variate is the ratio X1 / X2.
!
    X = X1 / X2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DIRICHLET_CHECK (N, A)
 
!*****************************************************************************80
!
!! DIRICHLET_CHECK checks the parameters of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, logical DIRICHLET_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    LOGICAL DIRICHLET_CHECK
    INTEGER (KIND=4) I
    LOGICAL POSITIVE
 
    POSITIVE = .FALSE.
 
    DO I = 1, N
 
        IF (A(I) <= 0.0D+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'DIRICHLET_CHECK - Warning!'
            WRITE (*, '(a)') '  A(I) <= 0.'
            WRITE (*, '(a,i8)') '  For I = ', I
            WRITE (*, '(a,g14.6)') '  A(I) = ', A (I)
            DIRICHLET_CHECK = .FALSE.
            RETURN
        ELSE IF (0.0D+00 < A(I)) THEN
            POSITIVE = .TRUE.
        END IF
 
    END DO
 
    IF ( .NOT. POSITIVE) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DIRICHLET_CHECK - Warning!'
        WRITE (*, '(a)') '  All parameters are zero!'
        DIRICHLET_CHECK = .FALSE.
        RETURN
    END IF
 
    DIRICHLET_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MEAN (N, A, MEAN)
 
!*****************************************************************************80
!
!! DIRICHLET_MEAN returns the means of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, real ( kind = 8 ) MEAN(N), the means of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) MEAN (N)
 
    MEAN (1:N) = A (1:N)
 
    CALL R8VEC_UNIT_SUM (N, MEAN)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DIRICHLET_MIX_CHECK (COMP_NUM, ELEM_NUM, A, COMP_WEIGHT)
 
!*****************************************************************************80
!
!! DIRICHLET_MIX_CHECK checks the parameters of a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet
!    mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities
!    for element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Output, logical DIRICHLET_MIX_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) COMP_NUM
    INTEGER (KIND=4) ELEM_NUM
 
    REAL (KIND=8) A (ELEM_NUM, COMP_NUM)
    INTEGER (KIND=4) COMP_I
    REAL (KIND=8) COMP_WEIGHT (COMP_NUM)
    LOGICAL DIRICHLET_MIX_CHECK
    INTEGER (KIND=4) ELEM_I
    LOGICAL POSITIVE
 
    DO COMP_I = 1, COMP_NUM
 
        DO ELEM_I = 1, ELEM_NUM
            IF (A(ELEM_I, COMP_I) <= 0.0D+00) THEN
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'DIRICHLET_MIX_CHECK - Warning!'
                WRITE (*, '(a)') '  A(ELEM,COMP) <= 0.'
                WRITE (*, '(a,i8)') '  COMP = ', COMP_I
                WRITE (*, '(a,i8)') '  ELEM = ', ELEM_I
                WRITE (*, '(a,g14.6)') '  A(COMP,ELEM) = ', A (ELEM_I, COMP_I)
                DIRICHLET_MIX_CHECK = .FALSE.
                RETURN
            END IF
        END DO
 
    END DO
 
    POSITIVE = .FALSE.
 
    DO COMP_I = 1, COMP_NUM
 
        IF (COMP_WEIGHT(COMP_I) < 0.0D+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'DIRICHLET_MIX_CHECK - Warning!'
            WRITE (*, '(a)') '  COMP_WEIGHT(COMP) < 0.'
            WRITE (*, '(a,i8)') '  COMP = ', COMP_I
            WRITE (*, '(a,g14.6)') '  COMP_WEIGHT(COMP) = ', COMP_WEIGHT (COMP_I)
            DIRICHLET_MIX_CHECK = .FALSE.
            RETURN
        ELSE IF (0.0D+00 < COMP_WEIGHT(COMP_I)) THEN
            POSITIVE = .TRUE.
        END IF
 
    END DO
 
    IF ( .NOT. POSITIVE) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DIRICHLET_MIX_CHECK - Warning!'
        WRITE (*, '(a)') '  All component weights are zero.'
        DIRICHLET_MIX_CHECK = .FALSE.
        RETURN
    END IF
 
    DIRICHLET_MIX_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MIX_MEAN (COMP_NUM, ELEM_NUM, A, COMP_WEIGHT, MEAN)
 
!*****************************************************************************80
!
!! DIRICHLET_MIX_MEAN returns the means of a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Output, real ( kind = 8 ) MEAN(ELEM_NUM), the means for each element.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) COMP_NUM
    INTEGER (KIND=4) ELEM_NUM
 
    REAL (KIND=8) A (ELEM_NUM, COMP_NUM)
    INTEGER (KIND=4) COMP_I
    REAL (KIND=8) COMP_MEAN (ELEM_NUM)
    REAL (KIND=8) COMP_WEIGHT (COMP_NUM)
    REAL (KIND=8) COMP_WEIGHT_SUM
    REAL (KIND=8) MEAN (ELEM_NUM)
 
    COMP_WEIGHT_SUM = SUM (COMP_WEIGHT)
 
    MEAN (1:ELEM_NUM) = 0.0D+00
 
    DO COMP_I = 1, COMP_NUM
        CALL DIRICHLET_MEAN (ELEM_NUM, A(1, COMP_I), COMP_MEAN)
        MEAN (1:ELEM_NUM) = MEAN (1:ELEM_NUM) + COMP_WEIGHT (COMP_I) * COMP_MEAN (1:ELEM_NUM)
    END DO
 
    MEAN (1:ELEM_NUM) = MEAN (1:ELEM_NUM) / COMP_WEIGHT_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MIX_PDF (X, COMP_NUM, ELEM_NUM, A, COMP_WEIGHT, PDF)
 
!*****************************************************************************80
!
!! DIRICHLET_MIX_PDF evaluates a Dirichlet mixture PDF.
!
!  Discussion:
!
!    The PDF is a weighted sum of Dirichlet PDF's.  Each PDF is a
!    "component", with an associated weight.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(ELEM_NUM), the argument of the PDF.
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) COMP_NUM
    INTEGER (KIND=4) ELEM_NUM
 
    REAL (KIND=8) A (ELEM_NUM, COMP_NUM)
    INTEGER (KIND=4) COMP_I
    REAL (KIND=8) COMP_PDF
    REAL (KIND=8) COMP_WEIGHT (COMP_NUM)
    REAL (KIND=8) COMP_WEIGHT_SUM
    REAL (KIND=8) PDF
    REAL (KIND=8) X (ELEM_NUM)
 
    COMP_WEIGHT_SUM = SUM (COMP_WEIGHT)
 
    PDF = 0.0D+00
    DO COMP_I = 1, COMP_NUM
 
        CALL DIRICHLET_PDF (X, ELEM_NUM, A(1, COMP_I), COMP_PDF)
 
        PDF = PDF + COMP_WEIGHT (COMP_I) * COMP_PDF / COMP_WEIGHT_SUM
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MIX_SAMPLE (COMP_NUM, ELEM_NUM, A, COMP_WEIGHT, SEED, COMP, X)
 
!*****************************************************************************80
!
!! DIRICHLET_MIX_SAMPLE samples a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of
!    an observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) COMP, the index of the component of the
!    Dirichlet mixture that was chosen to generate the sample.
!
!    Output, real ( kind = 8 ) X(ELEM_NUM), a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) COMP_NUM
    INTEGER (KIND=4) ELEM_NUM
 
    REAL (KIND=8) A (ELEM_NUM, COMP_NUM)
    INTEGER (KIND=4) COMP
    REAL (KIND=8) COMP_WEIGHT (COMP_NUM)
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (ELEM_NUM)
!
!  Choose a particular density component COMP.
!
    CALL DISCRETE_SAMPLE (COMP_NUM, COMP_WEIGHT, SEED, COMP)
!
!  Sample the density number COMP.
!
    CALL DIRICHLET_SAMPLE (ELEM_NUM, A(1, COMP), SEED, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MOMENT2 (N, A, M2)
 
!*****************************************************************************80
!
!! DIRICHLET_MOMENT2 returns the second moments of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, real ( kind = 8 ) M2(N,N), the second moments of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) A_SUM
    REAL (KIND=8) M2 (N, N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    A_SUM = SUM (A(1:N))
 
    DO I = 1, N
        DO J = 1, N
            IF (I == J) THEN
                M2 (I, J) = A (I) * (A(I)+1.0D+00) / (A_SUM*(A_SUM+1.0D+00))
            ELSE
                M2 (I, J) = A (I) * A (J) / (A_SUM*(A_SUM+1.0D+00))
            END IF
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_MULTINOMIAL_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! DIRICHLET_MULTINOMIAL_PDF evaluates a Dirichlet Multinomial PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Comb(A,B,X) * ( Gamma(C_Sum) / Gamma(C_Sum+A) )
!      Product ( 1 <= I <= B ) Gamma(C(I)+X(I)) / Gamma(C(I))
!
!    where:
!
!      Comb(A,B,X) is the multinomial coefficient C( A; X(1), X(2), ..., X(B) ),
!      C_Sum = Sum ( 1 <= I <= B ) C(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kenneth Lange,
!    Mathematical and Statistical Methods for Genetic Analysis,
!    Springer, 1997, page 45.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X(B); X(I) counts the number of occurrences of
!    outcome I, out of the total of A trials.
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of different possible outcomes on
!    one trial.
!
!    Input, real ( kind = 8 ) C(B); C(I) is the Dirichlet parameter associated
!    with outcome I.
!
!    Output, real ( kind = 8 ) PDF, the value of the Dirichlet multinomial PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    REAL (KIND=8) C_SUM
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    REAL (KIND=8) PDF_LOG
    INTEGER (KIND=4) X (B)
 
    C_SUM = SUM (C(1:B))
 
    PDF_LOG = - LOG_GAMMA (C_SUM+REAL(A, KIND=8)) + LOG_GAMMA (C_SUM) + LOG_GAMMA (REAL(A+1, KIND=8))
 
    DO I = 1, B
        PDF_LOG = PDF_LOG + LOG_GAMMA (C(I)+REAL(X(I), KIND=8)) - LOG_GAMMA (C(I)) - LOG_GAMMA &
       & (REAL(X(I)+1, KIND=8))
    END DO
 
    PDF = EXP (PDF_LOG)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_PDF (X, N, A, PDF)
 
!*****************************************************************************80
!
!! DIRICHLET_PDF evaluates the Dirichlet PDF.
!
!  Discussion:
!
!    PDF(N,A;X) = Product ( 1 <= I <= N ) X(I)^( A(I) - 1 )
!      * Gamma ( A_SUM ) / A_PROD
!
!    where
!
!      0 <= A(I) for all I;
!      0 <= X(I) for all I;
!      Sum ( 1 <= I <= N ) X(I) = 1;
!      A_SUM = Sum ( 1 <= I <= N ) A(I).
!      A_PROD = Product ( 1 <= I <= N ) Gamma ( A(I) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(N), the argument of the PDF.  Each X(I) should
!    be greater than 0.0D+00, and the X(I)'s must add up to 1.0.
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be
!    positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) A_PROD
    REAL (KIND=8) A_SUM
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X (N)
    REAL (KIND=8) X_SUM
 
    DO I = 1, N
        IF (X(I) <= 0.0D+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'DIRICHLET_PDF - Fatal error!'
            WRITE (*, '(a)') '  X(I) <= 0.'
            RETURN
        END IF
    END DO
 
    X_SUM = SUM (X(1:N))
 
    IF (TOL < ABS(X_SUM-1.0D+00)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DIRICHLET_PDF - Fatal error!'
        WRITE (*, '(a)') '  SUM X(I) =/= 1.'
        RETURN
    END IF
 
    A_SUM = SUM (A(1:N))
 
    A_PROD = 1.0D+00
    DO I = 1, N
        A_PROD = A_PROD * GAMMA (A(I))
    END DO
 
    PDF = GAMMA (A_SUM) / A_PROD
    DO I = 1, N
        PDF = PDF * X (I) ** (A(I)-1.0D+00)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_SAMPLE (N, A, SEED, X)
 
!*****************************************************************************80
!
!! DIRICHLET_SAMPLE samples the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 169.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be
!    positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the PDF.  The entries
!    of X should sum to 1.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) A2
    REAL (KIND=8) B2
    REAL (KIND=8) C2
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (N)
 
    A2 = 0.0D+00
    B2 = 1.0D+00
 
    DO I = 1, N
        C2 = A (I)
        CALL GAMMA_SAMPLE (A2, B2, C2, SEED, X(I))
    END DO
!
!  Rescale the vector to have unit sum.
!
    CALL R8VEC_UNIT_SUM (N, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DIRICHLET_VARIANCE (N, A, VARIANCE)
 
!*****************************************************************************80
!
!! DIRICHLET_VARIANCE returns the variances of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
!    Output, real ( kind = 8 ) VARIANCE(N), the variances of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) A_SUM
    INTEGER (KIND=4) I
    REAL (KIND=8) VARIANCE (N)
 
    A_SUM = SUM (A(1:N))
 
    DO I = 1, N
        VARIANCE (I) = A (I) * (A_SUM-A(I)) / (A_SUM**2*(A_SUM+1.0D+00))
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! DISCRETE_CDF evaluates the Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the item whose probability is desired.
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of outcomes
!    1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X < 1) THEN
        CDF = 0.0D+00
    ELSE IF (X < A) THEN
        CDF = SUM (B(1:X)) / SUM (B(1:A))
    ELSE IF (A <= X) THEN
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! DISCRETE_CDF_INV inverts the Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of outcomes
!    1 through A.  Each entry must be nonnegative.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument for which
!    CDF(X-1) < CDF <= CDF(X)
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    REAL (KIND=8) CDF
    REAL (KIND=8) CUM
    INTEGER (KIND=4) J
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DISCRETE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    B_SUM = SUM (B(1:A))
 
    CUM = 0.0D+00
 
    DO J = 1, A
 
        CUM = CUM + B (J) / B_SUM
 
        IF (CDF <= CUM) THEN
            X = J
            RETURN
        END IF
 
    END DO
 
    X = A
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DISCRETE_CHECK (A, B)
 
!*****************************************************************************80
!
!! DISCRETE_CHECK checks the parameters of the Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, logical DISCRETE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    LOGICAL DISCRETE_CHECK
    INTEGER (KIND=4) J
 
    DO J = 1, A
        IF (B(J) < 0.0D+00) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'DISCRETE_CHECK - Warning!'
            WRITE (*, '(a)') '  Negative probabilities not allowed.'
            DISCRETE_CHECK = .FALSE.
            RETURN
        END IF
    END DO
 
    B_SUM = SUM (B(1:A))
 
    IF (B_SUM == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DISCRETE_CHECK - Warning!'
        WRITE (*, '(a)') '  Total probablity is zero.'
        DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    DISCRETE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! DISCRETE_MEAN evaluates the mean of the Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
 
    B_SUM = SUM (B(1:A))
 
    MEAN = 0.0D+00
    DO J = 1, A
        MEAN = MEAN + REAL (J, KIND=8) * B (J)
    END DO
 
    MEAN = MEAN / B_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! DISCRETE_PDF evaluates the Discrete PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B(X) if 1 <= X <= A
!                = 0    otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the item whose probability is desired.
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    B_SUM = SUM (B(1:A))
 
    IF (1 <= X .AND. X <= A) THEN
        PDF = B (X) / B_SUM
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! DISCRETE_SAMPLE samples the Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    B_SUM = SUM (B(1:A))
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL DISCRETE_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISCRETE_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! DISCRETE_VARIANCE evaluates the variance of the Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) B_SUM
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    B_SUM = SUM (B(1:A))
 
    MEAN = 0.0D+00
    DO J = 1, A
        MEAN = MEAN + REAL (J, KIND=8) * B (J)
    END DO
 
    MEAN = MEAN / B_SUM
 
    VARIANCE = 0.0D+00
    DO J = 1, A
        VARIANCE = VARIANCE + B (J) * (J-MEAN) ** 2
    END DO
 
    VARIANCE = VARIANCE / B_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISK_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! DISK_MEAN returns the mean of points in a disk.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the disk.
!    The disk is centered at (A,B) and has radius C.
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN(2), the mean value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN (2)
 
    MEAN (1) = A
    MEAN (2) = B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISK_SAMPLE (A, B, C, SEED, X1, X2)
 
!*****************************************************************************80
!
!! DISK_SAMPLE samples points from a disk.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the disk.
!    The disk is centered at (A,B) and has radius C.
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X1, X2, a sampled point of the disk.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ANGLE
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) RADIUS_FRAC
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X1
    REAL (KIND=8) X2
 
    RADIUS_FRAC = R8_UNIFORM_01 (SEED)
    RADIUS_FRAC = SQRT (RADIUS_FRAC)
 
    ANGLE = 2.0D+00 * R8_PI * R8_UNIFORM_01 (SEED)
 
    X1 = A + C * RADIUS_FRAC * COS (ANGLE)
    X2 = B + C * RADIUS_FRAC * SIN (ANGLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DISK_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! DISK_VARIANCE returns the variance of points in a disk.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the disk.
!    The disk is centered at (A,B) and has radius C.
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 0.5D+00 * C * C
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION E_CONSTANT ()
 
!*****************************************************************************80
!
!! E_CONSTANT returns the value of E.
!
!  Discussion:
!
!   "E" was named in honor of Euler.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) E_CONSTANT, the base of the natural
!    logarithm system.
!
    IMPLICIT NONE
 
    REAL (KIND=8) E_CONSTANT
 
    E_CONSTANT = 2.71828182845904523536028747135266249775724709369995D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CDF evaluates the Empirical Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) BSUM
    REAL (KIND=8) C (A)
    REAL (KIND=8) CDF
    INTEGER (KIND=4) I
    REAL (KIND=8) X
 
    CDF = 0.0D+00
 
    BSUM = SUM (B(1:A))
 
    DO I = 1, A
 
        IF (X < C(I)) THEN
            RETURN
        END IF
 
        CDF = CDF + B (I) / BSUM
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CDF_INV inverts the Empirical Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) X, the smallest argument whose CDF is greater
!    than or equal to CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) BSUM
    REAL (KIND=8) C (A)
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    INTEGER (KIND=4) I
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    BSUM = SUM (B(1:A))
 
    X = C (1)
    CDF2 = B (1) / BSUM
 
    DO I = 2, A
 
        IF (CDF <= CDF2) THEN
            RETURN
        END IF
 
        X = C (I)
        CDF2 = CDF2 + B (I) / BSUM
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EMPIRICAL_DISCRETE_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_CHECK checks the parameters of the Empirical Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, logical EMPIRICAL_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) C (A)
    LOGICAL EMPIRICAL_DISCRETE_CHECK
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    IF (A <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CHECK - Warning!'
        WRITE (*, '(a)') '  A must be positive.'
        WRITE (*, '(a,i12)') '  Input A = ', A
        WRITE (*, '(a)') '  A is the number of weights.'
        EMPIRICAL_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (ANY(B(1:A) < 0.0D+00)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CHECK - Warning!'
        WRITE (*, '(a)') '  Some B(*) < 0.'
        WRITE (*, '(a)') '  But all B values must be nonnegative.'
        EMPIRICAL_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (ALL(B(1:A) == 0.0D+00)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CHECK - Warning!'
        WRITE (*, '(a)') '  All B(*) = 0.'
        WRITE (*, '(a)') '  But at least one B values must be nonzero.'
        EMPIRICAL_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    DO I = 1, A
        DO J = I + 1, A
            IF (C(I) == C(J)) THEN
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CHECK - Warning!'
                WRITE (*, '(a)') '  All values C must be unique.'
                WRITE (*, '(a)') '  But at least two values are identical.'
                EMPIRICAL_DISCRETE_CHECK = .FALSE.
                RETURN
            END IF
        END DO
    END DO
 
    DO I = 1, A - 1
        IF (C(I+1) < C(I)) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'EMPIRICAL_DISCRETE_CHECK - Warning!'
            WRITE (*, '(a)') '  The values in C must be in ascending order.'
            EMPIRICAL_DISCRETE_CHECK = .FALSE.
            RETURN
        END IF
    END DO
 
    EMPIRICAL_DISCRETE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_MEAN returns the mean of the Empirical Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) C (A)
    REAL (KIND=8) MEAN
 
    MEAN = DOT_PRODUCT (B(1:A), C(1:A)) / SUM (B(1:A))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_PDF evaluates the Empirical Discrete PDF.
!
!  Discussion:
!
!    A set of A values C(1:A) are assigned nonnegative weights B(1:A),
!    with at least one B nonzero.  The probability of C(I) is the
!    value of B(I) divided by the sum of the weights.
!
!    The C's must be distinct, and given in ascending order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) C (A)
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    DO I = 1, A
        IF (X == C(I)) THEN
            PDF = B (I) / SUM (B(1:A))
            RETURN
        END IF
    END DO
 
    PDF = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_SAMPLE samples the Empirical Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) C (A)
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL EMPIRICAL_DISCRETE_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EMPIRICAL_DISCRETE_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_VARIANCE: variance of the Empirical Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
 
    REAL (KIND=8) B (A)
    REAL (KIND=8) BSUM
    REAL (KIND=8) C (A)
    INTEGER (KIND=4) I
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    BSUM = SUM (B(1:A))
 
    CALL EMPIRICAL_DISCRETE_MEAN (A, B, C, MEAN)
 
    VARIANCE = 0.0D+00
 
    DO I = 1, A
        VARIANCE = VARIANCE + (B(I)/BSUM) * (C(I)-MEAN) ** 2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_LETTER_CDF_INV (CDF, C)
 
!*****************************************************************************80
!
!! ENGLISH_LETTER_CDF_INV inverts the English Letter CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Lewand,
!    Cryptological Mathematics,
!    Mathematics Association of America, 2000,
!    ISBN13: 978-0883857199
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, a cumulative probability between 0 and 1.
!
!    Input, character C, the corresponding letter.
!
    IMPLICIT NONE
 
    CHARACTER C
    REAL (KIND=8) CDF
    REAL (KIND=8), SAVE :: CDF_VEC (27) = (/ 0.00000, 0.08167, 0.09659, 0.12441, 0.16694, &
   & 0.29396, 0.31624, 0.33639, 0.39733, 0.46699, 0.46852, 0.47624, 0.51649, 0.54055, 0.60804, &
   & 0.68311, 0.70240, 0.70335, 0.76322, 0.82649, 0.91705, 0.94463, 0.95441, 0.97802, 0.97952, &
   & 0.99926, 1.00000 /)
    INTEGER (KIND=4) I
 
    C = ' '
 
    DO I = 2, 27
        IF (CDF <= CDF_VEC(I)) THEN
            C = ACHAR (IACHAR('a')+I-2)
            EXIT
        END IF
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_LETTER_CDF (C, CDF)
 
!*****************************************************************************80
!
!! ENGLISH_LETTER_CDF evaluates the English Letter CDF.
!
!  Discussion:
!
!    CDF('c') = 0.12441
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Lewand,
!    Cryptological Mathematics,
!    Mathematics Association of America, 2000,
!    ISBN13: 978-0883857199
!
!  Parameters:
!
!    Input, character C, the letter whose probability is desired.
!    'a' <= c <= 'z', but case is ignored.
!
!    Output, real ( kind = 8 ) CDF, the probability that a random letter is less
!    than or equal to C.
!
    IMPLICIT NONE
 
    CHARACTER C
    REAL (KIND=8) CDF
    REAL (KIND=8), SAVE :: CDF_VEC (27) = (/ 0.00000, 0.08167, 0.09659, 0.12441, 0.16694, &
   & 0.29396, 0.31624, 0.33639, 0.39733, 0.46699, 0.46852, 0.47624, 0.51649, 0.54055, 0.60804, &
   & 0.68311, 0.70240, 0.70335, 0.76322, 0.82649, 0.91705, 0.94463, 0.95441, 0.97802, 0.97952, &
   & 0.99926, 1.00000 /)
    INTEGER (KIND=4) I
 
    IF ('a' <= C .AND. C <= 'z') THEN
        I = IACHAR (C) - IACHAR ('a') + 2
        CDF = CDF_VEC (I)
    ELSE IF ('A' <= C .AND. C <= 'Z') THEN
        I = IACHAR (C) - IACHAR ('A') + 2
        CDF = CDF_VEC (I)
    ELSE
        CDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_LETTER_PDF (C, PDF)
 
!*****************************************************************************80
!
!! ENGLISH_LETTER_PDF evaluates the English Letter PDF.
!
!  Discussion:
!
!    PDF('c') = 0.02782
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Lewand,
!    Cryptological Mathematics,
!    Mathematics Association of America, 2000,
!    ISBN13: 978-0883857199
!
!  Parameters:
!
!    Input, character C, the letter whose probability is desired.
!    'a' <= c <= 'z', but case is ignored.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    CHARACTER C
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    REAL (KIND=8), SAVE :: PDF_VEC (26) = (/ 0.08167, 0.01492, 0.02782, 0.04253, 0.12702, &
   & 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, &
   & 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02361, 0.00150, 0.01974, &
   & 0.00074 /)
 
    IF ('a' <= C .AND. C <= 'z') THEN
        I = IACHAR (C) - IACHAR ('a') + 1
        PDF = PDF_VEC (I)
    ELSE IF ('A' <= C .AND. C <= 'Z') THEN
        I = IACHAR (C) - IACHAR ('A') + 1
        PDF = PDF_VEC (I)
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_LETTER_SAMPLE (SEED, C)
 
!*****************************************************************************80
!
!! ENGLISH_LETTER_SAMPLE samples the English Letter PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SEED, a seed for the random number generator.
!
!    Output, character C, a sample of the PDF.
!
!    Output, integer ( kind = 4 ) SEED, an updated seed for the random
!    number generator.
!
    IMPLICIT NONE
 
    CHARACTER C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL ENGLISH_LETTER_CDF_INV (CDF, C)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_CDF (X, CDF)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_CDF evaluates the English Sentence Length CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the sentence length whose CDF is desired.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: SENTENCE_LENGTH_MAX = 79
 
    REAL (KIND=8) CDF
    REAL (KIND=8), DIMENSION (SENTENCE_LENGTH_MAX) :: PDF_VEC = (/ 0.00806D+00, 0.01370D+00, &
   & 0.01862D+00, 0.02547D+00, 0.03043D+00, 0.03189D+00, 0.03516D+00, 0.03545D+00, 0.03286D+00, &
   & 0.03533D+00, 0.03562D+00, 0.03788D+00, 0.03669D+00, 0.03751D+00, 0.03518D+00, 0.03541D+00, &
   & 0.03434D+00, 0.03305D+00, 0.03329D+00, 0.03103D+00, 0.02867D+00, 0.02724D+00, 0.02647D+00, &
   & 0.02526D+00, 0.02086D+00, 0.02178D+00, 0.02128D+00, 0.01801D+00, 0.01690D+00, 0.01556D+00, &
   & 0.01512D+00, 0.01326D+00, 0.01277D+00, 0.01062D+00, 0.01051D+00, 0.00901D+00, 0.00838D+00, &
   & 0.00764D+00, 0.00683D+00, 0.00589D+00, 0.00624D+00, 0.00488D+00, 0.00477D+00, 0.00406D+00, &
   & 0.00390D+00, 0.00350D+00, 0.00318D+00, 0.00241D+00, 0.00224D+00, 0.00220D+00, 0.00262D+00, &
   & 0.00207D+00, 0.00174D+00, 0.00174D+00, 0.00128D+00, 0.00121D+00, 0.00103D+00, 0.00117D+00, &
   & 0.00124D+00, 0.00082D+00, 0.00088D+00, 0.00061D+00, 0.00061D+00, 0.00075D+00, 0.00063D+00, &
   & 0.00056D+00, 0.00052D+00, 0.00057D+00, 0.00031D+00, 0.00029D+00, 0.00021D+00, 0.00017D+00, &
   & 0.00021D+00, 0.00034D+00, 0.00031D+00, 0.00011D+00, 0.00011D+00, 0.00008D+00, 0.00006D+00 &
   & /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99768D+00
    INTEGER (KIND=4) X
 
    IF (X < 1) THEN
        CDF = 0.0D+00
    ELSE IF (X < SENTENCE_LENGTH_MAX) THEN
        CDF = SUM (PDF_VEC(1:X)) / PDF_SUM
    ELSE IF (SENTENCE_LENGTH_MAX <= X) THEN
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_CDF_INV inverts the English Sentence Length CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding sentence length for which
!    CDF(X-1) < CDF <= CDF(X)
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: SENTENCE_LENGTH_MAX = 79
 
    REAL (KIND=8) CDF
    REAL (KIND=8) CUM
    INTEGER (KIND=4) J
    REAL (KIND=8), DIMENSION (SENTENCE_LENGTH_MAX) :: PDF_VEC = (/ 0.00806D+00, 0.01370D+00, &
   & 0.01862D+00, 0.02547D+00, 0.03043D+00, 0.03189D+00, 0.03516D+00, 0.03545D+00, 0.03286D+00, &
   & 0.03533D+00, 0.03562D+00, 0.03788D+00, 0.03669D+00, 0.03751D+00, 0.03518D+00, 0.03541D+00, &
   & 0.03434D+00, 0.03305D+00, 0.03329D+00, 0.03103D+00, 0.02867D+00, 0.02724D+00, 0.02647D+00, &
   & 0.02526D+00, 0.02086D+00, 0.02178D+00, 0.02128D+00, 0.01801D+00, 0.01690D+00, 0.01556D+00, &
   & 0.01512D+00, 0.01326D+00, 0.01277D+00, 0.01062D+00, 0.01051D+00, 0.00901D+00, 0.00838D+00, &
   & 0.00764D+00, 0.00683D+00, 0.00589D+00, 0.00624D+00, 0.00488D+00, 0.00477D+00, 0.00406D+00, &
   & 0.00390D+00, 0.00350D+00, 0.00318D+00, 0.00241D+00, 0.00224D+00, 0.00220D+00, 0.00262D+00, &
   & 0.00207D+00, 0.00174D+00, 0.00174D+00, 0.00128D+00, 0.00121D+00, 0.00103D+00, 0.00117D+00, &
   & 0.00124D+00, 0.00082D+00, 0.00088D+00, 0.00061D+00, 0.00061D+00, 0.00075D+00, 0.00063D+00, &
   & 0.00056D+00, 0.00052D+00, 0.00057D+00, 0.00031D+00, 0.00029D+00, 0.00021D+00, 0.00017D+00, &
   & 0.00021D+00, 0.00034D+00, 0.00031D+00, 0.00011D+00, 0.00011D+00, 0.00008D+00, 0.00006D+00 &
   & /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99768D+00
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ENGLISH_SENTENCE_LENGTH_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CUM = 0.0D+00
 
    DO J = 1, SENTENCE_LENGTH_MAX
 
        CUM = CUM + PDF_VEC (J)
 
        IF (CDF <= CUM/PDF_SUM) THEN
            X = J
            RETURN
        END IF
 
    END DO
 
    X = SENTENCE_LENGTH_MAX
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_MEAN (MEAN)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_MEAN: mean of the English Sentence Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: SENTENCE_LENGTH_MAX = 79
 
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8), DIMENSION (SENTENCE_LENGTH_MAX) :: PDF_VEC = (/ 0.00806D+00, 0.01370D+00, &
   & 0.01862D+00, 0.02547D+00, 0.03043D+00, 0.03189D+00, 0.03516D+00, 0.03545D+00, 0.03286D+00, &
   & 0.03533D+00, 0.03562D+00, 0.03788D+00, 0.03669D+00, 0.03751D+00, 0.03518D+00, 0.03541D+00, &
   & 0.03434D+00, 0.03305D+00, 0.03329D+00, 0.03103D+00, 0.02867D+00, 0.02724D+00, 0.02647D+00, &
   & 0.02526D+00, 0.02086D+00, 0.02178D+00, 0.02128D+00, 0.01801D+00, 0.01690D+00, 0.01556D+00, &
   & 0.01512D+00, 0.01326D+00, 0.01277D+00, 0.01062D+00, 0.01051D+00, 0.00901D+00, 0.00838D+00, &
   & 0.00764D+00, 0.00683D+00, 0.00589D+00, 0.00624D+00, 0.00488D+00, 0.00477D+00, 0.00406D+00, &
   & 0.00390D+00, 0.00350D+00, 0.00318D+00, 0.00241D+00, 0.00224D+00, 0.00220D+00, 0.00262D+00, &
   & 0.00207D+00, 0.00174D+00, 0.00174D+00, 0.00128D+00, 0.00121D+00, 0.00103D+00, 0.00117D+00, &
   & 0.00124D+00, 0.00082D+00, 0.00088D+00, 0.00061D+00, 0.00061D+00, 0.00075D+00, 0.00063D+00, &
   & 0.00056D+00, 0.00052D+00, 0.00057D+00, 0.00031D+00, 0.00029D+00, 0.00021D+00, 0.00017D+00, &
   & 0.00021D+00, 0.00034D+00, 0.00031D+00, 0.00011D+00, 0.00011D+00, 0.00008D+00, 0.00006D+00 &
   & /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99768D+00
 
    MEAN = 0.0D+00
    DO J = 1, SENTENCE_LENGTH_MAX
        MEAN = MEAN + REAL (J, KIND=8) * PDF_VEC (J)
    END DO
 
    MEAN = MEAN / PDF_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_PDF (X, PDF)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_PDF evaluates the English Sentence Length PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B(X) if 1 <= X <= A
!                = 0    otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the sentence length whose probability
!    is desired.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: SENTENCE_LENGTH_MAX = 79
 
    REAL (KIND=8) PDF
    REAL (KIND=8), DIMENSION (SENTENCE_LENGTH_MAX) :: PDF_VEC = (/ 0.00806D+00, 0.01370D+00, &
   & 0.01862D+00, 0.02547D+00, 0.03043D+00, 0.03189D+00, 0.03516D+00, 0.03545D+00, 0.03286D+00, &
   & 0.03533D+00, 0.03562D+00, 0.03788D+00, 0.03669D+00, 0.03751D+00, 0.03518D+00, 0.03541D+00, &
   & 0.03434D+00, 0.03305D+00, 0.03329D+00, 0.03103D+00, 0.02867D+00, 0.02724D+00, 0.02647D+00, &
   & 0.02526D+00, 0.02086D+00, 0.02178D+00, 0.02128D+00, 0.01801D+00, 0.01690D+00, 0.01556D+00, &
   & 0.01512D+00, 0.01326D+00, 0.01277D+00, 0.01062D+00, 0.01051D+00, 0.00901D+00, 0.00838D+00, &
   & 0.00764D+00, 0.00683D+00, 0.00589D+00, 0.00624D+00, 0.00488D+00, 0.00477D+00, 0.00406D+00, &
   & 0.00390D+00, 0.00350D+00, 0.00318D+00, 0.00241D+00, 0.00224D+00, 0.00220D+00, 0.00262D+00, &
   & 0.00207D+00, 0.00174D+00, 0.00174D+00, 0.00128D+00, 0.00121D+00, 0.00103D+00, 0.00117D+00, &
   & 0.00124D+00, 0.00082D+00, 0.00088D+00, 0.00061D+00, 0.00061D+00, 0.00075D+00, 0.00063D+00, &
   & 0.00056D+00, 0.00052D+00, 0.00057D+00, 0.00031D+00, 0.00029D+00, 0.00021D+00, 0.00017D+00, &
   & 0.00021D+00, 0.00034D+00, 0.00031D+00, 0.00011D+00, 0.00011D+00, 0.00008D+00, 0.00006D+00 &
   & /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99768D+00
    INTEGER (KIND=4) X
 
    IF (1 <= X .AND. X <= SENTENCE_LENGTH_MAX) THEN
        PDF = PDF_VEC (X) / PDF_SUM
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_SAMPLE samples the English Sentence Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL ENGLISH_SENTENCE_LENGTH_CDF_INV (CDF, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_SENTENCE_LENGTH_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_VARIANCE: variance of English Sentence Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: SENTENCE_LENGTH_MAX = 79
 
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8), DIMENSION (SENTENCE_LENGTH_MAX) :: PDF_VEC = (/ 0.00806D+00, 0.01370D+00, &
   & 0.01862D+00, 0.02547D+00, 0.03043D+00, 0.03189D+00, 0.03516D+00, 0.03545D+00, 0.03286D+00, &
   & 0.03533D+00, 0.03562D+00, 0.03788D+00, 0.03669D+00, 0.03751D+00, 0.03518D+00, 0.03541D+00, &
   & 0.03434D+00, 0.03305D+00, 0.03329D+00, 0.03103D+00, 0.02867D+00, 0.02724D+00, 0.02647D+00, &
   & 0.02526D+00, 0.02086D+00, 0.02178D+00, 0.02128D+00, 0.01801D+00, 0.01690D+00, 0.01556D+00, &
   & 0.01512D+00, 0.01326D+00, 0.01277D+00, 0.01062D+00, 0.01051D+00, 0.00901D+00, 0.00838D+00, &
   & 0.00764D+00, 0.00683D+00, 0.00589D+00, 0.00624D+00, 0.00488D+00, 0.00477D+00, 0.00406D+00, &
   & 0.00390D+00, 0.00350D+00, 0.00318D+00, 0.00241D+00, 0.00224D+00, 0.00220D+00, 0.00262D+00, &
   & 0.00207D+00, 0.00174D+00, 0.00174D+00, 0.00128D+00, 0.00121D+00, 0.00103D+00, 0.00117D+00, &
   & 0.00124D+00, 0.00082D+00, 0.00088D+00, 0.00061D+00, 0.00061D+00, 0.00075D+00, 0.00063D+00, &
   & 0.00056D+00, 0.00052D+00, 0.00057D+00, 0.00031D+00, 0.00029D+00, 0.00021D+00, 0.00017D+00, &
   & 0.00021D+00, 0.00034D+00, 0.00031D+00, 0.00011D+00, 0.00011D+00, 0.00008D+00, 0.00006D+00 &
   & /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99768D+00
    REAL (KIND=8) VARIANCE
 
    MEAN = 0.0D+00
    DO J = 1, SENTENCE_LENGTH_MAX
        MEAN = MEAN + REAL (J, KIND=8) * PDF_VEC (J)
    END DO
 
    MEAN = MEAN / PDF_SUM
 
    VARIANCE = 0.0D+00
    DO J = 1, SENTENCE_LENGTH_MAX
        VARIANCE = VARIANCE + PDF_VEC (J) * (J-MEAN) ** 2
    END DO
 
    VARIANCE = VARIANCE / PDF_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_CDF (X, CDF)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_CDF evaluates the English Word Length CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the word length whose CDF is desired.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: WORD_LENGTH_MAX = 27
 
    REAL (KIND=8) CDF
    REAL (KIND=8), DIMENSION (WORD_LENGTH_MAX) :: PDF_VEC = (/ 0.03160D+00, 0.16975D+00, &
   & 0.21192D+00, 0.15678D+00, 0.10852D+00, 0.08524D+00, 0.07724D+00, 0.05623D+00, 0.04032D+00, &
   & 0.02766D+00, 0.01582D+00, 0.00917D+00, 0.00483D+00, 0.00262D+00, 0.00099D+00, 0.00050D+00, &
   & 0.00027D+00, 0.00022D+00, 0.00011D+00, 0.00006D+00, 0.00005D+00, 0.00002D+00, 0.00001D+00, &
   & 0.00001D+00, 0.00001D+00, 0.00001D+00, 0.00001D+00 /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99997D+00
    INTEGER (KIND=4) X
 
    IF (X < 1) THEN
        CDF = 0.0D+00
    ELSE IF (X < WORD_LENGTH_MAX) THEN
        CDF = SUM (PDF_VEC(1:X)) / PDF_SUM
    ELSE IF (WORD_LENGTH_MAX <= X) THEN
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_CDF_INV inverts the English Word Length CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding word length for which
!    CDF(X-1) < CDF <= CDF(X)
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: WORD_LENGTH_MAX = 27
 
    REAL (KIND=8) CDF
    REAL (KIND=8) CUM
    INTEGER (KIND=4) J
    REAL (KIND=8), DIMENSION (WORD_LENGTH_MAX) :: PDF_VEC = (/ 0.03160D+00, 0.16975D+00, &
   & 0.21192D+00, 0.15678D+00, 0.10852D+00, 0.08524D+00, 0.07724D+00, 0.05623D+00, 0.04032D+00, &
   & 0.02766D+00, 0.01582D+00, 0.00917D+00, 0.00483D+00, 0.00262D+00, 0.00099D+00, 0.00050D+00, &
   & 0.00027D+00, 0.00022D+00, 0.00011D+00, 0.00006D+00, 0.00005D+00, 0.00002D+00, 0.00001D+00, &
   & 0.00001D+00, 0.00001D+00, 0.00001D+00, 0.00001D+00 /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99997D+00
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ENGLISH_WORD_LENGTH_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CUM = 0.0D+00
 
    DO J = 1, WORD_LENGTH_MAX
 
        CUM = CUM + PDF_VEC (J)
 
        IF (CDF <= CUM/PDF_SUM) THEN
            X = J
            RETURN
        END IF
 
    END DO
 
    X = WORD_LENGTH_MAX
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_MEAN (MEAN)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_MEAN evaluates the mean of the English Word Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: WORD_LENGTH_MAX = 27
 
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8), DIMENSION (WORD_LENGTH_MAX) :: PDF_VEC = (/ 0.03160D+00, 0.16975D+00, &
   & 0.21192D+00, 0.15678D+00, 0.10852D+00, 0.08524D+00, 0.07724D+00, 0.05623D+00, 0.04032D+00, &
   & 0.02766D+00, 0.01582D+00, 0.00917D+00, 0.00483D+00, 0.00262D+00, 0.00099D+00, 0.00050D+00, &
   & 0.00027D+00, 0.00022D+00, 0.00011D+00, 0.00006D+00, 0.00005D+00, 0.00002D+00, 0.00001D+00, &
   & 0.00001D+00, 0.00001D+00, 0.00001D+00, 0.00001D+00 /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99997D+00
 
    MEAN = 0.0D+00
    DO J = 1, WORD_LENGTH_MAX
        MEAN = MEAN + REAL (J, KIND=8) * PDF_VEC (J)
    END DO
 
    MEAN = MEAN / PDF_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_PDF (X, PDF)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_PDF evaluates the English Word Length PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B(X) if 1 <= X <= A
!                = 0    otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the word length whose probability
!    is desired.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: WORD_LENGTH_MAX = 27
 
    REAL (KIND=8) PDF
    REAL (KIND=8), DIMENSION (WORD_LENGTH_MAX) :: PDF_VEC = (/ 0.03160D+00, 0.16975D+00, &
   & 0.21192D+00, 0.15678D+00, 0.10852D+00, 0.08524D+00, 0.07724D+00, 0.05623D+00, 0.04032D+00, &
   & 0.02766D+00, 0.01582D+00, 0.00917D+00, 0.00483D+00, 0.00262D+00, 0.00099D+00, 0.00050D+00, &
   & 0.00027D+00, 0.00022D+00, 0.00011D+00, 0.00006D+00, 0.00005D+00, 0.00002D+00, 0.00001D+00, &
   & 0.00001D+00, 0.00001D+00, 0.00001D+00, 0.00001D+00 /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99997D+00
    INTEGER (KIND=4) X
 
    IF (1 <= X .AND. X <= WORD_LENGTH_MAX) THEN
        PDF = PDF_VEC (X) / PDF_SUM
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_SAMPLE samples the English Word Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL ENGLISH_WORD_LENGTH_CDF_INV (CDF, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ENGLISH_WORD_LENGTH_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_VARIANCE: variance of the English Word Length PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: WORD_LENGTH_MAX = 27
 
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8), DIMENSION (WORD_LENGTH_MAX) :: PDF_VEC = (/ 0.03160D+00, 0.16975D+00, &
   & 0.21192D+00, 0.15678D+00, 0.10852D+00, 0.08524D+00, 0.07724D+00, 0.05623D+00, 0.04032D+00, &
   & 0.02766D+00, 0.01582D+00, 0.00917D+00, 0.00483D+00, 0.00262D+00, 0.00099D+00, 0.00050D+00, &
   & 0.00027D+00, 0.00022D+00, 0.00011D+00, 0.00006D+00, 0.00005D+00, 0.00002D+00, 0.00001D+00, &
   & 0.00001D+00, 0.00001D+00, 0.00001D+00, 0.00001D+00 /)
    REAL (KIND=8), PARAMETER :: PDF_SUM = 0.99997D+00
    REAL (KIND=8) VARIANCE
 
    MEAN = 0.0D+00
    DO J = 1, WORD_LENGTH_MAX
        MEAN = MEAN + REAL (J, KIND=8) * PDF_VEC (J)
    END DO
 
    MEAN = MEAN / PDF_SUM
 
    VARIANCE = 0.0D+00
    DO J = 1, WORD_LENGTH_MAX
        VARIANCE = VARIANCE + PDF_VEC (J) * (J-MEAN) ** 2
    END DO
 
    VARIANCE = VARIANCE / PDF_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! ERLANG_CDF evaluates the Erlang CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) CDF
    REAL (KIND=8) P2
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    IF (X < A) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        X2 = (X-A) / B
        P2 = REAL (C, KIND=8)
 
        CDF = R8_GAMMA_INC (P2, X2)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! ERLANG_CDF_INV inverts the Erlang CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ERLANG_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = A
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
        RETURN
    END IF
 
    X1 = A
    CDF1 = 0.0D+00
 
    X2 = A + 1.0D+00
 
    DO
 
        CALL ERLANG_CDF (X2, A, B, C, CDF2)
 
        IF (CDF < CDF2) THEN
            EXIT
        END IF
 
        X2 = A + 2.0D+00 * (X2-A)
 
    END DO
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL ERLANG_CDF (X3, A, B, C, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'ERLANG_CDF_INV - Warning!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ERLANG_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! ERLANG_CHECK checks the parameters of the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, logical ERLANG_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    LOGICAL ERLANG_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ERLANG_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.0'
        ERLANG_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ERLANG_CHECK - Warning!'
        WRITE (*, '(a)') '  C <= 0.'
        ERLANG_CHECK = .FALSE.
        RETURN
    END IF
 
    ERLANG_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! ERLANG_MEAN returns the mean of the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) MEAN
 
    MEAN = A + B * REAL (C, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! ERLANG_PDF evaluates the Erlang PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( ( X - A ) / B )^( C - 1 )
!      / ( B * Gamma ( C ) * EXP ( ( X - A ) / B ) )
!
!    for 0 < B, 0 < C integer, A <= X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = Y ** (C-1) / (B*LOG_GAMMA(REAL(C, KIND=8))*EXP(Y))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! ERLANG_SAMPLE samples the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    INTEGER (KIND=4) C
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    A2 = 0.0D+00
    B2 = B
    X = A
    DO I = 1, C
        CALL EXPONENTIAL_SAMPLE (A2, B2, SEED, X2)
        X = X + X2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERLANG_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! ERLANG_VARIANCE returns the variance of the Erlang PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * REAL (C)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EULER_CONSTANT ()
 
!*****************************************************************************80
!
!! EULER_CONSTANT returns the value of the Euler-Mascheroni constant.
!
!  Discussion:
!
!    The Euler-Mascheroni constant is often denoted by a lower-case
!    Gamma.  Gamma is defined as
!
!      Gamma = limit ( M -> Infinity )
!        ( Sum ( 1 <= N <= M ) 1 / N ) - Log ( M )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) EULER_CONSTANT, the value of the
!    Euler-Mascheroni constant.
!
    IMPLICIT NONE
 
    REAL (KIND=8) EULER_CONSTANT
 
    EULER_CONSTANT = 0.577215664901532860606512090082402431042D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_CDF (X, CDF)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_CDF evaluates the Exponential 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - EXP (-X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_CDF_INV inverts the Exponential 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EXPONENTIAL_01_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = - LOG (1.0D+00-CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_MEAN (MEAN)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_MEAN returns the mean of the Exponential 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 1.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_PDF (X, PDF)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_PDF evaluates the Exponential 01 PDF.
!
!  Discussion:
!
!    PDF(X) = EXP ( - X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = EXP (-X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_SAMPLE samples the Exponential PDF with parameter 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    X = - LOG (1.0D+00-CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_01_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! EXPONENTIAL_01_VARIANCE returns the variance of the Exponential 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 1.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! EXPONENTIAL_CDF evaluates the Exponential CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - EXP ((A-X)/B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! EXPONENTIAL_CDF_INV inverts the Exponential CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EXPONENTIAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A - B * LOG (1.0D+00-CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_CDF_VALUES (N_DATA, LAMBDA, X, FX)
 
!*****************************************************************************80
!
!! EXPONENTIAL_CDF_VALUES returns some values of the Exponential CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = ExponentialDistribution [ lambda ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) LAMBDA, the parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 9
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.3934693402873666D+00, &
   & 0.6321205588285577D+00, 0.7768698398515702D+00, 0.8646647167633873D+00, &
   & 0.8646647167633873D+00, 0.9816843611112658D+00, 0.9975212478233336D+00, &
   & 0.9996645373720975D+00, 0.9999546000702375D+00 /)
    REAL (KIND=8) LAMBDA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        LAMBDA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        LAMBDA = LAMBDA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EXPONENTIAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! EXPONENTIAL_CHECK checks the parameters of the Exponential CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical EXPONENTIAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL EXPONENTIAL_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EXPONENTIAL_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.0'
        EXPONENTIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    EXPONENTIAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! EXPONENTIAL_MEAN returns the mean of the Exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A + B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! EXPONENTIAL_PDF evaluates the Exponential PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = ( 1 / B ) * EXP ( ( A - X ) / B )
!
!    The time interval between two Poisson events is a random
!    variable with the Exponential PDF.  The parameter B is the
!    average interval between events.
!
!    In another context, the Exponential PDF is related to
!    the Boltzmann distribution, which describes the relative
!    probability of finding a system, which is in thermal equilibrium
!    at absolute temperature T, in a given state having energy E.
!    The relative probability is
!
!      Boltzmann_Relative_Probability(E,T) = exp ( - E / ( k * T ) ),
!
!    where k is the Boltzmann constant,
!
!      k = 1.38 * 10^(-23) joules / degree Kelvin
!
!    and normalization requires a determination of the possible
!    energy states of the system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < A) THEN
        PDF = 0.0D+00
    ELSE
        PDF = (1.0D+00/B) * EXP ((A-X)/B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! EXPONENTIAL_SAMPLE samples the Exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL EXPONENTIAL_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXPONENTIAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! EXPONENTIAL_VARIANCE returns the variance of the Exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_CDF evaluates the Extreme Values CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    CDF = EXP (-EXP(-Y))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_CDF_INV inverts the Extreme Values CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EXTREME_VALUES_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A - B * LOG (-LOG(CDF))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_CDF_VALUES (N_DATA, ALPHA, BETA, X, FX)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_CDF_VALUES returns some values of the Extreme Values CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = ExtremeValuesDistribution [ alpha, beta ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) ALPHA, the first parameter of the distribution.
!
!    Output, real ( kind = 8 ) BETA, the second parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: ALPHA_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    REAL (KIND=8) BETA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: BETA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.3678794411714423D+00, &
   & 0.8734230184931166D+00, 0.9818510730616665D+00, 0.9975243173927525D+00, &
   & 0.5452392118926051D+00, 0.4884435800065159D+00, 0.4589560693076638D+00, &
   & 0.4409910259429826D+00, 0.5452392118926051D+00, 0.3678794411714423D+00, &
   & 0.1922956455479649D+00, 0.6598803584531254D-01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        ALPHA = 0.0D+00
        BETA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        ALPHA = ALPHA_VEC (N_DATA)
        BETA = BETA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EXTREME_VALUES_CHECK (A, B)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_CHECK checks the parameters of the Extreme Values CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical EXTREME_VALUES_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL EXTREME_VALUES_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'EXTREME_VALUES_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.'
        EXTREME_VALUES_CHECK = .FALSE.
        RETURN
    END IF
 
    EXTREME_VALUES_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_MEAN returns the mean of the Extreme Values PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A + B * EULER_CONSTANT ()
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_PDF evaluates the Extreme Values PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!      ( 1 / B ) * exp ( ( A - X ) / B ) * exp ( - exp ( ( A - X ) / B  ) ).
!
!    The Extreme Values PDF is also known as the Fisher-Tippet PDF
!    and the Log-Weibull PDF.
!
!    The special case A = 0 and B = 1 is the Gumbel PDF.
!
!    The Extreme Values PDF is the limiting distribution for the
!    smallest or largest value in a large sample drawn from
!    any of a great variety of distributions.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    PDF = (1.0D+00/B) * EXP ((A-X)/B-EXP((A-X)/B))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_SAMPLE samples the Extreme Values PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL EXTREME_VALUES_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EXTREME_VALUES_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! EXTREME_VALUES_VARIANCE returns the variance of the Extreme Values PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = R8_PI * R8_PI * B * B / 6.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_CDF (X, M, N, CDF)
 
!*****************************************************************************80
!
!! F_CDF evaluates the F central CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ARG1
    REAL (KIND=8) ARG2
    REAL (KIND=8) ARG3
    REAL (KIND=8) CDF
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        ARG1 = 0.5D+00 * REAL (N, KIND=8)
        ARG2 = 0.5D+00 * REAL (M, KIND=8)
        ARG3 = REAL (N, KIND=8) / (REAL(N, KIND=8)+REAL(M, KIND=8)*X)
 
        CDF = 1.0D+00 - BETA_INC (ARG1, ARG2, ARG3)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_CDF_VALUES (N_DATA, A, B, X, FX)
 
!*****************************************************************************80
!
!! F_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = FRatioDistribution [ dfn, dfd ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, integer B, the parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 20
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 1, 1, 5, 1, 2, 4, 1, 6, 8, 1, 3, 6, &
   & 1, 1, 1, 1, 2, 3, 4, 5 /)
    INTEGER (KIND=4) B
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 1, 5, 1, 5, 10, 20, 5, 6, 16, 5, &
   & 10, 12, 5, 5, 5, 5, 5, 5, 5, 5 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.4999714850534485D+00, 0.4996034370170990D+00, 0.7496993658293228D+00, &
   & 0.7504656462757382D+00, 0.7514156325324275D+00, 0.8999867031372156D+00, &
   & 0.8997127554259699D+00, 0.9002845660853669D+00, 0.9500248817817622D+00, &
   & 0.9500574946122442D+00, 0.9501926400000000D+00, 0.9750133887312993D+00, &
   & 0.9900022327445249D+00, 0.9949977837872073D+00, 0.9989999621122122D+00, &
   & 0.5687988496283079D+00, 0.5351452100063650D+00, 0.5143428032407864D+00, &
   & 0.5000000000000000D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.00D+00, 0.528D+00, 1.89D+00, &
   & 1.69D+00, 1.60D+00, 1.47D+00, 4.06D+00, 3.05D+00, 2.09D+00, 6.61D+00, 3.71D+00, 3.00D+00, &
   & 10.01D+00, 16.26D+00, 22.78D+00, 47.18D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0
        B = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION F_CHECK (M, N)
 
!*****************************************************************************80
!
!! F_CHECK checks the parameters of the F PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, logical F_CHECK, is TRUE if the parameters are legal.
!
    IMPLICIT NONE
 
    LOGICAL F_CHECK
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    IF (M < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_CHECK - Warning!'
        WRITE (*, '(a)') '  M < 1.'
        F_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (N < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_CHECK - Warning!'
        WRITE (*, '(a)') '  N < 1.'
        F_CHECK = .FALSE.
        RETURN
    END IF
 
    F_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_MEAN (M, N, MEAN)
 
!*****************************************************************************80
!
!! F_MEAN returns the mean of the F central PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the mean is not defined unless 3 <= N.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) N
 
    IF (N < 3) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_MEAN - Fatal error!'
        WRITE (*, '(a)') '  The mean is not defined for N < 3.'
        RETURN
    END IF
 
    MEAN = REAL (N, KIND=8) / REAL (N-2, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_PDF (X, M, N, PDF)
 
!*****************************************************************************80
!
!! F_PDF evaluates the F central PDF.
!
!  Discussion:
!
!    PDF(M,N;X) = M^(M/2) * X^((M-2)/2)
!      / ( Beta(M/2,N/2) * N^(M/2) * ( 1 + (M/N) * X )^((M+N)/2)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) BOT1
    REAL (KIND=8) BOT2
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
    REAL (KIND=8) TOP
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        A = REAL (M, KIND=8)
        B = REAL (N, KIND=8)
 
        TOP = SQRT (A**M*B**N*X**(M-2))
        BOT1 = R8_BETA (A/2.0D+00, B/2.0D+00)
        BOT2 = SQRT ((B+A*X)**(M+N))
 
        PDF = TOP / (BOT1*BOT2)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_SAMPLE (M, N, SEED, X)
 
!*****************************************************************************80
!
!! F_SAMPLE samples the F central PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) XM
    REAL (KIND=8) XN
 
    A = REAL (M, KIND=8)
    CALL CHI_SQUARE_SAMPLE (A, SEED, XM)
 
    A = REAL (N, KIND=8)
    CALL CHI_SQUARE_SAMPLE (A, SEED, XN)
 
    X = REAL (N, KIND=8) * XM / (REAL(M, KIND=8)*XN)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_VARIANCE (M, N, VARIANCE)
 
!*****************************************************************************80
!
!! F_VARIANCE returns the variance of the F central PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the variance is not defined unless 5 <= N.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) VARIANCE
 
    IF (N < 5) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  The variance is not defined for N < 5.'
        RETURN
    END IF
 
    VARIANCE = REAL (2*N*N*(M+N-2), KIND=8) / REAL (M*(N-2)**2*(N-4), KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_NONCENTRAL_CDF_VALUES (N_DATA, N1, N2, LAMBDA, X, FX)
 
!*****************************************************************************80
!
!! F_NONCENTRAL_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NoncentralFRatioDistribution [ n1, n2, lambda ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) N1, integer N2, the numerator and denominator
!    degrees of freedom.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 22
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.6367825323508774D+00, 0.5840916116305482D+00, 0.3234431872392788D+00, &
   & 0.4501187879813550D+00, 0.6078881441188312D+00, 0.7059275551414605D+00, &
   & 0.7721782003263727D+00, 0.8191049017635072D+00, 0.3170348430749965D+00, &
   & 0.4327218008454471D+00, 0.4502696915707327D+00, 0.4261881186594096D+00, &
   & 0.6753687206341544D+00, 0.4229108778879005D+00, 0.6927667261228938D+00, &
   & 0.3632174676491226D+00, 0.4210054012695865D+00, 0.4266672258818927D+00, &
   & 0.4464016600524644D+00, 0.8445888579504827D+00, 0.4339300273343604D+00 /)
    REAL (KIND=8) LAMBDA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.00D+00, 0.00D+00, 0.25D+00, &
   & 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, 1.00D+00, 1.00D+00, &
   & 1.00D+00, 2.00D+00, 1.00D+00, 1.00D+00, 0.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
   & 1.00D+00 /)
    INTEGER (KIND=4) N_DATA
    INTEGER (KIND=4) N1
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: N1_VEC = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, &
   & 2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 16 /)
    INTEGER (KIND=4) N2
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: N2_VEC = (/ 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &
   & 10, 5, 5, 5, 5, 1, 5, 6, 12, 16, 8 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.00D+00, 1.00D+00, 1.00D+00, &
   & 0.50D+00, 1.00D+00, 2.00D+00, 3.00D+00, 4.00D+00, 5.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
   & 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, &
   & 2.00D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N1 = 0
        N2 = 0
        LAMBDA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N1 = N1_VEC (N_DATA)
        N2 = N2_VEC (N_DATA)
        LAMBDA = LAMBDA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION F_NONCENTRAL_CHECK (A, M, N)
 
!*****************************************************************************80
!
!! F_NONCENTRAL_CHECK checks the parameters of the F noncentral PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, logical F_NONCENTRAL_CHECK, is TRUE if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL F_NONCENTRAL_CHECK
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_NONCENTRAL_CHECK - Warning!'
        WRITE (*, '(a)') '  A <= 0.'
        F_NONCENTRAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (M < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_NONCENTRAL_CHECK - Warning!'
        WRITE (*, '(a)') '  M < 1.'
        F_NONCENTRAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (N < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_NONCENTRAL_CHECK - Warning!'
        WRITE (*, '(a)') '  N < 1.'
        F_NONCENTRAL_CHECK = .FALSE.
        RETURN
    END IF
 
    F_NONCENTRAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_NONCENTRAL_MEAN (A, M, N, MEAN)
 
!*****************************************************************************80
!
!! F_NONCENTRAL_MEAN returns the mean of the F noncentral PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!
!    Input, integer ( kind = 4 ) M, N, parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the mean is not defined unless 3 <= N.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    INTEGER (KIND=4) M
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) N
 
    IF (N < 3) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_NONCENTRAL_MEAN - Fatal error!'
        WRITE (*, '(a)') '  The mean is not defined for N < 3.'
        RETURN
    END IF
 
    MEAN = (REAL(M, KIND=8)+A) * REAL (N, KIND=8) / (REAL(M, KIND=8)*REAL(N-2, KIND=8))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE F_NONCENTRAL_VARIANCE (A, M, N, VARIANCE)
 
!*****************************************************************************80
!
!! F_NONCENTRAL_VARIANCE returns the variance of the F noncentral PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!
!    Input, integer ( kind = 4 ) M, N, parameters of the PDF.
!    1 <= M,
!    1 <= N.
!    Note, however, that the variance is not defined unless 5 <= N.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    INTEGER (KIND=4) M
    REAL (KIND=8) MR
    INTEGER (KIND=4) N
    REAL (KIND=8) NR
    REAL (KIND=8) VARIANCE
 
    IF (N < 5) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'F_NONCENTRAL_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  The variance is not defined for N < 5.'
        RETURN
    END IF
 
    MR = REAL (M, KIND=8)
    NR = REAL (N, KIND=8)
 
    VARIANCE = ((MR+A)**2+2.0D+00*(MR+A)*NR**2) / ((NR-2.0D+00)*(NR-4.0D+00)*MR**2) - (MR+A) ** &
   & 2 * NR ** 2 / ((NR-2.0D+00)**2*MR**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FERMI_DIRAC_SAMPLE (U, V, SEED, Z)
 
!*****************************************************************************80
!
!! FERMI_DIRAC_SAMPLE samples a (continuous) Fermi-Dirac distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2008
!
!  Author:
!
!    Original BASIC version by Frederick Ruckdeschel.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Frederick Ruckdeschel,
!    BASIC Scientific Subroutines,
!    Volume I,
!    McGraw Hill, 1980,
!    ISBN: 0-07-054201-5,
!    LC: QA76.95.R82.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) U, V, the parameters of the distribution.
!    The value of U represents the halfway point for the distribution.
!    Half the probability is to the left, and half to the right, of
!    the value U.  The value of V controls the shape of the distribution.
!    The ratio U/V determines the relative shape of the distribution.
!    Values of U/V in excess of 100 will risk overflow.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) Z, a sample from the Fermi-Dirac distribution.
!    Output values will be nonnegative, and roughly half of them should
!    be less than or equal to U.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4), PARAMETER :: ITER_MAX = 1000
    INTEGER (KIND=4) ITER_NUM
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Y1
    REAL (KIND=8) Z
 
    X = R8_UNIFORM_01 (SEED)
    Y = 1.0D+00
    A = EXP (4.0D+00*U/V)
    B = (X-1.0D+00) * LOG (1.0D+00+A)
 
    ITER_NUM = 0
 
    DO
 
        Y1 = B + LOG (A+EXP(Y))
 
        IF (ABS(Y-Y1) < 0.001D+00) THEN
            EXIT
        END IF
 
        Y = Y1
 
        ITER_NUM = ITER_NUM + 1
 
        IF (ITER_MAX < ITER_NUM) THEN
            EXIT
        END IF
 
    END DO
 
    Z = V * Y1 / 4.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISHER_PDF (X, KAPPA, MU, PDF)
 
!*****************************************************************************80
!
!! FISHER_PDF evaluates the Fisher PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!      PDF(KAPPA,MU;X) = C(KAPPA) * exp ( KAPPA * MU' * X )
!
!    where:
!
!      0 <= KAPPA is the concentration parameter,
!      MU is a point on the unit sphere, the mean direction,
!      X is any point on the unit sphere,
!      and C(KAPPA) is a normalization factor:
!
!      C(KAPPA) = sqrt ( KAPPA ) / ( ( 2 * pi )^(3/2) * I(0.5,KAPPA) )
!
!    where
!
!      I(nu,X) is the Bessel function of order NU and argument X.
!
!    For a fixed value of MU, the value of KAPPA determines the
!    tendency of sample points to tend to be near MU.  In particular,
!    KAPPA = 0 corresponds to a uniform distribution of points on the
!    sphere, but as KAPPA increases, the sample points will tend to
!    cluster more closely to MU.
!
!    The Fisher distribution for points on the unit sphere is
!    analogous to the normal distribution of points on a line,
!    and, more precisely, to the von Mises distribution on a circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    LC: QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X(3), the argument of the PDF.
!    X should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of X.
!
!    Input, real ( kind = 8 ) KAPPA, the concentration parameter.
!    0 <= KAPPA is required.
!
!    Input, real ( kind = 8 ) MU(3), the mean direction.
!    MU should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of MU.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: NB = 1
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ARG
    REAL (KIND=8) B (NB)
    REAL (KIND=8) CF
    INTEGER (KIND=4) IZE
    REAL (KIND=8) KAPPA
    REAL (KIND=8) MU (3)
    REAL (KIND=8) MU_NORM
    INTEGER (KIND=4) NCALC
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X (3)
    REAL (KIND=8) X_NORM
 
    IF (KAPPA < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISHER_PDF - Fatal error!'
        WRITE (*, '(a)') '  KAPPA must be nonnegative.'
        WRITE (*, '(a,g14.6)') '  Input KAPPA = ', KAPPA
        RETURN
    END IF
 
    IF (KAPPA == 0.0D+00) THEN
        PDF = 1.0D+00 / (4.0D+00*R8_PI)
        RETURN
    END IF
!
!  Compute the normalization factor CF.
!
    ALPHA = 0.5D+00
    IZE = 1
 
    CALL RIBESL (KAPPA, ALPHA, NB, IZE, B, NCALC)
 
    CF = SQRT (KAPPA) / (SQRT((2.0D+00*R8_PI)**3)*B(1))
!
!  Normalize MU.
!
    MU_NORM = SQRT (SUM(MU(1:3)**2))
 
    IF (MU_NORM == 0.0D+00) THEN
        PDF = CF
        RETURN
    END IF
!
!  Normalize X.
!
    X_NORM = SQRT (SUM(X(1:3)**2))
 
    IF (X_NORM == 0.0D+00) THEN
        PDF = CF
        RETURN
    END IF
!
!  Evaluate the PDF.
!
    ARG = KAPPA * DOT_PRODUCT (X(1:3), MU(1:3)) / (X_NORM*MU_NORM)
 
    PDF = CF * EXP (ARG)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISHER_SAMPLE (KAPPA, MU, N, SEED, XYZ)
 
!*****************************************************************************80
!
!! FISHER_SAMPLE samples the Fisher distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Nicholas Fisher, Toby Lewis, Brian Embleton,
!    Statistical Analysis of Spherical Data,
!    Cambridge, 2003,
!    ISBN13: 978-0521456999,
!    LC: QA276.F489.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) KAPPA, the concentration parameter.
!
!    Input, real ( kind = 8 ) MU(3), the mean direction.
!    MU should have unit Euclidean norm, but this routine will
!    automatically work with a normalized version of MU.
!
!    Input, integer ( kind = 4 ) N, the number of samples to choose.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) XYZ(3,N), a sample of the Fisher distribution.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) ALPHA, BETA, the colatitude (theta) and
!    longitude (phi) of the mean direction.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (3, 3)
    REAL (KIND=8) ALPHA
    REAL (KIND=8) BETA
    REAL (KIND=8) KAPPA
    REAL (KIND=8) LAMBDA
    REAL (KIND=8) MU (3)
    REAL (KIND=8) MU_NORM
    REAL (KIND=8) PHI (1:N)
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) THETA (1:N)
    REAL (KIND=8) XYZ (3, N)
 
    MU_NORM = SQRT (SUM(MU(1:3)**2))
 
    IF (MU_NORM == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISHER_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  MU(1:3) = 0.'
        RETURN
    END IF
 
    ALPHA = - ACOS (MU(3)/MU_NORM)
    BETA = ATAN2 (MU(2), MU(1))
 
    LAMBDA = EXP (-2.0D+00*KAPPA)
 
    CALL R8VEC_UNIFORM_01 (N, SEED, THETA)
 
    IF (KAPPA == 0.0D+00) THEN
 
        THETA (1:N) = 2.0D+00 * ASIN (SQRT(1.0D+00-THETA(1:N)))
 
    ELSE
 
        THETA (1:N) = 2.0D+00 * ASIN &
       & (SQRT(-LOG(THETA(1:N)*(1.0D+00-LAMBDA)+LAMBDA)/(2.0D+00*KAPPA)))
 
    END IF
 
    CALL R8VEC_UNIFORM_01 (N, SEED, PHI)
 
    PHI (1:N) = 2.0D+00 * R8_PI * PHI (1:N)
!
!  Compute the unrotated points.
!
    XYZ (1, 1:N) = SIN (THETA(1:N)) * COS (PHI(1:N))
    XYZ (2, 1:N) = SIN (THETA(1:N)) * SIN (PHI(1:N))
    XYZ (3, 1:N) = COS (THETA(1:N))
!
!  Compute the rotation matrix.
!
    A (1, 1) = COS (ALPHA) * COS (BETA)
    A (2, 1) = - SIN (BETA)
    A (3, 1) = SIN (ALPHA) * COS (BETA)
 
    A (1, 2) = COS (ALPHA) * SIN (BETA)
    A (2, 2) = + COS (BETA)
    A (3, 2) = SIN (ALPHA) * SIN (BETA)
 
    A (1, 3) = - SIN (ALPHA)
    A (2, 3) = 0.0D+00
    A (3, 3) = COS (ALPHA)
!
!  Rotate the points.
!
    XYZ (1:3, 1:N) = MATMUL (A(1:3, 1:3), XYZ(1:3, 1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! FISK_CDF evaluates the Fisk CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 / (1.0D+00+(B/(X-A))**C)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! FISK_CDF_INV inverts the Fisk CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISK_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF <= 0.0D+00) THEN
        X = A
    ELSE IF (CDF < 1.0D+00) THEN
        X = A + B * (CDF/(1.0D+00-CDF)) ** (1.0D+00/C)
    ELSE IF (1.0D+00 <= CDF) THEN
        X = HUGE (X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION FISK_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! FISK_CHECK checks the parameters of the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical FISK_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL FISK_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISK_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.'
        FISK_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISK_CHECK - Warning!'
        WRITE (*, '(a)') '  C <= 0.'
        FISK_CHECK = .FALSE.
        RETURN
    END IF
 
    FISK_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! FISK_MEAN returns the mean of the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 April 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    IF (C <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISK_MEAN - Fatal error!'
        WRITE (*, '(a)') '  No mean defined for C <= 1.0'
        RETURN
    END IF
 
    MEAN = A + R8_PI * (B/C) * R8_CSC (R8_PI/C)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! FISK_PDF evaluates the Fisk PDF.
!
!  Discussion:
!
!    The Fisk PDF is also known as the Log Logistic PDF.
!
!    The formula for the PDF is:
!
!    PDF(A,B,C;X) =
!      ( C / B ) * ( ( X - A ) / B )^( C - 1 ) /
!      ( 1 + ( ( X - A ) / B )^C )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = (C/B) * Y ** (C-1.0D+00) / (1.0D+00+Y**C) ** 2
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! FISK_SAMPLE samples the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL FISK_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FISK_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! FISK_VARIANCE returns the variance of the Fisk PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) G
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    IF (C <= 2.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FISK_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  No variance defined for C <= 2.0'
        RETURN
    END IF
 
    G = R8_PI / C
 
    VARIANCE = B * B * (2.0D+00*G*R8_CSC(2.0D+00*G)-(G*R8_CSC(G))**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_CDF evaluates the Folded Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
 
    IF (X < 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE
        X1 = (X-A) / B
        CALL NORMAL_01_CDF (X1, CDF1)
        X2 = (-X-A) / B
        CALL NORMAL_01_CDF (X2, CDF2)
        CDF = CDF1 - CDF2
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_CDF_INV inverts the Folded Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 <= X.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
    REAL (KIND=8) XA
    REAL (KIND=8) XB
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FOLDED_NORMAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
        RETURN
    END IF
!
!  Find X1, for which the value of CDF will be too small.
!
    IF (0.0D+00 <= A) THEN
        CALL NORMAL_CDF_INV (CDF, A, B, X1)
    ELSE
        CALL NORMAL_CDF_INV (CDF,-A, B, X1)
    END IF
    X1 = MAX (X1, 0.0D+00)
    CALL FOLDED_NORMAL_CDF (X1, A, B, CDF1)
!
!  Find X2, for which the value of CDF will be too big.
!
    CDF2 = (1.0D+00-CDF) / 2.0D+00
 
    CALL NORMAL_CDF_INV (CDF2, A, B, XA)
    CALL NORMAL_CDF_INV (CDF2,-A, B, XB)
    X2 = MAX (ABS(XA), ABS(XB))
    CALL FOLDED_NORMAL_CDF (X2, A, B, CDF2)
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL FOLDED_NORMAL_CDF (X3, A, B, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'FOLDED_NORMAL_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION FOLDED_NORMAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_CHECK checks the parameters of the Folded Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, logical FOLDED_NORMAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL FOLDED_NORMAL_CHECK
 
    IF (A < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FOLDED_NORMAL_CHECK - Warning!'
        WRITE (*, '(a)') '  A < 0.'
        FOLDED_NORMAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FOLDED_NORMAL_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.'
        FOLDED_NORMAL_CHECK = .FALSE.
        RETURN
    END IF
 
    FOLDED_NORMAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_MEAN returns the mean of the Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) MEAN
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    A2 = A / B
 
    CALL NORMAL_01_CDF (A2, CDF)
 
    MEAN = B * SQRT (2.0D+00/R8_PI) * EXP (-0.5D+00*A2*A2) - A * (1.0D+00-2.0D+00*CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_PDF evaluates the Folded Normal PDF.
!
!  Discussion:
!
!    The formula for the PDF is:
!
!    PDF(A;X) = sqrt ( 2 / PI ) * ( 1 / B ) * cosh ( A * X / B^2 )
!      * exp ( - 0.5D+00 * ( X^2 + A^2 ) / B^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = SQRT (2.0D+00/R8_PI) * (1.0D+00/B) * COSH (A*X/B**2) * EXP &
       & (-0.5D+00*(X*X+A*A)/B**2)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_SAMPLE samples the Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4), INTENT(INOUT) :: SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL FOLDED_NORMAL_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FOLDED_NORMAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! FOLDED_NORMAL_VARIANCE returns the variance of the Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    CALL FOLDED_NORMAL_MEAN (A, B, MEAN)
 
    VARIANCE = A * A + B * B - MEAN * MEAN
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_CDF (X, ALPHA, CDF)
 
!*****************************************************************************80
!
!! FRECHET_CDF evaluates the Frechet CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_CDF - Fatal error!'
        WRITE (*, '(a)') '  ALPHA <= 0.0.'
        RETURN
    END IF
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE
        CDF = EXP (-1.0D+00/X**ALPHA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_CDF_INV (CDF, ALPHA, X)
 
!*****************************************************************************80
!
!! FRECHET_CDF_INV inverts the Frechet CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  ALPHA <= 0.0.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
    ELSE
        X = (-1.0D+00/LOG(CDF)) ** (1.0D+00/ALPHA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_MEAN (ALPHA, MEAN)
 
!*****************************************************************************80
!
!! FRECHET_MEAN returns the mean of the Frechet PDF.
!
!  Discussion:
!
!    The distribution does not have a mean value unless 1 < ALPHA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 1.0 < ALPHA.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) MEAN
 
    IF (ALPHA <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_MEAN - Fatal error!'
        WRITE (*, '(a)') '  Mean does not exist if ALPHA <= 1.'
        RETURN
    END IF
 
    MEAN = GAMMA ((ALPHA-1.0D+00)/ALPHA)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_PDF (X, ALPHA, PDF)
 
!*****************************************************************************80
!
!! FRECHET_PDF evaluates the Frechet PDF.
!
!  Discussion:
!
!    PDF(X) = ALPHA * exp ( -1 / X^ALPHA ) / X^(ALPHA+1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_PDF - Fatal error!'
        WRITE (*, '(a)') '  ALPHA <= 0.0.'
        RETURN
    END IF
 
    PDF = ALPHA * EXP (-1.0D+00/X**ALPHA) / X ** (ALPHA+1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_SAMPLE (ALPHA, SEED, X)
 
!*****************************************************************************80
!
!! FRECHET_SAMPLE samples the Frechet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    IF (ALPHA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  ALPHA <= 0.0.'
        RETURN
    END IF
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL FRECHET_CDF_INV (CDF, ALPHA, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FRECHET_VARIANCE (ALPHA, VARIANCE)
 
!*****************************************************************************80
!
!! FRECHET_VARIANCE returns the variance of the Frechet PDF.
!
!  Discussion:
!
!    The PDF does not have a variance unless 2 < ALPHA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 2.0 < ALPHA.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    IF (ALPHA <= 2.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'FRECHET_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  Variance does not exist if ALPHA <= 2.'
        RETURN
    END IF
 
    MEAN = GAMMA ((ALPHA-1.0D+00)/ALPHA)
 
    VARIANCE = GAMMA ((ALPHA-2.0D+00)/ALPHA) - MEAN * MEAN
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! GAMMA_CDF evaluates the Gamma CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) P2
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    X2 = (X-A) / B
    P2 = C
 
    CDF = R8_GAMMA_INC (P2, X2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_CDF_VALUES (N_DATA, MU, SIGMA, X, FX)
 
!*****************************************************************************80
!
!! GAMMA_CDF_VALUES returns some values of the Gamma CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = GammaDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the variance of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.8646647167633873D+00, &
   & 0.9816843611112658D+00, 0.9975212478233336D+00, 0.9996645373720975D+00, &
   & 0.6321205588285577D+00, 0.4865828809674080D+00, 0.3934693402873666D+00, &
   & 0.3296799539643607D+00, 0.4421745996289254D+00, 0.1911531694619419D+00, &
   & 0.6564245437845009D-01, 0.1857593622214067D-01 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) SIGMA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: SIGMA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        SIGMA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        SIGMA = SIGMA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GAMMA_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! GAMMA_CHECK checks the parameters of the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical GAMMA_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL GAMMA_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GAMMA_CHECK - Warning!'
        WRITE (*, '(a)') '  B <= 0.'
        WRITE (*, '(a,g14.6)') '  B = ', B
        GAMMA_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GAMMA_CHECK - Warning!'
        WRITE (*, '(a)') '  C <= 0.'
        WRITE (*, '(a,g14.6)') '  C = ', C
        GAMMA_CHECK = .FALSE.
        RETURN
    END IF
 
    GAMMA_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_INC_VALUES (N_DATA, A, X, FX)
 
!*****************************************************************************80
!
!! GAMMA_INC_VALUES returns some values of the incomplete Gamma function.
!
!  Discussion:
!
!    The (normalized) incomplete Gamma function P(A,X) is defined as:
!
!      PN(A,X) = 1/Gamma(A) * Integral ( 0 <= T <= X ) T^(A-1) * exp(-T) dT.
!
!    With this definition, for all A and X,
!
!      0 <= PN(A,X) <= 1
!
!    and
!
!      PN(A,INFINITY) = 1.0
!
!    In Mathematica, the function can be evaluated by:
!
!      1 - GammaRegularized[A,X]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, the parameter of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 20
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.10D+00, 0.10D+00, 0.10D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.10D+01, 0.10D+01, 0.10D+01, 0.11D+01, 0.11D+01, 0.11D+01, &
   & 0.20D+01, 0.20D+01, 0.20D+01, 0.60D+01, 0.60D+01, 0.11D+02, 0.26D+02, 0.41D+02 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.7382350532339351D+00, &
   & 0.9083579897300343D+00, 0.9886559833621947D+00, 0.3014646416966613D+00, &
   & 0.7793286380801532D+00, 0.9918490284064973D+00, 0.9516258196404043D-01, &
   & 0.6321205588285577D+00, 0.9932620530009145D+00, 0.7205974576054322D-01, &
   & 0.5891809618706485D+00, 0.9915368159845525D+00, 0.1018582711118352D-01, &
   & 0.4421745996289254D+00, 0.9927049442755639D+00, 0.4202103819530612D-01, &
   & 0.9796589705830716D+00, 0.9226039842296429D+00, 0.4470785799755852D+00, &
   & 0.7444549220718699D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.30D-01, 0.30D+00, 0.15D+01, &
   & 0.75D-01, 0.75D+00, 0.35D+01, 0.10D+00, 0.10D+01, 0.50D+01, 0.10D+00, 0.10D+01, 0.50D+01, &
   & 0.15D+00, 0.15D+01, 0.70D+01, 0.25D+01, 0.12D+02, 0.16D+02, 0.25D+02, 0.45D+02 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! GAMMA_MEAN returns the mean of the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A + B * C
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! GAMMA_PDF evaluates the Gamma PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = exp ( - ( X - A ) / B ) * ( ( X - A ) / B )^(C-1)
!      / ( B * GAMMA ( C ) )
!
!    GAMMA_PDF(A,B,C;X), where C is an integer, is the Erlang PDF.
!    GAMMA_PDF(A,B,1;X) is the Exponential PDF.
!    GAMMA_PDF(0,2,C/2;X) is the Chi Squared PDF with C degrees of freedom.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A controls the location of the peak;  A is often chosen to be 0.0.
!    B is the "scale" parameter; 0.0 < B, and is often 1.0.
!    C is the "shape" parameter; 0.0 < C, and is often 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = Y ** (C-1.0D+00) / (B*GAMMA(C)*EXP(Y))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! GAMMA_SAMPLE samples the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2000
!
!  Author:
!
!    Original FORTRAN77 version by Joachim Ahrens, Ulrich Dieter.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47 - 54.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions.
!    Computing,
!    Volume 12, 1974, pages 223 - 246.
!
!    Joachim Ahrens, KD Kohrt, Ulrich Dieter,
!    Algorithm 599,
!    ACM Transactions on Mathematical Software,
!    Volume 9, Number 2, June 1983, pages 255-257.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
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
    REAL (KIND=8) BCOEF
    REAL (KIND=8) C
    REAL (KIND=8) CO
    REAL (KIND=8) D
    REAL (KIND=8) E
    REAL (KIND=8), PARAMETER :: E1 = 1.0D+00
    REAL (KIND=8), PARAMETER :: E2 = 0.4999897D+00
    REAL (KIND=8), PARAMETER :: E3 = 0.1668290D+00
    REAL (KIND=8), PARAMETER :: E4 = 0.0407753D+00
    REAL (KIND=8), PARAMETER :: E5 = 0.0102930D+00
    REAL (KIND=8), PARAMETER :: EULER = 2.71828182845904D+00
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
    REAL (KIND=8) S
    INTEGER (KIND=4) SEED
    REAL (KIND=8) SI
    REAL (KIND=8) S2
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) X
!
!  Allow C = 0.
!
    IF (C == 0.0D+00) THEN
        X = A
        RETURN
    END IF
!
!  C < 1.
!
    IF (C < 1.0D+00) THEN
 
        DO
 
            U = R8_UNIFORM_01 (SEED)
            T = 1.0D+00 + C / EULER
            P = U * T
 
            CALL EXPONENTIAL_01_SAMPLE (SEED, S)
 
            IF (P < 1.0D+00) THEN
                X = EXP (LOG(P)/C)
                IF (X <= S) THEN
                    EXIT
                END IF
            ELSE
                X = - LOG ((T-P)/C)
                IF ((1.0D+00-C)*LOG(X) <= S) THEN
                    EXIT
                END IF
            END IF
 
        END DO
 
        X = A + B * X
        RETURN
!
!  1 <= C.
!
    ELSE
 
        S2 = C - 0.5D+00
        S = SQRT (C-0.5D+00)
        D = SQRT (32.0D+00) - 12.0D+00 * SQRT (C-0.5D+00)
 
        CALL NORMAL_01_SAMPLE (SEED, T)
        X = (SQRT(C-0.5D+00)+0.5D+00*T) ** 2
 
        IF (0.0D+00 <= T) THEN
            X = A + B * X
            RETURN
        END IF
 
        U = R8_UNIFORM_01 (SEED)
 
        IF (D*U <= T**3) THEN
            X = A + B * X
            RETURN
        END IF
 
        R = 1.0D+00 / C
 
        Q0 = ((((((Q7*R+Q6)*R+Q5)*R+Q4)*R+Q3)*R+Q2)*R+Q1) * R
 
        IF (C <= 3.686D+00) THEN
            BCOEF = 0.463D+00 + S - 0.178D+00 * S2
            SI = 1.235D+00
            CO = 0.195D+00 / S - 0.079D+00 + 0.016D+00 * S
        ELSE IF (C <= 13.022D+00) THEN
            BCOEF = 1.654D+00 + 0.0076D+00 * S2
            SI = 1.68D+00 / S + 0.275D+00
            CO = 0.062D+00 / S + 0.024D+00
        ELSE
            BCOEF = 1.77D+00
            SI = 0.75D+00
            CO = 0.1515D+00 / S
        END IF
 
        IF (0.0D+00 < SQRT(C-0.5D+00)+0.5D+00*T) THEN
 
            V = 0.5D+00 * T / S
 
            IF (0.25D+00 < ABS(V)) THEN
                Q = Q0 - S * T + 0.25D+00 * T * T + 2.0D+00 * S2 * LOG (1.0D+00+V)
            ELSE
                Q = Q0 + 0.5D+00 * T ** 2 * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * V
            END IF
 
            IF (LOG(1.0D+00-U) <= Q) THEN
                X = A + B * X
                RETURN
            END IF
 
        END IF
 
        DO
 
            CALL EXPONENTIAL_01_SAMPLE (SEED, E)
 
            U = R8_UNIFORM_01 (SEED)
 
            U = 2.0D+00 * U - 1.0D+00
            T = BCOEF + SIGN (SI*E, U)
 
            IF (-0.7187449D+00 <= T) THEN
 
                V = 0.5D+00 * T / S
 
                IF (0.25D+00 < ABS(V)) THEN
                    Q = Q0 - S * T + 0.25D+00 * T ** 2 + 2.0D+00 * S2 * LOG (1.0D+00+V)
                ELSE
                    Q = Q0 + 0.5D+00 * T ** 2 * ((((((A7*V+A6)*V+A5)*V+A4)*V+A3)*V+A2)*V+A1) * &
                   & V
                END IF
 
                IF (0.0D+00 < Q) THEN
 
                    IF (0.5D+00 < Q) THEN
                        W = EXP (Q) - 1.0D+00
                    ELSE
                        W = ((((E5*Q+E4)*Q+E3)*Q+E2)*Q+E1) * Q
                    END IF
 
                    IF (CO*ABS(U) <= W*EXP(E-0.5D+00*T**2)) THEN
                        X = A + B * (S+0.5D+00*T) ** 2
                        RETURN
                    END IF
 
                END IF
 
            END IF
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! GAMMA_VARIANCE returns the variance of the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * C
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! GENLOGISTIC_CDF evaluates the Generalized Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    CDF = 1.0D+00 / (1.0D+00+EXP(-Y)) ** C
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! GENLOGISTIC_CDF_INV inverts the Generalized Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENLOGISTIC_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = - HUGE (X)
    ELSE IF (CDF < 1.0D+00) THEN
        X = A - B * LOG (CDF**(-1.0D+00/C)-1.0D+00)
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GENLOGISTIC_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! GENLOGISTIC_CHECK checks the parameters of the Generalized Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical GENLOGISTIC_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL GENLOGISTIC_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENLOGISTIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        GENLOGISTIC_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GENLOGISTIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C <= 0.'
        GENLOGISTIC_CHECK = .FALSE.
        RETURN
    END IF
 
    GENLOGISTIC_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! GENLOGISTIC_MEAN returns the mean of the Generalized Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A + B * (EULER_CONSTANT()+DIGAMMA(C))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! GENLOGISTIC_PDF evaluates the Generalized Logistic PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( C / B ) * exp ( ( A - X ) / B ) /
!      ( ( 1 + exp ( ( A - X ) / B ) )^(C+1) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    PDF = (C/B) * EXP (-Y) / (1.0D+00+EXP(-Y)) ** (C+1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! GENLOGISTIC_SAMPLE samples the Generalized Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL GENLOGISTIC_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GENLOGISTIC_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! GENLOGISTIC_VARIANCE returns the variance of the Generalized Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * (R8_PI*R8_PI/6.0D+00+TRIGAMMA(C))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! GEOMETRIC_CDF evaluates the Geometric CDF.
!
!  Discussion:
!
!    CDF(X,P) is the probability that there will be at least one
!    successful trial in the first X Bernoulli trials, given that
!    the probability of success in a single trial is P.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of trials.
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X <= 0) THEN
        CDF = 0.0D+00
    ELSE IF (A == 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (A == 1.0D+00) THEN
        CDF = 1.0D+00
    ELSE
        CDF = 1.0D+00 - (1.0D+00-A) ** X
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! GEOMETRIC_CDF_INV inverts the Geometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0D+00
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding value of X.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GEOMETRIC_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (A == 1.0D+00) THEN
        X = 1
    ELSE IF (A == 0.0D+00) THEN
        X = HUGE (X)
    ELSE
        X = 1 + INT (LOG(1.0D+00-CDF)/LOG(1.0D+00-A))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_CDF_VALUES (N_DATA, X, P, CDF)
 
!*****************************************************************************80
!
!! GEOMETRIC_CDF_VALUES returns values of the geometric CDF.
!
!  Discussion:
!
!    The geometric or Pascal probability density function gives the
!    probability that the first success will happen on the X-th Bernoulli
!    trial, given that the probability of a success on a single trial is P.
!
!    The value of CDF ( X, P ) is the probability that the first success
!    will happen on or before the X-th trial.
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = GeometricDistribution [ p ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall / CRC Press, 2000.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) X, the number of trials.
!
!    Output, real ( kind = 8 ) P, the probability of success
!    on one trial.
!
!    Output, real ( kind = 8 ) CDF, the cumulative density function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 14
 
    REAL (KIND=8) CDF
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: CDF_VEC = (/ 0.1900000000000000D+00, &
   & 0.2710000000000000D+00, 0.3439000000000000D+00, 0.6861894039100000D+00, &
   & 0.3600000000000000D+00, 0.4880000000000000D+00, 0.5904000000000000D+00, &
   & 0.9141006540800000D+00, 0.7599000000000000D+00, 0.8704000000000000D+00, &
   & 0.9375000000000000D+00, 0.9843750000000000D+00, 0.9995117187500000D+00, &
   & 0.9999000000000000D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) P
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: P_VEC = (/ 0.1D+00, 0.1D+00, 0.1D+00, 0.1D+00, &
   & 0.2D+00, 0.2D+00, 0.2D+00, 0.2D+00, 0.3D+00, 0.4D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.9D+00 &
   & /)
    INTEGER (KIND=4) X
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1, 2, 3, 10, 1, 2, 3, 10, 3, 3, 3, &
   & 5, 10, 3 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0
        P = 0.0D+00
        CDF = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        P = P_VEC (N_DATA)
        CDF = CDF_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GEOMETRIC_CHECK (A)
 
!*****************************************************************************80
!
!! GEOMETRIC_CHECK checks the parameter of the Geometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, logical GEOMETRIC_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL GEOMETRIC_CHECK
 
    IF (A < 0.0D+00 .OR. 1.0D+00 < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 0 or 1 < A.'
        GEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    GEOMETRIC_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! GEOMETRIC_MEAN returns the mean of the Geometric PDF.
!
!  Discussion:
!
!    MEAN is the expected value of the number of trials required
!    to obtain a single success.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = 1.0D+00 / A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! GEOMETRIC_PDF evaluates the Geometric PDF.
!
!  Discussion:
!
!    PDF(A;X) = A * ( 1 - A )^(X-1)
!
!    PDF(A;X) is the probability that exactly X Bernoulli trials, each
!    with probability of success A, will be required to achieve
!    a single success.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of trials.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
!
!  Special cases.
!
    IF (X < 1) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (A == 0.0D+00) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (A == 1.0D+00) THEN
 
        IF (X == 1) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
 
    ELSE
 
        PDF = A * (1.0D+00-A) ** (X-1)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! GEOMETRIC_SAMPLE samples the Geometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL GEOMETRIC_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEOMETRIC_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! GEOMETRIC_VARIANCE returns the variance of the Geometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of success on one trial.
!    0.0 <= A <= 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (1.0D+00-A) / (A*A)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GOMPERTZ_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! GOMPERTZ_CDF evaluates the Gompertz CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - EXP (-B*(A**X-1.0D+00)/LOG(A))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GOMPERTZ_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! GOMPERTZ_CDF_INV inverts the Gompertz CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GOMPERTZ_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF < 1.0D+00) THEN
        X = LOG (1.0D+00-LOG(1.0D+00-CDF)*LOG(A)/B) / LOG (A)
    ELSE
        X = HUGE (X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GOMPERTZ_CHECK (A, B)
 
!*****************************************************************************80
!
!! GOMPERTZ_CHECK checks the parameters of the Gompertz PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, logical GOMPERTZ_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL GOMPERTZ_CHECK
 
    IF (A <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GOMPERTZ_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 1.0!'
        GOMPERTZ_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GOMPERTZ_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.0!'
        GOMPERTZ_CHECK = .FALSE.
        RETURN
    END IF
 
    GOMPERTZ_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GOMPERTZ_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! GOMPERTZ_PDF evaluates the Gompertz PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B * A^X / exp ( B * ( A^X - 1 ) / log ( A ) )
!
!    for
!
!      0.0 <= X
!      1.0 <  A
!      0.0 <  B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (1.0D+00 < A) THEN
 
        PDF = EXP (LOG(B)+X*LOG(A)-(B/LOG(A))*(A**X-1.0D+00))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GOMPERTZ_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! GOMPERTZ_SAMPLE samples the Gompertz PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1 < A, 0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL GOMPERTZ_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_CDF (X, CDF)
 
!*****************************************************************************80
!
!! GUMBEL_CDF evaluates the Gumbel CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    CDF = EXP (-EXP(-X))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! GUMBEL_CDF_INV inverts the Gumbel CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GUMBEL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = - LOG (-LOG(CDF))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_MEAN (MEAN)
 
!*****************************************************************************80
!
!! GUMBEL_MEAN returns the mean of the Gumbel PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = EULER_CONSTANT ()
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_PDF (X, PDF)
 
!*****************************************************************************80
!
!! GUMBEL_PDF evaluates the Gumbel PDF.
!
!  Discussion:
!
!    PDF(X) = exp ( -X ) * exp ( - exp ( -X  ) ).
!
!    GUMBEL_PDF(X) = EXTREME_PDF(0,1;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    PDF = EXP (-X-EXP(-X))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! GUMBEL_SAMPLE samples the Gumbel PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL GUMBEL_CDF_INV (CDF, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUMBEL_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! GUMBEL_VARIANCE returns the variance of the Gumbel PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = R8_PI * R8_PI / 6.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! HALF_NORMAL_CDF evaluates the Half Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE
        CALL NORMAL_CDF (X, A, B, CDF2)
        CDF = 2.0D+00 * CDF2 - 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! HALF_NORMAL_CDF_INV inverts the Half Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HALF_NORMAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CDF2 = 0.5D+00 * (CDF+1.0D+00)
 
    CALL NORMAL_CDF_INV (CDF2, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION HALF_NORMAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! HALF_NORMAL_CHECK checks the parameters of the Half Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical HALF_NORMAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL HALF_NORMAL_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HALF_NORMAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        HALF_NORMAL_CHECK = .FALSE.
        RETURN
    END IF
 
    HALF_NORMAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! HALF_NORMAL_MEAN returns the mean of the Half Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    MEAN = A + B * SQRT (2.0D+00/R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! HALF_NORMAL_PDF evaluates the Half Normal PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =
!      sqrt ( 2 / PI ) * ( 1 / B ) * exp ( - 0.5D+00 * ( ( X - A ) / B )^2 )
!
!    for A <= X
!
!    The Half Normal PDF is a special case of both the Chi PDF and the
!    Folded Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = SQRT (2.0D+00/R8_PI) * (1.0D+00/B) * EXP (-0.5D+00*Y*Y)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! HALF_NORMAL_SAMPLE samples the Half Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL HALF_NORMAL_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HALF_NORMAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! HALF_NORMAL_VARIANCE returns the variance of the Half Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * (1.0D+00-2.0D+00/R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_CDF (X, N, M, L, CDF)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_CDF evaluates the Hypergeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) C1_LOG
    REAL (KIND=8) C2_LOG
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) X2
 
    C1_LOG = I4_CHOOSE_LOG (L-M, N)
    C2_LOG = I4_CHOOSE_LOG (L, N)
 
    PDF = EXP (C1_LOG-C2_LOG)
    CDF = PDF
 
    DO X2 = 0, X - 1
 
        PDF = PDF * REAL ((M-X2)*(N-X2), KIND=8) / REAL ((X2+1)*(L-M-N+X2+1), KIND=8)
 
        CDF = CDF + PDF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_CDF_VALUES (N_DATA, SAM, SUC, POP, N, FX)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_CDF_VALUES returns some values of the hypergeometric CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = HypergeometricDistribution [ sam, suc, pop ]
!      CDF [ dist, n ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 651-652.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) SAM, integer SUC, integer POP, the sample
!    size, success size, and population parameters of the function.
!
!    Output, integer ( kind = 4 ) N, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 16
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.6001858177500578D-01, &
   & 0.2615284665839845D+00, 0.6695237889132748D+00, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.5332595856827856D+00, 0.1819495964117640D+00, &
   & 0.4448047017527730D-01, 0.9999991751316731D+00, 0.9926860896560750D+00, &
   & 0.8410799901444538D+00, 0.3459800113391901D+00, 0.0000000000000000D+00, &
   & 0.2088888139634505D-02, 0.3876752992448843D+00, 0.9135215248834896D+00 /)
    INTEGER (KIND=4) N
    INTEGER (KIND=4) N_DATA
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 7, 8, 9, 10, 6, 6, 6, 6, 6, 6, 6, &
   & 6, 0, 0, 0, 0 /)
    INTEGER (KIND=4) POP
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: POP_VEC = (/ 100, 100, 100, 100, 100, 100, &
   & 100, 100, 100, 100, 100, 100, 90, 200, 1000, 10000 /)
    INTEGER (KIND=4) SAM
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: SAM_VEC = (/ 10, 10, 10, 10, 6, 7, 8, 9, 10, &
   & 10, 10, 10, 10, 10, 10, 10 /)
    INTEGER (KIND=4) SUC
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: SUC_VEC = (/ 90, 90, 90, 90, 90, 90, 90, 90, &
   & 10, 30, 50, 70, 90, 90, 90, 90 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        SAM = 0
        SUC = 0
        POP = 0
        N = 0
        FX = 0.0D+00
    ELSE
        SAM = SAM_VEC (N_DATA)
        SUC = SUC_VEC (N_DATA)
        POP = POP_VEC (N_DATA)
        N = N_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION HYPERGEOMETRIC_CHECK (N, M, L)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_CHECK checks the parameters of the Hypergeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls in the population.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, logical HYPERGEOMETRIC_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    LOGICAL HYPERGEOMETRIC_CHECK
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    IF (N < 0 .OR. L < N) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYPERGEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  Input N is out of range.'
        HYPERGEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (M < 0 .OR. L < M) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYPERGEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  Input M is out of range.'
        HYPERGEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (L < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYPERGEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  Input L is out of range.'
        HYPERGEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    HYPERGEOMETRIC_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_MEAN (N, M, L, MEAN)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_MEAN returns the mean of the Hypergeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls in the population.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) N
 
    MEAN = REAL (N*M, KIND=8) / REAL (L, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_PDF (X, N, M, L, PDF)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_PDF evaluates the Hypergeometric PDF.
!
!  Discussion:
!
!    PDF(N,M,L;X) = C(M,X) * C(L-M,N-X) / C(L,N).
!
!    PDF(N,M,L;X) is the probability of drawing X white balls in a
!    single random sample of size N from a population containing
!    M white balls and a total of L balls.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the desired number of white balls.
!    0 <= X <= N, usually, although any value of X can be given.
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) PDF, the probability of exactly K white balls.
!
    IMPLICIT NONE
 
    REAL (KIND=8) C1
    REAL (KIND=8) C2
    REAL (KIND=8) C3
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
    REAL (KIND=8) PDF_LOG
    INTEGER (KIND=4) X
!
!  Special cases.
!
    IF (X < 0) THEN
 
        PDF = 1.0D+00
 
    ELSE IF (N < X) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (M < X) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (L < X) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (N == 0) THEN
 
        IF (X == 0) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
 
    ELSE
 
        C1 = I4_CHOOSE_LOG (M, X)
        C2 = I4_CHOOSE_LOG (L-M, N-X)
        C3 = I4_CHOOSE_LOG (L, N)
 
        PDF_LOG = C1 + C2 - C3
 
        PDF = EXP (PDF_LOG)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_SAMPLE (N, M, L, SEED, X)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_SAMPLE samples the Hypergeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 165.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C1_LOG
    REAL (KIND=8) C2_LOG
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    INTEGER (KIND=4) X
 
    C1_LOG = I4_CHOOSE_LOG (L-M, N)
    C2_LOG = I4_CHOOSE_LOG (L, N)
 
    A = EXP (C1_LOG-C2_LOG)
    B = A
 
    U = R8_UNIFORM_01 (SEED)
 
    X = 0
 
    DO WHILE (A < U)
 
        B = B * REAL ((M-X)*(N-X), KIND=8) / REAL ((X+1)*(L-M-N+X+1), KIND=8)
 
        A = A + B
 
        X = X + 1
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPERGEOMETRIC_VARIANCE (N, M, L, VARIANCE)
 
!*****************************************************************************80
!
!! HYPERGEOMETRIC_VARIANCE returns the variance of the Hypergeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) VARIANCE
 
    VARIANCE = REAL (N*M*(L-M)*(L-N), KIND=8) / REAL (L*L*(L-1), KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_CHOOSE (N, K)
 
!*****************************************************************************80
!
!! I4_CHOOSE computes the binomial coefficient C(N,K) as an I4.
!
!  Discussion:
!
!    The value is calculated in such a way as to avoid overflow and
!    roundoff.  The calculation is done in integer arithmetic.
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
!    30 October 2014
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
!    Output, integer ( kind = 4 ) I4_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) I4_CHOOSE
    INTEGER (KIND=4) K
    INTEGER (KIND=4) MN
    INTEGER (KIND=4) MX
    INTEGER (KIND=4) N
    INTEGER (KIND=4) VALUE
 
    MN = MIN (K, N-K)
    MX = MAX (K, N-K)
 
    IF (MN < 0) THEN
 
        VALUE = 0
 
    ELSE IF (MN == 0) THEN
 
        VALUE = 1
 
    ELSE
 
        VALUE = MX + 1
 
        DO I = 2, MN
            VALUE = (VALUE*(MX+I)) / I
        END DO
 
    END IF
 
    I4_CHOOSE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_CHOOSE_LOG (N, K)
 
!*****************************************************************************80
!
!! I4_CHOOSE_LOG computes the logarithm of the Binomial coefficient.
!
!  Discussion:
!
!    The formula is:
!
!      LOG ( C(N,K) ) = LOG ( N! / ( K! * (N-K)! ) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) I4_CHOOSE_LOG, the logarithm of C(N,K).
!
    IMPLICIT NONE
 
    REAL (KIND=8) I4_CHOOSE_LOG
    INTEGER (KIND=4) K
    INTEGER (KIND=4) N
 
    I4_CHOOSE_LOG = LOG_GAMMA (REAL(N+1, KIND=8)) - LOG_GAMMA (REAL(K+1, KIND=8)) - LOG_GAMMA &
   & (REAL(N-K+1, KIND=8))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_HUGE ()
 
!*****************************************************************************80
!
!! I4_HUGE returns a "huge" I4.
!
!  Discussion:
!
!    On an IEEE 32 bit machine, I4_HUGE should be 2^31 - 1, and its
!    bit pattern should be
!
!     01111111111111111111111111111111
!
!    In this case, its numerical value is 2147483647.
!
!    Using the Dec/Compaq/HP Alpha FORTRAN compiler FORT, I could
!    use I4_HUGE() and HUGE interchangeably.
!
!    However, when using the G95, the values returned by HUGE were
!    not equal to 2147483647, apparently, and were causing severe
!    and obscure errors in my random number generator, which needs to
!    add I4_HUGE to the seed whenever the seed is negative.  So I
!    am backing away from invoking HUGE, whereas I4_HUGE is under
!    my control.
!
!    Explanation: because under G95 the default integer type is 64 bits!
!    So HUGE ( 1 ) = a very very huge integer indeed, whereas
!    I4_HUGE ( ) = the same old 32 bit big value.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) I4_HUGE, a "huge" I4.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I4_HUGE
 
    I4_HUGE = 2147483647
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_IS_POWER_OF_10 (N)
 
!*****************************************************************************80
!
!! I4_IS_POWER_OF_10 reports whether an I4 is a power of 10.
!
!  Discussion:
!
!    The powers of 10 are 1, 10, 100, 1000, 10000, and so on.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the integer to be tested.
!
!    Output, logical ( kind = 4 ) I4_IS_POWER_OF_10, TRUE if N is a power of 10.
!
    IMPLICIT NONE
 
    LOGICAL (KIND=4) I4_IS_POWER_OF_10
    INTEGER (KIND=4) N
    INTEGER (KIND=4) N_COPY
 
    N_COPY = N
    I4_IS_POWER_OF_10 = .FALSE.
 
    IF (N_COPY <= 0) THEN
        RETURN
    END IF
 
    DO WHILE ( 1 < N_COPY)
 
        IF (MOD(N_COPY, 10) /= 0) THEN
            RETURN
        END IF
 
        N_COPY = N_COPY / 10
 
    END DO
 
    I4_IS_POWER_OF_10 = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_UNIFORM_AB (A, B, SEED)
 
!*****************************************************************************80
!
!! I4_UNIFORM_AB returns a scaled pseudorandom I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ) value.
!
!    The pseudorandom number will be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) I4_UNIFORM_AB, a number between A and B.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    INTEGER (KIND=4) I4_UNIFORM_AB
    INTEGER (KIND=4) K
    REAL (KIND=4) R
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) VALUE
 
    IF (SEED == 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_UNIFORM_AB - Fatal error!'
        WRITE (*, '(a)') '  Input value of SEED = 0.'
        RETURN
    END IF
 
    K = SEED / 127773
 
    SEED = 16807 * (SEED-K*127773) - K * 2836
 
    IF (SEED < 0) THEN
        SEED = SEED + I4_HUGE ()
    END IF
 
    R = REAL (SEED, KIND=4) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
    R = (1.0E+00-R) * (REAL(MIN(A, B), KIND=4)-0.5E+00) + R * (REAL(MAX(A, B), KIND=4)+0.5E+00)
!
!  Use rounding to convert R to an integer between A and B.
!
    VALUE = NINT (R, KIND=4)
 
    VALUE = MAX (VALUE, MIN(A, B))
    VALUE = MIN (VALUE, MAX(A, B))
 
    I4_UNIFORM_AB = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4ROW_MAX (M, N, A, AMAX)
 
!*****************************************************************************80
!
!! I4ROW_MAX returns the maximums of the rows of an I4ROW.
!
!  Discussion:
!
!    An I4ROW is an M by N array of I4's, regarded
!    as an array of M rows of length N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), the array to be examined.
!
!    Output, integer ( kind = 4 ) AMAX(M), the maximums of the rows
!    of the array.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M, N)
    INTEGER (KIND=4) AMAX (M)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, M
 
        AMAX (I) = A (I, 1)
        DO J = 2, N
            IF (AMAX(I) < A(I, J)) THEN
                AMAX (I) = A (I, J)
            END IF
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4ROW_MEAN (M, N, A, MEAN)
 
!*****************************************************************************80
!
!! I4ROW_MEAN returns the means of the rows of an I4ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of data.
!
!    Input, integer ( kind = 4 ) A(M,N), the array.
!
!    Output, real ( kind = 8 ) MEAN(M), the mean of each row.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M, N)
    INTEGER (KIND=4) I
    REAL (KIND=8) MEAN (M)
 
    DO I = 1, M
        MEAN (I) = SUM (A(I, 1:N)) / REAL (N, KIND=8)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4ROW_MIN (M, N, A, AMIN)
 
!*****************************************************************************80
!
!! I4ROW_MIN returns the minimums of the rows of an I4ROW.
!
!  Discussion:
!
!    An I4ROW is an M by N array of I4's, regarded
!    as an array of M rows of length N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), the array to be examined.
!
!    Output, integer ( kind = 4 ) AMIN(M), the minimums of the rows.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M, N)
    INTEGER (KIND=4) AMIN (M)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, M
 
        AMIN (I) = A (I, 1)
        DO J = 2, N
            IF (A(I, J) < AMIN(I)) THEN
                AMIN (I) = A (I, J)
            END IF
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4ROW_VARIANCE (M, N, A, VARIANCE)
 
!*****************************************************************************80
!
!! I4ROW_VARIANCE returns the variances of the rows of an I4ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns of data.
!
!    Input, integer ( kind = 4 ) A(M,N), the array.
!
!    Output, real ( kind = 8 ) VARIANCE(M), the variance of each row.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M, N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE (M)
 
    DO I = 1, M
 
        MEAN = REAL (SUM(A(I, 1:N)), KIND=8) / REAL (N, KIND=8)
 
        VARIANCE (I) = 0.0D+00
        DO J = 1, N
            VARIANCE (I) = VARIANCE (I) + (REAL(A(I, J), KIND=8)-MEAN) ** 2
        END DO
 
        IF (1 < N) THEN
            VARIANCE (I) = VARIANCE (I) / REAL (N-1, KIND=8)
        ELSE
            VARIANCE (I) = 0.0D+00
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_MAX (N, A, AMAX)
 
!*****************************************************************************80
!
!! I4VEC_MAX computes the maximum element of an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, integer ( kind = 4 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) AMAX, the value of the largest entry.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (N)
    INTEGER (KIND=4) AMAX
 
    AMAX = MAXVAL (A(1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_MEAN (N, X, MEAN)
 
!*****************************************************************************80
!
!! I4VEC_MEAN returns the mean of an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) X(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean, or average, of
!    the vector entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) X (N)
 
    MEAN = REAL (SUM(X(1:N)), KIND=8) / REAL (N, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_MIN (N, A, AMIN)
 
!*****************************************************************************80
!
!! I4VEC_MIN computes the minimum element of an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, integer ( kind = 4 ) A(N), the array.
!
!    Output, integer ( kind = 4 ) AMIN, the value of the smallest entry.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (N)
    INTEGER (KIND=4) AMIN
 
    AMIN = MINVAL (A(1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (N)
    INTEGER (KIND=4) BIG
    INTEGER (KIND=4) I
    CHARACTER (LEN=*) TITLE
 
    IF (TITLE /= ' ') THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') TRIM (TITLE)
    END IF
 
    BIG = MAXVAL (ABS(A(1:N)))
 
    WRITE (*, '(a)') ' '
    IF (BIG < 1000) THEN
        DO I = 1, N
            WRITE (*, '(i8,1x,i4)') I, A (I)
        END DO
    ELSE IF (BIG < 1000000) THEN
        DO I = 1, N
            WRITE (*, '(i8,1x,i7)') I, A (I)
        END DO
    ELSE
        DO I = 1, N
            WRITE (*, '(i8,i11)') I, A (I)
        END DO
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_RUN_COUNT (N, A, RUN_COUNT)
 
!*****************************************************************************80
!
!! I4VEC_RUN_COUNT counts runs of equal values in an I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of integer values.
!
!    A run is a sequence of equal values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be examined.
!
!    Output, integer ( kind = 4 ) RUN_COUNT, the number of runs.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) RUN_COUNT
    INTEGER (KIND=4) TEST
 
    RUN_COUNT = 0
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    TEST = 0
 
    DO I = 1, N
 
        IF (I == 1 .OR. A(I) /= TEST) THEN
            RUN_COUNT = RUN_COUNT + 1
            TEST = A (I)
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_UNIFORM_AB (N, A, B, SEED, X)
 
!*****************************************************************************80
!
!! I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    The pseudorandom numbers should be scaled to be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the vector.
!
!    Input, integer ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, integer ( kind = 4 ) X(N), a vector of numbers between A and B.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    INTEGER (KIND=4) I
    INTEGER (KIND=4), PARAMETER :: I4_HUGE = 2147483647
    INTEGER (KIND=4) K
    REAL (KIND=4) R
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) VALUE
    INTEGER (KIND=4) X (N)
 
    IF (SEED == 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4VEC_UNIFORM_AB - Fatal error!'
        WRITE (*, '(a)') '  Input value of SEED = 0.'
        RETURN
    END IF
 
    DO I = 1, N
 
        K = SEED / 127773
 
        SEED = 16807 * (SEED-K*127773) - K * 2836
 
        IF (SEED < 0) THEN
            SEED = SEED + I4_HUGE
        END IF
 
        R = REAL (SEED, KIND=4) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
        R = (1.0E+00-R) * (REAL(MIN(A, B), KIND=4)-0.5E+00) + R * (REAL(MAX(A, B), &
       & KIND=4)+0.5E+00)
!
!  Use rounding to convert R to an integer between A and B.
!
        VALUE = NINT (R, KIND=4)
 
        VALUE = MAX (VALUE, MIN(A, B))
        VALUE = MIN (VALUE, MAX(A, B))
 
        X (I) = VALUE
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_UNIQUE_COUNT (N, A, UNIQUE_NUM)
 
!*****************************************************************************80
!
!! I4VEC_UNIQUE_COUNT counts the unique elements in an unsorted I4VEC.
!
!  Discussion:
!
!    An I4VEC is a vector of I4's.
!
!    Because the array is unsorted, this algorithm is O(N^2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Input, integer ( kind = 4 ) A(N), the unsorted array to examine.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    of A.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) UNIQUE_NUM
 
    UNIQUE_NUM = 0
 
    DO I = 1, N
 
        UNIQUE_NUM = UNIQUE_NUM + 1
 
        DO J = 1, I - 1
 
            IF (A(I) == A(J)) THEN
                UNIQUE_NUM = UNIQUE_NUM - 1
                EXIT
            END IF
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4VEC_VARIANCE (N, X, VARIANCE)
 
!*****************************************************************************80
!
!! I4VEC_VARIANCE returns the variance of an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
    INTEGER (KIND=4) X (N)
 
    CALL I4VEC_MEAN (N, X, MEAN)
 
    VARIANCE = SUM ((REAL(X(1:N), KIND=8)-MEAN)**2)
 
    IF (1 < N) THEN
        VARIANCE = VARIANCE / REAL (N-1, KIND=8)
    ELSE
        VARIANCE = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INVERSE_GAUSSIAN_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_CDF evaluates the Inverse Gaussian CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 < X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        X1 = SQRT (B/X) * (X-A) / A
        CALL NORMAL_01_CDF (X1, CDF1)
 
        X2 = - SQRT (B/X) * (X+A) / A
        CALL NORMAL_01_CDF (X2, CDF2)
 
        CDF = CDF1 + EXP (2.0D+00*B/A) * CDF2
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION INVERSE_GAUSSIAN_CHECK (A, B)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_CHECK checks the parameters of the Inverse Gaussian CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical INVERSE_GAUSSIAN_CHECK, is true if the parameters
!    are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL INVERSE_GAUSSIAN_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        INVERSE_GAUSSIAN_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'INVERSE_GAUSSIAN_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        INVERSE_GAUSSIAN_CHECK = .FALSE.
        RETURN
    END IF
 
    INVERSE_GAUSSIAN_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INVERSE_GAUSSIAN_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_MEAN returns the mean of the Inverse Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INVERSE_GAUSSIAN_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_PDF evaluates the Inverse Gaussian PDF.
!
!  Discussion:
!
!    The Inverse Gaussian PDF is also known as the Wald PDF
!    and the Inverse Normal PDF.
!
!    PDF(A,B;X)
!      = sqrt ( B / ( 2 * PI * X^3 ) )
!        * exp ( - B * ( X - A )^2 / ( 2.0D+00 * A^2 * X ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = SQRT (B/(2.0D+00*R8_PI*X**3)) * EXP (-B*(X-A)**2/(2.0D+00*A*A*X))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INVERSE_GAUSSIAN_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_SAMPLE samples the Inverse Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PHI
    INTEGER (KIND=4) SEED
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    PHI = B / A
    CALL NORMAL_01_SAMPLE (SEED, Z)
    Y = Z * Z
 
    T = 1.0D+00 + 0.5D+00 * (Y-SQRT(4.0D+00*PHI*Y+Y*Y)) / PHI
    U = R8_UNIFORM_01 (SEED)
 
    IF (U*(1.0D+00+T) <= 1.0D+00) THEN
        X = A * T
    ELSE
        X = A / T
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE INVERSE_GAUSSIAN_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! INVERSE_GAUSSIAN_VARIANCE returns the variance of the Inverse Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = A ** 3 / B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! LAPLACE_CDF evaluates the Laplace CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    IF (X <= A) THEN
        CDF = 0.5D+00 * EXP (Y)
    ELSE
        CDF = 1.0D+00 - 0.5D+00 * EXP (-Y)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! LAPLACE_CDF_INV inverts the Laplace CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LAPLACE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF <= 0.5D+00) THEN
        X = A + B * LOG (2.0D+00*CDF)
    ELSE
        X = A - B * LOG (2.0D+00*(1.0D+00-CDF))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_CDF_VALUES (N_DATA, MU, BETA, X, FX)
 
!*****************************************************************************80
!
!! LAPLACE_CDF_VALUES returns some values of the Laplace CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = LaplaceDistribution [ mu, beta ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) BETA, the shape parameter.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) BETA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: BETA_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.8160602794142788D+00, 0.9323323583816937D+00, 0.9751064658160680D+00, &
   & 0.6967346701436833D+00, 0.6417343447131054D+00, 0.6105996084642976D+00, &
   & 0.5906346234610091D+00, 0.5000000000000000D+00, 0.3032653298563167D+00, &
   & 0.1839397205857212D+00, 0.1115650800742149D+00 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.0000000000000000D+01, &
   & 0.0000000000000000D+01, 0.0000000000000000D+01, 0.0000000000000000D+01, &
   & 0.0000000000000000D+01, 0.0000000000000000D+01, 0.0000000000000000D+01, &
   & 0.0000000000000000D+01, 0.1000000000000000D+01, 0.2000000000000000D+01, &
   & 0.3000000000000000D+01, 0.4000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.0000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        BETA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        BETA = BETA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LAPLACE_CHECK (A, B)
 
!*****************************************************************************80
!
!! LAPLACE_CHECK checks the parameters of the Laplace PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical LAPLACE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL LAPLACE_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LAPLACE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        LAPLACE_CHECK = .FALSE.
        RETURN
    END IF
 
    LAPLACE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! LAPLACE_MEAN returns the mean of the Laplace PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! LAPLACE_PDF evaluates the Laplace PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = exp ( - abs ( X - A ) / B ) / ( 2 * B )
!
!    The Laplace PDF is also known as the Double Exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    PDF = EXP (-ABS(X-A)/B) / (2.0D+00*B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! LAPLACE_SAMPLE samples the Laplace PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LAPLACE_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAPLACE_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! LAPLACE_VARIANCE returns the variance of the Laplace PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 2.0D+00 * B * B
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LERCH (A, B, C)
 
!*****************************************************************************80
!
!! LERCH estimates the Lerch transcendent function.
!
!  Discussion:
!
!    The Lerch transcendent function is defined as:
!
!      LERCH ( A, B, C ) = Sum ( 0 <= K < Infinity ) A^K / ( C + K )^B
!
!    excluding any term with ( C + K ) = 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
!
!  Thanks:
!
!    Oscar van Vlijmen
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the function.
!
!    Output, real ( kind = 8 ) LERCH, an approximation to the Lerch
!    transcendent function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A_K
    REAL (KIND=8) B
    REAL (KIND=8) C
    INTEGER (KIND=4) K
    REAL (KIND=8) LERCH
    REAL (KIND=8) SUM2
    REAL (KIND=8) SUM2_OLD
 
    SUM2 = 0.0D+00
    K = 0
    A_K = 1.0D+00
 
    DO
 
        SUM2_OLD = SUM2
 
        IF (C+REAL(K, KIND=8) == 0.0D+00) THEN
            K = K + 1
            A_K = A_K * A
            CYCLE
        END IF
 
        SUM2 = SUM2 + A_K / (C+REAL(K, KIND=8)) ** B
 
        IF (SUM2 <= SUM2_OLD) THEN
            EXIT
        END IF
 
        K = K + 1
        A_K = A_K * A
 
    END DO
 
    LERCH = SUM2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEVY_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! LEVY_CDF evaluates the Levy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    Normally, A <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEVY_CDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0'
        RETURN
    END IF
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - R8_ERROR_F (SQRT(B/(2.0D+00*(X-A))))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEVY_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! LEVY_CDF_INV inverts the Levy CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) X
    REAL (KIND=8) X1
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEVY_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEVY_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0'
        RETURN
    END IF
 
    CDF1 = 1.0D+00 - 0.5D+00 * CDF
    CALL NORMAL_01_CDF_INV (CDF1, X1)
    X = A + B / (X1*X1)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEVY_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! LEVY_PDF evaluates the Levy PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = sqrt ( B / ( 2 * PI ) )
!               * exp ( - B / ( 2 * ( X - A ) )
!               / ( X - A )^(3/2)
!
!    for A <= X.
!
!    Note that the Levy PDF does not have a finite mean or variance.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    Normally, A <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEVY_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0'
        RETURN
    END IF
 
    IF (X <= A) THEN
        PDF = 0.0D+00
    ELSE
        PDF = SQRT (B/(2.0D+00*R8_PI)) * EXP (-B/(2.0D+00*(X-A))) / SQRT ((X-A)**3)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEVY_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! LEVY_SAMPLE samples the Levy PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LEVY_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! LOGISTIC_CDF evaluates the Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    CDF = 1.0D+00 / (1.0D+00+EXP((A-X)/B))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! LOGISTIC_CDF_INV inverts the Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOGISTIC_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A - B * LOG ((1.0D+00-CDF)/CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_CDF_VALUES (N_DATA, MU, BETA, X, FX)
 
!*****************************************************************************80
!
!! LOGISTIC_CDF_VALUES returns some values of the Logistic CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = LogisticDistribution [ mu, beta ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) BETA, the shape parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) BETA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: BETA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.8807970779778824D+00, 0.9820137900379084D+00, 0.9975273768433652D+00, &
   & 0.6224593312018546D+00, 0.5825702064623147D+00, 0.5621765008857981D+00, &
   & 0.5498339973124779D+00, 0.6224593312018546D+00, 0.5000000000000000D+00, &
   & 0.3775406687981454D+00, 0.2689414213699951D+00 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        BETA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        BETA = BETA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LOGISTIC_CHECK (A, B)
 
!*****************************************************************************80
!
!! LOGISTIC_CHECK checks the parameters of the Logistic CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical LOGISTIC_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL LOGISTIC_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOGISTIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        LOGISTIC_CHECK = .FALSE.
        RETURN
    END IF
 
    LOGISTIC_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! LOGISTIC_MEAN returns the mean of the Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! LOGISTIC_PDF evaluates the Logistic PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = exp ( ( A - X ) / B ) /
!      ( B * ( 1 + exp ( ( A - X ) / B ) )^2 )
!
!    The Logistic PDF is also known as the Sech-Squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) TEMP
    REAL (KIND=8) X
 
    TEMP = EXP ((A-X)/B)
 
    PDF = TEMP / (B*(1.0D+00+TEMP)**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! LOGISTIC_SAMPLE samples the Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LOGISTIC_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOGISTIC_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! LOGISTIC_VARIANCE returns the variance of the Logistic PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = R8_PI * R8_PI * B * B / 3.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! LOG_NORMAL_CDF evaluates the Lognormal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) LOGX
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        LOGX = LOG (X)
 
        CALL NORMAL_CDF (LOGX, A, B, CDF)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! LOG_NORMAL_CDF_INV inverts the Lognormal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) LOGX
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_NORMAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CALL NORMAL_CDF_INV (CDF, A, B, LOGX)
 
    X = EXP (LOGX)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_CDF_VALUES (N_DATA, MU, SIGMA, X, FX)
 
!*****************************************************************************80
!
!! LOG_NORMAL_CDF_VALUES returns some values of the Log Normal CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = LogNormalDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the shape parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.2275013194817921D-01, &
   & 0.2697049307349095D+00, 0.5781741008028732D+00, 0.7801170895122241D+00, &
   & 0.4390310097476894D+00, 0.4592655190218048D+00, 0.4694258497695908D+00, &
   & 0.4755320473858733D+00, 0.3261051056816658D+00, 0.1708799040927608D+00, &
   & 0.7343256357952060D-01, 0.2554673736161761D-01 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) SIGMA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: SIGMA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        SIGMA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        SIGMA = SIGMA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LOG_NORMAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! LOG_NORMAL_CHECK checks the parameters of the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, logical LOG_NORMAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL LOG_NORMAL_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_NORMAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        LOG_NORMAL_CHECK = .FALSE.
        RETURN
    END IF
 
    LOG_NORMAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! LOG_NORMAL_MEAN returns the mean of the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = EXP (A+0.5D+00*B*B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! LOG_NORMAL_PDF evaluates the Lognormal PDF.
!
!  Discussion:
!
!    PDF(A,B;X)
!      = exp ( - 0.5 * ( ( log ( X ) - A ) / B )^2 )
!        / ( B * X * sqrt ( 2 * PI ) )
!
!    The Lognormal PDF is also known as the Cobb-Douglas PDF,
!    and as the Antilog_normal PDF.
!
!    The Lognormal PDF describes a variable X whose logarithm
!    is normally distributed.
!
!    The special case A = 0, B = 1 is known as Gilbrat's PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = EXP (-0.5D+00*((LOG(X)-A)/B)**2) / (B*X*SQRT(2.0D+00*R8_PI))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! LOG_NORMAL_SAMPLE samples the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LOG_NORMAL_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_NORMAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! LOG_NORMAL_VARIANCE returns the variance of the Lognormal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = EXP (2.0D+00*A+B*B) * (EXP(B*B)-1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! LOG_SERIES_CDF evaluates the Logarithmic Series CDF.
!
!  Discussion:
!
!    Simple summation is used, with a recursion to generate successive
!    values of the PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Thanks:
!
!    Oscar van Vlijmen
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) X2
 
    CDF = 0.0D+00
 
    DO X2 = 1, X
 
        IF (X2 == 1) THEN
            PDF = - A / LOG (1.0D+00-A)
        ELSE
            PDF = REAL (X2-1, KIND=8) * A * PDF / REAL (X2, KIND=8)
        END IF
 
        CDF = CDF + PDF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! LOG_SERIES_CDF_INV inverts the Logarithmic Series CDF.
!
!  Discussion:
!
!    Simple summation is used.  The only protection against an
!    infinite loop caused by roundoff is that X cannot be larger
!    than 1000.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF for which
!    CDF(X-1) <= CDF <= CDF(X).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4), PARAMETER :: XMAX = 1000
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_SERIES_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CDF2 = 0.0D+00
    X = 1
 
    DO WHILE (CDF2 < CDF .AND. X < XMAX)
 
        IF (X == 1) THEN
            PDF = - A / LOG (1.0D+00-A)
        ELSE
            PDF = REAL (X-1, KIND=8) * A * PDF / REAL (X, KIND=8)
        END IF
 
        CDF2 = CDF2 + PDF
 
        X = X + 1
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_CDF_VALUES (N_DATA, T, N, FX)
 
!*****************************************************************************80
!
!! LOG_SERIES_CDF_VALUES returns some values of the log series CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = LogSeriesDistribution [ t ]
!      CDF [ dist, n ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) T, the parameter of the function.
!
!    Output, integer ( kind = 4 ) N, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 29
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.9491221581029903D+00, &
   & 0.9433541128559735D+00, 0.9361094611773272D+00, 0.9267370278044118D+00, &
   & 0.9141358246245129D+00, 0.8962840235449100D+00, 0.8690148741955517D+00, &
   & 0.8221011541254772D+00, 0.7213475204444817D+00, 0.6068261510845583D+00, &
   & 0.5410106403333613D+00, 0.4970679476476894D+00, 0.4650921887927060D+00, &
   & 0.4404842934597863D+00, 0.4207860535926143D+00, 0.4045507673897055D+00, &
   & 0.3908650337129266D+00, 0.2149757685421097D+00, 0.0000000000000000D+00, &
   & 0.2149757685421097D+00, 0.3213887739704539D+00, 0.3916213575531612D+00, &
   & 0.4437690508633213D+00, 0.4850700239649681D+00, 0.5191433267738267D+00, &
   & 0.5480569580144867D+00, 0.5731033910767085D+00, 0.5951442521714636D+00, &
   & 0.6147826594068904D+00 /)
    INTEGER (KIND=4) N
    INTEGER (KIND=4) N_DATA
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
   & 1, 1, 1, 1, 1, 1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
    REAL (KIND=8) T
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: T_VEC = (/ 0.1000000000000000D+00, &
   & 0.1111111111111111D+00, 0.1250000000000000D+00, 0.1428571428571429D+00, &
   & 0.1666666666666667D+00, 0.2000000000000000D+00, 0.2500000000000000D+00, &
   & 0.3333333333333333D+00, 0.5000000000000000D+00, 0.6666666666666667D+00, &
   & 0.7500000000000000D+00, 0.8000000000000000D+00, 0.8333333333333333D+00, &
   & 0.8571485714857149D+00, 0.8750000000000000D+00, 0.8888888888888889D+00, &
   & 0.9000000000000000D+00, 0.9900000000000000D+00, 0.9900000000000000D+00, &
   & 0.9900000000000000D+00, 0.9900000000000000D+00, 0.9900000000000000D+00, &
   & 0.9900000000000000D+00, 0.9900000000000000D+00, 0.9900000000000000D+00, &
   & 0.9900000000000000D+00, 0.9900000000000000D+00, 0.9900000000000000D+00, &
   & 0.9900000000000000D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        T = 0.0D+00
        N = 0
        FX = 0.0D+00
    ELSE
        T = T_VEC (N_DATA)
        N = N_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LOG_SERIES_CHECK (A)
 
!*****************************************************************************80
!
!! LOG_SERIES_CHECK checks the parameter of the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, logical LOG_SERIES_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL LOG_SERIES_CHECK
 
    IF (A <= 0.0D+00 .OR. 1.0D+00 <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_SERIES_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.0D+00 or 1.0D+00 <= A'
        LOG_SERIES_CHECK = .FALSE.
        RETURN
    END IF
 
    LOG_SERIES_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! LOG_SERIES_MEAN returns the mean of the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = - A / ((1.0D+00-A)*LOG(1.0D+00-A))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! LOG_SERIES_PDF evaluates the Logarithmic Series PDF.
!
!  Discussion:
!
!    PDF(A;X) = - A**X / ( X * log ( 1 - A ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X <= 0) THEN
        PDF = 0.0D+00
    ELSE
        PDF = - A ** X / (REAL(X, KIND=8)*LOG(1.0D+00-A))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! LOG_SERIES_SAMPLE samples the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer-Verlag, 1986, page 547.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) V
    INTEGER (KIND=4) X
 
    U = R8_UNIFORM_01 (SEED)
    V = R8_UNIFORM_01 (SEED)
 
    X = INT (1.0D+00+LOG(V)/(LOG(1.0D+00-(1.0D+00-A)**U)))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_SERIES_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! LOG_SERIES_VARIANCE returns the variance of the Logarithmic Series PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A < 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) VARIANCE
 
    ALPHA = - 1.0D+00 / LOG (1.0D+00-A)
 
    VARIANCE = A * ALPHA * (1.0D+00-ALPHA*A) / (1.0D+00-A) ** 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_UNIFORM_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_CDF evaluates the Log Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE IF (X < B) THEN
        CDF = (LOG(X)-LOG(A)) / (LOG(B)-LOG(A))
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_UNIFORM_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_CDF_INV inverts the Log Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_UNIFORM_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A * EXP ((LOG(B)-LOG(A))*CDF)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LOG_UNIFORM_CHECK (A, B)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_CHECK checks the parameters of the Log Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Output, logical LOG_UNIFORM_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL LOG_UNIFORM_CHECK
 
    IF (A <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_UNIFORM_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 1.'
        LOG_UNIFORM_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LOG_UNIFORM_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= A.'
        LOG_UNIFORM_CHECK = .FALSE.
        RETURN
    END IF
 
    LOG_UNIFORM_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_UNIFORM_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_MEAN returns the mean of the Log Uniform PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = (B-A) / (LOG(B)-LOG(A))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_UNIFORM_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_PDF evaluates the Log Uniform PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1 / ( X * ( log ( B ) - log ( A ) ) ) for A <= X <= B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < A) THEN
        PDF = 0.0D+00
    ELSE IF (X <= B) THEN
        PDF = 1.0D+00 / (X*(LOG(B)-LOG(A)))
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOG_UNIFORM_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! LOG_UNIFORM_SAMPLE samples the Log Uniform PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    1.0 < A < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LOG_UNIFORM_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_CDF (X, CDF)
 
!*****************************************************************************80
!
!! LORENTZ_CDF evaluates the Lorentz CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    CDF = 0.5D+00 + ATAN (X) / R8_PI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! LORENTZ_CDF_INV inverts the Lorentz CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LORENTZ_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = TAN (R8_PI*(CDF-0.5D+00))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_MEAN (MEAN)
 
!*****************************************************************************80
!
!! LORENTZ_MEAN returns the mean of the Lorentz PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_PDF (X, PDF)
 
!*****************************************************************************80
!
!! LORENTZ_PDF evaluates the Lorentz PDF.
!
!  Discussion:
!
!    PDF(X) = 1 / ( PI * ( 1 + X^2 ) )
!
!    The chief interest of the Lorentz PDF is that it is easily
!    inverted, and can be used to dominate other PDF's in an
!    acceptance/rejection method.
!
!    LORENTZ_PDF(X) = CAUCHY_PDF(0,1;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    PDF = 1.0D+00 / (R8_PI*(1.0D+00+X*X))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! LORENTZ_SAMPLE samples the Lorentz PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL LORENTZ_CDF_INV (CDF, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LORENTZ_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! LORENTZ_VARIANCE returns the variance of the Lorentz PDF.
!
!  Discussion:
!
!    The variance of the Lorentz PDF is not well defined.  This routine
!    is made available for completeness only, and simply returns
!    a "very large" number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE
 
    VARIANCE = HUGE (VARIANCE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! MAXWELL_CDF evaluates the Maxwell CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) P2
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        X2 = X / A
        P2 = 1.5D+00
 
        CDF = R8_GAMMA_INC (P2, X2)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! MAXWELL_CDF_INV inverts the Maxwell CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MAXWELL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
        RETURN
    END IF
 
    X1 = 0.0D+00
    CDF1 = 0.0D+00
 
    X2 = 1.0D+00
 
    DO
 
        CALL MAXWELL_CDF (X2, A, CDF2)
 
        IF (CDF < CDF2) THEN
            EXIT
        END IF
 
        X2 = 2.0D+00 * X2
 
        IF (1000000.0D+00 < X2) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'MAXWELL_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Initial bracketing effort fails.'
            RETURN
        END IF
 
    END DO
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL MAXWELL_CDF (X3, A, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'MAXWELL_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION MAXWELL_CHECK (A)
 
!*****************************************************************************80
!
!! MAXWELL_CHECK checks the parameters of the Maxwell CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, logical MAXWELL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL MAXWELL_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MAXWELL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.0.'
        MAXWELL_CHECK = .FALSE.
        RETURN
    END IF
 
    MAXWELL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! MAXWELL_MEAN returns the mean of the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean value.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = SQRT (2.0D+00) * A * GAMMA (2.0D+00) / GAMMA (1.5D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! MAXWELL_PDF evaluates the Maxwell PDF.
!
!  Discussion:
!
!    PDF(A;X) = exp ( - 0.5D+00 * ( X / A )^2 ) * ( X / A )^2 /
!      ( sqrt ( 2 ) * A * GAMMA ( 1.5D+00 ) )
!
!    MAXWELL_PDF(A;X) = CHI_PDF(0,A,3;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0 < X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= 0.0D+00) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = X / A
 
        PDF = EXP (-0.5D+00*Y*Y) * Y * Y / (SQRT(2.0D+00)*A*GAMMA(1.5D+00))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! MAXWELL_SAMPLE samples the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    A2 = 3.0D+00
    CALL CHI_SQUARE_SAMPLE (A2, SEED, X)
 
    X = A * SQRT (X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MAXWELL_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! MAXWELL_VARIANCE returns the variance of the Maxwell PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = A * A * (3.0D+00-2.0D+00*(GAMMA(2.0D+00)/GAMMA(1.5D+00))**2)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION MULTICOEF_CHECK (NFACTOR, FACTOR)
 
!*****************************************************************************80
!
!! MULTICOEF_CHECK checks the parameters of the multinomial coefficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NFACTOR, the number of factors.
!    1 <= NFACTOR.
!
!    Input, integer ( kind = 4 ) FACTOR(NFACTOR), contains the factors.
!    0.0 <= FACTOR(I).
!
!    Output, logical MULTICOEF_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) NFACTOR
 
    INTEGER (KIND=4) FACTOR (NFACTOR)
    INTEGER (KIND=4) I
    LOGICAL MULTICOEF_CHECK
 
    IF (NFACTOR < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTICOEF_CHECK - Fatal error!'
        WRITE (*, '(a)') '  NFACTOR < 1.'
        MULTICOEF_CHECK = .FALSE.
        RETURN
    END IF
 
    DO I = 1, NFACTOR
 
        IF (FACTOR(I) < 0) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'MULTICOEF_CHECK - Fatal error'
            WRITE (*, '(a,i8)') '  Factor ', I
            WRITE (*, '(a,i8)') '  = ', FACTOR (I)
            WRITE (*, '(a)') '  But this value must be nonnegative.'
            MULTICOEF_CHECK = .FALSE.
            RETURN
        END IF
 
    END DO
 
    MULTICOEF_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_COEF1 (NFACTOR, FACTOR, NCOMB)
 
!*****************************************************************************80
!
!! MULTINOMIAL_COEF1 computes a Multinomial coefficient.
!
!  Discussion:
!
!    The multinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where FACTOR(1) objects are indistinguishable of type 1,
!    ... and FACTOR(NFACTOR) are indistinguishable of type NFACTOR,
!    and N is the sum of FACTOR(1) through FACTOR(NFACTOR).
!
!    NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
!
!    The log of the gamma function is used, to avoid overflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NFACTOR, the number of factors.
!    1 <= NFACTOR.
!
!    Input, integer ( kind = 4 ) FACTOR(NFACTOR), contains the factors.
!    0.0 <= FACTOR(I).
!
!    Output, integer ( kind = 4 ) NCOMB, the value of the multinomial
!    coefficient.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) NFACTOR
 
    LOGICAL CHECK
    REAL (KIND=8) FACN
    INTEGER (KIND=4) FACTOR (NFACTOR)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    INTEGER (KIND=4) NCOMB
 
    CHECK = MULTICOEF_CHECK (NFACTOR, FACTOR)
 
    IF ( .NOT. CHECK) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTINOMIAL_COEF1 - Fatal error!'
        WRITE (*, '(a)') '  MULTICOEF_CHECK failed.'
        NCOMB = - I4_HUGE ()
        RETURN
    END IF
!
!  The factors sum to N.
!
    N = SUM (FACTOR(1:NFACTOR))
 
    FACN = LOG_GAMMA (REAL(N+1, KIND=8))
 
    DO I = 1, NFACTOR
 
        FACN = FACN - LOG_GAMMA (REAL(FACTOR(I)+1, KIND=8))
 
    END DO
 
    NCOMB = NINT (EXP(FACN))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_COEF2 (NFACTOR, FACTOR, NCOMB)
 
!*****************************************************************************80
!
!! MULTINOMIAL_COEF2 computes a Multinomial coefficient.
!
!  Discussion:
!
!    The multinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where FACTOR(1) objects are indistinguishable of type 1,
!    ... and FACTOR(NFACTOR) are indistinguishable of type NFACTOR,
!    and N is the sum of FACTOR(1) through FACTOR(NFACTOR).
!
!    NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
!
!    A direct method is used, which should be exact.  However, there
!    is a possibility of intermediate overflow of the result.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NFACTOR, the number of factors.
!    1 <= NFACTOR.
!
!    Input, integer ( kind = 4 ) FACTOR(NFACTOR), contains the factors.
!    0.0 <= FACTOR(I).
!
!    Output, integer ( kind = 4 ) NCOMB, the value of the multinomial
!    coefficient.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) NFACTOR
 
    LOGICAL CHECK
    INTEGER (KIND=4) FACTOR (NFACTOR)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) K
    INTEGER (KIND=4) NCOMB
 
    CHECK = MULTICOEF_CHECK (NFACTOR, FACTOR)
 
    IF ( .NOT. CHECK) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTINOMIAL_COEF2 - Fatal error!'
        WRITE (*, '(a)') '  MULTICOEF_CHECK failed.'
        NCOMB = - I4_HUGE ()
        RETURN
    END IF
 
    NCOMB = 1
    K = 0
 
    DO I = 1, NFACTOR
 
        DO J = 1, FACTOR (I)
            K = K + 1
            NCOMB = (NCOMB*K) / J
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION MULTINOMIAL_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! MULTINOMIAL_CHECK checks the parameters of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    Sum ( 1 <= I <= B ) C(I) = 1.0.
!
!    Output, logical MULTINOMIAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    REAL (KIND=8) C_SUM
    INTEGER (KIND=4) I
    LOGICAL MULTINOMIAL_CHECK
 
    IF (B < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < 1.'
        MULTINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    DO I = 1, B
 
        IF (C(I) < 0.0D+00 .OR. 1.0D+00 < C(I)) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'MULTINOMIAL_CHECK - Fatal error!'
            WRITE (*, '(a)') '  Input C(I) is out of range.'
            MULTINOMIAL_CHECK = .FALSE.
            RETURN
        END IF
 
    END DO
 
    C_SUM = SUM (C)
 
    IF (0.0001D+00 < ABS(1.0D+00-C_SUM)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MULTINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  The probabilities do not sum to 1.'
        MULTINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    MULTINOMIAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_COVARIANCE (A, B, C, COVARIANCE)
 
!*****************************************************************************80
!
!! MULTINOMIAL_COVARIANCE returns the covariances of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    SUM ( 1 <= I <= B) C(I) = 1.0.
!
!    Output, real ( kind = 8 ) COVARIANCE(B,B), the covariance matrix.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    REAL (KIND=8) COVARIANCE (B, B)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, B
        DO J = 1, B
 
            IF (I == J) THEN
                COVARIANCE (I, J) = REAL (A, KIND=8) * C (I) * (1.0D+00-C(I))
            ELSE
                COVARIANCE (I, J) = - REAL (A, KIND=8) * C (I) * C (J)
            END IF
 
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! MULTINOMIAL_MEAN returns the means of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    SUM ( 1 <= I <= B) C(I) = 1.0.
!
!    Output, real ( kind = 8 ) MEAN(B), MEAN(I) is the expected value of the
!    number of outcome I in N trials.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    REAL (KIND=8) MEAN (B)
 
    MEAN (1:B) = REAL (A, KIND=8) * C (1:B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! MULTINOMIAL_PDF computes a Multinomial PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Comb(A,B,X) * Product ( 1 <= I <= B ) C(I)^X(I)
!
!    where Comb(A,B,X) is the multinomial coefficient
!      C( A; X(1), X(2), ..., X(B) )
!
!    PDF(A,B,C;X) is the probability that in A trials there
!    will be exactly X(I) occurrences of event I, whose probability
!    on one trial is C(I), for I from 1 to B.
!
!    As soon as A or B gets large, the number of possible X's explodes,
!    and the probability of any particular X can become extremely small.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X(B); X(I) counts the number of occurrences of
!    outcome I, out of the total of A trials.
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of different possible outcomes on
!    one trial.
!
!    Input, real ( kind = 8 ) C(B); C(I) is the probability of outcome I on
!    any one trial.
!
!    Output, real ( kind = 8 ) PDF, the value of the multinomial PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    INTEGER (KIND=4) I
    REAL (KIND=8) PDF
    REAL (KIND=8) PDF_LOG
    INTEGER (KIND=4) X (B)
!
!  To try to avoid overflow, do the calculation in terms of logarithms.
!  Note that Gamma(A+1) = A factorial.
!
    PDF_LOG = LOG_GAMMA (REAL(A+1, KIND=8))
 
    DO I = 1, B
        PDF_LOG = PDF_LOG + X (I) * LOG (C(I)) - LOG_GAMMA (REAL(X(I)+1, KIND=8))
    END DO
 
    PDF = EXP (PDF_LOG)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! MULTINOMIAL_SAMPLE samples the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer-Verlag, New York, 1986, page 559.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the total number of trials.
!    0 <= A.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on
!    one trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    sum ( 1 <= I <= B) C(I) = 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X(B); X(I) is the number of
!    occurrences of event I during the N trials.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    INTEGER (KIND=4) IFACTOR
    INTEGER (KIND=4) NTOT
    REAL (KIND=8) PROB
    INTEGER (KIND=4) SEED
    REAL (KIND=8) SUM2
    INTEGER (KIND=4) X (B)
 
    NTOT = A
 
    SUM2 = 1.0D+00
 
    X (1:B) = 0
 
    DO IFACTOR = 1, B - 1
 
        PROB = C (IFACTOR) / SUM2
!
!  Generate a binomial random deviate for NTOT trials with
!  single trial success probability PROB.
!
        CALL BINOMIAL_SAMPLE (NTOT, PROB, SEED, X(IFACTOR))
 
        NTOT = NTOT - X (IFACTOR)
        IF (NTOT <= 0) THEN
            RETURN
        END IF
 
        SUM2 = SUM2 - C (IFACTOR)
 
    END DO
!
!  The last factor gets what's left.
!
    X (B) = NTOT
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOMIAL_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! MULTINOMIAL_VARIANCE returns the variances of the Multinomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of trials.
!
!    Input, integer ( kind = 4 ) B, the number of outcomes possible on one
!    trial.  1 <= B.
!
!    Input, real ( kind = 8 ) C(B).  C(I) is the probability of outcome I on
!    any trial.
!    0.0 <= C(I) <= 1.0D+00,
!    sum ( 1 <= I <= B ) C(I) = 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE(B), VARIANCE(I) is the variance of the
!    total number of events of type I.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
 
    INTEGER (KIND=4) A
    REAL (KIND=8) C (B)
    INTEGER (KIND=4) I
    REAL (KIND=8) VARIANCE (B)
 
    DO I = 1, B
        VARIANCE (I) = REAL (A, KIND=8) * C (I) * (1.0D+00-C(I))
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTINOULLI_PDF (X, N, THETA, PDF)
 
!*****************************************************************************80
!
!! MULTINOULLI_PDF evaluates the Multinoulli PDF.
!
!  Discussion:
!
!    PDF(X) = THETA(X) for 1 <= X <= N.
!           = 0 otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the index of the outcome.
!    1 <= X <= N.
!
!    Input, integer ( kind = 4 ) N, the number of legal outcomes.
!
!    Input, real ( kind = 8 ) THETA(N), the probability of each outcome.
!
!    Output, real ( kind = 8 ) PDF, the probability of outcome X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) PDF
    REAL (KIND=8) THETA (N)
    INTEGER (KIND=4) X
 
    IF (1 <= X .AND. X <= N) THEN
        PDF = THETA (X)
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MULTIVARIATE_NORMAL_SAMPLE (N, MEAN, COVAR_FACTOR, SEED, X)
 
!*****************************************************************************80
!
!! MULTIVARIATE_NORMAL_SAMPLE samples the Multivariate Normal PDF.
!
!  Discussion:
!
!    PDF ( Mean(1:N), S(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( S )
!      * exp ( - ( X - Mean )' * inverse ( S ) * ( X - Mean ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      Mean is the mean vector of length N,
!      S is an N by N positive definite symmetric covariance matrix.
!
!    The properties of S guarantee that it has a lower triangular
!    matrix L, the Cholesky factor, such that S = L * L'.  It is the
!    matrix L, rather than S, that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 167.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MEAN(N), the mean vector.
!
!    Input, real ( kind = 8 ) COVAR_FACTOR(N,N), the lower triangular Cholesky
!    factor L of the covariance matrix S.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample point of the distribution.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) COVAR_FACTOR (N, N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) MEAN (N)
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (N)
    REAL (KIND=8) Z
 
    DO I = 1, N
 
        CALL NORMAL_01_SAMPLE (SEED, Z)
 
        X (I) = MEAN (I)
 
        DO J = 1, I
            X (I) = X (I) + COVAR_FACTOR (I, J) * Z
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NAKAGAMI_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! NAKAGAMI_CDF evaluates the Nakagami CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) P2
    REAL (KIND=8) X
    REAL (KIND=8) X2
    REAL (KIND=8) Y
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (0.0D+00 < X) THEN
 
        Y = (X-A) / B
        X2 = C * Y * Y
        P2 = C
 
        CDF = R8_GAMMA_INC (P2, X2)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NAKAGAMI_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! NAKAGAMI_CDF_INV inverts the Nakagami CDF.
!
!  Discussion:
!
!    A simple bisection method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B.
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: R8_HUGE = 1.0D+30
    REAL (KIND=8), PARAMETER :: TOL = 0.000001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NAKAGAMI_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = C * A * A
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = R8_HUGE
        RETURN
    END IF
 
    X1 = A
    CDF1 = 0.0D+00
 
    X2 = A + 1.0
 
    DO
 
        CALL NAKAGAMI_CDF (X2, A, B, C, CDF2)
 
        IF (CDF < CDF2) THEN
            EXIT
        END IF
 
        X2 = A + 2.0D+00 * (X2-A)
 
    END DO
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL NAKAGAMI_CDF (X3, A, B, C, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'NAKAGAMI_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION NAKAGAMI_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! NAKAGAMI_CHECK checks the parameters of the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical NAKAGAMI_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL NAKAGAMI_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NAKAGAMI_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        NAKAGAMI_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NAKAGAMI_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C <= 0.'
        NAKAGAMI_CHECK = .FALSE.
        RETURN
    END IF
 
    NAKAGAMI_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NAKAGAMI_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! NAKAGAMI_MEAN returns the mean of the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A + B * GAMMA (C+0.5D+00) / (SQRT(C)*GAMMA(C))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NAKAGAMI_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! NAKAGAMI_PDF evaluates the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= 0.0D+00) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (0.0D+00 < X) THEN
 
        Y = (X-A) / B
 
        PDF = 2.0D+00 * C ** C / (B*GAMMA(C)) * Y ** (2.0D+00*C-1.0D+00) * EXP (-C*Y*Y)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NAKAGAMI_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! NAKAGAMI_VARIANCE returns the variance of the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) T1
    REAL (KIND=8) T2
    REAL (KIND=8) VARIANCE
 
    T1 = GAMMA (C+0.5D+00)
    T2 = GAMMA (C)
 
    VARIANCE = B * B * (1.0D+00-T1*T1/(C*T2*T2))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF evaluates the Negative Binomial CDF.
!
!  Discussion:
!
!    A simple summing approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) CNK
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) Y
 
    CDF = 0.0D+00
 
    DO Y = A, X
 
        CNK = I4_CHOOSE (Y-1, A-1)
 
        PDF = REAL (CNK, KIND=8) * B ** A * (1.0D+00-B) ** (Y-A)
 
        CDF = CDF + PDF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_INV inverts the Negative Binomial CDF.
!
!  Discussion:
!
!    A simple discrete approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, integer ( kind = 4 ) X, the smallest X whose cumulative density
!    function is greater than or equal to CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CUM
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4), PARAMETER :: X_MAX = 1000
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NEGATIVE_BINOMIAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
 
    CUM = 0.0D+00
 
    X = A
 
    DO
 
        CALL NEGATIVE_BINOMIAL_PDF (X, A, B, PDF)
 
        CUM = CUM + PDF
 
        IF (CDF <= CUM .OR. X_MAX <= X) THEN
            EXIT
        END IF
 
        X = X + 1
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_CDF_VALUES (N_DATA, F, S, P, CDF)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF_VALUES returns values of the negative binomial CDF.
!
!  Discussion:
!
!    Assume that a coin has a probability P of coming up heads on
!    any one trial.  Suppose that we plan to flip the coin until we
!    achieve a total of S heads.  If we let F represent the number of
!    tails that occur in this process, then the value of F satisfies
!    a negative binomial PDF:
!
!      PDF(F,S,P) = Choose ( F from F+S-1 ) * P^S * (1-P)^F
!
!    The negative binomial CDF is the probability that there are F or
!    fewer failures upon the attainment of the S-th success.  Thus,
!
!      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = NegativeBinomialDistribution [ s, p ]
!      CDF [ dist, f ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    FC Powell,
!    Statistical Tables for Sociology, Biology and Physical Sciences,
!    Cambridge University Press, 1982.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) F, the maximum number of failures.
!
!    Output, integer ( kind = 4 ) S, the number of successes.
!
!    Output, real ( kind = 8 ) P, the probability of a success on one trial.
!
!    Output, real ( kind = 8 ) CDF, the probability of at most F failures
!    before the S-th success.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 27
 
    REAL (KIND=8) CDF
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: CDF_VEC = (/ 0.6367187500000000D+00, &
   & 0.3632812500000000D+00, 0.1445312500000000D+00, 0.5000000000000000D+00, &
   & 0.2265625000000000D+00, 0.6250000000000000D-01, 0.3437500000000000D+00, &
   & 0.1093750000000000D+00, 0.1562500000000000D-01, 0.1792000000000000D+00, &
   & 0.4096000000000000D-01, 0.4096000000000000D-02, 0.7047000000000000D-01, &
   & 0.1093500000000000D-01, 0.7290000000000000D-03, 0.9861587127990000D+00, &
   & 0.9149749500510000D+00, 0.7471846521450000D+00, 0.8499053647030009D+00, &
   & 0.5497160941090026D+00, 0.2662040052146710D+00, 0.6513215599000000D+00, &
   & 0.2639010709000000D+00, 0.7019082640000000D-01, 0.1000000000000000D+01, &
   & 0.1990000000000000D-01, 0.1000000000000000D-03 /)
    INTEGER (KIND=4) F
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: F_VEC = (/ 4, 3, 2, 3, 2, 1, 2, 1, 0, 2, 1, 0, &
   & 2, 1, 0, 11, 10, 9, 17, 16, 15, 9, 8, 7, 2, 1, 0 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) P
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: P_VEC = (/ 0.50D+00, 0.50D+00, 0.50D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.40D+00, 0.40D+00, 0.40D+00, &
   & 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.10D+00, 0.10D+00, 0.10D+00, &
   & 0.10D+00, 0.10D+00, 0.10D+00, 0.10D-01, 0.10D-01, 0.10D-01 /)
    INTEGER (KIND=4) S
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: S_VEC = (/ 4, 5, 6, 4, 5, 6, 4, 5, 6, 4, 5, 6, &
   & 4, 5, 6, 1, 2, 3, 1, 2, 3, 1, 2, 3, 0, 1, 2 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        F = 0
        S = 0
        P = 0.0D+00
        CDF = 0.0D+00
    ELSE
        F = F_VEC (N_DATA)
        S = S_VEC (N_DATA)
        P = P_VEC (N_DATA)
        CDF = CDF_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION NEGATIVE_BINOMIAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CHECK checks the parameters of the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, logical NEGATIVE_BINOMIAL_CHECK, is true if the
!    parameters are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    LOGICAL NEGATIVE_BINOMIAL_CHECK
 
    IF (A < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 0.'
        NEGATIVE_BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00 .OR. 1.0D+00 < B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NEGATIVE_BINOMIAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0 or 1 < B.'
        NEGATIVE_BINOMIAL_CHECK = .FALSE.
        RETURN
    END IF
 
    NEGATIVE_BINOMIAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_MEAN returns the mean of the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = REAL (A, KIND=8) / B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_PDF evaluates the Negative Binomial PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = C(X-1,A-1) * B^A * ( 1 - B )^(X-A)
!
!    PDF(A,B;X) is the probability that the A-th success will
!    occur on the X-th trial, given that the probability
!    of a success on a single trial is B.
!
!    The Negative Binomial PDF is also known as the Pascal PDF or
!    the "Polya" PDF.
!
!    NEGATIVE_BINOMIAL_PDF(1,B;X) = GEOMETRIC_PDF(B;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of trials.
!    A <= X.
!
!    Input, integer ( kind = 4 ) A, the number of successes required.
!    0 <= A <= X, normally.
!
!    Input, real ( kind = 8 ) B, the probability of a success on a single trial.
!    0.0 < B <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    INTEGER (KIND=4) CNK
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        CNK = I4_CHOOSE (X-1, A-1)
 
        PDF = REAL (CNK, KIND=8) * B ** A * (1.0D+00-B) ** (X-A)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_SAMPLE samples the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    INTEGER (KIND=4) NUM_SUCCESS
    REAL (KIND=8) R
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    IF (B == 1.0D+00) THEN
        X = A
        RETURN
    ELSE IF (B == 0.0D+00) THEN
        X = I4_HUGE ()
        RETURN
    END IF
 
    X = 0
    NUM_SUCCESS = 0
 
    DO WHILE (NUM_SUCCESS < A)
 
        X = X + 1
        R = R8_UNIFORM_01 (SEED)
 
        IF (R <= B) THEN
            NUM_SUCCESS = NUM_SUCCESS + 1
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NEGATIVE_BINOMIAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_VARIANCE returns the variance of the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = REAL (A, KIND=8) * (1.0D+00-B) / (B*B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_CDF (X, CDF)
 
!*****************************************************************************80
!
!! NORMAL_01_CDF evaluates the Normal 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    AG Adams,
!    Algorithm 39,
!    Areas Under the Normal Curve,
!    Computer Journal,
!    Volume 12, pages 197-198, 1969.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: A1 = 0.398942280444D+00
    REAL (KIND=8), PARAMETER :: A2 = 0.399903438504D+00
    REAL (KIND=8), PARAMETER :: A3 = 5.75885480458D+00
    REAL (KIND=8), PARAMETER :: A4 = 29.8213557808D+00
    REAL (KIND=8), PARAMETER :: A5 = 2.62433121679D+00
    REAL (KIND=8), PARAMETER :: A6 = 48.6959930692D+00
    REAL (KIND=8), PARAMETER :: A7 = 5.92885724438D+00
    REAL (KIND=8), PARAMETER :: B0 = 0.398942280385D+00
    REAL (KIND=8), PARAMETER :: B1 = 3.8052D-08
    REAL (KIND=8), PARAMETER :: B2 = 1.00000615302D+00
    REAL (KIND=8), PARAMETER :: B3 = 3.98064794D-04
    REAL (KIND=8), PARAMETER :: B4 = 1.98615381364D+00
    REAL (KIND=8), PARAMETER :: B5 = 0.151679116635D+00
    REAL (KIND=8), PARAMETER :: B6 = 5.29330324926D+00
    REAL (KIND=8), PARAMETER :: B7 = 4.8385912808D+00
    REAL (KIND=8), PARAMETER :: B8 = 15.1508972451D+00
    REAL (KIND=8), PARAMETER :: B9 = 0.742380924027D+00
    REAL (KIND=8), PARAMETER :: B10 = 30.789933034D+00
    REAL (KIND=8), PARAMETER :: B11 = 3.99019417011D+00
    REAL (KIND=8) CDF
    REAL (KIND=8) Q
    REAL (KIND=8) X
    REAL (KIND=8) Y
!
!  |X| <= 1.28.
!
    IF (ABS(X) <= 1.28D+00) THEN
 
        Y = 0.5D+00 * X * X
 
        Q = 0.5D+00 - ABS (X) * (A1-A2*Y/(Y+A3-A4/(Y+A5+A6/(Y+A7))))
!
!  1.28 < |X| <= 12.7
!
    ELSE IF (ABS(X) <= 12.7D+00) THEN
 
        Y = 0.5D+00 * X * X
 
        Q = EXP (-Y) * B0 / (ABS(X)-B1+B2/(ABS(X)+B3+B4/(ABS(X)-B5+B6/(ABS(X)+B7-B8/(ABS(X)+B9+&
       & B10/(ABS(X)+B11))))))
!
!  12.7 < |X|
!
    ELSE
 
        Q = 0.0D+00
 
    END IF
!
!  Take account of negative X.
!
    IF (X < 0.0D+00) THEN
        CDF = Q
    ELSE
        CDF = 1.0D+00 - Q
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_CDF_INV (P, X)
 
!*****************************************************************************80
!
!! NORMAL_01_CDF_INV inverts the standard normal CDF.
!
!  Discussion:
!
!    The result is accurate to about 1 part in 10^16.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 June 2007
!
!  Author:
!
!    Original FORTRAN77 version by Michael Wichura.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Michael Wichura,
!    Algorithm AS241:
!    The Percentage Points of the Normal Distribution,
!    Applied Statistics,
!    Volume 37, Number 3, pages 477-484, 1988.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.  If P is outside this range, an
!    "infinite" value will be returned.
!
!    Output, real ( kind = 8 ) X, the normal deviate value
!    with the property that the probability of a standard normal deviate being
!    less than or equal to the value is P.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: A = (/ 3.3871328727963666080D+00, &
   & 1.3314166789178437745D+02, 1.9715909503065514427D+03, 1.3731693765509461125D+04, &
   & 4.5921953931549871457D+04, 6.7265770927008700853D+04, 3.3430575583588128105D+04, &
   & 2.5090809287301226727D+03 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: B = (/ 1.0D+00, 4.2313330701600911252D+01, &
   & 6.8718700749205790830D+02, 5.3941960214247511077D+03, 2.1213794301586595867D+04, &
   & 3.9307895800092710610D+04, 2.8729085735721942674D+04, 5.2264952788528545610D+03 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: C = (/ 1.42343711074968357734D+00, &
   & 4.63033784615654529590D+00, 5.76949722146069140550D+00, 3.64784832476320460504D+00, &
   & 1.27045825245236838258D+00, 2.41780725177450611770D-01, 2.27238449892691845833D-02, &
   & 7.74545014278341407640D-04 /)
    REAL (KIND=8), PARAMETER :: CONST1 = 0.180625D+00
    REAL (KIND=8), PARAMETER :: CONST2 = 1.6D+00
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: D = (/ 1.0D+00, 2.05319162663775882187D+00, &
   & 1.67638483018380384940D+00, 6.89767334985100004550D-01, 1.48103976427480074590D-01, &
   & 1.51986665636164571966D-02, 5.47593808499534494600D-04, 1.05075007164441684324D-09 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: E = (/ 6.65790464350110377720D+00, &
   & 5.46378491116411436990D+00, 1.78482653991729133580D+00, 2.96560571828504891230D-01, &
   & 2.65321895265761230930D-02, 1.24266094738807843860D-03, 2.71155556874348757815D-05, &
   & 2.01033439929228813265D-07 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: F = (/ 1.0D+00, 5.99832206555887937690D-01, &
   & 1.36929880922735805310D-01, 1.48753612908506148525D-02, 7.86869131145613259100D-04, &
   & 1.84631831751005468180D-05, 1.42151175831644588870D-07, 2.04426310338993978564D-15 /)
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) R
    REAL (KIND=8), PARAMETER :: SPLIT1 = 0.425D+00
    REAL (KIND=8), PARAMETER :: SPLIT2 = 5.0D+00
    REAL (KIND=8) X
 
    IF (P <= 0.0D+00) THEN
        X = - HUGE (X)
        RETURN
    END IF
 
    IF (1.0D+00 <= P) THEN
        X = HUGE (X)
        RETURN
    END IF
 
    Q = P - 0.5D+00
 
    IF (ABS(Q) <= SPLIT1) THEN
 
        R = CONST1 - Q * Q
        X = Q * R8POLY_VALUE_HORNER (7, A, R) / R8POLY_VALUE_HORNER (7, B, R)
 
    ELSE
 
        IF (Q < 0.0D+00) THEN
            R = P
        ELSE
            R = 1.0D+00 - P
        END IF
 
        IF (R <= 0.0D+00) THEN
 
            X = HUGE (X)
 
        ELSE
 
            R = SQRT (-LOG(R))
 
            IF (R <= SPLIT2) THEN
 
                R = R - CONST2
                X = R8POLY_VALUE_HORNER (7, C, R) / R8POLY_VALUE_HORNER (7, D, R)
 
            ELSE
 
                R = R - SPLIT2
                X = R8POLY_VALUE_HORNER (7, E, R) / R8POLY_VALUE_HORNER (7, F, R)
 
            END IF
 
        END IF
 
        IF (Q < 0.0D+00) THEN
            X = - X
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_CDF_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! NORMAL_01_CDF_VALUES returns some values of the Normal 01 CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ 0, 1 ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 17
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.5398278372770290D+00, 0.5792597094391030D+00, 0.6179114221889526D+00, &
   & 0.6554217416103242D+00, 0.6914624612740131D+00, 0.7257468822499270D+00, &
   & 0.7580363477769270D+00, 0.7881446014166033D+00, 0.8159398746532405D+00, &
   & 0.8413447460685429D+00, 0.9331927987311419D+00, 0.9772498680518208D+00, &
   & 0.9937903346742239D+00, 0.9986501019683699D+00, 0.9997673709209645D+00, &
   & 0.9999683287581669D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.0000000000000000D+00, &
   & 0.1000000000000000D+00, 0.2000000000000000D+00, 0.3000000000000000D+00, &
   & 0.4000000000000000D+00, 0.5000000000000000D+00, 0.6000000000000000D+00, &
   & 0.7000000000000000D+00, 0.8000000000000000D+00, 0.9000000000000000D+00, &
   & 0.1000000000000000D+01, 0.1500000000000000D+01, 0.2000000000000000D+01, &
   & 0.2500000000000000D+01, 0.3000000000000000D+01, 0.3500000000000000D+01, &
   & 0.4000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_MEAN (MEAN)
 
!*****************************************************************************80
!
!! NORMAL_01_MEAN returns the mean of the Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_PDF (X, PDF)
 
!*****************************************************************************80
!
!! NORMAL_01_PDF evaluates the Normal 01 PDF.
!
!  Discussion:
!
!    The Normal 01 PDF is also called the "Standard Normal" PDF, or
!    the Normal PDF with 0 mean and variance 1.
!
!    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    PDF = EXP (-0.5D+00*X*X) / SQRT (2.0D+00*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_SAMPLE (SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_01_SAMPLE samples the standard normal probability distribution.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the standard normal PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R1
    REAL (KIND=8) R2
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    R1 = R8_UNIFORM_01 (SEED)
    R2 = R8_UNIFORM_01 (SEED)
    X = SQRT (-2.0D+00*LOG(R1)) * COS (2.0D+00*R8_PI*R2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_SAMPLES (N, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_01_SAMPLES returns multiple samples of the standard normal PDF.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    The Box-Muller method is used, which is efficient, but
!    generates two values at a time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of samples.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X(N), samples of the standard normal PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) R1 (N)
    REAL (KIND=8) R2 (N)
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (N)
 
    CALL R8VEC_UNIFORM_01 (N, SEED, R1)
    CALL R8VEC_UNIFORM_01 (N, SEED, R2)
    X (1:N) = SQRT (-2.0D+00*LOG(R1(1:N))) * COS (2.0D+00*R8_PI*R2(1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! NORMAL_01_VARIANCE returns the variance of the Normal 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 1.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_VECTOR (N, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_01_VECTOR samples the standard normal probability distribution.
!
!  Discussion:
!
!    The standard normal probability distribution function (PDF) has
!    mean 0 and standard deviation 1.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    Before calling this routine, the user may call RANDOM_SEED
!    in order to set the seed of the random number generator.
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values desired.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
!  Local parameters:
!
!    Local, real R(N+1), is used to store some uniform random values.
!    Its dimension is N+1, but really it is only needed to be the
!    smallest even number greater than or equal to N.
!
!    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
!    X that we need to compute.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) M
    REAL (KIND=8) R (N+1)
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (N)
    INTEGER (KIND=4) X_HI_INDEX
    INTEGER (KIND=4) X_LO_INDEX
!
!  Record the range of X we need to fill in.
!
    X_LO_INDEX = 1
    X_HI_INDEX = N
 
    IF (X_HI_INDEX-X_LO_INDEX+1 == 1) THEN
 
        R (1) = R8_UNIFORM_01 (SEED)
        R (2) = R8_UNIFORM_01 (SEED)
 
        X (X_HI_INDEX) = SQRT (-2.0D+00*LOG(R(1))) * COS (2.0D+00*R8_PI*R(2))
!
!  If we require an even number of values, that's easy.
!
    ELSE IF (MOD(X_HI_INDEX-X_LO_INDEX+1, 2) == 0) THEN
 
        M = (X_HI_INDEX-X_LO_INDEX+1) / 2
 
        CALL R8VEC_UNIFORM_01 (2*M, SEED, R)
 
        X (X_LO_INDEX:X_HI_INDEX-1:2) = SQRT (-2.0D+00*LOG(R(1:2*M-1:2))) * COS &
       & (2.0D+00*R8_PI*R(2:2*M:2))
 
        X (X_LO_INDEX+1:X_HI_INDEX:2) = SQRT (-2.0D+00*LOG(R(1:2*M-1:2))) * SIN &
       & (2.0D+00*R8_PI*R(2:2*M:2))
!
!  If we require an odd number of values, we generate an even number,
!  and handle the last pair specially, storing one in X(N), and
!  saving the other for later.
!
    ELSE
 
        X_HI_INDEX = X_HI_INDEX - 1
 
        M = (X_HI_INDEX-X_LO_INDEX+1) / 2 + 1
 
        CALL R8VEC_UNIFORM_01 (2*M, SEED, R)
 
        X (X_LO_INDEX:X_HI_INDEX-1:2) = SQRT (-2.0D+00*LOG(R(1:2*M-3:2))) * COS &
       & (2.0D+00*R8_PI*R(2:2*M-2:2))
 
        X (X_LO_INDEX+1:X_HI_INDEX:2) = SQRT (-2.0D+00*LOG(R(1:2*M-3:2))) * SIN &
       & (2.0D+00*R8_PI*R(2:2*M-2:2))
 
        X (N) = SQRT (-2.0D+00*LOG(R(2*M-1))) * COS (2.0D+00*R8_PI*R(2*M))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_CDF (X, MU, SIGMA, CDF)
 
!*****************************************************************************80
!
!! NORMAL_CDF evaluates the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-MU) / SIGMA
 
    CALL NORMAL_01_CDF (Y, CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_CDF_INV (CDF, MU, SIGMA, X)
 
!*****************************************************************************80
!
!! NORMAL_CDF_INV inverts the Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NORMAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    CALL NORMAL_01_CDF_INV (CDF, X2)
 
    X = MU + SIGMA * X2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_CDF_VALUES (N_DATA, MU, SIGMA, X, FX)
 
!*****************************************************************************80
!
!! NORMAL_CDF_VALUES returns some values of the Normal CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NormalDistribution [ mu, sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) MU, the mean of the distribution.
!
!    Output, real ( kind = 8 ) SIGMA, the variance of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.9772498680518208D+00, 0.9999683287581669D+00, 0.9999999990134124D+00, &
   & 0.6914624612740131D+00, 0.6305586598182364D+00, 0.5987063256829237D+00, &
   & 0.5792597094391030D+00, 0.6914624612740131D+00, 0.5000000000000000D+00, &
   & 0.3085375387259869D+00, 0.1586552539314571D+00 /)
    REAL (KIND=8) MU
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: MU_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) SIGMA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: SIGMA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        MU = 0.0D+00
        SIGMA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        MU = MU_VEC (N_DATA)
        SIGMA = SIGMA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION NORMAL_CHECK (MU, SIGMA)
 
!*****************************************************************************80
!
!! NORMAL_CHECK checks the parameters of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, logical NORMAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MU
    LOGICAL NORMAL_CHECK
    REAL (KIND=8) SIGMA
 
    IF (SIGMA == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NORMAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  SIGMA == 0.'
        NORMAL_CHECK = .FALSE.
        RETURN
    END IF
 
    NORMAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_MEAN (MU, SIGMA, MEAN)
 
!*****************************************************************************80
!
!! NORMAL_MEAN returns the mean of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
    REAL (KIND=8) MU
    REAL (KIND=8) SIGMA
 
    MEAN = MU
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_PDF (X, MU, SIGMA, PDF)
 
!*****************************************************************************80
!
!! NORMAL_PDF evaluates the Normal PDF.
!
!  Discussion:
!
!    PDF(X;MU,SIGMA)
!      = exp ( - 0.5 * ( ( X - MU ) / SIGMA )^2 ) / sqrt ( 2 * PI * SIGMA^2 )
!
!    The normal PDF is also known as the Gaussian PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MU
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-MU) / SIGMA
 
    PDF = EXP (-0.5D+00*Y*Y) / SQRT (2.0D+00*R8_PI*SIGMA*SIGMA)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_SAMPLE (MU, SIGMA, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_SAMPLE samples the Normal PDF.
!
!  Discussion:
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MU
    INTEGER (KIND=4) SEED
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X
 
    CALL NORMAL_01_SAMPLE (SEED, X)
 
    X = MU + SIGMA * X
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_SAMPLES (N, MU, SIGMA, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_SAMPLES returns multiple samples of the Normal PDF.
!
!  Discussion:
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of samples.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X(N), the samples of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MU
    INTEGER (KIND=4) SEED
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X (N)
 
    CALL NORMAL_01_SAMPLES (N, SEED, X)
 
    X (1:N) = MU + SIGMA * X (1:N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_VARIANCE (MU, SIGMA, VARIANCE)
 
!*****************************************************************************80
!
!! NORMAL_VARIANCE returns the variance of the Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!    SIGMA should not be zero.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MU
    REAL (KIND=8) SIGMA
    REAL (KIND=8) VARIANCE
 
    VARIANCE = SIGMA * SIGMA
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_VECTOR (N, MU, SIGMA, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_VECTOR samples the normal probability distribution.
!
!  Discussion:
!
!    The normal probability distribution function (PDF) has
!    a user-specified mean and standard deviation.
!
!    This routine can generate a vector of values on one call.  It
!    has the feature that it should provide the same results
!    in the same order no matter how we break up the task.
!
!    Before calling this routine, the user may call RANDOM_SEED
!    in order to set the seed of the random number generator.
!
!    The Box-Muller method is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values desired.
!
!    Input, real ( kind = 8 ) MU, SIGMA, the mean and standard deviation.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X(N), a sample of the standard normal PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MU
    INTEGER (KIND=4) SEED
    REAL (KIND=8) SIGMA
    REAL (KIND=8) X (N)
 
    CALL NORMAL_01_VECTOR (N, SEED, X)
 
    X (1:N) = MU + SIGMA * X (1:N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_CDF (X, MU, S, A, B, CDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_CDF evaluates the truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
    CALL NORMAL_01_CDF (XI, XI_CDF)
 
    CDF = (XI_CDF-ALPHA_CDF) / (BETA_CDF-ALPHA_CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_CDF_INV (CDF, MU, S, A, B, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_CDF_INV inverts the truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NORMAL_TRUNCATED_AB_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    XI_CDF = (BETA_CDF-ALPHA_CDF) * CDF + ALPHA_CDF
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_MEAN (MU, S, A, B, MEAN)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_MEAN returns the mean of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviatione of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) ALPHA_PDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) BETA_PDF
    REAL (KIND=8) MEAN
    REAL (KIND=8) MU
    REAL (KIND=8) S
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    CALL NORMAL_01_PDF (ALPHA, ALPHA_PDF)
    CALL NORMAL_01_PDF (BETA, BETA_PDF)
 
    MEAN = MU + S * (ALPHA_PDF-BETA_PDF) / (BETA_CDF-ALPHA_CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_PDF (X, MU, S, A, B, PDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_PDF evaluates the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) PDF
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_PDF
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
    CALL NORMAL_01_PDF (XI, XI_PDF)
 
    PDF = XI_PDF / (BETA_CDF-ALPHA_CDF) / S
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_SAMPLE (MU, S, A, B, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_SAMPLE samples the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    U = R8_UNIFORM_01 (SEED)
    XI_CDF = ALPHA_CDF + U * (BETA_CDF-ALPHA_CDF)
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_AB_VARIANCE (MU, S, A, B, VARIANCE)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_AB_VARIANCE: variance of the truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper truncation limits.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) ALPHA_PDF
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) BETA_PDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) VARIANCE
 
    ALPHA = (A-MU) / S
    BETA = (B-MU) / S
 
    CALL NORMAL_01_PDF (ALPHA, ALPHA_PDF)
    CALL NORMAL_01_PDF (BETA, BETA_PDF)
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    VARIANCE = S * S * (1.0D+00+(ALPHA*ALPHA_PDF-BETA*BETA_PDF)/(BETA_CDF-ALPHA_CDF)-&
   & ((ALPHA_PDF-BETA_PDF)/(BETA_CDF-ALPHA_CDF))**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_CDF (X, MU, S, A, CDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_CDF evaluates the lower truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    ALPHA = (A-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_CDF (XI, XI_CDF)
 
    CDF = (XI_CDF-ALPHA_CDF) / (1.0D+00-ALPHA_CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_CDF_INV (CDF, MU, S, A, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_CDF_INV inverts the lower truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NORMAL_TRUNCATED_A_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    ALPHA = (A-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
 
    XI_CDF = (1.0D+00-ALPHA_CDF) * CDF + ALPHA_CDF
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_MEAN (MU, S, A, MEAN)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_MEAN returns the mean of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviatione of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) ALPHA_PDF
    REAL (KIND=8) MEAN
    REAL (KIND=8) MU
    REAL (KIND=8) S
 
    ALPHA = (A-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
 
    CALL NORMAL_01_PDF (ALPHA, ALPHA_PDF)
 
    MEAN = MU + S * ALPHA_PDF / (1.0D+00-ALPHA_CDF)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_PDF (X, MU, S, A, PDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_PDF evaluates the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) PDF
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_PDF
 
    ALPHA = (A-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
    CALL NORMAL_01_PDF (XI, XI_PDF)
 
    PDF = XI_PDF / (1.0D+00-ALPHA_CDF) / S
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_SAMPLE (MU, S, A, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_SAMPLE samples the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    ALPHA = (A-MU) / S
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
 
    U = R8_UNIFORM_01 (SEED)
    XI_CDF = ALPHA_CDF + U * (1.0D+00-ALPHA_CDF)
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_A_VARIANCE (MU, S, A, VARIANCE)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_A_VARIANCE: variance of the lower truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) A, the lower truncation limit.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) ALPHA_CDF
    REAL (KIND=8) ALPHA_PDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) VARIANCE
 
    ALPHA = (A-MU) / S
 
    CALL NORMAL_01_PDF (ALPHA, ALPHA_PDF)
 
    CALL NORMAL_01_CDF (ALPHA, ALPHA_CDF)
 
    VARIANCE = S * S * &
   & (1.0D+00+(ALPHA*ALPHA_PDF)/(1.0D+00-ALPHA_CDF)-(ALPHA_PDF/(1.0D+00-ALPHA_CDF))**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_CDF (X, MU, S, B, CDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_CDF evaluates the upper truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    BETA = (B-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
    CALL NORMAL_01_CDF (XI, XI_CDF)
 
    CDF = XI_CDF / BETA_CDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_CDF_INV (CDF, MU, S, B, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_CDF_INV inverts the upper truncated Normal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'NORMAL_TRUNCATED_B_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    XI_CDF = BETA_CDF * CDF
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_MEAN (MU, S, B, MEAN)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_MEAN returns the mean of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviatione of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) BETA_PDF
    REAL (KIND=8) MEAN
    REAL (KIND=8) MU
    REAL (KIND=8) S
 
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    CALL NORMAL_01_PDF (BETA, BETA_PDF)
 
    MEAN = MU - S * BETA_PDF / BETA_CDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_PDF (X, MU, S, B, PDF)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_PDF evaluates the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) PDF
    REAL (KIND=8) S
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_PDF
 
    BETA = (B-MU) / S
    XI = (X-MU) / S
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
    CALL NORMAL_01_PDF (XI, XI_PDF)
 
    PDF = XI_PDF / BETA_CDF / S
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_SAMPLE (MU, S, B, SEED, X)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_SAMPLE samples the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) X
    REAL (KIND=8) XI
    REAL (KIND=8) XI_CDF
 
    BETA = (B-MU) / S
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    U = R8_UNIFORM_01 (SEED)
    XI_CDF = U * BETA_CDF
    CALL NORMAL_01_CDF_INV (XI_CDF, XI)
 
    X = MU + S * XI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_TRUNCATED_B_VARIANCE (MU, S, B, VARIANCE)
 
!*****************************************************************************80
!
!! NORMAL_TRUNCATED_B_VARIANCE: variance of the upper truncated Normal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) MU, S, the mean and standard deviation of the
!    parent Normal distribution.
!
!    Input, real ( kind = 8 ) B, the upper truncation limit.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_CDF
    REAL (KIND=8) BETA_PDF
    REAL (KIND=8) MU
    REAL (KIND=8) S
    REAL (KIND=8) VARIANCE
 
    BETA = (B-MU) / S
 
    CALL NORMAL_01_PDF (BETA, BETA_PDF)
 
    CALL NORMAL_01_CDF (BETA, BETA_CDF)
 
    VARIANCE = S * S * (1.0D+00-(BETA*BETA_PDF)/BETA_CDF-(BETA_PDF/BETA_CDF)**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE OWEN_VALUES (N_DATA, H, A, T)
 
!*****************************************************************************80
!
!! OWEN_VALUES returns some values of Owen's T function.
!
!  Discussion:
!
!    Owen's T function is useful for computation of the bivariate normal
!    distribution and the distribution of a skewed normal distribution.
!
!    Although it was originally formulated in terms of the bivariate
!    normal function, the function can be defined more directly as
!
!      T(H,A) = 1 / ( 2 * pi ) *
!        Integral ( 0 <= X <= A ) e^(-H^2*(1+X^2)/2) / (1+X^2) dX
!
!    In Mathematica, the function can be evaluated by:
!
!      fx = 1/(2*Pi) * Integrate [ E^(-h^2*(1+x^2)/2)/(1+x^2), {x,0,a} ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) H, a parameter.
!
!    Output, real ( kind = 8 ) A, the upper limit of the integral.
!
!    Output, real ( kind = 8 ) T, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 22
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.5000000000000000D+00, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.5000000000000000D+00, 0.1000000000000000D+01, 0.2000000000000000D+01, &
   & 0.3000000000000000D+01, 0.5000000000000000D+00, 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.5000000000000000D+00, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.5000000000000000D+00, 0.1000000000000000D+01, 0.2000000000000000D+01, &
   & 0.3000000000000000D+01, 0.1000000000000000D+02, 0.1000000000000000D+03 /)
    REAL (KIND=8) H
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: H_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.2500000000000000D+00, 0.2500000000000000D+00, &
   & 0.2500000000000000D+00, 0.2500000000000000D+00, 0.1250000000000000D+00, &
   & 0.1250000000000000D+00, 0.1250000000000000D+00, 0.1250000000000000D+00, &
   & 0.7812500000000000D-02, 0.7812500000000000D-02, 0.7812500000000000D-02, &
   & 0.7812500000000000D-02, 0.7812500000000000D-02, 0.7812500000000000D-02 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) T
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: T_VEC = (/ 0.4306469112078537D-01, &
   & 0.6674188216570097D-01, 0.7846818699308410D-01, 0.7929950474887259D-01, &
   & 0.6448860284750376D-01, 0.1066710629614485D+00, 0.1415806036539784D+00, &
   & 0.1510840430760184D+00, 0.7134663382271778D-01, 0.1201285306350883D+00, &
   & 0.1666128410939293D+00, 0.1847501847929859D+00, 0.7317273327500385D-01, &
   & 0.1237630544953746D+00, 0.1737438887583106D+00, 0.1951190307092811D+00, &
   & 0.7378938035365546D-01, 0.1249951430754052D+00, 0.1761984774738108D+00, &
   & 0.1987772386442824D+00, 0.2340886964802671D+00, 0.2479460829231492D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        H = 0.0D+00
        A = 0.0D+00
        T = 0.0D+00
    ELSE
        H = H_VEC (N_DATA)
        A = A_VEC (N_DATA)
        T = T_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! PARETO_CDF evaluates the Pareto CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X < A) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - (A/X) ** B
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! PARETO_CDF_INV inverts the Pareto CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PARETO_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A / (1.0D+00-CDF) ** (1.0D+00/B)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PARETO_CHECK (A, B)
 
!*****************************************************************************80
!
!! PARETO_CHECK checks the parameters of the Pareto CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical PARETO_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL PARETO_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PARETO_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        PARETO_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PARETO_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        PARETO_CHECK = .FALSE.
        RETURN
    END IF
 
    PARETO_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! PARETO_MEAN returns the mean of the Pareto PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    IF (B <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PARETO_MEAN - Fatal error!'
        WRITE (*, '(a)') '  For B <= 1, the mean does not exist.'
        MEAN = 0.0D+00
        RETURN
    END IF
 
    MEAN = B * A / (B-1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! PARETO_PDF evaluates the Pareto PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B * A^B / X^(B+1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < A) THEN
        PDF = 0.0D+00
    ELSE
        PDF = B * (A**B) / X ** (B+1.0D+00)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! PARETO_SAMPLE samples the Pareto PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL PARETO_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARETO_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! PARETO_VARIANCE returns the variance of the Pareto PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    IF (B <= 2.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PARETO_VARIANCE - Warning!'
        WRITE (*, '(a)') '  For B <= 2, the variance does not exist.'
        VARIANCE = 0.0D+00
        RETURN
    END IF
 
    VARIANCE = A * A * B / ((B-1.0D+00)**2*(B-2.0D+00))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PEARSON_05_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! PEARSON_05_CHECK checks the parameters of the Pearson 5 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, logical PEARSON_05_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL PEARSON_05_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PEARSON_05_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        PEARSON_05_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PEARSON_05_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        PEARSON_05_CHECK = .FALSE.
        RETURN
    END IF
 
    PEARSON_05_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PEARSON_05_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! PEARSON_05_MEAN evaluates the mean of the Pearson 5 PDF.
!
!  Discussion:
!
!    The mean is undefined for B <= 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    IF (B <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PEARSON_05_MEAN - Warning!'
        WRITE (*, '(a)') '  MEAN undefined for B <= 1.'
        MEAN = C
        RETURN
    END IF
 
    MEAN = C + A / (B-1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PEARSON_05_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! PEARSON_05_PDF evaluates the Pearson 5 PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = A^B * ( X - C )^(-B-1)
!      * exp ( - A / ( X - C ) ) / Gamma ( B )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    C < X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X <= C) THEN
        PDF = 0.0D+00
    ELSE
        PDF = (A**B) * (X-C) ** (-B-1.0D+00) * EXP (-A/(X-C)) / GAMMA (B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PEARSON_05_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! PEARSON_05_SAMPLE samples the Pearson 5 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    REAL (KIND=8) C
    REAL (KIND=8) C2
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    A2 = 0.0D+00
    B2 = B
    C2 = 1.0D+00 / A
 
    CALL GAMMA_SAMPLE (A2, B2, C2, SEED, X2)
 
    X = C + 1.0D+00 / X2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PLANCK_CHECK (A, B)
 
!*****************************************************************************80
!
!! PLANCK_CHECK checks the parameters of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical PLANCK_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL PLANCK_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PLANCK_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        PLANCK_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PLANCK_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        PLANCK_CHECK = .FALSE.
        RETURN
    END IF
 
    PLANCK_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PLANCK_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! PLANCK_MEAN returns the mean of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = (B+1.0D+00) * R8_ZETA (B+2.0D+00) / R8_ZETA (B+1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PLANCK_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! PLANCK_PDF evaluates the Planck PDF.
!
!  Discussion:
!
!    The Planck PDF has the form
!
!      PDF(A,B;X) = A^(B+1) * X^B / ( exp ( A * X ) - 1 ) / K
!
!    where K is the normalization constant, and has the value
!
!      K = Gamma ( B + 1 ) * Zeta ( B + 1 ).
!
!    The original Planck distribution governed the frequencies in
!    blackbody radiation at a given temperature T, and has the form
!
!      PDF(A;X) = K * X^3 / ( exp ( A * X ) - 1 )
!
!    with
!
!      K = 15 / PI^4.
!
!    Thus, in terms of the Planck PDF, the original Planck distribution
!    has A = 1, B = 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Norman Johnson, Samuel Kotz, Balakrishnan,
!    Continuous Univariate Distributions, second edition,
!    Wiley, 1994,
!    QA273.6.J6
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) K
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        K = GAMMA (B+1.0D+00) * R8_ZETA (B+1.0D+00)
        PDF = A ** (B+1.0D+00) * X ** B / (EXP(A*X)-1.0D+00) / K
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PLANCK_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! PLANCK_SAMPLE samples the Planck PDF.
!
!  Discussion:
!
!    The Planck sampling seems to be giving incorrect results.
!    I suspect this has to do with a possible problem in the
!    ZIPF_SAMPLE routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer Verlag, 1986, pages 552.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    REAL (KIND=8) C2
    REAL (KIND=8) G
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    INTEGER (KIND=4) Z
 
    A2 = 0.0D+00
    B2 = 1.0D+00
    C2 = B + 1.0D+00
 
    CALL GAMMA_SAMPLE (A2, B2, C2, SEED, G)
 
    CALL ZIPF_SAMPLE (C2, SEED, Z)
 
    X = G / (A*REAL(Z, KIND=8))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PLANCK_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! PLANCK_VARIANCE returns the variance of the Planck PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    CALL PLANCK_MEAN (A, B, MEAN)
 
    VARIANCE = (B+1.0D+00) * (B+2.0D+00) * R8_ZETA (B+3.0D+00) / R8_ZETA (B+1.0D+00) - MEAN * &
   & MEAN
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POINT_DISTANCE_1D_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! POINT_DISTANCE_1D_PDF evaluates the point distance PDF in 1D.
!
!  Discussion:
!
!    It is assumed that a set of points has been generated in 1D
!    according to a Poisson process.  The number of points in a region
!    of size LENGTH is a Poisson variate with mean value B * LENGTH.
!
!    For a point chosen at random, we may now find the nearest
!    Poisson point, the second nearest and so on.  We are interested
!    in the PDF that governs the expected behavior of the distances
!    of rank A = 1, 2, 3, ... with Poisson density B.
!
!    Note that this PDF is a form of the Gamma PDF.???
!
!    PDF(A,B;X) = B^A * X^( A - 1 ) * exp ( - B * X ) / ( A - 1 )!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X.
!
!    Input, integer ( kind = 4 ) A, indicates the degree of nearness of the
!    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
!    0 < A.
!
!    Input, real ( kind = 8 ) B, the point density.  0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (A < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_1D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter A < 1.'
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_1D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0.'
        RETURN
    END IF
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = B ** A * X ** (A-1) * EXP (-B*X) / R8_FACTORIAL (A-1)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POINT_DISTANCE_2D_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! POINT_DISTANCE_2D_PDF evaluates the point distance PDF in 2D.
!
!  Discussion:
!
!    It is assumed that a set of points has been generated in 2D
!    according to a Poisson process.  The number of points in a region
!    of size AREA is a Poisson variate with mean value B * AREA.
!
!    For a point chosen at random, we may now find the nearest
!    Poisson point, the second nearest and so on.  We are interested
!    in the PDF that governs the expected behavior of the distances
!    of rank A = 1, 2, 3, ... with Poisson density B.
!
!    PDF(A,B;X) = 2 * ( B * PI )^A * X^( 2 * A - 1 )
!      * EXP ( - B * PI * X * X ) / ( A - 1 )!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 579.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X.
!
!    Input, integer ( kind = 4 ) A, indicates the degree of nearness of the
!    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
!    0 < A.
!
!    Input, real ( kind = 8 ) B, the point density.  0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (A < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_2D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter A < 1.'
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_2D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0.'
        RETURN
    END IF
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 2.0D+00 * (B*R8_PI) ** A * X ** (2*A-1) * EXP (-B*R8_PI*X*X) / R8_FACTORIAL (A-1)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POINT_DISTANCE_3D_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! POINT_DISTANCE_3D_PDF evaluates the point distance PDF in the 3D.
!
!  Discussion:
!
!    It is assumed that a set of points has been generated in 3D
!    according to a Poisson process.  The number of points in a region
!    of size VOLUME is a Poisson variate with mean value B * VOLUME.
!
!    For a point chosen at random, we may now find the nearest
!    Poisson point, the second nearest and so on.  We are interested
!    in the PDF that governs the expected behavior of the distances
!    of rank A = 1, 2, 3, ... with Poisson density B.
!
!    PDF(A,B;X) = 3 * ( (4/3) * B * PI )^A * X^( 3 * A - 1 )
!      * EXP ( - (4/3) * B * PI * X * X * X ) / ( A - 1 )!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 580.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X.
!
!    Input, integer ( kind = 4 ) A, indicates the degree of nearness of the
!    point.  A = 1 means the nearest point, A = 2 the second nearest, and so on.
!    0 < A.
!
!    Input, real ( kind = 8 ) B, the Poisson point density.  0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (A < 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_3D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter A < 1.'
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POINT_DISTANCE_3D_PDF - Fatal error!'
        WRITE (*, '(a)') '  Input parameter B <= 0.0.'
        RETURN
    END IF
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 3.0D+00 * ((4.0D+00/3.0D+00)*B*R8_PI) ** A * X ** (3*A-1) * EXP &
       & (-(4.0D+00/3.0D+00)*B*R8_PI*X**3) / R8_FACTORIAL (A-1)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! POISSON_CDF evaluates the Poisson CDF.
!
!  Discussion:
!
!    CDF(X,A) is the probability that the number of events observed
!    in a unit time period will be no greater than X, given that the
!    expected number of events in a unit time period is A.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!    0 <= X.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) I
    REAL (KIND=8) LAST
    REAL (KIND=8) NEW
    REAL (KIND=8) SUM2
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        NEW = EXP (-A)
        SUM2 = NEW
 
        DO I = 1, X
            LAST = NEW
            NEW = LAST * A / REAL (I, KIND=8)
            SUM2 = SUM2 + NEW
        END DO
 
        CDF = SUM2
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_CDF_VALUES (N_DATA, A, X, FX)
 
!*****************************************************************************80
!
!! POISSON_CDF_VALUES returns some values of the Poisson CDF.
!
!  Discussion:
!
!    CDF(X)(A) is the probability of at most X successes in unit time,
!    given that the expected mean number of successes is A.
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = PoissonDistribution [ a ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 653-658.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, the parameter of the function.
!
!    Output, integer ( kind = 4 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 21
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.02D+00, 0.10D+00, 0.10D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, 2.00D+00, &
   & 2.00D+00, 2.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, 5.00D+00 &
   & /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.9801986733067553D+00, &
   & 0.9048374180359596D+00, 0.9953211598395555D+00, 0.6065306597126334D+00, &
   & 0.9097959895689501D+00, 0.9856123220330293D+00, 0.3678794411714423D+00, &
   & 0.7357588823428846D+00, 0.9196986029286058D+00, 0.9810118431238462D+00, &
   & 0.1353352832366127D+00, 0.4060058497098381D+00, 0.6766764161830635D+00, &
   & 0.8571234604985470D+00, 0.6737946999085467D-02, 0.4042768199451280D-01, &
   & 0.1246520194830811D+00, 0.2650259152973617D+00, 0.4404932850652124D+00, &
   & 0.6159606548330631D+00, 0.7621834629729387D+00 /)
    INTEGER (KIND=4) N_DATA
    INTEGER (KIND=4) X
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, &
   & 2, 3, 0, 1, 2, 3, 4, 5, 6 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        X = 0
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! POISSON_CDF_INV inverts the Poisson CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, a value of the CDF.
!    0 <= CDF < 1.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) I
    REAL (KIND=8) LAST
    REAL (KIND=8) NEW
    REAL (KIND=8) SUM2
    REAL (KIND=8) SUMOLD
    INTEGER (KIND=4) X
    INTEGER (KIND=4), PARAMETER :: XMAX = 100
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POISSON_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
!
!  Now simply start at X = 0, and find the first value for which
!  CDF(X-1) <= CDF <= CDF(X).
!
    SUM2 = 0.0D+00
 
    DO I = 0, XMAX
 
        SUMOLD = SUM2
 
        IF (I == 0) THEN
            NEW = EXP (-A)
            SUM2 = NEW
        ELSE
            LAST = NEW
            NEW = LAST * A / REAL (I, KIND=8)
            SUM2 = SUM2 + NEW
        END IF
 
        IF (SUMOLD <= CDF .AND. CDF <= SUM2) THEN
            X = I
            RETURN
        END IF
 
    END DO
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') 'POISSON_CDF_INV - Warning!'
    WRITE (*, '(a,i8)') '  Exceeded XMAX = ', XMAX
 
    X = XMAX
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION POISSON_CHECK (A)
 
!*****************************************************************************80
!
!! POISSON_CHECK checks the parameter of the Poisson PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, logical POISSON_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL POISSON_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POISSON_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        POISSON_CHECK = .FALSE.
        RETURN
    END IF
 
    POISSON_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! POISSON_MEAN returns the mean of the Poisson PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_KERNEL (R, N, C, X, Y, P)
 
!*****************************************************************************80
!
!! POISSON_KERNEL evaluates the Poisson kernel.
!
!  Discussion:
!
!    P(X,Y) = ( R^2 - |X-C|^2 ) / ( R * A * |X-Y|^N )
!
!    where the N-dimensional ball has radius R and center C,
!    and A is the area of the unit sphere.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) C(N), the center of the ball.
!
!    Input, real ( kind = 8 ) X(N), a point inside the ball.
!
!    Input, real ( kind = 8 ) Y(N), a point on the surface of the ball.
!
!    Output, real ( kind = 8 ) P, the Poisson kernel function P(X,Y).
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) AREA
    REAL (KIND=8) B
    REAL (KIND=8) C (N)
    REAL (KIND=8) P
    REAL (KIND=8) R
    REAL (KIND=8) T
    REAL (KIND=8) X (N)
    REAL (KIND=8) XC_DIFF_NORM
    REAL (KIND=8) XY_DIFF_NORM
    REAL (KIND=8) Y (N)
 
    XC_DIFF_NORM = R8VEC_DIFF_NORM (N, X, C)
    XY_DIFF_NORM = R8VEC_DIFF_NORM (N, X, Y)
    AREA = SPHERE_UNIT_AREA_ND (N)
 
    T = (R+XC_DIFF_NORM) * (R-XC_DIFF_NORM)
    B = R * AREA * (XY_DIFF_NORM) ** N
    P = T / B
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! POISSON_PDF evaluates the Poisson PDF.
!
!  Discussion:
!
!    PDF(A;X) = EXP ( - A ) * A^X / X!
!
!    PDF(A;X) is the probability that the number of events observed
!    in a unit time period will be X, given the expected number
!    of events in a unit time.
!
!    The parameter A is the expected number of events per unit time.
!
!    The Poisson PDF is a discrete version of the Exponential PDF.
!
!    The time interval between two Poisson events is a random
!    variable with the Exponential PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        PDF = 0.0D+00
    ELSE
        PDF = EXP (-A) * A ** X / R8_FACTORIAL (X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! POISSON_SAMPLE samples the Poisson PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL POISSON_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POISSON_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! POISSON_VARIANCE returns the variance of the Poisson PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) VARIANCE
 
    VARIANCE = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! POWER_CDF evaluates the Power CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B,
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (X <= B) THEN
        CDF = (X/B) ** A
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! POWER_CDF_INV inverts the Power CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) X, the argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POWER_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
    ELSE IF (CDF < 1.0D+00) THEN
        X = B * EXP (LOG(CDF)/A)
    ELSE
        X = B
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION POWER_CHECK (A, B)
 
!*****************************************************************************80
!
!! POWER_CHECK checks the parameter of the Power PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, logical POWER_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL POWER_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POWER_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        POWER_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'POWER_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        POWER_CHECK = .FALSE.
        RETURN
    END IF
 
    POWER_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! POWER_MEAN returns the mean of the Power PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A * B / (A+1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! POWER_PDF evaluates the Power PDF.
!
!  Discussion:
!
!    PDF(A;X) = (A/B) * (X/B)^(A-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, Stephen Kokoska,
!    CRC Standard Probability and Statistics Tables and Formulae,
!    Chapman and Hall/CRC, 2000, pages 152-153.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= B.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00 .OR. B < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = (A/B) * (X/B) ** (A-1.0D+00)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! POWER_SAMPLE samples the Power PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL POWER_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POWER_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! POWER_VARIANCE returns the variance of the Power PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A, 0.0D+00 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B * A / ((A+1.0D+00)**2*(A+2.0D+00))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PSI_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! PSI_VALUES returns some values of the Psi or Digamma function.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      PolyGamma[x]
!
!    or
!
!      PolyGamma[0,x]
!
!    PSI(X) = d ln ( Gamma ( X ) ) / d X = Gamma'(X) / Gamma(X)
!
!    PSI(1) = -Euler's constant.
!
!    PSI(X+1) = PSI(X) + 1 / X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 11
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ - 0.5772156649015329D+00, - &
   & 0.4237549404110768D+00, - 0.2890398965921883D+00, - 0.1691908888667997D+00, - &
   & 0.6138454458511615D-01, 0.3648997397857652D-01, 0.1260474527734763D+00, &
   & 0.2085478748734940D+00, 0.2849914332938615D+00, 0.3561841611640597D+00, &
   & 0.4227843350984671D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, &
   & 1.4D+00, 1.5D+00, 1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, 2.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_CDF evaluates the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the maximum number of trials.
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        CDF = 0.0D+00
    ELSE IF (X == 0) THEN
        CDF = A
    ELSE IF (B == 0.0D+00) THEN
        CDF = 1.0D+00
    ELSE
        CDF = A + (1.0D+00-A) * (1.0D+00-B**X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_CDF_INV inverts the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0D+00
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding value of X.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'QUASIGEOMETRIC_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF < A) THEN
        X = 0
    ELSE IF (B == 0.0D+00) THEN
        X = 1
    ELSE
        X = 1 + INT ((LOG(1.0D+00-CDF)-LOG(1.0D+00-A))/LOG(B))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION QUASIGEOMETRIC_CHECK (A, B)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_CHECK checks the parameters of the Quasigeometric CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, logical QUASIGEOMETRIC_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL QUASIGEOMETRIC_CHECK
 
    IF (A < 0.0D+00 .OR. 1.0D+00 < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'QUASIGEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 0 or 1 < A.'
        QUASIGEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B < 0.0D+00 .OR. 1.0D+00 <= B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'QUASIGEOMETRIC_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < 0 or 1 <= B.'
        QUASIGEOMETRIC_CHECK = .FALSE.
        RETURN
    END IF
 
    QUASIGEOMETRIC_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_MEAN returns the mean of the Quasigeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = (1.0D+00-A) / (1.0D+00-B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_PDF evaluates the Quasigeometric PDF.
!
!  Discussion:
!
!    PDF(A,B;X) =    A                     if 0  = X;
!               = (1-A) * (1-B) * B^(X-1)  if 1 <= X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Darren Glass, Philip Lowry,
!    Quasiquasigeometric Distributions and Extra Inning Baseball Games,
!    Mathematics Magazine,
!    Volume 81, Number 2, April 2008, pages 127-137.
!
!    Paul Nahin,
!    Digital Dice: Computational Solutions to Practical Probability Problems,
!    Princeton University Press, 2008,
!    ISBN13: 978-0-691-12698-2,
!    LC: QA273.25.N34.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the independent variable.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (X == 0) THEN
 
        PDF = A
 
    ELSE IF (B == 0.0D+00) THEN
 
        IF (X == 1) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
 
    ELSE
 
        PDF = (1.0D+00-A) * (1.0D+00-B) * B ** (X-1)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_SAMPLE samples the Quasigeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL QUASIGEOMETRIC_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE QUASIGEOMETRIC_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! QUASIGEOMETRIC_VARIANCE returns the variance of the Quasigeometric PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the probability of 0 successes.
!    0.0 <= A <= 1.0.
!
!    Input, real ( kind = 8 ) B, the depreciation constant.
!    0.0 <= B < 1.0.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (1.0D+00-A) * (A+B) / (1.0D+00-B) / (1.0D+00-B)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_BETA (A, B)
 
!*****************************************************************************80
!
!! R8_BETA returns the value of the Beta function.
!
!  Discussion:
!
!    The Beta function is defined as
!
!      BETA(A,B) = ( GAMMA ( A ) * GAMMA ( B ) ) / GAMMA ( A + B )
!                = Integral ( 0 <= T <= 1 ) T^(A-1) (1-T)^(B-1) dT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    0.0 < A,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) R8_BETA, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) R8_BETA
 
    IF (A <= 0.0D+00 .OR. B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA - Fatal error!'
        WRITE (*, '(a)') '  Both A and B must be greater than 0.'
        RETURN
    END IF
 
    R8_BETA = EXP (LOG_GAMMA(A)+LOG_GAMMA(B)-LOG_GAMMA(A+B))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CEILING (R)
 
!*****************************************************************************80
!
!! R8_CEILING rounds an R8 "up" to the nearest integer.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1
!   -1.0  -1
!   -0.9   0
!    0.0   0
!    5.0   5
!    5.1   6
!    5.9   6
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the real value to be rounded up.
!
!    Output, integer ( kind = 4 ) R8_CEILING, the rounded value.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) R8_CEILING
    REAL (KIND=8) R
    INTEGER (KIND=4) VALUE
 
    VALUE = INT (R)
    IF (REAL(VALUE, KIND=8) < R) THEN
        VALUE = VALUE + 1
    END IF
 
    R8_CEILING = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CSC (THETA)
 
!*****************************************************************************80
!
!! R8_CSC returns the cosecant of X.
!
!  Discussion:
!
!    CSC ( THETA ) = 1.0 / SIN ( THETA )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) THETA, the angle, in radians, whose
!    cosecant is desired.  It must be the case that SIN ( THETA ) is not zero.
!
!    Output, real ( kind = 8 ) R8_CSC, the cosecant of THETA.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R8_CSC
    REAL (KIND=8) THETA
 
    R8_CSC = SIN (THETA)
 
    IF (R8_CSC == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_CSC - Fatal error!'
        WRITE (*, '(a,g14.6)') '  R8_CSC undefined for THETA = ', THETA
        RETURN
    END IF
 
    R8_CSC = 1.0D+00 / R8_CSC
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_ERROR_F (X)
 
!*****************************************************************************80
!
!! R8_ERROR_F evaluates the error function ERF.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) ) * Integral ( 0 <= T <= X ) EXP ( - T^2 ) dT.
!
!    Properties of the function include:
!
!      Limit ( X -> -Infinity ) ERF(X) =          -1.0;
!                               ERF(0) =           0.0;
!                               ERF(0.476936...) = 0.5;
!      Limit ( X -> +Infinity ) ERF(X) =          +1.0.
!
!      0.5D+00 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2006
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    Rational Chebyshev Approximations for the Error Function,
!    Mathematics of Computation,
!    1969, pages 631-638.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the error function.
!
!    Output, real ( kind = 8 ) R8_ERROR_F, the value of the error function.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: A = (/ 3.16112374387056560D+00, &
   & 1.13864154151050156D+02, 3.77485237685302021D+02, 3.20937758913846947D+03, &
   & 1.85777706184603153D-01 /)
    REAL (KIND=8), PARAMETER, DIMENSION (4) :: B = (/ 2.36012909523441209D+01, &
   & 2.44024637934444173D+02, 1.28261652607737228D+03, 2.84423683343917062D+03 /)
    REAL (KIND=8), PARAMETER, DIMENSION (9) :: C = (/ 5.64188496988670089D-01, &
   & 8.88314979438837594D+00, 6.61191906371416295D+01, 2.98635138197400131D+02, &
   & 8.81952221241769090D+02, 1.71204761263407058D+03, 2.05107837782607147D+03, &
   & 1.23033935479799725D+03, 2.15311535474403846D-08 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: D = (/ 1.57449261107098347D+01, &
   & 1.17693950891312499D+02, 5.37181101862009858D+02, 1.62138957456669019D+03, &
   & 3.29079923573345963D+03, 4.36261909014324716D+03, 3.43936767414372164D+03, &
   & 1.23033935480374942D+03 /)
    REAL (KIND=8) DEL
    INTEGER (KIND=4) I
    REAL (KIND=8), PARAMETER, DIMENSION (6) :: P = (/ 3.05326634961232344D-01, &
   & 3.60344899949804439D-01, 1.25781726111229246D-01, 1.60837851487422766D-02, &
   & 6.58749161529837803D-04, 1.63153871373020978D-02 /)
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: Q = (/ 2.56852019228982242D+00, &
   & 1.87295284992346047D+00, 5.27905102951428412D-01, 6.05183413124413191D-02, &
   & 2.33520497626869185D-03 /)
    REAL (KIND=8) R8_ERROR_F
    REAL (KIND=8), PARAMETER :: SQRPI = 0.56418958354775628695D+00
    REAL (KIND=8), PARAMETER :: THRESH = 0.46875D+00
    REAL (KIND=8) X
    REAL (KIND=8) XABS
    REAL (KIND=8), PARAMETER :: XBIG = 26.543D+00
    REAL (KIND=8) XDEN
    REAL (KIND=8) XNUM
    REAL (KIND=8) XSQ
 
    XABS = ABS ((X))
!
!  Evaluate ERF(X) for |X| <= 0.46875.
!
    IF (XABS <= THRESH) THEN
 
        IF (EPSILON(XABS) < XABS) THEN
            XSQ = XABS * XABS
        ELSE
            XSQ = 0.0D+00
        END IF
 
        XNUM = A (5) * XSQ
        XDEN = XSQ
        DO I = 1, 3
            XNUM = (XNUM+A(I)) * XSQ
            XDEN = (XDEN+B(I)) * XSQ
        END DO
 
        R8_ERROR_F = X * (XNUM+A(4)) / (XDEN+B(4))
!
!  Evaluate ERFC(X) for 0.46875 <= |X| <= 4.0.
!
    ELSE IF (XABS <= 4.0D+00) THEN
 
        XNUM = C (9) * XABS
        XDEN = XABS
        DO I = 1, 7
            XNUM = (XNUM+C(I)) * XABS
            XDEN = (XDEN+D(I)) * XABS
        END DO
 
        R8_ERROR_F = (XNUM+C(8)) / (XDEN+D(8))
        XSQ = REAL (INT(XABS*16.0D+00), KIND=8) / 16.0D+00
        DEL = (XABS-XSQ) * (XABS+XSQ)
        R8_ERROR_F = EXP (-XSQ*XSQ) * EXP (-DEL) * R8_ERROR_F
 
        R8_ERROR_F = (0.5D+00-R8_ERROR_F) + 0.5D+00
 
        IF (X < 0.0D+00) THEN
            R8_ERROR_F = - R8_ERROR_F
        END IF
!
!  Evaluate ERFC(X) for 4.0D+00 < |X|.
!
    ELSE
 
        IF (XBIG <= XABS) THEN
 
            IF (0.0D+00 < X) THEN
                R8_ERROR_F = 1.0D+00
            ELSE
                R8_ERROR_F = - 1.0D+00
            END IF
 
        ELSE
 
            XSQ = 1.0D+00 / (XABS*XABS)
 
            XNUM = P (6) * XSQ
            XDEN = XSQ
            DO I = 1, 4
                XNUM = (XNUM+P(I)) * XSQ
                XDEN = (XDEN+Q(I)) * XSQ
            END DO
 
            R8_ERROR_F = XSQ * (XNUM+P(5)) / (XDEN+Q(5))
            R8_ERROR_F = (SQRPI-R8_ERROR_F) / XABS
            XSQ = REAL (INT(XABS*16.0D+00), KIND=8) / 16.0D+00
            DEL = (XABS-XSQ) * (XABS+XSQ)
            R8_ERROR_F = EXP (-XSQ*XSQ) * EXP (-DEL) * R8_ERROR_F
 
            R8_ERROR_F = (0.5D+00-R8_ERROR_F) + 0.5D+00
 
            IF (X < 0.0D+00) THEN
                R8_ERROR_F = - R8_ERROR_F
            END IF
 
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_ERROR_F_INVERSE (Y)
 
!*****************************************************************************80
!
!! R8_ERROR_F_INVERSE inverts the error function ERF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Y, the value of the error function.
!
!    Output, real ( kind = 8 ) R8_ERROR_F_INVERSE, the value X such that
!    ERF(X) = Y.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R8_ERROR_F_INVERSE
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    Z = (Y+1.0D+00) / 2.0D+00
 
    CALL NORMAL_01_CDF_INV (Z, X)
 
    R8_ERROR_F_INVERSE = X / SQRT (2.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_FACTORIAL (N)
 
!*****************************************************************************80
!
!! R8_FACTORIAL computes the factorial of N.
!
!  Discussion:
!
!    factorial ( N ) = product ( 1 <= I <= N ) I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL, the factorial of N.
!
    IMPLICIT NONE
 
    REAL (KIND=8) R8_FACTORIAL
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8) VALUE
 
    VALUE = 1.0D+00
 
    DO I = 1, N
        VALUE = VALUE * REAL (I, KIND=8)
    END DO
 
    R8_FACTORIAL = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_FACTORIAL_LOG (N)
 
!*****************************************************************************80
!
!! R8_FACTORIAL_LOG returns the logarithm of N factorial.
!
!  Discussion:
!
!    N! = Product ( 1 <= I <= N ) I
!
!    N! = Gamma(N+1).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the function.
!    0 <= N.
!
!    Output, real ( kind = 8 ) R8_FACTORIAL_LOG, the logarithm of N!.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8) R8_FACTORIAL_LOG
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_FACTORIAL_LOG - Fatal error!'
        WRITE (*, '(a)') '  N < 0.'
        RETURN
    END IF
 
    R8_FACTORIAL_LOG = 0.0D+00
 
    DO I = 2, N
        R8_FACTORIAL_LOG = R8_FACTORIAL_LOG + LOG (REAL(I, KIND=8))
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA (X)
 
!*****************************************************************************80
!
!! R8_GAMMA evaluates Gamma(X) for a real argument.
!
!  Discussion:
!
!    This routine calculates the gamma function for a real argument X.
!
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the gamma
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for 12 <= X are from reference 2.
!
!  Modified:
!
!    11 February 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    An Overview of Software Development for Special Functions,
!    in Numerical Analysis Dundee, 1975,
!    edited by GA Watson,
!    Lecture Notes in Mathematics 506,
!    Springer, 1976.
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
!
!    Output, real ( kind = 8 ) R8_GAMMA, the value of the function.
!
    IMPLICIT NONE
!
!  Coefficients for minimax approximation over (12, INF).
!
    REAL (KIND=8), DIMENSION (7) :: C = (/ - 1.910444077728D-03, 8.4171387781295D-04, - &
   & 5.952379913043012D-04, 7.93650793500350248D-04, - 2.777777777777681622553D-03, &
   & 8.333333333333333331554247D-02, 5.7083835261D-03 /)
    REAL (KIND=8), PARAMETER :: EPS = 2.22D-16
    REAL (KIND=8) FACT
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8), PARAMETER :: ONE = 1.0D+00
    REAL (KIND=8), DIMENSION (8) :: P = (/ - 1.71618513886549492533811D+00, &
   & 2.47656508055759199108314D+01, - 3.79804256470945635097577D+02, &
   & 6.29331155312818442661052D+02, 8.66966202790413211295064D+02, - &
   & 3.14512729688483675254357D+04, - 3.61444134186911729807069D+04, &
   & 6.64561438202405440627855D+04 /)
    LOGICAL PARITY
    REAL (KIND=8), PARAMETER :: R8_PI = 3.1415926535897932384626434D+00
    REAL (KIND=8), DIMENSION (8) :: Q = (/ - 3.08402300119738975254353D+01, &
   & 3.15350626979604161529144D+02, - 1.01515636749021914166146D+03, - &
   & 3.10777167157231109440444D+03, 2.25381184209801510330112D+04, &
   & 4.75584627752788110767815D+03, - 1.34659959864969306392456D+05, - &
   & 1.15132259675553483497211D+05 /)
    REAL (KIND=8) R8_GAMMA
    REAL (KIND=8) RES
    REAL (KIND=8), PARAMETER :: SQRTPI = 0.9189385332046727417803297D+00
    REAL (KIND=8) SUM
    REAL (KIND=8), PARAMETER :: TWELVE = 12.0D+00
    REAL (KIND=8), PARAMETER :: TWO = 2.0D+00
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XBIG = 171.624D+00
    REAL (KIND=8) XDEN
    REAL (KIND=8), PARAMETER :: XINF = 1.0D+30
    REAL (KIND=8), PARAMETER :: XMININ = 2.23D-308
    REAL (KIND=8) XNUM
    REAL (KIND=8) Y
    REAL (KIND=8) Y1
    REAL (KIND=8) YSQ
    REAL (KIND=8) Z
    REAL (KIND=8), PARAMETER :: ZERO = 0.0D+00
 
    PARITY = .FALSE.
    FACT = ONE
    N = 0
    Y = X
!
!  Argument is negative.
!
    IF (Y <= ZERO) THEN
 
        Y = - X
        Y1 = AINT (Y)
        RES = Y - Y1
 
        IF (RES /= ZERO) THEN
 
            IF (Y1 /= AINT(Y1*HALF)*TWO) THEN
                PARITY = .TRUE.
            END IF
 
            FACT = - R8_PI / SIN (R8_PI*RES)
            Y = Y + ONE
 
        ELSE
 
            RES = XINF
            R8_GAMMA = RES
            RETURN
 
        END IF
 
    END IF
!
!  Argument is positive.
!
    IF (Y < EPS) THEN
!
!  Argument < EPS.
!
        IF (XMININ <= Y) THEN
            RES = ONE / Y
        ELSE
            RES = XINF
            R8_GAMMA = RES
            RETURN
        END IF
 
    ELSE IF (Y < TWELVE) THEN
 
        Y1 = Y
!
!  0.0 < argument < 1.0.
!
        IF (Y < ONE) THEN
 
            Z = Y
            Y = Y + ONE
!
!  1.0 < argument < 12.0.
!  Reduce argument if necessary.
!
        ELSE
 
            N = INT (Y) - 1
            Y = Y - REAL (N, KIND=8)
            Z = Y - ONE
 
        END IF
!
!  Evaluate approximation for 1.0 < argument < 2.0.
!
        XNUM = ZERO
        XDEN = ONE
        DO I = 1, 8
            XNUM = (XNUM+P(I)) * Z
            XDEN = XDEN * Z + Q (I)
        END DO
 
        RES = XNUM / XDEN + ONE
!
!  Adjust result for case  0.0 < argument < 1.0.
!
        IF (Y1 < Y) THEN
 
            RES = RES / Y1
!
!  Adjust result for case 2.0 < argument < 12.0.
!
        ELSE IF (Y < Y1) THEN
 
            DO I = 1, N
                RES = RES * Y
                Y = Y + ONE
            END DO
 
        END IF
 
    ELSE
!
!  Evaluate for 12.0 <= argument.
!
        IF (Y <= XBIG) THEN
 
            YSQ = Y * Y
            SUM = C (7)
            DO I = 1, 6
                SUM = SUM / YSQ + C (I)
            END DO
            SUM = SUM / Y - Y + SQRTPI
            SUM = SUM + (Y-HALF) * LOG (Y)
            RES = EXP (SUM)
 
        ELSE
 
            RES = XINF
            R8_GAMMA = RES
            RETURN
 
        END IF
 
    END IF
!
!  Final adjustments and return.
!
    IF (PARITY) THEN
        RES = - RES
    END IF
 
    IF (FACT /= ONE) THEN
        RES = FACT / RES
    END IF
 
    R8_GAMMA = RES
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_INC (P, X)
 
!*****************************************************************************80
!
!! R8_GAMMA_INC computes the incomplete Gamma function.
!
!  Discussion:
!
!    GAMMA_INC(P,       0) = 0,
!    GAMMA_INC(P,Infinity) = 1.
!
!    GAMMA_INC(P,X) = Integral ( 0 <= T <= X ) T^(P-1) EXP(-T) DT / GAMMA(P).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2001
!
!  Author:
!
!    Original FORTRAN77 version by B L Shea.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BL Shea,
!    Chi-squared and Incomplete Gamma Integral,
!    Algorithm AS239,
!    Applied Statistics,
!    Volume 37, Number 3, 1988, pages 466-473.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the exponent parameter.
!    0.0 < P.
!
!    Input, real ( kind = 8 ) X, the integral limit parameter.
!    If X is less than or equal to 0, the value is returned as 0.
!
!    Output, real ( kind = 8 ) R8_GAMMA_INC, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ARG
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: EXP_ARG_MIN = - 88.0D+00
    REAL (KIND=8), PARAMETER :: OVERFLOW = 1.0D+37
    REAL (KIND=8) P
    REAL (KIND=8), PARAMETER :: PLIMIT = 1000.0D+00
    REAL (KIND=8) PN1
    REAL (KIND=8) PN2
    REAL (KIND=8) PN3
    REAL (KIND=8) PN4
    REAL (KIND=8) PN5
    REAL (KIND=8) PN6
    REAL (KIND=8) R8_GAMMA_INC
    REAL (KIND=8) RN
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-07
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XBIG = 1.0D+08
 
    R8_GAMMA_INC = 0.0D+00
 
    IF (P <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_GAMMA_INC - Fatal error!'
        WRITE (*, '(a)') '  Parameter P <= 0.'
        RETURN
    END IF
 
    IF (X <= 0.0D+00) THEN
        R8_GAMMA_INC = 0.0D+00
        RETURN
    END IF
!
!  Use a normal approximation if PLIMIT < P.
!
    IF (PLIMIT < P) THEN
        PN1 = 3.0D+00 * SQRT (P) * ((X/P)**(1.0D+00/3.0D+00)+1.0D+00/(9.0D+00*P)-1.0D+00)
        CALL NORMAL_01_CDF (PN1, CDF)
        R8_GAMMA_INC = CDF
        RETURN
    END IF
!
!  Is X extremely large compared to P?
!
    IF (XBIG < X) THEN
        R8_GAMMA_INC = 1.0D+00
        RETURN
    END IF
!
!  Use Pearson's series expansion.
!  (P is not large enough to force overflow in the log of Gamma.
!
    IF (X <= 1.0D+00 .OR. X < P) THEN
 
        ARG = P * LOG (X) - X - LOG_GAMMA (P+1.0D+00)
        C = 1.0D+00
        R8_GAMMA_INC = 1.0D+00
        A = P
 
        DO
 
            A = A + 1.0D+00
            C = C * X / A
            R8_GAMMA_INC = R8_GAMMA_INC + C
 
            IF (C <= TOL) THEN
                EXIT
            END IF
 
        END DO
 
        ARG = ARG + LOG (R8_GAMMA_INC)
 
        IF (EXP_ARG_MIN <= ARG) THEN
            R8_GAMMA_INC = EXP (ARG)
        ELSE
            R8_GAMMA_INC = 0.0D+00
        END IF
 
    ELSE
!
!  Use a continued fraction expansion.
!
        ARG = P * LOG (X) - X - LOG_GAMMA (P)
        A = 1.0D+00 - P
        B = A + X + 1.0D+00
        C = 0.0D+00
        PN1 = 1.0D+00
        PN2 = X
        PN3 = X + 1.0D+00
        PN4 = X * B
        R8_GAMMA_INC = PN3 / PN4
 
        DO
 
            A = A + 1.0D+00
            B = B + 2.0D+00
            C = C + 1.0D+00
            PN5 = B * PN3 - A * C * PN1
            PN6 = B * PN4 - A * C * PN2
 
            IF (0.0D+00 < ABS(PN6)) THEN
 
                RN = PN5 / PN6
 
                IF (ABS(R8_GAMMA_INC-RN) <= MIN(TOL, TOL*RN)) THEN
 
                    ARG = ARG + LOG (R8_GAMMA_INC)
 
                    IF (EXP_ARG_MIN <= ARG) THEN
                        R8_GAMMA_INC = 1.0D+00 - EXP (ARG)
                    ELSE
                        R8_GAMMA_INC = 1.0D+00
                    END IF
 
                    RETURN
 
                END IF
 
                R8_GAMMA_INC = RN
 
            END IF
 
            PN1 = PN3
            PN2 = PN4
            PN3 = PN5
            PN4 = PN6
!
!  Rescale terms in continued fraction if terms are large.
!
            IF (OVERFLOW <= ABS(PN5)) THEN
                PN1 = PN1 / OVERFLOW
                PN2 = PN2 / OVERFLOW
                PN3 = PN3 / OVERFLOW
                PN4 = PN4 / OVERFLOW
            END IF
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_LOG (X)
 
!*****************************************************************************80
!
!! R8_GAMMA_LOG calculates the natural logarithm of GAMMA ( X ).
!
!  Discussion:
!
!    Computation is based on an algorithm outlined in references 1 and 2.
!    The program uses rational functions that theoretically approximate
!    LOG(GAMMA(X)) to at least 18 significant decimal digits.  The
!    approximation for 12 < X is from Hart et al, while approximations
!    for X < 12.0D+00 are similar to those in Cody and Hillstrom,
!    but are unpublished.
!
!    The accuracy achieved depends on the arithmetic system, the compiler,
!    intrinsic functions, and proper selection of the machine dependent
!    constants.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Kenneth Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the Gamma Function,
!    Mathematics of Computation,
!    Volume 21, 1967, pages 198-203.
!
!    Kenneth Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
!    Charles Mesztenyi, John Rice, Henry Thacher, Christoph Witzgall,
!    Computer Approximations,
!    Wiley, 1968.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the Gamma function.
!    X must be positive.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG, the logarithm of the Gamma
!    function of X.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) BETA, the radix for the floating-point
!    representation.
!
!    Local, integer MAXEXP, the smallest positive power of BETA that overflows.
!
!    Local, real ( kind = 8 ) XBIG, the largest argument for which
!    LN(GAMMA(X)) is representable in the machine, the solution to the equation
!      LN(GAMMA(XBIG)) = BETA^MAXEXP.
!
!    Local, real ( kind = 8 ) FRTBIG, a rough estimate of the fourth root
!    of XBIG.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER, DIMENSION (7) :: C = (/ - 1.910444077728D-03, &
   & 8.4171387781295D-04, - 5.952379913043012D-04, 7.93650793500350248D-04, - &
   & 2.777777777777681622553D-03, 8.333333333333333331554247D-02, 5.7083835261D-03 /)
    REAL (KIND=8) CORR
    REAL (KIND=8), PARAMETER :: D1 = - 5.772156649015328605195174D-01
    REAL (KIND=8), PARAMETER :: D2 = 4.227843350984671393993777D-01
    REAL (KIND=8), PARAMETER :: D4 = 1.791759469228055000094023D+00
    INTEGER (KIND=4) I
    REAL (KIND=8), PARAMETER :: FRTBIG = 1.42D+09
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: P1 = (/ 4.945235359296727046734888D+00, &
   & 2.018112620856775083915565D+02, 2.290838373831346393026739D+03, &
   & 1.131967205903380828685045D+04, 2.855724635671635335736389D+04, &
   & 3.848496228443793359990269D+04, 2.637748787624195437963534D+04, &
   & 7.225813979700288197698961D+03 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: P2 = (/ 4.974607845568932035012064D+00, &
   & 5.424138599891070494101986D+02, 1.550693864978364947665077D+04, &
   & 1.847932904445632425417223D+05, 1.088204769468828767498470D+06, &
   & 3.338152967987029735917223D+06, 5.106661678927352456275255D+06, &
   & 3.074109054850539556250927D+06 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: P4 = (/ 1.474502166059939948905062D+04, &
   & 2.426813369486704502836312D+06, 1.214755574045093227939592D+08, &
   & 2.663432449630976949898078D+09, 2.940378956634553899906876D+10, &
   & 1.702665737765398868392998D+11, 4.926125793377430887588120D+11, &
   & 5.606251856223951465078242D+11 /)
    REAL (KIND=8), PARAMETER :: PNT68 = 0.6796875D+00
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: Q1 = (/ 6.748212550303777196073036D+01, &
   & 1.113332393857199323513008D+03, 7.738757056935398733233834D+03, &
   & 2.763987074403340708898585D+04, 5.499310206226157329794414D+04, &
   & 6.161122180066002127833352D+04, 3.635127591501940507276287D+04, &
   & 8.785536302431013170870835D+03 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: Q2 = (/ 1.830328399370592604055942D+02, &
   & 7.765049321445005871323047D+03, 1.331903827966074194402448D+05, &
   & 1.136705821321969608938755D+06, 5.267964117437946917577538D+06, &
   & 1.346701454311101692290052D+07, 1.782736530353274213975932D+07, &
   & 9.533095591844353613395747D+06 /)
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: Q4 = (/ 2.690530175870899333379843D+03, &
   & 6.393885654300092398984238D+05, 4.135599930241388052042842D+07, &
   & 1.120872109616147941376570D+09, 1.488613728678813811542398D+10, &
   & 1.016803586272438228077304D+11, 3.417476345507377132798597D+11, &
   & 4.463158187419713286462081D+11 /)
    REAL (KIND=8) R8_GAMMA_LOG
    REAL (KIND=8) RES
    REAL (KIND=8), PARAMETER :: SQRTPI = 0.9189385332046727417803297D+00
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XBIG = 4.08D+36
    REAL (KIND=8) XDEN
    REAL (KIND=8) XM1
    REAL (KIND=8) XM2
    REAL (KIND=8) XM4
    REAL (KIND=8) XNUM
    REAL (KIND=8) XSQ
!
!  Return immediately if the argument is out of range.
!
    IF (X <= 0.0D+00 .OR. XBIG < X) THEN
        R8_GAMMA_LOG = HUGE (R8_GAMMA_LOG)
        RETURN
    END IF
 
    IF (X <= EPSILON(X)) THEN
 
        RES = - LOG (X)
 
    ELSE IF (X <= 1.5D+00) THEN
 
        IF (X < PNT68) THEN
            CORR = - LOG (X)
            XM1 = X
        ELSE
            CORR = 0.0D+00
            XM1 = (X-0.5D+00) - 0.5D+00
        END IF
 
        IF (X <= 0.5D+00 .OR. PNT68 <= X) THEN
 
            XDEN = 1.0D+00
            XNUM = 0.0D+00
 
            DO I = 1, 8
                XNUM = XNUM * XM1 + P1 (I)
                XDEN = XDEN * XM1 + Q1 (I)
            END DO
 
            RES = CORR + (XM1*(D1+XM1*(XNUM/XDEN)))
 
        ELSE
 
            XM2 = (X-0.5D+00) - 0.5D+00
            XDEN = 1.0D+00
            XNUM = 0.0D+00
            DO I = 1, 8
                XNUM = XNUM * XM2 + P2 (I)
                XDEN = XDEN * XM2 + Q2 (I)
            END DO
 
            RES = CORR + XM2 * (D2+XM2*(XNUM/XDEN))
 
        END IF
 
    ELSE IF (X <= 4.0D+00) THEN
 
        XM2 = X - 2.0D+00
        XDEN = 1.0D+00
        XNUM = 0.0D+00
        DO I = 1, 8
            XNUM = XNUM * XM2 + P2 (I)
            XDEN = XDEN * XM2 + Q2 (I)
        END DO
 
        RES = XM2 * (D2+XM2*(XNUM/XDEN))
 
    ELSE IF (X <= 12.0D+00) THEN
 
        XM4 = X - 4.0D+00
        XDEN = - 1.0D+00
        XNUM = 0.0D+00
        DO I = 1, 8
            XNUM = XNUM * XM4 + P4 (I)
            XDEN = XDEN * XM4 + Q4 (I)
        END DO
 
        RES = D4 + XM4 * (XNUM/XDEN)
 
    ELSE
 
        RES = 0.0D+00
 
        IF (X <= FRTBIG) THEN
 
            RES = C (7)
            XSQ = X * X
 
            DO I = 1, 6
                RES = RES / XSQ + C (I)
            END DO
 
        END IF
 
        RES = RES / X
        CORR = LOG (X)
        RES = RES + SQRTPI - 0.5D+00 * CORR
        RES = RES + X * (CORR-1.0D+00)
 
    END IF
 
    R8_GAMMA_LOG = RES
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA_LOG_INT (N)
 
!*****************************************************************************80
!
!! R8_GAMMA_LOG_INT computes the logarithm of Gamma of an integer N.
!
!  Discussion:
!
!    log ( n! ) = r8_gamma_log_int ( n + 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the argument of the logarithm of the
!    Gamma function.  0 < N.
!
!    Output, real ( kind = 8 ) R8_GAMMA_LOG_INT, the logarithm of
!    the Gamma function of N.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
    REAL (KIND=8) R8_GAMMA_LOG_INT
 
    IF (N <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_GAMMA_LOG_INT - Fatal error!'
        WRITE (*, '(a,i12)') '  Illegal input value of N = ', N
        WRITE (*, '(a)') '  But N must be strictly positive.'
        RETURN
    END IF
 
    R8_GAMMA_LOG_INT = LOG_GAMMA (REAL(N, KIND=8))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_IS_INT (R)
 
!*****************************************************************************80
!
!! R8_IS_INT determines if an R8 represents an integer value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the number to be checked.
!
!    Output, logical R8_IS_INT, is TRUE if R is an integer value.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    REAL (KIND=8) R
    LOGICAL R8_IS_INT
 
    IF (REAL(I4_HUGE(), KIND=8) < R) THEN
        R8_IS_INT = .FALSE.
    ELSE IF (R <-REAL(I4_HUGE(), KIND=8)) THEN
        R8_IS_INT = .FALSE.
    ELSE IF (R == REAL(INT(R), KIND=8)) THEN
        R8_IS_INT = .TRUE.
    ELSE
        R8_IS_INT = .FALSE.
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_PI ()
 
!*****************************************************************************80
!
!! R8_PI returns the value of pi to 16 decimal places.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision R8_PI, the value of pi.
!
    IMPLICIT NONE
 
    DOUBLE PRECISION R8_PI
 
    R8_PI = 3.141592653589793D+00
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_UNIFORM_01 (SEED)
 
!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2^31 - 1 )
!      r8_uniform_01 = seed / ( 2^31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R8_UNIFORM_01
!      SEED      SEED
!
!         12345   207482415  0.096616
!     207482415  1790989824  0.833995
!    1790989824  2035175616  0.947702
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley Interscience, page 95, 1998.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0. On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) K
    REAL (KIND=8) R8_UNIFORM_01
    INTEGER (KIND=4) SEED
 
    IF (SEED == 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_UNIFORM_01 - Fatal error!'
        WRITE (*, '(a)') '  Input value of SEED = 0.'
        RETURN
    END IF
 
    K = SEED / 127773
 
    SEED = 16807 * (SEED-K*127773) - K * 2836
 
    IF (SEED < 0) THEN
        SEED = SEED + I4_HUGE ()
    END IF
!
!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
!
    R8_UNIFORM_01 = REAL (SEED, KIND=8) * 4.656612875D-10
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_ZETA (P)
 
!*****************************************************************************80
!
!! R8_ZETA estimates the Riemann Zeta function.
!
!  Discussion:
!
!    For 1 < P, the Riemann Zeta function is defined as:
!
!      ZETA ( P ) = Sum ( 1 <= N < Infinity ) 1 / N^P
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the power to which the integers are raised.
!    P must be greater than 1.  For integral P up to 20, a
!    precomputed value of ZETA is returned; otherwise the infinite
!    sum is approximated.
!
!    Output, real ( kind = 8 ) R8_ZETA, an approximation to the Riemann
!    Zeta function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
    REAL (KIND=8) P
    REAL (KIND=8), PARAMETER :: R8_HUGE = 1.0D+30
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) R8_ZETA
    REAL (KIND=8) ZSUM
    REAL (KIND=8) ZSUM_OLD
 
    IF (P <= 1.0D+00) THEN
        R8_ZETA = R8_HUGE
    ELSE IF (P == 2.0D+00) THEN
        R8_ZETA = R8_PI ** 2 / 6.0D+00
    ELSE IF (P == 3.0D+00) THEN
        R8_ZETA = 1.2020569032D+00
    ELSE IF (P == 4.0D+00) THEN
        R8_ZETA = R8_PI ** 4 / 90.0D+00
    ELSE IF (P == 5.0D+00) THEN
        R8_ZETA = 1.0369277551D+00
    ELSE IF (P == 6.0D+00) THEN
        R8_ZETA = R8_PI ** 6 / 945.0D+00
    ELSE IF (P == 7.0D+00) THEN
        R8_ZETA = 1.0083492774D+00
    ELSE IF (P == 8.0D+00) THEN
        R8_ZETA = R8_PI ** 8 / 9450.0D+00
    ELSE IF (P == 9.0D+00) THEN
        R8_ZETA = 1.0020083928D+00
    ELSE IF (P == 10.0D+00) THEN
        R8_ZETA = R8_PI ** 10 / 93555.0D+00
    ELSE IF (P == 11.0D+00) THEN
        R8_ZETA = 1.0004941886D+00
    ELSE IF (P == 12.0D+00) THEN
        R8_ZETA = 1.0002460866D+00
    ELSE IF (P == 13.0D+00) THEN
        R8_ZETA = 1.0001227133D+00
    ELSE IF (P == 14.0D+00) THEN
        R8_ZETA = 1.0000612482D+00
    ELSE IF (P == 15.0D+00) THEN
        R8_ZETA = 1.0000305882D+00
    ELSE IF (P == 16.0D+00) THEN
        R8_ZETA = 1.0000152823D+00
    ELSE IF (P == 17.0D+00) THEN
        R8_ZETA = 1.0000076372D+00
    ELSE IF (P == 18.0D+00) THEN
        R8_ZETA = 1.0000038173D+00
    ELSE IF (P == 19.0D+00) THEN
        R8_ZETA = 1.0000019082D+00
    ELSE IF (P == 20.0D+00) THEN
        R8_ZETA = 1.0000009540D+00
    ELSE
 
        ZSUM = 0.0D+00
        N = 0
 
        DO
 
            N = N + 1
            ZSUM_OLD = ZSUM
            ZSUM = ZSUM + 1.0D+00 / (REAL(N, KIND=8)) ** P
            IF (ZSUM <= ZSUM_OLD) THEN
                EXIT
            END IF
 
        END DO
 
        R8_ZETA = ZSUM
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8MAT_PRINT (M, N, A, TITLE)
 
!*****************************************************************************80
!
!! R8MAT_PRINT prints an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in A.
!
!    Input, integer ( kind = 4 ) N, the number of columns in A.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    CHARACTER (LEN=*) TITLE
 
    CALL R8MAT_PRINT_SOME (M, N, A, 1, 1, M, N, TITLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8MAT_PRINT_SOME (M, N, A, ILO, JLO, IHI, JHI, TITLE)
 
!*****************************************************************************80
!
!! R8MAT_PRINT_SOME prints some of an R8MAT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) A(M,N), an M by N matrix to be printed.
!
!    Input, integer ( kind = 4 ) ILO, JLO, the first row and column to print.
!
!    Input, integer ( kind = 4 ) IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
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
 
    IF (0 < LEN_TRIM(TITLE)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') TRIM (TITLE)
    END IF
 
    DO J2LO = MAX (JLO, 1), MIN (JHI, N), INCX
 
        J2HI = J2LO + INCX - 1
        J2HI = MIN (J2HI, N)
        J2HI = MIN (J2HI, JHI)
 
        INC = J2HI + 1 - J2LO
 
        WRITE (*, '(a)') ' '
 
        DO J = J2LO, J2HI
            J2 = J + 1 - J2LO
            WRITE (CTEMP(J2), '(i7,7x)') J
        END DO
 
        WRITE (*, '(''  Col   '',5a14)') CTEMP (1:INC)
        WRITE (*, '(a)') '  Row'
        WRITE (*, '(a)') ' '
 
        I2LO = MAX (ILO, 1)
        I2HI = MIN (IHI, M)
 
        DO I = I2LO, I2HI
 
            DO J2 = 1, INC
 
                J = J2LO - 1 + J2
 
                IF (R8_IS_INT(A(I, J))) THEN
                    WRITE (CTEMP(J2), '(f8.0,6x)') A (I, J)
                ELSE
                    WRITE (CTEMP(J2), '(g14.6)') A (I, J)
                END IF
 
            END DO
 
            WRITE (*, '(i5,1x,5a14)') I, (CTEMP(J), J=1, INC)
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8POLY_VALUE_HORNER (M, C, X)
 
!*****************************************************************************80
!
!! R8POLY_VALUE_HORNER evaluates a polynomial using Horner's method.
!
!  Discussion:
!
!    The polynomial
!
!      p(x) = c0 + c1 * x + c2 * x^2 + ... + cm * x^m
!
!    is to be evaluated at the value X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the degree.
!
!    Input, real ( kind = 8 ) C(0:M), the polynomial coefficients.
!    C(I) is the coefficient of X^I.
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) R8POLY_VALUE_HORNER, the polynomial value.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
 
    REAL (KIND=8) C (0:M)
    INTEGER (KIND=4) I
    REAL (KIND=8) R8POLY_VALUE_HORNER
    REAL (KIND=8) VALUE
    REAL (KIND=8) X
 
    VALUE = C (M)
    DO I = M - 1, 0, - 1
        VALUE = VALUE * X + C (I)
    END DO
 
    R8POLY_VALUE_HORNER = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8ROW_MAX (M, N, A, AMAX)
 
!*****************************************************************************80
!
!! R8ROW_MAX returns the maximums of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MAX =
!      3
!      7
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) AMAX(M), the maximums of the rows.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    REAL (KIND=8) AMAX (M)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, M
 
        AMAX (I) = A (I, 1)
        DO J = 2, N
            IF (AMAX(I) < A(I, J)) THEN
                AMAX (I) = A (I, J)
            END IF
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8ROW_MEAN (M, N, X, MEAN)
 
!*****************************************************************************80
!
!! R8ROW_MEAN returns the means of rows of an R8ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the array whose row means are desired.
!
!    Output, real ( kind = 8 ) MEAN(M), the means, or averages,
!    of the rows of X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    REAL (KIND=8) MEAN (M)
    REAL (KIND=8) X (M, N)
 
    DO I = 1, M
        MEAN (I) = SUM (X(I, 1:N)) / REAL (N)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8ROW_MIN (M, N, A, AMIN)
 
!*****************************************************************************80
!
!! R8ROW_MIN returns the minimums of an R8ROW.
!
!  Discussion:
!
!    An R8ROW is an M by N array of R8 values, regarded
!    as an array of M rows of length N.
!
!  Example:
!
!    A =
!      1  2  3
!      2  6  7
!
!    MIN =
!      1
!      2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the array.
!
!    Input, real ( kind = 8 ) A(M,N), the array to be examined.
!
!    Output, real ( kind = 8 ) AMIN(M), the minimums of the rows.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (M, N)
    REAL (KIND=8) AMIN (M)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
 
    DO I = 1, M
 
        AMIN (I) = A (I, 1)
        DO J = 2, N
            IF (A(I, J) < AMIN(I)) THEN
                AMIN (I) = A (I, J)
            END IF
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8ROW_VARIANCE (M, N, X, VARIANCE)
 
!*****************************************************************************80
!
!! R8ROW_VARIANCE returns the variances of the rows of an R8ROW.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, real ( kind = 8 ) X(M,N), the array whose row means are desired.
!
!    Output, real ( kind = 8 ) VARIANCE(M), the variances of the rows of X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE (M)
    REAL (KIND=8) X (M, N)
 
    DO I = 1, M
 
        MEAN = SUM (X(I, 1:N)) / REAL (N, KIND=8)
 
        VARIANCE (I) = SUM ((X(I, 1:N)-MEAN)**2)
 
        IF (1 < N) THEN
            VARIANCE (I) = VARIANCE (I) / REAL (N-1, KIND=8)
        ELSE
            VARIANCE (I) = 0.0D+00
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_CIRCULAR_VARIANCE (N, X, CIRCULAR_VARIANCE)
 
!*****************************************************************************80
!
!! R8VEC_CIRCULAR_VARIANCE returns the circular variance of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) CIRCULAR VARIANCE, the circular variance
!    of the vector entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) CIRCULAR_VARIANCE
    REAL (KIND=8) MEAN
    REAL (KIND=8) X (N)
 
    CALL R8VEC_MEAN (N, X, MEAN)
 
    CIRCULAR_VARIANCE = (SUM(COS(X(1:N)-MEAN))) ** 2 + (SUM(SIN(X(1:N)-MEAN))) ** 2
 
    CIRCULAR_VARIANCE = SQRT (CIRCULAR_VARIANCE) / REAL (N, KIND=8)
 
    CIRCULAR_VARIANCE = 1.0D+00 - CIRCULAR_VARIANCE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8VEC_DIFF_NORM (N, A, B)
 
!*****************************************************************************80
!
!! R8VEC_DIFF_NORM returns the L2 norm of the difference of R8VEC's.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    The vector L2 norm is defined as:
!
!      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in A.
!
!    Input, real ( kind = 8 ) A(N), B(N), the vectors
!
!    Output, real ( kind = 8 ) R8VEC_DIFF_NORM, the L2 norm of A - B.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) B (N)
    REAL (KIND=8) R8VEC_DIFF_NORM
 
    R8VEC_DIFF_NORM = SQRT (SUM((A(1:N)-B(1:N))**2))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_MAX (N, A, AMAX)
 
!*****************************************************************************80
!
!! R8VEC_MAX returns the maximum value in an R8VEC.
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
!    30 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMAX, the value of the largest entry.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) AMAX
 
    AMAX = MAXVAL (A(1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_MEAN (N, X, MEAN)
 
!*****************************************************************************80
!
!! R8VEC_MEAN returns the mean of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean, or average,
!    of the vector entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MEAN
    REAL (KIND=8) X (N)
 
    MEAN = SUM (X(1:N)) / REAL (N, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_MIN (N, A, AMIN)
 
!*****************************************************************************80
!
!! R8VEC_MIN returns the minimum value of an R8VEC.
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
!    17 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input, real ( kind = 8 ) A(N), the array.
!
!    Output, real ( kind = 8 ) AMIN, the value of the smallest entry.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) AMIN
 
    AMIN = MINVAL (A(1:N))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! R8VEC_PRINT prints an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 December 1999
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
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    INTEGER (KIND=4) I
    CHARACTER (LEN=*) TITLE
 
    IF (TITLE /= ' ') THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') TRIM (TITLE)
    END IF
 
    WRITE (*, '(a)') ' '
    DO I = 1, N
        WRITE (*, '(i8,g14.6)') I, A (I)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_UNIFORM (N, A, B, SEED, R)
 
!*****************************************************************************80
!
!! R8VEC_UNIFORM returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of real ( kind = 8 ) values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) I
    INTEGER (KIND=4) K
    INTEGER (KIND=4) SEED
    REAL (KIND=8) R (N)
 
    IF (SEED == 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8VEC_UNIFORM - Fatal error!'
        WRITE (*, '(a)') '  Input value of SEED = 0.'
        RETURN
    END IF
 
    DO I = 1, N
 
        K = SEED / 127773
 
        SEED = 16807 * (SEED-K*127773) - K * 2836
 
        IF (SEED < 0) THEN
            SEED = SEED + I4_HUGE ()
        END IF
 
        R (I) = A + (B-A) * REAL (SEED, KIND=8) * 4.656612875D-10
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_UNIFORM_01 (N, SEED, R)
 
!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of real ( kind = 8 ) values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) K
    INTEGER (KIND=4) SEED
    REAL (KIND=8) R (N)
 
    IF (SEED == 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8VEC_UNIFORM_01 - Fatal error!'
        WRITE (*, '(a)') '  Input value of SEED = 0.'
        RETURN
    END IF
 
    DO I = 1, N
 
        K = SEED / 127773
 
        SEED = 16807 * (SEED-K*127773) - K * 2836
 
        IF (SEED < 0) THEN
            SEED = SEED + I4_HUGE ()
        END IF
 
        R (I) = REAL (SEED, KIND=8) * 4.656612875D-10
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_UNIT_SUM (N, A)
 
!*****************************************************************************80
!
!! R8VEC_UNIT_SUM normalizes an R8VEC to have unit sum.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, real A(N), the vector to be normalized.  On output,
!    the entries of A should have unit sum.  However, if the input vector
!    has zero sum, the routine halts.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (N)
    REAL (KIND=8) A_SUM
 
    A_SUM = SUM (A(1:N))
 
    IF (A_SUM == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8VEC_UNIT_SUM - Fatal error!'
        WRITE (*, '(a)') '  The vector entries sum to 0.'
        RETURN
    END IF
 
    A (1:N) = A (1:N) / A_SUM
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_VARIANCE (N, X, VARIANCE)
 
!*****************************************************************************80
!
!! R8VEC_VARIANCE returns the variance of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
    REAL (KIND=8) X (N)
 
    CALL R8VEC_MEAN (N, X, MEAN)
 
    VARIANCE = SUM ((X(1:N)-MEAN)**2)
 
    IF (1 < N) THEN
        VARIANCE = VARIANCE / REAL (N-1, KIND=8)
    ELSE
        VARIANCE = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RANDOM_INITIALIZE (SEED_INPUT)
 
!*****************************************************************************80
!
!! RANDOM_INITIALIZE initializes the FORTRAN90 random number seed.
!
!  Discussion:
!
!    If you don't initialize the FORTRAN90 random number generator
!    routine RANDOM_NUMBER, which is used by calls like
!
!      call random_number ( harvest = x )
!
!    then its behavior is not specified.  That may be OK for you.  But
!    you may want to be able to force the same sequence to be generated
!    each time, or to force a different sequence.
!
!    To control the sequence of random numbers, you need to set the seed.
!    In FORTRAN90, this is done by calling the RANDOM+SEED routine.
!    You can call it with no arguments, in fact.  But if you call
!    it with no arguments:
!
!      call random_seed ( )
!
!    then its behavior (or more particularly, the behavior of RANDOM_NUMBER)
!    is still not specified.  You might hope that the system will go out
!    and pick a nice random seed for you, but there's no guarantee.
!
!
!    For example, on the DEC ALPHA, if you compile a program that calls
!    RANDOM_NUMBER, then every time you run it, you get the same sequence
!    of "random" values.  If you compile a program that calls RANDOM_SEED
!    with no arguments, and then calls RANDOM_NUMBER, you still get the
!    same sequence each time you run the program.
!
!    In order to actually try to scramble up the random number generator
!    a bit, this routine goes through the tedious process of getting the
!    size of the random number seed, making up values based on the current
!    time, and setting the random number seed.
!
!    Unfortunately, the RANDOM_SEED routine has a very elastic definition.
!    It does not use a single scalar integer SEED.  Instead, it communicates
!    with the user through an integer vector whose size is not specified.
!    You actually have to "ask" the routine to tell you the size of this
!    vector.  Then you can fill up the vector with values to be used to
!    influence the seeding of the random number routine.  The details of
!    how the seed affects the sequence are also unspecified, but the only
!    thing we are reasonably confident about is that specifying the same
!    seed should result in the same sequence, and specifying different
!    seeds should result in different sequences!
!
!    I assume this is far more than you wanted to know.  (It's certainly
!    more than I wanted to know!)
!
!    The argument SEED is an input quantity only, so it is legal
!    to type
!
!      call random_initialize ( 0 )
!
!    or
!
!      call random_initialize ( 18867 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SEED_INPUT, the user's "suggestion" for a seed.
!    However, if the input value is 0, the routine will come up with
!    its own "suggestion", based on the system clock.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) COUNT
    INTEGER (KIND=4) COUNT_MAX
    INTEGER (KIND=4) COUNT_RATE
    LOGICAL, PARAMETER :: DEBUG = .FALSE.
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) SEED_INPUT
    INTEGER (KIND=4), ALLOCATABLE :: SEED_VECTOR (:)
    INTEGER (KIND=4) SEED_SIZE
    REAL (KIND=8) T
    INTEGER (KIND=4), PARAMETER :: WARM_UP = 100
 
    SEED = SEED_INPUT
!
!  Initialize the random seed routine.
!
    CALL RANDOM_SEED ()
!
!  Determine the size of the random number seed vector.
!
    CALL RANDOM_SEED (SIZE=SEED_SIZE)
!
!  Allocate a vector of the right size to be used as a random seed.
!
    ALLOCATE (SEED_VECTOR(SEED_SIZE))
!
!  If the user supplied a SEED value, use that.
!
!  Otherwise, use the system clock value to make up a value that is
!  likely to change based on when this routine is called.
!
    IF (SEED /= 0) THEN
 
        IF (DEBUG) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'RANDOM_INITIALIZE'
            WRITE (*, '(a,i20)') '  Initialize RANDOM_NUMBER, user SEED = ', SEED
        END IF
 
    ELSE
 
        CALL SYSTEM_CLOCK (COUNT, COUNT_RATE, COUNT_MAX)
 
        SEED = COUNT
 
        IF (DEBUG) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'RANDOM_INITIALIZE'
            WRITE (*, '(a,i20)') '  Initialize RANDOM_NUMBER, arbitrary SEED = ', SEED
        END IF
 
    END IF
!
!  Set the seed vector.  We don't know the significance of the
!  individual entries of the internal seed vector, so we'll just set
!  all entries to SEED.
!
    SEED_VECTOR (1:SEED_SIZE) = SEED
!
!  Now call RANDOM_SEED, and tell it to use this seed vector.
!
    CALL RANDOM_SEED (PUT=SEED_VECTOR(1:SEED_SIZE))
!
!  Free up the seed space.
!
    DEALLOCATE (SEED_VECTOR)
!
!  Call the random number routine a bunch of times just to "warm it up".
!
    DO I = 1, WARM_UP
        CALL RANDOM_NUMBER (HARVEST=T)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! RAYLEIGH_CDF evaluates the Rayleigh CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    0.0 <= X.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - EXP (-X**2/(2.0D+00*A**2))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_CDF_INV (CDF, A, X)
 
!*****************************************************************************80
!
!! RAYLEIGH_CDF_INV inverts the Rayleigh CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RAYLEIGH_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = SQRT (-2.0D+00*A*A*LOG(1.0D+00-CDF))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_CDF_VALUES (N_DATA, SIGMA, X, FX)
 
!*****************************************************************************80
!
!! RAYLEIGH_CDF_VALUES returns some values of the Rayleigh CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = RayleighDistribution [ sigma ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) SIGMA, the shape parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 9
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.8646647167633873D+00, &
   & 0.9996645373720975D+00, 0.9999999847700203D+00, 0.999999999999987D+00, &
   & 0.8646647167633873D+00, 0.3934693402873666D+00, 0.1992625970831920D+00, &
   & 0.1175030974154046D+00, 0.7688365361336422D-01 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) SIGMA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: SIGMA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        SIGMA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        SIGMA = SIGMA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION RAYLEIGH_CHECK (A)
 
!*****************************************************************************80
!
!! RAYLEIGH_CHECK checks the parameter of the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, logical RAYLEIGH_CHECK, is true if the parameter is legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL RAYLEIGH_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RAYLEIGH_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.'
        RAYLEIGH_CHECK = .FALSE.
        RETURN
    END IF
 
    RAYLEIGH_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! RAYLEIGH_MEAN returns the mean of the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    MEAN = A * SQRT (0.5D+00*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! RAYLEIGH_PDF evaluates the Rayleigh PDF.
!
!  Discussion:
!
!    PDF(A;X) = ( X / A^2 ) * EXP ( - X^2 / ( 2 * A^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE
        PDF = (X/A**2) * EXP (-X**2/(2.0D+00*A**2))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! RAYLEIGH_SAMPLE samples the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0.0 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL RAYLEIGH_CDF_INV (CDF, A, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RAYLEIGH_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! RAYLEIGH_VARIANCE returns the variance of the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameters of the PDF.
!    0.0 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 2.0D+00 * A ** 2 * (1.0D+00-0.25D+00*R8_PI)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! RECIPROCAL_CDF evaluates the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (0.0D+00 < X) THEN
 
        CDF = LOG (A/X) / LOG (A/B)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! RECIPROCAL_CDF_INV inverts the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RECIPROCAL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = 0.0D+00
    ELSE IF (0.0D+00 < CDF) THEN
        X = B ** CDF / A ** (CDF-1.0D+00)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION RECIPROCAL_CHECK (A, B)
 
!*****************************************************************************80
!
!! RECIPROCAL_CHECK checks the parameters of the Reciprocal CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, logical RECIPROCAL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL RECIPROCAL_CHECK
 
    IF (A <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RECIPROCAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 0.0'
        RECIPROCAL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RECIPROCAL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < A'
        RECIPROCAL_CHECK = .FALSE.
        RETURN
    END IF
 
    RECIPROCAL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! RECIPROCAL_MEAN returns the mean of the Reciprocal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = (A-B) / LOG (A/B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! RECIPROCAL_PDF evaluates the Reciprocal PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 1.0D+00 / ( X * LOG ( B / A ) )
!    for 0.0D+00 <= X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        PDF = 0.0D+00
    ELSE IF (0.0D+00 < X) THEN
        PDF = 1.0D+00 / (X*LOG(B/A))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! RECIPROCAL_SAMPLE samples the Reciprocal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    X = B ** CDF / A ** (CDF-1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RECIPROCAL_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! RECIPROCAL_VARIANCE returns the variance of the Reciprocal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) D
    REAL (KIND=8) VARIANCE
 
    D = LOG (A/B)
 
    VARIANCE = (A-B) * (A*(D-2.0D+00)+B*(D+2.0D+00)) / (2.0D+00*D**2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RIBESL (X, ALPHA, NB, IZE, B, NCALC)
 
!*****************************************************************************80
!
!! RIBESL calculates I Bessel function with non-integer orders.
!
!  Discussion:
!
!    This routine calculates Bessel functions I SUB(N+ALPHA) (X)
!    for non-negative argument X, and non-negative order N+ALPHA,
!    with or without exponential scaling.
!
!    This program is based on a program written by David
!    Sookne that computes values of the Bessel functions J or
!    I of real argument and integer order.  Modifications include
!    the restriction of the computation to the I Bessel function
!    of non-negative real argument, the extension of the computation
!    to arbitrary positive order, the inclusion of optional
!    exponential scaling, and the elimination of most underflow.
!
!    In case of an error, NCALC will not equal NB, and not all I's are
!    calculated to the desired accuracy.
!
!    If NCALC < 0:  An argument is out of range. For example,
!    NB <= 0, IZE is not 1 or 2, or IZE = 1 and EXPARG <= ABS(X)
!    In this case, the B-vector is not calculated, and NCALC is
!    set to MIN(NB,0)-1 so that NCALC /= NB.
!
!    If 0 < NCALC < NB, then not all requested function values could
!    be calculated accurately.  This usually occurs because NB is
!    much larger than ABS(X).  In this case, B(N) is calculated
!    to the desired accuracy for N <= NCALC, but precision
!    is lost for NCALC < N <= NB.  If B(N) does not vanish
!    for NCALC < N (because it is too small to be represented),
!    and B(N)/B(NCALC) = 10 ^ (-K), then only the first NSIG-K
!    significant figures of B(N) can be trusted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 2007
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Frank Olver, David Sookne,
!    A Note on Backward Recurrence Algorithms,
!    Mathematics of Computation,
!    Volume 26, 1972, pages 941-947.
!
!    David Sookne,
!    Bessel Functions of Real Argument and Integer Order,
!    NBS Journal of Research B,
!    Volume 77B, 1973, pages 125-132.
!
!    William Cody,
!    Algorithm 597:
!    Sequence of Modified Bessel Functions of the First Kind,
!    ACM Transactions of Mathematical Software,
!    Volume 9, Number 2, June 1983, pages 242-245.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument for which the functions
!    are to be calculated.
!
!    Input, real ( kind = 8 ) ALPHA,the fractional part of the order
!    for which the functions are to be calculated.
!    0 <= ALPHA < 1.0.
!
!    Input, integer ( kind = 4 ) NB, the number of functions to be calculated.
!    The first function calculated is of order ALPHA, and the
!    last is of order (NB - 1 + ALPHA).  1 <= NB.
!
!    Input, integer ( kind = 4 ) IZE, scaling option.
!    1, unscaled I's are to calculated,
!    2, exponentially scaled I's are to be calculated.
!
!    Output, real ( kind = 8 ) B(NB), the values of the functions
!    I(ALPHA,X) through I(NB-1+ALPHA,X), with scaling if requested.
!
!    Output, integer ( kind = 4 ) NCALC, error indicator.
!    If NCALC = NB, then all the requested values were calculated
!    to the desired accuracy.
!
!  Local Parameeters:
!
!    BETA, the radix for the floating-point system.
!
!    MINEXP, smallest representable power of BETA.
!
!    MAXEXP, smallest power of BETA that overflows
!
!    IT, number of bits in the mantissa of a working precision variable.
!
!    NSIG, decimal significance desired.  Should be set to
!    INT(LOG10(2)*IT+1).  Setting NSIG lower will result
!    in decreased accuracy while setting NSIG higher will
!    increase CPU time without increasing accuracy.  The
!    truncation error is limited to a relative error of
!    T=.5*10^(-NSIG).
!
!    ENTEN, 10.0^K, where K is the largest integer such that
!    ENTEN is machine-representable in working precision
!
!    ENSIG, 10.0^NSIG
!
!    RTNSIG, 10.0^(-K) for the smallest integer K such that
!    NSIG/4 <= K.
!
!    ENMTEN, smallest ABS(X) such that X/4 does not underflow
!
!    XLARGE, upper limit on the magnitude of X when IZE=2.  Bear
!    in mind that if ABS(X)=N, then at least N iterations
!    of the backward recursion will be executed.  The value
!    of 10.0^4 is used on every machine.
!
!    EXPARG, largest working precision argument that the library
!    EXP routine can handle and upper limit on the
!    magnitude of X when IZE=1; approximately log(BETA^MAXEXP).
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) NB
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8) B (NB)
    REAL (KIND=8), PARAMETER :: CONST = 1.585D+00
    REAL (KIND=8) EM
    REAL (KIND=8) EMPAL
    REAL (KIND=8) EMP2AL
    REAL (KIND=8) EN
    REAL (KIND=8), PARAMETER :: ENMTEN = 8.9D-308
    REAL (KIND=8), PARAMETER :: ENSIG = 1.0D+16
    REAL (KIND=8), PARAMETER :: ENTEN = 1.0D+308
    REAL (KIND=8), PARAMETER :: EXPARG = 709.0D+00
    LOGICAL FLAG
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    REAL (KIND=8) HALFX
    INTEGER (KIND=4) IZE
    INTEGER (KIND=4) K
    INTEGER (KIND=4) L
    INTEGER (KIND=4) MAGX
    INTEGER (KIND=4) N
    INTEGER (KIND=4) NBMX
    INTEGER (KIND=4) NCALC
    INTEGER (KIND=4) NEND
    INTEGER (KIND=4), PARAMETER :: NSIG = 16
    INTEGER (KIND=4) NSTART
    REAL (KIND=8), PARAMETER :: ONE = 1.0D+00
    REAL (KIND=8) P
    REAL (KIND=8) PLAST
    REAL (KIND=8) POLD
    REAL (KIND=8) PSAVE
    REAL (KIND=8) PSAVEL
    REAL (KIND=8), PARAMETER :: RTNSIG = 1.0D-04
    REAL (KIND=8) TEMPA
    REAL (KIND=8) TEMPB
    REAL (KIND=8) TEMPC
    REAL (KIND=8) TEST
    REAL (KIND=8) TOTAL
    REAL (KIND=8) TOVER
    REAL (KIND=8), PARAMETER :: TWO = 2.0D+00
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER :: XLARGE = 1.0D+04
    REAL (KIND=8), PARAMETER :: ZERO = 0.0D+00
!
!  Check for X, NB, OR IZE out of range.
!
    IF (NB <= 0) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
 
    IF (X < 0.0D+00) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
 
    IF (ALPHA < 0.0D+00) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
 
    IF (1.0D+00 <= ALPHA) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
 
    IF (IZE == 1 .AND. EXPARG < X) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
 
    IF (IZE == 2 .AND. XLARGE < X) THEN
        NCALC = MIN (NB, 0) - 1
        RETURN
    END IF
!
!  Use 2-term ascending series for small X.
!
    NCALC = NB
    MAGX = INT (X)
!
!  Initialize the forward sweep, the P-sequence of Olver.
!
    IF (RTNSIG <= X) THEN
 
        NBMX = NB - MAGX
        N = MAGX + 1
        EN = REAL (N+N, KIND=8) + (ALPHA+ALPHA)
        PLAST = ONE
        P = EN / X
!
!  Calculate general significance test.
!
        TEST = ENSIG + ENSIG
 
        IF (5*NSIG < 2*MAGX) THEN
            TEST = SQRT (TEST*P)
        ELSE
            TEST = TEST / CONST ** MAGX
        END IF
!
!  Calculate P-sequence until N = NB-1.  Check for possible overflow.
!
        FLAG = .FALSE.
 
        IF (3 <= NBMX) THEN
 
            TOVER = ENTEN / ENSIG
            NSTART = MAGX + 2
            NEND = NB - 1
 
            DO K = NSTART, NEND
 
                N = K
                EN = EN + TWO
                POLD = PLAST
                PLAST = P
                P = EN * PLAST / X + POLD
!
!  To avoid overflow, divide P-sequence by TOVER.  Calculate
!  P-sequence until 1 < ABS(P).
!
                IF (TOVER < P) THEN
 
                    TOVER = ENTEN
                    P = P / TOVER
                    PLAST = PLAST / TOVER
                    PSAVE = P
                    PSAVEL = PLAST
                    NSTART = N + 1
 
                    DO
 
                        N = N + 1
                        EN = EN + TWO
                        POLD = PLAST
                        PLAST = P
                        P = EN * PLAST / X + POLD
 
                        IF (1.0D+00 < P) THEN
                            EXIT
                        END IF
 
                    END DO
 
                    TEMPB = EN / X
!
!  Calculate backward test, and find NCALC, the highest N
!  such that the test is passed.
!
                    TEST = POLD * PLAST / ENSIG
                    TEST = TEST * (HALF-HALF/(TEMPB*TEMPB))
                    P = PLAST * TOVER
                    N = N - 1
                    EN = EN - TWO
                    NEND = MIN (NB, N)
 
                    NCALC = NEND + 1
 
                    DO L = NSTART, NEND
 
                        POLD = PSAVEL
                        PSAVEL = PSAVE
                        PSAVE = EN * PSAVEL / X + POLD
 
                        IF (TEST < PSAVE*PSAVEL) THEN
                            NCALC = L
                            EXIT
                        END IF
 
                    END DO
 
                    NCALC = NCALC - 1
                    FLAG = .TRUE.
                    EXIT
 
                END IF
 
            END DO
 
            IF ( .NOT. FLAG) THEN
 
                N = NEND
                EN = REAL (N+N, KIND=8) + (ALPHA+ALPHA)
!
!  Calculate special significance test for 2 < NBMX.
!
                TEST = MAX (TEST, SQRT(PLAST*ENSIG)*SQRT(P+P))
 
            END IF
 
        END IF
!
!  Calculate P-sequence until significance test passed.
!
        IF ( .NOT. FLAG) THEN
 
            DO
 
                N = N + 1
                EN = EN + TWO
                POLD = PLAST
                PLAST = P
                P = EN * PLAST / X + POLD
 
                IF (TEST <= P) THEN
                    EXIT
                END IF
 
            END DO
 
        END IF
!
!  Initialize the backward recursion and the normalization sum.
!
        N = N + 1
        EN = EN + TWO
        TEMPB = ZERO
        TEMPA = ONE / P
        EM = REAL (N, KIND=8) - ONE
        EMPAL = EM + ALPHA
        EMP2AL = (EM-ONE) + (ALPHA+ALPHA)
        TOTAL = TEMPA * EMPAL * EMP2AL / EM
        NEND = N - NB
!
!  N < NB, so store B(N) and set higher orders to zero.
!
        IF (NEND < 0) THEN
 
            B (N) = TEMPA
            NEND = - NEND
 
            DO L = 1, NEND
                B (N+L) = ZERO
            END DO
 
            NEND = N - 2
!
!  Calculate via difference equation and store B(N), until N = 2.
!
            IF (0 < NEND) THEN
 
                DO L = 1, NEND
                    N = N - 1
                    EN = EN - TWO
                    B (N) = (EN*B(N+1)) / X + B (N+2)
                    EM = EM - ONE
                    EMP2AL = EMP2AL - ONE
                    IF (N == 2) THEN
                        EMP2AL = ONE
                    END IF
                    EMPAL = EMPAL - ONE
                    TOTAL = (TOTAL+B(N)*EMPAL) * EMP2AL / EM
                END DO
 
            END IF
!
!  Calculate B(1).
!
            B (1) = TWO * EMPAL * B (2) / X + B (3)
 
            TOTAL = (TOTAL+TOTAL) + B (1)
!
!  Recur backward via difference equation, calculating (but
!  not storing) B(N), until N = NB.
!
        ELSE
 
            IF (0 < NEND) THEN
 
                DO L = 1, NEND
 
                    N = N - 1
                    EN = EN - TWO
                    TEMPC = TEMPB
                    TEMPB = TEMPA
                    TEMPA = (EN*TEMPB) / X + TEMPC
                    EM = EM - ONE
                    EMP2AL = EMP2AL - ONE
 
                    IF (N == 1) THEN
                        EXIT
                    END IF
 
                    IF (N == 2) THEN
                        EMP2AL = ONE
                    END IF
 
                    EMPAL = EMPAL - ONE
                    TOTAL = (TOTAL+TEMPA*EMPAL) * EMP2AL / EM
 
                END DO
 
            END IF
!
!  Store B(NB).
!
            B (N) = TEMPA
 
            IF (NB <= 1) THEN
 
                TOTAL = (TOTAL+TOTAL) + TEMPA
!
!  Calculate and Store B(NB-1).
!
            ELSE
 
                N = N - 1
                EN = EN - TWO
                B (N) = (EN*TEMPA) / X + TEMPB
 
                IF (1 < N) THEN
 
                    EM = EM - ONE
                    EMP2AL = EMP2AL - ONE
 
                    IF (N == 2) THEN
                        EMP2AL = ONE
                    END IF
 
                    EMPAL = EMPAL - ONE
                    TOTAL = (TOTAL+B(N)*EMPAL) * EMP2AL / EM
 
                    NEND = N - 2
!
!  Calculate via difference equation and store B(N), until N = 2.
!
                    IF (0 < NEND) THEN
 
                        DO L = 1, NEND
                            N = N - 1
                            EN = EN - TWO
                            B (N) = (EN*B(N+1)) / X + B (N+2)
                            EM = EM - ONE
                            EMP2AL = EMP2AL - ONE
                            IF (N == 2) THEN
                                EMP2AL = ONE
                            END IF
                            EMPAL = EMPAL - ONE
                            TOTAL = (TOTAL+B(N)*EMPAL) * EMP2AL / EM
                        END DO
 
                    END IF
!
!  Calculate B(1).
!
                    B (1) = TWO * EMPAL * B (2) / X + B (3)
 
                END IF
 
                TOTAL = (TOTAL+TOTAL) + B (1)
 
            END IF
 
        END IF
!
!  Normalize.  Divide all B(N) by TOTAL.
!
 
 
        IF (ALPHA /= ZERO) THEN
            TOTAL = TOTAL * GAMMA (ONE+ALPHA) * (X*HALF) ** (-ALPHA)
        END IF
 
        IF (IZE == 1) THEN
            TOTAL = TOTAL * EXP (-X)
        END IF
 
        TEMPA = ENMTEN
 
        IF (1.0D+00 < TOTAL) THEN
            TEMPA = TEMPA * TOTAL
        END IF
 
        DO N = 1, NB
            IF (B(N) < TEMPA) THEN
                B (N) = ZERO
            END IF
            B (N) = B (N) / TOTAL
        END DO
 
        RETURN
!
!  Two-term ascending series for small X.
!
    ELSE
 
        TEMPA = ONE
        EMPAL = ONE + ALPHA
        HALFX = ZERO
 
        IF (ENMTEN < X) THEN
            HALFX = HALF * X
        END IF
 
        IF (ALPHA /= ZERO) THEN
            TEMPA = HALFX ** ALPHA / GAMMA (EMPAL)
        END IF
 
        IF (IZE == 2) THEN
            TEMPA = TEMPA * EXP (-X)
        END IF
 
        TEMPB = ZERO
 
        IF (ONE < X+ONE) THEN
            TEMPB = HALFX * HALFX
        END IF
 
        B (1) = TEMPA + TEMPA * TEMPB / EMPAL
 
        IF (X /= ZERO .AND. B(1) == ZERO) THEN
            NCALC = 0
        END IF
 
        IF (1 < NB) THEN
 
            IF (X == ZERO) THEN
 
                B (2:NB) = ZERO
!
!  Calculate higher-order functions.
!
            ELSE
 
                TEMPC = HALFX
                TOVER = (ENMTEN+ENMTEN) / X
 
                IF (TEMPB /= ZERO) THEN
                    TOVER = ENMTEN / TEMPB
                END IF
 
                DO N = 2, NB
 
                    TEMPA = TEMPA / EMPAL
                    EMPAL = EMPAL + ONE
                    TEMPA = TEMPA * TEMPC
 
                    IF (TEMPA <= TOVER*EMPAL) THEN
                        TEMPA = ZERO
                    END IF
 
                    B (N) = TEMPA + TEMPA * TEMPB / EMPAL
 
                    IF (B(N) == ZERO .AND. N < NCALC) THEN
                        NCALC = N - 1
                    END IF
 
                END DO
 
            END IF
 
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RUNS_MEAN (M, N, MEAN)
 
!*****************************************************************************80
!
!! RUNS_MEAN returns the mean of the Runs PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    REAL (KIND=8) MEAN
    INTEGER (KIND=4) N
 
    MEAN = REAL (M+2*M*N+N, KIND=8) / REAL (M+N, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RUNS_PDF (M, N, R, PDF)
 
!*****************************************************************************80
!
!! RUNS_PDF evaluates the Runs PDF.
!
!  Discussion:
!
!    Suppose we have M symbols of one type and N of another, and we consider
!    the various possible permutations of these symbols.
!
!    Let "R" be the number of runs in a given permutation.  By a "run", we
!    mean a maximal sequence of identical symbols.  Thus, for instance,
!    the permutation
!
!      ABBBAAAAAAAA
!
!    has three runs.
!
!    The probability that a permutation of M+N symbols, with M of one kind
!    and N of another, will have exactly R runs is:
!
!      PDF(M,N)(R) = 2 * C(M-1,R/2-1) * C(N-1,R/2-1)
!                    / C(M+N,N) for R even;
!
!                  = ( C(M-1,(R-1)/2) * C(N-1,(R-3)/2 )
!                    + C(M-1,(R-3)/2) * C(N-1,(R-1)/2 )
!                    ) / C(M+N,N) for R odd.
!
!    The minimum number of runs is:
!
!      1 if M or N is 0,
!      2 otherwise.
!
!    The maximum number of runs is:
!
!      M + N,                if M = N
!      2 * min ( M, N ) + 1  otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kalimutha Krishnamoorthy,
!    Handbook of Statistical Distributions with Applications,
!    Chapman and Hall, 2006,
!    ISBN: 1-58488-635-8,
!    LC: QA273.6.K75.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Input, integer ( kind = 4 ) R, the number of runs.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) PDF
    INTEGER (KIND=4) R
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RUN_PDF - Fatal error!'
        WRITE (*, '(a)') '  M must be at least 0.'
        WRITE (*, '(a,i8)') '  The input value of M = ', M
        RETURN
    END IF
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RUN_PDF - Fatal error!'
        WRITE (*, '(a)') '  N must be at least 0.'
        WRITE (*, '(a,i8)') '  The input value of N = ', N
        RETURN
    END IF
 
    IF (N+M <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'RUN_PDF - Fatal error!'
        WRITE (*, '(a)') '  M+N must be at least 1.'
        WRITE (*, '(a,i8)') '  The input value of M+N = ', M + N
        RETURN
    END IF
!
!  If all the symbols are of one type, there is always 1 run.
!
    IF (M == 0 .OR. N == 0) THEN
        IF (R == 1) THEN
            PDF = 1.0D+00
        ELSE
            PDF = 0.0D+00
        END IF
        RETURN
    END IF
!
!  Take care of extreme values of R.
!
    IF (R < 2 .OR. M+N < R) THEN
        PDF = 0.0D+00
        RETURN
    END IF
!
!  The normal cases.
!
    IF (MOD(R, 2) == 0) THEN
 
        PDF = REAL (2*I4_CHOOSE(M-1, (R/2)-1)*I4_CHOOSE(N-1, (R/2)-1), KIND=8) / REAL &
       & (I4_CHOOSE(M+N, N), KIND=8)
 
    ELSE
 
        PDF = REAL (I4_CHOOSE(M-1, (R-1)/2)*I4_CHOOSE(N-1, (R-3)/2)+I4_CHOOSE(M-1, &
       & (R-3)/2)*I4_CHOOSE(N-1, (R-1)/2), KIND=8) / REAL (I4_CHOOSE(M+N, N), KIND=8)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RUNS_SAMPLE (M, N, SEED, R)
 
!*****************************************************************************80
!
!! RUNS_SAMPLE samples the Runs PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) R, the number of runs.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M+N)
    INTEGER (KIND=4) R
    INTEGER (KIND=4) SEED
 
    CALL RUNS_SIMULATE (M, N, SEED, A)
 
    CALL I4VEC_RUN_COUNT (M+N, A, R)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RUNS_SIMULATE (M, N, SEED, A)
 
!*****************************************************************************80
!
!! RUNS_SIMULATE simulates a case governed by the Runs PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) A(M+N), a sequence of M 0's and N 1's chosen
!    uniformly at random.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) A (M+N)
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) K
    INTEGER (KIND=4) SEED
 
    A (1:M) = 0
    A (M+1:M+N) = 1
 
    DO I = 1, M + N - 1
 
        J = I4_UNIFORM_AB (I, M+N, SEED)
 
        K = A (I)
        A (I) = A (J)
        A (J) = K
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE RUNS_VARIANCE (M, N, VARIANCE)
 
!*****************************************************************************80
!
!! RUNS_VARIANCE returns the variance of the Runs PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8) VARIANCE
 
    VARIANCE = REAL (2*M*N*(2*M*N-M-N), KIND=8) / REAL ((M+N)*(M+N)*(M+N-1), KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SECH (X)
 
!*****************************************************************************80
!
!! SECH returns the hyperbolic secant.
!
!  Discussion:
!
!    SECH ( X ) = 1.0D+00 / COSH ( X ) = 2.0D+00 / ( EXP ( X ) + EXP ( - X ) )
!
!    SECH is not a built-in function in FORTRAN, and occasionally it
!    is handier, or more concise, to be able to refer to it directly
!    rather than through its definition in terms of the sine function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) SECH, the hyperbolic secant of X.
!
    IMPLICIT NONE
 
    REAL (KIND=8) SECH
    REAL (KIND=8) X
 
    SECH = 1.0D+00 / COSH (X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! SECH_CDF evaluates the Hyperbolic Secant CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    CDF = 2.0D+00 * ATAN (EXP(Y)) / R8_PI
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! SECH_CDF_INV inverts the Hyperbolic Secant CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SECH_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = - HUGE (X)
    ELSE IF (CDF < 1.0D+00) THEN
        X = A + B * LOG (TAN(0.5D+00*R8_PI*CDF))
    ELSE IF (1.0D+00 == CDF) THEN
        X = HUGE (X)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SECH_CHECK (A, B)
 
!*****************************************************************************80
!
!! SECH_CHECK checks the parameters of the Hyperbolic Secant CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical SECH_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL SECH_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SECH_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.0'
        SECH_CHECK = .FALSE.
        RETURN
    END IF
 
    SECH_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! SECH_MEAN returns the mean of the Hyperbolic Secant PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! SECH_PDF evaluates the Hypebolic Secant PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = sech ( ( X - A ) / B ) / ( PI * B )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    PDF = SECH (Y) / (R8_PI*B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! SECH_SAMPLE samples the Hyperbolic Secant PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    X = A + B * LOG (TAN(0.5D+00*R8_PI*CDF))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SECH_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! SECH_VARIANCE returns the variance of the Hyperbolic Secant PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 0.25D+00 * (R8_PI*B) ** 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_CDF evaluates the Semicircular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= A-B) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (X <= A+B) THEN
 
        Y = (X-A) / B
 
        CDF = 0.5D+00 + (Y*SQRT(1.0D+00-Y**2)+ASIN(Y)) / R8_PI
 
    ELSE IF (A+B < X) THEN
 
        CDF = 1.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_CDF_INV inverts the Semicircular CDF.
!
!  Discussion:
!
!    A simple bisection method is used on the interval [ A - B, A + B ].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: TOL = 0.0001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SEMICIRCULAR_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = A - B
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = A + B
        RETURN
    END IF
 
    X1 = A - B
    CDF1 = 0.0D+00
 
    X2 = A + B
    CDF2 = 1.0D+00
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL SEMICIRCULAR_CDF (X3, A, B, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'SEMICIRCULAR_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SEMICIRCULAR_CHECK (A, B)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_CHECK checks the parameters of the Semicircular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameter of the PDF.
!    0.0 < B.
!
!    Output, logical SEMICIRCULAR_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL SEMICIRCULAR_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SEMICIRCULAR_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.0'
        SEMICIRCULAR_CHECK = .FALSE.
        RETURN
    END IF
 
    SEMICIRCULAR_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_MEAN returns the mean of the Semicircular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_PDF evaluates the Semicircular PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = ( 2 / ( B * PI ) ) * SQRT ( 1 - ( ( X - A ) / B )^2 )
!    for A - B <= X <= A + B
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X < A-B) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (X <= A+B) THEN
 
        Y = (X-A) / B
 
        PDF = 2.0D+00 / (B*R8_PI) * SQRT (1.0D+00-Y**2)
 
    ELSE IF (A+B < X) THEN
 
        PDF = 0.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_SAMPLE samples the Semicircular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ANGLE
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) RADIUS
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    RADIUS = R8_UNIFORM_01 (SEED)
    RADIUS = B * SQRT (RADIUS)
    ANGLE = R8_PI * R8_UNIFORM_01 (SEED)
    X = A + RADIUS * COS (ANGLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SEMICIRCULAR_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! SEMICIRCULAR_VARIANCE returns the variance of the Semicircular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = B * B / 4.0D+00
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SIN_POWER_INT (A, B, N)
 
!*****************************************************************************80
!
!! SIN_POWER_INT evaluates the sine power integral.
!
!  Discussion:
!
!    The function is defined by
!
!      SIN_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( sin ( t ))^n dt
!
!    The algorithm uses the following fact:
!
!      Integral sin^n ( t ) = (1/n) * (
!        sin^(n-1)(t) * cos(t) + ( n-1 ) * Integral sin^(n-2) ( t ) dt )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, real ( kind = 8 ) A, B, the limits of integration.
!
!    Input, integer ( kind = 4 ) N, the power of the sine function.
!
!    Output, real ( kind = 8 ) SIN_POWER_INT, the value of the integral.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CA
    REAL (KIND=8) CB
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MLO
    INTEGER (KIND=4) N
    REAL (KIND=8) SA
    REAL (KIND=8) SB
    REAL (KIND=8) SIN_POWER_INT
    REAL (KIND=8) VALUE
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SIN_POWER_INT - Fatal error!'
        WRITE (*, '(a)') '  Power N < 0.'
        VALUE = 0.0D+00
        RETURN
    END IF
 
    SA = SIN (A)
    SB = SIN (B)
    CA = COS (A)
    CB = COS (B)
 
    IF (MOD(N, 2) == 0) THEN
 
        VALUE = B - A
        MLO = 2
    ELSE
        VALUE = CA - CB
        MLO = 3
    END IF
 
    DO M = MLO, N, 2
        VALUE = (REAL(M-1, KIND=8)*VALUE+SA**(M-1)*CA-SB**(M-1)*CB) / REAL (M, KIND=8)
    END DO
 
    SIN_POWER_INT = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SPHERE_UNIT_AREA_ND (DIM_NUM)
 
!*****************************************************************************80
!
!! SPHERE_UNIT_AREA_ND computes the surface area of a unit sphere in ND.
!
!  Discussion:
!
!    The unit sphere in ND satisfies:
!
!      sum ( 1 <= I <= DIM_NUM ) X(I) * X(I) = 1
!
!    Results for the first few values of N are:
!
!    DIM_NUM   Area
!
!     2    2        * PI
!     3    4        * PI
!     4  ( 2 /   1) * PI^2
!     5  ( 8 /   3) * PI^2
!     6  ( 1 /   1) * PI^3
!     7  (16 /  15) * PI^3
!     8  ( 1 /   3) * PI^4
!     9  (32 / 105) * PI^4
!    10  ( 1 /  12) * PI^5
!
!    For the unit sphere, Area(DIM_NUM) = DIM_NUM * Volume(DIM_NUM)
!
!    Sphere_Unit_Area ( DIM_NUM ) = 2 * PI^(DIM_NUM/2) / Gamma ( DIM_NUM / 2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIM_NUM, the dimension of the space.
!
!    Output, real ( kind = 8 ) SPHERE_UNIT_AREA_ND, the area of the sphere.
!
    IMPLICIT NONE
 
    REAL (KIND=8) AREA
    INTEGER (KIND=4) DIM_NUM
    INTEGER (KIND=4) I
    INTEGER (KIND=4) M
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) SPHERE_UNIT_AREA_ND
 
    IF (MOD(DIM_NUM, 2) == 0) THEN
        M = DIM_NUM / 2
        AREA = 2.0D+00 * (R8_PI) ** M
        DO I = 1, M - 1
            AREA = AREA / REAL (I, KIND=8)
        END DO
    ELSE
        M = (DIM_NUM-1) / 2
        AREA = (R8_PI) ** M * 2.0D+00 ** DIM_NUM
        DO I = M + 1, 2 * M
            AREA = AREA / REAL (I, KIND=8)
        END DO
    END IF
 
    SPHERE_UNIT_AREA_ND = AREA
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION STIRLING2_VALUE (N, M)
 
!*****************************************************************************80
!
!! STIRLING2_VALUE computes a Stirling number of the second kind.
!
!  Discussion:
!
!    S2(N,M) represents the number of distinct partitions of N elements
!    into M nonempty sets.  For a fixed N, the sum of the Stirling
!    numbers S2(N,M) is represented by B(N), called "Bell's number",
!    and represents the number of distinct partitions of N elements.
!
!    For example, with 4 objects, there are:
!
!    1 partition into 1 set:
!
!      (A,B,C,D)
!
!    7 partitions into 2 sets:
!
!      (A,B,C) (D)
!      (A,B,D) (C)
!      (A,C,D) (B)
!      (A) (B,C,D)
!      (A,B) (C,D)
!      (A,C) (B,D)
!      (A,D) (B,C)
!
!    6 partitions into 3 sets:
!
!      (A,B) (C) (D)
!      (A) (B,C) (D)
!      (A) (B) (C,D)
!      (A,C) (B) (D)
!      (A,D) (B) (C)
!      (A) (B,D) (C)
!
!    1 partition into 4 sets:
!
!      (A) (B) (C) (D)
!
!    So S2(4,1) = 1, S2(4,2) = 7, S2(4,3) = 6, S2(4,4) = 1, and B(4) = 15.
!
!
!  First terms:
!
!    N/M: 1    2    3    4    5    6    7    8
!
!    1    1    0    0    0    0    0    0    0
!    2    1    1    0    0    0    0    0    0
!    3    1    3    1    0    0    0    0    0
!    4    1    7    6    1    0    0    0    0
!    5    1   15   25   10    1    0    0    0
!    6    1   31   90   65   15    1    0    0
!    7    1   63  301  350  140   21    1    0
!    8    1  127  966 1701 1050  266   28    1
!
!  Recursion:
!
!    S2(N,1) = 1 for all N.
!    S2(I,I) = 1 for all I.
!    S2(I,J) = 0 if I < J.
!
!    S2(N,M) = M * S2(N-1,M) + S2(N-1,M-1)
!
!  Properties:
!
!    sum ( 1 <= K <= M ) S2(I,K) * S1(K,J) = Delta(I,J)
!
!    X^N = sum ( 0 <= K <= N ) S2(N,K) X_K
!    where X_K is the falling factorial function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of rows of the table.
!
!    Input, integer ( kind = 4 ) M, the number of columns of the table.
!
!    Output, integer ( kind = 4 ) STIRLING2_VALUE, the value of S2(N,M).
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    INTEGER (KIND=4) S2 (N, M)
    INTEGER (KIND=4) STIRLING2_VALUE
 
    IF (N <= 0) THEN
        STIRLING2_VALUE = 0
        RETURN
    END IF
 
    IF (M <= 0) THEN
        STIRLING2_VALUE = 0
        RETURN
    END IF
 
    S2 (1, 1) = 1
    S2 (1, 2:M) = 0
 
    DO I = 2, N
 
        S2 (I, 1) = 1
 
        DO J = 2, M
            S2 (I, J) = J * S2 (I-1, J) + S2 (I-1, J-1)
        END DO
 
    END DO
 
    STIRLING2_VALUE = S2 (N, M)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! STUDENT_CDF evaluates the central Student T CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    REAL (KIND=8) C
    REAL (KIND=8) C2
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    A2 = 0.5D+00 * C
    B2 = 0.5D+00
    C2 = C / (C+Y*Y)
 
    IF (Y <= 0.0D+00) THEN
        CDF = 0.5D+00 * BETA_INC (A2, B2, C2)
    ELSE
        CDF = 1.0D+00 - 0.5D+00 * BETA_INC (A2, B2, C2)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_CDF_VALUES (N_DATA, C, X, FX)
 
!*****************************************************************************80
!
!! STUDENT_CDF_VALUES returns some values of the Student CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = StudentTDistribution [ c ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 13
 
    REAL (KIND=8) C
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: C_VEC = (/ 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
   & 5.0D+00, 2.0D+00, 5.0D+00, 2.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.6000231200328521D+00, &
   & 0.6001080279134390D+00, 0.6001150934648930D+00, 0.6000995134721354D+00, &
   & 0.5999341989834830D+00, 0.7498859393137811D+00, 0.7500879487671045D+00, &
   & 0.9500004222186464D+00, 0.9499969138365968D+00, 0.9900012348724744D+00, &
   & 0.9900017619355059D+00, 0.9900004567580596D+00, 0.9900007637471291D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.325D+00, 0.289D+00, 0.277D+00, &
   & 0.271D+00, 0.267D+00, 0.816D+00, 0.727D+00, 2.920D+00, 2.015D+00, 6.965D+00, 4.541D+00, &
   & 3.747D+00, 3.365D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        C = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        C = C_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION STUDENT_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! STUDENT_CHECK checks the parameter of the central Student T CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL STUDENT_CHECK
 
    IF (B == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'STUDENT_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B must be nonzero.'
        STUDENT_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'STUDENT_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C must be greater than 0.'
        STUDENT_CHECK = .FALSE.
        RETURN
    END IF
 
    STUDENT_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! STUDENT_MEAN returns the mean of the central Student T PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! STUDENT_PDF evaluates the central Student T PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = Gamma ( (C+1)/2 ) /
!      ( Gamma ( C / 2 ) * Sqrt ( PI * C )
!      * ( 1 + ((X-A)/B)^2/C )^(C + 1/2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    Y = (X-A) / B
 
    PDF = GAMMA (0.5D+00*(C+1.0D+00)) / &
   & (SQRT(R8_PI*C)*GAMMA(0.5D+00*C)*SQRT((1.0D+00+Y*Y/C)**(2*C+1.0D+00)))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! STUDENT_SAMPLE samples the central Student T PDF.
!
!  Discussion:
!
!    For the sampling algorithm, it is necessary that 2 < C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2
    REAL (KIND=8) B
    REAL (KIND=8) B2
    REAL (KIND=8) C
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (C < 3.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'STUDENT_SAMPLE - Fatal error!'
        WRITE (*, '(a)') '  Sampling fails for C <= 2.'
        RETURN
    END IF
 
    A2 = 0.0D+00
    B2 = C / (C-2)
 
    CALL NORMAL_SAMPLE (A2, B2, SEED, X2)
 
    CALL CHI_SQUARE_SAMPLE (C, SEED, X3)
    X3 = X3 * C / (C-2.0D+00)
 
    X = A + B * X2 * SQRT (C) / X3
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! STUDENT_VARIANCE returns the variance of the central Student T PDF.
!
!  Discussion:
!
!    The variance is not defined unless 2 < C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    IF (C <= 2.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'STUDENT_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  Variance not defined for C <= 2.'
        RETURN
    END IF
 
    VARIANCE = B * B * C / (C-2.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_NONCENTRAL_CDF (X, IDF, D, CDF)
 
!*****************************************************************************80
!
!! STUDENT_NONCENTRAL_CDF evaluates the noncentral Student T CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    Original FORTRAN77 version by B E Cooper.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BE Cooper,
!    Algorithm AS 5:
!    The Integral of the Non-Central T-Distribution,
!    Applied Statistics,
!    Volume 17, 1968, page 193.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) IDF, the number of degrees of freedom.
!
!    Input, real ( kind = 8 ) D, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    INTEGER (KIND=4), PARAMETER :: A_MAX = 100
    REAL (KIND=8) AK
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) D
    REAL (KIND=8) DRB
    REAL (KIND=8), PARAMETER :: EMIN = 12.5D+00
    REAL (KIND=8) F
    REAL (KIND=8) FK
    REAL (KIND=8) FMKM1
    REAL (KIND=8) FMKM2
    INTEGER (KIND=4) IDF
    INTEGER (KIND=4) K
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) SUM2
    REAL (KIND=8) TEMP
    REAL (KIND=8) X
 
    F = REAL (IDF, KIND=8)
 
    IF (IDF == 1) THEN
 
        A = X / SQRT (F)
        B = F / (F+X**2)
        DRB = D * SQRT (B)
 
        CALL NORMAL_01_CDF (DRB, CDF2)
        CDF = 1.0D+00 - CDF2 + 2.0D+00 * TFN (DRB, A)
 
    ELSE IF (IDF <= A_MAX) THEN
 
        A = X / SQRT (F)
        B = F / (F+X*X)
        DRB = D * SQRT (B)
        SUM2 = 0.0D+00
 
        FMKM2 = 0.0D+00
        IF (ABS(DRB) < EMIN) THEN
            CALL NORMAL_01_CDF (A*DRB, CDF2)
            FMKM2 = A * SQRT (B) * EXP (-0.5D+00*DRB**2) * CDF2 / SQRT (2.0D+00*R8_PI)
        END IF
 
        FMKM1 = B * D * A * FMKM2
        IF (ABS(D) < EMIN) THEN
            FMKM1 = FMKM1 + 0.5D+00 * B * A * EXP (-0.5D+00*D**2) / R8_PI
        END IF
 
        IF (MOD(IDF, 2) == 0) THEN
            SUM2 = FMKM2
        ELSE
            SUM2 = FMKM1
        END IF
 
        AK = 1.0D+00
 
        DO K = 2, IDF - 2, 2
 
            FK = REAL (K, KIND=8)
 
            FMKM2 = B * (D*A*AK*FMKM1+FMKM2) * (FK-1.0D+00) / FK
 
            AK = 1.0D+00 / (AK*(FK-1.0D+00))
            FMKM1 = B * (D*A*AK*FMKM2+FMKM1) * FK / (FK+1.0D+00)
 
            IF (MOD(IDF, 2) == 0) THEN
                SUM2 = SUM2 + FMKM2
            ELSE
                SUM2 = SUM2 + FMKM1
            END IF
 
            AK = 1.0D+00 / (AK*FK)
 
        END DO
 
        IF (MOD(IDF, 2) == 0) THEN
            CALL NORMAL_01_CDF (D, CDF2)
            CDF = 1.0D+00 - CDF2 + SUM2 * SQRT (2.0D+00*R8_PI)
        ELSE
            CALL NORMAL_01_CDF (DRB, CDF2)
            CDF = 1.0D+00 - CDF2 + 2.0D+00 * (SUM2+TFN(DRB, A))
        END IF
!
!  Normal approximation.
!
    ELSE
 
        A = SQRT (0.5D+00*F) * EXP (LOG_GAMMA(0.5D+00*(F-1.0D+00))-LOG_GAMMA(0.5D+00*F)) * D
 
        TEMP = (X-A) / SQRT (F*(1.0D+00+D**2)/(F-2.0D+00)-A**2)
 
        CALL NORMAL_01_CDF (TEMP, CDF2)
        CDF = CDF2
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_NONCENTRAL_CDF_VALUES (N_DATA, DF, LAMBDA, X, FX)
 
!*****************************************************************************80
!
!! STUDENT_NONCENTRAL_CDF_VALUES returns values of the noncentral Student CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = NoncentralStudentTDistribution [ df, lambda ]
!      CDF [ dist, x ]
!
!    Mathematica seems to have some difficulty computing this function
!    to the desired number of digits.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) DF, real ( kind = 8 ) LAMBDA, the parameters
!    of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 30
 
    INTEGER (KIND=4) DF
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: DF_VEC = (/ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, &
   & 3, 1, 2, 3, 15, 20, 25, 1, 2, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.8975836176504333D+00, &
   & 0.9522670169D+00, 0.9711655571887813D+00, 0.8231218864D+00, 0.9049021510D+00, &
   & 0.9363471834D+00, 0.7301025986D+00, 0.8335594263D+00, 0.8774010255D+00, 0.5248571617D+00, &
   & 0.6293856597D+00, 0.6800271741D+00, 0.20590131975D+00, 0.2112148916D+00, 0.2074730718D+00, &
   & 0.9981130072D+00, 0.9994873850D+00, 0.9998391562D+00, 0.168610566972D+00, &
   & 0.16967950985D+00, 0.1701041003D+00, 0.9247683363D+00, 0.7483139269D+00, 0.4659802096D+00, &
   & 0.9761872541D+00, 0.8979689357D+00, 0.7181904627D+00, 0.9923658945D+00, 0.9610341649D+00, &
   & 0.8688007350D+00 /)
    REAL (KIND=8) LAMBDA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 4.0D+00, &
   & 4.0D+00, 4.0D+00, 7.0D+00, 7.0D+00, 7.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, &
   & 4.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 3.00D+00, 3.00D+00, 3.00D+00, &
   & 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, 3.00D+00, &
   & 3.00D+00, 3.00D+00, 3.00D+00, 15.00D+00, 15.00D+00, 15.00D+00, 0.05D+00, 0.05D+00, &
   & 0.05D+00, 4.00D+00, 4.00D+00, 4.00D+00, 5.00D+00, 5.00D+00, 5.00D+00, 6.00D+00, 6.00D+00, &
   & 6.00D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        DF = 0
        LAMBDA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        DF = DF_VEC (N_DATA)
        LAMBDA = LAMBDA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TFN (H, A)
 
!*****************************************************************************80
!
!! TFN calculates the T function of Owen.
!
!  Discussion:
!
!    Owen's T function is useful for computation of the bivariate normal
!    distribution and the distribution of a skewed normal distribution.
!
!    Although it was originally formulated in terms of the bivariate
!    normal function, the function can be defined more directly as
!
!      T(H,A) = 1 / ( 2 * pi ) *
!        Integral ( 0 <= X <= A ) e^( -H^2 * (1+X^2) / 2 ) / (1+X^2) dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by J C Young, C E Minder.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Donald Owen,
!    Tables for computing the bivariate normal distribution,
!    Annals of Mathematical Statistics,
!    Volume 27, pages 1075-1090, 1956.
!
!    JC Young, CE Minder,
!    Algorithm AS 76,
!    An Algorithm Useful in Calculating Non-Central T and
!    Bivariate Normal Distributions,
!    Applied Statistics,
!    Volume 23, Number 3, 1974, pages 455-457.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) H, A, the arguments of the T function.
!
!    Output, real ( kind = 8 ) TFN, the value of the T function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: NGAUSS = 10
 
    REAL (KIND=8) A
    REAL (KIND=8) AS
    REAL (KIND=8) H
    REAL (KIND=8) H1
    REAL (KIND=8) H2
    REAL (KIND=8) HS
    INTEGER (KIND=4) I
    REAL (KIND=8) RT
    REAL (KIND=8) TFN
    REAL (KIND=8), PARAMETER :: TWO_PI_INVERSE = 0.1591549430918953D+00
    REAL (KIND=8), PARAMETER :: TV1 = 1.0D-35
    REAL (KIND=8), PARAMETER :: TV2 = 15.0D+00
    REAL (KIND=8), PARAMETER :: TV3 = 15.0D+00
    REAL (KIND=8), PARAMETER :: TV4 = 1.0D-05
    REAL (KIND=8), PARAMETER, DIMENSION (NGAUSS) :: WEIGHT = (/ &
   & 0.666713443086881375935688098933D-01, 0.149451349150580593145776339658D+00, &
   & 0.219086362515982043995534934228D+00, 0.269266719309996355091226921569D+00, &
   & 0.295524224714752870173892994651D+00, 0.295524224714752870173892994651D+00, &
   & 0.269266719309996355091226921569D+00, 0.219086362515982043995534934228D+00, &
   & 0.149451349150580593145776339658D+00, 0.666713443086881375935688098933D-01 /)
    REAL (KIND=8) X
    REAL (KIND=8), PARAMETER, DIMENSION (NGAUSS) :: XTAB = (/ - &
   & 0.973906528517171720077964012084D+00, - 0.865063366688984510732096688423D+00, - &
   & 0.679409568299024406234327365115D+00, - 0.433395394129247190799265943166D+00, - &
   & 0.148874338981631210884826001130D+00, 0.148874338981631210884826001130D+00, &
   & 0.433395394129247190799265943166D+00, 0.679409568299024406234327365115D+00, &
   & 0.865063366688984510732096688423D+00, 0.973906528517171720077964012084D+00 /)
!
!  Test for H near zero.
!
    IF (ABS(H) < TV1) THEN
        TFN = ATAN (A) * TWO_PI_INVERSE
!
!  Test for large values of abs(H).
!
    ELSE IF (TV2 < ABS(H)) THEN
        TFN = 0.0D+00
!
!  Test for A near zero.
!
    ELSE IF (ABS(A) < TV1) THEN
        TFN = 0.0D+00
!
!  Test whether abs(A) is so large that it must be truncated.
!  If so, the truncated value of A is H2.
!
    ELSE
 
        HS = - 0.5D+00 * H * H
        H2 = A
        AS = A * A
!
!  Computation of truncation point by Newton iteration.
!
        IF (TV3 <= LOG(1.0D+00+AS)-HS*AS) THEN
 
            H1 = 0.5D+00 * A
            AS = 0.25D+00 * AS
 
            DO
 
                RT = AS + 1.0D+00
                H2 = H1 + (HS*AS+TV3-LOG(RT)) / (2.0D+00*H1*(1.0D+00/RT-HS))
                AS = H2 * H2
 
                IF (ABS(H2-H1) < TV4) THEN
                    EXIT
                END IF
 
                H1 = H2
 
            END DO
 
        END IF
!
!  Gaussian quadrature on the interval [0,H2].
!
        RT = 0.0D+00
        DO I = 1, NGAUSS
            X = 0.5D+00 * H2 * (XTAB(I)+1.0D+00)
            RT = RT + WEIGHT (I) * EXP (HS*(1.0D+00+X*X)) / (1.0D+00+X*X)
        END DO
 
        TFN = RT * (0.5D+00*H2) * TWO_PI_INVERSE
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TIMESTAMP ()
 
!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
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
 
    CHARACTER (LEN=8) AMPM
    INTEGER (KIND=4) D
    INTEGER (KIND=4) H
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MM
    CHARACTER (LEN=9), PARAMETER, DIMENSION (12) :: MONTH = (/ 'January  ', 'February ', 'March&
   &    ', 'April    ', 'May      ', 'June     ', 'July     ', 'August   ', 'September', 'Octob&
   &er  ', 'November ', 'December ' /)
    INTEGER (KIND=4) N
    INTEGER (KIND=4) S
    INTEGER (KIND=4) VALUES (8)
    INTEGER (KIND=4) Y
 
    CALL DATE_AND_TIME (VALUES=VALUES)
 
    Y = VALUES (1)
    M = VALUES (2)
    D = VALUES (3)
    H = VALUES (5)
    N = VALUES (6)
    S = VALUES (7)
    MM = VALUES (8)
 
    IF (H < 12) THEN
        AMPM = 'AM'
    ELSE IF (H == 12) THEN
        IF (N == 0 .AND. S == 0) THEN
            AMPM = 'Noon'
        ELSE
            AMPM = 'PM'
        END IF
    ELSE
        H = H - 12
        IF (H < 12) THEN
            AMPM = 'PM'
        ELSE IF (H == 12) THEN
            IF (N == 0 .AND. S == 0) THEN
                AMPM = 'Midnight'
            ELSE
                AMPM = 'AM'
            END IF
        END IF
    END IF
 
    WRITE (*, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)') D, TRIM (MONTH(M)), Y, H, '&
   &:', N, ':', S, '.', MM, TRIM (AMPM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! TRIANGLE_CDF evaluates the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
 
        CDF = 0.0D+00
 
    ELSE IF (X <= B) THEN
 
        IF (A == B) THEN
            CDF = 0.0D+00
        ELSE
            CDF = (X-A) * (X-A) / (B-A) / (C-A)
        END IF
 
    ELSE IF (X <= C) THEN
 
        CDF = (B-A) / (C-A) + (2.0D+00*C-B-X) * (X-B) / (C-B) / (C-A)
 
    ELSE
 
        CDF = 1.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! TRIANGLE_CDF_INV inverts the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF_MID
    REAL (KIND=8) D
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    D = 2.0D+00 / (C-A)
    CDF_MID = 0.5D+00 * D * (B-A)
 
    IF (CDF <= CDF_MID) THEN
        X = A + SQRT (CDF*(B-A)*(C-A))
    ELSE
        X = C - SQRT ((C-B)*((C-B)-(CDF-CDF_MID)*(C-A)))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRIANGLE_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! TRIANGLE_CHECK checks the parameters of the Triangle CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, logical TRIANGLE_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL TRIANGLE_CHECK
 
    IF (B < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < A.'
        TRIANGLE_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C < B) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C < B.'
        TRIANGLE_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (A == C) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A == C.'
        TRIANGLE_CHECK = .FALSE.
        RETURN
    END IF
 
    TRIANGLE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! TRIANGLE_MEAN returns the mean of the Triangle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the discrete uniform PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = A + (C+B-2.0D+00*A) / 3.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! TRIANGLE_PDF evaluates the Triangle PDF.
!
!  Discussion:
!
!    Given points A <= B <= C, the probability is 0 to the left of A,
!    rises linearly to a maximum of 2/(C-A) at B, drops linearly to zero
!    at C, and is zero for all values greater than C.
!
!    PDF(A,B,C;X)
!      = 2 * ( X - A ) / ( B - A ) / ( C - A ) for A <= X <= B
!      = 2 * ( C - X ) / ( C - B ) / ( C - A ) for B <= X <= C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
 
        PDF = 0.0D+00
 
    ELSE IF (X <= B) THEN
 
        IF (A == B) THEN
            PDF = 0.0D+00
        ELSE
            PDF = 2.0D+00 * (X-A) / (B-A) / (C-A)
        END IF
 
    ELSE IF (X <= C) THEN
 
        IF (B == C) THEN
            PDF = 0.0D+00
        ELSE
            PDF = 2.0D+00 * (C-X) / (C-B) / (C-A)
        END IF
 
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! TRIANGLE_SAMPLE samples the Triangle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL TRIANGLE_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! TRIANGLE_VARIANCE returns the variance of the Triangle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) VARIANCE
 
    VARIANCE = ((C-A)*(C-A)-(C-A)*(B-A)+(B-A)*(B-A)) / 18.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! TRIANGULAR_CDF evaluates the Triangular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        CDF = 0.0D+00
    ELSE IF (X <= 0.5D+00*(A+B)) THEN
        CDF = 2.0D+00 * (X**2-2.0D+00*A*X+A**2) / (B-A) ** 2
    ELSE IF (X <= B) THEN
        CDF = 0.5D+00 + (-2.0D+00*X**2+4.0D+00*B*X+0.5D+00*A**2-A*B-1.5D+00*B**2) / (B-A) ** 2
    ELSE
        CDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! TRIANGULAR_CDF_INV inverts the Triangular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGULAR_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF <= 0.5D+00) THEN
        X = A + 0.5D+00 * (B-A) * SQRT (2.0D+00*CDF)
    ELSE
        X = B - 0.5D+00 * (B-A) * SQRT (2.0D+00*(1.0D+00-CDF))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRIANGULAR_CHECK (A, B)
 
!*****************************************************************************80
!
!! TRIANGULAR_CHECK checks the parameters of the Triangular CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, logical TRIANGULAR_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL TRIANGULAR_CHECK
 
    IF (B <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGULAR_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= A.'
        TRIANGULAR_CHECK = .FALSE.
        RETURN
    END IF
 
    TRIANGULAR_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! TRIANGULAR_MEAN returns the mean of the Triangular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = 0.5D+00 * (A+B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! TRIANGULAR_PDF evaluates the Triangular PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = 4 * ( X - A ) / ( B - A )^2 for A <= X <= (A+B)/2
!               = 4 * ( B - X ) / ( B - A )^2 for (A+B)/2 <= X <= B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X <= A) THEN
        PDF = 0.0D+00
    ELSE IF (X <= 0.5D+00*(A+B)) THEN
        PDF = 4.0D+00 * (X-A) / (B-A) ** 2
    ELSE IF (X <= B) THEN
        PDF = 4.0D+00 * (B-X) / (B-A) ** 2
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! TRIANGULAR_SAMPLE samples the Triangular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL TRIANGULAR_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGULAR_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! TRIANGULAR_VARIANCE returns the variance of the Triangular PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (B-A) ** 2 / 24.0D+00
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRIGAMMA (X)
 
!*****************************************************************************80
!
!! TRIGAMMA calculates the TriGamma function.
!
!  Discussion:
!
!    TriGamma(x) = d^2 log ( Gamma ( x ) ) / dx^2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    FORTRAN77 original version by B Schneider
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BE Schneider,
!    Algorithm AS 121:
!    Trigamma Function,
!    Applied Statistics,
!    Volume 27, Number 1, page 97-99, 1978.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the trigamma function.
!    0 < X.
!
!    Output, real ( kind = 8 ) TRIGAMMA, the value of the
!    trigamma function at X.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: A = 0.0001D+00
    REAL (KIND=8), PARAMETER :: B = 5.0D+00
    REAL (KIND=8), PARAMETER :: B2 = 1.0D+00 / 6.0D+00
    REAL (KIND=8), PARAMETER :: B4 = - 1.0D+00 / 30.0D+00
    REAL (KIND=8), PARAMETER :: B6 = 1.0D+00 / 42.0D+00
    REAL (KIND=8), PARAMETER :: B8 = - 1.0D+00 / 30.0D+00
    REAL (KIND=8) TRIGAMMA
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
!
!  1): If X is not positive, fail.
!
    IF (X <= 0.0D+00) THEN
 
        TRIGAMMA = 0.0D+00
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIGAMMA - Fatal error!'
        WRITE (*, '(a)') '  X <= 0.'
        RETURN
!
!  2): If X is smaller than A, use a small value approximation.
!
    ELSE IF (X <= A) THEN
 
        TRIGAMMA = 1.0D+00 / X ** 2
!
!  3): Otherwise, increase the argument to B <= ( X + I ).
!
    ELSE
 
        Z = X
        TRIGAMMA = 0.0D+00
 
        DO WHILE (Z < B)
            TRIGAMMA = TRIGAMMA + 1.0D+00 / Z ** 2
            Z = Z + 1.0D+00
        END DO
!
!  ...and then apply an asymptotic formula.
!
        Y = 1.0D+00 / Z ** 2
 
        TRIGAMMA = TRIGAMMA + 0.5D+00 * Y + (1.0D+00+Y*(B2+Y*(B4+Y*(B6+Y*B8)))) / Z
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_CDF (X, CDF)
 
!*****************************************************************************80
!
!! UNIFORM_01_CDF evaluates the Uniform 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00) THEN
        CDF = 0.0D+00
    ELSE IF (1.0D+00 < X) THEN
        CDF = 1.0D+00
    ELSE
        CDF = X
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_CDF_INV (CDF, X)
 
!*****************************************************************************80
!
!! UNIFORM_01_CDF_INV inverts the Uniform 01 CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'UNIFORM_01_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = CDF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_MEAN (MEAN)
 
!*****************************************************************************80
!
!! UNIFORM_01_MEAN returns the mean of the Uniform 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) MEAN
 
    MEAN = 0.5D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_ORDER_SAMPLE (N, SEED, X)
 
!*****************************************************************************80
!
!! UNIFORM_01_ORDER_SAMPLE samples the Uniform 01 Order PDF.
!
!  Discussion:
!
!    In effect, this routine simply generates N samples of the
!    Uniform 01 PDF; but it generates them in order.  (Actually,
!    it generates them in descending order, but stores them in
!    the array in ascending order).  This saves the work of
!    sorting the results.  Moreover, if the order statistics
!    for another PDF are desired, and the inverse CDF is available,
!    then the desired values may be generated, presorted, by
!    calling this routine and using the results as input to the
!    inverse CDF routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 168.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the sample.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), N samples of the Uniform 01 PDF, in
!    ascending order.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) X (N)
 
    V = 1.0D+00
    DO I = N, 1, - 1
        U = R8_UNIFORM_01 (SEED)
        V = V * U ** (1.0D+00/REAL(I, KIND=8))
        X (I) = V
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_PDF (X, PDF)
 
!*****************************************************************************80
!
!! UNIFORM_01_PDF evaluates the Uniform 01 PDF.
!
!  Discussion:
!
!    PDF(X) = 1 for 0 <= X <= 1
!           = 0 otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X <= 1.0.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < 0.0D+00 .OR. 1.0D+00 < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 1.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION UNIFORM_01_SAMPLE (SEED)
 
!*****************************************************************************80
!
!! UNIFORM_01_SAMPLE is a portable random number generator.
!
!  Discussion:
!
!    SEED = SEED * (7^5) mod ( 2^31 - 1 )
!    UNIFORM_01_SAMPLE = SEED * / ( 2^31 - 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the integer "seed" used to
!    generate the output random number, and updated in preparation for the
!    next one.  SEED should not be zero.
!
!    Output, real ( kind = 8 ) UNIFORM_01_SAMPLE, a random value between 0
!    and 1.
!
!  Local parameters:
!
!    IA = 7^5
!    IB = 2^15
!    IB16 = 2^16
!    IP = 2^31-1
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: IA = 16807
    INTEGER (KIND=4), PARAMETER :: IB15 = 32768
    INTEGER (KIND=4), PARAMETER :: IB16 = 65536
    INTEGER (KIND=4), PARAMETER :: IP = 2147483647
    INTEGER (KIND=4) IPRHI
    INTEGER (KIND=4) IXHI
    INTEGER (KIND=4) K
    INTEGER (KIND=4) LEFTLO
    INTEGER (KIND=4) LOXA
    INTEGER (KIND=4) SEED
    REAL (KIND=8) UNIFORM_01_SAMPLE
!
!  Don't let SEED be 0 or IP
!
    IF (SEED == 0 .OR. SEED == IP) THEN
        SEED = IP / 2
    END IF
!
!  Get the 15 high order bits of SEED.
!
    IXHI = SEED / IB16
!
!  Get the 16 low bits of SEED and form the low product.
!
    LOXA = (SEED-IXHI*IB16) * IA
!
!  Get the 15 high order bits of the low product.
!
    LEFTLO = LOXA / IB16
!
!  Form the 31 highest bits of the full product.
!
    IPRHI = IXHI * IA + LEFTLO
!
!  Get overflow past the 31st bit of full product.
!
    K = IPRHI / IB15
!
!  Assemble all the parts and presubtract IP.  The parentheses are essential.
!
    SEED = (((LOXA-LEFTLO*IB16)-IP)+(IPRHI-K*IB15)*IB16) + K
!
!  Add IP back in if necessary.
!
    IF (SEED < 0) THEN
        SEED = SEED + IP
    END IF
!
!  Multiply by 1 / (2^31-1).
!
    UNIFORM_01_SAMPLE = REAL (SEED, KIND=8) * 4.656612875D-10
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_01_VARIANCE (VARIANCE)
 
!*****************************************************************************80
!
!! UNIFORM_01_VARIANCE returns the variance of the Uniform 01 PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) VARIANCE
 
    VARIANCE = 1.0D+00 / 12.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! UNIFORM_CDF evaluates the Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (X < A) THEN
        CDF = 0.0D+00
    ELSE IF (B < X) THEN
        CDF = 1.0D+00
    ELSE
        CDF = (X-A) / (B-A)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! UNIFORM_CDF_INV inverts the Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'UNIFORM_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A + (B-A) * CDF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION UNIFORM_CHECK (A, B)
 
!*****************************************************************************80
!
!! UNIFORM_CHECK checks the parameters of the Uniform CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, logical UNIFORM_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL UNIFORM_CHECK
 
    IF (B <= A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'UNIFORM_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= A.'
        UNIFORM_CHECK = .FALSE.
        RETURN
    END IF
 
    UNIFORM_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! UNIFORM_MEAN returns the mean of the Uniform PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = 0.5D+00 * (A+B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! UNIFORM_PDF evaluates the Uniform PDF.
!
!  Discussion:
!
!    The Uniform PDF is also known as the "Rectangular" or "de Moivre" PDF.
!
!    PDF(A,B;X) = 1 / ( B - A ) for A <= X <= B
!               = 0 otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8) X
 
    IF (X < A .OR. B < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 1.0D+00 / (B-A)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! UNIFORM_SAMPLE samples the Uniform PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL UNIFORM_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! UNIFORM_VARIANCE returns the variance of the Uniform PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = (B-A) ** 2 / 12.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CDF evaluates the Uniform Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X < A) THEN
        CDF = 0.0D+00
    ELSE IF (B < X) THEN
        CDF = 1.0D+00
    ELSE
        CDF = REAL (X+1-A, KIND=8) / REAL (B+1-A, KIND=8)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CDF_INV inverts the Uniform Discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, integer ( kind = 4 ) X, the smallest argument whose CDF is greater
!    than or equal to CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    REAL (KIND=8) A2
    INTEGER (KIND=4) B
    REAL (KIND=8) B2
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
    REAL (KIND=8) X2
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'UNIFORM_DISCRETE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    A2 = REAL (A, KIND=8) - 0.5D+00
    B2 = REAL (B, KIND=8) + 0.5D+00
    X2 = A + CDF * (B2-A2)
 
    X = NINT (X2)
 
    X = MAX (X, A)
    X = MIN (X, B)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION UNIFORM_DISCRETE_CHECK (A, B)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_CHECK checks the parameters of the Uniform discrete CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, logical UNIFORM_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    LOGICAL UNIFORM_DISCRETE_CHECK
 
    IF (B < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'UNIFORM_DISCRETE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < A.'
        UNIFORM_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    UNIFORM_DISCRETE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_MEAN returns the mean of the Uniform discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    REAL (KIND=8) MEAN
 
    MEAN = 0.5D+00 * REAL (A+B, KIND=8)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_PDF evaluates the Uniform discrete PDF.
!
!  Discussion:
!
!    The Uniform Discrete PDF is also known as the "Rectangular"
!    Discrete PDF.
!
!    PDF(A,B;X) = 1 / ( B + 1 - A ) for A <= X <= B.
!
!    The parameters define the interval of integers
!    for which the PDF is nonzero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < A .OR. B < X) THEN
        PDF = 0.0D+00
    ELSE
        PDF = 1.0D+00 / REAL (B+1-A, KIND=8)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_SAMPLE samples the Uniform discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL UNIFORM_DISCRETE_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_DISCRETE_VARIANCE (A, B, VARIANCE)
 
!*****************************************************************************80
!
!! UNIFORM_DISCRETE_VARIANCE returns the variance of the Uniform discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4) B
    REAL (KIND=8) VARIANCE
 
    VARIANCE = REAL ((B+1-A)**2-1, KIND=8) / 12.0D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE UNIFORM_NSPHERE_SAMPLE (N, SEED, X)
 
!*****************************************************************************80
!
!! UNIFORM_NSPHERE_SAMPLE samples the Uniform Unit Sphere PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 168.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of the sphere.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a point on the unit N sphere, chosen
!    with a uniform probability.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X (N)
 
    DO I = 1, N
        CALL NORMAL_01_SAMPLE (SEED, X(I))
    END DO
 
    X (1:N) = X (1:N) / SQRT (SUM(X(1:N)**2))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! VON_MISES_CDF evaluates the von Mises CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2005
!
!  Author:
!
!    Original FORTRAN77 version by Geoffrey Hill.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Geoffrey Hill,
!    Algorithm 518,
!    Incomplete Bessel Function I0: The von Mises Distribution,
!    ACM Transactions on Mathematical Software,
!    Volume 3, Number 3, September 1977, pages 279-284.
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: A1 = 12.0D+00
    REAL (KIND=8), PARAMETER :: A2 = 0.8D+00
    REAL (KIND=8), PARAMETER :: A3 = 8.0D+00
    REAL (KIND=8), PARAMETER :: A4 = 1.0D+00
    REAL (KIND=8) ARG
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: C1 = 56.0D+00
    REAL (KIND=8) CDF
    REAL (KIND=8), PARAMETER :: CK = 10.5D+00
    REAL (KIND=8) CN
    REAL (KIND=8) ERFX
    INTEGER (KIND=4) IP
    INTEGER (KIND=4) N
    REAL (KIND=8) P
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) R
    REAL (KIND=8) S
    REAL (KIND=8) SN
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
!
!  We expect -PI <= X - A <= PI.
!
    IF (X-A <=-R8_PI) THEN
        CDF = 0.0D+00
        RETURN
    END IF
 
    IF (R8_PI <= X-A) THEN
        CDF = 1.0D+00
        RETURN
    END IF
!
!  Convert the angle (X - A) modulo 2 PI to the range ( 0, 2 * PI ).
!
    Z = B
 
    U = MOD (X-A+R8_PI, 2.0D+00*R8_PI)
 
    IF (U < 0.0D+00) THEN
        U = U + 2.0D+00 * R8_PI
    END IF
 
    Y = U - R8_PI
!
!  For small B, sum IP terms by backwards recursion.
!
    IF (Z <= CK) THEN
 
        V = 0.0D+00
 
        IF (0.0D+00 < Z) THEN
 
            IP = INT (Z*A2-A3/(Z+A4)+A1)
            P = REAL (IP, KIND=8)
            S = SIN (Y)
            C = COS (Y)
            Y = P * Y
            SN = SIN (Y)
            CN = COS (Y)
            R = 0.0D+00
            Z = 2.0D+00 / Z
 
            DO N = 2, IP
                P = P - 1.0D+00
                Y = SN
                SN = SN * C - CN * S
                CN = CN * C + Y * S
                R = 1.0D+00 / (P*Z+R)
                V = (SN/P+V) * R
            END DO
 
        END IF
 
        CDF = (U*0.5D+00+V) / R8_PI
!
!  For large B, compute the normal approximation and left tail.
!
    ELSE
 
        C = 24.0D+00 * Z
        V = C - C1
        R = SQRT ((54.0D+00/(347.0D+00/V+26.0D+00-C)-6.0D+00+C)/12.0D+00)
        Z = SIN (0.5D+00*Y) * R
        S = 2.0D+00 * Z ** 2
        V = V - S + 3.0D+00
        Y = (C-S-S-16.0D+00) / 3.0D+00
        Y = ((S+1.75D+00)*S+83.5D+00) / V - Y
        ARG = Z * (1.0D+00-S/Y**2)
        ERFX = R8_ERROR_F (ARG)
        CDF = 0.5D+00 * ERFX + 0.5D+00
 
    END IF
 
    CDF = MAX (CDF, 0.0D+00)
    CDF = MIN (CDF, 1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! VON_MISES_CDF_INV inverts the von Mises CDF.
!
!  Discussion:
!
!    A simple bisection method is used on the interval [ A - PI, A + PI ].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!    A - PI <= X <= A + PI.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF1
    REAL (KIND=8) CDF2
    REAL (KIND=8) CDF3
    INTEGER (KIND=4) IT
    INTEGER (KIND=4), PARAMETER :: IT_MAX = 100
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8), PARAMETER :: TOL = 0.000001D+00
    REAL (KIND=8) X
    REAL (KIND=8) X1
    REAL (KIND=8) X2
    REAL (KIND=8) X3
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'VON_MISES_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    IF (CDF == 0.0D+00) THEN
        X = A - R8_PI
        RETURN
    ELSE IF (1.0D+00 == CDF) THEN
        X = A + R8_PI
        RETURN
    END IF
 
    X1 = A - R8_PI
    CDF1 = 0.0D+00
 
    X2 = A + R8_PI
    CDF2 = 1.0D+00
!
!  Now use bisection.
!
    IT = 0
 
    DO
 
        IT = IT + 1
 
        X3 = 0.5D+00 * (X1+X2)
        CALL VON_MISES_CDF (X3, A, B, CDF3)
 
        IF (ABS(CDF3-CDF) < TOL) THEN
            X = X3
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'VON_MISES_CDF_INV - Fatal error!'
            WRITE (*, '(a)') '  Iteration limit exceeded.'
            RETURN
        END IF
 
        IF (SIGN(1.0D+00, CDF3-CDF) == SIGN(1.0D+00, CDF1-CDF)) THEN
            X1 = X3
            CDF1 = CDF3
        ELSE
            X2 = X3
            CDF2 = CDF3
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_CDF_VALUES (N_DATA, A, B, X, FX)
 
!*****************************************************************************80
!
!! VON_MISES_CDF_VALUES returns some values of the von Mises CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    QA276.M335
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Output, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 23
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.0D+00, 0.1D+01, 0.1D+01, 0.1D+01, 0.1D+01, 0.1D+01, 0.1D+01, - 0.2D+01, - 0.1D+01, &
   & 0.0D+01, 0.1D+01, 0.2D+01, 0.3D+01, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00 &
   & /)
    REAL (KIND=8) B
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.1D+01, 0.1D+01, 0.1D+01, 0.1D+01, &
   & 0.1D+01, 0.2D+01, 0.2D+01, 0.2D+01, 0.2D+01, 0.2D+01, 0.2D+01, 0.3D+01, 0.3D+01, 0.3D+01, &
   & 0.3D+01, 0.3D+01, 0.3D+01, 0.0D+00, 0.1D+01, 0.2D+01, 0.3D+01, 0.4D+01, 0.5D+01 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.2535089956281180D-01, &
   & 0.1097539041177346D+00, 0.5000000000000000D+00, 0.8043381312498558D+00, &
   & 0.9417460124555197D+00, 0.5000000000000000D+00, 0.6018204118446155D+00, &
   & 0.6959356933122230D+00, 0.7765935901304593D+00, 0.8410725934916615D+00, &
   & 0.8895777369550366D+00, 0.9960322705517925D+00, 0.9404336090170247D+00, &
   & 0.5000000000000000D+00, 0.5956639098297530D-01, 0.3967729448207649D-02, &
   & 0.2321953958111930D-03, 0.6250000000000000D+00, 0.7438406999109122D+00, &
   & 0.8369224904294019D+00, 0.8941711407897124D+00, 0.9291058600568743D+00, &
   & 0.9514289900655436D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ - 0.2617993977991494D+01, - &
   & 0.1570796326794897D+01, 0.0000000000000000D+00, 0.1047197551196598D+01, &
   & 0.2094395102393195D+01, 0.1000000000000000D+01, 0.1200000000000000D+01, &
   & 0.1400000000000000D+01, 0.1600000000000000D+01, 0.1800000000000000D+01, &
   & 0.2000000000000000D+01, 0.0000000000000000D+00, 0.0000000000000000D+00, &
   & 0.0000000000000000D+00, 0.0000000000000000D+00, 0.0000000000000000D+00, &
   & 0.0000000000000000D+00, 0.7853981633974483D+00, 0.7853981633974483D+00, &
   & 0.7853981633974483D+00, 0.7853981633974483D+00, 0.7853981633974483D+00, &
   & 0.7853981633974483D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION VON_MISES_CHECK (A, B)
 
!*****************************************************************************80
!
!! VON_MISES_CHECK checks the parameters of the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, logical VON_MISES_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    LOGICAL VON_MISES_CHECK
 
    IF (A <-R8_PI .OR. R8_PI < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'VON_MISES_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < -PI or PI < A.'
        VON_MISES_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'VON_MISES_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B < 0.0'
        VON_MISES_CHECK = .FALSE.
        RETURN
    END IF
 
    VON_MISES_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_CIRCULAR_VARIANCE (A, B, CIRCULAR_VARIANCE)
 
!*****************************************************************************80
!
!! VON_MISES_CIRCULAR_VARIANCE: circular variance of the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) CIRCULAR_VARIANCE, the circular variance
!    of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CIRCULAR_VARIANCE
 
    CIRCULAR_VARIANCE = 1.0D+00 - BESSEL_I1 (B) / BESSEL_I0 (B)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_MEAN (A, B, MEAN)
 
!*****************************************************************************80
!
!! VON_MISES_MEAN returns the mean of the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) MEAN
 
    MEAN = A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! VON_MISES_PDF evaluates the von Mises PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = EXP ( B * COS ( X - A ) ) / ( 2 * PI * I0(B) )
!
!    where:
!
!      I0(*) is the modified Bessel function of the first
!      kind of order 0.
!
!    The von Mises distribution for points on the unit circle is
!    analogous to the normal distribution of points on a line.
!    The variable X is interpreted as a deviation from the angle A,
!    with B controlling the amount of dispersion.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 160.
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!    Merran Evans, Nicholas Hastings, Brian Peacock,
!    Statistical Distributions,
!    Wiley, 2000,
!    LC: QA273.6.E92, pages 189-191.
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    LC: QA276.M335
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A - PI <= X <= A + PI.
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) X
 
    IF (X < A-R8_PI) THEN
        PDF = 0.0D+00
    ELSE IF (X <= A+R8_PI) THEN
        PDF = EXP (B*COS(X-A)) / (2.0D+00*R8_PI*BESSEL_I0(B))
    ELSE
        PDF = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VON_MISES_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! VON_MISES_SAMPLE samples the von Mises PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Best, Nicholas Fisher,
!    Efficient Simulation of the von Mises Distribution,
!    Applied Statistics,
!    Volume 28, Number 2, pages 152-157.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) F
    REAL (KIND=8), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) R
    REAL (KIND=8) RHO
    INTEGER (KIND=4) SEED
    REAL (KIND=8) TAU
    REAL (KIND=8) U1
    REAL (KIND=8) U2
    REAL (KIND=8) U3
    REAL (KIND=8) X
    REAL (KIND=8) Z
 
    TAU = 1.0D+00 + SQRT (1.0D+00+4.0D+00*B*B)
    RHO = (TAU-SQRT(2.0D+00*TAU)) / (2.0D+00*B)
    R = (1.0D+00+RHO**2) / (2.0D+00*RHO)
 
    DO
 
        U1 = R8_UNIFORM_01 (SEED)
        Z = COS (R8_PI*U1)
        F = (1.0D+00+R*Z) / (R+Z)
        C = B * (R-F)
 
        U2 = R8_UNIFORM_01 (SEED)
 
        IF (U2 < C*(2.0D+00-C)) THEN
            EXIT
        END IF
 
        IF (C <= LOG(C/U2)+1.0D+00) THEN
            EXIT
        END IF
 
    END DO
 
    U3 = R8_UNIFORM_01 (SEED)
 
    X = A + SIGN (1.0D+00, U3-0.5D+00) * ACOS (F)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_CDF (X, A, B, C, CDF)
 
!*****************************************************************************80
!
!! WEIBULL_CDF evaluates the Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!    A <= X.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X < A) THEN
        CDF = 0.0D+00
    ELSE
        Y = (X-A) / B
        CDF = 1.0D+00 - 1.0D+00 / EXP (Y**C)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_CDF_INV (CDF, A, B, C, X)
 
!*****************************************************************************80
!
!! WEIBULL_CDF_INV inverts the Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 < CDF < 1.0.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) X, the corresponding argument of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = A + B * (-LOG(1.0D+00-CDF)) ** (1.0D+00/C)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_CDF_VALUES (N_DATA, ALPHA, BETA, X, FX)
 
!*****************************************************************************80
!
!! WEIBULL_CDF_VALUES returns some values of the Weibull CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = WeibullDistribution [ alpha, beta ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) ALPHA, the first parameter of the distribution.
!
!    Output, real ( kind = 8 ) BETA, the second parameter of the distribution.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 12
 
    REAL (KIND=8) ALPHA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: ALPHA_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.3000000000000000D+01, &
   & 0.4000000000000000D+01, 0.5000000000000000D+01 /)
    REAL (KIND=8) BETA
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: BETA_VEC = (/ 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.5000000000000000D+00, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.5000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.8646647167633873D+00, &
   & 0.9816843611112658D+00, 0.9975212478233336D+00, 0.9996645373720975D+00, &
   & 0.6321205588285577D+00, 0.4865828809674080D+00, 0.3934693402873666D+00, &
   & 0.3296799539643607D+00, 0.8946007754381357D+00, 0.9657818816883340D+00, &
   & 0.9936702845725143D+00, 0.9994964109502630D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.1000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.4000000000000000D+01, &
   & 0.2000000000000000D+01, 0.2000000000000000D+01, 0.2000000000000000D+01, &
   & 0.2000000000000000D+01, 0.3000000000000000D+01, 0.3000000000000000D+01, &
   & 0.3000000000000000D+01, 0.3000000000000000D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        ALPHA = 0.0D+00
        BETA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        ALPHA = ALPHA_VEC (N_DATA)
        BETA = BETA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION WEIBULL_CHECK (A, B, C)
 
!*****************************************************************************80
!
!! WEIBULL_CHECK checks the parameters of the Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, logical WEIBULL_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    LOGICAL WEIBULL_CHECK
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        WEIBULL_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (C <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_CHECK - Fatal error!'
        WRITE (*, '(a)') '  C <= 0.'
        WEIBULL_CHECK = .FALSE.
        RETURN
    END IF
 
    WEIBULL_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_MEAN (A, B, C, MEAN)
 
!*****************************************************************************80
!
!! WEIBULL_MEAN returns the mean of the Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) MEAN
 
    MEAN = B * GAMMA ((C+1.0D+00)/C) + A
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_PDF (X, A, B, C, PDF)
 
!*****************************************************************************80
!
!! WEIBULL_PDF evaluates the Weibull PDF.
!
!  Discussion:
!
!    PDF(A,B,C;X) = ( C / B ) * ( ( X - A ) / B )^( C - 1 )
!     * EXP ( - ( ( X - A ) / B )^C ).
!
!    The Weibull PDF is also known as the Frechet PDF.
!
!    WEIBULL_PDF(A,B,1;X) is the Exponential PDF.
!
!    WEIBULL_PDF(0,1,2;X) is the Rayleigh PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) PDF
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X < A) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        Y = (X-A) / B
 
        PDF = (C/B) * Y ** (C-1.0D+00) / EXP (Y**C)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_SAMPLE (A, B, C, SEED, X)
 
!*****************************************************************************80
!
!! WEIBULL_SAMPLE samples the Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    REAL (KIND=8) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL WEIBULL_CDF_INV (CDF, A, B, C, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_VARIANCE (A, B, C, VARIANCE)
 
!*****************************************************************************80
!
!! WEIBULL_VARIANCE returns the variance of the Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) G1
    REAL (KIND=8) G2
    REAL (KIND=8) VARIANCE
 
    G1 = GAMMA ((C+2.0D+00)/C)
    G2 = GAMMA ((C+1.0D+00)/C)
 
    VARIANCE = B * B * (G1-G2*G2)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_DISCRETE_CDF (X, A, B, CDF)
 
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF evaluates the Discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!    0 <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        CDF = 0.0D+00
    ELSE
        CDF = 1.0D+00 - (1.0D+00-A) ** ((X+1)**B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_DISCRETE_CDF_INV (CDF, A, B, X)
 
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CDF_INV inverts the Discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, integer ( kind = 4 ) X, the corresponding argument.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) X
 
    IF (CDF < 0.0D+00 .OR. 1.0D+00 < CDF) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_DISCRETE_CDF_INV - Fatal error!'
        WRITE (*, '(a)') '  CDF < 0 or 1 < CDF.'
        RETURN
    END IF
 
    X = R8_CEILING ((LOG(1.0D+00-CDF)/LOG(1.0D+00-A))**(1.0D+00/B)-1.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION WEIBULL_DISCRETE_CHECK (A, B)
 
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CHECK checks the parameters of the discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, logical WEIBULL_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    LOGICAL WEIBULL_DISCRETE_CHECK
 
    IF (A < 0.0D+00 .OR. 1.0D+00 < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_DISCRETE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A < 0 or 1 < A.'
        WEIBULL_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    IF (B <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'WEIBULL_DISCRETE_CHECK - Fatal error!'
        WRITE (*, '(a)') '  B <= 0.'
        WEIBULL_DISCRETE_CHECK = .FALSE.
        RETURN
    END IF
 
    WEIBULL_DISCRETE_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_DISCRETE_PDF (X, A, B, PDF)
 
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_PDF evaluates the discrete Weibull PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = ( 1 - A )^X^B - ( 1 - A )^(X+1)^B.
!
!    WEIBULL_DISCRETE_PDF(A,1;X) = GEOMETRIC_PDF(A;X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    0 <= X
!
!    Input, real ( kind = 8 ) A, B, the parameters that define the PDF.
!    0 <= A <= 1,
!    0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 0) THEN
        PDF = 0.0D+00
    ELSE
        PDF = (1.0D+00-A) ** (X**B) - (1.0D+00-A) ** ((X+1)**B)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE WEIBULL_DISCRETE_SAMPLE (A, B, SEED, X)
 
!*****************************************************************************80
!
!! WEIBULL_DISCRETE_SAMPLE samples the discrete Weibull PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CDF
    INTEGER (KIND=4) SEED
    INTEGER (KIND=4) X
 
    CDF = R8_UNIFORM_01 (SEED)
 
    CALL WEIBULL_DISCRETE_CDF_INV (CDF, A, B, X)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_CDF (X, A, CDF)
 
!*****************************************************************************80
!
!! ZIPF_CDF evaluates the Zipf CDF.
!
!  Discussion:
!
!    Simple summation is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    1 <= N
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE :: ASAVE = 0.0D+00
    REAL (KIND=8), SAVE :: C = 0.0D+00
    REAL (KIND=8) CDF
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) Y
 
    IF (X < 1) THEN
 
        CDF = 0.0D+00
 
    ELSE
 
        IF (A /= ASAVE) THEN
 
            C = R8_ZETA (A)
            ASAVE = A
 
        END IF
 
        CDF = 0.0D+00
        DO Y = 1, X
            PDF = (1.0D+00/REAL(Y, KIND=8)**A) / C
            CDF = CDF + PDF
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_CDF_INV (A, CDF, X)
 
!*****************************************************************************80
!
!! ZIPF_CDF_INV inverts the Zipf CDF.
!
!  Discussion:
!
!    Simple summation is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0 < A.
!
!    Output, integer ( kind = 4 ) X, the argument such that
!    CDF(X-1) < CDF <= CDF(X)
!    1 <= X <= 1000
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) C
    REAL (KIND=8) CDF
    REAL (KIND=8) CDF2
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
    INTEGER (KIND=4) Y
 
    IF (CDF <= 0.0) THEN
 
        X = 1
 
    ELSE
 
        C = R8_ZETA (A)
        CDF2 = 0.0D+00
 
        X = 1000
 
        DO Y = 1, 1000
            PDF = (1.0D+00/Y**A) / C
            CDF2 = CDF2 + PDF
            IF (CDF <= CDF2) THEN
                X = Y
                EXIT
            END IF
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ZIPF_CHECK (A)
 
!*****************************************************************************80
!
!! ZIPF_CHECK checks the parameter of the Zipf PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, logical ZIPF_CHECK, is true if the parameters are legal.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    LOGICAL ZIPF_CHECK
 
    IF (A <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ZIPF_CHECK - Fatal error!'
        WRITE (*, '(a)') '  A <= 1.'
        ZIPF_CHECK = .FALSE.
        RETURN
    END IF
 
    ZIPF_CHECK = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_MEAN (A, MEAN)
 
!*****************************************************************************80
!
!! ZIPF_MEAN returns the mean of the Zipf PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!    The mean is only defined for 2 < A.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
 
    IF (A <= 2.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ZIPF_MEAN - Fatal error!'
        WRITE (*, '(a)') '  No mean defined for A <= 2.'
        RETURN
    END IF
 
    MEAN = R8_ZETA (A-1.0D+00) / R8_ZETA (A)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_PDF (X, A, PDF)
 
!*****************************************************************************80
!
!! ZIPF_PDF evaluates the Zipf PDF.
!
!  Discussion:
!
!    PDF(A;X) = ( 1 / X^A ) / C
!
!    where the normalizing constant is chosen so that
!
!    C = Sum ( 1 <= I < Infinity ) 1 / I^A.
!
!    From observation, the frequency of different words in long
!    sequences of text seems to follow the Zipf PDF, with
!    parameter A slightly greater than 1.  The Zipf PDF is sometimes
!    known as the "discrete Pareto" PDF.
!
!    Lotka's law is a version of the Zipf PDF in which A is 2 or approximately
!    2.  Lotka's law describes the frequency of publications by authors in a
!    given field, and estimates that the number of authors with X papers is
!    about 1/X^A of the number of authors with 1 paper.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alfred Lotka,
!    The frequency distribution of scientific productivity,
!    Journal of the Washington Academy of Sciences,
!    Volume 16, Number 12, 1926, pages 317-324.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    1 <= N
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE :: ASAVE = 0.0D+00
    REAL (KIND=8), SAVE :: C = 0.0D+00
    REAL (KIND=8) PDF
    INTEGER (KIND=4) X
 
    IF (X < 1) THEN
 
        PDF = 0.0D+00
 
    ELSE
 
        IF (A /= ASAVE) THEN
 
            C = R8_ZETA (A)
            ASAVE = A
 
        END IF
 
        PDF = (1.0D+00/REAL(X, KIND=8)**A) / C
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_SAMPLE (A, SEED, X)
 
!*****************************************************************************80
!
!! ZIPF_SAMPLE samples the Zipf PDF.
!
!  Discussion:
!
!    I am concerned that there may be a discrepancy in the results
!    of this routine, which do not seem to have the predicted variances.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer Verlag, 1986, pages 550-551.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    INTEGER (KIND=4) SEED
    REAL (KIND=8) T
    REAL (KIND=8) TEST
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    INTEGER (KIND=4) X
 
    TEST = REAL (I4_HUGE(), KIND=8)
 
    B = 2.0D+00 ** (A-1.0D+00)
 
    DO
 
        U = R8_UNIFORM_01 (SEED)
        V = R8_UNIFORM_01 (SEED)
        W = AINT (1.0D+00/U**(1.0D+00/(A-1.0D+00)))
!
!  Very small values of U can cause W to be very large,
!  bigger than the largest integer...
!
        IF (TEST < W) THEN
            CYCLE
        END IF
 
        T = ((W+1.0D+00)/W) ** (A-1.0D+00)
 
        IF (V*W*(T-1.0D+00)*B <= T*(B-1.0D+00)) THEN
            EXIT
        END IF
 
    END DO
 
    X = INT (W)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZIPF_VARIANCE (A, VARIANCE)
 
!*****************************************************************************80
!
!! ZIPF_VARIANCE returns the variance of the Zipf PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!    The variance is only defined for 3 < A.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) MEAN
    REAL (KIND=8) VARIANCE
 
    IF (A <= 3.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ZIPF_VARIANCE - Fatal error!'
        WRITE (*, '(a)') '  No variance defined for A <= 3.0.'
        RETURN
    END IF
 
    CALL ZIPF_MEAN (A, MEAN)
 
    VARIANCE = R8_ZETA (A-2.0D+00) / R8_ZETA (A) - MEAN * MEAN
 
    RETURN
END
 
!******************************************************************************

END MODULE ModLib_Probability

!******************************************************************************

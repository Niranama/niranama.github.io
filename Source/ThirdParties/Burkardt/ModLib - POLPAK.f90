
MODULE ModLib_PolPak
  
!** PURPOSE OF THIS MODULE:
    ! contains routines that evaluate the recursively defined polynomial families of
    ! - Bernoulli
    ! - Bernstein
    ! - Cardan
    ! - Charlier
    ! - Chebyshev
    ! - Euler
    ! - Gegenbauer
    ! - Hermite
    ! - Jacobi
    ! - Krawtchouk
    ! - Laguerre
    ! - Legendre
    ! - Meixner
    ! - Zernike

!** REFERENCES:
    ! These routines are from POLPAK package by John Burkardt

!** USE STATEMENTS:
    USE ModLib_SpecFunc,    ONLY: LGAMMA => GAMMA_LOG

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_PolPak'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
 
SUBROUTINE AGM_VALUES (N_DATA, A, B, FX)
 
!*****************************************************************************80
!
!! agm_values() returns some values of the arithmetic-geometry mean (AGM).
!
!  Discussion:
!
!    The AGM is defined for nonnegative A and B.
!
!    The AGM of numbers A and B is defined by setting
!
!      A(0) = A,
!      B(0) = B
!
!      A(N+1) = ( A(N) + B(N) ) / 2
!      B(N+1) = sqrt ( A(N) * B(N) )
!
!    The two sequences both converge to AGM(A,B).
!
!    In Mathematica, the AGM can be evaluated by
!
!      ArithmeticGeometricMean [ a, b ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2008
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    real ( kind = rk ) A, B, the arguments of the function.
!
!    real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 15
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 22.0D+00, 83.0D+00, 42.0D+00, &
   & 26.0D+00, 4.0D+00, 6.0D+00, 40.0D+00, 80.0D+00, 90.0D+00, 9.0D+00, 53.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.5D+00 /)
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 96.0D+00, 56.0D+00, 7.0D+00, &
   & 11.0D+00, 63.0D+00, 45.0D+00, 75.0D+00, 0.0D+00, 35.0D+00, 1.0D+00, 53.0D+00, 2.0D+00, &
   & 4.0D+00, 8.0D+00, 8.0D+00 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 52.274641198704240049D+00, &
   & 68.836530059858524345D+00, 20.659301196734009322D+00, 17.696854873743648823D+00, &
   & 23.867049721753300163D+00, 20.717015982805991662D+00, 56.127842255616681863D+00, &
   & 0.000000000000000000D+00, 59.269565081229636528D+00, 3.9362355036495554780D+00, &
   & 53.000000000000000000D+00, 1.4567910310469068692D+00, 2.2430285802876025701D+00, &
   & 3.6157561775973627487D+00, 4.0816924080221632670D+00 /)
    INTEGER N_DATA
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION AGUD (G)
 
!*****************************************************************************80
!
!! agud() evaluates the inverse Gudermannian function.
!
!  Discussion:
!
!    The Gudermannian function relates the hyperbolic and trigonometric
!    functions.  For any argument X, there is a corresponding value
!    G so that
!
!      SINH(X) = TAN(G).
!
!    This value G(X) is called the Gudermannian of X.  The inverse
!    Gudermannian function is given as input a value G and computes
!    the corresponding value X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = rk ) G, the value of the Gudermannian.
!
!  Output:
!
!    real ( kind = rk ) AGUD, the argument of the Gudermannian.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) AGUD
    REAL (KIND=RK) G
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
 
    AGUD = LOG (TAN(0.25D+00*R8_PI+0.5D+00*G))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ALIGN_ENUM (M, N)
 
!*****************************************************************************80
!
!! align_enum() counts the alignments of two sequences of M and N elements.
!
!  Discussion:
!
!    We assume that we have sequences A and B of M and N characters each.
!    An alignment of the two sequences is a rule matching corresponding
!    elements of one sequence to another.  Some elements of either sequence
!    can be matched to a null element.  If A(I1) and A(I2) are matched
!    to B(J1) and B(J2), and I1 < I2, then it must be the case that J1 < J2.
!
!    The 5 alignments of a sequence of 1 to a sequence of 2 are:
!
!          _1_   _2_   __3__   __4__   __5__
!
!      A:  1 -   - 1   - 1 -   - - 1   1 - -
!      B:  1 2   1 2   1 - 2   1 2 -   - 1 2
!
!    The formula is:
!
!      F(0,0) = 1
!      F(1,0) = 1
!      F(0,1) = 1
!      F(M,N) = F(M-1,N) + F(M-1,N-1) + F(M,N-1)
!
!    To compute F(M,N), it is not necessary to keep an M+1 by N+1
!    array in memory.  A vector of length N will do.
!
!    F(N,N) is approximately ( 1 + sqrt(2) )^(2*N+1) / sqrt ( N )
!
!  Example:
!
!    The initial portion of the table is:
!
!
!  M/N   0    1    2    3    4       5       6       7       8       9      10
!
!   0    1    1    1    1    1       1       1       1       1       1       1
!   1    1    3    5    7    9      11      13      15      17      19      21
!   2    1    5   13   25   41      61      85     113     145     181     221
!   3    1    7   25   63  129     231     377     575     833    1159    1561
!   4    1    9   41  129  321     681    1289    2241    3649    5641    8361
!   5    1   11   61  231  681    1683    3653    7183   13073   22363   36365
!   6    1   13   85  377 1289    3653    8989   19825   40081   75517  134245
!   7    1   15  113  575 2241    7183   19825   48639  108545  224143  433905
!   8    1   17  145  833 3649   13073   40081  108545  265729  598417 1256465
!   9    1   19  181 1159 5641   22363   75517  224143  598417 1462563 3317445
!  10    1   21  221 1561 8361   36365  134245  433905 1256465 3317445 8097453
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Michael Waterman,
!    Introduction to Computational Biology,
!    Chapman and Hall, 1995,
!    ISBN: 0412993910,
!    LC: QH438.4.M33.W38.
!
!  Input:
!
!    integer M, N, the number of elements of the two sequences.
!
!  Output:
!
!    integer ALIGN_ENUM, the number of possible alignments of the sequences.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER ALIGN_ENUM
    INTEGER FI (0:N)
    INTEGER FIM1J
    INTEGER FIM1JM1
    INTEGER I
    INTEGER J
    INTEGER M
 
    IF (M < 0) THEN
        ALIGN_ENUM = 0
        RETURN
    ELSE IF (N < 0) THEN
        ALIGN_ENUM = 0
        RETURN
    ELSE IF (M == 0) THEN
        ALIGN_ENUM = 1
        RETURN
    ELSE IF (N == 0) THEN
        ALIGN_ENUM = 1
        RETURN
    END IF
 
    FI (0:N) = 1
 
    DO I = 1, M
 
        FIM1JM1 = 1
 
        DO J = 1, N
 
            FIM1J = FI (J)
 
            FI (J) = FI (J) + FI (J-1) + FIM1JM1
 
            FIM1JM1 = FIM1J
 
        END DO
    END DO
 
    ALIGN_ENUM = FI (N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BELL (N, B)
 
!*****************************************************************************80
!
!! bell() returns the Bell numbers from 0 to N.
!
!  Discussion:
!
!    The Bell number B(N) is the number of restricted growth functions on N.
!
!    Note that the Stirling numbers of the second kind, S^m_n, count the
!    number of partitions of N objects into M classes, and so it is
!    true that
!
!      B(N) = S^1_N + S^2_N + ... + S^N_N.
!
!    The Bell numbers were named for Eric Temple Bell.
!
!    The Bell number B(N) is defined as the number of partitions (of
!    any size) of a set of N distinguishable objects.
!
!    A partition of a set is a division of the objects of the set into
!    subsets.
!
!  Example:
!
!    There are 15 partitions of a set of 4 objects:
!
!      (1234),
!      (123) (4),
!      (124) (3),
!      (12) (34),
!      (12) (3) (4),
!      (134) (2),
!      (13) (24),
!      (13) (2) (4),
!      (14) (23),
!      (1) (234),
!      (1) (23) (4),
!      (14) (2) (3),
!      (1) (24) (3),
!      (1) (2) (34),
!      (1) (2) (3) (4).
!
!    and so B(4) = 15.
!
!  First values:
!
!     N         B(N)
!     0           1
!     1           1
!     2           2
!     3           5
!     4          15
!     5          52
!     6         203
!     7         877
!     8        4140
!     9       21147
!    10      115975
!
!  Recursion:
!
!    B(I) = sum ( 1 <= J <= I ) Binomial ( I-1, J-1 ) * B(I-J)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N, the number of Bell numbers desired.
!
!  Output:
!
!    integer B(0:N), the Bell numbers from 0 to N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER B (0:N)
    INTEGER COMBO
    INTEGER I
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    B (0) = 1
 
    DO I = 1, N
        B (I) = 0
        DO J = 1, I
            COMBO = I4_CHOOSE (I-1, J-1)
            B (I) = B (I) + COMBO * B (I-J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BELL_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! bell_values() returns some values of the Bell numbers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    integer N, the order of the Bell number.
!
!    integer C, the value of the Bell number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 11
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, &
   & 115975 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BELL_POLY_COEF (N, C)
 
!*****************************************************************************80
!
!! bell_poly_coef(): Coefficients of a Bell polynomial.
!
!  First terms:
!
!    N    0    1    2    3    4    5    6    7    8
!
!    0    1
!    1    0    1
!    2    0    1    1
!    3    0    1    3    1
!    4    0    1    7    6    1
!    5    0    1   15   25   10    1
!    6    0    1   31   90   65   15    1
!    7    0    1   63  301  350  140   21    1
!    8    0    1  127  966 1701 1050  266   28    1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 2018
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N, the order of the polynomial.
!
!  Output:
!
!    integer C(0:N), the coefficients.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER J
    INTEGER C (0:N)
 
    C (0) = 1
    C (1:N) = 0
 
    DO I = 1, N
        DO J = I, 1, - 1
            C (J) = J * C (J) + C (J-1)
        END DO
        C (0) = 0
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BENFORD (IVAL)
 
!*****************************************************************************80
!
!! benford() returns the Benford probability of one or more significant digits.
!
!  Discussion:
!
!    Benford's law is an empirical formula explaining the observed
!    distribution of initial digits in lists culled from newspapers,
!    tax forms, stock market prices, and so on.  It predicts the observed
!    high frequency of the initial digit 1, for instance.
!
!    The probabilities of digits 1 through 9 are guaranteed
!    to add up to 1, since
!
!      log10 ( 2/1 ) + log10 ( 3/2) + log10 ( 4/3 ) + ... + log10 ( 10/9 )
!      = log10 ( 2/1 * 3/2 * 4/3 * ... * 10/9 ) = log10 ( 10 ) = 1.
!
!   The formula is:
!
!      Prob ( First significant digits are IVAL ) =
!        log10 ( ( IVAL + 1 ) / IVAL ).
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
!  Input:
!
!    integer IVAL, the string of significant digits to
!    be checked.  If IVAL is 1, then we are asking for the Benford probability
!    that a value will have first digit 1.  If IVAL is 123, we are asking for
!    the probability that the first three digits will be 123, and so on.
!    Note that IVAL must not be 0 or negative.
!
!  Output:
!
!    real ( kind = rk ) BENFORD, the Benford probability that an
!    item taken from a real world distribution will have the initial
!    digits IVAL.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) BENFORD
    INTEGER IVAL
 
    IF (IVAL <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BENFORD - Fatal error!'
        WRITE (*, '(a)') '  The input argument must be positive.'
        WRITE (*, '(a,i8)') '  Your value was ', IVAL
        STOP 1
    END IF
 
    BENFORD = LOG10 (REAL(IVAL+1, KIND=RK)/REAL(IVAL, KIND=RK))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_NUMBER (N, B)
 
!*****************************************************************************80
!
!! bernoulli_number() computes the Bernoulli numbers B(0) through B(N).
!
!  Discussion:
!
!    The Bernoulli numbers are rational.
!
!    If we define the sum of the M-th powers of the first N integers as:
!
!      SIGMA(M,N) = sum ( 0 <= I <= N ) I^M
!
!    and let C(I,J) be the combinatorial coefficient:
!
!      C(I,J) = I! / ( ( I - J )! * J! )
!
!    then the Bernoulli numbers B(J) satisfy:
!
!      SIGMA(M,N) = 1/(M+1) * sum ( 0 <= J <= M ) C(M+1,J) B(J) * (N+1)^(M+1-J)
!
!  First values:
!
!   B0  1                   =         1.00000000000
!   B1 -1/2                 =        -0.50000000000
!   B2  1/6                 =         1.66666666666
!   B3  0                   =         0
!   B4 -1/30                =        -0.03333333333
!   B5  0                   =         0
!   B6  1/42                =         0.02380952380
!   B7  0                   =         0
!   B8 -1/30                =        -0.03333333333
!   B9  0                   =         0
!  B10  5/66                =         0.07575757575
!  B11  0                   =         0
!  B12 -691/2730            =        -0.25311355311
!  B13  0                   =         0
!  B14  7/6                 =         1.16666666666
!  B15  0                   =         0
!  B16 -3617/510            =        -7.09215686274
!  B17  0                   =         0
!  B18  43867/798           =        54.97117794486
!  B19  0                   =         0
!  B20 -174611/330          =      -529.12424242424
!  B21  0                   =         0
!  B22  854,513/138         =      6192.123
!  B23  0                   =         0
!  B24 -236364091/2730      =    -86580.257
!  B25  0                   =         0
!  B26  8553103/6           =   1425517.16666
!  B27  0                   =         0
!  B28 -23749461029/870     = -27298231.0678
!  B29  0                   =         0
!  B30  8615841276005/14322 = 601580873.901
!
!  Recursion:
!
!    With C(N+1,K) denoting the standard binomial coefficient,
!
!    B(0) = 1.0
!    B(N) = - ( sum ( 0 <= K < N ) C(N+1,K) * B(K) ) / C(N+1,N)
!
!  Warning:
!
!    This recursion, which is used in this routine, rapidly results
!    in significant errors.
!
!  Special Values:
!
!    Except for B(1), all Bernoulli numbers of odd index are 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the highest Bernoulli
!    number to compute.
!
!  Output:
!
!    Output, real ( kind = rk ) B(0:N), B(I) contains the I-th Bernoulli number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) B (0:N)
    REAL (KIND=RK) B_SUM
    INTEGER C (0:N+1)
    INTEGER I
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    B (0) = 1.0D+00
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    B (1) = - 0.5D+00
 
    C (0) = 1
    C (1) = 2
    C (2) = 1
 
    DO I = 2, N
 
        CALL COMB_ROW_NEXT (I+1, C)
 
        IF (MOD(I, 2) == 1) THEN
 
            B (I) = 0.0D+00
 
        ELSE
 
            B_SUM = 0.0D+00
            DO J = 0, I - 1
                B_SUM = B_SUM + B (J) * REAL (C(J), KIND=RK)
            END DO
 
            B (I) = - B_SUM / REAL (C(I), KIND=RK)
 
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_NUMBER2 (N, B)
 
!*****************************************************************************80
!
!! bernoulli_number2() evaluates the Bernoulli numbers.
!
!  Discussion:
!
!    The Bernoulli numbers are rational.
!
!    If we define the sum of the M-th powers of the first N integers as:
!
!      SIGMA(M,N) = sum ( 0 <= I <= N ) I^M
!
!    and let C(I,J) be the combinatorial coefficient:
!
!      C(I,J) = I! / ( ( I - J )! * J! )
!
!    then the Bernoulli numbers B(J) satisfy:
!
!      SIGMA(M,N) = 1/(M+1) * sum ( 0 <= J <= M ) C(M+1,J) B(J) * (N+1)^(M+1-J)
!
!    Note that the Bernoulli numbers grow rapidly.  Bernoulli number
!    62 is probably the last that can be computed on the VAX without
!    overflow.
!
!    A different method than that used in BERN is employed.
!
!  First values:
!
!   B0  1                   =         1.00000000000
!   B1 -1/2                 =        -0.50000000000
!   B2  1/6                 =         1.66666666666
!   B3  0                   =         0
!   B4 -1/30                =        -0.03333333333
!   B5  0                   =         0
!   B6  1/42                =         0.02380952380
!   B7  0                   =         0
!   B8 -1/30                =        -0.03333333333
!   B9  0                   =         0
!  B10  5/66                =         0.07575757575
!  B11  0                   =         0
!  B12 -691/2730            =        -0.25311355311
!  B13  0                   =         0
!  B14  7/6                 =         1.16666666666
!  B15  0                   =         0
!  B16 -3617/510            =        -7.09215686274
!  B17  0                   =         0
!  B18  43867/798           =        54.97117794486
!  B19  0                   =         0
!  B20 -174611/330          =      -529.12424242424
!  B21  0                   =         0
!  B22  854,513/138         =      6192.123
!  B23  0                   =         0
!  B24 -236364091/2730      =    -86580.257
!  B25  0                   =         0
!  B26  8553103/6           =   1425517.16666
!  B27  0                   =         0
!  B28 -23749461029/870     = -27298231.0678
!  B29  0                   =         0
!  B30  8615841276005/14322 = 601580873.901
!
!  Recursion:
!
!    With C(N+1,K) denoting the standard binomial coefficient,
!
!    B(0) = 1.0
!    B(N) = - ( sum ( 0 <= K < N ) C(N+1,K) * B(K) ) / C(N+1,N)
!
!  Special Values:
!
!    Except for B(1), all Bernoulli numbers of odd index are 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the highest order Bernoulli number
!    to compute.
!
!  Output:
!
!    Output, real ( kind = rk ) B(0:N), the requested Bernoulli numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ALTPI
    REAL (KIND=RK) B (0:N)
    INTEGER I
    INTEGER K
    INTEGER, PARAMETER :: KMAX = 400
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) SGN
    REAL (KIND=RK) SUM2
    REAL (KIND=RK) T
    REAL (KIND=RK) TERM
    REAL (KIND=RK), PARAMETER :: TOL = 1.0D-06
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    B (0) = 1.0D+00
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    B (1) = - 0.5D+00
 
    IF (N < 2) THEN
        RETURN
    END IF
 
    ALTPI = LOG (2.0D+00*R8_PI)
!
!  Initial estimates for B(I), I = 2 to N
!
    B (2) = LOG (2.0D+00)
    DO I = 3, N
        IF (MOD(I, 2) == 1) THEN
            B (I) = 0.0D+00
        ELSE
            B (I) = LOG (REAL(I*(I-1), KIND=RK)) + B (I-2)
        END IF
    END DO
 
    B (2) = 1.0D+00 / 6.0D+00
 
    IF (N <= 3) THEN
        RETURN
    END IF
 
    B (4) = - 1.0D+00 / 30.0D+00
 
    SGN = - 1.0D+00
 
    DO I = 6, N, 2
 
        SGN = - SGN
        T = 2.0D+00 * SGN * EXP (B(I)-REAL(I, KIND=RK)*ALTPI)
 
        SUM2 = 1.0D+00
 
        DO K = 2, KMAX
 
            TERM = REAL (K, KIND=RK) ** (-I)
            SUM2 = SUM2 + TERM
 
            IF (TERM <= TOL*SUM2) THEN
                EXIT
            END IF
 
        END DO
 
        B (I) = T * SUM2
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_NUMBER3 (N, B)
 
!*****************************************************************************80
!
!! bernoulli_number3() computes the value of the Bernoulli number B(N).
!
!  Discussion:
!
!    The Bernoulli numbers are rational.
!
!    If we define the sum of the M-th powers of the first N integers as:
!
!      SIGMA(M,N) = sum ( 0 <= I <= N ) I^M
!
!    and let C(I,J) be the combinatorial coefficient:
!
!      C(I,J) = I! / ( ( I - J )! * J! )
!
!    then the Bernoulli numbers B(J) satisfy:
!
!      SIGMA(M,N) =
!        1/(M+1) * sum ( 0 <= J <= M ) C(M+1,J) B(J) * (N+1)^(M+1-J)
!
!  First values:
!
!     B0  1                   =         1.00000000000
!     B1 -1/2                 =        -0.50000000000
!     B2  1/6                 =         1.66666666666
!     B3  0                   =         0
!     B4 -1/30                =        -0.03333333333
!     B5  0                   =         0
!     B6  1/42                =         0.02380952380
!     B7  0                   =         0
!     B8 -1/30                =        -0.03333333333
!     B9  0                   =         0
!    B10  5/66                =         0.07575757575
!    B11  0                   =         0
!    B12 -691/2730            =        -0.25311355311
!    B13  0                   =         0
!    B14  7/6                 =         1.16666666666
!    B15  0                   =         0
!    B16 -3617/510            =        -7.09215686274
!    B17  0                   =         0
!    B18  43867/798           =        54.97117794486
!    B19  0                   =         0
!    B20 -174611/330          =      -529.12424242424
!    B21  0                   =         0
!    B22  854513/138          =      6192.123
!    B23  0                   =         0
!    B24 -236364091/2730      =    -86580.257
!    B25  0                   =         0
!    B26  8553103/6           =   1425517.16666
!    B27  0                   =         0
!    B28 -23749461029/870     = -27298231.0678
!    B29  0                   =         0
!    B30  8615841276005/14322 = 601580873.901
!
!  Recursion:
!
!    With C(N+1,K) denoting the standard binomial coefficient,
!
!    B(0) = 1.0
!    B(N) = - ( sum ( 0 <= K < N ) C(N+1,K) * B(K) ) / C(N+1,N)
!
!  Special Values:
!
!    Except for B(1), all Bernoulli numbers of odd index are 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the Bernoulli number
!    to compute.
!
!  Output:
!
!    Output, real ( kind = rk ) B, the desired Bernoulli number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) B
    INTEGER IT
    INTEGER, PARAMETER :: IT_MAX = 1000
    INTEGER N
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) SUM2
    REAL (KIND=RK) TERM
    REAL (KIND=RK), PARAMETER :: TOL = 5.0D-07
 
    IF (N < 0) THEN
 
        B = 0.0D+00
 
    ELSE IF (N == 0) THEN
 
        B = 1.0D+00
 
    ELSE IF (N == 1) THEN
 
        B = - 0.5D+00
 
    ELSE IF (N == 2) THEN
 
        B = 1.0D+00 / 6.0D+00
 
    ELSE IF (MOD(N, 2) == 1) THEN
 
        B = 0.0D+00
 
    ELSE
 
        SUM2 = 0.0D+00
 
        DO IT = 1, IT_MAX
 
            TERM = 1.0D+00 / REAL (IT**N, KIND=RK)
            SUM2 = SUM2 + TERM
 
            IF (ABS(TERM) < TOL .OR. ABS(TERM) < TOL*ABS(SUM2)) THEN
                EXIT
            END IF
 
        END DO
 
        B = 2.0D+00 * SUM2 * R8_FACTORIAL (N) / (2.0D+00*R8_PI) ** N
 
        IF (MOD(N, 4) == 0) THEN
            B = - B
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_NUMBER_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! bernoulli_number_values() returns some values of the Bernoulli numbers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the Bernoulli number.
!
!    Output, real ( kind = rk ) C, the value of the Bernoulli number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 10
 
    REAL (KIND=RK) C
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1.0000000000D+00, - 0.5000000000D+00, &
   & 0.1666666667D+00, 0.0000000000D+00, - 0.0333333333D+00, - 0.02380952381D+00, - &
   & 0.0333333333D+00, 0.0757575757D+00, - 529.1242424D+00, 601580873.9D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 6, 8, 10, 20, 30 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_POLY (N, X, BX)
 
!*****************************************************************************80
!
!! bernoulli_poly() evaluates the Bernoulli polynomial of order N at X.
!
!  Discussion:
!
!    Thanks to Bart Vandewoestyne for pointing out an error in the previous
!    documentation, 31 January 2008.
!
!    Special values of the Bernoulli polynomial include:
!
!      B(N,0) = B(N,1) = B(N), the N-th Bernoulli number.
!
!      B'(N,X) = N * B(N-1,X)
!
!      B(N,X+1) - B(N,X) = N * X^(N-1)
!      B(N,X) = (-1)^N * B(N,1-X)
!
!    A formula for the Bernoulli polynomial in terms of the Bernoulli
!    numbers is:
!
!      B(N,X) = sum ( 0 <= K <= N ) B(K) * C(N,K) * X^(N-K)
!
!    The first few polynomials include:
!
!      B(0,X) = 1
!      B(1,X) = X    - 1/2
!      B(2,X) = X^2 -   X      +  1/6
!      B(3,X) = X^3 - 3/2*X^2 +  1/2*X
!      B(4,X) = X^4 - 2*X^3   +      X^2 - 1/30
!      B(5,X) = X^5 - 5/2*X^4 +  5/3*X^3 - 1/6*X
!      B(6,X) = X^6 - 3*X^5   +  5/2*X^4 - 1/2*X^2 + 1/42
!      B(7,X) = X^7 - 7/2*X^6 +  7/2*X^5 - 7/6*X^3 + 1/6*X
!      B(8,X) = X^8 - 4*X^7   + 14/3*X^6 - 7/3*X^4 + 2/3*X^2 - 1/30
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the Bernoulli polynomial to
!    be evaluated.  N must be 0 or greater.
!
!    Input, real ( kind = rk ) X, the value of X at which the polynomial is to
!    be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) BX, the value of B(N,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) B (0:N)
    REAL (KIND=RK) BX
    INTEGER C (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    CALL BERNOULLI_NUMBER (N, B)
!
!  Get row N of Pascal's triangle.
!
    DO I = 0, N
        CALL COMB_ROW_NEXT (I, C)
    END DO
 
    BX = 1.0D+00
    DO I = 1, N
        BX = BX * X + B (I) * REAL (C(I), KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNOULLI_POLY2 (N, X, BX)
 
!*****************************************************************************80
!
!! bernoulli_poly2() evaluates the N-th Bernoulli polynomial at X.
!
!  Discussion:
!
!    Thanks to Bart Vandewoestyne for pointing out an error in the previous
!    documentation, 31 January 2008.
!
!    Special values of the Bernoulli polynomial include:
!
!      B(N,0) = B(N,1) = B(N), the N-th Bernoulli number.
!
!      B'(N,X) = N * B(N-1,X)
!
!      B(N,X+1) - B(N,X) = N * X^(N-1)
!      B(N,X) = (-1)^N * B(N,1-X)
!
!    A formula for the Bernoulli polynomial in terms of the Bernoulli
!    numbers is:
!
!      B(N,X) = sum ( 0 <= K <= N ) B(K) * C(N,K) * X^(N-K)
!
!    The first few polynomials include:
!
!      B(0,X) = 1
!      B(1,X) = X    - 1/2
!      B(2,X) = X^2 -   X      +  1/6
!      B(3,X) = X^3 - 3/2*X^2 +  1/2*X
!      B(4,X) = X^4 - 2*X^3   +      X^2 - 1/30
!      B(5,X) = X^5 - 5/2*X^4 +  5/3*X^3 - 1/6*X
!      B(6,X) = X^6 - 3*X^5   +  5/2*X^4 - 1/2*X^2 + 1/42
!      B(7,X) = X^7 - 7/2*X^6 +  7/2*X^5 - 7/6*X^3 + 1/6*X
!      B(8,X) = X^8 - 4*X^7   + 14/3*X^6 - 7/3*X^4 + 2/3*X^2 - 1/30
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the Bernoulli polynomial to
!    be evaluated.  N must be 0 or greater.
!
!    Input, real ( kind = rk ) X, the value at which the polynomial is to
!    be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) BX, the value of B(N,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) B
    REAL (KIND=RK) BX
    REAL (KIND=RK) FACT
    INTEGER I
    INTEGER N
    REAL (KIND=RK) X
 
    FACT = 1.0D+00
 
    CALL BERNOULLI_NUMBER3 (0, B)
 
    BX = B
 
    DO I = 1, N
        FACT = FACT * REAL (N+1-I, KIND=RK) / REAL (I, KIND=RK)
        CALL BERNOULLI_NUMBER3 (I, B)
        BX = BX * X + FACT * B
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNSTEIN_POLY (N, X, BERN)
 
!*****************************************************************************80
!
!! bernstein_poly() evaluates the Bernstein polynomials at a point X.
!
!  Discussion:
!
!    The Bernstein polynomials are assumed to be based on [0,1].
!
!    The formula is:
!
!      B(N,I,X) = [N!/(I!*(N-I)!)] * (1-X)^(N-I) * X^I
!
!  First values:
!
!    B(0,0,X) = 1
!
!    B(1,0,X) =      1-X
!    B(1,1,X) =               X
!
!    B(2,0,X) =     (1-X)^2
!    B(2,1,X) = 2 * (1-X)   * X
!    B(2,2,X) =               X^2
!
!    B(3,0,X) =     (1-X)^3
!    B(3,1,X) = 3 * (1-X)^2 * X
!    B(3,2,X) = 3 * (1-X)   * X^2
!    B(3,3,X) =               X^3
!
!    B(4,0,X) =     (1-X)^4
!    B(4,1,X) = 4 * (1-X)^3 * X
!    B(4,2,X) = 6 * (1-X)^2 * X^2
!    B(4,3,X) = 4 * (1-X)   * X^3
!    B(4,4,X) =               X^4
!
!  Special values:
!
!    B(N,I,X) has a unique maximum value at X = I/N.
!
!    B(N,I,X) has an I-fold zero at 0 and and N-I fold zero at 1.
!
!    B(N,I,1/2) = C(N,K) / 2^N
!
!    For a fixed X and N, the polynomials add up to 1:
!
!      Sum ( 0 <= I <= N ) B(N,I,X) = 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the degree of the Bernstein polynomials
!    to be used.  For any N, there is a set of N+1 Bernstein polynomials,
!    each of degree N, which form a basis for polynomials on [0,1].
!
!    Input, real ( kind = rk ) X, the evaluation point.
!
!  Output:
!
!    Output, real ( kind = rk ) BERN(0:N), the values of the N+1
!    Bernstein polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) BERN (0:N)
    INTEGER I
    INTEGER J
    REAL (KIND=RK) X
 
    IF (N == 0) THEN
 
        BERN (0) = 1.0D+00
 
    ELSE IF (0 < N) THEN
 
        BERN (0) = 1.0D+00 - X
        BERN (1) = X
 
        DO I = 2, N
            BERN (I) = X * BERN (I-1)
            DO J = I - 1, 1, - 1
                BERN (J) = X * BERN (J-1) + (1.0D+00-X) * BERN (J)
            END DO
            BERN (0) = (1.0D+00-X) * BERN (0)
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BERNSTEIN_POLY_01_VALUES (N_DATA, N, K, X, B)
 
!*****************************************************************************80
!
!! bernstein_poly_01_values() returns some values of the Bernstein polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the degree of the polynomial.
!
!    Output, integer K, the index of the polynomial.
!
!    Output, real ( kind = rk ) X, the argument of the polynomial.
!
!    Output, real ( kind = rk ) B, the value of the polynomial B(N,K,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 15
 
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: B_VEC = (/ 1.0D+00, 0.75D+00, 0.25D+00, &
   & 0.5625D+00, 0.3750D+00, 0.0625D+00, 0.421875D+00, 0.421875D+00, 0.140625D+00, &
   & 0.015625D+00, 0.31640625D+00, 0.421875D+00, 0.2109375D+00, 0.046875D+00, 0.00390625D+00 /)
    INTEGER K
    INTEGER, SAVE, DIMENSION (NMAX) :: K_VEC = (/ 0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4 &
   & /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4 &
   & /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 0.25D+00, 0.25D+00, 0.25D+00, &
   & 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, &
   & 0.25D+00, 0.25D+00, 0.25D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        K = 0
        X = 0.0D+00
        B = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        K = K_VEC (N_DATA)
        X = X_VEC (N_DATA)
        B = B_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_VALUES (N_DATA, X, Y, FXY)
 
!*****************************************************************************80
!
!! beta_values() returns some values of the Beta function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 April 2001
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, Y, the arguments of the function.
!
!    Output, real ( kind = rk ) FXY, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 17
 
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: B_VEC = (/ 5.000000D+00, 2.500000D+00, &
   & 1.666667D+00, 1.250000D+00, 5.000000D+00, 2.500000D+00, 1.000000D+00, 1.666667D-01, &
   & 0.333333D-01, 7.142857D-03, 1.587302D-03, 0.238095D-01, 5.952381D-03, 1.984127D-03, &
   & 7.936508D-04, 3.607504D-04, 8.325008D-05 /)
    REAL (KIND=RK) FXY
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 0.2D+00, 0.4D+00, 0.6D+00, 0.8D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00, 6.0D+00, 6.0D+00, &
   & 6.0D+00, 6.0D+00, 7.0D+00 /)
    REAL (KIND=RK) Y
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: Y_VEC = (/ 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   & 0.2D+00, 0.4D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
   & 5.0D+00, 6.0D+00, 7.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
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
 
SUBROUTINE BPAB (N, A, B, X, BERN)
 
!*****************************************************************************80
!
!! bpab() evaluates at X the Bernstein polynomials based in [A,B].
!
!  Discussion:
!
!    The formula is:
!
!      BERN(N,I,X) = [N!/(I!*(N-I)!)] * (B-X)^(N-I) * (X-A)^I / (B-A)^N
!
!  First values:
!
!    B(0,0,X) =   1
!
!    B(1,0,X) = (      B-X                ) / (B-A)
!    B(1,1,X) = (                 X-A     ) / (B-A)
!
!    B(2,0,X) = (     (B-X)^2             ) / (B-A)^2
!    B(2,1,X) = ( 2 * (B-X)    * (X-A)    ) / (B-A)^2
!    B(2,2,X) = (                (X-A)^2  ) / (B-A)^2
!
!    B(3,0,X) = (     (B-X)^3             ) / (B-A)^3
!    B(3,1,X) = ( 3 * (B-X)^2  * (X-A)    ) / (B-A)^3
!    B(3,2,X) = ( 3 * (B-X)    * (X-A)^2  ) / (B-A)^3
!    B(3,3,X) = (                (X-A)^3  ) / (B-A)^3
!
!    B(4,0,X) = (     (B-X)^4             ) / (B-A)^4
!    B(4,1,X) = ( 4 * (B-X)^3  * (X-A)    ) / (B-A)^4
!    B(4,2,X) = ( 6 * (B-X)^2  * (X-A)^2  ) / (B-A)^4
!    B(4,3,X) = ( 4 * (B-X)    * (X-A)^3  ) / (B-A)^4
!    B(4,4,X) = (                (X-A)^4  ) / (B-A)^4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the degree of the Bernstein polynomials
!    to be used.  For any N, there is a set of N+1 Bernstein polynomials,
!    each of degree N, which form a basis for polynomials on [A,B].
!
!    Input, real ( kind = rk ) A, B, the endpoints of the interval on which the
!    polynomials are to be based.  A and B should not be equal.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials
!    are to be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) BERN(0:N), the values of the N+1
!    Bernstein polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) BERN (0:N)
    INTEGER I
    INTEGER J
    REAL (KIND=RK) X
 
    IF (B == A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BPAB - Fatal error!'
        WRITE (*, '(a,g14.6)') '  A = B = ', A
        STOP 1
    END IF
 
    IF (N == 0) THEN
 
        BERN (0) = 1.0D+00
 
    ELSE IF (0 < N) THEN
 
        BERN (0) = (B-X) / (B-A)
        BERN (1) = (X-A) / (B-A)
 
        DO I = 2, N
            BERN (I) = (X-A) * BERN (I-1) / (B-A)
            DO J = I - 1, 1, - 1
                BERN (J) = ((B-X)*BERN(J)+(X-A)*BERN(J-1)) / (B-A)
            END DO
            BERN (0) = (B-X) * BERN (0) / (B-A)
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDAN_POLY (N, X, S, CX)
 
!*****************************************************************************80
!
!! cardan_poly() evaluates the Cardan polynomials.
!
!  Discussion:
!
!    If we write the N-th polynomial in terms of its coefficients:
!
!      C(N,S,X) = sum ( 0 <= I <= N ) D(N,I) * S^(N-I)/2 * X^I
!
!    then we have the recursion:
!
!      D(0,0) = 1
!
!      D(1,1) = 1
!      D(1,0) = 0
!
!      D(N,N) = 1
!      D(N,K) = D(N-1,K-1) - D(N-2,K)
!
!  Example:
!
!     N  C(N,S,X)
!
!     0  2
!     1  X
!     2  X^2  -  2 S
!     3  X^3  -  3 S X
!     4  X^4  -  4 S X^2 +  2 S^2
!     5  X^5  -  5 S X^3 +  5 S^2 X
!     6  X^6  -  6 S X^4 +  9 S^2 X^2 -  2 S^3
!     7  X^7  -  7 S X^5 + 14 S^2 X^3 -  7 S^3 X
!     8  X^8  -  8 S X^6 + 20 S^2 X^4 - 16 S^3 X^3 +  2 S^4
!     9  X^9  -  9 S X^7 + 27 S^2 X^5 - 30 S^3 X^3 +  9 S^4 X
!    10  X^10 - 10 S X^8 + 35 S^2 X^6 - 50 S^3 X^4 + 25 S^4 X^2 -  2 S^5
!    11  X^11 - 11 S X^9 + 44 S^2 X^7 - 77 S^3 X^5 + 55 S^4 X^3 - 11 S^5 X
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Osler,
!    Cardan Polynomials and the Reduction of Radicals,
!    Mathematics Magazine,
!    Volume 74, Number 1, February 2001, pages 26-32.
!
!  Parameters:
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials
!    are to be computed.
!
!    Input, real ( kind = rk ) S, the value of the parameter, which
!    must be positive.
!
!  Output:
!
!    Output, real ( kind = rk ) CX(0:N), the values of the Cardan
!    polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    REAL (KIND=RK) FACT
    INTEGER I
    REAL (KIND=RK) S
    REAL (KIND=RK) S2
    REAL (KIND=RK) X
    REAL (KIND=RK) X2 (1)
 
    S2 = SQRT (S)
    X2 (1) = 0.5D+00 * X / S2
 
    CALL CHEBY_T_POLY (1, N, X2, CX)
 
    FACT = 1.0D+00
 
    DO I = 0, N
        CX (I) = 2.0D+00 * FACT * CX (I)
        FACT = FACT * S2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDAN_POLY_COEF (N, S, C)
 
!*****************************************************************************80
!
!! cardan_poly_coef() computes the coefficients of the N-th Cardan polynomial.
!
!  First terms:
!
!    2
!    0      1
!   -2 S    0      1
!    0     -3 S    0      1
!    2 S^2  0     -4 S    0      1
!    0      5 S^2  0     -5 S    0      1
!   -2 S^3  0      9 S^2  0     -6 S    0      1
!    0      7 S^3  0     14 S^2  0     -7 S    0      1
!    2 S^4  0    -16 S^3  0     20 S^2  0     -8 S    0       1
!    0      9 S^4  0    -30 S^3  0     27 S^2  0     -9 S     0     1
!   -2 S^5  0     25 S^4  0    -50 S^3  0     35 S^2  0     -10 S   0   1
!    0    -11 S^5  0     55 S^4  0    -77 S^3  0    +44 S^2   0   -11 S 0 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Thomas Osler,
!    Cardan Polynomials and the Reduction of Radicals,
!    Mathematics Magazine,
!    Volume 74, Number 1, February 2001, pages 26-32.
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial
!
!    Input, real ( kind = rk ) S, the value of the parameter, which
!    must be positive.
!
!  Output:
!
!    Output, real ( kind = rk ) C(0:N), the coefficients.  C(0) is the
!    constant term, and C(N) is the coefficient of X^N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N)
    REAL (KIND=RK) CM1 (0:N)
    REAL (KIND=RK) CM2 (0:N)
    INTEGER I
    REAL (KIND=RK) S
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0) = 2.0D+00
    C (1:N) = 0.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CM1 (0:N) = C (0:N)
 
    C (0) = 0.0D+00
    C (1) = 1.0D+00
    C (2:N) = 0.0D+00
 
    DO I = 2, N
 
        CM2 (0:I-2) = CM1 (0:I-2)
        CM1 (0:I-1) = C (0:I-1)
 
        C (0) = 0.0D+00
        C (1:I) = CM1 (0:I-1)
        C (0:I-2) = C (0:I-2) - S * CM2 (0:I-2)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDINAL_COS (J, M, N, T, C)
 
!*****************************************************************************80
!
!! cardinal_cos() evaluates the J-th cardinal cosine basis function.
!
!  Discussion:
!
!    The base points are T(I) = pi * I / ( M + 1 ), 0 <= I <= M + 1.
!    Basis function J is 1 at T(J), and 0 at T(I) for I /= J
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Boyd,
!    Exponentially convergent Fourier-Chebyshev quadrature schemes on
!    bounded and infinite intervals,
!    Journal of Scientific Computing,
!    Volume 2, Number 2, 1987, pages 99-109.
!
!  Parameters:
!
!    Input, integer J, the index of the basis function.
!    0 <= J <= M + 1.
!
!    Input, integer M, indicates the size of the basis set.
!
!    Input, integer N, the number of sample points.
!
!    Input, real ( kind = rk ) T(N), one or more points in [0,pi] where the
!    basis function is to be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) C(N), the value of the function at T.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (N)
    REAL (KIND=RK) CJ
    INTEGER I
    INTEGER J
    INTEGER M
    REAL (KIND=RK), PARAMETER :: R8_EPS = 2.220446049250313D-016
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) T (N)
    REAL (KIND=RK) TJ
 
    IF (MOD(J, M+1) == 0) THEN
        CJ = 2.0D+00
    ELSE
        CJ = 1.0D+00
    END IF
 
    TJ = R8_PI * REAL (J, KIND=RK) / REAL (M+1, KIND=RK)
 
    DO I = 1, N
 
        IF (ABS(T(I)-TJ) <= R8_EPS) THEN
            C (I) = 1.0D+00
        ELSE
            C (I) = R8_MOP (J+1) * SIN (T(I)) * SIN (REAL(M+1, KIND=RK)*T(I)) / CJ / REAL (M+1, &
           & KIND=RK) / (COS(T(I))-COS(TJ))
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CARDINAL_SIN (J, M, N, T, S)
 
!*****************************************************************************80
!
!! cardinal_sin() evaluates the J-th cardinal sine basis function.
!
!  Discussion:
!
!    The base points are T(I) = pi * I / ( M + 1 ), 0 <= I <= M + 1.
!    Basis function J is 1 at T(J), and 0 at T(I) for I /= J
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    John Boyd,
!    Exponentially convergent Fourier-Chebyshev quadrature schemes on
!    bounded and infinite intervals,
!    Journal of Scientific Computing,
!    Volume 2, Number 2, 1987, pages 99-109.
!
!  Parameters:
!
!    Input, integer J, the index of the basis function.
!    0 <= J <= M + 1.
!
!    Input, integer M, indicates the size of the basis set.
!
!    Input, integer N, the number of sample points.
!
!    Input, real ( kind = rk ) T(N), one or more points in [0,pi] where the
!    basis function is to be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) S(N), the value of the function at T.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER J
    INTEGER M
    REAL (KIND=RK), PARAMETER :: R8_EPS = 2.220446049250313D-016
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) S (N)
    REAL (KIND=RK) T (N)
    REAL (KIND=RK) TJ
 
    TJ = R8_PI * REAL (J, KIND=RK) / REAL (M+1, KIND=RK)
 
    DO I = 1, N
 
        IF (ABS(T(I)-TJ) <= R8_EPS) THEN
            S (I) = 1.0D+00
        ELSE
            S (I) = R8_MOP (J+1) * SIN (TJ) * SIN (REAL(M+1, KIND=RK)*T(I)) / REAL (M+1, &
           & KIND=RK) / (COS(T(I))-COS(TJ))
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CATALAN (N, C)
 
!*****************************************************************************80
!
!! catalan() computes the Catalan numbers, from C(0) to C(N).
!
!  Discussion:
!
!    The Catalan number C(N) counts:
!
!    1) the number of binary trees on N vertices;
!    2) the number of ordered trees on N+1 vertices;
!    3) the number of full binary trees on 2N+1 vertices;
!    4) the number of well formed sequences of 2N parentheses;
!    5) the number of ways 2N ballots can be counted, in order,
!       with N positive and N negative, so that the running sum
!       is never negative;
!    6) the number of standard tableaus in a 2 by N rectangular Ferrers diagram;
!    7) the number of monotone functions from [1..N} to [1..N} which
!       satisfy f(i) <= i for all i;
!    8) the number of ways to triangulate a polygon with N+2 vertices.
!
!    The formula is:
!
!      C(N) = (2*N)! / ( (N+1) * (N!) * (N!) )
!           = 1 / (N+1) * COMB ( 2N, N )
!           = 1 / (2N+1) * COMB ( 2N+1, N+1).
!
!  First values:
!
!     C(0)     1
!     C(1)     1
!     C(2)     2
!     C(3)     5
!     C(4)    14
!     C(5)    42
!     C(6)   132
!     C(7)   429
!     C(8)  1430
!     C(9)  4862
!    C(10) 16796
!
!  Recursion:
!
!    C(N) = 2 * (2*N-1) * C(N-1) / (N+1)
!    C(N) = sum ( 1 <= I <= N-1 ) C(I) * C(N-I)
!
!  Example:
!
!    N = 3
!
!    ()()()
!    ()(())
!    (()())
!    (())()
!    ((()))
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Dennis Stanton, Dennis White,
!    Constructive Combinatorics,
!    Springer, 1986,
!    ISBN: 0387963472.
!
!  Parameters:
!
!    Input, integer N, the number of Catalan numbers desired.
!
!  Output:
!
!    Output, integer C(0:N), the Catalan numbers from C(0) to C(N).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER C (0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0) = 1
!
!  The extra parentheses ensure that the integer division is
!  done AFTER the integer multiplication.
!
    DO I = 1, N
        C (I) = (C(I-1)*2*(2*I-1)) / (I+1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION CATALAN_CONSTANT ()
 
!*****************************************************************************80
!
!! catalan_constant() returns the value of Catalan's constant.
!
!  Discussion:
!
!    Catalan's constant, which may be denoted by G, is defined as
!
!      G = sum ( 0 <= K ) ( -1 )^K / ( 2 * K + 1 )^2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Output:
!
!    real ( kind = rk ) CATALAN_CONSTANT, the value of Catalan's
!    constant.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) CATALAN_CONSTANT
 
    CATALAN_CONSTANT = 0.915965594177D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CATALAN_ROW_NEXT (IDO, N, IROW)
 
!*****************************************************************************80
!
!! catalan_row_next() computes row N of Catalan's triangle.
!
!  Example:
!
!    I\J 0   1   2   3   4   5   6
!
!    0   1
!    1   1   1
!    2   1   2   2
!    3   1   3   5   5
!    4   1   4   9  14  14
!    5   1   5  14  28  42  42
!    6   1   6  20  48  90 132 132
!
!  Recursion:
!
!    C(0,0) = 1
!    C(I,0) = 1
!    C(I,J) = 0 for I < J
!    C(I,J) = C(I,J-1) + C(I-1,J)
!    C(I,I) is the I-th Catalan number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer IDO, indicates whether this is a call for
!    the 'next' row of the triangle.
!    IDO = 0, this is a startup call.  Row N is desired, but
!    presumably this is a first call, or row N-1 was not computed
!    on the previous call.
!    IDO = 1, this is not the first call, and row N-1 was computed
!    on the previous call.  In this case, much work can be saved
!    by using the information from the previous values of IROW
!    to build the next values.
!
!    Input, integer N, the index of the row of the triangle
!    desired.
!
!    integer IROW(0:N), the row of coefficients.
!    If IDO = 0, then IROW is not required to be set on input.
!    If IDO = 1, then IROW must be set on input to the value of
!    row N-1.
!
!  Output:
!
!    integer IROW(0:N), the row of coefficients.
!    The next row.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER IDO
    INTEGER IROW (0:N)
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    IF (IDO == 0) THEN
 
        IROW (0) = 1
        IROW (1:N) = 0
 
        DO I = 1, N
 
            IROW (0) = 1
 
            DO J = 1, I - 1
                IROW (J) = IROW (J) + IROW (J-1)
            END DO
 
            IROW (I) = IROW (I-1)
 
        END DO
 
    ELSE
 
        IROW (0) = 1
 
        DO J = 1, N - 1
            IROW (J) = IROW (J) + IROW (J-1)
        END DO
 
        IF (1 <= N) THEN
            IROW (N) = IROW (N-1)
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CATALAN_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! catalan_values() returns some values of the Catalan numbers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the Catalan number.
!
!    Output, integer C, the value of the Catalan number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 11
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, &
   & 16796 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHARLIER (N, A, X, VALUE)
 
!*****************************************************************************80
!
!! charlier() evaluates Charlier polynomials at a point.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    J Simoes Pereira,
!    Algorithm 234: Poisson-Charliers Polynomials,
!    Communications of the ACM,
!    Volume 7, Number 7, page 420, July 1964.
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!    Gabor Szego,
!    Orthogonal Polynomials,
!    American Mathematical Society, 1975,
!    ISBN: 0821810235,
!    LC: QA3.A5.v23.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45.
!
!  Parameters:
!
!    Input, integer N, the maximum order of the polynomial.
!    N must be at least 0.
!
!    Input, real ( kind = rk ) A, the parameter.  A must not be 0.
!
!    Input, real ( kind = rk ) X, the evaluation point.
!
!  Output:
!
!    Output, real ( kind = rk ) VALUE(0:N), the value of the polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A
    INTEGER I
    REAL (KIND=RK) VALUE (0:N)
    REAL (KIND=RK) X
 
    IF (A == 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHARLIER - Fatal error!'
        WRITE (*, '(a)') '  Parameter A cannot be zero.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHARLIER - Fatal error!'
        WRITE (*, '(a)') '  Parameter N must be nonnegative.'
        STOP 1
    END IF
 
    VALUE (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    VALUE (1) = - X / A
 
    IF (N == 1) THEN
        RETURN
    END IF
 
    DO I = 1, N - 1
        VALUE (I+1) = ((REAL(I, KIND=RK)+A-X)*VALUE(I)-REAL(I, KIND=RK)*VALUE(I-1)) / A
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_T_POLY (M, N, X, V)
 
!*****************************************************************************80
!
!! cheby_t_poly() evaluates Chebyshev polynomials T(n,x).
!
!  Discussion:
!
!    Chebyshev polynomials are useful as a basis for representing the
!    approximation of functions since they are well conditioned, in the sense
!    that in the interval [-1,1] they each have maximum absolute value 1.
!    Hence an error in the value of a coefficient of the approximation, of
!    size epsilon, is exactly reflected in an error of size epsilon between
!    the computed approximation and the theoretical approximation.
!
!    Typical usage is as follows, where we assume for the moment
!    that the interval of approximation is [-1,1].  The value
!    of N is chosen, the highest polynomial to be used in the
!    approximation.  Then the function to be approximated is
!    evaluated at the N+1 points XJ which are the zeroes of the N+1-th
!    Chebyshev polynomial.  Let these values be denoted by F(XJ).
!
!    The coefficients of the approximation are now defined by
!
!      C(I) = 2/(N+1) * sum ( 1 <= J <= N+1 ) F(XJ) T(I,XJ)
!
!    except that C(0) is given a value which is half that assigned
!    to it by the above formula,
!
!    and the representation is
!
!    F(X) approximated by sum ( 0 <= J <= N ) C(J) T(J,X)
!
!    Now note that, again because of the fact that the Chebyshev polynomials
!    have maximum absolute value 1, if the higher order terms of the
!    coefficients C are small, then we have the option of truncating
!    the approximation by dropping these terms, and we will have an
!    exact value for maximum perturbation to the approximation that
!    this will cause.
!
!    It should be noted that typically the error in approximation
!    is dominated by the first neglected basis function (some multiple of
!    T(N+1,X) in the example above).  If this term were the exact error,
!    then we would have found the minimax polynomial, the approximating
!    polynomial of smallest maximum deviation from the original function.
!    The minimax polynomial is hard to compute, and another important
!    feature of the Chebyshev approximation is that it tends to behave
!    like the minimax polynomial while being easy to compute.
!
!    To evaluate a sum like
!
!      sum ( 0 <= J <= N ) C(J) T(J,X),
!
!    Clenshaw's recurrence formula is recommended instead of computing the
!    polynomial values, forming the products and summing.
!
!    Assuming that the coefficients C(J) have been computed
!    for J = 0 to N, then the coefficients of the representation of the
!    indefinite integral of the function may be computed by
!
!      B(I) = ( C(I-1) - C(I+1))/2*(I-1) for I=1 to N+1,
!
!    with
!
!      C(N+1)=0
!      B(0) arbitrary.
!
!    Also, the coefficients of the representation of the derivative of the
!    function may be computed by:
!
!      D(I) = D(I+2)+2*I*C(I) for I=N-1, N-2, ..., 0,
!
!    with
!
!      D(N+1) = D(N)=0.
!
!    Some of the above may have to adjusted because of the irregularity of C(0).
!
!    The formula is:
!
!      T(N,X) = COS(N*ARCCOS(X))
!
!  Differential equation:
!
!    (1-X*X) Y'' - X Y' + N N Y = 0
!
!  First terms:
!
!    T(0,X) =  1
!    T(1,X) =  1 X
!    T(2,X) =  2 X^2 -   1
!    T(3,X) =  4 X^3 -   3 X
!    T(4,X) =  8 X^4 -   8 X^2 +  1
!    T(5,X) = 16 X^5 -  20 X^3 +  5 X
!    T(6,X) = 32 X^6 -  48 X^4 + 18 X^2 - 1
!    T(7,X) = 64 X^7 - 112 X^5 + 56 X^3 - 7 X
!
!  Inequality:
!
!    abs ( T(N,X) ) <= 1 for -1 <= X <= 1
!
!  Orthogonality:
!
!    For integration over [-1,1] with weight
!
!      W(X) = 1 / sqrt(1-X*X),
!
!    if we write the inner product of T(I,X) and T(J,X) as
!
!      < T(I,X), T(J,X) > = integral ( -1 <= X <= 1 ) W(X) T(I,X) T(J,X) dX
!
!    then the result is:
!
!      < T(I,X), T(J,X) > = 0    if I /= J
!      < T(I,X), T(J,X) > = PI/2 if I == J /= 0
!      < T(I,X), T(J,X) > = PI   if I == J == 0
!
!    A discrete orthogonality relation is also satisfied at each of
!    the N zeroes of T(N,X):  sum ( 1 <= K <= N ) T(I,X) * T(J,X)
!                              = 0 if I /= J
!                              = N/2 if I == J /= 0
!                              = N if I == J == 0
!
!  Recursion:
!
!    T(0,X) = 1,
!    T(1,X) = X,
!    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
!
!    T'(N,X) = N * ( -X * T(N,X) + T(N-1,X) ) / ( 1 - X^2 )
!
!  Special values:
!
!    T(N,1) = 1
!    T(N,-1) = (-1)^N
!    T(2N,0) = (-1)^N
!    T(2N+1,0) = 0
!    T(N,X) = (-1)^N * T(N,-X)
!
!  Zeroes:
!
!    M-th zero of T(N,X) is X = cos((2*M-1)*PI/(2*N)), M = 1 to N.
!
!  Extrema:
!
!    M-th extremum of T(N,X) is X = cos(PI*M/N), M = 0 to N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2001
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
!  Parameters:
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(1:M), the evaluation points.
!
!  Output:
!
!    Output, real ( kind = rk ) V(1:M,0:N), the values of the polynomials.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER J
    REAL (KIND=RK) X (1:M)
    REAL (KIND=RK) V (1:M, 0:N)
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    V (1:M, 0) = 1.0D+00
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    V (1:M, 1) = X (1:M)
 
    DO J = 2, N
        V (1:M, J) = 2.0D+00 * X (1:M) * V (1:M, J-1) - V (1:M, J-2)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_T_POLY_COEF (N, C)
 
!*****************************************************************************80
!
!! cheby_t_poly_coef() evaluates coefficients of Chebyshev polynomials T(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     1
!     2     -1     0      2
!     3      0    -3      0      4
!     4      1     0     -8      0       8
!     5      0     5      0    -20       0    16
!     6     -1     0     18      0     -48     0     32
!     7      0    -7      0     56       0  -112      0    64
!
!  Recursion:
!
!    T(0,X) = 1,
!    T(1,X) = X,
!    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 May 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!  Output:
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the Chebyshev T
!    polynomials.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N, 0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0:N, 0:N) = 0.0D+00
 
    C (0, 0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    C (1, 1) = 1.0D+00
 
    DO I = 2, N
        C (I, 0) = - C (I-2, 0)
        C (I, 1:I-2) = 2.0D+00 * C (I-1, 0:I-3) - C (I-2, 1:I-2)
        C (I, I-1) = 2.0D+00 * C (I-1, I-2)
        C (I, I) = 2.0D+00 * C (I-1, I-1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_T_POLY_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! cheby_t_poly_values() returns values of Chebyshev polynomials T(n,x).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 13
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FX_VEC = (/ 1.0000000000D+00, 0.8000000000D+00, &
   & 0.2800000000D+00, - 0.3520000000D+00, - 0.8432000000D+00, - 0.9971200000D+00, - &
   & 0.7521920000D+00, - 0.2063872000D+00, 0.4219724800D+00, 0.8815431680D+00, &
   & 0.9884965888D+00, 0.7000513741D+00, 0.1315856097D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, &
   & 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_T_POLY_ZERO (N, Z)
 
!*****************************************************************************80
!
!! cheby_t_poly_zero() returns zeroes of Chebyshev polynomials T(n,x).
!
!  Discussion:
!
!    The I-th zero of T(N,X) is cos((2*I-1)*PI/(2*N)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N, the order of the polynomial.
!
!  Output:
!
!    real ( kind = rk ) Z(N), the zeroes of T(N,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ANGLE
    INTEGER I
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) Z (N)
 
    DO I = 1, N
        ANGLE = REAL (2*I-1, KIND=RK) * R8_PI / REAL (2*N, KIND=RK)
        Z (I) = COS (ANGLE)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_U_POLY (M, N, X, V)
 
!*****************************************************************************80
!
!! cheby_u_poly() evaluates Chebyshev polynomials U(n,x).
!
!  Discussion:
!
!    The formula is:
!
!      If |X| <= 1, then
!
!        U(N,X) = sin ( (N+1) * arccos(X) ) / sqrt ( 1 - X^2 )
!               = sin ( (N+1) * arccos(X) ) / sin ( arccos(X) )
!
!      else
!
!        U(N,X) = sinh ( (N+1) * arccosh(X) ) / sinh ( arccosh(X) )
!
!  Differential equation:
!
!    (1-X*X) Y'' - 3 X Y' + N (N+2) Y = 0
!
!  First terms:
!
!    U(0,X) =   1
!    U(1,X) =   2 X
!    U(2,X) =   4 X^2 -   1
!    U(3,X) =   8 X^3 -   4 X
!    U(4,X) =  16 X^4 -  12 X^2 +  1
!    U(5,X) =  32 X^5 -  32 X^3 +  6 X
!    U(6,X) =  64 X^6 -  80 X^4 + 24 X^2 - 1
!    U(7,X) = 128 X^7 - 192 X^5 + 80 X^3 - 8X
!
!  Orthogonality:
!
!    For integration over [-1,1] with weight
!
!      W(X) = sqrt(1-X*X),
!
!    we have
!
!      < U(I,X), U(J,X) > = integral ( -1 <= X <= 1 ) W(X) U(I,X) U(J,X) dX
!
!    then the result is:
!
!      < U(I,X), U(J,X) >  =  0    if I /= J
!      < U(I,X), U(J,X) >  =  PI/2 if I == J
!
!  Recursion:
!
!    U(0,X) = 1,
!    U(1,X) = 2 * X,
!    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
!
!  Special values:
!
!    U(N,1) = N + 1
!    U(2N,0) = (-1)^N
!    U(2N+1,0) = 0
!    U(N,X) = (-1)^N * U(N,-X)
!
!  Zeroes:
!
!    M-th zero of U(N,X) is X = cos( M*PI/(N+1)), M = 1 to N
!
!  Extrema:
!
!    M-th extremum of U(N,X) is X = cos( M*PI/N), M = 0 to N
!
!  Norm:
!
!    Integral ( -1 <= X <= 1 ) ( 1 - X^2 ) * U(N,X)^2 dX = PI/2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 January 2015
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
!  Parameters:
!
!    Input, integer M, the number of evaluation points.
!
!    Input, integer N, the highest polynomial to compute.
!
!    Input, real ( kind = rk ) X(1:M), the evaluation points.
!
!  Output:
!
!    Output, real ( kind = rk ) V(1:M,0:N), the values of the polynomials.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER J
    REAL (KIND=RK) X (1:M)
    REAL (KIND=RK) V (1:M, 0:N)
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    V (1:M, 0) = 1.0D+00
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    V (1:M, 1) = 2.0D+00 * X (1:M)
 
    DO J = 2, N
        V (1:M, J) = 2.0D+00 * X (1:M) * V (1:M, J-1) - V (1:M, J-2)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_U_POLY_COEF (N, C)
 
!*****************************************************************************80
!
!! cheby_u_poly_coef() evaluates coefficients of Chebyshev polynomials U(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     2
!     2     -1     0      4
!     3      0    -4      0      8
!     4      1     0    -12      0      16
!     5      0     6      0    -32       0    32
!     6     -1     0     24      0     -80     0     64
!     7      0    -8      0     80       0  -192      0   128
!
!  Recursion:
!
!    U(0,X) = 1,
!    U(1,X) = 2*X,
!    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!  Output:
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the Chebyshev U
!    polynomials.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N, 0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0:N, 0:N) = 0.0D+00
 
    C (0, 0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    C (1, 1) = 2.0D+00
 
    DO I = 2, N
        C (I, 0) = - C (I-2, 0)
        C (I, 1:I-2) = 2.0D+00 * C (I-1, 0:I-3) - C (I-2, 1:I-2)
        C (I, I-1) = 2.0D+00 * C (I-1, I-2)
        C (I, I) = 2.0D+00 * C (I-1, I-1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_U_POLY_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! cheby_u_poly_values() returns values of the Chebyshev polynomial U(n,x).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      ChebyshevU[n,x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 April 2012
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 13
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1000000000000000D+01, &
   & 0.1600000000000000D+01, 0.1560000000000000D+01, 0.8960000000000000D+00, - &
   & 0.1264000000000000D+00, - 0.1098240000000000D+01, - 0.1630784000000000D+01, - &
   & 0.1511014400000000D+01, - 0.7868390400000000D+00, 0.2520719360000000D+00, &
   & 0.1190154137600000D+01, 0.1652174684160000D+01, 0.1453325357056000D+01 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, &
   & 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00, 0.8D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBY_U_POLY_ZERO (N, Z)
 
!*****************************************************************************80
!
!! cheby_u_poly_zero() returns zeroes of Chebyshev polynomials U(n,x).
!
!  Discussion:
!
!    The I-th zero of U(N,X) is cos((I-1)*PI/(N-1)), I = 1 to N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!  Output:
!
!    Output, real ( kind = rk ) Z(N), the zeroes of U(N,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ANGLE
    INTEGER I
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) Z (N)
 
    DO I = 1, N
        ANGLE = REAL (I, KIND=RK) * R8_PI / REAL (N+1, KIND=RK)
        Z (I) = COS (ANGLE)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHEBYSHEV_DISCRETE (N, M, X, V)
 
!*****************************************************************************80
!
!! chebyshev_discrete() evaluates discrete Chebyshev polynomials at a point.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!  Parameters:
!
!    Input, integer N, the highest order of the polynomials to
!    be evaluated.  0 <= N <= M.
!
!    Input, integer M, the maximum order of the polynomials.
!    0 <= M.
!
!    Input, real ( kind = rk ) X, the evaluation point.
!
!  Output:
!
!    Output, real ( kind = rk ) V(0:N), the value of the polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER M
    REAL (KIND=RK) X
    REAL (KIND=RK) V (0:N)
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHEBYSHEV_DISCRETE - Fatal error!'
        WRITE (*, '(a)') '  Parameter M must be nonnegative.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHEBYSHEV_DISCRETE - Fatal error!'
        WRITE (*, '(a)') '  Parameter N must be nonnegative.'
        STOP 1
    END IF
 
    IF (M < N) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHEBYSHEV_DISCRETE - Fatal error!'
        WRITE (*, '(a)') '  Parameter N must be no greater than M.'
        STOP 1
    END IF
 
    V (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    V (1) = 2.0D+00 * X + REAL (1-M, KIND=RK)
 
    IF (N == 1) THEN
        RETURN
    END IF
 
    DO I = 1, N - 1
        V (I+1) = (REAL(2*I+1, KIND=RK)*(2.0D+00*X+REAL(1-M, KIND=RK))*V(I)-REAL(I*(M+I)*(M-I), &
       & KIND=RK)*V(I-1)) / REAL (I+1, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION COLLATZ_COUNT (N)
 
!*****************************************************************************80
!
!! collatz_count() counts the number of terms in a Collatz sequence.
!
!  Discussion:
!
!    The rules for generation of the Collatz sequence are recursive.
!    If T is the current entry of the sequence, (T is
!    assumed to be a positive integer), then the next
!    entry, U is determined as follows:
!
!      if T is 1 (or less)
!        terminate the sequence;
!      else if T is even
!        U = T/2.
!      else (if T is odd and not 1)
!        U = 3*T+1;
!
!     N  Sequence                                                Length
!
!     1                                                               1
!     2   1                                                           2
!     3  10,  5, 16,  8,  4,  2,  1                                   8
!     4   2   1                                                       3
!     5  16,  8,  4,  2,  1                                           6
!     6   3, 10,  5, 16,  8,  4,  2,  1                               9
!     7  22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1   17
!     8   4,  2,  1                                                   4
!     9  28, 14,  7, ...                                             20
!    10   5, 16,  8,  4,  2,  1                                       7
!    11  34, 17, 52, 26, 13, 40, 20, 10,  5, 16, 8, 4, 2, 1          15
!    12   6,  3, 10,  5, 16,  8,  4,  2,  1                          10
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer N, the first element of the sequence.
!
!  Output:
!
!    Output, integer COLLATZ_COUNT, the number of elements in
!    the Collatz sequence that begins with N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER COLLATZ_COUNT
    INTEGER COUNT
    INTEGER N
    INTEGER N_LOCAL
 
    COUNT = 1
    N_LOCAL = N
 
    DO
 
        IF (N_LOCAL <= 1) THEN
            EXIT
        ELSE IF (MOD(N_LOCAL, 2) == 0) THEN
            N_LOCAL = N_LOCAL / 2
        ELSE
            N_LOCAL = 3 * N_LOCAL + 1
        END IF
 
        COUNT = COUNT + 1
 
    END DO
 
    COLLATZ_COUNT = COUNT
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COLLATZ_COUNT_MAX (N, I_MAX, J_MAX)
 
!*****************************************************************************80
!
!! collatz_count_max() seeks the maximum Collatz count for 1 through N.
!
!  Discussion:
!
!    For each integer I, we compute a sequence of values that
!    terminate when we reach 1.  The number of steps required to
!    reach 1 is the "rank" of I, and we are searching the numbers
!    from 1 to N for the number with maximum rank.
!
!    For a given I, the sequence is produced by:
!
!    1) J = 1, X(J) = I;
!    2) If X(J) = 1, stop.
!    3) J = J + 1;
!       if X(J-1) was even, X(J) = X(J-1)/2;
!       else                X(J) = 3 * X(J-1) + 1;
!    4) Go to 3
!
!  Example:
!
!            N     I_MAX J_MAX
!
!           10         9    20
!          100        97   119
!        1,000       871   179
!       10,000     6,171   262
!      100,000    77,031   351
!    1,000,000   837,799   525
!   10,000,000 8,400,511   686
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the maximum integer to check.
!
!  Output:
!
!    Output, integer I_MAX, J_MAX, an integer I with the maximum
!    rank, and the value of the maximum rank.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER I_MAX
    INTEGER J
    INTEGER J_MAX
    INTEGER N
    INTEGER X
 
    I_MAX = - 1
    J_MAX = - 1
 
    DO I = 1, N
 
        J = 1
        X = I
 
        DO WHILE (X /=  1)
            J = J + 1
            IF (MOD(X, 2) == 0) THEN
                X = X / 2
            ELSE
                X = 3 * X + 1
            END IF
        END DO
 
        IF (J_MAX < J) THEN
            I_MAX = I
            J_MAX = J
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COLLATZ_COUNT_VALUES (N_DATA, N, COUNT)
 
!*****************************************************************************80
!
!! collatz_count_values() returns some values of the Collatz count function.
!
!  Discussion:
!
!    The rules for generation of the Collatz sequence are recursive.
!    If T is the current entry of the sequence, (T is
!    assumed to be a positive integer), then the next
!    entry, U is determined as follows:
!
!      if T is 1 (or less)
!        terminate the sequence;
!      else if T is even
!        U = T/2.
!      else (if T is odd and not 1)
!        U = 3*T+1;
!
!    The Collatz count is the length of the Collatz sequence for a given
!    starting value.  By convention, we include the initial value in the
!    count, so the minimum value of the count is 1.
!
!     N  Sequence                                                 Count
!
!     1                                                               1
!     2   1                                                           2
!     3  10,  5, 16,  8,  4,  2,  1                                   8
!     4   2   1                                                       3
!     5  16,  8,  4,  2,  1                                           6
!     6   3, 10,  5, 16,  8,  4,  2,  1                               9
!     7  22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1   17
!     8   4,  2,  1                                                   4
!     9  28, 14,  7, ...                                             20
!    10   5, 16,  8,  4,  2,  1                                       7
!    11  34, 17, 52, 26, 13, 40, 20, 10,  5, 16, 8, 4, 2, 1          15
!    12   6,  3, 10,  5, 16,  8,  4,  2,  1                          10
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the initial value of a Collatz sequence.
!
!    Output, integer COUNT, the length of the Collatz sequence
!    starting with N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 20
 
    INTEGER COUNT
    INTEGER, SAVE, DIMENSION (N_MAX) :: COUNT_VEC = (/ 1, 2, 8, 3, 6, 9, 17, 4, 20, 7, 112, 25, &
   & 26, 27, 17, 28, 111, 18, 83, 29 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 27, 50, 100, &
   & 200, 300, 400, 500, 600, 700, 800 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        COUNT = 0
    ELSE
        N = N_VEC (N_DATA)
        COUNT = COUNT_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COMB_ROW_NEXT (N, ROW)
 
!*****************************************************************************80
!
!! comb_row_next() computes the next row of Pascal's triangle.
!
!  Discussion:
!
!    Row N contains the combinatorial coefficients
!
!      C(N,0), C(N,1), C(N,2), ... C(N,N)
!
!    The sum of the elements of row N is equal to 2^N.
!
!    The formula is
!
!      C(N,K) = N! / ( K! * (N-K)! )
!
!  First terms:
!
!     N K:0  1   2   3   4   5   6   7  8  9 10
!
!     0   1
!     1   1  1
!     2   1  2   1
!     3   1  3   3   1
!     4   1  4   6   4   1
!     5   1  5  10  10   5   1
!     6   1  6  15  20  15   6   1
!     7   1  7  21  35  35  21   7   1
!     8   1  8  28  56  70  56  28   8  1
!     9   1  9  36  84 126 126  84  36  9  1
!    10   1 10  45 120 210 252 210 120 45 10  1
!
!  Recursion:
!
!    C(N,K) = C(N-1,K-1)+C(N-1,K)
!
!  Special values:
!
!    C(N,0) = C(N,N) = 1
!    C(N,1) = C(N,N-1) = N
!    C(N,N-2) = sum ( 1 <= I <= N ) N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 December 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, indicates the desired row.
!
!    integer ROW(0:N). row N-1 is
!    contained in entries 0 through N-1.
!
!  Output:
!
!    integer ROW(0:N), the next row.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER ROW (0:N)
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    ROW (N) = 1
    DO I = N - 1, 1, - 1
        ROW (I) = ROW (I) + ROW (I-1)
    END DO
    ROW (0) = 1
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COMMUL (N, NFACTOR, FACTOR, NCOMB)
 
!*****************************************************************************80
!
!! commul() computes a multinomial combinatorial coefficient.
!
!  Discussion:
!
!    The multinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where FACTOR(1) objects are indistinguishable of type 1,
!    ... and FACTOR(K) are indistinguishable of type NFACTOR.
!
!    The formula is:
!
!      NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, determines the numerator.
!
!    Input, integer NFACTOR, the number of factors in the
!    numerator.
!
!    Input, integer FACTOR(NFACTOR).
!    FACTOR contains the NFACTOR values used in the denominator.
!    Note that the sum of these entries should be N,
!    and that all entries should be nonnegative.
!
!  Output:
!
!    Output, integer NCOMB, the value of the multinomial
!    coefficient.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER NFACTOR
 
    REAL (KIND=RK) ARG
    REAL (KIND=RK) FACK
    REAL (KIND=RK) FACN
    INTEGER FACTOR (NFACTOR)
    INTEGER I
    INTEGER ISUM
    INTEGER N
    INTEGER NCOMB
 
    IF (NFACTOR < 1) THEN
        NCOMB = 1
        RETURN
    END IF
 
    DO I = 1, NFACTOR
 
        IF (FACTOR(I) < 0) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'COMMUL - Fatal error!'
            WRITE (*, '(a,i8,a,i8)') '  Entry ', I, ' of FACTOR = ', FACTOR (I)
            WRITE (*, '(a)') '  But this value must be nonnegative.'
            STOP 1
        END IF
 
    END DO
 
    ISUM = SUM (FACTOR(1:NFACTOR))
 
    IF (ISUM /= N) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COMMUL - Fatal error!'
        WRITE (*, '(a,i8)') '  The sum of the FACTOR entries is ', ISUM
        WRITE (*, '(a,i8)') '  But it must equal N = ', N
        STOP 1
    END IF
 
    ARG = REAL (N+1, KIND=RK)
    FACN = LGAMMA (ARG)
 
    DO I = 1, NFACTOR
 
        ARG = REAL (FACTOR(I)+1, KIND=RK)
        FACK = LGAMMA (ARG)
        FACN = FACN - FACK
 
    END DO
 
    NCOMB = NINT (EXP(FACN))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COMPLETE_SYMMETRIC_POLY (N, R, X, VALUE)
 
!*****************************************************************************80
!
!! complete_symmetric_poly() evaluates a complete symmetric polynomial.
!
!  Discussion:
!
!    N\R  0   1         2               3
!      +--------------------------------------------------------
!    0 |  1   0         0               0
!    1 |  1   X1        X1^2            X1^3
!    2 |  1   X1+X2     X1^2+X1X2+X2^2  X1^3+X1^2X2+X1X2^2+X2^3
!    3 |  1   X1+X2+X3  ...
!
!    If X = ( 1, 2, 3, 4, 5, ... ) then
!
!    N\R  0     1     2     3     4 ...
!      +--------------------------------------------------------
!    0 |  1     0     0     0     0
!    1 |  1     1     1     1     1
!    2 |  1     3     7    15    31
!    3 |  1     6    25    90   301
!    4 |  1    10    65   350  1701
!    5 |  1    15   140  1050  6951
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 November 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of variables.
!    0 <= N.
!
!    Input, integer R, the degree of the polynomial.
!    0 <= R.
!
!    Input, real ( kind = rk ) X(N), the value of the variables.
!
!  Output:
!
!    Output, real ( kind = rk ) VALUE, the value of TAU(N,R)(X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER R
 
    INTEGER NN
    INTEGER RR
    REAL (KIND=RK) TAU (0:MAX(N, R))
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X (N)
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ''
        WRITE (*, '(a)') 'COMPLETE_SYMMETRIC_POLY - Fatal error!'
        WRITE (*, '(a)') '  N < 0.'
        STOP 1
    END IF
 
    IF (R < 0) THEN
        WRITE (*, '(a)') ''
        WRITE (*, '(a)') 'COMPLETE_SYMMETRIC_POLY - Fatal error!'
        WRITE (*, '(a)') '  R < 0.'
        STOP 1
    END IF
 
    TAU (0:MAX(N, R)) = 0.0D+00
 
    TAU (0) = 1.0D+00
    DO NN = 1, N
        DO RR = 1, R
            TAU (RR) = TAU (RR) + X (NN) * TAU (RR-1)
        END DO
    END DO
 
    VALUE = TAU (R)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION COS_POWER_INT (A, B, N)
 
!*****************************************************************************80
!
!! cos_power_int() evaluates the cosine power integral.
!
!  Discussion:
!
!    The function is defined by
!
!      COS_POWER_INT(A,B,N) = Integral ( A <= t <= B ) ( cos ( t ))^n dt
!
!    The algorithm uses the following fact:
!
!      Integral cos^n ( t ) = - (1/n) * (
!        cos^(n-1)(t) * sin(t) + ( n-1 ) * Integral cos^(n-2) ( t ) dt )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, real ( kind = rk ) A, B, the limits of integration.
!
!    Input, integer N, the power of the sine function.
!
!  Output:
!
!    Output, real ( kind = rk ) COS_POWER_INT, the value of the integral.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) CA
    REAL (KIND=RK) CB
    REAL (KIND=RK) COS_POWER_INT
    INTEGER M
    INTEGER MLO
    INTEGER N
    REAL (KIND=RK) SA
    REAL (KIND=RK) SB
 
    REAL (KIND=RK) VALUE
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'COS_POWER_INT - Fatal error!'
        WRITE (*, '(a)') '  Power N < 0.'
        VALUE = 0.0D+00
        STOP 1
    END IF
 
    SA = SIN (A)
    SB = SIN (B)
    CA = COS (A)
    CB = COS (B)
 
    IF (MOD(N, 2) == 0) THEN
        VALUE = B - A
        MLO = 2
    ELSE
        VALUE = SB - SA
        MLO = 3
    END IF
 
    DO M = MLO, N, 2
        VALUE = (REAL(M-1, KIND=RK)*VALUE-CA**(M-1)*SA+CB**(M-1)*SB) / REAL (M, KIND=RK)
    END DO
 
    COS_POWER_INT = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE COS_POWER_INT_VALUES (N_DATA, A, B, N, FX)
 
!*****************************************************************************80
!
!! cos_power_int_values() returns some values of the cosine power integral.
!
!  Discussion:
!
!    The function has the form
!
!      COS_POWER_INT(A,B,N) = Integral ( A <= t <= B ) ( cos(T) )^N dt
!
!    In Mathematica, the function can be evaluated by:
!
!      Integrate [ ( Cos[x] )^n, { x, a, b } ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2012
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) A, B, the limits of integration.
!
!    Output, integer N, the power.
!
!    Output, real ( kind = rk ) FX, the function value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 11
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.00D+00, 0.00D+00, 0.00D+00, &
   & 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00 /)
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 3.141592653589793D+00, &
   & 3.141592653589793D+00, 3.141592653589793D+00, 3.141592653589793D+00, &
   & 3.141592653589793D+00, 3.141592653589793D+00, 3.141592653589793D+00, &
   & 3.141592653589793D+00, 3.141592653589793D+00, 3.141592653589793D+00, 3.141592653589793D+00 &
   & /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 3.141592653589793D+00, 0.0D+00, &
   & 1.570796326794897D+00, 0.0D+00, 1.178097245096172D+00, 0.0D+00, 0.9817477042468104D+00, &
   & 0.0D+00, 0.8590292412159591D+00, 0.0D+00, 0.7731263170943632D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        N = 0
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        N = N_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DELANNOY (M, N, A)
 
!*****************************************************************************80
!
!! delannoy() returns the Delannoy numbers up to orders (M,N).
!
!  Discussion:
!
!    The Delannoy number A(M,N) counts the number of distinct paths
!    from (0,0) to (M,N) in which the only steps used are
!    (1,1), (1,0) and (0,1).
!
!  First values:
!
!      \N 0  1   2   3    4     5     6      7      8
!     M-+--------------------------------------------
!     0 | 1  1   1   1    1     1     1      1      1
!     1 | 1  3   5   7    9    11    13     15     17
!     2 | 1  5  13  25   41    61    85    113    145
!     3 | 1  7  25  63  129   231   377    575    833
!     4 | 1  9  41 129  321   681  1289   2241   3649
!     5 | 1 11  61 231  681  1683  3653   7183  13073
!     6 | 1 13  85 377 1289  3653  8989  19825  40081
!     7 | 1 15 113 575 2241  7183 19825  48639 108545
!     8 | 1 17 145 833 3649 13073 40081 108545 265729
!
!  Recursion:
!
!    A(0,0) = 1
!    A(M,0) = 1
!    A(0,N) = 1
!    A(M,N) = A(M-1,N) + A(M,N-1) + A(M-1,N-1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer M, N, define the highest order number to
!    compute.
!
!  Output:
!
!    Output, integer A(0:M,0:N), the Delannoy numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER A (0:M, 0:N)
    INTEGER I
    INTEGER J
 
    IF (M < 0) THEN
        RETURN
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    A (0, 0) = 1
 
    A (1:M, 0) = 1
    A (0, 1:N) = 1
 
    DO I = 1, M
        DO J = 1, N
            A (I, J) = A (I-1, J) + A (I, J-1) + A (I-1, J-1)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DOMINO_TILING_NUM (M, N, VALUE)
 
!*****************************************************************************80
!
!! domino_tiling_num() counts tilings of an MxN rectangle by dominoes.
!
!  Discussion:
!
!    An 8x8 chessboard has 12,988,816 such tilings.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 June 2018
!
!  Author:
!
!    Original Python version by John D Cook.
!    FORTRAN90 version by John Burkardt.
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!  Output:
!
!    Output, integer VALUE, the number of tilings.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) ANGLE_K
    REAL (KIND=RK) ANGLE_L
    REAL (KIND=RK) COS_K
    REAL (KIND=RK) COS_L
    INTEGER K
    INTEGER L
    INTEGER M
    INTEGER N
    COMPLEX (KIND=RK) PROD
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    INTEGER VALUE
 
    PROD = CMPLX (1.0D+00, 0.0D+00)
 
    DO K = 1, M
        ANGLE_K = R8_PI * REAL (K, KIND=RK) / REAL (M+1, KIND=RK)
        COS_K = COS (ANGLE_K)
        DO L = 1, N
            ANGLE_L = R8_PI * REAL (L, KIND=RK) / REAL (N+1, KIND=RK)
            COS_L = COS (ANGLE_L)
            PROD = PROD * 2.0D+00 * CMPLX (COS_K, COS_L)
        END DO
    END DO
 
    VALUE = NINT (SQRT(ABS(PROD)))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ERF_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! erf_values() returns some values of the ERF or "error" function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 21
 
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: BVEC = (/ 0.0000000000D+00, 0.1124629160D+00, &
   & 0.2227025892D+00, 0.3286267595D+00, 0.4283923550D+00, 0.5204998778D+00, 0.6038560908D+00, &
   & 0.6778011938D+00, 0.7421009647D+00, 0.7969082124D+00, 0.8427007929D+00, 0.8802050696D+00, &
   & 0.9103139782D+00, 0.9340079449D+00, 0.9522851198D+00, 0.9661051465D+00, 0.9763483833D+00, &
   & 0.9837904586D+00, 0.9890905016D+00, 0.9927904292D+00, 0.9953222650D+00 /)
    REAL (KIND=RK) FX
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: XVEC = (/ 0.0D+00, 0.1D+00, 0.2D+00, 0.3D+00, &
   & 0.4D+00, 0.5D+00, 0.6D+00, 0.7D+00, 0.8D+00, 0.9D+00, 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, &
   & 1.4D+00, 1.5D+00, 1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, 2.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = XVEC (N_DATA)
        FX = BVEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EULER_MASCHERONI ()
 
!*****************************************************************************80
!
!! euler_mascheroni() returns the value of the Euler-Mascheroni constant.
!
!  Discussion:
!
!    The Euler-Mascheroni constant is often denoted by a lower-case gamma.
!
!      gamma = limit ( N -> +oo )
!        ( sum ( 1 <= I <= N ) 1 / I ) - log ( N )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2022
!
!  Author:
!
!    John Burkardt
!
!  Output:
!
!    real ( kind = rk ) euler_mascheroni, the value of the
!    Euler-Mascheroni constant.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) EULER_MASCHERONI
 
    EULER_MASCHERONI = 0.577215664901532860606512090082402431042D+00
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EULER_NUMBER (N, E)
 
!*****************************************************************************80
!
!! euler_number() computes the Euler numbers.
!
!  Discussion:
!
!    The Euler numbers can be evaluated in Mathematica by:
!
!      EulerE[n]
!
!    These numbers rapidly get too big to store in an ordinary integer!
!
!    The terms of odd index are 0.
!
!    E(N) = -C(N,N-2) * E(N-2) - C(N,N-4) * E(N-4) - ... - C(N,0) * E(0).
!
!  First terms:
!
!    E0  = 1
!    E1  = 0
!    E2  = -1
!    E3  = 0
!    E4  = 5
!    E5  = 0
!    E6  = -61
!    E7  = 0
!    E8  = 1385
!    E9  = 0
!    E10 = -50521
!    E11 = 0
!    E12 = 2702765
!    E13 = 0
!    E14 = -199360981
!    E15 = 0
!    E16 = 19391512145
!    E17 = 0
!    E18 = -2404879675441
!    E19 = 0
!    E20 = 370371188237525
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 February 2003
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input, integer N, the index of the last Euler number
!    to compute.
!
!  Output:
!
!    Output, integer E(0:N), the Euler numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER E (0:N)
    INTEGER I
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    E (0) = 1
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    E (1) = 0
 
    IF (N == 1) THEN
        RETURN
    END IF
 
    E (2) = - 1
 
    DO I = 3, N
 
        E (I) = 0
 
        IF (MOD(I, 2) == 0) THEN
 
            DO J = 2, I, 2
                E (I) = E (I) - I4_CHOOSE (I, J) * E (I-J)
            END DO
 
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EULER_NUMBER2 (N)
 
!*****************************************************************************80
!
!! euler_number2() computes the Euler numbers.
!
!  Discussion:
!
!    The Euler numbers can be evaluated in Mathematica by:
!
!      EulerE[n]
!
!  First terms:
!
!    E0  = 1
!    E1  = 0
!    E2  = -1
!    E3  = 0
!    E4  = 5
!    E5  = 0
!    E6  = -61
!    E7  = 0
!    E8  = 1385
!    E9  = 0
!    E10 = -50521
!    E11 = 0
!    E12 = 2702765
!    E13 = 0
!    E14 = -199360981
!    E15 = 0
!    E16 = 19391512145
!    E17 = 0
!    E18 = -2404879675441
!    E19 = 0
!    E20 = 370371188237525
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2004
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input, integer N, the index of the Euler number to compute.
!
!  Output:
!
!    Output, real ( kind = rk ) EULER_NUMBER2, the value of E(N).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) EULER_NUMBER2
    REAL (KIND=RK), SAVE, DIMENSION (0:6) :: E = (/ 1.0D+00, - 1.0D+00, 5.0D+00, - 61.0D+00, &
   & 1385.0D+00, - 50521.0D+00, 2702765.0D+00 /)
    INTEGER I
    INTEGER, PARAMETER :: ITMAX = 1000
    INTEGER N
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) SUM1
    REAL (KIND=RK) TERM
 
    IF (N < 0) THEN
        EULER_NUMBER2 = 0.0D+00
        RETURN
    END IF
 
    IF (N == 0) THEN
        EULER_NUMBER2 = E (0)
        RETURN
    END IF
 
    IF (MOD(N, 2) == 1) THEN
        EULER_NUMBER2 = 0.0D+00
        RETURN
    END IF
 
    IF (N <= 12) THEN
        EULER_NUMBER2 = E (N/2)
        RETURN
    END IF
 
    SUM1 = 0.0D+00
    DO I = 1, ITMAX
 
        TERM = 1.0D+00 / REAL ((2*I-1)**(N+1), KIND=RK)
 
        IF (MOD(I, 2) == 1) THEN
            SUM1 = SUM1 + TERM
        ELSE
            SUM1 = SUM1 - TERM
        END IF
 
        IF (ABS(TERM) < 1.0D-10) THEN
            EXIT
        ELSE IF (ABS(TERM) < 1.0D-08*ABS(SUM1)) THEN
            EXIT
        END IF
 
    END DO
 
    EULER_NUMBER2 = 2.0D+00 ** (N+2) * SUM1 * R8_FACTORIAL (N) / R8_PI ** (N+1)
 
    IF (MOD(N, 4) /= 0) THEN
        EULER_NUMBER2 = - EULER_NUMBER2
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EULER_NUMBER_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! euler_number_values() returns some values of the Euler numbers.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2015
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the Euler number.
!
!    Output, integer C, the value of the Euler number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 8
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 0, - 1, 5, - 61, 1385, - 50521, 2702765 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 4, 6, 8, 10, 12 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EULER_POLY (N, X)
 
!*****************************************************************************80
!
!! euler_poly() evaluates the N-th Euler polynomial at X.
!
!  First values:
!
!    E(0,X) = 1
!    E(1,X) = X - 1/2
!    E(2,X) = X^2 - X
!    E(3,X) = X^3 - 3/2 X^2 + 1/4
!    E(4,X) = X^4 - 2*X^3 + X
!    E(5,X) = X^5 - 5/2 X^4 + 5/2 X^2 - 1/2
!    E(6,X) = X^6 - 3 X^5 + 5 X^3 - 3 X
!    E(7,X) = X^7 - 7/2 X^6 + 35/4 X^4 - 21/2 X^2 + 17/8
!    E(8,X) = X^8 - 4 X^7 + 14 X^5 - 28 X^3 + 17 X
!
!  Special values:
!
!    E'(N,X) = N * E(N-1,X)
!
!    E(N,1/2) = E(N) / 2^N, where E(N) is the N-th Euler number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the Euler polynomial to
!    be evaluated.  N must be 0 or greater.
!
!    Input, real ( kind = rk ) X, the value at which the polynomial is to
!    be evaluated.
!
!  Output:
!
!    Output, real ( kind = rk ) EULER_POLY, the value of E(N,X).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) BX1
    REAL (KIND=RK) BX2
    REAL (KIND=RK) EULER_POLY
    INTEGER N
    REAL (KIND=RK) X
 
    CALL BERNOULLI_POLY2 (N+1, X, BX1)
    CALL BERNOULLI_POLY2 (N+1, 0.5D+00*X, BX2)
 
    EULER_POLY = 2.0D+00 * (BX1-BX2*2.0D+00**(N+1)) / REAL (N+1, KIND=RK)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE EULERIAN (N, E)
 
!*****************************************************************************80
!
!! eulerian() computes the Eulerian number E(N,K).
!
!  Discussion:
!
!    A run in a permutation is a sequence of consecutive ascending values.
!
!    E(N,K) is the number of permutations of N objects which contain
!    exactly K runs.
!
!  Examples:
!
!     N = 7
!
!     1     0     0     0     0     0     0
!     1     1     0     0     0     0     0
!     1     4     1     0     0     0     0
!     1    11    11     1     0     0     0
!     1    26    66    26     1     0     0
!     1    57   302   302    57     1     0
!     1   120  1191  2416  1191   120     1
!
!  Recursion:
!
!    E(N,K) = K * E(N-1,K) + (N-K+1) * E(N-1,K-1).
!
!  Properties:
!
!    E(N,1) = E(N,N) = 1.
!    E(N,K) = 0 if K <= 0 or N < K.
!    sum ( 1 <= K <= N ) E(N,K) = N!.
!    X^N = sum ( 0 <= K <= N ) COMB(X+K-1, N ) E(N,K)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Dennis Stanton, Dennis White,
!    Constructive Combinatorics,
!    Springer Verlag, 1986
!
!  Parameters:
!
!    Input, integer N, the number of rows desired.
!
!  Output:
!
!    Output, integer E(N,N), the first N rows of Eulerian numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER E (N, N)
    INTEGER I
    INTEGER J
 
    IF (N < 1) THEN
        RETURN
    END IF
!
!  Construct rows 1, 2, ..., N of the Eulerian triangle.
!
    E (1, 1) = 1
    E (1, 2:N) = 0
 
    DO I = 2, N
        E (I, 1) = 1
        DO J = 2, N
            E (I, J) = J * E (I-1, J) + (I-J+1) * E (I-1, J-1)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
RECURSIVE FUNCTION F_HOFSTADTER (N) RESULT (VALUE)
 
!*****************************************************************************80
!
!! f_hofstadter() computes the Hofstadter F sequence.
!
!  Discussion:
!
!    F(N) = 0                if N = 0
!         = N - F ( N - 1 ), otherwise.
!
!    F(N) is defined for all nonnegative integers, and turns out
!    to be equal to int ( ( N + 1 ) / 2 ).
!
!  Table:
!
!     N  F(N)
!    --  ----
!
!     0     0
!     1     1
!     2     1
!     3     2
!     4     2
!     5     3
!     6     3
!     7     4
!     8     4
!     9     5
!    10     5
!    11     6
!    12     6
!    13     7
!    14     7
!    15     8
!    16     8
!    17     9
!    18     9
!    19    10
!    20    10
!    21    11
!    22    11
!    23    12
!    24    12
!    25    13
!    26    13
!    27    14
!    28    14
!    29    15
!    30    15
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
!    Douglas Hofstadter,
!    Goedel, Escher, Bach,
!    Basic Books, 1979,
!    ISBN: 0465026567,
!    LC: QA9.8H63.
!
!  Parameters:
!
!    Input, integer N, the argument of the function.
!
!  Output:
!
!    Output, integer F_HOFSTADTER, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER ARG
    INTEGER N
    INTEGER VALUE
 
    IF (N <= 0) THEN
        VALUE = 0
    ELSE
        ARG = N - 1
        VALUE = N - F_HOFSTADTER (ARG)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FACTORIAL_LOG_VALUES (N_DATA, N, FN)
 
!*****************************************************************************80
!
!! factorial_log_values() returns values of log(n!).
!
!  Discussion:
!
!    The function log(n!) can be written as
!
!     log(n!) = sum ( 1 <= i <= n ) log ( i )
!
!    In Mathematica, the function can be evaluated by:
!
!      Log[n!]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 August 2004
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Input:
!
!    integer N_DATA.  The user sets N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    integer N, the argument of the function.
!
!    real ( kind = rk ) FN, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 27
 
    REAL (KIND=RK) FN
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FN_VEC = (/ 0.0000000000000000D+00, &
   & 0.0000000000000000D+00, 0.6931471805599453D+00, 0.1791759469228055D+01, &
   & 0.3178053830347946D+01, 0.4787491742782046D+01, 0.6579251212010101D+01, &
   & 0.8525161361065414D+01, 0.1060460290274525D+02, 0.1280182748008147D+02, &
   & 0.1510441257307552D+02, 0.1750230784587389D+02, 0.1998721449566189D+02, &
   & 0.2255216385312342D+02, 0.2519122118273868D+02, 0.2789927138384089D+02, &
   & 0.3067186010608067D+02, 0.3350507345013689D+02, 0.3639544520803305D+02, &
   & 0.3933988418719949D+02, 0.4233561646075349D+02, 0.5800360522298052D+02, &
   & 0.1484777669517730D+03, 0.3637393755555635D+03, 0.6050201058494237D+03, &
   & 0.2611330458460156D+04, 0.5912128178488163D+04 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, &
   & 13, 14, 15, 16, 17, 18, 19, 20, 25, 50, 100, 150, 500, 1000 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        FN = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        FN = FN_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FIBONACCI_DIRECT (N, F)
 
!*****************************************************************************80
!
!! fibonacci_direct() computes the N-th Fibonacci number directly.
!
!  Discussion:
!
!    A direct formula for the N-th Fibonacci number is:
!
!      F(N) = ( PHIP^N - PHIM^N ) / sqrt(5)
!
!    where
!
!      PHIP = ( 1 + sqrt(5) ) / 2,
!      PHIM = ( 1 - sqrt(5) ) / 2.
!
!  Example:
!
!     N   F
!    --  --
!     0   0
!     1   1
!     2   1
!     3   2
!     4   3
!     5   5
!     6   8
!     7  13
!     8  21
!     9  34
!    10  55
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
!  Input:
!
!    integer N, the index of the Fibonacci number
!    to compute.  N should be nonnegative.
!
!  Output:
!
!    integer F, the value of the N-th Fibonacci number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER F
    INTEGER N
    REAL (KIND=RK), PARAMETER :: SQRT5 = 2.236068D+00
    REAL (KIND=RK), PARAMETER :: PHIM = (1.0D+00-SQRT5) / 2.0D+00
    REAL (KIND=RK), PARAMETER :: PHIP = (1.0D+00+SQRT5) / 2.0D+00
 
    IF (N < 0) THEN
        F = 0
    ELSE
        F = NINT ((PHIP**N-PHIM**N)/SQRT(5.0D+00))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FIBONACCI_FLOOR (N, F, I)
 
!*****************************************************************************80
!
!! fibonacci_floor() returns the largest Fibonacci number less than or equal to N.
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
!    Input, integer N, the positive integer whose Fibonacci
!    "floor" is desired.
!
!    Output, integer F, the largest Fibonacci number less
!    than or equal to N.
!
!    Output, integer I, the index of the F.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER F
    INTEGER I
    INTEGER N
 
    IF (N <= 0) THEN
 
        I = 0
        F = 0
 
    ELSE
 
        I = INT (LOG(0.5D+00*REAL(2*N+1, &
       & KIND=RK)*SQRT(5.0D+00))/LOG(0.5D+00*(1.0D+00+SQRT(5.0D+00))))
 
        CALL FIBONACCI_DIRECT (I, F)
 
        IF (N < F) THEN
            I = I - 1
            CALL FIBONACCI_DIRECT (I, F)
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE FIBONACCI_RECURSIVE (N, F)
 
!*****************************************************************************80
!
!! fibonacci_recursive() computes the first N Fibonacci numbers.
!
!  Discussion:
!
!    The 'golden ratio'
!
!      PHI = (1+sqrt(5))/2
!
!    satisfies the algebraic equation:
!
!      X*X-X-1=0
!
!    which is often written as:
!
!       X        1
!      --- =  ------
!       1      X - 1
!
!    expressing the fact that a rectangle, whose sides are in proportion X:1,
!    is similar to the rotated rectangle after a square of side 1 is removed.
!
!      <----X---->
!
!      +-----*---*
!      |     |   |  1
!      |     |   |
!      +-----*---+
!      <--1-><X-1>
!
!    A direct formula for the N-th Fibonacci number can be found.
!
!    Let
!
!      PHIP = ( 1 + sqrt(5) ) / 2
!      PHIM = ( 1 - sqrt(5) ) / 2
!
!    Then
!
!      F(N) = ( PHIP^N + PHIM^N ) / sqrt(5)
!
!    Moreover, F(N) can be computed by computing PHIP^N / sqrt(5) and rounding
!    to the nearest whole number.
!
!    The function
!
!      F(X) = X / ( 1 - X - X^2 )
!
!    has a power series whose coefficients are the Fibonacci numbers:
!
!      F(X) = 0 + 1*X + 1*X^2 + 2*X^3 + 3*X^4 + 5*X^5+...
!
!  First terms:
!
!      0
!      1
!      1
!      2
!      3
!      5
!      8
!     13
!     21
!     34
!     55
!     89
!    144
!
!    The 40th number is                  102,334,155.
!    The 50th number is               12,586,269,025.
!    The 100th number is 354,224,848,179,261,915,075.
!
!  Recursion:
!
!    F(0) = 0
!    F(1) = 1
!
!    F(N) = F(N-1) + F(N-2)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the highest Fibonacci number to compute.
!
!    Output, integer F(N), the first N Fibonacci numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER F (N)
    INTEGER I
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    F (1) = 1
 
    IF (N <= 1) THEN
        RETURN
    END IF
 
    F (2) = 1
 
    DO I = 3, N
        F (I) = F (I-1) + F (I-2)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
RECURSIVE FUNCTION G_HOFSTADTER (N) RESULT (VALUE)
 
!*****************************************************************************80
!
!! g_hofstadter() computes the Hofstadter G sequence.
!
!  Discussion:
!
!    G(N) = 0                      if N = 0
!         = N - G ( G ( N - 1 ) ), otherwise.
!
!    G(N) is defined for all nonnegative integers.
!
!    The value of G(N) turns out to be related to the Zeckendorf
!    representation of N as a sum of non-consecutive Fibonacci numbers.
!    To compute G(N), determine the Zeckendorf representation:
!
!      N = sum ( 1 <= I <= M ) F(I)
!
!    and reduce the index of each Fibonacci number by 1:
!
!      G(N) = sum ( 1 <= I <= M ) F(I-1)
!
!    However, this is NOT how the computation is done in this routine.
!    Instead, a straightforward recursive function call is defined
!    to correspond to the definition of the mathematical function.
!
!  Table:
!
!     N  G(N)  Zeckendorf   Decremented
!    --  ----  ----------   -----------
!
!     1   1    1            1
!     2   1    2            1
!     3   2    3            2
!     4   3    3 + 1        2 + 1
!     5   3    5            3
!     6   4    5 + 1        3 + 1
!     7   4    5 + 2        3 + 1
!     8   5    8            5
!     9   6    8 + 1        5 + 1
!    10   6    8 + 2        5 + 1
!    11   7    8 + 3        5 + 2
!    12   8    8 + 3 + 1    5 + 2 + 1
!    13   8    13           8
!    14   9    13 + 1       8 + 1
!    15   9    13 + 2       8 + 1
!    16  10    13 + 3       8 + 2
!    17  11    13 + 3 + 1   8 + 2 + 1
!    18  11    13 + 5       8 + 3
!    19  12    13 + 5 + 1   8 + 3 + 1
!    20  12    13 + 5 + 2   8 + 3 + 1
!    21  13    21           13
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Douglas Hofstadter,
!    Goedel, Escher, Bach,
!    Basic Books, 1979,
!    ISBN: 0465026567,
!    LC: QA9.8H63.
!
!  Parameters:
!
!    Input, integer N, the argument of the function.
!
!    Output, integer G_HOFSTADTER, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER ARG
    INTEGER N
    INTEGER VALUE
 
    IF (N <= 0) THEN
        VALUE = 0
    ELSE
        ARG = N - 1
        VALUE = N - G_HOFSTADTER (G_HOFSTADTER(ARG))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_LOG_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! gamma_log_values() returns some values of the Log Gamma function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2001
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 18
 
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: BVEC = (/ 1.524064183D+00, 0.7966780066D+00, &
   & 0.3982337117D+00, 0.1520599127D+00, 0.000000000D+00, - 0.04987246543D+00, - &
   & 0.08537410945D+00, - 0.1081747934D+00, - 0.1196128950D+00, - 0.1207822040D+00, - &
   & 0.1125917658D+00, - 0.09580771625D+00, - 0.07108385116D+00, - 0.03898428380D+00, &
   & 0.000000000D+00, 12.80182743D+00, 39.33988571D+00, 71.25704193D+00 /)
    REAL (KIND=RK) FX
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: XVEC = (/ 0.2D+00, 0.4D+00, 0.6D+00, 0.8D+00, &
   & 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, 1.4D+00, 1.5D+00, 1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, &
   & 2.0D+00, 10.0D+00, 20.0D+00, 30.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        X = XVEC (N_DATA)
        FX = BVEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! gamma_values() returns some values of the Gamma function.
!
!  Discussion:
!
!    The Gamma function is defined as:
!
!      Gamma(Z) = Integral ( 0 <= t < +oo) t^(Z-1) exp(-t) dt
!
!    It satisfies the recursion:
!
!      Gamma(X+1) = X * Gamma(X)
!
!    Gamma is undefined for nonpositive integral X.
!    Gamma(0.5) = sqrt(PI)
!    For N a positive integer, Gamma(N+1) = N!, the standard factorial.
!
!    In Mathematica, the function can be evaluated by:
!
!      Gamma[x]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 May 2007
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 25
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ - 0.3544907701811032D+01, - &
   & 0.1005871979644108D+03, 0.9943258511915060D+02, 0.9513507698668732D+01, &
   & 0.4590843711998803D+01, 0.2218159543757688D+01, 0.1772453850905516D+01, &
   & 0.1489192248812817D+01, 0.1164229713725303D+01, 0.1000000000000000D+01, &
   & 0.9513507698668732D+00, 0.9181687423997606D+00, 0.8974706963062772D+00, &
   & 0.8872638175030753D+00, 0.8862269254527580D+00, 0.8935153492876903D+00, &
   & 0.9086387328532904D+00, 0.9313837709802427D+00, 0.9617658319073874D+00, &
   & 0.1000000000000000D+01, 0.2000000000000000D+01, 0.6000000000000000D+01, &
   & 0.3628800000000000D+06, 0.1216451004088320D+18, 0.8841761993739702D+31 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ - 0.50D+00, - 0.01D+00, 0.01D+00, &
   & 0.10D+00, 0.20D+00, 0.40D+00, 0.50D+00, 0.60D+00, 0.80D+00, 1.00D+00, 1.10D+00, 1.20D+00, &
   & 1.30D+00, 1.40D+00, 1.50D+00, 1.60D+00, 1.70D+00, 1.80D+00, 1.90D+00, 2.00D+00, 3.00D+00, &
   & 4.00D+00, 10.00D+00, 20.00D+00, 30.00D+00 /)
 
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
 
SUBROUTINE GEGENBAUER_POLY (N, ALPHA, X, CX)
 
!*****************************************************************************80
!
!! gegenbauer_poly() computes the Gegenbauer polynomials C(I,ALPHA,X).
!
!  Discussion:
!
!    The Gegenbauer polynomial can be evaluated in Mathematica with
!    the command
!
!      GegenbauerC[n,m,x]
!
!    ALPHA must be greater than -0.5.
!
!    If ALPHA = 1, the Gegenbauer polynomials reduce to the Chebyshev
!    polynomials of the second kind.
!
!  Differential equation:
!
!    (1-X*X) Y'' - (2 ALPHA + 1) X Y' + N (N + 2 ALPHA) Y = 0
!
!  Recursion:
!
!    C(0,ALPHA,X) = 1,
!    C(1,ALPHA,X) = 2*ALPHA*X
!    C(N,ALPHA,X) = ( (2*N-2+2*ALPHA) * X * C(N-1,ALPHA,X)
!                   + ( -N+2-2*ALPHA)     * C(N-2,ALPHA,X) ) / N
!
!  Norm:
!
!    Integral ( -1 <= X <= 1 )
!      ( 1 - X^2 )^( ALPHA - 0.5 ) * C(N,ALPHA,X)^2 dX
!
!    = PI * 2^( 1 - 2 * ALPHA ) * Gamma ( N + 2 * ALPHA )
!      / ( N! * ( N + ALPHA ) * ( Gamma ( ALPHA ) )^2 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2004
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Input, real ( kind = rk ) ALPHA, a parameter which is part of the
!    definition of the Gegenbauer polynomials.  It must be greater than -0.5.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials
!    are to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 Gegenbauer
!    polynomials at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ALPHA
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    IF (ALPHA <=-0.5D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GEGENBAUER_POLY - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Illegal value of ALPHA = ', ALPHA
        WRITE (*, '(a)') '  but ALPHA must be greater than -0.5.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = 2.0D+00 * ALPHA * X
 
    DO I = 2, N
        CX (I) = ((REAL(2*I-2, KIND=RK)+2.0D+00*ALPHA)*X*CX(I-1)+(REAL(-I+2, &
       & KIND=RK)-2.0D+00*ALPHA)*CX(I-2)) / REAL (I, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEGENBAUER_POLY_VALUES (N_DATA, N, A, X, FX)
 
!*****************************************************************************80
!
!! gegenbauer_poly_values() returns some values of the Gegenbauer polynomials.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2004
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order parameter of the function.
!
!    Output, real ( kind = rk ) A, the real parameter of the function.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 38
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.0D+00, 1.0D+00, 2.0D+00, &
   & 3.0D+00, 4.0D+00, 5.0D+00, 6.0D+00, 7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00, 3.0D+00, 3.0D+00, &
   & 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00, &
   & 3.0D+00, 3.0D+00, 3.0D+00, 3.0D+00 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 1.0000000000D+00, 0.2000000000D+00, &
   & - 0.4400000000D+00, - 0.2800000000D+00, 0.2320000000D+00, 0.3075200000D+00, - &
   & 0.0805760000D+00, - 0.2935168000D+00, - 0.0395648000D+00, 0.2459712000D+00, &
   & 0.1290720256D+00, 0.0000000000D+00, - 0.3600000000D+00, - 0.0800000000D+00, &
   & 0.8400000000D+00, 2.4000000000D+00, 4.6000000000D+00, 7.4400000000D+00, 10.9200000000D+00, &
   & 15.0400000000D+00, 19.8000000000D+00, 25.2000000000D+00, - 9.0000000000D+00, - &
   & 0.1612800000D+00, - 6.6729600000D+00, - 8.3750400000D+00, - 5.5267200000D+00, &
   & 0.0000000000D+00, 5.5267200000D+00, 8.3750400000D+00, 6.6729600000D+00, 0.1612800000D+00, &
   & - 9.0000000000D+00, - 15.4252800000D+00, - 9.6969600000D+00, 22.4409600000D+00, &
   & 100.8892800000D+00, 252.0000000000D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 2, 2, 2, &
   & 2, 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.20D+00, 0.20D+00, 0.20D+00, &
   & 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.20D+00, 0.40D+00, &
   & 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, &
   & 0.40D+00, - 0.50D+00, - 0.40D+00, - 0.30D+00, - 0.20D+00, - 0.10D+00, 0.00D+00, 0.10D+00, &
   & 0.20D+00, 0.30D+00, 0.40D+00, 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.90D+00, 1.00D+00 &
   & /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        A = 0.0
        X = 0.0
        FX = 0.0
    ELSE
        N = N_VEC (N_DATA)
        A = A_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEN_HERMITE_POLY (N, X, MU, P)
 
!*****************************************************************************80
!
!! gen_hermite_poly() evaluates the generalized Hermite polynomials at X.
!
!  Discussion:
!
!    The generalized Hermite polynomials are orthogonal under the weight
!    function:
!
!      w(x) = |x|^(2*MU) * exp ( - x^2 )
!
!    over the interval (-oo,+oo).
!
!    When MU = 0, the generalized Hermite polynomial reduces to the standard
!    Hermite polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2010
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Theodore Chihara,
!    An Introduction to Orthogonal Polynomials,
!    Gordon and Breach, 1978,
!    ISBN: 0677041500,
!    LC: QA404.5 C44.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials are
!    to be evaluated.
!
!    Input, real ( kind = rk ) MU, the parameter.
!    - 1 / 2 < MU.
!
!    Output, real ( kind = rk ) P(0:N), the values of the first N+1
!    polynomials at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    REAL (KIND=RK) MU
    REAL (KIND=RK) P (0:N)
    REAL (KIND=RK) THETA
    REAL (KIND=RK) X
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    P (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    P (1) = 2.0D+00 * X
 
    DO I = 1, N - 1
 
        IF (MOD(I, 2) == 0) THEN
            THETA = 0.0D+00
        ELSE
            THETA = 2.0D+00 * MU
        END IF
 
        P (I+1) = 2.0D+00 * X * P (I) - 2.0D+00 * (REAL(I, KIND=RK)+THETA) * P (I-1)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GEN_LAGUERRE_POLY (N, ALPHA, X, CX)
 
!*****************************************************************************80
!
!! gen_laguerre_poly() evaluates generalized Laguerre polynomials.
!
!  Differential equation:
!
!    X * Y'' + (ALPHA+1-X) * Y' + N * Y = 0
!
!  Recursion:
!
!    L(0,ALPHA,X) = 1
!    L(1,ALPHA,X) = 1+ALPHA-X
!
!    L(N,ALPHA,X) = ( (2*N-1+ALPHA-X) * L(N-1,ALPHA,X)
!                   - (N-1+ALPHA) * L(N-2,ALPHA,X) ) / N
!
!  Restrictions:
!
!    -1 < ALPHA
!
!  Special values:
!
!    For ALPHA = 0, the generalized Laguerre polynomial L(N,ALPHA,X)
!    is equal to the Laguerre polynomial L(N,X).
!
!    For ALPHA integral, the generalized Laguerre polynomial
!    L(N,ALPHA,X) equals the associated Laguerre polynomial L(N,ALPHA,X).
!
!  Norm:
!
!    Integral ( 0 <= X < +oo ) exp ( - X ) * L(N,ALPHA,X)^2 dX
!    = Gamma ( N + ALPHA + 1 ) / N!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2002
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
!  Parameters:
!
!    Input, integer N, the highest order function to compute.
!
!    Input, real ( kind = rk ) ALPHA, the parameter.  -1 < ALPHA is required.
!
!    Input, real ( kind = rk ) X, the point at which the functions are to be
!    evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the functions of
!    degrees 0 through N evaluated at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ALPHA
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    IF (ALPHA <=-1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'GEN_LAGUERRE_POLY - Fatal error!'
        WRITE (*, '(a,g14.6)') '  The input value of ALPHA is ', ALPHA
        WRITE (*, '(a)') '  but ALPHA must be greater than -1.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = 1.0D+00 + ALPHA - X
 
    DO I = 2, N
        CX (I) = ((REAL(2*I-1, KIND=RK)+ALPHA-X)*CX(I-1)+(REAL(-I+1, KIND=RK)-ALPHA)*CX(I-2)) / &
       & REAL (I, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GUD (X)
 
!*****************************************************************************80
!
!! gud() evaluates the Gudermannian function.
!
!  Discussion:
!
!    The Gudermannian function relates the hyperbolic and trigonometric
!    functions.  For any argument X, there is a corresponding value
!    GAMMA so that
!
!      sinh(x) = tan(gamma).
!
!    The value GAMMA is called the Gudermannian of X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the Gudermannian.
!
!    Output, real ( kind = rk ) GUD, the value of the Gudermannian.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) GUD
    REAL (KIND=RK) X
 
    GUD = 2.0D+00 * ATAN (TANH(0.5D+00*X))
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GUD_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! gud_values() returns some values of the Gudermannian function.
!
!  Discussion:
!
!    The Gudermannian function relates the hyperbolic and trigonometric
!    functions.  For any argument X, there is a corresponding value
!    gd(x) so that
!
!      SINH(x) = TAN(gd(x)).
!
!    This value GD is called the Gudermannian of X and symbolized
!    GD(X).  The inverse Gudermannian function is given as input a value
!    GD and computes the corresponding value X.
!
!    GD(X) = 2 * arctan ( exp ( X ) ) - PI / 2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 2004
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 13
 
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ - 1.301760336D+00, - &
   & 0.8657694832D+00, 0.0000000000D+00, 0.09983374879D+00, 0.1986798470D+00, 0.4803810791D+00, &
   & 0.8657694832D+00, 1.131728345D+00, 1.301760336D+00, 1.406993569D+00, 1.471304341D+00, &
   & 1.510419908D+00, 1.534169144D+00 /)
    REAL (KIND=RK) FX
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ - 2.0D+00, - 1.0D+00, 0.0D+00, &
   & 0.1D+00, 0.2D+00, 0.5D+00, 1.0D+00, 1.5D+00, 2.0D+00, 2.5D+00, 3.0D+00, 3.5D+00, 4.0D+00 &
   & /)
 
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
 
RECURSIVE FUNCTION H_HOFSTADTER (N) RESULT (VALUE)
 
!*****************************************************************************80
!
!! h_hofstadter() computes the Hofstadter H sequence.
!
!  Discussion:
!
!    H(N) = 0                          if N = 0
!         = N - H ( H ( H ( N - 1 ) ), otherwise.
!
!    H(N) is defined for all nonnegative integers.
!
!  Example:
!
!     N  H(N)
!    --  ----
!
!     0     0
!     1     1
!     2     1
!     3     2
!     4     3
!     5     4
!     6     4
!     7     5
!     8     5
!     9     6
!    10     7
!    11     7
!    12     8
!    13     9
!    14    10
!    15    10
!    16    11
!    17    12
!    18    13
!    19    13
!    20    14
!    21    14
!    22    15
!    23    16
!    24    17
!    25    17
!    26    18
!    27    18
!    28    19
!    29    20
!    30    20
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
!    Douglas Hofstadter,
!    Goedel, Escher, Bach,
!    Basic Books, 1979,
!    ISBN: 0465026567,
!    LC: QA9.8H63.
!
!  Parameters:
!
!    Input, integer N, the argument of the function.
!
!    Output, integer H_HOFSTADTER, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER ARG
    INTEGER N
    INTEGER VALUE
 
    IF (N <= 0) THEN
        VALUE = 0
    ELSE
        ARG = N - 1
        VALUE = N - H_HOFSTADTER (H_HOFSTADTER(H_HOFSTADTER(ARG)))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION HARMONIC (N)
 
!*****************************************************************************80
!
!! harmonic() computes the Nth harmonic number.
!
!  Discussion:
!
!    H(N) = Sum ( 1 <= I <= N ) 1 / I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2022
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N: the index of the harmonic number.
!
!  Output:
!
!    real ( kind = rk ) harmonic: the value of the harmonic number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) HARMONIC
    INTEGER I
    INTEGER N
    REAL (KIND=RK) VALUE
 
    VALUE = 0.0D+00
    DO I = 1, N
        VALUE = VALUE + 1.0D+00 / REAL (I, KIND=RK)
    END DO
 
    HARMONIC = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION HARMONIC_ESTIMATE (N)
 
!*****************************************************************************80
!
!! harmonic_estimate() estimates the Nth harmonic number.
!
!  Discussion:
!
!    H(N) = Sum ( 1 <= I <= N ) 1 / I
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 May 2022
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N: the index of the harmonic number.
!
!  Output:
!
!    real ( kind = rk ) harmonic_estimate: the estimated value of the harmonic number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) HARMONIC_ESTIMATE
    INTEGER N
    REAL (KIND=RK) N_REAL
    REAL (KIND=RK) VALUE
 
    N_REAL = REAL (N, KIND=RK)
 
    VALUE = LOG (N_REAL) + EULER_MASCHERONI () + 0.5D+00 / N_REAL - 1.0D+00 / 2.0D+00 / N_REAL &
   & ** 2 + 1.0D+00 / 120.0D+00 / N_REAL ** 4
 
    HARMONIC_ESTIMATE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HARMONIC_VALUES (N_DATA, N, H)
 
!*****************************************************************************80
!
!! harmonic_values() returns some values of the Harmonic number sequence.
!
!  Discussion:
!
!    H(N) = sum ( 1 <= I <= N ) 1 / I
!
!    In Mathematica, the function can be evaluated by:
!
!      HarmonicNumber[n]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2022
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N_DATA.  The user sets N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    integer N, the index of the Harmonic number.
!
!    real ( kind = rk ) H, the value of the Harmonic number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 40
 
    REAL (KIND=RK) H
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: H_VEC = (/ 1.000000000000000D+00, &
   & 1.500000000000000D+00, 1.833333333333333D+00, 2.083333333333333D+00, &
   & 2.283333333333333D+00, 2.450000000000000D+00, 2.592857142857143D+00, &
   & 2.717857142857143D+00, 2.828968253968254D+00, 2.928968253968254D+00, &
   & 3.019877344877345D+00, 3.103210678210678D+00, 3.180133755133755D+00, &
   & 3.251562326562327D+00, 3.318228993228993D+00, 3.380728993228993D+00, &
   & 3.439552522640758D+00, 3.495108078196313D+00, 3.547739657143682D+00, &
   & 3.597739657143682D+00, 3.645358704762730D+00, 3.690813250217275D+00, &
   & 3.734291511086840D+00, 3.775958177753507D+00, 3.815958177753507D+00, &
   & 3.854419716215045D+00, 3.891456753252082D+00, 3.927171038966368D+00, &
   & 3.961653797587058D+00, 3.994987130920391D+00, 4.027245195436520D+00, &
   & 4.058495195436520D+00, 4.088798225739550D+00, 4.118209990445433D+00, &
   & 4.146781419016861D+00, 4.174559196794639D+00, 4.201586223821666D+00, &
   & 4.227902013295350D+00, 4.253543038936376D+00, 4.278543038936376D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, &
   & 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, &
   & 36, 37, 38, 39, 40 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        H = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        H = H_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HERMITE_POLY_PHYS (N, X, CX)
 
!*****************************************************************************80
!
!! hermite_poly_phys() evaluates the physicist's Hermite polynomials at X.
!
!  Differential equation:
!
!    Y'' - 2 X Y' + 2 N Y = 0
!
!  First terms:
!
!      1
!      2 X
!      4 X^2     -  2
!      8 X^3     - 12 X
!     16 X^4     - 48 X^2     + 12
!     32 X^5    - 160 X^3    + 120 X
!     64 X^6    - 480 X^4    + 720 X^2    - 120
!    128 X^7   - 1344 X^5   + 3360 X^3   - 1680 X
!    256 X^8   - 3584 X^6  + 13440 X^4  - 13440 X^2   + 1680
!    512 X^9   - 9216 X^7  + 48384 X^5  - 80640 X^3  + 30240 X
!   1024 X^10 - 23040 X^8 + 161280 X^6 - 403200 X^4 + 302400 X^2 - 30240
!
!  Recursion:
!
!    H(0,X) = 1,
!    H(1,X) = 2*X,
!    H(N,X) = 2*X * H(N-1,X) - 2*(N-1) * H(N-2,X)
!
!  Norm:
!
!    Integral ( -oo < X < oo ) exp ( - X^2 ) * H(N,X)^2 dX
!    = sqrt ( PI ) * 2^N * N!
!
!    H(N,X) = (-1)^N * exp ( X^2 ) * dn/dXn ( exp(-X^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 October 2002
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
!    Larry Andrews,
!    Special Functions of Mathematics for Engineers,
!    Second Edition,
!    Oxford University Press, 1998.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials are
!    to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 Hermite
!    polynomials at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = 2.0D+00 * X
 
    DO I = 2, N
        CX (I) = 2.0D+00 * X * CX (I-1) - 2.0D+00 * REAL (I-1, KIND=RK) * CX (I-2)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HERMITE_POLY_PHYS_COEF (N, C)
 
!*****************************************************************************80
!
!! hermite_poly_phys_coef(): coefficients of physicist Hermite polynomial H(n,x).
!
!  First terms:
!
!    N/K     0     1      2      3       4     5      6    7      8    9   10
!
!     0      1
!     1      0     2
!     2     -2     0      4
!     3      0   -12      0      8
!     4     12     0    -48      0      16
!     5      0   120      0   -160       0    32
!     6   -120     0    720      0    -480     0     64
!     7      0 -1680      0   3360       0 -1344      0   128
!     8   1680     0 -13440      0   13440     0  -3584     0    256
!     9      0 30240      0 -80640       0 48384      0 -9216      0 512
!    10 -30240     0 302400      0 -403200     0 161280     0 -23040   0 1024
!
!  Recursion:
!
!    H(0,X) = 1,
!    H(1,X) = 2*X,
!    H(N,X) = 2*X * H(N-1,X) - 2*(N-1) * H(N-2,X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the Hermite
!    polynomials.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N, 0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0:N, 0:N) = 0.0D+00
 
    C (0, 0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    C (1, 1) = 2.0D+00
 
    DO I = 2, N
        C (I, 0) = - 2.0D+00 * REAL (I-1, KIND=RK) * C (I-2, 0)
        C (I, 1:I-2) = 2.0D+00 * C (I-1, 0:I-3) - 2.0D+00 * REAL (I-1, KIND=RK) * C (I-2, &
       & 1:I-2)
        C (I, I-1) = 2.0D+00 * C (I-1, I-2)
        C (I, I) = 2.0D+00 * C (I-1, I-1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HERMITE_POLY_PHYS_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! hermite_poly_phys_values() returns some values of the Hermite polynomial.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) X, the point where the polynomial is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 17
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FX_VEC = (/ 1.0D+00, 10.0D+00, 98.0D+00, &
   & 940.0D+00, 8812.0D+00, 80600.0D+00, 717880.0D+00, 6211600.0D+00, 520656800.0D+00, &
   & 421271200D+00, 3275529760.0D+00, 24329873600.0D+00, 171237081280.0D+00, 41.0D+00, - &
   & 8.0D+00, 3816.0D+00, 3041200.0D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 5, &
   & 5, 5, 5 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, &
   & 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 0.5D+00, &
   & 1.0D+00, 3.0D+00, 10.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE HYPER_2F1_VALUES (N_DATA, A, B, C, X, FX)
 
!*****************************************************************************80
!
!! hyper_2f1_values() returns some values of the hypergeometric function 2F1.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      fx = Hypergeometric2F1 [ a, b, c, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 September 2007
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
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996,
!    ISBN: 0-8493-2479-3,
!    LC: QA47.M315.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) A, B, C, X, the parameters.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 24
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ - 2.5D+00, - 0.5D+00, 0.5D+00, &
   & 2.5D+00, - 2.5D+00, - 0.5D+00, 0.5D+00, 2.5D+00, - 2.5D+00, - 0.5D+00, 0.5D+00, 2.5D+00, &
   & 3.3D+00, 1.1D+00, 1.1D+00, 3.3D+00, 3.3D+00, 1.1D+00, 1.1D+00, 3.3D+00, 3.3D+00, 1.1D+00, &
   & 1.1D+00, 3.3D+00 /)
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 3.3D+00, 1.1D+00, 1.1D+00, 3.3D+00, &
   & 3.3D+00, 1.1D+00, 1.1D+00, 3.3D+00, 3.3D+00, 1.1D+00, 1.1D+00, 3.3D+00, 6.7D+00, 6.7D+00, &
   & 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00 &
   & /)
    REAL (KIND=RK) C
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: C_VEC = (/ 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, &
   & 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, 6.7D+00, - 5.5D+00, - &
   & 0.5D+00, 0.5D+00, 4.5D+00, - 5.5D+00, - 0.5D+00, 0.5D+00, 4.5D+00, - 5.5D+00, - 0.5D+00, &
   & 0.5D+00, 4.5D+00 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.72356129348997784913D+00, &
   & 0.97911109345277961340D+00, 1.0216578140088564160D+00, 1.4051563200112126405D+00, &
   & 0.46961431639821611095D+00, 0.95296194977446325454D+00, 1.0512814213947987916D+00, &
   & 2.3999062904777858999D+00, 0.29106095928414718320D+00, 0.92536967910373175753D+00, &
   & 1.0865504094806997287D+00, 5.7381565526189046578D+00, 15090.669748704606754D+00, - &
   & 104.31170067364349677D+00, 21.175050707768812938D+00, 4.1946915819031922850D+00, &
   & 1.0170777974048815592D+10, - 24708.635322489155868D+00, 1372.2304548384989560D+00, &
   & 58.092728706394652211D+00, 5.8682087615124176162D+18, - 4.4635010147295996680D+08, &
   & 5.3835057561295731310D+06, 20396.913776019659426D+00 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.25D+00, 0.25D+00, 0.25D+00, &
   & 0.25D+00, 0.55D+00, 0.55D+00, 0.55D+00, 0.55D+00, 0.85D+00, 0.85D+00, 0.85D+00, 0.85D+00, &
   & 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.55D+00, 0.55D+00, 0.55D+00, 0.55D+00, 0.85D+00, &
   & 0.85D+00, 0.85D+00, 0.85D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        C = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        C = C_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_CHOOSE (N, K)
 
!*****************************************************************************80
!
!! i4_choose() computes the binomial coefficient C(N,K).
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
!    02 June 2007
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
!    Input, integer N, K, are the values of N and K.
!
!    Output, integer I4_CHOOSE, the number of combinations of N
!    things taken K at a time.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER I4_CHOOSE
    INTEGER K
    INTEGER MN
    INTEGER MX
    INTEGER N
    INTEGER VALUE
 
    MN = MIN (K, N-K)
 
    IF (MN < 0) THEN
 
        VALUE = 0
 
    ELSE IF (MN == 0) THEN
 
        VALUE = 1
 
    ELSE
 
        MX = MAX (K, N-K)
        VALUE = MX + 1
 
        DO I = 2, MN
            VALUE = (VALUE*(MX+I)) / I
        END DO
 
    END IF
 
    I4_CHOOSE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
!*****************************************************************************80
!
!! i4_factor() factors an integer into prime factors.
!
!  Discussion:
!
!    This routine tries to decompose an integer into prime factors,
!    but the decomposition may be left incomplete either because
!    there are too many factors for the allowed space, or because
!    the factors are too large for the table of primes to handle.
!
!    The form of the factorization therefore includes an unresolved
!    factor "NLEFT":
!
!      N = NLEFT * Product ( I = 1 to NFACTOR ) FACTOR(I)^POWER(I).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be factored.  N may be
!    positive, negative, or 0.
!
!    Input, integer MAXFACTOR, the maximum number of prime
!    factors for which storage has been allocated.
!
!    Output, integer NFACTOR, the number of prime factors of
!    N discovered by the routine.
!
!    Output, integer FACTOR(MAXFACTOR), the prime factors of N.
!
!    Output, integer POWER(MAXFACTOR).  POWER(I) is the power of
!    the FACTOR(I) in the representation of N.
!
!    Output, integer NLEFT, the factor of N that the routine
!    could not divide out.  If NLEFT is 1, then N has been completely factored.
!    Otherwise, NLEFT represents factors of N involving large primes.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER MAXFACTOR
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER MAXPRIME
    INTEGER N
    INTEGER NLEFT
    INTEGER NFACTOR
    INTEGER P
    INTEGER POWER (MAXFACTOR)
 
    NFACTOR = 0
 
    FACTOR (1:MAXFACTOR) = 0
    POWER (1:MAXFACTOR) = 0
 
    NLEFT = N
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    IF (ABS(N) == 1) THEN
        NFACTOR = 1
        FACTOR (1) = 1
        POWER (1) = 1
        RETURN
    END IF
!
!  Find out how many primes we stored.
!
    MAXPRIME = PRIME (-1)
!
!  Try dividing the remainder by each prime.
!
    DO I = 1, MAXPRIME
 
        P = PRIME (I)
 
        IF (MOD(ABS(NLEFT), P) == 0) THEN
 
            IF (NFACTOR < MAXFACTOR) THEN
 
                NFACTOR = NFACTOR + 1
                FACTOR (NFACTOR) = P
 
                DO
 
                    POWER (NFACTOR) = POWER (NFACTOR) + 1
                    NLEFT = NLEFT / P
 
                    IF (MOD(ABS(NLEFT), P) /= 0) THEN
                        EXIT
                    END IF
 
                END DO
 
                IF (ABS(NLEFT) == 1) THEN
                    EXIT
                END IF
 
            END IF
 
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_FACTORIAL (N)
 
!*****************************************************************************80
!
!! i4_factorial() computes the factorial of N.
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
!    26 June 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!    0 <= N <= 13 is required.
!
!    Output, integer I4_FACTORIAL, the factorial of N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER I4_FACTORIAL
    INTEGER N
 
    I4_FACTORIAL = 1
 
    IF (13 < N) THEN
        I4_FACTORIAL = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_FACTORIAL - Fatal error!'
        WRITE (*, '(a)') '  I4_FACTORIAL(N) cannot be computed as an integer'
        WRITE (*, '(a)') '  for 13 < N.'
        WRITE (*, '(a,i8)') '  Input value N = ', N
        STOP 1
    END IF
 
    DO I = 1, N
        I4_FACTORIAL = I4_FACTORIAL * I
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_FACTORIAL_VALUES (N_DATA, N, FN)
 
!*****************************************************************************80
!
!! i4_factorial_values() returns values of the factorial function.
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
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the function.
!
!    Output, integer FN, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 13
 
    INTEGER, SAVE, DIMENSION (NMAX) :: FNVEC = (/ 1, 1, 2, 6, 24, 120, 720, 5040, 40320, &
   & 362880, 3628800, 39916800, 479001600 /)
    INTEGER FN
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: NVEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        FN = 0
    ELSE
        N = NVEC (N_DATA)
        FN = FNVEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_FACTORIAL2 (N)
 
!*****************************************************************************80
!
!! i4_factorial2() computes the double factorial function.
!
!  Discussion:
!
!    The formula is:
!
!      FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                      = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N    Factorial2(N)
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the argument of the double factorial
!    function.  If N is less than 1, I4_FACTORIAL2 is returned as 1.
!
!    Output, integer I4_FACTORIAL2, the value of the function..
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I4_FACTORIAL2
    INTEGER N
    INTEGER N_COPY
 
    IF (N < 1) THEN
        I4_FACTORIAL2 = 1
        RETURN
    END IF
 
    N_COPY = N
    I4_FACTORIAL2 = 1
 
    DO WHILE ( 1 < N_COPY)
        I4_FACTORIAL2 = I4_FACTORIAL2 * N_COPY
        N_COPY = N_COPY - 2
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_FACTORIAL2_VALUES (N_DATA, N, FN)
 
!*****************************************************************************80
!
!! i4_factorial2_values() returns values of the double factorial function.
!
!  Discussion:
!
!    The formula is:
!
!      FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
!                      = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
!
!  Example:
!
!     N    Factorial2(N)
!
!     0     1
!     1     1
!     2     2
!     3     3
!     4     8
!     5    15
!     6    48
!     7   105
!     8   384
!     9   945
!    10  3840
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2004
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
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, page 16.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the function.
!
!    Output, integer FN, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 16
 
    INTEGER, SAVE, DIMENSION (N_MAX) :: FN_VEC = (/ 1, 1, 2, 3, 8, 15, 48, 105, 384, 945, 3840, &
   & 10395, 46080, 135135, 645120, 2027025 /)
    INTEGER FN
    INTEGER N_DATA
    INTEGER N
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, &
   & 13, 14, 15 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        FN = 0
    ELSE
        N = N_VEC (N_DATA)
        FN = FN_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_HUGE ()
 
!*****************************************************************************80
!
!! i4_huge() returns a "huge" I4.
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
!    An I4 is an integer value.
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
!    Output, integer I4_HUGE, a "huge" I4.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I4_HUGE
 
    I4_HUGE = 2147483647
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_IS_FIBONACCI (I4)
 
!*****************************************************************************80
!
!! i4_is_fibonacci() reports whether an integer is prime.
!
!  Discussion:
!
!    The positive integer i4 is a Fibonacci number if and only if
!    5*I4^2+4 or 5*I4^2-4 is a perfect square.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I4, the integer to be tested.
!
!    Output, logical I4_IS_FIBONACCI, is TRUE if N is
!    a Fibonacci number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I4
    LOGICAL I4_IS_FIBONACCI
    INTEGER T1
    REAL (KIND=RK) T2
    INTEGER T3
 
    I4_IS_FIBONACCI = .FALSE.
 
    IF (I4 <= 0) THEN
        RETURN
    END IF
 
    T1 = 5 * I4 ** 2 + 4
    T2 = SQRT (REAL(T1, KIND=RK))
    T3 = INT (T2+0.5D+00)
    IF (T3*T3 == T1) THEN
        I4_IS_FIBONACCI = .TRUE.
        RETURN
    END IF
 
    T1 = 5 * I4 ** 2 - 4
    T2 = SQRT (REAL(T1, KIND=RK))
    T3 = INT (T2+0.5D+00)
    IF (T3*T3 == T1) THEN
        I4_IS_FIBONACCI = .TRUE.
        RETURN
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_IS_PRIME (N)
 
!*****************************************************************************80
!
!! i4_is_prime() reports whether an integer is prime.
!
!  Discussion:
!
!    A simple, unoptimized sieve of Erasthosthenes is used to
!    check whether N can be divided by any integer between 2
!    and SQRT(N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be tested.
!
!    Output, logical I4_IS_PRIME, is TRUE if N is prime, and FALSE
!    otherwise.  Note that negative numbers and 0 are not
!    considered prime.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    LOGICAL I4_IS_PRIME
    INTEGER N
    INTEGER NHI
 
    IF (N <= 0) THEN
        I4_IS_PRIME = .FALSE.
        RETURN
    END IF
 
    IF (N <= 3) THEN
        I4_IS_PRIME = .TRUE.
        RETURN
    END IF
 
    NHI = INT (SQRT(REAL(N, KIND=RK)))
 
    DO I = 2, NHI
        IF (MOD(N, I) == 0) THEN
            I4_IS_PRIME = .FALSE.
            RETURN
        END IF
    END DO
 
    I4_IS_PRIME = .TRUE.
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_IS_TRIANGULAR (I)
 
!*****************************************************************************80
!
!! i4_is_triangular() determines whether an integer is triangular.
!
!  Discussion:
!
!    The N-th triangular number is equal to the sum of the first
!    N integers.
!
!  First Values:
!
!    Index  Value
!     0      0
!     1      1
!     2      3
!     3      6
!     4     10
!     5     15
!     6     21
!     7     28
!     8     36
!     9     45
!    10     55
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, the integer to be checked.
!
!    Output, logical I4_IS_TRIANGULAR, is TRUE if I is triangular.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    LOGICAL I4_IS_TRIANGULAR
    INTEGER J
    INTEGER K
 
    IF (I < 0) THEN
 
        I4_IS_TRIANGULAR = .FALSE.
 
    ELSE IF (I == 0) THEN
 
        I4_IS_TRIANGULAR = .TRUE.
 
    ELSE
 
        CALL I4_TO_TRIANGLE_LOWER (I, J, K)
 
        IF (J == K) THEN
            I4_IS_TRIANGULAR = .TRUE.
        ELSE
            I4_IS_TRIANGULAR = .FALSE.
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_PARTITION_DISTINCT_COUNT (N, Q)
 
!*****************************************************************************80
!
!! i4_partition_distinct_count() returns any value of Q(N).
!
!  Discussion:
!
!    A partition of an integer N is a representation of the integer
!    as the sum of nonzero positive integers.  The order of the summands
!    does not matter.  The number of partitions of N is symbolized
!    by P(N).  Thus, the number 5 has P(N) = 7, because it has the
!    following partitions:
!
!    5 = 5
!      = 4 + 1
!      = 3 + 2
!      = 3 + 1 + 1
!      = 2 + 2 + 1
!      = 2 + 1 + 1 + 1
!      = 1 + 1 + 1 + 1 + 1
!
!    However, if we require that each member of the partition
!    be distinct, we are computing something symbolized by Q(N).
!    The number 5 has Q(N) = 3, because it has the following partitions
!    into distinct parts:
!
!    5 = 5
!      = 4 + 1
!      = 3 + 2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2003
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
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!
!    Output, integer Q, the number of partitions of the integer
!    into distinct parts.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER C (0:N)
    INTEGER I
    INTEGER K
    INTEGER K2
    INTEGER K_SIGN
    INTEGER Q
 
    C (0) = 1
 
    DO I = 1, N
 
        IF (I4_IS_TRIANGULAR(I)) THEN
            C (I) = 1
        ELSE
            C (I) = 0
        END IF
 
        K = 0
        K_SIGN = - 1
 
        DO
 
            K = K + 1
            K_SIGN = - K_SIGN
            K2 = K * (3*K+1)
 
            IF (I < K2) THEN
                EXIT
            END IF
 
            C (I) = C (I) + K_SIGN * C (I-K2)
 
        END DO
 
        K = 0
        K_SIGN = - 1
 
        DO
 
            K = K + 1
            K_SIGN = - K_SIGN
            K2 = K * (3*K-1)
 
            IF (I < K2) THEN
                EXIT
            END IF
 
            C (I) = C (I) + K_SIGN * C (I-K2)
 
        END DO
 
    END DO
 
    Q = C (N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_SWAP (I, J)
 
!*****************************************************************************80
!
!! i4_swap() swaps two I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    integer I, J, a pair of values.
!
!  Output:
!
!    integer I, J, the values have been interchanged.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
 
    K = I
    I = J
    J = K
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_TO_TRIANGLE_LOWER (K, I, J)
 
!*****************************************************************************80
!
!! i4_to_triangle_lower() converts an integer to lower triangular coordinates.
!
!  Discussion:
!
!    Triangular coordinates are handy when storing a naturally triangular
!    array (such as the lower half of a matrix) in a linear array.
!
!    Thus, for example, we might consider storing
!
!    (1,1)
!    (2,1) (2,2)
!    (3,1) (3,2) (3,3)
!    (4,1) (4,2) (4,3) (4,4)
!
!    as the linear array
!
!    (1,1) (2,1) (2,2) (3,1) (3,2) (3,3) (4,1) (4,2) (4,3) (4,4)
!
!    Here, the quantities in parenthesis represent the natural row and
!    column indices of a single number when stored in a rectangular array.
!
!    In this routine, we are given the location K of an item in the
!    linear array, and wish to determine the row I and column J
!    of the item when stored in the triangular array.
!
!  Example:
!
!     K  I  J
!
!     0  0  0
!     1  1  1
!     2  2  1
!     3  2  2
!     4  3  1
!     5  3  2
!     6  3  3
!     7  4  1
!     8  4  2
!     9  4  3
!    10  4  4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the linear index of the (I,J) element,
!    which must be nonnegative.
!
!    Output, integer I, J, the row and column indices.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
 
    IF (K < 0) THEN
 
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_TO_TRIANGLE_LOWER - Fatal error!'
        WRITE (*, '(a)') '  K < 0.'
        WRITE (*, '(a,i8)') '  K = ', K
        STOP 1
 
    ELSE IF (K == 0) THEN
 
        I = 0
        J = 0
        RETURN
 
    END IF
 
    I = INT (SQRT(REAL(2*K, KIND=RK)))
 
    IF (I*I+I < 2*K) THEN
        I = I + 1
    END IF
 
    J = K - (I*(I-1)) / 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4_TO_TRIANGLE_UPPER (K, I, J)
 
!*****************************************************************************80
!
!! i4_to_triangle_upper() converts an integer to upper triangular coordinates.
!
!  Discussion:
!
!    Triangular coordinates are handy when storing a naturally triangular
!    array (such as the upper half of a matrix) in a linear array.
!
!    Thus, for example, we might consider storing
!
!    (1,1) (1,2) (1,3) (1,4)
!          (2,2) (2,3) (2,4)
!                (3,3) (3,4)
!                      (4,4)
!
!    as the linear array
!
!    (1,1) (1,2) (2,2) (1,3) (2,3) (3,3) (1,4) (2,4) (3,4) (4,4)
!
!    Here, the quantities in parenthesis represent the natural row and
!    column indices of a single number when stored in a rectangular array.
!
!    In this routine, we are given the location K of an item in the
!    linear array, and wish to determine the row I and column J
!    of the item when stored in the triangular array.
!
!  Example:
!
!     K  I  J
!
!     0  0  0
!     1  1  1
!     2  1  2
!     3  2  2
!     4  1  3
!     5  2  3
!     6  3  3
!     7  1  4
!     8  2  4
!     9  3  4
!    10  4  4
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the linear index of the (I,J) element,
!    which must be nonnegative.
!
!    Output, integer I, J, the row and column indices.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
 
    IF (K < 0) THEN
 
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'I4_TO_TRIANGLE_LOWER - Fatal error!'
        WRITE (*, '(a)') '  K < 0.'
        WRITE (*, '(a,i8)') '  K = ', K
        STOP 1
 
    ELSE IF (K == 0) THEN
 
        I = 0
        J = 0
        RETURN
 
    END IF
 
    J = INT (SQRT(REAL(2*K, KIND=RK)))
 
    IF (J*J+J < 2*K) THEN
        J = J + 1
    END IF
 
    I = K - (J*(J-1)) / 2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION I4_UNIFORM_AB (A, B)
 
!*****************************************************************************80
!
!! i4_uniform_ab() returns a scaled pseudorandom I4.
!
!  Discussion:
!
!    An I4 is an integer value.
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
!    02 October 2012
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Second Edition,
!    Springer, 1987,
!    ISBN: 0387964673,
!    LC: QA76.9.C65.B73.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, December 1986, pages 362-376.
!
!    Pierre L'Ecuyer,
!    Random Number Generation,
!    in Handbook of Simulation,
!    edited by Jerry Banks,
!    Wiley, 1998,
!    ISBN: 0471134031,
!    LC: T57.62.H37.
!
!    Peter Lewis, Allen Goodman, James Miller,
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, Number 2, 1969, pages 136-143.
!
!  Parameters:
!
!    Input, integer A, B, the limits of the interval.
!
!  Output:
!
!    Output, integer I4_UNIFORM_AB, a number between A and B.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER A
    INTEGER B
    INTEGER I4_UNIFORM_AB
    REAL R
    INTEGER VALUE
 
    CALL RANDOM_NUMBER (HARVEST=R)
!
!  Scale R to lie between A-0.5 and B+0.5.
!
    R = (1.0E+00-R) * (REAL(MIN(A, B))-0.5E+00) + R * (REAL(MAX(A, B))+0.5E+00)
!
!  Use rounding to convert R to an integer between A and B.
!
    VALUE = NINT (R)
 
    VALUE = MAX (VALUE, MIN(A, B))
    VALUE = MIN (VALUE, MAX(A, B))
 
    I4_UNIFORM_AB = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4MAT_PRINT (M, N, A, TITLE)
 
!*****************************************************************************80
!
!! i4mat_print() prints an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of rows in A.
!
!    Input, integer N, the number of columns in A.
!
!    Input, integer A(M,N), the matrix to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER A (M, N)
    INTEGER IHI
    INTEGER ILO
    INTEGER JHI
    INTEGER JLO
    CHARACTER (LEN=*) TITLE
 
    ILO = 1
    IHI = M
    JLO = 1
    JHI = N
 
    CALL I4MAT_PRINT_SOME (M, N, A, ILO, JLO, IHI, JHI, TITLE)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE I4MAT_PRINT_SOME (M, N, A, ILO, JLO, IHI, JHI, TITLE)
 
!*****************************************************************************80
!
!! i4mat_print_some() prints some of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is a rectangular array of I4 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, N, the number of rows and columns.
!
!    Input, integer A(M,N), an M by N matrix to be printed.
!
!    Input, integer ILO, JLO, the first row and column to print.
!
!    Input, integer IHI, JHI, the last row and column to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: INCX = 10
    INTEGER M
    INTEGER N
 
    INTEGER A (M, N)
    CHARACTER (LEN=8) CTEMP (INCX)
    INTEGER I
    INTEGER I2HI
    INTEGER I2LO
    INTEGER IHI
    INTEGER ILO
    INTEGER INC
    INTEGER J
    INTEGER J2
    INTEGER J2HI
    INTEGER J2LO
    INTEGER JHI
    INTEGER JLO
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
 
    IF (M <= 0 .OR. N <= 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') '  (None)'
        RETURN
    END IF
 
    DO J2LO = MAX (JLO, 1), MIN (JHI, N), INCX
 
        J2HI = J2LO + INCX - 1
        J2HI = MIN (J2HI, N)
        J2HI = MIN (J2HI, JHI)
 
        INC = J2HI + 1 - J2LO
 
        WRITE (*, '(a)') ' '
 
        DO J = J2LO, J2HI
            J2 = J + 1 - J2LO
            WRITE (CTEMP(J2), '(i8)') J
        END DO
 
        WRITE (*, '(''  Col '',10a8)') CTEMP (1:INC)
        WRITE (*, '(a)') '  Row'
        WRITE (*, '(a)') ' '
 
        I2LO = MAX (ILO, 1)
        I2HI = MIN (IHI, M)
 
        DO I = I2LO, I2HI
 
            DO J2 = 1, INC
 
                J = J2LO - 1 + J2
 
                WRITE (CTEMP(J2), '(i8)') A (I, J)
 
            END DO
 
            WRITE (*, '(i5,a,10a8)') I, ':', (CTEMP(J), J=1, INC)
 
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE JACOBI_POLY (N, ALPHA, BETA, X, CX)
 
!*****************************************************************************80
!
!! jacobi_poly() evaluates the Jacobi polynomials at X.
!
!  Differential equation:
!
!    (1-X*X) Y'' + (BETA-ALPHA-(ALPHA+BETA+2) X) Y' + N (N+ALPHA+BETA+1) Y = 0
!
!  Recursion:
!
!    P(0,ALPHA,BETA,X) = 1,
!
!    P(1,ALPHA,BETA,X) = ( (2+ALPHA+BETA)*X + (ALPHA-BETA) ) / 2
!
!    P(N,ALPHA,BETA,X)  =
!      (
!        (2*N+ALPHA+BETA-1)
!        * ((ALPHA^2-BETA^2)+(2*N+ALPHA+BETA)*(2*N+ALPHA+BETA-2)*X)
!        * P(N-1,ALPHA,BETA,X)
!        -2*(N-1+ALPHA)*(N-1+BETA)*(2*N+ALPHA+BETA) * P(N-2,ALPHA,BETA,X)
!      ) / 2*N*(N+ALPHA+BETA)*(2*N-2+ALPHA+BETA)
!
!  Restrictions:
!
!    -1 < ALPHA
!    -1 < BETA
!
!  Norm:
!
!    Integral ( -1 <= X <= 1 ) ( 1 - X )^ALPHA * ( 1 + X )^BETA
!      * P(N,ALPHA,BETA,X)^2 dX
!    = 2^(ALPHA+BETA+1) * Gamma ( N + ALPHA + 1 ) * Gamma ( N + BETA + 1 ) /
!      ( 2 * N + ALPHA + BETA ) * N! * Gamma ( N + ALPHA + BETA + 1 )
!
!  Special values:
!
!    P(N,ALPHA,BETA,1) = (N+ALPHA)!/(N!*ALPHA!) for integer ALPHA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 October 2002
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Input, real ( kind = rk ) ALPHA, one of the parameters defining the Jacobi
!    polynomials, ALPHA must be greater than -1.
!
!    Input, real ( kind = rk ) BETA, the second parameter defining the Jacobi
!    polynomials, BETA must be greater than -1.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials are
!    to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 Jacobi
!    polynomials at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) ALPHA
    REAL (KIND=RK) BETA
    REAL (KIND=RK) CX (0:N)
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) C3
    REAL (KIND=RK) C4
    INTEGER I
    REAL (KIND=RK) R_I
    REAL (KIND=RK) X
 
    IF (ALPHA <=-1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'JACOBI_POLY - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Illegal input value of ALPHA = ', ALPHA
        WRITE (*, '(a)') '  But ALPHA must be greater than -1.'
        STOP 1
    END IF
 
    IF (BETA <=-1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'JACOBI_POLY - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Illegal input value of BETA = ', BETA
        WRITE (*, '(a)') '  But BETA must be greater than -1.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = (1.0D+00+0.5D+00*(ALPHA+BETA)) * X + 0.5D+00 * (ALPHA-BETA)
 
    DO I = 2, N
 
        R_I = REAL (I, KIND=RK)
 
        C1 = 2.0D+00 * R_I * (R_I+ALPHA+BETA) * (2.0D+00*R_I-2.0D+00+ALPHA+BETA)
 
        C2 = (2.0D+00*R_I-1.0D+00+ALPHA+BETA) * (2.0D+00*R_I+ALPHA+BETA) * &
       & (2.0D+00*R_I-2.0D+00+ALPHA+BETA)
 
        C3 = (2.0D+00*R_I-1.0D+00+ALPHA+BETA) * (ALPHA+BETA) * (ALPHA-BETA)
 
        C4 = - 2.0D+00 * (R_I-1.0D+00+ALPHA) * (R_I-1.0D+00+BETA) * (2.0D+00*R_I+ALPHA+BETA)
 
        CX (I) = ((C3+C2*X)*CX(I-1)+C4*CX(I-2)) / C1
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE JACOBI_POLY_VALUES (N_DATA, N, A, B, X, FX)
 
!*****************************************************************************80
!
!! jacobi_poly_values() returns some values of the Jacobi polynomial.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      JacobiP[ n, a, b, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 April 2012
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the degree of the polynomial.
!
!    Output, real ( kind = rk ) A, B, parameters of the function.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 26
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.0D+00, 0.0D+00 /)
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
   & 5.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1000000000000000D+01, &
   & 0.2500000000000000D+00, - 0.3750000000000000D+00, - 0.4843750000000000D+00, - &
   & 0.1328125000000000D+00, 0.2753906250000000D+00, - 0.1640625000000000D+00, - &
   & 0.1174804687500000D+01, - 0.2361328125000000D+01, - 0.2616210937500000D+01, &
   & 0.1171875000000000D+00, 0.4218750000000000D+00, 0.5048828125000000D+00, &
   & 0.5097656250000000D+00, 0.4306640625000000D+00, - 0.6000000000000000D+01, &
   & 0.3862000000000000D-01, 0.8118400000000000D+00, 0.3666000000000000D-01, - &
   & 0.4851200000000000D+00, - 0.3125000000000000D+00, 0.1891200000000000D+00, &
   & 0.4023400000000000D+00, 0.1216000000000000D-01, - 0.4396200000000000D+00, &
   & 0.1000000000000000D+01 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &
   & 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, - 1.0D+00, - 0.8D+00, - 0.6D+00, - 0.4D+00, - 0.2D+00, 0.0D+00, 0.2D+00, 0.4D+00, &
   & 0.6D+00, 0.8D+00, 1.0D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        A = 0.0D+00
        B = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE JACOBI_SYMBOL (Q, P, J)
 
!*****************************************************************************80
!
!! jacobi_symbol() evaluates the Jacobi symbol (Q/P).
!
!  Discussion:
!
!    If P is prime, then
!
!      Jacobi Symbol (Q/P) = Legendre Symbol (Q/P)
!
!    Else
!
!      let P have the prime factorization
!
!        P = Product ( 1 <= I <= N ) P(I)^E(I)
!
!      Jacobi Symbol (Q/P) =
!
!        Product ( 1 <= I <= N ) Legendre Symbol (Q/P(I))^E(I)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 86-87.
!
!  Parameters:
!
!    Input, integer Q, an integer whose Jacobi symbol with
!    respect to P is desired.
!
!    Input, integer P, the number with respect to which the Jacobi
!    symbol of Q is desired.  P should be 2 or greater.
!
!    Output, integer J, the Jacobi symbol (Q/P).
!    Ordinarily, J will be -1, 0 or 1.
!    -2, not enough factorization space.
!    -3, an error during Legendre symbol calculation.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER J
    INTEGER L
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER P
    INTEGER POWER (MAXFACTOR)
    INTEGER PP
    INTEGER Q
    INTEGER QQ
!
!  P must be greater than 1.
!
    IF (P <= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'JACOBI_SYMBOL - Fatal error!'
        WRITE (*, '(a)') '  P must be greater than 1.'
        L = - 2
        STOP 1
    END IF
!
!  Decompose P into factors of prime powers.
!
    CALL I4_FACTOR (P, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'JACOBI_SYMBOL - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space.'
        J = - 2
        STOP 1
    END IF
!
!  Force Q to be nonnegative.
!
    QQ = Q
 
    DO WHILE (QQ <  0)
        QQ = QQ + P
    END DO
!
!  For each prime factor, compute the Legendre symbol, and
!  multiply the Jacobi symbol by the appropriate factor.
!
    J = 1
    DO I = 1, NFACTOR
        PP = FACTOR (I)
        CALL LEGENDRE_SYMBOL (QQ, PP, L)
        IF (L <-1) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'JACOBI_SYMBOL - Fatal error!'
            WRITE (*, '(a)') '  Error during Legendre symbol calculation.'
            J = - 3
            STOP 1
        END IF
        J = J * L ** POWER (I)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE KRAWTCHOUK (N, P, X, M, V)
 
!*****************************************************************************80
!
!! krawtchouk() evaluates the Krawtchouk polynomials at X.
!
!  Discussion:
!
!    The polynomial has a parameter P, which must be striclty between
!    0 and 1, and a parameter M which must be a nonnegative integer.
!
!    The Krawtchouk polynomial of order N, with parameters P and M,
!    evaluated at X, may be written K(N,P,X,M).
!
!    The first two terms are:
!
!      K(0,P,X,M) = 1
!      K(1,P,X,M) = X - P * M
!
!    and the recursion, for fixed P and M is
!
!                             ( N + 1 ) * K(N+1,P,X,M) =
!        ( X - ( N + P * ( M - 2 * N))) * K(N,  P,X,M)
!       - ( M - N + 1 ) * P * ( 1 - P ) * K(N-1,P,X,M)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to evaluate.
!    0 <= N.
!
!    Input, real ( kind = rk ) P, the parameter.  0 < P < 1.
!
!    Input, real ( kind = rk ) X, the evaluation parameter.
!
!    Input, integer M, the parameter.  0 <= M.
!
!    Output, real ( kind = rk ) V(0:N), the values of the Krawtchouk polynomials
!    of orders 0 through N at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER M
    REAL (KIND=RK) P
    REAL (KIND=RK) X
    REAL (KIND=RK) V (0:N)
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'KRAWTCHOUK - Fatal error!'
        WRITE (*, '(a)') '  0 <= N is required.'
        STOP 1
    END IF
 
    IF (P <= 0.0D+00 .OR. 1.0D+00 <= P) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'KRAWTCHOUK - Fatal error!'
        WRITE (*, '(a)') '  0 < P < 1 is required.'
        STOP 1
    END IF
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'KRAWTCHOUK - Fatal error!'
        WRITE (*, '(a)') '  0 <= M is required.'
        STOP 1
    END IF
 
    V (0) = 1.0D+00
 
    IF (1 <= N) THEN
        V (1) = X - P * REAL (M, KIND=RK)
    END IF
 
    DO I = 1, N - 1
        V (I+1) = ((X-(REAL(I, KIND=RK)+P*REAL(M-2*I, KIND=RK)))*V(I)-REAL(M-I+1, &
       & KIND=RK)*P*(1.0D+00-P)*V(I-1)) / REAL (I+1, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAGUERRE_ASSOCIATED (N, M, X, CX)
 
!*****************************************************************************80
!
!! laguerre_associated() evaluates associated Laguerre polynomials L(N,M,X).
!
!  Differential equation:
!
!    X Y'' + (M+1-X) Y' + (N-M) Y = 0
!
!  First terms:
!
!    M = 0
!
!    L(0,0,X) =   1
!    L(1,0,X) =  -X   +  1
!    L(2,0,X) =   X^2 -  4 X   +  2
!    L(3,0,X) =  -X^3 +  9 X^2 -  18 X   +    6
!    L(4,0,X) =   X^4 - 16 X^3 +  72 X^2 -   96 X +     24
!    L(5,0,X) =  -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -  600 x   +  120
!    L(6,0,X) =   X^6 - 36 X^5 + 450 X^4 - 2400 X^3 + 5400 X^2 - 4320 X + 720
!
!    M = 1
!
!    L(0,1,X) =    0
!    L(1,1,X) =   -1,
!    L(2,1,X) =    2 X - 4,
!    L(3,1,X) =   -3 X^2 + 18 X - 18,
!    L(4,1,X) =    4 X^3 - 48 X^2 + 144 X - 96
!
!    M = 2
!
!    L(0,2,X) =    0
!    L(1,2,X) =    0,
!    L(2,2,X) =    2,
!    L(3,2,X) =   -6 X + 18,
!    L(4,2,X) =   12 X^2 - 96 X + 144
!
!    M = 3
!
!    L(0,3,X) =    0
!    L(1,3,X) =    0,
!    L(2,3,X) =    0,
!    L(3,3,X) =   -6,
!    L(4,3,X) =   24 X - 96
!
!    M = 4
!
!    L(0,4,X) =    0
!    L(1,4,X) =    0
!    L(2,4,X) =    0
!    L(3,4,X) =    0
!    L(4,4,X) =   24
!
!  Recursion:
!
!    if N = 0:
!
!      L(N,M,X)   = 0
!
!    if N = 1:
!
!      L(N,M,X)   = (M+1-X)
!
!    if 2 <= N:
!
!      L(N,M,X)   = ( (M+2*N-1-X) * L(N-1,M,X)
!                  +   (1-M-N)     * L(N-2,M,X) ) / N
!
!  Special values:
!
!    For M = 0, the associated Laguerre polynomials L(N,M,X) are equal
!    to the Laguerre polynomials L(N,X).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 February 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Input, integer M, the parameter.  M must be nonnegative.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials are
!    to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the associated Laguerre polynomials of
!    degrees 0 through N evaluated at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    INTEGER M
    REAL (KIND=RK) X
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LAGUERRE_ASSOCIATED - Fatal error!'
        WRITE (*, '(a,i8)') '  Input value of M = ', M
        WRITE (*, '(a)') '  but M must be nonnegative.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = REAL (M+1, KIND=RK) - X
 
    DO I = 2, N
        CX (I) = ((REAL(M+2*I-1, KIND=RK)-X)*CX(I-1)+REAL(-M-I+1, KIND=RK)*CX(I-2)) / REAL (I, &
       & KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAGUERRE_POLY (N, X, CX)
 
!*****************************************************************************80
!
!! laguerre_poly() evaluates the Laguerre polynomials at X.
!
!  Differential equation:
!
!    X * Y'' + (1-X) * Y' + N * Y = 0
!
!  First terms:
!
!      1
!     -X    +  1
!   (  X^2 -  4 X     +  2 ) / 2
!   ( -X^3 +  9 X^2 -  18 X    +    6 ) / 6
!   (  X^4 - 16 X^3 +  72 X^2 -   96 X +      24 ) / 24
!   ( -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -  600 x    +  120 ) / 120
!   (  X^6 - 36 X^5 + 450 X^4 - 2400 X^3 + 5400 X^2 - 4320 X + 720 ) / 720
!   ( -X^7 + 49 X^6 - 882 X^5 + 7350 X^4 - 29400 X^3
!      + 52920 X^2 - 35280 X + 5040 ) / 5040
!
!  Recursion:
!
!    L(0,X) = 1,
!    L(1,X) = 1-X,
!    N * L(N,X) = (2*N-1-X) * L(N-1,X) - (N-1) * L(N-2,X)
!
!  Orthogonality:
!
!    Integral ( 0 <= X < oo ) exp ( - X ) * L(N,X) * L(M,X) dX
!    = 0 if N /= M
!    = 1 if N == M
!
!  Special values:
!
!    L(N,0) = 1.
!
!  Relations:
!
!    L(N,X) = (-1)^N / N! * exp ( x ) * (d/dx)^n ( exp ( - x ) * x^n )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials are
!    to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the Laguerre polynomials of
!    degree 0 through N evaluated at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = 1.0D+00 - X
 
    DO I = 2, N
 
        CX (I) = ((REAL(2*I-1, KIND=RK)-X)*CX(I-1)-REAL(I-1, KIND=RK)*CX(I-2)) / REAL (I, &
       & KIND=RK)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAGUERRE_POLY_COEF (N, C)
 
!*****************************************************************************80
!
!! laguerre_poly_coef() evaluates the Laguerre polynomial coefficients.
!
!  First terms:
!
!    0: 1
!    1: 1  -1
!    2: 1  -2  1/2
!    3: 1  -3  3/2  1/6
!    4: 1  -4  4   -2/3  1/24
!    5: 1  -5  5   -5/3  5/24  -1/120
!
!  Recursion:
!
!    L(0) = ( 1,  0, 0, ..., 0 )
!    L(1) = ( 1, -1, 0, ..., 0 )
!    L(N) = (2*N-1-X) * L(N-1) - (N-1) * L(N-2) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2003
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
!  Parameters:
!
!    Input, integer N, the highest order polynomial to compute.
!    Note that polynomials 0 through N will be computed.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the
!    Laguerre polynomials of degree 0 through N.  Each polynomial
!   is stored as a row.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N, 0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0:N, 0:N) = 0.0D+00
 
    C (0:N, 0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    C (1, 1) = - 1.0D+00
 
    DO I = 2, N
 
        C (I, 1:N) = (REAL(2*I-1, KIND=RK)*C(I-1, 1:N)+REAL(-I+1, KIND=RK)*C(I-2, 1:N)-C(I-1, &
       & 0:N-1)) / REAL (I, KIND=RK)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAGUERRE_POLYNOMIAL_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! laguerre_polynomial_values(): some values of the Laguerre polynomial L(n,x).
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      LaguerreL[n,x]
!
!  Differential equation:
!
!    X * Y'' + (1-X) * Y' + N * Y = 0
!
!  First terms:
!
!      1
!     -X    +  1
!   (  X^2 -  4 X     +  2 ) / 2
!   ( -X^3 +  9 X^2 -  18 X    +    6 ) / 6
!   (  X^4 - 16 X^3 +  72 X^2 -   96 X +      24 ) / 24
!   ( -X^5 + 25 X^4 - 200 X^3 +  600 X^2 -  600 x    +  120 ) / 120
!   (  X^6 - 36 X^5 + 450 X^4 - 2400 X^3 + 5400 X^2 - 4320 X + 720 ) / 720
!   ( -X^7 + 49 X^6 - 882 X^5 + 7350 X^4 - 29400 X^3 + 52920 X^2 - 35280 X
!     + 5040 ) / 5040
!
!  Recursion:
!
!    L(0,X) = 1,
!    L(1,X) = 1-X,
!    N * L(N,X) = (2*N-1-X) * L(N-1,X) - (N-1) * L(N-2,X)
!
!  Orthogonality:
!
!    Integral ( 0 <= X < oo ) exp ( - X ) * L(N,X) * L(M,X) dX
!    = 0 if N /= M
!    = 1 if N == M
!
!  Special values:
!
!    L(N,0) = 1.
!
!  Relations:
!
!    L(N,X) = (-1)^N / N! * exp ( x ) * (d/dx)^n ( exp ( - x ) * x^n )
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) X, the point where the polynomial is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 17
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1000000000000000D+01, &
   & 0.0000000000000000D+00, - 0.5000000000000000D+00, - 0.6666666666666667D+00, - &
   & 0.6250000000000000D+00, - 0.4666666666666667D+00, - 0.2569444444444444D+00, - &
   & 0.4047619047619048D-01, 0.1539930555555556D+00, 0.3097442680776014D+00, &
   & 0.4189459325396825D+00, 0.4801341790925124D+00, 0.4962122235082305D+00, - &
   & 0.4455729166666667D+00, 0.8500000000000000D+00, - 0.3166666666666667D+01, &
   & 0.3433333333333333D+02 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 5, &
   & 5, 5, 5 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 0.5D+00, &
   & 3.0D+00, 5.0D+00, 1.0D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LAMBERT_W (X)
 
!*****************************************************************************80
!
!! lambert_w() computes the Lambert W function.
!
!  Discussion:
!
!    The function W(X) is defined implicitly by:
!
!      W(X) * e^W(X) = X
!
!    The function is also known as the "Omega" function.
!
!    In Mathematica, the function can be evaluated by:
!
!      W = ProductLog [ X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Corless, Gaston Gonnet, David Hare, David Jeffrey, Donald Knuth,
!    On the Lambert W Function,
!    Advances in Computational Mathematics,
!    Volume 5, 1996, pages 329-359.
!
!    Brian Hayes,
!    "Why W?",
!    The American Scientist,
!    Volume 93, March-April 2005, pages 104-108.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    real ( kind = rk ) X, the argument of the function.
!
!  Output:
!
!    real ( kind = rk ) LAMBERT_W, the value of the Lambert W function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) LAMBERT_W
    INTEGER IT
    INTEGER, PARAMETER :: IT_MAX = 100
    REAL (KIND=RK), PARAMETER :: TOL = 1.0D-10
    REAL (KIND=RK) W
    REAL (KIND=RK) X
 
    W = LAMBERT_W_ESTIMATE (X)
    IT = 0
 
    DO
 
        IF (IT_MAX < IT) THEN
            EXIT
        END IF
 
        IF (ABS((X-W*EXP(W))) < TOL*ABS((W+1.0D+00)*EXP(W))) THEN
            EXIT
        END IF
 
        W = W - (W*EXP(W)-X) / &
       & ((W+1.0D+00)*EXP(W)-(W+2.0D+00)*(W*EXP(W)-X)/(2.0D+00*W+2.0D+00))
 
        IT = IT + 1
 
    END DO
 
    LAMBERT_W = W
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LAMBERT_W_ESTIMATE (X)
 
!*****************************************************************************80
!
!! lambert_w_estimate() is estimates the Lambert W function.
!
!  Discussion:
!
!    This crude approximation can be used as a good starting point
!    for an iterative process.
!
!    The function W(X) is defined implicitly by:
!
!      W(X) * e^W(X) = X
!
!    The function is also known as the "Omega" function.
!
!    In Mathematica, the function can be evaluated by:
!
!      W = ProductLog [ X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Corless, Gaston Gonnet, David Hare, David Jeffrey, Donald Knuth,
!    On the Lambert W Function,
!    Advances in Computational Mathematics,
!    Volume 5, 1996, pages 329-359.
!
!    Brian Hayes,
!    "Why W?",
!    The American Scientist,
!    Volume 93, March-April 2005, pages 104-108.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    real ( kind = rk ) X, the argument of the function.
!
!  Output:
!
!    real ( kind = rk ) LAMBERT_W_ESTIMATE, a crude approximation
!    to the Lambert W function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) LAMBERT_W_ESTIMATE
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X
 
    IF (X <= 500.0D+00) THEN
 
        VALUE = 0.04D+00 + 0.665D+00 * (1.0D+00+0.0195D+00*LOG(X+1.0D+00)) * LOG (X+1.0D+00)
 
    ELSE
 
        VALUE = LOG (X-4.0D+00) - (1.0D+00-1.0D+00/LOG(X)) * LOG (LOG(X))
 
    END IF
 
    LAMBERT_W_ESTIMATE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LAMBERT_W_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! lambert_w_values() returns some values of the Lambert W function.
!
!  Discussion:
!
!    The function W(X) is defined implicitly by:
!
!      W(X) * e^W(X) = X
!
!    The function is also known as the "Omega" function.
!
!    In Mathematica, the function can be evaluated by:
!
!      W = ProductLog [ X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Corless, Gaston Gonnet, David Hare, David Jeffrey, Donald Knuth,
!    On the Lambert W Function,
!    Advances in Computational Mathematics,
!    Volume 5, 1996, pages 329-359.
!
!    Brian Hayes,
!    "Why W?",
!    The American Scientist,
!    Volume 93, March-April 2005, pages 104-108.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 22
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.0000000000000000D+00, &
   & 0.3517337112491958D+00, 0.5671432904097839D+00, 0.7258613577662263D+00, &
   & 0.8526055020137255D+00, 0.9585863567287029D+00, 0.1000000000000000D+01, &
   & 0.1049908894964040D+01, 0.1130289326974136D+01, 0.1202167873197043D+01, &
   & 0.1267237814307435D+01, 0.1326724665242200D+01, 0.1381545379445041D+01, &
   & 0.1432404775898300D+01, 0.1479856830173851D+01, 0.1524345204984144D+01, &
   & 0.1566230953782388D+01, 0.1605811996320178D+01, 0.1745528002740699D+01, &
   & 0.3385630140290050D+01, 0.5249602852401596D+01, 0.1138335808614005D+02 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.0000000000000000D+00, &
   & 0.5000000000000000D+00, 0.1000000000000000D+01, 0.1500000000000000D+01, &
   & 0.2000000000000000D+01, 0.2500000000000000D+01, 0.2718281828459045D+01, &
   & 0.3000000000000000D+01, 0.3500000000000000D+01, 0.4000000000000000D+01, &
   & 0.4500000000000000D+01, 0.5000000000000000D+01, 0.5500000000000000D+01, &
   & 0.6000000000000000D+01, 0.6500000000000000D+01, 0.7000000000000000D+01, &
   & 0.7500000000000000D+01, 0.8000000000000000D+01, 0.1000000000000000D+02, &
   & 0.1000000000000000D+03, 0.1000000000000000D+04, 0.1000000000000000D+07 /)
 
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
 
SUBROUTINE LEGENDRE_ASSOCIATED (N, M, X, CX)
 
!*****************************************************************************80
!
!! legendre_associated() evaluates the associated Legendre functions.
!
!  Differential equation:
!
!    (1-X*X) * Y'' - 2 * X * Y + ( N (N+1) - (M*M/(1-X*X)) * Y = 0
!
!  First terms:
!
!    M = 0  ( = Legendre polynomials of first kind P(N,X) )
!
!    P00 =    1
!    P10 =    1 X
!    P20 = (  3 X^2 -     1)/2
!    P30 = (  5 X^3 -   3 X)/2
!    P40 = ( 35 X^4 -  30 X^2 +     3)/8
!    P50 = ( 63 X^5 -  70 X^3 +  15 X)/8
!    P60 = (231 X^6 - 315 X^4 + 105 X^2 -    5)/16
!    P70 = (429 X^7 - 693 X^5 + 315 X^3 - 35 X)/16
!
!    M = 1
!
!    P01 =   0
!    P11 =   1 * SQRT(1-X*X)
!    P21 =   3 * SQRT(1-X*X) * X
!    P31 = 1.5 * SQRT(1-X*X) * (5*X*X-1)
!    P41 = 2.5 * SQRT(1-X*X) * (7*X*X*X-3*X)
!
!    M = 2
!
!    P02 =   0
!    P12 =   0
!    P22 =   3 * (1-X*X)
!    P32 =  15 * (1-X*X) * X
!    P42 = 7.5 * (1-X*X) * (7*X*X-1)
!
!    M = 3
!
!    P03 =   0
!    P13 =   0
!    P23 =   0
!    P33 =  15 * (1-X*X)^1.5
!    P43 = 105 * (1-X*X)^1.5 * X
!
!    M = 4
!
!    P04 =   0
!    P14 =   0
!    P24 =   0
!    P34 =   0
!    P44 = 105 * (1-X*X)^2
!
!  Recursion:
!
!    if N < M:
!      P(N,M) = 0
!    if N = M:
!      P(N,M) = (2*M-1)!! * (1-X*X)^(M/2) where N!! means the product of
!      all the odd integers less than or equal to N.
!    if N = M+1:
!      P(N,M) = X*(2*M+1)*P(M,M)
!    if M+1 < N:
!      P(N,M) = ( X*(2*N-1)*P(N-1,M) - (N+M-1)*P(N-2,M) )/(N-M)
!
!  Special values:
!
!    P(N,0,X) = P(N,X), that is, for M=0, the associated Legendre
!    function of the first kind equals the Legendre polynomial of the
!    first kind.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 May 2004
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
!  Parameters:
!
!    Input, integer N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real ( kind = rk ) X, the point at which the function is to be
!    evaluated.  X must satisfy -1 <= X <= 1.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 functions.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    REAL (KIND=RK) FACT
    INTEGER I
    INTEGER M
    REAL (KIND=RK) SOMX2
    REAL (KIND=RK) X
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED - Fatal error!'
        WRITE (*, '(a,i8)') '  Input value of M is ', M
        WRITE (*, '(a)') '  but M must be nonnegative.'
        STOP 1
    END IF
 
    IF (N < M) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED - Fatal error!'
        WRITE (*, '(a,i8)') '  Input value of M = ', M
        WRITE (*, '(a,i8)') '  Input value of N = ', N
        WRITE (*, '(a)') '  but M must be less than or equal to N.'
        STOP 1
    END IF
 
    IF (X <-1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Input value of X = ', X
        WRITE (*, '(a)') '  but X must be no less than -1.'
        STOP 1
    END IF
 
    IF (1.0D+00 < X) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Input value of X = ', X
        WRITE (*, '(a)') '  but X must be no more than 1.'
        STOP 1
    END IF
 
    CX (0:M-1) = 0.0D+00
 
    CX (M) = 1.0D+00
    SOMX2 = SQRT (1.0D+00-X*X)
 
    FACT = 1.0D+00
    DO I = 1, M
        CX (M) = - CX (M) * FACT * SOMX2
        FACT = FACT + 2.0D+00
    END DO
 
    IF (M+1 <= N) THEN
        CX (M+1) = X * REAL (2*M+1, KIND=RK) * CX (M)
    END IF
 
    DO I = M + 2, N
        CX (I) = (REAL(2*I-1, KIND=RK)*X*CX(I-1)+REAL(-I-M+1, KIND=RK)*CX(I-2)) / REAL (I-M, &
       & KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_ASSOCIATED_NORMALIZED (N, M, X, CX)
 
!*****************************************************************************80
!
!! LEGENDRE_ASSOCIATED_NORMALIZED(): normalized associated Legendre functions.
!
!  Discussion:
!
!    The unnormalized associated Legendre functions P_N^M(X) have
!    the property that
!
!      Integral ( -1 <= X <= 1 ) ( P_N^M(X) )^2 dX
!      = 2 * ( N + M )! / ( ( 2 * N + 1 ) * ( N - M )! )
!
!    By dividing the function by the square root of this term,
!    the normalized associated Legendre functions have norm 1.
!
!    However, we plan to use these functions to build spherical
!    harmonics, so we use a slightly different normalization factor of
!
!      sqrt ( ( ( 2 * N + 1 ) * ( N - M )! ) / ( 4 * pi * ( N + M )! ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2005
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
!  Parameters:
!
!    Input, integer N, the maximum first index of the Legendre
!    function, which must be at least 0.
!
!    Input, integer M, the second index of the Legendre function,
!    which must be at least 0, and no greater than N.
!
!    Input, real ( kind = rk ) X, the point at which the function is to be
!    evaluated.  X must satisfy -1 <= X <= 1.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 functions.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    REAL (KIND=RK) FACTOR
    INTEGER I
    INTEGER M
    INTEGER MM
    REAL (KIND=RK) :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) SOMX2
    REAL (KIND=RK) X
 
    IF (M < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED_NORMALIZED - Fatal error!'
        WRITE (*, '(a,i8)') '  Input value of M is ', M
        WRITE (*, '(a)') '  but M must be nonnegative.'
        STOP 1
    END IF
 
    IF (N < M) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED_NORMALIZED - Fatal error!'
        WRITE (*, '(a,i8)') '  Input value of M = ', M
        WRITE (*, '(a,i8)') '  Input value of N = ', N
        WRITE (*, '(a)') '  but M must be less than or equal to N.'
        STOP 1
    END IF
 
    IF (X <-1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED_NORMALIZED - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Input value of X = ', X
        WRITE (*, '(a)') '  but X must be no less than -1.'
        STOP 1
    END IF
 
    IF (1.0D+00 < X) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_ASSOCIATED_NORMALIZED - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Input value of X = ', X
        WRITE (*, '(a)') '  but X must be no more than 1.'
        STOP 1
    END IF
!
!  Entries 0 through M-1 are zero!
!
    CX (0:M-1) = 0.0D+00
 
    CX (M) = 1.0D+00
    SOMX2 = SQRT (1.0D+00-X*X)
 
    FACTOR = 1.0D+00
    DO I = 1, M
        CX (M) = - CX (M) * FACTOR * SOMX2
        FACTOR = FACTOR + 2.0D+00
    END DO
 
    IF (M+1 <= N) THEN
        CX (M+1) = X * REAL (2*M+1, KIND=RK) * CX (M)
    END IF
 
    DO I = M + 2, N
        CX (I) = (REAL(2*I-1, KIND=RK)*X*CX(I-1)+REAL(-I-M+1, KIND=RK)*CX(I-2)) / REAL (I-M, &
       & KIND=RK)
    END DO
!
!  Normalization.
!
    DO MM = M, N
        FACTOR = SQRT ((REAL(2*MM+1, &
       & KIND=RK)*R8_FACTORIAL(MM-M))/(4.0D+00*R8_PI*R8_FACTORIAL(MM+M)))
        CX (MM) = CX (MM) * FACTOR
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_ASSOCIATED_VALUES (N_DATA, N, M, X, FX)
 
!*****************************************************************************80
!
!! LEGENDRE_ASSOCIATED_VALUES() returns values of associated Legendre functions.
!
!  Discussion:
!
!    The function considered is the associated Legendre function P^M_N(X).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 June 2001
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, integer M, real ( kind = rk ) X,
!    the arguments of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 19
 
    INTEGER N
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 5, 6, &
   & 7, 8, 9, 10 /)
    INTEGER M
    INTEGER, SAVE, DIMENSION (NMAX) :: M_VEC = (/ 0, 0, 0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 2, 2, 3, &
   & 3, 4, 4, 5 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FX_VEC = (/ 0.000000D+00, 0.500000D+00, &
   & 0.707107D+00, 1.000000D+00, - 0.866025D+00, - 0.125000D+00, - 1.29904D+00, 2.25000D+00, - &
   & 0.437500D+00, - 0.324759D+00, 5.62500D+00, - 9.74278D+00, 4.21875D+00, - 4.92187D+00, &
   & 12.7874D+00, 116.685D+00, - 1050.67D+00, - 2078.49D+00, 30086.2D+00 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 0.0D+00, 0.5D+00, 0.7071067D+00, &
   & 1.0D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        M = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        M = M_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES (N_DATA, N, M, X, FX)
 
!*****************************************************************************80
!
!! LEGENDRE_ASSOCIATED_NORMALIZED_SPHERE_VALUES(): normalized associated Legendre.
!
!  Discussion:
!
!    The function considered is the associated Legendre polynomial P^M_N(X).
!
!    In Mathematica, the function can be evaluated by:
!
!      LegendreP [ n, m, x ]
!
!    The function is normalized by dividing by
!
!      sqrt ( 4 * pi * ( n + m )! / ( 4 * pi * n + 1 ) / ( n - m )! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2010
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, integer M,
!    real ( kind = rk ) X, the arguments of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 21
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.2820947917738781D+00, &
   & 0.2443012559514600D+00, - 0.2992067103010745D+00, - 0.07884789131313000D+00, - &
   & 0.3345232717786446D+00, 0.2897056515173922D+00, - 0.3265292910163510D+00, - &
   & 0.06997056236064664D+00, 0.3832445536624809D+00, - 0.2709948227475519D+00, - &
   & 0.2446290772414100D+00, 0.2560660384200185D+00, 0.1881693403754876D+00, - &
   & 0.4064922341213279D+00, 0.2489246395003027D+00, 0.08405804426339821D+00, &
   & 0.3293793022891428D+00, - 0.1588847984307093D+00, - 0.2808712959945307D+00, &
   & 0.4127948151484925D+00, - 0.2260970318780046D+00 /)
    INTEGER M
    INTEGER, SAVE, DIMENSION (N_MAX) :: M_VEC = (/ 0, 0, 1, 0, 1, 2, 0, 1, 2, 3, 0, 1, 2, 3, 4, &
   & 0, 1, 2, 3, 4, 5 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, &
   & 5, 5, 5, 5, 5, 5 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.50D+00, 0.50D+00, 0.50D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00 &
   & /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        M = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        M = M_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_FUNCTION_Q (N, X, CX)
 
!*****************************************************************************80
!
!! LEGENDRE_FUNCTION_Q() evaluates the Legendre Q functions.
!
!  Differential equation:
!
!    (1-X*X) Y'' - 2 X Y' + N (N+1) = 0
!
!  First terms:
!
!    Q(0,X) = 0.5 * log((1+X)/(1-X))
!    Q(1,X) = Q(0,X)*    X - 1
!    Q(2,X) = Q(0,X)*( 3*X^2-1)/4 - 1.5*X
!    Q(3,X) = Q(0,X)*( 5*X^3-3*X)/4 - 2.5*X^2 + 2/3
!    Q(4,X) = Q(0,X)*(35*X^4-30*X^2+3)/16 - 35/8 * X^3 + 55/24 * X
!    Q(5,X) = Q(0,X)*(63*X^5-70*X^3+15*X)/16 - 63/8*X^4 + 49/8*X^2 - 8/15
!
!  Recursion:
!
!    Q(0) = 0.5 * log ( (1+X) / (1-X) )
!    Q(1) = 0.5 * X * log ( (1+X) / (1-X) ) - 1.0
!
!    Q(N) = ( (2*N-1) * X * Q(N-1) - (N-1) * Q(N-2) ) / N
!
!  Restrictions:
!
!    -1 < X < 1
!
!  Special values:
!
!    Note that the Legendre function Q(N,X) is equal to the
!    associated Legendre function of the second kind,
!    Q(N,M,X) with M = 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2003
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
!  Parameters:
!
!    Input, integer N, the highest order function to evaluate.
!
!    Input, real ( kind = rk ) X, the point at which the functions are to be
!    evaluated.  X must satisfy -1 < X < 1.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the first N+1 Legendre
!    functions at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    INTEGER I
    REAL (KIND=RK) X
!
!  Check the value of X.
!
    IF (X <=-1.0D+00 .OR. 1.0D+00 <= X) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_FUNCTION_Q - Fatal error!'
        WRITE (*, '(a,g14.6)') '  Illegal input value of X = ', X
        WRITE (*, '(a)') '  But X must be between -1 and 1.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 0.5D+00 * LOG ((1.0D+00+X)/(1.0D+00-X))
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    CX (1) = X * CX (0) - 1.0D+00
 
    DO I = 2, N
        CX (I) = (REAL(2*I-1, KIND=RK)*X*CX(I-1)+REAL(-I+1, KIND=RK)*CX(I-2)) / REAL (I, &
       & KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_FUNCTION_Q_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! LEGENDRE_FUNCTION_Q_VALUES() returns values of the Legendre Q function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 12
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FX_VEC = (/ 0.00000000D+00, - 1.00000000D+00, &
   & 0.00000000D+00, 0.66666667D+00, - 0.40634921D+00, 0.00000000D+00, 0.54930614D+00, - &
   & 0.72534693D+00, - 0.81866327D+00, - 0.19865477D+00, - 0.11616303D+00, 0.29165814D+00 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 9, 10, 0, 1, 2, 3, 9, 10 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: X_VEC = (/ 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, &
   & 0.0D+00, 0.0D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_POLY (N, X, CX, CPX)
 
!*****************************************************************************80
!
!! LEGENDRE_POLY() evaluates the Legendre polynomials P(N,X) at X.
!
!  Discussion:
!
!    P(N,1) = 1.
!    P(N,-1) = (-1)^N.
!    | P(N,X) | <= 1 in [-1,1].
!
!    P(N,0,X) = P(N,X), that is, for M=0, the associated Legendre
!    function of the first kind and order N equals the Legendre polynomial
!    of the first kind and order N.
!
!    The N zeroes of P(N,X) are the abscissas used for Gauss-Legendre
!    quadrature of the integral of a function F(X) with weight function 1
!    over the interval [-1,1].
!
!    The Legendre polynomials are orthogonal under the inner product defined
!    as integration from -1 to 1:
!
!      Integral ( -1 <= X <= 1 ) P(I,X) * P(J,X) dX
!        = 0 if I =/= J
!        = 2 / ( 2*I+1 ) if I = J.
!
!    Except for P(0,X), the integral of P(I,X) from -1 to 1 is 0.
!
!    A function F(X) defined on [-1,1] may be approximated by the series
!      C0*P(0,X) + C1*P(1,X) + ... + CN*P(N,X)
!    where
!      C(I) = (2*I+1)/(2) * Integral ( -1 <= X <= 1 ) F(X) P(I,X) dx.
!
!    The formula is:
!
!      P(N,X) = (1/2^N) * sum ( 0 <= M <= N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
!
!  Differential equation:
!
!    (1-X*X) * P(N,X)'' - 2 * X * P(N,X)' + N * (N+1) = 0
!
!  First terms:
!
!    P( 0,X) =       1
!    P( 1,X) =       1 X
!    P( 2,X) =  (    3 X^2 -       1)/2
!    P( 3,X) =  (    5 X^3 -     3 X)/2
!    P( 4,X) =  (   35 X^4 -    30 X^2 +     3)/8
!    P( 5,X) =  (   63 X^5 -    70 X^3 +    15 X)/8
!    P( 6,X) =  (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
!    P( 7,X) =  (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
!    P( 8,X) =  ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
!    P( 9,X) =  (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
!    P(10,X) =  (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2
!                 -63 ) /256
!
!  Recursion:
!
!    P(0,X) = 1
!    P(1,X) = X
!    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
!
!    P'(0,X) = 0
!    P'(1,X) = 1
!    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 2004
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
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Input, real ( kind = rk ) X, the point at which the polynomials
!    are to be evaluated.
!
!    Output, real ( kind = rk ) CX(0:N), the values of the Legendre polynomials
!    of order 0 through N at the point X.
!
!    Output, real ( kind = rk ) CPX(0:N), the values of the derivatives of the
!    Legendre polynomials of order 0 through N at the point X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) CX (0:N)
    REAL (KIND=RK) CPX (0:N)
    INTEGER I
    REAL (KIND=RK) X
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    CX (0) = 1.0D+00
    CPX (0) = 0.0D+00
 
    IF (N < 1) THEN
        RETURN
    END IF
 
    CX (1) = X
    CPX (1) = 1.0D+00
 
    DO I = 2, N
 
        CX (I) = (REAL(2*I-1, KIND=RK)*X*CX(I-1)-REAL(I-1, KIND=RK)*CX(I-2)) / REAL (I, &
       & KIND=RK)
 
        CPX (I) = (REAL(2*I-1, KIND=RK)*(CX(I-1)+X*CPX(I-1))-REAL(I-1, KIND=RK)*CPX(I-2)) / &
       & REAL (I, KIND=RK)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_POLY_COEF (N, C)
 
!*****************************************************************************80
!
!! LEGENDRE_POLY_COEF() evaluates the Legendre polynomial coefficients.
!
!  First terms:
!
!     1
!     0     1
!    -1/2   0      3/2
!     0    -3/2    0     5/2
!     3/8   0    -30/8   0     35/8
!     0    15/8    0   -70/8    0     63/8
!    -5/16  0    105/16  0   -315/16   0    231/16
!     0   -35/16   0   315/16   0   -693/16   0    429/16
!
!     1.00000
!     0.00000  1.00000
!    -0.50000  0.00000  1.50000
!     0.00000 -1.50000  0.00000  2.5000
!     0.37500  0.00000 -3.75000  0.00000  4.37500
!     0.00000  1.87500  0.00000 -8.75000  0.00000  7.87500
!    -0.31250  0.00000  6.56250  0.00000 -19.6875  0.00000  14.4375
!     0.00000 -2.1875   0.00000  19.6875  0.00000 -43.3215  0.00000  26.8125
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2003
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
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, integer N, the highest order polynomial to evaluate.
!    Note that polynomials 0 through N will be evaluated.
!
!    Output, real ( kind = rk ) C(0:N,0:N), the coefficients of the
!    Legendre polynomials of degree 0 through N.  Each polynomial is
!    stored as a row.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N, 0:N)
    INTEGER I
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    C (0:N, 0:N) = 0.0D+00
 
    C (0, 0) = 1.0D+00
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    C (1, 1) = 1.0D+00
 
    DO I = 2, N
        C (I, 0:I-2) = REAL (-I+1, KIND=RK) * C (I-2, 0:I-2) / REAL (I, KIND=RK)
        C (I, 1:I) = C (I, 1:I) + REAL (I+I-1, KIND=RK) * C (I-1, 0:I-1) / REAL (I, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_POLY_VALUES (N_DATA, N, X, FX)
 
!*****************************************************************************80
!
!! LEGENDRE_POLY_VALUES() returns values of the Legendre polynomials.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      LegendreP [ n, x ]
!
!  Differential equation:
!
!    (1-X*X) * P(N,X)'' - 2 * X * P(N,X)' + N * (N+1) = 0
!
!  First terms:
!
!    P( 0,X) =       1
!    P( 1,X) =       1 X
!    P( 2,X) =  (    3 X^2 -       1)/2
!    P( 3,X) =  (    5 X^3 -     3 X)/2
!    P( 4,X) =  (   35 X^4 -    30 X^2 +     3)/8
!    P( 5,X) =  (   63 X^5 -    70 X^3 +    15 X)/8
!    P( 6,X) =  (  231 X^6 -   315 X^4 +   105 X^2 -     5)/16
!    P( 7,X) =  (  429 X^7 -   693 X^5 +   315 X^3 -    35 X)/16
!    P( 8,X) =  ( 6435 X^8 - 12012 X^6 +  6930 X^4 -  1260 X^2 +   35)/128
!    P( 9,X) =  (12155 X^9 - 25740 X^7 + 18018 X^5 -  4620 X^3 +  315 X)/128
!    P(10,X) =  (46189 X^10-109395 X^8 + 90090 X^6 - 30030 X^4 + 3465 X^2
!                 -63 ) /256
!
!  Recursion:
!
!    P(0,X) = 1
!    P(1,X) = X
!    P(N,X) = ( (2*N-1)*X*P(N-1,X)-(N-1)*P(N-2,X) ) / N
!
!    P'(0,X) = 0
!    P'(1,X) = 1
!    P'(N,X) = ( (2*N-1)*(P(N-1,X)+X*P'(N-1,X)-(N-1)*P'(N-2,X) ) / N
!
!  Formula:
!
!    P(N)(X) = (1/2^N) * sum ( 0 <= M <= N/2 ) C(N,M) C(2N-2M,N) X^(N-2*M)
!
!  Orthogonality:
!
!    Integral ( -1 <= X <= 1 ) P(I,X) * P(J,X) dX
!      = 0 if I =/= J
!      = 2 / ( 2*I+1 ) if I = J.
!
!  Approximation:
!
!    A function F(X) defined on [-1,1] may be approximated by the series
!
!      C0*P(0,X) + C1*P(1,X) + ... + CN*P(N,X)
!
!    where
!
!      C(I) = (2*I+1)/(2) * Integral ( -1 <= X <= 1 ) F(X) P(I,X) dx.
!
!  Special values:
!
!    P(N,1) = 1.
!    P(N,-1) = (-1)^N.
!    | P(N,X) | <= 1 in [-1,1].
!
!    Pm(N,0,X) = P(N,X), that is, for M=0, the associated Legendre
!    function of the first kind and order N equals the Legendre polynomial
!    of the first kind and order N.
!
!    The N zeroes of P(N,X) are the abscissas used for Gauss-Legendre
!    quadrature of the integral of a function F(X) with weight function 1
!    over the interval [-1,1].
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the order of the function.
!
!    Output, real ( kind = rk ) X, the point where the function is evaluated.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 22
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1000000000000000D+01, &
   & 0.2500000000000000D+00, - 0.4062500000000000D+00, - 0.3359375000000000D+00, &
   & 0.1577148437500000D+00, 0.3397216796875000D+00, 0.2427673339843750D-01, - &
   & 0.2799186706542969D+00, - 0.1524540185928345D+00, 0.1768244206905365D+00, &
   & 0.2212002165615559D+00, 0.0000000000000000D+00, - 0.1475000000000000D+00, - &
   & 0.2800000000000000D+00, - 0.3825000000000000D+00, - 0.4400000000000000D+00, - &
   & 0.4375000000000000D+00, - 0.3600000000000000D+00, - 0.1925000000000000D+00, &
   & 0.8000000000000000D-01, 0.4725000000000000D+00, 0.1000000000000000D+01 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 3, 3, 3, &
   & 3, 3, 3, 3, 3, 3, 3, 3 /)
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.25D+00, 0.25D+00, 0.25D+00, &
   & 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.00D+00, &
   & 0.10D+00, 0.20D+00, 0.30D+00, 0.40D+00, 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.90D+00, &
   & 1.00D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LEGENDRE_SYMBOL (Q, P, L)
 
!*****************************************************************************80
!
!! LEGENDRE_SYMBOL() evaluates the Legendre symbol (Q/P).
!
!  Discussion:
!
!    Let P be an odd prime.  Q is a QUADRATIC RESIDUE modulo P
!    if there is an integer R such that R^2 = Q ( mod P ).
!    The Legendre symbol ( Q / P ) is defined to be:
!
!      + 1 if Q ( mod P ) /= 0 and Q is a quadratic residue modulo P,
!      - 1 if Q ( mod P ) /= 0 and Q is not a quadratic residue modulo P,
!        0 if Q ( mod P ) == 0.
!
!    We can also define ( Q / P ) for P = 2 by:
!
!      + 1 if Q ( mod P ) /= 0
!        0 if Q ( mod P ) == 0
!
!    For any prime P, exactly half of the integers from 1 to P-1
!    are quadratic residues.
!
!    ( 0 / P ) = 0.
!
!    ( Q / P ) = ( mod ( Q, P ) / P ).
!
!    ( Q / P ) = ( Q1 / P ) * ( Q2 / P ) if Q = Q1 * Q2.
!
!    If Q is prime, and P is prime and greater than 2, then:
!
!      if ( Q == 1 ) then
!
!        ( Q / P ) = 1
!
!      else if ( Q == 2 ) then
!
!        ( Q / P ) = + 1 if mod ( P, 8 ) = 1 or mod ( P, 8 ) = 7,
!        ( Q / P ) = - 1 if mod ( P, 8 ) = 3 or mod ( P, 8 ) = 5.
!
!      else
!
!        ( Q / P ) = - ( P / Q ) if Q = 3 ( mod 4 ) and P = 3 ( mod 4 ),
!                  =   ( P / Q ) otherwise.
!
!  Example:
!
!    (0/7) =   0
!    (1/7) = + 1  ( 1^2 = 1 mod 7 )
!    (2/7) = + 1  ( 3^2 = 2 mod 7 )
!    (3/7) = - 1
!    (4/7) = + 1  ( 2^2 = 4 mod 7 )
!    (5/7) = - 1
!    (6/7) = - 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Charles Pinter,
!    A Book of Abstract Algebra,
!    McGraw Hill, 1982, pages 236-237.
!
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 86-87.
!
!  Parameters:
!
!    Input, integer Q, an integer whose Legendre symbol with
!    respect to P is desired.
!
!    Input, integer P, a prime number, greater than 1, with respect
!    to which the Legendre symbol of Q is desired.
!
!    Output, integer L, the Legendre symbol (Q/P).
!    Ordinarily, L will be -1, 0 or 1.
!    L = -2, P is less than or equal to 1.
!    L = -3, P is not prime.
!    L = -4, the internal stack of factors overflowed.
!    L = -5, not enough factorization space.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
    INTEGER, PARAMETER :: MAXSTACK = 50
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER L
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER NMORE
    INTEGER NSTACK
    INTEGER P
    INTEGER POWER (MAXFACTOR)
    INTEGER PP
    INTEGER PSTACK (MAXSTACK)
    INTEGER Q
    INTEGER QQ
    INTEGER QSTACK (MAXSTACK)
!
!  P must be greater than 1.
!
    IF (P <= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_SYMBOL - Fatal error!'
        WRITE (*, '(a)') '  P must be greater than 1.'
        L = - 2
        STOP 1
    END IF
!
!  P must be prime.
!
    IF ( .NOT. I4_IS_PRIME(P)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'LEGENDRE_SYMBOL - Fatal error!'
        WRITE (*, '(a)') '  P is not prime.'
        L = - 3
        STOP 1
    END IF
!
!  ( k*P / P ) = 0.
!
    IF (MOD(Q, P) == 0) THEN
        L = 0
        RETURN
    END IF
!
!  For the special case P = 2, (Q/P) = 1 for all odd numbers.
!
    IF (P == 2) THEN
        L = 1
        RETURN
    END IF
!
!  Make a copy of Q, and force it to be nonnegative.
!
    QQ = Q
 
    DO WHILE (QQ <  0)
        QQ = QQ + P
    END DO
 
    NSTACK = 0
    PP = P
    L = 1
 
    DO
 
        QQ = MOD (QQ, PP)
!
!  Decompose QQ into factors of prime powers.
!
        CALL I4_FACTOR (QQ, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
        IF (NLEFT /= 1) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'LEGENDRE_SYMBOL - Fatal error!'
            WRITE (*, '(a)') '  Not enough factorization space.'
            L = - 5
            STOP 1
        END IF
!
!  Each factor which is an odd power is added to the stack.
!
        NMORE = 0
 
        DO I = 1, NFACTOR
 
            IF (MOD(POWER(I), 2) == 1) THEN
 
                NMORE = NMORE + 1
                NSTACK = NSTACK + 1
 
                IF (MAXSTACK < NSTACK) THEN
                    WRITE (*, '(a)') ' '
                    WRITE (*, '(a)') 'LEGENDRE_SYMBOL - Fatal error!'
                    WRITE (*, '(a)') '  Stack overflow!'
                    L = - 4
                    STOP 1
                END IF
 
                PSTACK (NSTACK) = PP
                QSTACK (NSTACK) = FACTOR (I)
 
            END IF
 
        END DO
 
        IF (NMORE /= 0) THEN
 
            QQ = QSTACK (NSTACK)
            NSTACK = NSTACK - 1
!
!  Check for a QQ of 1 or 2.
!
            IF (QQ == 1) THEN
 
                L = + 1 * L
 
            ELSE IF (QQ == 2 .AND. (MOD(PP, 8) == 1 .OR. MOD(PP, 8) == 7)) THEN
 
                L = + 1 * L
 
            ELSE IF (QQ == 2 .AND. (MOD(PP, 8) == 3 .OR. MOD(PP, 8) == 5)) THEN
 
                L = - 1 * L
 
            ELSE
 
                IF (MOD(PP, 4) == 3 .AND. MOD(QQ, 3) == 3) THEN
                    L = - 1 * L
                END IF
 
                CALL I4_SWAP (PP, QQ)
 
                CYCLE
 
            END IF
 
        END IF
!
!  If the stack is empty, we're done.
!
        IF (NSTACK == 0) THEN
            EXIT
        END IF
!
!  Otherwise, get the last P and Q from the stack, and process them.
!
        PP = PSTACK (NSTACK)
        QQ = QSTACK (NSTACK)
        NSTACK = NSTACK - 1
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION LERCH (Z, S, A)
 
!*****************************************************************************80
!
!! LERCH() estimates the Lerch transcendent function.
!
!  Discussion:
!
!    The Lerch transcendent function is defined as:
!
!      LERCH ( Z, S, A ) = Sum ( 0 <= K < +oo ) Z^K / ( A + K )^S
!
!    excluding any term with ( A + K ) = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      LerchPhi[z,s,a]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Thanks:
!
!    Oscar van Vlijmen
!
!  Parameters:
!
!    Input, real ( kind = rk ) Z, integer S, real ( kind = rk ) A,
!    the parameters of the function.
!
!    Output, real ( kind = rk ) LERCH, an approximation to the Lerch
!    transcendent function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) EPS
    INTEGER K
    REAL (KIND=RK) LERCH
    INTEGER S
    REAL (KIND=RK) TERM
    REAL (KIND=RK) TOTAL
    REAL (KIND=RK) Z
    REAL (KIND=RK) Z_K
 
    IF (Z <= 0.0D+00) THEN
        LERCH = 0.0D+00
        RETURN
    END IF
 
    EPS = 1.0D-10
    TOTAL = 0.0D+00
    K = 0
    Z_K = 1.0D+00
 
    DO
 
        IF (A+REAL(K, KIND=RK) /= 0.0D+00) THEN
 
            TERM = Z_K / (A+REAL(K, KIND=RK)) ** S
            TOTAL = TOTAL + TERM
 
            IF (ABS(TERM) <= EPS*(1.0D+00+ABS(TOTAL))) THEN
                EXIT
            END IF
 
        END IF
 
        K = K + 1
        Z_K = Z_K * Z
 
    END DO
 
    LERCH = TOTAL
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LERCH_VALUES (N_DATA, Z, S, A, FX)
 
!*****************************************************************************80
!
!! LERCH_VALUES() returns some values of the Lerch transcendent function.
!
!  Discussion:
!
!    The Lerch function is defined as
!
!      Phi(z,s,a) = Sum ( 0 <= k < +oo ) z^k / ( a + k )^s
!
!    omitting any terms with ( a + k ) = 0.
!
!    In Mathematica, the function can be evaluated by:
!
!      LerchPhi[z,s,a]
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
!  Reference:
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) Z, the parameters of the function.
!
!    Output, integer S, the parameters of the function.
!
!    Output, real ( kind = rk ) A, the parameters of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 12
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.0D+00, 0.0D+00, 0.0D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 3.0D+00, 3.0D+00, 3.0D+00 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.1644934066848226D+01, &
   & 0.1202056903159594D+01, 0.1000994575127818D+01, 0.1164481052930025D+01, &
   & 0.1074426387216080D+01, 0.1000492641212014D+01, 0.2959190697935714D+00, &
   & 0.1394507503935608D+00, 0.9823175058446061D-03, 0.1177910993911311D+00, &
   & 0.3868447922298962D-01, 0.1703149614186634D-04 /)
    INTEGER N_DATA
    INTEGER S
    INTEGER, SAVE, DIMENSION (N_MAX) :: S_VEC = (/ 2, 3, 10, 2, 3, 10, 2, 3, 10, 2, 3, 10 /)
    REAL (KIND=RK) Z
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: Z_VEC = (/ 0.1000000000000000D+01, &
   & 0.1000000000000000D+01, 0.1000000000000000D+01, 0.5000000000000000D+00, &
   & 0.5000000000000000D+00, 0.5000000000000000D+00, 0.3333333333333333D+00, &
   & 0.3333333333333333D+00, 0.3333333333333333D+00, 0.1000000000000000D+00, &
   & 0.1000000000000000D+00, 0.1000000000000000D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        Z = 0.0D+00
        S = 0
        A = 0.0D+00
        FX = 0.0D+00
    ELSE
        Z = Z_VEC (N_DATA)
        S = S_VEC (N_DATA)
        A = A_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE LOCK (N, A)
 
!*****************************************************************************80
!
!! LOCK() returns the number of codes for a lock with N buttons.
!
!  Discussion:
!
!    A button lock has N numbered buttons.  To open the lock, groups
!    of buttons must be pressed in the correct order.  Each button
!    may be pushed no more than once.  Thus, a code for the lock is
!    an ordered list of the groups of buttons to be pushed.
!
!    For this discussion, we will assume that EVERY button is pushed
!    at some time, as part of the code.  To count the total number
!    of codes, including those which don't use all the buttons, then
!    the number is 2 * A(N), or 2 * A(N) - 1 if we don't consider the
!    empty code to be valid.
!
!    If there are 3 buttons, then there are 13 possible "full button" codes:
!
!      (123)
!      (12) (3)
!      (13) (2)
!      (23) (1)
!      (1) (23)
!      (2) (13)
!      (3) (12)
!      (1) (2) (3)
!      (1) (3) (2)
!      (2) (1) (3)
!      (2) (3) (1)
!      (3) (1) (2)
!      (3) (2) (1)
!
!    and, if we don't need to push all the buttons, every "full button" code
!    above yields a distinct "partial button" code by dropping the last set
!    of buttons:
!
!      ()
!      (12)
!      (13)
!      (23)
!      (1)
!      (2)
!      (3)
!      (1) (2)
!      (1) (3)
!      (2) (1)
!      (2) (3)
!      (3) (1)
!      (3) (2)
!
!  Example:
!
!     N         A(N)
!     0           1
!     1           1
!     2           3
!     3          13
!     4          75
!     5         541
!     6        4683
!     7       47293
!     8      545835
!     9     7087261
!    10   102247563
!
!  Recursion:
!
!    A(I) = sum ( 0 <= J < I ) Binomial ( I, N-J ) * A(J)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Daniel Velleman, Gregory Call,
!    Permutations and Combination Locks,
!    Mathematics Magazine,
!    Volume 68, Number 4, October 1995, pages 243-253.
!
!  Parameters:
!
!    Input, integer N, the maximum number of lock buttons.
!
!    Output, integer A(0:N), the number of lock codes.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER A (0:N)
    INTEGER I
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    A (0) = 1
 
    DO I = 1, N
        A (I) = 0
        DO J = 0, I - 1
            A (I) = A (I) + I4_CHOOSE (I, I-J) * A (J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MEIXNER (N, BETA, C, X, V)
 
!*****************************************************************************80
!
!! MEIXNER() evaluates Meixner polynomials at a point.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 March 2009
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Walter Gautschi,
!    Orthogonal Polynomials: Computation and Approximation,
!    Oxford, 2004,
!    ISBN: 0-19-850672-4,
!    LC: QA404.5 G3555.
!
!  Parameters:
!
!    Input, integer N, the maximum order of the polynomial.
!    N must be at least 0.
!
!    Input, real ( kind = rk ) BETA, the Beta parameter.  0 < BETA.
!
!    Input, real ( kind = rk ) C, the C parameter.  0 < C < 1.
!
!    Input, real ( kind = rk ) X, the evaluation point.
!
!    Output, real ( kind = rk ) V(0:N), the value of the polynomials at X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) BETA
    REAL (KIND=RK) C
    INTEGER I
    REAL (KIND=RK) V (0:N)
    REAL (KIND=RK) X
 
    IF (BETA <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MEIXNER - Fatal error!'
        WRITE (*, '(a)') '  Parameter BETA must be positive.'
        STOP 1
    END IF
 
    IF (C <= 0.0D+00 .OR. 1.0D+00 <= C) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MEIXNER - Fatal error!'
        WRITE (*, '(a)') '  Parameter C must be strictly between 0 and 1.'
        STOP 1
    END IF
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MEIXNER - Fatal error!'
        WRITE (*, '(a)') '  Parameter N must be nonnegative.'
        STOP 1
    END IF
 
    V (0) = 1.0D+00
 
    IF (N == 0) THEN
        RETURN
    END IF
 
    V (1) = (C-1.0D+00) * X / BETA / C + 1.0D+00
 
    IF (N == 1) THEN
        RETURN
    END IF
 
    DO I = 1, N - 1
        V (I+1) = (((C-1.0D+00)*X+(1.0D+00+C)*REAL(I, KIND=RK)+BETA*C)*V(I)-REAL(I, &
       & KIND=RK)*V(I-1)) / (REAL(I, KIND=RK)+BETA)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION MERTENS (N)
 
!*****************************************************************************80
!
!! MERTENS() evaluates the Mertens function.
!
!  Discussion:
!
!    The Mertens function M(N) is the sum from 1 to N of the Moebius
!    function MU.  That is,
!
!    M(N) = sum ( 1 <= I <= N ) MU(I)
!
!        N   M(N)
!        --  ----
!         1     1
!         2     0
!         3    -1
!         4    -1
!         5    -2
!         6    -1
!         7    -2
!         8    -2
!         9    -2
!        10    -1
!        11    -2
!        12    -2
!       100     1
!      1000     2
!     10000   -23
!    100000   -48
!
!    The determinant of the Redheffer matrix of order N is equal
!    to the Mertens function M(N).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    M Deleglise, J Rivat,
!    Computing the Summation of the Moebius Function,
!    Experimental Mathematics,
!    Volume 5, 1996, pages 291-295.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer N, the argument.
!
!    Output, integer MERTENS, the value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER MERTENS
    INTEGER MU_I
    INTEGER N
    INTEGER VALUE
 
    VALUE = 0
 
    DO I = 1, N
        CALL MOEBIUS (I, MU_I)
        VALUE = VALUE + MU_I
    END DO
 
    MERTENS = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MERTENS_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! MERTENS_VALUES() returns some values of the Mertens function.
!
!  Discussion:
!
!    The Mertens function M(N) is the sum from 1 to N of the Moebius
!    function MU.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    M Deleglise, J Rivat,
!    Computing the Summation of the Moebius Function,
!    Experimental Mathematics,
!    Volume 5, 1996, pages 291-295.
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the Mertens function.
!
!    Output, integer C, the value of the Mertens function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 15
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 0, - 1, - 1, - 2, - 1, - 2, - 2, - 2, - 1, &
   & - 2, - 2, 1, 2, - 23 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 100, &
   & 1000, 10000 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MOEBIUS (N, MU)
 
!*****************************************************************************80
!
!! MOEBIUS() returns the value of MU(N), the Moebius function of N.
!
!  Discussion:
!
!    MU(N) is defined as follows:
!
!      MU(N) = 1 if N = 1;
!              0 if N is divisible by the square of a prime;
!              (-1)^K, if N is the product of K distinct primes.
!
!    As special cases, MU(N) is -1 if N is a prime, and MU(N) is 0
!    if N is a square, cube, etc.
!
!    The Moebius function is related to Euler's totient function:
!
!      PHI(N) = Sum ( D divides N ) MU(D) * ( N / D ).
!
!  Example:
!
!     N  MU(N)
!
!     1    1
!     2   -1
!     3   -1
!     4    0
!     5   -1
!     6    1
!     7   -1
!     8    0
!     9    0
!    10    1
!    11   -1
!    12    0
!    13   -1
!    14    1
!    15    1
!    16    0
!    17   -1
!    18    0
!    19   -1
!    20    0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value to be analyzed.
!
!    Output, integer MU, the value of MU(N).
!    If N is less than or equal to 0, MU will be returned as -2.
!    If there was not enough internal space for factoring, MU
!    is returned as -3.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER MU
    INTEGER N
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER POWER (MAXFACTOR)
 
    IF (N <= 0) THEN
        MU = - 2
        RETURN
    END IF
 
    IF (N == 1) THEN
        MU = 1
        RETURN
    END IF
!
!  Factor N.
!
    CALL I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'MOEBIUS - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space.'
        MU = - 3
        STOP 1
    END IF
 
    MU = 1
 
    DO I = 1, NFACTOR
 
        MU = - MU
 
        IF (1 < POWER(I)) THEN
            MU = 0
            RETURN
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MOEBIUS_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! MOEBIUS_VALUES() returns some values of the Moebius function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the Moebius function.
!
!    Output, integer C, the value of the Moebius function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 20
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, - 1, - 1, 0, - 1, 1, - 1, 0, 0, 1, - 1, 0, &
   & - 1, 1, 1, 0, - 1, 0, - 1, 0 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, &
   & 14, 15, 16, 17, 18, 19, 20 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE MOTZKIN (N, A)
 
!*****************************************************************************80
!
!! MOTZKIN() returns the Motzkin numbers up to order N.
!
!  Discussion:
!
!    The Motzkin number A(N) counts the number of distinct paths
!    from (0,0) to (0,N) in which the only steps used are
!    (1,1), (1,-1) and (1,0), and the path is never allowed to
!    go below the X axis.
!
!  Example:
!
!     N  A(N)
!
!     0    1
!     1    1
!     2    2
!     3    4
!     4    9
!     5   21
!     6   51
!     7  127
!     8  323
!     9  835
!    10 2188
!
!  Recursion:
!
!    A(N) = A(N-1) + sum ( 0 <= K <= N-2 ) A(K) * A(N-2-K)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 May 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer N, the highest order Motzkin number to compute.
!
!    Output, integer A(0:N), the Motzkin numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER A (0:N)
    INTEGER I
    INTEGER J
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    A (0) = 1
 
    DO I = 1, N
        A (I) = A (I-1)
        DO J = 0, I - 2
            A (I) = A (I) + A (J) * A (I-2-J)
        END DO
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_CDF_INVERSE (P, VALUE)
 
!*****************************************************************************80
!
!! NORMAL_01_CDF_INVERSE() inverts the standard normal CDF.
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
!    Input, real ( kind = rk ) P, the value of the cumulative probability
!    densitity function.  0 < P < 1.  If P is outside this range, an
!    "infinite" value will be returned.
!
!    Output, real ( kind = rk ) VALUE, the normal deviate value
!    with the property that the probability of a standard normal deviate being
!    less than or equal to the value is P.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: A = (/ 3.3871328727963666080D+00, &
   & 1.3314166789178437745D+02, 1.9715909503065514427D+03, 1.3731693765509461125D+04, &
   & 4.5921953931549871457D+04, 6.7265770927008700853D+04, 3.3430575583588128105D+04, &
   & 2.5090809287301226727D+03 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: B = (/ 1.0D+00, 4.2313330701600911252D+01, &
   & 6.8718700749205790830D+02, 5.3941960214247511077D+03, 2.1213794301586595867D+04, &
   & 3.9307895800092710610D+04, 2.8729085735721942674D+04, 5.2264952788528545610D+03 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: C = (/ 1.42343711074968357734D+00, &
   & 4.63033784615654529590D+00, 5.76949722146069140550D+00, 3.64784832476320460504D+00, &
   & 1.27045825245236838258D+00, 2.41780725177450611770D-01, 2.27238449892691845833D-02, &
   & 7.74545014278341407640D-04 /)
    REAL (KIND=RK), PARAMETER :: CONST1 = 0.180625D+00
    REAL (KIND=RK), PARAMETER :: CONST2 = 1.6D+00
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: D = (/ 1.0D+00, 2.05319162663775882187D+00, &
   & 1.67638483018380384940D+00, 6.89767334985100004550D-01, 1.48103976427480074590D-01, &
   & 1.51986665636164571966D-02, 5.47593808499534494600D-04, 1.05075007164441684324D-09 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: E = (/ 6.65790464350110377720D+00, &
   & 5.46378491116411436990D+00, 1.78482653991729133580D+00, 2.96560571828504891230D-01, &
   & 2.65321895265761230930D-02, 1.24266094738807843860D-03, 2.71155556874348757815D-05, &
   & 2.01033439929228813265D-07 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (0:7) :: F = (/ 1.0D+00, 5.99832206555887937690D-01, &
   & 1.36929880922735805310D-01, 1.48753612908506148525D-02, 7.86869131145613259100D-04, &
   & 1.84631831751005468180D-05, 1.42151175831644588870D-07, 2.04426310338993978564D-15 /)
    REAL (KIND=RK) P
    REAL (KIND=RK) Q
    REAL (KIND=RK) R
    REAL (KIND=RK), PARAMETER :: SPLIT1 = 0.425D+00
    REAL (KIND=RK), PARAMETER :: SPLIT2 = 5.0D+00
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X
 
    IF (P <= 0.0D+00) THEN
        VALUE = - HUGE (P)
        RETURN
    END IF
 
    IF (1.0D+00 <= P) THEN
        VALUE = HUGE (P)
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
 
    VALUE = X
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE NORMAL_01_CDF_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! NORMAL_01_CDF_VALUES() returns some values of the Normal 01 CDF.
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 17
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.5000000000000000D+00, &
   & 0.5398278372770290D+00, 0.5792597094391030D+00, 0.6179114221889526D+00, &
   & 0.6554217416103242D+00, 0.6914624612740131D+00, 0.7257468822499270D+00, &
   & 0.7580363477769270D+00, 0.7881446014166033D+00, 0.8159398746532405D+00, &
   & 0.8413447460685429D+00, 0.9331927987311419D+00, 0.9772498680518208D+00, &
   & 0.9937903346742239D+00, 0.9986501019683699D+00, 0.9997673709209645D+00, &
   & 0.9999683287581669D+00 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.0000000000000000D+00, &
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
 
SUBROUTINE OMEGA (N, NDIV)
 
!*****************************************************************************80
!
!! OMEGA() returns OMEGA(N), the number of distinct prime divisors of N.
!
!  Discussion:
!
!    The formula is:
!
!      If N = 1, then
!
!        OMEGA(N) = 1
!
!      else if the prime factorization of N is
!
!        N = P1^E1 * P2^E2 * ... * PM^EM,
!
!      then
!
!        OMEGA(N) = M
!
!  Example:
!
!     N   OMEGA(N)
!
!     1    1
!     2    1
!     3    1
!     4    1
!     5    1
!     6    2
!     7    1
!     8    1
!     9    1
!    10    2
!    11    1
!    12    2
!    13    1
!    14    2
!    15    2
!    16    1
!    17    1
!    18    2
!    19    1
!    20    2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value to be analyzed.  N must be 1 or
!    greater.
!
!    Output, integer NDIV, the value of OMEGA(N).  But if N is 0 or
!    less, NDIV is returned as 0, a nonsense value.  If there is
!    not enough room for factoring, NDIV is returned as -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER N
    INTEGER NDIV
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER POWER (MAXFACTOR)
 
    IF (N <= 0) THEN
        NDIV = 0
        RETURN
    END IF
 
    IF (N == 1) THEN
        NDIV = 1
        RETURN
    END IF
!
!  Factor N.
!
    CALL I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'OMEGA - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space.'
        NDIV = - 1
        STOP 1
    END IF
 
    NDIV = NFACTOR
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE OMEGA_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! OMEGA_VALUES() returns some values of the OMEGA function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,s,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the OMEGA function.
!
!    Output, integer C, the value of the OMEGA function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 23
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 3, 1, 4, 4, 3, &
   & 1, 5, 2, 2, 1, 6, 7, 8 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 101, 210, &
   & 1320, 1764, 2003, 2310, 2827, 8717, 12553, 30030, 510510, 9699690 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PARTITION_DISTINCT_COUNT_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! partition_distinct_count_values() returns some values of Q(N).
!
!  Discussion:
!
!    A partition of an integer N is a representation of the integer
!    as the sum of nonzero positive integers.  The order of the summands
!    does not matter.  The number of partitions of N is symbolized
!    by P(N).  Thus, the number 5 has P(N) = 7, because it has the
!    following partitions:
!
!    5 = 5
!      = 4 + 1
!      = 3 + 2
!      = 3 + 1 + 1
!      = 2 + 2 + 1
!      = 2 + 1 + 1 + 1
!      = 1 + 1 + 1 + 1 + 1
!
!    However, if we require that each member of the partition
!    be distinct, so that no nonzero summand occurs more than once,
!    we are computing something symbolized by Q(N).
!    The number 5 has Q(N) = 3, because it has the following partitions
!    into distinct parts:
!
!    5 = 5
!      = 4 + 1
!      = 3 + 2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the integer.
!
!    Output, integer C, the number of partitions of the integer
!    into distinct parts.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 21
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 1, 1, 2, 2, 3, 4, 5, 6, 8, 10, 12, 15, 18, &
   & 22, 27, 32, 38, 46, 54, 64 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, &
   & 14, 15, 16, 17, 18, 19, 20 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PENTAGON_NUM (N, P)
 
!*****************************************************************************80
!
!! pentagon_num() computes the N-th pentagonal number.
!
!  Discussion:
!
!    The pentagonal number P(N) counts the number of dots in a figure of
!    N nested pentagons.  The pentagonal numbers are defined for both
!    positive and negative N.
!
!    The formula is:
!
!      P(N) = ( N * ( 3 * N - 1 ) ) / 2
!
!  First values:
!
!    N   P
!
!   -5  40
!   -4  26
!   -3  15
!   -2   7
!   -1   2
!    0   0
!    1   1
!    2   5
!    3  12
!    4  22
!    5  35
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the pentagonal number desired.
!
!    Output, integer P, the value of the N-th pentagonal number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER P
 
    P = (N*(3*N-1)) / 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PHI (N, PHIN)
 
!*****************************************************************************80
!
!! phi() computes the number of relatively prime predecessors of an integer.
!
!  Discussion:
!
!    PHI(N) is the number of integers between 1 and N which are
!    relatively prime to N.  I and J are relatively prime if they
!    have no common factors.  The function PHI(N) is known as
!    "Euler's totient function".
!
!    By convention, 1 and N are relatively prime.
!
!    The formula is:
!
!      PHI(U*V) = PHI(U) * PHI(V) if U and V are relatively prime.
!
!      PHI(P^K) = P^(K-1) * ( P - 1 ) if P is prime.
!
!      PHI(N) = N * Product ( P divides N ) ( 1 - 1 / P )
!
!      N = Sum ( D divides N ) PHI(D).
!
!  Example:
!
!     N  PHI(N)
!
!     1    1
!     2    1
!     3    2
!     4    2
!     5    4
!     6    2
!     7    6
!     8    4
!     9    6
!    10    4
!    11   10
!    12    4
!    13   12
!    14    6
!    15    8
!    16    8
!    17   16
!    18    6
!    19   18
!    20    8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value to be analyzed.
!
!    Output, integer PHIN, the value of PHI(N).  If N is less than
!    or equal to 0, PHI will be returned as 0.  If there is not enough
!    room for full factoring of N, PHI will be returned as -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER N
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER PHIN
    INTEGER POWER (MAXFACTOR)
 
    IF (N <= 0) THEN
        PHIN = 0
        RETURN
    END IF
 
    IF (N == 1) THEN
        PHIN = 1
        RETURN
    END IF
!
!  Factor N.
!
    CALL I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PHI - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space!'
        PHIN = - 1
        STOP 1
    END IF
 
    PHIN = 1
    DO I = 1, NFACTOR
        PHIN = PHIN * FACTOR (I) ** (POWER(I)-1) * (FACTOR(I)-1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PHI_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! phi_values() returns some values of the PHI function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the PHI function.
!
!    Output, integer C, the value of the PHI function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 20
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 8, 8, 16, 20, &
   & 16, 40, 148, 200, 200, 648 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, &
   & 50, 60, 100, 149, 500, 750, 999 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PI_ESTIMATE (N)
 
!*****************************************************************************80
!
!! pi_estimate() estimates Pi(n), the number of primes less than or equal to n.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2022
!
!  Input:
!
!    integer N: the argument.
!
!  Output:
!
!    real ( kind = rk ) pi_estimate: the estimate for Pi(n).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    REAL (KIND=8) N_DOUBLE
    REAL (KIND=8) PI_ESTIMATE
 
    IF (N == 0) THEN
        PI_ESTIMATE = 0.0
    ELSE
        N_DOUBLE = REAL (N, KIND=RK)
        PI_ESTIMATE = N / LOG (N_DOUBLE)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PI_VALUES (N_DATA, N, P)
 
!*****************************************************************************80
!
!! pi_values() returns values of the Pi function.
!
!  Discussion:
!
!    Pi[n] is the number of primes less than or equal to n.
!
!    In Mathematica, the function can be evaluated by:
!
!      PrimePi[n]
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  The user sets N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    integer N, the argument.
!
!    integer P, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 17
 
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, &
   & 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 /)
    INTEGER P
    INTEGER, SAVE, DIMENSION (N_MAX) :: P_VEC = (/ 4, 8, 10, 12, 15, 17, 19, 22, 24, 25, 168, &
   & 1229, 9592, 78498, 664579, 5761455, 50847534 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        P = 0
    ELSE
        N = N_VEC (N_DATA)
        P = P_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PLANE_PARTITION_NUM (N)
 
!*****************************************************************************80
!
!! plane_partition_num() returns the number of plane partitions of the integer N.
!
!  Discussion:
!
!    A plane partition of a positive integer N is a partition of N in which
!    the parts have been arranged in a 2D array that is nonincreasing across
!    rows and columns.  There are six plane partitions of 3:
!
!      3   2 1   2   1 1 1   1 1   1
!                1           1     1
!                                  1
!
!  First Values:
!
!     N PP(N)
!     0    1
!     1    1
!     2    3
!     3    6
!     4   13
!     5   24
!     6   48
!     7   86
!     8  160
!     9  282
!    10  500
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 April 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Frank Olver, Daniel Lozier, Ronald Boisvert, Charles Clark,
!    NIST Handbook of Mathematical Functions,
!    Cambridge University Press, 2010,
!    ISBN: 978-0521140638,
!    LC: QA331.N57.
!
!  Parameters:
!
!    Input, integer N, the number, which must be at least 0.
!
!    Output, integer PLANE_PARTITION_NUM, the number of
!    plane partitions of N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER J
    INTEGER K
    INTEGER NN
    INTEGER PLANE_PARTITION_NUM
    INTEGER PP (0:N)
    INTEGER S2
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ''
        WRITE (*, '(a)') 'PLANE_PARTITION_NUM - Fatal error!'
        WRITE (*, '(a)') '  0 <= N is required.'
        STOP 1
    END IF
 
    NN = 0
    PP (NN) = 1
 
    NN = 1
    IF (NN <= N) THEN
        PP (NN) = 1
    END IF
 
    DO NN = 2, N
        PP (NN) = 0
        DO J = 1, NN
            S2 = 0
            DO K = 1, J
                IF (MOD(J, K) == 0) THEN
                    S2 = S2 + K * K
                END IF
            END DO
            PP (NN) = PP (NN) + PP (NN-J) * S2
        END DO
        PP (NN) = PP (NN) / NN
    END DO
 
    PLANE_PARTITION_NUM = PP (N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE POLY_BERNOULLI (N, K, B)
 
!*****************************************************************************80
!
!! poly_bernoulli() evaluates the poly-Bernolli numbers with negative index.
!
!  Discussion:
!
!    The poly-Bernoulli numbers B_n^k were defined by M Kaneko
!    formally as the coefficients of X^n/n! in a particular power
!    series.  He also showed that, when the super-index is negative,
!    we have
!
!      B_n^(-k) = Sum ( 0 <= j <= min ( n, k ) )
!        (j!)^2 * S(n+1,j+1) * S(k+1,j+1)
!
!    where S(n,k) is the Stirling number of the second kind, the number of
!    ways to partition a set of size n into k nonempty subset.
!
!    B_n^(-k) is also the number of "lonesum matrices", that is, 0-1
!    matrices of n rows and k columns which are uniquely reconstructable
!    from their row and column sums.
!
!    The poly-Bernoulli numbers get large very quickly.
!
!  Table:
!
!    \ K 0  1    2     3      4       5        6
!    N
!    0   1  1    1     1      1       1        1
!    1   1  2    4     8     16      32       64
!    2   1  4   14    46    146     454     1394
!    3   1  8   46   230   1066    4718    20266
!    4   1 16  146  1066   6902   41506   237686
!    5   1 32  454  4718  41506  329462  2441314
!    6   1 64 1394 20266 237686 2441314 22934774
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Chad Brewbaker,
!    Lonesum (0,1) Matrices and Poly-Bernoulli Numbers of Negative Index,
!    MS Thesis,
!    Iowa State University, 2005.
!
!    M Kaneko,
!    Poly-Bernoulli Numbers,
!    Journal Theorie des Nombres Bordeaux,
!    Volume 9, 1997, pages 221-228.
!
!  Parameters:
!
!    Input, integer N, K, the indices.  N and K should be
!    nonnegative.
!
!    Output, integer B, the value of B_N^(-K).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER B
    INTEGER J
    INTEGER JFACT
    INTEGER JHI
    INTEGER K
    INTEGER M
    INTEGER N
    INTEGER, ALLOCATABLE, DIMENSION (:, :) :: S
 
    IF (N < 0) THEN
        B = 0
        RETURN
    ELSE IF (N == 0) THEN
        B = 1
        RETURN
    END IF
 
    IF (K < 0) THEN
        B = 0
        RETURN
    ELSE IF (K == 0) THEN
        B = 1
        RETURN
    END IF
 
    JHI = MIN (N, K)
    M = MAX (N, K) + 1
 
    ALLOCATE (S(1:M, 1:M))
    CALL STIRLING2 (M, M, S)
 
    JFACT = 1
    B = 0
 
    DO J = 0, JHI
 
        B = B + JFACT * JFACT * S (N+1, J+1) * S (K+1, J+1)
 
        JFACT = JFACT * (J+1)
 
    END DO
 
    DEALLOCATE (S)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION POLY_COEF_COUNT (DIM, DEGREE)
 
!*****************************************************************************80
!
!! poly_coef_count(): polynomial coefficient count given dimension and degree.
!
!  Discussion:
!
!    To count all monomials of degree 5 or less in dimension 3,
!    we can count all monomials of degree 5 in dimension 4.
!
!    To count all monomials of degree 5 in dimension 4, we imagine
!    that each of the variables X, Y, Z and W is a "box" and that
!    we need to drop 5 pebbles into these boxes.  Every distinct
!    way of doing this represents a degree 5 monomial in dimension 4.
!    Ignoring W gives us monomials up to degree five in dimension 3.
!
!    To count them, we draw 3 lines as separators to indicate the
!    4 boxes, and then imagine all distinct sequences involving
!    the three lines and the 5 pebbles.  Indicate the lines by 1's
!    and the pebbles by 0's and we're asking for the number of
!    permutations of 3 1's and 5 0's, which is 8! / (3! 5!)
!
!    In other words, 56 = 8! / (3! 5!) is:
!    * the number of monomials of degree exactly 5 in dimension 4,
!    * the number of monomials of degree 5 or less in dimension 3,
!    * the number of polynomial coefficients of a polynomial of
!      degree 5 in (X,Y,Z).
!
!    In general, the formula for the number of monomials of degree DEG
!    or less in dimension DIM is
!
!      (DEG+DIM)! / (DEG! * DIM!)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer DIM, the dimension of the polynomial.
!    0 <= DIM.
!
!    Input, integer DEGREE, the degree of the polynomnial
!    0 <= DEGREE
!
!    Output, integer POLY_COEF_COUNT, the number of coefficients
!    in the general polynomial of dimension DIM and degree DEGREE.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER DEGREE
    INTEGER DIM
    INTEGER POLY_COEF_COUNT
 
    IF (DIM < 0) THEN
        POLY_COEF_COUNT = - 1
    ELSE IF (DEGREE < 0) THEN
        POLY_COEF_COUNT = - 1
    ELSE
        POLY_COEF_COUNT = I4_CHOOSE (DEGREE+DIM, DEGREE)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PRIME (N)
 
!*****************************************************************************80
!
!! prime() returns any of the first PRIME_MAX prime numbers.
!
!  Discussion:
!
!    PRIME_MAX is 1600, and the largest prime stored is 13499.
!
!    Thanks to Bart Vandewoestyne for pointing out a typo, 18 February 2005.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 February 2005
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
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996, pages 95-98.
!
!  Parameters:
!
!    Input, integer N, the index of the desired prime number.
!    In general, is should be true that 0 <= N <= PRIME_MAX.
!    N = -1 returns PRIME_MAX, the index of the largest prime available.
!    N = 0 is legal, returning PRIME = 1.
!
!    Output, integer PRIME, the N-th prime.  If N is out of range,
!    PRIME is returned as -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: PRIME_MAX = 1600
 
    INTEGER, SAVE :: ICALL = 0
    INTEGER N
    INTEGER, SAVE, DIMENSION (PRIME_MAX) :: NPVEC
    INTEGER PRIME
 
    IF (ICALL == 0) THEN
 
        ICALL = 1
 
        NPVEC (1:100) = (/ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, &
       & 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, &
       & 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, &
       & 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, &
       & 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, &
       & 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541 /)
 
        NPVEC (101:200) = (/ 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, &
       & 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, &
       & 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, &
       & 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, &
       & 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, &
       & 1069, 1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, &
       & 1181, 1187, 1193, 1201, 1213, 1217, 1223 /)
 
        NPVEC (201:300) = (/ 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, &
       & 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423, 1427, &
       & 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, &
       & 1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, &
       & 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699, 1709, &
       & 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, &
       & 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, &
       & 1949, 1951, 1973, 1979, 1987 /)
 
        NPVEC (301:400) = (/ 1993, 1997, 1999, 2003, 2011, 2017, 2027, 2029, 2039, 2053, 2063, &
       & 2069, 2081, 2083, 2087, 2089, 2099, 2111, 2113, 2129, 2131, 2137, 2141, 2143, 2153, &
       & 2161, 2179, 2203, 2207, 2213, 2221, 2237, 2239, 2243, 2251, 2267, 2269, 2273, 2281, &
       & 2287, 2293, 2297, 2309, 2311, 2333, 2339, 2341, 2347, 2351, 2357, 2371, 2377, 2381, &
       & 2383, 2389, 2393, 2399, 2411, 2417, 2423, 2437, 2441, 2447, 2459, 2467, 2473, 2477, &
       & 2503, 2521, 2531, 2539, 2543, 2549, 2551, 2557, 2579, 2591, 2593, 2609, 2617, 2621, &
       & 2633, 2647, 2657, 2659, 2663, 2671, 2677, 2683, 2687, 2689, 2693, 2699, 2707, 2711, &
       & 2713, 2719, 2729, 2731, 2741 /)
 
        NPVEC (401:500) = (/ 2749, 2753, 2767, 2777, 2789, 2791, 2797, 2801, 2803, 2819, 2833, &
       & 2837, 2843, 2851, 2857, 2861, 2879, 2887, 2897, 2903, 2909, 2917, 2927, 2939, 2953, &
       & 2957, 2963, 2969, 2971, 2999, 3001, 3011, 3019, 3023, 3037, 3041, 3049, 3061, 3067, &
       & 3079, 3083, 3089, 3109, 3119, 3121, 3137, 3163, 3167, 3169, 3181, 3187, 3191, 3203, &
       & 3209, 3217, 3221, 3229, 3251, 3253, 3257, 3259, 3271, 3299, 3301, 3307, 3313, 3319, &
       & 3323, 3329, 3331, 3343, 3347, 3359, 3361, 3371, 3373, 3389, 3391, 3407, 3413, 3433, &
       & 3449, 3457, 3461, 3463, 3467, 3469, 3491, 3499, 3511, 3517, 3527, 3529, 3533, 3539, &
       & 3541, 3547, 3557, 3559, 3571 /)
 
        NPVEC (501:600) = (/ 3581, 3583, 3593, 3607, 3613, 3617, 3623, 3631, 3637, 3643, 3659, &
       & 3671, 3673, 3677, 3691, 3697, 3701, 3709, 3719, 3727, 3733, 3739, 3761, 3767, 3769, &
       & 3779, 3793, 3797, 3803, 3821, 3823, 3833, 3847, 3851, 3853, 3863, 3877, 3881, 3889, &
       & 3907, 3911, 3917, 3919, 3923, 3929, 3931, 3943, 3947, 3967, 3989, 4001, 4003, 4007, &
       & 4013, 4019, 4021, 4027, 4049, 4051, 4057, 4073, 4079, 4091, 4093, 4099, 4111, 4127, &
       & 4129, 4133, 4139, 4153, 4157, 4159, 4177, 4201, 4211, 4217, 4219, 4229, 4231, 4241, &
       & 4243, 4253, 4259, 4261, 4271, 4273, 4283, 4289, 4297, 4327, 4337, 4339, 4349, 4357, &
       & 4363, 4373, 4391, 4397, 4409 /)
 
        NPVEC (601:700) = (/ 4421, 4423, 4441, 4447, 4451, 4457, 4463, 4481, 4483, 4493, 4507, &
       & 4513, 4517, 4519, 4523, 4547, 4549, 4561, 4567, 4583, 4591, 4597, 4603, 4621, 4637, &
       & 4639, 4643, 4649, 4651, 4657, 4663, 4673, 4679, 4691, 4703, 4721, 4723, 4729, 4733, &
       & 4751, 4759, 4783, 4787, 4789, 4793, 4799, 4801, 4813, 4817, 4831, 4861, 4871, 4877, &
       & 4889, 4903, 4909, 4919, 4931, 4933, 4937, 4943, 4951, 4957, 4967, 4969, 4973, 4987, &
       & 4993, 4999, 5003, 5009, 5011, 5021, 5023, 5039, 5051, 5059, 5077, 5081, 5087, 5099, &
       & 5101, 5107, 5113, 5119, 5147, 5153, 5167, 5171, 5179, 5189, 5197, 5209, 5227, 5231, &
       & 5233, 5237, 5261, 5273, 5279 /)
 
        NPVEC (701:800) = (/ 5281, 5297, 5303, 5309, 5323, 5333, 5347, 5351, 5381, 5387, 5393, &
       & 5399, 5407, 5413, 5417, 5419, 5431, 5437, 5441, 5443, 5449, 5471, 5477, 5479, 5483, &
       & 5501, 5503, 5507, 5519, 5521, 5527, 5531, 5557, 5563, 5569, 5573, 5581, 5591, 5623, &
       & 5639, 5641, 5647, 5651, 5653, 5657, 5659, 5669, 5683, 5689, 5693, 5701, 5711, 5717, &
       & 5737, 5741, 5743, 5749, 5779, 5783, 5791, 5801, 5807, 5813, 5821, 5827, 5839, 5843, &
       & 5849, 5851, 5857, 5861, 5867, 5869, 5879, 5881, 5897, 5903, 5923, 5927, 5939, 5953, &
       & 5981, 5987, 6007, 6011, 6029, 6037, 6043, 6047, 6053, 6067, 6073, 6079, 6089, 6091, &
       & 6101, 6113, 6121, 6131, 6133 /)
 
        NPVEC (801:900) = (/ 6143, 6151, 6163, 6173, 6197, 6199, 6203, 6211, 6217, 6221, 6229, &
       & 6247, 6257, 6263, 6269, 6271, 6277, 6287, 6299, 6301, 6311, 6317, 6323, 6329, 6337, &
       & 6343, 6353, 6359, 6361, 6367, 6373, 6379, 6389, 6397, 6421, 6427, 6449, 6451, 6469, &
       & 6473, 6481, 6491, 6521, 6529, 6547, 6551, 6553, 6563, 6569, 6571, 6577, 6581, 6599, &
       & 6607, 6619, 6637, 6653, 6659, 6661, 6673, 6679, 6689, 6691, 6701, 6703, 6709, 6719, &
       & 6733, 6737, 6761, 6763, 6779, 6781, 6791, 6793, 6803, 6823, 6827, 6829, 6833, 6841, &
       & 6857, 6863, 6869, 6871, 6883, 6899, 6907, 6911, 6917, 6947, 6949, 6959, 6961, 6967, &
       & 6971, 6977, 6983, 6991, 6997 /)
 
        NPVEC (901:1000) = (/ 7001, 7013, 7019, 7027, 7039, 7043, 7057, 7069, 7079, 7103, 7109, &
       & 7121, 7127, 7129, 7151, 7159, 7177, 7187, 7193, 7207, 7211, 7213, 7219, 7229, 7237, &
       & 7243, 7247, 7253, 7283, 7297, 7307, 7309, 7321, 7331, 7333, 7349, 7351, 7369, 7393, &
       & 7411, 7417, 7433, 7451, 7457, 7459, 7477, 7481, 7487, 7489, 7499, 7507, 7517, 7523, &
       & 7529, 7537, 7541, 7547, 7549, 7559, 7561, 7573, 7577, 7583, 7589, 7591, 7603, 7607, &
       & 7621, 7639, 7643, 7649, 7669, 7673, 7681, 7687, 7691, 7699, 7703, 7717, 7723, 7727, &
       & 7741, 7753, 7757, 7759, 7789, 7793, 7817, 7823, 7829, 7841, 7853, 7867, 7873, 7877, &
       & 7879, 7883, 7901, 7907, 7919 /)
 
        NPVEC (1001:1100) = (/ 7927, 7933, 7937, 7949, 7951, 7963, 7993, 8009, 8011, 8017, &
       & 8039, 8053, 8059, 8069, 8081, 8087, 8089, 8093, 8101, 8111, 8117, 8123, 8147, 8161, &
       & 8167, 8171, 8179, 8191, 8209, 8219, 8221, 8231, 8233, 8237, 8243, 8263, 8269, 8273, &
       & 8287, 8291, 8293, 8297, 8311, 8317, 8329, 8353, 8363, 8369, 8377, 8387, 8389, 8419, &
       & 8423, 8429, 8431, 8443, 8447, 8461, 8467, 8501, 8513, 8521, 8527, 8537, 8539, 8543, &
       & 8563, 8573, 8581, 8597, 8599, 8609, 8623, 8627, 8629, 8641, 8647, 8663, 8669, 8677, &
       & 8681, 8689, 8693, 8699, 8707, 8713, 8719, 8731, 8737, 8741, 8747, 8753, 8761, 8779, &
       & 8783, 8803, 8807, 8819, 8821, 8831 /)
 
        NPVEC (1101:1200) = (/ 8837, 8839, 8849, 8861, 8863, 8867, 8887, 8893, 8923, 8929, &
       & 8933, 8941, 8951, 8963, 8969, 8971, 8999, 9001, 9007, 9011, 9013, 9029, 9041, 9043, &
       & 9049, 9059, 9067, 9091, 9103, 9109, 9127, 9133, 9137, 9151, 9157, 9161, 9173, 9181, &
       & 9187, 9199, 9203, 9209, 9221, 9227, 9239, 9241, 9257, 9277, 9281, 9283, 9293, 9311, &
       & 9319, 9323, 9337, 9341, 9343, 9349, 9371, 9377, 9391, 9397, 9403, 9413, 9419, 9421, &
       & 9431, 9433, 9437, 9439, 9461, 9463, 9467, 9473, 9479, 9491, 9497, 9511, 9521, 9533, &
       & 9539, 9547, 9551, 9587, 9601, 9613, 9619, 9623, 9629, 9631, 9643, 9649, 9661, 9677, &
       & 9679, 9689, 9697, 9719, 9721, 9733 /)
 
        NPVEC (1201:1300) = (/ 9739, 9743, 9749, 9767, 9769, 9781, 9787, 9791, 9803, 9811, &
       & 9817, 9829, 9833, 9839, 9851, 9857, 9859, 9871, 9883, 9887, 9901, 9907, 9923, 9929, &
       & 9931, 9941, 9949, 9967, 9973, 10007, 10009, 10037, 10039, 10061, 10067, 10069, 10079, &
       & 10091, 10093, 10099, 10103, 10111, 10133, 10139, 10141, 10151, 10159, 10163, 10169, &
       & 10177, 10181, 10193, 10211, 10223, 10243, 10247, 10253, 10259, 10267, 10271, 10273, &
       & 10289, 10301, 10303, 10313, 10321, 10331, 10333, 10337, 10343, 10357, 10369, 10391, &
       & 10399, 10427, 10429, 10433, 10453, 10457, 10459, 10463, 10477, 10487, 10499, 10501, &
       & 10513, 10529, 10531, 10559, 10567, 10589, 10597, 10601, 10607, 10613, 10627, 10631, &
       & 10639, 10651, 10657 /)
 
        NPVEC (1301:1400) = (/ 10663, 10667, 10687, 10691, 10709, 10711, 10723, 10729, 10733, &
       & 10739, 10753, 10771, 10781, 10789, 10799, 10831, 10837, 10847, 10853, 10859, 10861, &
       & 10867, 10883, 10889, 10891, 10903, 10909, 10937, 10939, 10949, 10957, 10973, 10979, &
       & 10987, 10993, 11003, 11027, 11047, 11057, 11059, 11069, 11071, 11083, 11087, 11093, &
       & 11113, 11117, 11119, 11131, 11149, 11159, 11161, 11171, 11173, 11177, 11197, 11213, &
       & 11239, 11243, 11251, 11257, 11261, 11273, 11279, 11287, 11299, 11311, 11317, 11321, &
       & 11329, 11351, 11353, 11369, 11383, 11393, 11399, 11411, 11423, 11437, 11443, 11447, &
       & 11467, 11471, 11483, 11489, 11491, 11497, 11503, 11519, 11527, 11549, 11551, 11579, &
       & 11587, 11593, 11597, 11617, 11621, 11633, 11657 /)
 
        NPVEC (1401:1500) = (/ 11677, 11681, 11689, 11699, 11701, 11717, 11719, 11731, 11743, &
       & 11777, 11779, 11783, 11789, 11801, 11807, 11813, 11821, 11827, 11831, 11833, 11839, &
       & 11863, 11867, 11887, 11897, 11903, 11909, 11923, 11927, 11933, 11939, 11941, 11953, &
       & 11959, 11969, 11971, 11981, 11987, 12007, 12011, 12037, 12041, 12043, 12049, 12071, &
       & 12073, 12097, 12101, 12107, 12109, 12113, 12119, 12143, 12149, 12157, 12161, 12163, &
       & 12197, 12203, 12211, 12227, 12239, 12241, 12251, 12253, 12263, 12269, 12277, 12281, &
       & 12289, 12301, 12323, 12329, 12343, 12347, 12373, 12377, 12379, 12391, 12401, 12409, &
       & 12413, 12421, 12433, 12437, 12451, 12457, 12473, 12479, 12487, 12491, 12497, 12503, &
       & 12511, 12517, 12527, 12539, 12541, 12547, 12553 /)
 
        NPVEC (1501:1600) = (/ 12569, 12577, 12583, 12589, 12601, 12611, 12613, 12619, 12637, &
       & 12641, 12647, 12653, 12659, 12671, 12689, 12697, 12703, 12713, 12721, 12739, 12743, &
       & 12757, 12763, 12781, 12791, 12799, 12809, 12821, 12823, 12829, 12841, 12853, 12889, &
       & 12893, 12899, 12907, 12911, 12917, 12919, 12923, 12941, 12953, 12959, 12967, 12973, &
       & 12979, 12983, 13001, 13003, 13007, 13009, 13033, 13037, 13043, 13049, 13063, 13093, &
       & 13099, 13103, 13109, 13121, 13127, 13147, 13151, 13159, 13163, 13171, 13177, 13183, &
       & 13187, 13217, 13219, 13229, 13241, 13249, 13259, 13267, 13291, 13297, 13309, 13313, &
       & 13327, 13331, 13337, 13339, 13367, 13381, 13397, 13399, 13411, 13417, 13421, 13441, &
       & 13451, 13457, 13463, 13469, 13477, 13487, 13499 /)
 
    END IF
 
    IF (N ==-1) THEN
        PRIME = PRIME_MAX
    ELSE IF (N == 0) THEN
        PRIME = 1
    ELSE IF (N <= PRIME_MAX) THEN
        PRIME = NPVEC (N)
    ELSE
        PRIME = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'PRIME - Fatal error!'
        WRITE (*, '(a,i8)') '  Illegal prime index N = ', N
        WRITE (*, '(a,i8)') '  N should be between 1 and PRIME_MAX =', PRIME_MAX
        STOP 1
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE PSI_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! psi_values() returns some values of the Psi or Digamma function.
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 11
 
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ - 0.5772156649015329D+00, - &
   & 0.4237549404110768D+00, - 0.2890398965921883D+00, - 0.1691908888667997D+00, - &
   & 0.6138454458511615D-01, 0.3648997397857652D-01, 0.1260474527734763D+00, &
   & 0.2085478748734940D+00, 0.2849914332938615D+00, 0.3561841611640597D+00, &
   & 0.4227843350984671D+00 /)
    INTEGER N_DATA
    REAL (KIND=RK) X
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, &
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
 
FUNCTION PYRAMID_NUM (N)
 
!*****************************************************************************80
!
!! pyramid_num() returns the N-th pyramidal number.
!
!  Discussion:
!
!    The N-th pyramidal number P(N) is formed by the sum of the first
!    N triangular numbers T(J):
!
!      T(J) = sum ( 1 <= J <= N ) J
!
!      P(N) = sum ( 1 <= I <= N ) T(I)
!
!    By convention, T(0) = 0.
!
!    The formula is:
!
!      P(N) = ( (N+1)^3 - (N+1) ) / 6
!
!    Note that geometrically, this pyramid will have a triangular base,
!    not a square one!
!
!  Example:
!
!    0    0
!    1    1
!    2    4
!    3   10
!    4   20
!    5   35
!    6   56
!    7   84
!    8  120
!    9  165
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the desired number, which
!    must be at least 0.
!
!    Output, integer PYRAMID_NUM, the N-th pyramidal number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER PYRAMID_NUM
 
    PYRAMID_NUM = ((N+1)**3-(N+1)) / 6
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION PYRAMID_SQUARE_NUM (N)
 
!*****************************************************************************80
!
!! pyramid_square_num() returns the N-th pyramidal square number.
!
!  Discussion:
!
!    The N-th pyramidal square number PS(N) is formed by the sum of the first
!    N squares S:
!
!      S(I) = I^2
!
!      PS(N) = sum ( 1 <= I <= N ) S(I)
!
!    By convention, PS(0) = 0.
!
!    The formula is:
!
!      PS(N) = ( N * ( N + 1 ) * ( 2*N+1 ) ) / 6
!
!    Note that geometrically, this pyramid will have a square base.
!
!  Example:
!
!    0    0
!    1    1
!    2    5
!    3   14
!    4   30
!    5   55
!    6   91
!    7  140
!    8  204
!    9  285
!   10  385
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index.
!    0 <= N.
!
!    Output, integer PYRAMID_SQUARE_NUM, the N-th
!    pyramid square number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER PYRAMID_SQUARE_NUM
 
    PYRAMID_SQUARE_NUM = (N*(N+1)*(2*N+1)) / 6
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_AGM (A, B)
 
!*****************************************************************************80
!
!! r8_agm() computes the arithmetic-geometric mean of A and B.
!
!  Discussion:
!
!    The AGM is defined for nonnegative A and B.
!
!    The AGM of numbers A and B is defined by by an iteration:
!
!      A(0) = A
!      B(0) = B
!
!      A(N+1) = ( A(N) + B(N) ) / 2
!      B(N+1) = sqrt ( A(N) * B(N) )
!
!    The two sequences both converge to AGM(A,B).  Convergence can be
!    assumed when the two values are sufficiently close.
!
!    In Mathematica, the AGM can be evaluated by
!
!      ArithmeticGeometricMean [ a, b ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2008
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
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, the arguments whose AGM is to be computed.
!    0 <= A, 0 <= B.
!
!    Output, real ( kind = rk ) R8_AGM, the arithmetic-geometric mean of A and B.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) A1
    REAL (KIND=RK) A2
    REAL (KIND=RK) B
    REAL (KIND=RK) B1
    REAL (KIND=RK) B2
    INTEGER IT
    INTEGER, PARAMETER :: IT_MAX = 1000
    REAL (KIND=RK) R8_AGM
    REAL (KIND=RK) TOL
 
    IF (A < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_AGM - Fatal error!'
        WRITE (*, '(a)') '  A < 0.'
        STOP 1
    END IF
 
    IF (B < 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_AGM - Fatal error!'
        WRITE (*, '(a)') '  B < 0.'
        STOP 1
    END IF
 
    IF (A == 0.0D+00 .OR. B == 0.0D+00) THEN
        R8_AGM = 0.0D+00
        RETURN
    END IF
 
    IF (A == B) THEN
        R8_AGM = A
        RETURN
    END IF
 
    IT = 0
    TOL = 100.0D+00 * EPSILON (TOL)
 
    A1 = A
    B1 = B
 
    DO
 
        IT = IT + 1
 
        A2 = (A1+B1) / 2.0D+00
        B2 = SQRT (A1*B1)
 
        IF (ABS(A2-B2) <= TOL*(A2+B2)) THEN
            EXIT
        END IF
 
        IF (IT_MAX < IT) THEN
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'R8_AGM - Fatal error!'
            WRITE (*, '(a,i8)') '  Exceeded iteration limit ', IT_MAX
            WRITE (*, '(a,g14.6)') '  Estimated value = ', A2
            STOP 1
        END IF
 
        A1 = A2
        B1 = B2
 
    END DO
 
    R8_AGM = A2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_BETA (X, Y)
 
!*****************************************************************************80
!
!! r8_beta() returns the value of the Beta function.
!
!  Discussion:
!
!    The Beta function can be defined in terms of the Gamma function:
!
!      BETA(X,Y) = ( GAMMA(X) * GAMMA(Y) ) / GAMMA(X+Y)
!
!      Both X and Y must be greater than 0.
!
!    The function has the following properties:
!
!      BETA(X,Y) = BETA(Y,X).
!      BETA(X,Y) = Integral ( 0 <= T <= 1 ) T^(X-1) (1-T)^(Y-1) dT.
!      BETA(X,Y) = GAMMA(X) * GAMMA(Y) / GAMMA(X+Y)
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
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, Y, the two parameters that define
!    the Beta function.  X and Y must be greater than 0.
!
!    Output, real ( kind = rk ) R8_BETA, the value of the Beta function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) R8_BETA
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
 
    IF (X <= 0.0D+00 .OR. Y <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_BETA - Fatal error!'
        WRITE (*, '(a)') '  Both X and Y must be greater than 0.'
        STOP 1
    END IF
 
    R8_BETA = EXP (LGAMMA(X)+LGAMMA(Y)-LGAMMA(X+Y))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CHOOSE (N, K)
 
!*****************************************************************************80
!
!! r8_choose() computes the combinatorial coefficient C(N,K).
!
!  Discussion:
!
!    Real arithmetic is used, and C(N,K) is computed directly, via
!    Gamma functions, rather than recursively.
!
!    C(N,K) is the number of distinct combinations of K objects
!    chosen from a set of N distinct objects.  A combination is
!    like a set, in that order does not matter.
!
!    The formula is:
!
!      C(N,K) = N! / ( (N-K)! * K! )
!
!  Example:
!
!    The number of combinations of 2 things chosen from 5 is 10.
!
!    C(5,2) = ( 5 * 4 * 3 * 2 * 1 ) / ( ( 3 * 2 * 1 ) * ( 2 * 1 ) ) = 10.
!
!    The actual combinations may be represented as:
!
!      (1,2), (1,3), (1,4), (1,5), (2,3),
!      (2,4), (2,5), (3,4), (3,5), (4,5).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value of N.
!
!    Input, integer K, the value of K.
!
!    Output, real ( kind = rk ) R8_CHOOSE, the value of C(N,K)
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) ARG
    REAL (KIND=RK) FACK
    REAL (KIND=RK) FACN
    REAL (KIND=RK) FACNMK
    INTEGER K
    INTEGER N
    REAL (KIND=RK) R8_CHOOSE
    REAL (KIND=RK) VALUE
 
    IF (N < 0) THEN
 
        VALUE = 0.0D+00
 
    ELSE IF (K == 0) THEN
 
        VALUE = 1.0D+00
 
    ELSE IF (K == 1) THEN
 
        VALUE = REAL (N, KIND=RK)
 
    ELSE IF (1 < K .AND. K < N-1) THEN
 
        ARG = REAL (N+1, KIND=RK)
        FACN = LGAMMA (ARG)
 
        ARG = REAL (K+1, KIND=RK)
        FACK = LGAMMA (ARG)
 
        ARG = REAL (N-K+1, KIND=RK)
        FACNMK = LGAMMA (ARG)
 
        VALUE = ANINT (EXP(FACN-FACK-FACNMK))
 
    ELSE IF (K == N-1) THEN
 
        VALUE = REAL (N, KIND=RK)
 
    ELSE IF (K == N) THEN
 
        VALUE = 1.0D+00
 
    ELSE
 
        VALUE = 0.0D+00
 
    END IF
 
    R8_CHOOSE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_CUBE_ROOT (X)
 
!*****************************************************************************80
!
!! r8_cube_root() returns the cube root of an R8.
!
!  Discussion:
!
!    This routine is designed to avoid the possible problems that can occur
!    when formulas like 0.0^(1/3) or (-1.0)^(1/3) are to be evaluated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = rk ) X, the number whose cube root is desired.
!
!  Output:
!
!    real ( kind = rk ) R8_CUBE_ROOT, the cube root of X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) R8_CUBE_ROOT
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X
 
    IF (0.0D+00 < X) THEN
        VALUE = X ** (1.0D+00/3.0D+00)
    ELSE IF (X == 0.0D+00) THEN
        VALUE = 0.0D+00
    ELSE
        VALUE = - (ABS(X)) ** (1.0D+00/3.0D+00)
    END IF
 
    R8_CUBE_ROOT = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_EPSILON ()
 
!*****************************************************************************80
!
!! r8_epsilon() returns the R8 roundoff unit.
!
!  Discussion:
!
!    The roundoff unit is a number R which is a power of 2 with the
!    property that, to the precision of the computer's arithmetic,
!      1 < 1 + R
!    but
!      1 = ( 1 + R / 2 )
!
!    FORTRAN90 provides the superior library routine
!
!      EPSILON ( X )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = rk ) R8_EPSILON, the round-off unit.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) R8_EPSILON
 
    R8_EPSILON = 2.220446049250313D-016
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_ERF (X)
 
!*****************************************************************************80
!
!! r8_erf() evaluates the error function.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) ) * Integral ( 0 <= t <= X ) exp ( - t^2 ) dt
!
!    Properties of the function include:
!
!      Limit ( X -> -oo ) ERF(X) =          -1.0;
!                         ERF(0) =           0.0;
!                         ERF(0.476936...) = 0.5;
!      Limit ( X -> +oo ) ERF(X) =          +1.0.
!
!      0.5 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
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
!    Input, real ( kind = rk ) X, the argument of the error function.
!
!    Output, real ( kind = rk ) R8_ERF, the value of the error function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK), PARAMETER, DIMENSION (5) :: A = (/ 3.16112374387056560D+00, &
   & 1.13864154151050156D+02, 3.77485237685302021D+02, 3.20937758913846947D+03, &
   & 1.85777706184603153D-01 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (4) :: B = (/ 2.36012909523441209D+01, &
   & 2.44024637934444173D+02, 1.28261652607737228D+03, 2.84423683343917062D+03 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (9) :: C = (/ 5.64188496988670089D-01, &
   & 8.88314979438837594D+00, 6.61191906371416295D+01, 2.98635138197400131D+02, &
   & 8.81952221241769090D+02, 1.71204761263407058D+03, 2.05107837782607147D+03, &
   & 1.23033935479799725D+03, 2.15311535474403846D-08 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (8) :: D = (/ 1.57449261107098347D+01, &
   & 1.17693950891312499D+02, 5.37181101862009858D+02, 1.62138957456669019D+03, &
   & 3.29079923573345963D+03, 4.36261909014324716D+03, 3.43936767414372164D+03, &
   & 1.23033935480374942D+03 /)
    REAL (KIND=RK) DEL
    INTEGER I
    REAL (KIND=RK), PARAMETER, DIMENSION (6) :: P = (/ 3.05326634961232344D-01, &
   & 3.60344899949804439D-01, 1.25781726111229246D-01, 1.60837851487422766D-02, &
   & 6.58749161529837803D-04, 1.63153871373020978D-02 /)
    REAL (KIND=RK), PARAMETER, DIMENSION (5) :: Q = (/ 2.56852019228982242D+00, &
   & 1.87295284992346047D+00, 5.27905102951428412D-01, 6.05183413124413191D-02, &
   & 2.33520497626869185D-03 /)
    REAL (KIND=RK) R8_ERF
    REAL (KIND=RK), PARAMETER :: SQRPI = 0.56418958354775628695D+00
    REAL (KIND=RK), PARAMETER :: THRESH = 0.46875D+00
    REAL (KIND=RK) X
    REAL (KIND=RK) XABS
    REAL (KIND=RK), PARAMETER :: XBIG = 26.543D+00
    REAL (KIND=RK) XDEN
    REAL (KIND=RK) XNUM
    REAL (KIND=RK) XSQ
 
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
 
        R8_ERF = X * (XNUM+A(4)) / (XDEN+B(4))
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
 
        R8_ERF = (XNUM+C(8)) / (XDEN+D(8))
        XSQ = REAL (INT(XABS*16.0D+00), KIND=RK) / 16.0D+00
        DEL = (XABS-XSQ) * (XABS+XSQ)
        R8_ERF = EXP (-XSQ*XSQ) * EXP (-DEL) * R8_ERF
 
        R8_ERF = (0.5D+00-R8_ERF) + 0.5D+00
 
        IF (X < 0.0D+00) THEN
            R8_ERF = - R8_ERF
        END IF
!
!  Evaluate ERFC(X) for 4.0D+00 < |X|.
!
    ELSE
 
        IF (XBIG <= XABS) THEN
 
            IF (0.0D+00 < X) THEN
                R8_ERF = 1.0D+00
            ELSE
                R8_ERF = - 1.0D+00
            END IF
 
        ELSE
 
            XSQ = 1.0D+00 / (XABS*XABS)
 
            XNUM = P (6) * XSQ
            XDEN = XSQ
            DO I = 1, 4
                XNUM = (XNUM+P(I)) * XSQ
                XDEN = (XDEN+Q(I)) * XSQ
            END DO
 
            R8_ERF = XSQ * (XNUM+P(5)) / (XDEN+Q(5))
            R8_ERF = (SQRPI-R8_ERF) / XABS
            XSQ = REAL (INT(XABS*16.0D+00), KIND=RK) / 16.0D+00
            DEL = (XABS-XSQ) * (XABS+XSQ)
            R8_ERF = EXP (-XSQ*XSQ) * EXP (-DEL) * R8_ERF
 
            R8_ERF = (0.5D+00-R8_ERF) + 0.5D+00
 
            IF (X < 0.0D+00) THEN
                R8_ERF = - R8_ERF
            END IF
 
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_ERF_INVERSE (Y)
 
!*****************************************************************************80
!
!! r8_erf_inverse() inverts the error function.
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
!    Input, real ( kind = rk ) Y, the value of the error function.
!
!    Output, real ( kind = rk ) R8_ERF_INVERSE, the value X such that
!    ERROR_F(X) = Y.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) R8_ERF_INVERSE
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    REAL (KIND=RK) Z
 
    Z = (Y+1.0D+00) / 2.0D+00
 
    CALL NORMAL_01_CDF_INVERSE (Z, X)
 
    R8_ERF_INVERSE = X / SQRT (2.0D+00)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_FACTORIAL (N)
 
!*****************************************************************************80
!
!! r8_factorial() computes the factorial of N.
!
!  Discussion:
!
!    The formula is:
!
!      factorial ( N ) = product ( 1 <= I <= N ) I
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
!    Input, integer N, the argument of the factorial function.
!    If N is less than 1, the function value is returned as 1.
!
!    Output, real ( kind = rk ) R8_FACTORIAL, the factorial of N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER N
    REAL (KIND=RK) R8_FACTORIAL
 
    R8_FACTORIAL = 1.0D+00
 
    DO I = 1, N
        R8_FACTORIAL = R8_FACTORIAL * REAL (I, KIND=RK)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_FACTORIAL_LOG (N)
 
!*****************************************************************************80
!
!! r8_factorial_log() computes the natural logarithm of the factorial of N.
!
!  Discussion:
!
!    The formula is:
!
!      log ( FACTORIAL ( N ) )
!        = log ( product ( 1 <= I <= N ) I )
!        = sum ( ( 1 <= I <= N ) log ( I ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the argument of the factorial function.
!    If N is less than 1, the value is returned as 0.
!
!    Output, real ( kind = rk ) R8_FACTORIAL_LOG, the logarithm of
!    the factorial of N.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER N
    REAL (KIND=RK) R8_FACTORIAL_LOG
 
    R8_FACTORIAL_LOG = 0.0D+00
 
    DO I = 1, N
        R8_FACTORIAL_LOG = R8_FACTORIAL_LOG + LOG (REAL(I, KIND=RK))
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8_FACTORIAL_LOG_VALUES (N_DATA, N, FN)
 
!*****************************************************************************80
!
!! r8_factorial_log_values() returns values of log(factorial(N)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the function.
!
!    Output, real ( kind = rk ) FN, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 27
 
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FNVEC = (/ 0.0D+00, 0.0D+00, 0.6931472D+00, &
   & 1.791757D+00, 3.178051D+00, 4.787489D+00, 6.579246D+00, 8.525160D+00, 10.60460D+00, &
   & 12.80182D+00, 15.10441D+00, 17.50232D+00, 19.98722D+00, 22.55216D+00, 25.19123D+00, &
   & 27.89927D+00, 30.67186D+00, 33.50508D+00, 36.39544D+00, 39.33987D+00, 42.33561D+00, &
   & 58.00362D+00, 148.4778D+00, 363.7394D+00, 605.0201D+00, 2611.331D+00, 5912.128D+00 /)
    REAL (KIND=RK) FN
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: NVEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, &
   & 14, 15, 16, 17, 18, 19, 20, 25, 50, 100, 150, 500, 1000 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        FN = 0.0D+00
    ELSE
        N = NVEC (N_DATA)
        FN = FNVEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8_FACTORIAL_VALUES (N_DATA, N, FN)
 
!*****************************************************************************80
!
!! r8_factorial_values() returns values of the real factorial function.
!
!  Discussion:
!
!    Although the factorial is an integer valued function, it quickly
!    becomes too large for an integer to hold.  This routine still accepts
!    an integer as the input argument, but returns the function value
!    as a real number.
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
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    ISBN: 0-486-61272-4,
!    LC: QA47.A34.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the function.
!
!    Output, real ( kind = rk ) FN, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 23
 
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: FNVEC = (/ 1.0D+00, 1.0D+00, 2.0D+00, 6.0D+00, &
   & 24.0D+00, 120.0D+00, 720.0D+00, 5040.0D+00, 40320.0D+00, 362880.0D+00, 3628800.0D+00, &
   & 39916800.0D+00, 479001600.0D+00, 6227020800.0D+00, 87178291200.0D+00, 1307674368000.0D+00, &
   & 2.0922789888D+13, 3.5568742810D+14, 6.4023737057D+15, 1.2164510041D+17, 2.4329020082D+18, &
   & 1.5511210043D+25, 2.6525285981D+32 /)
    REAL (KIND=RK) FN
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: NVEC = (/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, &
   & 14, 15, 16, 17, 18, 19, 20, 25, 30 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        FN = 0.0D+00
    ELSE
        N = NVEC (N_DATA)
        FN = FNVEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_GAMMA (X)
 
!*****************************************************************************80
!
!! r8_gamma() evaluates Gamma(X) for a real argument.
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
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
!    Input, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) R8_GAMMA, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK), DIMENSION (7) :: C = (/ - 1.910444077728D-03, 8.4171387781295D-04, - &
   & 5.952379913043012D-04, 7.93650793500350248D-04, - 2.777777777777681622553D-03, &
   & 8.333333333333333331554247D-02, 5.7083835261D-03 /)
    REAL (KIND=RK), PARAMETER :: EPS = 2.22D-16
    REAL (KIND=RK) FACT
    INTEGER I
    INTEGER N
    REAL (KIND=RK), DIMENSION (8) :: P = (/ - 1.71618513886549492533811D+00, &
   & 2.47656508055759199108314D+01, - 3.79804256470945635097577D+02, &
   & 6.29331155312818442661052D+02, 8.66966202790413211295064D+02, - &
   & 3.14512729688483675254357D+04, - 3.61444134186911729807069D+04, &
   & 6.64561438202405440627855D+04 /)
    LOGICAL PARITY
    REAL (KIND=RK), DIMENSION (8) :: Q = (/ - 3.08402300119738975254353D+01, &
   & 3.15350626979604161529144D+02, - 1.01515636749021914166146D+03, - &
   & 3.10777167157231109440444D+03, 2.25381184209801510330112D+04, &
   & 4.75584627752788110767815D+03, - 1.34659959864969306392456D+05, - &
   & 1.15132259675553483497211D+05 /)
    REAL (KIND=RK) R8_GAMMA
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.1415926535897932384626434D+00
    REAL (KIND=RK) RES
    REAL (KIND=RK), PARAMETER :: SQRTPI = 0.9189385332046727417803297D+00
    REAL (KIND=RK) SUM
    REAL (KIND=RK) X
    REAL (KIND=RK), PARAMETER :: XBIG = 171.624D+00
    REAL (KIND=RK) XDEN
    REAL (KIND=RK), PARAMETER :: XINF = 1.0D+30
    REAL (KIND=RK), PARAMETER :: XMININ = 2.23D-308
    REAL (KIND=RK) XNUM
    REAL (KIND=RK) Y
    REAL (KIND=RK) Y1
    REAL (KIND=RK) YSQ
    REAL (KIND=RK) Z
 
    PARITY = .FALSE.
    FACT = 1.0D+00
    N = 0
    Y = X
!
!  Argument is negative.
!
    IF (Y <= 0.0D+00) THEN
 
        Y = - X
        Y1 = AINT (Y)
        RES = Y - Y1
 
        IF (RES /= 0.0D+00) THEN
 
            IF (Y1 /= AINT(Y1*0.5D+00)*2.0D+00) THEN
                PARITY = .TRUE.
            END IF
 
            FACT = - R8_PI / SIN (R8_PI*RES)
            Y = Y + 1.0D+00
 
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
            RES = 1.0D+00 / Y
        ELSE
            RES = XINF
            R8_GAMMA = RES
            RETURN
        END IF
 
    ELSE IF (Y < 12.0D+00) THEN
 
        Y1 = Y
!
!  0.0 < argument < 1.0.
!
        IF (Y < 1.0D+00) THEN
 
            Z = Y
            Y = Y + 1.0D+00
!
!  1.0 < argument < 12.0.
!  Reduce argument if necessary.
!
        ELSE
 
            N = INT (Y) - 1
            Y = Y - REAL (N, KIND=RK)
            Z = Y - 1.0D+00
 
        END IF
!
!  Evaluate approximation for 1.0 < argument < 2.0.
!
        XNUM = 0.0D+00
        XDEN = 1.0D+00
        DO I = 1, 8
            XNUM = (XNUM+P(I)) * Z
            XDEN = XDEN * Z + Q (I)
        END DO
 
        RES = XNUM / XDEN + 1.0D+00
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
                Y = Y + 1.0D+00
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
            SUM = SUM + (Y-0.5D+00) * LOG (Y)
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
 
    IF (FACT /= 1.0D+00) THEN
        RES = FACT / RES
    END IF
 
    R8_GAMMA = RES
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_HUGE ()
 
!*****************************************************************************80
!
!! r8_huge() returns a very large R8.
!
!  Discussion:
!
!    The value returned by this function is NOT required to be the
!    maximum representable R8.  This value varies from machine to machine,
!    from compiler to compiler, and may cause problems when being printed.
!    We simply want a "very large" but non-infinite number.
!
!    FORTRAN90 provides a built-in routine HUGE ( X ) that
!    can return the maximum representable number of the same datatype
!    as X, if that is what is really desired.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 October 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = rk ) R8_HUGE, a "huge" value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) R8_HUGE
 
    R8_HUGE = 1.0D+30
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_HYPER_2F1 (A_INPUT, B_INPUT, C_INPUT, X_INPUT)
 
!*****************************************************************************80
!
!! r8_hyper_2f1() evaluates the hypergeometric function F(A,B,C,X).
!
!  Discussion:
!
!    A minor bug was corrected.  The HW variable, used in several places as
!    the "old" value of a quantity being iteratively improved, was not
!    being initialized.  JVB, 11 February 2008.
!
!    The original version of this program allowed the input arguments to
!    be modified, although they were restored to their input values before exit.
!    This is unacceptable if the input arguments are allowed to be constants.
!    The code has been modified so that the input arguments are never modified.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 July 2015
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program provided that the copyright
!    is acknowledged.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45
!
!  Parameters:
!
!    Input, real ( kind = rk ) A_INPUT, B_INPUT, C_INPUT, X_INPUT,
!    the arguments of the function.  The user is allowed to pass these
!    values as constants or variables.
!    C_INPUT must not be equal to a nonpositive integer.
!    X_INPUT < 1.
!
!    Output, real ( kind = rk ) R8_HYPER_2F1, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) A_INPUT
    REAL (KIND=RK) A0
    REAL (KIND=RK) AA
    REAL (KIND=RK) B
    REAL (KIND=RK) B_INPUT
    REAL (KIND=RK) BB
    REAL (KIND=RK) C
    REAL (KIND=RK) C_INPUT
    REAL (KIND=RK) C0
    REAL (KIND=RK) C1
    REAL (KIND=RK), PARAMETER :: EL = 0.5772156649015329D+00
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    REAL (KIND=RK) G2
    REAL (KIND=RK) G3
    REAL (KIND=RK) GA
    REAL (KIND=RK) GABC
    REAL (KIND=RK) GAM
    REAL (KIND=RK) GB
    REAL (KIND=RK) GBM
    REAL (KIND=RK) GC
    REAL (KIND=RK) GCA
    REAL (KIND=RK) GCAB
    REAL (KIND=RK) GCB
    REAL (KIND=RK) GM
    REAL (KIND=RK) HW
    INTEGER J
    INTEGER K
    LOGICAL L0
    LOGICAL L1
    LOGICAL L2
    LOGICAL L3
    LOGICAL L4
    LOGICAL L5
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PA
    REAL (KIND=RK) PB
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) R8_HYPER_2F1
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=RK) RM
    REAL (KIND=RK) RP
    REAL (KIND=RK) SM
    REAL (KIND=RK) SP
    REAL (KIND=RK) SP0
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X
    REAL (KIND=RK) X_INPUT
    REAL (KIND=RK) X1
!
!  Immediately copy the input arguments!
!
    A = A_INPUT
    B = B_INPUT
    C = C_INPUT
    X = X_INPUT
 
    L0 = (C == AINT(C)) .AND. (C < 0.0D+00)
    L1 = (1.0D+00-X < 1.0D-15) .AND. (C-A-B <= 0.0D+00)
    L2 = (A == AINT(A)) .AND. (A < 0.0D+00)
    L3 = (B == AINT(B)) .AND. (B < 0.0D+00)
    L4 = (C-A == AINT(C-A)) .AND. (C-A <= 0.0D+00)
    L5 = (C-B == AINT(C-B)) .AND. (C-B <= 0.0D+00)
 
    IF (L0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_HYPER_2F1 - Fatal error!'
        WRITE (*, '(a)') '  Integral C < 0.'
        WRITE (*, '(a)') '  The hypergeometric series is divergent.'
        STOP 1
    END IF
 
    IF (L1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_HYPER_2F1 - Fatal error!'
        WRITE (*, '(a)') '  The hypergeometric series is divergent.'
        WRITE (*, '(a)') '  1 - X < 0, C - A - B < 0.'
        STOP 1
    END IF
 
    IF (0.95D+00 < X) THEN
        EPS = 1.0D-08
    ELSE
        EPS = 1.0D-15
    END IF
 
    IF (X == 0.0D+00 .OR. A == 0.0D+00 .OR. B == 0.0D+00) THEN
 
        VALUE = 1.0D+00
        R8_HYPER_2F1 = VALUE
        RETURN
 
    ELSE IF (1.0D+00-X == EPS .AND. 0.0D+00 < C-A-B) THEN
 
        GC = GAMMA (C)
        GCAB = GAMMA (C-A-B)
        GCA = GAMMA (C-A)
        GCB = GAMMA (C-B)
        VALUE = GC * GCAB / (GCA*GCB)
        R8_HYPER_2F1 = VALUE
        RETURN
 
    ELSE IF (1.0D+00+X <= EPS .AND. ABS(C-A+B-1.0D+00) <= EPS) THEN
 
        G0 = SQRT (R8_PI) * 2.0D+00 ** (-A)
        G1 = GAMMA (C)
        G2 = GAMMA (1.0D+00+A/2.0D+00-B)
        G3 = GAMMA (0.5D+00+0.5D+00*A)
        VALUE = G0 * G1 / (G2*G3)
        R8_HYPER_2F1 = VALUE
        RETURN
 
    ELSE IF (L2 .OR. L3) THEN
 
        IF (L2) THEN
            NM = INT (ABS(A))
        END IF
 
        IF (L3) THEN
            NM = INT (ABS(B))
        END IF
 
        VALUE = 1.0D+00
        R = 1.0D+00
 
        DO K = 1, NM
            R = R * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X
            VALUE = VALUE + R
        END DO
 
        R8_HYPER_2F1 = VALUE
        RETURN
 
    ELSE IF (L4 .OR. L5) THEN
 
        IF (L4) THEN
            NM = INT (ABS(C-A))
        END IF
 
        IF (L5) THEN
            NM = INT (ABS(C-B))
        END IF
 
        VALUE = 1.0D+00
        R = 1.0D+00
        DO K = 1, NM
            R = R * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X
            VALUE = VALUE + R
        END DO
        VALUE = (1.0D+00-X) ** (C-A-B) * VALUE
        R8_HYPER_2F1 = VALUE
        RETURN
 
    END IF
 
    AA = A
    BB = B
    X1 = X
 
    IF (X < 0.0D+00) THEN
        X = X / (X-1.0D+00)
        IF (A < C .AND. B < A .AND. 0.0D+00 < B) THEN
            A = BB
            B = AA
        END IF
        B = C - B
    END IF
 
    IF (0.75D+00 <= X) THEN
 
        GM = 0.0D+00
 
        IF (ABS(C-A-B-AINT(C-A-B)) < 1.0D-15) THEN
 
            M = INT (C-A-B)
            GA = GAMMA (A)
            GB = GAMMA (B)
            GC = GAMMA (C)
            GAM = GAMMA (A+M)
            GBM = GAMMA (B+M)
 
            PA = R8_PSI (A)
            PB = R8_PSI (B)
 
            IF (M /= 0) THEN
                GM = 1.0D+00
            END IF
 
            DO J = 1, ABS (M) - 1
                GM = GM * J
            END DO
 
            RM = 1.0D+00
            DO J = 1, ABS (M)
                RM = RM * J
            END DO
 
            F0 = 1.0D+00
            R0 = 1.0D+00
            R1 = 1.0D+00
            SP0 = 0.0D+00
            SP = 0.0D+00
 
            IF (0 <= M) THEN
 
                C0 = GM * GC / (GAM*GBM)
                C1 = - GC * (X-1.0D+00) ** M / (GA*GB*RM)
 
                DO K = 1, M - 1
                    R0 = R0 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(K-M)) * (1.0D+00-X)
                    F0 = F0 + R0
                END DO
 
                DO K = 1, M
                    SP0 = SP0 + 1.0D+00 / (A+K-1.0D+00) + 1.0D+00 / (B+K-1.0D+00) - 1.0D+00 / &
                   & REAL (K, KIND=RK)
                END DO
 
                F1 = PA + PB + SP0 + 2.0D+00 * EL + LOG (1.0D+00-X)
                HW = F1
 
                DO K = 1, 250
 
                    SP = SP + (1.0D+00-A) / (K*(A+K-1.0D+00)) + (1.0D+00-B) / (K*(B+K-1.0D+00))
 
                    SM = 0.0D+00
                    DO J = 1, M
                        SM = SM + (1.0D+00-A) / ((J+K)*(A+J+K-1.0D+00)) + 1.0D+00 / &
                       & (B+J+K-1.0D+00)
                    END DO
 
                    RP = PA + PB + 2.0D+00 * EL + SP + SM + LOG (1.0D+00-X)
 
                    R1 = R1 * (A+M+K-1.0D+00) * (B+M+K-1.0D+00) / (K*(M+K)) * (1.0D+00-X)
 
                    F1 = F1 + R1 * RP
 
                    IF (ABS(F1-HW) < ABS(F1)*EPS) THEN
                        EXIT
                    END IF
 
                    HW = F1
 
                END DO
 
                VALUE = F0 * C0 + F1 * C1
 
            ELSE IF (M < 0) THEN
 
                M = - M
                C0 = GM * GC / (GA*GB*(1.0D+00-X)**M)
                C1 = - (-1) ** M * GC / (GAM*GBM*RM)
 
                DO K = 1, M - 1
                    R0 = R0 * (A-M+K-1.0D+00) * (B-M+K-1.0D+00) / (K*(K-M)) * (1.0D+00-X)
                    F0 = F0 + R0
                END DO
 
                DO K = 1, M
                    SP0 = SP0 + 1.0D+00 / REAL (K, KIND=RK)
                END DO
 
                F1 = PA + PB - SP0 + 2.0D+00 * EL + LOG (1.0D+00-X)
                HW = F1
 
                DO K = 1, 250
 
                    SP = SP + (1.0D+00-A) / (K*(A+K-1.0D+00)) + (1.0D+00-B) / (K*(B+K-1.0D+00))
 
                    SM = 0.0D+00
                    DO J = 1, M
                        SM = SM + 1.0D+00 / REAL (J+K, KIND=RK)
                    END DO
 
                    RP = PA + PB + 2.0D+00 * EL + SP - SM + LOG (1.0D+00-X)
 
                    R1 = R1 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(M+K)) * (1.0D+00-X)
 
                    F1 = F1 + R1 * RP
 
                    IF (ABS(F1-HW) < ABS(F1)*EPS) THEN
                        EXIT
                    END IF
 
                    HW = F1
 
                END DO
 
                VALUE = F0 * C0 + F1 * C1
 
            END IF
 
        ELSE
 
            GA = GAMMA (A)
            GB = GAMMA (B)
            GC = GAMMA (C)
            GCA = GAMMA (C-A)
            GCB = GAMMA (C-B)
            GCAB = GAMMA (C-A-B)
            GABC = GAMMA (A+B-C)
            C0 = GC * GCAB / (GCA*GCB)
            C1 = GC * GABC / (GA*GB) * (1.0D+00-X) ** (C-A-B)
            VALUE = 0.0D+00
            HW = VALUE
            R0 = C0
            R1 = C1
 
            DO K = 1, 250
 
                R0 = R0 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(A+B-C+K)) * (1.0D+00-X)
 
                R1 = R1 * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C-A-B+K)) * (1.0D+00-X)
 
                VALUE = VALUE + R0 + R1
 
                IF (ABS(VALUE-HW) < ABS(VALUE)*EPS) THEN
                    EXIT
                END IF
 
                HW = VALUE
 
            END DO
 
            VALUE = VALUE + C0 + C1
 
        END IF
 
    ELSE
 
        A0 = 1.0D+00
 
        IF (A < C .AND. C < 2.0D+00*A .AND. B < C .AND. C < 2.0D+00*B) THEN
 
            A0 = (1.0D+00-X) ** (C-A-B)
            A = C - A
            B = C - B
 
        END IF
 
        VALUE = 1.0D+00
        HW = VALUE
        R = 1.0D+00
 
        DO K = 1, 250
 
            R = R * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X
 
            VALUE = VALUE + R
 
            IF (ABS(VALUE-HW) <= ABS(VALUE)*EPS) THEN
                EXIT
            END IF
 
            HW = VALUE
 
        END DO
 
        VALUE = A0 * VALUE
 
    END IF
 
    IF (X1 < 0.0D+00) THEN
        X = X1
        C0 = 1.0D+00 / (1.0D+00-X) ** AA
        VALUE = C0 * VALUE
    END IF
 
    A = AA
    B = BB
 
    IF (120 < K) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'R8_HYPER_2F1 - Warning!'
        WRITE (*, '(a)') '  A large number of iterations were needed.'
        WRITE (*, '(a)') '  The accuracy of the results should be checked.'
    END IF
 
    R8_HYPER_2F1 = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_MOP (I)
 
!*****************************************************************************80
!
!! r8_mop() returns the I-th power of -1 as an R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = rk ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, the power of -1.
!
!    Output, real ( kind = rk ) R8_MOP, the I-th power of -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    REAL (KIND=RK) R8_MOP
    REAL (KIND=RK) VALUE
 
    IF (MOD(I, 2) == 0) THEN
        VALUE = + 1.0D+00
    ELSE
        VALUE = - 1.0D+00
    END IF
 
    R8_MOP = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_NINT (X)
 
!*****************************************************************************80
!
!! r8_nint() returns the nearest integer to an R8.
!
!  Example:
!
!        X        R8_NINT
!
!      1.3         1
!      1.4         1
!      1.5         1 or 2
!      1.6         2
!      0.0         0
!     -0.7        -1
!     -1.1        -1
!     -1.6        -2
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the value.
!
!    Output, integer R8_NINT, the nearest integer to X.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER R8_NINT
    INTEGER S
    REAL (KIND=RK) X
 
    IF (X < 0.0D+00) THEN
        S = - 1
    ELSE
        S = 1
    END IF
 
    R8_NINT = S * INT (ABS(X)+0.5D+00)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8_PSI (XX)
 
!*****************************************************************************80
!
!! r8_psi() evaluates the function Psi(X).
!
!  Discussion:
!
!    This routine evaluates the logarithmic derivative of the
!    Gamma function,
!
!      PSI(X) = d/dX ( GAMMA(X) ) / GAMMA(X)
!             = d/dX LN ( GAMMA(X) )
!
!    for real X, where either
!
!      - XMAX1 < X < - XMIN, and X is not a negative integer,
!
!    or
!
!      XMIN < X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 January 2008
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody, Anthony Strecok, Henry Thacher,
!    Chebyshev Approximations for the Psi Function,
!    Mathematics of Computation,
!    Volume 27, Number 121, January 1973, pages 123-127.
!
!  Parameters:
!
!    Input, real ( kind = rk ) XX, the argument of the function.
!
!    Output, real ( kind = rk ) R8_PSI, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) AUG
    REAL (KIND=RK) DEN
    INTEGER I
    INTEGER N
    INTEGER NQ
    REAL (KIND=RK), DIMENSION (9) :: P1 = (/ 4.5104681245762934160D-03, &
   & 5.4932855833000385356D+00, 3.7646693175929276856D+02, 7.9525490849151998065D+03, &
   & 7.1451595818951933210D+04, 3.0655976301987365674D+05, 6.3606997788964458797D+05, &
   & 5.8041312783537569993D+05, 1.6585695029761022321D+05 /)
    REAL (KIND=RK), DIMENSION (7) :: P2 = (/ - 2.7103228277757834192D+00, - &
   & 1.5166271776896121383D+01, - 1.9784554148719218667D+01, - 8.8100958828312219821D+00, - &
   & 1.4479614616899842986D+00, - 7.3689600332394549911D-02, - 6.5135387732718171306D-21 /)
    REAL (KIND=RK), PARAMETER :: PIOV4 = 0.78539816339744830962D+00
    REAL (KIND=RK), DIMENSION (8) :: Q1 = (/ 9.6141654774222358525D+01, &
   & 2.6287715790581193330D+03, 2.9862497022250277920D+04, 1.6206566091533671639D+05, &
   & 4.3487880712768329037D+05, 5.4256384537269993733D+05, 2.4242185002017985252D+05, &
   & 6.4155223783576225996D-08 /)
    REAL (KIND=RK), DIMENSION (6) :: Q2 = (/ 4.4992760373789365846D+01, &
   & 2.0240955312679931159D+02, 2.4736979003315290057D+02, 1.0742543875702278326D+02, &
   & 1.7463965060678569906D+01, 8.8427520398873480342D-01 /)
    REAL (KIND=RK) R8_PSI
    REAL (KIND=RK) SGN
    REAL (KIND=RK) UPPER
    REAL (KIND=RK) W
    REAL (KIND=RK) X
    REAL (KIND=RK), PARAMETER :: X01 = 187.0D+00
    REAL (KIND=RK), PARAMETER :: X01D = 128.0D+00
    REAL (KIND=RK), PARAMETER :: X02 = 6.9464496836234126266D-04
    REAL (KIND=RK), PARAMETER :: XINF = 1.70D+38
    REAL (KIND=RK), PARAMETER :: XLARGE = 2.04D+15
    REAL (KIND=RK), PARAMETER :: XMAX1 = 3.60D+16
    REAL (KIND=RK), PARAMETER :: XMIN1 = 5.89D-39
    REAL (KIND=RK), PARAMETER :: XSMALL = 2.05D-09
    REAL (KIND=RK) XX
    REAL (KIND=RK) Z
 
    X = XX
    W = ABS (X)
    AUG = 0.0D+00
!
!  Check for valid arguments, then branch to appropriate algorithm.
!
    IF (XMAX1 <=-X .OR. W < XMIN1) THEN
 
        IF (0.0D+00 < X) THEN
            R8_PSI = - XINF
        ELSE
            R8_PSI = XINF
        END IF
 
        RETURN
    END IF
 
    IF (X < 0.5D+00) THEN
!
!  X < 0.5, use reflection formula: psi(1-x) = psi(x) + pi * cot(pi*x)
!  Use 1/X for PI*COTAN(PI*X)  when  XMIN1 < |X| <= XSMALL.
!
        IF (W <= XSMALL) THEN
 
            AUG = - 1.0D+00 / X
!
!  Argument reduction for cotangent.
!
        ELSE
 
            IF (X < 0.0D+00) THEN
                SGN = PIOV4
            ELSE
                SGN = - PIOV4
            END IF
 
            W = W - REAL (INT(W), KIND=RK)
            NQ = INT (W*4.0D+00)
            W = 4.0D+00 * (W-REAL(NQ, KIND=RK)*0.25D+00)
!
!  W is now related to the fractional part of 4.0 * X.
!  Adjust argument to correspond to values in the first
!  quadrant and determine the sign.
!
            N = NQ / 2
 
            IF (N+N /= NQ) THEN
                W = 1.0D+00 - W
            END IF
 
            Z = PIOV4 * W
 
            IF (MOD(N, 2) /= 0) THEN
                SGN = - SGN
            END IF
!
!  Determine the final value for  -pi * cotan(pi*x).
!
            N = (NQ+1) / 2
            IF (MOD(N, 2) == 0) THEN
!
!  Check for singularity.
!
                IF (Z == 0.0D+00) THEN
 
                    IF (0.0D+00 < X) THEN
                        R8_PSI = - XINF
                    ELSE
                        R8_PSI = XINF
                    END IF
 
                    RETURN
                END IF
 
                AUG = SGN * (4.0D+00/TAN(Z))
 
            ELSE
 
                AUG = SGN * (4.0D+00*TAN(Z))
 
            END IF
 
        END IF
 
        X = 1.0D+00 - X
 
    END IF
!
!  0.5 <= X <= 3.0.
!
    IF (X <= 3.0D+00) THEN
 
        DEN = X
        UPPER = P1 (1) * X
        DO I = 1, 7
            DEN = (DEN+Q1(I)) * X
            UPPER = (UPPER+P1(I+1)) * X
        END DO
        DEN = (UPPER+P1(9)) / (DEN+Q1(8))
        X = (X-X01/X01D) - X02
        R8_PSI = DEN * X + AUG
        RETURN
 
    END IF
!
!  3.0 < X.
!
    IF (X < XLARGE) THEN
        W = 1.0D+00 / (X*X)
        DEN = W
        UPPER = P2 (1) * W
        DO I = 1, 5
            DEN = (DEN+Q2(I)) * W
            UPPER = (UPPER+P2(I+1)) * W
        END DO
        AUG = (UPPER+P2(7)) / (DEN+Q2(6)) - 0.5D+00 / X + AUG
    END IF
 
    R8_PSI = AUG + LOG (X)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8POLY_DEGREE (NA, A)
 
!*****************************************************************************80
!
!! r8poly_degree() returns the degree of a polynomial.
!
!  Discussion:
!
!    The degree of a polynomial is the index of the highest power
!    of X with a nonzero coefficient.
!
!    The degree of a constant polynomial is 0.  The degree of the
!    zero polynomial is debatable, but this routine returns the
!    degree as 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 Januaruy 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NA, the dimension of A.
!
!    Input, real ( kind = rk ) A(0:NA), the coefficients of the polynomials.
!
!    Output, integer R8POLY_DEGREE, the degree of A.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER NA
 
    REAL (KIND=RK) A (0:NA)
    INTEGER R8POLY_DEGREE
    INTEGER VALUE
 
    VALUE = NA
 
    DO WHILE ( 0 < VALUE)
 
        IF (A(VALUE) /= 0.0D+00) THEN
            EXIT
        END IF
 
        VALUE = VALUE - 1
 
    END DO
 
    R8POLY_DEGREE = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8POLY_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! r8poly_print() prints out a polynomial.
!
!  Discussion:
!
!    The power sum form is:
!
!      p(x) = a(0) + a(1) * x + ... + a(n-1) * x^(n-1) + a(n) * x^(n)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the dimension of A.
!
!    Input, real ( kind = rk ) A(0:N), the polynomial coefficients.
!    A(0) is the constant term and
!    A(N) is the coefficient of X^N.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A (0:N)
    INTEGER I
    REAL (KIND=RK) MAG
    INTEGER N2
    CHARACTER PLUS_MINUS
    CHARACTER (LEN=*) TITLE
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
    WRITE (*, '(a)') ' '
 
    N2 = R8POLY_DEGREE (N, A)
 
    IF (N2 < 0) THEN
        WRITE (*, '( ''  p(x) = 0'' )')
        RETURN
    END IF
 
    IF (A(N2) < 0.0D+00) THEN
        PLUS_MINUS = '-'
    ELSE
        PLUS_MINUS = ' '
    END IF
 
    MAG = ABS (A(N2))
 
    IF (2 <= N2) THEN
        WRITE (*, '( ''  p(x) = '', a1, g14.6, '' * x ^ '', i3 )') PLUS_MINUS, MAG, N2
    ELSE IF (N2 == 1) THEN
        WRITE (*, '( ''  p(x) = '', a1, g14.6, '' * x'' )') PLUS_MINUS, MAG
    ELSE IF (N2 == 0) THEN
        WRITE (*, '( ''  p(x) = '', a1, g14.6 )') PLUS_MINUS, MAG
    END IF
 
    DO I = N2 - 1, 0, - 1
 
        IF (A(I) < 0.0D+00) THEN
            PLUS_MINUS = '-'
        ELSE
            PLUS_MINUS = '+'
        END IF
 
        MAG = ABS (A(I))
 
        IF (MAG /= 0.0D+00) THEN
 
            IF (2 <= I) THEN
                WRITE (*, ' ( ''         '', a1, g14.6, '' * x ^ '', i3 )') PLUS_MINUS, MAG, I
            ELSE IF (I == 1) THEN
                WRITE (*, ' ( ''         '', a1, g14.6, '' * x'' )') PLUS_MINUS, MAG
            ELSE IF (I == 0) THEN
                WRITE (*, ' ( ''         '', a1, g14.6 )') PLUS_MINUS, MAG
            END IF
        END IF
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION R8POLY_VALUE_HORNER (M, C, X)
 
!*****************************************************************************80
!
!! r8poly_value_horner() evaluates a polynomial using Horner's method.
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
!    02 January 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the degree.
!
!    Input, real ( kind = rk ) C(0:M), the polynomial coefficients.
!    C(I) is the coefficient of X^I.
!
!    Input, real ( kind = rk ) X, the evaluation point.
!
!    Output, real ( kind = rk ) R8POLY_VALUE_HORNER, the polynomial value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
 
    REAL (KIND=RK) C (0:M)
    INTEGER I
    REAL (KIND=RK) R8POLY_VALUE_HORNER
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) X
 
    VALUE = C (M)
    DO I = M - 1, 0, - 1
        VALUE = VALUE * X + C (I)
    END DO
 
    R8POLY_VALUE_HORNER = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_LINSPACE (N, A, B, X)
 
!*****************************************************************************80
!
!! r8vec_linspace() creates a vector of linearly spaced values.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
!
!    In other words, the interval is divided into N-1 even subintervals,
!    and the endpoints of intervals are used as the points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vector.
!
!    Input, real ( kind = rk ) A, B, the first and last entries.
!
!    Output, real ( kind = rk ) X(N), a vector of linearly spaced data.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    INTEGER I
    REAL (KIND=RK) X (N)
 
    IF (N == 1) THEN
 
        X (1) = (A+B) / 2.0D+00
 
    ELSE
 
        DO I = 1, N
            X (I) = (REAL(N-I, KIND=RK)*A+REAL(I-1, KIND=RK)*B) / REAL (N-1, KIND=RK)
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_PRINT (N, A, TITLE)
 
!*****************************************************************************80
!
!! r8vec_print() prints an R8VEC.
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
!    Input, integer N, the number of components of the vector.
!
!    Input, real ( kind = rk ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A (N)
    INTEGER I
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
 
SUBROUTINE R8VEC_PRINT_SOME (N, A, MAX_PRINT, TITLE)
 
!*****************************************************************************80
!
!! r8vec_print_some() prints "some" of an R8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
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
!    Input, integer N, the number of entries of the vector.
!
!    Input, real ( kind = rk ) A(N), the vector to be printed.
!
!    Input, integer MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A (N)
    INTEGER I
    INTEGER MAX_PRINT
    CHARACTER (LEN=*) TITLE
 
    IF (MAX_PRINT <= 0) THEN
        RETURN
    END IF
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    WRITE (*, '(a)') ' '
    WRITE (*, '(a)') TRIM (TITLE)
    WRITE (*, '(a)') ' '
 
    IF (N <= MAX_PRINT) THEN
 
        DO I = 1, N
            WRITE (*, '(2x,i8,2x,g14.6)') I, A (I)
        END DO
 
    ELSE IF (3 <= MAX_PRINT) THEN
 
        DO I = 1, MAX_PRINT - 2
            WRITE (*, '(2x,i8,2x,g14.6)') I, A (I)
        END DO
        WRITE (*, '(a)') '  ......  ..............'
        I = N
        WRITE (*, '(2x,i8,2x,g14.6)') I, A (I)
 
    ELSE
 
        DO I = 1, MAX_PRINT - 1
            WRITE (*, '(2x,i8,2x,g14.6)') I, A (I)
        END DO
        I = MAX_PRINT
        WRITE (*, '(2x,i8,2x,g14.6,2x,a)') I, A (I), '...more entries...'
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE R8VEC_UNIFORM (N, A, B, R)
 
!*****************************************************************************80
!
!! r8vec_uniform() returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of real ( kind = rk ) values.
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
!    Input, integer M, the number of entries in the vector.
!
!    Input, real ( kind = rk ) A, B, the lower and upper limits.
!
!  Output:
!
!    Output, real ( kind = rk ) R(N), the vector of pseudorandom values.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) R (N)
 
    CALL RANDOM_NUMBER (HARVEST=R(1:N))
    R (1:N) = A + (B-A) * R (1:N)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SIGMA (N, SIGMA_N)
 
!*****************************************************************************80
!
!! sigma() returns the value of SIGMA(N), the divisor sum.
!
!  Discussion:
!
!    SIGMA(N) is the sum of the distinct divisors of N, including 1 and N.
!
!    The formula is:
!
!      SIGMA(U*V) = SIGMA(U) * SIGMA(V) if U and V are relatively prime.
!
!      SIGMA(P^K) = ( P^(K+1) - 1 ) / ( P - 1 ) if P is prime.
!
!  First values:
!
!     N  SIGMA(N)
!
!     1    1
!     2    3
!     3    4
!     4    7
!     5    6
!     6   12
!     7    8
!     8   15
!     9   13
!    10   18
!    11   12
!    12   28
!    13   14
!    14   24
!    15   24
!    16   31
!    17   18
!    18   39
!    19   20
!    20   42
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value to be analyzed.
!
!    Output, integer SIGMA_N, the value of SIGMA(N).  If N is
!    less than or equal to 0, SIGMA_N will be returned as 0.  If there is not
!    enough room for factoring N, SIGMA_N is returned as -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER N
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER POWER (MAXFACTOR)
    INTEGER SIGMA_N
 
    IF (N <= 0) THEN
        SIGMA_N = 0
        RETURN
    END IF
 
    IF (N == 1) THEN
        SIGMA_N = 1
        RETURN
    END IF
!
!  Factor N.
!
    CALL I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SIGMA - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space.'
        SIGMA_N = - 1
        STOP 1
    END IF
 
    SIGMA_N = 1
    DO I = 1, NFACTOR
        SIGMA_N = (SIGMA_N*(FACTOR(I)**(POWER(I)+1)-1)) / (FACTOR(I)-1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SIGMA_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! SIGMA_VALUES() returns some values of the Sigma function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the Sigma function.
!
!    Output, integer C, the value of the Sigma function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 20
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 72, 128, &
   & 255, 176, 576, 1170, 618, 984, 2232, 2340 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 30, 127, 128, &
   & 129, 210, 360, 617, 815, 816, 1000 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SIMPLEX_NUM (M, N)
 
!*****************************************************************************80
!
!! SIMPLEX_NUM() evaluates the N-th Simplex number in M dimensions.
!
!  Discussion:
!
!     N\M: 1    2    3    4    5
!    --   --   --   --   --   --
!     0    0    0    0    0    0
!     1    1    1    1    1    1
!     2    2    3    4    5    6
!     3    3    6   10   15   21
!     4    4   10   20   35   56
!     5    5   15   35   70  126
!     6    6   21   56  126  252
!     7    7   28   84  210  462
!     8    8   36  120  330  792
!     9    9   45  165  495 1287
!    10   10   55  220  715 2002
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 February 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer M, the spatial dimension.
!
!    Input, integer N, the index of the number.
!
!    Output, integer SIMPLEX_NUM, the desired value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER M
    INTEGER N
    INTEGER SIMPLEX_NUM
    INTEGER VALUE
 
    VALUE = 1
    DO I = 1, M
        VALUE = (VALUE*(N+I-1)) / I
    END DO
 
    SIMPLEX_NUM = VALUE
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION SIN_POWER_INT (A, B, N)
 
!*****************************************************************************80
!
!! SIN_POWER_INT() evaluates the sine power integral.
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
!    Input, real ( kind = rk ) A, B, the limits of integration.
!
!    Input, integer N, the power of the sine function.
!
!    Output, real ( kind = rk ) SIN_POWER_INT, the value of the integral.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) CA
    REAL (KIND=RK) CB
    INTEGER M
    INTEGER MLO
    INTEGER N
    REAL (KIND=RK) SA
    REAL (KIND=RK) SB
    REAL (KIND=RK) SIN_POWER_INT
    REAL (KIND=RK) VALUE
 
    IF (N < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'SIN_POWER_INT - Fatal error!'
        WRITE (*, '(a)') '  Power N < 0.'
        VALUE = 0.0D+00
        STOP 1
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
        VALUE = (REAL(M-1, KIND=RK)*VALUE+SA**(M-1)*CA-SB**(M-1)*CB) / REAL (M, KIND=RK)
    END DO
 
    SIN_POWER_INT = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SIN_POWER_INT_VALUES (N_DATA, A, B, N, FX)
 
!*****************************************************************************80
!
!! SIN_POWER_INT_VALUES() returns some values of the sine power integral.
!
!  Discussion:
!
!    The function has the form
!
!      SIN_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( sin(T) )^N dt
!
!    In Mathematica, the function can be evaluated by:
!
!      Integrate [ ( Sin[x] )^n, { x, a, b } ]
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
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) A, B, the limits of integration.
!
!    Output, integer N, the power.
!
!    Output, real ( kind = rk ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 10
 
    REAL (KIND=RK) A
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.10D+02, 0.00D+00, 0.00D+00, &
   & 0.00D+00, 0.00D+00, 0.00D+00, 0.00D+00, 0.10D+01, 0.00D+00, 0.00D+00 /)
    REAL (KIND=RK) B
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.20D+02, 0.10D+01, 0.10D+01, &
   & 0.10D+01, 0.10D+01, 0.10D+01, 0.20D+01, 0.20D+01, 0.10D+01, 0.10D+01 /)
    REAL (KIND=RK) FX
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.10000000000000000000D+02, &
   & 0.45969769413186028260D+00, 0.27267564329357957615D+00, 0.17894056254885809051D+00, &
   & 0.12402556531520681830D+00, 0.88974396451575946519D-01, 0.90393123848149944133D+00, &
   & 0.81495684202992349481D+00, 0.21887522421729849008D-01, 0.17023439374069324596D-01 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 0, 1, 2, 3, 4, 5, 5, 5, 10, 11 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0.0D+00
        B = 0.0D+00
        N = 0
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        N = N_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SLICES (DIM_NUM, SLICE_NUM, PIECE_NUM)
 
!*****************************************************************************80
!
!! SLICES(): maximum number of pieces created by a given number of slices.
!
!  Discussion:
!
!    If we imagine slicing a pizza, each slice produce more pieces.
!    The position of the slice affects the number of pieces created, but there
!    is a maximum.
!
!    This function determines the maximum number of pieces created by a given
!    number of slices, applied to a space of a given dimension.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 2011
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Banks,
!    Slicing Pizzas, Racing Turtles, and Further Adventures in
!    Applied Mathematics,
!    Princeton, 1999,
!    ISBN13: 9780691059471,
!    LC: QA93.B358.
!
!  Parameters:
!
!    Input, integer DIM_NUM, the spatial dimension.
!
!    Input, integer SLICE_NUM, the number of slices.
!
!    Input, integer PIECE_NUM, the maximum number of pieces that
!    can be created by the given number of slices applied in the given
!    dimension.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER DIM_NUM
    INTEGER J
    INTEGER PIECE_NUM
    INTEGER SLICE_NUM
 
    PIECE_NUM = 0
    DO J = 0, MIN (DIM_NUM, SLICE_NUM)
        PIECE_NUM = PIECE_NUM + I4_CHOOSE (SLICE_NUM, J)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SPHERICAL_HARMONIC (L, M, THETA, PHI, C, S)
 
!*****************************************************************************80
!
!! SPHERICAL_HARMONIC() evaluates spherical harmonic functions.
!
!  Discussion:
!
!    The spherical harmonic function Y(L,M,THETA,PHI,X) is the
!    angular part of the solution to Laplace's equation in spherical
!    coordinates.
!
!    Y(L,M,THETA,PHI,X) is related to the associated Legendre
!    function as follows:
!
!      Y(L,M,THETA,PHI,X) = FACTOR * P(L,M,cos(THETA)) * exp ( i * M * PHI )
!
!    Here, FACTOR is a normalization factor:
!
!      FACTOR = sqrt ( ( 2 * L + 1 ) * ( L - M )! / ( 4 * PI * ( L + M )! ) )
!
!    In Mathematica, a spherical harmonic function can be evaluated by
!
!      SphericalHarmonicY [ l, m, theta, phi ]
!
!    Note that notational tradition in physics requires that THETA
!    and PHI represent the reverse of what they would normally mean
!    in mathematical notation; that is, THETA goes up and down, and
!    PHI goes around.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 March 2005
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
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
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
!    Input, integer L, the first index of the spherical harmonic
!    function.  Normally, 0 <= L.
!
!    Input, integer M, the second index of the spherical harmonic
!    function.  Normally, -L <= M <= L.
!
!    Input, real ( kind = rk ) THETA, the polar or latitudinal angle, for which
!    0 <= THETA <= PI.
!
!    Input, real ( kind = rk ) PHI, the longitudinal angle, for which
!    0 <= PHI <= 2*PI.
!
!    Output, real ( kind = rk ) C(0:L), S(0:L), the real and imaginary
!    parts of the functions Y(L,0:L,THETA,PHI).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER L
 
    REAL (KIND=RK) C (0:L)
    INTEGER M
    INTEGER M_ABS
    REAL (KIND=RK) PHI
    REAL (KIND=RK) PLM (0:L)
    REAL (KIND=RK) S (0:L)
    REAL (KIND=RK) THETA
 
    M_ABS = ABS (M)
 
    CALL LEGENDRE_ASSOCIATED_NORMALIZED (L, M_ABS, COS(THETA), PLM)
 
    C (0:L) = PLM (0:L) * COS (REAL(M, KIND=RK)*PHI)
    S (0:L) = PLM (0:L) * SIN (REAL(M, KIND=RK)*PHI)
 
    IF (M < 0) THEN
        C (0:L) = - C (0:L)
        S (0:L) = - S (0:L)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE SPHERICAL_HARMONIC_VALUES (N_DATA, L, M, THETA, PHI, YR, YI)
 
!*****************************************************************************80
!
!! SPHERICAL_HARMONIC_VALUES() returns values of spherical harmonic functions.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by
!
!      SphericalHarmonicY [ l, m, theta, phi ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2005
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
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Cambridge University Press, 1999,
!    ISBN: 0-521-64314-7,
!    LC: QA76.95.W65.
!
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer L, integer M,
!    real ( kind = rk ) THETA, PHI, the arguments of the function.
!
!    Output, real ( kind = rk ) YR, YI, the real and imaginary parts of
!    the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 20
 
    INTEGER L
    INTEGER, SAVE, DIMENSION (NMAX) :: L_VEC = (/ 0, 1, 2, 3, 4, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, &
   & 3, 3, 3, 3, 3 /)
    INTEGER M
    INTEGER, SAVE, DIMENSION (NMAX) :: M_VEC = (/ 0, 0, 1, 2, 3, 5, 4, 3, 2, 1, 2, 2, 2, 2, 2, &
   & - 1, - 1, - 1, - 1, - 1 /)
    INTEGER N_DATA
    REAL (KIND=RK) PHI
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: PHI_VEC = (/ 0.1047197551196598D+01, &
   & 0.1047197551196598D+01, 0.1047197551196598D+01, 0.1047197551196598D+01, &
   & 0.1047197551196598D+01, 0.6283185307179586D+00, 0.6283185307179586D+00, &
   & 0.6283185307179586D+00, 0.6283185307179586D+00, 0.6283185307179586D+00, &
   & 0.7853981633974483D+00, 0.7853981633974483D+00, 0.7853981633974483D+00, &
   & 0.7853981633974483D+00, 0.7853981633974483D+00, 0.4487989505128276D+00, &
   & 0.8975979010256552D+00, 0.1346396851538483D+01, 0.1795195802051310D+01, &
   & 0.2243994752564138D+01 /)
    REAL (KIND=RK) THETA
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: THETA_VEC = (/ 0.5235987755982989D+00, &
   & 0.5235987755982989D+00, 0.5235987755982989D+00, 0.5235987755982989D+00, &
   & 0.5235987755982989D+00, 0.2617993877991494D+00, 0.2617993877991494D+00, &
   & 0.2617993877991494D+00, 0.2617993877991494D+00, 0.2617993877991494D+00, &
   & 0.6283185307179586D+00, 0.1884955592153876D+01, 0.3141592653589793D+01, &
   & 0.4398229715025711D+01, 0.5654866776461628D+01, 0.3926990816987242D+00, &
   & 0.3926990816987242D+00, 0.3926990816987242D+00, 0.3926990816987242D+00, &
   & 0.3926990816987242D+00 /)
    REAL (KIND=RK) YI
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: YI_VEC = (/ 0.0000000000000000D+00, &
   & 0.0000000000000000D+00, - 0.2897056515173922D+00, 0.1916222768312404D+00, &
   & 0.0000000000000000D+00, 0.0000000000000000D+00, 0.3739289485283311D-02, - &
   & 0.4219517552320796D-01, 0.1876264225575173D+00, - 0.3029973424491321D+00, &
   & 0.4139385503112256D+00, - 0.1003229830187463D+00, 0.0000000000000000D+00, - &
   & 0.1003229830187463D+00, 0.4139385503112256D+00, - 0.1753512375142586D+00, - &
   & 0.3159720118970196D+00, - 0.3940106541811563D+00, - 0.3940106541811563D+00, - &
   & 0.3159720118970196D+00 /)
    REAL (KIND=RK) YR
    REAL (KIND=RK), SAVE, DIMENSION (NMAX) :: YR_VEC = (/ 0.2820947917738781D+00, &
   & 0.4231421876608172D+00, - 0.1672616358893223D+00, - 0.1106331731112457D+00, &
   & 0.1354974113737760D+00, 0.5390423109043568D-03, - 0.5146690442951909D-02, &
   & 0.1371004361349490D-01, 0.6096352022265540D-01, - 0.4170400640977983D+00, &
   & 0.0000000000000000D+00, 0.0000000000000000D+00, 0.0000000000000000D+00, &
   & 0.0000000000000000D+00, 0.0000000000000000D+00, 0.3641205966137958D+00, &
   & 0.2519792711195075D+00, 0.8993036065704300D-01, - 0.8993036065704300D-01, - &
   & 0.2519792711195075D+00 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        L = 0
        M = 0
        THETA = 0.0D+00
        PHI = 0.0D+00
        YR = 0.0D+00
        YI = 0.0D+00
    ELSE
        L = L_VEC (N_DATA)
        M = M_VEC (N_DATA)
        THETA = THETA_VEC (N_DATA)
        PHI = PHI_VEC (N_DATA)
        YR = YR_VEC (N_DATA)
        YI = YI_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION STIRLING_ESTIMATE (N)
 
!*****************************************************************************80
!
!! stirling_estimate() estimates log(n!) by Stirling's approximation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2022
!
!  Author:
!
!    Paul Nahin
!
!  Reference:
!
!    Paul Nahin,
!    Dueling Idiots and Other Probability Puzzlers,
!    Princeton, 2012,
!    ISBN: 978-0691155005.
!
!  Input:
!
!    integer N: the argument.
!
!  Output:
!
!    real ( kind = rk ) stirling_estimate: Stirling's estimate for log(n!).
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    REAL (KIND=8) N_RK
    REAL (KIND=RK), PARAMETER :: R8_PI = 3.141592653589793D+00
    REAL (KIND=8) STIRLING_ESTIMATE
 
    IF (N == 0) THEN
        STIRLING_ESTIMATE = 0.0D+00
    ELSE
        N_RK = REAL (N, KIND=RK)
        STIRLING_ESTIMATE = 0.5D+00 * LOG (2.0D+00*R8_PI*N_RK) + N_RK * (LOG(N_RK)-1.0D+00)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STIRLING1 (N, M, S1)
 
!*****************************************************************************80
!
!! stirling1() computes the Stirling numbers of the first kind.
!
!  Discussion:
!
!    The absolute value of the Stirling number S1(N,M) gives the number
!    of permutations on N objects having exactly M cycles, while the
!    sign of the Stirling number records the sign (odd or even) of
!    the permutations.  For example, there are six permutations on 3 objects:
!
!      A B C   3 cycles (A) (B) (C)
!      A C B   2 cycles (A) (BC)
!      B A C   2 cycles (AB) (C)
!      B C A   1 cycle  (ABC)
!      C A B   1 cycle  (ABC)
!      C B A   2 cycles (AC) (B)
!
!    There are
!
!      2 permutations with 1 cycle, and S1(3,1) = 2
!      3 permutations with 2 cycles, and S1(3,2) = -3,
!      1 permutation with 3 cycles, and S1(3,3) = 1.
!
!    Since there are N! permutations of N objects, the sum of the absolute
!    values of the Stirling numbers in a given row,
!
!      sum ( 1 <= I <= N ) abs ( S1(N,I) ) = N!
!
!  First terms:
!
!    N/M:  1     2      3     4     5    6    7    8
!
!    1     1     0      0     0     0    0    0    0
!    2    -1     1      0     0     0    0    0    0
!    3     2    -3      1     0     0    0    0    0
!    4    -6    11     -6     1     0    0    0    0
!    5    24   -50     35   -10     1    0    0    0
!    6  -120   274   -225    85   -15    1    0    0
!    7   720 -1764   1624  -735   175  -21    1    0
!    8 -5040 13068 -13132  6769 -1960  322  -28    1
!
!  Recursion:
!
!    S1(N,1) = (-1)^(N-1) * (N-1)! for all N.
!    S1(I,I) = 1 for all I.
!    S1(I,J) = 0 if I < J.
!
!    S1(N,M) = S1(N-1,M-1) - (N-1) * S1(N-1,M)
!
!  Properties:
!
!    sum ( 1 <= K <= M ) S2(I,K) * S1(K,J) = Delta(I,J)
!
!    X_N = sum ( 0 <= K <= N ) S1(N,K) X^K
!    where X_N is the falling factorial function.
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
!    Input, integer N, the number of rows of the table.
!
!    Input, integer M, the number of columns of the table.
!
!    Output, integer S1(N,M), the Stirling numbers of the
!    first kind.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER I
    INTEGER J
    INTEGER S1 (N, M)
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    IF (M <= 0) THEN
        RETURN
    END IF
 
    S1 (1, 1) = 1
    S1 (1, 2:M) = 0
 
    DO I = 2, N
 
        S1 (I, 1) = - (I-1) * S1 (I-1, 1)
 
        DO J = 2, M
            S1 (I, J) = S1 (I-1, J-1) - (I-1) * S1 (I-1, J)
        END DO
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STIRLING2 (N, M, S2)
 
!*****************************************************************************80
!
!! stirling2() computes the Stirling numbers of the second kind.
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
!    The Stirling numbers of the second kind S(N,1:N) are the coefficients of
!    the Bell polynomial B(N,X):
!
!      B(0,X) = 1
!      B(N,X) = sum ( 1 <= M <= N ) S(N,M) * X^M
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
!    Input, integer N, the number of rows of the table.
!
!    Input, integer M, the number of columns of the table.
!
!    Output, integer S2(N,M), the Stirling numbers of the
!    second kind.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M
    INTEGER N
 
    INTEGER I
    INTEGER J
    INTEGER S2 (N, M)
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    IF (M <= 0) THEN
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
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TAU (N, TAUN)
 
!*****************************************************************************80
!
!! tau() returns the value of TAU(N), the number of distinct divisors of N.
!
!  Discussion:
!
!    TAU(N) is the number of distinct divisors of N, including 1 and N.
!
!    If the prime factorization of N is
!
!      N = P1^E1 * P2^E2 * ... * PM^EM,
!
!    then
!
!      TAU(N) = ( E1 + 1 ) * ( E2 + 1 ) * ... * ( EM + 1 ).
!
!    One consequence of this fact is that TAU is odd if and only
!    if N is a perfect square.
!
!  First values:
!
!     N   TAU(N)
!
!     1    1
!     2    2
!     3    2
!     4    3
!     5    2
!     6    4
!     7    2
!     8    4
!     9    3
!    10    4
!    11    2
!    12    6
!    13    2
!    14    4
!    15    4
!    16    5
!    17    2
!    18    6
!    19    2
!    20    6
!    21    4
!    22    4
!    23    2
!    24    8
!    25    3
!    26    4
!    27    4
!    28    6
!    29    2
!    30    8
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value to be analyzed.  N must be 1 or
!    greater.
!
!    Output, integer TAUN, the value of TAU(N).  But if N is 0 or
!    less, TAUN is returned as 0, a nonsense value.  If there is
!    not enough room for factoring, TAUN is returned as -1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: MAXFACTOR = 20
 
    INTEGER FACTOR (MAXFACTOR)
    INTEGER I
    INTEGER N
    INTEGER NFACTOR
    INTEGER NLEFT
    INTEGER POWER (MAXFACTOR)
    INTEGER TAUN
 
    IF (N <= 0) THEN
        TAUN = 0
        RETURN
    END IF
 
    IF (N == 1) THEN
        TAUN = 1
        RETURN
    END IF
!
!  Factor N.
!
    CALL I4_FACTOR (N, MAXFACTOR, NFACTOR, FACTOR, POWER, NLEFT)
 
    IF (NLEFT /= 1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TAU - Fatal error!'
        WRITE (*, '(a)') '  Not enough factorization space.'
        TAUN = - 1
        STOP 1
    END IF
 
    TAUN = 1
    DO I = 1, NFACTOR
        TAUN = TAUN * (POWER(I)+1)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TAU_VALUES (N_DATA, N, C)
 
!*****************************************************************************80
!
!! tau_values() returns some values of the Tau function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 2003
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the Tau function.
!
!    Output, integer C, the value of the Tau function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: NMAX = 20
 
    INTEGER C
    INTEGER, SAVE, DIMENSION (NMAX) :: C_VEC = (/ 1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 12, 12, 4, &
   & 18, 24, 2, 8, 14, 28 /)
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (NMAX) :: N_VEC = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 23, 72, 126, &
   & 226, 300, 480, 521, 610, 832, 960 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (NMAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        C = 0
    ELSE
        N = N_VEC (N_DATA)
        C = C_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TETRAHEDRON_NUM (N)
 
!*****************************************************************************80
!
!! tetrahedron_num() returns the N-th tetrahedral number.
!
!  Discussion:
!
!    The N-th tetrahedral number T3(N) is formed by the sum of the first
!    N triangular numbers:
!
!      T3(N) = sum ( 1 <= I <= N ) T2(I)
!            = sum ( 1 <= I <= N ) sum ( 1 <= J < I ) J
!
!    By convention, T3(0) = 0.
!
!    The formula is:
!
!      T3(N) = ( N * ( N + 1 ) * ( N + 2 ) ) / 6
!
!  First Values:
!
!     0
!     1
!     4
!    10
!    20
!    35
!    56
!    84
!   120
!   165
!   220
!   275
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 October 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the desired number, which
!    must be at least 0.
!
!    Output, integer TETRAHEDRON_NUM, the N-th tetrahedron number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER TETRAHEDRON_NUM
 
    TETRAHEDRON_NUM = (N*(N+1)*(N+2)) / 6
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRIANGLE_NUM (N)
 
!*****************************************************************************80
!
!! triangle_num() returns the N-th triangular number.
!
!  Discussion:
!
!    The N-th triangular number T(N) is formed by the sum of the first
!    N integers:
!
!      T(N) = sum ( 1 <= I <= N ) I
!
!    By convention, T(0) = 0.
!
!    T(N) can be computed quickly by the formula:
!
!      T(N) = ( N * ( N + 1 ) ) / 2
!
!  First Values:
!
!     0
!     1
!     3
!     6
!    10
!    15
!    21
!    28
!    36
!    45
!    55
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the desired number,
!    which must be at least 0.
!
!    Output, integer TRIANGLE_NUM, the N-th triangular number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    INTEGER TRIANGLE_NUM
 
    TRIANGLE_NUM = (N*(N+1)) / 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_LOWER_TO_I4 (I, J, K)
 
!*****************************************************************************80
!
!! triangle_lower_to_i4() converts a lower triangular coordinate to an integer.
!
!  Discussion:
!
!    Triangular coordinates are handy when storing a naturally triangular
!    array (such as the lower half of a matrix) in a linear array.
!
!    Thus, for example, we might consider storing
!
!    (1,1)
!    (2,1) (2,2)
!    (3,1) (3,2) (3,3)
!    (4,1) (4,2) (4,3) (4,4)
!
!    as the linear array
!
!    (1,1) (2,1) (2,2) (3,1) (3,2) (3,3) (4,1) (4,2) (4,3) (4,4)
!
!    Here, the quantities in parenthesis represent the natural row and
!    column indices of a single number when stored in a rectangular array.
!
!    Thus, our goal is, given the row I and column J of the data,
!    to produce the value K which indicates its position in the linear
!    array.
!
!    The triangular numbers are the indices associated with the
!    diagonal elements of the original array, T(1,1), T(2,2), T(3,3)
!    and so on.
!
!    The formula is:
!
!      K = J + ( (I-1) * I ) / 2
!
!  First Values:
!
!     I  J  K
!
!     0  0  0
!     1  1  1
!     2  1  2
!     2  2  3
!     3  1  4
!     3  2  5
!     3  3  6
!     4  1  7
!     4  2  8
!     4  3  9
!     4  4 10
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the row and column indices.  I and J must
!    be nonnegative, and J must not be greater than I.
!
!    Output, integer K, the linear index of the (I,J) element.
 
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
 
    IF (I < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_LOWER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  I < 0.'
        WRITE (*, '(a,i8)') '  I = ', I
        STOP 1
    ELSE IF (J < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_LOWER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  J < 0.'
        WRITE (*, '(a,i8)') '  J = ', J
        STOP 1
    ELSE IF (I < J) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_LOWER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  I < J.'
        WRITE (*, '(a,i8)') '  I = ', I
        WRITE (*, '(a,i8)') '  J = ', J
        STOP 1
    END IF
 
    K = J + ((I-1)*I) / 2
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIANGLE_UPPER_TO_I4 (I, J, K)
 
!*****************************************************************************80
!
!! triangle_upper_to_i4() converts an upper triangular coordinate to an integer.
!
!  Discussion:
!
!    Triangular coordinates are handy when storing a naturally triangular
!    array (such as the upper half of a matrix) in a linear array.
!
!    Thus, for example, we might consider storing
!
!    (1,1) (1,2) (1,3) (1,4)
!          (2,2) (2,3) (2,4)
!          (3,2) (3,3) (3,4)
!          (4,2) (4,3) (4,4)
!
!    as the linear array
!
!    (1,1) (1,2) (2,2) (1,3) (2,3) (3,3) (1,4) (2,4) (3,4) (4,4)
!
!    Here, the quantities in parenthesis represent the natural row and
!    column indices of a single number when stored in a rectangular array.
!
!    Thus, our goal is, given the row I and column J of the data,
!    to produce the value K which indicates its position in the linear
!    array.
!
!    The triangular numbers are the indices associated with the
!    diagonal elements of the original array, T(1,1), T(2,2), T(3,3)
!    and so on.
!
!    The formula is:
!
!      K = I + ( (J-1) * J ) / 2
!
!  First Values:
!
!     I  J  K
!
!     0  0  0
!     1  1  1
!     1  2  2
!     2  2  3
!     1  3  4
!     2  3  5
!     3  3  6
!     1  4  7
!     2  4  8
!     3  4  9
!     4  4 10
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2017
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, the row and column indices.  I and J must
!    be nonnegative, and I must not be greater than J.
!
!    Output, integer K, the linear index of the (I,J) element.
 
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
 
    IF (I < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  I < 0.'
        WRITE (*, '(a,i8)') '  I = ', I
        STOP 1
    ELSE IF (J < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  J < 0.'
        WRITE (*, '(a,i8)') '  J = ', J
        STOP 1
    ELSE IF (J < I) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRIANGLE_UPPER_TO_I4 - Fatal error!'
        WRITE (*, '(a)') '  J < I.'
        WRITE (*, '(a,i8)') '  I = ', I
        WRITE (*, '(a,i8)') '  J = ', J
        STOP 1
    END IF
 
    K = I + ((J-1)*J) / 2
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRIBONACCI_DIRECT (N)
 
!*****************************************************************************80
!
!! tribonacci_direct() computes the N-th Tribonacci number directly.
!
!  Example:
!
!     N   T
!    --  --
!     1   0
!     2   0
!     3   1
!     4   1
!     5   2
!     6   4
!     7   7
!     8  13
!     9  24
!    10  44
!    11  81
!    12 149
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N, the index of the number to compute.
!    N should be positive.
!
!  Output:
!
!    integer TRIBONACCI_DIRECT, the value of the N-th number.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) ALPHA
    COMPLEX (KIND=RK) BETA
    COMPLEX (KIND=RK) GAMMA
    INTEGER N
    INTEGER T
    INTEGER TRIBONACCI_DIRECT
 
    CALL TRIBONACCI_ROOTS (ALPHA, BETA, GAMMA)
 
    IF (N <= 0) THEN
        T = 0
    ELSE
        T = NINT (REAL(ALPHA**N/(-ALPHA**2+4.0D+00*ALPHA-1.0D+00)+BETA**N/(-BETA**2+4.0D+00&
       & *BETA-1.0D+00)+GAMMA**N/(-GAMMA**2+4.0D+00*GAMMA-1.0D+00), KIND=RK))
    END IF
 
    TRIBONACCI_DIRECT = T
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIBONACCI_RECURSIVE (N, F)
 
!*****************************************************************************80
!
!! tribonacci_recursive() computes the first N Tribonacci numbers.
!
!  Recursion:
!
!    F(1) = 1
!    F(2) = 1
!    F(3) = 1
!
!    F(N) = F(N-1) + F(N-2) + F(N-3)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    integer N, the highest number to compute.
!
!  Output:
!
!    integer F(N), the first N Tribonacci numbers.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER F (N)
    INTEGER I
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    F (1) = 0
 
    IF (N <= 1) THEN
        RETURN
    END IF
 
    F (2) = 0
 
    IF (N <= 2) THEN
        RETURN
    END IF
 
    F (3) = 1
 
    DO I = 4, N
        F (I) = F (I-1) + F (I-2) + F (I-3)
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE TRIBONACCI_ROOTS (ALPHA, BETA, GAMMA)
 
!*****************************************************************************80
!
!! tribonacci_roots() returns the Tribonacci roots.
!
!  Discussion:
!
!    The Nth Tribonacci number is defined by:
!      T(N) = T(N-1) + T(N-2) + T(N-3)
!    with
!      T(1) = 0, T(2) = 0, T(3) = 1.
!
!    The related polynomial equation
!      x^3 - x^2 - x - 1 = 0
!
!     ALPHA, BETA, and GAMMA are the roots of this equation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 May 2021
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    W R Spickerman,
!    Binet's formula for the Tribonacci sequence,
!    Fibonacci Quarterly,
!    Volume 20, Number 2, pages 118-120, May 1982.
!
!  Output:
!
!    real ( kind = rk ) ALPHA,
!    complex ( kind = rk ) BETA,
!    complex ( kind = rk ) GAMMA, the roots.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) ALPHA
    REAL (KIND=RK) B
    COMPLEX (KIND=RK) BETA
    COMPLEX (KIND=RK) GAMMA
    REAL (KIND=RK) RHO
    REAL (KIND=RK) TAU
 
    RHO = R8_CUBE_ROOT (19.0D+00+3.0D+00*SQRT(33.0D+00))
    TAU = R8_CUBE_ROOT (19.0D+00-3.0D+00*SQRT(33.0D+00))
 
    A = (2.0D+00-RHO-TAU) / 6.0D+00
    B = SQRT (3.0D+00) * (RHO-TAU) / 6.0D+00
 
    ALPHA = (1.0D+00+RHO+TAU) / 3.0D+00
 
    BETA = CMPLX (A,+B)
    GAMMA = CMPLX (A,-B)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION TRINOMIAL (I, J, K)
 
!*****************************************************************************80
!
!! trinomial() computes a trinomial coefficient.
!
!  Discussion:
!
!    The trinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where I objects are of type 1, J of type 2, and K of type 3.
!    and N = I + J + K.
!
!    T(I,J,K) = N! / ( I! J! K! )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2015
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer I, J, K, the factors.
!    All should be nonnegative.
!
!    Output, integer TRINOMIAL, the trinomial coefficient.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER L
    INTEGER T
    INTEGER TRINOMIAL
    INTEGER VALUE
!
!  Each factor must be nonnegative.
!
    IF (I < 0 .OR. J < 0 .OR. K < 0) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'TRINOMIAL - Fatal error!'
        WRITE (*, '(a)') '  Negative factor encountered.'
        STOP 1
    END IF
 
    VALUE = 1
 
    T = 1
 
    DO L = 1, I
!   value = value * t / l
        T = T + 1
    END DO
 
    DO L = 1, J
        VALUE = VALUE * T / L
        T = T + 1
    END DO
 
    DO L = 1, K
        VALUE = VALUE * T / L
        T = T + 1
    END DO
 
    TRINOMIAL = VALUE
 
    RETURN
END
 
!******************************************************************************
 
RECURSIVE FUNCTION V_HOFSTADTER (N) RESULT (VALUE)
 
!*****************************************************************************80
!
!! v_hofstadter() computes the Hofstadter V sequence.
!
!  Discussion:
!
!    V(N) = 0                          if N = 0
!         = 1                          if 1 <= N <= 4
!         = V(N-V(N-1)) + V(N-V(N-4)), otherwise.
!
!    V(N) is defined for all nonnegative integers.
!
!  Table:
!
!     N  V(N)
!    --  ----
!
!     0     0
!     1     1
!     2     1
!     3     1
!     4     1
!     5     2
!     6     3
!     7     4
!     8     5
!     9     5
!    10     6
!    11     6
!    12     7
!    13     8
!    14     8
!    15     9
!    16     9
!    17    10
!    18    11
!    19    11
!    20    11
!    21    12
!    22    12
!    23    13
!    24    14
!    25    14
!    26    15
!    27    15
!    28    16
!    29    17
!    30    17
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 July 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the argument of the function.
!
!    Output, integer V_HOFSTADTER, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER ARG1
    INTEGER ARG2
    INTEGER N
    INTEGER VALUE
 
    IF (N <= 0) THEN
 
        VALUE = 0
 
    ELSE IF (N <= 4) THEN
 
        VALUE = 1
 
    ELSE
 
        ARG1 = N - 1
        ARG2 = N - 4
 
        VALUE = V_HOFSTADTER (N-V_HOFSTADTER(ARG1)) + V_HOFSTADTER (N-V_HOFSTADTER(ARG2))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE VIBONACCI (N, V)
 
!*****************************************************************************80
!
!! vibonacci() computes the first N Vibonacci numbers.
!
!  Discussion:
!
!    The "Vibonacci numbers" are a generalization of the Fibonacci numbers:
!      V(N+1) = +/- V(N) +/- V(N-1)
!    where the signs are chosen randomly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Brian Hayes,
!    The Vibonacci Numbers,
!    American Scientist,
!    July-August 1999, Volume 87, Number 4.
!
!    Divakar Viswanath,
!    Random Fibonacci sequences and the number 1.13198824,
!    Mathematics of Computation,
!    1998.
!
!  Parameters:
!
!    Input, integer N, the highest number to compute.
!
!  Output:
!
!    Output, integer V(N), the first N Vibonacci numbers.  By
!    convention, V(1) and V(2) are taken to be 1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    INTEGER I
    INTEGER J
    INTEGER S1
    INTEGER S2
    INTEGER V (N)
 
    IF (N <= 0) THEN
        RETURN
    END IF
 
    V (1) = 1
 
    IF (N <= 1) THEN
        RETURN
    END IF
 
    V (2) = 1
 
    DO I = 3, N
 
        J = I4_UNIFORM_AB (0, 1)
 
        IF (J == 0) THEN
            S1 = - 1
        ELSE
            S1 = + 1
        END IF
 
        J = I4_UNIFORM_AB (0, 1)
 
        IF (J == 0) THEN
            S2 = - 1
        ELSE
            S2 = + 1
        END IF
 
        V (I) = S1 * V (I-1) + S2 * V (I-2)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZECKENDORF (N, M_MAX, M, I_LIST, F_LIST)
 
!*****************************************************************************80
!
!! zeckendorf() produces the Zeckendorf decomposition of a positive integer.
!
!  Discussion:
!
!    Zeckendorf proved that every positive integer can be represented
!    uniquely as the sum of non-consecutive Fibonacci numbers.
!
!    N = sum ( 1 <= I <= M ) F_LIST(I)
!
!  Example:
!
!     N    Decomposition
!
!    50    34 + 13 + 3
!    51    34 + 13 + 3 + 1
!    52    34 + 13 + 5
!    53    34 + 13 + 5 + 1
!    54    34 + 13 + 5 + 2
!    55    55
!    56    55 + 1
!    57    55 + 2
!    58    55 + 3
!    59    55 + 3 + 1
!    60    55 + 5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the positive integer to be decomposed.
!
!    Input, integer M_MAX, the maximum dimension of I_LIST
!    and F_LIST.
!
!    Output, integer M, the number of parts in the decomposition.
!
!    Output, integer I_LIST(M_MAX), contains in entries 1
!    through M the index of the Fibonacci numbers in the decomposition.
!
!    Output, integer F_LIST(M_MAX), contains in entries 1
!    through M the value of the Fibonacci numbers in the decomposition.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER M_MAX
 
    INTEGER F
    INTEGER F_LIST (M_MAX)
    INTEGER I
    INTEGER I_LIST (M_MAX)
    INTEGER M
    INTEGER N
    INTEGER N_COPY
 
    M = 0
 
    N_COPY = N
!
!  Extract a sequence of Fibonacci numbers.
!
    DO WHILE ( 0 < N_COPY .AND. M < M_MAX)
        CALL FIBONACCI_FLOOR (N_COPY, F, I)
        M = M + 1
        I_LIST (M) = I
        N_COPY = N_COPY - F
    END DO
!
!  Replace any pair of consecutive indices ( I, I-1 ) by I+1.
!
    DO I = M, 2, - 1
 
        IF (I_LIST(I-1) == I_LIST(I)+1) THEN
            I_LIST (I-1) = I_LIST (I-1) + 1
            I_LIST (I:M-1) = I_LIST (I+1:M)
            I_LIST (M) = 0
            M = M - 1
        END IF
 
    END DO
!
!  Fill in the actual values of the Fibonacci numbers.
!
    DO I = 1, M
        CALL FIBONACCI_DIRECT (I_LIST(I), F_LIST(I))
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZERNIKE_POLY (M, N, RHO, Z)
 
!*****************************************************************************80
!
!! zernike_poly() evaluates a Zernike polynomial at RHO.
!
!  Discussion:
!
!    This routine uses the facts that:
!
!    *) R^M_N = 0 if M < 0, or N < 0, or N < M.
!    *) R^M_M = RHO^M
!    *) R^M_N = 0 if mod ( N - M, 2 ) = 1.
!
!    and the recursion:
!
!    R^M_(N+2) = A * [ ( B * RHO^2 - C ) * R^M_N - D * R^M_(N-2) ]
!
!    where
!
!    A = ( N + 2 ) / ( ( N + 2 )^2 - M^2 )
!    B = 4 * ( N + 1 )
!    C = ( N + M )^2 / N + ( N - M + 2 )^2 / ( N + 2 )
!    D = ( N^2 - M^2 ) / N
!
!    I wish I could clean up the recursion in the code, but for
!    now, I have to treat the case M = 0 specially.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer M, the upper index.
!
!    Input, integer N, the lower index.
!
!    Input, real ( kind = rk ) RHO, the radial coordinate.
!
!    Output, real ( kind = rk ) Z, the value of the Zernike
!    polynomial R^M_N at the point RHO.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) C
    REAL (KIND=RK) D
    INTEGER M
    INTEGER N
    INTEGER NN
    REAL (KIND=RK) RHO
    REAL (KIND=RK) Z
    REAL (KIND=RK) ZM2
    REAL (KIND=RK) ZP2
!
!  Do checks.
!
    IF (M < 0) THEN
        Z = 0.0D+00
        RETURN
    END IF
 
    IF (N < 0) THEN
        Z = 0.0D+00
        RETURN
    END IF
 
    IF (N < M) THEN
        Z = 0.0D+00
        RETURN
    END IF
 
    IF (MOD(N-M, 2) == 1) THEN
        Z = 0.0D+00
        RETURN
    END IF
 
    ZM2 = 0.0D+00
    Z = RHO ** M
 
    IF (M == 0) THEN
 
        IF (N == 0) THEN
            RETURN
        END IF
 
        ZM2 = Z
        Z = 2.0D+00 * RHO * RHO - 1.0D+00
 
        DO NN = M + 2, N - 2, 2
 
            A = REAL (NN+2, KIND=RK) / REAL ((NN+2)**2-M**2, KIND=RK)
            B = REAL (4*(NN+1), KIND=RK)
            C = REAL ((NN+M)**2, KIND=RK) / REAL (NN, KIND=RK) + REAL ((NN-M+2)**2, KIND=RK) / &
           & REAL (NN+2, KIND=RK)
            D = REAL (NN**2-M**2, KIND=RK) / REAL (NN, KIND=RK)
 
            ZP2 = A * ((B*RHO**2-C)*Z-D*ZM2)
            ZM2 = Z
            Z = ZP2
 
        END DO
 
    ELSE
 
        DO NN = M, N - 2, 2
 
            A = REAL (NN+2, KIND=RK) / REAL ((NN+2)**2-M**2, KIND=RK)
            B = REAL (4*(NN+1), KIND=RK)
            C = REAL ((NN+M)**2, KIND=RK) / REAL (NN, KIND=RK) + REAL ((NN-M+2)**2, KIND=RK) / &
           & REAL (NN+2, KIND=RK)
            D = REAL (NN**2-M**2, KIND=RK) / REAL (NN, KIND=RK)
 
            ZP2 = A * ((B*RHO**2-C)*Z-D*ZM2)
            ZM2 = Z
            Z = ZP2
 
        END DO
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZERNIKE_POLY_COEF (M, N, C)
 
!*****************************************************************************80
!
!! zernike_poly_coef(): coefficients of a Zernike polynomial.
!
!  Discussion:
!
!    With our coefficients stored in C(0:N), the
!    radial function R^M_N(RHO) is given by
!
!      R^M_N(RHO) = C(0)
!                 + C(1) * RHO
!                 + C(2) * RHO^2
!                 + ...
!                 + C(N) * RHO^N
!
!    and the odd and even Zernike polynomials are
!
!      Z^M_N(RHO,PHI,odd)  = R^M_N(RHO) * sin(PHI)
!      Z^M_N(RHO,PHI,even) = R^M_N(RHO) * cos(PHI)
!
!    The first few "interesting" values of R are:
!
!    R^0_0 = 1
!
!    R^1_1 = RHO
!
!    R^0_2 = 2 * RHO^2 - 1
!    R^2_2 =     RHO^2
!
!    R^1_3 = 3 * RHO^3 - 2 * RHO
!    R^3_3 =     RHO^3
!
!    R^0_4 = 6 * RHO^4 - 6 * RHO^2 + 1
!    R^2_4 = 4 * RHO^4 - 3 * RHO^2
!    R^4_4 =     RHO^4
!
!    R^1_5 = 10 * RHO^5 - 12 * RHO^3 + 3 * RHO
!    R^3_5 =  5 * RHO^5 -  4 * RHO^3
!    R^5_5 =      RHO^5
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein,
!    CRC Concise Encyclopedia of Mathematics,
!    CRC Press, 2002,
!    Second edition,
!    ISBN: 1584883472,
!    LC: QA5.W45
!
!  Parameters:
!
!    Input, integer M, N, the parameters of the polynomial.
!    Normally, 0 <= M <= N and 0 <= N.
!
!    Output, real ( kind = rk ) C(0:N), the coefficients of the polynomial.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
 
    REAL (KIND=RK) C (0:N)
    INTEGER L
    INTEGER M
    INTEGER NM_MINUS
    INTEGER NM_PLUS
 
    C (0:N) = 0.0D+00
 
    IF (N < 0) THEN
        RETURN
    END IF
 
    IF (M < 0) THEN
        RETURN
    END IF
 
    IF (N < M) THEN
        RETURN
    END IF
 
    IF (MOD(N-M, 2) == 1) THEN
        RETURN
    END IF
 
    NM_PLUS = (M+N) / 2
    NM_MINUS = (N-M) / 2
 
    C (N) = R8_CHOOSE (N, NM_PLUS)
 
    DO L = 0, NM_MINUS - 1
 
        C (N-2*L-2) = - REAL ((NM_PLUS-L)*(NM_MINUS-L), KIND=RK) * C (N-2*L) / REAL &
       & ((N-L)*(L+1), KIND=RK)
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ZETA_M1 (P, TOL)
 
!*****************************************************************************80
!
!! zeta_m1() estimates the Riemann Zeta function minus 1.
!
!  Discussion:
!
!    This function includes the Euler-McLaurin correction.
!
!    ZETA_M1 ( P ) = ZETA ( P ) - 1
!
!    ZETA(P) has the form 1 + small terms.  Computing ZETA(P)-1
!    allows for greater accuracy in the small terms.
!
!  Definition:
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
!    16 January 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Thompson,
!    Atlas for Computing Mathematical Functions,
!    Wiley, 1997,
!    ISBN: 0471181714,
!    LC: QA331 T385
!
!  Parameters:
!
!    Input, real ( kind = rk ) P, the power to which the integers are raised.
!    P must be greater than 1.
!
!    Input, real ( kind = rk ) TOL, the requested relative tolerance.
!
!    Output, real ( kind = rk ) ZETA_M1, an approximation to the Riemann
!    Zeta function minus 1.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    REAL (KIND=RK) BASE
    INTEGER K
    INTEGER N
    REAL (KIND=RK) NEGP
    REAL (KIND=RK) NSTERM
    REAL (KIND=RK) P
    REAL (KIND=RK) POWER
    REAL (KIND=RK) TOL
    REAL (KIND=RK) VALUE
    REAL (KIND=RK) ZETA_M1
 
    IF (P <= 1.0D+00) THEN
        WRITE (*, '(a)') ''
        WRITE (*, '(a)') 'ZETA_M1 - Fatal error!'
        WRITE (*, '(a)') '  Exponent P <= 1.0.'
    END IF
 
    NSTERM = P * (P+1.0D+00) * (P+2.0D+00) * (P+3.0D+00) * (P+4.0D+00) / 30240.0D+00
 
    BASE = NSTERM * (2.0D+00**P) / TOL
    POWER = 1.0D+00 / (P+5.0D+00)
    N = INT (BASE**POWER)
 
    IF (N < 10) THEN
        N = 10
    END IF
 
    NEGP = - P
    VALUE = 0.0D+00
    DO K = 2, N - 1
        BASE = REAL (K, KIND=RK)
        VALUE = VALUE + BASE ** NEGP
    END DO
!
!  Euler-McLaurin correction.
!
    BASE = REAL (N, KIND=RK)
 
    VALUE = VALUE + (BASE**NEGP) * (0.5D+00+REAL(N, &
   & KIND=RK)/(P-1.0D+00)+P*(1.0D+00-(P+1.0D+00)*(P+2.0D+00)/REAL(60*N*N, KIND=RK))/REAL(12*N, &
   & KIND=RK)+NSTERM/(BASE**(P+5.0D+00)))
 
    ZETA_M1 = VALUE
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZETA_M1_VALUES (N_DATA, P, ZETA_M1)
 
!*****************************************************************************80
!
!! zeta_m1_values() returns some values of the Riemann Zeta Minus One function.
!
!  Discussion:
!
!    ZETA_M1(N) = ZETA(N) - 1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2017
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = rk ) P, the argument.
!
!    Output, real ( kind = rk ) ZETA_M1, the value.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 17
 
    INTEGER N_DATA
    REAL (KIND=RK) P
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: P_VEC = (/ 2.0D+00, 2.5D+00, 3.0D+00, 3.5D+00, &
   & 4.0D+00, 5.0D+00, 6.0D+00, 7.0D+00, 8.0D+00, 9.0D+00, 10.0D+00, 11.0D+00, 12.0D+00, &
   & 16.0D+00, 20.0D+00, 30.0D+00, 40.0D+00 /)
    REAL (KIND=RK) ZETA_M1
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: ZETA_M1_VEC = (/ 0.64493406684822643647D+00, &
   & 0.3414872573D+00, 0.20205690315959428540D+00, 0.1267338673D+00, 0.8232323371113819152D-01, &
   & 0.3692775514336992633D-01, 0.1734306198444913971D-01, 0.834927738192282684D-02, &
   & 0.407735619794433939D-02, 0.200839292608221442D-02, 0.99457512781808534D-03, &
   & 0.49418860411946456D-03, 0.24608655330804830D-03, 0.1528225940865187D-04, &
   & 0.95396203387280D-06, 0.93132743242D-10, 0.90949478D-12 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        P = 0.0D+00
        ZETA_M1 = 0.0D+00
    ELSE
        P = P_VEC (N_DATA)
        ZETA_M1 = ZETA_M1_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ZETA_NAIVE (P)
 
!*****************************************************************************80
!
!! zeta_naive() estimates the Riemann Zeta function.
!
!  Discussion:
!
!    For 1 < P, the Riemann Zeta function is defined as:
!
!      ZETA ( P ) = Sum ( 1 <= N < +oo ) 1 / N^P
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
!  Reference:
!
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, real ( kind = rk ) P, the power to which the integers are raised.
!    P must be greater than 1.
!
!    Output, real ( kind = rk ) ZETA_NAIVE, an approximation to the Riemann
!    Zeta function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER N
    REAL (KIND=RK) P
    REAL (KIND=RK) TOTAL
    REAL (KIND=RK) TOTAL_OLD
    REAL (KIND=RK) ZETA_NAIVE
 
    IF (P <= 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'ZETA_NAIVE - Fatal error!'
        WRITE (*, '(a)') '  Exponent P <= 1.0.'
        ZETA_NAIVE = - 1.0D+00
        STOP 1
    END IF
 
    TOTAL = 0.0D+00
    N = 0
 
    DO
 
        N = N + 1
        TOTAL_OLD = TOTAL
        TOTAL = TOTAL + 1.0D+00 / (REAL(N, KIND=RK)) ** P
 
        IF (TOTAL <= TOTAL_OLD .OR. 1000 <= N) THEN
            EXIT
        END IF
 
    END DO
 
    ZETA_NAIVE = TOTAL
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE ZETA_VALUES (N_DATA, N, Z)
 
!*****************************************************************************80
!
!! zeta_values() returns some values of the Riemann Zeta function.
!
!  Discussion:
!
!    ZETA(N) = sum ( 1 <= I < +oo ) 1 / I^N
!
!    In Mathematica, the function can be evaluated by:
!
!      Zeta[n]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 2004
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
!  Input:
!
!    integer N_DATA.  Set N_DATA to 0 before the first call.
!
!  Output:
!
!    integer N_DATA.  The routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer N, the argument of the Zeta function.
!
!    Output, real ( kind = rk ) Z, the value of the Zeta function.
!
    IMPLICIT NONE
 
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)
 
    INTEGER, PARAMETER :: N_MAX = 15
 
    INTEGER N
    INTEGER N_DATA
    INTEGER, SAVE, DIMENSION (N_MAX) :: N_VEC = (/ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 16, 20, &
   & 30, 40 /)
    REAL (KIND=RK) Z
    REAL (KIND=RK), SAVE, DIMENSION (N_MAX) :: ZETA_VEC = (/ 0.164493406684822643647D+01, &
   & 0.120205690315959428540D+01, 0.108232323371113819152D+01, 0.103692775514336992633D+01, &
   & 0.101734306198444913971D+01, 0.100834927738192282684D+01, 0.100407735619794433939D+01, &
   & 0.100200839292608221442D+01, 0.100099457512781808534D+01, 0.100049418860411946456D+01, &
   & 0.100024608655330804830D+01, 0.100001528225940865187D+01, 0.100000095396203387280D+01, &
   & 0.100000000093132743242D+01, 0.100000000000090949478D+01 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        N = 0
        Z = 0.0D+00
    ELSE
        N = N_VEC (N_DATA)
        Z = ZETA_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************

END MODULE ModLib_PolPak

!******************************************************************************

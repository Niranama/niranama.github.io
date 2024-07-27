
MODULE ModLib_CDFLib
       
!** PURPOSE OF THIS MODULE:
    ! contains routines that evaluates the cumulative density function (CDF) associated
    ! with common probability distributions, by Barry Brown, James Lovato, Kathy Russell. 
    !
    ! CDFLIB includes routines for evaluating the cumulative density functions of a variety
    !   of standard probability distributions. An unusual feature of this library is its
    !   ability to easily compute any one parameter of the CDF given the others. This means
    !   that a single routine can evaluate the CDF given the usual parameters, or determine
    !   the value of a parameter that produced a given CDF value.
    ! The probability distributions covered include:
    !   - the Beta distribution;
    !   - the binomial distribution;
    !   - the chi-square distribution;
    !   - the noncentral chi-square distribution;
    !   - the F distribution;
    !   - the noncentral F distribution;
    !   - the Gamma distribution;
    !   - the negative binomial distribution;
    !   - the normal distribution;
    !   - the Poisson distribution;
    !   - the T distribution;
    ! Note that the F and noncentral F distributions are not necessarily monotone in either
    ! degree of freedom argument. Consequently, there may be two degree of freedom arguments
    ! that satisfy the specified condition. An arbitrary one of these will be found by the
    ! routines.

!** REFERENCES:
    ! These routines are from CDFLIB package by John Burkardt

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_CDFLib'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
 
FUNCTION ALGDIV (A, B)
 
!*****************************************************************************80
!
!! ALGDIV computes ln ( Gamma ( B ) / Gamma ( A + B ) ) when 8 <= B.
!
!  Discussion:
!
!    In this algorithm, DEL(X) is the function defined by
!
!      ln ( Gamma(X) ) = ( X - 0.5 ) * ln ( X ) - X + 0.5 * ln ( 2 * PI )
!                      + DEL(X).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, define the arguments.
!
!    Output, real ( kind = 8 ) ALGDIV, the value of ln(Gamma(B)/Gamma(A+B)).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALGDIV
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: C0 = 0.833333333333333D-01
    REAL (KIND=8), PARAMETER :: C1 = - 0.277777777760991D-02
    REAL (KIND=8), PARAMETER :: C2 = 0.793650666825390D-03
    REAL (KIND=8), PARAMETER :: C3 = - 0.595202931351870D-03
    REAL (KIND=8), PARAMETER :: C4 = 0.837308034031215D-03
    REAL (KIND=8), PARAMETER :: C5 = - 0.165322962780713D-02
    REAL (KIND=8) D
    REAL (KIND=8) H
    REAL (KIND=8) S11
    REAL (KIND=8) S3
    REAL (KIND=8) S5
    REAL (KIND=8) S7
    REAL (KIND=8) S9
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    IF (B < A) THEN
        H = B / A
        C = 1.0D+00 / (1.0D+00+H)
        X = H / (1.0D+00+H)
        D = A + (B-0.5D+00)
    ELSE
        H = A / B
        C = H / (1.0D+00+H)
        X = 1.0D+00 / (1.0D+00+H)
        D = B + (A-0.5D+00)
    END IF
!
!  Set SN = (1 - X^N)/(1 - X).
!
    X2 = X * X
    S3 = 1.0D+00 + (X+X2)
    S5 = 1.0D+00 + (X+X2*S3)
    S7 = 1.0D+00 + (X+X2*S5)
    S9 = 1.0D+00 + (X+X2*S7)
    S11 = 1.0D+00 + (X+X2*S9)
!
!  Set W = DEL(B) - DEL(A + B).
!
    T = (1.0D+00/B) ** 2
    W = ((((C5*S11*T+C4*S9)*T+C3*S7)*T+C2*S5)*T+C1*S3) * T + C0
 
    W = W * (C/B)
!
!  Combine the results.
!
    U = D * ALNREL (A/B)
    V = A * (LOG(B)-1.0D+00)
 
    IF (V < U) THEN
        ALGDIV = (W-V) - U
    ELSE
        ALGDIV = (W-U) - V
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ALNREL (A)
 
!*****************************************************************************80
!
!! ALNREL evaluates the function ln ( 1 + A ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument.
!
!    Output, real ( kind = 8 ) ALNREL, the value of ln ( 1 + A ).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALNREL
    REAL (KIND=8), PARAMETER :: P1 = - 0.129418923021993D+01
    REAL (KIND=8), PARAMETER :: P2 = 0.405303492862024D+00
    REAL (KIND=8), PARAMETER :: P3 = - 0.178874546012214D-01
    REAL (KIND=8), PARAMETER :: Q1 = - 0.162752256355323D+01
    REAL (KIND=8), PARAMETER :: Q2 = 0.747811014037616D+00
    REAL (KIND=8), PARAMETER :: Q3 = - 0.845104217945565D-01
    REAL (KIND=8) T
    REAL (KIND=8) T2
    REAL (KIND=8) W
    REAL (KIND=8) X
 
    IF (ABS(A) <= 0.375D+00) THEN
 
        T = A / (A+2.0D+00)
        T2 = T * T
 
        W = (((P3*T2+P2)*T2+P1)*T2+1.0D+00) / (((Q3*T2+Q2)*T2+Q1)*T2+1.0D+00)
 
        ALNREL = 2.0D+00 * T * W
 
    ELSE
 
        X = 1.0D+00 + REAL (A, KIND=8)
        ALNREL = LOG (X)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION APSER (A, B, X, EPS)
 
!*****************************************************************************80
!
!! APSER computes the incomplete beta ratio I(SUB(1-X))(B,A).
!
!  Discussion:
!
!    APSER is used only for cases where
!
!      A <= min ( EPS, EPS * B ),
!      B * X <= 1, and
!      X <= 0.5.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, X, the parameters of the
!    incomplete beta ratio.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) APSER, the computed value of the
!    incomplete beta ratio.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) AJ
    REAL (KIND=8) APSER
    REAL (KIND=8) B
    REAL (KIND=8) BX
    REAL (KIND=8) C
    REAL (KIND=8) EPS
    REAL (KIND=8), PARAMETER :: G = 0.577215664901533D+00
    REAL (KIND=8) J
    REAL (KIND=8) S
    REAL (KIND=8) T
    REAL (KIND=8) TOL
    REAL (KIND=8) X
 
    BX = B * X
    T = X - BX
 
    IF (B*EPS <= 0.02D+00) THEN
        C = LOG (X) + PSI (B) + G + T
    ELSE
        C = LOG (BX) + G + T
    END IF
 
    TOL = 5.0D+00 * EPS * ABS (C)
    J = 1.0D+00
    S = 0.0D+00
 
    DO
 
        J = J + 1.0D+00
        T = T * (X-BX/J)
        AJ = T / J
        S = S + AJ
 
        IF (ABS(AJ) <= TOL) THEN
            EXIT
        END IF
 
    END DO
 
    APSER = - A * (C+S)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BCORR (A0, B0)
 
!*****************************************************************************80
!
!! BCORR evaluates DEL(A0) + DEL(B0) - DEL(A0 + B0).
!
!  Discussion:
!
!    The function DEL(A) is a remainder term that is used in the expression:
!
!      ln ( Gamma ( A ) ) = ( A - 0.5 ) * ln ( A )
!        - A + 0.5 * ln ( 2 * PI ) + DEL ( A ),
!
!    or, in other words, DEL ( A ) is defined as:
!
!      DEL ( A ) = ln ( Gamma ( A ) ) - ( A - 0.5 ) * ln ( A )
!        + A + 0.5 * ln ( 2 * PI ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A0, B0, the arguments.
!    It is assumed that 8 <= A0 and 8 <= B0.
!
!    Output, real ( kind = 8 ) BCORR, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) BCORR
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: C0 = 0.833333333333333D-01
    REAL (KIND=8), PARAMETER :: C1 = - 0.277777777760991D-02
    REAL (KIND=8), PARAMETER :: C2 = 0.793650666825390D-03
    REAL (KIND=8), PARAMETER :: C3 = - 0.595202931351870D-03
    REAL (KIND=8), PARAMETER :: C4 = 0.837308034031215D-03
    REAL (KIND=8), PARAMETER :: C5 = - 0.165322962780713D-02
    REAL (KIND=8) H
    REAL (KIND=8) S11
    REAL (KIND=8) S3
    REAL (KIND=8) S5
    REAL (KIND=8) S7
    REAL (KIND=8) S9
    REAL (KIND=8) T
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    A = MIN (A0, B0)
    B = MAX (A0, B0)
 
    H = A / B
    C = H / (1.0D+00+H)
    X = 1.0D+00 / (1.0D+00+H)
    X2 = X * X
!
!  Set SN = (1 - X**N)/(1 - X)
!
    S3 = 1.0D+00 + (X+X2)
    S5 = 1.0D+00 + (X+X2*S3)
    S7 = 1.0D+00 + (X+X2*S5)
    S9 = 1.0D+00 + (X+X2*S7)
    S11 = 1.0D+00 + (X+X2*S9)
!
!  Set W = DEL(B) - DEL(A + B)
!
    T = (1.0D+00/B) ** 2
 
    W = ((((C5*S11*T+C4*S9)*T+C3*S7)*T+C2*S5)*T+C1*S3) * T + C0
 
    W = W * (C/B)
!
!  Compute  DEL(A) + W.
!
    T = (1.0D+00/A) ** 2
 
    BCORR = (((((C5*T+C4)*T+C3)*T+C2)*T+C1)*T+C0) / A + W
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA (A, B)
 
!*****************************************************************************80
!
!! BETA evaluates the beta function.
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
!    Input, real ( kind = 8 ) A, B, the arguments of the beta function.
!
!    Output, real ( kind = 8 ) BETA, the value of the beta function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) BETA
 
    BETA = EXP (BETA_LOG(A, B))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_ASYM (A, B, LAMBDA, EPS)
 
!*****************************************************************************80
!
!! BETA_ASYM computes an asymptotic expansion for IX(A,B), for large A and B.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that both A and B
!    are greater than or equal to 15.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!    It is assumed that 0 <= LAMBDA.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: NUM = 20
 
    REAL (KIND=8) A
    REAL (KIND=8) A0 (NUM+1)
    REAL (KIND=8) B
    REAL (KIND=8) B0 (NUM+1)
    REAL (KIND=8) BETA_ASYM
    REAL (KIND=8) BSUM
    REAL (KIND=8) C (NUM+1)
    REAL (KIND=8) D (NUM+1)
    REAL (KIND=8) DSUM
    REAL (KIND=8), PARAMETER :: E0 = 1.12837916709551D+00
    REAL (KIND=8), PARAMETER :: E1 = 0.353553390593274D+00
    REAL (KIND=8) EPS
    REAL (KIND=8) F
    REAL (KIND=8) H
    REAL (KIND=8) H2
    REAL (KIND=8) HN
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) J0
    REAL (KIND=8) J1
    REAL (KIND=8) LAMBDA
    INTEGER (KIND=4) M
    INTEGER (KIND=4) MM1
    INTEGER (KIND=4) MMJ
    INTEGER (KIND=4) N
    INTEGER (KIND=4) NP1
    REAL (KIND=8) R
    REAL (KIND=8) R0
    REAL (KIND=8) R1
    REAL (KIND=8) S
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8) T0
    REAL (KIND=8) T1
    REAL (KIND=8) U
    REAL (KIND=8) W
    REAL (KIND=8) W0
    REAL (KIND=8) Z
    REAL (KIND=8) Z0
    REAL (KIND=8) Z2
    REAL (KIND=8) ZN
    REAL (KIND=8) ZNM1
 
    BETA_ASYM = 0.0D+00
 
    IF (A < B) THEN
        H = A / B
        R0 = 1.0D+00 / (1.0D+00+H)
        R1 = (B-A) / B
        W0 = 1.0D+00 / SQRT (A*(1.0D+00+H))
    ELSE
        H = B / A
        R0 = 1.0D+00 / (1.0D+00+H)
        R1 = (B-A) / A
        W0 = 1.0D+00 / SQRT (B*(1.0D+00+H))
    END IF
 
    F = A * RLOG1 (-LAMBDA/A) + B * RLOG1 (LAMBDA/B)
    T = EXP (-F)
    IF (T == 0.0D+00) THEN
        RETURN
    END IF
 
    Z0 = SQRT (F)
    Z = 0.5D+00 * (Z0/E1)
    Z2 = F + F
 
    A0 (1) = (2.0D+00/3.0D+00) * R1
    C (1) = - 0.5D+00 * A0 (1)
    D (1) = - C (1)
    J0 = (0.5D+00/E0) * ERROR_FC (1, Z0)
    J1 = E1
    SUM1 = J0 + D (1) * W0 * J1
 
    S = 1.0D+00
    H2 = H * H
    HN = 1.0D+00
    W = W0
    ZNM1 = Z
    ZN = Z2
 
    DO N = 2, NUM, 2
 
        HN = H2 * HN
        A0 (N) = 2.0D+00 * R0 * (1.0D+00+H*HN) / (N+2.0D+00)
        NP1 = N + 1
        S = S + HN
        A0 (NP1) = 2.0D+00 * R1 * S / (N+3.0D+00)
 
        DO I = N, NP1
 
            R = - 0.5D+00 * (I+1.0D+00)
            B0 (1) = R * A0 (1)
            DO M = 2, I
                BSUM = 0.0D+00
                MM1 = M - 1
                DO J = 1, MM1
                    MMJ = M - J
                    BSUM = BSUM + (J*R-MMJ) * A0 (J) * B0 (MMJ)
                END DO
                B0 (M) = R * A0 (M) + BSUM / M
            END DO
 
            C (I) = B0 (I) / (I+1.0D+00)
 
            DSUM = 0.0
            DO J = 1, I - 1
                DSUM = DSUM + D (I-J) * C (J)
            END DO
            D (I) = - (DSUM+C(I))
 
        END DO
 
        J0 = E1 * ZNM1 + (N-1.0D+00) * J0
        J1 = E1 * ZN + N * J1
        ZNM1 = Z2 * ZNM1
        ZN = Z2 * ZN
        W = W0 * W
        T0 = D (N) * W * J0
        W = W0 * W
        T1 = D (NP1) * W * J1
        SUM1 = SUM1 + (T0+T1)
 
        IF ((ABS(T0)+ABS(T1)) <= EPS*SUM1) THEN
            U = EXP (-BCORR(A, B))
            BETA_ASYM = E0 * T * U * SUM1
            RETURN
        END IF
 
    END DO
 
    U = EXP (-BCORR(A, B))
    BETA_ASYM = E0 * T * U * SUM1
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_FRAC (A, B, X, Y, LAMBDA, EPS)
 
!*****************************************************************************80
!
!! BETA_FRAC evaluates a continued fraction expansion for IX(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that both A and
!    B are greater than 1.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) BETA_FRAC, the value of the continued
!    fraction approximation for IX(A,B).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ALPHA
    REAL (KIND=8) AN
    REAL (KIND=8) ANP1
    REAL (KIND=8) B
    REAL (KIND=8) BETA
    REAL (KIND=8) BETA_FRAC
    REAL (KIND=8) BN
    REAL (KIND=8) BNP1
    REAL (KIND=8) C
    REAL (KIND=8) C0
    REAL (KIND=8) C1
    REAL (KIND=8) E
    REAL (KIND=8) EPS
    REAL (KIND=8) LAMBDA
    REAL (KIND=8) N
    REAL (KIND=8) P
    REAL (KIND=8) R
    REAL (KIND=8) R0
    REAL (KIND=8) S
    REAL (KIND=8) T
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) YP1
 
    BETA_FRAC = BETA_RCOMP (A, B, X, Y)
 
    IF (BETA_FRAC == 0.0D+00) THEN
        RETURN
    END IF
 
    C = 1.0D+00 + LAMBDA
    C0 = B / A
    C1 = 1.0D+00 + 1.0D+00 / A
    YP1 = Y + 1.0D+00
 
    N = 0.0D+00
    P = 1.0D+00
    S = A + 1.0D+00
    AN = 0.0D+00
    BN = 1.0D+00
    ANP1 = 1.0D+00
    BNP1 = C / C1
    R = C1 / C
!
!  Continued fraction calculation.
!
    DO
 
        N = N + 1.0D+00
        T = N / A
        W = N * (B-N) * X
        E = A / S
        ALPHA = (P*(P+C0)*E*E) * (W*X)
        E = (1.0D+00+T) / (C1+T+T)
        BETA = N + W / S + E * (C+N*YP1)
        P = 1.0D+00 + T
        S = S + 2.0D+00
!
!  Update AN, BN, ANP1, and BNP1.
!
        T = ALPHA * AN + BETA * ANP1
        AN = ANP1
        ANP1 = T
        T = ALPHA * BN + BETA * BNP1
        BN = BNP1
        BNP1 = T
 
        R0 = R
        R = ANP1 / BNP1
 
        IF (ABS(R-R0) <= EPS*R) THEN
            BETA_FRAC = BETA_FRAC * R
            EXIT
        END IF
!
!  Rescale AN, BN, ANP1, and BNP1.
!
        AN = AN / BNP1
        BN = BN / BNP1
        ANP1 = R
        BNP1 = 1.0D+00
 
    END DO
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_GRAT (A, B, X, Y, W, EPS, IERR)
 
!*****************************************************************************80
!
!! BETA_GRAT evaluates an asymptotic expansion for IX(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that 15 <= A
!    and B <= 1, and that B is less than A.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Input/output, real ( kind = 8 ) W, a quantity to which the
!    result of the computation is to be added on output.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, integer ( kind = 4 ) IERR, an error flag, which is 0 if no error
!    was detected.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) BM1
    REAL (KIND=8) BP2N
    REAL (KIND=8) C (30)
    REAL (KIND=8) CN
    REAL (KIND=8) COEF
    REAL (KIND=8) D (30)
    REAL (KIND=8) DJ
    REAL (KIND=8) EPS
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IERR
    REAL (KIND=8) J
    REAL (KIND=8) L
    REAL (KIND=8) LNX
    INTEGER (KIND=4) N
    REAL (KIND=8) N2
    REAL (KIND=8) NU
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) R
    REAL (KIND=8) S
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8) T2
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    BM1 = (B-0.5D+00) - 0.5D+00
    NU = A + 0.5D+00 * BM1
 
    IF (Y <= 0.375D+00) THEN
        LNX = ALNREL (-Y)
    ELSE
        LNX = LOG (X)
    END IF
 
    Z = - NU * LNX
 
    IF (B*Z == 0.0D+00) THEN
        IERR = 1
        RETURN
    END IF
!
!  Computation of the expansion.
!
!  Set R = EXP(-Z)*Z^B/GAMMA(B)
!
    R = B * (1.0D+00+GAM1(B)) * EXP (B*LOG(Z))
    R = R * EXP (A*LNX) * EXP (0.5D+00*BM1*LNX)
    U = ALGDIV (B, A) + B * LOG (NU)
    U = R * EXP (-U)
 
    IF (U == 0.0D+00) THEN
        IERR = 1
        RETURN
    END IF
 
    CALL GAMMA_RAT1 (B, Z, R, P, Q, EPS)
 
    V = 0.25D+00 * (1.0D+00/NU) ** 2
    T2 = 0.25D+00 * LNX * LNX
    L = W / U
    J = Q / R
    SUM1 = J
    T = 1.0D+00
    CN = 1.0D+00
    N2 = 0.0D+00
 
    DO N = 1, 30
 
        BP2N = B + N2
        J = (BP2N*(BP2N+1.0D+00)*J+(Z+BP2N+1.0D+00)*T) * V
        N2 = N2 + 2.0D+00
        T = T * T2
        CN = CN / (N2*(N2+1.0D+00))
        C (N) = CN
        S = 0.0D+00
 
        COEF = B - N
        DO I = 1, N - 1
            S = S + COEF * C (I) * D (N-I)
            COEF = COEF + B
        END DO
 
        D (N) = BM1 * CN + S / N
        DJ = D (N) * J
        SUM1 = SUM1 + DJ
 
        IF (SUM1 <= 0.0D+00) THEN
            IERR = 1
            RETURN
        END IF
 
        IF (ABS(DJ) <= EPS*(SUM1+L)) THEN
            IERR = 0
            W = W + U * SUM1
            RETURN
        END IF
 
    END DO
 
    IERR = 0
    W = W + U * SUM1
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BETA_INC (A, B, X, Y, W, W1, IERR)
 
!*****************************************************************************80
!
!! BETA_INC evaluates the incomplete beta function IX(A,B).
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y.  X is the argument of the
!    function, and should satisy 0 <= X <= 1.  Y should equal 1 - X.
!
!    Output, real ( kind = 8 ) W, W1, the values of IX(A,B) and
!    1-IX(A,B).
!
!    Output, integer ( kind = 4 ) IERR, the error flag.
!    0, no error was detected.
!    1, A or B is negative;
!    2, A = B = 0;
!    3, X < 0 or 1 < X;
!    4, Y < 0 or 1 < Y;
!    5, X + Y /= 1;
!    6, X = A = 0;
!    7, Y = B = 0.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) EPS
    INTEGER (KIND=4) IERR
    INTEGER (KIND=4) IERR1
    INTEGER (KIND=4) IND
    REAL (KIND=8) LAMBDA
    INTEGER (KIND=4) N
    REAL (KIND=8) T
    REAL (KIND=8) W
    REAL (KIND=8) W1
    REAL (KIND=8) X
    REAL (KIND=8) X0
    REAL (KIND=8) Y
    REAL (KIND=8) Y0
    REAL (KIND=8) Z
 
    EPS = EPSILON (EPS)
    W = 0.0D+00
    W1 = 0.0D+00
 
    IF (A < 0.0D+00 .OR. B < 0.0D+00) THEN
        IERR = 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a,i8)') '  IERR = ', IERR
        RETURN
    END IF
 
    IF (A == 0.0D+00 .AND. B == 0.0D+00) THEN
        IERR = 2
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a,i8)') '  IERR = ', IERR
        RETURN
    END IF
 
    IF (X < 0.0D+00 .OR. 1.0D+00 < X) THEN
        IERR = 3
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a,i8)') '  IERR = ', IERR
        RETURN
    END IF
 
    IF (Y < 0.0D+00 .OR. 1.0D+00 < Y) THEN
        IERR = 4
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a,i8)') '  IERR = ', IERR
        RETURN
    END IF
 
    Z = ((X+Y)-0.5D+00) - 0.5D+00
 
    IF (3.0D+00*EPS < ABS(Z)) THEN
        IERR = 5
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'BETA_INC - Fatal error!'
        WRITE (*, '(a,i8)') '  IERR = ', IERR
        RETURN
    END IF
 
    IERR = 0
 
    IF (X == 0.0D+00) THEN
        W = 0.0D+00
        W1 = 1.0D+00
        IF (A == 0.0D+00) THEN
            IERR = 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'BETA_INC - Fatal error!'
            WRITE (*, '(a,i8)') '  IERR = ', IERR
        END IF
        RETURN
    END IF
 
    IF (Y == 0.0D+00) THEN
        IF (B == 0.0D+00) THEN
            IERR = 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'BETA_INC - Fatal error!'
            WRITE (*, '(a,i8)') '  IERR = ', IERR
            RETURN
        END IF
        W = 1.0D+00
        W1 = 0.0D+00
        RETURN
    END IF
 
    IF (A == 0.0D+00) THEN
        W = 1.0D+00
        W1 = 0.0D+00
        RETURN
    END IF
 
    IF (B == 0.0D+00) THEN
        W = 0.0D+00
        W1 = 1.0D+00
        RETURN
    END IF
 
    EPS = MAX (EPS, 1.0D-15)
 
    IF (MAX(A, B) < 0.001D+00*EPS) THEN
        GO TO 260
    END IF
 
    IND = 0
    A0 = A
    B0 = B
    X0 = X
    Y0 = Y
 
    IF (1.0D+00 < MIN(A0, B0)) THEN
        GO TO 40
    END IF
!
!  Procedure for A0 <= 1 or B0 <= 1
!
    IF (0.5D+00 < X) THEN
        IND = 1
        A0 = B
        B0 = A
        X0 = Y
        Y0 = X
    END IF
 
    IF (B0 < MIN(EPS, EPS*A0)) THEN
        GO TO 90
    END IF
 
    IF (A0 < MIN(EPS, EPS*B0) .AND. B0*X0 <= 1.0D+00) THEN
        GO TO 100
    END IF
 
    IF (1.0D+00 < MAX(A0, B0)) THEN
        GO TO 20
    END IF
 
    IF (MIN(0.2D+00, B0) <= A0) THEN
        GO TO 110
    END IF
 
    IF (X0**A0 <= 0.9D+00) THEN
        GO TO 110
    END IF
 
    IF (0.3D+00 <= X0) THEN
        GO TO 120
    END IF
 
    N = 20
    GO TO 140
 
20  CONTINUE
 
    IF (B0 <= 1.0D+00) THEN
        GO TO 110
    END IF
 
    IF (0.3D+00 <= X0) THEN
        GO TO 120
    END IF
 
    IF (0.1D+00 <= X0) THEN
        GO TO 30
    END IF
 
    IF ((X0*B0)**A0 <= 0.7D+00) THEN
        GO TO 110
    END IF
 
30  CONTINUE
 
    IF (15.0D+00 < B0) THEN
        GO TO 150
    END IF
 
    N = 20
    GO TO 140
!
!  PROCEDURE for 1 < A0 and 1 < B0.
!
40  CONTINUE
 
    IF (A <= B) THEN
        LAMBDA = A - (A+B) * X
    ELSE
        LAMBDA = (A+B) * Y - B
    END IF
 
    IF (LAMBDA < 0.0D+00) THEN
        IND = 1
        A0 = B
        B0 = A
        X0 = Y
        Y0 = X
        LAMBDA = ABS (LAMBDA)
    END IF
 
70  CONTINUE
 
    IF (B0 < 40.0D+00 .AND. B0*X0 <= 0.7D+00) THEN
        GO TO 110
    END IF
 
    IF (B0 < 40.0D+00) THEN
        GO TO 160
    END IF
 
    IF (B0 < A0) THEN
        GO TO 80
    END IF
 
    IF (A0 <= 100.0D+00) THEN
        GO TO 130
    END IF
 
    IF (0.03D+00*A0 < LAMBDA) THEN
        GO TO 130
    END IF
 
    GO TO 200
 
80  CONTINUE
 
    IF (B0 <= 100.0D+00) THEN
        GO TO 130
    END IF
 
    IF (0.03D+00*B0 < LAMBDA) THEN
        GO TO 130
    END IF
 
    GO TO 200
!
!  Evaluation of the appropriate algorithm.
!
90  CONTINUE
 
    W = FPSER (A0, B0, X0, EPS)
    W1 = 0.5D+00 + (0.5D+00-W)
    GO TO 250
 
100 CONTINUE
 
    W1 = APSER (A0, B0, X0, EPS)
    W = 0.5D+00 + (0.5D+00-W1)
    GO TO 250
 
110 CONTINUE
 
    W = BETA_PSER (A0, B0, X0, EPS)
    W1 = 0.5D+00 + (0.5D+00-W)
    GO TO 250
 
120 CONTINUE
 
    W1 = BETA_PSER (B0, A0, Y0, EPS)
    W = 0.5D+00 + (0.5D+00-W1)
    GO TO 250
 
130 CONTINUE
 
    W = BETA_FRAC (A0, B0, X0, Y0, LAMBDA, 15.0D+00*EPS)
    W1 = 0.5D+00 + (0.5D+00-W)
    GO TO 250
 
140 CONTINUE
 
    W1 = BETA_UP (B0, A0, Y0, X0, N, EPS)
    B0 = B0 + N
 
150 CONTINUE
 
    CALL BETA_GRAT (B0, A0, Y0, X0, W1, 15.0D+00*EPS, IERR1)
    W = 0.5D+00 + (0.5D+00-W1)
    GO TO 250
 
160 CONTINUE
 
    N = B0
    B0 = B0 - N
 
    IF (B0 == 0.0D+00) THEN
        N = N - 1
        B0 = 1.0D+00
    END IF
 
170 CONTINUE
 
    W = BETA_UP (B0, A0, Y0, X0, N, EPS)
 
    IF (X0 <= 0.7D+00) THEN
        W = W + BETA_PSER (A0, B0, X0, EPS)
        W1 = 0.5D+00 + (0.5D+00-W)
        GO TO 250
    END IF
 
    IF (A0 <= 15.0D+00) THEN
        N = 20
        W = W + BETA_UP (A0, B0, X0, Y0, N, EPS)
        A0 = A0 + N
    END IF
 
190 CONTINUE
 
    CALL BETA_GRAT (A0, B0, X0, Y0, W, 15.0D+00*EPS, IERR1)
    W1 = 0.5D+00 + (0.5D+00-W)
    GO TO 250
 
200 CONTINUE
 
    W = BETA_ASYM (A0, B0, LAMBDA, 100.0D+00*EPS)
    W1 = 0.5D+00 + (0.5D+00-W)
    GO TO 250
!
!  Termination of the procedure.
!
250 CONTINUE
 
    IF (IND /= 0) THEN
        T = W
        W = W1
        W1 = T
    END IF
 
    RETURN
!
!  Procedure for A and B < 0.001 * EPS
!
260 CONTINUE
 
    W = B / (A+B)
    W1 = A / (A+B)
 
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
!      BETA_INC(A,B,X) = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!                      / Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    Note that in Mathematica, the expressions:
!
!      BETA[A,B]   = Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!      BETA[X,A,B] = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!
!    and thus, to evaluate the incomplete Beta function requires:
!
!      BETA_INC(A,B,X) = BETA[X,A,B] / BETA[A,B]
!
!  Modified:
!
!    17 February 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 30
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.5D+00, 10.0D+00, 10.0D+00, 10.0D+00, 10.0D+00, 20.0D+00, &
   & 20.0D+00, 20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, 30.0D+00, 40.0D+00 /)
    REAL (KIND=8) B
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 2.0D+00, 2.0D+00, 5.0D+00, 0.5D+00, 5.0D+00, 5.0D+00, 10.0D+00, 5.0D+00, &
   & 10.0D+00, 10.0D+00, 20.0D+00, 20.0D+00, 10.0D+00, 10.0D+00, 20.0D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.0637686D+00, 0.2048328D+00, &
   & 1.0000000D+00, 0.0D+00, 0.0050126D+00, 0.0513167D+00, 0.2928932D+00, 0.5000000D+00, &
   & 0.028D+00, 0.104D+00, 0.216D+00, 0.352D+00, 0.500D+00, 0.648D+00, 0.784D+00, 0.896D+00, &
   & 0.972D+00, 0.4361909D+00, 0.1516409D+00, 0.0897827D+00, 1.0000000D+00, 0.5000000D+00, &
   & 0.4598773D+00, 0.2146816D+00, 0.9507365D+00, 0.5000000D+00, 0.8979414D+00, 0.2241297D+00, &
   & 0.7586405D+00, 0.7001783D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.01D+00, 0.10D+00, 1.00D+00, 0.0D+00, &
   & 0.01D+00, 0.10D+00, 0.50D+00, 0.50D+00, 0.1D+00, 0.2D+00, 0.3D+00, 0.4D+00, 0.5D+00, &
   & 0.6D+00, 0.7D+00, 0.8D+00, 0.9D+00, 0.50D+00, 0.90D+00, 0.50D+00, 1.00D+00, 0.50D+00, &
   & 0.80D+00, 0.60D+00, 0.80D+00, 0.50D+00, 0.60D+00, 0.70D+00, 0.80D+00, 0.70D+00 /)
 
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
 
FUNCTION BETA_LOG (A0, B0)
 
!*****************************************************************************80
!
!! BETA_LOG evaluates the logarithm of the beta function.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A0, B0, the parameters of the function.
!    A0 and B0 should be nonnegative.
!
!    Output, real ( kind = 8 ) BETA_LOG, the value of the logarithm
!    of the Beta function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) BETA_LOG
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: E = 0.918938533204673D+00
    REAL (KIND=8) H
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) W
    REAL (KIND=8) Z
 
    A = MIN (A0, B0)
    B = MAX (A0, B0)
!
!  8 < A.
!
    IF (8.0D+00 <= A) THEN
 
        W = BCORR (A, B)
        H = A / B
        C = H / (1.0D+00+H)
        U = - (A-0.5D+00) * LOG (C)
        V = B * ALNREL (H)
 
        IF (V < U) THEN
            BETA_LOG = (((-0.5D+00*LOG(B)+E)+W)-V) - U
        ELSE
            BETA_LOG = (((-0.5D+00*LOG(B)+E)+W)-U) - V
        END IF
 
        RETURN
    END IF
!
!  Procedure when A < 1
!
    IF (A < 1.0D+00) THEN
 
        IF (B < 8.0D+00) THEN
            BETA_LOG = GAMMA_LOG (A) + (GAMMA_LOG(B)-GAMMA_LOG(A+B))
        ELSE
            BETA_LOG = GAMMA_LOG (A) + ALGDIV (A, B)
        END IF
 
        RETURN
 
    END IF
!
!  Procedure when 1 <= A < 8
!
    IF (2.0D+00 < A) THEN
        GO TO 40
    END IF
 
    IF (B <= 2.0D+00) THEN
        BETA_LOG = GAMMA_LOG (A) + GAMMA_LOG (B) - GSUMLN (A, B)
        RETURN
    END IF
 
    W = 0.0D+00
 
    IF (B < 8.0D+00) THEN
        GO TO 60
    END IF
 
    BETA_LOG = GAMMA_LOG (A) + ALGDIV (A, B)
    RETURN
 
40  CONTINUE
!
!  Reduction of A when 1000 < B.
!
    IF (1000.0D+00 < B) THEN
 
        N = A - 1.0D+00
        W = 1.0D+00
        DO I = 1, N
            A = A - 1.0D+00
            W = W * (A/(1.0D+00+A/B))
        END DO
 
        BETA_LOG = (LOG(W)-N*LOG(B)) + (GAMMA_LOG(A)+ALGDIV(A, B))
 
        RETURN
    END IF
 
    N = A - 1.0D+00
    W = 1.0D+00
    DO I = 1, N
        A = A - 1.0D+00
        H = A / B
        W = W * (H/(1.0D+00+H))
    END DO
    W = LOG (W)
 
    IF (8.0D+00 <= B) THEN
        BETA_LOG = W + GAMMA_LOG (A) + ALGDIV (A, B)
        RETURN
    END IF
!
!  Reduction of B when B < 8.
!
60  CONTINUE
 
    N = B - 1.0D+00
    Z = 1.0D+00
    DO I = 1, N
        B = B - 1.0D+00
        Z = Z * (B/(A+B))
    END DO
 
    BETA_LOG = W + LOG (Z) + (GAMMA_LOG(A)+(GAMMA_LOG(B)-GSUMLN(A, B)))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_PSER (A, B, X, EPS)
 
!*****************************************************************************80
!
!! BETA_PSER uses a power series expansion to evaluate IX(A,B)(X).
!
!  Discussion:
!
!    BETA_PSER is used when B <= 1 or B*X <= 0.7.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters.
!
!    Input, real ( kind = 8 ) X, the point where the function
!    is to be evaluated.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_PSER, the approximate value of IX(A,B)(X).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) APB
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) BETA_PSER
    REAL (KIND=8) C
    REAL (KIND=8) EPS
    INTEGER (KIND=4) I
    INTEGER (KIND=4) M
    REAL (KIND=8) N
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8) TOL
    REAL (KIND=8) U
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Z
 
    BETA_PSER = 0.0D+00
 
    IF (X == 0.0D+00) THEN
        RETURN
    END IF
!
!  Compute the factor X**A/(A*BETA(A,B))
!
    A0 = MIN (A, B)
 
    IF (1.0D+00 <= A0) THEN
 
        Z = A * LOG (X) - BETA_LOG (A, B)
        BETA_PSER = EXP (Z) / A
 
    ELSE
 
        B0 = MAX (A, B)
 
        IF (B0 <= 1.0D+00) THEN
 
            BETA_PSER = X ** A
            IF (BETA_PSER == 0.0D+00) THEN
                RETURN
            END IF
 
            APB = A + B
 
            IF (APB <= 1.0D+00) THEN
                Z = 1.0D+00 + GAM1 (APB)
            ELSE
                U = A + B - 1.0D+00
                Z = (1.0D+00+GAM1(U)) / APB
            END IF
 
            C = (1.0D+00+GAM1(A)) * (1.0D+00+GAM1(B)) / Z
            BETA_PSER = BETA_PSER * C * (B/APB)
 
        ELSE IF (B0 < 8.0D+00) THEN
 
            U = GAMMA_LN1 (A0)
            M = B0 - 1.0D+00
 
            C = 1.0D+00
            DO I = 1, M
                B0 = B0 - 1.0D+00
                C = C * (B0/(A0+B0))
            END DO
 
            U = LOG (C) + U
            Z = A * LOG (X) - U
            B0 = B0 - 1.0D+00
            APB = A0 + B0
 
            IF (APB <= 1.0D+00) THEN
                T = 1.0D+00 + GAM1 (APB)
            ELSE
                U = A0 + B0 - 1.0D+00
                T = (1.0D+00+GAM1(U)) / APB
            END IF
 
            BETA_PSER = EXP (Z) * (A0/A) * (1.0D+00+GAM1(B0)) / T
 
        ELSE IF (8.0D+00 <= B0) THEN
 
            U = GAMMA_LN1 (A0) + ALGDIV (A0, B0)
            Z = A * LOG (X) - U
            BETA_PSER = (A0/A) * EXP (Z)
 
        END IF
 
    END IF
 
    IF (BETA_PSER == 0.0D+00 .OR. A <= 0.1D+00*EPS) THEN
        RETURN
    END IF
!
!  Compute the series.
!
    SUM1 = 0.0D+00
    N = 0.0D+00
    C = 1.0D+00
    TOL = EPS / A
 
    DO
 
        N = N + 1.0D+00
        C = C * (0.5D+00+(0.5D+00-B/N)) * X
        W = C / (A+N)
        SUM1 = SUM1 + W
 
        IF (ABS(W) <= TOL) THEN
            EXIT
        END IF
 
    END DO
 
    BETA_PSER = BETA_PSER * (1.0D+00+A*SUM1)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_RCOMP (A, B, X, Y)
 
!*****************************************************************************80
!
!! BETA_RCOMP evaluates X^A * Y^B / Beta(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, define the numerator of the fraction.
!
!    Output, real ( kind = 8 ) BETA_RCOMP, the value of X^A * Y^B / Beta(A,B).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) APB
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) BETA_RCOMP
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: CONST = 0.398942280401433D+00
    REAL (KIND=8) E
    REAL (KIND=8) H
    INTEGER (KIND=4) I
    REAL (KIND=8) LAMBDA
    REAL (KIND=8) LNX
    REAL (KIND=8) LNY
    INTEGER (KIND=4) N
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) X
    REAL (KIND=8) X0
    REAL (KIND=8) Y
    REAL (KIND=8) Y0
    REAL (KIND=8) Z
 
    BETA_RCOMP = 0.0D+00
    IF (X == 0.0D+00 .OR. Y == 0.0D+00) THEN
        RETURN
    END IF
 
    A0 = MIN (A, B)
 
    IF (A0 < 8.0D+00) THEN
 
        IF (X <= 0.375D+00) THEN
            LNX = LOG (X)
            LNY = ALNREL (-X)
        ELSE IF (Y <= 0.375D+00) THEN
            LNX = ALNREL (-Y)
            LNY = LOG (Y)
        ELSE
            LNX = LOG (X)
            LNY = LOG (Y)
        END IF
 
        Z = A * LNX + B * LNY
 
        IF (1.0D+00 <= A0) THEN
            Z = Z - BETA_LOG (A, B)
            BETA_RCOMP = EXP (Z)
            RETURN
        END IF
!
!  Procedure for A < 1 or B < 1
!
        B0 = MAX (A, B)
 
        IF (B0 <= 1.0D+00) THEN
 
            BETA_RCOMP = EXP (Z)
            IF (BETA_RCOMP == 0.0D+00) THEN
                RETURN
            END IF
 
            APB = A + B
 
            IF (APB <= 1.0D+00) THEN
                Z = 1.0D+00 + GAM1 (APB)
            ELSE
                U = A + B - 1.0D+00
                Z = (1.0D+00+GAM1(U)) / APB
            END IF
 
            C = (1.0D+00+GAM1(A)) * (1.0D+00+GAM1(B)) / Z
            BETA_RCOMP = BETA_RCOMP * (A0*C) / (1.0D+00+A0/B0)
 
        ELSE IF (B0 < 8.0D+00) THEN
 
            U = GAMMA_LN1 (A0)
            N = B0 - 1.0D+00
 
            C = 1.0D+00
            DO I = 1, N
                B0 = B0 - 1.0D+00
                C = C * (B0/(A0+B0))
            END DO
            U = LOG (C) + U
 
            Z = Z - U
            B0 = B0 - 1.0D+00
            APB = A0 + B0
 
            IF (APB <= 1.0D+00) THEN
                T = 1.0D+00 + GAM1 (APB)
            ELSE
                U = A0 + B0 - 1.0D+00
                T = (1.0D+00+GAM1(U)) / APB
            END IF
 
            BETA_RCOMP = A0 * EXP (Z) * (1.0D+00+GAM1(B0)) / T
 
        ELSE IF (8.0D+00 <= B0) THEN
 
            U = GAMMA_LN1 (A0) + ALGDIV (A0, B0)
            BETA_RCOMP = A0 * EXP (Z-U)
 
        END IF
 
    ELSE
 
        IF (A <= B) THEN
            H = A / B
            X0 = H / (1.0D+00+H)
            Y0 = 1.0D+00 / (1.0D+00+H)
            LAMBDA = A - (A+B) * X
        ELSE
            H = B / A
            X0 = 1.0D+00 / (1.0D+00+H)
            Y0 = H / (1.0D+00+H)
            LAMBDA = (A+B) * Y - B
        END IF
 
        E = - LAMBDA / A
 
        IF (ABS(E) <= 0.6D+00) THEN
            U = RLOG1 (E)
        ELSE
            U = E - LOG (X/X0)
        END IF
 
        E = LAMBDA / B
 
        IF (ABS(E) <= 0.6D+00) THEN
            V = RLOG1 (E)
        ELSE
            V = E - LOG (Y/Y0)
        END IF
 
        Z = EXP (-(A*U+B*V))
        BETA_RCOMP = CONST * SQRT (B*X0) * Z * EXP (-BCORR(A, B))
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_RCOMP1 (MU, A, B, X, Y)
 
!*****************************************************************************80
!
!! BETA_RCOMP1 evaluates exp(MU) * X^A * Y^B / Beta(A,B).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MU, ?
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, quantities whose powers form part of
!    the expression.
!
!    Output, real ( kind = 8 ) BETA_RCOMP1, the value of
!    exp(MU) * X**A * Y**B / Beta(A,B).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A0
    REAL (KIND=8) APB
    REAL (KIND=8) B
    REAL (KIND=8) B0
    REAL (KIND=8) BETA_RCOMP1
    REAL (KIND=8) C
    REAL (KIND=8), PARAMETER :: CONST = 0.398942280401433D+00
    REAL (KIND=8) E
    REAL (KIND=8) H
    INTEGER (KIND=4) I
    REAL (KIND=8) LAMBDA
    REAL (KIND=8) LNX
    REAL (KIND=8) LNY
    INTEGER (KIND=4) MU
    INTEGER (KIND=4) N
    REAL (KIND=8) T
    REAL (KIND=8) U
    REAL (KIND=8) V
    REAL (KIND=8) X
    REAL (KIND=8) X0
    REAL (KIND=8) Y
    REAL (KIND=8) Y0
    REAL (KIND=8) Z
 
    A0 = MIN (A, B)
!
!  Procedure for 8 <= A and 8 <= B.
!
    IF (8.0D+00 <= A0) THEN
 
        IF (A <= B) THEN
            H = A / B
            X0 = H / (1.0D+00+H)
            Y0 = 1.0D+00 / (1.0D+00+H)
            LAMBDA = A - (A+B) * X
        ELSE
            H = B / A
            X0 = 1.0D+00 / (1.0D+00+H)
            Y0 = H / (1.0D+00+H)
            LAMBDA = (A+B) * Y - B
        END IF
 
        E = - LAMBDA / A
 
        IF (ABS(E) <= 0.6D+00) THEN
            U = RLOG1 (E)
        ELSE
            U = E - LOG (X/X0)
        END IF
 
        E = LAMBDA / B
 
        IF (ABS(E) <= 0.6D+00) THEN
            V = RLOG1 (E)
        ELSE
            V = E - LOG (Y/Y0)
        END IF
 
        Z = ESUM (MU,-(A*U+B*V))
        BETA_RCOMP1 = CONST * SQRT (B*X0) * Z * EXP (-BCORR(A, B))
!
!  Procedure for A < 8 or B < 8.
!
    ELSE
 
        IF (X <= 0.375D+00) THEN
            LNX = LOG (X)
            LNY = ALNREL (-X)
        ELSE IF (Y <= 0.375D+00) THEN
            LNX = ALNREL (-Y)
            LNY = LOG (Y)
        ELSE
            LNX = LOG (X)
            LNY = LOG (Y)
        END IF
 
        Z = A * LNX + B * LNY
 
        IF (1.0D+00 <= A0) THEN
            Z = Z - BETA_LOG (A, B)
            BETA_RCOMP1 = ESUM (MU, Z)
            RETURN
        END IF
!
!  Procedure for A < 1 or B < 1.
!
        B0 = MAX (A, B)
 
        IF (8.0D+00 <= B0) THEN
            U = GAMMA_LN1 (A0) + ALGDIV (A0, B0)
            BETA_RCOMP1 = A0 * ESUM (MU, Z-U)
            RETURN
        END IF
 
        IF (1.0D+00 < B0) THEN
!
!  Algorithm for 1 < B0 < 8
!
            U = GAMMA_LN1 (A0)
            N = B0 - 1.0D+00
 
            C = 1.0D+00
            DO I = 1, N
                B0 = B0 - 1.0D+00
                C = C * (B0/(A0+B0))
            END DO
            U = LOG (C) + U
 
            Z = Z - U
            B0 = B0 - 1.0D+00
            APB = A0 + B0
 
            IF (APB <= 1.0D+00) THEN
                T = 1.0D+00 + GAM1 (APB)
            ELSE
                U = A0 + B0 - 1.0D+00
                T = (1.0D+00+GAM1(U)) / APB
            END IF
 
            BETA_RCOMP1 = A0 * ESUM (MU, Z) * (1.0D+00+GAM1(B0)) / T
!
!  Algorithm for B0 <= 1
!
        ELSE
 
            BETA_RCOMP1 = ESUM (MU, Z)
            IF (BETA_RCOMP1 == 0.0D+00) THEN
                RETURN
            END IF
 
            APB = A + B
 
            IF (APB <= 1.0D+00) THEN
                Z = 1.0D+00 + GAM1 (APB)
            ELSE
                U = REAL (A, KIND=8) + REAL (B, KIND=8) - 1.0D+00
                Z = (1.0D+00+GAM1(U)) / APB
            END IF
 
            C = (1.0D+00+GAM1(A)) * (1.0D+00+GAM1(B)) / Z
            BETA_RCOMP1 = BETA_RCOMP1 * (A0*C) / (1.0D+00+A0/B0)
 
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION BETA_UP (A, B, X, Y, N, EPS)
 
!*****************************************************************************80
!
!! BETA_UP evaluates IX(A,B) - IX(A+N,B) where N is a positive integer.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.
!
!    Input, real ( kind = 8 ) X, Y, ?
!
!    Input, integer ( kind = 4 ) N, the increment to the first argument of IX.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
!    Output, real ( kind = 8 ) BETA_UP, the value of IX(A,B) - IX(A+N,B).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) AP1
    REAL (KIND=8) APB
    REAL (KIND=8) B
    REAL (KIND=8) BETA_UP
    REAL (KIND=8) D
    REAL (KIND=8) EPS
    INTEGER (KIND=4) I
    INTEGER (KIND=4) K
    REAL (KIND=8) L
    INTEGER (KIND=4) MU
    INTEGER (KIND=4) N
    REAL (KIND=8) R
    REAL (KIND=8) T
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Y
!
!  Obtain the scaling factor EXP(-MU) AND
!  EXP(MU)*(X**A*Y**B/BETA(A,B))/A
!
    APB = A + B
    AP1 = A + 1.0D+00
    MU = 0
    D = 1.0D+00
 
    IF (N /= 1) THEN
 
        IF (1.0D+00 <= A) THEN
 
            IF (1.1D+00*AP1 <= APB) THEN
                MU = ABS (EXPARG(1))
                K = EXPARG (0)
                IF (K < MU) THEN
                    MU = K
                END IF
                T = MU
                D = EXP (-T)
            END IF
 
        END IF
 
    END IF
 
    BETA_UP = BETA_RCOMP1 (MU, A, B, X, Y) / A
 
    IF (N == 1 .OR. BETA_UP == 0.0D+00) THEN
        RETURN
    END IF
 
    W = D
!
!  Let K be the index of the maximum term.
!
    K = 0
 
    IF (1.0D+00 < B) THEN
 
        IF (Y <= 0.0001D+00) THEN
 
            K = N - 1
 
        ELSE
 
            R = (B-1.0D+00) * X / Y - A
 
            IF (1.0D+00 <= R) THEN
                K = N - 1
                T = N - 1
                IF (R < T) THEN
                    K = R
                END IF
            END IF
 
        END IF
!
!  Add the increasing terms of the series.
!
        DO I = 1, K
            L = I - 1
            D = ((APB+L)/(AP1+L)) * X * D
            W = W + D
        END DO
 
    END IF
!
!  Add the remaining terms of the series.
!
    DO I = K + 1, N - 1
        L = I - 1
        D = ((APB+L)/(AP1+L)) * X * D
        W = W + D
        IF (D <= EPS*W) THEN
            BETA_UP = BETA_UP * W
            RETURN
        END IF
    END DO
 
    BETA_UP = BETA_UP * W
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE BINOMIAL_CDF_VALUES (N_DATA, A, B, X, FX)
 
!*****************************************************************************80
!
!! BINOMIAL_CDF_VALUES returns some values of the binomial CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) B, integer X, the
!    arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 17
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 2, 2, 2, 2, 2, 4, 4, 4, 4, 10, 10, &
   & 10, 10, 10, 10, 10, 10 /)
    REAL (KIND=8) B
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 0.05D+00, 0.05D+00, 0.05D+00, &
   & 0.50D+00, 0.50D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.25D+00, 0.05D+00, 0.10D+00, 0.15D+00, &
   & 0.20D+00, 0.25D+00, 0.30D+00, 0.40D+00, 0.50D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.9025D+00, 0.9975D+00, 1.0000D+00, &
   & 0.2500D+00, 0.7500D+00, 0.3164D+00, 0.7383D+00, 0.9492D+00, 0.9961D+00, 0.9999D+00, &
   & 0.9984D+00, 0.9901D+00, 0.9672D+00, 0.9219D+00, 0.8497D+00, 0.6331D+00, 0.3770D+00 /)
    INTEGER (KIND=4) N_DATA
    INTEGER (KIND=4) X
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0, 1, 2, 0, 1, 0, 1, 2, 3, 4, 4, 4, &
   & 4, 4, 4, 4, 4 /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0
        B = 0.0D+00
        X = 0
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
 
SUBROUTINE CDFBET (WHICH, P, Q, X, Y, A, B, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFBET evaluates the CDF of the Beta Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the beta distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly by code associated with the reference.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The beta density is proportional to t^(A-1) * (1-t)^(B-1).
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which of the next four
!    argument values is to be calculated from the others.
!    1: Calculate P and Q from X, Y, A and B;
!    2: Calculate X and Y from P, Q, A and B;
!    3: Calculate A from P, Q, X, Y and B;
!    4: Calculate B from P, Q, X, Y and A.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    chi-square distribution.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) Q, equals 1-P.  Input range: [0, 1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration
!    of the beta density.  If it is an input value, it should lie in
!    the range [0,1].  If it is an output value, it will be searched for
!    in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Y, equal to 1-X.  If it is an input
!    value, it should lie in the range [0,1].  If it is an output value,
!    it will be searched for in the range [0,1].
!
!    Input/output, real ( kind = 8 ) A, the first parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Input/output, real ( kind = 8 ) B, the second parameter of the beta
!    density.  If it is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched
!    for in the range [1D-300,1D300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if X + Y /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) B
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) X
    REAL (KIND=8) XHI
    REAL (KIND=8) XLO
    REAL (KIND=8) Y
 
    STATUS = 0
    BOUND = 0.0D+00
 
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFBET - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFBET - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 2
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            BOUND = 1.0D+00
            STATUS = - 2
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            BOUND = 1.0D+00
            STATUS = - 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless X is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (X < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter X is out of range.'
            RETURN
        ELSE IF (1.0D+00 < X) THEN
            BOUND = 1.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter X is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Y is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (Y < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Y is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Y) THEN
            BOUND = 1.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Y is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless A is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (A <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter A is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless B is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (B <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter B is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Check that X + Y = 1.
!
    IF (WHICH /= 2) THEN
        IF (3.0D+00*EPSILON(X) < ABS((X+Y)-1.0D+00)) THEN
            STATUS = 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  X + Y /= 1.'
            RETURN
        END IF
    END IF
!
!  Compute P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMBET (X, Y, A, B, P, Q)
        STATUS = 0
!
!  Compute X and Y.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTZR (0.0D+00, 1.0D+00, ATOL, TOL)
 
        IF (P <= Q) THEN
 
            STATUS = 0
            FX = 0.0D+00
            CALL DZROR (STATUS, X, FX, XLO, XHI, QLEFT, QHI)
            Y = 1.0D+00 - X
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMBET (X, Y, A, B, CUM, CCUM)
                FX = CUM - P
                CALL DZROR (STATUS, X, FX, XLO, XHI, QLEFT, QHI)
                Y = 1.0D+00 - X
 
            END DO
 
        ELSE
 
            STATUS = 0
            FX = 0.0D+00
            CALL DZROR (STATUS, Y, FX, XLO, XHI, QLEFT, QHI)
            X = 1.0D+00 - Y
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMBET (X, Y, A, B, CUM, CCUM)
                FX = CCUM - Q
                CALL DZROR (STATUS, Y, FX, XLO, XHI, QLEFT, QHI)
                X = 1.0D+00 - Y
 
            END DO
 
        END IF
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = 1.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Compute A.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        A = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, A, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMBET (X, Y, A, B, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, A, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
 
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
 
        END IF
!
!  Compute B.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        B = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, B, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMBET (X, Y, A, B, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, B, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBET - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFBIN (WHICH, P, Q, S, XN, PR, OMPR, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFBIN evaluates the CDF of the Binomial distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the binomial distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    P is the probablility of S or fewer successes in XN binomial trials,
!    each trial having an individual probability of success of PR.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.24.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which of argument values is to
!    be calculated from the others.
!    1: Calculate P and Q from S, XN, PR and OMPR;
!    2: Calculate S from P, Q, XN, PR and OMPR;
!    3: Calculate XN from P, Q, S, PR and OMPR;
!    4: Calculate PR and OMPR from P, Q, S and XN.
!
!    Input/output, real ( kind = 8 ) P, the cumulation, from 0 to S,
!    of the binomial distribution.  If P is an input value, it should
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) S, the number of successes observed.
!    Whether this is an input or output value, it should lie in the
!    range [0,XN].
!
!    Input/output, real ( kind = 8 ) XN, the number of binomial trials.
!    If this is an input value it should lie in the range: (0, +infinity).
!    If it is an output value it will be searched for in the
!    range [1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PR, the probability of success in each
!    binomial trial.  Whether this is an input or output value, it should
!    lie in the range: [0,1].
!
!    Input/output, real ( kind = 8 ) OMPR, equal to 1-PR.  Whether this is an
!    input or output value, it should lie in the range [0,1].  Also, it should
!    be the case that PR + OMPR = 1.
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if PR + OMPR /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) OMPR
    REAL (KIND=8) P
    REAL (KIND=8) PR
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    REAL (KIND=8) S
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) XHI
    REAL (KIND=8) XLO
    REAL (KIND=8) XN
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFBET - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFBET - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless XN is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (XN <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter XN is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless S is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (S < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter S is out of range.'
            RETURN
        ELSE IF (WHICH /= 3 .AND. XN < S) THEN
            BOUND = XN
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter S is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless PR is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (PR < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PR is out of range.'
            RETURN
        ELSE IF (1.0D+00 < PR) THEN
            BOUND = 1.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PR is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless OMPR is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (OMPR < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter OMPR is out of range.'
            RETURN
        ELSE IF (1.0D+00 < OMPR) THEN
            BOUND = 1.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  Input parameter OMPR is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Check that PR + OMPR = 1.
!
    IF (WHICH /= 4) THEN
        IF (3.0D+00*EPSILON(1.0D+00) < ABS((PR+OMPR)-1.0D+00)) THEN
            STATUS = 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFBET - Fatal error!'
            WRITE (*, '(a)') '  PR + OMPR /= 1.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMBIN (S, XN, PR, OMPR, P, Q)
        STATUS = 0
!
!  Calculate S.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, XN, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        S = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMBIN (S, XN, PR, OMPR, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = XN
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate XN.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        XN = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, XN, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMBIN (S, XN, PR, OMPR, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, XN, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
                RETURN
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
                RETURN
            END IF
        END IF
!
!  Calculate PR and OMPR.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL DSTZR (0.0D+00, 1.0D+00, ATOL, TOL)
 
        IF (P <= Q) THEN
 
            STATUS = 0
            CALL DZROR (STATUS, PR, FX, XLO, XHI, QLEFT, QHI)
            OMPR = 1.0D+00 - PR
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMBIN (S, XN, PR, OMPR, CUM, CCUM)
                FX = CUM - P
                CALL DZROR (STATUS, PR, FX, XLO, XHI, QLEFT, QHI)
                OMPR = 1.0D+00 - PR
 
            END DO
 
        ELSE
 
            STATUS = 0
            CALL DZROR (STATUS, OMPR, FX, XLO, XHI, QLEFT, QHI)
            PR = 1.0D+00 - OMPR
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMBIN (S, XN, PR, OMPR, CUM, CCUM)
                FX = CCUM - Q
                CALL DZROR (STATUS, OMPR, FX, XLO, XHI, QLEFT, QHI)
                PR = 1.0D+00 - OMPR
 
            END DO
 
        END IF
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = 1.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFBIN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFCHI (WHICH, P, Q, X, DF, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFCHI evaluates the CDF of the chi square distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the chi square distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The CDF of the chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ ChiSquareDistribution [ DF ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.19.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X and DF;
!    2: Calculate X from P, Q and DF;
!    3: Calculate DF from P, Q and X.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of
!    the chi-square distribution.  If this is an input value, it should
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration
!    of the chi-square distribution.  If this is an input
!    value, it should lie in the range: [0, +infinity).  If it is an output
!    value, it will be searched for in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DF, the degrees of freedom of the
!    chi-square distribution.  If this is an input value, it should lie
!    in the range: (0, +infinity).  If it is an output value, it will be
!    searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +10, an error was returned from CUMGAM.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) PORQ
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) X
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFCHI - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
 
    IF (3 < WHICH) THEN
        BOUND = 3.0
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFCHI - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless X is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (X < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter X is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DF is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (DF <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DF is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHI - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Select the minimum of P and Q.
!
    IF (WHICH /= 1) THEN
        PORQ = MIN (P, Q)
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        STATUS = 0
        CALL CUMCHI (X, DF, P, Q)
 
        IF (1.5D+00 < PORQ) THEN
            STATUS = 10
            RETURN
        END IF
!
!  Calculate X.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        X = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, X, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMCHI (X, DF, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            IF (1.5D+00 < FX+PORQ) THEN
                STATUS = 10
                RETURN
            END IF
 
            CALL DINVR (STATUS, X, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
 
        END IF
!
!  Calculate DF.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DF = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMCHI (X, DF, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            IF (1.5D+00 < FX+PORQ) THEN
                STATUS = 10
                RETURN
            END IF
 
            CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFCHN (WHICH, P, Q, X, DF, PNONC, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFCHN evaluates the CDF of the Noncentral Chi-Square.
!
!  Discussion:
!
!    This routine calculates any one parameter of the noncentral chi-square
!    distribution given values for the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter (PNONC).  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.25.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, DF and PNONC;
!    2: Calculate X from P, DF and PNONC;
!    3: Calculate DF from P, X and PNONC;
!    4: Calculate PNONC from P, X and DF.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of
!    the noncentral chi-square distribution.  If this is an input
!    value, it should lie in the range: [0, 1.0-1.0D-16).
!
!    Input/output, real ( kind = 8 ) Q, is generally not used by this
!    subroutine and is only included for similarity with other routines.
!    However, if P is to be computed, then a value will also be computed
!    for Q.
!
!    Input, real ( kind = 8 ) X, the upper limit of integration of the
!    noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be sought in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DF, the number of degrees of freedom
!    of the noncentral chi-square distribution.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.  If this is an input value, it
!    should lie in the range: [0, +infinity).  If it is an output value,
!    it will be searched for in the range: [0,1.0D+4]
!
!    Output, integer ( kind = 4 ) STATUS, reports on the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if the answer appears to be lower than the lowest search bound;
!    2, if the answer appears to be higher than the greatest search bound.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-50
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) PNONC
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TENT4 = 1.0D+04
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) X
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFCHN - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFCHN - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless X is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (X < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter X is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DF is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (DF <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DF is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless PNONC is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (PNONC < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFCHN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PNONC is out of range.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMCHN (X, DF, PNONC, P, Q)
        STATUS = 0
!
!  Calculate X.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        X = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, X, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMCHN (X, DF, PNONC, CUM, CCUM)
            FX = CUM - P
            CALL DINVR (STATUS, X, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate DF.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DF = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMCHN (X, DF, PNONC, CUM, CCUM)
            FX = CUM - P
            CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate PNONC.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL DSTINV (0.0D+00, TENT4, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        PNONC = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, PNONC, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMCHN (X, DF, PNONC, CUM, CCUM)
            FX = CUM - P
            CALL DINVR (STATUS, PNONC, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = TENT4
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFCHN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFF (WHICH, P, Q, F, DFN, DFD, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFF evaluates the CDF of the F distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The value of the cumulative F distribution is not necessarily
!    monotone in either degrees of freedom.  There thus may be two
!    values that provide a given CDF value.  This routine assumes
!    monotonicity and will find an arbitrary one of the two values.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.6.2.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN and DFD;
!    2: Calculate F from P, Q, DFN and DFD;
!    3: Calculate DFN from P, Q, F and DFD;
!    4: Calculate DFD from P, Q, F and DFN.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the F-density.  If it is an input value, it should lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the F-density.  If this is an input value, it should lie in the
!    range [0, +infinity).  If it is an output value, it will be searched
!    for in the range [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of
!    freedom of the numerator sum of squares.  If this is an input value,
!    it should lie in the range: (0, +infinity).  If it is an output value,
!    it will be searched for in the range: [ 1.0D-300, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the  range: [ 1.0D-300, 1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) BOUND_HI
    REAL (KIND=8) BOUND_LO
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DFD
    REAL (KIND=8) DFN
    REAL (KIND=8) F
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFF - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFF - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless F is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (F < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter F is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DFN is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (DFN <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DFN is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DFD is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (DFD <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DFD is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFF - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMF (F, DFN, DFD, P, Q)
        STATUS = 0
!
!  Calculate F.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        F = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMF (F, DFN, DFD, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
 
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
 
        END IF
!
!  Calculate DFN.
!
!  Note that, in the original calculation, the lower bound for DFN was 0.
!  Using DFN = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
    ELSE IF (WHICH == 3) THEN
 
        BOUND_LO = 1.0D+00
        BOUND_HI = INF
 
        CALL DSTINV (BOUND_LO, BOUND_HI, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DFN = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DFN, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMF (F, DFN, DFD, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, DFN, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
 
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = BOUND_LO
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND_LO
                RETURN
            ELSE
                STATUS = 2
                BOUND = BOUND_HI
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND_HI
                RETURN
            END IF
 
        END IF
!
!  Calculate DFD.
!
!  Note that, in the original calculation, the lower bound for DFD was 0.
!  Using DFD = 0 causes an error in CUMF when it calls BETA_INC.
!  The lower bound was set to the more reasonable value of 1.
!  JVB, 14 April 2007.
!
    ELSE IF (WHICH == 4) THEN
 
        BOUND_LO = 1.0D+00
        BOUND_HI = INF
 
        CALL DSTINV (BOUND_LO, BOUND_HI, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DFD = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DFD, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMF (F, DFN, DFD, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, DFD, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = BOUND_LO
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND_LO
            ELSE
                STATUS = 2
                BOUND = BOUND_HI
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFF - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND_HI
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFFNC (WHICH, P, Q, F, DFN, DFD, PNONC, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFFNC evaluates the CDF of the Noncentral F distribution.
!
!  Discussion:
!
!    This routine originally used 1.0D+300 as the upper bound for the
!    interval in which many of the missing parameters are to be sought.
!    Since the underlying rootfinder routine needs to evaluate the
!    function at this point, it is no surprise that the program was
!    experiencing overflows.  A less extravagant upper bound
!    is being tried for now!
!
!    This routine calculates any one parameter of the Noncentral F distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The computation time required for this routine is proportional
!    to the noncentrality parameter PNONC.  Very large values of
!    this parameter can consume immense computer resources.  This is
!    why the search range is bounded by 10,000.
!
!    The value of the cumulative noncentral F distribution is not
!    necessarily monotone in either degree of freedom.  There thus
!    may be two values that provide a given CDF value.  This routine
!    assumes monotonicity and will find an arbitrary one of the two
!    values.
!
!    The CDF of the noncentral F distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralFRatioDistribution [ DFN, DFD, PNONC ], X ]
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.6.20.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, DFN, DFD and PNONC;
!    2: Calculate F from P, Q, DFN, DFD and PNONC;
!    3: Calculate DFN from P, Q, F, DFD and PNONC;
!    4: Calculate DFD from P, Q, F, DFN and PNONC;
!    5: Calculate PNONC from P, Q, F, DFN and DFD.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to F of
!    the noncentral F-density.  If P is an input value it should
!    lie in the range [0,1) (Not including 1!).
!
!    Dummy, real ( kind = 8 ) Q, is not used by this subroutine,
!    and is only included for similarity with the other routines.
!    Its input value is not checked.  If P is to be computed, the
!    Q is set to 1 - P.
!
!    Input/output, real ( kind = 8 ) F, the upper limit of integration
!    of the noncentral F-density.  If this is an input value, it should
!    lie in the range: [0, +infinity).  If it is an output value, it
!    will be searched for in the range: [0,1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFN, the number of degrees of freedom
!    of the numerator sum of squares.  If this is an input value, it should
!    lie in the range: (0, +infinity).  If it is an output value, it will
!    be searched for in the range: [ 1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) DFD, the number of degrees of freedom
!    of the denominator sum of squares.  If this is an input value, it should
!    be in range: (0, +infinity).  If it is an output value, it will be
!    searched for in the range [1.0, 1.0D+30].
!
!    Input/output, real ( kind = 8 ) PNONC, the noncentrality parameter
!    If this is an input value, it should be nonnegative.
!    If it is an output value, it will be searched for in the range: [0,1.0D+4].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DFD
    REAL (KIND=8) DFN
    REAL (KIND=8) F
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+30
    REAL (KIND=8) P
    REAL (KIND=8) PNONC
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TENT4 = 1.0D+04
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFFNC - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 5.'
        RETURN
    END IF
 
    IF (5 < WHICH) THEN
        BOUND = 5.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFFNC - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 5.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless F is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (F < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter F is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DFN is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (DFN <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DFN is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DFD is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (DFD <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DFD is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless PNONC is to be computed, make sure it is legal.
!
    IF (WHICH /= 5) THEN
        IF (PNONC < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFFNC - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PNONC is out of range.'
            RETURN
        END IF
    END IF
!
! Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMFNC (F, DFN, DFD, PNONC, P, Q)
        STATUS = 0
!
!  Calculate F.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        F = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMFNC (F, DFN, DFD, PNONC, CUM, CCUM)
            FX = CUM - P
            CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
                RETURN
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
                RETURN
            END IF
        END IF
!
!  Calculate DFN.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (1.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DFN = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DFN, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMFNC (F, DFN, DFD, PNONC, CUM, CCUM)
            FX = CUM - P
 
            CALL DINVR (STATUS, DFN, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate DFD.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL DSTINV (1.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DFD = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DFD, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMFNC (F, DFN, DFD, PNONC, CUM, CCUM)
            FX = CUM - P
            CALL DINVR (STATUS, DFD, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate PNONC.
!
    ELSE IF (WHICH == 5) THEN
 
        CALL DSTINV (0.0D+00, TENT4, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        PNONC = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, PNONC, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMFNC (F, DFN, DFD, PNONC, CUM, CCUM)
            FX = CUM - P
 
            CALL DINVR (STATUS, PNONC, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = TENT4
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFFNC - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFGAM (WHICH, P, Q, X, SHAPE, SCALE, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFGAM evaluates the CDF of the Gamma Distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Gamma distribution
!    given the others.
!
!    The cumulative distribution function P is calculated directly.
!
!    Computation of the other parameters involves a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The gamma density is proportional to T**(SHAPE - 1) * EXP(- SCALE * T)
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 654:
!    Computation of the incomplete gamma function ratios and their inverse,
!    ACM Transactions on Mathematical Software,
!    Volume 12, 1986, pages 377-393.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, SHAPE and SCALE;
!    2: Calculate X from P, Q, SHAPE and SCALE;
!    3: Calculate SHAPE from P, Q, X and SCALE;
!    4: Calculate SCALE from P, Q, X and SHAPE.
!
!    Input/output, real ( kind = 8 ) P, the integral from 0 to X of the
!    Gamma density.  If this is an input value, it should lie in the
!    range: [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Gamma density.  If this is an input value, it should lie in the
!    range: [0, +infinity).  If it is an output value, it will lie in
!    the range: [0,1E300].
!
!    Input/output, real ( kind = 8 ) SHAPE, the shape parameter of the
!    Gamma density.  If this is an input value, it should lie in the range:
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: [1.0D-300,1.0D+300].
!
!    Input/output, real ( kind = 8 ) SCALE, the scale parameter of the
!    Gamma density.  If this is an input value, it should lie in the range
!    (0, +infinity).  If it is an output value, it will be searched for
!    in the range: (1.0D-300,1.0D+300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +10, if the Gamma or inverse Gamma routine cannot compute the answer.
!    This usually happens only for X and SHAPE very large (more than 1.0D+10.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) FX
    INTEGER (KIND=4) IERR
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) PORQ
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    REAL (KIND=8) SCALE
    REAL (KIND=8) SHAPE
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) STATUS, WHICH
    REAL (KIND=8) X
    REAL (KIND=8) XSCALE
    REAL (KIND=8) XX
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFGAM - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFGAM - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless X is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (X < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter X is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless SHAPE is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (SHAPE <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter SHAPE is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless SCALE is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (SCALE <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  Input parameter SCALE is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+0)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFGAM - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Select the minimum of P or Q.
!
    IF (WHICH /= 1) THEN
        PORQ = MIN (P, Q)
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        STATUS = 0
        XSCALE = X * SCALE
        CALL CUMGAM (XSCALE, SHAPE, P, Q)
 
        IF (1.5D+00 < PORQ) THEN
            STATUS = 10
        END IF
!
!  Calculate X.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL GAMMA_INC_INV (SHAPE, XX,-1.0D+00, P, Q, IERR)
 
        IF (IERR < 0.0D+00) THEN
            STATUS = 10
            RETURN
        END IF
 
        X = XX / SCALE
        STATUS = 0
!
!  Calculate SHAPE.
!
    ELSE IF (WHICH == 3) THEN
 
        XSCALE = X * SCALE
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        SHAPE = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, SHAPE, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMGAM (XSCALE, SHAPE, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            IF (P <= Q .AND. 1.5D+00 < CUM) THEN
                STATUS = 10
                RETURN
            ELSE IF (Q < P .AND. 1.5D+00 < CCUM) THEN
                STATUS = 10
                RETURN
            END IF
 
            CALL DINVR (STATUS, SHAPE, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFGAM - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFGAM - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate SCALE.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL GAMMA_INC_INV (SHAPE, XX,-1.0D+00, P, Q, IERR)
 
        IF (IERR < 0.0D+00) THEN
            STATUS = 10
        ELSE
            SCALE = XX / X
            STATUS = 0
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFNBN (WHICH, P, Q, F, S, PR, OMPR, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFNBN evaluates the CDF of the Negative Binomial distribution
!
!  Discussion:
!
!    This routine calculates any one parameter of the negative binomial
!    distribution given values for the others.
!
!    The cumulative negative binomial distribution returns the
!    probability that there will be F or fewer failures before the
!    S-th success in binomial trials each of which has probability of
!    success PR.
!
!    The individual term of the negative binomial is the probability of
!    F failures before S successes and is
!    Choose( F, S+F-1 ) * PR^(S) * (1-PR)^F
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.26.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from F, S, PR and OMPR;
!    2: Calculate F from P, Q, S, PR and OMPR;
!    3: Calculate S from P, Q, F, PR and OMPR;
!    4: Calculate PR and OMPR from P, Q, F and S.
!
!    Input/output, real ( kind = 8 ) P, the cumulation from 0 to F of
!    the negative binomial distribution.  If P is an input value, it
!    should lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) F, the upper limit of cumulation of
!    the binomial distribution.  There are F or fewer failures before
!    the S-th success.  If this is an input value, it may lie in the
!    range [0,+infinity), and if it is an output value, it will be searched
!    for in the range [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) S, the number of successes.
!    If this is an input value, it should lie in the range: [0, +infinity).
!    If it is an output value, it will be searched for in the range:
!    [0, 1.0D+300].
!
!    Input/output, real ( kind = 8 ) PR, the probability of success in each
!    binomial trial.  Whether an input or output value, it should lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) OMPR, the value of (1-PR).  Whether an
!    input or output value, it should lie in the range [0,1].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1;
!    +4, if PR + OMPR /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) F
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) OMPR
    REAL (KIND=8) P
    REAL (KIND=8) PR
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    REAL (KIND=8) S
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) XHI
    REAL (KIND=8) XLO
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFNBN - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
 
    IF (4 < WHICH) THEN
        BOUND = 4.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFNBN - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless F is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (F < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter F is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless S is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (S < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter S is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless PR is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (PR < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PR is out of range.'
            RETURN
        ELSE IF (1.0D+00 < PR) THEN
            BOUND = 1.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter PR is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless OMPR is to be computed, make sure it is legal.
!
    IF (WHICH /= 4) THEN
        IF (OMPR < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter OMPR is out of range.'
            RETURN
        ELSE IF (1.0D+00 < OMPR) THEN
            BOUND = 1.0D+00
            STATUS = - 7
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  Input parameter OMPR is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Check that PR + OMPR = 1.
!
    IF (WHICH /= 4) THEN
        IF (3.0D+00*EPSILON(PR) < ABS((PR+OMPR)-1.0D+00)) THEN
            STATUS = 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNBN - Fatal error!'
            WRITE (*, '(a)') '  PR + OMPR /= 1.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMNBN (F, S, PR, OMPR, P, Q)
        STATUS = 0
!
!  Calculate F.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        F = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMNBN (F, S, PR, OMPR, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, F, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate S.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        S = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMNBN (F, S, PR, OMPR, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBn - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate PR and OMPR.
!
    ELSE IF (WHICH == 4) THEN
 
        CALL DSTZR (0.0D+00, 1.0D+00, ATOL, TOL)
 
        IF (P <= Q) THEN
 
            STATUS = 0
            CALL DZROR (STATUS, PR, FX, XLO, XHI, QLEFT, QHI)
            OMPR = 1.0D+00 - PR
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMNBN (F, S, PR, OMPR, CUM, CCUM)
                FX = CUM - P
                CALL DZROR (STATUS, PR, FX, XLO, XHI, QLEFT, QHI)
                OMPR = 1.0D+00 - PR
 
            END DO
 
        ELSE
 
            STATUS = 0
            CALL DZROR (STATUS, OMPR, FX, XLO, XHI, QLEFT, QHI)
            PR = 1.0D+00 - OMPR
 
            DO WHILE (STATUS ==  1)
 
                CALL CUMNBN (F, S, PR, OMPR, CUM, CCUM)
                FX = CCUM - Q
                CALL DZROR (STATUS, OMPR, FX, XLO, XHI, QLEFT, QHI)
                PR = 1.0D+00 - OMPR
 
            END DO
 
        END IF
 
        IF (STATUS ==-1) THEN
 
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = 1.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFNBN - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
 
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFNOR (WHICH, P, Q, X, MEAN, SD, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFNOR evaluates the CDF of the Normal distribution.
!
!  Discussion:
!
!    A slightly modified version of ANORM from SPECFUN
!    is used to calculate the cumulative standard normal distribution.
!
!    The rational functions from pages 90-95 of Kennedy and Gentle
!    are used as starting values to a Newton iteration which
!    compute the inverse standard normal.  Therefore no searches are
!    necessary for any parameter.
!
!    For X < -15, the asymptotic expansion for the normal is used  as
!    the starting value in finding the inverse standard normal.
!
!    The normal density is proportional to
!    exp ( - 0.5D+00 * (( X - MEAN)/SD)**2)
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.2.12.
!
!    William Cody,
!    Algorithm 715:
!    SPECFUN - A Portable FORTRAN Package of
!    Special Function Routines and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, pages 22-32, 1993.
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from X, MEAN and SD;
!    2: Calculate X from P, Q, MEAN and SD;
!    3: Calculate MEAN from P, Q, X and SD;
!    4: Calculate SD from P, Q, X and MEAN.
!
!    Input/output, real ( kind = 8 ) P, the integral from -infinity to X
!    of the Normal density.  If this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) X, the upper limit of integration of
!    the Normal density.
!
!    Input/output, real ( kind = 8 ) MEAN, the mean of the Normal density.
!
!    Input/output, real ( kind = 8 ) SD, the standard deviation of the
!    Normal density.  If this is an input value, it should lie in the
!    range (0,+infinity).
!
!    Output, integer ( kind = 4 ) STATUS, the status of the calculation.
!    0, if calculation completed correctly;
!    -I, if input parameter number I is out of range;
!    1, if answer appears to be lower than lowest search bound;
!    2, if answer appears to be higher than greatest search bound;
!    3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8) BOUND
    REAL (KIND=8) MEAN
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) SD
    INTEGER (KIND=4) STATUS
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) X
    REAL (KIND=8) Z
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    STATUS = 0
 
    IF (WHICH < 1) THEN
        STATUS = - 1
        BOUND = 1.0D+00
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFNOR - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    ELSE IF (4 < WHICH) THEN
        STATUS = - 1
        BOUND = 4.0D+00
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFNOR - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 4.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
 
    IF (WHICH /= 4) THEN
        IF (SD <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 6
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFNOR - Fatal error!'
            WRITE (*, '(a)') '  Input parameter SD is out of range.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        Z = (X-MEAN) / SD
        CALL CUMNOR (Z, P, Q)
!
!  Calculate X.
!
    ELSE IF (WHICH == 2) THEN
 
        Z = DINVNR (P, Q)
        X = SD * Z + MEAN
!
!  Calculate MEAN.
!
    ELSE IF (WHICH == 3) THEN
 
        Z = DINVNR (P, Q)
        MEAN = X - SD * Z
!
!  Calculate SD.
!
    ELSE IF (WHICH == 4) THEN
 
        Z = DINVNR (P, Q)
        SD = (X-MEAN) / Z
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFPOI (WHICH, P, Q, S, XLAM, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFPOI evaluates the CDF of the Poisson distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the Poisson distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.  The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.21.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1: Calculate P and Q from S and XLAM;
!    2: Calculate A from P, Q and XLAM;
!    3: Calculate XLAM from P, Q and S.
!
!    Input/output, real ( kind = 8 ) P, the cumulation from 0 to S of the
!    Poisson density.  Whether this is an input or output value, it will
!    lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) S, the upper limit of cumulation of
!    the Poisson CDF.  If this is an input value, it should lie in
!    the range: [0, +infinity).  If it is an output value, it will be
!    searched for in the range: [0,1.0D+300].
!
!    Input/output, real ( kind = 8 ) XLAM, the mean of the Poisson
!    distribution.  If this is an input value, it should lie in the range
!    [0, +infinity).  If it is an output value, it will be searched for
!    in the range: [0,1E300].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+300
    REAL (KIND=8) P
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    REAL (KIND=8) S
    INTEGER (KIND=4) STATUS
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
    REAL (KIND=8) XLAM
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFPOI - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
 
    IF (3 < WHICH) THEN
        BOUND = 3.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFPOI - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless S is to be computed, make sure it is legal.
!
    IF (WHICH /= 2) THEN
        IF (S < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 4
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter S is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless XLAM is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (XLAM < 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  Input parameter XLAM is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(P) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFPOI - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMPOI (S, XLAM, P, Q)
        STATUS = 0
!
!  Calculate S.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        S = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMPOI (S, XLAM, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, S, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFPOI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFPOI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate XLAM.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (0.0D+00, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        XLAM = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, XLAM, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMPOI (S, XLAM, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, XLAM, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFPOI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFPOI - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CDFT (WHICH, P, Q, T, DF, STATUS, BOUND)
 
!*****************************************************************************80
!
!! CDFT evaluates the CDF of the T distribution.
!
!  Discussion:
!
!    This routine calculates any one parameter of the T distribution
!    given the others.
!
!    The value P of the cumulative distribution function is calculated
!    directly.
!
!    Computation of other parameters involve a seach for a value that
!    produces the desired value of P.   The search relies on the
!    monotonicity of P with respect to the other parameters.
!
!    The original version of this routine allowed the search interval
!    to extend from -1.0D+300 to +1.0D+300, which is fine until you
!    try to evaluate a function at such a point!
!
!  Modified:
!
!    14 April 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.27.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WHICH, indicates which argument is to be
!    calculated from the others.
!    1 : Calculate P and Q from T and DF;
!    2 : Calculate T from P, Q and DF;
!    3 : Calculate DF from P, Q and T.
!
!    Input/output, real ( kind = 8 ) P, the integral from -infinity to T of
!    the T-density.  Whether an input or output value, this will lie in the
!    range [0,1].
!
!    Input/output, real ( kind = 8 ) Q, equal to 1-P.  If Q is an input
!    value, it should lie in the range [0,1].  If Q is an output value,
!    it will lie in the range [0,1].
!
!    Input/output, real ( kind = 8 ) T, the upper limit of integration of
!    the T-density.  If this is an input value, it may have any value.
!    It it is an output value, it will be searched for in the range
!    [ -1.0D+30, 1.0D+30 ].
!
!    Input/output, real ( kind = 8 ) DF, the number of degrees of freedom
!    of the T distribution.  If this is an input value, it should lie
!    in the range: (0 , +infinity).  If it is an output value, it will be
!    searched for in the range: [1, 1.0D+10].
!
!    Output, integer ( kind = 4 ) STATUS, reports the status of the computation.
!     0, if the calculation completed correctly;
!    -I, if the input parameter number I is out of range;
!    +1, if the answer appears to be lower than lowest search bound;
!    +2, if the answer appears to be higher than greatest search bound;
!    +3, if P + Q /= 1.
!
!    Output, real ( kind = 8 ) BOUND, is only defined if STATUS is nonzero.
!    If STATUS is negative, then this is the value exceeded by parameter I.
!    if STATUS is 1 or 2, this is the search bound that was exceeded.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: ATOL = 1.0D-10
    REAL (KIND=8) BOUND
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) FX
    REAL (KIND=8), PARAMETER :: INF = 1.0D+30
    REAL (KIND=8), PARAMETER :: MAXDF = 1.0D+10
    REAL (KIND=8) P
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    INTEGER (KIND=4) STATUS
    REAL (KIND=8) T
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-08
    INTEGER (KIND=4) WHICH
 
    STATUS = 0
    BOUND = 0.0D+00
!
!  Check the arguments.
!
    IF (WHICH < 1) THEN
        BOUND = 1.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFT - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
 
    IF (3 < WHICH) THEN
        BOUND = 3.0D+00
        STATUS = - 1
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CDFT - Fatal error!'
        WRITE (*, '(a)') '  The input parameter WHICH is out of range.'
        WRITE (*, '(a)') '  Legal values are between 1 and 3.'
        RETURN
    END IF
!
!  Unless P is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (P < 0.0D+00) THEN
            STATUS = - 2
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        ELSE IF (1.0D+00 < P) THEN
            STATUS = - 2
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  Input parameter P is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless Q is to be computed, make sure it is legal.
!
    IF (WHICH /= 1) THEN
        IF (Q < 0.0D+00) THEN
            STATUS = - 3
            BOUND = 0.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        ELSE IF (1.0D+00 < Q) THEN
            STATUS = - 3
            BOUND = 1.0D+00
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  Input parameter Q is out of range.'
            RETURN
        END IF
    END IF
!
!  Unless DF is to be computed, make sure it is legal.
!
    IF (WHICH /= 3) THEN
        IF (DF <= 0.0D+00) THEN
            BOUND = 0.0D+00
            STATUS = - 5
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  Input parameter DF is out of range.'
            RETURN
        END IF
    END IF
!
!  Check that P + Q = 1.
!
    IF (WHICH /= 1) THEN
        IF (3.0D+00*EPSILON(1.0D+00) < ABS((P+Q)-1.0D+00)) THEN
            STATUS = 3
            WRITE (*, '(a)') ' '
            WRITE (*, '(a)') 'CDFT - Fatal error!'
            WRITE (*, '(a)') '  P + Q /= 1.'
            RETURN
        END IF
    END IF
!
!  Calculate P and Q.
!
    IF (WHICH == 1) THEN
 
        CALL CUMT (T, DF, P, Q)
        STATUS = 0
!
!  Calculate T.
!
    ELSE IF (WHICH == 2) THEN
 
        CALL DSTINV (-INF, INF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        T = DT1 (P, Q, DF)
        FX = 0.0D+00
 
        CALL DINVR (STATUS, T, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMT (T, DF, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, T, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = - INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFT - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = INF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFT - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
!
!  Calculate DF.
!
    ELSE IF (WHICH == 3) THEN
 
        CALL DSTINV (1.0D+00, MAXDF, 0.5D+00, 0.5D+00, 5.0D+00, ATOL, TOL)
 
        STATUS = 0
        DF = 5.0D+00
        FX = 0.0D+00
 
        CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        DO WHILE (STATUS ==  1)
 
            CALL CUMT (T, DF, CUM, CCUM)
 
            IF (P <= Q) THEN
                FX = CUM - P
            ELSE
                FX = CCUM - Q
            END IF
 
            CALL DINVR (STATUS, DF, FX, QLEFT, QHI)
 
        END DO
 
        IF (STATUS ==-1) THEN
            IF (QLEFT) THEN
                STATUS = 1
                BOUND = 0.0D+00
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFT - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be lower than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            ELSE
                STATUS = 2
                BOUND = MAXDF
                WRITE (*, '(a)') ' '
                WRITE (*, '(a)') 'CDFT - Warning!'
                WRITE (*, '(a)') '  The desired answer appears to be higher than'
                WRITE (*, '(a,g14.6)') '  the search bound of ', BOUND
            END IF
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CHI_NONCENTRAL_CDF_VALUES (N_DATA, X, LAMBDA, DF, CDF)
 
!*****************************************************************************80
!
!! CHI_NONCENTRAL_CDF_VALUES returns values of the noncentral chi CDF.
!
!  Discussion:
!
!    The CDF of the noncentral chi square distribution can be evaluated
!    within Mathematica by commands such as:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF [ NoncentralChiSquareDistribution [ DF, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
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
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) LAMBDA, the noncentrality parameter.
!
!    Output, integer ( kind = 4 ) DF, the number of degrees of freedom.
!
!    Output, real ( kind = 8 ) CDF, the noncentral chi CDF.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 27
 
    REAL (KIND=8) CDF
    REAL, SAVE, DIMENSION (N_MAX) :: CDF_VEC = (/ 0.839944D+00, 0.695906D+00, 0.535088D+00, &
   & 0.764784D+00, 0.620644D+00, 0.469167D+00, 0.307088D+00, 0.220382D+00, 0.150025D+00, &
   & 0.307116D-02, 0.176398D-02, 0.981679D-03, 0.165175D-01, 0.202342D-03, 0.498448D-06, &
   & 0.151325D-01, 0.209041D-02, 0.246502D-03, 0.263684D-01, 0.185798D-01, 0.130574D-01, &
   & 0.583804D-01, 0.424978D-01, 0.308214D-01, 0.105788D+00, 0.794084D-01, 0.593201D-01 /)
    INTEGER (KIND=4) DF
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: DF_VEC = (/ 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, &
   & 3, 60, 80, 100, 1, 2, 3, 10, 10, 10, 10, 10, 10, 10, 10, 10 /)
    REAL (KIND=8) LAMBDA
    REAL, SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.5D+00, 0.5D+00, 0.5D+00, 1.0D+00, &
   & 1.0D+00, 1.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, &
   & 30.0D+00, 30.0D+00, 5.0D+00, 5.0D+00, 5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 2.0D+00, &
   & 3.0D+00, 4.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL, SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, &
   & 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, 3.000D+00, &
   & 60.000D+00, 60.000D+00, 60.000D+00, 0.050D+00, 0.050D+00, 0.050D+00, 4.000D+00, 4.000D+00, &
   & 4.000D+00, 5.000D+00, 5.000D+00, 5.000D+00, 6.000D+00, 6.000D+00, 6.000D+00 /)
 
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
 
SUBROUTINE CHI_SQUARE_CDF_VALUES (N_DATA, A, X, FX)
 
!*****************************************************************************80
!
!! CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
!
!  Discussion:
!
!    The value of CHI_CDF ( DF, X ) can be evaluated in Mathematica by
!    commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ChiSquareDistribution[DF], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 21
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 1, 2, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4, &
   & 5, 3, 3, 3, 3, 3, 10, 10, 10 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.0796557D+00, 0.00498752D+00, &
   & 0.112463D+00, 0.00995017D+00, 0.472911D+00, 0.181269D+00, 0.0597575D+00, 0.0175231D+00, &
   & 0.682689D+00, 0.393469D+00, 0.198748D+00, 0.090204D+00, 0.0374342D+00, 0.427593D+00, &
   & 0.608375D+00, 0.738536D+00, 0.828203D+00, 0.88839D+00, 0.000172116D+00, 0.00365985D+00, &
   & 0.0185759D+00 /)
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
 
SUBROUTINE CUMBET (X, Y, A, B, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMBET evaluates the cumulative incomplete beta distribution.
!
!  Discussion:
!
!    This routine calculates the CDF to X of the incomplete beta distribution
!    with parameters A and B.  This is the integral from 0 to x
!    of (1/B(a,b))*f(t)) where f(t) = t**(a-1) * (1-t)**(b-1)
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) Y, the value of 1-X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the values of the cumulative
!    density function and complementary cumulative density function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    INTEGER (KIND=4) IERR
    REAL (KIND=8) X
    REAL (KIND=8) Y
 
    IF (X <= 0.0D+00) THEN
 
        CUM = 0.0
        CCUM = 1.0D+00
 
    ELSE IF (Y <= 0.0D+00) THEN
 
        CUM = 1.0D+00
        CCUM = 0.0
 
    ELSE
 
        CALL BETA_INC (A, B, X, Y, CUM, CCUM, IERR)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMBIN (S, XN, PR, OMPR, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMBIN evaluates the cumulative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability of 0 to S successes in XN binomial
!    trials, each of which has a probability of success, PR.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.24.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of summation.
!
!    Input, real ( kind = 8 ) XN, the number of trials.
!
!    Input, real ( kind = 8 ) PR, the probability of success in one trial.
!
!    Input, real ( kind = 8 ) OMPR, equals ( 1 - PR ).
!
!    Output, real ( kind = 8 ) CUM, the cumulative binomial distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    binomial distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) OMPR
    REAL (KIND=8) PR
    REAL (KIND=8) S
    REAL (KIND=8) XN
 
    IF (S < XN) THEN
 
        CALL CUMBET (PR, OMPR, S+1.0D+00, XN-S, CCUM, CUM)
 
    ELSE
 
        CUM = 1.0D+00
        CCUM = 0.0D+00
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMCHI (X, DF, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMCHI evaluates the cumulative chi-square distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the degrees of freedom of the
!    chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, the cumulative chi-square distribution.
!
!    Output, real ( kind = 8 ) CCUM, the complement of the cumulative
!    chi-square distribution.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) X
    REAL (KIND=8) XX
 
    A = DF * 0.5D+00
    XX = X * 0.5D+00
 
    CALL CUMGAM (XX, A, CUM, CCUM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMCHN (X, DF, PNONC, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMCHN evaluates the cumulative noncentral chi-square distribution.
!
!  Discussion:
!
!    This routine calculates the cumulative noncentral chi-square
!    distribution, i.e., the probability that a random variable
!    which follows the noncentral chi-square distribution, with
!    noncentrality parameter PNONC and continuous degrees of
!    freedom DF, is less than or equal to X.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.4.25.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter of
!    the noncentral chi-square distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the CDF and complementary
!    CDF of the noncentral chi-square distribution.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) EPS, the convergence criterion.  The sum
!    stops when a term is less than EPS * SUM.
!
!    Local, integer NTIRED, the maximum number of terms to be evaluated
!    in each sum.
!
!    Local, logical QCONV, is TRUE if convergence was achieved, that is,
!    the program did not stop on NTIRED criterion.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ADJ
    REAL (KIND=8) CCUM
    REAL (KIND=8) CENTAJ
    REAL (KIND=8) CENTWT
    REAL (KIND=8) CHID2
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) DFD2
    REAL (KIND=8) DG
    REAL (KIND=8), PARAMETER :: EPS = 0.00001D+00
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICENT
    INTEGER (KIND=4) ITERB
    INTEGER (KIND=4) ITERF
    REAL (KIND=8) LCNTAJ
    REAL (KIND=8) LCNTWT
    REAL (KIND=8) LFACT
    INTEGER (KIND=4), PARAMETER :: NTIRED = 1000
    REAL (KIND=8) PCENT
    REAL (KIND=8) PNONC
    REAL (KIND=8) PTERM
    LOGICAL QSMALL
    REAL (KIND=8) SUM1
    REAL (KIND=8) SUMADJ
    REAL (KIND=8) TERM
    REAL (KIND=8) WT
    REAL (KIND=8) X
    REAL (KIND=8) XNONC
    REAL (KIND=8) XX
 
    QSMALL (XX) = SUM1 < 1.0D-20 .OR. XX < EPS * SUM1
    DG (I) = DF + 2.0D+00 * REAL (I, KIND=8)
 
    IF (X <= 0.0D+00) THEN
        CUM = 0.0D+00
        CCUM = 1.0D+00
        RETURN
    END IF
!
!  When the noncentrality parameter is (essentially) zero,
!  use cumulative chi-square distribution
!
    IF (PNONC <= 1.0D-10) THEN
        CALL CUMCHI (X, DF, CUM, CCUM)
        RETURN
    END IF
 
    XNONC = PNONC / 2.0D+00
!
!  The following code calculates the weight, chi-square, and
!  adjustment term for the central term in the infinite series.
!  The central term is the one in which the poisson weight is
!  greatest.  The adjustment term is the amount that must
!  be subtracted from the chi-square to move up two degrees
!  of freedom.
!
    ICENT = INT (XNONC)
    IF (ICENT == 0) THEN
        ICENT = 1
    END IF
 
    CHID2 = X / 2.0D+00
!
!  Calculate central weight term.
!
    LFACT = GAMMA_LOG (REAL(ICENT+1, KIND=8))
    LCNTWT = - XNONC + ICENT * LOG (XNONC) - LFACT
    CENTWT = EXP (LCNTWT)
!
!  Calculate central chi-square.
!
    CALL CUMCHI (X, DG(ICENT), PCENT, CCUM)
!
!  Calculate central adjustment term.
!
    DFD2 = DG (ICENT) / 2.0D+00
    LFACT = GAMMA_LOG (1.0D+00+DFD2)
    LCNTAJ = DFD2 * LOG (CHID2) - CHID2 - LFACT
    CENTAJ = EXP (LCNTAJ)
    SUM1 = CENTWT * PCENT
!
!  Sum backwards from the central term towards zero.
!  Quit whenever either
!  (1) the zero term is reached, or
!  (2) the term gets small relative to the sum, or
!  (3) More than NTIRED terms are totaled.
!
    ITERB = 0
    SUMADJ = 0.0D+00
    ADJ = CENTAJ
    WT = CENTWT
    I = ICENT
    TERM = 0.0D+00
 
    DO
 
        DFD2 = DG (I) / 2.0D+00
!
!  Adjust chi-square for two fewer degrees of freedom.
!  The adjusted value ends up in PTERM.
!
        ADJ = ADJ * DFD2 / CHID2
        SUMADJ = SUMADJ + ADJ
        PTERM = PCENT + SUMADJ
!
!  Adjust Poisson weight for J decreased by one.
!
        WT = WT * (I/XNONC)
        TERM = WT * PTERM
        SUM1 = SUM1 + TERM
        I = I - 1
        ITERB = ITERB + 1
 
        IF (NTIRED < ITERB .OR. QSMALL(TERM) .OR. I == 0) THEN
            EXIT
        END IF
 
    END DO
 
    ITERF = 0
!
!  Now sum forward from the central term towards infinity.
!  Quit when either
!    (1) the term gets small relative to the sum, or
!    (2) More than NTIRED terms are totaled.
!
    SUMADJ = CENTAJ
    ADJ = CENTAJ
    WT = CENTWT
    I = ICENT
!
!  Update weights for next higher J.
!
    DO
 
        WT = WT * (XNONC/(I+1))
!
!  Calculate PTERM and add term to sum.
!
        PTERM = PCENT - SUMADJ
        TERM = WT * PTERM
        SUM1 = SUM1 + TERM
!
!  Update adjustment term for DF for next iteration.
!
        I = I + 1
        DFD2 = DG (I) / 2.0D+00
        ADJ = ADJ * CHID2 / DFD2
        SUMADJ = SUMADJ + ADJ
        ITERF = ITERF + 1
 
        IF (NTIRED < ITERF .OR. QSMALL(TERM)) THEN
            EXIT
        END IF
 
    END DO
 
    CUM = SUM1
    CCUM = 0.5D+00 + (0.5D+00-CUM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMF (F, DFN, DFD, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMF evaluates the cumulative F distribution.
!
!  Discussion:
!
!    This routine computes the integral from 0 to F of the F density with DFN
!    numerator and DFD denominator degrees of freedom.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.28.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of
!    freedom for the numerator and denominator.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the value of the F CDF and
!    the complementary F CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DFD
    REAL (KIND=8) DFN
    REAL (KIND=8) DSUM
    REAL (KIND=8) F
    INTEGER (KIND=4) IERR
    REAL (KIND=8) PROD
    REAL (KIND=8) XX
    REAL (KIND=8) YY
 
    IF (F <= 0.0D+00) THEN
        CUM = 0.0D+00
        CCUM = 1.0D+00
        RETURN
    END IF
 
    PROD = DFN * F
!
!  XX is such that the incomplete beta with parameters
!  DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM
!
!  YY is 1 - XX
!
!  Calculate the smaller of XX and YY accurately.
!
    DSUM = DFD + PROD
    XX = DFD / DSUM
 
    IF (0.5D+00 < XX) THEN
        YY = PROD / DSUM
        XX = 1.0D+00 - YY
    ELSE
        YY = 1.0D+00 - XX
    END IF
 
    CALL BETA_INC (0.5D+00*DFD, 0.5D+00*DFN, XX, YY, CCUM, CUM, IERR)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMFNC (F, DFN, DFD, PNONC, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMFNC evaluates the cumulative noncentral F distribution.
!
!  Discussion:
!
!    This routine computes the noncentral F distribution with DFN and DFD
!    degrees of freedom and noncentrality parameter PNONC.
!
!    The series is calculated backward and forward from J = LAMBDA/2
!    (this is the term with the largest Poisson weight) until
!    the convergence criterion is met.
!
!    The sum continues until a succeeding term is less than EPS
!    times the sum or the sum is very small.  EPS is
!    set to 1.0D-4 in a data statement which can be changed.
!
!    The original version of this routine allowed the input values
!    of DFN and DFD to be negative (nonsensical) or zero (which
!    caused numerical overflow.)  I have forced both these values
!    to be at least 1.
!
!  Modified:
!
!    19 May 2007
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.16, 26.6.17, 26.6.18, 26.6.20.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DFN, DFD, the number of degrees of freedom
!    in the numerator and denominator.  Both DFN and DFD must be positive,
!    and normally would be integers.  This routine requires that they
!    be no less than 1.
!
!    Input, real ( kind = 8 ) PNONC, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the noncentral F CDF and
!    complementary CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) ADN
    REAL (KIND=8) ARG1
    REAL (KIND=8) AUP
    REAL (KIND=8) B
    REAL (KIND=8) BETDN
    REAL (KIND=8) BETUP
    REAL (KIND=8) CENTWT
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DFD
    REAL (KIND=8) DFN
    REAL (KIND=8) DNTERM
    REAL (KIND=8) DSUM
    REAL (KIND=8) DUMMY
    REAL (KIND=8), PARAMETER :: EPS = 0.0001D+00
    REAL (KIND=8) EXPON
    REAL (KIND=8) F
    INTEGER (KIND=4) I
    INTEGER (KIND=4) ICENT
    INTEGER (KIND=4) IERR
    REAL (KIND=8) PNONC
    REAL (KIND=8) PROD
    REAL (KIND=8) SUM1
    REAL (KIND=8) UPTERM
    REAL (KIND=8) XMULT
    REAL (KIND=8) XNONC
    REAL (KIND=8) XX
    REAL (KIND=8) YY
 
    IF (F <= 0.0D+00) THEN
        CUM = 0.0D+00
        CCUM = 1.0D+00
        RETURN
    END IF
 
    IF (DFN < 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CUMFNC - Fatal error!'
        WRITE (*, '(a)') '  DFN < 1.'
        RETURN
    END IF
 
    IF (DFD < 1.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CUMFNC - Fatal error!'
        WRITE (*, '(a)') '  DFD < 1.'
        RETURN
    END IF
!
!  Handle case in which the noncentrality parameter is essentially zero.
!
    IF (PNONC < 1.0D-10) THEN
        CALL CUMF (F, DFN, DFD, CUM, CCUM)
        RETURN
    END IF
 
    XNONC = PNONC / 2.0D+00
!
!  Calculate the central term of the Poisson weighting factor.
!
    ICENT = INT (XNONC)
 
    IF (ICENT == 0) THEN
        ICENT = 1
    END IF
!
!  Compute central weight term.
!
    CENTWT = EXP (-XNONC+ICENT*LOG(XNONC)-GAMMA_LOG(REAL(ICENT+1, KIND=8)))
!
!  Compute central incomplete beta term.
!  Ensure that minimum of arg to beta and 1 - arg is computed accurately.
!
    PROD = DFN * F
    DSUM = DFD + PROD
    YY = DFD / DSUM
 
    IF (0.5D+00 < YY) THEN
        XX = PROD / DSUM
        YY = 1.0D+00 - XX
    ELSE
        XX = 1.0D+00 - YY
    END IF
 
    ARG1 = 0.5D+00 * DFN + REAL (ICENT, KIND=8)
    CALL BETA_INC (ARG1, 0.5D+00*DFD, XX, YY, BETDN, DUMMY, IERR)
 
    ADN = DFN / 2.0D+00 + REAL (ICENT, KIND=8)
    AUP = ADN
    B = DFD / 2.0D+00
    BETUP = BETDN
    SUM1 = CENTWT * BETDN
!
!  Now sum terms backward from ICENT until convergence or all done.
!
    XMULT = CENTWT
    I = ICENT
    DNTERM = EXP (GAMMA_LOG(ADN+B)-GAMMA_LOG(ADN+1.0D+00)-GAMMA_LOG(B)+ADN*LOG(XX)+B*LOG(YY))
 
    DO
 
        IF (I <= 0) THEN
            EXIT
        END IF
 
        IF (SUM1 < EPSILON(XMULT*BETDN) .OR. XMULT*BETDN < EPS*SUM1) THEN
            EXIT
        END IF
 
        XMULT = XMULT * (REAL(I, KIND=8)/XNONC)
        I = I - 1
        ADN = ADN - 1.0D+00
        DNTERM = (ADN+1.0D+00) / ((ADN+B)*XX) * DNTERM
        BETDN = BETDN + DNTERM
        SUM1 = SUM1 + XMULT * BETDN
 
    END DO
 
    I = ICENT + 1
!
!  Now sum forward until convergence.
!
    XMULT = CENTWT
 
    IF ((AUP-1.0D+00+B) == 0) THEN
 
        EXPON = - GAMMA_LOG (AUP) - GAMMA_LOG (B) + (AUP-1.0D+00) * LOG (XX) + B * LOG (YY)
 
    ELSE
 
        EXPON = GAMMA_LOG (AUP-1.0D+00+B) - GAMMA_LOG (AUP) - GAMMA_LOG (B) + (AUP-1.0D+00) * &
       & LOG (XX) + B * LOG (YY)
 
    END IF
!
!  The fact that DCDFLIB blithely assumes that 1.0E+30 is a reasonable
!  value to plug into any function, and that G95 computes corresponding
!  function values of, say 1.0E-303, and then chokes with a floating point
!  error when asked to combine such a value with a reasonable floating
!  point quantity, has driven me to the following sort of check that
!  was last fashionable in the 1960's!
!
    IF (EXPON <= LOG(EPSILON(EXPON))) THEN
        UPTERM = 0.0D+00
    ELSE
        UPTERM = EXP (EXPON)
    END IF
 
    DO
 
        XMULT = XMULT * (XNONC/REAL(I, KIND=8))
        I = I + 1
        AUP = AUP + 1.0D+00
        UPTERM = (AUP+B-2.0D+00) * XX / (AUP-1.0D+00) * UPTERM
        BETUP = BETUP - UPTERM
        SUM1 = SUM1 + XMULT * BETUP
 
        IF (SUM1 < EPSILON(XMULT*BETUP) .OR. XMULT*BETUP < EPS*SUM1) THEN
            EXIT
        END IF
 
    END DO
 
    CUM = SUM1
    CCUM = 0.5D+00 + (0.5D+00-CUM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMGAM (X, A, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMGAM evaluates the cumulative incomplete gamma distribution.
!
!  Discussion:
!
!    This routine computes the cumulative distribution function of the
!    incomplete gamma distribution, i.e., the integral from 0 to X of
!
!      (1/GAM(A))*EXP(-T)*T^(A-1) DT
!
!    where GAM(A) is the complete gamma function of A:
!
!      GAM(A) = integral from 0 to infinity of EXP(-T)*T^(A-1) DT
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the upper limit of integration.
!
!    Input, real ( kind = 8 ) A, the shape parameter of the incomplete
!    Gamma distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the incomplete Gamma CDF and
!    complementary CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
 
        CUM = 0.0D+00
        CCUM = 1.0D+00
 
    ELSE
 
        CALL GAMMA_INC (A, X, CUM, CCUM, 0)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMNBN (F, S, PR, OMPR, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMNBN evaluates the cumulative negative binomial distribution.
!
!  Discussion:
!
!    This routine returns the probability that there will be F or
!    fewer failures before there are S successes, with each binomial
!    trial having a probability of success PR.
!
!    Prob(# failures = F | S successes, PR)  =
!                        ( S + F - 1 )
!                        (            ) * PR^S * (1-PR)^F
!                        (      F     )
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.5.26.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) F, the number of failures.
!
!    Input, real ( kind = 8 ) S, the number of successes.
!
!    Input, real ( kind = 8 ) PR, OMPR, the probability of success on
!    each binomial trial, and the value of (1-PR).
!
!    Output, real ( kind = 8 ) CUM, CCUM, the negative binomial CDF,
!    and the complementary CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) F
    REAL (KIND=8) OMPR
    REAL (KIND=8) PR
    REAL (KIND=8) S
 
    CALL CUMBET (PR, OMPR, S, F+1.D+00, CUM, CCUM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMNOR (ARG, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMNOR computes the cumulative normal distribution.
!
!  Discussion:
!
!    This function evaluates the normal distribution function:
!
!                              / x
!                     1       |       -t*t/2
!          P(x) = ----------- |      e       dt
!                 sqrt(2 pi)  |
!                             /-oo
!
!    This transportable program uses rational functions that
!    theoretically approximate the normal distribution function to
!    at least 18 significant decimal digits.  The accuracy achieved
!    depends on the arithmetic system, the compiler, the intrinsic
!    functions, and proper selection of the machine dependent
!    constants.
!
!  Author:
!
!    William Cody
!    Mathematics and Computer Science Division
!    Argonne National Laboratory
!    Argonne, IL 60439
!
!  Reference:
!
!    William Cody,
!    Rational Chebyshev approximations for the error function,
!    Mathematics of Computation,
!    1969, pages 631-637.
!
!    William Cody,
!    Algorithm 715:
!    SPECFUN - A Portable FORTRAN Package of Special Function Routines
!    and Test Drivers,
!    ACM Transactions on Mathematical Software,
!    Volume 19, Number 1, 1993, pages 22-32.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ARG, the upper limit of integration.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the Normal density CDF and
!    complementary CDF.
!
!  Local Parameters:
!
!    Local, real ( kind = 8 ) EPS, the argument below which anorm(x)
!    may be represented by 0.5 and above which  x*x  will not underflow.
!    A conservative value is the largest machine number X
!    such that   1.0D+00 + X = 1.0D+00   to machine precision.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: A = (/ 2.2352520354606839287D+00, &
   & 1.6102823106855587881D+02, 1.0676894854603709582D+03, 1.8154981253343561249D+04, &
   & 6.5682337918207449113D-02 /)
    REAL (KIND=8) ARG
    REAL (KIND=8), PARAMETER, DIMENSION (4) :: B = (/ 4.7202581904688241870D+01, &
   & 9.7609855173777669322D+02, 1.0260932208618978205D+04, 4.5507789335026729956D+04 /)
    REAL (KIND=8), PARAMETER, DIMENSION (9) :: C = (/ 3.9894151208813466764D-01, &
   & 8.8831497943883759412D+00, 9.3506656132177855979D+01, 5.9727027639480026226D+02, &
   & 2.4945375852903726711D+03, 6.8481904505362823326D+03, 1.1602651437647350124D+04, &
   & 9.8427148383839780218D+03, 1.0765576773720192317D-08 /)
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8), PARAMETER, DIMENSION (8) :: D = (/ 2.2266688044328115691D+01, &
   & 2.3538790178262499861D+02, 1.5193775994075548050D+03, 6.4855582982667607550D+03, &
   & 1.8615571640885098091D+04, 3.4900952721145977266D+04, 3.8912003286093271411D+04, &
   & 1.9685429676859990727D+04 /)
    REAL (KIND=8) DEL
    REAL (KIND=8) EPS
    INTEGER (KIND=4) I
    REAL (KIND=8), PARAMETER, DIMENSION (6) :: P = (/ 2.1589853405795699D-01, &
   & 1.274011611602473639D-01, 2.2235277870649807D-02, 1.421619193227893466D-03, &
   & 2.9112874951168792D-05, 2.307344176494017303D-02 /)
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: Q = (/ 1.28426009614491121D+00, &
   & 4.68238212480865118D-01, 6.59881378689285515D-02, 3.78239633202758244D-03, &
   & 7.29751555083966205D-05 /)
    REAL (KIND=8), PARAMETER :: ROOT32 = 5.656854248D+00
    REAL (KIND=8), PARAMETER :: SIXTEN = 16.0D+00
    REAL (KIND=8) TEMP
    REAL (KIND=8), PARAMETER :: SQRPI = 3.9894228040143267794D-01
    REAL (KIND=8), PARAMETER :: THRSH = 0.66291D+00
    REAL (KIND=8) X
    REAL (KIND=8) XDEN
    REAL (KIND=8) XNUM
    REAL (KIND=8) Y
    REAL (KIND=8) XSQ
!
!  Machine dependent constants
!
    EPS = EPSILON (1.0D+00) * 0.5D+00
 
    X = ARG
    Y = ABS (X)
 
    IF (Y <= THRSH) THEN
!
!  Evaluate  anorm  for  |X| <= 0.66291
!
        IF (EPS < Y) THEN
            XSQ = X * X
        ELSE
            XSQ = 0.0D+00
        END IF
 
        XNUM = A (5) * XSQ
        XDEN = XSQ
        DO I = 1, 3
            XNUM = (XNUM+A(I)) * XSQ
            XDEN = (XDEN+B(I)) * XSQ
        END DO
        CUM = X * (XNUM+A(4)) / (XDEN+B(4))
        TEMP = CUM
        CUM = 0.5D+00 + TEMP
        CCUM = 0.5D+00 - TEMP
!
!  Evaluate ANORM for 0.66291 <= |X| <= sqrt(32)
!
    ELSE IF (Y <= ROOT32) THEN
 
        XNUM = C (9) * Y
        XDEN = Y
        DO I = 1, 7
            XNUM = (XNUM+C(I)) * Y
            XDEN = (XDEN+D(I)) * Y
        END DO
        CUM = (XNUM+C(8)) / (XDEN+D(8))
        XSQ = AINT (Y*SIXTEN) / SIXTEN
        DEL = (Y-XSQ) * (Y+XSQ)
        CUM = EXP (-XSQ*XSQ*0.5D+00) * EXP (-DEL*0.5D+00) * CUM
        CCUM = 1.0D+00 - CUM
 
        IF (0.0D+00 < X) THEN
            CALL R8_SWAP (CUM, CCUM)
        END IF
!
!  Evaluate ANORM for sqrt(32) < |X|.
!
    ELSE
 
        CUM = 0.0D+00
        XSQ = 1.0D+00 / (X*X)
        XNUM = P (6) * XSQ
        XDEN = XSQ
        DO I = 1, 4
            XNUM = (XNUM+P(I)) * XSQ
            XDEN = (XDEN+Q(I)) * XSQ
        END DO
 
        CUM = XSQ * (XNUM+P(5)) / (XDEN+Q(5))
        CUM = (SQRPI-CUM) / Y
        XSQ = AINT (X*SIXTEN) / SIXTEN
        DEL = (X-XSQ) * (X+XSQ)
        CUM = EXP (-XSQ*XSQ*0.5D+00) * EXP (-DEL*0.5D+00) * CUM
        CCUM = 1.0D+00 - CUM
 
        IF (0.0D+00 < X) THEN
            CALL R8_SWAP (CUM, CCUM)
        END IF
 
    END IF
 
    IF (CUM < TINY(CUM)) THEN
        CUM = 0.0D+00
    END IF
 
    IF (CCUM < TINY(CCUM)) THEN
        CCUM = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMPOI (S, XLAM, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMPOI evaluates the cumulative Poisson distribution.
!
!  Discussion:
!
!    This routine returns the probability of S or fewer events in a Poisson
!    distribution with mean XLAM.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.4.21.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) S, the upper limit of cumulation of the
!    Poisson density function.
!
!    Input, real ( kind = 8 ) XLAM, the mean of the Poisson distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the Poisson density CDF and
!    complementary CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CCUM
    REAL (KIND=8) CHI
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) S
    REAL (KIND=8) XLAM
 
    DF = 2.0D+00 * (S+1.0D+00)
    CHI = 2.0D+00 * XLAM
 
    CALL CUMCHI (CHI, DF, CCUM, CUM)
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE CUMT (T, DF, CUM, CCUM)
 
!*****************************************************************************80
!
!! CUMT evaluates the cumulative T distribution.
!
!  Author:
!
!    Barry Brown, James Lovato, Kathy Russell
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    Formula 26.5.27.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) T, the upper limit of integration.
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of
!    the T distribution.
!
!    Output, real ( kind = 8 ) CUM, CCUM, the T distribution CDF and
!    complementary CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DF
    REAL (KIND=8) OMA
    REAL (KIND=8) T
    REAL (KIND=8) XX
    REAL (KIND=8) YY
 
    XX = DF / (DF+T**2)
    YY = T ** 2 / (DF+T**2)
 
    CALL CUMBET (XX, YY, 0.5D+00*DF, 0.5D+00, A, OMA)
 
    IF (T <= 0.0D+00) THEN
        CUM = 0.5D+00 * A
        CCUM = OMA + CUM
    ELSE
        CCUM = 0.5D+00 * A
        CUM = OMA + CCUM
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DBETRM (A, B)
 
!*****************************************************************************80
!
!! DBETRM computes the Sterling remainder for the complete beta function.
!
!  Discussion:
!
!    Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B)
!    where Lgamma is the log of the (complete) gamma function
!
!    Let ZZ be approximation obtained if each log gamma is approximated
!    by Sterling's formula, i.e.,
!
!      Sterling(Z) = log ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * log ( Z ) - Z
!
!    The Sterling remainder is Log(Beta(A,B)) - ZZ.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the Beta function.
!
!    Output, real ( kind = 8 ) DBETRM, the Sterling remainder.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) DBETRM
!
!  Try to sum from smallest to largest.
!
    DBETRM = - DSTREM (A+B)
    DBETRM = DBETRM + DSTREM (MAX(A, B))
    DBETRM = DBETRM + DSTREM (MIN(A, B))
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DEXPM1 (X)
 
!*****************************************************************************80
!
!! DEXPM1 evaluates the function EXP(X) - 1.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value at which exp(X)-1 is desired.
!
!    Output, real ( kind = 8 ) DEXPM1, the value of exp(X)-1.
!
    IMPLICIT NONE
 
    REAL (KIND=8) BOT
    REAL (KIND=8) DEXPM1
    REAL (KIND=8), PARAMETER :: P1 = 0.914041914819518D-09
    REAL (KIND=8), PARAMETER :: P2 = 0.238082361044469D-01
    REAL (KIND=8), PARAMETER :: Q1 = - 0.499999999085958D+00
    REAL (KIND=8), PARAMETER :: Q2 = 0.107141568980644D+00
    REAL (KIND=8), PARAMETER :: Q3 = - 0.119041179760821D-01
    REAL (KIND=8), PARAMETER :: Q4 = 0.595130811860248D-03
    REAL (KIND=8) TOP
    REAL (KIND=8) W
    REAL (KIND=8) X
 
    IF (ABS(X) <= 0.15D+00) THEN
 
        TOP = (P2*X+P1) * X + 1.0D+00
        BOT = (((Q4*X+Q3)*X+Q2)*X+Q1) * X + 1.0D+00
        DEXPM1 = X * (TOP/BOT)
 
    ELSE
 
        W = EXP (X)
 
        IF (X <= 0.0D+00) THEN
            DEXPM1 = (W-0.5D+00) - 0.5D+00
        ELSE
            DEXPM1 = W * (0.5D+00+(0.5D+00-1.0D+00/W))
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DINVNR (P, Q)
 
!*****************************************************************************80
!
!! DINVNR computes the inverse of the normal distribution.
!
!  Discussion:
!
!    This routine returns X such that
!
!      CUMNOR(X) = P,
!
!    that is, so that
!
!      P = integral ( -oo <= T <= X ) exp(-U*U/2)/sqrt(2*PI) dU
!
!    The rational function on page 95 of Kennedy and Gentle is used as a
!    starting value for the Newton method of finding roots.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the probability, and the complementary
!    probability.
!
!    Output, real ( kind = 8 ) DINVNR, the argument X for which the
!    Normal CDF has the value P.
!
    IMPLICIT NONE
 
    REAL (KIND=8) CCUM
    REAL (KIND=8) CUM
    REAL (KIND=8) DINVNR
    REAL (KIND=8) DX
    REAL (KIND=8), PARAMETER :: EPS = 1.0D-13
    INTEGER (KIND=4) I
    INTEGER (KIND=4), PARAMETER :: MAXIT = 100
    REAL (KIND=8) P
    REAL (KIND=8) PP
    REAL (KIND=8) Q
    REAL (KIND=8), PARAMETER :: R2PI = 0.3989422804014326D+00
    REAL (KIND=8) STRTX
    REAL (KIND=8) XCUR
 
    PP = MIN (P, Q)
    STRTX = STVALN (PP)
    XCUR = STRTX
!
!  Newton iterations.
!
    DO I = 1, MAXIT
 
        CALL CUMNOR (XCUR, CUM, CCUM)
        DX = (CUM-PP) / (R2PI*EXP(-0.5D+00*XCUR*XCUR))
        XCUR = XCUR - DX
 
        IF (ABS(DX/XCUR) < EPS) THEN
            IF (P <= Q) THEN
                DINVNR = XCUR
            ELSE
                DINVNR = - XCUR
            END IF
            RETURN
        END IF
 
    END DO
 
    IF (P <= Q) THEN
        DINVNR = STRTX
    ELSE
        DINVNR = - STRTX
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DINVR (STATUS, X, FX, QLEFT, QHI)
 
!*****************************************************************************80
!
!! DINVR bounds the zero of the function and invokes DZROR.
!
!  Discussion:
!
!    This routine seeks to find bounds on a root of the function and
!    invokes DZROR to perform the zero finding.  DSTINV must have been
!    called before this routine in order to set its parameters.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) STATUS.  At the beginning of a zero
!    finding problem, STATUS should be set to 0 and this routine invoked.
!    The value of parameters other than X will be ignored on this call.
!    If this routine needs the function to be evaluated, it will set STATUS
!    to 1 and return.  The value of the function should be set in FX and
!    this routine again called without changing any of its other parameters.
!    If this routine finishes without error, it returns with STATUS 0,
!    and X an approximate root of F(X).
!    If this routine cannot bound the function, it returns a negative STATUS and
!    sets QLEFT and QHI.
!
!    Output, real ( kind = 8 ) X, the value at which F(X) is to be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X) calculated by the user
!    on the previous call, when this routine returned with STATUS = 1.
!
!    Output, logical QLEFT, is defined only if QMFINV returns FALSE.  In that
!    case, QLEFT is TRUE if the stepping search terminated unsucessfully
!    at SMALL, and FALSE if the search terminated unsucessfully at BIG.
!
!    Output, logical QHI, is defined only if QMFINV returns FALSE.  In that
!    case, it is TRUE if Y < F(X) at the termination of the search and FALSE
!    if F(X) < Y.
!
    IMPLICIT NONE
 
    REAL (KIND=8) :: ABSSTP
    REAL (KIND=8) :: ABSTOL
    REAL (KIND=8) :: BIG
    REAL (KIND=8) FBIG
    REAL (KIND=8) FSMALL
    REAL (KIND=8) FX
    INTEGER (KIND=4) I99999
    LOGICAL QBDD
    LOGICAL QCOND
    LOGICAL QDUM1
    LOGICAL QDUM2
    LOGICAL QHI
    LOGICAL QINCR
    LOGICAL QLEFT
    LOGICAL QLIM
    LOGICAL QUP
    REAL (KIND=8) :: RELSTP
    REAL (KIND=8) :: RELTOL
    REAL (KIND=8) :: SMALL
    INTEGER (KIND=4) STATUS
    REAL (KIND=8) STEP
    REAL (KIND=8) :: STPMUL
    REAL (KIND=8) X
    REAL (KIND=8) XHI
    REAL (KIND=8) XLB
    REAL (KIND=8) XLO
    REAL (KIND=8) XSAVE
    REAL (KIND=8) XUB
    REAL (KIND=8) YY
    REAL (KIND=8) ZABSST
    REAL (KIND=8) ZABSTO
    REAL (KIND=8) ZBIG
    REAL (KIND=8) ZRELST
    REAL (KIND=8) ZRELTO
    REAL (KIND=8) ZSMALL
    REAL (KIND=8) ZSTPMU
 
    SAVE
 
    IF (0 < STATUS) THEN
        GO TO I99999
    END IF
 
    QCOND = .NOT. (SMALL <= X .AND. X <= BIG)
 
    IF ( .NOT. (SMALL <= X .AND. X <= BIG)) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DINVR - Fatal error!'
        WRITE (*, '(a)') '  The values SMALL, X, BIG are not monotone.'
        RETURN
    END IF
 
    XSAVE = X
!
!  See that SMALL and BIG bound the zero and set QINCR.
!
    X = SMALL
!
!  GET-function-VALUE
!
    ASSIGN 10 TO I99999
    STATUS = 1
    RETURN
 
10  CONTINUE
 
    FSMALL = FX
    X = BIG
!
!  GET-function-VALUE
!
    ASSIGN 20 TO I99999
    STATUS = 1
    RETURN
 
20  CONTINUE
 
    FBIG = FX
 
    QINCR = (FSMALL < FBIG)
 
    IF (FSMALL <= FBIG) THEN
 
        IF (0.0D+00 < FSMALL) THEN
            STATUS = - 1
            QLEFT = .TRUE.
            QHI = .TRUE.
            RETURN
        END IF
 
        IF (FBIG < 0.0D+00) THEN
            STATUS = - 1
            QLEFT = .FALSE.
            QHI = .FALSE.
            RETURN
        END IF
 
    ELSE IF (FBIG < FSMALL) THEN
 
        IF (FSMALL < 0.0D+00) THEN
            STATUS = - 1
            QLEFT = .TRUE.
            QHI = .FALSE.
            RETURN
        END IF
 
        IF (0.0D+00 < FBIG) THEN
            STATUS = - 1
            QLEFT = .FALSE.
            QHI = .TRUE.
            RETURN
        END IF
 
    END IF
 
    X = XSAVE
    STEP = MAX (ABSSTP, RELSTP*ABS(X))
!
!  YY = F(X) - Y
!  GET-function-VALUE
!
    ASSIGN 90 TO I99999
    STATUS = 1
    RETURN
 
90  CONTINUE
 
    YY = FX
 
    IF (YY == 0.0D+00) THEN
        STATUS = 0
        RETURN
    END IF
 
100 CONTINUE
 
    QUP = (QINCR .AND. (YY < 0.0D+00)) .OR. ( .NOT. QINCR .AND. (0.0D+00 < YY))
!
!  Handle case in which we must step higher.
!
    IF ( .NOT. QUP) THEN
        GO TO 170
    END IF
 
    XLB = XSAVE
    XUB = MIN (XLB+STEP, BIG)
    GO TO 120
 
110 CONTINUE
 
    IF (QCOND) THEN
        GO TO 150
    END IF
!
!  YY = F(XUB) - Y
!
120 CONTINUE
 
    X = XUB
!
!  GET-function-VALUE
!
    ASSIGN 130 TO I99999
    STATUS = 1
    RETURN
 
130 CONTINUE
 
    YY = FX
    QBDD = (QINCR .AND. (0.0D+00 <= YY)) .OR. ( .NOT. QINCR .AND. (YY <= 0.0D+00))
    QLIM = (BIG <= XUB)
    QCOND = QBDD .OR. QLIM
 
    IF ( .NOT. QCOND) THEN
        STEP = STPMUL * STEP
        XLB = XUB
        XUB = MIN (XLB+STEP, BIG)
    END IF
 
    GO TO 110
 
150 CONTINUE
 
    IF (QLIM .AND. .NOT. QBDD) THEN
        STATUS = - 1
        QLEFT = .FALSE.
        QHI = .NOT. QINCR
        X = BIG
        RETURN
    END IF
 
160 CONTINUE
 
    GO TO 240
!
!  Handle the case in which we must step lower.
!
170 CONTINUE
 
    XUB = XSAVE
    XLB = MAX (XUB-STEP, SMALL)
    GO TO 190
 
180 CONTINUE
 
    IF (QCOND) THEN
        GO TO 220
    END IF
!
!  YY = F(XLB) - Y
!
190 CONTINUE
 
    X = XLB
!
!  GET-function-VALUE
!
    ASSIGN 200 TO I99999
    STATUS = 1
    RETURN
 
200 CONTINUE
 
    YY = FX
    QBDD = (QINCR .AND. (YY <= 0.0D+00)) .OR. ( .NOT. QINCR .AND. (0.0D+00 <= YY))
    QLIM = XLB <= SMALL
    QCOND = QBDD .OR. QLIM
 
    IF ( .NOT. QCOND) THEN
        STEP = STPMUL * STEP
        XUB = XLB
        XLB = MAX (XUB-STEP, SMALL)
    END IF
 
    GO TO 180
 
220 CONTINUE
 
    IF (QLIM .AND. ( .NOT. QBDD)) THEN
        STATUS = - 1
        QLEFT = .TRUE.
        QHI = QINCR
        X = SMALL
        RETURN
    END IF
 
230 CONTINUE
240 CONTINUE
 
    CALL DSTZR (XLB, XUB, ABSTOL, RELTOL)
!
!  If we reach here, XLB and XUB bound the zero of F.
!
    STATUS = 0
    GO TO 260
 
250 CONTINUE
 
    IF (STATUS /= 1) THEN
        X = XLO
        STATUS = 0
        RETURN
    END IF
 
260 CONTINUE
 
    CALL DZROR (STATUS, X, FX, XLO, XHI, QDUM1, QDUM2)
 
    IF (STATUS /= 1) THEN
        GO TO 250
    END IF
!
!  GET-function-VALUE
!
    ASSIGN 270 TO I99999
    STATUS = 1
    RETURN
 
270 CONTINUE
    GO TO 250
 
    ENTRY DSTINV (ZSMALL, ZBIG, ZABSST, ZRELST, ZSTPMU, ZABSTO, ZRELTO)
 
!*****************************************************************************80
!
!! DSTINV SeT INverse finder - Reverse Communication
!
!  Discussion:
!
!    This routine is given a monotone function F, and a value Y,
!    and seeks an argument value X such that F(X) = Y.
!
!    This routine uses reverse communication -- see DINVR.
!    This routine sets quantities needed by DINVR.
!
!    F must be a monotone function, the results of QMFINV are
!    otherwise undefined.  QINCR must be TRUE if F is nondecreasing
!    and FALSE if F is nonincreasing.
!
!    QMFINV will return TRUE if and only if F(SMALL) and
!    F(BIG) bracket Y, i. e.,
!      QINCR is TRUE and F(SMALL) <= Y <= F(BIG) or
!      QINCR is FALSE and F(BIG) <= Y <= F(SMALL)
!
!    If QMFINV returns TRUE, then the X returned satisfies
!    the following condition.  Let
!      TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!    then if QINCR is TRUE,
!      F(X-TOL(X)) <= Y <= F(X+TOL(X))
!    and if QINCR is FALSE
!      F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X))
!
!    Compares F(X) with Y for the input value of X then uses QINCR
!    to determine whether to step left or right to bound the
!    desired X.  The initial step size is
!
!      max ( ABSSTP, RELSTP * ABS ( S ) )
!
!    for the input value of X.
!
!    Iteratively steps right or left until it bounds X.
!    At each step which doesn't bound X, the step size is doubled.
!    The routine is careful never to step beyond SMALL or BIG.  If
!    it hasn't bounded X at SMALL or BIG, QMFINV returns FALSE
!    after setting QLEFT and QHI.
!
!    If X is successfully bounded then Algorithm R of the paper
!    Bus and Dekker is employed to find the zero of the function F(X)-Y.
!    This is routine QRZERO.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) ZSMALL, ZBIG, the left and right endpoints
!    of the interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ZABSST, ZRELSTP, the initial step size in
!    the search is max ( ZABSST, ZRELST * abs ( X ) ).
!
!    Input, real ( kind = 8 ) STPMUL.  When a step doesn't bound the zero,
!    the stepsize is multiplied by STPMUL and another step taken.  A
!    popular value is 2.0.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution
!
    SMALL = ZSMALL
    BIG = ZBIG
    ABSSTP = ZABSST
    RELSTP = ZRELST
    STPMUL = ZSTPMU
    ABSTOL = ZABSTO
    RELTOL = ZRELTO
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DLANOR (X)
 
!*****************************************************************************80
!
!! DLANOR evaluates the logarithm of the asymptotic Normal CDF.
!
!  Discussion:
!
!    This routine computes the logarithm of the cumulative normal distribution
!    from abs ( x ) to infinity for  5 <= abs ( X ).
!
!    The relative error at X = 5 is about 0.5D-5.
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions
!    1966, Formula 26.2.12.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the value at which the Normal CDF is to be
!    evaluated.  It is assumed that 5 <= abs ( X ).
!
!    Output, real ( kind = 8 ) DLANOR, the logarithm of the asymptotic
!    Normal CDF.
!
    IMPLICIT NONE
 
    REAL (KIND=8) APPROX
    REAL (KIND=8), SAVE, DIMENSION (0:11) :: COEF = (/ - 1.0D+00, 3.0D+00, - 15.0D+00, &
   & 105.0D+00, - 945.0D+00, 10395.0D+00, - 135135.0D+00, 2027025.0D+00, - 34459425.0D+00, &
   & 654729075.0D+00, - 13749310575D+00, 316234143225.0D+00 /)
    REAL (KIND=8) CORREC
    REAL (KIND=8), PARAMETER :: DLSQPI = 0.91893853320467274177D+00
    REAL (KIND=8) DLANOR
    REAL (KIND=8) X
    REAL (KIND=8) XX
    REAL (KIND=8) XX2
 
    XX = ABS (X)
 
    IF (ABS(X) < 5.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DLANOR - Fatal error!'
        WRITE (*, '(a)') '  The argument X is too small.'
    END IF
 
    APPROX = - DLSQPI - 0.5D+00 * X ** 2 - LOG (ABS(X))
 
    XX2 = XX * XX
    CORREC = EVAL_POL (COEF, 11, 1.0D+00/XX2) / XX2
    CORREC = ALNREL (CORREC)
 
    DLANOR = APPROX + CORREC
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DSTREM (Z)
 
!*****************************************************************************80
!
!! DSTREM computes the Sterling remainder ln ( Gamma ( Z ) ) - Sterling ( Z ).
!
!  Discussion:
!
!    This routine returns
!
!      ln ( Gamma ( Z ) ) - Sterling ( Z )
!
!    where Sterling(Z) is Sterling's approximation to ln ( Gamma ( Z ) ).
!
!    Sterling(Z) = ln ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * ln ( Z ) - Z
!
!    If 6 <= Z, the routine uses 9 terms of a series in Bernoulli numbers,
!    with values calculated using Maple.
!
!    Otherwise, the difference is computed explicitly.
!
!  Modified:
!
!    14 June 2004
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, the value at which the Sterling
!    remainder is to be calculated.  Z must be positive.
!
!    Output, real ( kind = 8 ) DSTREM, the Sterling remainder.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: NCOEF = 9
 
    REAL (KIND=8), PARAMETER, DIMENSION (0:NCOEF) :: COEF = (/ 0.0D+00, &
   & 0.0833333333333333333333333333333D+00, - 0.00277777777777777777777777777778D+00, &
   & 0.000793650793650793650793650793651D+00, - 0.000595238095238095238095238095238D+00, &
   & 0.000841750841750841750841750841751D+00, - 0.00191752691752691752691752691753D+00, &
   & 0.00641025641025641025641025641026D+00, - 0.0295506535947712418300653594771D+00, &
   & 0.179644372368830573164938490016D+00 /)
    REAL (KIND=8) DSTREM
    REAL (KIND=8), PARAMETER :: HLN2PI = 0.91893853320467274178D+00
    REAL (KIND=8) STERL
    REAL (KIND=8) Z
 
    IF (Z <= 0.0D+00) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'DSTREM - Fatal error!'
        WRITE (*, '(a)') '  Zero or negative argument Z.'
        RETURN
    END IF
 
    IF (6.0D+00 < Z) THEN
        DSTREM = EVAL_POL (COEF, NCOEF, 1.0D+00/Z**2) * Z
    ELSE
        STERL = HLN2PI + (Z-0.5D+00) * LOG (Z) - Z
        DSTREM = GAMMA_LOG (Z) - STERL
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION DT1 (P, Q, DF)
 
!*****************************************************************************80
!
!! DT1 computes an approximate inverse of the cumulative T distribution.
!
!  Discussion:
!
!    This routine returns the inverse of the T distribution function, that is,
!    the integral from 0 to INVT of the T density is P.  This is an
!    initial approximation.
!
!    Thanks to Charles Katholi for pointing out that the RESHAPE
!    function should not use a range in the "SHAPE" field (0:4,4),
!    but simply the number of rows and columns (5,4), JVB, 04 May 2006.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, Q, the value whose inverse from the
!    T distribution CDF is desired, and the value (1-P).
!
!    Input, real ( kind = 8 ) DF, the number of degrees of freedom of the
!    T distribution.
!
!    Output, real ( kind = 8 ) DT1, the approximate value of X for which
!    the T density CDF with DF degrees of freedom has value P.
!
    IMPLICIT NONE
 
    REAL (KIND=8), DIMENSION (0:4, 4) :: COEF = RESHAPE ( (/ 1.0D+00, 1.0D+00, 0.0D+00, &
   & 0.0D+00, 0.0D+00, 3.0D+00, 16.0D+00, 5.0D+00, 0.0D+00, 0.0D+00,-15.0D+00, 17.0D+00, &
   & 19.0D+00, 3.0D+00, 0.0D+00,-945.0D+00,-1920.0D+00, 1482.0D+00, 776.0D+00, 79.0D+00 /), (/ &
   & 5, 4 /))
    REAL (KIND=8), PARAMETER, DIMENSION (4) :: DENOM = (/ 4.0D+00, 96.0D+00, 384.0D+00, &
   & 92160.0D+00 /)
    REAL (KIND=8) DENPOW
    REAL (KIND=8) DF
    REAL (KIND=8) DT1
    INTEGER (KIND=4) I
    INTEGER (KIND=4), PARAMETER, DIMENSION (4) :: IDEG = (/ 1, 2, 3, 4 /)
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) SUM1
    REAL (KIND=8) TERM
    REAL (KIND=8) X
    REAL (KIND=8) XP
    REAL (KIND=8) XX
 
    X = ABS (DINVNR(P, Q))
    XX = X * X
 
    SUM1 = X
    DENPOW = 1.0D+00
    DO I = 1, 4
        TERM = EVAL_POL (COEF(0, I), IDEG(I), XX) * X
        DENPOW = DENPOW * DF
        SUM1 = SUM1 + TERM / (DENPOW*DENOM(I))
    END DO
 
    IF (0.5D+00 <= P) THEN
        XP = SUM1
    ELSE
        XP = - SUM1
    END IF
 
    DT1 = XP
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE DZROR (STATUS, X, FX, XLO, XHI, QLEFT, QHI)
 
!*****************************************************************************80
!
!! DZROR seeks a zero of a function, using reverse communication.
!
!  Discussion:
!
!    This routine performs the zero finding.  STZROR must have been called
!    before this routine in order to set its parameters.
!
!  Modified:
!
!    09 June 2004
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) STATUS.  At the beginning of a zero
!    finding problem, STATUS should be set to 0 and ZROR invoked.  The value
!    of other parameters will be ignored on this call.
!    When ZROR needs the function evaluated, it will set
!    STATUS to 1 and return.  The value of the function
!    should be set in FX and ZROR again called without
!    changing any of its other parameters.
!    When ZROR has finished without error, it will return
!    with STATUS 0.  In that case (XLO,XHI) bound the answe
!    If ZROR finds an error (which implies that F(XLO)-Y an
!    F(XHI)-Y have the same sign, it returns STATUS -1.  In
!    this case, XLO and XHI are undefined.
!
!    Output, real ( kind = 8 ) X, the value of X at which F(X) is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) FX, the value of F(X), which must be calculated
!    by the user when ZROR has returned on the previous call with STATUS = 1.
!
!    Output, real ( kind = 8 ) XLO, XHI, are lower and upper bounds for the
!    solution when ZROR returns with STATUS = 0.
!
!    Output, logical QLEFT,is TRUE if the stepping search terminated
!    unsucessfully at XLO.  If it is FALSE, the search terminated
!    unsucessfully at XHI.
!
!    Output, logical QHI, is TRUE if Y < F(X) at the termination of the
!    search and FALSE if F(X) < Y at the termination of the search.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) ABSTOL
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) D
    INTEGER (KIND=4) EXT
    REAL (KIND=8) FA
    REAL (KIND=8) FB
    REAL (KIND=8) FC
    REAL (KIND=8) FD
    REAL (KIND=8) FDA
    REAL (KIND=8) FDB
    LOGICAL FIRST
    REAL (KIND=8) FTOL
    REAL (KIND=8) FX
    INTEGER (KIND=4) I99999
    REAL (KIND=8) M
    REAL (KIND=8) MB
    REAL (KIND=8) P
    REAL (KIND=8) Q
    LOGICAL QHI
    LOGICAL QLEFT
    LOGICAL QRZERO
    REAL (KIND=8) RELTOL
    INTEGER (KIND=4) STATUS
    REAL (KIND=8) TOL
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) XHI
    REAL (KIND=8) XLO
    REAL (KIND=8) :: XXHI = 0.0D+00
    REAL (KIND=8) :: XXLO = 0.0D+00
    REAL (KIND=8) ZABSTL
    REAL (KIND=8) ZRELTL
    REAL (KIND=8) ZX
    REAL (KIND=8) ZXHI
    REAL (KIND=8) ZXLO
 
    SAVE
 
    FTOL (ZX) = 0.5D+00 * MAX (ABSTOL, RELTOL*ABS(ZX))
 
    IF (0 < STATUS) THEN
        GO TO 280
    END IF
 
    XLO = XXLO
    XHI = XXHI
    B = XLO
    X = XLO
!
!     GET-function-VALUE
!
    ASSIGN 10 TO I99999
    GO TO 270
 
10  CONTINUE
 
    FB = FX
    XLO = XHI
    A = XLO
    X = XLO
!
!     GET-function-VALUE
!
    ASSIGN 20 TO I99999
    GO TO 270
!
!  Check that F(ZXLO) < 0 < F(ZXHI)  or F(ZXLO) > 0 > F(ZXHI)
!
20  CONTINUE
 
    IF (FB < 0.0D+00) THEN
        IF (FX < 0.0D+00) THEN
            STATUS = - 1
            QLEFT = (FX < FB)
            QHI = .FALSE.
            RETURN
        END IF
    END IF
 
    IF (0.0D+00 < FB) THEN
        IF (0.0D+00 < FX) THEN
            STATUS = - 1
            QLEFT = (FB < FX)
            QHI = .TRUE.
            RETURN
        END IF
    END IF
 
    FA = FX
    FIRST = .TRUE.
 
70  CONTINUE
 
    C = A
    FC = FA
    EXT = 0
 
80  CONTINUE
 
    IF (ABS(FC) < ABS(FB)) THEN
 
        IF (C == A) THEN
            D = A
            FD = FA
        END IF
 
        A = B
        FA = FB
        XLO = C
        B = XLO
        FB = FC
        C = A
        FC = FA
 
    END IF
 
    TOL = FTOL (XLO)
    M = (C+B) * 0.5D+00
    MB = M - B
 
    IF ( .NOT. (TOL < ABS(MB))) THEN
        GO TO 240
    END IF
 
    IF (3 < EXT) THEN
        W = MB
        GO TO 190
    END IF
 
110 CONTINUE
 
    TOL = SIGN (TOL, MB)
    P = (B-A) * FB
!
!  I had to insert a rudimentary check on the divisions here
!  to avoid ninny errors, JVB, 09 June 2004.
!
    IF (FIRST) THEN
 
        Q = FA - FB
        FIRST = .FALSE.
 
    ELSE
 
        IF (D == B) THEN
            FDB = 1.0D+00
        ELSE
            FDB = (FD-FB) / (D-B)
        END IF
 
        IF (D == A) THEN
            FDA = 1.0D+00
        ELSE
            FDA = (FD-FA) / (D-A)
        END IF
 
        P = FDA * P
        Q = FDB * FA - FDA * FB
 
    END IF
 
130 CONTINUE
 
    IF (P < 0.0D+00) THEN
        P = - P
        Q = - Q
    END IF
 
140 CONTINUE
 
    IF (EXT == 3) THEN
        P = P * 2.0D+00
    END IF
 
    IF ( .NOT. ((P*1.0D+00) == 0.0D+00 .OR. P <= (Q*TOL))) THEN
        GO TO 150
    END IF
 
    W = TOL
    GO TO 180
 
150 CONTINUE
 
    IF (P < MB*Q) THEN
        W = P / Q
    ELSE
        W = MB
    END IF
 
180 CONTINUE
190 CONTINUE
 
    D = A
    FD = FA
    A = B
    FA = FB
    B = B + W
    XLO = B
    X = XLO
!
!  GET-function-VALUE
!
    ASSIGN 200 TO I99999
    GO TO 270
 
200 CONTINUE
 
    FB = FX
 
    IF (0.0D+00 <= FC*FB) THEN
 
        GO TO 70
 
    ELSE
 
        IF (W == MB) THEN
            EXT = 0
        ELSE
            EXT = EXT + 1
        END IF
 
        GO TO 80
 
    END IF
 
240 CONTINUE
 
    XHI = C
    QRZERO = (0.0D+00 <= FC .AND. FB <= 0.0D+00) .OR. (FC < 0.0D+00 .AND. FB >=0.0D+00)
 
    IF (QRZERO) THEN
        STATUS = 0
    ELSE
        STATUS = - 1
    END IF
 
    RETURN
 
    ENTRY DSTZR (ZXLO, ZXHI, ZABSTL, ZRELTL)
 
!*****************************************************************************80
!
!! DSTZR - SeT ZeRo finder - Reverse communication version
!
!  Discussion:
!
!    This routine sets quantities needed by ZROR.  The function of ZROR
!    and the quantities set is given here.
!
!    Given a function F, find XLO such that F(XLO) = 0.
!
!     Input condition. F is a real ( kind = 8 ) function of a single
!     real ( kind = 8 ) argument and XLO and XHI are such that
!          F(XLO)*F(XHI)  <=  0.0
!
!     If the input condition is met, QRZERO returns .TRUE.
!     and output values of XLO and XHI satisfy the following
!          F(XLO)*F(XHI)  <= 0.
!          ABS ( F(XLO) ) <= ABS ( F(XHI) )
!          ABS ( XLO - XHI ) <= TOL(X)
!     where
!          TOL(X) = MAX ( ABSTOL, RELTOL * ABS ( X ) )
!
!     If this algorithm does not find XLO and XHI satisfying
!     these conditions then QRZERO returns .FALSE.  This
!     implies that the input condition was not met.
!
!  Reference:
!
!    JCP Bus, TJ Dekker,
!    Two Efficient Algorithms with Guaranteed Convergence for
!    Finding a Zero of a Function,
!    ACM Transactions on Mathematical Software,
!    Volume 1, Number 4, pages 330-345, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XLO, XHI, the left and right endpoints of the
!    interval to be searched for a solution.
!
!    Input, real ( kind = 8 ) ABSTOL, RELTOL, two numbers that determine
!    the accuracy of the solution.
!
    XXLO = ZXLO
    XXHI = ZXHI
    ABSTOL = ZABSTL
    RELTOL = ZRELTL
    RETURN
!
!     TO GET-function-VALUE
!
270 STATUS = 1
    RETURN
 
280 CONTINUE
    GO TO I99999
 
END
 
!******************************************************************************
 
SUBROUTINE ERF_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! ERF_VALUES returns some values of the ERF or "error" function.
!
!  Discussion:
!
!    ERF(X) = ( 2 / sqrt ( PI ) * integral ( 0 <= T <= X ) exp ( - T^2 ) dT
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
!    US Department of Commerce, 1964.
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
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 21
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.0000000000D+00, 0.1124629160D+00, &
   & 0.2227025892D+00, 0.3286267595D+00, 0.4283923550D+00, 0.5204998778D+00, 0.6038560908D+00, &
   & 0.6778011938D+00, 0.7421009647D+00, 0.7969082124D+00, 0.8427007929D+00, 0.8802050696D+00, &
   & 0.9103139782D+00, 0.9340079449D+00, 0.9522851198D+00, 0.9661051465D+00, 0.9763483833D+00, &
   & 0.9837904586D+00, 0.9890905016D+00, 0.9927904292D+00, 0.9953222650D+00 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.0D+00, 0.1D+00, 0.2D+00, 0.3D+00, &
   & 0.4D+00, 0.5D+00, 0.6D+00, 0.7D+00, 0.8D+00, 0.9D+00, 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, &
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
 
FUNCTION ERROR_F (X)
 
!*****************************************************************************80
!
!! ERROR_F evaluates the error function.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) )
!        * Integral ( 0 <= T <= X ) EXP ( - T**2 ) dT.
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
!  Modified:
!
!    17 November 2006
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) ERF, the value of the error function at X.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER, DIMENSION (5) :: A = (/ 0.771058495001320D-04, - &
   & 0.133733772997339D-02, 0.323076579225834D-01, 0.479137145607681D-01, 0.128379167095513D+00 &
   & /)
    REAL (KIND=8) AX
    REAL (KIND=8), PARAMETER, DIMENSION (3) :: B = (/ 0.301048631703895D-02, &
   & 0.538971687740286D-01, 0.375795757275549D+00 /)
    REAL (KIND=8) BOT
    REAL (KIND=8), PARAMETER :: C = 0.564189583547756D+00
    REAL (KIND=8) ERROR_F
    REAL (KIND=8), DIMENSION (8) :: P = (/ - 1.36864857382717D-07, 5.64195517478974D-01, &
   & 7.21175825088309D+00, 4.31622272220567D+01, 1.52989285046940D+02, 3.39320816734344D+02, &
   & 4.51918953711873D+02, 3.00459261020162D+02 /)
    REAL (KIND=8), DIMENSION (8) :: Q = (/ 1.00000000000000D+00, 1.27827273196294D+01, &
   & 7.70001529352295D+01, 2.77585444743988D+02, 6.38980264465631D+02, 9.31354094850610D+02, &
   & 7.90950925327898D+02, 3.00459260956983D+02 /)
    REAL (KIND=8), DIMENSION (5) :: R = (/ 2.10144126479064D+00, 2.62370141675169D+01, &
   & 2.13688200555087D+01, 4.65807828718470D+00, 2.82094791773523D-01 /)
    REAL (KIND=8), PARAMETER, DIMENSION (4) :: S = (/ 9.41537750555460D+01, &
   & 1.87114811799590D+02, 9.90191814623914D+01, 1.80124575948747D+02 /)
    REAL (KIND=8) T
    REAL (KIND=8) TOP
    REAL (KIND=8) X
    REAL (KIND=8) X2
 
    AX = ABS (X)
 
    IF (AX <= 0.5D+00) THEN
 
        T = X * X
 
        TOP = ((((A(1)*T+A(2))*T+A(3))*T+A(4))*T+A(5)) + 1.0D+00
 
        BOT = ((B(1)*T+B(2))*T+B(3)) * T + 1.0D+00
        ERROR_F = AX * (TOP/BOT)
 
    ELSE IF (AX <= 4.0D+00) THEN
 
        TOP = ((((((P(1)*AX+P(2))*AX+P(3))*AX+P(4))*AX+P(5))*AX+P(6))*AX+P(7)) * AX + P (8)
 
        BOT = ((((((Q(1)*AX+Q(2))*AX+Q(3))*AX+Q(4))*AX+Q(5))*AX+Q(6))*AX+Q(7)) * AX + Q (8)
 
        ERROR_F = 0.5D+00 + (0.5D+00-EXP(-X*X)*TOP/BOT)
 
    ELSE IF (AX < 5.8D+00) THEN
 
        X2 = X * X
        T = 1.0D+00 / X2
 
        TOP = (((R(1)*T+R(2))*T+R(3))*T+R(4)) * T + R (5)
 
        BOT = (((S(1)*T+S(2))*T+S(3))*T+S(4)) * T + 1.0D+00
 
        ERROR_F = (C-TOP/(X2*BOT)) / AX
        ERROR_F = 0.5D+00 + (0.5D+00-EXP(-X2)*ERROR_F)
 
    ELSE
 
        ERROR_F = 1.0D+00
 
    END IF
 
    IF (X < 0.0D+00) THEN
        ERROR_F = - ERROR_F
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ERROR_FC (IND, X)
 
!*****************************************************************************80
!
!! ERROR_FC evaluates the complementary error function.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IND, chooses the scaling.
!    If IND is nonzero, then the value returned has been multiplied by
!    EXP(X*X).
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) ERROR_FC, the value of the complementary
!    error function.
!
    IMPLICIT NONE
 
    REAL (KIND=8), DIMENSION (5) :: A = (/ 0.771058495001320D-04, - 0.133733772997339D-02, &
   & 0.323076579225834D-01, 0.479137145607681D-01, 0.128379167095513D+00 /)
    REAL (KIND=8) AX
    REAL (KIND=8), DIMENSION (3) :: B = (/ 0.301048631703895D-02, 0.538971687740286D-01, &
   & 0.375795757275549D+00 /)
    REAL (KIND=8) BOT
    REAL (KIND=8), PARAMETER :: C = 0.564189583547756D+00
    REAL (KIND=8) E
    REAL (KIND=8) ERROR_FC
    INTEGER (KIND=4) IND
    REAL (KIND=8), DIMENSION (8) :: P = (/ - 1.36864857382717D-07, 5.64195517478974D-01, &
   & 7.21175825088309D+00, 4.31622272220567D+01, 1.52989285046940D+02, 3.39320816734344D+02, &
   & 4.51918953711873D+02, 3.00459261020162D+02 /)
    REAL (KIND=8), DIMENSION (8) :: Q = (/ 1.00000000000000D+00, 1.27827273196294D+01, &
   & 7.70001529352295D+01, 2.77585444743988D+02, 6.38980264465631D+02, 9.31354094850610D+02, &
   & 7.90950925327898D+02, 3.00459260956983D+02 /)
    REAL (KIND=8), DIMENSION (5) :: R = (/ 2.10144126479064D+00, 2.62370141675169D+01, &
   & 2.13688200555087D+01, 4.65807828718470D+00, 2.82094791773523D-01 /)
    REAL (KIND=8), DIMENSION (4) :: S = (/ 9.41537750555460D+01, 1.87114811799590D+02, &
   & 9.90191814623914D+01, 1.80124575948747D+02 /)
    REAL (KIND=8) T
    REAL (KIND=8) TOP
    REAL (KIND=8) W
    REAL (KIND=8) X
!
!  ABS ( X ) <= 0.5
!
    AX = ABS (X)
 
    IF (AX <= 0.5D+00) THEN
 
        T = X * X
 
        TOP = ((((A(1)*T+A(2))*T+A(3))*T+A(4))*T+A(5)) + 1.0D+00
 
        BOT = ((B(1)*T+B(2))*T+B(3)) * T + 1.0D+00
 
        ERROR_FC = 0.5D+00 + (0.5D+00-X*(TOP/BOT))
 
        IF (IND /= 0) THEN
            ERROR_FC = EXP (T) * ERROR_FC
        END IF
 
        RETURN
 
    END IF
!
!  0.5 < abs ( X ) <= 4
!
    IF (AX <= 4.0D+00) THEN
 
        TOP = ((((((P(1)*AX+P(2))*AX+P(3))*AX+P(4))*AX+P(5))*AX+P(6))*AX+P(7)) * AX + P (8)
 
        BOT = ((((((Q(1)*AX+Q(2))*AX+Q(3))*AX+Q(4))*AX+Q(5))*AX+Q(6))*AX+Q(7)) * AX + Q (8)
 
        ERROR_FC = TOP / BOT
!
!  4 < ABS ( X )
!
    ELSE
 
        IF (X <=-5.6D+00) THEN
 
            IF (IND == 0) THEN
                ERROR_FC = 2.0D+00
            ELSE
                ERROR_FC = 2.0D+00 * EXP (X*X)
            END IF
 
            RETURN
 
        END IF
 
        IF (IND == 0) THEN
 
            IF (100.0D+00 < X) THEN
                ERROR_FC = 0.0D+00
                RETURN
            END IF
 
            IF (-EXPARG(1) < X*X) THEN
                ERROR_FC = 0.0D+00
                RETURN
            END IF
 
        END IF
 
        T = (1.0D+00/X) ** 2
 
        TOP = (((R(1)*T+R(2))*T+R(3))*T+R(4)) * T + R (5)
 
        BOT = (((S(1)*T+S(2))*T+S(3))*T+S(4)) * T + 1.0D+00
 
        ERROR_FC = (C-T*TOP/BOT) / AX
 
    END IF
!
!  Final assembly.
!
    IF (IND /= 0) THEN
 
        IF (X < 0.0D+00) THEN
            ERROR_FC = 2.0D+00 * EXP (X*X) - ERROR_FC
        END IF
 
    ELSE
 
        W = X * X
        T = W
        E = W - T
        ERROR_FC = ((0.5D+00+(0.5D+00-E))*EXP(-T)) * ERROR_FC
 
        IF (X < 0.0D+00) THEN
            ERROR_FC = 2.0D+00 - ERROR_FC
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION ESUM (MU, X)
 
!*****************************************************************************80
!
!! ESUM evaluates exp ( MU + X ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MU, part of the argument.
!
!    Input, real ( kind = 8 ) X, part of the argument.
!
!    Output, real ( kind = 8 ) ESUM, the value of exp ( MU + X ).
!
    IMPLICIT NONE
 
    REAL (KIND=8) ESUM
    INTEGER (KIND=4) MU
    REAL (KIND=8) W
    REAL (KIND=8) X
 
    IF (X <= 0.0D+00) THEN
        IF (0 <= MU) THEN
            W = MU + X
            IF (W <= 0.0D+00) THEN
                ESUM = EXP (W)
                RETURN
            END IF
        END IF
    ELSE IF (0.0D+00 < X) THEN
        IF (MU <= 0) THEN
            W = MU + X
            IF (0.0D+00 <= W) THEN
                ESUM = EXP (W)
                RETURN
            END IF
        END IF
    END IF
 
    W = MU
    ESUM = EXP (W) * EXP (X)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EVAL_POL (A, N, X)
 
!*****************************************************************************80
!
!! EVAL_POL evaluates a polynomial at X.
!
!  Discussion:
!
!    EVAL_POL = A(0) + A(1)*X + ... + A(N)*X**N
!
!  Modified:
!
!    15 December 1999
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A(0:N), coefficients of the polynomial.
!
!    Input, integer ( kind = 4 ) N, length of A.
!
!    Input, real ( kind = 8 ) X, the point at which the polynomial
!    is to be evaluated.
!
!    Output, real ( kind = 8 ) EVAL_POL, the value of the polynomial at X.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) N
 
    REAL (KIND=8) A (0:N)
    REAL (KIND=8) EVAL_POL
    INTEGER (KIND=4) I
    REAL (KIND=8) TERM
    REAL (KIND=8) X
 
    TERM = A (N)
    DO I = N - 1, 0, - 1
        TERM = TERM * X + A (I)
    END DO
 
    EVAL_POL = TERM
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION EXPARG (L)
 
!*****************************************************************************80
!
!! EXPARG returns the largest or smallest legal argument for EXP.
!
!  Discussion:
!
!    Only an approximate limit for the argument of EXP is desired.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) L, indicates which limit is desired.
!    If L = 0, then the largest positive argument for EXP is desired.
!    Otherwise, the largest negative argument for EXP for which the
!    result is nonzero is desired.
!
!    Output, real ( kind = 8 ) EXPARG, the desired value.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) B
    REAL (KIND=8) EXPARG
    INTEGER (KIND=4) L
    REAL (KIND=8) LNB
    INTEGER (KIND=4) M
!
!  Get the arithmetic base.
!
    B = IPMPAR (4)
!
!  Compute the logarithm of the arithmetic base.
!
    IF (B == 2) THEN
        LNB = 0.69314718055995D+00
    ELSE IF (B == 8) THEN
        LNB = 2.0794415416798D+00
    ELSE IF (B == 16) THEN
        LNB = 2.7725887222398D+00
    ELSE
        LNB = LOG (REAL(B, KIND=8))
    END IF
 
    IF (L /= 0) THEN
        M = IPMPAR (9) - 1
        EXPARG = 0.99999D+00 * (M*LNB)
    ELSE
        M = IPMPAR (10)
        EXPARG = 0.99999D+00 * (M*LNB)
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
!    The value of F_CDF ( DFN, DFD, X ) can be evaluated in Mathematica by
!    commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[FRatioDistribution[ DFN, DFD ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, integer ( kind = 4 ) A, integer B, real ( kind = 8 ) X, the
!    arguments of the function.
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
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.500000D+00, 0.499971D+00, &
   & 0.499603D+00, 0.749699D+00, 0.750466D+00, 0.751416D+00, 0.899987D+00, 0.899713D+00, &
   & 0.900285D+00, 0.950025D+00, 0.950057D+00, 0.950193D+00, 0.975013D+00, 0.990002D+00, &
   & 0.994998D+00, 0.999000D+00, 0.568799D+00, 0.535145D+00, 0.514343D+00, 0.500000D+00 /)
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
 
SUBROUTINE F_NONCENTRAL_CDF_VALUES (N_DATA, A, B, LAMBDA, X, FX)
 
!*****************************************************************************80
!
!! F_NONCENTRAL_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    The value of NONCENTRAL_F_CDF ( DFN, DFD, LAMDA, X ) can be evaluated
!    in Mathematica by commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[NoncentralFRatioDistribution[ DFN, DFD, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, integer ( kind = 4 ) A, B, real ( kind = 8 ) LAMBDA, the
!    parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 22
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, &
   & 3, 3, 4, 4, 5, 5, 6, 6, 8, 16 /)
    INTEGER (KIND=4) B
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: B_VEC = (/ 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, &
   & 10, 5, 5, 5, 5, 1, 5, 6, 12, 16, 8 /)
    REAL (KIND=8) FX
    REAL, SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.500000D+00, 0.636783D+00, 0.584092D+00, &
   & 0.323443D+00, 0.450119D+00, 0.607888D+00, 0.705928D+00, 0.772178D+00, 0.819105D+00, &
   & 0.317035D+00, 0.432722D+00, 0.450270D+00, 0.426188D+00, 0.337744D+00, 0.422911D+00, &
   & 0.692767D+00, 0.363217D+00, 0.421005D+00, 0.426667D+00, 0.446402D+00, 0.844589D+00, &
   & 0.816368D+00 /)
    REAL (KIND=8) LAMBDA
    REAL, SAVE, DIMENSION (N_MAX) :: LAMBDA_VEC = (/ 0.00D+00, 0.000D+00, 0.25D+00, 1.00D+00, &
   & 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
   & 2.00D+00, 1.00D+00, 1.00D+00, 0.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00 &
   & /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL, SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 1.00D+00, 1.00D+00, 1.00D+00, 0.50D+00, &
   & 1.00D+00, 2.00D+00, 3.00D+00, 4.00D+00, 5.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
   & 1.00D+00, 1.00D+00, 2.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, 2.00D+00, 2.00D+00 &
   & /)
 
    IF (N_DATA < 0) THEN
        N_DATA = 0
    END IF
 
    N_DATA = N_DATA + 1
 
    IF (N_MAX < N_DATA) THEN
        N_DATA = 0
        A = 0
        B = 0
        LAMBDA = 0.0D+00
        X = 0.0D+00
        FX = 0.0D+00
    ELSE
        A = A_VEC (N_DATA)
        B = B_VEC (N_DATA)
        LAMBDA = LAMBDA_VEC (N_DATA)
        X = X_VEC (N_DATA)
        FX = FX_VEC (N_DATA)
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION FPSER (A, B, X, EPS)
 
!*****************************************************************************80
!
!! FPSER evaluates IX(A,B)(X) for very small B.
!
!  Discussion:
!
!    This routine is appropriate for use when
!
!      B < min ( EPS, EPS * A )
!
!    and
!
!      X <= 0.5.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, parameters of the function.
!
!    Input, real ( kind = 8 ) X, the point at which the function is to
!    be evaluated.
!
!    Input, real ( kind = 8 ) EPS, a tolerance.
!
!    Output, real ( kind = 8 ) FPSER, the value of IX(A,B)(X).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) AN
    REAL (KIND=8) B
    REAL (KIND=8) C
    REAL (KIND=8) EPS
    REAL (KIND=8) FPSER
    REAL (KIND=8) S
    REAL (KIND=8) T
    REAL (KIND=8) TOL
    REAL (KIND=8) X
 
    FPSER = 1.0D+00
 
    IF (1.0D-03*EPS < A) THEN
        FPSER = 0.0D+00
        T = A * LOG (X)
        IF (T < EXPARG(1)) THEN
            RETURN
        END IF
        FPSER = EXP (T)
    END IF
!
!  1/B(A,B) = B
!
    FPSER = (B/A) * FPSER
    TOL = EPS / A
    AN = A + 1.0D+00
    T = X
    S = T / AN
 
    DO
 
        AN = AN + 1.0D+00
        T = X * T
        C = T / AN
        S = S + C
 
        IF (ABS(C) <= TOL) THEN
            EXIT
        END IF
 
    END DO
 
    FPSER = FPSER * (1.0D+00+A*S)
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GAM1 (A)
 
!*****************************************************************************80
!
!! GAM1 computes 1 / GAMMA(A+1) - 1 for -0.5 <= A <= 1.5
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, forms the argument of the Gamma function.
!
!    Output, real ( kind = 8 ) GAM1, the value of 1 / GAMMA ( A + 1 ) - 1.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) BOT
    REAL (KIND=8) D
    REAL (KIND=8) GAM1
    REAL (KIND=8), PARAMETER, DIMENSION (7) :: P = (/ 0.577215664901533D+00, - &
   & 0.409078193005776D+00, - 0.230975380857675D+00, 0.597275330452234D-01, &
   & 0.766968181649490D-02, - 0.514889771323592D-02, 0.589597428611429D-03 /)
    REAL (KIND=8), DIMENSION (5) :: Q = (/ 0.100000000000000D+01, 0.427569613095214D+00, &
   & 0.158451672430138D+00, 0.261132021441447D-01, 0.423244297896961D-02 /)
    REAL (KIND=8), DIMENSION (9) :: R = (/ - 0.422784335098468D+00, - 0.771330383816272D+00, - &
   & 0.244757765222226D+00, 0.118378989872749D+00, 0.930357293360349D-03, - &
   & 0.118290993445146D-01, 0.223047661158249D-02, 0.266505979058923D-03, - &
   & 0.132674909766242D-03 /)
    REAL (KIND=8), PARAMETER :: S1 = 0.273076135303957D+00
    REAL (KIND=8), PARAMETER :: S2 = 0.559398236957378D-01
    REAL (KIND=8) T
    REAL (KIND=8) TOP
    REAL (KIND=8) W
 
    D = A - 0.5D+00
 
    IF (0.0D+00 < D) THEN
        T = D - 0.5D+00
    ELSE
        T = A
    END IF
 
    IF (T == 0.0D+00) THEN
 
        GAM1 = 0.0D+00
 
    ELSE IF (0.0D+00 < T) THEN
 
        TOP = (((((P(7)*T+P(6))*T+P(5))*T+P(4))*T+P(3))*T+P(2)) * T + P (1)
 
        BOT = (((Q(5)*T+Q(4))*T+Q(3))*T+Q(2)) * T + 1.0D+00
 
        W = TOP / BOT
 
        IF (D <= 0.0D+00) THEN
            GAM1 = A * W
        ELSE
            GAM1 = (T/A) * ((W-0.5D+00)-0.5D+00)
        END IF
 
    ELSE IF (T < 0.0D+00) THEN
 
        TOP = (((((((R(9)*T+R(8))*T+R(7))*T+R(6))*T+R(5))*T+R(4))*T+R(3))*T+R(2)) * T + R (1)
 
        BOT = (S2*T+S1) * T + 1.0D+00
        W = TOP / BOT
 
        IF (D <= 0.0D+00) THEN
            GAM1 = A * ((W+0.5D+00)+0.5D+00)
        ELSE
            GAM1 = T * W / A
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GAMMA (A)
 
!*****************************************************************************80
!
!! GAMMA evaluates the gamma function.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument of the Gamma function.
!
!    Output, real ( kind = 8 ) GAMMA, the value of the Gamma function.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) BOT
    REAL (KIND=8), PARAMETER :: D = 0.41893853320467274178D+00
    REAL (KIND=8) G
    REAL (KIND=8) GAMMA
    INTEGER (KIND=4) I
    INTEGER (KIND=4) J
    REAL (KIND=8) LNX
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    REAL (KIND=8), DIMENSION (7) :: P = (/ 0.539637273585445D-03, 0.261939260042690D-02, &
   & 0.204493667594920D-01, 0.730981088720487D-01, 0.279648642639792D+00, &
   & 0.553413866010467D+00, 1.0D+00 /)
    REAL (KIND=8), PARAMETER :: PI = 3.1415926535898D+00
    REAL (KIND=8), DIMENSION (7) :: Q = (/ - 0.832979206704073D-03, 0.470059485860584D-02, &
   & 0.225211131035340D-01, - 0.170458969313360D+00, - 0.567902761974940D-01, &
   & 0.113062953091122D+01, 1.0D+00 /)
    REAL (KIND=8), PARAMETER :: R1 = 0.820756370353826D-03
    REAL (KIND=8), PARAMETER :: R2 = - 0.595156336428591D-03
    REAL (KIND=8), PARAMETER :: R3 = 0.793650663183693D-03
    REAL (KIND=8), PARAMETER :: R4 = - 0.277777777770481D-02
    REAL (KIND=8), PARAMETER :: R5 = 0.833333333333333D-01
    REAL (KIND=8) S
    REAL (KIND=8) T
    REAL (KIND=8) TOP
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Z
 
    GAMMA = 0.0D+00
    X = A
 
    IF (ABS(A) < 15.0D+00) THEN
!
!  Evaluation of GAMMA(A) for |A| < 15
!
        T = 1.0D+00
        M = INT (A) - 1
!
!  Let T be the product of A-J when 2 <= A.
!
        IF (0 <= M) THEN
 
            DO J = 1, M
                X = X - 1.0D+00
                T = X * T
            END DO
 
            X = X - 1.0D+00
!
!  Let T be the product of A+J WHEN A < 1
!
        ELSE
 
            T = A
 
            IF (A <= 0.0D+00) THEN
 
                M = - M - 1
 
                DO J = 1, M
                    X = X + 1.0D+00
                    T = X * T
                END DO
 
                X = (X+0.5D+00) + 0.5D+00
                T = X * T
                IF (T == 0.0D+00) THEN
                    RETURN
                END IF
 
            END IF
!
!  Check if 1/T can overflow.
!
            IF (ABS(T) < 1.0D-30) THEN
                IF (1.0001D+00 < ABS(T)*HUGE(T)) THEN
                    GAMMA = 1.0D+00 / T
                END IF
                RETURN
            END IF
 
        END IF
!
!  Compute Gamma(1 + X) for 0 <= X < 1.
!
        TOP = P (1)
        BOT = Q (1)
        DO I = 2, 7
            TOP = TOP * X + P (I)
            BOT = BOT * X + Q (I)
        END DO
 
        GAMMA = TOP / BOT
!
!  Termination.
!
        IF (1.0D+00 <= A) THEN
            GAMMA = GAMMA * T
        ELSE
            GAMMA = GAMMA / T
        END IF
!
!  Evaluation of Gamma(A) FOR 15 <= ABS ( A ).
!
    ELSE
 
        IF (1000.0D+00 <= ABS(A)) THEN
            RETURN
        END IF
 
        IF (A <= 0.0D+00) THEN
 
            X = - A
            N = X
            T = X - N
 
            IF (0.9D+00 < T) THEN
                T = 1.0D+00 - T
            END IF
 
            S = SIN (PI*T) / PI
 
            IF (MOD(N, 2) == 0) THEN
                S = - S
            END IF
 
            IF (S == 0.0D+00) THEN
                RETURN
            END IF
 
        END IF
!
!  Compute the modified asymptotic sum.
!
        T = 1.0D+00 / (X*X)
 
        G = ((((R1*T+R2)*T+R3)*T+R4)*T+R5) / X
 
        LNX = LOG (X)
!
!  Final assembly.
!
        Z = X
        G = (D+G) + (Z-0.5D+00) * (LNX-1.0D+00)
        W = G
        T = G - REAL (W, KIND=8)
 
        IF (0.99999D+00*EXPARG(0) < W) THEN
            RETURN
        END IF
 
        GAMMA = EXP (W) * (1.0D+00+T)
 
        IF (A < 0.0D+00) THEN
            GAMMA = (1.0D+00/(GAMMA*S)) / X
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_INC (A, X, ANS, QANS, IND)
 
!*****************************************************************************80
!
!! GAMMA_INC evaluates the incomplete gamma ratio functions P(A,X) and Q(A,X).
!
!  Modified:
!
!    16 April 2005
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, the arguments of the incomplete
!    gamma ratio.  A and X must be nonnegative.  A and X cannot
!    both be zero.
!
!    Output, real ( kind = 8 ) ANS, QANS.  On normal output,
!    ANS = P(A,X) and QANS = Q(A,X).  However, ANS is set to 2 if
!    A or X is negative, or both are 0, or when the answer is
!    computationally indeterminate because A is extremely large
!    and X is very close to A.
!
!    Input, integer ( kind = 4 ) IND, indicates the accuracy request:
!    0, as much accuracy as possible.
!    1, to within 1 unit of the 6-th significant digit,
!    otherwise, to within 1 unit of the 3rd significant digit.
!
!  Local Parameters:
!
!     ALOG10 = LN(10)
!     RT2PIN = 1/SQRT(2*PI)
!     RTPI   = SQRT(PI)
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2N
    REAL (KIND=8) A2NM1
    REAL (KIND=8) ACC
    REAL (KIND=8), DIMENSION (3) :: ACC0 = (/ 5.0D-15, 5.0D-07, 5.0D-04 /)
    REAL (KIND=8), PARAMETER :: ALOG10 = 2.30258509299405D+00
    REAL (KIND=8) AM0
    REAL (KIND=8) AMN
    REAL (KIND=8) AN
    REAL (KIND=8) AN0
    REAL (KIND=8) ANS
    REAL (KIND=8) APN
    REAL (KIND=8) B2N
    REAL (KIND=8) B2NM1
    REAL (KIND=8) BIG (3)
    REAL (KIND=8) C
    REAL (KIND=8) C0
    REAL (KIND=8) C1
    REAL (KIND=8) C2
    REAL (KIND=8) C3
    REAL (KIND=8) C4
    REAL (KIND=8) C5
    REAL (KIND=8) C6
    REAL (KIND=8) CMA
    REAL (KIND=8) D0 (13)
    REAL (KIND=8) D1 (12)
    REAL (KIND=8) D2 (10)
    REAL (KIND=8) D3 (8)
    REAL (KIND=8) D4 (6)
    REAL (KIND=8) D5 (4)
    REAL (KIND=8) D6 (2)
    REAL (KIND=8) D10
    REAL (KIND=8) D20
    REAL (KIND=8) D30
    REAL (KIND=8) D40
    REAL (KIND=8) D50
    REAL (KIND=8) D60
    REAL (KIND=8) D70
    REAL (KIND=8) E
    REAL (KIND=8) E0
    REAL (KIND=8) E00 (3)
    REAL (KIND=8) G
    REAL (KIND=8) H
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IND
    INTEGER (KIND=4) IOP
    REAL (KIND=8) J
    REAL (KIND=8) L
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    INTEGER (KIND=4) N_MAX
    REAL (KIND=8) QANS
    REAL (KIND=8) R
    REAL (KIND=8), PARAMETER :: RT2PIN = 0.398942280401433D+00
    REAL (KIND=8) RTA
    REAL (KIND=8), PARAMETER :: RTPI = 1.77245385090552D+00
    REAL (KIND=8) RTX
    REAL (KIND=8) S
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8) T1
    REAL (KIND=8) TOL
    REAL (KIND=8) TWOA
    REAL (KIND=8) U
    REAL (KIND=8) W
    REAL (KIND=8) WK (20)
    REAL (KIND=8) X
    REAL (KIND=8) X0
    REAL (KIND=8) X00 (3)
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    DATA BIG (1) / 20.0D+00 /, BIG (2) / 14.0D+00 /, BIG (3) / 10.0D+00 /
    DATA E00 (1) / 0.25D-03 /, E00 (2) / 0.25D-01 /, E00 (3) / 0.14D+00 /
    DATA X00 (1) / 31.0D+00 /, X00 (2) / 17.0D+00 /, X00 (3) / 9.7D+00 /
    DATA D0 (1) / 0.833333333333333D-01 /
    DATA D0 (2) / - 0.148148148148148D-01 /
    DATA D0 (3) / 0.115740740740741D-02 /, D0 (4) / 0.352733686067019D-03 /
    DATA D0 (5) / - 0.178755144032922D-03 /, D0 (6) / 0.391926317852244D-04 /
    DATA D0 (7) / - 0.218544851067999D-05 /, D0 (8) / - 0.185406221071516D-05 /
    DATA D0 (9) / 0.829671134095309D-06 /, D0 (10) / - 0.176659527368261D-06 /
    DATA D0 (11) / 0.670785354340150D-08 /, D0 (12) / 0.102618097842403D-07 /
    DATA D0 (13) / - 0.438203601845335D-08 /
    DATA D10 / - 0.185185185185185D-02 /, D1 (1) / - 0.347222222222222D-02 /
    DATA D1 (2) / 0.264550264550265D-02 /, D1 (3) / - 0.990226337448560D-03 /
    DATA D1 (4) / 0.205761316872428D-03 /, D1 (5) / - 0.401877572016461D-06 /
    DATA D1 (6) / - 0.180985503344900D-04 /, D1 (7) / 0.764916091608111D-05 /
    DATA D1 (8) / - 0.161209008945634D-05 /, D1 (9) / 0.464712780280743D-08 /
    DATA D1 (10) / 0.137863344691572D-06 /, D1 (11) / - 0.575254560351770D-07 /
    DATA D1 (12) / 0.119516285997781D-07 /
    DATA D20 / 0.413359788359788D-02 /, D2 (1) / - 0.268132716049383D-02 /
    DATA D2 (2) / 0.771604938271605D-03 /, D2 (3) / 0.200938786008230D-05 /
    DATA D2 (4) / - 0.107366532263652D-03 /, D2 (5) / 0.529234488291201D-04 /
    DATA D2 (6) / - 0.127606351886187D-04 /, D2 (7) / 0.342357873409614D-07 /
    DATA D2 (8) / 0.137219573090629D-05 /, D2 (9) / - 0.629899213838006D-06 /
    DATA D2 (10) / 0.142806142060642D-06 /
    DATA D30 / 0.649434156378601D-03 /, D3 (1) / 0.229472093621399D-03 /
    DATA D3 (2) / - 0.469189494395256D-03 /, D3 (3) / 0.267720632062839D-03 /
    DATA D3 (4) / - 0.756180167188398D-04 /, D3 (5) / - 0.239650511386730D-06 /
    DATA D3 (6) / 0.110826541153473D-04 /, D3 (7) / - 0.567495282699160D-05 /
    DATA D3 (8) / 0.142309007324359D-05 /
    DATA D40 / - 0.861888290916712D-03 /, D4 (1) / 0.784039221720067D-03 /
    DATA D4 (2) / - 0.299072480303190D-03 /, D4 (3) / - 0.146384525788434D-05 /
    DATA D4 (4) / 0.664149821546512D-04 /, D4 (5) / - 0.396836504717943D-04 /
    DATA D4 (6) / 0.113757269706784D-04 /
    DATA D50 / - 0.336798553366358D-03 /, D5 (1) / - 0.697281375836586D-04 /
    DATA D5 (2) / 0.277275324495939D-03 /, D5 (3) / - 0.199325705161888D-03 /
    DATA D5 (4) / 0.679778047793721D-04 /
    DATA D60 / 0.531307936463992D-03 /, D6 (1) / - 0.592166437353694D-03 /
    DATA D6 (2) / 0.270878209671804D-03 /
    DATA D70 / 0.344367606892378D-03 /
 
    E = EPSILON (1.0D+00)
 
    IF (A < 0.0D+00 .OR. X < 0.0D+00) THEN
        ANS = 2.0D+00
        RETURN
    END IF
 
    IF (A == 0.0D+00 .AND. X == 0.0D+00) THEN
        ANS = 2.0D+00
        RETURN
    END IF
 
    IF (A*X == 0.0D+00) THEN
        IF (X <= A) THEN
            ANS = 0.0D+00
            QANS = 1.0D+00
        ELSE
            ANS = 1.0D+00
            QANS = 0.0D+00
        END IF
        RETURN
    END IF
 
    IOP = IND + 1
    IF (IOP /= 1 .AND. IOP /= 2) IOP = 3
    ACC = MAX (ACC0(IOP), E)
    E0 = E00 (IOP)
    X0 = X00 (IOP)
!
!  Select the appropriate algorithm.
!
    IF (1.0D+00 <= A) THEN
        GO TO 10
    END IF
 
    IF (A == 0.5D+00) THEN
        GO TO 390
    END IF
 
    IF (X < 1.1D+00) THEN
        GO TO 160
    END IF
 
    T1 = A * LOG (X) - X
    U = A * EXP (T1)
 
    IF (U == 0.0D+00) THEN
        ANS = 1.0D+00
        QANS = 0.0D+00
        RETURN
    END IF
 
    R = U * (1.0D+00+GAM1(A))
    GO TO 250
 
10  CONTINUE
 
    IF (BIG(IOP) <= A) THEN
        GO TO 30
    END IF
 
    IF (X < A .OR. X0 <= X) THEN
        GO TO 20
    END IF
 
    TWOA = A + A
    M = INT (TWOA)
 
    IF (TWOA == REAL(M, KIND=8)) THEN
        I = M / 2
        IF (A == REAL(I, KIND=8)) THEN
            GO TO 210
        END IF
        GO TO 220
    END IF
 
20  CONTINUE
 
    T1 = A * LOG (X) - X
    R = EXP (T1) / GAMMA (A)
    GO TO 40
 
30  CONTINUE
 
    L = X / A
 
    IF (L == 0.0D+00) THEN
        ANS = 0.0D+00
        QANS = 1.0D+00
        RETURN
    END IF
 
    S = 0.5D+00 + (0.5D+00-L)
    Z = RLOG (L)
    IF (700.0D+00/A <= Z) THEN
        GO TO 410
    END IF
 
    Y = A * Z
    RTA = SQRT (A)
 
    IF (ABS(S) <= E0/RTA) THEN
        GO TO 330
    END IF
 
    IF (ABS(S) <= 0.4D+00) THEN
        GO TO 270
    END IF
 
    T = (1.0D+00/A) ** 2
    T1 = (((0.75D+00*T-1.0D+00)*T+3.5D+00)*T-105.0D+00) / (A*1260.0D+00)
    T1 = T1 - Y
    R = RT2PIN * RTA * EXP (T1)
 
40  CONTINUE
 
    IF (R == 0.0D+00) THEN
        IF (X <= A) THEN
            ANS = 0.0D+00
            QANS = 1.0D+00
        ELSE
            ANS = 1.0D+00
            QANS = 0.0D+00
        END IF
        RETURN
    END IF
 
    IF (X <= MAX(A, ALOG10)) THEN
        GO TO 50
    END IF
 
    IF (X < X0) THEN
        GO TO 250
    END IF
 
    GO TO 100
!
!  Taylor series for P/R.
!
50  CONTINUE
 
    APN = A + 1.0D+00
    T = X / APN
    WK (1) = T
 
    N = 20
 
    DO I = 2, 20
        APN = APN + 1.0D+00
        T = T * (X/APN)
        IF (T <= 1.0D-03) THEN
            N = I
            EXIT
        END IF
        WK (I) = T
    END DO
 
    SUM1 = T
 
    TOL = 0.5D+00 * ACC
 
    DO
 
        APN = APN + 1.0D+00
        T = T * (X/APN)
        SUM1 = SUM1 + T
 
        IF (T <= TOL) THEN
            EXIT
        END IF
 
    END DO
 
    N_MAX = N - 1
    DO M = 1, N_MAX
        N = N - 1
        SUM1 = SUM1 + WK (N)
    END DO
 
    ANS = (R/A) * (1.0D+00+SUM1)
    QANS = 0.5D+00 + (0.5D+00-ANS)
    RETURN
!
!  Asymptotic expansion.
!
100 CONTINUE
 
    AMN = A - 1.0D+00
    T = AMN / X
    WK (1) = T
 
    N = 20
 
    DO I = 2, 20
        AMN = AMN - 1.0D+00
        T = T * (AMN/X)
        IF (ABS(T) <= 1.0D-03) THEN
            N = I
            EXIT
        END IF
        WK (I) = T
    END DO
 
    SUM1 = T
 
    DO
 
        IF (ABS(T) <= ACC) THEN
            EXIT
        END IF
 
        AMN = AMN - 1.0D+00
        T = T * (AMN/X)
        SUM1 = SUM1 + T
 
    END DO
 
    N_MAX = N - 1
    DO M = 1, N_MAX
        N = N - 1
        SUM1 = SUM1 + WK (N)
    END DO
    QANS = (R/X) * (1.0D+00+SUM1)
    ANS = 0.5D+00 + (0.5D+00-QANS)
    RETURN
!
!  Taylor series for P(A,X)/X**A
!
160 CONTINUE
 
    AN = 3.0D+00
    C = X
    SUM1 = X / (A+3.0D+00)
    TOL = 3.0D+00 * ACC / (A+1.0D+00)
 
    DO
 
        AN = AN + 1.0D+00
        C = - C * (X/AN)
        T = C / (A+AN)
        SUM1 = SUM1 + T
 
        IF (ABS(T) <= TOL) THEN
            EXIT
        END IF
 
    END DO
 
    J = A * X * ((SUM1/6.0D+00-0.5D+00/(A+2.0D+00))*X+1.0D+00/(A+1.0D+00))
 
    Z = A * LOG (X)
    H = GAM1 (A)
    G = 1.0D+00 + H
 
    IF (X < 0.25D+00) THEN
        GO TO 180
    END IF
 
    IF (A < X/2.59D+00) THEN
        GO TO 200
    END IF
 
    GO TO 190
 
180 CONTINUE
 
    IF (-0.13394D+00 < Z) THEN
        GO TO 200
    END IF
 
190 CONTINUE
 
    W = EXP (Z)
    ANS = W * G * (0.5D+00+(0.5D+00-J))
    QANS = 0.5D+00 + (0.5D+00-ANS)
    RETURN
 
200 CONTINUE
 
    L = REXP (Z)
    W = 0.5D+00 + (0.5D+00+L)
    QANS = (W*J-L) * G - H
 
    IF (QANS < 0.0D+00) THEN
        ANS = 1.0D+00
        QANS = 0.0D+00
        RETURN
    END IF
 
    ANS = 0.5D+00 + (0.5D+00-QANS)
    RETURN
!
!  Finite sums for Q when 1 <= A and 2*A is an integer.
!
210 CONTINUE
 
    SUM1 = EXP (-X)
    T = SUM1
    N = 1
    C = 0.0D+00
    GO TO 230
 
220 CONTINUE
 
    RTX = SQRT (X)
    SUM1 = ERROR_FC (0, RTX)
    T = EXP (-X) / (RTPI*RTX)
    N = 0
    C = - 0.5D+00
 
230 CONTINUE
 
    DO WHILE (N /= I)
        N = N + 1
        C = C + 1.0D+00
        T = (X*T) / C
        SUM1 = SUM1 + T
    END DO
 
240 CONTINUE
 
    QANS = SUM1
    ANS = 0.5D+00 + (0.5D+00-QANS)
    RETURN
!
!  Continued fraction expansion.
!
250 CONTINUE
 
    TOL = MAX (5.0D+00*E, ACC)
    A2NM1 = 1.0D+00
    A2N = 1.0D+00
    B2NM1 = X
    B2N = X + (1.0D+00-A)
    C = 1.0D+00
 
    DO
 
        A2NM1 = X * A2N + C * A2NM1
        B2NM1 = X * B2N + C * B2NM1
        AM0 = A2NM1 / B2NM1
        C = C + 1.0D+00
        CMA = C - A
        A2N = A2NM1 + CMA * A2N
        B2N = B2NM1 + CMA * B2N
        AN0 = A2N / B2N
 
        IF (ABS(AN0-AM0) < TOL*AN0) THEN
            EXIT
        END IF
 
    END DO
 
    QANS = R * AN0
    ANS = 0.5D+00 + (0.5D+00-QANS)
    RETURN
!
!  General Temme expansion.
!
270 CONTINUE
 
    IF (ABS(S) <= 2.0D+00*E .AND. 3.28D-03 < A*E*E) THEN
        ANS = 2.0D+00
        RETURN
    END IF
 
    C = EXP (-Y)
    W = 0.5D+00 * ERROR_FC (1, SQRT(Y))
    U = 1.0D+00 / A
    Z = SQRT (Z+Z)
 
    IF (L < 1.0D+00) THEN
        Z = - Z
    END IF
 
    IF (IOP < 2) THEN
 
        IF (ABS(S) <= 1.0D-03) THEN
 
            C0 = ((((((D0(7)*Z+D0(6))*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+D0(1)) * Z - &
           & 1.0D+00 / 3.0D+00
 
            C1 = (((((D1(6)*Z+D1(5))*Z+D1(4))*Z+D1(3))*Z+D1(2))*Z+D1(1)) * Z + D10
 
            C2 = ((((D2(5)*Z+D2(4))*Z+D2(3))*Z+D2(2))*Z+D2(1)) * Z + D20
 
            C3 = (((D3(4)*Z+D3(3))*Z+D3(2))*Z+D3(1)) * Z + D30
 
            C4 = (D4(2)*Z+D4(1)) * Z + D40
            C5 = (D5(2)*Z+D5(1)) * Z + D50
            C6 = D6 (1) * Z + D60
 
            T = ((((((D70*U+C6)*U+C5)*U+C4)*U+C3)*U+C2)*U+C1) * U + C0
 
        ELSE
 
            C0 = ((((((((((((D0(13)*Z+D0(12))*Z+D0(11))*Z+D0(10))*Z+D0(9))*Z+D0(8))*Z+D0(7))*Z+&
           & D0(6))*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+D0(1)) * Z - 1.0D+00 / 3.0D+00
 
            C1 = (((((((((((D1(12)*Z+D1(11))*Z+D1(10))*Z+D1(9))*Z+D1(8))*Z+D1(7))*Z+D1(6))*Z+&
           & D1(5))*Z+D1(4))*Z+D1(3))*Z+D1(2))*Z+D1(1)) * Z + D10
 
            C2 = (((((((((D2(10)*Z+D2(9))*Z+D2(8))*Z+D2(7))*Z+D2(6))*Z+D2(5))*Z+D2(4))*Z+&
           & D2(3))*Z+D2(2))*Z+D2(1)) * Z + D20
 
            C3 = (((((((D3(8)*Z+D3(7))*Z+D3(6))*Z+D3(5))*Z+D3(4))*Z+D3(3))*Z+D3(2))*Z+D3(1)) * &
           & Z + D30
 
            C4 = (((((D4(6)*Z+D4(5))*Z+D4(4))*Z+D4(3))*Z+D4(2))*Z+D4(1)) * Z + D40
 
            C5 = (((D5(4)*Z+D5(3))*Z+D5(2))*Z+D5(1)) * Z + D50
 
            C6 = (D6(2)*Z+D6(1)) * Z + D60
 
            T = ((((((D70*U+C6)*U+C5)*U+C4)*U+C3)*U+C2)*U+C1) * U + C0
 
        END IF
 
    ELSE IF (IOP == 2) THEN
 
        C0 = (((((D0(6)*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+D0(1)) * Z - 1.0D+00 / 3.0D+00
 
        C1 = (((D1(4)*Z+D1(3))*Z+D1(2))*Z+D1(1)) * Z + D10
        C2 = D2 (1) * Z + D20
        T = (C2*U+C1) * U + C0
 
    ELSE IF (2 < IOP) THEN
 
        T = ((D0(3)*Z+D0(2))*Z+D0(1)) * Z - 1.0D+00 / 3.0D+00
 
    END IF
 
310 CONTINUE
 
    IF (1.0D+00 <= L) THEN
        QANS = C * (W+RT2PIN*T/RTA)
        ANS = 0.5D+00 + (0.5D+00-QANS)
    ELSE
        ANS = C * (W-RT2PIN*T/RTA)
        QANS = 0.5D+00 + (0.5D+00-ANS)
    END IF
 
    RETURN
!
!  Temme expansion for L = 1
!
330 CONTINUE
 
    IF (3.28D-03 < A*E*E) THEN
        ANS = 2.0D+00
        RETURN
    END IF
 
    C = 0.5D+00 + (0.5D+00-Y)
    W = (0.5D+00-SQRT(Y)*(0.5D+00+(0.5D+00-Y/3.0D+00))/RTPI) / C
    U = 1.0D+00 / A
    Z = SQRT (Z+Z)
 
    IF (L < 1.0D+00) THEN
        Z = - Z
    END IF
 
    IF (IOP < 2) THEN
 
        C0 = ((((((D0(7)*Z+D0(6))*Z+D0(5))*Z+D0(4))*Z+D0(3))*Z+D0(2))*Z+D0(1)) * Z - 1.0D+00 / &
       & 3.0D+00
 
        C1 = (((((D1(6)*Z+D1(5))*Z+D1(4))*Z+D1(3))*Z+D1(2))*Z+D1(1)) * Z + D10
 
        C2 = ((((D2(5)*Z+D2(4))*Z+D2(3))*Z+D2(2))*Z+D2(1)) * Z + D20
 
        C3 = (((D3(4)*Z+D3(3))*Z+D3(2))*Z+D3(1)) * Z + D30
 
        C4 = (D4(2)*Z+D4(1)) * Z + D40
        C5 = (D5(2)*Z+D5(1)) * Z + D50
        C6 = D6 (1) * Z + D60
 
        T = ((((((D70*U+C6)*U+C5)*U+C4)*U+C3)*U+C2)*U+C1) * U + C0
 
    ELSE IF (IOP == 2) THEN
 
        C0 = (D0(2)*Z+D0(1)) * Z - 1.0D+00 / 3.0D+00
        C1 = D1 (1) * Z + D10
        T = (D20*U+C1) * U + C0
 
    ELSE IF (2 < IOP) THEN
 
        T = D0 (1) * Z - 1.0D+00 / 3.0D+00
 
    END IF
 
    GO TO 310
!
!  Special cases
!
390 CONTINUE
 
    IF (X < 0.25D+00) THEN
        ANS = ERROR_F (SQRT(X))
        QANS = 0.5D+00 + (0.5D+00-ANS)
    ELSE
        QANS = ERROR_FC (0, SQRT(X))
        ANS = 0.5D+00 + (0.5D+00-QANS)
    END IF
 
    RETURN
 
410 CONTINUE
 
    IF (ABS(S) <= 2.0D+00*E) THEN
        ANS = 2.0D+00
        RETURN
    END IF
 
    IF (X <= A) THEN
        ANS = 0.0D+00
        QANS = 1.0D+00
    ELSE
        ANS = 1.0D+00
        QANS = 0.0D+00
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_INC_INV (A, X, X0, P, Q, IERR)
 
!*****************************************************************************80
!
!! GAMMA_INC_INV computes the inverse incomplete gamma ratio function.
!
!  Discussion:
!
!    The routine is given positive A, and nonnegative P and Q where P + Q = 1.
!    The value X is computed with the property that P(A,X) = P and Q(A,X) = Q.
!    Schroder iteration is employed.  The routine attempts to compute X
!    to 10 significant digits if this is possible for the particular computer
!    arithmetic being used.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter in the incomplete gamma
!    ratio.  A must be positive.
!
!    Output, real ( kind = 8 ) X, the computed point for which the
!    incomplete gamma functions have the values P and Q.
!
!    Input, real ( kind = 8 ) X0, an optional initial approximation
!    for the solution X.  If the user does not want to supply an
!    initial approximation, then X0 should be set to 0, or a negative
!    value.
!
!    Input, real ( kind = 8 ) P, Q, the values of the incomplete gamma
!    functions, for which the corresponding argument is desired.
!
!    Output, integer ( kind = 4 ) IERR, error flag.
!    0, the solution was obtained. Iteration was not used.
!    0 < K, The solution was obtained. IERR iterations were performed.
!    -2, A <= 0
!    -3, No solution was obtained. The ratio Q/A is too large.
!    -4, P + Q /= 1
!    -6, 20 iterations were performed. The most recent value obtained
!        for X is given.  This cannot occur if X0 <= 0.
!    -7, Iteration failed. No value is given for X.
!        This may occur when X is approximately 0.
!    -8, A value for X has been obtained, but the routine is not certain
!        of its accuracy.  Iteration cannot be performed in this
!        case. If X0 <= 0, this can occur only when P or Q is
!        approximately 0. If X0 is positive then this can occur when A is
!        exceedingly close to X and A is extremely large (say A .GE. 1.E20).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: A0 = 3.31125922108741D+00
    REAL (KIND=8), PARAMETER :: A1 = 11.6616720288968D+00
    REAL (KIND=8), PARAMETER :: A2 = 4.28342155967104D+00
    REAL (KIND=8), PARAMETER :: A3 = 0.213623493715853D+00
    REAL (KIND=8) AM1
    REAL (KIND=8) AMAX
    REAL (KIND=8), DIMENSION (2) :: AMIN = (/ 500.0D+00, 100.0D+00 /)
    REAL (KIND=8) AP1
    REAL (KIND=8) AP2
    REAL (KIND=8) AP3
    REAL (KIND=8) APN
    REAL (KIND=8) B
    REAL (KIND=8), PARAMETER :: B1 = 6.61053765625462D+00
    REAL (KIND=8), PARAMETER :: B2 = 6.40691597760039D+00
    REAL (KIND=8), PARAMETER :: B3 = 1.27364489782223D+00
    REAL (KIND=8), PARAMETER :: B4 = .036117081018842D+00
    REAL (KIND=8), DIMENSION (2) :: BMIN = (/ 1.0D-28, 1.0D-13 /)
    REAL (KIND=8), PARAMETER :: C = 0.577215664901533D+00
    REAL (KIND=8) C1
    REAL (KIND=8) C2
    REAL (KIND=8) C3
    REAL (KIND=8) C4
    REAL (KIND=8) C5
    REAL (KIND=8) D
    REAL (KIND=8), DIMENSION (2) :: DMIN = (/ 1.0D-06, 1.0D-04 /)
    REAL (KIND=8) E
    REAL (KIND=8) E2
    REAL (KIND=8), DIMENSION (2) :: EMIN = (/ 2.0D-03, 6.0D-03 /)
    REAL (KIND=8) EPS
    REAL (KIND=8), DIMENSION (2) :: EPS0 = (/ 1.0D-10, 1.0D-08 /)
    REAL (KIND=8) G
    REAL (KIND=8) H
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    INTEGER (KIND=4) IERR
    INTEGER (KIND=4) IOP
    REAL (KIND=8), PARAMETER :: LN10 = 2.302585D+00
    REAL (KIND=8) P
    REAL (KIND=8) PN
    REAL (KIND=8) Q
    REAL (KIND=8) QG
    REAL (KIND=8) QN
    REAL (KIND=8) R
    REAL (KIND=8) RTA
    REAL (KIND=8) S
    REAL (KIND=8) S2
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8), PARAMETER :: TOL = 1.0D-05
    REAL (KIND=8), PARAMETER :: TWO = 2.0D+00
    REAL (KIND=8) U
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) X0
    REAL (KIND=8) XN
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    E = EPSILON (E)
 
    X = 0.0D+00
 
    IF (A <= 0.0D+00) THEN
        IERR = - 2
        RETURN
    END IF
 
    T = P + Q - 1.0D+00
 
    IF (E < ABS(T)) THEN
        IERR = - 4
        RETURN
    END IF
 
    IERR = 0
 
    IF (P == 0.0D+00) THEN
        RETURN
    END IF
 
    IF (Q == 0.0D+00) THEN
        X = HUGE (X)
        RETURN
    END IF
 
    IF (A == 1.0D+00) THEN
        IF (0.9D+00 <= Q) THEN
            X = - ALNREL (-P)
        ELSE
            X = - LOG (Q)
        END IF
        RETURN
    END IF
 
    E2 = TWO * E
    AMAX = 0.4D-10 / (E*E)
 
    IF (1.0D-10 < E) THEN
        IOP = 2
    ELSE
        IOP = 1
    END IF
 
    EPS = EPS0 (IOP)
    XN = X0
 
    IF (0.0D+00 < X0) THEN
        GO TO 160
    END IF
!
!  Selection of the initial approximation XN of X when A < 1.
!
    IF (1.0D+00 < A) THEN
        GO TO 80
    END IF
 
    G = GAMMA (A+1.0D+00)
    QG = Q * G
 
    IF (QG == 0.0D+00) THEN
        X = HUGE (X)
        IERR = - 8
        RETURN
    END IF
 
    B = QG / A
 
    IF (0.6D+00*A < QG) THEN
        GO TO 40
    END IF
 
    IF (A < 0.30D+00 .AND. 0.35D+00 <= B) THEN
        T = EXP (-(B+C))
        U = T * EXP (T)
        XN = T * EXP (U)
        GO TO 160
    END IF
 
    IF (0.45D+00 <= B) THEN
        GO TO 40
    END IF
 
    IF (B == 0.0D+00) THEN
        X = HUGE (X)
        IERR = - 8
        RETURN
    END IF
 
    Y = - LOG (B)
    S = HALF + (HALF-A)
    Z = LOG (Y)
    T = Y - S * Z
 
    IF (0.15D+00 <= B) THEN
        XN = Y - S * LOG (T) - LOG (1.0D+00+S/(T+1.0D+00))
        GO TO 220
    END IF
 
    IF (0.01D+00 < B) THEN
        U = ((T+TWO*(3.0D+00-A))*T+(TWO-A)*(3.0D+00-A)) / ((T+(5.0D+00-A))*T+TWO)
        XN = Y - S * LOG (T) - LOG (U)
        GO TO 220
    END IF
 
30  CONTINUE
 
    C1 = - S * Z
    C2 = - S * (1.0D+00+C1)
 
    C3 = S * ((HALF*C1+(TWO-A))*C1+(2.5D+00-1.5D+00*A))
 
    C4 = - S * (((C1/3.0D+00+(2.5D+00-1.5D+00*A))*C1+((A-6.0D+00)*A+7.0D+00))*C1+((11.0D+00*A-&
   & 46.0D+00)*A+47.0D+00)/6.0D+00)
 
    C5 = - S * ((((-C1/4.0D+00+(11.0D+00*A-17.0D+00)/6.0D+00)*C1+((-3.0D+00*A+13.0D+00)*A-&
   & 13.0D+00))*C1+HALF*(((TWO*A-25.0D+00)*A+72.0D+00)*A-61.0D+00))*C1+(((25.0D+00*A-195.0D+00)*&
   & A+477.0D+00)*A-379.0D+00)/12.0D+00)
 
    XN = ((((C5/Y+C4)/Y+C3)/Y+C2)/Y+C1) + Y
 
    IF (1.0D+00 < A) THEN
        GO TO 220
    END IF
 
    IF (BMIN(IOP) < B) THEN
        GO TO 220
    END IF
 
    X = XN
    RETURN
 
40  CONTINUE
 
    IF (B*Q <= 1.0D-08) THEN
        XN = EXP (-(Q/A+C))
    ELSE IF (0.9D+00 < P) THEN
        XN = EXP ((ALNREL(-Q)+GAMMA_LN1(A))/A)
    ELSE
        XN = EXP (LOG(P*G)/A)
    END IF
 
    IF (XN == 0.0D+00) THEN
        IERR = - 3
        RETURN
    END IF
 
    T = HALF + (HALF-XN/(A+1.0D+00))
    XN = XN / T
    GO TO 160
!
!  Selection of the initial approximation XN of X when 1 < A.
!
80  CONTINUE
 
    IF (0.5D+00 < Q) THEN
        W = LOG (P)
    ELSE
        W = LOG (Q)
    END IF
 
    T = SQRT (-TWO*W)
 
    S = T - (((A3*T+A2)*T+A1)*T+A0) / ((((B4*T+B3)*T+B2)*T+B1)*T+1.0D+00)
 
    IF (0.5D+00 < Q) THEN
        S = - S
    END IF
 
    RTA = SQRT (A)
    S2 = S * S
 
    XN = A + S * RTA + (S2-1.0D+00) / 3.0D+00 + S * (S2-7.0D+00) / (36.0D+00*RTA) - &
   & ((3.0D+00*S2+7.0D+00)*S2-16.0D+00) / (810.0D+00*A) + S * &
   & ((9.0D+00*S2+256.0D+00)*S2-433.0D+00) / (38880.0D+00*A*RTA)
 
    XN = MAX (XN, 0.0D+00)
 
    IF (AMIN(IOP) <= A) THEN
 
        X = XN
        D = HALF + (HALF-X/A)
 
        IF (ABS(D) <= DMIN(IOP)) THEN
            RETURN
        END IF
 
    END IF
 
110 CONTINUE
 
    IF (P <= 0.5D+00) THEN
        GO TO 130
    END IF
 
    IF (XN < 3.0D+00*A) THEN
        GO TO 220
    END IF
 
    Y = - (W+GAMMA_LOG(A))
    D = MAX (TWO, A*(A-1.0D+00))
 
    IF (LN10*D <= Y) THEN
        S = 1.0D+00 - A
        Z = LOG (Y)
        GO TO 30
    END IF
 
120 CONTINUE
 
    T = A - 1.0D+00
    XN = Y + T * LOG (XN) - ALNREL (-T/(XN+1.0D+00))
    XN = Y + T * LOG (XN) - ALNREL (-T/(XN+1.0D+00))
    GO TO 220
 
130 CONTINUE
 
    AP1 = A + 1.0D+00
 
    IF (0.70D+00*AP1 < XN) THEN
        GO TO 170
    END IF
 
    W = W + GAMMA_LOG (AP1)
 
    IF (XN <= 0.15*AP1) THEN
        AP2 = A + TWO
        AP3 = A + 3.0D+00
        X = EXP ((W+X)/A)
        X = EXP ((W+X-LOG(1.0D+00+(X/AP1)*(1.0D+00+X/AP2)))/A)
        X = EXP ((W+X-LOG(1.0D+00+(X/AP1)*(1.0D+00+X/AP2)))/A)
        X = EXP ((W+X-LOG(1.0D+00+(X/AP1)*(1.0D+00+(X/AP2)*(1.0D+00+X/AP3))))/A)
        XN = X
 
        IF (XN <= 1.0D-02*AP1) THEN
            IF (XN <= EMIN(IOP)*AP1) THEN
                RETURN
            END IF
            GO TO 170
        END IF
 
    END IF
 
    APN = AP1
    T = XN / APN
    SUM1 = 1.0D+00 + T
 
    DO
 
        APN = APN + 1.0D+00
        T = T * (XN/APN)
        SUM1 = SUM1 + T
 
        IF (T <= 1.0D-04) THEN
            EXIT
        END IF
 
    END DO
 
    T = W - LOG (SUM1)
    XN = EXP ((XN+T)/A)
    XN = XN * (1.0D+00-(A*LOG(XN)-XN-T)/(A-XN))
    GO TO 170
!
!  Schroder iteration using P.
!
160 CONTINUE
 
    IF (0.5D+00 < P) THEN
        GO TO 220
    END IF
 
170 CONTINUE
 
    IF (P <= 1.0D+10*TINY(P)) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    AM1 = (A-HALF) - HALF
 
180 CONTINUE
 
    IF (AMAX < A) THEN
        D = HALF + (HALF-XN/A)
        IF (ABS(D) <= E2) THEN
            X = XN
            IERR = - 8
            RETURN
        END IF
    END IF
 
190 CONTINUE
 
    IF (20 <= IERR) THEN
        IERR = - 6
        RETURN
    END IF
 
    IERR = IERR + 1
    CALL GAMMA_INC (A, XN, PN, QN, 0)
 
    IF (PN == 0.0D+00 .OR. QN == 0.0D+00) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    R = RCOMP (A, XN)
 
    IF (R == 0.0D+00) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    T = (PN-P) / R
    W = HALF * (AM1-XN)
 
    IF (ABS(T) <= 0.1D+00 .AND. ABS(W*T) <= 0.1D+00) THEN
        GO TO 200
    END IF
 
    X = XN * (1.0D+00-T)
 
    IF (X <= 0.0D+00) THEN
        IERR = - 7
        RETURN
    END IF
 
    D = ABS (T)
    GO TO 210
 
200 CONTINUE
 
    H = T * (1.0D+00+W*T)
    X = XN * (1.0D+00-H)
 
    IF (X <= 0.0D+00) THEN
        IERR = - 7
        RETURN
    END IF
 
    IF (1.0D+00 <= ABS(W) .AND. ABS(W)*T*T <= EPS) THEN
        RETURN
    END IF
 
    D = ABS (H)
 
210 CONTINUE
 
    XN = X
 
    IF (D <= TOL) THEN
 
        IF (D <= EPS) THEN
            RETURN
        END IF
 
        IF (ABS(P-PN) <= TOL*P) THEN
            RETURN
        END IF
 
    END IF
 
    GO TO 180
!
!  Schroder iteration using Q.
!
220 CONTINUE
 
    IF (Q <= 1.0D+10*TINY(Q)) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    AM1 = (A-HALF) - HALF
 
230 CONTINUE
 
    IF (AMAX < A) THEN
        D = HALF + (HALF-XN/A)
        IF (ABS(D) <= E2) THEN
            X = XN
            IERR = - 8
            RETURN
        END IF
    END IF
 
    IF (20 <= IERR) THEN
        IERR = - 6
        RETURN
    END IF
 
    IERR = IERR + 1
    CALL GAMMA_INC (A, XN, PN, QN, 0)
 
    IF (PN == 0.0D+00 .OR. QN == 0.0D+00) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    R = RCOMP (A, XN)
 
    IF (R == 0.0D+00) THEN
        X = XN
        IERR = - 8
        RETURN
    END IF
 
    T = (Q-QN) / R
    W = HALF * (AM1-XN)
 
    IF (ABS(T) <= 0.1 .AND. ABS(W*T) <= 0.1) THEN
        GO TO 250
    END IF
 
    X = XN * (1.0D+00-T)
 
    IF (X <= 0.0D+00) THEN
        IERR = - 7
        RETURN
    END IF
 
    D = ABS (T)
    GO TO 260
 
250 CONTINUE
 
    H = T * (1.0D+00+W*T)
    X = XN * (1.0D+00-H)
 
    IF (X <= 0.0D+00) THEN
        IERR = - 7
        RETURN
    END IF
 
    IF (1.0D+00 <= ABS(W) .AND. ABS(W)*T*T <= EPS) THEN
        RETURN
    END IF
 
    D = ABS (H)
 
260 CONTINUE
 
    XN = X
 
    IF (TOL < D) THEN
        GO TO 230
    END IF
 
    IF (D <= EPS) THEN
        RETURN
    END IF
 
    IF (ABS(Q-QN) <= TOL*Q) THEN
        RETURN
    END IF
 
    GO TO 230
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
!      PN(A,X) = 1/GAMMA(A) * Integral ( 0 <= T <= X ) T**(A-1) * exp(-T) dT.
!
!    With this definition, for all A and X,
!
!      0 <= PN(A,X) <= 1
!
!    and
!
!      PN(A,INFINITY) = 1.0
!
!    Mathematica can compute this value as
!
!      1 - GammaRegularized[A,X]
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
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 20
 
    REAL (KIND=8) A
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 0.1D+00, 0.1D+00, 0.1D+00, 0.5D+00, &
   & 0.5D+00, 0.5D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.1D+00, 1.1D+00, 1.1D+00, 2.0D+00, 2.0D+00, &
   & 2.0D+00, 6.0D+00, 6.0D+00, 11.0D+00, 26.0D+00, 41.0D+00 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.7420263D+00, 0.9119753D+00, &
   & 0.9898955D+00, 0.2931279D+00, 0.7656418D+00, 0.9921661D+00, 0.0951626D+00, 0.6321206D+00, &
   & 0.9932621D+00, 0.0757471D+00, 0.6076457D+00, 0.9933425D+00, 0.0091054D+00, 0.4130643D+00, &
   & 0.9931450D+00, 0.0387318D+00, 0.9825937D+00, 0.9404267D+00, 0.4863866D+00, 0.7359709D+00 &
   & /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 3.1622777D-02, 3.1622777D-01, &
   & 1.5811388D+00, 7.0710678D-02, 7.0710678D-01, 3.5355339D+00, 0.1000000D+00, 1.0000000D+00, &
   & 5.0000000D+00, 1.0488088D-01, 1.0488088D+00, 5.2440442D+00, 1.4142136D-01, 1.4142136D+00, &
   & 7.0710678D+00, 2.4494897D+00, 1.2247449D+01, 1.6583124D+01, 2.5495098D+01, 4.4821870D+01 &
   & /)
 
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
 
FUNCTION GAMMA_LN1 (A)
 
!*****************************************************************************80
!
!! GAMMA_LN1 evaluates ln ( Gamma ( 1 + A ) ), for -0.2 <= A <= 1.25.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, defines the argument of the function.
!
!    Output, real ( kind = 8 ) GAMMA_LN1, the value of ln ( Gamma ( 1 + A ) ).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) BOT
    REAL (KIND=8) GAMMA_LN1
    REAL (KIND=8), PARAMETER :: P0 = 0.577215664901533D+00
    REAL (KIND=8), PARAMETER :: P1 = 0.844203922187225D+00
    REAL (KIND=8), PARAMETER :: P2 = - 0.168860593646662D+00
    REAL (KIND=8), PARAMETER :: P3 = - 0.780427615533591D+00
    REAL (KIND=8), PARAMETER :: P4 = - 0.402055799310489D+00
    REAL (KIND=8), PARAMETER :: P5 = - 0.673562214325671D-01
    REAL (KIND=8), PARAMETER :: P6 = - 0.271935708322958D-02
    REAL (KIND=8), PARAMETER :: Q1 = 0.288743195473681D+01
    REAL (KIND=8), PARAMETER :: Q2 = 0.312755088914843D+01
    REAL (KIND=8), PARAMETER :: Q3 = 0.156875193295039D+01
    REAL (KIND=8), PARAMETER :: Q4 = 0.361951990101499D+00
    REAL (KIND=8), PARAMETER :: Q5 = 0.325038868253937D-01
    REAL (KIND=8), PARAMETER :: Q6 = 0.667465618796164D-03
    REAL (KIND=8), PARAMETER :: R0 = 0.422784335098467D+00
    REAL (KIND=8), PARAMETER :: R1 = 0.848044614534529D+00
    REAL (KIND=8), PARAMETER :: R2 = 0.565221050691933D+00
    REAL (KIND=8), PARAMETER :: R3 = 0.156513060486551D+00
    REAL (KIND=8), PARAMETER :: R4 = 0.170502484022650D-01
    REAL (KIND=8), PARAMETER :: R5 = 0.497958207639485D-03
    REAL (KIND=8), PARAMETER :: S1 = 0.124313399877507D+01
    REAL (KIND=8), PARAMETER :: S2 = 0.548042109832463D+00
    REAL (KIND=8), PARAMETER :: S3 = 0.101552187439830D+00
    REAL (KIND=8), PARAMETER :: S4 = 0.713309612391000D-02
    REAL (KIND=8), PARAMETER :: S5 = 0.116165475989616D-03
    REAL (KIND=8) TOP
    REAL (KIND=8) X
 
    IF (A < 0.6D+00) THEN
 
        TOP = (((((P6*A+P5)*A+P4)*A+P3)*A+P2)*A+P1) * A + P0
 
        BOT = (((((Q6*A+Q5)*A+Q4)*A+Q3)*A+Q2)*A+Q1) * A + 1.0D+00
 
        GAMMA_LN1 = - A * (TOP/BOT)
 
    ELSE
 
        X = (A-0.5D+00) - 0.5D+00
 
        TOP = (((((R5*X+R4)*X+R3)*X+R2)*X+R1)*X+R0)
 
        BOT = (((((S5*X+S4)*X+S3)*X+S2)*X+S1)*X+1.0D+00)
 
        GAMMA_LN1 = X * (TOP/BOT)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION GAMMA_LOG (A)
 
!*****************************************************************************80
!
!! GAMMA_LOG evaluates ln ( Gamma ( A ) ) for positive A.
!
!  Author:
!
!    Alfred Morris,
!    Naval Surface Weapons Center,
!    Dahlgren, Virginia.
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the argument of the function.
!    A should be positive.
!
!    Output, real ( kind = 8 ), GAMMA_LOG, the value of ln ( Gamma ( A ) ).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8), PARAMETER :: C0 = 0.833333333333333D-01
    REAL (KIND=8), PARAMETER :: C1 = - 0.277777777760991D-02
    REAL (KIND=8), PARAMETER :: C2 = 0.793650666825390D-03
    REAL (KIND=8), PARAMETER :: C3 = - 0.595202931351870D-03
    REAL (KIND=8), PARAMETER :: C4 = 0.837308034031215D-03
    REAL (KIND=8), PARAMETER :: C5 = - 0.165322962780713D-02
    REAL (KIND=8), PARAMETER :: D = 0.418938533204673D+00
    REAL (KIND=8) GAMMA_LOG
    INTEGER (KIND=4) I
    INTEGER (KIND=4) N
    REAL (KIND=8) T
    REAL (KIND=8) W
 
    IF (A <= 0.8D+00) THEN
 
        GAMMA_LOG = GAMMA_LN1 (A) - LOG (A)
 
    ELSE IF (A <= 2.25D+00) THEN
 
        T = (A-0.5D+00) - 0.5D+00
        GAMMA_LOG = GAMMA_LN1 (T)
 
    ELSE IF (A < 10.0D+00) THEN
 
        N = A - 1.25D+00
        T = A
        W = 1.0D+00
        DO I = 1, N
            T = T - 1.0D+00
            W = T * W
        END DO
 
        GAMMA_LOG = GAMMA_LN1 (T-1.0D+00) + LOG (W)
 
    ELSE
 
        T = (1.0D+00/A) ** 2
 
        W = (((((C5*T+C4)*T+C3)*T+C2)*T+C1)*T+C0) / A
 
        GAMMA_LOG = (D+W) + (A-0.5D+00) * (LOG(A)-1.0D+00)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_RAT1 (A, X, R, P, Q, EPS)
 
!*****************************************************************************80
!
!! GAMMA_RAT1 evaluates the incomplete gamma ratio functions P(A,X) and Q(A,X).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, the parameters of the functions.
!    It is assumed that A <= 1.
!
!    Input, real ( kind = 8 ) R, the value exp(-X) * X**A / Gamma(A).
!
!    Output, real ( kind = 8 ) P, Q, the values of P(A,X) and Q(A,X).
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) A2N
    REAL (KIND=8) A2NM1
    REAL (KIND=8) AM0
    REAL (KIND=8) AN
    REAL (KIND=8) AN0
    REAL (KIND=8) B2N
    REAL (KIND=8) B2NM1
    REAL (KIND=8) C
    REAL (KIND=8) CMA
    REAL (KIND=8) EPS
    REAL (KIND=8) G
    REAL (KIND=8) H
    REAL (KIND=8) J
    REAL (KIND=8) L
    REAL (KIND=8) P
    REAL (KIND=8) Q
    REAL (KIND=8) R
    REAL (KIND=8) SUM1
    REAL (KIND=8) T
    REAL (KIND=8) TOL
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) Z
 
    IF (A*X == 0.0D+00) THEN
 
        IF (X <= A) THEN
            P = 0.0D+00
            Q = 1.0D+00
        ELSE
            P = 1.0D+00
            Q = 0.0D+00
        END IF
 
        RETURN
    END IF
 
    IF (A == 0.5D+00) THEN
 
        IF (X < 0.25D+00) THEN
            P = ERROR_F (SQRT(X))
            Q = 0.5D+00 + (0.5D+00-P)
        ELSE
            Q = ERROR_FC (0, SQRT(X))
            P = 0.5D+00 + (0.5D+00-Q)
        END IF
 
        RETURN
 
    END IF
!
!  Taylor series for P(A,X)/X**A
!
    IF (X < 1.1D+00) THEN
 
        AN = 3.0
        C = X
        SUM1 = X / (A+3.0D+00)
        TOL = 0.1D+00 * EPS / (A+1.0D+00)
 
        DO
 
            AN = AN + 1.0D+00
            C = - C * (X/AN)
            T = C / (A+AN)
            SUM1 = SUM1 + T
 
            IF (ABS(T) <= TOL) THEN
                EXIT
            END IF
 
        END DO
 
        J = A * X * ((SUM1/6.0D+00-0.5D+00/(A+2.0D+00))*X+1.0D+00/(A+1.0D+00))
 
        Z = A * LOG (X)
        H = GAM1 (A)
        G = 1.0D+00 + H
 
        IF (X < 0.25D+00) THEN
            GO TO 30
        END IF
 
        IF (A < X/2.59D+00) THEN
            GO TO 50
        ELSE
            GO TO 40
        END IF
 
30      CONTINUE
 
        IF (-0.13394D+00 < Z) THEN
            GO TO 50
        END IF
 
40      CONTINUE
 
        W = EXP (Z)
        P = W * G * (0.5D+00+(0.5D+00-J))
        Q = 0.5D+00 + (0.5D+00-P)
        RETURN
 
50      CONTINUE
 
        L = REXP (Z)
        W = 0.5D+00 + (0.5D+00+L)
        Q = (W*J-L) * G - H
 
        IF (Q < 0.0D+00) THEN
            P = 1.0D+00
            Q = 0.0D+00
        ELSE
            P = 0.5D+00 + (0.5D+00-Q)
        END IF
!
!  Continued fraction expansion.
!
    ELSE
 
        A2NM1 = 1.0D+00
        A2N = 1.0D+00
        B2NM1 = X
        B2N = X + (1.0D+00-A)
        C = 1.0D+00
 
        DO
 
            A2NM1 = X * A2N + C * A2NM1
            B2NM1 = X * B2N + C * B2NM1
            AM0 = A2NM1 / B2NM1
            C = C + 1.0D+00
            CMA = C - A
            A2N = A2NM1 + CMA * A2N
            B2N = B2NM1 + CMA * B2N
            AN0 = A2N / B2N
 
            IF (ABS(AN0-AM0) < EPS*AN0) THEN
                EXIT
            END IF
 
        END DO
 
        Q = R * AN0
        P = 0.5D+00 + (0.5D+00-Q)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE GAMMA_VALUES (N_DATA, X, FX)
 
!*****************************************************************************80
!
!! GAMMA_VALUES returns some values of the Gamma function.
!
!  Definition:
!
!    Gamma(Z) = Integral ( 0 <= T < Infinity) T**(Z-1) exp(-T) dT
!
!  Recursion:
!
!    Gamma(X+1) = X * Gamma(X)
!
!  Restrictions:
!
!    0 < X ( a software restriction).
!
!  Special values:
!
!    GAMMA(0.5) = sqrt(PI)
!
!    For N a positive integer, GAMMA(N+1) = N!, the standard factorial.
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
!    US Department of Commerce, 1964.
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
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 18
 
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 4.590845D+00, 2.218160D+00, &
   & 1.489192D+00, 1.164230D+00, 1.0000000000D+00, 0.9513507699D+00, 0.9181687424D+00, &
   & 0.8974706963D+00, 0.8872638175D+00, 0.8862269255D+00, 0.8935153493D+00, 0.9086387329D+00, &
   & 0.9313837710D+00, 0.9617658319D+00, 1.0000000000D+00, 3.6288000D+05, 1.2164510D+17, &
   & 8.8417620D+30 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) X
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: X_VEC = (/ 0.2D+00, 0.4D+00, 0.6D+00, 0.8D+00, &
   & 1.0D+00, 1.1D+00, 1.2D+00, 1.3D+00, 1.4D+00, 1.5D+00, 1.6D+00, 1.7D+00, 1.8D+00, 1.9D+00, &
   & 2.0D+00, 10.0D+00, 20.0D+00, 30.0D+00 /)
 
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
 
FUNCTION GSUMLN (A, B)
 
!*****************************************************************************80
!
!! GSUMLN evaluates the function ln(Gamma(A + B)).
!
!  Discussion:
!
!    GSUMLN is used for 1 <= A <= 2 and 1 <= B <= 2
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, values whose sum is the argument of
!    the Gamma function.
!
!    Output, real ( kind = 8 ) GSUMLN, the value of ln(Gamma(A+B)).
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) B
    REAL (KIND=8) GSUMLN
    REAL (KIND=8) X
 
    X = A + B - 2.0D+00
 
    IF (X <= 0.25D+00) THEN
        GSUMLN = GAMMA_LN1 (1.0D+00+X)
    ELSE IF (X <= 1.25D+00) THEN
        GSUMLN = GAMMA_LN1 (X) + ALNREL (X)
    ELSE
        GSUMLN = GAMMA_LN1 (X-1.0D+00) + LOG (X*(1.0D+00+X))
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION IPMPAR (I)
 
!*****************************************************************************80
!
!! IPMPAR returns integer machine constants.
!
!  Discussion:
!
!    Input arguments 1 through 3 are queries about integer arithmetic.
!    We assume integers are represented in the N-digit, base A form
!
!      sign * ( X(N-1)*A^(N-1) + ... + X(1)*A + X(0) )
!
!    where 0 <= X(0:N-1) < A.
!
!    Then:
!
!      IPMPAR(1) = A, the base of integer arithmetic;
!      IPMPAR(2) = N, the number of base A digits;
!      IPMPAR(3) = A^N - 1, the largest magnitude.
!
!    It is assumed that the single and real ( kind = 8 ) floating
!    point arithmetics have the same base, say B, and that the
!    nonzero numbers are represented in the form
!
!      sign * (B^E) * (X(1)/B + ... + X(M)/B^M)
!
!    where X(1:M) is one of { 0, 1,..., B-1 }, and 1 <= X(1) and
!    EMIN <= E <= EMAX.
!
!    Input argument 4 is a query about the base of real arithmetic:
!
!      IPMPAR(4) = B, the base of single and real ( kind = 8 ) arithmetic.
!
!    Input arguments 5 through 7 are queries about single precision
!    floating point arithmetic:
!
!     IPMPAR(5) = M, the number of base B digits for single precision.
!     IPMPAR(6) = EMIN, the smallest exponent E for single precision.
!     IPMPAR(7) = EMAX, the largest exponent E for single precision.
!
!    Input arguments 8 through 10 are queries about real ( kind = 8 )
!    floating point arithmetic:
!
!     IPMPAR(8) = M, the number of base B digits for real ( kind = 8 ).
!     IPMPAR(9) = EMIN, the smallest exponent E for real ( kind = 8 ).
!     IPMPAR(10) = EMAX, the largest exponent E for real ( kind = 8 ).
!
!  Reference:
!
!    Phyllis Fox, Andrew Hall, Norman Schryer,
!    Algorithm 528:
!    Framework for a Portable FORTRAN Subroutine Library,
!    ACM Transactions on Mathematical Software,
!    Volume 4, 1978, pages 176-188.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the desired constant.
!
!    Output, integer ( kind = 4 ) IPMPAR, the value of the desired constant.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IMACH (10)
    INTEGER (KIND=4) IPMPAR
!
!     MACHINE CONSTANTS FOR AMDAHL MACHINES.
!
!     data imach( 1) /   2 /
!     data imach( 2) /  31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /  16 /
!     data imach( 5) /   6 /
!     data imach( 6) / -64 /
!     data imach( 7) /  63 /
!     data imach( 8) /  14 /
!     data imach( 9) / -64 /
!     data imach(10) /  63 /
!
!     Machine constants for the AT&T 3B SERIES, AT&T
!     PC 7300, AND AT&T 6300.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the BURROUGHS 1700 SYSTEM.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   33 /
!     data imach( 3) / 8589934591 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -256 /
!     data imach( 7) /  255 /
!     data imach( 8) /   60 /
!     data imach( 9) / -256 /
!     data imach(10) /  255 /
!
!     Machine constants for the BURROUGHS 5700 SYSTEM.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /    8 /
!     data imach( 5) /   13 /
!     data imach( 6) /  -50 /
!     data imach( 7) /   76 /
!     data imach( 8) /   26 /
!     data imach( 9) /  -50 /
!     data imach(10) /   76 /
!
!     Machine constants for the BURROUGHS 6700/7700 SYSTEMS.
!
!     data imach( 1) /      2 /
!     data imach( 2) /     39 /
!     data imach( 3) / 549755813887 /
!     data imach( 4) /      8 /
!     data imach( 5) /     13 /
!     data imach( 6) /    -50 /
!     data imach( 7) /     76 /
!     data imach( 8) /     26 /
!     data imach( 9) / -32754 /
!     data imach(10) /  32780 /
!
!     Machine constants for the CDC 6000/7000 SERIES
!     60 BIT ARITHMETIC, AND THE CDC CYBER 995 64 BIT
!     ARITHMETIC (NOS OPERATING SYSTEM).
!
!     data imach( 1) /    2 /
!     data imach( 2) /   48 /
!     data imach( 3) / 281474976710655 /
!     data imach( 4) /    2 /
!     data imach( 5) /   48 /
!     data imach( 6) / -974 /
!     data imach( 7) / 1070 /
!     data imach( 8) /   95 /
!     data imach( 9) / -926 /
!     data imach(10) / 1070 /
!
!     Machine constants for the CDC CYBER 995 64 BIT
!     ARITHMETIC (NOS/VE OPERATING SYSTEM).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    48 /
!     data imach( 6) / -4096 /
!     data imach( 7) /  4095 /
!     data imach( 8) /    96 /
!     data imach( 9) / -4096 /
!     data imach(10) /  4095 /
!
!     Machine constants for the CRAY 1, XMP, 2, AND 3.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    63 /
!     data imach( 3) / 9223372036854775807 /
!     data imach( 4) /     2 /
!     data imach( 5) /    47 /
!     data imach( 6) / -8189 /
!     data imach( 7) /  8190 /
!     data imach( 8) /    94 /
!     data imach( 9) / -8099 /
!     data imach(10) /  8190 /
!
!     Machine constants for the data GENERAL ECLIPSE S/200.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     Machine constants for the HARRIS 220.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   23 /
!     data imach( 3) / 8388607 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   38 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the HONEYWELL 600/6000
!     AND DPS 8/70 SERIES.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   63 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 2100
!     3 WORD real ( kind = 8 ) OPTION WITH FTN4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   39 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 2100
!     4 WORD real ( kind = 8 ) OPTION WITH FTN4
!
!     data imach( 1) /    2 /
!     data imach( 2) /   15 /
!     data imach( 3) / 32767 /
!     data imach( 4) /    2 /
!     data imach( 5) /   23 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   55 /
!     data imach( 9) / -128 /
!     data imach(10) /  127 /
!
!     Machine constants for the HP 9000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -126 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the IBM 360/370 SERIES,
!     THE ICL 2900, THE ITEL AS/6, THE XEROX SIGMA
!     5/7/9 AND THE SEL SYSTEMS 85/86.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /   16 /
!     data imach( 5) /    6 /
!     data imach( 6) /  -64 /
!     data imach( 7) /   63 /
!     data imach( 8) /   14 /
!     data imach( 9) /  -64 /
!     data imach(10) /   63 /
!
!     Machine constants for the IBM PC.
!
!      data imach(1)/2/
!      data imach(2)/31/
!      data imach(3)/2147483647/
!      data imach(4)/2/
!      data imach(5)/24/
!      data imach(6)/-125/
!      data imach(7)/128/
!      data imach(8)/53/
!      data imach(9)/-1021/
!      data imach(10)/1024/
!
!     Machine constants for the MACINTOSH II - ABSOFT
!     MACFORTRAN II.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the MICROVAX - VMS FORTRAN.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the PDP-11 FORTRAN SUPPORTING
!     32-BIT integer ARITHMETIC.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
!     Machine constants for the SEQUENT BALANCE 8000.
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     Machine constants for the SILICON GRAPHICS IRIS-4D
!     SERIES (MIPS R3000 PROCESSOR).
!
!     data imach( 1) /     2 /
!     data imach( 2) /    31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /     2 /
!     data imach( 5) /    24 /
!     data imach( 6) /  -125 /
!     data imach( 7) /   128 /
!     data imach( 8) /    53 /
!     data imach( 9) / -1021 /
!     data imach(10) /  1024 /
!
!     MACHINE CONSTANTS FOR IEEE ARITHMETIC MACHINES, SUCH AS THE AT&T
!     3B SERIES, MOTOROLA 68000 BASED MACHINES (E.G. SUN 3 AND AT&T
!     PC 7300), AND 8087 BASED MICROS (E.G. IBM PC AND AT&T 6300).
!
    DATA IMACH (1) / 2 /
    DATA IMACH (2) / 31 /
    DATA IMACH (3) / 2147483647 /
    DATA IMACH (4) / 2 /
    DATA IMACH (5) / 24 /
    DATA IMACH (6) / - 125 /
    DATA IMACH (7) / 128 /
    DATA IMACH (8) / 53 /
    DATA IMACH (9) / - 1021 /
    DATA IMACH (10) / 1024 /
!
!     Machine constants for the UNIVAC 1100 SERIES.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   35 /
!     data imach( 3) / 34359738367 /
!     data imach( 4) /    2 /
!     data imach( 5) /   27 /
!     data imach( 6) / -128 /
!     data imach( 7) /  127 /
!     data imach( 8) /   60 /
!     data imach( 9) /-1024 /
!     data imach(10) / 1023 /
!
!     Machine constants for the VAX 11/780.
!
!     data imach( 1) /    2 /
!     data imach( 2) /   31 /
!     data imach( 3) / 2147483647 /
!     data imach( 4) /    2 /
!     data imach( 5) /   24 /
!     data imach( 6) / -127 /
!     data imach( 7) /  127 /
!     data imach( 8) /   56 /
!     data imach( 9) / -127 /
!     data imach(10) /  127 /
!
    IPMPAR = IMACH (I)
 
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
!      PDF(F,S,P) = Choose ( F from F+S-1 ) * P**S * (1-P)**F
!
!    The negative binomial CDF is the probability that there are F or
!    fewer failures upon the attainment of the S-th success.  Thus,
!
!      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 June 2004
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
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: CDF_VEC = (/ 0.6367D+00, 0.3633D+00, 0.1445D+00, &
   & 0.5000D+00, 0.2266D+00, 0.0625D+00, 0.3438D+00, 0.1094D+00, 0.0156D+00, 0.1792D+00, &
   & 0.0410D+00, 0.0041D+00, 0.0705D+00, 0.0109D+00, 0.0007D+00, 0.9862D+00, 0.9150D+00, &
   & 0.7472D+00, 0.8499D+00, 0.5497D+00, 0.2662D+00, 0.6513D+00, 0.2639D+00, 0.0702D+00, &
   & 1.0000D+00, 0.0199D+00, 0.0001D+00 /)
    INTEGER (KIND=4) F
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: F_VEC = (/ 4, 3, 2, 3, 2, 1, 2, 1, 0, 2, 1, 0, &
   & 2, 1, 0, 11, 10, 9, 17, 16, 15, 9, 8, 7, 2, 1, 0 /)
    INTEGER (KIND=4) N_DATA
    REAL (KIND=8) P
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: P_VEC = (/ 0.50D+00, 0.50D+00, 0.50D+00, &
   & 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.50D+00, 0.40D+00, 0.40D+00, 0.40D+00, &
   & 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.30D+00, 0.10D+00, 0.10D+00, 0.10D+00, &
   & 0.10D+00, 0.10D+00, 0.10D+00, 0.01D+00, 0.01D+00, 0.01D+00 /)
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
!    US Department of Commerce, 1964.
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
!    US Department of Commerce, 1964.
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
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, real ( kind = 8 ) A, integer X, the arguments of the function.
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
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.980D+00, 0.905D+00, 0.995D+00, &
   & 0.607D+00, 0.910D+00, 0.986D+00, 0.368D+00, 0.736D+00, 0.920D+00, 0.981D+00, 0.135D+00, &
   & 0.406D+00, 0.677D+00, 0.857D+00, 0.007D+00, 0.040D+00, 0.125D+00, 0.265D+00, 0.441D+00, &
   & 0.616D+00, 0.762D+00 /)
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
 
FUNCTION PSI (XX)
 
!*****************************************************************************80
!
!! PSI evaluates the psi or digamma function, d/dx ln(gamma(x)).
!
!  Discussion:
!
!    The main computation involves evaluation of rational Chebyshev
!    approximations.  PSI was written at Argonne National Laboratory
!    for FUNPACK, and subsequently modified by A. H. Morris of NSWC.
!
!  Reference:
!
!    William Cody, Anthony Strecok, Henry Thacher,
!    Chebyshev Approximations for the Psi Function,
!    Mathematics of Computation,
!    Volume 27, 1973, pages 123-127.
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) XX, the argument of the psi function.
!
!    Output, real ( kind = 8 ) PSI, the value of the psi function.  PSI
!    is assigned the value 0 when the psi function is undefined.
!
    IMPLICIT NONE
 
    REAL (KIND=8) AUG
    REAL (KIND=8) DEN
    REAL (KIND=8), PARAMETER :: DX0 = 1.461632144968362341262659542325721325D+00
    INTEGER (KIND=4) I
    INTEGER (KIND=4) M
    INTEGER (KIND=4) N
    INTEGER (KIND=4) NQ
    REAL (KIND=8), PARAMETER, DIMENSION (7) :: P1 = (/ 0.895385022981970D-02, &
   & 0.477762828042627D+01, 0.142441585084029D+03, 0.118645200713425D+04, &
   & 0.363351846806499D+04, 0.413810161269013D+04, 0.130560269827897D+04 /)
    REAL (KIND=8), DIMENSION (4) :: P2 = (/ - 0.212940445131011D+01, - 0.701677227766759D+01, - &
   & 0.448616543918019D+01, - 0.648157123766197D+00 /)
    REAL (KIND=8), PARAMETER :: PIOV4 = 0.785398163397448D+00
    REAL (KIND=8) PSI
!
!  Coefficients for rational approximation of
!  PSI(X) / (X - X0),  0.5D+00 <= X <= 3.0D+00
!
    REAL (KIND=8), DIMENSION (6) :: Q1 = (/ 0.448452573429826D+02, 0.520752771467162D+03, &
   & 0.221000799247830D+04, 0.364127349079381D+04, 0.190831076596300D+04, 0.691091682714533D-05 &
   & /)
    REAL (KIND=8), DIMENSION (4) :: Q2 = (/ 0.322703493791143D+02, 0.892920700481861D+02, &
   & 0.546117738103215D+02, 0.777788548522962D+01 /)
    REAL (KIND=8) SGN
    REAL (KIND=8) UPPER
    REAL (KIND=8) W
    REAL (KIND=8) X
    REAL (KIND=8) XMAX1
    REAL (KIND=8) XMX0
    REAL (KIND=8) XSMALL
    REAL (KIND=8) XX
    REAL (KIND=8) Z
!
!  XMAX1 is the largest positive floating point constant with entirely
!  integer representation.  It is also used as negative of lower bound
!  on acceptable negative arguments and as the positive argument beyond which
!  psi may be represented as LOG(X).
!
    XMAX1 = REAL (IPMPAR(3), KIND=8)
    XMAX1 = MIN (XMAX1, 1.0D+00/EPSILON(XMAX1))
!
!  XSMALL is the absolute argument below which PI*COTAN(PI*X)
!  may be represented by 1/X.
!
    XSMALL = 1.0D-09
 
    X = XX
    AUG = 0.0D+00
 
    IF (X == 0.0D+00) THEN
        PSI = 0.0D+00
        RETURN
    END IF
!
!  X < 0.5,  Use reflection formula PSI(1-X) = PSI(X) + PI * COTAN(PI*X)
!
    IF (X < 0.5D+00) THEN
!
!  0 < ABS ( X ) <= XSMALL.  Use 1/X as a substitute for PI*COTAN(PI*X)
!
        IF (ABS(X) <= XSMALL) THEN
            AUG = - 1.0D+00 / X
            GO TO 40
        END IF
!
!  Reduction of argument for cotangent.
!
        W = - X
        SGN = PIOV4
 
        IF (W <= 0.0D+00) THEN
            W = - W
            SGN = - SGN
        END IF
!
!  Make an error exit if X <= -XMAX1
!
        IF (XMAX1 <= W) THEN
            PSI = 0.0D+00
            RETURN
        END IF
 
        NQ = INT (W)
        W = W - REAL (NQ, KIND=8)
        NQ = INT (W*4.0D+00)
        W = 4.0D+00 * (W-REAL(NQ, KIND=8)*0.25D+00)
!
!  W is now related to the fractional part of 4.0D+00 * X.
!  Adjust argument to correspond to values in first
!  quadrant and determine sign.
!
        N = NQ / 2
        IF (N+N /= NQ) THEN
            W = 1.0D+00 - W
        END IF
 
        Z = PIOV4 * W
        M = N / 2
 
        IF (M+M /= N) THEN
            SGN = - SGN
        END IF
!
!  Determine final value for -PI * COTAN(PI*X).
!
        N = (NQ+1) / 2
        M = N / 2
        M = M + M
 
        IF (M == N) THEN
 
            IF (Z == 0.0D+00) THEN
                PSI = 0.0D+00
                RETURN
            END IF
 
            AUG = 4.0D+00 * SGN * (COS(Z)/SIN(Z))
 
        ELSE
 
            AUG = 4.0D+00 * SGN * (SIN(Z)/COS(Z))
 
        END IF
 
40      CONTINUE
 
        X = 1.0D+00 - X
 
    END IF
!
!  0.5 <= X <= 3
!
    IF (X <= 3.0D+00) THEN
 
        DEN = X
        UPPER = P1 (1) * X
 
        DO I = 1, 5
            DEN = (DEN+Q1(I)) * X
            UPPER = (UPPER+P1(I+1)) * X
        END DO
 
        DEN = (UPPER+P1(7)) / (DEN+Q1(6))
        XMX0 = REAL (X, KIND=8) - DX0
        PSI = DEN * XMX0 + AUG
!
!  3 < X < XMAX1
!
    ELSE IF (X < XMAX1) THEN
 
        W = 1.0D+00 / X ** 2
        DEN = W
        UPPER = P2 (1) * W
 
        DO I = 1, 3
            DEN = (DEN+Q2(I)) * W
            UPPER = (UPPER+P2(I+1)) * W
        END DO
 
        AUG = UPPER / (DEN+Q2(4)) - 0.5D+00 / X + AUG
        PSI = AUG + LOG (X)
!
!  XMAX1 <= X
!
    ELSE
 
        PSI = AUG + LOG (X)
 
    END IF
 
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
!    PSI(X) = d LN ( GAMMA ( X ) ) / d X = GAMMA'(X) / GAMMA(X)
!
!    PSI(1) = - Euler's constant.
!
!    PSI(X+1) = PSI(X) + 1 / X.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ - 0.5772156649D+00, - &
   & 0.4237549404D+00, - 0.2890398966D+00, - 0.1691908889D+00, - 0.0613845446D+00, - &
   & 0.0364899740D+00, 0.1260474528D+00, 0.2085478749D+00, 0.2849914333D+00, 0.3561841612D+00, &
   & 0.4227843351D+00 /)
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
 
SUBROUTINE R8_SWAP (X, Y)
 
!*****************************************************************************80
!
!! R8_SWAP swaps two R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 8 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
    IMPLICIT NONE
 
    REAL (KIND=8) X
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    Z = X
    X = Y
    Y = Z
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION RCOMP (A, X)
 
!*****************************************************************************80
!
!! RCOMP evaluates exp(-X) * X**A / Gamma(A).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, X, arguments of the quantity to be computed.
!
!    Output, real ( kind = 8 ) RCOMP, the value of exp(-X) * X**A / Gamma(A).
!
!  Local parameters:
!
!    RT2PIN = 1/SQRT(2*PI)
!
    IMPLICIT NONE
 
    REAL (KIND=8) A
    REAL (KIND=8) RCOMP
    REAL (KIND=8), PARAMETER :: RT2PIN = 0.398942280401433D+00
    REAL (KIND=8) T
    REAL (KIND=8) T1
    REAL (KIND=8) U
    REAL (KIND=8) X
 
    IF (A < 20.0D+00) THEN
 
        T = A * LOG (X) - X
 
        IF (A < 1.0D+00) THEN
            RCOMP = (A*EXP(T)) * (1.0D+00+GAM1(A))
        ELSE
            RCOMP = EXP (T) / GAMMA (A)
        END IF
 
    ELSE
 
        U = X / A
 
        IF (U == 0.0D+00) THEN
            RCOMP = 0.0D+00
        ELSE
            T = (1.0D+00/A) ** 2
            T1 = (((0.75D+00*T-1.0D+00)*T+3.5D+00)*T-105.0D+00) / (A*1260.0D+00)
            T1 = T1 - A * RLOG (U)
            RCOMP = RT2PIN * SQRT (A) * EXP (T1)
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION REXP (X)
 
!*****************************************************************************80
!
!! REXP evaluates the function EXP(X) - 1.
!
!  Modified:
!
!    09 December 1999
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) REXP, the value of EXP(X)-1.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: P1 = 0.914041914819518D-09
    REAL (KIND=8), PARAMETER :: P2 = 0.238082361044469D-01
    REAL (KIND=8), PARAMETER :: Q1 = - 0.499999999085958D+00
    REAL (KIND=8), PARAMETER :: Q2 = 0.107141568980644D+00
    REAL (KIND=8), PARAMETER :: Q3 = - 0.119041179760821D-01
    REAL (KIND=8), PARAMETER :: Q4 = 0.595130811860248D-03
    REAL (KIND=8) REXP
    REAL (KIND=8) W
    REAL (KIND=8) X
 
    IF (ABS(X) <= 0.15D+00) THEN
 
        REXP = X * (((P2*X+P1)*X+1.0D+00)/((((Q4*X+Q3)*X+Q2)*X+Q1)*X+1.0D+00))
 
    ELSE
 
        W = EXP (X)
 
        IF (X <= 0.0D+00) THEN
            REXP = (W-0.5D+00) - 0.5D+00
        ELSE
            REXP = W * (0.5D+00+(0.5D+00-1.0D+00/W))
        END IF
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION RLOG (X)
 
!*****************************************************************************80
!
!! RLOG computes X - 1 - LN(X).
!
!  Modified:
!
!    06 August 2004
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) RLOG, the value of the function.
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: A = 0.566749439387324D-01
    REAL (KIND=8), PARAMETER :: B = 0.456512608815524D-01
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    REAL (KIND=8), PARAMETER :: P0 = 0.333333333333333D+00
    REAL (KIND=8), PARAMETER :: P1 = - 0.224696413112536D+00
    REAL (KIND=8), PARAMETER :: P2 = 0.620886815375787D-02
    REAL (KIND=8), PARAMETER :: Q1 = - 0.127408923933623D+01
    REAL (KIND=8), PARAMETER :: Q2 = 0.354508718369557D+00
    REAL (KIND=8) R
    REAL (KIND=8) RLOG
    REAL (KIND=8) T
    REAL (KIND=8), PARAMETER :: TWO = 2.0D+00
    REAL (KIND=8) U
    REAL (KIND=8) W
    REAL (KIND=8) W1
    REAL (KIND=8) X
 
    IF (X < 0.61D+00) THEN
 
        R = (X-0.5D+00) - 0.5D+00
        RLOG = R - LOG (X)
 
    ELSE IF (X < 1.57D+00) THEN
 
        IF (X < 0.82D+00) THEN
 
            U = X - 0.7D+00
            U = U / 0.7D+00
            W1 = A - U * 0.3D+00
 
        ELSE IF (X < 1.18D+00) THEN
 
            U = (X-HALF) - HALF
            W1 = 0.0D+00
 
        ELSE IF (X < 1.57D+00) THEN
 
            U = 0.75D+00 * X - 1.0D+00
            W1 = B + U / 3.0D+00
 
        END IF
 
        R = U / (U+TWO)
        T = R * R
        W = ((P2*T+P1)*T+P0) / ((Q2*T+Q1)*T+1.0D+00)
        RLOG = TWO * T * (1.0D+00/(1.0D+00-R)-R*W) + W1
 
    ELSE IF (1.57D+00 <= X) THEN
 
        R = (X-HALF) - HALF
        RLOG = R - LOG (X)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
FUNCTION RLOG1 (X)
 
!*****************************************************************************80
!
!! RLOG1 evaluates the function X - ln ( 1 + X ).
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) RLOG1, the value of X - ln ( 1 + X ).
!
    IMPLICIT NONE
 
    REAL (KIND=8), PARAMETER :: A = 0.566749439387324D-01
    REAL (KIND=8), PARAMETER :: B = 0.456512608815524D-01
    REAL (KIND=8) H
    REAL (KIND=8), PARAMETER :: HALF = 0.5D+00
    REAL (KIND=8), PARAMETER :: P0 = 0.333333333333333D+00
    REAL (KIND=8), PARAMETER :: P1 = - 0.224696413112536D+00
    REAL (KIND=8), PARAMETER :: P2 = 0.620886815375787D-02
    REAL (KIND=8), PARAMETER :: Q1 = - 0.127408923933623D+01
    REAL (KIND=8), PARAMETER :: Q2 = 0.354508718369557D+00
    REAL (KIND=8) R
    REAL (KIND=8) RLOG1
    REAL (KIND=8) T
    REAL (KIND=8), PARAMETER :: TWO = 2.0D+00
    REAL (KIND=8) W
    REAL (KIND=8) W1
    REAL (KIND=8) X
 
    IF (X <-0.39D+00) THEN
 
        W = (X+HALF) + HALF
        RLOG1 = X - LOG (W)
 
    ELSE IF (X <-0.18D+00) THEN
 
        H = X + 0.3D+00
        H = H / 0.7D+00
        W1 = A - H * 0.3D+00
 
        R = H / (H+2.0D+00)
        T = R * R
        W = ((P2*T+P1)*T+P0) / ((Q2*T+Q1)*T+1.0D+00)
        RLOG1 = TWO * T * (1.0D+00/(1.0D+00-R)-R*W) + W1
 
    ELSE IF (X <= 0.18D+00) THEN
 
        H = X
        W1 = 0.0D+00
 
        R = H / (H+TWO)
        T = R * R
        W = ((P2*T+P1)*T+P0) / ((Q2*T+Q1)*T+1.0D+00)
        RLOG1 = TWO * T * (1.0D+00/(1.0D+00-R)-R*W) + W1
 
    ELSE IF (X <= 0.57D+00) THEN
 
        H = 0.75D+00 * X - 0.25D+00
        W1 = B + H / 3.0D+00
 
        R = H / (H+2.0D+00)
        T = R * R
        W = ((P2*T+P1)*T+P0) / ((Q2*T+Q1)*T+1.0D+00)
        RLOG1 = TWO * T * (1.0D+00/(1.0D+00-R)-R*W) + W1
 
    ELSE
 
        W = (X+HALF) + HALF
        RLOG1 = X - LOG (W)
 
    END IF
 
    RETURN
END
 
!******************************************************************************
 
SUBROUTINE STUDENT_CDF_VALUES (N_DATA, A, X, FX)
 
!*****************************************************************************80
!
!! STUDENT_CDF_VALUES returns some values of the Student CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
    IMPLICIT NONE
 
    INTEGER (KIND=4), PARAMETER :: N_MAX = 13
 
    INTEGER (KIND=4) A
    INTEGER (KIND=4), SAVE, DIMENSION (N_MAX) :: A_VEC = (/ 1, 2, 3, 4, 5, 2, 5, 2, 5, 2, 3, 4, &
   & 5 /)
    REAL (KIND=8) FX
    REAL (KIND=8), SAVE, DIMENSION (N_MAX) :: FX_VEC = (/ 0.60D+00, 0.60D+00, 0.60D+00, &
   & 0.60D+00, 0.60D+00, 0.75D+00, 0.75D+00, 0.95D+00, 0.95D+00, 0.99D+00, 0.99D+00, 0.99D+00, &
   & 0.99D+00 /)
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
 
FUNCTION STVALN (P)
 
!*****************************************************************************80
!
!! STVALN provides starting values for the inverse of the normal distribution.
!
!  Discussion:
!
!    The routine returns an X for which it is approximately true that
!      P = CUMNOR(X),
!    that is,
!      P = Integral ( -infinity < U <= X ) exp(-U*U/2)/sqrt(2*PI) dU.
!
!  Reference:
!
!    William Kennedy, James Gentle,
!    Statistical Computing,
!    Marcel Dekker, NY, 1980, page 95,
!    QA276.4 K46
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the probability whose normal deviate
!    is sought.
!
!    Output, real ( kind = 8 ) STVALN, the normal deviate whose probability
!    is approximately P.
!
    IMPLICIT NONE
 
    REAL (KIND=8) P
    REAL (KIND=8) SGN
    REAL (KIND=8) STVALN
    REAL (KIND=8), PARAMETER, DIMENSION (0:4) :: XDEN = (/ 0.993484626060D-01, &
   & 0.588581570495D+00, 0.531103462366D+00, 0.103537752850D+00, 0.38560700634D-02 /)
    REAL (KIND=8), PARAMETER, DIMENSION (0:4) :: XNUM = (/ - 0.322232431088D+00, - &
   & 1.000000000000D+00, - 0.342242088547D+00, - 0.204231210245D-01, - 0.453642210148D-04 /)
    REAL (KIND=8) Y
    REAL (KIND=8) Z
 
    IF (P <= 0.5D+00) THEN
 
        SGN = - 1.0D+00
        Z = P
 
    ELSE
 
        SGN = 1.0D+00
        Z = 1.0D+00 - P
 
    END IF
 
    Y = SQRT (-2.0D+00*LOG(Z))
    STVALN = Y + EVAL_POL (XNUM, 4, Y) / EVAL_POL (XDEN, 4, Y)
    STVALN = SGN * STVALN
 
    RETURN
END
 
!******************************************************************************

END MODULE ModLib_CDFLib

!******************************************************************************

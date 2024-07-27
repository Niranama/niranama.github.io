
MODULE ModLib_SpecFunc
  
!** PURPOSE OF THIS MODULE:
    ! contains routines that compute the value of various special functions,
    ! by Shanjie Zhang, Jianming Jin.  

!** REFERENCES:
    ! These routines are from SPECIAL_FUNCTIONS package by John Burkardt

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! name of module
    CHARACTER(LEN=*), PARAMETER     :: ModName = 'ModLib_SpecFunc'

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
!   01: BERNOULLI AND EULER NUMBERS
!
!------------------------------------------------------------------------------

SUBROUTINE BERNOA (N, BN)

!******************************************************************************
!
!! bernoa() computes the Bernoulli number Bn.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    11 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the index.
!
!    Output, real ( kind = rk ) BN, the value of the N-th Bernoulli number.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BN (0:N)
    INTEGER J
    INTEGER K
    INTEGER M
    REAL (KIND=RK) R
    REAL (KIND=RK) S

    BN (0) = 1.0D+00
    BN (1) = - 0.5D+00

    DO M = 2, N
        S = - (1.0D+00/(M+1.0D+00)-0.5D+00)
        DO K = 2, M - 1
            R = 1.0D+00
            DO J = 2, K
                R = R * (J+M-K) / J
            END DO
            S = S - R * BN (K)
        END DO
        BN (M) = S
    END DO

    DO M = 3, N, 2
        BN (M) = 0.0D+00
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE BERNOB (N, BN)

!******************************************************************************
!
!! bernob() computes the Bernoulli number Bn.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    11 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the index.
!
!    Output, real ( kind = rk ) BN, the value of the N-th Bernoulli number.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BN (0:N)
    INTEGER K
    INTEGER M
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) S
    REAL (KIND=RK) TPI

    TPI = 6.283185307179586D+00
    BN (0) = 1.0D+00
    BN (1) = - 0.5D+00
    BN (2) = 1.0D+00 / 6.0D+00
    R1 = (2.0D+00/TPI) ** 2

    DO M = 4, N, 2

        R1 = - R1 * (M-1) * M / (TPI*TPI)
        R2 = 1.0D+00

        DO K = 2, 10000
            S = (1.0D+00/K) ** M
            R2 = R2 + S
            IF (S < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        BN (M) = R1 * R2

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE EULERA (N, EN)

!******************************************************************************
!
!! EULERA computes the Euler number En.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    10 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the index of the highest value to compute.
!
!    Output, real ( kind = rk ) EN(0:N), the Euler numbers up to the N-th value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) EN (0:N)
    INTEGER J
    INTEGER K
    INTEGER M
    REAL (KIND=RK) R
    REAL (KIND=RK) S

    EN (0) = 1.0D+00

    DO M = 1, N / 2
        S = 1.0D+00
        DO K = 1, M - 1
            R = 1.0D+00
            DO J = 1, 2 * K
                R = R * (2.0D+00*M-2.0D+00*K+J) / J
            END DO
            S = S + R * EN (2*K)
        END DO
        EN (2*M) = - S
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE EULERB (N, EN)

!******************************************************************************
!
!! EULERB computes the Euler number En.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    09 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the index of the highest value to compute.
!
!    Output, real ( kind = rk ) EN(0:N), the Euler numbers up to the N-th value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) EN (0:N)
    REAL (KIND=RK) HPI
    REAL (KIND=RK) ISGN
    INTEGER K
    INTEGER M
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) S

    HPI = 2.0D+00 / 3.141592653589793D+00
    EN (0) = 1.0D+00
    EN (2) = - 1.0D+00
    R1 = - 4.0D+00 * HPI ** 3

    DO M = 4, N, 2
        R1 = - R1 * (M-1) * M * HPI * HPI
        R2 = 1.0D+00
        ISGN = 1.0D+00
        DO K = 3, 1000, 2
            ISGN = - ISGN
            S = (1.0D+00/K) ** (M+1)
            R2 = R2 + ISGN * S
            IF (S < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        EN (M) = R1 * R2

    END DO

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   02: ORTHOGONAL POLYNOMIALS
!
!------------------------------------------------------------------------------

SUBROUTINE OTHPL (KF, N, X, PL, DPL)

!******************************************************************************
!
!! OTHPL computes orthogonal polynomials Tn(x), Un(x), Ln(x) or Hn(x).
!
!  Discussion:
!
!    This procedure computes orthogonal polynomials: Tn(x) or Un(x),
!    or Ln(x) or Hn(x), and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    08 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KT, the function code:
!    1 for Chebyshev polynomial Tn(x)
!    2 for Chebyshev polynomial Un(x)
!    3 for Laguerre polynomial Ln(x)
!    4 for Hermite polynomial Hn(x)
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PL(0:N), DPL(0:N), the value and derivative of
!    the polynomials of order 0 through N at X.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) C
    REAL (KIND=RK) DPL (0:N)
    REAL (KIND=RK) DY0
    REAL (KIND=RK) DY1
    REAL (KIND=RK) DYN
    INTEGER K
    INTEGER KF
    REAL (KIND=RK) PL (0:N)
    REAL (KIND=RK) X
    REAL (KIND=RK) Y0
    REAL (KIND=RK) Y1
    REAL (KIND=RK) YN

    A = 2.0D+00
    B = 0.0D+00
    C = 1.0D+00
    Y0 = 1.0D+00
    Y1 = 2.0D+00 * X
    DY0 = 0.0D+00
    DY1 = 2.0D+00
    PL (0) = 1.0D+00
    PL (1) = 2.0D+00 * X
    DPL (0) = 0.0D+00
    DPL (1) = 2.0D+00

    IF (KF == 1) THEN
        Y1 = X
        DY1 = 1.0D+00
        PL (1) = X
        DPL (1) = 1.0D+00
    ELSE IF (KF == 3) THEN
        Y1 = 1.0D+00 - X
        DY1 = - 1.0D+00
        PL (1) = 1.0D+00 - X
        DPL (1) = - 1.0D+00
    END IF

    DO K = 2, N

        IF (KF == 3) THEN
            A = - 1.0D+00 / K
            B = 2.0D+00 + A
            C = 1.0D+00 + A
        ELSE IF (KF == 4) THEN
            C = 2.0D+00 * (K-1.0D+00)
        END IF

        YN = (A*X+B) * Y1 - C * Y0
        DYN = A * Y1 + (A*X+B) * DY1 - C * DY0
        PL (K) = YN
        DPL (K) = DYN
        Y0 = Y1
        Y1 = YN
        DY0 = DY1
        DY1 = DYN

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LAGZO (N, X, W)

!******************************************************************************
!
!! lagzo() computes zeros of the Laguerre polynomial, and integration weights.
!
!  Discussion:
!
!    This procedure computes the zeros of Laguerre polynomial Ln(x) in the
!    interval [0,], and the corresponding weighting coefficients for
!    Gauss-Laguerre integration.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the Laguerre polynomial.
!
!    Output, real ( kind = rk ) X(N), the zeros of the Laguerre polynomial.
!
!    Output, real ( kind = rk ) W(N), the weighting coefficients.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) FD
    REAL (KIND=RK) GD
    REAL (KIND=RK) HN
    INTEGER I
    INTEGER IT
    INTEGER J
    INTEGER K
    INTEGER NR
    REAL (KIND=RK) P
    REAL (KIND=RK) PD
    REAL (KIND=RK) PF
    REAL (KIND=RK) Q
    REAL (KIND=RK) W (N)
    REAL (KIND=RK) WP
    REAL (KIND=RK) X (N)
    REAL (KIND=RK) Z
    REAL (KIND=RK) Z0

    HN = 1.0D+00 / REAL (N, KIND=RK)

    DO NR = 1, N

        IF (NR == 1) THEN
            Z = HN
        ELSE
            Z = X (NR-1) + HN * NR ** 1.27D+00
        END IF

        IT = 0

        DO

            IT = IT + 1
            Z0 = Z
            P = 1.0D+00
            DO I = 1, NR - 1
                P = P * (Z-X(I))
            END DO

            F0 = 1.0D+00
            F1 = 1.0D+00 - Z
            DO K = 2, N
                PF = ((2.0D+00*K-1.0D+00-Z)*F1-(K-1.0D+00)*F0) / K
                PD = K / Z * (PF-F1)
                F0 = F1
                F1 = PF
            END DO

            FD = PF / P

            Q = 0.0D+00
            DO I = 1, NR - 1
                WP = 1.0D+00
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        WP = WP * (Z-X(J))
                    END IF
                END DO
                Q = Q + WP
            END DO

            GD = (PD-Q*FD) / P
            Z = Z - FD / GD

            IF (40 < IT .OR. ABS((Z-Z0)/Z) <= 1.0D-15) THEN
                EXIT
            END IF

        END DO

        X (NR) = Z
        W (NR) = 1.0D+00 / (Z*PD*PD)

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LEGZO (N, X, W)

!******************************************************************************
!
!! LEGZO computes the zeros of Legendre polynomials, and integration weights.
!
!  Discussion:
!
!    This procedure computes the zeros of Legendre polynomial Pn(x) in the
!    interval [-1,1], and the corresponding weighting coefficients for
!    Gauss-Legendre integration.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    13 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) X(N), W(N), the zeros of the polynomial,
!    and the corresponding weights.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) FD
    REAL (KIND=RK) GD
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER N0
    INTEGER NR
    REAL (KIND=RK) P
    REAL (KIND=RK) PD
    REAL (KIND=RK) PF
    REAL (KIND=RK) Q
    REAL (KIND=RK) W (N)
    REAL (KIND=RK) WP
    REAL (KIND=RK) X (N)
    REAL (KIND=RK) Z
    REAL (KIND=RK) Z0

    N0 = (N+1) / 2

    DO NR = 1, N0

        Z = COS (3.1415926D+00*(NR-0.25D+00)/N)

        DO

            Z0 = Z
            P = 1.0D+00
            DO I = 1, NR - 1
                P = P * (Z-X(I))
            END DO
            F0 = 1.0D+00
            IF (NR == N0 .AND. N /= 2*INT(N/2)) THEN
                Z = 0.0D+00
            END IF
            F1 = Z
            DO K = 2, N
                PF = (2.0D+00-1.0D+00/K) * Z * F1 - (1.0D+00-1.0D+00/K) * F0
                PD = K * (F1-Z*PF) / (1.0D+00-Z*Z)
                F0 = F1
                F1 = PF
            END DO

            IF (Z == 0.0D+00) THEN
                EXIT
            END IF

            FD = PF / P
            Q = 0.0D+00
            DO I = 1, NR - 1
                WP = 1.0D+00
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        WP = WP * (Z-X(J))
                    END IF
                END DO
                Q = Q + WP
            END DO
            GD = (PD-Q*FD) / P
            Z = Z - FD / GD

            IF (ABS(Z-Z0) < ABS(Z)*1.0D-15) THEN
                EXIT
            END IF

        END DO

        X (NR) = Z
        X (N+1-NR) = - Z
        W (NR) = 2.0D+00 / ((1.0D+00-Z*Z)*PD*PD)
        W (N+1-NR) = W (NR)

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE HERZO (N, X, W)

!******************************************************************************
!
!! HERZO computes the zeros the Hermite polynomial Hn(x).
!
!  Discussion:
!
!    This procedure computes the zeros of Hermite polynomial Ln(x)
!    in the interval [-1,+1], and the corresponding
!    weighting coefficients for Gauss-Hermite integration.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the polynomial.
!
!    Output, real ( kind = rk ) X(N), the zeros.
!
!    Output, real ( kind = rk ) W(N), the corresponding weights.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) FD
    REAL (KIND=RK) GD
    REAL (KIND=RK) HD
    REAL (KIND=RK) HF
    REAL (KIND=RK) HN
    INTEGER I
    INTEGER IT
    INTEGER J
    INTEGER K
    INTEGER NR
    REAL (KIND=RK) P
    REAL (KIND=RK) Q
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) W (N)
    REAL (KIND=RK) WP
    REAL (KIND=RK) X (N)
    REAL (KIND=RK) Z
    REAL (KIND=RK) Z0
    REAL (KIND=RK) ZL

    HN = 1.0D+00 / N
    ZL = - 1.1611D+00 + 1.46D+00 * SQRT (REAL(N, KIND=RK))

    DO NR = 1, N / 2

        IF (NR == 1) THEN
            Z = ZL
        ELSE
            Z = Z - HN * (N/2+1-NR)
        END IF

        IT = 0

        DO

            IT = IT + 1
            Z0 = Z
            F0 = 1.0D+00
            F1 = 2.0D+00 * Z
            DO K = 2, N
                HF = 2.0D+00 * Z * F1 - 2.0D+00 * (K-1.0D+00) * F0
                HD = 2.0D+00 * K * F1
                F0 = F1
                F1 = HF
            END DO

            P = 1.0D+00
            DO I = 1, NR - 1
                P = P * (Z-X(I))
            END DO
            FD = HF / P

            Q = 0.0D+00
            DO I = 1, NR - 1
                WP = 1.0D+00
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        WP = WP * (Z-X(J))
                    END IF
                END DO
                Q = Q + WP
            END DO

            GD = (HD-Q*FD) / P
            Z = Z - FD / GD

            IF (40 < IT .OR. ABS((Z-Z0)/Z) <= 1.0D-15) THEN
                EXIT
            END IF

        END DO

        X (NR) = Z
        X (N+1-NR) = - Z
        R = 1.0D+00
        DO K = 1, N
            R = 2.0D+00 * R * K
        END DO
        W (NR) = 3.544907701811D+00 * R / (HD*HD)
        W (N+1-NR) = W (NR)

    END DO

    IF (N /= 2*INT(N/2)) THEN
        R1 = 1.0D+00
        R2 = 1.0D+00
        DO J = 1, N
            R1 = 2.0D+00 * R1 * J
            IF ((N+1)/2 <= J) THEN
                R2 = R2 * J
            END IF
        END DO
        W (N/2+1) = 0.88622692545276D+00 * R1 / (R2*R2)
        X (N/2+1) = 0.0D+00
    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   03: GAMMA, BETA AND PSI FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE GAMMA (X, GA)

!******************************************************************************
!
!! GAMMA evaluates the Gamma function.
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program that the copyright
!    is acknowledged.
!
!  Modified:
!
!    08 September 2007
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
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
!    Input, real ( kind = rk ) X, the argument.
!    X must not be 0, or any negative integer.
!
!    Output, real ( kind = rk ) GA, the value of the Gamma function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), DIMENSION (26) :: G = (/ 1.0D+00, 0.5772156649015329D+00, - &
   & 0.6558780715202538D+00, - 0.420026350340952D-01, 0.1665386113822915D+00, - &
   & 0.421977345555443D-01, - 0.96219715278770D-02, 0.72189432466630D-02, - &
   & 0.11651675918591D-02, - 0.2152416741149D-03, 0.1280502823882D-03, - 0.201348547807D-04, - &
   & 0.12504934821D-05, 0.11330272320D-05, - 0.2056338417D-06, 0.61160950D-08, 0.50020075D-08, &
   & - 0.11812746D-08, 0.1043427D-09, 0.77823D-11, - 0.36968D-11, 0.51D-12, - 0.206D-13, - &
   & 0.54D-14, 0.14D-14, 0.1D-15 /)
    REAL (KIND=RK) GA
    REAL (KIND=RK) GR
    INTEGER K
    INTEGER M
    INTEGER M1
    REAL (KIND=RK), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=RK) R
    REAL (KIND=RK) X
    REAL (KIND=RK) Z

    IF (X == AINT(X)) THEN

        IF (0.0D+00 < X) THEN
            GA = 1.0D+00
            M1 = INT (X) - 1
            DO K = 2, M1
                GA = GA * K
            END DO
        ELSE
            GA = 1.0D+300
        END IF

    ELSE

        IF (1.0D+00 < ABS(X)) THEN
            Z = ABS (X)
            M = INT (Z)
            R = 1.0D+00
            DO K = 1, M
                R = R * (Z-REAL(K, KIND=RK))
            END DO
            Z = Z - REAL (M, KIND=RK)
        ELSE
            Z = X
        END IF

        GR = G (26)
        DO K = 25, 1, - 1
            GR = GR * Z + G (K)
        END DO

        GA = 1.0D+00 / (GR*Z)

        IF (1.0D+00 < ABS(X)) THEN
            GA = GA * R
            IF (X < 0.0D+00) THEN
                GA = - PI / (X*GA*SIN(PI*X))
            END IF
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE LGAMA (KF, X, GL)

!******************************************************************************
!
!! LGAMA computes the gamma function or its logarithm.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KF, the argument code.
!    1, for gamma(x);
!    2, for ln(gamma(x)).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) GL, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (10) :: A = (/ 8.333333333333333D-02, - &
   & 2.777777777777778D-03, 7.936507936507937D-04, - 5.952380952380952D-04, &
   & 8.417508417508418D-04, - 1.917526917526918D-03, 6.410256410256410D-03, - &
   & 2.955065359477124D-02, 1.796443723688307D-01, - 1.39243221690590D+00 /)
    REAL (KIND=RK) GL
    REAL (KIND=RK) GL0
    INTEGER K
    INTEGER KF
    INTEGER N
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X2
    REAL (KIND=RK) XP

    X0 = X

    IF (X == 1.0D+00 .OR. X == 2.0D+00) THEN
        GL = 0.0D+00
        IF (KF == 1) THEN
            GL = 1.0D+00
        END IF
        RETURN
    ELSE IF (X <= 7.0D+00) THEN
        N = INT (7.0D+00-X)
        X0 = X + N
    END IF

    X2 = 1.0D+00 / (X0*X0)
    XP = 6.283185307179586477D+00
    GL0 = A (10)

    DO K = 9, 1, - 1
        GL0 = GL0 * X2 + A (K)
    END DO

    GL = GL0 / X0 + 0.5D+00 * LOG (XP) + (X0-0.5D+00) * LOG (X0) - X0

    IF (X <= 7.0D+00) THEN
        DO K = 1, N
            GL = GL - LOG (X0-1.0D+00)
            X0 = X0 - 1.0D+00
        END DO
    END IF

    IF (KF == 1) THEN
        GL = EXP (GL)
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CGAMA (X, Y, KF, GR, GI)

!******************************************************************************
!
!! cgama() computes the Gamma function for complex argument.
!
!  Discussion:
!
!    This procedcure computes the gamma function (z) or ln[(z)]
!    for a complex argument
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    26 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts of
!    the argument Z.
!
!    Input, integer KF, the function code.
!    0 for ln[(z)]
!    1 for (z)
!
!    Output, real ( kind = rk ) GR, GI, the real and imaginary parts of
!    the selected function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (10) :: A = (/ 8.333333333333333D-02, - &
   & 2.777777777777778D-03, 7.936507936507937D-04, - 5.952380952380952D-04, &
   & 8.417508417508418D-04, - 1.917526917526918D-03, 6.410256410256410D-03, - &
   & 2.955065359477124D-02, 1.796443723688307D-01, - 1.39243221690590D+00 /)
    REAL (KIND=RK) G0
    REAL (KIND=RK) GI
    REAL (KIND=RK) GI1
    REAL (KIND=RK) GR
    REAL (KIND=RK) GR1
    INTEGER J
    INTEGER K
    INTEGER KF
    INTEGER NA
    REAL (KIND=RK) PI
    REAL (KIND=RK) SI
    REAL (KIND=RK) SR
    REAL (KIND=RK) T
    REAL (KIND=RK) TH
    REAL (KIND=RK) TH1
    REAL (KIND=RK) TH2
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X1
    REAL (KIND=RK) Y
    REAL (KIND=RK) Y1
    REAL (KIND=RK) Z1
    REAL (KIND=RK) Z2

    PI = 3.141592653589793D+00

    IF (Y == 0.0D+00 .AND. X == INT(X) .AND. X <= 0.0D+00) THEN
        GR = 1.0D+300
        GI = 0.0D+00
        RETURN
    ELSE IF (X < 0.0D+00) THEN
        X1 = X
        Y1 = Y
        X = - X
        Y = - Y
    END IF

    X0 = X

    IF (X <= 7.0D+00) THEN
        NA = INT (7-X)
        X0 = X + NA
    END IF

    Z1 = SQRT (X0*X0+Y*Y)
    TH = ATAN (Y/X0)
    GR = (X0-0.5D+00) * LOG (Z1) - TH * Y - X0 + 0.5D+00 * LOG (2.0D+00*PI)
    GI = TH * (X0-0.5D+00) + Y * LOG (Z1) - Y

    DO K = 1, 10
        T = Z1 ** (1-2*K)
        GR = GR + A (K) * T * COS ((2.0D+00*K-1.0D+00)*TH)
        GI = GI - A (K) * T * SIN ((2.0D+00*K-1.0D+00)*TH)
    END DO

    IF (X <= 7.0D+00) THEN
        GR1 = 0.0D+00
        GI1 = 0.0D+00
        DO J = 0, NA - 1
            GR1 = GR1 + 0.5D+00 * LOG ((X+J)**2+Y*Y)
            GI1 = GI1 + ATAN (Y/(X+J))
        END DO
        GR = GR - GR1
        GI = GI - GI1
    END IF

    IF (X1 < 0.0D+00) THEN
        Z1 = SQRT (X*X+Y*Y)
        TH1 = ATAN (Y/X)
        SR = - SIN (PI*X) * COSH (PI*Y)
        SI = - COS (PI*X) * SINH (PI*Y)
        Z2 = SQRT (SR*SR+SI*SI)
        TH2 = ATAN (SI/SR)
        IF (SR < 0.0D+00) THEN
            TH2 = PI + TH2
        END IF
        GR = LOG (PI/(Z1*Z2)) - GR
        GI = - TH1 - TH2 - GI
        X = X1
        Y = Y1
    END IF

    IF (KF == 1) THEN
        G0 = EXP (GR)
        GR = G0 * COS (GI)
        GI = G0 * SIN (GI)
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE BETA (P, Q, BT)

!******************************************************************************
!
!! beta() computes the Beta function B(p,q).
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program that the copyright
!    is acknowledged.
!
!  Modified:
!
!    12 March 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
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
!    Input, real ( kind = rk ) P, Q, the parameters.
!    0 < P, 0 < Q.
!
!    Output, real ( kind = rk ) BT, the value of B(P,Q).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BT
    REAL (KIND=RK) GP
    REAL (KIND=RK) GPQ
    REAL (KIND=RK) GQ
    REAL (KIND=RK) P
    REAL (KIND=RK) PPQ
    REAL (KIND=RK) Q

    CALL GAMMA (P, GP)
    CALL GAMMA (Q, GQ)
    PPQ = P + Q
    CALL GAMMA (PPQ, GPQ)
    BT = GP * GQ / GPQ

    RETURN
END

!******************************************************************************

SUBROUTINE PSI (X, PS)

!******************************************************************************
!
!! PSI computes the PSI function.
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program that the copyright
!    is acknowledged.
!
!  Modified:
!
!    08 September 2007
!
!  Author:
!
!    Original FORTRAN77 by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
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
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PS, the value of the PSI function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), PARAMETER :: A1 = - 0.83333333333333333D-01
    REAL (KIND=RK), PARAMETER :: A2 = 0.83333333333333333D-02
    REAL (KIND=RK), PARAMETER :: A3 = - 0.39682539682539683D-02
    REAL (KIND=RK), PARAMETER :: A4 = 0.41666666666666667D-02
    REAL (KIND=RK), PARAMETER :: A5 = - 0.75757575757575758D-02
    REAL (KIND=RK), PARAMETER :: A6 = 0.21092796092796093D-01
    REAL (KIND=RK), PARAMETER :: A7 = - 0.83333333333333333D-01
    REAL (KIND=RK), PARAMETER :: A8 = 0.4432598039215686D+00
    REAL (KIND=RK), PARAMETER :: EL = 0.5772156649015329D+00
    INTEGER K
    INTEGER N
    REAL (KIND=RK), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=RK) PS
    REAL (KIND=RK) S
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XA

    XA = ABS (X)
    S = 0.0D+00

    IF (X == AINT(X) .AND. X <= 0.0D+00) THEN

        PS = 1.0D+300
        RETURN

    ELSE IF (XA == AINT(XA)) THEN

        N = INT (XA)
        DO K = 1, N - 1
            S = S + 1.0D+00 / REAL (K, KIND=RK)
        END DO

        PS = - EL + S

    ELSE IF (XA+0.5D+00 == AINT(XA+0.5D+00)) THEN

        N = INT (XA-0.5D+00)

        DO K = 1, N
            S = S + 1.0D+00 / REAL (2*K-1, KIND=RK)
        END DO

        PS = - EL + 2.0D+00 * S - 1.386294361119891D+00

    ELSE

        IF (XA < 10.0D+00) THEN

            N = 10 - INT (XA)
            DO K = 0, N - 1
                S = S + 1.0D+00 / (XA+REAL(K, KIND=RK))
            END DO

            XA = XA + REAL (N, KIND=RK)

        END IF

        X2 = 1.0D+00 / (XA*XA)

        PS = LOG (XA) - 0.5D+00 / XA + X2 * &
       & (((((((A8*X2+A7)*X2+A6)*X2+A5)*X2+A4)*X2+A3)*X2+A2)*X2+A1)

        PS = PS - S

    END IF

    IF (X < 0.0D+00) THEN
        PS = PS - PI * COS (PI*X) / SIN (PI*X) - 1.0D+00 / X
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CPSI (X, Y, PSR, PSI)

!******************************************************************************
!
!! CPSI computes the psi function for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    16 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts
!    of the argument.
!
!    Output, real ( kind = rk ) PSR, PSI, the real and imaginary parts
!    of the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (8) :: A = (/ - 0.8333333333333D-01, &
   & 0.83333333333333333D-02, - 0.39682539682539683D-02, 0.41666666666666667D-02, - &
   & 0.75757575757575758D-02, 0.21092796092796093D-01, - 0.83333333333333333D-01, &
   & 0.4432598039215686D+00 /)
    REAL (KIND=RK) CT2
    INTEGER K
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PSI
    REAL (KIND=RK) PSR
    REAL (KIND=RK) RI
    REAL (KIND=RK) RR
    REAL (KIND=RK) TH
    REAL (KIND=RK) TM
    REAL (KIND=RK) TN
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X1
    REAL (KIND=RK) Y
    REAL (KIND=RK) Y1
    REAL (KIND=RK) Z0
    REAL (KIND=RK) Z2

    PI = 3.141592653589793D+00

    IF (Y == 0.0D+00 .AND. X == INT(X) .AND. X <= 0.0D+00) THEN

        PSR = 1.0D+300
        PSI = 0.0D+00

    ELSE

        IF (X < 0.0D+00) THEN
            X1 = X
            Y1 = Y
            X = - X
            Y = - Y
        END IF

        X0 = X

        IF (X < 8.0D+00) THEN
            N = 8 - INT (X)
            X0 = X + N
        END IF

        IF (X0 == 0.0D+00) THEN
            IF (Y /= 0.0D+00) THEN
                TH = 0.5D+00 * PI
            ELSE
                TH = 0.0D+00
            END IF
        ELSE
            TH = ATAN (Y/X0)
        END IF

        Z2 = X0 * X0 + Y * Y
        Z0 = SQRT (Z2)
        PSR = LOG (Z0) - 0.5D+00 * X0 / Z2
        PSI = TH + 0.5D+00 * Y / Z2
        DO K = 1, 8
            PSR = PSR + A (K) * Z2 ** (-K) * COS (2.0D+00*K*TH)
            PSI = PSI - A (K) * Z2 ** (-K) * SIN (2.0D+00*K*TH)
        END DO

        IF (X < 8.0D+00) THEN
            RR = 0.0D+00
            RI = 0.0D+00
            DO K = 1, N
                RR = RR + (X0-K) / ((X0-K)**2.0D+00+Y*Y)
                RI = RI + Y / ((X0-K)**2.0D+00+Y*Y)
            END DO
            PSR = PSR - RR
            PSI = PSI + RI
        END IF

        IF (X1 < 0.0D+00) THEN
            TN = TAN (PI*X)
            TM = TANH (PI*Y)
            CT2 = TN * TN + TM * TM
            PSR = PSR + X / (X*X+Y*Y) + PI * (TN-TN*TM*TM) / CT2
            PSI = PSI - Y / (X*X+Y*Y) - PI * TM * (1.0D+00+TN*TN) / CT2
            X = X1
            Y = Y1
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE INCOB (A, B, X, BIX)

!******************************************************************************
!
!! INCOB computes the incomplete beta function Ix(a,b).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BIX, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) BIX
    REAL (KIND=RK) BT
    REAL (KIND=RK) DK (51)
    REAL (KIND=RK) FK (51)
    INTEGER K
    REAL (KIND=RK) S0
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2
    REAL (KIND=RK) TA
    REAL (KIND=RK) TB
    REAL (KIND=RK) X

    S0 = (A+1.0D+00) / (A+B+2.0D+00)
    CALL BETA (A, B, BT)

    IF (X <= S0) THEN

        DO K = 1, 20
            DK (2*K) = K * (B-K) * X / (A+2.0D+00*K-1.0D+00) / (A+2.0D+00*K)
        END DO

        DO K = 0, 20
            DK (2*K+1) = - (A+K) * (A+B+K) * X / (A+2.0D+00*K) / (A+2.0D+00*K+1.0D+00)
        END DO

        T1 = 0.0D+00
        DO K = 20, 1, - 1
            T1 = DK (K) / (1.0D+00+T1)
        END DO
        TA = 1.0D+00 / (1.0D+00+T1)
        BIX = X ** A * (1.0D+00-X) ** B / (A*BT) * TA

    ELSE

        DO K = 1, 20
            FK (2*K) = K * (A-K) * (1.0D+00-X) / (B+2.0D+00*K-1.0D+00) / (B+2.0D+00*K)
        END DO

        DO K = 0, 20
            FK (2*K+1) = - (B+K) * (A+B+K) * (1.0D+00-X) / (B+2.0D+00*K) / &
           & (B+2.0D+00*K+1.0D+00)
        END DO

        T2 = 0.0D+00
        DO K = 20, 1, - 1
            T2 = FK (K) / (1.0D+00+T2)
        END DO
        TB = 1.0D+00 / (1.0D+00+T2)
        BIX = 1.0D+00 - X ** A * (1.0D+00-X) ** B / (B*BT) * TB

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE INCOG (A, X, GIN, GIM, GIP)

!******************************************************************************
!
!! INCOG computes the incomplete gamma function r(a,x), ,(a,x), P(a,x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, the parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) GIN, GIM, GIP, the values of
!    r(a,x), (a,x), P(a,x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) GA
    REAL (KIND=RK) GIM
    REAL (KIND=RK) GIN
    REAL (KIND=RK) GIP
    INTEGER K
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) T0
    REAL (KIND=RK) X
    REAL (KIND=RK) XAM

    XAM = - X + A * LOG (X)

    IF (700.0D+00 < XAM .OR. 170.0D+00 < A) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'INCOG - Fatal error!'
        WRITE (*, '(a)') '  A and/or X is too large!'
        STOP
    END IF

    IF (X == 0.0D+00) THEN

        GIN = 0.0D+00
        CALL GAMMA (A, GA)
        GIM = GA
        GIP = 0.0D+00

    ELSE IF (X <= 1.0D+00+A) THEN

        S = 1.0D+00 / A
        R = S
        DO K = 1, 60
            R = R * X / (A+K)
            S = S + R
            IF (ABS(R/S) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        GIN = EXP (XAM) * S
        CALL GAMMA (A, GA)
        GIP = GIN / GA
        GIM = GA - GIN

    ELSE IF (1.0D+00+A < X) THEN

        T0 = 0.0D+00
        DO K = 60, 1, - 1
            T0 = (K-A) / (1.0D+00+K/(X+T0))
        END DO
        GIM = EXP (XAM) / (X+T0)
        CALL GAMMA (A, GA)
        GIN = GA - GIM
        GIP = 1.0D+00 - GIM / GA

    END IF

    RETURN
END

!******************************************************************************

FUNCTION GAMMA_LOG (X)

!******************************************************************************
!
!! GAMMA_LOG evaluates the logarithm of the gamma function.
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
!    Input, real ( kind = rk ) X, the argument of the function.
!
!    Output, real ( kind = rk ) GAMMA_LOG, the value of the function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), DIMENSION (7) :: C = (/ - 1.910444077728D-03, 8.4171387781295D-04, - &
   & 5.952379913043012D-04, 7.93650793500350248D-04, - 2.777777777777681622553D-03, &
   & 8.333333333333333331554247D-02, 5.7083835261D-03 /)
    REAL (KIND=RK) CORR
    REAL (KIND=RK) :: D1 = - 5.772156649015328605195174D-01
    REAL (KIND=RK) :: D2 = 4.227843350984671393993777D-01
    REAL (KIND=RK) :: D4 = 1.791759469228055000094023D+00
    REAL (KIND=RK), PARAMETER :: FRTBIG = 2.25D+76
    INTEGER I
    REAL (KIND=RK), DIMENSION (8) :: P1 = (/ 4.945235359296727046734888D+00, &
   & 2.018112620856775083915565D+02, 2.290838373831346393026739D+03, &
   & 1.131967205903380828685045D+04, 2.855724635671635335736389D+04, &
   & 3.848496228443793359990269D+04, 2.637748787624195437963534D+04, &
   & 7.225813979700288197698961D+03 /)
    REAL (KIND=RK), DIMENSION (8) :: P2 = (/ 4.974607845568932035012064D+00, &
   & 5.424138599891070494101986D+02, 1.550693864978364947665077D+04, &
   & 1.847932904445632425417223D+05, 1.088204769468828767498470D+06, &
   & 3.338152967987029735917223D+06, 5.106661678927352456275255D+06, &
   & 3.074109054850539556250927D+06 /)
    REAL (KIND=RK), DIMENSION (8) :: P4 = (/ 1.474502166059939948905062D+04, &
   & 2.426813369486704502836312D+06, 1.214755574045093227939592D+08, &
   & 2.663432449630976949898078D+09, 2.940378956634553899906876D+10, &
   & 1.702665737765398868392998D+11, 4.926125793377430887588120D+11, &
   & 5.606251856223951465078242D+11 /)
    REAL (KIND=RK), DIMENSION (8) :: Q1 = (/ 6.748212550303777196073036D+01, &
   & 1.113332393857199323513008D+03, 7.738757056935398733233834D+03, &
   & 2.763987074403340708898585D+04, 5.499310206226157329794414D+04, &
   & 6.161122180066002127833352D+04, 3.635127591501940507276287D+04, &
   & 8.785536302431013170870835D+03 /)
    REAL (KIND=RK), DIMENSION (8) :: Q2 = (/ 1.830328399370592604055942D+02, &
   & 7.765049321445005871323047D+03, 1.331903827966074194402448D+05, &
   & 1.136705821321969608938755D+06, 5.267964117437946917577538D+06, &
   & 1.346701454311101692290052D+07, 1.782736530353274213975932D+07, &
   & 9.533095591844353613395747D+06 /)
    REAL (KIND=RK), DIMENSION (8) :: Q4 = (/ 2.690530175870899333379843D+03, &
   & 6.393885654300092398984238D+05, 4.135599930241388052042842D+07, &
   & 1.120872109616147941376570D+09, 1.488613728678813811542398D+10, &
   & 1.016803586272438228077304D+11, 3.417476345507377132798597D+11, &
   & 4.463158187419713286462081D+11 /)
    REAL (KIND=RK) GAMMA_LOG
    REAL (KIND=RK) RES
    REAL (KIND=RK), PARAMETER :: SQRTPI = 0.9189385332046727417803297D+00
    REAL (KIND=RK) X
    REAL (KIND=RK), PARAMETER :: XBIG = 2.55D+305
    REAL (KIND=RK) XDEN
    REAL (KIND=RK), PARAMETER :: XINF = 1.79D+308
    REAL (KIND=RK) XM1
    REAL (KIND=RK) XM2
    REAL (KIND=RK) XM4
    REAL (KIND=RK) XNUM
    REAL (KIND=RK) Y
    REAL (KIND=RK) YSQ

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
    GAMMA_LOG = RES

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   04: LEGENDRE FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE LPMN (MM, M, N, X, PM, PD)

!******************************************************************************
!
!! LPMN computes associated Legendre functions Pmn(X) and derivatives P'mn(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    19 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer MM, the leading dimension of PM and PD.
!
!    Input, integer M, the order of Pmn(x).
!
!    Input, integer N, the degree of Pmn(x).
!
!    Input, real ( kind = rk ) X, the argument of Pmn(x).
!
!    Output, real ( kind = rk ) PM(0:MM,0:N), PD(0:MM,0:N), the
!    values of Pmn(x) and Pmn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER MM
    INTEGER N

    INTEGER I
    INTEGER J
    INTEGER LS
    INTEGER M
    REAL (KIND=RK) PD (0:MM, 0:N)
    REAL (KIND=RK) PM (0:MM, 0:N)
    REAL (KIND=RK) X
    REAL (KIND=RK) XQ
    REAL (KIND=RK) XS

    DO I = 0, N
        DO J = 0, M
            PM (J, I) = 0.0D+00
            PD (J, I) = 0.0D+00
        END DO
    END DO

    PM (0, 0) = 1.0D+00

    IF (ABS(X) == 1.0D+00) THEN

        DO I = 1, N
            PM (0, I) = X ** I
            PD (0, I) = 0.5D+00 * I * (I+1.0D+00) * X ** (I+1)
        END DO

        DO J = 1, N
            DO I = 1, M
                IF (I == 1) THEN
                    PD (I, J) = 1.0D+300
                ELSE IF (I == 2) THEN
                    PD (I, J) = - 0.25D+00 * (J+2) * (J+1) * J * (J-1) * X ** (J+1)
                END IF
            END DO
        END DO

        RETURN

    END IF

    IF (1.0D+00 < ABS(X)) THEN
        LS = - 1
    ELSE
        LS = + 1
    END IF

    XQ = SQRT (LS*(1.0D+00-X*X))
    XS = LS * (1.0D+00-X*X)
    DO I = 1, M
        PM (I, I) = - LS * (2.0D+00*I-1.0D+00) * XQ * PM (I-1, I-1)
    END DO

    DO I = 0, M
        PM (I, I+1) = (2.0D+00*I+1.0D+00) * X * PM (I, I)
    END DO

    DO I = 0, M
        DO J = I + 2, N
            PM (I, J) = ((2.0D+00*J-1.0D+00)*X*PM(I, J-1)-(I+J-1.0D+00)*PM(I, J-2)) / (J-I)
        END DO
    END DO

    PD (0, 0) = 0.0D+00
    DO J = 1, N
        PD (0, J) = LS * J * (PM(0, J-1)-X*PM(0, J)) / XS
    END DO

    DO I = 1, M
        DO J = I, N
            PD (I, J) = LS * I * X * PM (I, J) / XS + (J+I) * (J-I+1.0D+00) / XQ * PM (I-1, J)
        END DO
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LPMV (V, M, X, PMV)

!******************************************************************************
!
!! LPMV computes associated Legendre functions Pmv(X) with arbitrary degree.
!
!  Discussion:
!
!    Compute the associated Legendre function Pmv(x) with an integer order
!    and an arbitrary nonnegative degree v.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    19 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the degree of Pmv(x).
!
!    Input, integer M, the order of Pmv(x).
!
!    Input, real ( kind = rk ) X, the argument of Pm(x).
!
!    Output, real ( kind = rk ) PMV, the value of Pm(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C0
    REAL (KIND=RK) EL
    REAL (KIND=RK) EPS
    INTEGER J
    INTEGER K
    INTEGER M
    INTEGER NV
    REAL (KIND=RK) PA
    REAL (KIND=RK) PI
    REAL (KIND=RK) PMV
    REAL (KIND=RK) PSS
    REAL (KIND=RK) PSV
    REAL (KIND=RK) PV0
    REAL (KIND=RK) QR
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) RG
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) S1
    REAL (KIND=RK) S2
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VS
    REAL (KIND=RK) X
    REAL (KIND=RK) XQ

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    EPS = 1.0D-14
    NV = INT (V)
    V0 = V - NV

    IF (X ==-1.0D+00 .AND. V /= NV) THEN
        IF (M == 0) THEN
            PMV = - 1.0D+300
        ELSE
            PMV = 1.0D+300
        END IF
        RETURN
    END IF

    C0 = 1.0D+00

    IF (M /= 0) THEN

        RG = V * (V+M)
        DO J = 1, M - 1
            RG = RG * (V*V-J*J)
        END DO
        XQ = SQRT (1.0D+00-X*X)
        R0 = 1.0D+00
        DO J = 1, M
            R0 = 0.5D+00 * R0 * XQ / J
        END DO
        C0 = R0 * RG

    END IF

    IF (V0 == 0.0D+00) THEN

        PMV = 1.0D+00
        R = 1.0D+00
        DO K = 1, NV - M
            R = 0.5D+00 * R * (-NV+M+K-1.0D+00) * (NV+M+K) / (K*(K+M)) * (1.0D+00+X)
            PMV = PMV + R
        END DO
        PMV = (-1.0D+00) ** NV * C0 * PMV

    ELSE

        IF (-0.35D+00 <= X) THEN

            PMV = 1.0D+00
            R = 1.0D+00
            DO K = 1, 100
                R = 0.5D+00 * R * (-V+M+K-1.0D+00) * (V+M+K) / (K*(M+K)) * (1.0D+00-X)
                PMV = PMV + R
                IF (12 < K .AND. ABS(R/PMV) < EPS) THEN
                    EXIT
                END IF
            END DO

            PMV = (-1.0D+00) ** M * C0 * PMV

        ELSE

            VS = SIN (V*PI) / PI
            PV0 = 0.0D+00

            IF (M /= 0) THEN

                QR = SQRT ((1.0D+00-X)/(1.0D+00+X))
                R2 = 1.0D+00
                DO J = 1, M
                    R2 = R2 * QR * J
                END DO
                S0 = 1.0D+00
                R1 = 1.0D+00
                DO K = 1, M - 1
                    R1 = 0.5D+00 * R1 * (-V+K-1) * (V+K) / (K*(K-M)) * (1.0D+00+X)
                    S0 = S0 + R1
                END DO
                PV0 = - VS * R2 / M * S0

            END IF

            CALL PSI (V, PSV)
            PA = 2.0D+00 * (PSV+EL) + PI / TAN (PI*V) + 1.0D+00 / V

            S1 = 0.0D+00
            DO J = 1, M
                S1 = S1 + (J*J+V*V) / (J*(J*J-V*V))
            END DO

            PMV = PA + S1 - 1.0D+00 / (M-V) + LOG (0.5D+00*(1.0D+00+X))
            R = 1.0D+00
            DO K = 1, 100
                R = 0.5D+00 * R * (-V+M+K-1.0D+00) * (V+M+K) / (K*(K+M)) * (1.0D+00+X)
                S = 0.0D+00
                DO J = 1, M
                    S = S + ((K+J)**2+V*V) / ((K+J)*((K+J)**2-V*V))
                END DO
                S2 = 0.0D+00
                DO J = 1, K
                    S2 = S2 + 1.0D+00 / (J*(J*J-V*V))
                END DO
                PSS = PA + S + 2.0D+00 * V * V * S2 - 1.0D+00 / (M+K-V) + LOG &
               & (0.5D+00*(1.0D+00+X))
                R2 = PSS * R
                PMV = PMV + R2
                IF (ABS(R2/PMV) < EPS) THEN
                    EXIT
                END IF
            END DO

            PMV = PV0 + PMV * VS * C0

        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE LPN (N, X, PN, PD)

!******************************************************************************
!
!! LPN computes Legendre polynomials Pn(x) and derivatives Pn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the maximum degree.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PN(0:N), PD(0:N), the values and derivatives
!    of the polyomials of degrees 0 to N at X.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    INTEGER K
    REAL (KIND=RK) P0
    REAL (KIND=RK) P1
    REAL (KIND=RK) PD (0:N)
    REAL (KIND=RK) PF
    REAL (KIND=RK) PN (0:N)
    REAL (KIND=RK) X

    PN (0) = 1.0D+00
    PN (1) = X
    PD (0) = 0.0D+00
    PD (1) = 1.0D+00
    P0 = 1.0D+00
    P1 = X

    DO K = 2, N

        PF = (2.0D+00*K-1.0D+00) / K * X * P1 - (K-1.0D+00) / K * P0
        PN (K) = PF

        IF (ABS(X) == 1.0D+00) THEN
            PD (K) = 0.5D+00 * X ** (K+1) * K * (K+1.0D+00)
        ELSE
            PD (K) = K * (P1-X*PF) / (1.0D+00-X*X)
        END IF

        P0 = P1
        P1 = PF

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LPNI (N, X, PN, PD, PL)

!******************************************************************************
!
!! LPNI computes Legendre polynomials Pn(x), derivatives, and integrals.
!
!  Discussion:
!
!    This routine computes Legendre polynomials Pn(x), Pn'(x)
!    and the integral of Pn(t) from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    13 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the maximum degree.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PN(0:N), PD(0:N), PL(0:N), the values,
!    derivatives and integrals of the polyomials of degrees 0 to N at X.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    INTEGER J
    INTEGER K
    INTEGER N1
    REAL (KIND=RK) P0
    REAL (KIND=RK) P1
    REAL (KIND=RK) PD (0:N)
    REAL (KIND=RK) PF
    REAL (KIND=RK) PL (0:N)
    REAL (KIND=RK) PN (0:N)
    REAL (KIND=RK) R
    REAL (KIND=RK) X

    PN (0) = 1.0D+00
    PN (1) = X
    PD (0) = 0.0D+00
    PD (1) = 1.0D+00
    PL (0) = X
    PL (1) = 0.5D+00 * X * X
    P0 = 1.0D+00
    P1 = X

    DO K = 2, N

        PF = (2.0D+00*K-1.0D+00) / K * X * P1 - (K-1.0D+00) / K * P0
        PN (K) = PF

        IF (ABS(X) == 1.0D+00) THEN
            PD (K) = 0.5D+00 * X ** (K+1) * K * (K+1.0D+00)
        ELSE
            PD (K) = K * (P1-X*PF) / (1.0D+00-X*X)
        END IF

        PL (K) = (X*PN(K)-PN(K-1)) / (K+1.0D+00)
        P0 = P1
        P1 = PF

        IF (K /= 2*INT(K/2)) THEN

            R = 1.0D+00 / (K+1.0D+00)
            N1 = (K-1) / 2
            DO J = 1, N1
                R = (0.5D+00/J-1.0D+00) * R
            END DO
            PL (K) = PL (K) + R

        END IF

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LQMN (MM, M, N, X, QM, QD)

!******************************************************************************
!
!! LQMN computes associated Legendre functions Qmn(x) and derivatives.
!
!  Discussion:
!
!    This routine computes the associated Legendre functions of the
!    second kind, Qmn(x) and Qmn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    13 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer MM, determines the leading dimension
!    of QM and QD.
!
!    Input, integer M, the order of Qmn(x).
!
!    Input, integer N, the degree of Qmn(x).
!
!    Output, real ( kind = rk ) QM(0:MM,0:N), QD(0:MM,0:N), contains the values
!    of Qmn(x) and Qmn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER MM
    INTEGER N

    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER KM
    INTEGER LS
    INTEGER M
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) Q10
    REAL (KIND=RK) QD (0:MM, 0:N)
    REAL (KIND=RK) QF
    REAL (KIND=RK) QF0
    REAL (KIND=RK) QF1
    REAL (KIND=RK) QF2
    REAL (KIND=RK) QM (0:MM, 0:N)
    REAL (KIND=RK) X
    REAL (KIND=RK) XQ
    REAL (KIND=RK) XS

    IF (ABS(X) == 1.0D+00) THEN
        DO I = 0, M
            DO J = 0, N
                QM (I, J) = 1.0D+300
                QD (I, J) = 1.0D+300
            END DO
        END DO
        RETURN
    END IF

    IF (1.0D+00 < ABS(X)) THEN
        LS = - 1
    ELSE
        LS = 1
    END IF

    XS = LS * (1.0D+00-X*X)
    XQ = SQRT (XS)
    Q0 = 0.5D+00 * LOG (ABS((X+1.0D+00)/(X-1.0D+00)))

    IF (ABS(X) < 1.0001D+00) THEN
        QM (0, 0) = Q0
        QM (0, 1) = X * Q0 - 1.0D+00
        QM (1, 0) = - 1.0D+00 / XQ
        QM (1, 1) = - XQ * (Q0+X/(1.0D+00-X*X))
        DO I = 0, 1
            DO J = 2, N
                QM (I, J) = ((2.0D+00*J-1.0D+00)*X*QM(I, J-1)-(J+I-1.0D+00)*QM(I, J-2)) / (J-I)
            END DO
        END DO

        DO J = 0, N
            DO I = 2, M
                QM (I, J) = - 2.0D+00 * (I-1.0D+00) * X / XQ * QM (I-1, J) - LS * (J+I-1.0D+00) &
               & * (J-I+2.0D+00) * QM (I-2, J)
            END DO
        END DO

    ELSE

        IF (1.1D+00 < ABS(X)) THEN
            KM = 40 + M + N
        ELSE
            KM = (40+M+N) * INT (-1.0D+00-1.8D+00*LOG(X-1.0D+00))
        END IF

        QF2 = 0.0D+00
        QF1 = 1.0D+00
        DO K = KM, 0, - 1
            QF0 = ((2*K+3.0D+00)*X*QF1-(K+2.0D+00)*QF2) / (K+1.0D+00)
            IF (K <= N) THEN
                QM (0, K) = QF0
            END IF
            QF2 = QF1
            QF1 = QF0
        END DO

        DO K = 0, N
            QM (0, K) = Q0 * QM (0, K) / QF0
        END DO

        QF2 = 0.0D+00
        QF1 = 1.0D+00
        DO K = KM, 0, - 1
            QF0 = ((2*K+3.0D+00)*X*QF1-(K+1.0D+00)*QF2) / (K+2.0D+00)
            IF (K <= N) THEN
                QM (1, K) = QF0
            END IF
            QF2 = QF1
            QF1 = QF0
        END DO

        Q10 = - 1.0D+00 / XQ
        DO K = 0, N
            QM (1, K) = Q10 * QM (1, K) / QF0
        END DO

        DO J = 0, N
            Q0 = QM (0, J)
            Q1 = QM (1, J)
            DO I = 0, M - 2
                QF = - 2.0D+00 * (I+1) * X / XQ * Q1 + (J-I) * (J+I+1.0D+00) * Q0
                QM (I+2, J) = QF
                Q0 = Q1
                Q1 = QF
            END DO
        END DO

    END IF

    QD (0, 0) = LS / XS
    DO J = 1, N
        QD (0, J) = LS * J * (QM(0, J-1)-X*QM(0, J)) / XS
    END DO

    DO J = 0, N
        DO I = 1, M
            QD (I, J) = LS * I * X / XS * QM (I, J) + (I+J) * (J-I+1.0D+00) / XQ * QM (I-1, J)
        END DO
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LQNA (N, X, QN, QD)

!******************************************************************************
!
!! LQNA computes Legendre function Qn(x) and derivatives Qn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    19 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the degree of Qn(x).
!
!    Input, real ( kind = rk ) X, the argument of Qn(x).
!
!    Output, real ( kind = rk ) QN(0:N), QD(0:N), the values of
!    Qn(x) and Qn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    INTEGER K
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) QD (0:N)
    REAL (KIND=RK) QF
    REAL (KIND=RK) QN (0:N)
    REAL (KIND=RK) X

    IF (ABS(X) == 1.0D+00) THEN

        DO K = 0, N
            QN (K) = 1.0D+300
            QD (K) = - 1.0D+300
        END DO

    ELSE IF (ABS(X) < 1.0D+00) THEN

        Q0 = 0.5D+00 * LOG ((1.0D+00+X)/(1.0D+00-X))
        Q1 = X * Q0 - 1.0D+00
        QN (0) = Q0
        QN (1) = Q1
        QD (0) = 1.0D+00 / (1.0D+00-X*X)
        QD (1) = QN (0) + X * QD (0)
        DO K = 2, N
            QF = ((2*K-1)*X*Q1-(K-1)*Q0) / K
            QN (K) = QF
            QD (K) = (QN(K-1)-X*QF) * K / (1.0D+00-X*X)
            Q0 = Q1
            Q1 = QF
        END DO

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE LQNB (N, X, QN, QD)

!******************************************************************************
!
!! LQNB computes Legendre function Qn(x) and derivatives Qn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    19 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the degree of Qn(x).
!
!    Input, real ( kind = rk ) X, the argument of Qn(x).
!
!    Output, real ( kind = rk ) QN(0:N), QD(0:N), the values of
!    Qn(x) and Qn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) EPS
    INTEGER J
    INTEGER K
    INTEGER L
    INTEGER NL
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) QC1
    REAL (KIND=RK) QC2
    REAL (KIND=RK) QD (0:N)
    REAL (KIND=RK) QF
    REAL (KIND=RK) QF0
    REAL (KIND=RK) QF1
    REAL (KIND=RK) QF2
    REAL (KIND=RK) QN (0:N)
    REAL (KIND=RK) QR
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    EPS = 1.0D-14

    IF (ABS(X) == 1.0D+00) THEN
        DO K = 0, N
            QN (K) = 1.0D+300
            QD (K) = 1.0D+300
        END DO
        RETURN
    END IF

    IF (X <= 1.021D+00) THEN

        X2 = ABS ((1.0D+00+X)/(1.0D+00-X))
        Q0 = 0.5D+00 * LOG (X2)
        Q1 = X * Q0 - 1.0D+00
        QN (0) = Q0
        QN (1) = Q1
        QD (0) = 1.0D+00 / (1.0D+00-X*X)
        QD (1) = QN (0) + X * QD (0)
        DO K = 2, N
            QF = ((2.0D+00*K-1.0D+00)*X*Q1-(K-1.0D+00)*Q0) / K
            QN (K) = QF
            QD (K) = (QN(K-1)-X*QF) * K / (1.0D+00-X*X)
            Q0 = Q1
            Q1 = QF
        END DO

    ELSE

        QC2 = 1.0D+00 / X
        DO J = 1, N
            QC2 = QC2 * J / ((2.0D+00*J+1.0D+00)*X)
            IF (J == N-1) THEN
                QC1 = QC2
            END IF
        END DO

        DO L = 0, 1

            NL = N + L
            QF = 1.0D+00
            QR = 1.0D+00
            DO K = 1, 500
                QR = QR * (0.5D+00*NL+K-1.0D+00) * (0.5D+00*(NL-1)+K) / ((NL+K-0.5D+00)*K*X*X)
                QF = QF + QR
                IF (ABS(QR/QF) < EPS) THEN
                    EXIT
                END IF
            END DO

            IF (L == 0) THEN
                QN (N-1) = QF * QC1
            ELSE
                QN (N) = QF * QC2
            END IF

        END DO

        QF2 = QN (N)
        QF1 = QN (N-1)
        DO K = N, 2, - 1
            QF0 = ((2.0D+00*K-1.0D+00)*X*QF1-K*QF2) / (K-1.0D+00)
            QN (K-2) = QF0
            QF2 = QF1
            QF1 = QF0
        END DO

        QD (0) = 1.0D+00 / (1.0D+00-X*X)
        DO K = 1, N
            QD (K) = K * (QN(K-1)-X*QN(K)) / (1.0D+00-X*X)
        END DO

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CLPMN (MM, M, N, X, Y, CPM, CPD)

!******************************************************************************
!
!! CLPMN: associated Legendre functions and derivatives for complex argument.
!
!  Discussion:
!
!    Compute the associated Legendre functions Pmn(z)
!    and their derivatives Pmn'(z) for a complex argument
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer MM, the physical dimension of CPM and CPD.
!
!    Input, integer M, N, the order and degree of Pmn(z).
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts of
!    the argument Z.
!
!    Output, complex ( kind = ck ) CPM(0:MM,0:N), CPD(0:MM,0:N), the values of
!    Pmn(z) and Pmn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER MM
    INTEGER N
    COMPLEX (KIND=CK) CPD (0:MM, 0:N)
    COMPLEX (KIND=CK) CPM (0:MM, 0:N)
    INTEGER I
    INTEGER J
    INTEGER LS
    INTEGER M
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZQ
    COMPLEX (KIND=CK) ZS

    Z = CMPLX (X, Y, KIND=CK)

    DO I = 0, N
        DO J = 0, M
            CPM (J, I) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CPD (J, I) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        END DO
    END DO

    CPM (0, 0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

    IF (ABS(X) == 1.0D+00 .AND. Y == 0.0D+00) THEN

        DO I = 1, N
            CPM (0, I) = X ** I
            CPD (0, I) = 0.5D+00 * I * (I+1) * X ** (I+1)
        END DO

        DO J = 1, N
            DO I = 1, M
                IF (I == 1) THEN
                    CPD (I, J) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
                ELSE IF (I == 2) THEN
                    CPD (I, J) = - 0.25D+00 * (J+2) * (J+1) * J * (J-1) * X ** (J+1)
                END IF
            END DO
        END DO

        RETURN

    END IF

    IF (1.0D+00 < ABS(Z)) THEN
        LS = - 1
    ELSE
        LS = 1
    END IF

    ZQ = SQRT (LS*(1.0D+00-Z*Z))
    ZS = LS * (1.0D+00-Z*Z)
    DO I = 1, M
        CPM (I, I) = - LS * (2.0D+00*I-1.0D+00) * ZQ * CPM (I-1, I-1)
    END DO
    DO I = 0, M
        CPM (I, I+1) = (2.0D+00*I+1.0D+00) * Z * CPM (I, I)
    END DO

    DO I = 0, M
        DO J = I + 2, N
            CPM (I, J) = ((2.0D+00*J-1.0D+00)*Z*CPM(I, J-1)-(I+J-1.0D+00)*CPM(I, J-2)) / (J-I)
        END DO
    END DO

    CPD (0, 0) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    DO J = 1, N
        CPD (0, J) = LS * J * (CPM(0, J-1)-Z*CPM(0, J)) / ZS
    END DO

    DO I = 1, M
        DO J = I, N
            CPD (I, J) = LS * I * Z * CPM (I, J) / ZS + (J+I) * (J-I+1.0D+00) / ZQ * CPM (I-1, &
           & J)
        END DO
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CLPN (N, X, Y, CPN, CPD)

!******************************************************************************
!
!! CLPN computes Legendre functions and derivatives for complex argument.
!
!  Discussion:
!
!    Compute Legendre polynomials Pn(z) and their derivatives Pn'(z) for
!    a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the degree.
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts
!    of the argument.
!
!    Output, complex ( kind = ck ) CPN(0:N), CPD(0:N), the values of Pn(z)
!    and Pn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    COMPLEX (KIND=CK) CP0
    COMPLEX (KIND=CK) CP1
    COMPLEX (KIND=CK) CPD (0:N)
    COMPLEX (KIND=CK) CPF
    COMPLEX (KIND=CK) CPN (0:N)
    INTEGER K
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z

    Z = CMPLX (X, Y, KIND=CK)

    CPN (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
    CPN (1) = Z
    CPD (0) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CPD (1) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

    CP0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
    CP1 = Z
    DO K = 2, N
        CPF = (2.0D+00*K-1.0D+00) / K * Z * CP1 - (K-1.0D+00) / K * CP0
        CPN (K) = CPF
        IF (ABS(X) == 1.0D+00 .AND. Y == 0.0D+00) THEN
            CPD (K) = 0.5D+00 * X ** (K+1) * K * (K+1.0D+00)
        ELSE
            CPD (K) = K * (CP1-Z*CPF) / (1.0D+00-Z*Z)
        END IF
        CP0 = CP1
        CP1 = CPF
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CLQMN (MM, M, N, X, Y, CQM, CQD)

!******************************************************************************
!
!! CLQMN: associated Legendre functions and derivatives for complex argument.
!
!  Discussion:
!
!    This procedure computes the associated Legendre functions of the second
!    kind, Qmn(z) and Qmn'(z), for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer MM, the physical dimension of CQM and CQD.
!
!    Input, integer M, N, the order and degree of Qmn(z).
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts of the
!    argument Z.
!
!    Output, complex ( kind = ck ) CQM(0:MM,0:N), CQD(0:MM,0:N), the values of
!    Qmn(z) and Qmn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER MM
    INTEGER N

    COMPLEX (KIND=CK) CQ0
    COMPLEX (KIND=CK) CQ1
    COMPLEX (KIND=CK) CQ10
    COMPLEX (KIND=CK) CQF
    COMPLEX (KIND=CK) CQF0
    COMPLEX (KIND=CK) CQF1
    COMPLEX (KIND=CK) CQF2
    COMPLEX (KIND=CK) CQM (0:MM, 0:N)
    COMPLEX (KIND=CK) CQD (0:MM, 0:N)
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER KM
    INTEGER LS
    INTEGER M
    REAL (KIND=RK) X
    REAL (KIND=RK) XC
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZQ
    COMPLEX (KIND=CK) ZS

    Z = CMPLX (X, Y, KIND=CK)

    IF (ABS(X) == 1.0D+00 .AND. Y == 0.0D+00) THEN
        DO I = 0, M
            DO J = 0, N
                CQM (I, J) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
                CQD (I, J) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            END DO
        END DO
        RETURN
    END IF

    XC = ABS (Z)

    IF (AIMAG(Z) == 0.0D+00 .OR. XC < 1.0D+00) THEN
        LS = 1
    END IF

    IF (1.0D+00 < XC) THEN
        LS = - 1
    END IF

    ZQ = SQRT (LS*(1.0D+00-Z*Z))
    ZS = LS * (1.0D+00-Z*Z)
    CQ0 = 0.5D+00 * LOG (LS*(1.0D+00+Z)/(1.0D+00-Z))

    IF (XC < 1.0001D+00) THEN

        CQM (0, 0) = CQ0
        CQM (0, 1) = Z * CQ0 - 1.0D+00
        CQM (1, 0) = - 1.0D+00 / ZQ
        CQM (1, 1) = - ZQ * (CQ0+Z/(1.0D+00-Z*Z))
        DO I = 0, 1
            DO J = 2, N
                CQM (I, J) = ((2.0D+00*J-1.0D+00)*Z*CQM(I, J-1)-(J+I-1.0D+00)*CQM(I, J-2)) / &
               & (J-I)
            END DO
        END DO

        DO J = 0, N
            DO I = 2, M
                CQM (I, J) = - 2.0D+00 * (I-1.0D+00) * Z / ZQ * CQM (I-1, J) - LS * &
               & (J+I-1.0D+00) * (J-I+2.0D+00) * CQM (I-2, J)
            END DO
        END DO

    ELSE

        IF (1.1D+00 < XC) THEN
            KM = 40 + M + N
        ELSE
            KM = (40+M+N) * INT (-1.0D+00-1.8D+00*LOG(XC-1.0D+00))
        END IF

        CQF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CQF1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = KM, 0, - 1
            CQF0 = ((2*K+3.0D+00)*Z*CQF1-(K+2.0D+00)*CQF2) / (K+1.0D+00)
            IF (K <= N) THEN
                CQM (0, K) = CQF0
            END IF
            CQF2 = CQF1
            CQF1 = CQF0
        END DO

        DO K = 0, N
            CQM (0, K) = CQ0 * CQM (0, K) / CQF0
        END DO

        CQF2 = 0.0D+00
        CQF1 = 1.0D+00
        DO K = KM, 0, - 1
            CQF0 = ((2*K+3.0D+00)*Z*CQF1-(K+1.0D+00)*CQF2) / (K+2.0D+00)
            IF (K <= N) THEN
                CQM (1, K) = CQF0
            END IF
            CQF2 = CQF1
            CQF1 = CQF0
        END DO

        CQ10 = - 1.0D+00 / ZQ
        DO K = 0, N
            CQM (1, K) = CQ10 * CQM (1, K) / CQF0
        END DO

        DO J = 0, N
            CQ0 = CQM (0, J)
            CQ1 = CQM (1, J)
            DO I = 0, M - 2
                CQF = - 2.0D+00 * (I+1) * Z / ZQ * CQ1 + (J-I) * (J+I+1.0D+00) * CQ0
                CQM (I+2, J) = CQF
                CQ0 = CQ1
                CQ1 = CQF
            END DO
        END DO

    END IF

    CQD (0, 0) = LS / ZS
    DO J = 1, N
        CQD (0, J) = LS * J * (CQM(0, J-1)-Z*CQM(0, J)) / ZS
    END DO

    DO J = 0, N
        DO I = 1, M
            CQD (I, J) = LS * I * Z / ZS * CQM (I, J) + (I+J) * (J-I+1.0D+00) / ZQ * CQM (I-1, &
           & J)
        END DO
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CLQN (N, X, Y, CQN, CQD)

!******************************************************************************
!
!! CLQN: Legendre function Qn(z) and derivative Wn'(z) for complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the degree of Qn(z).
!
!    Input, real ( kind = rk ) X, Y, the real and imaginary parts of the
!    argument Z.
!
!    Output, complex ( kind = ck ) CQN(0:N), CQD(0:N), the values of Qn(z)
!    and Qn'(z.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    COMPLEX (KIND=CK) CQ0
    COMPLEX (KIND=CK) CQ1
    COMPLEX (KIND=CK) CQF0
    COMPLEX (KIND=CK) CQF1
    COMPLEX (KIND=CK) CQF2
    COMPLEX (KIND=CK) CQN (0:N)
    COMPLEX (KIND=CK) CQD (0:N)
    INTEGER K
    INTEGER KM
    INTEGER LS
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z

    Z = CMPLX (X, Y, KIND=CK)

    IF (Z == 1.0D+00) THEN
        DO K = 0, N
            CQN (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CQD (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO
        RETURN
    END IF

    IF (1.0D+00 < ABS(Z)) THEN
        LS = - 1
    ELSE
        LS = + 1
    END IF

    CQ0 = 0.5D+00 * LOG (LS*(1.0D+00+Z)/(1.0D+00-Z))
    CQ1 = Z * CQ0 - 1.0D+00
    CQN (0) = CQ0
    CQN (1) = CQ1

    IF (ABS(Z) < 1.0001D+00) THEN

        CQF0 = CQ0
        CQF1 = CQ1
        DO K = 2, N
            CQF2 = ((2.0D+00*K-1.0D+00)*Z*CQF1-(K-1.0D+00)*CQF0) / K
            CQN (K) = CQF2
            CQF0 = CQF1
            CQF1 = CQF2
        END DO

    ELSE

        IF (1.1D+00 < ABS(Z)) THEN
            KM = 40 + N
        ELSE
            KM = (40+N) * INT (-1.0D+00-1.8D+00*LOG(ABS(Z-1.0D+00)))
        END IF

        CQF2 = 0.0D+00
        CQF1 = 1.0D+00
        DO K = KM, 0, - 1
            CQF0 = ((2*K+3.0D+00)*Z*CQF1-(K+2.0D+00)*CQF2) / (K+1.0D+00)
            IF (K <= N) THEN
                CQN (K) = CQF0
            END IF
            CQF2 = CQF1
            CQF1 = CQF0
        END DO
        DO K = 0, N
            CQN (K) = CQN (K) * CQ0 / CQF0
        END DO
    END IF

    CQD (0) = (CQN(1)-Z*CQN(0)) / (Z*Z-1.0D+00)
    DO K = 1, N
        CQD (K) = (K*Z*CQN(K)-K*CQN(K-1)) / (Z*Z-1.0D+00)
    END DO

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   05: BESSEL FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE JY01A (X, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)

!******************************************************************************
!
!! JY01A computes Bessel functions J0(x), J1(x), Y0(x), Y1(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1,
!    the values of J0(x), J0'(x), J1(x), J1'(x), Y0(x), Y0'(x), Y1(x), Y1'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (12) :: A = (/ - 0.7031250000000000D-01, &
   & 0.1121520996093750D+00, - 0.5725014209747314D+00, 0.6074042001273483D+01, - &
   & 0.1100171402692467D+03, 0.3038090510922384D+04, - 0.1188384262567832D+06, &
   & 0.6252951493434797D+07, - 0.4259392165047669D+09, 0.3646840080706556D+11, - &
   & 0.3833534661393944D+13, 0.4854014686852901D+15 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: A1 = (/ 0.1171875000000000D+00, - &
   & 0.1441955566406250D+00, 0.6765925884246826D+00, - 0.6883914268109947D+01, &
   & 0.1215978918765359D+03, - 0.3302272294480852D+04, 0.1276412726461746D+06, - &
   & 0.6656367718817688D+07, 0.4502786003050393D+09, - 0.3833857520742790D+11, &
   & 0.4011838599133198D+13, - 0.5060568503314727D+15 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B = (/ 0.7324218750000000D-01, - &
   & 0.2271080017089844D+00, 0.1727727502584457D+01, - 0.2438052969955606D+02, &
   & 0.5513358961220206D+03, - 0.1825775547429318D+05, 0.8328593040162893D+06, - &
   & 0.5006958953198893D+08, 0.3836255180230433D+10, - 0.3649010818849833D+12, &
   & 0.4218971570284096D+14, - 0.5827244631566907D+16 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B1 = (/ - 0.1025390625000000D+00, &
   & 0.2775764465332031D+00, - 0.1993531733751297D+01, 0.2724882731126854D+02, - &
   & 0.6038440767050702D+03, 0.1971837591223663D+05, - 0.8902978767070678D+06, &
   & 0.5310411010968522D+08, - 0.4043620325107754D+10, 0.3827011346598605D+12, - &
   & 0.4406481417852278D+14, 0.6065091351222699D+16 /)
    REAL (KIND=RK) BJ0
    REAL (KIND=RK) BJ1
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) CS0
    REAL (KIND=RK) CS1
    REAL (KIND=RK) CU
    REAL (KIND=RK) DJ0
    REAL (KIND=RK) DJ1
    REAL (KIND=RK) DY0
    REAL (KIND=RK) DY1
    REAL (KIND=RK) EC
    INTEGER K
    INTEGER K0
    REAL (KIND=RK) P0
    REAL (KIND=RK) P1
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) RP2
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2
    REAL (KIND=RK) W0
    REAL (KIND=RK) W1
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    X2 = X * X

    IF (X == 0.0D+00) THEN
        BJ0 = 1.0D+00
        BJ1 = 0.0D+00
        DJ0 = 0.0D+00
        DJ1 = 0.5D+00
        BY0 = - 1.0D+300
        BY1 = - 1.0D+300
        DY0 = 1.0D+300
        DY1 = 1.0D+300
        RETURN
    END IF

    IF (X <= 12.0D+00) THEN

        BJ0 = 1.0D+00
        R = 1.0D+00
        DO K = 1, 30
            R = - 0.25D+00 * R * X2 / (K*K)
            BJ0 = BJ0 + R
            IF (ABS(R) < ABS(BJ0)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        BJ1 = 1.0D+00
        R = 1.0D+00
        DO K = 1, 30
            R = - 0.25D+00 * R * X2 / (K*(K+1.0D+00))
            BJ1 = BJ1 + R
            IF (ABS(R) < ABS(BJ1)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        BJ1 = 0.5D+00 * X * BJ1
        EC = LOG (X/2.0D+00) + 0.5772156649015329D+00
        CS0 = 0.0D+00
        W0 = 0.0D+00
        R0 = 1.0D+00
        DO K = 1, 30
            W0 = W0 + 1.0D+00 / K
            R0 = - 0.25D+00 * R0 / (K*K) * X2
            R = R0 * W0
            CS0 = CS0 + R
            IF (ABS(R) < ABS(CS0)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        BY0 = RP2 * (EC*BJ0-CS0)
        CS1 = 1.0D+00
        W1 = 0.0D+00
        R1 = 1.0D+00
        DO K = 1, 30
            W1 = W1 + 1.0D+00 / K
            R1 = - 0.25D+00 * R1 / (K*(K+1)) * X2
            R = R1 * (2.0D+00*W1+1.0D+00/(K+1.0D+00))
            CS1 = CS1 + R
            IF (ABS(R) < ABS(CS1)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        BY1 = RP2 * (EC*BJ1-1.0D+00/X-0.25D+00*X*CS1)

    ELSE

        IF (X < 35.0D+00) THEN
            K0 = 12
        ELSE IF (X < 50.0D+00) THEN
            K0 = 10
        ELSE
            K0 = 8
        END IF

        T1 = X - 0.25D+00 * PI
        P0 = 1.0D+00
        Q0 = - 0.125D+00 / X
        DO K = 1, K0
            P0 = P0 + A (K) * X ** (-2*K)
            Q0 = Q0 + B (K) * X ** (-2*K-1)
        END DO
        CU = SQRT (RP2/X)
        BJ0 = CU * (P0*COS(T1)-Q0*SIN(T1))
        BY0 = CU * (P0*SIN(T1)+Q0*COS(T1))
        T2 = X - 0.75D+00 * PI
        P1 = 1.0D+00
        Q1 = 0.375D+00 / X
        DO K = 1, K0
            P1 = P1 + A1 (K) * X ** (-2*K)
            Q1 = Q1 + B1 (K) * X ** (-2*K-1)
        END DO
        CU = SQRT (RP2/X)
        BJ1 = CU * (P1*COS(T2)-Q1*SIN(T2))
        BY1 = CU * (P1*SIN(T2)+Q1*COS(T2))

    END IF

    DJ0 = - BJ1
    DJ1 = BJ0 - BJ1 / X
    DY0 = - BY1
    DY1 = BY0 - BY1 / X

    RETURN
END

!******************************************************************************

SUBROUTINE JY01B (X, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)

!******************************************************************************
!
!! JY01B computes Bessel functions J0(x), J1(x), Y0(x), Y1(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1,
!    the values of J0(x), J0'(x), J1(x), J1'(x), Y0(x), Y0'(x), Y1(x), Y1'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) BJ0
    REAL (KIND=RK) BJ1
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) DJ0
    REAL (KIND=RK) DJ1
    REAL (KIND=RK) DY0
    REAL (KIND=RK) DY1
    REAL (KIND=RK) P0
    REAL (KIND=RK) P1
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) T
    REAL (KIND=RK) T2
    REAL (KIND=RK) TA0
    REAL (KIND=RK) TA1
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN

        BJ0 = 1.0D+00
        BJ1 = 0.0D+00
        DJ0 = 0.0D+00
        DJ1 = 0.5D+00
        BY0 = - 1.0D+300
        BY1 = - 1.0D+300
        DY0 = 1.0D+300
        DY1 = 1.0D+300
        RETURN

    ELSE IF (X <= 4.0D+00) THEN

        T = X / 4.0D+00
        T2 = T * T

        BJ0 = ((((((-0.5014415D-03*T2+0.76771853D-02)*T2-0.0709253492D+00)*T2+0.4443584263D+00)&
       & *T2-1.7777560599D+00)*T2+3.9999973021D+00)*T2-3.9999998721D+00) * T2 + 1.0D+00

        BJ1 = T * (((((((-0.1289769D-03*T2+0.22069155D-02)*T2-0.0236616773D+00)*T2+&
       & 0.1777582922D+00)*T2-0.8888839649D+00)*T2+2.6666660544D+00)*T2-3.9999999710D+00)*T2+&
       & 1.9999999998D+00)

        BY0 = (((((((-0.567433D-04*T2+0.859977D-03)*T2-0.94855882D-02)*T2+0.0772975809D+00)*T2-&
       & 0.4261737419D+00)*T2+1.4216421221D+00)*T2-2.3498519931D+00)*T2+1.0766115157D+00) * T2 +&
       &  0.3674669052D+00

        BY0 = 2.0D+00 / PI * LOG (X/2.0D+00) * BJ0 + BY0

        BY1 = ((((((((0.6535773D-03*T2-0.0108175626D+00)*T2+0.107657606D+00)*T2-0.7268945577D+00)&
       & *T2+3.1261399273D+00)*T2-7.3980241381D+00)*T2+6.8529236342D+00)*T2+0.3932562018D+00)&
       & *T2-0.6366197726D+00) / X

        BY1 = 2.0D+00 / PI * LOG (X/2.0D+00) * BJ1 + BY1

    ELSE

        T = 4.0D+00 / X
        T2 = T * T
        A0 = SQRT (2.0D+00/(PI*X))

        P0 = &
       & ((((-0.9285D-05*T2+0.43506D-04)*T2-0.122226D-03)*T2+0.434725D-03)*T2-0.4394275D-02) * &
       & T2 + 0.999999997D+00

        Q0 = T * (((((0.8099D-05*T2-0.35614D-04)*T2+0.85844D-04)*T2-0.218024D-03)*T2&
       & +0.1144106D-02)*T2-0.031249995D+00)

        TA0 = X - 0.25D+00 * PI
        BJ0 = A0 * (P0*COS(TA0)-Q0*SIN(TA0))
        BY0 = A0 * (P0*SIN(TA0)+Q0*COS(TA0))

        P1 = &
       & ((((0.10632D-04*T2-0.50363D-04)*T2+0.145575D-03)*T2-0.559487D-03)*T2+0.7323931D-02) * &
       & T2 + 1.000000004D+00

        Q1 = T * (((((-0.9173D-05*T2+0.40658D-04)*T2-0.99941D-04)*T2+0.266891D-03)*T2-&
       & 0.1601836D-02)*T2+0.093749994D+00)

        TA1 = X - 0.75D+00 * PI
        BJ1 = A0 * (P1*COS(TA1)-Q1*SIN(TA1))
        BY1 = A0 * (P1*SIN(TA1)+Q1*COS(TA1))

    END IF

    DJ0 = - BJ1
    DJ1 = BJ0 - BJ1 / X
    DY0 = - BY1
    DY1 = BY0 - BY1 / X

    RETURN
END

!******************************************************************************

SUBROUTINE JYNA (N, X, NM, BJ, DJ, BY, DY)

!******************************************************************************
!
!! JYNA computes Bessel functions Jn(x) and Yn(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 April 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) BJ(0:N), DJ(0:N), BY(0:N), DY(0:N), the values
!    of Jn(x), Jn'(x), Yn(x), Yn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BJ (0:N)
    REAL (KIND=RK) BJ0
    REAL (KIND=RK) BJ1
    REAL (KIND=RK) BJK
    REAL (KIND=RK) BY (0:N)
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) CS
    REAL (KIND=RK) DJ (0:N)
    REAL (KIND=RK) DJ0
    REAL (KIND=RK) DJ1
    REAL (KIND=RK) DY (0:N)
    REAL (KIND=RK) DY0
    REAL (KIND=RK) DY1
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) X

    NM = N

    IF (X < 1.0D-100) THEN

        DO K = 0, N
            BJ (K) = 0.0D+00
            DJ (K) = 0.0D+00
            BY (K) = - 1.0D+300
            DY (K) = 1.0D+300
        END DO
        BJ (0) = 1.0D+00
        DJ (1) = 0.5D+00
        RETURN

    END IF

    CALL JY01B (X, BJ0, DJ0, BJ1, DJ1, BY0, DY0, BY1, DY1)
    BJ (0) = BJ0
    BJ (1) = BJ1
    BY (0) = BY0
    BY (1) = BY1
    DJ (0) = DJ0
    DJ (1) = DJ1
    DY (0) = DY0
    DY (1) = DY1

    IF (N <= 1) THEN
        RETURN
    END IF

    IF (N < INT(0.9D+00*X)) THEN

        DO K = 2, N
            BJK = 2.0D+00 * (K-1.0D+00) / X * BJ1 - BJ0
            BJ (K) = BJK
            BJ0 = BJ1
            BJ1 = BJK
        END DO

    ELSE

        M = MSTA1 (X, 200)

        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF

        F2 = 0.0D+00
        F1 = 1.0D-100
        DO K = M, 0, - 1
            F = 2.0D+00 * (K+1.0D+00) / X * F1 - F2
            IF (K <= NM) THEN
                BJ (K) = F
            END IF
            F2 = F1
            F1 = F
        END DO

        IF (ABS(BJ1) < ABS(BJ0)) THEN
            CS = BJ0 / F
        ELSE
            CS = BJ1 / F2
        END IF

        DO K = 0, NM
            BJ (K) = CS * BJ (K)
        END DO

    END IF

    DO K = 2, NM
        DJ (K) = BJ (K-1) - K / X * BJ (K)
    END DO

    F0 = BY (0)
    F1 = BY (1)
    DO K = 2, NM
        F = 2.0D+00 * (K-1.0D+00) / X * F1 - F0
        BY (K) = F
        F0 = F1
        F1 = F
    END DO

    DO K = 2, NM
        DY (K) = BY (K-1) - K * BY (K) / X
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE JYNB (N, X, NM, BJ, DJ, BY, DY)

!******************************************************************************
!
!! JYNB computes Bessel functions Jn(x) and Yn(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) BJ(0:N), DJ(0:N), BY(0:N), DY(0:N), the values
!    of Jn(x), Jn'(x), Yn(x), Yn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK), SAVE, DIMENSION (4) :: A = (/ - 0.7031250000000000D-01, &
   & 0.1121520996093750D+00, - 0.5725014209747314D+00, 0.6074042001273483D+01 /)
    REAL (KIND=RK), SAVE, DIMENSION (4) :: A1 = (/ 0.1171875000000000D+00, - &
   & 0.1441955566406250D+00, 0.6765925884246826D+00, - 0.6883914268109947D+01 /)
    REAL (KIND=RK), SAVE, DIMENSION (4) :: B = (/ 0.7324218750000000D-01, - &
   & 0.2271080017089844D+00, 0.1727727502584457D+01, - 0.2438052969955606D+02 /)
    REAL (KIND=RK), SAVE, DIMENSION (4) :: B1 = (/ - 0.1025390625000000D+00, &
   & 0.2775764465332031D+00, - 0.1993531733751297D+01, 0.2724882731126854D+02 /)
    REAL (KIND=RK) BJ (0:N)
    REAL (KIND=RK) BJ0
    REAL (KIND=RK) BJ1
    REAL (KIND=RK) BJK
    REAL (KIND=RK) BS
    REAL (KIND=RK) BY (0:N)
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) BYK
    REAL (KIND=RK) CU
    REAL (KIND=RK) DJ (0:N)
    REAL (KIND=RK) DY (0:N)
    REAL (KIND=RK) EC
    REAL (KIND=RK) F
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) P0
    REAL (KIND=RK) P1
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) R2P
    REAL (KIND=RK) S0
    REAL (KIND=RK) SU
    REAL (KIND=RK) SV
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    R2P = 0.63661977236758D+00
    NM = N

    IF (X < 1.0D-100) THEN
        DO K = 0, N
            BJ (K) = 0.0D+00
            DJ (K) = 0.0D+00
            BY (K) = - 1.0D+300
            DY (K) = 1.0D+300
        END DO
        BJ (0) = 1.0D+00
        DJ (1) = 0.5D+00
        RETURN
    END IF

    IF (X <= 300.0D+00 .OR. INT(0.9D+00*X) < N) THEN

        IF (N == 0) THEN
            NM = 1
        END IF

        M = MSTA1 (X, 200)

        IF (M < NM) THEN
            NM = M
        ELSE
            M = MSTA2 (X, NM, 15)
        END IF

        BS = 0.0D+00
        SU = 0.0D+00
        SV = 0.0D+00
        F2 = 0.0D+00
        F1 = 1.0D-100

        DO K = M, 0, - 1
            F = 2.0D+00 * (K+1.0D+00) / X * F1 - F2
            IF (K <= NM) THEN
                BJ (K) = F
            END IF
            IF (K == 2*INT(K/2) .AND. K /= 0) THEN
                BS = BS + 2.0D+00 * F
                SU = SU + (-1.0D+00) ** (K/2) * F / K
            ELSE IF (1 < K) THEN
                SV = SV + (-1.0D+00) ** (K/2) * K / (K*K-1.0D+00) * F
            END IF
            F2 = F1
            F1 = F
        END DO

        S0 = BS + F
        DO K = 0, NM
            BJ (K) = BJ (K) / S0
        END DO

        EC = LOG (X/2.0D+00) + 0.5772156649015329D+00
        BY0 = R2P * (EC*BJ(0)-4.0D+00*SU/S0)
        BY (0) = BY0
        BY1 = R2P * ((EC-1.0D+00)*BJ(1)-BJ(0)/X-4.0D+00*SV/S0)
        BY (1) = BY1

    ELSE

        T1 = X - 0.25D+00 * PI
        P0 = 1.0D+00
        Q0 = - 0.125D+00 / X
        DO K = 1, 4
            P0 = P0 + A (K) * X ** (-2*K)
            Q0 = Q0 + B (K) * X ** (-2*K-1)
        END DO
        CU = SQRT (R2P/X)
        BJ0 = CU * (P0*COS(T1)-Q0*SIN(T1))
        BY0 = CU * (P0*SIN(T1)+Q0*COS(T1))
        BJ (0) = BJ0
        BY (0) = BY0
        T2 = X - 0.75D+00 * PI
        P1 = 1.0D+00
        Q1 = 0.375D+00 / X
        DO K = 1, 4
            P1 = P1 + A1 (K) * X ** (-2*K)
            Q1 = Q1 + B1 (K) * X ** (-2*K-1)
        END DO
        BJ1 = CU * (P1*COS(T2)-Q1*SIN(T2))
        BY1 = CU * (P1*SIN(T2)+Q1*COS(T2))
        BJ (1) = BJ1
        BY (1) = BY1
        DO K = 2, NM
            BJK = 2.0D+00 * (K-1.0D+00) / X * BJ1 - BJ0
            BJ (K) = BJK
            BJ0 = BJ1
            BJ1 = BJK
        END DO
    END IF

    DJ (0) = - BJ (1)
    DO K = 1, NM
        DJ (K) = BJ (K-1) - K / X * BJ (K)
    END DO

    DO K = 2, NM
        BYK = 2.0D+00 * (K-1.0D+00) * BY1 / X - BY0
        BY (K) = BYK
        BY0 = BY1
        BY1 = BYK
    END DO

    DY (0) = - BY (1)
    DO K = 1, NM
        DY (K) = BY (K-1) - K * BY (K) / X
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CJY01 (Z, CBJ0, CDJ0, CBJ1, CDJ1, CBY0, CDY0, CBY1, CDY1)

!******************************************************************************
!
!! CJY01: complexBessel functions, derivatives, J0(z), J1(z), Y0(z), Y1(z).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CBJ0, CDJ0, CBJ1, CDJ1, CBY0, CDY0, CBY1,
!    CDY1, the values of J0(z), J0'(z), J1(z), J1'(z), Y0(z), Y0'(z),
!    Y1(z), Y1'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (12) :: A = (/ - 0.703125D-01, 0.112152099609375D+00, - &
   & 0.5725014209747314D+00, 0.6074042001273483D+01, - 0.1100171402692467D+03, &
   & 0.3038090510922384D+04, - 0.1188384262567832D+06, 0.6252951493434797D+07, - &
   & 0.4259392165047669D+09, 0.3646840080706556D+11, - 0.3833534661393944D+13, &
   & 0.4854014686852901D+15 /)
    REAL (KIND=RK) A0
    REAL (KIND=RK), SAVE, DIMENSION (12) :: A1 = (/ 0.1171875D+00, - 0.144195556640625D+00, &
   & 0.6765925884246826D+00, - 0.6883914268109947D+01, 0.1215978918765359D+03, - &
   & 0.3302272294480852D+04, 0.1276412726461746D+06, - 0.6656367718817688D+07, &
   & 0.4502786003050393D+09, - 0.3833857520742790D+11, 0.4011838599133198D+13, - &
   & 0.5060568503314727D+15 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B = (/ 0.732421875D-01, - 0.2271080017089844D+00, &
   & 0.1727727502584457D+01, - 0.2438052969955606D+02, 0.5513358961220206D+03, - &
   & 0.1825775547429318D+05, 0.8328593040162893D+06, - 0.5006958953198893D+08, &
   & 0.3836255180230433D+10, - 0.3649010818849833D+12, 0.4218971570284096D+14, - &
   & 0.5827244631566907D+16 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B1 = (/ - 0.1025390625D+00, 0.2775764465332031D+00, &
   & - 0.1993531733751297D+01, 0.2724882731126854D+02, - 0.6038440767050702D+03, &
   & 0.1971837591223663D+05, - 0.8902978767070678D+06, 0.5310411010968522D+08, - &
   & 0.4043620325107754D+10, 0.3827011346598605D+12, - 0.4406481417852278D+14, &
   & 0.6065091351222699D+16 /)
    COMPLEX (KIND=CK) CBJ0
    COMPLEX (KIND=CK) CBJ1
    COMPLEX (KIND=CK) CBY0
    COMPLEX (KIND=CK) CBY1
    COMPLEX (KIND=CK) CDJ0
    COMPLEX (KIND=CK) CDJ1
    COMPLEX (KIND=CK) CDY0
    COMPLEX (KIND=CK) CDY1
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CP
    COMPLEX (KIND=CK) CP0
    COMPLEX (KIND=CK) CP1
    COMPLEX (KIND=CK) CQ0
    COMPLEX (KIND=CK) CQ1
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CT1
    COMPLEX (KIND=CK) CT2
    COMPLEX (KIND=CK) CU
    REAL (KIND=RK) EL
    INTEGER K
    INTEGER K0
    REAL (KIND=RK) PI
    REAL (KIND=RK) RP2
    REAL (KIND=RK) W0
    REAL (KIND=RK) W1
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    RP2 = 2.0D+00 / PI
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z2 = Z * Z
    Z1 = Z

    IF (A0 == 0.0D+00) THEN
        CBJ0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CBJ1 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CDJ0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CDJ1 = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        CBY0 = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CBY1 = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CDY0 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CDY1 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        RETURN
    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 <= 12.0D+00) THEN

        CBJ0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 40
            CR = - 0.25D+00 * CR * Z2 / (K*K)
            CBJ0 = CBJ0 + CR
            IF (ABS(CR) < ABS(CBJ0)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBJ1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 40
            CR = - 0.25D+00 * CR * Z2 / (K*(K+1.0D+00))
            CBJ1 = CBJ1 + CR
            IF (ABS(CR) < ABS(CBJ1)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBJ1 = 0.5D+00 * Z1 * CBJ1
        W0 = 0.0D+00
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 40
            W0 = W0 + 1.0D+00 / K
            CR = - 0.25D+00 * CR / (K*K) * Z2
            CP = CR * W0
            CS = CS + CP
            IF (ABS(CP) < ABS(CS)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBY0 = RP2 * (LOG(Z1/2.0D+00)+EL) * CBJ0 - RP2 * CS
        W1 = 0.0D+00
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 40
            W1 = W1 + 1.0D+00 / K
            CR = - 0.25D+00 * CR / (K*(K+1)) * Z2
            CP = CR * (2.0D+00*W1+1.0D+00/(K+1.0D+00))
            CS = CS + CP
            IF (ABS(CP) < ABS(CS)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBY1 = RP2 * ((LOG(Z1/2.0D+00)+EL)*CBJ1-1.0D+00/Z1-0.25D+00*Z1*CS)

    ELSE

        IF (A0 < 35.0D+00) THEN
            K0 = 12
        ELSE IF (A0 < 50.0D+00) THEN
            K0 = 10
        ELSE
            K0 = 8
        END IF

        CT1 = Z1 - 0.25D+00 * PI

        CP0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CP0 = CP0 + A (K) * Z1 ** (-2*K)
        END DO

        CQ0 = - 0.125D+00 / Z1
        DO K = 1, K0
            CQ0 = CQ0 + B (K) * Z1 ** (-2*K-1)
        END DO

        CU = SQRT (RP2/Z1)
        CBJ0 = CU * (CP0*COS(CT1)-CQ0*SIN(CT1))
        CBY0 = CU * (CP0*SIN(CT1)+CQ0*COS(CT1))
        CT2 = Z1 - 0.75D+00 * PI

        CP1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CP1 = CP1 + A1 (K) * Z1 ** (-2*K)
        END DO

        CQ1 = 0.375D+00 / Z1
        DO K = 1, K0
            CQ1 = CQ1 + B1 (K) * Z1 ** (-2*K-1)
        END DO

        CBJ1 = CU * (CP1*COS(CT2)-CQ1*SIN(CT2))
        CBY1 = CU * (CP1*SIN(CT2)+CQ1*COS(CT2))

    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        IF (AIMAG(Z) < 0.0D+00) THEN
            CBY0 = CBY0 - 2.0D+00 * CI * CBJ0
            CBY1 = - (CBY1-2.0D+00*CI*CBJ1)
        ELSE
            CBY0 = CBY0 + 2.0D+00 * CI * CBJ0
            CBY1 = - (CBY1+2.0D+00*CI*CBJ1)
        END IF
        CBJ1 = - CBJ1
    END IF

    CDJ0 = - CBJ1
    CDJ1 = CBJ0 - 1.0D+00 / Z * CBJ1
    CDY0 = - CBY1
    CDY1 = CBY0 - 1.0D+00 / Z * CBY1

    RETURN
END

!******************************************************************************

SUBROUTINE CJYNA (N, Z, NM, CBJ, CDJ, CBY, CDY)

!******************************************************************************
!
!! CJYNA: Bessel functions and derivatives, Jn(z) and Yn(z) of complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of Jn(z) and Yn(z).
!
!    Input, complex ( kind = ck ) Z, the argument of Jn(z) and Yn(z).
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ), CBJ(0:N), CDJ(0:N), CBY(0:N), CDY(0:N),
!    the values of Jn(z), Jn'(z), Yn(z), Yn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CBJ (0:N)
    COMPLEX (KIND=CK) CBJ0
    COMPLEX (KIND=CK) CBJ1
    COMPLEX (KIND=CK) CBY (0:N)
    COMPLEX (KIND=CK) CBY0
    COMPLEX (KIND=CK) CBY1
    COMPLEX (KIND=CK) CDJ (0:N)
    COMPLEX (KIND=CK) CDJ0
    COMPLEX (KIND=CK) CDJ1
    COMPLEX (KIND=CK) CDY (0:N)
    COMPLEX (KIND=CK) CDY0
    COMPLEX (KIND=CK) CDY1
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CG0
    COMPLEX (KIND=CK) CG1
    COMPLEX (KIND=CK) CH0
    COMPLEX (KIND=CK) CH1
    COMPLEX (KIND=CK) CH2
    COMPLEX (KIND=CK) CJ0
    COMPLEX (KIND=CK) CJ1
    COMPLEX (KIND=CK) CJK
    COMPLEX (KIND=CK) CP11
    COMPLEX (KIND=CK) CP12
    COMPLEX (KIND=CK) CP21
    COMPLEX (KIND=CK) CP22
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CYK
    COMPLEX (KIND=CK) CYL1
    COMPLEX (KIND=CK) CYL2
    COMPLEX (KIND=CK) CYLK
    INTEGER K
    INTEGER LB
    INTEGER LB0
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PI
    REAL (KIND=RK) WA
    REAL (KIND=RK) YA0
    REAL (KIND=RK) YA1
    REAL (KIND=RK) YAK
    COMPLEX (KIND=CK) Z

    PI = 3.141592653589793D+00
    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-100) THEN
        DO K = 0, N
            CBJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBY (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDY (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO
        CBJ (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CDJ (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        RETURN
    END IF

    CALL CJY01 (Z, CBJ0, CDJ0, CBJ1, CDJ1, CBY0, CDY0, CBY1, CDY1)
    CBJ (0) = CBJ0
    CBJ (1) = CBJ1
    CBY (0) = CBY0
    CBY (1) = CBY1
    CDJ (0) = CDJ0
    CDJ (1) = CDJ1
    CDY (0) = CDY0
    CDY (1) = CDY1

    IF (N <= 1) THEN
        RETURN
    END IF

    IF (N < INT(0.25D+00*A0)) THEN

        CJ0 = CBJ0
        CJ1 = CBJ1
        DO K = 2, N
            CJK = 2.0D+00 * (K-1.0D+00) / Z * CJ1 - CJ0
            CBJ (K) = CJK
            CJ0 = CJ1
            CJ1 = CJK
        END DO

    ELSE

        M = MSTA1 (A0, 200)

        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (A0, N, 15)
        END IF

        CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
        DO K = M, 0, - 1
            CF = 2.0D+00 * (K+1.0D+00) / Z * CF1 - CF2
            IF (K <= NM) THEN
                CBJ (K) = CF
            END IF
            CF2 = CF1
            CF1 = CF
        END DO

        IF (ABS(CBJ1) < ABS(CBJ0)) THEN
            CS = CBJ0 / CF
        ELSE
            CS = CBJ1 / CF2
        END IF

        DO K = 0, NM
            CBJ (K) = CS * CBJ (K)
        END DO

    END IF

    DO K = 2, NM
        CDJ (K) = CBJ (K-1) - K / Z * CBJ (K)
    END DO
    YA0 = ABS (CBY0)
    LB = 0
    CG0 = CBY0
    CG1 = CBY1
    DO K = 2, NM
        CYK = 2.0D+00 * (K-1.0D+00) / Z * CG1 - CG0
        IF (ABS(CYK) <= 1.0D+290) THEN
            YAK = ABS (CYK)
            YA1 = ABS (CG0)
            IF (YAK < YA0 .AND. YAK < YA1) THEN
                LB = K
            END IF
            CBY (K) = CYK
            CG0 = CG1
            CG1 = CYK
        END IF
    END DO

    IF (4 < LB .AND. AIMAG(Z) /= 0.0D+00) THEN

        DO

            IF (LB == LB0) THEN
                EXIT
            END IF

            CH2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CH1 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            LB0 = LB
            DO K = LB, 1, - 1
                CH0 = 2.0D+00 * K / Z * CH1 - CH2
                CH2 = CH1
                CH1 = CH0
            END DO
            CP12 = CH0
            CP22 = CH2
            CH2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CH1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = LB, 1, - 1
                CH0 = 2.0D+00 * K / Z * CH1 - CH2
                CH2 = CH1
                CH1 = CH0
            END DO
            CP11 = CH0
            CP21 = CH2

            IF (LB == NM) THEN
                CBJ (LB+1) = 2.0D+00 * LB / Z * CBJ (LB) - CBJ (LB-1)
            END IF

            IF (ABS(CBJ(1)) < ABS(CBJ(0))) THEN
                CBY (LB+1) = (CBJ(LB+1)*CBY0-2.0D+00*CP11/(PI*Z)) / CBJ (0)
                CBY (LB) = (CBJ(LB)*CBY0+2.0D+00*CP12/(PI*Z)) / CBJ (0)
            ELSE
                CBY (LB+1) = (CBJ(LB+1)*CBY1-2.0D+00*CP21/(PI*Z)) / CBJ (1)
                CBY (LB) = (CBJ(LB)*CBY1+2.0D+00*CP22/(PI*Z)) / CBJ (1)
            END IF

            CYL2 = CBY (LB+1)
            CYL1 = CBY (LB)
            DO K = LB - 1, 0, - 1
                CYLK = 2.0D+00 * (K+1.0D+00) / Z * CYL1 - CYL2
                CBY (K) = CYLK
                CYL2 = CYL1
                CYL1 = CYLK
            END DO

            CYL1 = CBY (LB)
            CYL2 = CBY (LB+1)
            DO K = LB + 1, NM - 1
                CYLK = 2.0D+00 * K / Z * CYL2 - CYL1
                CBY (K+1) = CYLK
                CYL1 = CYL2
                CYL2 = CYLK
            END DO

            DO K = 2, NM
                WA = ABS (CBY(K))
                IF (WA < ABS(CBY(K-1))) THEN
                    LB = K
                END IF
            END DO

        END DO

    END IF

    DO K = 2, NM
        CDY (K) = CBY (K-1) - K / Z * CBY (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CJYNB (N, Z, NM, CBJ, CDJ, CBY, CDY)

!******************************************************************************
!
!! CJYNB: Bessel functions, derivatives, Jn(z) and Yn(z) of complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of Jn(z) and Yn(z).
!
!    Input, complex ( kind = ck ) Z, the argument of Jn(z) and Yn(z).
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CBJ(0:N), CDJ(0:N), CBY(0:N), CDY(0:N),
!    the values of Jn(z), Jn'(z), Yn(z), Yn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK), SAVE, DIMENSION (4) :: A = (/ - 0.7031250000000000D-01, &
   & 0.1121520996093750D+00, - 0.5725014209747314D+00, 0.6074042001273483D+01 /)
    REAL (KIND=RK) A0
    REAL (KIND=RK), SAVE, DIMENSION (4) :: A1 = (/ 0.1171875000000000D+00, - &
   & 0.1441955566406250D+00, 0.6765925884246826D+00, - 0.6883914268109947D+01 /)
    REAL (KIND=RK), SAVE, DIMENSION (4) :: B = (/ 0.7324218750000000D-01, - &
   & 0.2271080017089844D+00, 0.1727727502584457D+01, - 0.2438052969955606D+02 /)
    REAL (KIND=RK), SAVE, DIMENSION (4) :: B1 = (/ - 0.1025390625000000D+00, &
   & 0.2775764465332031D+00, - 0.1993531733751297D+01, 0.2724882731126854D+02 /)
    COMPLEX (KIND=CK) CBJ (0:N)
    COMPLEX (KIND=CK) CBJ0
    COMPLEX (KIND=CK) CBJ1
    COMPLEX (KIND=CK) CBJK
    COMPLEX (KIND=CK) CBS
    COMPLEX (KIND=CK) CBY (0:N)
    COMPLEX (KIND=CK) CBY0
    COMPLEX (KIND=CK) CBY1
    COMPLEX (KIND=CK) CDJ (0:N)
    COMPLEX (KIND=CK) CDY (0:N)
    COMPLEX (KIND=CK) CE
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CP0
    COMPLEX (KIND=CK) CP1
    COMPLEX (KIND=CK) CQ0
    COMPLEX (KIND=CK) CQ1
    COMPLEX (KIND=CK) CS0
    COMPLEX (KIND=CK) CSU
    COMPLEX (KIND=CK) CSV
    COMPLEX (KIND=CK) CT1
    COMPLEX (KIND=CK) CT2
    COMPLEX (KIND=CK) CU
    COMPLEX (KIND=CK) CYY
    REAL (KIND=RK) EL
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PI
    REAL (KIND=RK) R2P
    REAL (KIND=RK) Y0
    COMPLEX (KIND=CK) Z

    EL = 0.5772156649015329D+00
    PI = 3.141592653589793D+00
    R2P = 0.63661977236758D+00
    Y0 = ABS (AIMAG(Z))
    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-100) THEN
        DO K = 0, N
            CBJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBY (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDY (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO
        CBJ (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CDJ (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        RETURN
    END IF

    IF (A0 <= 300.0D+00 .OR. 80 < N) THEN

        IF (N == 0) THEN
            NM = 1
        END IF
        M = MSTA1 (A0, 200)
        IF (M < NM) THEN
            NM = M
        ELSE
            M = MSTA2 (A0, NM, 15)
        END IF

        CBS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CSU = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CSV = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)

        DO K = M, 0, - 1
            CF = 2.0D+00 * (K+1.0D+00) / Z * CF1 - CF2
            IF (K <= NM) THEN
                CBJ (K) = CF
            END IF
            IF (K == 2*INT(K/2) .AND. K .NE. 0) THEN
                IF (Y0 <= 1.0D+00) THEN
                    CBS = CBS + 2.0D+00 * CF
                ELSE
                    CBS = CBS + (-1.0D+00) ** (K/2) * 2.0D+00 * CF
                END IF
                CSU = CSU + (-1.0D+00) ** (K/2) * CF / K
            ELSE IF (1 < K) THEN
                CSV = CSV + (-1.0D+00) ** (K/2) * K / (K*K-1.0D+00) * CF
            END IF
            CF2 = CF1
            CF1 = CF
        END DO

        IF (Y0 <= 1.0D+00) THEN
            CS0 = CBS + CF
        ELSE
            CS0 = (CBS+CF) / COS (Z)
        END IF

        DO K = 0, NM
            CBJ (K) = CBJ (K) / CS0
        END DO

        CE = LOG (Z/2.0D+00) + EL
        CBY (0) = R2P * (CE*CBJ(0)-4.0D+00*CSU/CS0)
        CBY (1) = R2P * (-CBJ(0)/Z+(CE-1.0D+00)*CBJ(1)-4.0D+00*CSV/CS0)

    ELSE

        CT1 = Z - 0.25D+00 * PI
        CP0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 4
            CP0 = CP0 + A (K) * Z ** (-2*K)
        END DO
        CQ0 = - 0.125D+00 / Z
        DO K = 1, 4
            CQ0 = CQ0 + B (K) * Z ** (-2*K-1)
        END DO
        CU = SQRT (R2P/Z)
        CBJ0 = CU * (CP0*COS(CT1)-CQ0*SIN(CT1))
        CBY0 = CU * (CP0*SIN(CT1)+CQ0*COS(CT1))
        CBJ (0) = CBJ0
        CBY (0) = CBY0
        CT2 = Z - 0.75D+00 * PI
        CP1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 4
            CP1 = CP1 + A1 (K) * Z ** (-2*K)
        END DO
        CQ1 = 0.375D+00 / Z
        DO K = 1, 4
            CQ1 = CQ1 + B1 (K) * Z ** (-2*K-1)
        END DO
        CBJ1 = CU * (CP1*COS(CT2)-CQ1*SIN(CT2))
        CBY1 = CU * (CP1*SIN(CT2)+CQ1*COS(CT2))
        CBJ (1) = CBJ1
        CBY (1) = CBY1
        DO K = 2, NM
            CBJK = 2.0D+00 * (K-1.0D+00) / Z * CBJ1 - CBJ0
            CBJ (K) = CBJK
            CBJ0 = CBJ1
            CBJ1 = CBJK
        END DO
    END IF

    CDJ (0) = - CBJ (1)
    DO K = 1, NM
        CDJ (K) = CBJ (K-1) - K / Z * CBJ (K)
    END DO

    IF (1.0D+00 < ABS(CBJ(0))) THEN
        CBY (1) = (CBJ(1)*CBY(0)-2.0D+00/(PI*Z)) / CBJ (0)
    END IF

    DO K = 2, NM
        IF (ABS(CBJ(K-2)) <= ABS(CBJ(K-1))) THEN
            CYY = (CBJ(K)*CBY(K-1)-2.0D+00/(PI*Z)) / CBJ (K-1)
        ELSE
            CYY = (CBJ(K)*CBY(K-2)-4.0D+00*(K-1.0D+00)/(PI*Z*Z)) / CBJ (K-2)
        END IF
        CBY (K) = CYY
    END DO

    CDY (0) = - CBY (1)
    DO K = 1, NM
        CDY (K) = CBY (K-1) - K / Z * CBY (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE JYV (V, X, VM, BJ, DJ, BY, DY)

!******************************************************************************
!
!! JYV computes Bessel functions Jv(x) and Yv(x) and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Jv(x) and Yv(x).
!
!    Input, real ( kind = rk ) X, the argument of Jv(x) and Yv(x).
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) BJ(0:N), DJ(0:N), BY(0:N), DY(0:N),
!    the values of Jn+v0(x), Jn+v0'(x), Yn+v0(x), Yn+v0'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) B
    REAL (KIND=RK) BJ (0:*)
    REAL (KIND=RK) BJU0
    REAL (KIND=RK) BJU1
    REAL (KIND=RK) BJV0
    REAL (KIND=RK) BJV1
    REAL (KIND=RK) BJVL
    REAL (KIND=RK) BY (0:*)
    REAL (KIND=RK) BYV0
    REAL (KIND=RK) BYV1
    REAL (KIND=RK) BYVK
    REAL (KIND=RK) CK
    REAL (KIND=RK) CS
    REAL (KIND=RK) CS0
    REAL (KIND=RK) CS1
    REAL (KIND=RK) DJ (0:*)
    REAL (KIND=RK) DY (0:*)
    REAL (KIND=RK) EC
    REAL (KIND=RK) EL
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    INTEGER J
    INTEGER K
    INTEGER K0
    INTEGER L
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV0
    REAL (KIND=RK) PV1
    REAL (KIND=RK) PX
    REAL (KIND=RK) QX
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) RP
    REAL (KIND=RK) RP2
    REAL (KIND=RK) RQ
    REAL (KIND=RK) SK
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VG
    REAL (KIND=RK) VL
    REAL (KIND=RK) VM
    REAL (KIND=RK) VV
    REAL (KIND=RK) W0
    REAL (KIND=RK) W1
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XK

    EL = 0.5772156649015329D+00
    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    X2 = X * X
    N = INT (V)
    V0 = V - N

    IF (X < 1.0D-100) THEN

        DO K = 0, N
            BJ (K) = 0.0D+00
            DJ (K) = 0.0D+00
            BY (K) = - 1.0D+300
            DY (K) = 1.0D+300
        END DO

        IF (V0 == 0.0D+00) THEN
            BJ (0) = 1.0D+00
            DJ (1) = 0.5D+00
        ELSE
            DJ (0) = 1.0D+300
        END IF
        VM = V
        RETURN

    END IF

    IF (X <= 12.0D+00) THEN

        DO L = 0, 1
            VL = V0 + L
            BJVL = 1.0D+00
            R = 1.0D+00
            DO K = 1, 40
                R = - 0.25D+00 * R * X2 / (K*(K+VL))
                BJVL = BJVL + R
                IF (ABS(R) < ABS(BJVL)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            VG = 1.0D+00 + VL
            CALL GAMMA (VG, GA)
            A = (0.5D+00*X) ** VL / GA

            IF (L == 0) THEN
                BJV0 = BJVL * A
            ELSE
                BJV1 = BJVL * A
            END IF

        END DO

    ELSE

        IF (X < 35.0D+00) THEN
            K0 = 11
        ELSE IF (X < 50.0D+00) THEN
            K0 = 10
        ELSE
            K0 = 8
        END IF

        DO J = 0, 1

            VV = 4.0D+00 * (J+V0) * (J+V0)
            PX = 1.0D+00
            RP = 1.0D+00
            DO K = 1, K0
                RP = - 0.78125D-02 * RP * (VV-(4.0D+00*K-3.0D+00)**2) * &
               & (VV-(4.0D+00*K-1.0D+00)**2) / (K*(2.0D+00*K-1.0D+00)*X2)
                PX = PX + RP
            END DO
            QX = 1.0D+00
            RQ = 1.0D+00
            DO K = 1, K0
                RQ = - 0.78125D-02 * RQ * (VV-(4.0D+00*K-1.0D+00)**2) * &
               & (VV-(4.0D+00*K+1.0D+00)**2) / (K*(2.0D+00*K+1.0D+00)*X2)
                QX = QX + RQ
            END DO
            QX = 0.125D+00 * (VV-1.0D+00) * QX / X
            XK = X - (0.5D+00*(J+V0)+0.25D+00) * PI
            A0 = SQRT (RP2/X)
            CK = COS (XK)
            SK = SIN (XK)
            IF (J == 0) THEN
                BJV0 = A0 * (PX*CK-QX*SK)
                BYV0 = A0 * (PX*SK+QX*CK)
            ELSE IF (J == 1) THEN
                BJV1 = A0 * (PX*CK-QX*SK)
                BYV1 = A0 * (PX*SK+QX*CK)
            END IF

        END DO

    END IF

    BJ (0) = BJV0
    BJ (1) = BJV1
    DJ (0) = V0 / X * BJ (0) - BJ (1)
    DJ (1) = - (1.0D+00+V0) / X * BJ (1) + BJ (0)

    IF (2 <= N .AND. N <= INT(0.9D+00*X)) THEN
        F0 = BJV0
        F1 = BJV1
        DO K = 2, N
            F = 2.0D+00 * (K+V0-1.0D+00) / X * F1 - F0
            BJ (K) = F
            F0 = F1
            F1 = F
        END DO
    ELSE IF (2 <= N) THEN
        M = MSTA1 (X, 200)
        IF (M < N) THEN
            N = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF
        F2 = 0.0D+00
        F1 = 1.0D-100
        DO K = M, 0, - 1
            F = 2.0D+00 * (V0+K+1.0D+00) / X * F1 - F2
            IF (K <= N) THEN
                BJ (K) = F
            END IF
            F2 = F1
            F1 = F
        END DO

        IF (ABS(BJV1) < ABS(BJV0)) THEN
            CS = BJV0 / F
        ELSE
            CS = BJV1 / F2
        END IF
        DO K = 0, N
            BJ (K) = CS * BJ (K)
        END DO
    END IF

    DO K = 2, N
        DJ (K) = - (K+V0) / X * BJ (K) + BJ (K-1)
    END DO

    IF (X <= 12.0D+00) THEN

        IF (V0 /= 0.0D+00) THEN

            DO L = 0, 1

                VL = V0 + L
                BJVL = 1.0D+00
                R = 1.0D+00
                DO K = 1, 40
                    R = - 0.25D+00 * R * X2 / (K*(K-VL))
                    BJVL = BJVL + R
                    IF (ABS(R) < ABS(BJVL)*1.0D-15) THEN
                        EXIT
                    END IF
                END DO

                VG = 1.0D+00 - VL
                CALL GAMMA (VG, GB)
                B = (2.0D+00/X) ** VL / GB

                IF (L == 0) THEN
                    BJU0 = BJVL * B
                ELSE
                    BJU1 = BJVL * B
                END IF

            END DO

            PV0 = PI * V0
            PV1 = PI * (1.0D+00+V0)
            BYV0 = (BJV0*COS(PV0)-BJU0) / SIN (PV0)
            BYV1 = (BJV1*COS(PV1)-BJU1) / SIN (PV1)

        ELSE

            EC = LOG (X/2.0D+00) + EL
            CS0 = 0.0D+00
            W0 = 0.0D+00
            R0 = 1.0D+00
            DO K = 1, 30
                W0 = W0 + 1.0D+00 / K
                R0 = - 0.25D+00 * R0 / (K*K) * X2
                CS0 = CS0 + R0 * W0
            END DO
            BYV0 = RP2 * (EC*BJV0-CS0)
            CS1 = 1.0D+00
            W1 = 0.0D+00
            R1 = 1.0D+00
            DO K = 1, 30
                W1 = W1 + 1.0D+00 / K
                R1 = - 0.25D+00 * R1 / (K*(K+1)) * X2
                CS1 = CS1 + R1 * (2.0D+00*W1+1.0D+00/(K+1.0D+00))
            END DO
            BYV1 = RP2 * (EC*BJV1-1.0D+00/X-0.25D+00*X*CS1)

        END IF

    END IF

    BY (0) = BYV0
    BY (1) = BYV1
    DO K = 2, N
        BYVK = 2.0D+00 * (V0+K-1.0D+00) / X * BYV1 - BYV0
        BY (K) = BYVK
        BYV0 = BYV1
        BYV1 = BYVK
    END DO

    DY (0) = V0 / X * BY (0) - BY (1)
    DO K = 1, N
        DY (K) = - (K+V0) / X * BY (K) + BY (K-1)
    END DO

    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CJYVA (V, Z, VM, CBJ, CDJ, CBY, CDY)

!******************************************************************************
!
!! CJYVA: Bessel functions and derivatives, Jv(z) and Yv(z) of complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Jv(z) and Yv(z).
!
!    Input, complex ( kind = ck ), the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) CBJ(0:*), CDJ(0:*), CBY(0:*), CDY(0:*),
!    the values of Jn+v0(z), Jn+v0'(z), Yn+v0(z), Yn+v0'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CA
    COMPLEX (KIND=CK) CA0
    COMPLEX (KIND=CK) CB
    COMPLEX (KIND=CK) CBJ (0:*)
    COMPLEX (KIND=CK) CBY (0:*)
    COMPLEX (KIND=CK) CCK
    COMPLEX (KIND=CK) CDJ (0:*)
    COMPLEX (KIND=CK) CDY (0:*)
    COMPLEX (KIND=CK) CEC
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CFAC0
    COMPLEX (KIND=CK) CFAC1
    COMPLEX (KIND=CK) CG0
    COMPLEX (KIND=CK) CG1
    COMPLEX (KIND=CK) CH0
    COMPLEX (KIND=CK) CH1
    COMPLEX (KIND=CK) CH2
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CJU0
    COMPLEX (KIND=CK) CJU1
    COMPLEX (KIND=CK) CJV0
    COMPLEX (KIND=CK) CJV1
    COMPLEX (KIND=CK) CJVL
    COMPLEX (KIND=CK) CP11
    COMPLEX (KIND=CK) CP12
    COMPLEX (KIND=CK) CP21
    COMPLEX (KIND=CK) CP22
    COMPLEX (KIND=CK) CPZ
    COMPLEX (KIND=CK) CQZ
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CR0
    COMPLEX (KIND=CK) CR1
    COMPLEX (KIND=CK) CRP
    COMPLEX (KIND=CK) CRQ
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CS0
    COMPLEX (KIND=CK) CS1
    COMPLEX (KIND=CK) CSK
    COMPLEX (KIND=CK) CYK
    COMPLEX (KIND=CK) CYL1
    COMPLEX (KIND=CK) CYL2
    COMPLEX (KIND=CK) CYLK
    COMPLEX (KIND=CK) CYV0
    COMPLEX (KIND=CK) CYV1
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    INTEGER J
    INTEGER K
    INTEGER K0
    INTEGER L
    INTEGER LB
    INTEGER LB0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV0
    REAL (KIND=RK) PV1
    REAL (KIND=RK) RP2
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VG
    REAL (KIND=RK) VL
    REAL (KIND=RK) VM
    REAL (KIND=RK) VV
    REAL (KIND=RK) W0
    REAL (KIND=RK) W1
    REAL (KIND=RK) WA
    REAL (KIND=RK) YA0
    REAL (KIND=RK) YA1
    REAL (KIND=RK) YAK
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2
    COMPLEX (KIND=CK) ZK

    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z1 = Z
    Z2 = Z * Z
    N = INT (V)
    V0 = V - N
    PV0 = PI * V0
    PV1 = PI * (1.0D+00+V0)

    IF (A0 < 1.0D-100) THEN

        DO K = 0, N
            CBJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBY (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDY (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO

        IF (V0 == 0.0D+00) THEN
            CBJ (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CDJ (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        ELSE
            CDJ (0) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END IF

        VM = V
        RETURN

    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 <= 12.0D+00) THEN

        DO L = 0, 1
            VL = V0 + L
            CJVL = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                CR = - 0.25D+00 * CR * Z2 / (K*(K+VL))
                CJVL = CJVL + CR
                IF (ABS(CR) < ABS(CJVL)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            VG = 1.0D+00 + VL
            CALL GAMMA (VG, GA)
            CA = (0.5D+00*Z1) ** VL / GA

            IF (L == 0) THEN
                CJV0 = CJVL * CA
            ELSE
                CJV1 = CJVL * CA
            END IF

        END DO

    ELSE

        IF (A0 < 35.0D+00) THEN
            K0 = 11
        ELSE IF (A0 < 50.0D+00) THEN
            K0 = 10
        ELSE
            K0 = 8
        END IF

        DO J = 0, 1
            VV = 4.0D+00 * (J+V0) * (J+V0)
            CPZ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CRP = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, K0
                CRP = - 0.78125D-02 * CRP * (VV-(4.0D+00*K-3.0D+00)**2) * &
               & (VV-(4.0D+00*K-1.0D+00)**2) / (K*(2.0D+00*K-1.0D+00)*Z2)
                CPZ = CPZ + CRP
            END DO
            CQZ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CRQ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, K0
                CRQ = - 0.78125D-02 * CRQ * (VV-(4.0D+00*K-1.0D+00)**2) * &
               & (VV-(4.0D+00*K+1.0D+00)**2) / (K*(2.0D+00*K+1.0D+00)*Z2)
                CQZ = CQZ + CRQ
            END DO
            CQZ = 0.125D+00 * (VV-1.0D+00) * CQZ / Z1
            ZK = Z1 - (0.5D+00*(J+V0)+0.25D+00) * PI
            CA0 = SQRT (RP2/Z1)
            CCK = COS (ZK)
            CSK = SIN (ZK)
            IF (J == 0) THEN
                CJV0 = CA0 * (CPZ*CCK-CQZ*CSK)
                CYV0 = CA0 * (CPZ*CSK+CQZ*CCK)
            ELSE IF (J == 1) THEN
                CJV1 = CA0 * (CPZ*CCK-CQZ*CSK)
                CYV1 = CA0 * (CPZ*CSK+CQZ*CCK)
            END IF
        END DO

    END IF

    IF (A0 <= 12.0D+00) THEN

        IF (V0 .NE. 0.0D+00) THEN

            DO L = 0, 1
                VL = V0 + L
                CJVL = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                DO K = 1, 40
                    CR = - 0.25D+00 * CR * Z2 / (K*(K-VL))
                    CJVL = CJVL + CR
                    IF (ABS(CR) < ABS(CJVL)*1.0D-15) THEN
                        EXIT
                    END IF
                END DO

                VG = 1.0D+00 - VL
                CALL GAMMA (VG, GB)
                CB = (2.0D+00/Z1) ** VL / GB
                IF (L == 0) THEN
                    CJU0 = CJVL * CB
                ELSE
                    CJU1 = CJVL * CB
                END IF
            END DO
            CYV0 = (CJV0*COS(PV0)-CJU0) / SIN (PV0)
            CYV1 = (CJV1*COS(PV1)-CJU1) / SIN (PV1)

        ELSE

            CEC = LOG (Z1/2.0D+00) + 0.5772156649015329D+00
            CS0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            W0 = 0.0D+00
            CR0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 30
                W0 = W0 + 1.0D+00 / K
                CR0 = - 0.25D+00 * CR0 / (K*K) * Z2
                CS0 = CS0 + CR0 * W0
            END DO
            CYV0 = RP2 * (CEC*CJV0-CS0)
            CS1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            W1 = 0.0D+00
            CR1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 30
                W1 = W1 + 1.0D+00 / K
                CR1 = - 0.25D+00 * CR1 / (K*(K+1)) * Z2
                CS1 = CS1 + CR1 * (2.0D+00*W1+1.0D+00/(K+1.0D+00))
            END DO
            CYV1 = RP2 * (CEC*CJV1-1.0D+00/Z1-0.25D+00*Z1*CS1)

        END IF

    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN

        CFAC0 = EXP (PV0*CI)
        CFAC1 = EXP (PV1*CI)

        IF (AIMAG(Z) < 0.0D+00) THEN
            CYV0 = CFAC0 * CYV0 - 2.0D+00 * CI * COS (PV0) * CJV0
            CYV1 = CFAC1 * CYV1 - 2.0D+00 * CI * COS (PV1) * CJV1
            CJV0 = CJV0 / CFAC0
            CJV1 = CJV1 / CFAC1
        ELSE IF (0.0D+00 < AIMAG(Z)) THEN
            CYV0 = CYV0 / CFAC0 + 2.0D+00 * CI * COS (PV0) * CJV0
            CYV1 = CYV1 / CFAC1 + 2.0D+00 * CI * COS (PV1) * CJV1
            CJV0 = CFAC0 * CJV0
            CJV1 = CFAC1 * CJV1
        END IF

    END IF

    CBJ (0) = CJV0
    CBJ (1) = CJV1

    IF (2 <= N .AND. N <= INT(0.25D+00*A0)) THEN

        CF0 = CJV0
        CF1 = CJV1
        DO K = 2, N
            CF = 2.0D+00 * (K+V0-1.0D+00) / Z * CF1 - CF0
            CBJ (K) = CF
            CF0 = CF1
            CF1 = CF
        END DO

    ELSE IF (2 <= N) THEN

        M = MSTA1 (A0, 200)
        IF (M < N) THEN
            N = M
        ELSE
            M = MSTA2 (A0, N, 15)
        END IF
        CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
        DO K = M, 0, - 1
            CF = 2.0D+00 * (V0+K+1.0D+00) / Z * CF1 - CF2
            IF (K <= N) THEN
                CBJ (K) = CF
            END IF
            CF2 = CF1
            CF1 = CF
        END DO
        IF (ABS(CJV1) < ABS(CJV0)) THEN
            CS = CJV0 / CF
        ELSE
            CS = CJV1 / CF2
        END IF

        DO K = 0, N
            CBJ (K) = CS * CBJ (K)
        END DO

    END IF

    CDJ (0) = V0 / Z * CBJ (0) - CBJ (1)
    DO K = 1, N
        CDJ (K) = - (K+V0) / Z * CBJ (K) + CBJ (K-1)
    END DO

    CBY (0) = CYV0
    CBY (1) = CYV1
    YA0 = ABS (CYV0)
    LB = 0
    CG0 = CYV0
    CG1 = CYV1
    DO K = 2, N
        CYK = 2.0D+00 * (V0+K-1.0D+00) / Z * CG1 - CG0
        IF (ABS(CYK) <= 1.0D+290) THEN
            YAK = ABS (CYK)
            YA1 = ABS (CG0)
            IF (YAK < YA0 .AND. YAK < YA1) THEN
                LB = K
            END IF
            CBY (K) = CYK
            CG0 = CG1
            CG1 = CYK
        END IF
    END DO

    IF (4 < LB .AND. AIMAG(Z) /= 0.0D+00) THEN

        DO

            IF (LB == LB0) THEN
                EXIT
            END IF

            CH2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CH1 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            LB0 = LB
            DO K = LB, 1, - 1
                CH0 = 2.0D+00 * (K+V0) / Z * CH1 - CH2
                CH2 = CH1
                CH1 = CH0
            END DO
            CP12 = CH0
            CP22 = CH2
            CH2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CH1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = LB, 1, - 1
                CH0 = 2.0D+00 * (K+V0) / Z * CH1 - CH2
                CH2 = CH1
                CH1 = CH0
            END DO
            CP11 = CH0
            CP21 = CH2

            IF (LB == N) THEN
                CBJ (LB+1) = 2.0D+00 * (LB+V0) / Z * CBJ (LB) - CBJ (LB-1)
            END IF

            IF (ABS(CBJ(1)) < ABS(CBJ(0))) THEN
                CBY (LB+1) = (CBJ(LB+1)*CYV0-2.0D+00*CP11/(PI*Z)) / CBJ (0)
                CBY (LB) = (CBJ(LB)*CYV0+2.0D+00*CP12/(PI*Z)) / CBJ (0)
            ELSE
                CBY (LB+1) = (CBJ(LB+1)*CYV1-2.0D+00*CP21/(PI*Z)) / CBJ (1)
                CBY (LB) = (CBJ(LB)*CYV1+2.0D+00*CP22/(PI*Z)) / CBJ (1)
            END IF

            CYL2 = CBY (LB+1)
            CYL1 = CBY (LB)
            DO K = LB - 1, 0, - 1
                CYLK = 2.0D+00 * (K+V0+1.0D+00) / Z * CYL1 - CYL2
                CBY (K) = CYLK
                CYL2 = CYL1
                CYL1 = CYLK
            END DO

            CYL1 = CBY (LB)
            CYL2 = CBY (LB+1)
            DO K = LB + 1, N - 1
                CYLK = 2.0D+00 * (K+V0) / Z * CYL2 - CYL1
                CBY (K+1) = CYLK
                CYL1 = CYL2
                CYL2 = CYLK
            END DO

            DO K = 2, N
                WA = ABS (CBY(K))
                IF (WA < ABS(CBY(K-1))) THEN
                    LB = K
                END IF
            END DO

        END DO

    END IF

    CDY (0) = V0 / Z * CBY (0) - CBY (1)
    DO K = 1, N
        CDY (K) = CBY (K-1) - (K+V0) / Z * CBY (K)
    END DO
    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CJYVB (V, Z, VM, CBJ, CDJ, CBY, CDY)

!******************************************************************************
!
!! CJYVB: Bessel functions and derivatives, Jv(z) and Yv(z) of complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Jv(z) and Yv(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) CBJ(0:*), CDJ(0:*), CBY(0:*), CDY(0:*),
!    the values of Jn+v0(z), Jn+v0'(z), Yn+v0(z), Yn+v0'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CA
    COMPLEX (KIND=CK) CA0
    COMPLEX (KIND=CK) CB
    COMPLEX (KIND=CK) CBJ (0:*)
    COMPLEX (KIND=CK) CBY (0:*)
    COMPLEX (KIND=CK) CCK
    COMPLEX (KIND=CK) CDJ (0:*)
    COMPLEX (KIND=CK) CDY (0:*)
    COMPLEX (KIND=CK) CEC
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CFAC0
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CJU0
    COMPLEX (KIND=CK) CJV0
    COMPLEX (KIND=CK) CJVN
    COMPLEX (KIND=CK) CPZ
    COMPLEX (KIND=CK) CQZ
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CR0
    COMPLEX (KIND=CK) CRP
    COMPLEX (KIND=CK) CRQ
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CS0
    COMPLEX (KIND=CK) CSK
    COMPLEX (KIND=CK) CYV0
    COMPLEX (KIND=CK) CYY
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    INTEGER K
    INTEGER K0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV0
    REAL (KIND=RK) RP2
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VG
    REAL (KIND=RK) VM
    REAL (KIND=RK) VV
    REAL (KIND=RK) W0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2
    COMPLEX (KIND=CK) ZK

    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z1 = Z
    Z2 = Z * Z
    N = INT (V)
    V0 = V - N
    PV0 = PI * V0

    IF (A0 < 1.0D-100) THEN

        DO K = 0, N
            CBJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDJ (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBY (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDY (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO

        IF (V0 == 0.0D+00) THEN
            CBJ (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CDJ (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        ELSE
            CDJ (0) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END IF

        VM = V
        RETURN

    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 <= 12.0D+00) THEN

        CJV0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 40
            CR = - 0.25D+00 * CR * Z2 / (K*(K+V0))
            CJV0 = CJV0 + CR
            IF (ABS(CR) < ABS(CJV0)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        VG = 1.0D+00 + V0
        CALL GAMMA (VG, GA)
        CA = (0.5D+00*Z1) ** V0 / GA
        CJV0 = CJV0 * CA

    ELSE

        IF (A0 < 35.0D+00) THEN
            K0 = 11
        ELSE IF (A0 < 50.0D+00) THEN
            K0 = 10
        ELSE
            K0 = 8
        END IF

        VV = 4.0D+00 * V0 * V0
        CPZ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CRP = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CRP = - 0.78125D-02 * CRP * (VV-(4.0D+00*K-3.0D+00)**2) * &
           & (VV-(4.0D+00*K-1.0D+00)**2) / (K*(2.0D+00*K-1.0D+00)*Z2)
            CPZ = CPZ + CRP
        END DO
        CQZ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CRQ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CRQ = - 0.78125D-02 * CRQ * (VV-(4.0D+00*K-1.0D+00)**2) * &
           & (VV-(4.0D+00*K+1.0D+00)**2) / (K*(2.0D+00*K+1.0D+00)*Z2)
            CQZ = CQZ + CRQ
        END DO
        CQZ = 0.125D+00 * (VV-1.0D+00) * CQZ / Z1
        ZK = Z1 - (0.5D+00*V0+0.25D+00) * PI
        CA0 = SQRT (RP2/Z1)
        CCK = COS (ZK)
        CSK = SIN (ZK)
        CJV0 = CA0 * (CPZ*CCK-CQZ*CSK)
        CYV0 = CA0 * (CPZ*CSK+CQZ*CCK)

    END IF

    IF (A0 <= 12.0D+00) THEN

        IF (V0 .NE. 0.0D+00) THEN

            CJVN = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                CR = - 0.25D+00 * CR * Z2 / (K*(K-V0))
                CJVN = CJVN + CR
                IF (ABS(CR) < ABS(CJVN)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            VG = 1.0D+00 - V0
            CALL GAMMA (VG, GB)
            CB = (2.0D+00/Z1) ** V0 / GB
            CJU0 = CJVN * CB
            CYV0 = (CJV0*COS(PV0)-CJU0) / SIN (PV0)

        ELSE

            CEC = LOG (Z1/2.0D+00) + 0.5772156649015329D+00
            CS0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            W0 = 0.0D+00
            CR0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 30
                W0 = W0 + 1.0D+00 / K
                CR0 = - 0.25D+00 * CR0 / (K*K) * Z2
                CS0 = CS0 + CR0 * W0
            END DO
            CYV0 = RP2 * (CEC*CJV0-CS0)

        END IF

    END IF

    IF (N == 0) THEN
        N = 1
    END IF

    M = MSTA1 (A0, 200)
    IF (M < N) THEN
        N = M
    ELSE
        M = MSTA2 (A0, N, 15)
    END IF

    CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
    DO K = M, 0, - 1
        CF = 2.0D+00 * (V0+K+1.0D+00) / Z1 * CF1 - CF2
        IF (K <= N) THEN
            CBJ (K) = CF
        END IF
        CF2 = CF1
        CF1 = CF
    END DO

    CS = CJV0 / CF
    DO K = 0, N
        CBJ (K) = CS * CBJ (K)
    END DO

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN

        CFAC0 = EXP (PV0*CI)
        IF (AIMAG(Z) < 0.0D+00) THEN
            CYV0 = CFAC0 * CYV0 - 2.0D+00 * CI * COS (PV0) * CJV0
        ELSE IF (0.0D+00 < AIMAG(Z)) THEN
            CYV0 = CYV0 / CFAC0 + 2.0D+00 * CI * COS (PV0) * CJV0
        END IF

        DO K = 0, N
            IF (AIMAG(Z) < 0.0D+00) THEN
                CBJ (K) = EXP (-PI*(K+V0)*CI) * CBJ (K)
            ELSE IF (0.0D+00 < AIMAG(Z)) THEN
                CBJ (K) = EXP (PI*(K+V0)*CI) * CBJ (K)
            END IF
        END DO

        Z1 = Z1

    END IF

    CBY (0) = CYV0
    DO K = 1, N
        CYY = (CBJ(K)*CBY(K-1)-2.0D+00/(PI*Z)) / CBJ (K-1)
        CBY (K) = CYY
    END DO

    CDJ (0) = V0 / Z * CBJ (0) - CBJ (1)
    DO K = 1, N
        CDJ (K) = - (K+V0) / Z * CBJ (K) + CBJ (K-1)
    END DO

    CDY (0) = V0 / Z * CBY (0) - CBY (1)
    DO K = 1, N
        CDY (K) = CBY (K-1) - (K+V0) / Z * CBY (K)
    END DO

    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CJK (KM, A)

!******************************************************************************
!
!! CJK: asymptotic expansion coefficients for Bessel functions of large order.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KM, the maximum value of K.
!
!    Output, real ( kind = rk ) A(L), the value of Cj(k) where j and k are
!    related to L by L = j+1+[k*(k+1)]/2; j,k = 0,1,...,Km.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (*)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) G
    REAL (KIND=RK) G0
    INTEGER J
    INTEGER K
    INTEGER KM
    INTEGER L1
    INTEGER L2
    INTEGER L3
    INTEGER L4

    A (1) = 1.0D+00
    F0 = 1.0D+00
    G0 = 1.0D+00
    DO K = 0, KM - 1
        L1 = (K+1) * (K+2) / 2 + 1
        L2 = (K+1) * (K+2) / 2 + K + 2
        F = (0.5D+00*K+0.125D+00/(K+1)) * F0
        G = - (1.5D+00*K+0.625D+00/(3.0D+00*(K+1.0D+00))) * G0
        A (L1) = F
        A (L2) = G
        F0 = F
        G0 = G
    END DO

    DO K = 1, KM - 1
        DO J = 1, K
            L3 = K * (K+1) / 2 + J + 1
            L4 = (K+1) * (K+2) / 2 + J + 1
            A (L4) = (J+0.5D+00*K+0.125D+00/(2.0D+00*J+K+1.0D+00)) * A (L3) - &
           & (J+0.5D+00*K-1.0D+00+0.625D+00/(2.0D+00*J+K+1.0D+00)) * A (L3-1)
        END DO
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE JYZO (N, NT, RJ0, RJ1, RY0, RY1)

!******************************************************************************
!
!! JYZO computes the zeros of Bessel functions Jn(x), Yn(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the Bessel functions.
!
!    Input, integer NT, the number of zeros.
!
!    Output, real ( kind = rk ) RJ0(NT), RJ1(NT), RY0(NT), RY1(NT), the zeros
!    of Jn(x), Jn'(x), Yn(x), Yn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    REAL (KIND=RK) BJN
    REAL (KIND=RK) BYN
    REAL (KIND=RK) DJN
    REAL (KIND=RK) DYN
    REAL (KIND=RK) FJN
    REAL (KIND=RK) FYN
    INTEGER L
    INTEGER N
    REAL (KIND=RK) N_R8
    REAL (KIND=RK) RJ0 (NT)
    REAL (KIND=RK) RJ1 (NT)
    REAL (KIND=RK) RY0 (NT)
    REAL (KIND=RK) RY1 (NT)
    REAL (KIND=RK) X
    REAL (KIND=RK) X0

    N_R8 = REAL (N, KIND=RK)

    IF (N <= 20) THEN
        X = 2.82141D+00 + 1.15859D+00 * N_R8
    ELSE
        X = N + 1.85576D+00 * N_R8 ** 0.33333D+00 + 1.03315D+00 / N_R8 ** 0.33333D+00
    END IF

    L = 0

    DO

        X0 = X
        CALL JYNDD (N, X, BJN, DJN, FJN, BYN, DYN, FYN)
        X = X - BJN / DJN

        IF (1.0D-09 < ABS(X-X0)) THEN
            CYCLE
        END IF

        L = L + 1
        RJ0 (L) = X
        X = X + 3.1416D+00 + (0.0972D+00+0.0679D+00*N_R8-0.000354D+00*N_R8**2) / L

        IF (NT <= L) THEN
            EXIT
        END IF

    END DO

    IF (N <= 20) THEN
        X = 0.961587D+00 + 1.07703D+00 * N_R8
    ELSE
        X = N_R8 + 0.80861D+00 * N_R8 ** 0.33333D+00 + 0.07249D+00 / N_R8 ** 0.33333D+00
    END IF

    IF (N == 0) THEN
        X = 3.8317D+00
    END IF

    L = 0

    DO

        X0 = X
        CALL JYNDD (N, X, BJN, DJN, FJN, BYN, DYN, FYN)
        X = X - DJN / FJN
        IF (1.0D-09 < ABS(X-X0)) THEN
            CYCLE
        END IF
        L = L + 1
        RJ1 (L) = X
        X = X + 3.1416D+00 + (0.4955D+00+0.0915D+00*N_R8-0.000435D+00*N_R8**2) / L

        IF (NT <= L) THEN
            EXIT
        END IF

    END DO

    IF (N <= 20) THEN
        X = 1.19477D+00 + 1.08933D+00 * N_R8
    ELSE
        X = N_R8 + 0.93158D+00 * N_R8 ** 0.33333D+00 + 0.26035D+00 / N_R8 ** 0.33333D+00
    END IF

    L = 0

    DO

        X0 = X
        CALL JYNDD (N, X, BJN, DJN, FJN, BYN, DYN, FYN)
        X = X - BYN / DYN

        IF (1.0D-09 < ABS(X-X0)) THEN
            CYCLE
        END IF

        L = L + 1
        RY0 (L) = X
        X = X + 3.1416D+00 + (0.312D+00+0.0852D+00*N_R8-0.000403D+00*N_R8**2) / L

        IF (NT <= L) THEN
            EXIT
        END IF

    END DO

    IF (N <= 20) THEN
        X = 2.67257D+00 + 1.16099D+00 * N_R8
    ELSE
        X = N_R8 + 1.8211D+00 * N_R8 ** 0.33333D+00 + 0.94001D+00 / N_R8 ** 0.33333D+00
    END IF

    L = 0

    DO

        X0 = X
        CALL JYNDD (N, X, BJN, DJN, FJN, BYN, DYN, FYN)
        X = X - DYN / FYN

        IF (1.0D-09 < ABS(X-X0)) THEN
            CYCLE
        END IF

        L = L + 1
        RY1 (L) = X
        X = X + 3.1416D+00 + (0.197D+00+0.0643D+00*N_R8-0.000286D+00*N_R8**2) / L

        IF (NT <= L) THEN
            EXIT
        END IF

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE JDZO (NT, N, M, P, ZO)

!******************************************************************************
!
!! JDZO computes the zeros of Bessel functions Jn(x) and Jn'(x).
!
!  Discussion:
!
!    This procedure computes the zeros of Bessel functions Jn(x) and
!    Jn'(x), and arrange them in the order of their magnitudes.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer NT, the number of zeros.
!
!    Output, integer N(*), the  order of Jn(x) or Jn'(x) associated
!    with the L-th zero.
!
!    Output, integer M(*), the serial number of the zeros of Jn(x)
!    or Jn'(x) associated with the L-th zero ( L is the serial number of all the
!    zeros of Jn(x) and Jn'(x) ).
!
!    Output, character ( len = 4 ) P(L), 'TM' or 'TE', a code for designating
!    the zeros of Jn(x)  or Jn'(x).  In the waveguide applications, the zeros
!    of Jn(x) correspond to TM modes and those of Jn'(x) correspond to TE modes.
!
!    Output, real ( kind = rk ) ZO(*), the zeros of Jn(x) and Jn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BJ (101)
    REAL (KIND=RK) DJ (101)
    REAL (KIND=RK) FJ (101)
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER L
    INTEGER L0
    INTEGER L1
    INTEGER L2
    INTEGER M (1400)
    INTEGER M1 (70)
    INTEGER MM
    INTEGER N (1400)
    INTEGER N1 (70)
    INTEGER NM
    INTEGER NT
    CHARACTER (LEN=4) P (1400)
    CHARACTER (LEN=4) P1 (70)
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X1
    REAL (KIND=RK) X2
    REAL (KIND=RK) XM
    REAL (KIND=RK) ZO (1400)
    REAL (KIND=RK) ZOC (70)

    IF (NT < 600) THEN
        XM = - 1.0D+00 + 2.248485D+00 * REAL (NT, KIND=RK) ** 0.5D+00 - 0.0159382D+00 * NT + &
       & 3.208775D-04 * REAL (NT, KIND=RK) ** 1.5D+00
        NM = INT (14.5D+00+0.05875D+00*NT)
        MM = INT (0.02D+00*NT) + 6
    ELSE
        XM = 5.0D+00 + 1.445389D+00 * (REAL(NT, KIND=RK)) ** 0.5D+00 + 0.01889876D+00 * NT - &
       & 2.147763D-04 * (REAL(NT, KIND=RK)) ** 1.5D+00
        NM = INT (27.8D+00+0.0327D+00*NT)
        MM = INT (0.01088D+00*NT) + 10
    END IF

    L0 = 0

    DO I = 1, NM

        X1 = 0.407658D+00 + 0.4795504D+00 * (REAL(I-1, KIND=RK)) ** 0.5D+00 + 0.983618D+00 * &
       & (I-1)
        X2 = 1.99535D+00 + 0.8333883 * (REAL(I-1, KIND=RK)) ** 0.5D+00 + 0.984584D+00 * (I-1)
        L1 = 0

        DO J = 1, MM

            IF (I == 1 .AND. J == 1) THEN

                L1 = L1 + 1
                N1 (L1) = I - 1
                M1 (L1) = J
                IF (I == 1) THEN
                    M1 (L1) = J - 1
                END IF
                P1 (L1) = 'TE'
                ZOC (L1) = X

                IF (I <= 15) THEN
                    X1 = X + 3.057D+00 + 0.0122D+00 * (I-1) + (1.555D+00+0.41575D+00*(I-1)) / &
                   & (J+1) ** 2
                ELSE
                    X1 = X + 2.918D+00 + 0.01924D+00 * (I-1) + (6.26D+00+0.13205D+00*(I-1)) / &
                   & (J+1) ** 2
                END IF

            ELSE

                X = X1

                DO

                    CALL BJNDD (I, X, BJ, DJ, FJ)
                    X0 = X
                    X = X - DJ (I) / FJ (I)

                    IF (XM < X1) THEN
                        EXIT
                    END IF

                    IF (ABS(X-X0) <= 1.0D-10) THEN
                        L1 = L1 + 1
                        N1 (L1) = I - 1
                        M1 (L1) = J
                        IF (I == 1) THEN
                            M1 (L1) = J - 1
                        END IF
                        P1 (L1) = 'TE'
                        ZOC (L1) = X

                        IF (I <= 15) THEN
                            X1 = X + 3.057D+00 + 0.0122D+00 * (I-1) + &
                           & (1.555D+00+0.41575D+00*(I-1)) / (J+1) ** 2
                        ELSE
                            X1 = X + 2.918D+00 + 0.01924D+00 * (I-1) + &
                           & (6.26D+00+0.13205D+00*(I-1)) / (J+1) ** 2
                        END IF
                        EXIT
                    END IF

                END DO

            END IF

            X = X2

            DO

                CALL BJNDD (I, X, BJ, DJ, FJ)
                X0 = X
                X = X - BJ (I) / DJ (I)

                IF (XM < X) THEN
                    EXIT
                END IF

                IF (ABS(X-X0) <= 1.0D-10) THEN
                    EXIT
                END IF

            END DO

            IF (X <= XM) THEN

                L1 = L1 + 1
                N1 (L1) = I - 1
                M1 (L1) = J
                P1 (L1) = 'TM'
                ZOC (L1) = X
                IF (I <= 15) THEN
                    X2 = X + 3.11D+00 + 0.0138D+00 * (I-1) + (0.04832D+00+0.2804D+00*(I-1)) / &
                   & (J+1) ** 2
                ELSE
                    X2 = X + 3.001D+00 + 0.0105D+00 * (I-1) + (11.52D+00+0.48525D+00*(I-1)) / &
                   & (J+3) ** 2
                END IF

            END IF

        END DO

        L = L0 + L1
        L2 = L

        DO

            IF (L0 == 0) THEN
                DO K = 1, L
                    ZO (K) = ZOC (K)
                    N (K) = N1 (K)
                    M (K) = M1 (K)
                    P (K) = P1 (K)
                END DO
                L1 = 0
            ELSE IF (L0 /= 0) THEN
                IF (ZOC(L1) .LE. ZO(L0)) THEN
                    ZO (L0+L1) = ZO (L0)
                    N (L0+L1) = N (L0)
                    M (L0+L1) = M (L0)
                    P (L0+L1) = P (L0)
                    L0 = L0 - 1
                ELSE
                    ZO (L0+L1) = ZOC (L1)
                    N (L0+L1) = N1 (L1)
                    M (L0+L1) = M1 (L1)
                    P (L0+L1) = P1 (L1)
                    L1 = L1 - 1
                END IF
            END IF

            IF (L1 == 0) THEN
                EXIT
            END IF

        END DO

        L0 = L2

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CYZO (NT, KF, KC, ZO, ZV)

!******************************************************************************
!
!! CYZO computes zeros of complex Bessel functions Y0(z) and Y1(z) and Y1'(z).
!
!  Parameters:
!
!    Ths procedure computes the complex zeros of Y0(z), Y1(z) and Y1'(z),
!    and their associated values at the zeros using the modified Newton's
!    iteration method.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer NT, the number of zeros.
!
!    Input, integer KF, the function choice.
!    0 for Y0(z) and Y1(z0);
!    1 for Y1(z) and Y0(z1);
!    2 for Y1'(z) and Y1(z1').
!
!    Input, integer KC, complex/real choice.
!    0, for complex roots;
!    1, for real roots.
!
!    Output, real ( kind = rk ) ZO(NT), ZV(NT), the zeros of Y0(z) or Y1(z)
!    or Y1'(z), and the value of Y0'(z) or Y1'(z) or Y1(z) at the L-th zero.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    REAL (KIND=RK) H
    INTEGER I
    INTEGER IT
    INTEGER J
    INTEGER KC
    INTEGER KF
    INTEGER NR
    REAL (KIND=RK) W
    REAL (KIND=RK) W0
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZERO
    COMPLEX (KIND=CK) ZF
    COMPLEX (KIND=CK) ZFD
    COMPLEX (KIND=CK) ZGD
    COMPLEX (KIND=CK) ZO (NT)
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZQ
    COMPLEX (KIND=CK) ZV (NT)
    COMPLEX (KIND=CK) ZW

    IF (KC == 0) THEN
        X = - 2.4D+00
        Y = 0.54D+00
        H = 3.14D+00
    ELSE IF (KC == 1) THEN
        X = 0.89D+00
        Y = 0.0D+00
        H = - 3.14D+00
    END IF

    IF (KF == 1) THEN
        X = - 0.503D+00
    ELSE IF (KF == 2) THEN
        X = 0.577D+00
    END IF

    ZERO = CMPLX (X, Y, KIND=CK)

    DO NR = 1, NT

        IF (NR == 1) THEN
            Z = ZERO
        ELSE
            Z = ZO (NR-1) - H
        END IF

        IT = 0

        DO

            IT = IT + 1
            CALL CY01 (KF, Z, ZF, ZD)

            ZP = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO I = 1, NR - 1
                ZP = ZP * (Z-ZO(I))
            END DO

            ZFD = ZF / ZP

            ZQ = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            DO I = 1, NR - 1
                ZW = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        ZW = ZW * (Z-ZO(J))
                    END IF
                END DO
                ZQ = ZQ + ZW
            END DO

            ZGD = (ZD-ZQ*ZFD) / ZP
            Z = Z - ZFD / ZGD
            W0 = W
            W = ABS (Z)

            IF (50 < IT .OR. ABS((W-W0)/W) <= 1.0D-12) THEN
                EXIT
            END IF

        END DO

        ZO (NR) = Z

    END DO

    DO I = 1, NT
        Z = ZO (I)
        IF (KF == 0 .OR. KF == 2) THEN
            CALL CY01 (1, Z, ZF, ZD)
            ZV (I) = ZF
        ELSE IF (KF == 1) THEN
            CALL CY01 (0, Z, ZF, ZD)
            ZV (I) = ZF
        END IF
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LAMN (N, X, NM, BL, DL)

!******************************************************************************
!
!! LAMN computes lambda functions and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    14 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) BL(0:N), DL(0:N), the
!    value of the lambda function and its derivative of orders 0 through N.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BG
    REAL (KIND=RK) BK
    REAL (KIND=RK) BL (0:N)
    REAL (KIND=RK) BS
    REAL (KIND=RK) DL (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER I
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) UK
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    NM = N

    IF (ABS(X) < 1.0D-100) THEN
        DO K = 0, N
            BL (K) = 0.0D+00
            DL (K) = 0.0D+00
        END DO
        BL (0) = 1.0D+00
        DL (1) = 0.5D+00
        RETURN
    END IF

    IF (X <= 12.0D+00) THEN

        X2 = X * X

        DO K = 0, N
            BK = 1.0D+00
            R = 1.0D+00
            DO I = 1, 50
                R = - 0.25D+00 * R * X2 / (I*(I+K))
                BK = BK + R
                IF (ABS(R) < ABS(BK)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            BL (K) = BK
            IF (1 <= K) THEN
                DL (K-1) = - 0.5D+00 * X / K * BK
            END IF

        END DO

        UK = 1.0D+00
        R = 1.0D+00
        DO I = 1, 50
            R = - 0.25D+00 * R * X2 / (I*(I+N+1.0D+00))
            UK = UK + R
            IF (ABS(R) < ABS(UK)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        DL (N) = - 0.5D+00 * X / (N+1.0D+00) * UK
        RETURN

    END IF

    IF (N == 0) THEN
        NM = 1
    END IF

    M = MSTA1 (X, 200)

    IF (M < NM) THEN
        NM = M
    ELSE
        M = MSTA2 (X, NM, 15)
    END IF

    BS = 0.0D+00
    F0 = 0.0D+00
    F1 = 1.0D-100
    DO K = M, 0, - 1
        F = 2.0D+00 * (K+1.0D+00) * F1 / X - F0
        IF (K <= NM) THEN
            BL (K) = F
        END IF
        IF (K == 2*INT(K/2)) THEN
            BS = BS + 2.0D+00 * F
        END IF
        F0 = F1
        F1 = F
    END DO

    BG = BS - F
    DO K = 0, NM
        BL (K) = BL (K) / BG
    END DO

    R0 = 1.0D+00
    DO K = 1, NM
        R0 = 2.0D+00 * R0 * K / X
        BL (K) = R0 * BL (K)
    END DO

    DL (0) = - 0.5D+00 * X * BL (1)
    DO K = 1, NM
        DL (K) = 2.0D+00 * K / X * (BL(K-1)-BL(K))
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LAMV (V, X, VM, VL, DL)

!******************************************************************************
!
!! LAMV computes lambda functions and derivatives of arbitrary order.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) VL(0:*), DL(0:*), the Lambda function and
!    derivative, of orders N+V0.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) V

    REAL (KIND=RK) A0
    REAL (KIND=RK) BJV0
    REAL (KIND=RK) BJV1
    REAL (KIND=RK) BK
    REAL (KIND=RK) CK
    REAL (KIND=RK) CS
    REAL (KIND=RK) DL (0:INT(V))
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) FAC
    REAL (KIND=RK) GA
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER K0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PX
    REAL (KIND=RK) QX
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) RC
    REAL (KIND=RK) RP
    REAL (KIND=RK) RP2
    REAL (KIND=RK) RQ
    REAL (KIND=RK) SK
    REAL (KIND=RK) UK
    REAL (KIND=RK) V0
    REAL (KIND=RK) VK
    REAL (KIND=RK) VL (0:INT(V))
    REAL (KIND=RK) VM
    REAL (KIND=RK) VV
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XK

    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    X = ABS (X)
    X2 = X * X
    N = INT (V)
    V0 = V - N
    VM = V

    IF (X <= 12.0D+00) THEN

        DO K = 0, N

            VK = V0 + K
            BK = 1.0D+00
            R = 1.0D+00

            DO I = 1, 50
                R = - 0.25D+00 * R * X2 / (I*(I+VK))
                BK = BK + R
                IF (ABS(R) < ABS(BK)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            VL (K) = BK
            UK = 1.0D+00
            R = 1.0D+00
            DO I = 1, 50
                R = - 0.25D+00 * R * X2 / (I*(I+VK+1.0D+00))
                UK = UK + R
                IF (ABS(R) < ABS(UK)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            DL (K) = - 0.5D+00 * X / (VK+1.0D+00) * UK

        END DO

        RETURN

    END IF

    IF (X < 35.0D+00) THEN
        K0 = 11
    ELSE IF (X < 50.0D+00) THEN
        K0 = 10
    ELSE
        K0 = 8
    END IF

    DO J = 0, 1
        VV = 4.0D+00 * (J+V0) * (J+V0)
        PX = 1.0D+00
        RP = 1.0D+00
        DO K = 1, K0
            RP = - 0.78125D-02 * RP * (VV-(4.0D+00*K-3.0D+00)**2) * (VV-(4.0D+00*K-1.0D+00)**2) &
           & / (K*(2.0*K-1.0D+00)*X2)
            PX = PX + RP
        END DO
        QX = 1.0D+00
        RQ = 1.0D+00
        DO K = 1, K0
            RQ = - 0.78125D-02 * RQ * (VV-(4.0D+00*K-1.0D+00)**2) * (VV-(4.0D+00*K+1.0D+00)**2) &
           & / (K*(2.0D+00*K+1.0D+00)*X2)
            QX = QX + RQ
        END DO
        QX = 0.125D+00 * (VV-1.0D+00) * QX / X
        XK = X - (0.5D+00*(J+V0)+0.25D+00) * PI
        A0 = SQRT (RP2/X)
        CK = COS (XK)
        SK = SIN (XK)
        IF (J == 0) THEN
            BJV0 = A0 * (PX*CK-QX*SK)
        ELSE
            BJV1 = A0 * (PX*CK-QX*SK)
        END IF
    END DO

    IF (V0 == 0.0D+00) THEN
        GA = 1.0D+00
    ELSE
        CALL GAM0 (V0, GA)
        GA = V0 * GA
    END IF

    FAC = (2.0D+00/X) ** V0 * GA
    VL (0) = BJV0
    DL (0) = - BJV1 + V0 / X * BJV0
    VL (1) = BJV1
    DL (1) = BJV0 - (1.0D+00+V0) / X * BJV1
    R0 = 2.0D+00 * (1.0D+00+V0) / X

    IF (N <= 1) THEN
        VL (0) = FAC * VL (0)
        DL (0) = FAC * DL (0) - V0 / X * VL (0)
        VL (1) = FAC * R0 * VL (1)
        DL (1) = FAC * R0 * DL (1) - (1.0D+00+V0) / X * VL (1)
        RETURN
    END IF

    IF (2 <= N .AND. N <= INT(0.9D+00*X)) THEN

        F0 = BJV0
        F1 = BJV1
        DO K = 2, N
            F = 2.0D+00 * (K+V0-1.0D+00) / X * F1 - F0
            F0 = F1
            F1 = F
            VL (K) = F
        END DO

    ELSE IF (2 <= N) THEN

        M = MSTA1 (X, 200)
        IF (M < N) THEN
            N = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF
        F2 = 0.0D+00
        F1 = 1.0D-100
        DO K = M, 0, - 1
            F = 2.0D+00 * (V0+K+1.0D+00) / X * F1 - F2
            IF (K <= N) THEN
                VL (K) = F
            END IF
            F2 = F1
            F1 = F
        END DO

        IF (ABS(BJV0) <= ABS(BJV1)) THEN
            CS = BJV1 / F2
        ELSE
            CS = BJV0 / F
        END IF

        DO K = 0, N
            VL (K) = CS * VL (K)
        END DO

    END IF

    VL (0) = FAC * VL (0)
    DO J = 1, N
        RC = FAC * R0
        VL (J) = RC * VL (J)
        DL (J-1) = - 0.5D+00 * X / (J+V0) * VL (J)
        R0 = 2.0D+00 * (J+V0+1) / X * R0
    END DO
    DL (N) = 2.0D+00 * (V0+N) * (VL(N-1)-VL(N)) / X
    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CJYLV (V, Z, CBJV, CDJV, CBYV, CDYV)

!******************************************************************************
!
!! CJYLV: Bessel functions Jv(z), Yv(z) of complex argument and large order v.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Jv(z) and Yv(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CBJV, CDJV, CBYV, CDYV, the values of Jv(z),
!    Jv'(z), Yv(z), Yv'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (91)
    COMPLEX (KIND=CK) CBJV
    COMPLEX (KIND=CK) CBYV
    COMPLEX (KIND=CK) CDJV
    COMPLEX (KIND=CK) CDYV
    COMPLEX (KIND=CK) CETA
    COMPLEX (KIND=CK) CF (12)
    COMPLEX (KIND=CK) CFJ
    COMPLEX (KIND=CK) CFY
    COMPLEX (KIND=CK) CSJ
    COMPLEX (KIND=CK) CSY
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CT2
    COMPLEX (KIND=CK) CWS
    INTEGER I
    INTEGER K
    INTEGER KM
    INTEGER L
    INTEGER L0
    INTEGER LF
    REAL (KIND=RK) PI
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VR
    COMPLEX (KIND=CK) Z

    KM = 12
    CALL CJK (KM, A)
    PI = 3.141592653589793D+00

    DO L = 1, 0, - 1

        V0 = V - L
        CWS = SQRT (1.0D+00-(Z/V0)*(Z/V0))
        CETA = CWS + LOG (Z/V0/(1.0D+00+CWS))
        CT = 1.0D+00 / CWS
        CT2 = CT * CT

        DO K = 1, KM
            L0 = K * (K+1) / 2 + 1
            LF = L0 + K
            CF (K) = A (LF)
            DO I = LF - 1, L0, - 1
                CF (K) = CF (K) * CT2 + A (I)
            END DO
            CF (K) = CF (K) * CT ** K
        END DO

        VR = 1.0D+00 / V0
        CSJ = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, KM
            CSJ = CSJ + CF (K) * VR ** K
        END DO
        CBJV = SQRT (CT/(2.0D+00*PI*V0)) * EXP (V0*CETA) * CSJ
        IF (L == 1) THEN
            CFJ = CBJV
        END IF
        CSY = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, KM
            CSY = CSY + (-1.0D+00) ** K * CF (K) * VR ** K
        END DO
        CBYV = - SQRT (2.0D+00*CT/(PI*V0)) * EXP (-V0*CETA) * CSY
        IF (L == 1) THEN
            CFY = CBYV
        END IF

    END DO

    CDJV = - V / Z * CBJV + CFJ
    CDYV = - V / Z * CBYV + CFY

    RETURN
END

!******************************************************************************

SUBROUTINE AJYIK (X, VJ1, VJ2, VY1, VY2, VI1, VI2, VK1, VK2)

!******************************************************************************
!
!! ajyik() computes Bessel functions Jv(x), Yv(x), Iv(x), Kv(x).
!
!  Discussion:
!
!    Compute Bessel functions Jv(x) and Yv(x), and modified Bessel functions
!    Iv(x) and Kv(x), and their derivatives with v = 1/3, 2/3.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.  X should not be zero.
!
!    Output, real ( kind = rk ) VJ1, VJ2, VY1, VY2, VI1, VI2, VK1, VK2,
!    the values of J1/3(x), J2/3(x), Y1/3(x), Y2/3(x), I1/3(x), I2/3(x),
!    K1/3(x), K2/3(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) B0
    REAL (KIND=RK) C0
    REAL (KIND=RK) CK
    REAL (KIND=RK) GN
    REAL (KIND=RK) GN1
    REAL (KIND=RK) GN2
    REAL (KIND=RK) GP1
    REAL (KIND=RK) GP2
    INTEGER K
    INTEGER K0
    INTEGER L
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV1
    REAL (KIND=RK) PV2
    REAL (KIND=RK) PX
    REAL (KIND=RK) QX
    REAL (KIND=RK) R
    REAL (KIND=RK) RP
    REAL (KIND=RK) RP2
    REAL (KIND=RK) RQ
    REAL (KIND=RK) SK
    REAL (KIND=RK) SUM
    REAL (KIND=RK) UJ1
    REAL (KIND=RK) UJ2
    REAL (KIND=RK) UU0
    REAL (KIND=RK) VI1
    REAL (KIND=RK) VI2
    REAL (KIND=RK) VIL
    REAL (KIND=RK) VJ1
    REAL (KIND=RK) VJ2
    REAL (KIND=RK) VJL
    REAL (KIND=RK) VK1
    REAL (KIND=RK) VK2
    REAL (KIND=RK) VL
    REAL (KIND=RK) VSL
    REAL (KIND=RK) VV
    REAL (KIND=RK) VV0
    REAL (KIND=RK) VY1
    REAL (KIND=RK) VY2
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XK

    IF (X == 0.0D+00) THEN
        VJ1 = 0.0D+00
        VJ2 = 0.0D+00
        VY1 = - 1.0D+300
        VY2 = 1.0D+300
        VI1 = 0.0D+00
        VI2 = 0.0D+00
        VK1 = - 1.0D+300
        VK2 = - 1.0D+300
        RETURN
    END IF

    PI = 3.141592653589793D+00
    RP2 = 0.63661977236758D+00
    GP1 = 0.892979511569249D+00
    GP2 = 0.902745292950934D+00
    GN1 = 1.3541179394264D+00
    GN2 = 2.678938534707747D+00
    VV0 = 0.444444444444444D+00
    UU0 = 1.1547005383793D+00
    X2 = X * X

    IF (X < 35.0D+00) THEN
        K0 = 12
    ELSE IF (X < 50.0D+00) THEN
        K0 = 10
    ELSE
        K0 = 8
    END IF

    IF (X <= 12.0D+00) THEN

        DO L = 1, 2
            VL = L / 3.0D+00
            VJL = 1.0D+00
            R = 1.0D+00
            DO K = 1, 40
                R = - 0.25D+00 * R * X2 / (K*(K+VL))
                VJL = VJL + R
                IF (ABS(R) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            A0 = (0.5D+00*X) ** VL
            IF (L == 1) THEN
                VJ1 = A0 / GP1 * VJL
            ELSE
                VJ2 = A0 / GP2 * VJL
            END IF

        END DO

    ELSE

        DO L = 1, 2

            VV = VV0 * L * L
            PX = 1.0D+00
            RP = 1.0D+00

            DO K = 1, K0
                RP = - 0.78125D-02 * RP * (VV-(4.0D+00*K-3.0D+00)**2) * &
               & (VV-(4.0D+00*K-1.0D+00)**2) / (K*(2.0D+00*K-1.0D+00)*X2)
                PX = PX + RP
            END DO

            QX = 1.0D+00
            RQ = 1.0D+00
            DO K = 1, K0
                RQ = - 0.78125D-02 * RQ * (VV-(4.0D+00*K-1.0D+00)**2) * &
               & (VV-(4.0D+00*K+1.0D+00)**2) / (K*(2.0D+00*K+1.0D+00)*X2)
                QX = QX + RQ
            END DO

            QX = 0.125D+00 * (VV-1.0D+00) * QX / X
            XK = X - (0.5D+00*L/3.0D+00+0.25D+00) * PI
            A0 = SQRT (RP2/X)
            CK = COS (XK)
            SK = SIN (XK)
            IF (L == 1) THEN
                VJ1 = A0 * (PX*CK-QX*SK)
                VY1 = A0 * (PX*SK+QX*CK)
            ELSE
                VJ2 = A0 * (PX*CK-QX*SK)
                VY2 = A0 * (PX*SK+QX*CK)
            END IF

        END DO

    END IF

    IF (X <= 12.0D+00) THEN

        DO L = 1, 2

            VL = L / 3.0D+00
            VJL = 1.0D+00
            R = 1.0D+00
            DO K = 1, 40
                R = - 0.25D+00 * R * X2 / (K*(K-VL))
                VJL = VJL + R
                IF (ABS(R) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            B0 = (2.0D+00/X) ** VL
            IF (L == 1) THEN
                UJ1 = B0 * VJL / GN1
            ELSE
                UJ2 = B0 * VJL / GN2
            END IF

        END DO

        PV1 = PI / 3.0D+00
        PV2 = PI / 1.5D+00
        VY1 = UU0 * (VJ1*COS(PV1)-UJ1)
        VY2 = UU0 * (VJ2*COS(PV2)-UJ2)

    END IF

    IF (X <= 18.0D+00) THEN

        DO L = 1, 2
            VL = L / 3.0D+00
            VIL = 1.0D+00
            R = 1.0D+00
            DO K = 1, 40
                R = 0.25D+00 * R * X2 / (K*(K+VL))
                VIL = VIL + R
                IF (ABS(R) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            A0 = (0.5D+00*X) ** VL

            IF (L == 1) THEN
                VI1 = A0 / GP1 * VIL
            ELSE
                VI2 = A0 / GP2 * VIL
            END IF

        END DO

    ELSE

        C0 = EXP (X) / SQRT (2.0D+00*PI*X)

        DO L = 1, 2
            VV = VV0 * L * L
            VSL = 1.0D+00
            R = 1.0D+00
            DO K = 1, K0
                R = - 0.125D+00 * R * (VV-(2.0D+00*K-1.0D+00)**2) / (K*X)
                VSL = VSL + R
            END DO
            IF (L == 1) THEN
                VI1 = C0 * VSL
            ELSE
                VI2 = C0 * VSL
            END IF
        END DO

    END IF

    IF (X <= 9.0D+00) THEN

        DO L = 1, 2
            VL = L / 3.0D+00
            IF (L == 1) THEN
                GN = GN1
            ELSE
                GN = GN2
            END IF
            A0 = (2.0D+00/X) ** VL / GN
            SUM = 1.0D+00
            R = 1.0D+00
            DO K = 1, 60
                R = 0.25D+00 * R * X2 / (K*(K-VL))
                SUM = SUM + R
                IF (ABS(R) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            IF (L == 1) THEN
                VK1 = 0.5D+00 * UU0 * PI * (SUM*A0-VI1)
            ELSE
                VK2 = 0.5D+00 * UU0 * PI * (SUM*A0-VI2)
            END IF

        END DO

    ELSE

        C0 = EXP (-X) * SQRT (0.5D+00*PI/X)

        DO L = 1, 2
            VV = VV0 * L * L
            SUM = 1.0D+00
            R = 1.0D+00
            DO K = 1, K0
                R = 0.125D+00 * R * (VV-(2.0D+00*K-1.0D+00)**2) / (K*X)
                SUM = SUM + R
            END DO
            IF (L == 1) THEN
                VK1 = C0 * SUM
            ELSE
                VK2 = C0 * SUM
            END IF
        END DO

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE BJNDD (N, X, BJ, DJ, FJ)

!******************************************************************************
!
!! bjndd() computes Bessel functions Jn(x) and first and second derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    11 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BJ(N+1), DJ(N+1), FJ(N+1), the values of
!    Jn(x), Jn'(x) and Jn''(x) in the last entries.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BJ (N+1)
    REAL (KIND=RK) BS
    REAL (KIND=RK) DJ (N+1)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) FJ (N+1)
    INTEGER K
    INTEGER M
    INTEGER MT
    INTEGER NT
    REAL (KIND=RK) X

    DO NT = 1, 900
        MT = INT (0.5D+00*LOG10(6.28D+00*NT)-NT*LOG10(1.36D+00*ABS(X)/NT))
        IF (20 < MT) THEN
            EXIT
        END IF
    END DO

    M = NT
    BS = 0.0D+00
    F0 = 0.0D+00
    F1 = 1.0D-35
    DO K = M, 0, - 1
        F = 2.0D+00 * (K+1.0D+00) * F1 / X - F0
        IF (K <= N) THEN
            BJ (K+1) = F
        END IF
        IF (K == 2*INT(K/2)) THEN
            BS = BS + 2.0D+00 * F
        END IF
        F0 = F1
        F1 = F
    END DO

    DO K = 0, N
        BJ (K+1) = BJ (K+1) / (BS-F)
    END DO

    DJ (1) = - BJ (2)
    FJ (1) = - 1.0D+00 * BJ (1) - DJ (1) / X
    DO K = 1, N
        DJ (K+1) = BJ (K) - K * BJ (K+1) / X
        FJ (K+1) = (K*K/(X*X)-1.0D+00) * BJ (K+1) - DJ (K+1) / X
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CY01 (KF, Z, ZF, ZD)

!******************************************************************************
!
!! CY01 computes complex Bessel functions Y0(z) and Y1(z) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KF, the function choice.
!    0 for ZF = Y0(z) and ZD = Y0'(z);
!    1 for ZF = Y1(z) and ZD = Y1'(z);
!    2 for ZF = Y1'(z) and ZD = Y1''(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) ZF, ZD, the values of the requested function
!    and derivative.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (12) :: A = (/ - 0.703125D-01, 0.112152099609375D+00, - &
   & 0.5725014209747314D+00, 0.6074042001273483D+01, - 0.1100171402692467D+03, &
   & 0.3038090510922384D+04, - 0.1188384262567832D+06, 0.6252951493434797D+07, - &
   & 0.4259392165047669D+09, 0.3646840080706556D+11, - 0.3833534661393944D+13, &
   & 0.4854014686852901D+15 /)
    REAL (KIND=RK) A0
    REAL (KIND=RK), SAVE, DIMENSION (12) :: A1 = (/ 0.1171875D+00, - 0.144195556640625D+00, &
   & 0.6765925884246826D+00, - 0.6883914268109947D+01, 0.1215978918765359D+03, - &
   & 0.3302272294480852D+04, 0.1276412726461746D+06, - 0.6656367718817688D+07, &
   & 0.4502786003050393D+09, - 0.3833857520742790D+11, 0.4011838599133198D+13, - &
   & 0.5060568503314727D+15 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B = (/ 0.732421875D-01, - 0.2271080017089844D+00, &
   & 0.1727727502584457D+01, - 0.2438052969955606D+02, 0.5513358961220206D+03, - &
   & 0.1825775547429318D+05, 0.8328593040162893D+06, - 0.5006958953198893D+08, &
   & 0.3836255180230433D+10, - 0.3649010818849833D+12, 0.4218971570284096D+14, - &
   & 0.5827244631566907D+16 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B1 = (/ - 0.1025390625D+00, 0.2775764465332031D+00, &
   & - 0.1993531733751297D+01, 0.2724882731126854D+02, - 0.6038440767050702D+03, &
   & 0.1971837591223663D+05, - 0.8902978767070678D+06, 0.5310411010968522D+08, - &
   & 0.4043620325107754D+10, 0.3827011346598605D+12, - 0.4406481417852278D+14, &
   & 0.6065091351222699D+16 /)
    COMPLEX (KIND=CK) CBJ0
    COMPLEX (KIND=CK) CBJ1
    COMPLEX (KIND=CK) CBY0
    COMPLEX (KIND=CK) CBY1
    COMPLEX (KIND=CK) CDY0
    COMPLEX (KIND=CK) CDY1
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CP
    COMPLEX (KIND=CK) CP0
    COMPLEX (KIND=CK) CP1
    COMPLEX (KIND=CK) CQ0
    COMPLEX (KIND=CK) CQ1
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CT1
    COMPLEX (KIND=CK) CT2
    COMPLEX (KIND=CK) CU
    REAL (KIND=RK) EL
    INTEGER K
    INTEGER K0
    INTEGER KF
    REAL (KIND=RK) PI
    REAL (KIND=RK) RP2
    REAL (KIND=RK) W0
    REAL (KIND=RK) W1
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZF

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    RP2 = 2.0D+00 / PI
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z2 = Z * Z
    Z1 = Z

    IF (A0 == 0.0D+00) THEN

        CBJ0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CBJ1 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CBY0 = CMPLX (-1.0D+30, 0.0D+00, KIND=CK)
        CBY1 = CMPLX (-1.0D+30, 0.0D+00, KIND=CK)
        CDY0 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CDY1 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)

    ELSE

        IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
            Z1 = - Z
        END IF

        IF (A0 <= 12.0D+00) THEN

            CBJ0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                CR = - 0.25D+00 * CR * Z2 / (K*K)
                CBJ0 = CBJ0 + CR
                IF (ABS(CR) < ABS(CBJ0)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBJ1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                CR = - 0.25D+00 * CR * Z2 / (K*(K+1.0D+00))
                CBJ1 = CBJ1 + CR
                IF (ABS(CR) < ABS(CBJ1)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBJ1 = 0.5D+00 * Z1 * CBJ1
            W0 = 0.0D+00
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                W0 = W0 + 1.0D+00 / K
                CR = - 0.25D+00 * CR / (K*K) * Z2
                CP = CR * W0
                CS = CS + CP
                IF (ABS(CP) < ABS(CS)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBY0 = RP2 * (LOG(Z1/2.0D+00)+EL) * CBJ0 - RP2 * CS
            W1 = 0.0D+00
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 40
                W1 = W1 + 1.0D+00 / K
                CR = - 0.25D+00 * CR / (K*(K+1)) * Z2
                CP = CR * (2.0D+00*W1+1.0D+00/(K+1.0D+00))
                CS = CS + CP
                IF (ABS(CP) < ABS(CS)*1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBY1 = RP2 * ((LOG(Z1/2.0D+00)+EL)*CBJ1-1.0D+00/Z1-0.25D+00*Z1*CS)

        ELSE

            IF (A0 < 35.0D+00) THEN
                K0 = 12
            ELSE IF (A0 < 50.0D+00) THEN
                K0 = 10
            ELSE
                K0 = 8
            END IF

            CT1 = Z1 - 0.25D+00 * PI
            CP0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, K0
                CP0 = CP0 + A (K) * Z1 ** (-2*K)
            END DO
            CQ0 = - 0.125D+00 / Z1
            DO K = 1, K0
                CQ0 = CQ0 + B (K) * Z1 ** (-2*K-1)
            END DO
            CU = SQRT (RP2/Z1)
            CBJ0 = CU * (CP0*COS(CT1)-CQ0*SIN(CT1))
            CBY0 = CU * (CP0*SIN(CT1)+CQ0*COS(CT1))
            CT2 = Z1 - 0.75D+00 * PI
            CP1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, K0
                CP1 = CP1 + A1 (K) * Z1 ** (-2*K)
            END DO
            CQ1 = 0.375D+00 / Z1
            DO K = 1, K0
                CQ1 = CQ1 + B1 (K) * Z1 ** (-2*K-1)
            END DO
            CBJ1 = CU * (CP1*COS(CT2)-CQ1*SIN(CT2))
            CBY1 = CU * (CP1*SIN(CT2)+CQ1*COS(CT2))

        END IF

        IF (REAL(Z, KIND=RK) < 0.0D+00) THEN

            IF (AIMAG(Z) < 0.0D+00) THEN
                CBY0 = CBY0 - 2.0D+00 * CI * CBJ0
            ELSE
                CBY0 = CBY0 + 2.0D+00 * CI * CBJ0
            END IF

            IF (AIMAG(Z) < 0.0D+00) THEN
                CBY1 = - (CBY1-2.0D+00*CI*CBJ1)
            ELSE
                CBY1 = - (CBY1+2.0D+00*CI*CBJ1)
            END IF
            CBJ1 = - CBJ1

        END IF

        CDY0 = - CBY1
        CDY1 = CBY0 - 1.0D+00 / Z * CBY1

    END IF

    IF (KF == 0) THEN
        ZF = CBY0
        ZD = CDY0
    ELSE IF (KF == 1) THEN
        ZF = CBY1
        ZD = CDY1
    ELSE IF (KF == 2) THEN
        ZF = CDY1
        ZD = - CDY1 / Z - (1.0D+00-1.0D+00/(Z*Z)) * CBY1
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE GAM0 (X, GA)

!******************************************************************************
!
!! GAM0 computes the Gamma function for the LAMV function.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    09 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) GA, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), DIMENSION (25) :: G = (/ 1.0D+00, 0.5772156649015329D+00, - &
   & 0.6558780715202538D+00, - 0.420026350340952D-01, 0.1665386113822915D+00, - &
   & 0.421977345555443D-01, - 0.96219715278770D-02, 0.72189432466630D-02, - &
   & 0.11651675918591D-02, - 0.2152416741149D-03, 0.1280502823882D-03, - 0.201348547807D-04, - &
   & 0.12504934821D-05, 0.11330272320D-05, - 0.2056338417D-06, 0.61160950D-08, 0.50020075D-08, &
   & - 0.11812746D-08, 0.1043427D-09, 0.77823D-11, - 0.36968D-11, 0.51D-12, - 0.206D-13, - &
   & 0.54D-14, 0.14D-14 /)
    REAL (KIND=RK) GA
    REAL (KIND=RK) GR
    INTEGER K
    REAL (KIND=RK) X

    GR = G (25)
    DO K = 24, 1, - 1
        GR = GR * X + G (K)
    END DO

    GA = 1.0D+00 / (GR*X)

    RETURN
END

!******************************************************************************

FUNCTION MSTA1 (X, MP)

!******************************************************************************
!
!! MSTA1 determines a backward recurrence starting point for Jn(x).
!
!  Discussion:
!
!    This procedure determines the starting point for backward
!    recurrence such that the magnitude of
!    Jn(x) at that point is about 10^(-MP).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    08 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, integer MP, the negative logarithm of the
!    desired magnitude.
!
!    Output, integer MSTA1, the starting point.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER IT
    INTEGER MP
    INTEGER MSTA1
    INTEGER N0
    INTEGER N1
    INTEGER NN
    REAL (KIND=RK) X

    A0 = ABS (X)
    N0 = INT (1.1D+00*A0) + 1
    F0 = ENVJ (N0, A0) - MP
    N1 = N0 + 5
    F1 = ENVJ (N1, A0) - MP
    DO IT = 1, 20
        NN = N1 - INT (REAL(N1-N0, KIND=RK)/(1.0D+00-F0/F1))
        F = ENVJ (NN, A0) - MP
        IF (ABS(NN-N1) < 1) THEN
            EXIT
        END IF
        N0 = N1
        F0 = F1
        N1 = NN
        F1 = F
    END DO

    MSTA1 = NN

    RETURN
END

!******************************************************************************

FUNCTION MSTA2 (X, N, MP)

!******************************************************************************
!
!! MSTA2 determines a backward recurrence starting point for Jn(x).
!
!  Discussion:
!
!    This procedure determines the starting point for a backward
!    recurrence such that all Jn(x) has MP significant digits.
!
!    Jianming Jin supplied a modification to this code on 12 January 2016.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    14 January 2016
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of Jn(x).
!
!    Input, integer N, the order of Jn(x).
!
!    Input, integer MP, the number of significant digits.
!
!    Output, integer MSTA2, the starting point.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) EJN
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) HMP
    INTEGER IT
    INTEGER MP
    INTEGER MSTA2
    INTEGER N
    INTEGER N0
    INTEGER N1
    INTEGER NN
    REAL (KIND=RK) OBJ
    REAL (KIND=RK) X

    A0 = ABS (X)
    HMP = 0.5D+00 * MP
    EJN = ENVJ (N, A0)

    IF (EJN <= HMP) THEN
        OBJ = MP
!
!  Original code:
!
!   n0 = int ( 1.1D+00 * a0 )
!
!  Updated code:
!
        N0 = INT (1.1D+00*A0) + 1
    ELSE
        OBJ = HMP + EJN
        N0 = N
    END IF

    F0 = ENVJ (N0, A0) - OBJ
    N1 = N0 + 5
    F1 = ENVJ (N1, A0) - OBJ

    DO IT = 1, 20
        NN = N1 - INT (REAL(N1-N0, KIND=RK)/(1.0D+00-F0/F1))
        F = ENVJ (NN, A0) - OBJ
        IF (ABS(NN-N1) < 1) THEN
            EXIT
        END IF
        N0 = N1
        F0 = F1
        N1 = NN
        F1 = F
    END DO

    MSTA2 = NN + 10

    RETURN
END

!******************************************************************************

FUNCTION ENVJ (N, X)

!******************************************************************************
!
!! ENVJ is a utility function used by MSTA1 and MSTA2.
!
!  Discussion:
!
!    ENVJ estimates -log(Jn(x)) from the estimate
!    Jn(x) approx 1/sqrt(2*pi*n) * ( e*x/(2*n))^n
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    14 January 2016
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!    Modifications suggested by Vincent Lafage, 11 January 2016.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the Bessel function.
!
!    Input, real ( kind = rk ) X, the absolute value of the argument.
!
!    Output, real ( kind = rk ) ENVJ, the value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) ENVJ
    REAL (KIND=RK) LOGTEN
    INTEGER N
    REAL (KIND=RK) N_R8
    REAL (KIND=RK) X
!
!  Original code
!
    IF (.TRUE.) THEN

        ENVJ = 0.5D+00 * LOG10 (6.28D+00*N) - N * LOG10 (1.36D+00*X/N)
!
!  Modification suggested by Vincent Lafage.
!
    ELSE

        N_R8 = REAL (N, KIND=RK)
        LOGTEN = LOG (10.0D+00)
        ENVJ = GAMMA_LOG (N_R8+1.0D+00) / LOGTEN - N_R8 * LOG10 (X)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE JYNDD (N, X, BJN, DJN, FJN, BYN, DYN, FYN)

!******************************************************************************
!
!! JYNDD: Bessel functions Jn(x) and Yn(x), first and second derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BJN, DJN, FJN, BYN, DYN, FYN, the values of
!    Jn(x), Jn'(x), Jn"(x), Yn(x), Yn'(x), Yn"(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BJ (102)
    REAL (KIND=RK) BJN
    REAL (KIND=RK) BYN
    REAL (KIND=RK) BS
    REAL (KIND=RK) BY (102)
    REAL (KIND=RK) DJN
    REAL (KIND=RK) DYN
    REAL (KIND=RK) E0
    REAL (KIND=RK) EC
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) FJN
    REAL (KIND=RK) FYN
    INTEGER K
    INTEGER M
    INTEGER MT
    INTEGER N
    INTEGER NT
    REAL (KIND=RK) S1
    REAL (KIND=RK) SU
    REAL (KIND=RK) X

    DO NT = 1, 900
        MT = INT (0.5D+00*LOG10(6.28D+00*NT)-NT*LOG10(1.36D+00*ABS(X)/NT))
        IF (20 < MT) THEN
            EXIT
        END IF
    END DO

    M = NT
    BS = 0.0D+00
    F0 = 0.0D+00
    F1 = 1.0D-35
    SU = 0.0D+00
    DO K = M, 0, - 1
        F = 2.0D+00 * (K+1.0D+00) * F1 / X - F0
        IF (K <= N+1) THEN
            BJ (K+1) = F
        END IF
        IF (K == 2*INT(K/2)) THEN
            BS = BS + 2.0D+00 * F
            IF (K /= 0) THEN
                SU = SU + (-1.0D+00) ** (K/2) * F / K
            END IF
        END IF
        F0 = F1
        F1 = F
    END DO

    DO K = 0, N + 1
        BJ (K+1) = BJ (K+1) / (BS-F)
    END DO

    BJN = BJ (N+1)
    EC = 0.5772156649015329D+00
    E0 = 0.3183098861837907D+00
    S1 = 2.0D+00 * E0 * (LOG(X/2.0D+00)+EC) * BJ (1)
    F0 = S1 - 8.0D+00 * E0 * SU / (BS-F)
    F1 = (BJ(2)*F0-2.0D+00*E0/X) / BJ (1)

    BY (1) = F0
    BY (2) = F1
    DO K = 2, N + 1
        F = 2.0D+00 * (K-1.0D+00) * F1 / X - F0
        BY (K+1) = F
        F0 = F1
        F1 = F
    END DO

    BYN = BY (N+1)
    DJN = - BJ (N+2) + N * BJ (N+1) / X
    DYN = - BY (N+2) + N * BY (N+1) / X
    FJN = (N*N/(X*X)-1.0D+00) * BJN - DJN / X
    FYN = (N*N/(X*X)-1.0D+00) * BYN - DYN / X

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   06: MODIFIED BESSEL FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE IK01A (X, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)

!******************************************************************************
!
!! IK01A compute Bessel function I0(x), I1(x), K0(x), and K1(x).
!
!  Discussion:
!
!    This procedure computes modified Bessel functions I0(x), I1(x),
!    K0(x) and K1(x), and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    16 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1, the
!    values of I0(x), I0'(x), I1(x), I1'(x), K0(x), K0'(x), K1(x), K1'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (12) :: A = (/ 0.125D+00, 7.03125D-02, 7.32421875D-02, &
   & 1.1215209960938D-01, 2.2710800170898D-01, 5.7250142097473D-01, 1.7277275025845D+00, &
   & 6.0740420012735D+00, 2.4380529699556D+01, 1.1001714026925D+02, 5.5133589612202D+02, &
   & 3.0380905109224D+03 /)
    REAL (KIND=RK), SAVE, DIMENSION (8) :: A1 = (/ 0.125D+00, 0.2109375D+00, 1.0986328125D+00, &
   & 1.1775970458984D+01, 2.1461706161499D+02, 5.9511522710323D+03, 2.3347645606175D+05, &
   & 1.2312234987631D+07 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B = (/ - 0.375D+00, - 1.171875D-01, - &
   & 1.025390625D-01, - 1.4419555664063D-01, - 2.7757644653320D-01, - 6.7659258842468D-01, - &
   & 1.9935317337513D+00, - 6.8839142681099D+00, - 2.7248827311269D+01, - 1.2159789187654D+02, &
   & - 6.0384407670507D+02, - 3.3022722944809D+03 /)
    REAL (KIND=RK) BI0
    REAL (KIND=RK) BI1
    REAL (KIND=RK) BK0
    REAL (KIND=RK) BK1
    REAL (KIND=RK) CA
    REAL (KIND=RK) CB
    REAL (KIND=RK) CT
    REAL (KIND=RK) DI0
    REAL (KIND=RK) DI1
    REAL (KIND=RK) DK0
    REAL (KIND=RK) DK1
    REAL (KIND=RK) EL
    INTEGER K
    INTEGER K0
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) W0
    REAL (KIND=RK) WW
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XR
    REAL (KIND=RK) XR2

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    X2 = X * X

    IF (X == 0.0D+00) THEN

        BI0 = 1.0D+00
        BI1 = 0.0D+00
        BK0 = 1.0D+300
        BK1 = 1.0D+300
        DI0 = 0.0D+00
        DI1 = 0.5D+00
        DK0 = - 1.0D+300
        DK1 = - 1.0D+300
        RETURN

    ELSE IF (X <= 18.0D+00) THEN

        BI0 = 1.0D+00
        R = 1.0D+00
        DO K = 1, 50
            R = 0.25D+00 * R * X2 / (K*K)
            BI0 = BI0 + R
            IF (ABS(R/BI0) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        BI1 = 1.0D+00
        R = 1.0D+00
        DO K = 1, 50
            R = 0.25D+00 * R * X2 / (K*(K+1))
            BI1 = BI1 + R
            IF (ABS(R/BI1) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        BI1 = 0.5D+00 * X * BI1

    ELSE

        IF (X < 35.0D+00) THEN
            K0 = 12
        ELSE IF (X < 50.0D+00) THEN
            K0 = 9
        ELSE
            K0 = 7
        END IF

        CA = EXP (X) / SQRT (2.0D+00*PI*X)
        BI0 = 1.0D+00
        XR = 1.0D+00 / X
        DO K = 1, K0
            BI0 = BI0 + A (K) * XR ** K
        END DO
        BI0 = CA * BI0
        BI1 = 1.0D+00
        DO K = 1, K0
            BI1 = BI1 + B (K) * XR ** K
        END DO
        BI1 = CA * BI1

    END IF

    IF (X <= 9.0D+00) THEN

        CT = - (LOG(X/2.0D+00)+EL)
        BK0 = 0.0D+00
        W0 = 0.0D+00
        R = 1.0D+00
        DO K = 1, 50
            W0 = W0 + 1.0D+00 / K
            R = 0.25D+00 * R / (K*K) * X2
            BK0 = BK0 + R * (W0+CT)
            IF (ABS((BK0-WW)/BK0) < 1.0D-15) THEN
                EXIT
            END IF
            WW = BK0
        END DO

        BK0 = BK0 + CT

    ELSE

        CB = 0.5D+00 / X
        XR2 = 1.0D+00 / X2
        BK0 = 1.0D+00
        DO K = 1, 8
            BK0 = BK0 + A1 (K) * XR2 ** K
        END DO
        BK0 = CB * BK0 / BI0

    END IF

    BK1 = (1.0D+00/X-BI1*BK0) / BI0
    DI0 = BI1
    DI1 = BI0 - BI1 / X
    DK0 = - BK1
    DK1 = - BK0 - BK1 / X

    RETURN
END

!******************************************************************************

SUBROUTINE IK01B (X, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)

!******************************************************************************
!
!! IK01B: Bessel functions I0(x), I1(x), K0(x), and K1(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    17 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1, the
!    values of I0(x), I0'(x), I1(x), I1'(x), K0(x), K0'(x), K1(x), K1'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BI0
    REAL (KIND=RK) BI1
    REAL (KIND=RK) BK0
    REAL (KIND=RK) BK1
    REAL (KIND=RK) DI0
    REAL (KIND=RK) DI1
    REAL (KIND=RK) DK0
    REAL (KIND=RK) DK1
    REAL (KIND=RK) T
    REAL (KIND=RK) T2
    REAL (KIND=RK) X

    IF (X == 0.0D+00) THEN

        BI0 = 1.0D+00
        BI1 = 0.0D+00
        BK0 = 1.0D+300
        BK1 = 1.0D+300
        DI0 = 0.0D+00
        DI1 = 0.5D+00
        DK0 = - 1.0D+300
        DK1 = - 1.0D+300
        RETURN

    ELSE IF (X <= 3.75D+00) THEN

        T = X / 3.75D+00
        T2 = T * T

        BI0 = (((((0.0045813D+00*T2+0.0360768D+00)*T2+0.2659732D+00)*T2+1.2067492D+00)*T2+&
       & 3.0899424D+00)*T2+3.5156229D+00) * T2 + 1.0D+00

        BI1 = X * ((((((0.00032411D+00*T2+0.00301532D+00)*T2+0.02658733D+00)*T2+0.15084934D+00)&
       & *T2+0.51498869D+00)*T2+0.87890594D+00)*T2+0.5D+00)

    ELSE

        T = 3.75D+00 / X

        BI0 = ((((((((0.00392377D+00*T-0.01647633D+00)*T+0.02635537D+00)*T-0.02057706D+00)*T+&
       & 0.916281D-02)*T-0.157565D-02)*T+0.225319D-02)*T+0.01328592D+00)*T+0.39894228D+00) * &
       & EXP (X) / SQRT (X)

        BI1 = ((((((((-0.420059D-02*T+0.01787654D+00)*T-0.02895312D+00)*T+0.02282967D+00)*T-&
       & 0.01031555D+00)*T+0.163801D-02)*T-0.00362018D+00)*T-0.03988024D+00)*T+0.39894228D+00) &
       & * EXP (X) / SQRT (X)

    END IF

    IF (X <= 2.0D+00) THEN

        T = X / 2.0D+00
        T2 = T * T

        BK0 = (((((0.0000074D+00*T2+0.0001075D+00)*T2+0.00262698D+00)*T2+0.0348859D+00)*T2+&
       & 0.23069756D+00)*T2+0.4227842D+00) * T2 - 0.57721566D+00 - BI0 * LOG (T)

        BK1 = ((((((-0.00004686D+00*T2-0.00110404D+00)*T2-0.01919402D+00)*T2-0.18156897D+00)*T2-&
       & 0.67278579D+00)*T2+0.15443144D+00)*T2+1.0D+00) / X + BI1 * LOG (T)

    ELSE

        T = 2.0D+00 / X
        T2 = T * T

        BK0 = ((((((0.00053208D+00*T-0.0025154D+00)*T+0.00587872D+00)*T-0.01062446D+00)*T+&
       & 0.02189568D+00)*T-0.07832358D+00)*T+1.25331414D+00) * EXP (-X) / SQRT (X)

        BK1 = ((((((-0.00068245D+00*T+0.00325614D+00)*T-0.00780353D+00)*T+0.01504268D+00)*T-&
       & 0.0365562D+00)*T+0.23498619D+00)*T+1.25331414D+00) * EXP (-X) / SQRT (X)

    END IF

    DI0 = BI1
    DI1 = BI0 - BI1 / X
    DK0 = - BK1
    DK1 = - BK0 - BK1 / X

    RETURN
END

!******************************************************************************

SUBROUTINE IKNA (N, X, NM, BI, DI, BK, DK)

!******************************************************************************
!
!! IKNA compute Bessel function In(x) and Kn(x), and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    16 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of In(x) and Kn(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) BI(0:N), DI(0:N), BK(0:N), DK(0:N),
!    the values of In(x), In'(x), Kn(x), Kn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) BI (0:N)
    REAL (KIND=RK) BI0
    REAL (KIND=RK) BI1
    REAL (KIND=RK) BK (0:N)
    REAL (KIND=RK) BK0
    REAL (KIND=RK) BK1
    REAL (KIND=RK) DI (0:N)
    REAL (KIND=RK) DI0
    REAL (KIND=RK) DI1
    REAL (KIND=RK) DK (0:N)
    REAL (KIND=RK) DK0
    REAL (KIND=RK) DK1
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) G
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    REAL (KIND=RK) H
    REAL (KIND=RK) H0
    REAL (KIND=RK) H1
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) S0
    REAL (KIND=RK) X

    NM = N

    IF (X <= 1.0D-100) THEN
        DO K = 0, N
            BI (K) = 0.0D+00
            DI (K) = 0.0D+00
            BK (K) = 1.0D+300
            DK (K) = - 1.0D+300
        END DO
        BI (0) = 1.0D+00
        DI (1) = 0.5D+00
        RETURN
    END IF

    CALL IK01A (X, BI0, DI0, BI1, DI1, BK0, DK0, BK1, DK1)
    BI (0) = BI0
    BI (1) = BI1
    BK (0) = BK0
    BK (1) = BK1
    DI (0) = DI0
    DI (1) = DI1
    DK (0) = DK0
    DK (1) = DK1

    IF (N <= 1) THEN
        RETURN
    END IF

    IF (40.0D+00 < X .AND. N < INT(0.25D+00*X)) THEN

        H0 = BI0
        H1 = BI1
        DO K = 2, N
            H = - 2.0D+00 * (K-1.0D+00) / X * H1 + H0
            BI (K) = H
            H0 = H1
            H1 = H
        END DO

    ELSE

        M = MSTA1 (X, 200)

        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF

        F0 = 0.0D+00
        F1 = 1.0D-100
        DO K = M, 0, - 1
            F = 2.0D+00 * (K+1.0D+00) * F1 / X + F0
            IF (K <= NM) THEN
                BI (K) = F
            END IF
            F0 = F1
            F1 = F
        END DO
        S0 = BI0 / F
        DO K = 0, NM
            BI (K) = S0 * BI (K)
        END DO
    END IF

    G0 = BK0
    G1 = BK1
    DO K = 2, NM
        G = 2.0D+00 * (K-1.0D+00) / X * G1 + G0
        BK (K) = G
        G0 = G1
        G1 = G
    END DO

    DO K = 2, NM
        DI (K) = BI (K-1) - K / X * BI (K)
        DK (K) = - BK (K-1) - K / X * BK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE IKNB (N, X, NM, BI, DI, BK, DK)

!******************************************************************************
!
!! IKNB compute Bessel function In(x) and Kn(x).
!
!  Discussion:
!
!    Compute modified Bessel functions In(x) and Kn(x),
!    and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    17 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of In(x) and Kn(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) BI(0:N), DI(0:N), BK(0:N), DK(0:N),
!    the values of In(x), In'(x), Kn(x), Kn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    REAL (KIND=RK) BI (0:N)
    REAL (KIND=RK) BK (0:N)
    REAL (KIND=RK) BKL
    REAL (KIND=RK) BS
    REAL (KIND=RK) DI (0:N)
    REAL (KIND=RK) DK (0:N)
    REAL (KIND=RK) EL
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) G
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    INTEGER K
    INTEGER K0
    INTEGER L
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) S0
    REAL (KIND=RK) SK0
    REAL (KIND=RK) VT
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329d0
    NM = N

    IF (X <= 1.0D-100) THEN
        DO K = 0, N
            BI (K) = 0.0D+00
            DI (K) = 0.0D+00
            BK (K) = 1.0D+300
            DK (K) = - 1.0D+300
        END DO
        BI (0) = 1.0D+00
        DI (1) = 0.5D+00
        RETURN
    END IF

    IF (N == 0) THEN
        NM = 1
    END IF

    M = MSTA1 (X, 200)
    IF (M < NM) THEN
        NM = M
    ELSE
        M = MSTA2 (X, NM, 15)
    END IF

    BS = 0.0D+00
    SK0 = 0.0D+00
    F0 = 0.0D+00
    F1 = 1.0D-100
    DO K = M, 0, - 1
        F = 2.0D+00 * (K+1.0D+00) / X * F1 + F0
        IF (K <= NM) THEN
            BI (K) = F
        END IF
        IF (K /= 0 .AND. K == 2*INT(K/2)) THEN
            SK0 = SK0 + 4.0D+00 * F / K
        END IF
        BS = BS + 2.0D+00 * F
        F0 = F1
        F1 = F
    END DO

    S0 = EXP (X) / (BS-F)
    DO K = 0, NM
        BI (K) = S0 * BI (K)
    END DO

    IF (X <= 8.0D+00) THEN
        BK (0) = - (LOG(0.5D+00*X)+EL) * BI (0) + S0 * SK0
        BK (1) = (1.0D+00/X-BI(1)*BK(0)) / BI (0)
    ELSE
        A0 = SQRT (PI/(2.0D+00*X)) * EXP (-X)

        IF (X < 25.0D+00) THEN
            K0 = 16
        ELSE IF (X < 80.0D+00) THEN
            K0 = 10
        ELSE IF (X < 200.0D+00) THEN
            K0 = 8
        ELSE
            K0 = 6
        END IF

        DO L = 0, 1
            BKL = 1.0D+00
            VT = 4.0D+00 * L
            R = 1.0D+00
            DO K = 1, K0
                R = 0.125D+00 * R * (VT-(2.0D+00*K-1.0D+00)**2) / (K*X)
                BKL = BKL + R
            END DO
            BK (L) = A0 * BKL
        END DO
    END IF

    G0 = BK (0)
    G1 = BK (1)
    DO K = 2, NM
        G = 2.0D+00 * (K-1.0D+00) / X * G1 + G0
        BK (K) = G
        G0 = G1
        G1 = G
    END DO

    DI (0) = BI (1)
    DK (0) = - BK (1)
    DO K = 1, NM
        DI (K) = BI (K-1) - K / X * BI (K)
        DK (K) = - BK (K-1) - K / X * BK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE IKV (V, X, VM, BI, DI, BK, DK)

!******************************************************************************
!
!! IKV compute modified Bessel function Iv(x) and Kv(x) and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    17 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Iv(x) and Kv(x).
!    V = N + V0.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) BI(0:N), DI(0:N), BK(0:N), DK(0:N), the
!    values of In+v0(x), In+v0'(x), Kn+v0(x), Kn+v0'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A1
    REAL (KIND=RK) A2
    REAL (KIND=RK) BI (0:*)
    REAL (KIND=RK) BI0
    REAL (KIND=RK) BK (0:*)
    REAL (KIND=RK) BK0
    REAL (KIND=RK) BK1
    REAL (KIND=RK) BK2
    REAL (KIND=RK) CA
    REAL (KIND=RK) CB
    REAL (KIND=RK) CS
    REAL (KIND=RK) CT
    REAL (KIND=RK) DI (0:*)
    REAL (KIND=RK) DK (0:*)
    REAL (KIND=RK) F
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) GAN
    REAL (KIND=RK) GAP
    INTEGER K
    INTEGER K0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PIV
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) SUM
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) V0N
    REAL (KIND=RK) V0P
    REAL (KIND=RK) VM
    REAL (KIND=RK) VT
    REAL (KIND=RK) W0
    REAL (KIND=RK) WA
    REAL (KIND=RK) WW
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    PI = 3.141592653589793D+00
    X2 = X * X
    N = INT (V)
    V0 = V - N
    IF (N == 0) THEN
        N = 1
    END IF

    IF (X < 1.0D-100) THEN

        DO K = 0, N
            BI (K) = 0.0D+00
            DI (K) = 0.0D+00
            BK (K) = - 1.0D+300
            DK (K) = 1.0D+300
        END DO

        IF (V == 0.0D+00) THEN
            BI (0) = 1.0D+00
            DI (1) = 0.5D+00
        END IF

        VM = V
        RETURN

    END IF

    PIV = PI * V0
    VT = 4.0D+00 * V0 * V0

    IF (V0 == 0.0D+00) THEN
        A1 = 1.0D+00
    ELSE
        V0P = 1.0D+00 + V0
        CALL GAMMA (V0P, GAP)
        A1 = (0.5D+00*X) ** V0 / GAP
    END IF

    IF (X < 35.0D+00) THEN
        K0 = 14
    ELSE IF (X < 50.0D+00) THEN
        K0 = 10
    ELSE
        K0 = 8
    END IF

    IF (X <= 18.0D+00) THEN

        BI0 = 1.0D+00
        R = 1.0D+00
        DO K = 1, 30
            R = 0.25D+00 * R * X2 / (K*(K+V0))
            BI0 = BI0 + R
            IF (ABS(R/BI0) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        BI0 = BI0 * A1

    ELSE

        CA = EXP (X) / SQRT (2.0D+00*PI*X)
        SUM = 1.0D+00
        R = 1.0D+00
        DO K = 1, K0
            R = - 0.125D+00 * R * (VT-(2.0D+00*K-1.0D+00)**2) / (K*X)
            SUM = SUM + R
        END DO
        BI0 = CA * SUM

    END IF

    M = MSTA1 (X, 200)

    IF (M < N) THEN
        N = M
    ELSE
        M = MSTA2 (X, N, 15)
    END IF

    F2 = 0.0D+00
    F1 = 1.0D-100
    DO K = M, 0, - 1
        F = 2.0D+00 * (V0+K+1.0D+00) / X * F1 + F2
        IF (K <= N) THEN
            BI (K) = F
        END IF
        F2 = F1
        F1 = F
    END DO

    CS = BI0 / F
    DO K = 0, N
        BI (K) = CS * BI (K)
    END DO

    DI (0) = V0 / X * BI (0) + BI (1)
    DO K = 1, N
        DI (K) = - (K+V0) / X * BI (K) + BI (K-1)
    END DO

    IF (X <= 9.0D+00) THEN

        IF (V0 == 0.0D+00) THEN

            CT = - LOG (0.5D+00*X) - 0.5772156649015329D+00
            CS = 0.0D+00
            W0 = 0.0D+00
            R = 1.0D+00
            DO K = 1, 50
                W0 = W0 + 1.0D+00 / K
                R = 0.25D+00 * R / (K*K) * X2
                CS = CS + R * (W0+CT)
                WA = ABS (CS)
                IF (ABS((WA-WW)/WA) < 1.0D-15) THEN
                    EXIT
                END IF
                WW = WA
            END DO

            BK0 = CT + CS

        ELSE

            V0N = 1.0D+00 - V0
            CALL GAMMA (V0N, GAN)
            A2 = 1.0D+00 / (GAN*(0.5D+00*X)**V0)
            A1 = (0.5D+00*X) ** V0 / GAP
            SUM = A2 - A1
            R1 = 1.0D+00
            R2 = 1.0D+00
            DO K = 1, 120
                R1 = 0.25D+00 * R1 * X2 / (K*(K-V0))
                R2 = 0.25D+00 * R2 * X2 / (K*(K+V0))
                SUM = SUM + A2 * R1 - A1 * R2
                WA = ABS (SUM)
                IF (ABS((WA-WW)/WA) < 1.0D-15) THEN
                    EXIT
                END IF
                WW = WA
            END DO

            BK0 = 0.5D+00 * PI * SUM / SIN (PIV)

        END IF

    ELSE

        CB = EXP (-X) * SQRT (0.5D+00*PI/X)
        SUM = 1.0D+00
        R = 1.0D+00
        DO K = 1, K0
            R = 0.125D+00 * R * (VT-(2.0D+00*K-1.0D+00)**2) / (K*X)
            SUM = SUM + R
        END DO
        BK0 = CB * SUM

    END IF

    BK1 = (1.0D+00/X-BI(1)*BK0) / BI (0)
    BK (0) = BK0
    BK (1) = BK1
    DO K = 2, N
        BK2 = 2.0D+00 * (V0+K-1.0D+00) / X * BK1 + BK0
        BK (K) = BK2
        BK0 = BK1
        BK1 = BK2
    END DO

    DK (0) = V0 / X * BK (0) - BK (1)
    DO K = 1, N
        DK (K) = - (K+V0) / X * BK (K) - BK (K-1)
    END DO

    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CIK01 (Z, CBI0, CDI0, CBI1, CDI1, CBK0, CDK0, CBK1, CDK1)

!******************************************************************************
!
!! CIK01: modified Bessel I0(z), I1(z), K0(z) and K1(z) for complex argument.
!
!  Discussion:
!
!    This procedure computes the modified Bessel functions I0(z), I1(z),
!    K0(z), K1(z), and their derivatives for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CBI0, CDI0, CBI1, CDI1, CBK0, CDK0, CBK1,
!    CDK1, the values of I0(z), I0'(z), I1(z), I1'(z), K0(z), K0'(z), K1(z),
!    and K1'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (12) :: A = (/ 0.125D+00, 7.03125D-02, 7.32421875D-02, &
   & 1.1215209960938D-01, 2.2710800170898D-01, 5.7250142097473D-01, 1.7277275025845D+00, &
   & 6.0740420012735D+00, 2.4380529699556D+01, 1.1001714026925D+02, 5.5133589612202D+02, &
   & 3.0380905109224D+03 /)
    REAL (KIND=RK) A0
    REAL (KIND=RK), SAVE, DIMENSION (10) :: A1 = (/ 0.125D+00, 0.2109375D+00, 1.0986328125D+00, &
   & 1.1775970458984D+01, 2.1461706161499D+002, 5.9511522710323D+03, 2.3347645606175D+05, &
   & 1.2312234987631D+07, 8.401390346421D+08, 7.2031420482627D+10 /)
    REAL (KIND=RK), SAVE, DIMENSION (12) :: B = (/ - 0.375D+00, - 1.171875D-01, - &
   & 1.025390625D-01, - 1.4419555664063D-01, - 2.7757644653320D-01, - 6.7659258842468D-01, - &
   & 1.9935317337513D+00, - 6.8839142681099D+00, - 2.7248827311269D+01, - 1.2159789187654D+02, &
   & - 6.0384407670507D+02, - 3.3022722944809D+03 /)
    COMPLEX (KIND=CK) CA
    COMPLEX (KIND=CK) CB
    COMPLEX (KIND=CK) CBI0
    COMPLEX (KIND=CK) CBI1
    COMPLEX (KIND=CK) CBK0
    COMPLEX (KIND=CK) CBK1
    COMPLEX (KIND=CK) CDI0
    COMPLEX (KIND=CK) CDI1
    COMPLEX (KIND=CK) CDK0
    COMPLEX (KIND=CK) CDK1
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CW
    INTEGER K
    INTEGER K0
    REAL (KIND=RK) PI
    REAL (KIND=RK) W0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2
    COMPLEX (KIND=CK) ZR
    COMPLEX (KIND=CK) ZR2

    PI = 3.141592653589793D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z2 = Z * Z
    Z1 = Z

    IF (A0 == 0.0D+00) THEN
        CBI0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CBI1 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CDI0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CDI1 = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        CBK0 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CBK1 = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CDK0 = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        CDK1 = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        RETURN
    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 <= 18.0D+00) THEN

        CBI0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 50
            CR = 0.25D+00 * CR * Z2 / (K*K)
            CBI0 = CBI0 + CR
            IF (ABS(CR/CBI0) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBI1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 50
            CR = 0.25D+00 * CR * Z2 / (K*(K+1))
            CBI1 = CBI1 + CR
            IF (ABS(CR/CBI1) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBI1 = 0.5D+00 * Z1 * CBI1

    ELSE

        IF (A0 < 35.0D+00) THEN
            K0 = 12
        ELSE IF (A0 < 50.0D+00) THEN
            K0 = 9
        ELSE
            K0 = 7
        END IF

        CA = EXP (Z1) / SQRT (2.0D+00*PI*Z1)
        CBI0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        ZR = 1.0D+00 / Z1
        DO K = 1, K0
            CBI0 = CBI0 + A (K) * ZR ** K
        END DO
        CBI0 = CA * CBI0
        CBI1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CBI1 = CBI1 + B (K) * ZR ** K
        END DO
        CBI1 = CA * CBI1

    END IF

    IF (A0 <= 9.0D+00) THEN

        CS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        CT = - LOG (0.5D+00*Z1) - 0.5772156649015329D+00
        W0 = 0.0D+00
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 50
            W0 = W0 + 1.0D+00 / K
            CR = 0.25D+00 * CR / (K*K) * Z2
            CS = CS + CR * (W0+CT)
            IF (ABS((CS-CW)/CS) < 1.0D-15) THEN
                EXIT
            END IF
            CW = CS
        END DO

        CBK0 = CT + CS

    ELSE

        CB = 0.5D+00 / Z1
        ZR2 = 1.0D+00 / Z2
        CBK0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 10
            CBK0 = CBK0 + A1 (K) * ZR2 ** K
        END DO
        CBK0 = CB * CBK0 / CBI0

    END IF

    CBK1 = (1.0D+00/Z1-CBI1*CBK0) / CBI0

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN

        IF (AIMAG(Z) < 0.0D+00) THEN
            CBK0 = CBK0 + CI * PI * CBI0
            CBK1 = - CBK1 + CI * PI * CBI1
        ELSE
            CBK0 = CBK0 - CI * PI * CBI0
            CBK1 = - CBK1 - CI * PI * CBI1
        END IF

        CBI1 = - CBI1

    END IF

    CDI0 = CBI1
    CDI1 = CBI0 - 1.0D+00 / Z * CBI1
    CDK0 = - CBK1
    CDK1 = - CBK0 - 1.0D+00 / Z * CBK1

    RETURN
END

!******************************************************************************

SUBROUTINE CIKNA (N, Z, NM, CBI, CDI, CBK, CDK)

!******************************************************************************
!
!! CIKNA: modified Bessel functions In(z), Kn(z), derivatives, complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    30 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of In(z) and Kn(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CBI((0:N), CDI(0:N), CBK(0:N), CDK(0:N),
!    the values of In(z), In'(z), Kn(z), Kn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CBI (0:N)
    COMPLEX (KIND=CK) CBI0
    COMPLEX (KIND=CK) CBI1
    COMPLEX (KIND=CK) CBK (0:N)
    COMPLEX (KIND=CK) CBK0
    COMPLEX (KIND=CK) CBK1
    COMPLEX (KIND=CK) CDI (0:N)
    COMPLEX (KIND=CK) CDI0
    COMPLEX (KIND=CK) CDI1
    COMPLEX (KIND=CK) CDK (0:N)
    COMPLEX (KIND=CK) CDK0
    COMPLEX (KIND=CK) CDK1
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CKK
    COMPLEX (KIND=CK) CS
    INTEGER K
    INTEGER M
    INTEGER NM
    COMPLEX (KIND=CK) Z

    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-100) THEN
        DO K = 0, N
            CBI (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDI (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBK (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDK (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO
        CBI (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CDI (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        RETURN
    END IF

    CALL CIK01 (Z, CBI0, CDI0, CBI1, CDI1, CBK0, CDK0, CBK1, CDK1)

    CBI (0) = CBI0
    CBI (1) = CBI1
    CBK (0) = CBK0
    CBK (1) = CBK1
    CDI (0) = CDI0
    CDI (1) = CDI1
    CDK (0) = CDK0
    CDK (1) = CDK1

    IF (N <= 1) THEN
        RETURN
    END IF

    M = MSTA1 (A0, 200)

    IF (M < N) THEN
        NM = M
    ELSE
        M = MSTA2 (A0, N, 15)
    END IF

    CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
    DO K = M, 0, - 1
        CF = 2.0D+00 * (K+1.0D+00) / Z * CF1 + CF2
        IF (K <= NM) THEN
            CBI (K) = CF
        END IF
        CF2 = CF1
        CF1 = CF
    END DO

    CS = CBI0 / CF
    DO K = 0, NM
        CBI (K) = CS * CBI (K)
    END DO

    DO K = 2, NM
        IF (ABS(CBI(K-2)) < ABS(CBI(K-1))) THEN
            CKK = (1.0D+00/Z-CBI(K)*CBK(K-1)) / CBI (K-1)
        ELSE
            CKK = (CBI(K)*CBK(K-2)+2.0D+00*(K-1.0D+00)/(Z*Z)) / CBI (K-2)
        END IF
        CBK (K) = CKK
    END DO

    DO K = 2, NM
        CDI (K) = CBI (K-1) - K / Z * CBI (K)
        CDK (K) = - CBK (K-1) - K / Z * CBK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CIKNB (N, Z, NM, CBI, CDI, CBK, CDK)

!******************************************************************************
!
!! CIKNB computes complex modified Bessel functions In(z) and Kn(z).
!
!  Discussion:
!
!    This procedure also evaluates the derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    30 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of In(z) and Kn(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CB((0:N), CDI(0:N), CBK(0:N), CDK(0:N),
!    the values of In(z), In'(z), Kn(z), Kn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CA0
    COMPLEX (KIND=CK) CBI (0:N)
    COMPLEX (KIND=CK) CBKL
    COMPLEX (KIND=CK) CBS
    COMPLEX (KIND=CK) CDI (0:N)
    COMPLEX (KIND=CK) CBK (0:N)
    COMPLEX (KIND=CK) CDK (0:N)
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CG
    COMPLEX (KIND=CK) CG0
    COMPLEX (KIND=CK) CG1
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CS0
    COMPLEX (KIND=CK) CSK0
    REAL (KIND=RK) EL
    REAL (KIND=RK) FAC
    INTEGER K
    INTEGER K0
    INTEGER L
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PI
    REAL (KIND=RK) VT
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1

    PI = 3.141592653589793D+00
    EL = 0.57721566490153D+00
    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-100) THEN
        DO K = 0, N
            CBI (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CBK (K) = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
            CDI (K) = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            CDK (K) = - CMPLX (1.0D+30, 0.0D+00, KIND=CK)
        END DO
        CBI (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CDI (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        RETURN
    END IF

    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    ELSE
        Z1 = Z
    END IF

    IF (N == 0) THEN
        NM = 1
    END IF

    M = MSTA1 (A0, 200)

    IF (M < NM) THEN
        NM = M
    ELSE
        M = MSTA2 (A0, NM, 15)
    END IF

    CBS = 0.0D+00
    CSK0 = 0.0D+00
    CF0 = 0.0D+00
    CF1 = 1.0D-100

    DO K = M, 0, - 1
        CF = 2.0D+00 * (K+1.0D+00) * CF1 / Z1 + CF0
        IF (K <= NM) THEN
            CBI (K) = CF
        END IF
        IF (K /= 0 .AND. K == 2*INT(K/2)) THEN
            CSK0 = CSK0 + 4.0D+00 * CF / K
        END IF
        CBS = CBS + 2.0D+00 * CF
        CF0 = CF1
        CF1 = CF
    END DO

    CS0 = EXP (Z1) / (CBS-CF)

    DO K = 0, NM
        CBI (K) = CS0 * CBI (K)
    END DO

    IF (A0 <= 9.0D+00) THEN

        CBK (0) = - (LOG(0.5D+00*Z1)+EL) * CBI (0) + CS0 * CSK0
        CBK (1) = (1.0D+00/Z1-CBI(1)*CBK(0)) / CBI (0)

    ELSE

        CA0 = SQRT (PI/(2.0D+00*Z1)) * EXP (-Z1)

        IF (A0 < 25.0D+00) THEN
            K0 = 16
        ELSE IF (A0 < 80.0D+00) THEN
            K0 = 10
        ELSE IF (A0 < 200.0D+00) THEN
            K0 = 8
        ELSE
            K0 = 6
        END IF

        DO L = 0, 1
            CBKL = 1.0D+00
            VT = 4.0D+00 * L
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, K0
                CR = 0.125D+00 * CR * (VT-(2.0D+00*K-1.0D+00)**2) / (K*Z1)
                CBKL = CBKL + CR
            END DO
            CBK (L) = CA0 * CBKL
        END DO
    END IF

    CG0 = CBK (0)
    CG1 = CBK (1)
    DO K = 2, NM
        CG = 2.0D+00 * (K-1.0D+00) / Z1 * CG1 + CG0
        CBK (K) = CG
        CG0 = CG1
        CG1 = CG
    END DO

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        FAC = 1.0D+00
        DO K = 0, NM
            IF (AIMAG(Z) < 0.0D+00) THEN
                CBK (K) = FAC * CBK (K) + CI * PI * CBI (K)
            ELSE
                CBK (K) = FAC * CBK (K) - CI * PI * CBI (K)
            END IF
            CBI (K) = FAC * CBI (K)
            FAC = - FAC
        END DO
    END IF

    CDI (0) = CBI (1)
    CDK (0) = - CBK (1)
    DO K = 1, NM
        CDI (K) = CBI (K-1) - K / Z * CBI (K)
        CDK (K) = - CBK (K-1) - K / Z * CBK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CIKVA (V, Z, VM, CBI, CDI, CBK, CDK)

!******************************************************************************
!
!! CIKVA: modified Bessel functions Iv(z), Kv(z), arbitrary order, complex.
!
!  Discussion:
!
!    Compute the modified Bessel functions Iv(z), Kv(z)
!    and their derivatives for an arbitrary order and
!    complex argument
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of the functions.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) CBI(0:N), CDI(0:N), CBK(0:N), CDK(0:N),
!    the values of In+v0(z), In+v0'(z), Kn+v0(z), Kn+v0'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CA
    COMPLEX (KIND=CK) CA1
    COMPLEX (KIND=CK) CA2
    COMPLEX (KIND=CK) CB
    COMPLEX (KIND=CK) CBI (0:*)
    COMPLEX (KIND=CK) CBI0
    COMPLEX (KIND=CK) CDI (0:*)
    COMPLEX (KIND=CK) CBK (0:*)
    COMPLEX (KIND=CK) CBK0
    COMPLEX (KIND=CK) CBK1
    COMPLEX (KIND=CK) CDK (0:*)
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CG0
    COMPLEX (KIND=CK) CG1
    COMPLEX (KIND=CK) CGK
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CI0
    COMPLEX (KIND=CK) CP
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CR1
    COMPLEX (KIND=CK) CR2
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CSU
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CVK
    REAL (KIND=RK) GAN
    REAL (KIND=RK) GAP
    INTEGER K
    INTEGER K0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PIV
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) V0N
    REAL (KIND=RK) V0P
    REAL (KIND=RK) VM
    REAL (KIND=RK) VT
    REAL (KIND=RK) W0
    REAL (KIND=RK) WS
    REAL (KIND=RK) WS0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2

    PI = 3.141592653589793D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = ABS (Z)
    Z1 = Z
    Z2 = Z * Z
    N = INT (V)
    V0 = V - N
    PIV = PI * V0
    VT = 4.0D+00 * V0 * V0

    IF (N == 0) THEN
        N = 1
    END IF

    IF (A0 < 1.0D-100) THEN

        DO K = 0, N
            CBI (K) = 0.0D+00
            CDI (K) = 0.0D+00
            CBK (K) = - 1.0D+300
            CDK (K) = 1.0D+300
        END DO

        IF (V0 == 0.0D+00) THEN
            CBI (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CDI (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        END IF

        VM = V
        RETURN

    END IF

    IF (A0 < 35.0D+00) THEN
        K0 = 14
    ELSE IF (A0 < 50.0D+00) THEN
        K0 = 10
    ELSE
        K0 = 8
    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 < 18.0D+00) THEN

        IF (V0 == 0.0D+00) THEN
            CA1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        ELSE
            V0P = 1.0D+00 + V0
            CALL GAMMA (V0P, GAP)
            CA1 = (0.5D+00*Z1) ** V0 / GAP
        END IF

        CI0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 50
            CR = 0.25D+00 * CR * Z2 / (K*(K+V0))
            CI0 = CI0 + CR
            IF (ABS(CR) < ABS(CI0)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBI0 = CI0 * CA1

    ELSE

        CA = EXP (Z1) / SQRT (2.0D+00*PI*Z1)
        CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CR = - 0.125D+00 * CR * (VT-(2.0D+00*K-1.0D+00)**2) / (K*Z1)
            CS = CS + CR
        END DO
        CBI0 = CA * CS

    END IF

    M = MSTA1 (A0, 200)

    IF (M < N) THEN
        N = M
    ELSE
        M = MSTA2 (A0, N, 15)
    END IF

    CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
    DO K = M, 0, - 1
        CF = 2.0D+00 * (V0+K+1.0D+00) / Z1 * CF1 + CF2
        IF (K <= N) THEN
            CBI (K) = CF
        END IF
        CF2 = CF1
        CF1 = CF
    END DO

    CS = CBI0 / CF
    DO K = 0, N
        CBI (K) = CS * CBI (K)
    END DO

    IF (A0 <= 9.0D+00) THEN

        IF (V0 == 0.0D+00) THEN
            CT = - LOG (0.5D+00*Z1) - 0.5772156649015329D+00
            CS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            W0 = 0.0D+00
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 50
                W0 = W0 + 1.0D+00 / K
                CR = 0.25D+00 * CR / (K*K) * Z2
                CP = CR * (W0+CT)
                CS = CS + CP
                IF (10 <= K .AND. ABS(CP/CS) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBK0 = CT + CS

        ELSE

            V0N = 1.0D+00 - V0
            CALL GAMMA (V0N, GAN)
            CA2 = 1.0D+00 / (GAN*(0.5D+00*Z1)**V0)
            CA1 = (0.5D+00*Z1) ** V0 / GAP
            CSU = CA2 - CA1
            CR1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 50
                CR1 = 0.25D+00 * CR1 * Z2 / (K*(K-V0))
                CR2 = 0.25D+00 * CR2 * Z2 / (K*(K+V0))
                CSU = CSU + CA2 * CR1 - CA1 * CR2
                WS = ABS (CSU)
                IF (10 <= K .AND. ABS(WS-WS0)/WS < 1.0D-15) THEN
                    EXIT
                END IF
                WS0 = WS
            END DO

            CBK0 = 0.5D+00 * PI * CSU / SIN (PIV)

        END IF

    ELSE

        CB = EXP (-Z1) * SQRT (0.5D+00*PI/Z1)
        CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CR = 0.125D+00 * CR * (VT-(2.0D+00*K-1.0D+00)**2) / (K*Z1)
            CS = CS + CR
        END DO
        CBK0 = CB * CS

    END IF

    CBK1 = (1.0D+00/Z1-CBI(1)*CBK0) / CBI (0)
    CBK (0) = CBK0
    CBK (1) = CBK1
    CG0 = CBK0
    CG1 = CBK1

    DO K = 2, N
        CGK = 2.0D+00 * (V0+K-1.0D+00) / Z1 * CG1 + CG0
        CBK (K) = CGK
        CG0 = CG1
        CG1 = CGK
    END DO

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        DO K = 0, N
            CVK = EXP ((K+V0)*PI*CI)
            IF (AIMAG(Z) < 0.0D+00) THEN
                CBK (K) = CVK * CBK (K) + PI * CI * CBI (K)
                CBI (K) = CBI (K) / CVK
            ELSE IF (0.0D+00 < AIMAG(Z)) THEN
                CBK (K) = CBK (K) / CVK - PI * CI * CBI (K)
                CBI (K) = CVK * CBI (K)
            END IF
        END DO
    END IF

    CDI (0) = V0 / Z * CBI (0) + CBI (1)
    CDK (0) = V0 / Z * CBK (0) - CBK (1)
    DO K = 1, N
        CDI (K) = - (K+V0) / Z * CBI (K) + CBI (K-1)
        CDK (K) = - (K+V0) / Z * CBK (K) - CBK (K-1)
    END DO

    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CIKVB (V, Z, VM, CBI, CDI, CBK, CDK)

!******************************************************************************
!
!! CIKVB: modified Bessel functions,Iv(z), Kv(z), arbitrary order, complex.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of the functions.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, real ( kind = rk ) VM, the highest order computed.
!
!    Output, real ( kind = rk ) CBI(0:N), CDI(0:N), CBK(0:N), CDK(0:N),
!    the values of In+v0(z), In+v0'(z), Kn+v0(z), Kn+v0'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CA
    COMPLEX (KIND=CK) CA1
    COMPLEX (KIND=CK) CA2
    COMPLEX (KIND=CK) CB
    COMPLEX (KIND=CK) CBI (0:*)
    COMPLEX (KIND=CK) CBI0
    COMPLEX (KIND=CK) CDI (0:*)
    COMPLEX (KIND=CK) CBK (0:*)
    COMPLEX (KIND=CK) CBK0
    COMPLEX (KIND=CK) CDK (0:*)
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CF2
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CI0
    COMPLEX (KIND=CK) CKK
    COMPLEX (KIND=CK) CP
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CR1
    COMPLEX (KIND=CK) CR2
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CSU
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CVK
    REAL (KIND=RK) GAN
    REAL (KIND=RK) GAP
    INTEGER K
    INTEGER K0
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PIV
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) V0N
    REAL (KIND=RK) V0P
    REAL (KIND=RK) VM
    REAL (KIND=RK) VT
    REAL (KIND=RK) W0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) Z2

    Z1 = Z
    Z2 = Z * Z
    A0 = ABS (Z)
    PI = 3.141592653589793D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    N = INT (V)
    V0 = V - N
    PIV = PI * V0
    VT = 4.0D+00 * V0 * V0

    IF (N == 0) THEN
        N = 1
    END IF

    IF (A0 < 1.0D-100) THEN
        DO K = 0, N
            CBI (K) = 0.0D+00
            CDI (K) = 0.0D+00
            CBK (K) = - 1.0D+300
            CDK (K) = 1.0D+300
        END DO
        IF (V0 == 0.0D+00) THEN
            CBI (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CDI (1) = CMPLX (0.5D+00, 0.0D+00, KIND=CK)
        END IF
        VM = V
        RETURN
    END IF

    IF (A0 < 35.0D+00) THEN
        K0 = 14
    ELSE IF (A0 < 50.0D+00) THEN
        K0 = 10
    ELSE
        K0 = 8
    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 < 18.0D+00) THEN

        IF (V0 == 0.0D+00) THEN
            CA1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        ELSE
            V0P = 1.0D+00 + V0
            CALL GAMMA (V0P, GAP)
            CA1 = (0.5D+00*Z1) ** V0 / GAP
        END IF

        CI0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 50
            CR = 0.25D+00 * CR * Z2 / (K*(K+V0))
            CI0 = CI0 + CR
            IF (ABS(CR/CI0) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        CBI0 = CI0 * CA1

    ELSE

        CA = EXP (Z1) / SQRT (2.0D+00*PI*Z1)
        CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CR = - 0.125D+00 * CR * (VT-(2.0D+00*K-1.0D+00)**2) / (K*Z1)
            CS = CS + CR
        END DO
        CBI0 = CA * CS

    END IF

    M = MSTA1 (A0, 200)
    IF (M < N) THEN
        N = M
    ELSE
        M = MSTA2 (A0, N, 15)
    END IF

    CF2 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CF1 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
    DO K = M, 0, - 1
        CF = 2.0D+00 * (V0+K+1.0D+00) / Z1 * CF1 + CF2
        IF (K <= N) THEN
            CBI (K) = CF
        END IF
        CF2 = CF1
        CF1 = CF
    END DO
    CS = CBI0 / CF

    DO K = 0, N
        CBI (K) = CS * CBI (K)
    END DO

    IF (A0 <= 9.0D+00) THEN

        IF (V0 == 0.0D+00) THEN

            CT = - LOG (0.5D+00*Z1) - 0.5772156649015329D+00
            CS = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            W0 = 0.0D+00
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 50
                W0 = W0 + 1.0D+00 / K
                CR = 0.25D+00 * CR / (K*K) * Z2
                CP = CR * (W0+CT)
                CS = CS + CP
                IF (10 <= K .AND. ABS(CP/CS) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBK0 = CT + CS

        ELSE

            V0N = 1.0D+00 - V0
            CALL GAMMA (V0N, GAN)
            CA2 = 1.0D+00 / (GAN*(0.5D+00*Z1)**V0)
            CA1 = (0.5D+00*Z1) ** V0 / GAP
            CSU = CA2 - CA1
            CR1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            CR2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 50
                CR1 = 0.25D+00 * CR1 * Z2 / (K*(K-V0))
                CR2 = 0.25D+00 * CR2 * Z2 / (K*(K+V0))
                CP = CA2 * CR1 - CA1 * CR2
                CSU = CSU + CP
                IF (10 <= K .AND. ABS(CP/CSU) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

            CBK0 = 0.5D+00 * PI * CSU / SIN (PIV)

        END IF

    ELSE

        CB = EXP (-Z1) * SQRT (0.5D+00*PI/Z1)
        CS = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, K0
            CR = 0.125D+00 * CR * (VT-(2.0D+00*K-1.0D+00)**2) / (K*Z1)
            CS = CS + CR
        END DO

        CBK0 = CB * CS

    END IF

    CBK (0) = CBK0

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        DO K = 0, N
            CVK = EXP ((K+V0)*PI*CI)
            IF (AIMAG(Z) < 0.0D+00) THEN
                CBK (K) = CVK * CBK (K) + PI * CI * CBI (K)
                CBI (K) = CBI (K) / CVK
            ELSE IF (0.0D+00 < AIMAG(Z)) THEN
                CBK (K) = CBK (K) / CVK - PI * CI * CBI (K)
                CBI (K) = CVK * CBI (K)
            END IF
        END DO
    END IF

    DO K = 1, N
        CKK = (1.0D+00/Z-CBI(K)*CBK(K-1)) / CBI (K-1)
        CBK (K) = CKK
    END DO

    CDI (0) = V0 / Z * CBI (0) + CBI (1)
    CDK (0) = V0 / Z * CBK (0) - CBK (1)
    DO K = 1, N
        CDI (K) = - (K+V0) / Z * CBI (K) + CBI (K-1)
        CDK (K) = - (K+V0) / Z * CBK (K) - CBK (K-1)
    END DO

    VM = N + V0

    RETURN
END

!******************************************************************************

SUBROUTINE CIKLV (V, Z, CBIV, CDIV, CBKV, CDKV)

!******************************************************************************
!
!! CIKLV: modified Bessel functions Iv(z), Kv(z), complex argument, large order.
!
!  Discussion:
!
!    This procedure computes modified Bessel functions Iv(z) and
!    Kv(z) and their derivatives with a complex argument and a large order.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Iv(z) and Kv(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, real ( kind = rk ) CBIV, CDIV, CBKV, CDKV, the values of
!    Iv(z), Iv'(z), Kv(z), Kv'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (91)
    COMPLEX (KIND=CK) CBIV
    COMPLEX (KIND=CK) CBKV
    COMPLEX (KIND=CK) CDIV
    COMPLEX (KIND=CK) CDKV
    COMPLEX (KIND=CK) CETA
    COMPLEX (KIND=CK) CF (12)
    COMPLEX (KIND=CK) CFI
    COMPLEX (KIND=CK) CFK
    COMPLEX (KIND=CK) CSI
    COMPLEX (KIND=CK) CSK
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CT2
    COMPLEX (KIND=CK) CWS
    INTEGER I
    INTEGER K
    INTEGER KM
    INTEGER L
    INTEGER L0
    INTEGER LF
    REAL (KIND=RK) PI
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VR
    COMPLEX (KIND=CK) Z

    PI = 3.141592653589793D+00
    KM = 12
    CALL CJK (KM, A)

    DO L = 1, 0, - 1

        V0 = V - L
        CWS = SQRT (1.0D+00+(Z/V0)*(Z/V0))
        CETA = CWS + LOG (Z/V0/(1.0D+00+CWS))
        CT = 1.0D+00 / CWS
        CT2 = CT * CT
        DO K = 1, KM
            L0 = K * (K+1) / 2 + 1
            LF = L0 + K
            CF (K) = A (LF)
            DO I = LF - 1, L0, - 1
                CF (K) = CF (K) * CT2 + A (I)
            END DO
            CF (K) = CF (K) * CT ** K
        END DO
        VR = 1.0D+00 / V0
        CSI = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, KM
            CSI = CSI + CF (K) * VR ** K
        END DO
        CBIV = SQRT (CT/(2.0D+00*PI*V0)) * EXP (V0*CETA) * CSI
        IF (L == 1) THEN
            CFI = CBIV
        END IF
        CSK = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, KM
            CSK = CSK + (-1) ** K * CF (K) * VR ** K
        END DO
        CBKV = SQRT (PI*CT/(2.0D+00*V0)) * EXP (-V0*CETA) * CSK

        IF (L == 1) THEN
            CFK = CBKV
        END IF

    END DO

    CDIV = CFI - V / Z * CBIV
    CDKV = - CFK - V / Z * CBKV

    RETURN
END

!******************************************************************************

SUBROUTINE CH12N (N, Z, NM, CHF1, CHD1, CHF2, CHD2)

!******************************************************************************
!
!! ch12n() computes Hankel functions of first and second kinds, complex argument.
!
!  Discussion:
!
!    Both the Hankel functions and their derivatives are computed.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    26 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of the functions.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CHF1(0:n), CHD1(0:n), CHF2(0:n), CHD2(0:n),
!    the values of Hn(1)(z), Hn(1)'(z), Hn(2)(z), Hn(2)'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    COMPLEX (KIND=CK) CBI (0:250)
    COMPLEX (KIND=CK) CBJ (0:250)
    COMPLEX (KIND=CK) CBK (0:250)
    COMPLEX (KIND=CK) CBY (0:250)
    COMPLEX (KIND=CK) CDI (0:250)
    COMPLEX (KIND=CK) CDJ (0:250)
    COMPLEX (KIND=CK) CDK (0:250)
    COMPLEX (KIND=CK) CDY (0:250)
    COMPLEX (KIND=CK) CHD1 (0:N)
    COMPLEX (KIND=CK) CHD2 (0:N)
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CFAC
    COMPLEX (KIND=CK) CHF1 (0:N)
    COMPLEX (KIND=CK) CHF2 (0:N)
    COMPLEX (KIND=CK) CI
    INTEGER K
    INTEGER NM
    REAL (KIND=RK) PI
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZI

    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    PI = 3.141592653589793D+00

    IF (AIMAG(Z) < 0.0D+00) THEN

        CALL CJYNB (N, Z, NM, CBJ, CDJ, CBY, CDY)

        DO K = 0, NM
            CHF1 (K) = CBJ (K) + CI * CBY (K)
            CHD1 (K) = CDJ (K) + CI * CDY (K)
        END DO

        ZI = CI * Z
        CALL CIKNB (N, ZI, NM, CBI, CDI, CBK, CDK)
        CFAC = - 2.0D+00 / (PI*CI)

        DO K = 0, NM
            CHF2 (K) = CFAC * CBK (K)
            CHD2 (K) = CFAC * CI * CDK (K)
            CFAC = CFAC * CI
        END DO

    ELSE IF (0.0D+00 < AIMAG(Z)) THEN

        ZI = - CI * Z
        CALL CIKNB (N, ZI, NM, CBI, CDI, CBK, CDK)
        CF1 = - CI
        CFAC = 2.0D+00 / (PI*CI)

        DO K = 0, NM
            CHF1 (K) = CFAC * CBK (K)
            CHD1 (K) = - CFAC * CI * CDK (K)
            CFAC = CFAC * CF1
        END DO

        CALL CJYNB (N, Z, NM, CBJ, CDJ, CBY, CDY)

        DO K = 0, NM
            CHF2 (K) = CBJ (K) - CI * CBY (K)
            CHD2 (K) = CDJ (K) - CI * CDY (K)
        END DO

    ELSE

        CALL CJYNB (N, Z, NM, CBJ, CDJ, CBY, CDY)

        DO K = 0, NM
            CHF1 (K) = CBJ (K) + CI * CBY (K)
            CHD1 (K) = CDJ (K) + CI * CDY (K)
            CHF2 (K) = CBJ (K) - CI * CBY (K)
            CHD2 (K) = CDJ (K) - CI * CDY (K)
        END DO

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   07: INTEGRALS OF BESSEL FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE ITIKA (X, TI, TK)

!******************************************************************************
!
!! ITIKA computes the integral of the modified Bessel functions I0(t) and K0(t).
!
!  Discussion:
!
!    This procedure integrates modified Bessel functions I0(t) and
!    K0(t) with respect to t from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TI, TK, the integrals of I0(t) and K0(t)
!    from 0 to X.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (10) :: A = (/ 0.625D+00, 1.0078125D+00, 2.5927734375D+00, &
   & 9.1868591308594D+00, 4.1567974090576D+01, 2.2919635891914D+02, 1.491504060477D+03, &
   & 1.1192354495579D+04, 9.515939374212D+04, 9.0412425769041D+05 /)
    REAL (KIND=RK) B1
    REAL (KIND=RK) B2
    REAL (KIND=RK) E0
    REAL (KIND=RK) EL
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) RC1
    REAL (KIND=RK) RC2
    REAL (KIND=RK) RS
    REAL (KIND=RK) TI
    REAL (KIND=RK) TK
    REAL (KIND=RK) TW
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (X == 0.0D+00) THEN

        TI = 0.0D+00
        TK = 0.0D+00
        RETURN

    ELSE IF (X < 20.0D+00) THEN

        X2 = X * X
        TI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 50
            R = 0.25D+00 * R * (2*K-1.0D+00) / (2*K+1.0D+00) / (K*K) * X2
            TI = TI + R
            IF (ABS(R/TI) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        TI = TI * X

    ELSE

        TI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 10
            R = R / X
            TI = TI + A (K) * R
        END DO
        RC1 = 1.0D+00 / SQRT (2.0D+00*PI*X)
        TI = RC1 * EXP (X) * TI

    END IF

    IF (X < 12.0D+00) THEN

        E0 = EL + LOG (X/2.0D+00)
        B1 = 1.0D+00 - E0
        B2 = 0.0D+00
        RS = 0.0D+00
        R = 1.0D+00
        DO K = 1, 50
            R = 0.25D+00 * R * (2*K-1.0D+00) / (2*K+1.0D+00) / (K*K) * X2
            B1 = B1 + R * (1.0D+00/(2*K+1)-E0)
            RS = RS + 1.0D+00 / K
            B2 = B2 + R * RS
            TK = B1 + B2
            IF (ABS((TK-TW)/TK) < 1.0D-12) THEN
                EXIT
            END IF
            TW = TK
        END DO

        TK = TK * X

    ELSE

        TK = 1.0D+00
        R = 1.0D+00
        DO K = 1, 10
            R = - R / X
            TK = TK + A (K) * R
        END DO
        RC2 = SQRT (PI/(2.0D+00*X))
        TK = PI / 2.0D+00 - RC2 * TK * EXP (-X)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITIKB (X, TI, TK)

!******************************************************************************
!
!! ITIKB computes the integral of the Bessel functions I0(t) and K0(t).
!
!  Discussion:
!
!    This procedure integrates Bessel functions I0(t) and K0(t)
!    with respect to t from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    24 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TI, TK, the integral of I0(t) and K0(t)
!    from 0 to X.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) PI
    REAL (KIND=RK) T
    REAL (KIND=RK) T1
    REAL (KIND=RK) TI
    REAL (KIND=RK) TK
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN

        TI = 0.0D+00

    ELSE IF (X < 5.0D+00) THEN

        T1 = X / 5.0D+00
        T = T1 * T1
        TI = ((((((((0.59434D-03*T+0.4500642D-02)*T+0.044686921D+00)*T+0.300704878D+00)*T+&
       & 1.471860153D+00)*T+4.844024624D+00)*T+9.765629849D+00)*T+10.416666367D+00)*T+5.0D+00) &
       & * T1

    ELSE IF (5.0D+00 <= X .AND. X <= 8.0D+00) THEN

        T = 5.0D+00 / X
        TI = (((-0.015166D+00*T-0.0202292D+00)*T+0.1294122D+00)*T-0.0302912D+00) * T + &
       & 0.4161224D+00
        TI = TI * EXP (X) / SQRT (X)

    ELSE

        T = 8.0D+00 / X
        TI = (((((-0.0073995D+00*T+0.017744D+00)*T-0.0114858D+00)*T+0.55956D-02)*T+0.59191D-02)&
       & *T+0.0311734D+00) * T + 0.3989423D+00
        TI = TI * EXP (X) / SQRT (X)

    END IF

    IF (X == 0.0D+00) THEN

        TK = 0.0D+00

    ELSE IF (X <= 2.0D+00) THEN

        T1 = X / 2.0D+00
        T = T1 * T1
        TK = ((((((0.116D-05*T+0.2069D-04)*T+0.62664D-03)*T+0.01110118D+00)*T+0.11227902D+00)*T+&
       & 0.50407836D+00)*T+0.84556868D+00) * T1
        TK = TK - LOG (X/2.0D+00) * TI

    ELSE IF (2.0D+00 < X .AND. X <= 4.0D+00) THEN

        T = 2.0D+00 / X
        TK = (((0.0160395D+00*T-0.0781715D+00)*T+0.185984D+00)*T-0.3584641D+00) * T + &
       & 1.2494934D+00
        TK = PI / 2.0D+00 - TK * EXP (-X) / SQRT (X)

    ELSE IF (4.0D+00 < X .AND. X <= 7.0D+00) THEN

        T = 4.0D+00 / X
        TK = (((((0.37128D-02*T-0.0158449D+00)*T+0.0320504D+00)*T-0.0481455D+00)*T+0.0787284D+00)&
       & *T-0.1958273D+00) * T + 1.2533141D+00
        TK = PI / 2.0D+00 - TK * EXP (-X) / SQRT (X)

    ELSE

        T = 7.0D+00 / X
        TK = (((((0.33934D-03*T-0.163271D-02)*T+0.417454D-02)*T-0.933944D-02)*T+0.02576646D+00)&
       & *T-0.11190289D+00) * T + 1.25331414D+00
        TK = PI / 2.0D+00 - TK * EXP (-X) / SQRT (X)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITJYA (X, TJ, TY)

!******************************************************************************
!
!! ITJYA computes integrals of Bessel functions J0(t) and Y0(t).
!
!  Discussion:
!
!    This procedure integrates Bessel functions J0(t) and Y0(t) with
!    respect to t from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TJ, TY, the integrals of J0(t) and Y0(t)
!    from 0 to x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (18)
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) AF
    REAL (KIND=RK) BF
    REAL (KIND=RK) BG
    REAL (KIND=RK) EL
    REAL (KIND=RK) EPS
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) R2
    REAL (KIND=RK) RC
    REAL (KIND=RK) RS
    REAL (KIND=RK) TJ
    REAL (KIND=RK) TY
    REAL (KIND=RK) TY1
    REAL (KIND=RK) TY2
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XP

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    EPS = 1.0D-12

    IF (X == 0.0D+00) THEN

        TJ = 0.0D+00
        TY = 0.0D+00

    ELSE IF (X <= 20.0D+00) THEN

        X2 = X * X
        TJ = X
        R = X
        DO K = 1, 60
            R = - 0.25D+00 * R * (2*K-1.0D+00) / (2*K+1.0D+00) / (K*K) * X2
            TJ = TJ + R
            IF (ABS(R) < ABS(TJ)*EPS) THEN
                EXIT
            END IF
        END DO

        TY1 = (EL+LOG(X/2.0D+00)) * TJ
        RS = 0.0D+00
        TY2 = 1.0D+00
        R = 1.0D+00

        DO K = 1, 60
            R = - 0.25D+00 * R * (2*K-1.0D+00) / (2*K+1.0D+00) / (K*K) * X2
            RS = RS + 1.0D+00 / K
            R2 = R * (RS+1.0D+00/(2.0D+00*K+1.0D+00))
            TY2 = TY2 + R2
            IF (ABS(R2) < ABS(TY2)*EPS) THEN
                EXIT
            END IF
        END DO

        TY = (TY1-X*TY2) * 2.0D+00 / PI

    ELSE

        A0 = 1.0D+00
        A1 = 5.0D+00 / 8.0D+00
        A (1) = A1

        DO K = 1, 16
            AF = ((1.5D+00*(K+0.5D+00)*(K+5.0D+00/6.0D+00)*A1-0.5D+00*(K+0.5D+00)*(K+0.5D+00)&
           & *(K-0.5D+00)*A0)) / (K+1.0D+00)
            A (K+1) = AF
            A0 = A1
            A1 = AF
        END DO

        BF = 1.0D+00
        R = 1.0D+00
        DO K = 1, 8
            R = - R / (X*X)
            BF = BF + A (2*K) * R
        END DO
        BG = A (1) / X
        R = 1.0D+00 / X
        DO K = 1, 8
            R = - R / (X*X)
            BG = BG + A (2*K+1) * R
        END DO
        XP = X + 0.25D+00 * PI
        RC = SQRT (2.0D+00/(PI*X))
        TJ = 1.0D+00 - RC * (BF*COS(XP)+BG*SIN(XP))
        TY = RC * (BG*COS(XP)-BF*SIN(XP))

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITJYB (X, TJ, TY)

!******************************************************************************
!
!! ITJYB computes integrals of Bessel functions J0(t) and Y0(t).
!
!  Discussion:
!
!    This procedure integrates Bessel functions J0(t) and Y0(t)
!    with respect to t from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TJ, TY, the integrals of J0(t) and Y0(t)
!    from 0 to x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) F0
    REAL (KIND=RK) G0
    REAL (KIND=RK) PI
    REAL (KIND=RK) T
    REAL (KIND=RK) TJ
    REAL (KIND=RK) TY
    REAL (KIND=RK) X
    REAL (KIND=RK) X1
    REAL (KIND=RK) XT

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN

        TJ = 0.0D+00
        TY = 0.0D+00

    ELSE IF (X <= 4.0D+00) THEN

        X1 = X / 4.0D+00
        T = X1 * X1

        TJ = (((((((-0.133718D-03*T+0.2362211D-02)*T-0.025791036D+00)*T+0.197492634D+00)*T-&
       & 1.015860606D+00)*T+3.199997842D+00)*T-5.333333161D+00)*T+4.0D+00) * X1

        TY = ((((((((0.13351D-04*T-0.235002D-03)*T+0.3034322d-02)*T-0.029600855D+00)*T+&
       & 0.203380298D+00)*T-0.904755062D+00)*T+2.287317974D+00)*T-2.567250468D+00)*T+&
       & 1.076611469D+00) * X1

        TY = 2.0D+00 / PI * LOG (X/2.0D+00) * TJ - TY

    ELSE IF (X <= 8.0D+00) THEN

        XT = X - 0.25D+00 * PI
        T = 16.0D+00 / (X*X)

        F0 = ((((((0.1496119D-02*T-0.739083D-02)*T+0.016236617D+00)*T-0.022007499D+00)*T+&
       & 0.023644978D+00)*T-0.031280848D+00)*T+0.124611058D+00) * 4.0D+00 / X

        G0 = (((((0.1076103D-02*T-0.5434851D-02)*T+0.01242264D+00)*T-0.018255209D+00)*T+&
       & 0.023664841D+00)*T-0.049635633D+00) * T + 0.79784879D+00

        TJ = 1.0D+00 - (F0*COS(XT)-G0*SIN(XT)) / SQRT (X)

        TY = - (F0*SIN(XT)+G0*COS(XT)) / SQRT (X)

    ELSE

        T = 64.0D+00 / (X*X)
        XT = X - 0.25D+00 * PI

        F0 = (((((((-0.268482D-04*T+0.1270039D-03)*T-0.2755037D-03)*T+0.3992825D-03)*T-&
       & 0.5366169D-03)*T+0.10089872D-02)*T-0.40403539D-02)*T+0.0623347304D+00) * 8.0D+00 / X

        G0 = ((((((-0.226238D-04*T+0.1107299D-03)*T-0.2543955D-03)*T+0.4100676D-03)*T-&
       & 0.6740148D-03)*T+0.17870944D-02)*T-0.01256424405D+00) * T + 0.79788456D+00

        TJ = 1.0D+00 - (F0*COS(XT)-G0*SIN(XT)) / SQRT (X)

        TY = - (F0*SIN(XT)+G0*COS(XT)) / SQRT (X)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITTIKA (X, TTI, TTK)

!******************************************************************************
!
!! ITTIKA integrates (I0(t)-1)/t from 0 to x, K0(t)/t from x to infinity.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    23 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the integral limit.
!
!    Output, real ( kind = rk ) TTI, TTK, the integrals of [I0(t)-1]/t
!    from 0 to x, and of K0(t)/t from x to oo.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) B1
    REAL (KIND=RK), SAVE, DIMENSION (8) :: C = (/ 1.625D+00, 4.1328125D+00, 1.45380859375D+01, &
   & 6.553353881835D+01, 3.6066157150269D+02, 2.3448727161884D+03, 1.7588273098916D+04, &
   & 1.4950639538279D+05 /)
    REAL (KIND=RK) E0
    REAL (KIND=RK) EL
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) R2
    REAL (KIND=RK) RC
    REAL (KIND=RK) RS
    REAL (KIND=RK) TTI
    REAL (KIND=RK) TTK
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (X == 0.0D+00) THEN
        TTI = 0.0D+00
        TTK = 1.0D+300
        RETURN
    END IF

    IF (X < 40.0D+00) THEN
        TTI = 1.0D+00
        R = 1.0D+00
        DO K = 2, 50
            R = 0.25D+00 * R * (K-1.0D+00) / (K*K*K) * X * X
            TTI = TTI + R
            IF (ABS(R/TTI) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTI = TTI * 0.125D+00 * X * X

    ELSE

        TTI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 8
            R = R / X
            TTI = TTI + C (K) * R
        END DO
        RC = X * SQRT (2.0D+00*PI*X)
        TTI = TTI * EXP (X) / RC

    END IF

    IF (X <= 12.0D+00) THEN

        E0 = (0.5D+00*LOG(X/2.0D+00)+EL) * LOG (X/2.0D+00) + PI * PI / 24.0D+00 + 0.5D+00 * EL &
       & * EL
        B1 = 1.5D+00 - (EL+LOG(X/2.0D+00))
        RS = 1.0D+00
        R = 1.0D+00
        DO K = 2, 50
            R = 0.25D+00 * R * (K-1.0D+00) / (K*K*K) * X * X
            RS = RS + 1.0D+00 / K
            R2 = R * (RS+1.0D+00/(2.0D+00*K)-(EL+LOG(X/2.0D+00)))
            B1 = B1 + R2
            IF (ABS(R2/B1) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTK = E0 - 0.125D+00 * X * X * B1

    ELSE

        TTK = 1.0D+00
        R = 1.0D+00
        DO K = 1, 8
            R = - R / X
            TTK = TTK + C (K) * R
        END DO
        RC = X * SQRT (2.0D+00/PI*X)
        TTK = TTK * EXP (-X) / RC

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITTIKB (X, TTI, TTK)

!******************************************************************************
!
!! ITTIKB integrates (I0(t)-1)/t from 0 to x, K0(t)/t from x to infinity.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the integral limit.
!
!    Output, real ( kind = rk ) TTI, TTK, the integrals of
!    [I0(t)-1]/t from 0 to x, and K0(t)/t from x to oo.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) E0
    REAL (KIND=RK) EL
    REAL (KIND=RK) PI
    REAL (KIND=RK) T
    REAL (KIND=RK) T1
    REAL (KIND=RK) TTI
    REAL (KIND=RK) TTK
    REAL (KIND=RK) X
    REAL (KIND=RK) X1

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (X == 0.0D+00) THEN

        TTI = 0.0D+00

    ELSE IF (X <= 5.0D+00) THEN

        X1 = X / 5.0D+00
        T = X1 * X1
        TTI = (((((((0.1263D-03*T+0.96442D-03)*T+0.968217D-02)*T+0.06615507D+00)*T&
       & +0.33116853D+00)*T+1.13027241D+00)*T+2.44140746D+00)*T+3.12499991D+00) * T

    ELSE

        T = 5.0D+00 / X
        TTI = (((((((((2.1945464D+00*T-3.5195009D+00)*T-11.9094395D+00)*T+40.394734D+00)*T-&
       & 48.0524115D+00)*T+28.1221478D+00)*T-8.6556013D+00)*T+1.4780044D+00)*T-0.0493843D+00)*T+&
       & 0.1332055D+00) * T + 0.3989314D+00
        TTI = TTI * EXP (X) / (SQRT(X)*X)

    END IF

    IF (X == 0.0D+00) THEN

        TTK = 1.0D+300

    ELSE IF (X <= 2.0D+00) THEN

        T1 = X / 2.0D+00
        T = T1 * T1
        TTK = (((((0.77D-06*T+0.1544D-04)*T+0.48077D-03)*T+0.925821D-02)*T+0.10937537D+00)*T+&
       & 0.74999993D+00) * T
        E0 = EL + LOG (X/2.0D+00)
        TTK = PI * PI / 24.0D+00 + E0 * (0.5D+00*E0+TTI) - TTK

    ELSE IF (X <= 4.0D+00) THEN

        T = 2.0D+00 / X
        TTK = (((0.06084D+00*T-0.280367D+00)*T+0.590944D+00)*T-0.850013D+00) * T + 1.234684D+00
        TTK = TTK * EXP (-X) / (SQRT(X)*X)

    ELSE

        T = 4.0D+00 / X
        TTK = (((((0.02724D+00*T-0.1110396D+00)*T+0.2060126D+00)*T-0.2621446D+00)*T&
       & +0.3219184D+00)*T-0.5091339D+00) * T + 1.2533141D+00
        TTK = TTK * EXP (-X) / (SQRT(X)*X)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITTJYA (X, TTJ, TTY)

!******************************************************************************
!
!! ITTJYA integrates (1-J0(t))/t from 0 to x, and Y0(t)/t from x to infinity.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the integral limit.
!
!    Output, real ( kind = rk ) TTJ, TTY, the integrals of [1-J0(t)]/t
!    from 0 to x and of Y0(t)/t from x to oo.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) B1
    REAL (KIND=RK) BJ0
    REAL (KIND=RK) BJ1
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) E0
    REAL (KIND=RK) EL
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    INTEGER K
    INTEGER L
    REAL (KIND=RK) PI
    REAL (KIND=RK) PX
    REAL (KIND=RK) QX
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) RS
    REAL (KIND=RK) T
    REAL (KIND=RK) TTJ
    REAL (KIND=RK) TTY
    REAL (KIND=RK) VT
    REAL (KIND=RK) X
    REAL (KIND=RK) XK

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (X == 0.0D+00) THEN

        TTJ = 0.0D+00
        TTY = - 1.0D+300

    ELSE IF (X <= 20.0D+00) THEN

        TTJ = 1.0D+00
        R = 1.0D+00
        DO K = 2, 100
            R = - 0.25D+00 * R * (K-1.0D+00) / (K*K*K) * X * X
            TTJ = TTJ + R
            IF (ABS(R) < ABS(TTJ)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTJ = TTJ * 0.125D+00 * X * X
        E0 = 0.5D+00 * (PI*PI/6.0D+00-EL*EL) - (0.5D+00*LOG(X/2.0D+00)+EL) * LOG (X/2.0D+00)
        B1 = EL + LOG (X/2.0D+00) - 1.5D+00
        RS = 1.0D+00
        R = - 1.0D+00
        DO K = 2, 100
            R = - 0.25D+00 * R * (K-1.0D+00) / (K*K*K) * X * X
            RS = RS + 1.0D+00 / K
            R2 = R * (RS+1.0D+00/(2.0D+00*K)-(EL+LOG(X/2.0D+00)))
            B1 = B1 + R2
            IF (ABS(R2) < ABS(B1)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTY = 2.0D+00 / PI * (E0+0.125D+00*X*X*B1)

    ELSE

        A0 = SQRT (2.0D+00/(PI*X))

        DO L = 0, 1

            VT = 4.0D+00 * L * L
            PX = 1.0D+00
            R = 1.0D+00
            DO K = 1, 14
                R = - 0.0078125D+00 * R * (VT-(4.0D+00*K-3.0D+00)**2) / (X*K) * &
               & (VT-(4.0D+00*K-1.0D+00)**2) / ((2.0D+00*K-1.0D+00)*X)
                PX = PX + R
                IF (ABS(R) < ABS(PX)*1.0D-12) THEN
                    EXIT
                END IF
            END DO

            QX = 1.0D+00
            R = 1.0D+00
            DO K = 1, 14
                R = - 0.0078125D+00 * R * (VT-(4.0D+00*K-1.0D+00)**2) / (X*K) * &
               & (VT-(4.0D+00*K+1.0D+00)**2) / (2.0D+00*K+1.0D+00) / X
                QX = QX + R
                IF (ABS(R) < ABS(QX)*1.0D-12) THEN
                    EXIT
                END IF
            END DO

            QX = 0.125D+00 * (VT-1.0D+00) / X * QX
            XK = X - (0.25D+00+0.5D+00*L) * PI
            BJ1 = A0 * (PX*COS(XK)-QX*SIN(XK))
            BY1 = A0 * (PX*SIN(XK)+QX*COS(XK))
            IF (L == 0) THEN
                BJ0 = BJ1
                BY0 = BY1
            END IF

        END DO

        T = 2.0D+00 / X
        G0 = 1.0D+00
        R0 = 1.0D+00
        DO K = 1, 10
            R0 = - K * K * T * T * R0
            G0 = G0 + R0
        END DO

        G1 = 1.0D+00
        R1 = 1.0D+00
        DO K = 1, 10
            R1 = - K * (K+1.0D+00) * T * T * R1
            G1 = G1 + R1
        END DO

        TTJ = 2.0D+00 * G1 * BJ0 / (X*X) - G0 * BJ1 / X + EL + LOG (X/2.0D+00)
        TTY = 2.0D+00 * G1 * BY0 / (X*X) - G0 * BY1 / X

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITTJYB (X, TTJ, TTY)

!******************************************************************************
!
!! ITTJYB integrates (1-J0(t))/t from 0 to x, and Y0(t)/t from x to infinity.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the integral limit.
!
!    Output, real ( kind = rk ) TTJ, TTY, the integrals of [1-J0(t)]/t
!    from 0 to x and of Y0(t)/t from x to oo.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) E0
    REAL (KIND=RK) EL
    REAL (KIND=RK) F0
    REAL (KIND=RK) G0
    REAL (KIND=RK) PI
    REAL (KIND=RK) T
    REAL (KIND=RK) T1
    REAL (KIND=RK) TTJ
    REAL (KIND=RK) TTY
    REAL (KIND=RK) X
    REAL (KIND=RK) X1
    REAL (KIND=RK) XT

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (X == 0.0D+00) THEN

        TTJ = 0.0D+00
        TTY = - 1.0D+300

    ELSE IF (X <= 4.0D+00) THEN

        X1 = X / 4.0D+00
        T = X1 * X1

        TTJ = ((((((0.35817D-04*T-0.639765D-03)*T+0.7092535D-02)*T-0.055544803D+00)*T+&
       & 0.296292677D+00)*T-0.999999326D+00)*T+1.999999936D+00) * T

        TTY = (((((((-0.3546D-05*T+0.76217D-04)*T-0.1059499D-02)*T+0.010787555D+00)*T-&
       & 0.07810271D+00)*T+0.377255736D+00)*T-1.114084491D+00)*T+1.909859297D+00) * T

        E0 = EL + LOG (X/2.0D+00)
        TTY = PI / 6.0D+00 + E0 / PI * (2.0D+00*TTJ-E0) - TTY

    ELSE IF (X <= 8.0D+00) THEN

        XT = X + 0.25D+00 * PI
        T1 = 4.0D+00 / X
        T = T1 * T1

        F0 = (((((0.0145369D+00*T-0.0666297D+00)*T+0.1341551D+00)*T-0.1647797D+00)*T&
       & +0.1608874D+00)*T-0.2021547D+00) * T + 0.7977506D+00

        G0 = ((((((0.0160672D+00*T-0.0759339D+00)*T+0.1576116D+00)*T-0.1960154D+00)*T+&
       & 0.1797457D+00)*T-0.1702778D+00)*T+0.3235819D+00) * T1

        TTJ = (F0*COS(XT)+G0*SIN(XT)) / (SQRT(X)*X)
        TTJ = TTJ + EL + LOG (X/2.0D+00)
        TTY = (F0*SIN(XT)-G0*COS(XT)) / (SQRT(X)*X)

    ELSE

        T = 8.0D+00 / X
        XT = X + 0.25D+00 * PI

        F0 = (((((0.18118D-02*T-0.91909D-02)*T+0.017033D+00)*T-0.9394D-03)*T-0.051445D+00)*T-&
       & 0.11D-05) * T + 0.7978846D+00

        G0 = (((((-0.23731D-02*T+0.59842D-02)*T+0.24437D-02)*T-0.0233178D+00)*T+0.595D-04)*T+&
       & 0.1620695D+00) * T

        TTJ = (F0*COS(XT)+G0*SIN(XT)) / (SQRT(X)*X) + EL + LOG (X/2.0D+00)
        TTY = (F0*SIN(XT)-G0*COS(XT)) / (SQRT(X)*X)

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   08: SPHERICAL BESSEL FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE SPHI (N, X, NM, SI, DI)

!******************************************************************************
!
!! SPHI computes spherical Bessel functions in(x) and their derivatives in'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of In(X).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) SI(0:N), DI(0:N), the values and derivatives
!    of the function of orders 0 through N.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) CS
    REAL (KIND=RK) DI (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) SI (0:N)
    REAL (KIND=RK) SI0
    REAL (KIND=RK) X

    NM = N

    IF (ABS(X) < 1.0D-100) THEN
        DO K = 0, N
            SI (K) = 0.0D+00
            DI (K) = 0.0D+00
        END DO
        SI (0) = 1.0D+00
        DI (1) = 0.333333333333333D+00
        RETURN
    END IF

    SI (0) = SINH (X) / X
    SI (1) = - (SINH(X)/X-COSH(X)) / X
    SI0 = SI (0)

    IF (2 <= N) THEN

        M = MSTA1 (X, 200)
        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF
        F0 = 0.0D+00
        F1 = 1.0D+00 - 100
        DO K = M, 0, - 1
            F = (2.0D+00*K+3.0D+00) * F1 / X + F0
            IF (K <= NM) THEN
                SI (K) = F
            END IF
            F0 = F1
            F1 = F
        END DO
        CS = SI0 / F
        DO K = 0, NM
            SI (K) = CS * SI (K)
        END DO

    END IF

    DI (0) = SI (1)
    DO K = 1, NM
        DI (K) = SI (K-1) - (K+1.0D+00) / X * SI (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SPHJ (N, X, NM, SJ, DJ)

!******************************************************************************
!
!! SPHJ computes spherical Bessel functions jn(x) and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    12 January 2016
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin.
!    Modifications suggested by Vincent Lagage, 12 January 2016.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) SJ(0:N), the values of jn(x).
!
!    Output, real ( kind = rk ) DJ(0:N), the values of jn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) CS
    REAL (KIND=RK) DJ (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) SA
    REAL (KIND=RK) SB
    REAL (KIND=RK) SJ (0:N)
    REAL (KIND=RK) X

    NM = N
!
!  Original code.
!
    IF (.TRUE.) THEN

        IF (ABS(X) <= 1.0D-100) THEN
            DO K = 0, N
                SJ (K) = 0.0D+00
                DJ (K) = 0.0D+00
            END DO
            SJ (0) = 1.0D+00
            DJ (1) = 0.3333333333333333D+00
            RETURN
        END IF
!
!  Updated code.
!
    ELSE

        IF (ABS(X) <= 1.0D-16) THEN
            DO K = 0, N
                SJ (K) = 0.0D+00
                DJ (K) = 0.0D+00
            END DO
            SJ (0) = 1.0D+00
            IF (0 < N) THEN
                DO K = 1, N
                    SJ (K) = SJ (K-1) * X / REAL (2*K+1, KIND=RK)
                END DO
                DJ (1) = 1.0D+00 / 3.0D+00
            END IF
            RETURN
        END IF

    END IF

    SJ (0) = SIN (X) / X
    SJ (1) = (SJ(0)-COS(X)) / X

    IF (2 <= N) THEN

        SA = SJ (0)
        SB = SJ (1)
        M = MSTA1 (X, 200)
        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF

        F0 = 0.0D+00
        F1 = 1.0D+00 - 100
        DO K = M, 0, - 1
            F = (2.0D+00*K+3.0D+00) * F1 / X - F0
            IF (K <= NM) THEN
                SJ (K) = F
            END IF
            F0 = F1
            F1 = F
        END DO

        IF (ABS(SA) <= ABS(SB)) THEN
            CS = SB / F0
        ELSE
            CS = SA / F
        END IF

        DO K = 0, NM
            SJ (K) = CS * SJ (K)
        END DO

    END IF

    DJ (0) = (COS(X)-SIN(X)/X) / X
    DO K = 1, NM
        DJ (K) = SJ (K-1) - (K+1.0D+00) * SJ (K) / X
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SPHK (N, X, NM, SK, DK)

!******************************************************************************
!
!! SPHK computes modified spherical Bessel functions kn(x) and derivatives.
!
!  Discussion:
!
!    This procedure computes modified spherical Bessel functions
!    of the second kind, kn(x) and kn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) SK(0:N), DK(0:N), the values of kn(x) and kn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) DK (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER K
    INTEGER NM
    REAL (KIND=RK) SK (0:N)
    REAL (KIND=RK) PI
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    NM = N
    IF (X < 1.0D-60) THEN
        DO K = 0, N
            SK (K) = 1.0D+300
            DK (K) = - 1.0D+300
        END DO
        RETURN
    END IF

    SK (0) = 0.5D+00 * PI / X * EXP (-X)
    SK (1) = SK (0) * (1.0D+00+1.0D+00/X)
    F0 = SK (0)
    F1 = SK (1)
    DO K = 2, N
        F = (2.0D+00*K-1.0D+00) * F1 / X + F0
        SK (K) = F
        IF (1.0D+300 < ABS(F)) THEN
            EXIT
        END IF
        F0 = F1
        F1 = F
    END DO

    NM = K - 1

    DK (0) = - SK (1)
    DO K = 1, NM
        DK (K) = - SK (K-1) - (K+1.0D+00) / X * SK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SPHY (N, X, NM, SY, DY)

!******************************************************************************
!
!! SPHY computes spherical Bessel functions yn(x) and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) SY(0:N), DY(0:N), the values of yn(x) and yn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) DY (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER K
    INTEGER NM
    REAL (KIND=RK) SY (0:N)
    REAL (KIND=RK) X

    NM = N

    IF (X < 1.0D-60) THEN
        DO K = 0, N
            SY (K) = - 1.0D+300
            DY (K) = 1.0D+300
        END DO
        RETURN
    END IF

    SY (0) = - COS (X) / X
    SY (1) = (SY(0)-SIN(X)) / X
    F0 = SY (0)
    F1 = SY (1)
    DO K = 2, N
        F = (2.0D+00*K-1.0D+00) * F1 / X - F0
        SY (K) = F
        IF (1.0D+300 <= ABS(F)) THEN
            EXIT
        END IF
        F0 = F1
        F1 = F
    END DO

    NM = K - 1
    DY (0) = (SIN(X)+COS(X)/X) / X
    DO K = 1, NM
        DY (K) = SY (K-1) - (K+1.0D+00) * SY (K) / X
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CSPHIK (N, Z, NM, CSI, CDI, CSK, CDK)

!******************************************************************************
!
!! CSPHIK: complex modified spherical Bessel functions and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of in(z) and kn(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CSI(0:N), CDI(0:N), CSK(0:N), CDK(0:N),
!    the values of in(z), in'(z), kn(z), kn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CCOSH1
    COMPLEX (KIND=CK) CDI (0:N)
    COMPLEX (KIND=CK) CDK (0:N)
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CSI (0:N)
    COMPLEX (KIND=CK) CSI0
    COMPLEX (KIND=CK) CSI1
    COMPLEX (KIND=CK) CSINH1
    COMPLEX (KIND=CK) CSK (0:N)
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) PI
    COMPLEX (KIND=CK) Z

    PI = 3.141592653589793D+00
    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-60) THEN
        DO K = 0, N
            CSI (K) = 0.0D+00
            CDI (K) = 0.0D+00
            CSK (K) = 1.0D+300
            CDK (K) = - 1.0D+300
        END DO
        CSI (0) = 1.0D+00
        CDI (1) = 0.3333333333333333D+00
        RETURN
    END IF

    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    CSINH1 = SIN (CI*Z) / CI
    CCOSH1 = COS (CI*Z)
    CSI0 = CSINH1 / Z
    CSI1 = (-CSINH1/Z+CCOSH1) / Z
    CSI (0) = CSI0
    CSI (1) = CSI1

    IF (2 <= N) THEN

        M = MSTA1 (A0, 200)
        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (A0, N, 15)
        END IF

        CF0 = 0.0D+00
        CF1 = 1.0D+00 - 100
        DO K = M, 0, - 1
            CF = (2.0D+00*K+3.0D+00) * CF1 / Z + CF0
            IF (K <= NM) THEN
                CSI (K) = CF
            END IF
            CF0 = CF1
            CF1 = CF
        END DO

        IF (ABS(CSI0) <= ABS(CSI1)) THEN
            CS = CSI1 / CF0
        ELSE
            CS = CSI0 / CF
        END IF

        DO K = 0, NM
            CSI (K) = CS * CSI (K)
        END DO

    END IF

    CDI (0) = CSI (1)
    DO K = 1, NM
        CDI (K) = CSI (K-1) - (K+1.0D+00) * CSI (K) / Z
    END DO

    CSK (0) = 0.5D+00 * PI / Z * EXP (-Z)
    CSK (1) = CSK (0) * (1.0D+00+1.0D+00/Z)
    DO K = 2, NM
        IF (ABS(CSI(K-2)) < ABS(CSI(K-1))) THEN
            CSK (K) = (0.5D+00*PI/(Z*Z)-CSI(K)*CSK(K-1)) / CSI (K-1)
        ELSE
            CSK (K) = (CSI(K)*CSK(K-2)+(K-0.5D+00)*PI/Z**3) / CSI (K-2)
        END IF
    END DO

    CDK (0) = - CSK (1)
    DO K = 1, NM
        CDK (K) = - CSK (K-1) - (K+1.0D+00) * CSK (K) / Z
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CSPHJY (N, Z, NM, CSJ, CDJ, CSY, CDY)

!******************************************************************************
!
!! CSPHJY: spherical Bessel functions jn(z) and yn(z) for complex argument.
!
!  Discussion:
!
!    This procedure computes spherical Bessel functions jn(z) and yn(z)
!    and their derivatives for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of jn(z) and yn(z).
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, complex ( kind = ck ) CSJ(0:N0, CDJ(0:N), CSY(0:N), CDY(0:N),
!    the values of jn(z), jn'(z), yn(z), yn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CSJ (0:N)
    COMPLEX (KIND=CK) CDJ (0:N)
    COMPLEX (KIND=CK) CSY (0:N)
    COMPLEX (KIND=CK) CDY (0:N)
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CS
    COMPLEX (KIND=CK) CSA
    COMPLEX (KIND=CK) CSB
    INTEGER K
    INTEGER M
    INTEGER NM
    COMPLEX (KIND=CK) Z

    A0 = ABS (Z)
    NM = N

    IF (A0 < 1.0D-60) THEN
        DO K = 0, N
            CSJ (K) = 0.0D+00
            CDJ (K) = 0.0D+00
            CSY (K) = - 1.0D+300
            CDY (K) = 1.0D+300
        END DO
        CSJ (0) = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CDJ (1) = CMPLX (0.333333333333333D+00, 0.0D+00, KIND=CK)
        RETURN
    END IF

    CSJ (0) = SIN (Z) / Z
    CSJ (1) = (CSJ(0)-COS(Z)) / Z

    IF (2 <= N) THEN
        CSA = CSJ (0)
        CSB = CSJ (1)
        M = MSTA1 (A0, 200)
        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (A0, N, 15)
        END IF
        CF0 = 0.0D+00
        CF1 = 1.0D+00 - 100
        DO K = M, 0, - 1
            CF = (2.0D+00*K+3.0D+00) * CF1 / Z - CF0
            IF (K <= NM) THEN
                CSJ (K) = CF
            END IF
            CF0 = CF1
            CF1 = CF
        END DO

        IF (ABS(CSA) <= ABS(CSB)) THEN
            CS = CSB / CF0
        ELSE
            CS = CSA / CF
        END IF

        DO K = 0, NM
            CSJ (K) = CS * CSJ (K)
        END DO

    END IF

    CDJ (0) = (COS(Z)-SIN(Z)/Z) / Z
    DO K = 1, NM
        CDJ (K) = CSJ (K-1) - (K+1.0D+00) * CSJ (K) / Z
    END DO
    CSY (0) = - COS (Z) / Z
    CSY (1) = (CSY(0)-SIN(Z)) / Z
    CDY (0) = (SIN(Z)+COS(Z)/Z) / Z
    CDY (1) = (2.0D+00*CDY(0)-COS(Z)) / Z

    DO K = 2, NM
        IF (ABS(CSJ(K-2)) < ABS(CSJ(K-1))) THEN
            CSY (K) = (CSJ(K)*CSY(K-1)-1.0D+00/(Z*Z)) / CSJ (K-1)
        ELSE
            CSY (K) = (CSJ(K)*CSY(K-2)-(2.0D+00*K-1.0D+00)/Z**3) / CSJ (K-2)
        END IF
    END DO

    DO K = 2, NM
        CDY (K) = CSY (K-1) - (K+1.0D+00) * CSY (K) / Z
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE RCTJ (N, X, NM, RJ, DJ)

!******************************************************************************
!
!! RCTJ computes Riccati-Bessel function of the first kind, and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of jn(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) RJ(0:N), the values of x jn(x).
!
!    Output, real ( kind = rk ) DJ(0:N), the values of [x jn(x)]'.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) CS
    REAL (KIND=RK) DJ (0:N)
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER K
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) RJ (0:N)
    REAL (KIND=RK) RJ0
    REAL (KIND=RK) RJ1
    REAL (KIND=RK) X

    NM = N

    IF (ABS(X) < 1.0D-100) THEN
        DO K = 0, N
            RJ (K) = 0.0D+00
            DJ (K) = 0.0D+00
        END DO
        DJ (0) = 1.0D+00
        RETURN
    END IF

    RJ (0) = SIN (X)
    RJ (1) = RJ (0) / X - COS (X)
    RJ0 = RJ (0)
    RJ1 = RJ (1)

    IF (2 <= N) THEN

        M = MSTA1 (X, 200)

        IF (M < N) THEN
            NM = M
        ELSE
            M = MSTA2 (X, N, 15)
        END IF

        F0 = 0.0D+00
        F1 = 1.0D-100
        DO K = M, 0, - 1
            F = (2.0D+00*K+3.0D+00) * F1 / X - F0
            IF (K <= NM) THEN
                RJ (K) = F
            END IF
            F0 = F1
            F1 = F
        END DO

        IF (ABS(RJ1) < ABS(RJ0)) THEN
            CS = RJ0 / F
        ELSE
            CS = RJ1 / F0
        END IF

        DO K = 0, NM
            RJ (K) = CS * RJ (K)
        END DO

    END IF

    DJ (0) = COS (X)
    DO K = 1, NM
        DJ (K) = - K * RJ (K) / X + RJ (K-1)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE RCTY (N, X, NM, RY, DY)

!******************************************************************************
!
!! RCTY computes Riccati-Bessel function of the second kind, and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order of yn(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, integer NM, the highest order computed.
!
!    Output, real ( kind = rk ) RY(0:N), the values of x yn(x).
!
!    Output, real ( kind = rk ) DY(0:N), the values of [x yn(x)]'.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) DY (0:N)
    INTEGER K
    INTEGER NM
    REAL (KIND=RK) RF0
    REAL (KIND=RK) RF1
    REAL (KIND=RK) RF2
    REAL (KIND=RK) RY (0:N)
    REAL (KIND=RK) X

    NM = N

    IF (X < 1.0D-60) THEN
        DO K = 0, N
            RY (K) = - 1.0D+300
            DY (K) = 1.0D+300
        END DO
        RY (0) = - 1.0D+00
        DY (0) = 0.0D+00
        RETURN
    END IF

    RY (0) = - COS (X)
    RY (1) = RY (0) / X - SIN (X)
    RF0 = RY (0)
    RF1 = RY (1)
    DO K = 2, N
        RF2 = (2.0D+00*K-1.0D+00) * RF1 / X - RF0
        IF (1.0D+300 < ABS(RF2)) THEN
            EXIT
        END IF
        RY (K) = RF2
        RF0 = RF1
        RF1 = RF2
    END DO

    NM = K - 1
    DY (0) = SIN (X)
    DO K = 1, NM
        DY (K) = - K * RY (K) / X + RY (K-1)
    END DO

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   09: KELVIN FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE KLVNA (X, BER, BEI, GER, GEI, DER, DEI, HER, HEI)

!******************************************************************************
!
!! KLVNA: Kelvin functions ber(x), bei(x), ker(x), and kei(x), and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BER, BEI, GER, GEI, DER, DEI, HER, HEI,
!    the values of ber x, bei x, ker x, kei x, ber'x, bei'x, ker'x, kei'x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BEI
    REAL (KIND=RK) BER
    REAL (KIND=RK) CN0
    REAL (KIND=RK) CP0
    REAL (KIND=RK) CS
    REAL (KIND=RK) DEI
    REAL (KIND=RK) DER
    REAL (KIND=RK) EL
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FAC
    REAL (KIND=RK) GEI
    REAL (KIND=RK) GER
    REAL (KIND=RK) GS
    REAL (KIND=RK) HEI
    REAL (KIND=RK) HER
    INTEGER K
    INTEGER KM
    INTEGER M
    REAL (KIND=RK) PI
    REAL (KIND=RK) PN0
    REAL (KIND=RK) PN1
    REAL (KIND=RK) PP0
    REAL (KIND=RK) PP1
    REAL (KIND=RK) QN0
    REAL (KIND=RK) QN1
    REAL (KIND=RK) QP0
    REAL (KIND=RK) QP1
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) RC
    REAL (KIND=RK) RS
    REAL (KIND=RK) SN0
    REAL (KIND=RK) SP0
    REAL (KIND=RK) SS
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) X4
    REAL (KIND=RK) XC1
    REAL (KIND=RK) XC2
    REAL (KIND=RK) XD
    REAL (KIND=RK) XE1
    REAL (KIND=RK) XE2
    REAL (KIND=RK) XT

    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00
    EPS = 1.0D-15

    IF (X == 0.0D+00) THEN
        BER = 1.0D+00
        BEI = 0.0D+00
        GER = 1.0D+300
        GEI = - 0.25D+00 * PI
        DER = 0.0D+00
        DEI = 0.0D+00
        HER = - 1.0D+300
        HEI = 0.0D+00
        RETURN
    END IF

    X2 = 0.25D+00 * X * X
    X4 = X2 * X2

    IF (ABS(X) < 10.0D+00) THEN

        BER = 1.0D+00
        R = 1.0D+00
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2.0D+00*M-1.0D+00) ** 2 * X4
            BER = BER + R
            IF (ABS(R) < ABS(BER)*EPS) THEN
                EXIT
            END IF
        END DO

        BEI = X2
        R = X2
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2.0D+00*M+1.0D+00) ** 2 * X4
            BEI = BEI + R
            IF (ABS(R) < ABS(BEI)*EPS) THEN
                EXIT
            END IF
        END DO

        GER = - (LOG(X/2.0D+00)+EL) * BER + 0.25D+00 * PI * BEI
        R = 1.0D+00
        GS = 0.0D+00
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2.0D+00*M-1.0D+00) ** 2 * X4
            GS = GS + 1.0D+00 / (2.0D+00*M-1.0D+00) + 1.0D+00 / (2.0D+00*M)
            GER = GER + R * GS
            IF (ABS(R*GS) < ABS(GER)*EPS) THEN
                EXIT
            END IF
        END DO

        GEI = X2 - (LOG(X/2.0D+00)+EL) * BEI - 0.25D+00 * PI * BER
        R = X2
        GS = 1.0D+00
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2.0D+00*M+1.0D+00) ** 2 * X4
            GS = GS + 1.0D+00 / (2.0D+00*M) + 1.0D+00 / (2.0D+00*M+1.0D+00)
            GEI = GEI + R * GS
            IF (ABS(R*GS) < ABS(GEI)*EPS) THEN
                EXIT
            END IF
        END DO

        DER = - 0.25D+00 * X * X2
        R = DER
        DO M = 1, 60
            R = - 0.25D+00 * R / M / (M+1.0D+00) / (2.0D+00*M+1.0D+00) ** 2 * X4
            DER = DER + R
            IF (ABS(R) < ABS(DER)*EPS) THEN
                EXIT
            END IF
        END DO

        DEI = 0.5D+00 * X
        R = DEI
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2.0D+00*M-1.0D+00) / (2.0D+00*M+1.0D+00) * X4
            DEI = DEI + R
            IF (ABS(R) < ABS(DEI)*EPS) THEN
                EXIT
            END IF
        END DO

        R = - 0.25D+00 * X * X2
        GS = 1.5D+00
        HER = 1.5D+00 * R - BER / X - (LOG(X/2.0D+00)+EL) * DER + 0.25D+00 * PI * DEI
        DO M = 1, 60
            R = - 0.25D+00 * R / M / (M+1.0D+00) / (2.0D+00*M+1.0D+00) ** 2 * X4
            GS = GS + 1.0D+00 / (2*M+1.0D+00) + 1.0D+00 / (2*M+2.0D+00)
            HER = HER + R * GS
            IF (ABS(R*GS) < ABS(HER)*EPS) THEN
                EXIT
            END IF
        END DO

        R = 0.5D+00 * X
        GS = 1.0D+00
        HEI = 0.5D+00 * X - BEI / X - (LOG(X/2.0D+00)+EL) * DEI - 0.25D+00 * PI * DER
        DO M = 1, 60
            R = - 0.25D+00 * R / (M*M) / (2*M-1.0D+00) / (2*M+1.0D+00) * X4
            GS = GS + 1.0D+00 / (2.0D+00*M) + 1.0D+00 / (2*M+1.0D+00)
            HEI = HEI + R * GS
            IF (ABS(R*GS) < ABS(HEI)*EPS) THEN
                RETURN
            END IF
        END DO

    ELSE

        PP0 = 1.0D+00
        PN0 = 1.0D+00
        QP0 = 0.0D+00
        QN0 = 0.0D+00
        R0 = 1.0D+00

        IF (ABS(X) < 40.0D+00) THEN
            KM = 18
        ELSE
            KM = 10
        END IF

        FAC = 1.0D+00
        DO K = 1, KM
            FAC = - FAC
            XT = 0.25D+00 * K * PI - INT (0.125D+00*K) * 2.0D+00 * PI
            CS = COS (XT)
            SS = SIN (XT)
            R0 = 0.125D+00 * R0 * (2.0D+00*K-1.0D+00) ** 2 / K / X
            RC = R0 * CS
            RS = R0 * SS
            PP0 = PP0 + RC
            PN0 = PN0 + FAC * RC
            QP0 = QP0 + RS
            QN0 = QN0 + FAC * RS
        END DO

        XD = X / SQRT (2.0D+00)
        XE1 = EXP (XD)
        XE2 = EXP (-XD)
        XC1 = 1.0D+00 / SQRT (2.0D+00*PI*X)
        XC2 = SQRT (0.5D+00*PI/X)
        CP0 = COS (XD+0.125D+00*PI)
        CN0 = COS (XD-0.125D+00*PI)
        SP0 = SIN (XD+0.125D+00*PI)
        SN0 = SIN (XD-0.125D+00*PI)
        GER = XC2 * XE2 * (PN0*CP0-QN0*SP0)
        GEI = XC2 * XE2 * (-PN0*SP0-QN0*CP0)
        BER = XC1 * XE1 * (PP0*CN0+QP0*SN0) - GEI / PI
        BEI = XC1 * XE1 * (PP0*SN0-QP0*CN0) + GER / PI
        PP1 = 1.0D+00
        PN1 = 1.0D+00
        QP1 = 0.0D+00
        QN1 = 0.0D+00
        R1 = 1.0D+00
        FAC = 1.0D+00

        DO K = 1, KM
            FAC = - FAC
            XT = 0.25D+00 * K * PI - INT (0.125D+00*K) * 2.0D+00 * PI
            CS = COS (XT)
            SS = SIN (XT)
            R1 = 0.125D+00 * R1 * (4.0D+00-(2.0D+00*K-1.0D+00)**2) / K / X
            RC = R1 * CS
            RS = R1 * SS
            PP1 = PP1 + FAC * RC
            PN1 = PN1 + RC
            QP1 = QP1 + FAC * RS
            QN1 = QN1 + RS
        END DO

        HER = XC2 * XE2 * (-PN1*CN0+QN1*SN0)
        HEI = XC2 * XE2 * (PN1*SN0+QN1*CN0)
        DER = XC1 * XE1 * (PP1*CP0+QP1*SP0) - HEI / PI
        DEI = XC1 * XE1 * (PP1*SP0-QP1*CP0) + HER / PI

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE KLVNB (X, BER, BEI, GER, GEI, DER, DEI, HER, HEI)

!******************************************************************************
!
!! KLVNB: Kelvin functions ber(x), bei(x), ker(x), and kei(x), and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) BER, BEI, GER, GEI, DER, DEI, HER, HEI,
!    the values of ber x, bei x, ker x, kei x, ber'x, bei'x, ker'x, kei'x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BEI
    REAL (KIND=RK) BER
    REAL (KIND=RK) CSN
    REAL (KIND=RK) CSP
    REAL (KIND=RK) DEI
    REAL (KIND=RK) DER
    REAL (KIND=RK) FXI
    REAL (KIND=RK) FXR
    REAL (KIND=RK) GEI
    REAL (KIND=RK) GER
    REAL (KIND=RK) HEI
    REAL (KIND=RK) HER
    INTEGER L
    REAL (KIND=RK) PI
    REAL (KIND=RK) PNI
    REAL (KIND=RK) PNR
    REAL (KIND=RK) PPI
    REAL (KIND=RK) PPR
    REAL (KIND=RK) SSN
    REAL (KIND=RK) SSP
    REAL (KIND=RK) T
    REAL (KIND=RK) T2
    REAL (KIND=RK) TNI
    REAL (KIND=RK) TNR
    REAL (KIND=RK) TPI
    REAL (KIND=RK) TPR
    REAL (KIND=RK) U
    REAL (KIND=RK) V
    REAL (KIND=RK) X
    REAL (KIND=RK) YC1
    REAL (KIND=RK) YC2
    REAL (KIND=RK) YE1
    REAL (KIND=RK) YE2
    REAL (KIND=RK) YD

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN

        BER = 1.0D+00
        BEI = 0.0D+00
        GER = 1.0D+300
        GEI = - 0.25D+00 * PI
        DER = 0.0D+00
        DEI = 0.0D+00
        HER = - 1.0D+300
        HEI = 0.0D+00

    ELSE IF (X < 8.0D+00) THEN

        T = X / 8.0D+00
        T2 = T * T
        U = T2 * T2

        BER = ((((((-0.901D-05*U+0.122552D-02)*U-0.08349609D+00)*U+2.64191397D+00)*U-&
       & 32.36345652D+00)*U+113.77777774D+00)*U-64.0D+00) * U + 1.0D+00

        BEI = T * T * ((((((0.11346D-03*U-0.01103667D+00)*U+0.52185615D+00)*U-10.56765779D+00)&
       & *U+72.81777742D+00)*U-113.77777774D+00)*U+16.0D+00)

        GER = ((((((-0.2458D-04*U+0.309699D-02)*U-0.19636347D+00)*U+5.65539121D+00)*U-&
       & 60.60977451D+00)*U+171.36272133D+00)*U-59.05819744D+00) * U - 0.57721566D+00

        GER = GER - LOG (0.5D+00*X) * BER + 0.25D+00 * PI * BEI

        GEI = T2 * ((((((0.29532D-03*U-0.02695875D+00)*U+1.17509064D+00)*U-21.30060904D+00)*U+&
       & 124.2356965D+00)*U-142.91827687D+00)*U+6.76454936D+00)

        GEI = GEI - LOG (0.5D+00*X) * BEI - 0.25D+00 * PI * BER

        DER = X * T2 * ((((((-0.394D-05*U+0.45957D-03)*U-0.02609253D+00)*U+0.66047849D+00)*U-&
       & 6.0681481D+00)*U+14.22222222D+00)*U-4.0D+00)

        DEI = X * ((((((0.4609D-04*U-0.379386D-02)*U+0.14677204D+00)*U-2.31167514D+00)*U+&
       & 11.37777772D+00)*U-10.66666666D+00)*U+0.5D+00)

        HER = X * T2 * ((((((-0.1075D-04*U+0.116137D-02)*U-0.06136358D+00)*U+1.4138478D+00)*U-&
       & 11.36433272D+00)*U+21.42034017D+00)*U-3.69113734D+00)

        HER = HER - LOG (0.5D+00*X) * DER - BER / X + 0.25D+00 * PI * DEI

        HEI = X * ((((((0.11997D-03*U-0.926707D-02)*U+0.33049424D+00)*U-4.65950823D+00)*U+&
       & 19.41182758D+00)*U-13.39858846D+00)*U+0.21139217D+00)

        HEI = HEI - LOG (0.5D+00*X) * DEI - BEI / X - 0.25D+00 * PI * DER

    ELSE

        T = 8.0D+00 / X

        DO L = 1, 2

            V = (-1.0D+00) ** L * T

            TPR = ((((0.6D-06*V-0.34D-05)*V-0.252D-04)*V-0.906D-04)*V*V+0.0110486D+00) * V

            TPI = ((((0.19D-05*V+0.51D-05)*V*V-0.901D-04)*V-0.9765D-03)*V-0.0110485D+00) * V - &
           & 0.3926991D+00

            IF (L == 1) THEN
                TNR = TPR
                TNI = TPI
            END IF

        END DO

        YD = X / SQRT (2.0D+00)
        YE1 = EXP (YD+TPR)
        YE2 = EXP (-YD+TNR)
        YC1 = 1.0D+00 / SQRT (2.0D+00*PI*X)
        YC2 = SQRT (PI/(2.0D+00*X))
        CSP = COS (YD+TPI)
        SSP = SIN (YD+TPI)
        CSN = COS (-YD+TNI)
        SSN = SIN (-YD+TNI)
        GER = YC2 * YE2 * CSN
        GEI = YC2 * YE2 * SSN
        FXR = YC1 * YE1 * CSP
        FXI = YC1 * YE1 * SSP
        BER = FXR - GEI / PI
        BEI = FXI + GER / PI

        DO L = 1, 2

            V = (-1.0D+00) ** L * T

            PPR = &
           & (((((0.16D-05*V+0.117D-04)*V+0.346D-04)*V+0.5D-06)*V-0.13813D-02)*V-0.0625001D+00) &
           & * V + 0.7071068D+00

            PPI = &
           & (((((-0.32D-05*V-0.24D-05)*V+0.338D-04)*V+0.2452D-03)*V+0.13811D-02)*V-0.1D-06) * &
           & V + 0.7071068D+00

            IF (L == 1) THEN
                PNR = PPR
                PNI = PPI
            END IF

        END DO

        HER = GEI * PNI - GER * PNR
        HEI = - (GEI*PNR+GER*PNI)
        DER = FXR * PPR - FXI * PPI - HEI / PI
        DEI = FXI * PPR + FXR * PPI + HER / PI

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE KLVNZO (NT, KD, ZO)

!******************************************************************************
!
!! KLVNZO computes zeros of the Kelvin functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer NT, the number of zeros.
!
!    Input, integer KD, the function code.
!    1 for ber x,
!    2 for bei x,
!    3 for ker x,
!    4 for kei x,
!    5 for ber' x,
!    6 for bei' x,
!    7 for ker' x,
!    8 for kei' x.
!
!    Output, real ( kind = rk ) ZO(NT), the zeros of the given Kelvin function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    REAL (KIND=RK) BEI
    REAL (KIND=RK) BER
    REAL (KIND=RK) DDI
    REAL (KIND=RK) DDR
    REAL (KIND=RK) DEI
    REAL (KIND=RK) DER
    REAL (KIND=RK) GDI
    REAL (KIND=RK) GDR
    REAL (KIND=RK) GEI
    REAL (KIND=RK) GER
    REAL (KIND=RK) HEI
    REAL (KIND=RK) HER
    INTEGER KD
    INTEGER M
    REAL (KIND=RK) RT
    REAL (KIND=RK) RT0 (8)
    REAL (KIND=RK) ZO (NT)

    RT0 (1) = 2.84891D+00
    RT0 (2) = 5.02622D+00
    RT0 (3) = 1.71854D+00
    RT0 (4) = 3.91467D+00
    RT0 (5) = 6.03871D+00
    RT0 (6) = 3.77268D+00
    RT0 (7) = 2.66584D+00
    RT0 (8) = 4.93181D+00

    RT = RT0 (KD)

    DO M = 1, NT

        DO

            CALL KLVNA (RT, BER, BEI, GER, GEI, DER, DEI, HER, HEI)

            IF (KD == 1) THEN
                RT = RT - BER / DER
            ELSE IF (KD == 2) THEN
                RT = RT - BEI / DEI
            ELSE IF (KD == 3) THEN
                RT = RT - GER / HER
            ELSE IF (KD == 4) THEN
                RT = RT - GEI / HEI
            ELSE IF (KD == 5) THEN
                DDR = - BEI - DER / RT
                RT = RT - DER / DDR
            ELSE IF (KD == 6) THEN
                DDI = BER - DEI / RT
                RT = RT - DEI / DDI
            ELSE IF (KD == 7) THEN
                GDR = - GEI - HER / RT
                RT = RT - HER / GDR
            ELSE
                GDI = GER - HEI / RT
                RT = RT - HEI / GDI
            END IF

            IF (ABS(RT-RT0(KD)) <= 5.0D-10) THEN
                EXIT
            END IF

            RT0 (KD) = RT

        END DO

        ZO (M) = RT
        RT = RT + 4.44D+00

    END DO

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   10: AIRY FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE AIRYA (X, AI, BI, AD, BD)

!******************************************************************************
!
!! airya() computes Airy functions and their derivatives.
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program that the copyright
!    is acknowledged.
!
!  Modified:
!
!    30 June 2012
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of the Airy function.
!
!    Output, real ( kind = rk ) AI, BI, AD, BD, the values of Ai(x), Bi(x),
!    Ai'(x), Bi'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) AD
    REAL (KIND=RK) AI
    REAL (KIND=RK) BD
    REAL (KIND=RK) BI
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) PIR
    REAL (KIND=RK) SR3
    REAL (KIND=RK) VI1
    REAL (KIND=RK) VI2
    REAL (KIND=RK) VJ1
    REAL (KIND=RK) VJ2
    REAL (KIND=RK) VK1
    REAL (KIND=RK) VK2
    REAL (KIND=RK) VY1
    REAL (KIND=RK) VY2
    REAL (KIND=RK) X
    REAL (KIND=RK) XA
    REAL (KIND=RK) XQ
    REAL (KIND=RK) Z

    XA = ABS (X)
    PIR = 0.318309886183891D+00
    C1 = 0.355028053887817D+00
    C2 = 0.258819403792807D+00
    SR3 = 1.732050807568877D+00
    Z = XA ** 1.5D+00 / 1.5D+00
    XQ = SQRT (XA)

    CALL AJYIK (Z, VJ1, VJ2, VY1, VY2, VI1, VI2, VK1, VK2)

    IF (X == 0.0D+00) THEN
        AI = C1
        BI = SR3 * C1
        AD = - C2
        BD = SR3 * C2
    ELSE IF (0.0D+00 < X) THEN
        AI = PIR * XQ / SR3 * VK1
        BI = XQ * (PIR*VK1+2.0D+00/SR3*VI1)
        AD = - XA / SR3 * PIR * VK2
        BD = XA * (PIR*VK2+2.0D+00/SR3*VI2)
    ELSE
        AI = 0.5D+00 * XQ * (VJ1-VY1/SR3)
        BI = - 0.5D+00 * XQ * (VJ1/SR3+VY1)
        AD = 0.5D+00 * XA * (VJ2+VY2/SR3)
        BD = 0.5D+00 * XA * (VJ2/SR3-VY2)
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE AIRYB (X, AI, BI, AD, BD)

!******************************************************************************
!
!! airyb() computes Airy functions and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 June 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, argument of Airy function.
!
!    Output, real ( kind = rk ) AI, Ai(x).
!
!    Output, real ( kind = rk ) BI, Bi(x).
!
!    Output, real ( kind = rk ) AD, Ai'(x).
!
!    Output, real ( kind = rk ) BD, Bi'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) AD
    REAL (KIND=RK) AI
    REAL (KIND=RK) BD
    REAL (KIND=RK) BI
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) CK (41)
    REAL (KIND=RK) DF
    REAL (KIND=RK) DG
    REAL (KIND=RK) DK (41)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FX
    REAL (KIND=RK) GX
    INTEGER K
    INTEGER KM
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) RP
    REAL (KIND=RK) SAD
    REAL (KIND=RK) SAI
    REAL (KIND=RK) SBD
    REAL (KIND=RK) SBI
    REAL (KIND=RK) SDA
    REAL (KIND=RK) SDB
    REAL (KIND=RK) SR3
    REAL (KIND=RK) SSA
    REAL (KIND=RK) SSB
    REAL (KIND=RK) X
    REAL (KIND=RK) XA
    REAL (KIND=RK) XAR
    REAL (KIND=RK) XCS
    REAL (KIND=RK) XE
    REAL (KIND=RK) XF
    REAL (KIND=RK) XM
    REAL (KIND=RK) XP1
    REAL (KIND=RK) XQ
    REAL (KIND=RK) XR1
    REAL (KIND=RK) XR2
    REAL (KIND=RK) XSS

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    C1 = 0.355028053887817D+00
    C2 = 0.258819403792807D+00
    SR3 = 1.732050807568877D+00
    XA = ABS (X)
    XQ = SQRT (XA)

    IF (X <= 0.0D+00) THEN
        XM = 8.0D+00
    ELSE
        XM = 5.0D+00
    END IF

    IF (X == 0.0D+00) THEN
        AI = C1
        BI = SR3 * C1
        AD = - C2
        BD = SR3 * C2
        RETURN
    END IF

    IF (XA <= XM) THEN

        FX = 1.0D+00
        R = 1.0D+00
        DO K = 1, 40
            R = R * X / (3.0D+00*K) * X / (3.0D+00*K-1.0D+00) * X
            FX = FX + R
            IF (ABS(R) < ABS(FX)*EPS) THEN
                EXIT
            END IF
        END DO

        GX = X
        R = X
        DO K = 1, 40
            R = R * X / (3.0D+00*K) * X / (3.0D+00*K+1.0D+00) * X
            GX = GX + R
            IF (ABS(R) < ABS(GX)*EPS) THEN
                EXIT
            END IF
        END DO

        AI = C1 * FX - C2 * GX
        BI = SR3 * (C1*FX+C2*GX)
        DF = 0.5D+00 * X * X
        R = DF
        DO K = 1, 40
            R = R * X / (3.0D+00*K) * X / (3.0D+00*K+2.0D+00) * X
            DF = DF + R
            IF (ABS(R) < ABS(DF)*EPS) THEN
                EXIT
            END IF
        END DO

        DG = 1.0D+00
        R = 1.0D+00
        DO K = 1, 40
            R = R * X / (3.0D+00*K) * X / (3.0D+00*K-2.0D+00) * X
            DG = DG + R
            IF (ABS(R) < ABS(DG)*EPS) THEN
                EXIT
            END IF
        END DO

        AD = C1 * DF - C2 * DG
        BD = SR3 * (C1*DF+C2*DG)

    ELSE

        XE = XA * XQ / 1.5D+00
        XR1 = 1.0D+00 / XE
        XAR = 1.0D+00 / XQ
        XF = SQRT (XAR)
        RP = 0.5641895835477563D+00
        R = 1.0D+00
        DO K = 1, 40
            R = R * (6.0D+00*K-1.0D+00) / 216.0D+00 * (6.0D+00*K-3.0D+00) / K * &
           & (6.0D+00*K-5.0D+00) / (2.0D+00*K-1.0D+00)
            CK (K) = R
            DK (K) = - (6.0D+00*K+1.0D+00) / (6.0D+00*K-1.0D+00) * CK (K)
        END DO

        KM = INT (24.5D+00-XA)

        IF (XA < 6.0D+00) THEN
            KM = 14
        END IF

        IF (15.0D+00 < XA) THEN
            KM = 10
        END IF

        IF (0.0D+00 < X) THEN
            SAI = 1.0D+00
            SAD = 1.0D+00
            R = 1.0D+00
            DO K = 1, KM
                R = - R * XR1
                SAI = SAI + CK (K) * R
                SAD = SAD + DK (K) * R
            END DO
            SBI = 1.0D+00
            SBD = 1.0D+00
            R = 1.0D+00
            DO K = 1, KM
                R = R * XR1
                SBI = SBI + CK (K) * R
                SBD = SBD + DK (K) * R
            END DO
            XP1 = EXP (-XE)
            AI = 0.5D+00 * RP * XF * XP1 * SAI
            BI = RP * XF / XP1 * SBI
            AD = - 0.5D+00 * RP / XF * XP1 * SAD
            BD = RP / XF / XP1 * SBD
        ELSE
            XCS = COS (XE+PI/4.0D+00)
            XSS = SIN (XE+PI/4.0D+00)
            SSA = 1.0D+00
            SDA = 1.0D+00
            R = 1.0D+00
            XR2 = 1.0D+00 / (XE*XE)
            DO K = 1, KM
                R = - R * XR2
                SSA = SSA + CK (2*K) * R
                SDA = SDA + DK (2*K) * R
            END DO
            SSB = CK (1) * XR1
            SDB = DK (1) * XR1
            R = XR1
            DO K = 1, KM
                R = - R * XR2
                SSB = SSB + CK (2*K+1) * R
                SDB = SDB + DK (2*K+1) * R
            END DO
            AI = RP * XF * (XSS*SSA-XCS*SSB)
            BI = RP * XF * (XCS*SSA+XSS*SSB)
            AD = - RP / XF * (XCS*SDA+XSS*SDB)
            BD = RP / XF * (XSS*SDA-XCS*SDB)
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE AIRYZO (NT, KF, XA, XB, XC, XD)

!******************************************************************************
!
!! airyzo() computes the first NT zeros of Ai(x) and Ai'(x).
!
!   Discussion:
!
!    Compute the first NT zeros of Airy functions Ai(x) and Ai'(x),
!    a and a', and the associated values of Ai(a') and Ai'(a); and
!    the first NT zeros of Airy functions Bi(x) and Bi'(x), b and
!    b', and the associated values of Bi(b') and Bi'(b).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    14 March 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer NT, the number of zeros.
!
!    Input, integer KF, the function code.
!    1 for Ai(x) and Ai'(x);
!    2 for Bi(x) and Bi'(x).
!
!    Output, real ( kind = rk ) XA(m), a, the m-th zero of Ai(x) or
!    b, the m-th zero of Bi(x).
!
!    Output, real ( kind = rk ) XB(m), a', the m-th zero of Ai'(x) or
!    b', the m-th zero of Bi'(x).
!
!    Output, real ( kind = rk ) XC(m), Ai(a') or Bi(b').
!
!    Output, real ( kind = rk ) XD(m), Ai'(a) or Bi'(b)
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    REAL (KIND=RK) AD
    REAL (KIND=RK) AI
    REAL (KIND=RK) BD
    REAL (KIND=RK) BI
    INTEGER I
    INTEGER KF
    REAL (KIND=RK) PI
    REAL (KIND=RK) RT
    REAL (KIND=RK) RT0
    REAL (KIND=RK) U
    REAL (KIND=RK) U1
    REAL (KIND=RK) X
    REAL (KIND=RK) XA (NT)
    REAL (KIND=RK) XB (NT)
    REAL (KIND=RK) XC (NT)
    REAL (KIND=RK) XD (NT)

    PI = 3.141592653589793D+00

    DO I = 1, NT

        IF (KF == 1) THEN
            U = 3.0D+00 * PI * (4.0D+00*I-1) / 8.0D+00
            U1 = 1.0D+00 / (U*U)
            RT0 = - (U*U) ** (1.0/3.0) * &
           & ((((-15.5902D+00*U1+0.929844D+00)*U1-0.138889D+00)*U1+0.10416667D+00)*U1+1.0D+00)
        ELSE IF (KF == 2) THEN
            IF (I == 1) THEN
                RT0 = - 1.17371D+00
            ELSE
                U = 3.0D+00 * PI * (4.0D+00*I-3.0D+00) / 8.0D+00
                U1 = 1.0D+00 / (U*U)
                RT0 = - (U*U) ** (1.0D+00/3.0D+00) * ((((-15.5902D+00*U1+0.929844D+00)*U1-&
               & 0.138889D+00)*U1+0.10416667D+00)*U1+1.0D+00)
            END IF
        END IF

        DO

            X = RT0
            CALL AIRYB (X, AI, BI, AD, BD)

            IF (KF == 1) THEN
                RT = RT0 - AI / AD
            ELSE
                RT = RT0 - BI / BD
            END IF

            IF (ABS((RT-RT0)/RT) <= 1.0D-09) THEN
                EXIT
            END IF
            RT0 = RT

        END DO

        XA (I) = RT
        IF (KF == 1) THEN
            XD (I) = AD
        ELSE
            XD (I) = BD
        END IF

    END DO

    DO I = 1, NT

        IF (KF == 1) THEN
            IF (I == 1) THEN
                RT0 = - 1.01879D+00
            ELSE
                U = 3.0D+00 * PI * (4.0D+00*I-3.0D+00) / 8.0D+00
                U1 = 1.0D+00 / (U*U)
                RT0 = - (U*U) ** (1.0D+00/3.0D+00) * &
               & ((((15.0168D+00*U1-0.873954D+00)*U1+0.121528D+00)*U1-0.145833D+00)*U1+1.0D+00)
            END IF
        ELSE IF (KF == 2) THEN
            IF (I == 1) THEN
                RT0 = - 2.29444D+00
            ELSE
                U = 3.0D+00 * PI * (4.0D+00*I-1.0D+00) / 8.0D+00
                U1 = 1.0D+00 / (U*U)
                RT0 = - (U*U) ** (1.0D+00/3.0D+00) * &
               & ((((15.0168D+00*U1-0.873954D+00)*U1+0.121528D+00)*U1-0.145833D+00)*U1+1.0D+00)
            END IF
        END IF

        DO

            X = RT0
            CALL AIRYB (X, AI, BI, AD, BD)

            IF (KF == 1) THEN
                RT = RT0 - AD / (AI*X)
            ELSE
                RT = RT0 - BD / (BI*X)
            END IF

            IF (ABS((RT-RT0)/RT) <= 1.0D-09) THEN
                EXIT
            END IF

            RT0 = RT

        END DO

        XB (I) = RT
        IF (KF == 1) THEN
            XC (I) = AI
        ELSE
            XC (I) = BI
        END IF

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE ITAIRY (X, APT, BPT, ANT, BNT)

!****************************************************************************80
!
!! ITAIRY computes the integrals of Airy functions.
!
!  Discussion:
!
!    Compute the integrals of Airy functions with respect to t,
!    from 0 and x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    19 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) APT, BPT, ANT, BNT, the integrals, from 0 to x,
!    of Ai(t), Bi(t), Ai(-t), and Bi(-t).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK), SAVE, DIMENSION (16) :: A = (/ 0.569444444444444D+00, &
   & 0.891300154320988D+00, 0.226624344493027D+01, 0.798950124766861D+01, &
   & 0.360688546785343D+02, 0.198670292131169D+03, 0.129223456582211D+04, &
   & 0.969483869669600D+04, 0.824184704952483D+05, 0.783031092490225D+06, &
   & 0.822210493622814D+07, 0.945557399360556D+08, 0.118195595640730D+10, &
   & 0.159564653040121D+11, 0.231369166433050D+12, 0.358622522796969D+13 /)
    REAL (KIND=RK) ANT
    REAL (KIND=RK) APT
    REAL (KIND=RK) BNT
    REAL (KIND=RK) BPT
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FX
    REAL (KIND=RK) GX
    INTEGER K
    INTEGER L
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q1
    REAL (KIND=RK) Q2
    REAL (KIND=RK) R
    REAL (KIND=RK) SR3
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SU2
    REAL (KIND=RK) SU3
    REAL (KIND=RK) SU4
    REAL (KIND=RK) SU5
    REAL (KIND=RK) SU6
    REAL (KIND=RK) X
    REAL (KIND=RK) XE
    REAL (KIND=RK) XP6
    REAL (KIND=RK) XR1
    REAL (KIND=RK) XR2

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    C1 = 0.355028053887817D+00
    C2 = 0.258819403792807D+00
    SR3 = 1.732050807568877D+00

    IF (X == 0.0D+00) THEN

        APT = 0.0D+00
        BPT = 0.0D+00
        ANT = 0.0D+00
        BNT = 0.0D+00

    ELSE

        IF (ABS(X) <= 9.25D+00) THEN

            DO L = 0, 1

                X = (-1.0D+00) ** L * X
                FX = X
                R = X

                DO K = 1, 40
                    R = R * (3.0D+00*K-2.0D+00) / (3.0D+00*K+1.0D+00) * X / (3.0D+00*K) * X / &
                   & (3.0D+00*K-1.0D+00) * X
                    FX = FX + R
                    IF (ABS(R) < ABS(FX)*EPS) THEN
                        EXIT
                    END IF
                END DO

                GX = 0.5D+00 * X * X
                R = GX

                DO K = 1, 40
                    R = R * (3.0D+00*K-1.0D+00) / (3.0D+00*K+2.0D+00) * X / (3.0D+00*K) * X / &
                   & (3.0D+00*K+1.0D+00) * X
                    GX = GX + R
                    IF (ABS(R) < ABS(GX)*EPS) THEN
                        EXIT
                    END IF
                END DO

                ANT = C1 * FX - C2 * GX
                BNT = SR3 * (C1*FX+C2*GX)

                IF (L == 0) THEN
                    APT = ANT
                    BPT = BNT
                ELSE
                    ANT = - ANT
                    BNT = - BNT
                    X = - X
                END IF

            END DO

        ELSE

            Q2 = 1.414213562373095D+00
            Q0 = 0.3333333333333333D+00
            Q1 = 0.6666666666666667D+00
            XE = X * SQRT (X) / 1.5D+00
            XP6 = 1.0D+00 / SQRT (6.0D+00*PI*XE)
            SU1 = 1.0D+00
            R = 1.0D+00
            XR1 = 1.0D+00 / XE
            DO K = 1, 16
                R = - R * XR1
                SU1 = SU1 + A (K) * R
            END DO
            SU2 = 1.0D+00
            R = 1.0D+00
            DO K = 1, 16
                R = R * XR1
                SU2 = SU2 + A (K) * R
            END DO

            APT = Q0 - EXP (-XE) * XP6 * SU1
            BPT = 2.0D+00 * EXP (XE) * XP6 * SU2
            SU3 = 1.0D+00
            R = 1.0D+00
            XR2 = 1.0D+00 / (XE*XE)
            DO K = 1, 8
                R = - R * XR2
                SU3 = SU3 + A (2*K) * R
            END DO
            SU4 = A (1) * XR1
            R = XR1
            DO K = 1, 7
                R = - R * XR2
                SU4 = SU4 + A (2*K+1) * R
            END DO
            SU5 = SU3 + SU4
            SU6 = SU3 - SU4
            ANT = Q1 - Q2 * XP6 * (SU5*COS(XE)-SU6*SIN(XE))
            BNT = Q2 * XP6 * (SU5*SIN(XE)+SU6*COS(XE))

        END IF

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   11: STRUVE FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE STVH0 (X, SH0)

!******************************************************************************
!
!! STVH0 computes the Struve function H0(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) SH0, the value of H0(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) BY0
    INTEGER K
    INTEGER KM
    REAL (KIND=RK) P0
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q0
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SH0
    REAL (KIND=RK) T
    REAL (KIND=RK) T2
    REAL (KIND=RK) TA0
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    S = 1.0D+00
    R = 1.0D+00

    IF (X <= 20.0D+00) THEN
        A0 = 2.0D+00 * X / PI
        DO K = 1, 60
            R = - R * X / (2.0D+00*K+1.0D+00) * X / (2.0D+00*K+1.0D+00)
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        SH0 = A0 * S

    ELSE

        IF (X < 50.0D+00) THEN
            KM = INT (0.5D+00*(X+1.0D+00))
        ELSE
            KM = 25
        END IF

        DO K = 1, KM
            R = - R * ((2.0D+00*K-1.0D+00)/X) ** 2
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        T = 4.0D+00 / X
        T2 = T * T

        P0 = &
       & ((((-0.37043D-05*T2+0.173565D-04)*T2-0.487613D-04)*T2+0.17343D-03)*T2-0.1753062D-02) * &
       & T2 + 0.3989422793D+00

        Q0 = T * (((((0.32312D-05*T2-0.142078D-04)*T2+0.342468D-04)*T2-0.869791D-04)*T2+&
       & 0.4564324D-03)*T2-0.0124669441D+00)

        TA0 = X - 0.25D+00 * PI
        BY0 = 2.0D+00 / SQRT (X) * (P0*SIN(TA0)+Q0*COS(TA0))
        SH0 = 2.0D+00 / (PI*X) * S + BY0

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE STVH1 (X, SH1)

!******************************************************************************
!
!! STVH1 computes the Struve function H1(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) SH1, the value of H1(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) BY1
    INTEGER K
    INTEGER KM
    REAL (KIND=RK) P1
    REAL (KIND=RK) PI
    REAL (KIND=RK) Q1
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SH1
    REAL (KIND=RK) T
    REAL (KIND=RK) T2
    REAL (KIND=RK) TA1
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    R = 1.0D+00

    IF (X <= 20.0D+00) THEN

        S = 0.0D+00
        A0 = - 2.0D+00 / PI
        DO K = 1, 60
            R = - R * X * X / (4.0D+00*K*K-1.0D+00)
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        SH1 = A0 * S

    ELSE

        S = 1.0D+00

        IF (X <= 50.0D+00) THEN
            KM = INT (0.5D+00*X)
        ELSE
            KM = 25
        END IF

        DO K = 1, KM
            R = - R * (4.0D+00*K*K-1.0D+00) / (X*X)
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        T = 4.0D+00 / X
        T2 = T * T

        P1 = &
       & ((((0.42414D-05*T2-0.20092d-04)*T2+0.580759D-04)*T2-0.223203D-03)*T2+0.29218256D-02) * &
       & T2 + 0.3989422819D+00

        Q1 = T * (((((-0.36594D-05*T2+0.1622D-04)*T2-0.398708D-04)*T2+0.1064741D-03)*T2-&
       & 0.63904D-03)*T2+0.0374008364D+00)

        TA1 = X - 0.75D+00 * PI
        BY1 = 2.0D+00 / SQRT (X) * (P1*SIN(TA1)+Q1*COS(TA1))
        SH1 = 2.0D+00 / PI * (1.0D+00+S/(X*X)) + BY1

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE STVHV (V, X, HV)

!******************************************************************************
!
!! STVHV computes the Struve function Hv(x) with arbitrary order v.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    24 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of the function.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HV, the value of Hv(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BF
    REAL (KIND=RK) BF0
    REAL (KIND=RK) BF1
    REAL (KIND=RK) BY0
    REAL (KIND=RK) BY1
    REAL (KIND=RK) BYV
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    REAL (KIND=RK) HV
    INTEGER K
    INTEGER L
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) PU0
    REAL (KIND=RK) PU1
    REAL (KIND=RK) QU0
    REAL (KIND=RK) QU1
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) SA
    REAL (KIND=RK) SR
    REAL (KIND=RK) T0
    REAL (KIND=RK) T1
    REAL (KIND=RK) U
    REAL (KIND=RK) U0
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VA
    REAL (KIND=RK) VB
    REAL (KIND=RK) VT
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN
        IF (-1.0D+00 < V .OR. INT(V)-V == 0.5D+00) THEN
            HV = 0.0D+00
        ELSE IF (V <-1.0D+00) THEN
            HV = (-1.0D+00) ** (INT(0.5D+00-V)-1) * 1.0D+300
        ELSE IF (V ==-1.0D+00) THEN
            HV = 2.0D+00 / PI
        END IF
        RETURN
    END IF

    IF (X <= 20.0D+00) THEN

        V0 = V + 1.5D+00
        CALL GAMMA (V0, GA)
        S = 2.0D+00 / (SQRT(PI)*GA)
        R1 = 1.0D+00

        DO K = 1, 100
            VA = K + 1.5D+00
            CALL GAMMA (VA, GA)
            VB = V + K + 1.5D+00
            CALL GAMMA (VB, GB)
            R1 = - R1 * (0.5D+00*X) ** 2
            R2 = R1 / (GA*GB)
            S = S + R2
            IF (ABS(R2) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        HV = (0.5D+00*X) ** (V+1.0D+00) * S

    ELSE

        SA = (0.5D+00*X) ** (V-1.0D+00) / PI
        V0 = V + 0.5D+00
        CALL GAMMA (V0, GA)
        S = SQRT (PI) / GA
        R1 = 1.0D+00

        DO K = 1, 12
            VA = K + 0.5D+00
            CALL GAMMA (VA, GA)
            VB = - K + V + 0.5D+00
            CALL GAMMA (VB, GB)
            R1 = R1 / (0.5D+00*X) ** 2
            S = S + R1 * GA / GB
        END DO

        S0 = SA * S
        U = ABS (V)
        N = INT (U)
        U0 = U - N

        DO L = 0, 1

            VT = 4.0D+00 * (U0+L) ** 2
            R1 = 1.0D+00
            PU1 = 1.0D+00
            DO K = 1, 12
                R1 = - 0.0078125D+00 * R1 * (VT-(4.0D+00*K-3.0D+00)**2) * &
               & (VT-(4.0D+00*K-1.0D+00)**2) / ((2.0D+00*K-1.0D+00)*K*X*X)
                PU1 = PU1 + R1
            END DO

            QU1 = 1.0D+00
            R2 = 1.0D+00
            DO K = 1, 12
                R2 = - 0.0078125D+00 * R2 * (VT-(4.0D+00*K-1.0D+00)**2) * &
               & (VT-(4.0D+00*K+1.0D+00)**2) / ((2.0D+00*K+1.0D+00)*K*X*X)
                QU1 = QU1 + R2
            END DO
            QU1 = 0.125D+00 * (VT-1.0D+00) / X * QU1

            IF (L == 0) THEN
                PU0 = PU1
                QU0 = QU1
            END IF

        END DO

        T0 = X - (0.5D+00*U0+0.25D+00) * PI
        T1 = X - (0.5D+00*U0+0.75D+00) * PI
        SR = SQRT (2.0D+00/(PI*X))
        BY0 = SR * (PU0*SIN(T0)+QU0*COS(T0))
        BY1 = SR * (PU1*SIN(T1)+QU1*COS(T1))
        BF0 = BY0
        BF1 = BY1
        DO K = 2, N
            BF = 2.0D+00 * (K-1.0D+00+U0) / X * BF1 - BF0
            BF0 = BF1
            BF1 = BF
        END DO

        IF (N == 0) THEN
            BYV = BY0
        ELSE IF (N == 1) THEN
            BYV = BY1
        ELSE
            BYV = BF
        END IF
        HV = BYV + S0
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE STVL0 (X, SL0)

!******************************************************************************
!
!! STVL0 computes the modified Struve function L0(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) SL0, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) BI0
    INTEGER K
    INTEGER KM
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SL0
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    S = 1.0D+00
    R = 1.0D+00

    IF (X <= 20.0D+00) THEN

        A0 = 2.0D+00 * X / PI

        DO K = 1, 60
            R = R * (X/(2.0D+00*K+1.0D+00)) ** 2
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        SL0 = A0 * S

    ELSE

        IF (X < 50.0D+00) THEN
            KM = INT (0.5D+00*(X+1.0D+00))
        ELSE
            KM = 25
        END IF

        DO K = 1, KM
            R = R * ((2.0D+00*K-1.0D+00)/X) ** 2
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        A1 = EXP (X) / SQRT (2.0D+00*PI*X)
        R = 1.0D+00
        BI0 = 1.0D+00
        DO K = 1, 16
            R = 0.125D+00 * R * (2.0D+00*K-1.0D+00) ** 2 / (K*X)
            BI0 = BI0 + R
            IF (ABS(R/BI0) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        BI0 = A1 * BI0
        SL0 = - 2.0D+00 / (PI*X) * S + BI0

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE STVL1 (X, SL1)

!******************************************************************************
!
!! STVL1 computes the modified Struve function L1(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    05 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) SL1, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A1
    REAL (KIND=RK) BI1
    INTEGER K
    INTEGER KM
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SL1
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    R = 1.0D+00
    IF (X <= 20.0D+00) THEN
        S = 0.0D+00
        DO K = 1, 60
            R = R * X * X / (4.0D+00*K*K-1.0D+00)
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        SL1 = 2.0D+00 / PI * S

    ELSE

        S = 1.0D+00
        KM = INT (0.50D+00*X)
        KM = MIN (KM, 25)

        DO K = 1, KM
            R = R * (2.0D+00*K+3.0D+00) * (2.0D+00*K+1.0D+00) / (X*X)
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        SL1 = 2.0D+00 / PI * (-1.0D+00+1.0D+00/(X*X)+3.0D+00*S/X**4)
        A1 = EXP (X) / SQRT (2.0D+00*PI*X)
        R = 1.0D+00
        BI1 = 1.0D+00
        DO K = 1, 16
            R = - 0.125D+00 * R * (4.0D+00-(2.0D+00*K-1.0D+00)**2) / (K*X)
            BI1 = BI1 + R
            IF (ABS(R/BI1) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        SL1 = SL1 + A1 * BI1

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE STVLV (V, X, SLV)

!******************************************************************************
!
!! STVLV computes the modified Struve function Lv(x) with arbitary order.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    04 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order of Lv(x).
!
!    Input, real ( kind = rk ) X, the argument of Lv(x).
!
!    Output, real ( kind = rk ) SLV, the value of Lv(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BF
    REAL (KIND=RK) BF0
    REAL (KIND=RK) BF1
    REAL (KIND=RK) BIV
    REAL (KIND=RK) BIV0
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    INTEGER K
    INTEGER L
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) SA
    REAL (KIND=RK) SLV
    REAL (KIND=RK) U
    REAL (KIND=RK) U0
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) VA
    REAL (KIND=RK) VB
    REAL (KIND=RK) VT
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00

    IF (X == 0.0D+00) THEN

        IF (-1.0D+00 < V .OR. INT(V)-V == 0.5D+00) THEN
            SLV = 0.0D+00
        ELSE IF (V <-1.0D+00) THEN
            SLV = (-1) ** (INT(0.5D+00-V)-1) * 1.0D+300
        ELSE IF (V ==-1.0D+00) THEN
            SLV = 2.0D+00 / PI
        END IF

    ELSE IF (X <= 40.0D+00) THEN

        V0 = V + 1.5D+00
        CALL GAMMA (V0, GA)
        S = 2.0D+00 / (SQRT(PI)*GA)
        R1 = 1.0D+00
        DO K = 1, 100
            VA = K + 1.5D+00
            CALL GAMMA (VA, GA)
            VB = V + K + 1.5D+00
            CALL GAMMA (VB, GB)
            R1 = R1 * (0.5D+00*X) ** 2
            R2 = R1 / (GA*GB)
            S = S + R2
            IF (ABS(R2/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        SLV = (0.5D+00*X) ** (V+1.0D+00) * S

    ELSE

        SA = - 1.0D+00 / PI * (0.5D+00*X) ** (V-1.0D+00)
        V0 = V + 0.5D+00
        CALL GAMMA (V0, GA)
        S = - SQRT (PI) / GA
        R1 = - 1.0D+00
        DO K = 1, 12
            VA = K + 0.5D+00
            CALL GAMMA (VA, GA)
            VB = - K + V + 0.5D+00
            CALL GAMMA (VB, GB)
            R1 = - R1 / (0.5D+00*X) ** 2
            S = S + R1 * GA / GB
        END DO
        S0 = SA * S
        U = ABS (V)
        N = INT (U)
        U0 = U - N
        DO L = 0, 1
            VT = U0 + L
            R = 1.0D+00
            BIV = 1.0D+00
            DO K = 1, 16
                R = - 0.125D+00 * R * (4.0D+00*VT*VT-(2.0D+00*K-1.0D+00)**2) / (K*X)
                BIV = BIV + R
                IF (ABS(R/BIV) < 1.0D-12) THEN
                    EXIT
                END IF
            END DO

            IF (L == 0) THEN
                BIV0 = BIV
            END IF

        END DO

        BF0 = BIV0
        BF1 = BIV
        DO K = 2, N
            BF = - 2.0D+00 * (K-1.0D+00+U0) / X * BF1 + BF0
            BF0 = BF1
            BF1 = BF
        END DO

        IF (N == 0) THEN
            BIV = BIV0
        ELSE IF (1 < N) THEN
            BIV = BF
        END IF

        SLV = EXP (X) / SQRT (2.0D+00*PI*X) * BIV + S0

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITSH0 (X, TH0)

!******************************************************************************
!
!! ITSH0 integrates the Struve function H0(t) from 0 to x.
!
!  Discussion:
!
!    This procedure evaluates the integral of Struve function
!    H0(t) with respect to t from 0 and x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TH0, the integral of H0(t) from 0 to x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (25)
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) AF
    REAL (KIND=RK) BF
    REAL (KIND=RK) BG
    REAL (KIND=RK) EL
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) RD
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) TH0
    REAL (KIND=RK) TY
    REAL (KIND=RK) X
    REAL (KIND=RK) XP

    PI = 3.141592653589793D+00
    R = 1.0D+00

    IF (X <= 30.0D+00) THEN

        S = 0.5D+00

        DO K = 1, 100

            IF (K == 1) THEN
                RD = 0.5D+00
            ELSE
                RD = 1.0D+00
            END IF

            R = - R * RD * K / (K+1.0D+00) * (X/(2.0D+00*K+1.0D+00)) ** 2
            S = S + R

            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF

        END DO

        TH0 = 2.0D+00 / PI * X * X * S

    ELSE

        S = 1.0D+00
        DO K = 1, 12
            R = - R * K / (K+1.0D+00) * ((2.0D+00*K+1.0D+00)/X) ** 2
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        EL = 0.57721566490153D+00
        S0 = S / (PI*X*X) + 2.0D+00 / PI * (LOG(2.0D+00*X)+EL)
        A0 = 1.0D+00
        A1 = 5.0D+00 / 8.0D+00
        A (1) = A1
        DO K = 1, 20
            AF = ((1.5D+00*(K+0.5D+00)*(K+5.0D+00/6.0D+00)*A1-0.5D+00*(K+0.5D+00)*(K+0.5D+00)&
           & *(K-0.5D+00)*A0)) / (K+1.0D+00)
            A (K+1) = AF
            A0 = A1
            A1 = AF
        END DO

        BF = 1.0D+00
        R = 1.0D+00
        DO K = 1, 10
            R = - R / (X*X)
            BF = BF + A (2*K) * R
        END DO
        BG = A (1) / X
        R = 1.0D+00 / X
        DO K = 1, 10
            R = - R / (X*X)
            BG = BG + A (2*K+1) * R
        END DO
        XP = X + 0.25D+00 * PI
        TY = SQRT (2.0D+00/(PI*X)) * (BG*COS(XP)-BF*SIN(XP))
        TH0 = TY + S0

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITSL0 (X, TL0)

!******************************************************************************
!
!! ITSL0 integrates the Struve function L0(t) from 0 to x.
!
!  Discussion:
!
!    This procedure evaluates the integral of modified Struve function
!    L0(t) with respect to t from 0 to x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the upper limit of the integral.
!
!    Output, real ( kind = rk ) TL0, the integral of L0(t) from 0 to x.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (18)
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) AF
    REAL (KIND=RK) EL
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) RD
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) TI
    REAL (KIND=RK) TL0
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00
    R = 1.0D+00

    IF (X <= 20.0D+00) THEN

        S = 0.5D+00
        DO K = 1, 100

            IF (K == 1) THEN
                RD = 0.5D+00
            ELSE
                RD = 1.0D+00
            END IF
            R = R * RD * K / (K+1.0D+00) * (X/(2.0D+00*K+1.0D+00)) ** 2
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        TL0 = 2.0D+00 / PI * X * X * S

    ELSE

        S = 1.0D+00
        DO K = 1, 10
            R = R * K / (K+1.0D+00) * ((2.0D+00*K+1.0D+00)/X) ** 2
            S = S + R
            IF (ABS(R/S) < 1.0D-12) THEN
                EXIT
            END IF
        END DO

        EL = 0.57721566490153D+00
        S0 = - S / (PI*X*X) + 2.0D+00 / PI * (LOG(2.0D+00*X)+EL)
        A0 = 1.0D+00
        A1 = 5.0D+00 / 8.0D+00
        A (1) = A1
        DO K = 1, 10
            AF = ((1.5D+00*(K+0.50D+00)*(K+5.0D+00/6.0D+00)*A1-0.5D+00*(K+0.5D+00)**2*(K-&
           & 0.5D+00)*A0)) / (K+1.0D+00)
            A (K+1) = AF
            A0 = A1
            A1 = AF
        END DO

        TI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 11
            R = R / X
            TI = TI + A (K) * R
        END DO
        TL0 = TI / SQRT (2.0D+00*PI*X) * EXP (X) + S0

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ITTH0 (X, TTH)

!******************************************************************************
!
!! ITTH0 integrates H0(t)/t from x to oo.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    23 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the lower limit of the integral.
!
!    Output, real ( kind = rk ) TTH, the integral of H0(t)/t from x to oo.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) F0
    REAL (KIND=RK) G0
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) T
    REAL (KIND=RK) TTH
    REAL (KIND=RK) TTY
    REAL (KIND=RK) X
    REAL (KIND=RK) XT

    PI = 3.141592653589793D+00
    S = 1.0D+00
    R = 1.0D+00

    IF (X < 24.5D+00) THEN

        DO K = 1, 60
            R = - R * X * X * (2.0D+00*K-1.0D+00) / (2.0D+00*K+1.0D+00) ** 3
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTH = PI / 2.0D+00 - 2.0D+00 / PI * X * S

    ELSE

        DO K = 1, 10
            R = - R * (2.0D+00*K-1.0D+00) ** 3 / ((2.0D+00*K+1.0D+00)*X*X)
            S = S + R
            IF (ABS(R) < ABS(S)*1.0D-12) THEN
                EXIT
            END IF
        END DO

        TTH = 2.0D+00 / (PI*X) * S
        T = 8.0D+00 / X
        XT = X + 0.25D+00 * PI
        F0 = (((((0.18118D-02*T-0.91909D-02)*T+0.017033D+00)*T-0.9394D-03)*T-0.051445D+00)*T-&
       & 0.11D-05) * T + 0.7978846D+00
        G0 = (((((-0.23731D-02*T+0.59842D-02)*T+0.24437D-02)*T-0.0233178D+00)*T+0.595D-04)*T+&
       & 0.1620695D+00) * T
        TTY = (F0*SIN(XT)-G0*COS(XT)) / (SQRT(X)*X)
        TTH = TTH + TTY

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   12: HYPERGEOMETRIC AND CONFLUENT HYPERGEOMETRIC FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE HYGFX (A, B, C, X, HF)

!******************************************************************************
!
!! HYGFX evaluates the hypergeometric function F(A,B,C,X).
!
!  Licensing:
!
!    The original FORTRAN77 version of this routine is copyrighted by
!    Shanjie Zhang and Jianming Jin.  However, they give permission to
!    incorporate this routine into a user program that the copyright
!    is acknowledged.
!
!  Modified:
!
!    08 September 2007
!
!  Author:
!
!    Original FORTRAN77 version by Shanjie Zhang, Jianming Jin.
!    FORTRAN90 version by John Burkardt.
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
!    Input, real ( kind = rk ) A, B, C, X, the arguments of the function.
!    C must not be equal to a nonpositive integer.
!    X < 1.
!
!    Output, real HF, the value of the function.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) AA
    REAL (KIND=RK) B
    REAL (KIND=RK) BB
    REAL (KIND=RK) C
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
    REAL (KIND=RK) HF
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
    REAL (KIND=RK), PARAMETER :: PI = 3.141592653589793D+00
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) RM
    REAL (KIND=RK) RP
    REAL (KIND=RK) SM
    REAL (KIND=RK) SP
    REAL (KIND=RK) SP0
    REAL (KIND=RK) X
    REAL (KIND=RK) X1

    L0 = (C == AINT(C)) .AND. (C < 0.0D+00)
    L1 = (1.0D+00-X < 1.0D-15) .AND. (C-A-B <= 0.0D+00)
    L2 = (A == AINT(A)) .AND. (A < 0.0D+00)
    L3 = (B == AINT(B)) .AND. (B < 0.0D+00)
    L4 = (C-A == AINT(C-A)) .AND. (C-A <= 0.0D+00)
    L5 = (C-B == AINT(C-B)) .AND. (C-B <= 0.0D+00)

    IF (L0 .OR. L1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYGFX - Fatal error!'
        WRITE (*, '(a)') '  The hypergeometric series is divergent.'
        RETURN
    END IF

    IF (0.95D+00 < X) THEN
        EPS = 1.0D-08
    ELSE
        EPS = 1.0D-15
    END IF

    IF (X == 0.0D+00 .OR. A == 0.0D+00 .OR. B == 0.0D+00) THEN

        HF = 1.0D+00
        RETURN

    ELSE IF (1.0D+00-X == EPS .AND. 0.0D+00 < C-A-B) THEN

        CALL GAMMA (C, GC)
        CALL GAMMA (C-A-B, GCAB)
        CALL GAMMA (C-A, GCA)
        CALL GAMMA (C-B, GCB)
        HF = GC * GCAB / (GCA*GCB)
        RETURN

    ELSE IF (1.0D+00+X <= EPS .AND. ABS(C-A+B-1.0D+00) <= EPS) THEN

        G0 = SQRT (PI) * 2.0D+00 ** (-A)
        CALL GAMMA (C, G1)
        CALL GAMMA (1.0D+00+A/2.0D+00-B, G2)
        CALL GAMMA (0.5D+00+0.5D+00*A, G3)
        HF = G0 * G1 / (G2*G3)
        RETURN

    ELSE IF (L2 .OR. L3) THEN

        IF (L2) THEN
            NM = INT (ABS(A))
        END IF

        IF (L3) THEN
            NM = INT (ABS(B))
        END IF

        HF = 1.0D+00
        R = 1.0D+00

        DO K = 1, NM
            R = R * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X
            HF = HF + R
        END DO

        RETURN

    ELSE IF (L4 .OR. L5) THEN

        IF (L4) THEN
            NM = INT (ABS(C-A))
        END IF

        IF (L5) THEN
            NM = INT (ABS(C-B))
        END IF

        HF = 1.0D+00
        R = 1.0D+00
        DO K = 1, NM
            R = R * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X
            HF = HF + R
        END DO
        HF = (1.0D+00-X) ** (C-A-B) * HF
        RETURN

    END IF

    AA = A
    BB = B
    X1 = X
!
!  WARNING: ALTERATION OF INPUT ARGUMENTS A AND B, WHICH MIGHT BE CONSTANTS.
!
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
            CALL GAMMA (A, GA)
            CALL GAMMA (B, GB)
            CALL GAMMA (C, GC)
            CALL GAMMA (A+M, GAM)
            CALL GAMMA (B+M, GBM)
            CALL PSI (A, PA)
            CALL PSI (B, PB)

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

                HF = F0 * C0 + F1 * C1

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

                HF = F0 * C0 + F1 * C1

            END IF

        ELSE

            CALL GAMMA (A, GA)
            CALL GAMMA (B, GB)
            CALL GAMMA (C, GC)
            CALL GAMMA (C-A, GCA)
            CALL GAMMA (C-B, GCB)
            CALL GAMMA (C-A-B, GCAB)
            CALL GAMMA (A+B-C, GABC)
            C0 = GC * GCAB / (GCA*GCB)
            C1 = GC * GABC / (GA*GB) * (1.0D+00-X) ** (C-A-B)
            HF = 0.0D+00
            R0 = C0
            R1 = C1

            DO K = 1, 250

                R0 = R0 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(A+B-C+K)) * (1.0D+00-X)

                R1 = R1 * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C-A-B+K)) * (1.0D+00-X)

                HF = HF + R0 + R1

                IF (ABS(HF-HW) < ABS(HF)*EPS) THEN
                    EXIT
                END IF

                HW = HF

            END DO

            HF = HF + C0 + C1

        END IF

    ELSE

        A0 = 1.0D+00

        IF (A < C .AND. C < 2.0D+00*A .AND. B < C .AND. C < 2.0D+00*B) THEN

            A0 = (1.0D+00-X) ** (C-A-B)
            A = C - A
            B = C - B

        END IF

        HF = 1.0D+00
        R = 1.0D+00

        DO K = 1, 250

            R = R * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * X

            HF = HF + R

            IF (ABS(HF-HW) <= ABS(HF)*EPS) THEN
                EXIT
            END IF

            HW = HF

        END DO

        HF = A0 * HF

    END IF

    IF (X1 < 0.0D+00) THEN
        X = X1
        C0 = 1.0D+00 / (1.0D+00-X) ** AA
        HF = C0 * HF
    END IF

    A = AA
    B = BB

    IF (120 < K) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYGFX - Warning!'
        WRITE (*, '(a)') '  A large number of iterations were needed.'
        WRITE (*, '(a)') '  The accuracy of the results should be checked.'
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE HYGFZ (A, B, C, Z, ZHF)

!******************************************************************************
!
!! HYGFZ computes the hypergeometric function F(a,b,c,x) for complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, C, parameters.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) ZHF, the value of F(a,b,c,z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) AA
    REAL (KIND=RK) B
    REAL (KIND=RK) BB
    REAL (KIND=RK) C
    REAL (KIND=RK) CA
    REAL (KIND=RK) CB
    REAL (KIND=RK) EL
    REAL (KIND=RK) EPS
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    REAL (KIND=RK) G2
    REAL (KIND=RK) G3
    REAL (KIND=RK) GA
    REAL (KIND=RK) GAB
    REAL (KIND=RK) GABC
    REAL (KIND=RK) GAM
    REAL (KIND=RK) GB
    REAL (KIND=RK) GBA
    REAL (KIND=RK) GBM
    REAL (KIND=RK) GC
    REAL (KIND=RK) GCA
    REAL (KIND=RK) GCAB
    REAL (KIND=RK) GCB
    REAL (KIND=RK) GCBK
    REAL (KIND=RK) GM
    INTEGER J
    INTEGER K
    LOGICAL L0
    LOGICAL L1
    LOGICAL L2
    LOGICAL L3
    LOGICAL L4
    LOGICAL L5
    LOGICAL L6
    INTEGER M
    INTEGER MAB
    INTEGER MCAB
    INTEGER NCA
    INTEGER NCB
    INTEGER NM
    REAL (KIND=RK) PA
    REAL (KIND=RK) PAC
    REAL (KIND=RK) PB
    REAL (KIND=RK) PCA
    REAL (KIND=RK) PI
    REAL (KIND=RK) RK1
    REAL (KIND=RK) RK2
    REAL (KIND=RK) RM
    REAL (KIND=RK) SJ1
    REAL (KIND=RK) SJ2
    REAL (KIND=RK) SM
    REAL (KIND=RK) SP
    REAL (KIND=RK) SP0
    REAL (KIND=RK) SQ
    REAL (KIND=RK) T0
    REAL (KIND=RK) W0
    REAL (KIND=RK) WS
    REAL (KIND=RK) X
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z00
    COMPLEX (KIND=CK) Z1
    COMPLEX (KIND=CK) ZC0
    COMPLEX (KIND=CK) ZC1
    COMPLEX (KIND=CK) ZF0
    COMPLEX (KIND=CK) ZF1
    COMPLEX (KIND=CK) ZHF
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZP0
    COMPLEX (KIND=CK) ZR
    COMPLEX (KIND=CK) ZR0
    COMPLEX (KIND=CK) ZR1
    COMPLEX (KIND=CK) ZW

    X = REAL (Z, KIND=RK)
    Y = AIMAG (Z)
    EPS = 1.0D-15
    L0 = C == INT (C) .AND. C < 0.0D+00
    L1 = ABS (1.0D+00-X) < EPS .AND. Y == 0.0D+00 .AND. C - A - B <= 0.0D+00
    L2 = ABS (Z+1.0D+00) < EPS .AND. ABS (C-A+B-1.0D+00) < EPS
    L3 = A == INT (A) .AND. A < 0.0D+00
    L4 = B == INT (B) .AND. B < 0.0D+00
    L5 = C - A == INT (C-A) .AND. C - A <= 0.0D+00
    L6 = C - B == INT (C-B) .AND. C - B <= 0.0D+00
    AA = A
    BB = B
    A0 = ABS (Z)
    IF (0.95D+00 < A0) THEN
        EPS = 1.0D-08
    END IF
    PI = 3.141592653589793D+00
    EL = 0.5772156649015329D+00

    IF (L0 .OR. L1) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYGFZ - Fatal error!'
        WRITE (*, '(a)') '  The hypergeometric series is divergent.'
        STOP
    END IF

    IF (A0 == 0.0D+00 .OR. A == 0.0D+00 .OR. B == 0.0D+00) THEN

        ZHF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

    ELSE IF (Z == 1.0D+00 .AND. 0.0D+00 < C-A-B) THEN

        CALL GAMMA (C, GC)
        CALL GAMMA (C-A-B, GCAB)
        CALL GAMMA (C-A, GCA)
        CALL GAMMA (C-B, GCB)
        ZHF = GC * GCAB / (GCA*GCB)

    ELSE IF (L2) THEN

        G0 = SQRT (PI) * 2.0D+00 ** (-A)
        CALL GAMMA (C, G1)
        CALL GAMMA (1.0D+00+A/2.0D+00-B, G2)
        CALL GAMMA (0.5D+00+0.5D+00*A, G3)
        ZHF = G0 * G1 / (G2*G3)

    ELSE IF (L3 .OR. L4) THEN

        IF (L3) THEN
            NM = INT (ABS(A))
        END IF

        IF (L4) THEN
            NM = INT (ABS(B))
        END IF

        ZHF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        ZR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, NM
            ZR = ZR * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * Z
            ZHF = ZHF + ZR
        END DO

    ELSE IF (L5 .OR. L6) THEN

        IF (L5) THEN
            NM = INT (ABS(C-A))
        END IF

        IF (L6) THEN
            NM = INT (ABS(C-B))
        END IF

        ZHF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        ZR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, NM
            ZR = ZR * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C+K-1.0D+00)) * Z
            ZHF = ZHF + ZR
        END DO
        ZHF = (1.0D+00-Z) ** (C-A-B) * ZHF

    ELSE IF (A0 <= 1.0D+00) THEN

        IF (X < 0.0D+00) THEN

            Z1 = Z / (Z-1.0D+00)
            IF (A < C .AND. B < A .AND. 0.0D+00 < B) THEN
                A = BB
                B = AA
            END IF
            ZC0 = 1.0D+00 / ((1.0D+00-Z)**A)
            ZHF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            ZR0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO K = 1, 500
                ZR0 = ZR0 * (A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C+K-1.0D+00)) * Z1
                ZHF = ZHF + ZR0
                IF (ABS(ZHF-ZW) < ABS(ZHF)*EPS) THEN
                    EXIT
                END IF
                ZW = ZHF
            END DO

            ZHF = ZC0 * ZHF

        ELSE IF (0.90D+00 <= A0) THEN

            GM = 0.0D+00
            MCAB = INT (C-A-B+EPS*SIGN(1.0D+00, C-A-B))

            IF (ABS(C-A-B-MCAB) < EPS) THEN

                M = INT (C-A-B)
                CALL GAMMA (A, GA)
                CALL GAMMA (B, GB)
                CALL GAMMA (C, GC)
                CALL GAMMA (A+M, GAM)
                CALL GAMMA (B+M, GBM)
                CALL PSI (A, PA)
                CALL PSI (B, PB)
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
                ZF0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                ZR0 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                ZR1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                SP0 = 0.0D+00
                SP = 0.0D+00

                IF (0 <= M) THEN

                    ZC0 = GM * GC / (GAM*GBM)
                    ZC1 = - GC * (Z-1.0D+00) ** M / (GA*GB*RM)
                    DO K = 1, M - 1
                        ZR0 = ZR0 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(K-M)) * (1.0D+00-Z)
                        ZF0 = ZF0 + ZR0
                    END DO
                    DO K = 1, M
                        SP0 = SP0 + 1.0D+00 / (A+K-1.0D+00) + 1.0D+00 / (B+K-1.0D+00) - 1.0D+00 &
                       & / K
                    END DO
                    ZF1 = PA + PB + SP0 + 2.0D+00 * EL + LOG (1.0D+00-Z)
                    DO K = 1, 500
                        SP = SP + (1.0D+00-A) / (K*(A+K-1.0D+00)) + (1.0D+00-B) / &
                       & (K*(B+K-1.0D+00))
                        SM = 0.0D+00
                        DO J = 1, M
                            SM = SM + (1.0D+00-A) / ((J+K)*(A+J+K-1.0D+00)) + 1.0D+00 / &
                           & (B+J+K-1.0D+00)
                        END DO
                        ZP = PA + PB + 2.0D+00 * EL + SP + SM + LOG (1.0D+00-Z)
                        ZR1 = ZR1 * (A+M+K-1.0D+00) * (B+M+K-1.0D+00) / (K*(M+K)) * (1.0D+00-Z)
                        ZF1 = ZF1 + ZR1 * ZP
                        IF (ABS(ZF1-ZW) < ABS(ZF1)*EPS) THEN
                            EXIT
                        END IF
                        ZW = ZF1
                    END DO

                    ZHF = ZF0 * ZC0 + ZF1 * ZC1

                ELSE IF (M < 0) THEN

                    M = - M
                    ZC0 = GM * GC / (GA*GB*(1.0D+00-Z)**M)
                    ZC1 = - (-1.0D+00) ** M * GC / (GAM*GBM*RM)
                    DO K = 1, M - 1
                        ZR0 = ZR0 * (A-M+K-1.0D+00) * (B-M+K-1.0D+00) / (K*(K-M)) * (1.0D+00-Z)
                        ZF0 = ZF0 + ZR0
                    END DO

                    DO K = 1, M
                        SP0 = SP0 + 1.0D+00 / K
                    END DO

                    ZF1 = PA + PB - SP0 + 2.0D+00 * EL + LOG (1.0D+00-Z)

                    DO K = 1, 500
                        SP = SP + (1.0D+00-A) / (K*(A+K-1.0D+00)) + (1.0D+00-B) / &
                       & (K*(B+K-1.0D+00))
                        SM = 0.0D+00
                        DO J = 1, M
                            SM = SM + 1.0D+00 / (J+K)
                        END DO
                        ZP = PA + PB + 2.0D+00 * EL + SP - SM + LOG (1.0D+00-Z)
                        ZR1 = ZR1 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(M+K)) * (1.0D+00-Z)
                        ZF1 = ZF1 + ZR1 * ZP
                        IF (ABS(ZF1-ZW) < ABS(ZF1)*EPS) THEN
                            EXIT
                        END IF
                        ZW = ZF1

                    END DO

                    ZHF = ZF0 * ZC0 + ZF1 * ZC1

                END IF

            ELSE

                CALL GAMMA (A, GA)
                CALL GAMMA (B, GB)
                CALL GAMMA (C, GC)
                CALL GAMMA (C-A, GCA)
                CALL GAMMA (C-B, GCB)
                CALL GAMMA (C-A-B, GCAB)
                CALL GAMMA (A+B-C, GABC)
                ZC0 = GC * GCAB / (GCA*GCB)
                ZC1 = GC * GABC / (GA*GB) * (1.0D+00-Z) ** (C-A-B)
                ZHF = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
                ZR0 = ZC0
                ZR1 = ZC1
                DO K = 1, 500
                    ZR0 = ZR0 * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(A+B-C+K)) * (1.0D+00-Z)
                    ZR1 = ZR1 * (C-A+K-1.0D+00) * (C-B+K-1.0D+00) / (K*(C-A-B+K)) * (1.0D+00-Z)
                    ZHF = ZHF + ZR0 + ZR1
                    IF (ABS(ZHF-ZW) < ABS(ZHF)*EPS) THEN
                        EXIT
                    END IF
                    ZW = ZHF
                END DO

                ZHF = ZHF + ZC0 + ZC1

            END IF

        ELSE

            Z00 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

            IF (C-A < A .AND. C-B < B) THEN
                Z00 = (1.0D+00-Z) ** (C-A-B)
                A = C - A
                B = C - B
            END IF

            ZHF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            ZR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

            DO K = 1, 1500
                ZR = ZR * (A+K-1.0D+00) * (B+K-1.0D+00) / (K*(C+K-1.0D+00)) * Z
                ZHF = ZHF + ZR
                IF (ABS(ZHF-ZW) <= ABS(ZHF)*EPS) THEN
                    EXIT
                END IF
                ZW = ZHF
            END DO

            ZHF = Z00 * ZHF

        END IF

    ELSE IF (1.0D+00 < A0) THEN

        MAB = INT (A-B+EPS*SIGN(1.0D+00, A-B))

        IF (ABS(A-B-MAB) < EPS .AND. A0 <= 1.1D+00) THEN
            B = B + EPS
        END IF

        IF (EPS < ABS(A-B-MAB)) THEN

            CALL GAMMA (A, GA)
            CALL GAMMA (B, GB)
            CALL GAMMA (C, GC)
            CALL GAMMA (A-B, GAB)
            CALL GAMMA (B-A, GBA)
            CALL GAMMA (C-A, GCA)
            CALL GAMMA (C-B, GCB)
            ZC0 = GC * GBA / (GCA*GB*(-Z)**A)
            ZC1 = GC * GAB / (GCB*GA*(-Z)**B)
            ZR0 = ZC0
            ZR1 = ZC1
            ZHF = CMPLX (0.0D+00, 0.0D+00, KIND=CK)

            DO K = 1, 500
                ZR0 = ZR0 * (A+K-1.0D+00) * (A-C+K) / ((A-B+K)*K*Z)
                ZR1 = ZR1 * (B+K-1.0D+00) * (B-C+K) / ((B-A+K)*K*Z)
                ZHF = ZHF + ZR0 + ZR1
                IF (ABS((ZHF-ZW)/ZHF) <= EPS) THEN
                    EXIT
                END IF
                ZW = ZHF
            END DO

            ZHF = ZHF + ZC0 + ZC1

        ELSE

            IF (A-B < 0.0D+00) THEN
                A = BB
                B = AA
            END IF

            CA = C - A
            CB = C - B
            NCA = INT (CA+EPS*SIGN(1.0D+00, CA))
            NCB = INT (CB+EPS*SIGN(1.0D+00, CB))

            IF (ABS(CA-NCA) < EPS .OR. ABS(CB-NCB) < EPS) THEN
                C = C + EPS
            END IF

            CALL GAMMA (A, GA)
            CALL GAMMA (C, GC)
            CALL GAMMA (C-B, GCB)
            CALL PSI (A, PA)
            CALL PSI (C-A, PCA)
            CALL PSI (A-C, PAC)
            MAB = INT (A-B+EPS)
            ZC0 = GC / (GA*(-Z)**B)
            CALL GAMMA (A-B, GM)
            ZF0 = GM / GCB * ZC0
            ZR = ZC0
            DO K = 1, MAB - 1
                ZR = ZR * (B+K-1.0D+00) / (K*Z)
                T0 = A - B - K
                CALL GAMMA (T0, G0)
                CALL GAMMA (C-B-K, GCBK)
                ZF0 = ZF0 + ZR * G0 / GCBK
            END DO

            IF (MAB == 0) THEN
                ZF0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            END IF

            ZC1 = GC / (GA*GCB*(-Z)**A)
            SP = - 2.0D+00 * EL - PA - PCA
            DO J = 1, MAB
                SP = SP + 1.0D+00 / J
            END DO
            ZP0 = SP + LOG (-Z)
            SQ = 1.0D+00
            DO J = 1, MAB
                SQ = SQ * (B+J-1.0D+00) * (B-C+J) / J
            END DO
            ZF1 = (SQ*ZP0) * ZC1
            ZR = ZC1
            RK1 = 1.0D+00
            SJ1 = 0.0D+00

            DO K = 1, 10000
                ZR = ZR / Z
                RK1 = RK1 * (B+K-1.0D+00) * (B-C+K) / (K*K)
                RK2 = RK1
                DO J = K + 1, K + MAB
                    RK2 = RK2 * (B+J-1.0D+00) * (B-C+J) / J
                END DO
                SJ1 = SJ1 + (A-1.0D+00) / (K*(A+K-1.0D+00)) + (A-C-1.0D+00) / &
               & (K*(A-C+K-1.0D+00))
                SJ2 = SJ1
                DO J = K + 1, K + MAB
                    SJ2 = SJ2 + 1.0D+00 / J
                END DO
                ZP = - 2.0D+00 * EL - PA - PAC + SJ2 - 1.0D+00 / (K+A-C) - PI / TAN &
               & (PI*(K+A-C)) + LOG (-Z)
                ZF1 = ZF1 + RK2 * ZR * ZP
                WS = ABS (ZF1)
                IF (ABS((WS-W0)/WS) < EPS) THEN
                    EXIT
                END IF
                W0 = WS
            END DO

            ZHF = ZF0 + ZF1

        END IF

    END IF

    A = AA
    B = BB
    IF (150 < K) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'HYGFZ - Warning!'
        WRITE (*, '(a)') '  The solution returned may have low accuracy.'
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CCHG (A, B, Z, CHG)

!******************************************************************************
!
!! cchg() computes the confluent hypergeometric function.
!
!  Discussion:
!
!    This function computes the confluent hypergeometric function
!    M(a,b,z) with real parameters a, b and complex argument z.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    26 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Input:
!
!    real ( kind = rk ) A, B, parameter values.
!
!    complex ( kind = ck ) Z, the argument.
!
!  Output:
!
!    complex ( kind = ck ) CHG, the value of M(a,b,z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) B
    REAL (KIND=RK) BA
    COMPLEX (KIND=CK) CFAC
    COMPLEX (KIND=CK) CHG
    COMPLEX (KIND=CK) CHG1
    COMPLEX (KIND=CK) CHG2
    COMPLEX (KIND=CK) CHW
    COMPLEX (KIND=CK) CI
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CR1
    COMPLEX (KIND=CK) CR2
    COMPLEX (KIND=CK) CRG
    COMPLEX (KIND=CK) CS1
    COMPLEX (KIND=CK) CS2
    COMPLEX (KIND=CK) CY0
    COMPLEX (KIND=CK) CY1
    REAL (KIND=RK) G1
    REAL (KIND=RK) G2
    REAL (KIND=RK) G3
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER LA
    INTEGER M
    INTEGER N
    INTEGER NL
    INTEGER NS
    REAL (KIND=RK) PHI
    REAL (KIND=RK) PI
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z0

    PI = 3.141592653589793D+00
    CI = CMPLX (0.0D+00, 1.0D+00, KIND=CK)
    A0 = A
    A1 = A
    Z0 = Z

    IF (B == 0.0D+00 .OR. B ==-INT(ABS(B))) THEN
        CHG = CMPLX (1.0D+30, 0.0D+00, KIND=CK)
    ELSE IF (A == 0.0D+00 .OR. Z == 0.0D+00) THEN
        CHG = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
    ELSE IF (A ==-1.0D+00) THEN
        CHG = 1.0D+00 - Z / B
    ELSE IF (A == B) THEN
        CHG = EXP (Z)
    ELSE IF (A-B == 1.0D+00) THEN
        CHG = (1.0D+00+Z/B) * EXP (Z)
    ELSE IF (A == 1.0D+00 .AND. B == 2.0D+00) THEN
        CHG = (EXP(Z)-1.0D+00) / Z
    ELSE IF (A == INT(A) .AND. A < 0.0D+00) THEN
        M = INT (-A)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CHG = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, M
            CR = CR * (A+K-1.0D+00) / K / (B+K-1.0D+00) * Z
            CHG = CHG + CR
        END DO
    ELSE

        X0 = REAL (Z, KIND=RK)
        IF (X0 < 0.0D+00) THEN
            A = B - A
            A0 = A
            Z = - Z
        END IF

        IF (A < 2.0D+00) THEN
            NL = 0
        ELSE
            NL = 1
            LA = INT (A)
            A = A - LA - 1.0D+00
        END IF

        DO N = 0, NL

            IF (2.0D+00 <= A0) THEN
                A = A + 1.0D+00
            END IF

            IF (ABS(Z) < 20.0D+00+ABS(B) .OR. A < 0.0D+00) THEN

                CHG = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                CRG = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                DO J = 1, 500
                    CRG = CRG * (A+J-1.0D+00) / (J*(B+J-1.0D+00)) * Z
                    CHG = CHG + CRG
                    IF (ABS((CHG-CHW)/CHG) < 1.0D-15) THEN
                        EXIT
                    END IF
                    CHW = CHG
                END DO

            ELSE

                CALL GAMMA (A, G1)
                CALL GAMMA (B, G2)
                BA = B - A
                CALL GAMMA (BA, G3)
                CS1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                CS2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                CR1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                CR2 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

                DO I = 1, 8
                    CR1 = - CR1 * (A+I-1.0D+00) * (A-B+I) / (Z*I)
                    CR2 = CR2 * (B-A+I-1.0D+00) * (I-A) / (Z*I)
                    CS1 = CS1 + CR1
                    CS2 = CS2 + CR2
                END DO

                X = REAL (Z, KIND=RK)
                Y = AIMAG (Z)

                IF (X == 0.0D+00 .AND. 0.0D+00 <= Y) THEN
                    PHI = 0.5D+00 * PI
                ELSE IF (X == 0.0D+00 .AND. Y <= 0.0D+00) THEN
                    PHI = - 0.5D+00 * PI
                ELSE
                    PHI = ATAN (Y/X)
                END IF

                IF (-1.5D+00*PI < PHI .AND. PHI <=-0.5*PI) THEN
                    NS = - 1
                ELSE IF (-0.5D+00*PI < PHI .AND. PHI < 1.5D+00*PI) THEN
                    NS = 1
                END IF

                IF (Y == 0.0D+00) THEN
                    CFAC = COS (PI*A)
                ELSE
                    CFAC = EXP (NS*CI*PI*A)
                END IF

                CHG1 = G2 / G3 * Z ** (-A) * CFAC * CS1
                CHG2 = G2 / G1 * EXP (Z) * Z ** (A-B) * CS2
                CHG = CHG1 + CHG2

            END IF

            IF (N == 0) THEN
                CY0 = CHG
            ELSE IF (N == 1) THEN
                CY1 = CHG
            END IF

        END DO

        IF (2.0D+00 <= A0) THEN
            DO I = 1, LA - 1
                CHG = ((2.0D+00*A-B+Z)*CY1+(B-A)*CY0) / A
                CY0 = CY1
                CY1 = CHG
                A = A + 1.0D+00
            END DO
        END IF

        IF (X0 < 0.0D+00) THEN
            CHG = CHG * EXP (-Z)
        END IF

    END IF

    A = A1
    Z = Z0

    RETURN
END

!******************************************************************************

SUBROUTINE CHGM (A, B, X, HG)

!******************************************************************************
!
!! CHGM computes the confluent hypergeometric function M(a,b,x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    27 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HG, the value of M(a,b,x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) B
    REAL (KIND=RK) HG
    REAL (KIND=RK) HG1
    REAL (KIND=RK) HG2
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER LA
    INTEGER M
    INTEGER N
    INTEGER NL
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) RG
    REAL (KIND=RK) SUM1
    REAL (KIND=RK) SUM2
    REAL (KIND=RK) TA
    REAL (KIND=RK) TB
    REAL (KIND=RK) TBA
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) XG
    REAL (KIND=RK) Y0
    REAL (KIND=RK) Y1

    PI = 3.141592653589793D+00
    A0 = A
    A1 = A
    X0 = X
    HG = 0.0D+00

    IF (B == 0.0D+00 .OR. B ==-ABS(INT(B))) THEN
        HG = 1.0D+300
    ELSE IF (A == 0.0D+00 .OR. X == 0.0D+00) THEN
        HG = 1.0D+00
    ELSE IF (A ==-1.0D+00) THEN
        HG = 1.0D+00 - X / B
    ELSE IF (A == B) THEN
        HG = EXP (X)
    ELSE IF (A-B == 1.0D+00) THEN
        HG = (1.0D+00+X/B) * EXP (X)
    ELSE IF (A == 1.0D+00 .AND. B == 2.0D+00) THEN
        HG = (EXP(X)-1.0D+00) / X
    ELSE IF (A == INT(A) .AND. A < 0.0D+00) THEN
        M = INT (-A)
        R = 1.0D+00
        HG = 1.0D+00
        DO K = 1, M
            R = R * (A+K-1.0D+00) / K / (B+K-1.0D+00) * X
            HG = HG + R
        END DO
    END IF

    IF (HG /= 0.0D+00) THEN
        RETURN
    END IF

    IF (X < 0.0D+00) THEN
        A = B - A
        A0 = A
        X = ABS (X)
    END IF

    IF (A < 2.0D+00) THEN
        NL = 0
    END IF

    IF (2.0D+00 <= A) THEN
        NL = 1
        LA = INT (A)
        A = A - LA - 1.0D+00
    END IF

    DO N = 0, NL

        IF (2.0D+00 <= A0) THEN
            A = A + 1.0D+00
        END IF

        IF (X <= 30.0D+00+ABS(B) .OR. A < 0.0D+00) THEN

            HG = 1.0D+00
            RG = 1.0D+00
            DO J = 1, 500
                RG = RG * (A+J-1.0D+00) / (J*(B+J-1.0D+00)) * X
                HG = HG + RG
                IF (ABS(RG/HG) < 1.0D-15) THEN
                    EXIT
                END IF
            END DO

        ELSE

            CALL GAMMA (A, TA)
            CALL GAMMA (B, TB)
            XG = B - A
            CALL GAMMA (XG, TBA)
            SUM1 = 1.0D+00
            SUM2 = 1.0D+00
            R1 = 1.0D+00
            R2 = 1.0D+00
            DO I = 1, 8
                R1 = - R1 * (A+I-1.0D+00) * (A-B+I) / (X*I)
                R2 = - R2 * (B-A+I-1.0D+00) * (A-I) / (X*I)
                SUM1 = SUM1 + R1
                SUM2 = SUM2 + R2
            END DO
            HG1 = TB / TBA * X ** (-A) * COS (PI*A) * SUM1
            HG2 = TB / TA * EXP (X) * X ** (A-B) * SUM2
            HG = HG1 + HG2

        END IF

        IF (N == 0) THEN
            Y0 = HG
        ELSE IF (N == 1) THEN
            Y1 = HG
        END IF

    END DO

    IF (2.0D+00 <= A0) THEN
        DO I = 1, LA - 1
            HG = ((2.0D+00*A-B+X)*Y1+(B-A)*Y0) / A
            Y0 = Y1
            Y1 = HG
            A = A + 1.0D+00
        END DO
    END IF

    IF (X0 < 0.0D+00) THEN
        HG = HG * EXP (X0)
    END IF

    A = A1
    X = X0

    RETURN
END

!******************************************************************************

SUBROUTINE CHGU (A, B, X, HU, MD)

!******************************************************************************
!
!! CHGU computes the confluent hypergeometric function U(a,b,x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    27 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HU, U(a,b,x).
!
!    Output, integer MD, the method code.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A00
    REAL (KIND=RK) AA
    REAL (KIND=RK) B
    REAL (KIND=RK) B00
    LOGICAL BL1
    LOGICAL BL2
    LOGICAL BL3
    LOGICAL BN
    REAL (KIND=RK) HU
    REAL (KIND=RK) HU1
    INTEGER ID
    INTEGER ID1
    LOGICAL IL1
    LOGICAL IL2
    LOGICAL IL3
    INTEGER MD
    REAL (KIND=RK) X

    AA = A - B + 1.0D+00
    IL1 = A == INT (A) .AND. A <= 0.0D+00
    IL2 = AA == INT (AA) .AND. AA <= 0.0D+00
    IL3 = ABS (A*(A-B+1.0D+00)) / X <= 2.0D+00
    BL1 = X <= 5.0D+00 .OR. (X <= 10.0D+00 .AND. A <= 2.0D+00)
    BL2 = (5.0D+00 < X .AND. X <= 12.5D+00) .AND. (1.0D+00 <= A .AND. A+4.0D+00 <= B)
    BL3 = 12.5D+00 < X .AND. 5.0D+00 <= A .AND. A + 5.0D+00 <= B
    BN = B == INT (B) .AND. B .NE. 0.0D+00
    ID1 = - 100

    IF (B .NE. INT(B)) THEN
        CALL CHGUS (A, B, X, HU, ID1)
        MD = 1
        IF (6 <= ID1) THEN
            RETURN
        END IF
        HU1 = HU
    END IF

    IF (IL1 .OR. IL2 .OR. IL3) THEN
        CALL CHGUL (A, B, X, HU, ID)
        MD = 2
        IF (6 <= ID) THEN
            RETURN
        END IF
        IF (ID < ID1) THEN
            MD = 1
            ID = ID1
            HU = HU1
        END IF
    END IF

    IF (0.0D+00 <= A) THEN
        IF (BN .AND. (BL1 .OR. BL2 .OR. BL3)) THEN
            CALL CHGUBI (A, B, X, HU, ID)
            MD = 3
        ELSE
            CALL CHGUIT (A, B, X, HU, ID)
            MD = 4
        END IF
    ELSE
        IF (B <= A) THEN
            A00 = A
            B00 = B
            A = A - B + 1.0D+00
            B = 2.0D+00 - B
            CALL CHGUIT (A, B, X, HU, ID)
            HU = X ** (1.0D+00-B00) * HU
            A = A00
            B = B00
            MD = 4
        ELSE IF (BN .AND. ( .NOT. IL1)) THEN
            CALL CHGUBI (A, B, X, HU, ID)
            MD = 3
        END IF
    END IF

    IF (ID < 6) THEN
        WRITE (*, '(a)') ' '
        WRITE (*, '(a)') 'CHGU - Warning!'
        WRITE (*, '(a)') '  Accurate results were not obtained.'
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CHGUBI (A, B, X, HU, ID)

!******************************************************************************
!
!! CHGUBI: confluent hypergeometric function with integer argument B.
!
!  Discussion:
!
!    This procedure computes the confluent hypergeometric function
!    U(a,b,x) with integer b ( b = 1,2,... )
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HU, the value of U(a,b,x).
!
!    Output, integer ID, the estimated number of significant
!    digits.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) A1
    REAL (KIND=RK) A2
    REAL (KIND=RK) B
    REAL (KIND=RK) DA1
    REAL (KIND=RK) DA2
    REAL (KIND=RK) DB1
    REAL (KIND=RK) DB2
    REAL (KIND=RK) EL
    REAL (KIND=RK) GA
    REAL (KIND=RK) GA1
    REAL (KIND=RK) H0
    REAL (KIND=RK) HM1
    REAL (KIND=RK) HM2
    REAL (KIND=RK) HM3
    REAL (KIND=RK) HMAX
    REAL (KIND=RK) HMIN
    REAL (KIND=RK) HU
    REAL (KIND=RK) HU1
    REAL (KIND=RK) HU2
    REAL (KIND=RK) HW
    INTEGER ID
    INTEGER ID1
    INTEGER ID2
    INTEGER J
    INTEGER K
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PS
    REAL (KIND=RK) R
    REAL (KIND=RK) RN
    REAL (KIND=RK) RN1
    REAL (KIND=RK) S0
    REAL (KIND=RK) S1
    REAL (KIND=RK) S2
    REAL (KIND=RK) SA
    REAL (KIND=RK) SB
    REAL (KIND=RK) UA
    REAL (KIND=RK) UB
    REAL (KIND=RK) X

    ID = - 100
    EL = 0.5772156649015329D+00
    N = INT (ABS(B-1))
    RN1 = 1.0D+00
    RN = 1.0D+00
    DO J = 1, N
        RN = RN * J
        IF (J == N-1) THEN
            RN1 = RN
        END IF
    END DO

    CALL PSI (A, PS)
    CALL GAMMA (A, GA)

    IF (0.0D+00 < B) THEN
        A0 = A
        A1 = A - N
        A2 = A1
        CALL GAMMA (A1, GA1)
        UA = (-1) ** (N-1) / (RN*GA1)
        UB = RN1 / GA * X ** (-N)
    ELSE
        A0 = A + N
        A1 = A0
        A2 = A
        CALL GAMMA (A1, GA1)
        UA = (-1) ** (N-1) / (RN*GA) * X ** N
        UB = RN1 / GA1
    END IF

    HM1 = 1.0D+00
    R = 1.0D+00
    HMAX = 0.0D+00
    HMIN = 1.0D+300

    DO K = 1, 150
        R = R * (A0+K-1.0D+00) * X / ((N+K)*K)
        HM1 = HM1 + R
        HU1 = ABS (HM1)
        HMAX = MAX (HMAX, HU1)
        HMIN = MIN (HMIN, HU1)
        IF (ABS(HM1-H0) < ABS(HM1)*1.0D-15) THEN
            EXIT
        END IF
        H0 = HM1
    END DO

    DA1 = LOG10 (HMAX)
    IF (HMIN /= 0.0D+00) THEN
        DA2 = LOG10 (HMIN)
    END IF
    ID = 15 - INT (ABS(DA1-DA2))
    HM1 = HM1 * LOG (X)
    S0 = 0.0D+00
    DO M = 1, N
        IF (0.0D+00 <= B) THEN
            S0 = S0 - 1.0D+00 / M
        ELSE
            S0 = S0 + (1.0D+00-A) / (M*(A+M-1.0D+00))
        END IF
    END DO
    HM2 = PS + 2.0D+00 * EL + S0
    R = 1.0D+00
    HMAX = 0.0D+00
    HMIN = 1.0D+300
    DO K = 1, 150
        S1 = 0.0D+00
        S2 = 0.0D+00
        IF (0.0D+00 < B) THEN
            DO M = 1, K
                S1 = S1 - (M+2.0D+00*A-2.0D+00) / (M*(M+A-1.0D+00))
            END DO
            DO M = 1, N
                S2 = S2 + 1.0D+00 / (K+M)
            END DO
        ELSE
            DO M = 1, K + N
                S1 = S1 + (1.0D+00-A) / (M*(M+A-1.0D+00))
            END DO
            DO M = 1, K
                S2 = S2 + 1.0D+00 / M
            END DO
        END IF
        HW = 2.0D+00 * EL + PS + S1 - S2
        R = R * (A0+K-1.0D+00) * X / ((N+K)*K)
        HM2 = HM2 + R * HW
        HU2 = ABS (HM2)
        HMAX = MAX (HMAX, HU2)
        HMIN = MIN (HMIN, HU2)

        IF (ABS((HM2-H0)/HM2) < 1.0D-15) THEN
            EXIT
        END IF

        H0 = HM2

    END DO

    DB1 = LOG10 (HMAX)
    IF (HMIN /= 0.0D+00) THEN
        DB2 = LOG10 (HMIN)
    END IF
    ID1 = 15 - INT (ABS(DB1-DB2))
    ID = MIN (ID, ID1)

    IF (N == 0) THEN
        HM3 = 0.0D+00
    ELSE
        HM3 = 1.0D+00
    END IF

    R = 1.0D+00
    DO K = 1, N - 1
        R = R * (A2+K-1.0D+00) / ((K-N)*K) * X
        HM3 = HM3 + R
    END DO

    SA = UA * (HM1+HM2)
    SB = UB * HM3
    HU = SA + SB

    IF (SA /= 0.0D+00) THEN
        ID1 = INT (LOG10(ABS(SA)))
    END IF

    IF (HU /= 0.0D+00) THEN
        ID2 = INT (LOG10(ABS(HU)))
    END IF

    IF (SA*SB < 0.0D+00) THEN
        ID = ID - ABS (ID1-ID2)
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CHGUIT (A, B, X, HU, ID)

!******************************************************************************
!
!! CHGUIT computes the hypergeometric function using Gauss-Legendre integration.
!
!  Discussion:
!
!    This procedure computes the hypergeometric function U(a,b,x) by
!    using Gaussian-Legendre integration (n = 60)
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HU, U(a,b,z).
!
!    Output, integer ID, the estimated number of significant digits.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A1
    REAL (KIND=RK) B
    REAL (KIND=RK) B1
    REAL (KIND=RK) C
    REAL (KIND=RK) D
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) G
    REAL (KIND=RK) GA
    REAL (KIND=RK) HU
    REAL (KIND=RK) HU0
    REAL (KIND=RK) HU1
    REAL (KIND=RK) HU2
    INTEGER ID
    INTEGER J
    INTEGER K
    INTEGER M
    REAL (KIND=RK) S
    REAL (KIND=RK), SAVE, DIMENSION (30) :: T = (/ 0.259597723012478D-01, &
   & 0.778093339495366D-01, 0.129449135396945D+00, 0.180739964873425D+00, &
   & 0.231543551376029D+00, 0.281722937423262D+00, 0.331142848268448D+00, &
   & 0.379670056576798D+00, 0.427173741583078D+00, 0.473525841761707D+00, &
   & 0.518601400058570D+00, 0.562278900753945D+00, 0.604440597048510D+00, &
   & 0.644972828489477D+00, 0.683766327381356D+00, 0.720716513355730D+00, &
   & 0.755723775306586D+00, 0.788693739932264D+00, 0.819537526162146D+00, &
   & 0.848171984785930D+00, 0.874519922646898D+00, 0.898510310810046D+00, &
   & 0.920078476177628D+00, 0.939166276116423D+00, 0.955722255839996D+00, &
   & 0.969701788765053D+00, 0.981067201752598D+00, 0.989787895222222D+00, &
   & 0.995840525118838D+00, 0.999210123227436D+00 /)
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2
    REAL (KIND=RK) T3
    REAL (KIND=RK) T4
    REAL (KIND=RK), SAVE, DIMENSION (30) :: W = (/ 0.519078776312206D-01, &
   & 0.517679431749102D-01, 0.514884515009810D-01, 0.510701560698557D-01, &
   & 0.505141845325094D-01, 0.498220356905502D-01, 0.489955754557568D-01, &
   & 0.480370318199712D-01, 0.469489888489122D-01, 0.457343797161145D-01, &
   & 0.443964787957872D-01, 0.429388928359356D-01, 0.413655512355848D-01, &
   & 0.396806954523808D-01, 0.378888675692434D-01, 0.359948980510845D-01, &
   & 0.340038927249464D-01, 0.319212190192963D-01, 0.297524915007890D-01, &
   & 0.275035567499248D-01, 0.251804776215213D-01, 0.227895169439978D-01, &
   & 0.203371207294572D-01, 0.178299010142074D-01, 0.152746185967848D-01, &
   & 0.126781664768159D-01, 0.100475571822880D-01, 0.738993116334531D-02, &
   & 0.471272992695363D-02, 0.202681196887362D-02 /)
    REAL (KIND=RK) X

    ID = 7
    A1 = A - 1.0D+00
    B1 = B - A - 1.0D+00
    C = 12.0D+00 / X

    DO M = 10, 100, 5

        HU1 = 0.0D+00
        G = 0.5D+00 * C / M
        D = G
        DO J = 1, M
            S = 0.0D+00
            DO K = 1, 30
                T1 = D + G * T (K)
                T2 = D - G * T (K)
                F1 = EXP (-X*T1) * T1 ** A1 * (1.0D+00+T1) ** B1
                F2 = EXP (-X*T2) * T2 ** A1 * (1.0D+00+T2) ** B1
                S = S + W (K) * (F1+F2)
            END DO
            HU1 = HU1 + S * G
            D = D + 2.0D+00 * G
        END DO

        IF (ABS(1.0D+00-HU0/HU1) < 1.0D-07) THEN
            EXIT
        END IF

        HU0 = HU1

    END DO

    CALL GAMMA (A, GA)
    HU1 = HU1 / GA

    DO M = 2, 10, 2
        HU2 = 0.0D+00
        G = 0.5D+00 / M
        D = G
        DO J = 1, M
            S = 0.0D+00
            DO K = 1, 30
                T1 = D + G * T (K)
                T2 = D - G * T (K)
                T3 = C / (1.0D+00-T1)
                T4 = C / (1.0D+00-T2)
                F1 = T3 * T3 / C * EXP (-X*T3) * T3 ** A1 * (1.0D+00+T3) ** B1
                F2 = T4 * T4 / C * EXP (-X*T4) * T4 ** A1 * (1.0D+00+T4) ** B1
                S = S + W (K) * (F1+F2)
            END DO
            HU2 = HU2 + S * G
            D = D + 2.0D+00 * G
        END DO

        IF (ABS(1.0D+00-HU0/HU2) < 1.0D-07) THEN
            EXIT
        END IF

        HU0 = HU2

    END DO

    CALL GAMMA (A, GA)
    HU2 = HU2 / GA
    HU = HU1 + HU2

    RETURN
END

!******************************************************************************

SUBROUTINE CHGUL (A, B, X, HU, ID)

!******************************************************************************
!
!! CHGUL: confluent hypergeometric function U(a,b,x) for large argument X.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HU, the value of U(a,b,x).
!
!    Output, integer ID, the estimated number of
!    significant digits.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) AA
    REAL (KIND=RK) B
    REAL (KIND=RK) HU
    INTEGER ID
    LOGICAL IL1
    LOGICAL IL2
    INTEGER K
    INTEGER NM
    REAL (KIND=RK) R
    REAL (KIND=RK) RA
    REAL (KIND=RK) R0
    REAL (KIND=RK) X

    ID = - 100
    AA = A - B + 1.0D+00
    IL1 = (A == INT(A)) .AND. (A <= 0.0D+00)
    IL2 = (AA == INT(AA)) .AND. (AA <= 0.0D+00)

    IF (IL1 .OR. IL2) THEN

        IF (IL1) THEN
            NM = INT (ABS(A))
        END IF

        IF (IL2) THEN
            NM = INT (ABS(AA))
        END IF

        HU = 1.0D+00
        R = 1.0D+00
        DO K = 1, NM
            R = - R * (A+K-1.0D+00) * (A-B+K) / (K*X)
            HU = HU + R
        END DO
        HU = X ** (-A) * HU
        ID = 10

    ELSE

        HU = 1.0D+00
        R = 1.0D+00
        DO K = 1, 25
            R = - R * (A+K-1.0D+00) * (A-B+K) / (K*X)
            RA = ABS (R)
            IF ((5 < K .AND. R0 <= RA) .OR. RA < 1.0D-15) THEN
                EXIT
            END IF
            R0 = RA
            HU = HU + R
        END DO

        ID = INT (ABS(LOG10(RA)))
        HU = X ** (-A) * HU

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CHGUS (A, B, X, HU, ID)

!******************************************************************************
!
!! CHGUS: confluent hypergeometric function U(a,b,x) for small argument X.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    27 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, B, parameters.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) HU, U(a,b,x).
!
!    Output, integer ID, the estimated number of
!    significant digits.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) D1
    REAL (KIND=RK) D2
    REAL (KIND=RK) GA
    REAL (KIND=RK) GAB
    REAL (KIND=RK) GB
    REAL (KIND=RK) GB2
    REAL (KIND=RK) H0
    REAL (KIND=RK) HMAX
    REAL (KIND=RK) HMIN
    REAL (KIND=RK) HU
    REAL (KIND=RK) HU0
    REAL (KIND=RK) HUA
    INTEGER ID
    INTEGER J
    REAL (KIND=RK) PI
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) X
    REAL (KIND=RK) XG1
    REAL (KIND=RK) XG2

    ID = - 100
    PI = 3.141592653589793D+00
    CALL GAMMA (A, GA)
    CALL GAMMA (B, GB)
    XG1 = 1.0D+00 + A - B
    CALL GAMMA (XG1, GAB)
    XG2 = 2.0D+00 - B
    CALL GAMMA (XG2, GB2)
    HU0 = PI / SIN (PI*B)
    R1 = HU0 / (GAB*GB)
    R2 = HU0 * X ** (1.0D+00-B) / (GA*GB2)
    HU = R1 - R2
    HMAX = 0.0D+00
    HMIN = 1.0D+300
    DO J = 1, 150
        R1 = R1 * (A+J-1.0D+00) / (J*(B+J-1.0D+00)) * X
        R2 = R2 * (A-B+J) / (J*(1.0D+00-B+J)) * X
        HU = HU + R1 - R2
        HUA = ABS (HU)
        HMAX = MAX (HMAX, HUA)
        HMIN = MIN (HMIN, HUA)
        IF (ABS(HU-H0) < ABS(HU)*1.0D-15) THEN
            EXIT
        END IF
        H0 = HU
    END DO

    D1 = LOG10 (HMAX)
    IF (HMIN /= 0.0D+00) THEN
        D2 = LOG10 (HMIN)
    END IF
    ID = 15 - INT (ABS(D1-D2))

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   13: PARABOLIC CYLINDER FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE PBDV (V, X, DV, DP, PDF, PDD)

!******************************************************************************
!
!! PBDV computes parabolic cylinder functions Dv(x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) DV(0:*), DP(0:*), the values of
!    Dn+v0(x), Dn+v0'(x).
!
!    Output, real ( kind = rk ) PDF, PDD, the values of Dv(x) and Dv'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) DP (0:*)
    REAL (KIND=RK) DV (0:*)
    REAL (KIND=RK) EP
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER JA
    INTEGER K
    INTEGER L
    INTEGER M
    INTEGER NA
    INTEGER NK
    INTEGER NV
    REAL (KIND=RK) PD
    REAL (KIND=RK) PD0
    REAL (KIND=RK) PD1
    REAL (KIND=RK) PDD
    REAL (KIND=RK) PDF
    REAL (KIND=RK) S0
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) V1
    REAL (KIND=RK) V2
    REAL (KIND=RK) VH
    REAL (KIND=RK) X
    REAL (KIND=RK) XA

    XA = ABS (X)
    VH = V
    V = V + SIGN (1.0D+00, V)
    NV = INT (V)
    V0 = V - NV
    NA = ABS (NV)
    EP = EXP (-0.25D+00*X*X)

    IF (1 <= NA) THEN
        JA = 1
    END IF

    IF (0.0D+00 <= V) THEN
        IF (V0 == 0.0D+00) THEN
            PD0 = EP
            PD1 = X * EP
        ELSE
            DO L = 0, JA
                V1 = V0 + L
                IF (XA <= 5.8D+00) THEN
                    CALL DVSA (V1, X, PD1)
                ELSE
                    CALL DVLA (V1, X, PD1)
                END IF
                IF (L == 0) THEN
                    PD0 = PD1
                END IF
            END DO
        END IF

        DV (0) = PD0
        DV (1) = PD1
        DO K = 2, NA
            PDF = X * PD1 - (K+V0-1.0D+00) * PD0
            DV (K) = PDF
            PD0 = PD1
            PD1 = PDF
        END DO

    ELSE

        IF (X <= 0.0D+00) THEN

            IF (XA <= 5.8D+00) THEN
                CALL DVSA (V0, X, PD0)
                V1 = V0 - 1.0D+00
                CALL DVSA (V1, X, PD1)
            ELSE
                CALL DVLA (V0, X, PD0)
                V1 = V0 - 1.0D+00
                CALL DVLA (V1, X, PD1)
            END IF

            DV (0) = PD0
            DV (1) = PD1
            DO K = 2, NA
                PD = (-X*PD1+PD0) / (K-1.0D+00-V0)
                DV (K) = PD
                PD0 = PD1
                PD1 = PD
            END DO

        ELSE IF (X <= 2.0D+00) THEN

            V2 = NV + V0
            IF (NV == 0) THEN
                V2 = V2 - 1.0D+00
            END IF

            NK = INT (-V2)
            CALL DVSA (V2, X, F1)
            V1 = V2 + 1.0D+00
            CALL DVSA (V1, X, F0)
            DV (NK) = F1
            DV (NK-1) = F0
            DO K = NK - 2, 0, - 1
                F = X * F0 + (K-V0+1.0D+00) * F1
                DV (K) = F
                F1 = F0
                F0 = F
            END DO

        ELSE

            IF (XA <= 5.8D+00) THEN
                CALL DVSA (V0, X, PD0)
            ELSE
                CALL DVLA (V0, X, PD0)
            END IF

            DV (0) = PD0
            M = 100 + NA
            F1 = 0.0D+00
            F0 = 1.0D-30
            DO K = M, 0, - 1
                F = X * F0 + (K-V0+1.0D+00) * F1
                IF (K <= NA) THEN
                    DV (K) = F
                END IF
                F1 = F0
                F0 = F
            END DO
            S0 = PD0 / F
            DO K = 0, NA
                DV (K) = S0 * DV (K)
            END DO

        END IF

    END IF

    DO K = 0, NA - 1
        V1 = ABS (V0) + K
        IF (0.0D+00 <= V) THEN
            DP (K) = 0.5D+00 * X * DV (K) - DV (K+1)
        ELSE
            DP (K) = - 0.5D+00 * X * DV (K) - V1 * DV (K+1)
        END IF
    END DO

    PDF = DV (NA-1)
    PDD = DP (NA-1)
    V = VH

    RETURN
END

!******************************************************************************

SUBROUTINE PBVV (V, X, VV, VP, PVF, PVD)

!******************************************************************************
!
!! PBVV computes parabolic cylinder functions Vv(x) and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) V, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) VV(0:*), VP(0:*), the values of Vv(x), Vv'(x).
!
!    Output, real ( kind = rk ) PVF, PVD, the values of Vv(x) and Vv'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER JA
    INTEGER K
    INTEGER KV
    INTEGER L
    INTEGER M
    INTEGER NA
    INTEGER NV
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV0
    REAL (KIND=RK) PVD
    REAL (KIND=RK) PVF
    REAL (KIND=RK) Q2P
    REAL (KIND=RK) QE
    REAL (KIND=RK) S0
    REAL (KIND=RK) V
    REAL (KIND=RK) V0
    REAL (KIND=RK) V1
    REAL (KIND=RK) V2
    REAL (KIND=RK) VH
    REAL (KIND=RK) VP (0:*)
    REAL (KIND=RK) VV (0:*)
    REAL (KIND=RK) X
    REAL (KIND=RK) XA

    PI = 3.141592653589793D+00
    XA = ABS (X)
    VH = V
    V = V + SIGN (1.0D+00, V)
    NV = INT (V)
    V0 = V - NV
    NA = ABS (NV)
    QE = EXP (0.25D+00*X*X)
    Q2P = SQRT (2.0D+00/PI)

    IF (1 <= NA) THEN
        JA = 1
    END IF

    IF (V <= 0.0D+00) THEN

        IF (V0 == 0.0D+00) THEN

            IF (XA <= 7.5D+00) THEN
                CALL VVSA (V0, X, PV0)
            ELSE
                CALL VVLA (V0, X, PV0)
            END IF

            F0 = Q2P * QE
            F1 = X * F0
            VV (0) = PV0
            VV (1) = F0
            VV (2) = F1

        ELSE

            DO L = 0, JA
                V1 = V0 - L
                IF (XA <= 7.5D+00) THEN
                    CALL VVSA (V1, X, F1)
                ELSE
                    CALL VVLA (V1, X, F1)
                END IF
                IF (L == 0) THEN
                    F0 = F1
                END IF
            END DO

            VV (0) = F0
            VV (1) = F1

        END IF

        IF (V0 == 0.0D+00) THEN
            KV = 3
        ELSE
            KV = 2
        END IF

        DO K = KV, NA
            F = X * F1 + (K-V0-2.0D+00) * F0
            VV (K) = F
            F0 = F1
            F1 = F
        END DO

    ELSE

        IF (0.0D+00 <= X .AND. X <= 7.5D+00) THEN

            V2 = V
            IF (V2 < 1.0D+00) THEN
                V2 = V2 + 1.0D+00
            END IF

            CALL VVSA (V2, X, F1)
            V1 = V2 - 1.0D+00
            KV = INT (V2)
            CALL VVSA (V1, X, F0)
            VV (KV) = F1
            VV (KV-1) = F0
            DO K = KV - 2, 0, - 1
                F = X * F0 - (K+V0+2.0D+00) * F1
                IF (K <= NA) THEN
                    VV (K) = F
                END IF
                F1 = F0
                F0 = F
            END DO

        ELSE IF (7.5D+00 < X) THEN

            CALL VVLA (V0, X, PV0)
            M = 100 + ABS (NA)
            VV (1) = PV0
            F1 = 0.0D+00
            F0 = 1.0D-40
            DO K = M, 0, - 1
                F = X * F0 - (K+V0+2.0D+00) * F1
                IF (K <= NA) THEN
                    VV (K) = F
                END IF
                F1 = F0
                F0 = F
            END DO
            S0 = PV0 / F
            DO K = 0, NA
                VV (K) = S0 * VV (K)
            END DO

        ELSE

            IF (XA <= 7.5D+00) THEN
                CALL VVSA (V0, X, F0)
                V1 = V0 + 1.0D+00
                CALL VVSA (V1, X, F1)
            ELSE
                CALL VVLA (V0, X, F0)
                V1 = V0 + 1.0D+00
                CALL VVLA (V1, X, F1)
            END IF

            VV (0) = F0
            VV (1) = F1
            DO K = 2, NA
                F = (X*F1-F0) / (K+V0)
                VV (K) = F
                F0 = F1
                F1 = F
            END DO

        END IF

    END IF

    DO K = 0, NA - 1
        V1 = V0 + K
        IF (0.0D+00 <= V) THEN
            VP (K) = 0.5D+00 * X * VV (K) - (V1+1.0D+00) * VV (K+1)
        ELSE
            VP (K) = - 0.5D+00 * X * VV (K) + VV (K+1)
        END IF
    END DO

    PVF = VV (NA-1)
    PVD = VP (NA-1)
    V = VH

    RETURN
END

!******************************************************************************

SUBROUTINE PBWA (A, X, W1F, W1D, W2F, W2D)

!******************************************************************************
!
!! PBWA computes parabolic cylinder functions W(a,x) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) A, the parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) W1F, W1D, W2F, W2D, the values of
!    W(a,x), W'(a,x), W(a,-x), W'(a,-x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) D (100)
    REAL (KIND=RK) D1
    REAL (KIND=RK) D2
    REAL (KIND=RK) DL
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) G1
    REAL (KIND=RK) G2
    REAL (KIND=RK) H (100)
    REAL (KIND=RK) H0
    REAL (KIND=RK) H1
    REAL (KIND=RK) HL
    INTEGER K
    INTEGER L1
    INTEGER L2
    INTEGER M
    REAL (KIND=RK) P0
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) UGI
    REAL (KIND=RK) UGR
    REAL (KIND=RK) VGI
    REAL (KIND=RK) VGR
    REAL (KIND=RK) W1D
    REAL (KIND=RK) W1F
    REAL (KIND=RK) W2D
    REAL (KIND=RK) W2F
    REAL (KIND=RK) X
    REAL (KIND=RK) X1
    REAL (KIND=RK) X2
    REAL (KIND=RK) Y1
    REAL (KIND=RK) Y1D
    REAL (KIND=RK) Y1F
    REAL (KIND=RK) Y2D
    REAL (KIND=RK) Y2F

    EPS = 1.0D-15
    P0 = 0.59460355750136D+00

    IF (A == 0.0D+00) THEN
        G1 = 3.625609908222D+00
        G2 = 1.225416702465D+00
    ELSE
        X1 = 0.25D+00
        Y1 = 0.5D+00 * A
        CALL CGAMA (X1, Y1, 1, UGR, UGI)
        G1 = SQRT (UGR*UGR+UGI*UGI)
        X2 = 0.75D+00
        CALL CGAMA (X2, Y1, 1, VGR, VGI)
        G2 = SQRT (VGR*VGR+VGI*VGI)
    END IF

    F1 = SQRT (G1/G2)
    F2 = SQRT (2.0D+00*G2/G1)
    H0 = 1.0D+00
    H1 = A
    H (1) = A
    DO L1 = 4, 200, 2
        M = L1 / 2
        HL = A * H1 - 0.25D+00 * (L1-2.0D+00) * (L1-3.0D+00) * H0
        H (M) = HL
        H0 = H1
        H1 = HL
    END DO
    Y1F = 1.0D+00
    R = 1.0D+00
    DO K = 1, 100
        R = 0.5D+00 * R * X * X / (K*(2.0D+00*K-1.0D+00))
        R1 = H (K) * R
        Y1F = Y1F + R1
        IF (ABS(R1/Y1F) <= EPS .AND. 30 < K) THEN
            EXIT
        END IF
    END DO

    Y1D = A
    R = 1.0D+00
    DO K = 1, 100
        R = 0.5D+00 * R * X * X / (K*(2.0D+00*K+1.0D+00))
        R1 = H (K+1) * R
        Y1D = Y1D + R1
        IF (ABS(R1/Y1D) <= EPS .AND. 30 < K) THEN
            EXIT
        END IF
    END DO

    Y1D = X * Y1D
    D1 = 1.0D+00
    D2 = A
    D (1) = 1.0D+00
    D (2) = A
    DO L2 = 5, 160, 2
        M = (L2+1) / 2
        DL = A * D2 - 0.25D+00 * (L2-2.0D+00) * (L2-3.0D+00) * D1
        D (M) = DL
        D1 = D2
        D2 = DL
    END DO

    Y2F = 1.0D+00
    R = 1.0D+00
    DO K = 1, 100
        R = 0.5D+00 * R * X * X / (K*(2.0D+00*K+1.0D+00))
        R1 = D (K+1) * R
        Y2F = Y2F + R1
        IF (ABS(R1/Y2F) <= EPS .AND. 30 < K) THEN
            EXIT
        END IF
    END DO

    Y2F = X * Y2F
    Y2D = 1.0D+00
    R = 1.0D+00
    DO K = 1, 100
        R = 0.5D+00 * R * X * X / (K*(2.0D+00*K-1.0D+00))
        R1 = D (K+1) * R
        Y2D = Y2D + R1
        IF (ABS(R1/Y2D) <= EPS .AND. 30 < K) THEN
            EXIT
        END IF
    END DO

    W1F = P0 * (F1*Y1F-F2*Y2F)
    W2F = P0 * (F1*Y1F+F2*Y2F)
    W1D = P0 * (F1*Y1D-F2*Y2D)
    W2D = P0 * (F1*Y1D+F2*Y2D)

    RETURN
END

!******************************************************************************

SUBROUTINE CPBDN (N, Z, CPB, CPD)

!******************************************************************************
!
!! CPBDN: parabolic cylinder function Dn(z) and Dn'(z) for complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CPB(0:N), CPD(0:N), the values of Dn(z)
!    and Dn'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) C0
    COMPLEX (KIND=CK) CA0
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CFA
    COMPLEX (KIND=CK) CFB
    COMPLEX (KIND=CK) CPB (0:N)
    COMPLEX (KIND=CK) CPD (0:N)
    COMPLEX (KIND=CK) CS0
    INTEGER K
    INTEGER M
    INTEGER N0
    INTEGER N1
    INTEGER NM1
    REAL (KIND=RK) PI
    REAL (KIND=RK) X
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1

    PI = 3.141592653589793D+00
    X = REAL (Z, KIND=RK)
    A0 = ABS (Z)
    C0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
    CA0 = EXP (-0.25D+00*Z*Z)

    IF (0 <= N) THEN

        CF0 = CA0
        CF1 = Z * CA0
        CPB (0) = CF0
        CPB (1) = CF1
        DO K = 2, N
            CF = Z * CF1 - (K-1.0D+00) * CF0
            CPB (K) = CF
            CF0 = CF1
            CF1 = CF
        END DO

    ELSE

        N0 = - N

        IF (X <= 0.0D+00 .OR. ABS(Z) == 0.0D+00) THEN

            CF0 = CA0
            CPB (0) = CF0
            Z1 = - Z
            IF (A0 <= 7.0D+00) THEN
                CALL CPDSA (-1, Z1, CF1)
            ELSE
                CALL CPDLA (-1, Z1, CF1)
            END IF
            CF1 = SQRT (2.0D+00*PI) / CA0 - CF1
            CPB (1) = CF1
            DO K = 2, N0
                CF = (-Z*CF1+CF0) / (K-1.0D+00)
                CPB (K) = CF
                CF0 = CF1
                CF1 = CF
            END DO

        ELSE

            IF (A0 <= 3.0D+00) THEN

                CALL CPDSA (-N0, Z, CFA)
                CPB (N0) = CFA
                N1 = N0 + 1
                CALL CPDSA (-N1, Z, CFB)
                CPB (N1) = CFB
                NM1 = N0 - 1
                DO K = NM1, 0, - 1
                    CF = Z * CFA + (K+1.0D+00) * CFB
                    CPB (K) = CF
                    CFB = CFA
                    CFA = CF
                END DO

            ELSE

                M = 100 + ABS (N)
                CFA = C0
                CFB = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
                DO K = M, 0, - 1
                    CF = Z * CFB + (K+1.0D+00) * CFA
                    IF (K <= N0) THEN
                        CPB (K) = CF
                    END IF
                    CFA = CFB
                    CFB = CF
                END DO
                CS0 = CA0 / CF
                DO K = 0, N0
                    CPB (K) = CS0 * CPB (K)
                END DO

            END IF

        END IF

    END IF

    CPD (0) = - 0.5D+00 * Z * CPB (0)

    IF (0 <= N) THEN
        DO K = 1, N
            CPD (K) = - 0.5D+00 * Z * CPB (K) + K * CPB (K-1)
        END DO
    ELSE
        DO K = 1, N0
            CPD (K) = 0.5D+00 * Z * CPB (K) - CPB (K-1)
        END DO
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CPDLA (N, Z, CDN)

!****************************************************************************80
!
!! CPDLA computes complex parabolic cylinder function Dn(z) for large argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CDN, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    COMPLEX (KIND=CK) CB0
    COMPLEX (KIND=CK) CDN
    COMPLEX (KIND=CK) CR
    INTEGER K
    INTEGER N
    COMPLEX (KIND=CK) Z

    CB0 = Z ** N * EXP (-0.25D+00*Z*Z)
    CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
    CDN = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

    DO K = 1, 16

        CR = - 0.5D+00 * CR * (2.0D+00*K-N-1.0D+00) * (2.0D+00*K-N-2.0D+00) / (K*Z*Z)

        CDN = CDN + CR

        IF (ABS(CR) < ABS(CDN)*1.0D-12) THEN
            EXIT
        END IF

    END DO

    CDN = CB0 * CDN

    RETURN
END

!******************************************************************************

SUBROUTINE CPDSA (N, Z, CDN)

!******************************************************************************
!
!! CPDSA computes complex parabolic cylinder function Dn(z) for small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CDN, the value of DN(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    COMPLEX (KIND=CK) CA0
    COMPLEX (KIND=CK) CB0
    COMPLEX (KIND=CK) CDN
    COMPLEX (KIND=CK) CDW
    COMPLEX (KIND=CK) CR
    REAL (KIND=RK) EPS
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    REAL (KIND=RK) GA0
    REAL (KIND=RK) GM
    INTEGER M
    INTEGER N
    REAL (KIND=RK) PD
    REAL (KIND=RK) PI
    REAL (KIND=RK) SQ2
    REAL (KIND=RK) VA0
    REAL (KIND=RK) VM
    REAL (KIND=RK) VT
    REAL (KIND=RK) XN
    COMPLEX (KIND=CK) Z

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    SQ2 = SQRT (2.0D+00)
    CA0 = EXP (-0.25D+00*Z*Z)
    VA0 = 0.5D+00 * (1.0D+00-N)

    IF (N == 0) THEN

        CDN = CA0

    ELSE

        IF (ABS(Z) == 0.0D+00) THEN

            IF (VA0 <= 0.0D+00 .AND. VA0 == INT(VA0)) THEN
                CDN = 0.0D+00
            ELSE
                CALL GAIH (VA0, GA0)
                PD = SQRT (PI) / (2.0D+00**(-0.5D+00*N)*GA0)
                CDN = CMPLX (PD, 0.0D+00, KIND=CK)
            END IF

        ELSE

            XN = - N
            CALL GAIH (XN, G1)
            CB0 = 2.0D+00 ** (-0.5D+00*N-1.0D+00) * CA0 / G1
            VT = - 0.5D+00 * N
            CALL GAIH (VT, G0)
            CDN = CMPLX (G0, 0.0D+00, KIND=CK)
            CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)

            DO M = 1, 250
                VM = 0.5D+00 * (M-N)
                CALL GAIH (VM, GM)
                CR = - CR * SQ2 * Z / M
                CDW = GM * CR
                CDN = CDN + CDW
                IF (ABS(CDW) < ABS(CDN)*EPS) THEN
                    EXIT
                END IF
            END DO

            CDN = CB0 * CDN

        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE DVLA (VA, X, PD)

!******************************************************************************
!
!! DVLA computes parabolic cylinder functions Dv(x) for large argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    06 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) VA, the order.
!
!    Output, real ( kind = rk ) PD, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) EP
    REAL (KIND=RK) EPS
    REAL (KIND=RK) GL
    INTEGER K
    REAL (KIND=RK) PD
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) VA
    REAL (KIND=RK) VL
    REAL (KIND=RK) X
    REAL (KIND=RK) X1

    PI = 3.141592653589793D+00
    EPS = 1.0D-12
    EP = EXP (-0.25D+00*X*X)
    A0 = ABS (X) ** VA * EP
    R = 1.0D+00
    PD = 1.0D+00
    DO K = 1, 16
        R = - 0.5D+00 * R * (2.0D+00*K-VA-1.0D+00) * (2.0D+00*K-VA-2.0D+00) / (K*X*X)
        PD = PD + R
        IF (ABS(R/PD) < EPS) THEN
            EXIT
        END IF
    END DO

    PD = A0 * PD

    IF (X < 0.0D+00) THEN
        X1 = - X
        CALL VVLA (VA, X1, VL)
        CALL GAMMA (-VA, GL)
        PD = PI * VL / GL + COS (PI*VA) * PD
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE DVSA (VA, X, PD)

!******************************************************************************
!
!! DVSA computes parabolic cylinder functions Dv(x) for small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) VA, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PD, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) EP
    REAL (KIND=RK) EPS
    REAL (KIND=RK) G0
    REAL (KIND=RK) G1
    REAL (KIND=RK) GA0
    REAL (KIND=RK) GM
    INTEGER M
    REAL (KIND=RK) PD
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) SQ2
    REAL (KIND=RK) VA
    REAL (KIND=RK) VA0
    REAL (KIND=RK) VM
    REAL (KIND=RK) VT
    REAL (KIND=RK) X

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    SQ2 = SQRT (2.0D+00)
    EP = EXP (-0.25D+00*X*X)
    VA0 = 0.5D+00 * (1.0D+00-VA)

    IF (VA == 0.0D+00) THEN

        PD = EP

    ELSE

        IF (X == 0.0D+00) THEN
            IF (VA0 <= 0.0D+00 .AND. VA0 == INT(VA0)) THEN
                PD = 0.0D+00
            ELSE
                CALL GAMMA (VA0, GA0)
                PD = SQRT (PI) / (2.0D+00**(-0.5D+00*VA)*GA0)
            END IF

        ELSE

            CALL GAMMA (-VA, G1)
            A0 = 2.0D+00 ** (-0.5D+00*VA-1.0D+00) * EP / G1
            VT = - 0.5D+00 * VA
            CALL GAMMA (VT, G0)
            PD = G0
            R = 1.0D+00
            DO M = 1, 250
                VM = 0.5D+00 * (M-VA)
                CALL GAMMA (VM, GM)
                R = - R * SQ2 * X / M
                R1 = GM * R
                PD = PD + R1
                IF (ABS(R1) < ABS(PD)*EPS) THEN
                    EXIT
                END IF
            END DO

            PD = A0 * PD

        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE GAIH (X, GA)

!******************************************************************************
!
!! GAIH computes the GammaH function.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    09 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) GA, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) GA
    INTEGER K
    INTEGER M
    INTEGER M1
    REAL (KIND=RK) PI
    REAL (KIND=RK) X

    PI = 3.141592653589793D+00

    IF (X == INT(X) .AND. 0.0 < X) THEN
        GA = 1.0D+00
        M1 = INT (X-1.0D+00)
        DO K = 2, M1
            GA = GA * K
        END DO
    ELSE IF (X+0.5D+00 == INT(X+0.5D+00) .AND. 0.0D+00 < X) THEN
        M = INT (X)
        GA = SQRT (PI)
        DO K = 1, M
            GA = 0.5D+00 * GA * (2.0D+00*K-1.0D+00)
        END DO
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE VVLA (VA, X, PV)

!******************************************************************************
!
!! VVLA computes parabolic cylinder function Vv(x) for large arguments.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    04 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) VA, the order nu.
!
!    Output, real ( kind = rk ) PV, the value of V(nu,x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) DSL
    REAL (KIND=RK) EPS
    REAL (KIND=RK) GL
    INTEGER K
    REAL (KIND=RK) PDL
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV
    REAL (KIND=RK) QE
    REAL (KIND=RK) R
    REAL (KIND=RK) VA
    REAL (KIND=RK) X
    REAL (KIND=RK) X1

    PI = 3.141592653589793D+00
    EPS = 1.0D-12
    QE = EXP (0.25D+00*X*X)
    A0 = ABS (X) ** (-VA-1.0D+00) * SQRT (2.0D+00/PI) * QE

    R = 1.0D+00
    PV = 1.0D+00
    DO K = 1, 18
        R = 0.5D+00 * R * (2.0D+00*K+VA-1.0D+00) * (2.0D+00*K+VA) / (K*X*X)
        PV = PV + R
        IF (ABS(R/PV) < EPS) THEN
            EXIT
        END IF
    END DO

    PV = A0 * PV

    IF (X < 0.0D+00) THEN
        X1 = - X
        CALL DVLA (VA, X1, PDL)
        CALL GAMMA (-VA, GL)
        DSL = SIN (PI*VA) * SIN (PI*VA)
        PV = DSL * GL / PI * PDL - COS (PI*VA) * PV
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE VVSA (VA, X, PV)

!******************************************************************************
!
!! VVSA computes parabolic cylinder function V(nu,x) for small arguments.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    04 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) VA, the order nu.
!
!    Output, real ( kind = rk ) PV, the value of V(nu,x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) EP
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FAC
    REAL (KIND=RK) G1
    REAL (KIND=RK) GA0
    REAL (KIND=RK) GM
    REAL (KIND=RK) GW
    INTEGER M
    REAL (KIND=RK) PI
    REAL (KIND=RK) PV
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) SQ2
    REAL (KIND=RK) SV
    REAL (KIND=RK) SV0
    REAL (KIND=RK) V1
    REAL (KIND=RK) VA
    REAL (KIND=RK) VA0
    REAL (KIND=RK) VB0
    REAL (KIND=RK) VM
    REAL (KIND=RK) X

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    EP = EXP (-0.25D+00*X*X)
    VA0 = 1.0D+00 + 0.5D+00 * VA

    IF (X == 0.0D+00) THEN

        IF ((VA0 <= 0.0D+00 .AND. VA0 == INT(VA0)) .OR. VA == 0.0D+00) THEN
            PV = 0.0D+00
        ELSE
            VB0 = - 0.5D+00 * VA
            SV0 = SIN (VA0*PI)
            CALL GAMMA (VA0, GA0)
            PV = 2.0D+00 ** VB0 * SV0 / GA0
        END IF

    ELSE

        SQ2 = SQRT (2.0D+00)
        A0 = 2.0D+00 ** (-0.5D+00*VA) * EP / (2.0D+00*PI)
        SV = SIN (-(VA+0.5D+00)*PI)
        V1 = - 0.5D+00 * VA
        CALL GAMMA (V1, G1)
        PV = (SV+1.0D+00) * G1
        R = 1.0D+00
        FAC = 1.0D+00

        DO M = 1, 250
            VM = 0.5D+00 * (M-VA)
            CALL GAMMA (VM, GM)
            R = R * SQ2 * X / M
            FAC = - FAC
            GW = FAC * SV + 1.0D+00
            R1 = GW * R * GM
            PV = PV + R1
            IF (ABS(R1/PV) < EPS .AND. GW /= 0.0D+00) THEN
                EXIT
            END IF
        END DO

        PV = A0 * PV

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   14: MATHIEU FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE CVA1 (KD, M, Q, CV)

!******************************************************************************
!
!! CVA1 computes a sequence of characteristic values of Mathieu functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code.
!    1, for cem(x,q)  ( m = 0,2,4, )
!    2, for cem(x,q)  ( m = 1,3,5, )
!    3, for sem(x,q)  ( m = 1,3,5, )
!    4, for sem(x,q)  ( m = 2,4,6, )
!
!    Input, integer M, the maximum order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Output, real ( kind = rk ) CV(*), characteristic values.
!    For KD = 1, CV(1), CV(2), CV(3),..., correspond to
!    the characteristic values of cem for m = 0,2,4,...
!    For KD = 2, CV(1), CV(2), CV(3),..., correspond to
!    the characteristic values of cem for m = 1,3,5,...
!    For KD = 3, CV(1), CV(2), CV(3),..., correspond to
!    the characteristic values of sem for m = 1,3,5,...
!    For KD = 4, CV(1), CV(2), CV(3),..., correspond to
!    the characteristic values of sem for m = 0,2,4,...
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) CV (200)
    REAL (KIND=RK) D (500)
    REAL (KIND=RK) E (500)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F (500)
    REAL (KIND=RK) G (200)
    REAL (KIND=RK) H (200)
    INTEGER I
    INTEGER IC
    INTEGER ICM
    INTEGER J
    INTEGER K
    INTEGER K1
    INTEGER KD
    INTEGER M
    INTEGER NM
    INTEGER NM1
    REAL (KIND=RK) Q
    REAL (KIND=RK) S
    REAL (KIND=RK) T
    REAL (KIND=RK) T1
    REAL (KIND=RK) X1
    REAL (KIND=RK) XA
    REAL (KIND=RK) XB

    EPS = 1.0D-14

    IF (KD == 4) THEN
        ICM = M / 2
    ELSE
        ICM = INT (M/2) + 1
    END IF

    IF (Q == 0.0D+00) THEN

        IF (KD == 1) THEN
            DO IC = 1, ICM
                CV (IC) = 4.0D+00 * (IC-1.0D+00) ** 2
            END DO
        ELSE IF (KD /= 4) THEN
            DO IC = 1, ICM
                CV (IC) = (2.0D+00*IC-1.0D+00) ** 2
            END DO
        ELSE
            DO IC = 1, ICM
                CV (IC) = 4.0D+00 * IC * IC
            END DO
        END IF

    ELSE

        NM = INT (10D+00+1.5D+00*M+0.5D+00*Q)
        E (1) = 0.0D+00
        F (1) = 0.0D+00

        IF (KD == 1) THEN

            D (1) = 0.0D+00
            DO I = 2, NM
                D (I) = 4.0D+00 * (I-1.0D+00) ** 2
                E (I) = Q
                F (I) = Q * Q
            END DO
            E (2) = SQRT (2.0D+00) * Q
            F (2) = 2.0D+00 * Q * Q

        ELSE IF (KD /= 4) THEN

            D (1) = 1.0D+00 + (-1.0D+00) ** KD * Q
            DO I = 2, NM
                D (I) = (2.0D+00*I-1.0D+00) ** 2
                E (I) = Q
                F (I) = Q * Q
            END DO

        ELSE

            D (1) = 4.0D+00
            DO I = 2, NM
                D (I) = 4.0D+00 * I * I
                E (I) = Q
                F (I) = Q * Q
            END DO

        END IF

        XA = D (NM) + ABS (E(NM))
        XB = D (NM) - ABS (E(NM))

        NM1 = NM - 1
        DO I = 1, NM1
            T = ABS (E(I)) + ABS (E(I+1))
            T1 = D (I) + T
            XA = MAX (XA, T1)
            T1 = D (I) - T
            XB = MIN (XB, T1)
        END DO

        DO I = 1, ICM
            G (I) = XA
            H (I) = XB
        END DO

        DO K = 1, ICM

            DO K1 = K, ICM
                IF (G(K1) < G(K)) THEN
                    G (K) = G (K1)
                    EXIT
                END IF
            END DO

            IF (K /= 1 .AND. H(K) < H(K-1)) THEN
                H (K) = H (K-1)
            END IF

            DO

                X1 = (G(K)+H(K)) / 2.0D+00
                CV (K) = X1

                IF (ABS((G(K)-H(K))/X1) < EPS) THEN
                    EXIT
                END IF

                J = 0
                S = 1.0D+00
                DO I = 1, NM
                    IF (S == 0.0D+00) THEN
                        S = S + 1.0D-30
                    END IF
                    T = F (I) / S
                    S = D (I) - T - X1
                    IF (S < 0.0D+00) THEN
                        J = J + 1
                    END IF
                END DO

                IF (J < K) THEN
                    H (K) = X1
                ELSE
                    G (K) = X1
                    IF (ICM <= J) THEN
                        G (ICM) = X1
                    ELSE
                        H (J+1) = MAX (H(J+1), X1)
                        G (J) = MIN (G(J), X1)
                    END IF
                END IF

            END DO

            CV (K) = X1

        END DO

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CVA2 (KD, M, Q, A)

!******************************************************************************
!
!! CVA2 computes a specific characteristic value of Mathieu functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code:
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Output, real ( kind = rk ) A, the characteristic value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A1
    REAL (KIND=RK) A2
    REAL (KIND=RK) DELTA
    INTEGER I
    INTEGER IFLAG
    INTEGER KD
    INTEGER M
    INTEGER NDIV
    INTEGER NN
    REAL (KIND=RK) Q
    REAL (KIND=RK) Q1
    REAL (KIND=RK) Q2
    REAL (KIND=RK) QQ

    IF (M <= 12 .OR. Q <= 3.0D+00*M .OR. M*M < Q) THEN

        CALL CV0 (KD, M, Q, A)

        IF (Q /= 0.0D+00) THEN
            CALL REFINE (KD, M, Q, A, 1)
        END IF

    ELSE

        NDIV = 10
        DELTA = (M-3.0D+00) * M / REAL (NDIV, KIND=RK)

        IF ((Q-3.0D+00*M) <= (M*M-Q)) THEN

            DO

                NN = INT ((Q-3.0D+00*M)/DELTA) + 1
                DELTA = (Q-3.0D+00*M) / NN
                Q1 = 2.0D+00 * M
                CALL CVQM (M, Q1, A1)
                Q2 = 3.0D+00 * M
                CALL CVQM (M, Q2, A2)
                QQ = 3.0D+00 * M

                DO I = 1, NN

                    QQ = QQ + DELTA
                    A = (A1*Q2-A2*Q1+(A2-A1)*QQ) / (Q2-Q1)

                    IF (I == NN) THEN
                        IFLAG = - 1
                    ELSE
                        IFLAG = 1
                    END IF

                    CALL REFINE (KD, M, QQ, A, IFLAG)
                    Q1 = Q2
                    Q2 = QQ
                    A1 = A2
                    A2 = A

                END DO

                IF (IFLAG /=-10) THEN
                    EXIT
                END IF

                NDIV = NDIV * 2
                DELTA = (M-3.0D+00) * M / REAL (NDIV, KIND=RK)

            END DO

        ELSE

            DO

                NN = INT ((M*M-Q)/DELTA) + 1
                DELTA = (M*M-Q) / NN
                Q1 = M * (M-1.0D+00)
                CALL CVQL (KD, M, Q1, A1)
                Q2 = M * M
                CALL CVQL (KD, M, Q2, A2)
                QQ = M * M

                DO I = 1, NN

                    QQ = QQ - DELTA
                    A = (A1*Q2-A2*Q1+(A2-A1)*QQ) / (Q2-Q1)

                    IF (I == NN) THEN
                        IFLAG = - 1
                    ELSE
                        IFLAG = 1
                    END IF

                    CALL REFINE (KD, M, QQ, A, IFLAG)
                    Q1 = Q2
                    Q2 = QQ
                    A1 = A2
                    A2 = A

                END DO

                IF (IFLAG /=-10) THEN
                    EXIT
                END IF

                NDIV = NDIV * 2
                DELTA = (M-3.0D+00) * M / REAL (NDIV, KIND=RK)

            END DO

        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE FCOEF (KD, M, Q, A, FC)

!******************************************************************************
!
!! FCOEF: expansion coefficients for Mathieu and modified Mathieu functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    01 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code.
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the Mathieu function.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Input, real ( kind = rk ) A, the characteristic value of the Mathieu
!    functions for given m and q.
!
!    Output, real ( kind = rk ) FC(*), the expansion coefficients of Mathieu
!    functions ( k =  1,2,...,KM ).  FC(1),FC(2),FC(3),... correspond to
!    A0,A2,A4,... for KD = 1 case,
!    A1,A3,A5,... for KD = 2 case,
!    B1,B3,B5,... for KD = 3 case,
!    B2,B4,B6,... for KD = 4 case.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) F
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) F3
    REAL (KIND=RK) FC (251)
    INTEGER I
    INTEGER J
    INTEGER K
    INTEGER KB
    INTEGER KD
    INTEGER KM
    INTEGER L
    INTEGER M
    REAL (KIND=RK) Q
    REAL (KIND=RK) QM
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) SP
    REAL (KIND=RK) SS
    REAL (KIND=RK) U
    REAL (KIND=RK) V

    IF (Q <= 1.0D+00) THEN
        QM = 7.5D+00 + 56.1D+00 * SQRT (Q) - 134.7D+00 * Q + 90.7D+00 * SQRT (Q) * Q
    ELSE
        QM = 17.0D+00 + 3.1D+00 * SQRT (Q) - 0.126D+00 * Q + 0.0037D+00 * SQRT (Q) * Q
    END IF

    KM = INT (QM+0.5D+00*M)

    IF (Q == 0.0D+00) THEN

        DO K = 1, KM
            FC (K) = 0.0D+00
        END DO

        IF (KD == 1) THEN
            FC ((M+2)/2) = 1.0D+00
            IF (M == 0) THEN
                FC (1) = 1.0D+00 / SQRT (2.0D+00)
            END IF
        ELSE IF (KD == 4) THEN
            FC (M/2) = 1.0D+00
        ELSE
            FC ((M+1)/2) = 1.0D+00
        END IF

        RETURN

    END IF

    KB = 0
    S = 0.0D+00
    F = 1.0D-100
    U = 0.0D+00
    FC (KM) = 0.0D+00

    IF (KD == 1) THEN

        L = 0

        DO K = KM, 3, - 1

            V = U
            U = F
            F = (A-4.0D+00*K*K) * U / Q - V

            IF (ABS(F) < ABS(FC(K+1))) THEN

                KB = K
                FC (1) = 1.0D-100
                SP = 0.0D+00
                F3 = FC (K+1)
                FC (2) = A / Q * FC (1)
                FC (3) = (A-4.0D+00) * FC (2) / Q - 2.0D+00 * FC (1)
                U = FC (2)
                F1 = FC (3)

                DO I = 3, KB
                    V = U
                    U = F1
                    F1 = (A-4.0D+00*(I-1.0D+00)**2) * U / Q - V
                    FC (I+1) = F1
                    IF (I == KB) THEN
                        F2 = F1
                    ELSE
                        SP = SP + F1 * F1
                    END IF
                END DO

                SP = SP + 2.0D+00 * FC (1) ** 2 + FC (2) ** 2 + FC (3) ** 2
                SS = S + SP * (F3/F2) ** 2
                S0 = SQRT (1.0D+00/SS)
                DO J = 1, KM
                    IF (J <= KB+1) THEN
                        FC (J) = S0 * FC (J) * F3 / F2
                    ELSE
                        FC (J) = S0 * FC (J)
                    END IF
                END DO
                L = 1
                EXIT
            ELSE
                FC (K) = F
                S = S + F * F
            END IF

        END DO

        IF (L == 0) THEN
            FC (2) = Q * FC (3) / (A-4.0D+00-2.0D+00*Q*Q/A)
            FC (1) = Q / A * FC (2)
            S = S + 2.0D+00 * FC (1) ** 2 + FC (2) ** 2
            S0 = SQRT (1.0D+00/S)
            DO K = 1, KM
                FC (K) = S0 * FC (K)
            END DO
        END IF

    ELSE IF (KD == 2 .OR. KD == 3) THEN

        L = 0

        DO K = KM, 3, - 1

            V = U
            U = F
            F = (A-(2.0D+00*K-1)**2) * U / Q - V

            IF (ABS(FC(K)) <= ABS(F)) THEN
                FC (K-1) = F
                S = S + F * F
            ELSE
                KB = K
                F3 = FC (K)
                L = 1
                EXIT
            END IF

        END DO

        IF (L == 0) THEN

            FC (1) = Q / (A-1.0D+00-(-1)**KD*Q) * FC (2)
            S = S + FC (1) * FC (1)
            S0 = SQRT (1.0D+00/S)
            DO K = 1, KM
                FC (K) = S0 * FC (K)
            END DO

        ELSE

            FC (1) = 1.0D-100
            FC (2) = (A-1.0D+00-(-1)**KD*Q) / Q * FC (1)
            SP = 0.0D+00
            U = FC (1)
            F1 = FC (2)
            DO I = 2, KB - 1
                V = U
                U = F1
                F1 = (A-(2.0D+00*I-1.0D+00)**2) * U / Q - V
                IF (I /= KB-1) THEN
                    FC (I+1) = F1
                    SP = SP + F1 * F1
                ELSE
                    F2 = F1
                END IF
            END DO

            SP = SP + FC (1) ** 2 + FC (2) ** 2
            SS = S + SP * (F3/F2) ** 2
            S0 = 1.0D+00 / SQRT (SS)
            DO J = 1, KM
                IF (J < KB) THEN
                    FC (J) = S0 * FC (J) * F3 / F2
                ELSE
                    FC (J) = S0 * FC (J)
                END IF
            END DO

        END IF

    ELSE IF (KD == 4) THEN

        L = 0

        DO K = KM, 3, - 1
            V = U
            U = F
            F = (A-4.0D+00*K*K) * U / Q - V
            IF (ABS(FC(K)) <= ABS(F)) THEN
                FC (K-1) = F
                S = S + F * F
            ELSE
                KB = K
                F3 = FC (K)
                L = 1
                EXIT
            END IF
        END DO

        IF (L == 0) THEN

            FC (1) = Q / (A-4.0D+00) * FC (2)
            S = S + FC (1) * FC (1)
            S0 = SQRT (1.0D+00/S)
            DO K = 1, KM
                FC (K) = S0 * FC (K)
            END DO

        ELSE

            FC (1) = 1.0D-100
            FC (2) = (A-4.0D+00) / Q * FC (1)
            SP = 0.0D+00
            U = FC (1)
            F1 = FC (2)

            DO I = 2, KB - 1
                V = U
                U = F1
                F1 = (A-4.0D+00*I*I) * U / Q - V
                IF (I /= KB-1) THEN
                    FC (I+1) = F1
                    SP = SP + F1 * F1
                ELSE
                    F2 = F1
                END IF
            END DO

            SP = SP + FC (1) ** 2 + FC (2) ** 2
            SS = S + SP * (F3/F2) ** 2
            S0 = 1.0D+00 / SQRT (SS)

            DO J = 1, KM
                IF (J < KB) THEN
                    FC (J) = S0 * FC (J) * F3 / F2
                ELSE
                    FC (J) = S0 * FC (J)
                END IF
            END DO

        END IF

    END IF

    IF (FC(1) < 0.0D+00) THEN
        DO J = 1, KM
            FC (J) = - FC (J)
        END DO
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE MTU0 (KF, M, Q, X, CSF, CSD)

!******************************************************************************
!
!! MTU0 computes Mathieu functions CEM(x,q) and SEM(x,q) and derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    20 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KF, the function code.
!    1 for computing cem(x,q) and cem'(x,q)
!    2 for computing sem(x,q) and sem'(x,q).
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Input, real ( kind = rk ) X, the argument of the Mathieu functions,
!    in degrees.
!
!    Output, real ( kind = rk ) CSF, CSD, the values of cem(x,q) and cem'(x,q),
!    or of sem(x,q) and sem'(x,q).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) CSD
    REAL (KIND=RK) CSF
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FG (251)
    INTEGER IC
    INTEGER K
    INTEGER KD
    INTEGER KF
    INTEGER KM
    INTEGER M
    REAL (KIND=RK) Q
    REAL (KIND=RK) QM
    REAL (KIND=RK) RD
    REAL (KIND=RK) X
    REAL (KIND=RK) XR

    EPS = 1.0D-14

    IF (KF == 1) THEN

        IF (M == 2*INT(M/2)) THEN
            KD = 1
        ELSE
            KD = 2
        END IF

    ELSE

        IF (M /= 2*INT(M/2)) THEN
            KD = 3
        ELSE
            KD = 4
        END IF

    END IF

    CALL CVA2 (KD, M, Q, A)

    IF (Q <= 1.0D+00) THEN
        QM = 7.5D+00 + 56.1D+00 * SQRT (Q) - 134.7D+00 * Q + 90.7D+00 * SQRT (Q) * Q
    ELSE
        QM = 17.0D+00 + 3.1D+00 * SQRT (Q) - 0.126D+00 * Q + 0.0037D+00 * SQRT (Q) * Q
    END IF

    KM = INT (QM+0.5D+00*M)
    CALL FCOEF (KD, M, Q, A, FG)
    IC = INT (M/2) + 1
    RD = 1.74532925199433D-02
    XR = X * RD

    CSF = 0.0D+00

    DO K = 1, KM

        IF (KD == 1) THEN
            CSF = CSF + FG (K) * COS ((2.0D+00*K-2.0D+00)*XR)
        ELSE IF (KD == 2) THEN
            CSF = CSF + FG (K) * COS ((2.0D+00*K-1.0D+00)*XR)
        ELSE IF (KD == 3) THEN
            CSF = CSF + FG (K) * SIN ((2.0D+00*K-1.0D+00)*XR)
        ELSE IF (KD == 4) THEN
            CSF = CSF + FG (K) * SIN (2.0D+00*K*XR)
        END IF

        IF (IC <= K .AND. ABS(FG(K)) < ABS(CSF)*EPS) THEN
            EXIT
        END IF

    END DO

    CSD = 0.0D+00

    DO K = 1, KM

        IF (KD == 1) THEN
            CSD = CSD - (2*K-2) * FG (K) * SIN ((2*K-2)*XR)
        ELSE IF (KD == 2) THEN
            CSD = CSD - (2*K-1) * FG (K) * SIN ((2*K-1)*XR)
        ELSE IF (KD == 3) THEN
            CSD = CSD + (2*K-1) * FG (K) * COS ((2*K-1)*XR)
        ELSE IF (KD == 4) THEN
            CSD = CSD + 2.0D+00 * K * FG (K) * COS (2*K*XR)
        END IF

        IF (IC <= K .AND. ABS(FG(K)) < ABS(CSD)*EPS) THEN
            EXIT
        END IF

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE MTU12 (KF, KC, M, Q, X, F1R, D1R, F2R, D2R)

!******************************************************************************
!
!! MTU12 computes modified Mathieu functions of the first and second kind.
!
!  Discussion:
!
!    This procedure computes modified Mathieu functions of the first and
!    second kinds, Mcm(1)(2)(x,q) and Msm(1)(2)(x,q),
!    and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    31 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KF, the function code.
!    1 for computing Mcm(x,q);
!    2 for computing Msm(x,q).
!
!    Input, integer KC, the function code.
!    1, for computing the first kind
!    2, for computing the second kind or Msm(2)(x,q) and Msm(2)'(x,q)
!    3, for computing both the first and second kinds.
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Input, real ( kind = rk ) X, the argument of the Mathieu functions.
!
!    Output, real ( kind = rk ) F1R, D1R, F2R, D2R, the values of
!    Mcm(1)(x,q) or Msm(1)(x,q), Derivative of Mcm(1)(x,q) or Msm(1)(x,q),
!    Mcm(2)(x,q) or Msm(2)(x,q), Derivative of Mcm(2)(x,q) or Msm(2)(x,q).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) BJ1 (0:251)
    REAL (KIND=RK) BJ2 (0:251)
    REAL (KIND=RK) BY1 (0:251)
    REAL (KIND=RK) BY2 (0:251)
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) D1R
    REAL (KIND=RK) D2R
    REAL (KIND=RK) DJ1 (0:251)
    REAL (KIND=RK) DJ2 (0:251)
    REAL (KIND=RK) DY1 (0:251)
    REAL (KIND=RK) DY2 (0:251)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F1R
    REAL (KIND=RK) F2R
    REAL (KIND=RK) FG (251)
    INTEGER IC
    INTEGER K
    INTEGER KC
    INTEGER KD
    INTEGER KF
    INTEGER KM
    INTEGER M
    INTEGER NM
    REAL (KIND=RK) Q
    REAL (KIND=RK) QM
    REAL (KIND=RK) U1
    REAL (KIND=RK) U2
    REAL (KIND=RK) W1
    REAL (KIND=RK) W2
    REAL (KIND=RK) X

    EPS = 1.0D-14

    IF (KF == 1) THEN
        IF (M == 2*INT(M/2)) THEN
            KD = 1
        ELSE
            KD = 2
        END IF
    ELSE
        IF (M /= 2*INT(M/2)) THEN
            KD = 3
        ELSE
            KD = 4
        END IF
    END IF

    CALL CVA2 (KD, M, Q, A)

    IF (Q <= 1.0D+00) THEN
        QM = 7.5D+00 + 56.1D+00 * SQRT (Q) - 134.7D+00 * Q + 90.7D+00 * SQRT (Q) * Q
    ELSE
        QM = 17.0D+00 + 3.1D+00 * SQRT (Q) - 0.126D+00 * Q + 0.0037D+00 * SQRT (Q) * Q
    END IF

    KM = INT (QM+0.5D+00*M)
    CALL FCOEF (KD, M, Q, A, FG)

    IF (KD == 4) THEN
        IC = M / 2
    ELSE
        IC = INT (M/2) + 1
    END IF

    C1 = EXP (-X)
    C2 = EXP (X)
    U1 = SQRT (Q) * C1
    U2 = SQRT (Q) * C2

    CALL JYNB (KM, U1, NM, BJ1, DJ1, BY1, DY1)
    CALL JYNB (KM, U2, NM, BJ2, DJ2, BY2, DY2)

    IF (KC == 1) THEN

        F1R = 0.0D+00

        DO K = 1, KM

            IF (KD == 1) THEN
                F1R = F1R + (-1.0D+00) ** (IC+K) * FG (K) * BJ1 (K-1) * BJ2 (K-1)
            ELSE IF (KD == 2 .OR. KD == 3) THEN
                F1R = F1R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (BJ1(K-1)*BJ2(K)+(-1.0D+00)**KD*BJ1(K)*BJ2(K-1))
            ELSE
                F1R = F1R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (BJ1(K-1)*BJ2(K+1)-BJ1(K+1)*BJ2(K-1))
            END IF

            IF (5 <= K .AND. ABS(F1R-W1) < ABS(F1R)*EPS) THEN
                EXIT
            END IF

            W1 = F1R

        END DO

        F1R = F1R / FG (1)
        D1R = 0.0D+00
        DO K = 1, KM
            IF (KD == 1) THEN
                D1R = D1R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (C2*BJ1(K-1)*DJ2(K-1)-C1*DJ1(K-1)*BJ2(K-1))
            ELSE IF (KD == 2 .OR. KD == 3) THEN
                D1R = D1R + (-1.0D+00) ** (IC+K) * FG (K) * (C2*(BJ1(K-1)*DJ2(K)+(-1.0D+00)**KD&
               & *BJ1(K)*DJ2(K-1))-C1*(DJ1(K-1)*BJ2(K)+(-1.0D+00)**KD*DJ1(K)*BJ2(K-1)))
            ELSE
                D1R = D1R + (-1.0D+00) ** (IC+K) * FG (K) * (C2*(BJ1(K-1)*DJ2(K+1)-BJ1(K+&
               & 1)*DJ2(K-1))-C1*(DJ1(K-1)*BJ2(K+1)-DJ1(K+1)*BJ2(K-1)))
            END IF
            IF (5 <= K .AND. ABS(D1R-W2) < ABS(D1R)*EPS) THEN
                EXIT
            END IF
            W2 = D1R
        END DO

        D1R = D1R * SQRT (Q) / FG (1)

    ELSE

        F2R = 0.0D+00

        DO K = 1, KM
            IF (KD == 1) THEN
                F2R = F2R + (-1.0D+00) ** (IC+K) * FG (K) * BJ1 (K-1) * BY2 (K-1)
            ELSE IF (KD == 2 .OR. KD == 3) THEN
                F2R = F2R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (BJ1(K-1)*BY2(K)+(-1.0D+00)**KD*BJ1(K)*BY2(K-1))
            ELSE
                F2R = F2R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (BJ1(K-1)*BY2(K+1)-BJ1(K+1)*BY2(K-1))
            END IF
            IF (5 <= K .AND. ABS(F2R-W1) < ABS(F2R)*EPS) THEN
                EXIT
            END IF
            W1 = F2R
        END DO

        F2R = F2R / FG (1)
        D2R = 0.0D+00

        DO K = 1, KM
            IF (KD == 1) THEN
                D2R = D2R + (-1.0D+00) ** (IC+K) * FG (K) * &
               & (C2*BJ1(K-1)*DY2(K-1)-C1*DJ1(K-1)*BY2(K-1))
            ELSE IF (KD == 2 .OR. KD == 3) THEN
                D2R = D2R + (-1.0D+00) ** (IC+K) * FG (K) * (C2*(BJ1(K-1)*DY2(K)+(-1.0D+00)**KD&
               & *BJ1(K)*DY2(K-1))-C1*(DJ1(K-1)*BY2(K)+(-1.0D+00)**KD*DJ1(K)*BY2(K-1)))
            ELSE
                D2R = D2R + (-1.0D+00) ** (IC+K) * FG (K) * (C2*(BJ1(K-1)*DY2(K+1)-BJ1(K+&
               & 1)*DY2(K-1))-C1*(DJ1(K-1)*BY2(K+1)-DJ1(K+1)*BY2(K-1)))
            END IF

            IF (5 <= K .AND. ABS(D2R-W2) < ABS(D2R)*EPS) THEN
                EXIT
            END IF

            W2 = D2R

        END DO

        D2R = D2R * SQRT (Q) / FG (1)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CV0 (KD, M, Q, A0)

!******************************************************************************
!
!! CV0 computes the initial characteristic value of Mathieu functions.
!
!  Discussion:
!
!    This procedure computes the initial characteristic value of Mathieu
!    functions for m <= 12 or q <= 300 or q <= m*m.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!   03 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code:
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the functions.
!
!    Output, real ( kind = rk ) A0, the characteristic value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    INTEGER KD
    INTEGER M
    REAL (KIND=RK) Q
    REAL (KIND=RK) Q2

    Q2 = Q * Q

    IF (M == 0) THEN

        IF (Q <= 1.0D+00) THEN

            A0 = (((0.0036392D+00*Q2-0.0125868D+00)*Q2+0.0546875D+00)*Q2-0.5D+00) * Q2

        ELSE IF (Q <= 10.0D+00) THEN

            A0 = ((3.999267D-03*Q-9.638957D-02)*Q-0.88297D+00) * Q + 0.5542818D+00

        ELSE

            CALL CVQL (KD, M, Q, A0)

        END IF

    ELSE IF (M == 1) THEN

        IF (Q <= 1.0D+00 .AND. KD == 2) THEN

            A0 = (((-6.51D-04*Q-0.015625D+00)*Q-0.125D+00)*Q+1.0D+00) * Q + 1.0D+00

        ELSE IF (Q <= 1.0D+00 .AND. KD == 3) THEN

            A0 = (((-6.51D-04*Q+0.015625D+00)*Q-0.125D+00)*Q-1.0D+00) * Q + 1.0D+00

        ELSE IF (Q <= 10.0D+00 .AND. KD == 2) THEN

            A0 = (((-4.94603D-04*Q+1.92917D-02)*Q-0.3089229D+00)*Q+1.33372D+00) * Q + &
           & 0.811752D+00

        ELSE IF (Q <= 10.0D+00 .AND. KD == 3) THEN

            A0 = ((1.971096D-03*Q-5.482465D-02)*Q-1.152218D+00) * Q + 1.10427D+00

        ELSE

            CALL CVQL (KD, M, Q, A0)

        END IF

    ELSE IF (M == 2) THEN

        IF (Q <= 1.0D+00 .AND. KD == 1) THEN

            A0 = (((-0.0036391D+00*Q2+0.0125888D+00)*Q2-0.0551939D+00)*Q2+0.416667D+00) * Q2 + &
           & 4.0D+00

        ELSE IF (Q <= 1.0D+00 .AND. KD == 4) THEN

            A0 = (0.0003617D+00*Q2-0.0833333D+00) * Q2 + 4.0D+00

        ELSE IF (Q <= 15.0D+00 .AND. KD == 1) THEN

            A0 = (((3.200972D-04*Q-8.667445D-03)*Q-1.829032D-04)*Q+0.9919999D+00) * Q + &
           & 3.3290504D+00

        ELSE IF (Q <= 10.0D+00 .AND. KD == 4) THEN

            A0 = ((2.38446D-03*Q-0.08725329D+00)*Q-4.732542D-03) * Q + 4.00909D+00

        ELSE

            CALL CVQL (KD, M, Q, A0)

        END IF

    ELSE IF (M == 3) THEN

        IF (Q <= 1.0D+00 .AND. KD == 2) THEN
            A0 = ((6.348D-04*Q+0.015625D+00)*Q+0.0625) * Q2 + 9.0D+00
        ELSE IF (Q <= 1.0D+00 .AND. KD == 3) THEN
            A0 = ((6.348D-04*Q-0.015625D+00)*Q+0.0625D+00) * Q2 + 9.0D+00
        ELSE IF (Q <= 20.0D+00 .AND. KD == 2) THEN
            A0 = (((3.035731D-04*Q-1.453021D-02)*Q+0.19069602D+00)*Q-0.1039356D+00) * Q + &
           & 8.9449274D+00
        ELSE IF (Q <= 15.0D+00 .AND. KD == 3) THEN
            A0 = ((9.369364D-05*Q-0.03569325D+00)*Q+0.2689874D+00) * Q + 8.771735D+00
        ELSE
            CALL CVQL (KD, M, Q, A0)
        END IF

    ELSE IF (M == 4) THEN

        IF (Q <= 1.0D+00 .AND. KD == 1) THEN
            A0 = ((-2.1D-06*Q2+5.012D-04)*Q2+0.0333333) * Q2 + 16.0D+00
        ELSE IF (Q <= 1.0D+00 .AND. KD == 4) THEN
            A0 = ((3.7D-06*Q2-3.669D-04)*Q2+0.0333333D+00) * Q2 + 16.0D+00
        ELSE IF (Q <= 25.0D+00 .AND. KD == 1) THEN
            A0 = (((1.076676D-04*Q-7.9684875D-03)*Q+0.17344854D+00)*Q-0.5924058D+00) * Q + &
           & 16.620847D+00
        ELSE IF (Q <= 20.0D+00 .AND. KD == 4) THEN
            A0 = ((-7.08719D-04*Q+3.8216144D-03)*Q+0.1907493D+00) * Q + 15.744D+00
        ELSE
            CALL CVQL (KD, M, Q, A0)
        END IF

    ELSE IF (M == 5) THEN

        IF (Q <= 1.0D+00 .AND. KD == 2) THEN
            A0 = ((6.8D-6*Q+1.42D-05)*Q2+0.0208333D+00) * Q2 + 25.0D+00
        ELSE IF (Q <= 1.0D+00 .AND. KD == 3) THEN
            A0 = ((-6.8D-06*Q+1.42D-05)*Q2+0.0208333D+00) * Q2 + 25.0D+00
        ELSE IF (Q <= 35.0D+00 .AND. KD == 2) THEN
            A0 = (((2.238231D-05*Q-2.983416D-03)*Q+0.10706975D+00)*Q-0.600205D+00) * Q + &
           & 25.93515D+00
        ELSE IF (Q <= 25.0D+00 .AND. KD == 3) THEN
            A0 = ((-7.425364D-04*Q+2.18225D-02)*Q+4.16399D-02) * Q + 24.897D+00
        ELSE
            CALL CVQL (KD, M, Q, A0)
        END IF

    ELSE IF (M == 6) THEN

        IF (Q <= 1.0D+00) THEN
            A0 = (0.4D-06*Q2+0.0142857) * Q2 + 36.0D+00
        ELSE IF (Q <= 40.0D+00 .AND. KD == 1) THEN
            A0 = (((-1.66846D-05*Q+4.80263D-04)*Q+2.53998D-02)*Q-0.181233D+00) * Q + 36.423D+00
        ELSE IF (Q <= 35.0D+00 .AND. KD == 4) THEN
            A0 = ((-4.57146D-04*Q+2.16609D-02)*Q-2.349616D-02) * Q + 35.99251D+00
        ELSE
            CALL CVQL (KD, M, Q, A0)
        END IF

    ELSE IF (M == 7) THEN

        IF (Q <= 10.0D+00) THEN
            CALL CVQM (M, Q, A0)
        ELSE IF (Q <= 50.0D+00 .AND. KD == 2) THEN
            A0 = (((-1.411114D-05*Q+9.730514D-04)*Q-3.097887D-03)*Q+3.533597D-02) * Q + &
           & 49.0547D+00
        ELSE IF (Q <= 40.0D+00 .AND. KD == 3) THEN
            A0 = ((-3.043872D-04*Q+2.05511D-02)*Q-9.16292D-02) * Q + 49.19035D+00
        ELSE
            CALL CVQL (KD, M, Q, A0)
        END IF

    ELSE IF (8 <= M) THEN

        IF (Q <= 3.0D+00*M) THEN
            CALL CVQM (M, Q, A0)
        ELSE IF (M*M .LT. Q) THEN
            CALL CVQL (KD, M, Q, A0)
        ELSE IF (M == 8 .AND. KD == 1) THEN
            A0 = (((8.634308D-06*Q-2.100289D-03)*Q+0.169072D+00)*Q-4.64336D+00) * Q + &
           & 109.4211D+00
        ELSE IF (M == 8 .AND. KD == 4) THEN
            A0 = ((-6.7842D-05*Q+2.2057D-03)*Q+0.48296D+00) * Q + 56.59D+00
        ELSE IF (M == 9 .AND. KD == 2) THEN
            A0 = (((2.906435D-06*Q-1.019893D-03)*Q+0.1101965D+00)*Q-3.821851D+00) * Q + &
           & 127.6098D+00
        ELSE IF (M == 9 .AND. KD == 3) THEN
            A0 = ((-9.577289D-05*Q+0.01043839D+00)*Q+0.06588934D+00) * Q + 78.0198D+00
        ELSE IF (M == 10 .AND. KD == 1) THEN
            A0 = (((5.44927D-07*Q-3.926119D-04)*Q+0.0612099D+00)*Q-2.600805D+00) * Q + &
           & 138.1923D+00
        ELSE IF (M == 10 .AND. KD == 4) THEN
            A0 = ((-7.660143D-05*Q+0.01132506D+00)*Q-0.09746023D+00) * Q + 99.29494D+00
        ELSE IF (M == 11 .AND. KD == 2) THEN
            A0 = (((-5.67615D-07*Q+7.152722D-06)*Q+0.01920291D+00)*Q-1.081583D+00) * Q + &
           & 140.88D+00
        ELSE IF (M == 11 .AND. KD == 3) THEN
            A0 = ((-6.310551D-05*Q+0.0119247D+00)*Q-0.2681195D+00) * Q + 123.667D+00
        ELSE IF (M == 12 .AND. KD == 1) THEN
            A0 = (((-2.38351D-07*Q-2.90139D-05)*Q+0.02023088D+00)*Q-1.289D+00) * Q + &
           & 171.2723D+00
        ELSE IF (M == 12 .AND. KD == 4) THEN
            A0 = (((3.08902D-07*Q-1.577869D-04)*Q+0.0247911D+00)*Q-1.05454D+00) * Q + &
           & 161.471D+00

        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CVF (KD, M, Q, A, MJ, F)

!******************************************************************************
!
!! CVF computes F for the characteristic equation of Mathieu functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    16 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code:
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Input, real ( kind = rk ) A, the characteristic value.
!
!    Input, integer MJ, ?
!
!    Output, real ( kind = rk ) F, the value of the function for the
!    characteristic equation.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) B
    REAL (KIND=RK) F
    INTEGER IC
    INTEGER J
    INTEGER J0
    INTEGER JF
    INTEGER KD
    INTEGER L
    INTEGER L0
    INTEGER M
    INTEGER MJ
    REAL (KIND=RK) Q
    REAL (KIND=RK) T0
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2

    B = A
    IC = INT (M/2)
    L = 0
    L0 = 0
    J0 = 2
    JF = IC

    IF (KD == 1) THEN
        L0 = 2
        J0 = 3
    ELSE IF (KD == 2 .OR. KD == 3) THEN
        L = 1
    ELSE IF (KD == 4) THEN
        JF = IC - 1
    END IF

    T1 = 0.0D+00
    DO J = MJ, IC + 1, - 1
        T1 = - Q * Q / ((2.0D+00*J+L)**2-B+T1)
    END DO

    IF (M <= 2) THEN

        T2 = 0.0D+00

        IF (KD == 1) THEN
            IF (M == 0) THEN
                T1 = T1 + T1
            ELSE IF (M == 2) THEN
                T1 = - 2.0D+00 * Q * Q / (4.0D+00-B+T1) - 4.0D+00
            END IF
        ELSE IF (KD == 2) THEN
            IF (M == 1) THEN
                T1 = T1 + Q
            END IF
        ELSE IF (KD == 3) THEN
            IF (M == 1) THEN
                T1 = T1 - Q
            END IF
        END IF

    ELSE

        IF (KD == 1) THEN
            T0 = 4.0D+00 - B + 2.0D+00 * Q * Q / B
        ELSE IF (KD == 2) THEN
            T0 = 1.0D+00 - B + Q
        ELSE IF (KD == 3) THEN
            T0 = 1.0D+00 - B - Q
        ELSE IF (KD == 4) THEN
            T0 = 4.0D+00 - B
        END IF

        T2 = - Q * Q / T0
        DO J = J0, JF
            T2 = - Q * Q / ((2.0D+00*J-L-L0)**2-B+T2)
        END DO

    END IF

    F = (2.0D+00*IC+L) ** 2 + T1 + T2 - B

    RETURN
END

!******************************************************************************

SUBROUTINE CVQL (KD, M, Q, A0)

!******************************************************************************
!
!! CVQL computes the characteristic value of Mathieu functions for q <= 3*m.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    10 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code:
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter value.
!
!    Output, real ( kind = rk ) A0, the initial characteristic value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) C1
    REAL (KIND=RK) CV1
    REAL (KIND=RK) CV2
    REAL (KIND=RK) D1
    REAL (KIND=RK) D2
    REAL (KIND=RK) D3
    REAL (KIND=RK) D4
    INTEGER KD
    INTEGER M
    REAL (KIND=RK) P1
    REAL (KIND=RK) P2
    REAL (KIND=RK) Q
    REAL (KIND=RK) W
    REAL (KIND=RK) W2
    REAL (KIND=RK) W3
    REAL (KIND=RK) W4
    REAL (KIND=RK) W6

    IF (KD == 1 .OR. KD == 2) THEN
        W = 2.0D+00 * M + 1.0D+00
    ELSE
        W = 2.0D+00 * M - 1.0D+00
    END IF

    W2 = W * W
    W3 = W * W2
    W4 = W2 * W2
    W6 = W2 * W4
    D1 = 5.0D+00 + 34.0D+00 / W2 + 9.0D+00 / W4
    D2 = (33.0D+00+410.0D+00/W2+405.0D+00/W4) / W
    D3 = (63.0D+00+1260.0D+00/W2+2943.0D+00/W4+486.0D+00/W6) / W2
    D4 = (527.0D+00+15617.0D+00/W2+69001.0D+00/W4+41607.0D+00/W6) / W3
    C1 = 128.0D+00
    P2 = Q / W4
    P1 = SQRT (P2)
    CV1 = - 2.0D+00 * Q + 2.0D+00 * W * SQRT (Q) - (W2+1.0D+00) / 8.0D+00
    CV2 = (W+3.0D+00/W) + D1 / (32.0D+00*P1) + D2 / (8.0D+00*C1*P2)
    CV2 = CV2 + D3 / (64.0D+00*C1*P1*P2) + D4 / (16.0D+00*C1*C1*P2*P2)
    A0 = CV1 - CV2 / (C1*P1)

    RETURN
END

!******************************************************************************

SUBROUTINE CVQM (M, Q, A0)

!******************************************************************************
!
!! CVQM computes the characteristic value of Mathieu functions for q <= m*m.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter value.
!
!    Output, real ( kind = rk ) A0, the initial characteristic value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) HM1
    REAL (KIND=RK) HM3
    REAL (KIND=RK) HM5
    INTEGER M
    REAL (KIND=RK) Q

    HM1 = 0.5D+00 * Q / (M*M-1.0D+00)
    HM3 = 0.25D+00 * HM1 ** 3 / (M*M-4.0D+00)
    HM5 = HM1 * HM3 * Q / ((M*M-1.0D+00)*(M*M-9.0D+00))
    A0 = M * M + Q * (HM1+(5.0D+00*M*M+7.0D+00)*HM3+(9.0D+00*M**4+58.0D+00*M*M+29.0D+00)*HM5)

    RETURN
END

!******************************************************************************

SUBROUTINE REFINE (KD, M, Q, A, IFLAG)

!******************************************************************************
!
!! REFINE refines an estimate of the characteristic value of Mathieu functions.
!
!  Discussion:
!
!    This procedure calculates the accurate characteristic value
!    by the secant method.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    20 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KD, the case code:
!    1, for cem(x,q)  ( m = 0,2,4,...)
!    2, for cem(x,q)  ( m = 1,3,5,...)
!    3, for sem(x,q)  ( m = 1,3,5,...)
!    4, for sem(x,q)  ( m = 2,4,6,...)
!
!    Input, integer M, the order of the Mathieu functions.
!
!    Input, real ( kind = rk ) Q, the parameter of the Mathieu functions.
!
!    Input/output, real ( kind = rk ) A, the characteristic value, which
!    should have been refined on output.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) CA
    REAL (KIND=RK) DELTA
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    INTEGER IT
    INTEGER IFLAG
    INTEGER KD
    INTEGER M
    INTEGER MJ
    REAL (KIND=RK) Q
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X1

    EPS = 1.0D-14
    MJ = 10 + M
    CA = A
    DELTA = 0.0D+00
    X0 = A
    CALL CVF (KD, M, Q, X0, MJ, F0)
    X1 = 1.002D+00 * A
    CALL CVF (KD, M, Q, X1, MJ, F1)

    DO

        DO IT = 1, 100
            MJ = MJ + 1
            X = X1 - (X1-X0) / (1.0D+00-F0/F1)
            CALL CVF (KD, M, Q, X, MJ, F)
            IF (ABS(1.0D+00-X1/X) < EPS .OR. F == 0.0D+00) THEN
                EXIT
            END IF
            X0 = X1
            F0 = F1
            X1 = X
            F1 = F
        END DO

        A = X

        IF (0.05D+00 < DELTA) THEN
            A = CA
            IF (IFLAG < 0) THEN
                IFLAG = - 10
            END IF
            RETURN
        END IF

        IF (ABS((A-CA)/CA) <= 0.05D+00) THEN
            EXIT
        END IF

        X0 = CA
        DELTA = DELTA + 0.005D+00
        CALL CVF (KD, M, Q, X0, MJ, F0)
        X1 = (1.0D+00+DELTA) * CA
        CALL CVF (KD, M, Q, X1, MJ, F1)

    END DO

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   15: SPHEROIDAL WAVE FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE SCKA (M, N, C, CV, KD, CK)

!******************************************************************************
!
!! SCKA: expansion coefficients for prolate and oblate spheroidal functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    22 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter.
!
!    Input, integer N, the mode parameter.
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) CK(*), the expansion coefficients.
!    CK(1), CK(2),... correspond to c0, c2,..., and so on.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CS
    REAL (KIND=RK) CV
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) FL
    REAL (KIND=RK) FS
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER K1
    INTEGER KB
    INTEGER KD
    INTEGER M
    INTEGER N
    INTEGER NM
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) S0
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SU2

    IF (C <= 1.0D-10) THEN
        C = 1.0D-10
    END IF

    NM = 25 + INT ((N-M)/2+C)
    CS = C * C * KD

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    FS = 1.0D+00
    F1 = 0.0D+00
    F0 = 1.0D-100
    KB = 0
    CK (NM+1) = 0.0D+00

    DO K = NM, 1, - 1

        F = (((2.0D+00*K+M+IP)*(2.0D+00*K+M+1.0D+00+IP)-CV+CS)*F0-4.0D+00*(K+1.0D+00)*(K+M+&
       & 1.0D+00)*F1) / CS

        IF (ABS(CK(K+1)) < ABS(F)) THEN

            CK (K) = F
            F1 = F0
            F0 = F

            IF (1.0D+100 < ABS(F)) THEN
                DO K1 = NM, K, - 1
                    CK (K1) = CK (K1) * 1.0D-100
                END DO
                F1 = F1 * 1.0D-100
                F0 = F0 * 1.0D-100
            END IF

        ELSE

            KB = K
            FL = CK (K+1)
            F1 = 1.0D+00
            F2 = 0.25D+00 * ((M+IP)*(M+IP+1.0D+00)-CV+CS) / (M+1.0D+00) * F1
            CK (1) = F1

            IF (KB == 1) THEN
                FS = F2
            ELSE IF (KB == 2) THEN
                CK (2) = F2
                FS = 0.125D+00 * (((M+IP+2.0D+00)*(M+IP+3.0D+00)-CV+CS)*F2-CS*F1) / (M+2.0D+00)
            ELSE
                CK (2) = F2
                DO J = 3, KB + 1
                    F = 0.25D+00 * &
                   & (((2.0D+00*J+M+IP-4.0D+00)*(2.0D+00*J+M+IP-3.0D+00)-CV+CS)*F2-CS*F1) / &
                   & ((J-1.0D+00)*(J+M-1.0D+00))
                    IF (J <= KB) THEN
                        CK (J) = F
                    END IF
                    F1 = F2
                    F2 = F
                END DO
                FS = F
            END IF

            EXIT

        END IF

    END DO

    SU1 = 0.0D+00
    DO K = 1, KB
        SU1 = SU1 + CK (K)
    END DO

    SU2 = 0.0D+00
    DO K = KB + 1, NM
        SU2 = SU2 + CK (K)
    END DO

    R1 = 1.0D+00
    DO J = 1, (N+M+IP) / 2
        R1 = R1 * (J+0.5D+00*(N+M+IP))
    END DO

    R2 = 1.0D+00
    DO J = 1, (N-M-IP) / 2
        R2 = - R2 * J
    END DO

    IF (KB == 0) THEN
        S0 = R1 / (2.0D+00**N*R2*SU2)
    ELSE
        S0 = R1 / (2.0D+00**N*R2*(FL/FS*SU1+SU2))
    END IF

    DO K = 1, KB
        CK (K) = FL / FS * S0 * CK (K)
    END DO

    DO K = KB + 1, NM
        CK (K) = S0 * CK (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SCKB (M, N, C, DF, CK)

!******************************************************************************
!
!! SCKB: expansion coefficients for prolate and oblate spheroidal functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter.
!
!    Input, integer N, the mode parameter.
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients DK.
!
!    Output, real ( kind = rk ) CK(*), the expansion coefficients CK.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) D1
    REAL (KIND=RK) D2
    REAL (KIND=RK) D3
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) FAC
    INTEGER I
    INTEGER I1
    INTEGER I2
    INTEGER IP
    INTEGER K
    INTEGER M
    INTEGER N
    INTEGER NM
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) REG
    REAL (KIND=RK) SUM
    REAL (KIND=RK) SW

    C = MAX (C, 1.0D-10)

    NM = 25 + INT (0.5D+00*(N-M)+C)

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    IF (80 < M+NM) THEN
        REG = 1.0D-200
    ELSE
        REG = 1.0D+00
    END IF

    FAC = - 0.5D+00 ** M

    DO K = 0, NM - 1

        FAC = - FAC
        I1 = 2 * K + IP + 1
        R = REG
        DO I = I1, I1 + 2 * M - 1
            R = R * I
        END DO

        I2 = K + M + IP
        DO I = I2, I2 + K - 1
            R = R * (I+0.5D+00)
        END DO

        SUM = R * DF (K+1)
        DO I = K + 1, NM
            D1 = 2.0D+00 * I + IP
            D2 = 2.0D+00 * M + D1
            D3 = I + M + IP - 0.5D+00
            R = R * D2 * (D2-1.0D+00) * I * (D3+K) / (D1*(D1-1.0D+00)*(I-K)*D3)
            SUM = SUM + R * DF (I+1)
            IF (ABS(SW-SUM) < ABS(SUM)*1.0D-14) THEN
                EXIT
            END IF
            SW = SUM
        END DO

        R1 = REG
        DO I = 2, M + K
            R1 = R1 * I
        END DO

        CK (K+1) = FAC * SUM / R1

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SDMN (M, N, C, CV, KD, DF)

!******************************************************************************
!
!! SDMN: expansion coefficients for prolate and oblate spheroidal functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter.
!
!    Input, integer N, the mode parameter.
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) DF(*), expansion coefficients;
!    DF(1), DF(2), ... correspond to d0, d2, ... for even n-m and d1,
!    d3, ... for odd n-m
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (200)
    REAL (KIND=RK) C
    REAL (KIND=RK) CS
    REAL (KIND=RK) CV
    REAL (KIND=RK) D (200)
    REAL (KIND=RK) D2K
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DK0
    REAL (KIND=RK) DK1
    REAL (KIND=RK) DK2
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) FL
    REAL (KIND=RK) FS
    REAL (KIND=RK) G (200)
    INTEGER I
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER K1
    INTEGER KB
    INTEGER KD
    INTEGER M
    INTEGER N
    INTEGER NM
    REAL (KIND=RK) R1
    REAL (KIND=RK) R3
    REAL (KIND=RK) R4
    REAL (KIND=RK) S0
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SU2
    REAL (KIND=RK) SW

    NM = 25 + INT (0.5D+00*(N-M)+C)

    IF (C < 1.0D-10) THEN
        DO I = 1, NM
            DF (I) = 0D+00
        END DO
        DF ((N-M)/2+1) = 1.0D+00
        RETURN
    END IF

    CS = C * C * KD

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    DO I = 1, NM + 2
        IF (IP == 0) THEN
            K = 2 * (I-1)
        ELSE
            K = 2 * I - 1
        END IF
        DK0 = M + K
        DK1 = M + K + 1
        DK2 = 2 * (M+K)
        D2K = 2 * M + K
        A (I) = (D2K+2.0D+00) * (D2K+1.0D+00) / ((DK2+3.0D+00)*(DK2+5.0D+00)) * CS
        D (I) = DK0 * DK1 + (2.0D+00*DK0*DK1-2.0D+00*M*M-1.0D+00) / &
       & ((DK2-1.0D+00)*(DK2+3.0D+00)) * CS
        G (I) = K * (K-1.0D+00) / ((DK2-3.0D+00)*(DK2-1.0D+00)) * CS
    END DO

    FS = 1.0D+00
    F1 = 0.0D+00
    F0 = 1.0D-100
    KB = 0
    DF (NM+1) = 0.0D+00

    DO K = NM, 1, - 1

        F = - ((D(K+1)-CV)*F0+A(K+1)*F1) / G (K+1)

        IF (ABS(DF(K+1)) < ABS(F)) THEN

            DF (K) = F
            F1 = F0
            F0 = F
            IF (1.0D+100 < ABS(F)) THEN
                DO K1 = K, NM
                    DF (K1) = DF (K1) * 1.0D-100
                END DO
                F1 = F1 * 1.0D-100
                F0 = F0 * 1.0D-100
            END IF

        ELSE

            KB = K
            FL = DF (K+1)
            F1 = 1.0D-100
            F2 = - (D(1)-CV) / A (1) * F1
            DF (1) = F1

            IF (KB == 1) THEN

                FS = F2

            ELSE IF (KB == 2) THEN

                DF (2) = F2
                FS = - ((D(2)-CV)*F2+G(2)*F1) / A (2)

            ELSE

                DF (2) = F2
                DO J = 3, KB + 1
                    F = - ((D(J-1)-CV)*F2+G(J-1)*F1) / A (J-1)
                    IF (J <= KB) THEN
                        DF (J) = F
                    END IF
                    IF (1.0D+100 < ABS(F)) THEN
                        DO K1 = 1, J
                            DF (K1) = DF (K1) * 1.0D-100
                        END DO
                        F = F * 1.0D-100
                        F2 = F2 * 1.0D-100
                    END IF
                    F1 = F2
                    F2 = F
                END DO
                FS = F

            END IF

            EXIT

        END IF

    END DO

    SU1 = 0.0D+00

    R1 = 1.0D+00
    DO J = M + IP + 1, 2 * (M+IP)
        R1 = R1 * J
    END DO

    SU1 = DF (1) * R1
    DO K = 2, KB
        R1 = - R1 * (K+M+IP-1.5D+00) / (K-1.0D+00)
        SU1 = SU1 + R1 * DF (K)
    END DO

    SU2 = 0.0D+00
    DO K = KB + 1, NM
        IF (K /= 1) THEN
            R1 = - R1 * (K+M+IP-1.5D+00) / (K-1.0D+00)
        END IF
        SU2 = SU2 + R1 * DF (K)
        IF (ABS(SW-SU2) < ABS(SU2)*1.0D-14) THEN
            EXIT
        END IF
        SW = SU2
    END DO

    R3 = 1.0D+00
    DO J = 1, (M+N+IP) / 2
        R3 = R3 * (J+0.5D+00*(N+M+IP))
    END DO

    R4 = 1.0D+00
    DO J = 1, (N-M-IP) / 2
        R4 = - 4.0D+00 * R4 * J
    END DO

    S0 = R3 / (FL*(SU1/FS)+SU2) / R4
    DO K = 1, KB
        DF (K) = FL / FS * S0 * DF (K)
    END DO

    DO K = KB + 1, NM
        DF (K) = S0 * DF (K)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE SEGV (M, N, C, KD, CV, EG)

!******************************************************************************
!
!! SEGV computes the characteristic values of spheroidal wave functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter.
!
!    Input, integer N, the mode parameter.
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) CV, the characteristic value.
!
!    Output, real ( kind = rk ) EG(*), the characteristic value for
!    mode parameters m and n.  ( L = n - m + 1 )
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A (300)
    REAL (KIND=RK) B (100)
    REAL (KIND=RK) C
    REAL (KIND=RK) CS
    REAL (KIND=RK) CV
    REAL (KIND=RK) CV0 (100)
    REAL (KIND=RK) D (300)
    REAL (KIND=RK) D2K
    REAL (KIND=RK) DK0
    REAL (KIND=RK) DK1
    REAL (KIND=RK) DK2
    REAL (KIND=RK) E (300)
    REAL (KIND=RK) EG (200)
    REAL (KIND=RK) F (300)
    REAL (KIND=RK) G (300)
    REAL (KIND=RK) H (100)
    INTEGER I
    INTEGER ICM
    INTEGER J
    INTEGER K
    INTEGER K1
    INTEGER KD
    INTEGER L
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM1
    REAL (KIND=RK) S
    REAL (KIND=RK) T
    REAL (KIND=RK) T1
    REAL (KIND=RK) X1
    REAL (KIND=RK) XA
    REAL (KIND=RK) XB

    IF (C < 1.0D-10) THEN
        DO I = 1, N
            EG (I) = (I+M) * (I+M-1.0D+00)
        END DO
        CV = EG (N-M+1)
        RETURN
    END IF

    ICM = (N-M+2) / 2
    NM = 10 + INT (0.5D+00*(N-M)+C)
    CS = C * C * KD

    DO L = 0, 1

        DO I = 1, NM
            IF (L == 0) THEN
                K = 2 * (I-1)
            ELSE
                K = 2 * I - 1
            END IF
            DK0 = M + K
            DK1 = M + K + 1
            DK2 = 2 * (M+K)
            D2K = 2 * M + K
            A (I) = (D2K+2.0D+00) * (D2K+1.0D+00) / ((DK2+3.0D+00)*(DK2+5.0D+00)) * CS
            D (I) = DK0 * DK1 + (2.0D+00*DK0*DK1-2.0*M*M-1.0D+00) / &
           & ((DK2-1.0D+00)*(DK2+3.0D+00)) * CS
            G (I) = K * (K-1.0D+00) / ((DK2-3.0D+00)*(DK2-1.0D+00)) * CS
        END DO

        DO K = 2, NM
            E (K) = SQRT (A(K-1)*G(K))
            F (K) = E (K) * E (K)
        END DO

        F (1) = 0.0D+00
        E (1) = 0.0D+00
        XA = D (NM) + ABS (E(NM))
        XB = D (NM) - ABS (E(NM))
        NM1 = NM - 1
        DO I = 1, NM1
            T = ABS (E(I)) + ABS (E(I+1))
            T1 = D (I) + T
            IF (XA < T1) THEN
                XA = T1
            END IF
            T1 = D (I) - T
            IF (T1 < XB) THEN
                XB = T1
            END IF
        END DO

        DO I = 1, ICM
            B (I) = XA
            H (I) = XB
        END DO

        DO K = 1, ICM

            DO K1 = K, ICM
                IF (B(K1) < B(K)) THEN
                    B (K) = B (K1)
                    EXIT
                END IF
            END DO

            IF (K /= 1 .AND. H(K) < H(K-1)) THEN
                H (K) = H (K-1)
            END IF

            DO

                X1 = (B(K)+H(K)) / 2.0D+00
                CV0 (K) = X1

                IF (ABS((B(K)-H(K))/X1) < 1.0D-14) THEN
                    EXIT
                END IF

                J = 0
                S = 1.0D+00

                DO I = 1, NM

                    IF (S == 0.0D+00) THEN
                        S = S + 1.0D-30
                    END IF
                    T = F (I) / S
                    S = D (I) - T - X1
                    IF (S < 0.0D+00) THEN
                        J = J + 1
                    END IF
                END DO

                IF (J < K) THEN

                    H (K) = X1

                ELSE

                    B (K) = X1
                    IF (ICM <= J) THEN
                        B (ICM) = X1
                    ELSE
                        IF (H(J+1) < X1) THEN
                            H (J+1) = X1
                        END IF
                        IF (X1 < B(J)) THEN
                            B (J) = X1
                        END IF
                    END IF

                END IF

            END DO

            CV0 (K) = X1

            IF (L == 0) THEN
                EG (2*K-1) = CV0 (K)
            ELSE
                EG (2*K) = CV0 (K)
            END IF

        END DO

    END DO

    CV = EG (N-M+1)

    RETURN
END

!******************************************************************************

SUBROUTINE ASWFA (M, N, C, X, KD, CV, S1F, S1D)

!******************************************************************************
!
!! aswfa(): prolate and oblate spheroidal angular functions of the first kind.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    13 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter.
!
!    Input, integer N, the mode parameter, with N = M, M+1, ...
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument of the angular function.
!    |X| < 1.0.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Output, real ( kind = rk ) S1F, S1D, the angular function of the first
!    kind and its derivative.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CV
    REAL (KIND=RK) D0
    REAL (KIND=RK) D1
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) EPS
    INTEGER IP
    INTEGER K
    INTEGER KD
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM2
    REAL (KIND=RK) R
    REAL (KIND=RK) S1D
    REAL (KIND=RK) S1F
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SU2
    REAL (KIND=RK) X
    REAL (KIND=RK) X0
    REAL (KIND=RK) X1

    EPS = 1.0D-14
    X0 = X
    X = ABS (X)

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    NM = 10 + INT ((N-M)/2+C)
    NM2 = NM / 2 - 2
    CALL SDMN (M, N, C, CV, KD, DF)
    CALL SCKB (M, N, C, DF, CK)
    X1 = 1.0D+00 - X * X

    IF (M == 0 .AND. X1 == 0.0D+00) THEN
        A0 = 1.0D+00
    ELSE
        A0 = X1 ** (0.5D+00*M)
    END IF

    SU1 = CK (1)
    DO K = 1, NM2
        R = CK (K+1) * X1 ** K
        SU1 = SU1 + R
        IF (10 <= K .AND. ABS(R/SU1) < EPS) THEN
            EXIT
        END IF
    END DO

    S1F = A0 * X ** IP * SU1

    IF (X == 1.0D+00) THEN

        IF (M == 0) THEN
            S1D = IP * CK (1) - 2.0D+00 * CK (2)
        ELSE IF (M == 1) THEN
            S1D = - 1.0D+100
        ELSE IF (M == 2) THEN
            S1D = - 2.0D+00 * CK (1)
        ELSE IF (3 <= M) THEN
            S1D = 0.0D+00
        END IF

    ELSE

        D0 = IP - M / X1 * X ** (IP+1.0D+00)
        D1 = - 2.0D+00 * A0 * X ** (IP+1.0D+00)
        SU2 = CK (2)
        DO K = 2, NM2
            R = K * CK (K+1) * X1 ** (K-1.0D+00)
            SU2 = SU2 + R
            IF (10 <= K .AND. ABS(R/SU2) < EPS) THEN
                EXIT
            END IF
        END DO

        S1D = D0 * A0 * SU1 + D1 * SU2

    END IF

    IF (X0 < 0.0D+00) THEN
        IF (IP == 0) THEN
            S1D = - S1D
        ELSE IF (IP == 1) THEN
            S1F = - S1F
        END IF
    END IF

    X = X0

    RETURN
END

!******************************************************************************

SUBROUTINE ASWFB (M, N, C, X, KD, CV, S1F, S1D)

!******************************************************************************
!
!! aswfb(): prolate and oblate spheroidal angular functions of the first kind.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    20 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter, m = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M+1, M+2, ...
!
!    Input, real ( kind = rk ) C, the spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument, with |X| < 1.0.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Output, real ( kind = rk ) S1F, S1D, the angular function of the first
!    kind and its derivative.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) EPS
    INTEGER IP
    INTEGER K
    INTEGER KD
    INTEGER M
    INTEGER MK
    INTEGER N
    INTEGER NM
    INTEGER NM2
    REAL (KIND=RK) PD (0:251)
    REAL (KIND=RK) PM (0:251)
    REAL (KIND=RK) S1D
    REAL (KIND=RK) S1F
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SW
    REAL (KIND=RK) X

    EPS = 1.0D-14

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    NM = 25 + INT ((N-M)/2+C)
    NM2 = 2 * NM + M
    CALL SDMN (M, N, C, CV, KD, DF)
    CALL LPMNS (M, NM2, X, PM, PD)
    SU1 = 0.0D+00
    DO K = 1, NM
        MK = M + 2 * (K-1) + IP
        SU1 = SU1 + DF (K) * PM (MK)
        IF (ABS(SW-SU1) < ABS(SU1)*EPS) THEN
            EXIT
        END IF
        SW = SU1
    END DO

    S1F = (-1.0D+00) ** M * SU1

    SU1 = 0.0D+00
    DO K = 1, NM
        MK = M + 2 * (K-1) + IP
        SU1 = SU1 + DF (K) * PD (MK)
        IF (ABS(SW-SU1) < ABS(SU1)*EPS) THEN
            EXIT
        END IF
        SW = SU1
    END DO

    S1D = (-1.0D+00) ** M * SU1

    RETURN
END

!******************************************************************************

SUBROUTINE RSWFO (M, N, C, X, CV, KF, R1F, R1D, R2F, R2D)

!******************************************************************************
!
!! RSWFO computes prolate spheroidal radial function of first and second kinds.
!
!  Discussion:
!
!    This procedure computes oblate radial functions of the first
!    and second kinds, and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, integer KF, the function code.
!    1, for the first kind
!    2, for the second kind
!    3, for both the first and second kinds.
!
!    Output, real ( kind = rk ) R1F, the radial function of the first kind;
!
!    Output, real ( kind = rk ) R1D, the derivative of the radial function of
!    the first kind;
!
!    Output, real ( kind = rk ) R2F, the radial function of the second kind;
!
!    Output, real ( kind = rk ) R2D, the derivative of the radial function of
!    the second kind;
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    INTEGER ID
    INTEGER KD
    INTEGER KF
    INTEGER M
    INTEGER N
    REAL (KIND=RK) R1D
    REAL (KIND=RK) R1F
    REAL (KIND=RK) R2D
    REAL (KIND=RK) R2F
    REAL (KIND=RK) X

    KD = - 1
    CALL SDMN (M, N, C, CV, KD, DF)

    IF (KF /= 2) THEN
        CALL RMN1 (M, N, C, X, DF, KD, R1F, R1D)
    END IF

    IF (1 < KF) THEN
        ID = 10
        IF (1.0D-08 < X) THEN
            CALL RMN2L (M, N, C, X, DF, KD, R2F, R2D, ID)
        END IF
        IF (-1 < ID) THEN
            CALL RMN2SO (M, N, C, X, CV, DF, KD, R2F, R2D)
        END IF
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE RSWFP (M, N, C, X, CV, KF, R1F, R1D, R2F, R2D)

!******************************************************************************
!
!! RSWFP computes prolate spheroidal radial function of first and second kinds.
!
!  Discussion:
!
!    This procedure computes prolate spheriodal radial functions of the
!    first and second kinds, and their derivatives.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument of the radial function, 1 < X.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, integer KF, the function code.
!    1, for the first kind
!    2, for the second kind
!    3, for both the first and second kinds.
!
!    Output, real ( kind = rk ) R1F, the radial function of the first kind;
!
!    Output, real ( kind = rk ) R1D, the derivative of the radial function of
!    the first kind;
!
!    Output, real ( kind = rk ) R2F, the radial function of the second kind;
!
!    Output, real ( kind = rk ) R2D, the derivative of the radial function of
!    the second kind;
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    INTEGER ID
    INTEGER KD
    INTEGER KF
    INTEGER M
    INTEGER N
    REAL (KIND=RK) R1D
    REAL (KIND=RK) R1F
    REAL (KIND=RK) R2D
    REAL (KIND=RK) R2F
    REAL (KIND=RK) X

    KD = 1
    CALL SDMN (M, N, C, CV, KD, DF)

    IF (KF /= 2) THEN
        CALL RMN1 (M, N, C, X, DF, KD, R1F, R1D)
    END IF

    IF (1 < KF) THEN
        CALL RMN2L (M, N, C, X, DF, KD, R2F, R2D, ID)
        IF (-8 < ID) THEN
            CALL RMN2SP (M, N, C, X, CV, DF, KD, R2F, R2D)
        END IF
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE LPMNS (M, N, X, PM, PD)

!******************************************************************************
!
!! LPMNS computes associated Legendre functions Pmn(X) and derivatives P'mn(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the order of Pmn(x).
!
!    Input, integer N, the degree of Pmn(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) PM(0:N), PD(0:N), the values and derivatives
!    of the function from degree 0 to N.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    INTEGER K
    INTEGER M
    REAL (KIND=RK) PM (0:N)
    REAL (KIND=RK) PM0
    REAL (KIND=RK) PM1
    REAL (KIND=RK) PM2
    REAL (KIND=RK) PMK
    REAL (KIND=RK) PD (0:N)
    REAL (KIND=RK) X
    REAL (KIND=RK) X0

    DO K = 0, N
        PM (K) = 0.0D+00
        PD (K) = 0.0D+00
    END DO

    IF (ABS(X) == 1.0D+00) THEN

        DO K = 0, N
            IF (M == 0) THEN
                PM (K) = 1.0D+00
                PD (K) = 0.5D+00 * K * (K+1.0D+00)
                IF (X < 0.0D+00) THEN
                    PM (K) = (-1.0D+00) ** K * PM (K)
                    PD (K) = (-1.0D+00) ** (K+1) * PD (K)
                END IF
            ELSE IF (M == 1) THEN
                PD (K) = 1.0D+300
            ELSE IF (M == 2) THEN
                PD (K) = - 0.25D+00 * (K+2.0D+00) * (K+1.0D+00) * K * (K-1.0D+00)
                IF (X < 0.0D+00) THEN
                    PD (K) = (-1.0D+00) ** (K+1) * PD (K)
                END IF
            END IF
        END DO
        RETURN
    END IF

    X0 = ABS (1.0D+00-X*X)
    PM0 = 1.0D+00
    PMK = PM0
    DO K = 1, M
        PMK = (2.0D+00*K-1.0D+00) * SQRT (X0) * PM0
        PM0 = PMK
    END DO
    PM1 = (2.0D+00*M+1.0D+00) * X * PM0
    PM (M) = PMK
    PM (M+1) = PM1
    DO K = M + 2, N
        PM2 = ((2.0D+00*K-1.0D+00)*X*PM1-(K+M-1.0D+00)*PMK) / (K-M)
        PM (K) = PM2
        PMK = PM1
        PM1 = PM2
    END DO

    PD (0) = ((1.0D+00-M)*PM(1)-X*PM(0)) / (X*X-1.0D+00)
    DO K = 1, N
        PD (K) = (K*X*PM(K)-(K+M)*PM(K-1)) / (X*X-1.0D+00)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE LQMNS (M, N, X, QM, QD)

!******************************************************************************
!
!! LQMNS computes associated Legendre functions Qmn(x) and derivatives Qmn'(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the order.
!
!    Input, integer N, the degree.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) QM(0:N), QD(0:N), the values of Qmn(x)
!    and Qmn'(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    INTEGER K
    INTEGER KM
    INTEGER L
    INTEGER LS
    INTEGER M
    REAL (KIND=RK) Q0
    REAL (KIND=RK) Q00
    REAL (KIND=RK) Q01
    REAL (KIND=RK) Q0L
    REAL (KIND=RK) Q10
    REAL (KIND=RK) Q11
    REAL (KIND=RK) Q1L
    REAL (KIND=RK) QD (0:N)
    REAL (KIND=RK) QF0
    REAL (KIND=RK) QF1
    REAL (KIND=RK) QF2
    REAL (KIND=RK) QG0
    REAL (KIND=RK) QG1
    REAL (KIND=RK) QH0
    REAL (KIND=RK) QH1
    REAL (KIND=RK) QH2
    REAL (KIND=RK) QM (0:N)
    REAL (KIND=RK) QM0
    REAL (KIND=RK) QM1
    REAL (KIND=RK) QMK
    REAL (KIND=RK) X
    REAL (KIND=RK) XQ

    DO K = 0, N
        QM (K) = 0.0D+00
        QD (K) = 0.0D+00
    END DO

    IF (ABS(X) == 1.0D+00) THEN
        DO K = 0, N
            QM (K) = 1.0D+300
            QD (K) = 1.0D+300
        END DO
        RETURN
    END IF

    IF (1.0D+00 < ABS(X)) THEN
        LS = - 1
    ELSE
        LS = + 1
    END IF

    XQ = SQRT (LS*(1.0D+00-X*X))
    Q0 = 0.5D+00 * LOG (ABS((X+1.0D+00)/(X-1.0D+00)))
    Q00 = Q0
    Q10 = - 1.0D+00 / XQ
    Q01 = X * Q0 - 1.0D+00
    Q11 = - LS * XQ * (Q0+X/(1.0D+00-X*X))
    QF0 = Q00
    QF1 = Q10
    DO K = 2, M
        QM0 = - 2.0D+00 * (K-1.0D+00) / XQ * X * QF1 - LS * (K-1.0D+00) * (2.0D+00-K) * QF0
        QF0 = QF1
        QF1 = QM0
    END DO

    IF (M == 0) THEN
        QM0 = Q00
    ELSE IF (M == 1) THEN
        QM0 = Q10
    END IF

    QM (0) = QM0

    IF (ABS(X) < 1.0001D+00) THEN

        IF (M == 0 .AND. 0 < N) THEN

            QF0 = Q00
            QF1 = Q01
            DO K = 2, N
                QF2 = ((2.0D+00*K-1.0D+00)*X*QF1-(K-1.0D+00)*QF0) / K
                QM (K) = QF2
                QF0 = QF1
                QF1 = QF2
            END DO

        END IF
        QG0 = Q01
        QG1 = Q11
        DO K = 2, M
            QM1 = - 2.0D+00 * (K-1.0D+00) / XQ * X * QG1 - LS * K * (3.0D+00-K) * QG0
            QG0 = QG1
            QG1 = QM1
        END DO

        IF (M == 0) THEN
            QM1 = Q01
        ELSE IF (M == 1) THEN
            QM1 = Q11
        END IF
        QM (1) = QM1

        IF (M == 1 .AND. 1 < N) THEN

            QH0 = Q10
            QH1 = Q11
            DO K = 2, N
                QH2 = ((2.0D+00*K-1.0D+00)*X*QH1-K*QH0) / (K-1.0D+00)
                QM (K) = QH2
                QH0 = QH1
                QH1 = QH2
            END DO

        ELSE IF (2 <= M) THEN

            QG0 = Q00
            QG1 = Q01
            QH0 = Q10
            QH1 = Q11

            DO L = 2, N
                Q0L = ((2.0D+00*L-1.0D+00)*X*QG1-(L-1.0D+00)*QG0) / L
                Q1L = ((2.0D+00*L-1.0D+00)*X*QH1-L*QH0) / (L-1.0D+00)
                QF0 = Q0L
                QF1 = Q1L
                DO K = 2, M
                    QMK = - 2.0D+00 * (K-1.0D+00) / XQ * X * QF1 - LS * (K+L-1.0D+00) * &
                   & (L+2.0D+00-K) * QF0
                    QF0 = QF1
                    QF1 = QMK
                END DO
                QM (L) = QMK
                QG0 = QG1
                QG1 = Q0L
                QH0 = QH1
                QH1 = Q1L
            END DO

        END IF

    ELSE

        IF (1.1D+00 < ABS(X)) THEN
            KM = 40 + M + N
        ELSE
            KM = (40+M+N) * INT (-1.0D+00-1.8D+00*LOG(X-1.0D+00))
        END IF

        QF2 = 0.0D+00
        QF1 = 1.0D+00
        DO K = KM, 0, - 1
            QF0 = ((2.0D+00*K+3.0D+00)*X*QF1-(K+2.0D+00-M)*QF2) / (K+M+1.0D+00)
            IF (K <= N) THEN
                QM (K) = QF0
            END IF
            QF2 = QF1
            QF1 = QF0
        END DO

        DO K = 0, N
            QM (K) = QM (K) * QM0 / QF0
        END DO

    END IF

    IF (ABS(X) < 1.0D+00) THEN
        DO K = 0, N
            QM (K) = (-1) ** M * QM (K)
        END DO
    END IF

    QD (0) = ((1.0D+00-M)*QM(1)-X*QM(0)) / (X*X-1.0D+00)
    DO K = 1, N
        QD (K) = (K*X*QM(K)-(K+M)*QM(K-1)) / (X*X-1.0D+00)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CBK (M, N, C, CV, QT, CK, BK)

!******************************************************************************
!
!! cbk() computes coefficients for oblate radial functions with small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    20 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, real ( kind = rk ) QT, ?
!
!    Input, real ( kind = rk ) CK(*), ?
!
!    Output, real ( kind = rk ) BK(*), the coefficients.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BK (200)
    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CV
    REAL (KIND=RK) EPS
    INTEGER I
    INTEGER I1
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER M
    INTEGER N
    INTEGER N2
    INTEGER NM
    REAL (KIND=RK) QT
    REAL (KIND=RK) R1
    REAL (KIND=RK) S1
    REAL (KIND=RK) SW
    REAL (KIND=RK) T
    REAL (KIND=RK) U (200)
    REAL (KIND=RK) V (200)
    REAL (KIND=RK) W (200)

    EPS = 1.0D-14
    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF
    NM = 25 + INT (0.5D+00*(N-M)+C)
    U (1) = 0.0D+00
    N2 = NM - 2
    DO J = 2, N2
        U (J) = C * C
    END DO

    DO J = 1, N2
        V (J) = (2.0D+00*J-1.0D+00-IP) * (2.0D+00*(J-M)-IP) + M * (M-1.0D+00) - CV
    END DO

    DO J = 1, NM - 1
        W (J) = (2.0D+00*J-IP) * (2.0D+00*J+1.0D+00-IP)
    END DO

    IF (IP == 0) THEN

        DO K = 0, N2 - 1

            S1 = 0.0D+00
            I1 = K - M + 1

            DO I = I1, NM
                IF (0 <= I) THEN
                    R1 = 1.0D+00
                    DO J = 1, K
                        R1 = R1 * (I+M-J) / J
                    END DO
                    S1 = S1 + CK (I+1) * (2.0D+00*I+M) * R1
                    IF (ABS(S1-SW) < ABS(S1)*EPS) THEN
                        EXIT
                    END IF
                    SW = S1
                END IF
            END DO

            BK (K+1) = QT * S1

        END DO

    ELSE IF (IP == 1) THEN

        DO K = 0, N2 - 1

            S1 = 0.0D+00
            I1 = K - M + 1

            DO I = I1, NM

                IF (0 <= I) THEN

                    R1 = 1.0D+00
                    DO J = 1, K
                        R1 = R1 * (I+M-J) / J
                    END DO

                    IF (0 < I) THEN
                        S1 = S1 + CK (I) * (2.0D+00*I+M-1) * R1
                    END IF
                    S1 = S1 - CK (I+1) * (2.0D+00*I+M) * R1
                    IF (ABS(S1-SW) < ABS(S1)*EPS) THEN
                        EXIT
                    END IF
                    SW = S1

                END IF

            END DO

            BK (K+1) = QT * S1

        END DO

    END IF

    W (1) = W (1) / V (1)
    BK (1) = BK (1) / V (1)
    DO K = 2, N2
        T = V (K) - W (K-1) * U (K)
        W (K) = W (K) / T
        BK (K) = (BK(K)-BK(K-1)*U(K)) / T
    END DO

    DO K = N2 - 1, 1, - 1
        BK (K) = BK (K) - W (K) * BK (K+1)
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE GMN (M, N, C, X, BK, GF, GD)

!******************************************************************************
!
!! GMN computes quantities for oblate radial functions with small argument.
!
!  Discussion:
!
!    This procedure computes Gmn(-ic,ix) and its derivative for oblate
!    radial functions with a small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) BK(*), coefficients.
!
!    Output, real ( kind = rk ) GF, GD, the value of Gmn(-C,X) and Gmn'(-C,X).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BK (200)
    REAL (KIND=RK) C
    REAL (KIND=RK) EPS
    REAL (KIND=RK) GD
    REAL (KIND=RK) GD0
    REAL (KIND=RK) GD1
    REAL (KIND=RK) GF
    REAL (KIND=RK) GF0
    REAL (KIND=RK) GW
    INTEGER IP
    INTEGER K
    INTEGER M
    INTEGER N
    INTEGER NM
    REAL (KIND=RK) X
    REAL (KIND=RK) XM

    EPS = 1.0D-14

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    NM = 25 + INT (0.5D+00*(N-M)+C)
    XM = (1.0D+00+X*X) ** (-0.5D+00*M)
    GF0 = 0.0D+00
    DO K = 1, NM
        GF0 = GF0 + BK (K) * X ** (2.0D+00*K-2.0D+00)
        IF (ABS((GF0-GW)/GF0) < EPS .AND. 10 <= K) THEN
            EXIT
        END IF
        GW = GF0
    END DO

    GF = XM * GF0 * X ** (1-IP)

    GD1 = - M * X / (1.0D+00+X*X) * GF
    GD0 = 0.0D+00

    DO K = 1, NM

        IF (IP == 0) THEN
            GD0 = GD0 + (2.0D+00*K-1.0D+00) * BK (K) * X ** (2.0D+00*K-2.0D+00)
        ELSE
            GD0 = GD0 + 2.0D+00 * K * BK (K+1) * X ** (2.0D+00*K-1.0D+00)
        END IF

        IF (ABS((GD0-GW)/GD0) < EPS .AND. 10 <= K) THEN
            EXIT
        END IF

        GW = GD0

    END DO

    GD = GD1 + XM * GD0

    RETURN
END

!******************************************************************************

SUBROUTINE KMN (M, N, C, CV, KD, DF, DN, CK1, CK2)

!******************************************************************************
!
!! KMN: expansion coefficients of prolate or oblate spheroidal functions.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    02 August 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CK1
    REAL (KIND=RK) CK2
    REAL (KIND=RK) CS
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DN (200)
    REAL (KIND=RK) DNP
    REAL (KIND=RK) G0
    REAL (KIND=RK) GK0
    REAL (KIND=RK) GK1
    REAL (KIND=RK) GK2
    REAL (KIND=RK) GK3
    INTEGER I
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER KD
    INTEGER L
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM1
    INTEGER NN
    REAL (KIND=RK) R
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) R3
    REAL (KIND=RK) R4
    REAL (KIND=RK) R5
    REAL (KIND=RK) RV (200)
    REAL (KIND=RK) SA0
    REAL (KIND=RK) SB0
    REAL (KIND=RK) SU0
    REAL (KIND=RK) SW
    REAL (KIND=RK) T
    REAL (KIND=RK) TP (200)
    REAL (KIND=RK) U (200)
    REAL (KIND=RK) V (200)
    REAL (KIND=RK) W (200)

    NM = 25 + INT (0.5D+00*(N-M)+C)
    NN = NM + M
    CS = C * C * KD

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    DO I = 1, NN + 3

        IF (IP == 0) THEN
            K = - 2 * (I-1)
        ELSE
            K = - (2*I-3)
        END IF

        GK0 = 2.0D+00 * M + K
        GK1 = (M+K) * (M+K+1.0D+00)
        GK2 = 2.0D+00 * (M+K) - 1.0D+00
        GK3 = 2.0D+00 * (M+K) + 3.0D+00
        U (I) = GK0 * (GK0-1.0D+00) * CS / (GK2*(GK2+2.0D+00))
        V (I) = GK1 - CV + (2.0D+00*(GK1-M*M)-1.0D+00) * CS / (GK2*GK3)
        W (I) = (K+1.0D+00) * (K+2.0D+00) * CS / ((GK2+2.0D+00)*GK3)

    END DO

    DO K = 1, M
        T = V (M+1)
        DO L = 0, M - K - 1
            T = V (M-L) - W (M-L+1) * U (M-L) / T
        END DO
        RV (K) = - U (K) / T
    END DO

    R = 1.0D+00
    DO K = 1, M
        R = R * RV (K)
        DN (K) = DF (1) * R
    END DO

    TP (NN) = V (NN+1)
    DO K = NN - 1, M + 1, - 1
        TP (K) = V (K+1) - W (K+2) * U (K+1) / TP (K+1)
        IF (M+1 < K) THEN
            RV (K) = - U (K) / TP (K)
        END IF
    END DO

    IF (M == 0) THEN
        DNP = DF (1)
    ELSE
        DNP = DN (M)
    END IF

    DN (M+1) = (-1.0D+00) ** IP * DNP * CS / &
   & ((2.0D+00*M-1.0D+00)*(2.0D+00*M+1.0D+00-4.0D+00*IP)*TP(M+1))
    DO K = M + 2, NN
        DN (K) = RV (K) * DN (K-1)
    END DO

    R1 = 1.0D+00
    DO J = 1, (N+M+IP) / 2
        R1 = R1 * (J+0.5D+00*(N+M+IP))
    END DO
    NM1 = (N-M) / 2
    R = 1.0D+00
    DO J = 1, 2 * M + IP
        R = R * J
    END DO
    SU0 = R * DF (1)

    DO K = 2, NM
        R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        SU0 = SU0 + R * DF (K)
        IF (NM1 < K .AND. ABS((SU0-SW)/SU0) < 1.0D-14) THEN
            EXIT
        END IF
        SW = SU0
    END DO

    IF (KD /= 1) THEN

        R2 = 1.0D+00
        DO J = 1, M
            R2 = 2.0D+00 * C * R2 * J
        END DO
        R3 = 1.0D+00
        DO J = 1, (N-M-IP) / 2
            R3 = R3 * J
        END DO
        SA0 = (2.0D+00*(M+IP)+1.0D+00) * R1 / (2.0D+00**N*C**IP*R2*R3*DF(1))
        CK1 = SA0 * SU0

        IF (KD ==-1) THEN
            RETURN
        END IF

    END IF

    R4 = 1.0D+00
    DO J = 1, (N-M-IP) / 2
        R4 = 4.0D+00 * R4 * J
    END DO
    R5 = 1.0D+00
    DO J = 1, M
        R5 = R5 * (J+M) / C
    END DO

    IF (M == 0) THEN
        G0 = DF (1)
    ELSE
        G0 = DN (M)
    END IF

    SB0 = (IP+1.0D+00) * C ** (IP+1) / (2.0D+00*IP*(M-2.0D+00)+1.0D+00) / (2.0D+00*M-1.0D+00)

    CK2 = (-1) ** IP * SB0 * R4 * R5 * G0 / R1 * SU0

    RETURN
END

!******************************************************************************

SUBROUTINE QSTAR (M, N, C, CK, CK1, QS, QT)

!******************************************************************************
!
!! QSTAR computes Q*mn(-ic) for oblate radial functions with a small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    18 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) CK(*), ?
!
!    Input, real ( kind = rk ) CK1, ?
!
!    Output, real ( kind = rk ) QS, ?
!
!    Output, real ( kind = rk ) QT, ?
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) AP (200)
    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CK1
    INTEGER I
    INTEGER IP
    INTEGER K
    INTEGER L
    INTEGER M
    INTEGER N
    REAL (KIND=RK) QS
    REAL (KIND=RK) QS0
    REAL (KIND=RK) QT
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SK

    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    R = 1.0D+00 / CK (1) ** 2
    AP (1) = R
    DO I = 1, M
        S = 0.0D+00
        DO L = 1, I
            SK = 0.0D+00
            DO K = 0, L
                SK = SK + CK (K+1) * CK (L-K+1)
            END DO
            S = S + SK * AP (I-L+1)
        END DO
        AP (I+1) = - R * S
    END DO

    QS0 = AP (M+1)
    DO L = 1, M
        R = 1.0D+00
        DO K = 1, L
            R = R * (2.0D+00*K+IP) * (2.0D+00*K-1.0D+00+IP) / (2.0D+00*K) ** 2
        END DO
        QS0 = QS0 + AP (M-L+1) * R
    END DO

    QS = (-1.0D+00) ** IP * CK1 * (CK1*QS0) / C
    QT = - 2.0D+00 / CK1 * QS

    RETURN
END

!******************************************************************************

SUBROUTINE RMN1 (M, N, C, X, DF, KD, R1F, R1D)

!******************************************************************************
!
!! RMN1 computes prolate and oblate spheroidal functions of the first kind.
!
!  Discussion:
!
!    This procedure computes prolate and oblate spheroidal radial
!    functions of the first kind for given m, n, c and x.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    29 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) R1F, R1D, the function and derivative.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) B0
    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CX
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DJ (0:251)
    REAL (KIND=RK) EPS
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER KD
    INTEGER L
    INTEGER LG
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM1
    INTEGER NM2
    INTEGER NP
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R1
    REAL (KIND=RK) R1D
    REAL (KIND=RK) R1F
    REAL (KIND=RK) R2
    REAL (KIND=RK) R3
    REAL (KIND=RK) REG
    REAL (KIND=RK) SA0
    REAL (KIND=RK) SJ (0:251)
    REAL (KIND=RK) SUC
    REAL (KIND=RK) SUD
    REAL (KIND=RK) SUM
    REAL (KIND=RK) SW
    REAL (KIND=RK) SW1
    REAL (KIND=RK) X

    EPS = 1.0D-14
    NM1 = INT ((N-M)/2)
    IF (N-M == 2*NM1) THEN
        IP = 0
    ELSE
        IP = 1
    END IF
    NM = 25 + NM1 + INT (C)
    REG = 1.0D+00
    IF (80 < M+NM) THEN
        REG = 1.0D-200
    END IF
    R0 = REG
    DO J = 1, 2 * M + IP
        R0 = R0 * J
    END DO
    R = R0
    SUC = R * DF (1)
    DO K = 2, NM
        R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        SUC = SUC + R * DF (K)

        IF (NM1 < K .AND. ABS(SUC-SW) < ABS(SUC)*EPS) THEN
            EXIT
        END IF

        SW = SUC

    END DO

    IF (X == 0.0D+00) THEN

        CALL SCKB (M, N, C, DF, CK)
        SUM = 0.0D+00
        DO J = 1, NM
            SUM = SUM + CK (J)
            IF (ABS(SUM-SW1) < ABS(SUM)*EPS) THEN
                EXIT
            END IF
            SW1 = SUM
        END DO

        R1 = 1.0D+00
        DO J = 1, (N+M+IP) / 2
            R1 = R1 * (J+0.5D+00*(N+M+IP))
        END DO

        R2 = 1.0D+00
        DO J = 1, M
            R2 = 2.0D+00 * C * R2 * J
        END DO

        R3 = 1.0D+00
        DO J = 1, (N-M-IP) / 2
            R3 = R3 * J
        END DO

        SA0 = (2.0D+00*(M+IP)+1.0D+00) * R1 / (2.0D+00**N*C**IP*R2*R3)

        IF (IP == 0) THEN
            R1F = SUM / (SA0*SUC) * DF (1) * REG
            R1D = 0.0D+00
        ELSE IF (IP == 1) THEN
            R1F = 0.0D+00
            R1D = SUM / (SA0*SUC) * DF (1) * REG
        END IF

        RETURN

    END IF

    CX = C * X
    NM2 = 2 * NM + M
    CALL SPHJ (NM2, CX, NM2, SJ, DJ)
    A0 = (1.0D+00-KD/(X*X)) ** (0.5D+00*M) / SUC
    R1F = 0.0D+00
    DO K = 1, NM
        L = 2 * K + M - N - 2 + IP
        IF (L == 4*INT(L/4)) THEN
            LG = 1
        ELSE
            LG = - 1
        END IF
        IF (K == 1) THEN
            R = R0
        ELSE
            R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        END IF
        NP = M + 2 * K - 2 + IP
        R1F = R1F + LG * R * DF (K) * SJ (NP)
        IF (NM1 < K .AND. ABS(R1F-SW) < ABS(R1F)*EPS) THEN
            EXIT
        END IF
        SW = R1F
    END DO

    R1F = R1F * A0
    B0 = KD * M / X ** 3.0D+00 / (1.0D+00-KD/(X*X)) * R1F
    SUD = 0.0D+00

    DO K = 1, NM

        L = 2 * K + M - N - 2 + IP

        IF (L == 4*INT(L/4)) THEN
            LG = 1
        ELSE
            LG = - 1
        END IF

        IF (K == 1) THEN
            R = R0
        ELSE
            R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        END IF

        NP = M + 2 * K - 2 + IP
        SUD = SUD + LG * R * DF (K) * DJ (NP)
        IF (NM1 < K .AND. ABS(SUD-SW) < ABS(SUD)*EPS) THEN
            EXIT
        END IF
        SW = SUD
    END DO

    R1D = B0 + A0 * C * SUD

    RETURN
END

!******************************************************************************

SUBROUTINE RMN2L (M, N, C, X, DF, KD, R2F, R2D, ID)

!******************************************************************************
!
!! RMN2L: prolate and oblate spheroidal functions, second kind, large CX.
!
!  Discussion:
!
!    This procedure computes prolate and oblate spheroidal radial functions
!    of the second kind for given m, n, c and a large cx.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    30 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) R2F, R2D, the function and derivative values.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    REAL (KIND=RK) B0
    REAL (KIND=RK) C
    REAL (KIND=RK) CX
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DY (0:251)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) EPS1
    REAL (KIND=RK) EPS2
    INTEGER ID
    INTEGER ID1
    INTEGER ID2
    INTEGER IP
    INTEGER J
    INTEGER K
    INTEGER KD
    INTEGER L
    INTEGER LG
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM1
    INTEGER NM2
    INTEGER NP
    REAL (KIND=RK) R
    REAL (KIND=RK) R0
    REAL (KIND=RK) R2D
    REAL (KIND=RK) R2F
    REAL (KIND=RK) REG
    REAL (KIND=RK) SW
    REAL (KIND=RK) SUC
    REAL (KIND=RK) SUD
    REAL (KIND=RK) SY (0:251)
    REAL (KIND=RK) X

    EPS = 1.0D-14

    NM1 = INT ((N-M)/2)

    IF (N-M == 2*NM1) THEN
        IP = 0
    ELSE
        IP = 1
    END IF
    NM = 25 + NM1 + INT (C)

    IF (80 < M+NM) THEN
        REG = 1.0D-200
    ELSE
        REG = 1.0D+00
    END IF
    NM2 = 2 * NM + M
    CX = C * X
    CALL SPHY (NM2, CX, NM2, SY, DY)
    R0 = REG
    DO J = 1, 2 * M + IP
        R0 = R0 * J
    END DO
    R = R0
    SUC = R * DF (1)
    DO K = 2, NM
        R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        SUC = SUC + R * DF (K)
        IF (NM1 < K .AND. ABS(SUC-SW) < ABS(SUC)*EPS) THEN
            EXIT
        END IF
        SW = SUC
    END DO

    A0 = (1.0D+00-KD/(X*X)) ** (0.5D+00*M) / SUC
    R2F = 0.0D+00
    DO K = 1, NM
        L = 2 * K + M - N - 2 + IP
        IF (L == 4*INT(L/4)) THEN
            LG = 1
        ELSE
            LG = - 1
        END IF

        IF (K == 1) THEN
            R = R0
        ELSE
            R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        END IF

        NP = M + 2 * K - 2 + IP
        R2F = R2F + LG * R * (DF(K)*SY(NP))
        EPS1 = ABS (R2F-SW)
        IF (NM1 < K .AND. EPS1 < ABS(R2F)*EPS) THEN
            EXIT
        END IF
        SW = R2F
    END DO

    ID1 = INT (LOG10(EPS1/ABS(R2F)+EPS))
    R2F = R2F * A0

    IF (NM2 <= NP) THEN
        ID = 10
        RETURN
    END IF

    B0 = KD * M / X ** 3.0D+00 / (1.0D+00-KD/(X*X)) * R2F
    SUD = 0.0D+00
    DO K = 1, NM
        L = 2 * K + M - N - 2 + IP
        IF (L == 4*INT(L/4)) THEN
            LG = 1
        ELSE
            LG = - 1
        END IF
        IF (K == 1) THEN
            R = R0
        ELSE
            R = R * (M+K-1.0D+00) * (M+K+IP-1.5D+00) / (K-1.0D+00) / (K+IP-1.5D+00)
        END IF
        NP = M + 2 * K - 2 + IP
        SUD = SUD + LG * R * (DF(K)*DY(NP))
        EPS2 = ABS (SUD-SW)
        IF (NM1 < K .AND. EPS2 < ABS(SUD)*EPS) THEN
            EXIT
        END IF
        SW = SUD
    END DO

    R2D = B0 + A0 * C * SUD
    ID2 = INT (LOG10(EPS2/ABS(SUD)+EPS))
    ID = MAX (ID1, ID2)

    RETURN
END

!******************************************************************************

SUBROUTINE RMN2SO (M, N, C, X, CV, DF, KD, R2F, R2D)

!******************************************************************************
!
!! RMN2SO: oblate radial functions of the second kind with small argument.
!
!  Discussion:
!
!    This procedure computes oblate radial functions of the second kind
!    with a small argument, Rmn(-ic,ix) and Rmn'(-ic,ix).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    27 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) R2F, R2D, the values of Rmn(-ic,ix)
!    and Rmn'(-ic,ix).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BK (200)
    REAL (KIND=RK) C
    REAL (KIND=RK) CK (200)
    REAL (KIND=RK) CK1
    REAL (KIND=RK) CK2
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DN (200)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) GD
    REAL (KIND=RK) GF
    REAL (KIND=RK) H0
    INTEGER IP
    INTEGER J
    INTEGER KD
    INTEGER M
    INTEGER N
    INTEGER NM
    REAL (KIND=RK) PI
    REAL (KIND=RK) QS
    REAL (KIND=RK) QT
    REAL (KIND=RK) R1D
    REAL (KIND=RK) R1F
    REAL (KIND=RK) R2D
    REAL (KIND=RK) R2F
    REAL (KIND=RK) SUM
    REAL (KIND=RK) SW
    REAL (KIND=RK) X

    IF (ABS(DF(1)) <= 1.0D-280) THEN
        R2F = 1.0D+300
        R2D = 1.0D+300
        RETURN
    END IF

    EPS = 1.0D-14
    PI = 3.141592653589793D+00
    NM = 25 + INT ((N-M)/2+C)
    IF (N-M == 2*INT((N-M)/2)) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    CALL SCKB (M, N, C, DF, CK)
    CALL KMN (M, N, C, CV, KD, DF, DN, CK1, CK2)
    CALL QSTAR (M, N, C, CK, CK1, QS, QT)
    CALL CBK (M, N, C, CV, QT, CK, BK)

    IF (X == 0.0D+00) THEN

        SUM = 0.0D+00
        DO J = 1, NM
            SUM = SUM + CK (J)
            IF (ABS(SUM-SW) < ABS(SUM)*EPS) THEN
                EXIT
            END IF
            SW = SUM
        END DO

        IF (IP == 0) THEN
            R1F = SUM / CK1
            R2F = - 0.5D+00 * PI * QS * R1F
            R2D = QS * R1F + BK (1)
        ELSE IF (IP == 1) THEN
            R1D = SUM / CK1
            R2F = BK (1)
            R2D = - 0.5D+00 * PI * QS * R1D
        END IF

        RETURN

    ELSE

        CALL GMN (M, N, C, X, BK, GF, GD)
        CALL RMN1 (M, N, C, X, DF, KD, R1F, R1D)
        H0 = ATAN (X) - 0.5D+00 * PI
        R2F = QS * R1F * H0 + GF
        R2D = QS * (R1D*H0+R1F/(1.0D+00+X*X)) + GD

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE RMN2SP (M, N, C, X, CV, DF, KD, R2F, R2D)

!******************************************************************************
!
!! RMN2SP: prolate, oblate spheroidal radial functions, kind 2, small argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    28 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer M, the mode parameter;  M = 0, 1, 2, ...
!
!    Input, integer N, mode parameter, N = M, M + 1, M + 2, ...
!
!    Input, real ( kind = rk ) C, spheroidal parameter.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Input, real ( kind = rk ) CV, the characteristic value.
!
!    Input, real ( kind = rk ) DF(*), the expansion coefficients.
!
!    Input, integer KD, the function code.
!    1, the prolate function.
!    -1, the oblate function.
!
!    Output, real ( kind = rk ) R2F, R2D, the values of the function and
!    its derivative.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) CK1
    REAL (KIND=RK) CK2
    REAL (KIND=RK) CV
    REAL (KIND=RK) DF (200)
    REAL (KIND=RK) DN (200)
    REAL (KIND=RK) EPS
    REAL (KIND=RK) GA
    REAL (KIND=RK) GB
    REAL (KIND=RK) GC
    INTEGER IP
    INTEGER J
    INTEGER J1
    INTEGER J2
    INTEGER K
    INTEGER KD
    INTEGER KI
    INTEGER L1
    INTEGER M
    INTEGER N
    INTEGER NM
    INTEGER NM1
    INTEGER NM2
    INTEGER NM3
    REAL (KIND=RK) PD (0:251)
    REAL (KIND=RK) PM (0:251)
    REAL (KIND=RK) QD (0:251)
    REAL (KIND=RK) QM (0:251)
    REAL (KIND=RK) R1
    REAL (KIND=RK) R2
    REAL (KIND=RK) R2D
    REAL (KIND=RK) R2F
    REAL (KIND=RK) R3
    REAL (KIND=RK) R4
    REAL (KIND=RK) SD
    REAL (KIND=RK) SD0
    REAL (KIND=RK) SD1
    REAL (KIND=RK) SD2
    REAL (KIND=RK) SDM
    REAL (KIND=RK) SF
    REAL (KIND=RK) SPD1
    REAL (KIND=RK) SPD2
    REAL (KIND=RK) SPL
    REAL (KIND=RK) SU0
    REAL (KIND=RK) SU1
    REAL (KIND=RK) SU2
    REAL (KIND=RK) SUM
    REAL (KIND=RK) SW
    REAL (KIND=RK) X

    IF (ABS(DF(1)) < 1.0D-280) THEN
        R2F = 1.0D+300
        R2D = 1.0D+300
        RETURN
    END IF

    EPS = 1.0D-14

    NM1 = INT ((N-M)/2)

    IF (N-M == 2*NM1) THEN
        IP = 0
    ELSE
        IP = 1
    END IF

    NM = 25 + NM1 + INT (C)
    NM2 = 2 * NM + M
    CALL KMN (M, N, C, CV, KD, DF, DN, CK1, CK2)
    CALL LPMNS (M, NM2, X, PM, PD)
    CALL LQMNS (M, NM2, X, QM, QD)

    SU0 = 0.0D+00
    DO K = 1, NM
        J = 2 * K - 2 + M + IP
        SU0 = SU0 + DF (K) * QM (J)
        IF (NM1 < K .AND. ABS(SU0-SW) < ABS(SU0)*EPS) THEN
            EXIT
        END IF
        SW = SU0
    END DO

    SD0 = 0.0D+00

    DO K = 1, NM
        J = 2 * K - 2 + M + IP
        SD0 = SD0 + DF (K) * QD (J)
        IF (NM1 < K .AND. ABS(SD0-SW) < ABS(SD0)*EPS) THEN
            EXIT
        END IF
        SW = SD0
    END DO

    SU1 = 0.0D+00
    SD1 = 0.0D+00
    DO K = 1, M
        J = M - 2 * K + IP
        IF (J < 0) THEN
            J = - J - 1
        END IF
        SU1 = SU1 + DN (K) * QM (J)
        SD1 = SD1 + DN (K) * QD (J)
    END DO

    GA = ((X-1.0D+00)/(X+1.0D+00)) ** (0.5D+00*M)

    DO K = 1, M

        J = M - 2 * K + IP

        IF (0 <= J) THEN
            CYCLE
        END IF

        IF (J < 0) THEN
            J = - J - 1
        END IF
        R1 = 1.0D+00
        DO J1 = 1, J
            R1 = (M+J1) * R1
        END DO
        R2 = 1.0D+00
        DO J2 = 1, M - J - 2
            R2 = J2 * R2
        END DO
        R3 = 1.0D+00
        SF = 1.0D+00
        DO L1 = 1, J
            R3 = 0.5D+00 * R3 * (-J+L1-1.0D+00) * (J+L1) / ((M+L1)*L1) * (1.0D+00-X)
            SF = SF + R3
        END DO

        IF (M-J <= 1) THEN
            GB = 1.0D+00
        ELSE
            GB = (M-J-1.0D+00) * R2
        END IF

        SPL = R1 * GA * GB * SF
        SU1 = SU1 + (-1) ** (J+M) * DN (K) * SPL
        SPD1 = M / (X*X-1.0D+00) * SPL
        GC = 0.5D+00 * J * (J+1.0) / (M+1.0D+00)
        SD = 1.0D+00
        R4 = 1.0D+00
        DO L1 = 1, J - 1
            R4 = 0.5D+00 * R4 * (-J+L1) * (J+L1+1.0D+00) / ((M+L1+1.0D+00)*L1) * (1.0D+00-X)
            SD = SD + R4
        END DO

        SPD2 = R1 * GA * GB * GC * SD
        SD1 = SD1 + (-1) ** (J+M) * DN (K) * (SPD1+SPD2)

    END DO

    SU2 = 0.0D+00
    KI = (2*M+1+IP) / 2
    NM3 = NM + KI
    DO K = KI, NM3
        J = 2 * K - 1 - M - IP
        SU2 = SU2 + DN (K) * PM (J)
        IF (M < J .AND. ABS(SU2-SW) < ABS(SU2)*EPS) THEN
            EXIT
        END IF
        SW = SU2
    END DO

    SD2 = 0.0D+00

    DO K = KI, NM3
        J = 2 * K - 1 - M - IP
        SD2 = SD2 + DN (K) * PD (J)
        IF (M < J .AND. ABS(SD2-SW) < ABS(SD2)*EPS) THEN
            EXIT
        END IF
        SW = SD2
    END DO

    SUM = SU0 + SU1 + SU2
    SDM = SD0 + SD1 + SD2
    R2F = SUM / CK2
    R2D = SDM / CK2

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   16: ERROR FUNCTIONS AND FRESNEL INTEGRALS
!
!------------------------------------------------------------------------------

SUBROUTINE CERROR (Z, CER)

!******************************************************************************
!
!! cerror() computes the error function for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CER, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) C0
    COMPLEX (KIND=CK) CER
    COMPLEX (KIND=CK) CL
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CS
    INTEGER K
    REAL (KIND=RK) PI
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z1

    A0 = ABS (Z)
    C0 = EXP (-Z*Z)
    PI = 3.141592653589793D+00
    Z1 = Z

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        Z1 = - Z
    END IF

    IF (A0 <= 5.8D+00) THEN

        CS = Z1
        CR = Z1
        DO K = 1, 120
            CR = CR * Z1 * Z1 / (K+0.5D+00)
            CS = CS + CR
            IF (ABS(CR/CS) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        CER = 2.0D+00 * C0 * CS / SQRT (PI)

    ELSE

        CL = 1.0D+00 / Z1
        CR = CL
        DO K = 1, 13
            CR = - CR * (K-0.5D+00) / (Z1*Z1)
            CL = CL + CR
            IF (ABS(CR/CL) < 1.0D-15) THEN
                EXIT
            END IF
        END DO

        CER = 1.0D+00 - C0 * CL / SQRT (PI)

    END IF

    IF (REAL(Z, KIND=RK) < 0.0D+00) THEN
        CER = - CER
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CERZO (NT, ZO)

!******************************************************************************
!
!! cerzo() evaluates the complex zeros of the error function.
!
!  Discussion:
!
!    The modified Newton method is used.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    15 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer NT, the number of zeros.
!
!    Output, complex ( kind = ck ) ZO(NT), the zeros.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    INTEGER I
    INTEGER IT
    INTEGER J
    INTEGER NR
    REAL (KIND=RK) PI
    REAL (KIND=RK) PU
    REAL (KIND=RK) PV
    REAL (KIND=RK) PX
    REAL (KIND=RK) PY
    REAL (KIND=RK) W
    REAL (KIND=RK) W0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZF
    COMPLEX (KIND=CK) ZFD
    COMPLEX (KIND=CK) ZGD
    COMPLEX (KIND=CK) ZO (NT)
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZQ
    COMPLEX (KIND=CK) ZW

    PI = 3.141592653589793D+00

    DO NR = 1, NT

        PU = SQRT (PI*(4.0D+00*NR-0.5D+00))
        PV = PI * SQRT (2.0D+00*NR-0.25D+00)
        PX = 0.5D+00 * PU - 0.5D+00 * LOG (PV) / PU
        PY = 0.5D+00 * PU + 0.5D+00 * LOG (PV) / PU
        Z = CMPLX (PX, PY, KIND=CK)
        IT = 0

        DO

            IT = IT + 1
            CALL CERF (Z, ZF, ZD)
            ZP = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO I = 1, NR - 1
                ZP = ZP * (Z-ZO(I))
            END DO
            ZFD = ZF / ZP

            ZQ = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
            DO I = 1, NR - 1
                ZW = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        ZW = ZW * (Z-ZO(J))
                    END IF
                END DO
                ZQ = ZQ + ZW
            END DO

            ZGD = (ZD-ZQ*ZFD) / ZP
            Z = Z - ZFD / ZGD
            W0 = W
            W = ABS (Z)

            IF (50 < IT .OR. ABS((W-W0)/W) <= 1.0D-11) THEN
                EXIT
            END IF

        END DO

        ZO (NR) = Z

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE ERROR (X, ERR)

!******************************************************************************
!
!! ERROR evaluates the error function.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) ERR, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C0
    REAL (KIND=RK) EPS
    REAL (KIND=RK) ER
    REAL (KIND=RK) ERR
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    X2 = X * X

    IF (ABS(X) < 3.5D+00) THEN

        ER = 1.0D+00
        R = 1.0D+00

        DO K = 1, 50
            R = R * X2 / (K+0.5D+00)
            ER = ER + R
            IF (ABS(R) <= ABS(ER)*EPS) THEN
                EXIT
            END IF
        END DO

        C0 = 2.0D+00 / SQRT (PI) * X * EXP (-X2)
        ERR = C0 * ER

    ELSE

        ER = 1.0D+00
        R = 1.0D+00
        DO K = 1, 12
            R = - R * (K-0.5D+00) / X2
            ER = ER + R
        END DO

        C0 = EXP (-X2) / (ABS(X)*SQRT(PI))

        ERR = 1.0D+00 - C0 * ER
        IF (X < 0.0D+00) THEN
            ERR = - ERR
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE FCS (X, C, S)

!******************************************************************************
!
!! FCS computes Fresnel integrals C(x) and S(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    17 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) C, S, the function values.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) EPS
    REAL (KIND=RK) F
    REAL (KIND=RK) F0
    REAL (KIND=RK) F1
    REAL (KIND=RK) G
    INTEGER K
    INTEGER M
    REAL (KIND=RK) PI
    REAL (KIND=RK) PX
    REAL (KIND=RK) Q
    REAL (KIND=RK) R
    REAL (KIND=RK) S
    REAL (KIND=RK) SU
    REAL (KIND=RK) T
    REAL (KIND=RK) T0
    REAL (KIND=RK) T2
    REAL (KIND=RK) X
    REAL (KIND=RK) XA

    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    XA = ABS (X)
    PX = PI * XA
    T = 0.5D+00 * PX * XA
    T2 = T * T

    IF (XA == 0.0D+00) THEN

        C = 0.0D+00
        S = 0.0D+00

    ELSE IF (XA < 2.5D+00) THEN

        R = XA
        C = R
        DO K = 1, 50
            R = - 0.5D+00 * R * (4.0D+00*K-3.0D+00) / K / (2.0D+00*K-1.0D+00) / &
           & (4.0D+00*K+1.0D+00) * T2
            C = C + R
            IF (ABS(R) < ABS(C)*EPS) THEN
                EXIT
            END IF
        END DO

        S = XA * T / 3.0D+00
        R = S
        DO K = 1, 50
            R = - 0.5D+00 * R * (4.0D+00*K-1.0D+00) / K / (2.0D+00*K+1.0D+00) / &
           & (4.0D+00*K+3.0D+00) * T2
            S = S + R
            IF (ABS(R) < ABS(S)*EPS) THEN
                IF (X < 0.0D+00) THEN
                    C = - C
                    S = - S
                END IF
                RETURN
            END IF
        END DO

    ELSE IF (XA < 4.5D+00) THEN

        M = INT (42.0D+00+1.75D+00*T)
        SU = 0.0D+00
        C = 0.0D+00
        S = 0.0D+00
        F1 = 0.0D+00
        F0 = 1.0D-100

        DO K = M, 0, - 1
            F = (2.0D+00*K+3.0D+00) * F0 / T - F1
            IF (K == INT(K/2)*2) THEN
                C = C + F
            ELSE
                S = S + F
            END IF
            SU = SU + (2.0D+00*K+1.0D+00) * F * F
            F1 = F0
            F0 = F
        END DO

        Q = SQRT (SU)
        C = C * XA / Q
        S = S * XA / Q

    ELSE

        R = 1.0D+00
        F = 1.0D+00
        DO K = 1, 20
            R = - 0.25D+00 * R * (4.0D+00*K-1.0D+00) * (4.0D+00*K-3.0D+00) / T2
            F = F + R
        END DO
        R = 1.0D+00 / (PX*XA)
        G = R
        DO K = 1, 12
            R = - 0.25D+00 * R * (4.0D+00*K+1.0D+00) * (4.0D+00*K-1.0D+00) / T2
            G = G + R
        END DO

        T0 = T - INT (T/(2.0D+00*PI)) * 2.0D+00 * PI
        C = 0.5D+00 + (F*SIN(T0)-G*COS(T0)) / PX
        S = 0.5D+00 - (F*COS(T0)+G*SIN(T0)) / PX

    END IF

    IF (X < 0.0D+00) THEN
        C = - C
        S = - S
    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE FFK (KS, X, FR, FI, FM, FA, GR, GI, GM, GA)

!******************************************************************************
!
!! FFK computes modified Fresnel integrals F+/-(x) and K+/-(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    23 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KS, the sign code.
!    0, to calculate F+(x) and K+(x);
!    1, to calculate F_(x) and K_(x).
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) FR, FI, FM, FA, the values of
!    Re[F+/-(x)], Im[F+/-(x)], |F+/-(x)|, Arg[F+/-(x)]  (Degs.).
!
!    Output, real ( kind = rk ) GR, GI, GM, GA, the values of
!    Re[K+/-(x)], Im[K+/-(x)], |K+/-(x)|, Arg[K+/-(x)]  (Degs.).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C1
    REAL (KIND=RK) CS
    REAL (KIND=RK) EPS
    REAL (KIND=RK) FA
    REAL (KIND=RK) FI
    REAL (KIND=RK) FI0
    REAL (KIND=RK) FM
    REAL (KIND=RK) FR
    REAL (KIND=RK) GA
    REAL (KIND=RK) GI
    REAL (KIND=RK) GM
    REAL (KIND=RK) GR
    INTEGER K
    INTEGER KS
    INTEGER M
    REAL (KIND=RK) P2P
    REAL (KIND=RK) PI
    REAL (KIND=RK) PP2
    REAL (KIND=RK) S1
    REAL (KIND=RK) SRD
    REAL (KIND=RK) SS
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) X4
    REAL (KIND=RK) XA
    REAL (KIND=RK) XC
    REAL (KIND=RK) XF
    REAL (KIND=RK) XF0
    REAL (KIND=RK) XF1
    REAL (KIND=RK) XG
    REAL (KIND=RK) XP
    REAL (KIND=RK) XQ
    REAL (KIND=RK) XQ2
    REAL (KIND=RK) XR
    REAL (KIND=RK) XS
    REAL (KIND=RK) XSU
    REAL (KIND=RK) XW

    SRD = 57.29577951308233D+00
    EPS = 1.0D-15
    PI = 3.141592653589793D+00
    PP2 = 1.2533141373155D+00
    P2P = 0.7978845608028654D+00
    XA = ABS (X)
    X2 = X * X
    X4 = X2 * X2

    IF (X == 0.0D+00) THEN

        FR = 0.5D+00 * SQRT (0.5D+00*PI)
        FI = (-1.0D+00) ** KS * FR
        FM = SQRT (0.25D+00*PI)
        FA = (-1.0D+00) ** KS * 45.0D+00
        GR = 0.5D+00
        GI = 0.0D+00
        GM = 0.5D+00
        GA = 0.0D+00

    ELSE

        IF (XA <= 2.5D+00) THEN

            XR = P2P * XA
            C1 = XR
            DO K = 1, 50
                XR = - 0.5D+00 * XR * (4.0D+00*K-3.0D+00) / K / (2.0D+00*K-1.0D+00) / &
               & (4.0D+00*K+1.0D+00) * X4
                C1 = C1 + XR
                IF (ABS(XR/C1) < EPS) THEN
                    EXIT
                END IF
            END DO

            S1 = P2P * XA * XA * XA / 3.0D+00
            XR = S1
            DO K = 1, 50
                XR = - 0.5D+00 * XR * (4.0D+00*K-1.0D+00) / K / (2.0D+00*K+1.0D+00) / &
               & (4.0D+00*K+3.0D+00) * X4
                S1 = S1 + XR
                IF (ABS(XR/S1) < EPS) THEN
                    EXIT
                END IF
            END DO

        ELSE IF (XA < 5.5D+00) THEN

            M = INT (42.0D+00+1.75D+00*X2)
            XSU = 0.0D+00
            XC = 0.0D+00
            XS = 0.0D+00
            XF1 = 0.0D+00
            XF0 = 1.0D-100
            DO K = M, 0, - 1
                XF = (2.0D+00*K+3.0D+00) * XF0 / X2 - XF1
                IF (K == 2*INT(K/2)) THEN
                    XC = XC + XF
                ELSE
                    XS = XS + XF
                END IF
                XSU = XSU + (2.0D+00*K+1.0D+00) * XF * XF
                XF1 = XF0
                XF0 = XF
            END DO
            XQ = SQRT (XSU)
            XW = P2P * XA / XQ
            C1 = XC * XW
            S1 = XS * XW

        ELSE

            XR = 1.0D+00
            XF = 1.0D+00
            DO K = 1, 12
                XR = - 0.25D+00 * XR * (4.0D+00*K-1.0D+00) * (4.0D+00*K-3.0D+00) / X4
                XF = XF + XR
            END DO
            XR = 1.0D+00 / (2.0D+00*XA*XA)
            XG = XR
            DO K = 1, 12
                XR = - 0.25D+00 * XR * (4.0D+00*K+1.0D+00) * (4.0D+00*K-1.0D+00) / X4
                XG = XG + XR
            END DO
            C1 = 0.5D+00 + (XF*SIN(X2)-XG*COS(X2)) / SQRT (2.0D+00*PI) / XA
            S1 = 0.5D+00 - (XF*COS(X2)+XG*SIN(X2)) / SQRT (2.0D+00*PI) / XA

        END IF

        FR = PP2 * (0.5D+00-C1)
        FI0 = PP2 * (0.5D+00-S1)
        FI = (-1.0D+00) ** KS * FI0
        FM = SQRT (FR*FR+FI*FI)

        IF (0.0D+00 <= FR) THEN
            FA = SRD * ATAN (FI/FR)
        ELSE IF (0.0D+00 < FI) THEN
            FA = SRD * (ATAN(FI/FR)+PI)
        ELSE IF (FI < 0.0D+00) THEN
            FA = SRD * (ATAN(FI/FR)-PI)
        END IF

        XP = X * X + PI / 4.0D+00
        CS = COS (XP)
        SS = SIN (XP)
        XQ2 = 1.0D+00 / SQRT (PI)
        GR = XQ2 * (FR*CS+FI0*SS)
        GI = (-1.0D+00) ** KS * XQ2 * (FI0*CS-FR*SS)
        GM = SQRT (GR*GR+GI*GI)

        IF (0.0D+00 <= GR) THEN
            GA = SRD * ATAN (GI/GR)
        ELSE IF (0.0D+00 < GI) THEN
            GA = SRD * (ATAN(GI/GR)+PI)
        ELSE IF (GI < 0.0D+00) THEN
            GA = SRD * (ATAN(GI/GR)-PI)
        END IF

        IF (X < 0.0D+00) THEN
            FR = PP2 - FR
            FI = (-1.0D+00) ** KS * PP2 - FI
            FM = SQRT (FR*FR+FI*FI)
            FA = SRD * ATAN (FI/FR)
            GR = COS (X*X) - GR
            GI = - (-1.0D+00) ** KS * SIN (X*X) - GI
            GM = SQRT (GR*GR+GI*GI)
            GA = SRD * ATAN (GI/GR)
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE FCSZO (KF, NT, ZO)

!******************************************************************************
!
!! FCSZO computes complex zeros of Fresnel integrals C(x) or S(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    17 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer KF, the function code.
!    1 for C(z);
!    2 for S(z)
!
!    Input, integer NT, the total number of zeros desired.
!
!    Output, complex ( kind = ck ) Z0(NT), the zeros.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER NT

    INTEGER I
    INTEGER IT
    INTEGER J
    INTEGER KF
    INTEGER NR
    REAL (KIND=RK) PI
    REAL (KIND=RK) PSQ
    REAL (KIND=RK) PX
    REAL (KIND=RK) PY
    REAL (KIND=RK) W
    REAL (KIND=RK) W0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZF
    COMPLEX (KIND=CK) ZFD
    COMPLEX (KIND=CK) ZGD
    COMPLEX (KIND=CK) ZO (NT)
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZQ
    COMPLEX (KIND=CK) ZW

    PI = 3.141592653589793D+00

    DO NR = 1, NT

        IF (KF == 1) THEN
            PSQ = SQRT (4.0D+00*NR-1.0D+00)
        ELSE
            PSQ = 2.0D+00 * SQRT (REAL(NR, KIND=RK))
        END IF

        PX = PSQ - LOG (PI*PSQ) / (PI*PI*PSQ**3.0D+00)
        PY = LOG (PI*PSQ) / (PI*PSQ)
        Z = CMPLX (PX, PY, KIND=CK)

        IF (KF == 2) THEN
            IF (NR == 2) THEN
                Z = CMPLX (2.8334D+00, 0.2443D+00, KIND=CK)
            ELSE IF (NR == 3) THEN
                Z = CMPLX (3.4674D+00, 0.2185D+00, KIND=CK)
            ELSE IF (NR == 4) THEN
                Z = CMPLX (4.0025D+00, 0.2008D+00, KIND=CK)
            END IF
        END IF

        IT = 0

        DO

            IT = IT + 1

            IF (KF == 1) THEN
                CALL CFC (Z, ZF, ZD)
            ELSE
                CALL CFS (Z, ZF, ZD)
            END IF

            ZP = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
            DO I = 1, NR - 1
                ZP = ZP * (Z-ZO(I))
            END DO
            ZFD = ZF / ZP
            ZQ = CMPLX (0.0D+00, 0.0D+00, KIND=CK)

            DO I = 1, NR - 1
                ZW = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
                DO J = 1, NR - 1
                    IF (J /= I) THEN
                        ZW = ZW * (Z-ZO(J))
                    END IF
                END DO
                ZQ = ZQ + ZW
            END DO

            ZGD = (ZD-ZQ*ZFD) / ZP
            Z = Z - ZFD / ZGD
            W0 = W
            W = ABS (Z)

            IF (ABS((W-W0)/W) <= 1.0D-12) THEN
                EXIT
            END IF

            IF (50 < IT) THEN
                EXIT
            END IF

        END DO

        ZO (NR) = Z

    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE CERF (Z, CER, CDER)

!******************************************************************************
!
!! cerf() computes the error function and derivative for a complex argument.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    25 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ), the argument.
!
!    Output, complex ( kind = ck ) CER, CDER, the values of erf(z) and erf'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C0
    COMPLEX (KIND=CK) CDER
    COMPLEX (KIND=CK) CER
    REAL (KIND=RK) CS
    REAL (KIND=RK) EI1
    REAL (KIND=RK) EI2
    REAL (KIND=RK) EPS
    REAL (KIND=RK) ER
    REAL (KIND=RK) ER0
    REAL (KIND=RK) ER1
    REAL (KIND=RK) ER2
    REAL (KIND=RK) ERI
    REAL (KIND=RK) ERR
    INTEGER K
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) R
    REAL (KIND=RK) SS
    REAL (KIND=RK) W
    REAL (KIND=RK) W1
    REAL (KIND=RK) W2
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) Y
    COMPLEX (KIND=CK) Z

    EPS = 1.0D-12
    PI = 3.141592653589793D+00
    X = REAL (Z, KIND=RK)
    Y = AIMAG (Z)
    X2 = X * X

    IF (X <= 3.5D+00) THEN

        ER = 1.0D+00
        R = 1.0D+00
        DO K = 1, 100
            R = R * X2 / (K+0.5D+00)
            ER = ER + R
            IF (ABS(ER-W) <= EPS*ABS(ER)) THEN
                EXIT
            END IF
            W = ER
        END DO

        C0 = 2.0D+00 / SQRT (PI) * X * EXP (-X2)
        ER0 = C0 * ER

    ELSE

        ER = 1.0D+00
        R = 1.0D+00
        DO K = 1, 12
            R = - R * (K-0.5D+00) / X2
            ER = ER + R
        END DO
        C0 = EXP (-X2) / (X*SQRT(PI))
        ER0 = 1.0D+00 - C0 * ER

    END IF

    IF (Y == 0.0D+00) THEN

        ERR = ER0
        ERI = 0.0D+00

    ELSE

        CS = COS (2.0D+00*X*Y)
        SS = SIN (2.0D+00*X*Y)
        ER1 = EXP (-X2) * (1.0D+00-CS) / (2.0D+00*PI*X)
        EI1 = EXP (-X2) * SS / (2.0D+00*PI*X)
        ER2 = 0.0D+00
        DO N = 1, 100
            ER2 = ER2 + EXP (-0.25D+00*N*N) / (N*N+4.0D+00*X2) * &
           & (2.0D+00*X-2.0D+00*X*COSH(N*Y)*CS+N*SINH(N*Y)*SS)
            IF (ABS((ER2-W1)/ER2) < EPS) THEN
                EXIT
            END IF
            W1 = ER2
        END DO

        C0 = 2.0D+00 * EXP (-X2) / PI
        ERR = ER0 + ER1 + C0 * ER2
        EI2 = 0.0D+00
        DO N = 1, 100
            EI2 = EI2 + EXP (-0.25D+00*N*N) / (N*N+4.0D+00*X2) * &
           & (2.0D+00*X*COSH(N*Y)*SS+N*SINH(N*Y)*CS)
            IF (ABS((EI2-W2)/EI2) < EPS) THEN
                EXIT
            END IF
            W2 = EI2
        END DO

        ERI = EI1 + C0 * EI2

    END IF

    CER = CMPLX (ERR, ERI, KIND=CK)
    CDER = 2.0D+00 / SQRT (PI) * EXP (-Z*Z)

    RETURN
END

!******************************************************************************

SUBROUTINE CFC (Z, ZF, ZD)

!******************************************************************************
!
!! cfc() computes the complex Fresnel integral C(z) and C'(z).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    26 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) ZF, ZD, the values of C(z) and C'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    COMPLEX (KIND=CK) C
    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CG
    COMPLEX (KIND=CK) CR
    REAL (KIND=RK) EPS
    INTEGER K
    INTEGER M
    REAL (KIND=RK) PI
    REAL (KIND=RK) W0
    REAL (KIND=RK) WA
    REAL (KIND=RK) WA0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z0
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZF
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZP2

    EPS = 1.0D-14
    PI = 3.141592653589793D+00
    W0 = ABS (Z)
    ZP = 0.5D+00 * PI * Z * Z
    ZP2 = ZP * ZP
    Z0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)

    IF (Z == Z0) THEN

        C = Z0

    ELSE IF (W0 <= 2.5D+00) THEN

        CR = Z
        C = CR
        DO K = 1, 80
            CR = - 0.5D+00 * CR * (4.0D+00*K-3.0D+00) / K / (2.0D+00*K-1.0D+00) / &
           & (4.0D+00*K+1.0D+00) * ZP2
            C = C + CR
            WA = ABS (C)
            IF (ABS((WA-WA0)/WA) < EPS .AND. 10 < K) THEN
                EXIT
            END IF
            WA0 = WA
        END DO

    ELSE IF (2.5D+00 < W0 .AND. W0 < 4.5D+00) THEN

        M = 85
        C = Z0
        CF1 = Z0
        CF0 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
        DO K = M, 0, - 1
            CF = (2.0D+00*K+3.0D+00) * CF0 / ZP - CF1
            IF (K == INT(K/2)*2) THEN
                C = C + CF
            END IF
            CF1 = CF0
            CF0 = CF
        END DO
        C = SQRT (2.0D+00/(PI*ZP)) * SIN (ZP) / CF * C

    ELSE

        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 20
            CR = - 0.25D+00 * CR * (4.0D+00*K-1.0D+00) * (4.0D+00*K-3.0D+00) / ZP2
            CF = CF + CR
        END DO
        CR = 1.0D+00 / (PI*Z*Z)
        CG = CR
        DO K = 1, 12
            CR = - 0.25D+00 * CR * (4.0D+00*K+1.0D+00) * (4.0D+00*K-1.0D+00) / ZP2
            CG = CG + CR
        END DO
        C = 0.5D+00 + (CF*SIN(ZP)-CG*COS(ZP)) / (PI*Z)

    END IF

    ZF = C
    ZD = COS (0.5D+00*PI*Z*Z)

    RETURN
END

!******************************************************************************

SUBROUTINE CFS (Z, ZF, ZD)

!******************************************************************************
!
!! cfs() computes the complex Fresnel integral S(z) and S'(z).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    24 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) ZF, ZD, the values of S(z) and S'(z).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    COMPLEX (KIND=CK) CF
    COMPLEX (KIND=CK) CF0
    COMPLEX (KIND=CK) CF1
    COMPLEX (KIND=CK) CG
    COMPLEX (KIND=CK) CR
    REAL (KIND=RK) EPS
    INTEGER K
    INTEGER M
    REAL (KIND=RK) PI
    COMPLEX (KIND=CK) S
    REAL (KIND=RK) W0
    REAL (KIND=RK) WB
    REAL (KIND=RK) WB0
    COMPLEX (KIND=CK) Z
    COMPLEX (KIND=CK) Z0
    COMPLEX (KIND=CK) ZD
    COMPLEX (KIND=CK) ZF
    COMPLEX (KIND=CK) ZP
    COMPLEX (KIND=CK) ZP2

    EPS = 1.0D-14
    PI = 3.141592653589793D+00
    W0 = ABS (Z)
    ZP = 0.5D+00 * PI * Z * Z
    ZP2 = ZP * ZP
    Z0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)

    IF (Z == Z0) THEN

        S = Z0

    ELSE IF (W0 <= 2.5D+00) THEN

        S = Z * ZP / 3.0D+00
        CR = S
        DO K = 1, 80
            CR = - 0.5D+00 * CR * (4.0D+00*K-1.0D+00) / K / (2.0D+00*K+1.0D+00) / &
           & (4.0D+00*K+3.0D+00) * ZP2
            S = S + CR
            WB = ABS (S)
            IF (ABS(WB-WB0) < EPS .AND. 10 < K) THEN
                EXIT
            END IF
            WB0 = WB
        END DO

    ELSE IF (2.5D+00 < W0 .AND. W0 < 4.5D+00) THEN

        M = 85
        S = Z0
        CF1 = Z0
        CF0 = CMPLX (1.0D-30, 0.0D+00, KIND=CK)
        DO K = M, 0, - 1
            CF = (2.0D+00*K+3.0D+00) * CF0 / ZP - CF1
            IF (K /= INT(K/2)*2) THEN
                S = S + CF
            END IF
            CF1 = CF0
            CF0 = CF
        END DO
        S = SQRT (2.0D+00/(PI*ZP)) * SIN (ZP) / CF * S

    ELSE

        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CF = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 20
            CR = - 0.25D+00 * CR * (4.0D+00*K-1.0D+00) * (4.0D+00*K-3.0D+00) / ZP2
            CF = CF + CR
        END DO
        CR = 1.0D+00 / (PI*Z*Z)
        CG = CR
        DO K = 1, 12
            CR = - 0.25D+00 * CR * (4.0D+00*K+1.0D+00) * (4.0D+00*K-1.0D+00) / ZP2
            CG = CG + CR
        END DO
        S = 0.5D+00 - (CF*COS(ZP)+CG*SIN(ZP)) / (PI*Z)

    END IF

    ZF = S
    ZD = SIN (0.5D+00*PI*Z*Z)

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   17: COSINE AND SINE INTEGRALS
!
!------------------------------------------------------------------------------

SUBROUTINE CISIA (X, CI, SI)

!******************************************************************************
!
!! CISIA computes cosine Ci(x) and sine integrals Si(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    03 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of Ci(x) and Si(x).
!
!    Output, real ( kind = rk ) CI, SI, the values of Ci(x) and Si(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) BJ (101)
    REAL (KIND=RK) CI
    REAL (KIND=RK) EL
    REAL (KIND=RK) EPS
    INTEGER K
    INTEGER M
    REAL (KIND=RK) P2
    REAL (KIND=RK) SI
    REAL (KIND=RK) X
    REAL (KIND=RK) X2
    REAL (KIND=RK) XA
    REAL (KIND=RK) XA0
    REAL (KIND=RK) XA1
    REAL (KIND=RK) XCS
    REAL (KIND=RK) XF
    REAL (KIND=RK) XG
    REAL (KIND=RK) XG1
    REAL (KIND=RK) XG2
    REAL (KIND=RK) XR
    REAL (KIND=RK) XS
    REAL (KIND=RK) XSS

    P2 = 1.570796326794897D+00
    EL = 0.5772156649015329D+00
    EPS = 1.0D-15
    X2 = X * X

    IF (X == 0.0D+00) THEN

        CI = - 1.0D+300
        SI = 0.0D+00

    ELSE IF (X <= 16.0D+00) THEN

        XR = - 0.25D+00 * X2
        CI = EL + LOG (X) + XR
        DO K = 2, 40
            XR = - 0.5D+00 * XR * (K-1) / (K*K*(2*K-1)) * X2
            CI = CI + XR
            IF (ABS(XR) < ABS(CI)*EPS) THEN
                EXIT
            END IF
        END DO

        XR = X
        SI = X
        DO K = 1, 40
            XR = - 0.5D+00 * XR * (2*K-1) / K / (4*K*K+4*K+1) * X2
            SI = SI + XR
            IF (ABS(XR) < ABS(SI)*EPS) THEN
                RETURN
            END IF
        END DO

    ELSE IF (X <= 32.0D+00) THEN

        M = INT (47.2D+00+0.82D+00*X)
        XA1 = 0.0D+00
        XA0 = 1.0D-100
        DO K = M, 1, - 1
            XA = 4.0D+00 * K * XA0 / X - XA1
            BJ (K) = XA
            XA1 = XA0
            XA0 = XA
        END DO
        XS = BJ (1)
        DO K = 3, M, 2
            XS = XS + 2.0D+00 * BJ (K)
        END DO
        BJ (1) = BJ (1) / XS
        DO K = 2, M
            BJ (K) = BJ (K) / XS
        END DO
        XR = 1.0D+00
        XG1 = BJ (1)
        DO K = 2, M
            XR = 0.25D+00 * XR * (2.0D+00*K-3.0D+00) ** 2 / &
           & ((K-1.0D+00)*(2.0D+00*K-1.0D+00)**2) * X
            XG1 = XG1 + BJ (K) * XR
        END DO

        XR = 1.0D+00
        XG2 = BJ (1)
        DO K = 2, M
            XR = 0.25D+00 * XR * (2.0D+00*K-5.0D+00) ** 2 / &
           & ((K-1.0D+00)*(2.0D+00*K-3.0D+00)**2) * X
            XG2 = XG2 + BJ (K) * XR
        END DO

        XCS = COS (X/2.0D+00)
        XSS = SIN (X/2.0D+00)
        CI = EL + LOG (X) - X * XSS * XG1 + 2.0 * XCS * XG2 - 2.0 * XCS * XCS
        SI = X * XCS * XG1 + 2.0 * XSS * XG2 - SIN (X)

    ELSE

        XR = 1.0D+00
        XF = 1.0D+00
        DO K = 1, 9
            XR = - 2.0D+00 * XR * K * (2*K-1) / X2
            XF = XF + XR
        END DO
        XR = 1.0D+00 / X
        XG = XR
        DO K = 1, 8
            XR = - 2.0D+00 * XR * (2*K+1) * K / X2
            XG = XG + XR
        END DO
        CI = XF * SIN (X) / X - XG * COS (X) / X
        SI = P2 - XF * COS (X) / X - XG * SIN (X) / X

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE CISIB (X, CI, SI)

!******************************************************************************
!
!! CISIB computes cosine and sine integrals.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    20 March 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument of Ci(x) and Si(x).
!
!    Output, real ( kind = rk ) CI, SI, the values of Ci(x) and Si(x).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) CI
    REAL (KIND=RK) FX
    REAL (KIND=RK) GX
    REAL (KIND=RK) SI
    REAL (KIND=RK) X
    REAL (KIND=RK) X2

    X2 = X * X

    IF (X == 0.0D+00) THEN

        CI = - 1.0D+300
        SI = 0.0D+00

    ELSE IF (X <= 1.0D+00) THEN

        CI = ((((-3.0D-08*X2+3.10D-06)*X2-2.3148D-04)*X2+1.041667D-02)*X2-0.25D+00) * X2 + &
       & 0.577215665D+00 + LOG (X)

        SI = ((((3.1D-07*X2-2.834D-05)*X2+1.66667D-03)*X2-5.555556D-02)*X2+1.0D+00) * X

    ELSE

        FX = ((((X2+38.027264D+00)*X2+265.187033D+00)*X2+335.67732D+00)*X2+38.102495D+00) / &
       & ((((X2+40.021433D+00)*X2+322.624911D+00)*X2+570.23628D+00)*X2+157.105423D+00)

        GX = ((((X2+42.242855D+00)*X2+302.757865D+00)*X2+352.018498D+00)*X2+21.821899D+00) / &
       & ((((X2+48.196927D+00)*X2+482.485984D+00)*X2+1114.978885D+00)*X2+449.690326D+00) / X

        CI = FX * SIN (X) / X - GX * COS (X) / X

        SI = 1.570796327D+00 - FX * COS (X) / X - GX * SIN (X) / X

    END IF

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   18: ELLIPTIC INTEGRALS AND JACOBIAN ELLIPTIC FUNCTIONS
!
!------------------------------------------------------------------------------

SUBROUTINE COMELP (HK, CK, CE)

!******************************************************************************
!
!! COMELP computes complete elliptic integrals K(k) and E(k).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) HK, the modulus.  0 <= HK <= 1.
!
!    Output, real ( kind = rk ) CK, CE, the values of K(HK) and E(HK).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) AE
    REAL (KIND=RK) AK
    REAL (KIND=RK) BE
    REAL (KIND=RK) BK
    REAL (KIND=RK) CE
    REAL (KIND=RK) CK
    REAL (KIND=RK) HK
    REAL (KIND=RK) PK

    PK = 1.0D+00 - HK * HK

    IF (HK == 1.0D+00) THEN

        CK = 1.0D+300
        CE = 1.0D+00

    ELSE

        AK = &
       & (((0.01451196212D+00*PK+0.03742563713D+00)*PK+0.03590092383D+00)*PK+0.09666344259D+00) &
       & * PK + 1.38629436112D+00

        BK = &
       & (((0.00441787012D+00*PK+0.03328355346D+00)*PK+0.06880248576D+00)*PK+0.12498593597D+00) &
       & * PK + 0.5D+00

        CK = AK - BK * LOG (PK)

        AE = &
       & (((0.01736506451D+00*PK+0.04757383546D+00)*PK+0.0626060122D+00)*PK+0.44325141463D+00) &
       & * PK + 1.0D+00

        BE = &
       & (((0.00526449639D+00*PK+0.04069697526D+00)*PK+0.09200180037D+00)*PK+0.2499836831D+00) &
       & * PK

        CE = AE - BE * LOG (PK)

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ELIT (HK, PHI, FE, EE)

!******************************************************************************
!
!! ELIT: complete and incomplete elliptic integrals F(k,phi) and E(k,phi).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    12 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) HK, the modulus, between 0 and 1.
!
!    Input, real ( kind = rk ) PHI, the argument in degrees.
!
!    Output, real ( kind = rk ) FE, EE, the values of F(k,phi) and E(k,phi).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) B
    REAL (KIND=RK) B0
    REAL (KIND=RK) C
    REAL (KIND=RK) CE
    REAL (KIND=RK) CK
    REAL (KIND=RK) D
    REAL (KIND=RK) D0
    REAL (KIND=RK) EE
    REAL (KIND=RK) FAC
    REAL (KIND=RK) FE
    REAL (KIND=RK) G
    REAL (KIND=RK) HK
    INTEGER N
    REAL (KIND=RK) PHI
    REAL (KIND=RK) PI
    REAL (KIND=RK) R

    G = 0.0D+00
    PI = 3.14159265358979D+00
    A0 = 1.0D+00
    B0 = SQRT (1.0D+00-HK*HK)
    D0 = (PI/180.0D+00) * PHI
    R = HK * HK

    IF (HK == 1.0D+00 .AND. PHI == 90.0D+00) THEN

        FE = 1.0D+300
        EE = 1.0D+00

    ELSE IF (HK == 1.0D+00) THEN

        FE = LOG ((1.0D+00+SIN(D0))/COS(D0))
        EE = SIN (D0)

    ELSE

        FAC = 1.0D+00
        DO N = 1, 40
            A = (A0+B0) / 2.0D+00
            B = SQRT (A0*B0)
            C = (A0-B0) / 2.0D+00
            FAC = 2.0D+00 * FAC
            R = R + FAC * C * C
            IF (PHI /= 90.0D+00) THEN
                D = D0 + ATAN ((B0/A0)*TAN(D0))
                G = G + C * SIN (D)
                D0 = D + PI * INT (D/PI+0.5D+00)
            END IF
            A0 = A
            B0 = B
            IF (C < 1.0D-07) THEN
                EXIT
            END IF
        END DO

        CK = PI / (2.0D+00*A)
        CE = PI * (2.0D+00-R) / (4.0D+00*A)
        IF (PHI == 90.0D+00) THEN
            FE = CK
            EE = CE
        ELSE
            FE = D / (FAC*A)
            EE = FE * CE / CK + G
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ELIT3 (PHI, HK, C, EL3)

!******************************************************************************
!
!! ELIT3 computes the elliptic integral of the third kind.
!
!  Discussion:
!
!    Gauss-Legendre quadrature is used.
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    14 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) PHI, the argument in degrees.
!
!    Input, real ( kind = rk ) HK, the modulus, between 0 and 1.
!
!    Input, real ( kind = rk ) C, the parameter, between 0 and 1.
!
!    Output, real ( kind = rk ) EL3, the value of the elliptic integral
!    of the third kind.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) C
    REAL (KIND=RK) C0
    REAL (KIND=RK) C1
    REAL (KIND=RK) C2
    REAL (KIND=RK) EL3
    REAL (KIND=RK) F1
    REAL (KIND=RK) F2
    REAL (KIND=RK) HK
    INTEGER I
    LOGICAL LB1
    LOGICAL LB2
    REAL (KIND=RK) PHI
    REAL (KIND=RK), DIMENSION (10), SAVE :: T = (/ 0.9931285991850949D+00, &
   & 0.9639719272779138D+00, 0.9122344282513259D+00, 0.8391169718222188D+00, &
   & 0.7463319064601508D+00, 0.6360536807265150D+00, 0.5108670019508271D+00, &
   & 0.3737060887154195D+00, 0.2277858511416451D+00, 0.7652652113349734D-01 /)
    REAL (KIND=RK) T1
    REAL (KIND=RK) T2
    REAL (KIND=RK), DIMENSION (10), SAVE :: W = (/ 0.1761400713915212D-01, &
   & 0.4060142980038694D-01, 0.6267204833410907D-01, 0.8327674157670475D-01, &
   & 0.1019301198172404D+00, 0.1181945319615184D+00, 0.1316886384491766D+00, &
   & 0.1420961093183820D+00, 0.1491729864726037D+00, 0.1527533871307258D+00 /)

    LB1 = (HK == 1.0D+00) .AND. (ABS(PHI-90.0D+00) <= 1.0D-08)

    LB2 = C == 1.0D+00 .AND. ABS (PHI-90.0D+00) <= 1.0D-08

    IF (LB1 .OR. LB2) THEN
        EL3 = 1.0D+300
        RETURN
    END IF

    C1 = 0.87266462599716D-02 * PHI
    C2 = C1

    EL3 = 0.0D+00
    DO I = 1, 10
        C0 = C2 * T (I)
        T1 = C1 + C0
        T2 = C1 - C0
        F1 = 1.0D+00 / ((1.0D+00-C*SIN(T1)*SIN(T1))*SQRT(1.0D+00-HK*HK*SIN(T1)*SIN(T1)))
        F2 = 1.0D+00 / ((1.0D+00-C*SIN(T2)*SIN(T2))*SQRT(1.0D+00-HK*HK*SIN(T2)*SIN(T2)))
        EL3 = EL3 + W (I) * (F1+F2)
    END DO

    EL3 = C1 * EL3

    RETURN
END

!******************************************************************************

SUBROUTINE JELP (U, HK, ESN, ECN, EDN, EPH)

!******************************************************************************
!
!! JELP computes Jacobian elliptic functions SN(u), CN(u), DN(u).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    08 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) U, the argument.
!
!    Input, real ( kind = rk ) HK, the modulus, between 0 and 1.
!
!    Output, real ( kind = rk ) ESN, ECN, EDN, EPH, the values of
!    sn(u), cn(u), dn(u), and phi (in degrees).
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A
    REAL (KIND=RK) A0
    REAL (KIND=RK) B
    REAL (KIND=RK) B0
    REAL (KIND=RK) C
    REAL (KIND=RK) D
    REAL (KIND=RK) DN
    REAL (KIND=RK) ECN
    REAL (KIND=RK) EDN
    REAL (KIND=RK) EPH
    REAL (KIND=RK) ESN
    REAL (KIND=RK) HK
    INTEGER J
    INTEGER N
    REAL (KIND=RK) PI
    REAL (KIND=RK) R (40)
    REAL (KIND=RK) SA
    REAL (KIND=RK) T
    REAL (KIND=RK) U

    PI = 3.14159265358979D+00
    A0 = 1.0D+00
    B0 = SQRT (1.0D+00-HK*HK)

    DO N = 1, 40

        A = (A0+B0) / 2.0D+00
        B = SQRT (A0*B0)
        C = (A0-B0) / 2.0D+00
        R (N) = C / A

        IF (C < 1.0D-07) THEN
            EXIT
        END IF

        A0 = A
        B0 = B

    END DO

    DN = 2.0D+00 ** N * A * U

    DO J = N, 1, - 1
        T = R (J) * SIN (DN)
        SA = ATAN (T/SQRT(ABS(1.0D+00-T*T)))
        D = 0.5D+00 * (DN+SA)
        DN = D
    END DO

    EPH = D * 180.0D+00 / PI
    ESN = SIN (D)
    ECN = COS (D)
    EDN = SQRT (1.0D+00-HK*HK*ESN*ESN)

    RETURN
END

!******************************************************************************
 
!------------------------------------------------------------------------------
!
!   19: EXPONENTIAL INTEGRALS
!
!------------------------------------------------------------------------------

SUBROUTINE E1XA (X, E1)

!******************************************************************************
!
!! E1XA computes the exponential integral E1(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    06 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) E1, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) E1
    REAL (KIND=RK) ES1
    REAL (KIND=RK) ES2
    REAL (KIND=RK) X

    IF (X == 0.0D+00) THEN

        E1 = 1.0D+300

    ELSE IF (X <= 1.0D+00) THEN

        E1 = - LOG (X) + &
       & ((((1.07857D-03*X-9.76004D-03)*X+5.519968D-02)*X-0.24991055D+00)*X+0.99999193D+00) * X &
       & - 0.57721566D+00

    ELSE

        ES1 = (((X+8.5733287401D+00)*X+18.059016973D+00)*X+8.6347608925D+00) * X + &
       & 0.2677737343D+00

        ES2 = (((X+9.5733223454D+00)*X+25.6329561486D+00)*X+21.0996530827D+00) * X + &
       & 3.9584969228D+00

        E1 = EXP (-X) / X * ES1 / ES2

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE E1XB (X, E1)

!******************************************************************************
!
!! E1XB computes the exponential integral E1(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    06 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) E1, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) E1
    REAL (KIND=RK) GA
    INTEGER K
    INTEGER M
    REAL (KIND=RK) R
    REAL (KIND=RK) T
    REAL (KIND=RK) T0
    REAL (KIND=RK) X

    IF (X == 0.0D+00) THEN

        E1 = 1.0D+300

    ELSE IF (X <= 1.0D+00) THEN

        E1 = 1.0D+00
        R = 1.0D+00

        DO K = 1, 25
            R = - R * K * X / (K+1.0D+00) ** 2
            E1 = E1 + R
            IF (ABS(R) <= ABS(E1)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        GA = 0.5772156649015328D+00
        E1 = - GA - LOG (X) + X * E1

    ELSE

        M = 20 + INT (80.0D+00/X)
        T0 = 0.0D+00
        DO K = M, 1, - 1
            T0 = K / (1.0D+00+K/(X+T0))
        END DO
        T = 1.0D+00 / (X+T0)
        E1 = EXP (-X) * T

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE E1Z (Z, CE1)

!******************************************************************************
!
!! E1Z computes the complex exponential integral E1(z).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    16 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, complex ( kind = ck ) Z, the argument.
!
!    Output, complex ( kind = ck ) CE1, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: CK = KIND ((1.0D+00, 1.0D+00))
    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) A0
    COMPLEX (KIND=CK) CE1
    COMPLEX (KIND=CK) CR
    COMPLEX (KIND=CK) CT
    COMPLEX (KIND=CK) CT0
    REAL (KIND=RK) EL
    INTEGER K
    REAL (KIND=RK) PI
    REAL (KIND=RK) X
    COMPLEX (KIND=CK) Z

    PI = 3.141592653589793D+00
    EL = 0.5772156649015328D+00
    X = REAL (Z, KIND=RK)
    A0 = ABS (Z)

    IF (A0 == 0.0D+00) THEN
        CE1 = CMPLX (1.0D+300, 0.0D+00, KIND=CK)
    ELSE IF (A0 <= 10.0D+00 .OR. (X < 0.0D+00 .AND. A0 < 20.0D+00)) THEN
        CE1 = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        CR = CMPLX (1.0D+00, 0.0D+00, KIND=CK)
        DO K = 1, 150
            CR = - CR * K * Z / (K+1.0D+00) ** 2
            CE1 = CE1 + CR
            IF (ABS(CR) <= ABS(CE1)*1.0D-15) THEN
                EXIT
            END IF
        END DO

        CE1 = - EL - LOG (Z) + Z * CE1

    ELSE

        CT0 = CMPLX (0.0D+00, 0.0D+00, KIND=CK)
        DO K = 120, 1, - 1
            CT0 = K / (1.0D+00+K/(Z+CT0))
        END DO
        CT = 1.0D+00 / (Z+CT0)

        CE1 = EXP (-Z) * CT
        IF (X <= 0.0D+00 .AND. AIMAG(Z) == 0.0D+00) THEN
            CE1 = CE1 - PI * CMPLX (0.0D+00, 1.0D+00, KIND=CK)
        END IF

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE EIX (X, EI)

!******************************************************************************
!
!! EIX computes the exponential integral Ei(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    10 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) EI, the function value.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    REAL (KIND=RK) EI
    REAL (KIND=RK) GA
    INTEGER K
    REAL (KIND=RK) R
    REAL (KIND=RK) X

    IF (X == 0.0D+00) THEN

        EI = - 1.0D+300

    ELSE IF (X <= 40.0D+00) THEN

        EI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 100
            R = R * K * X / (K+1.0D+00) ** 2
            EI = EI + R
            IF (ABS(R/EI) <= 1.0D-15) THEN
                EXIT
            END IF
        END DO

        GA = 0.5772156649015328D+00
        EI = GA + LOG (X) + X * EI

    ELSE

        EI = 1.0D+00
        R = 1.0D+00
        DO K = 1, 20
            R = R * K / X
            EI = EI + R
        END DO
        EI = EXP (X) / X * EI

    END IF

    RETURN
END

!******************************************************************************

SUBROUTINE ENXA (N, X, EN)

!******************************************************************************
!
!! ENXA computes the exponential integral En(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    07 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) EN(0:N), the function values.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) E1
    REAL (KIND=RK) EK
    REAL (KIND=RK) EN (0:N)
    INTEGER K
    REAL (KIND=RK) X

    EN (0) = EXP (-X) / X
    CALL E1XB (X, E1)

    EN (1) = E1
    DO K = 2, N
        EK = (EXP(-X)-X*E1) / (K-1.0D+00)
        EN (K) = EK
        E1 = EK
    END DO

    RETURN
END

!******************************************************************************

SUBROUTINE ENXB (N, X, EN)

!******************************************************************************
!
!! ENXB computes the exponential integral En(x).
!
!  Licensing:
!
!    This routine is copyrighted by Shanjie Zhang and Jianming Jin.  However,
!    they give permission to incorporate this routine into a user program
!    provided that the copyright is acknowledged.
!
!  Modified:
!
!    10 July 2012
!
!  Author:
!
!    Shanjie Zhang, Jianming Jin
!
!  Reference:
!
!    Shanjie Zhang, Jianming Jin,
!    Computation of Special Functions,
!    Wiley, 1996,
!    ISBN: 0-471-11963-6,
!    LC: QA351.C45.
!
!  Parameters:
!
!    Input, integer N, the order.
!
!    Input, real ( kind = rk ) X, the argument.
!
!    Output, real ( kind = rk ) EN(0:N), the function values.
!
    IMPLICIT NONE

    INTEGER, PARAMETER :: RK = KIND (1.0D+00)

    INTEGER N

    REAL (KIND=RK) EN (0:N)
    REAL (KIND=RK) ENS
    INTEGER J
    INTEGER K
    INTEGER L
    INTEGER M
    REAL (KIND=RK) PS
    REAL (KIND=RK) R
    REAL (KIND=RK) RP
    REAL (KIND=RK) S
    REAL (KIND=RK) S0
    REAL (KIND=RK) T
    REAL (KIND=RK) T0
    REAL (KIND=RK) X

    IF (X == 0.0D+00) THEN

        EN (0) = 1.0D+300
        EN (1) = 1.0D+300
        DO K = 2, N
            EN (K) = 1.0D+00 / (K-1.0D+00)
        END DO
        RETURN

    ELSE IF (X <= 1.0D+00) THEN

        EN (0) = EXP (-X) / X
        DO L = 1, N
            RP = 1.0D+00
            DO J = 1, L - 1
                RP = - RP * X / J
            END DO
            PS = - 0.5772156649015328D+00
            DO M = 1, L - 1
                PS = PS + 1.0D+00 / M
            END DO
            ENS = RP * (-LOG(X)+PS)
            S = 0.0D+00
            DO M = 0, 20
                IF (M /= L-1) THEN
                    R = 1.0D+00
                    DO J = 1, M
                        R = - R * X / J
                    END DO
                    S = S + R / (M-L+1.0D+00)
                    IF (ABS(S-S0) < ABS(S)*1.0D-15) THEN
                        EXIT
                    END IF
                    S0 = S
                END IF
            END DO

            EN (L) = ENS - S

        END DO

    ELSE

        EN (0) = EXP (-X) / X
        M = 15 + INT (100.0D+00/X)
        DO L = 1, N
            T0 = 0.0D+00
            DO K = M, 1, - 1
                T0 = (L+K-1.0D+00) / (1.0D+00+K/(X+T0))
            END DO
            T = 1.0D+00 / (X+T0)
            EN (L) = EXP (-X) * T
        END DO

    END IF

    RETURN
END

!******************************************************************************

END MODULE ModLib_SpecFunc

!******************************************************************************

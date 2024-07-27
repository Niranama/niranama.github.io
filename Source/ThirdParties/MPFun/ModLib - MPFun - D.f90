!*****************************************************************************

!  MPFUN20-Fort: A thread-safe arbitrary precision computation package
!  Transcendental function module (module MPFUND)

!  Revision date:  13 Jun 2021

!  AUTHOR:
!    David H. Bailey
!    Lawrence Berkeley National Lab (retired) and University of California, Davis
!    Email: dhbailey@lbl.gov

!  COPYRIGHT AND DISCLAIMER:
!    All software in this package (c) 2021 David H. Bailey.
!    By downloading or using this software you agree to the copyright, disclaimer
!    and license agreement in the accompanying file DISCLAIMER.txt.

!  PURPOSE OF PACKAGE:
!    This package permits one to perform floating-point computations (real and
!    complex) to arbitrarily high numeric precision, by making only relatively
!    minor changes to existing Fortran-90 programs.  All basic arithmetic
!    operations and transcendental functions are supported, together with several
!    special functions.

!    In addition to fast execution times, one key feature of this package is a
!    100% THREAD-SAFE design, which means that user-level applications can be
!    easily converted for parallel execution, say using a threaded parallel
!    environment such as OpenMP.

!  DOCUMENTATION:
!    A detailed description of this package, and instructions for compiling
!    and testing this program on various specific systems are included in the
!    README file accompanying this package, and, in more detail, in the
!    following technical paper:

!    David H. Bailey, "MPFUN2020: A new thread-safe arbitrary precision package,"
!    available at http://www.davidhbailey.com/dhbpapers/mpfun2020.pdf.

!  DESCRIPTION OF THIS MODULE (MPFUND):
!    This module contains subroutines for basic transcendental functions,
!    including routines for cos, sin, inverse cos/sin, hyperbolic cos/sin,
!    and inverse hyperbolic cos, sin, as well as routines to compute pi and log(2).

MODULE ModLib_MPFUND
USE ModLib_MPFUNA
USE ModLib_MPFUNB
USE ModLib_MPFUNC

CONTAINS

SUBROUTINE MPAGMR (A, B, C, MPNW)

!   This performs the arithmetic-geometric mean (AGM) iterations on A and B.
!   The AGM algorithm is as follows: Set a_0 = a and b_0 = b, then iterate

!    a_{k+1} = (a_k + b_k)/2
!    b_{k+1} = sqrt (a_k * b_k)

!   until convergence (i.e., until a_k = b_k to available precision).
!   The result is returned in C.

IMPLICIT NONE
INTEGER ITRMX, J, MPNW, MPNW1
PARAMETER (ITRMX = 50)
INTEGER (MPIKND) A(0:), B(0:), C(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. B(0) < ABS (A(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPAGMR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
MPNW1 = MPNW + 1
CALL MPEQ (A, S1, MPNW1)
CALL MPEQ (B, S2, MPNW1)

DO J = 1, ITRMX
  CALL MPADD (S1, S2, S0, MPNW1)
  CALL MPMULD (S0, 0.5D0, S3, MPNW1)
  CALL MPMUL (S1, S2, S0, MPNW1)
  CALL MPSQRT (S0, S2, MPNW1)
  CALL MPEQ (S3, S1, MPNW1)

!   Check for convergence.

  CALL MPSUB (S1, S2, S0, MPNW1)
  IF (S0(2) == 0 .OR. S0(3) < 1 - MPNW1) GOTO 100
ENDDO

WRITE (MPLDB, 2)
2 FORMAT ('*** MPAGMR: Iteration limit exceeded.')
CALL MPABRT (5)

100 CONTINUE

CALL MPROUN (S1, MPNW)
CALL MPEQ (S1, C, MPNW)

RETURN
END SUBROUTINE MPAGMR

SUBROUTINE MPANG (X, Y, A, MPNW)

!   This computes the MPR angle A subtended by the MPR pair (X, Y) considered as
!   a point in the x-y plane.  This is more useful than an arctan or arcsin
!   routine, since it places the result correctly in the full circle, i.e.
!   -Pi < A <= Pi.  Pi must be precomputed to at least MPNW words precision
!   and the stored in the array in module MPMODA.

!   The Taylor series for Arcsin converges much more slowly than that of Sin,
!   so this routine solves Cos (a) = x or Sin (a) = y using one of the
!   following Newton iterations, both of which converge to a:

!     z_{k+1} = z_k - [x - Cos (z_k)] / Sin (z_k)
!     z_{k+1} = z_k + [y - Sin (z_k)] / Cos (z_k)

!   The first is selected if Abs (x) <= Abs (y); otherwise the second is used.
!   These iterations are performed with a maximum precision level MPNW that
!   is dynamically changed, approximately doubling with each iteration.

IMPLICIT NONE
INTEGER IQ, IX, IY, K, KK, MPNW, MPNW1, MQ, NIT, NX, NY, N1, N2
REAL (MPRKND) CL2, T1, T2, T3
PARAMETER (CL2 = 1.4426950408889633D0, NIT = 3)
INTEGER (MPIKND) A(0:), X(0:), Y(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6), S5(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. X(0) < MPNW + 4 .OR. X(0) < ABS (X(2)) + 4 .OR. &
  Y(0) < MPNW + 4 .OR. Y(0) < ABS (Y(2)) + 4 .OR. A(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPANG: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IX = SIGN (INT (1, MPIKND), X(2))
NX = MIN (INT (ABS (X(2))), MPNW)
IY = SIGN (INT (1, MPIKND), Y(2))
NY = MIN (INT (ABS (Y(2))), MPNW)
MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
S5(0) = MPNW + 7

!   Check if both X and Y are zero.

IF (NX == 0 .AND. NY == 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPANG: Both arguments are zero.')
  CALL MPABRT (7)
ENDIF

!   Check if Pi has been precomputed.

IF (MPNW1 > MPPICON(1)) THEN
  WRITE (MPLDB, 3) MPNW1
3 FORMAT ('*** MPANG: Pi must be precomputed to precision',I9,' words.'/ &
  'See documentation for details.')
  CALL MPABRT (8)
ENDIF

!   Check if one of X or Y is zero.

IF (NX == 0) THEN
  CALL MPEQ (MPPICON, S0, MPNW1)
  IF (IY .GT. 0) THEN
    CALL MPMULD (S0, 0.5D0, A, MPNW)
  ELSE
    CALL MPMULD (S0, -0.5D0, A, MPNW)
  ENDIF
  GOTO 120
ELSEIF (NY == 0) THEN
  IF (IX .GT. 0) THEN
    A(1) = MPNW
    A(2) = 0
    A(3) = 0
    A(4) = 0
    A(5) = 0
  ELSE
    CALL MPEQ (S0, A, MPNW)
  ENDIF
  GOTO 120
ENDIF

!   Determine the least integer MQ such that 2 ^ MQ .GE. MPNW.

T1 = MPNW1
MQ = CL2 * LOG (T1) + 2.D0 - MPRDFZ

!   Normalize x and y so that x^2 + y^2 = 1.

CALL MPMUL (X, X, S0, MPNW1)
CALL MPMUL (Y, Y, S1, MPNW1)
CALL MPADD (S0, S1, S2, MPNW1)
CALL MPSQRT (S2, S3, MPNW1)
CALL MPDIV (X, S3, S1, MPNW1)
CALL MPDIV (Y, S3, S2, MPNW1)

!   Compute initial approximation of the angle.

CALL MPMDC (S1, T1, N1, MPNW1)
CALL MPMDC (S2, T2, N2, MPNW1)
N1 = MAX (N1, -MPNBT)
N2 = MAX (N2, -MPNBT)
T1 = T1 * 2.D0 ** N1
T2 = T2 * 2.D0 ** N2
T3 = ATAN2 (T2, T1)
CALL MPDMC (T3, 0, S5, MPNW1)

!   The smaller of x or y will be used from now on to measure convergence.
!   This selects the Newton iteration (of the two listed above) that has the
!   largest denominator.

IF (ABS (T1) .LE. ABS (T2)) THEN
  KK = 1
  CALL MPEQ (S1, S0, MPNW1)
ELSE
  KK = 2
  CALL MPEQ (S2, S0, MPNW1)
ENDIF

MPNW1 = 4
IQ = 0

!   Perform the Newton-Raphson iteration described above with a dynamically
!   changing precision level MPNW (one greater than powers of two).

DO K = 1, MQ
  MPNW1 = MIN (2 * MPNW1 - 2, MPNW)

100  CONTINUE

  CALL MPCSSNR (S5, S1, S2, MPNW1)

  IF (KK == 1) THEN
    CALL MPSUB (S0, S1, S3, MPNW1)
    CALL MPDIV (S3, S2, S4, MPNW1)
    CALL MPSUB (S5, S4, S1, MPNW1)
  ELSE
    CALL MPSUB (S0, S2, S3, MPNW1)
    CALL MPDIV (S3, S1, S4, MPNW1)
    CALL MPADD (S5, S4, S1, MPNW1)
  ENDIF
  CALL MPEQ (S1, S5, MPNW1)

  IF (K == MQ - NIT .AND. IQ == 0) THEN
    IQ = 1
    GOTO 100
  ENDIF
ENDDO

!   Restore original precision level.

CALL MPROUN (S5, MPNW)
CALL MPEQ (S5, A, MPNW)

120 CONTINUE

RETURN
END SUBROUTINE MPANG

SUBROUTINE MPCAGM (A, B, C, MPNW)

!   This performs the arithmetic-geometric mean (AGM) iterations on A and B
!   for MPC arguments A and B.
!   The AGM algorithm is as follows: Set a_0 = a and b_0 = b, then iterate

!    a_{k+1} = (a_k + b_k)/2
!    b_{k+1} = sqrt (a_k * b_k)

!   until convergence (i.e., until a_k = b_k to available precision).
!   The result is returned in C.

IMPLICIT NONE
INTEGER ITRMX, J, LA, LB, LC, MP7, MPNW, MPNW1
PARAMETER (ITRMX = 50)
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S0(0:2*MPNW+13), S1(0:2*MPNW+13), S2(0:2*MPNW+13), S3(0:2*MPNW+13)

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 .OR. &
  C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCAGM: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MP7 = MPNW + 7
S0(0) = MP7
S0(MP7) = MP7
S1(0) = MP7
S1(MP7) = MP7
S2(0) = MP7
S2(MP7) = MP7
S3(0) = MP7
S3(MP7) = MP7
MPNW1 = MPNW + 1
CALL MPCEQ (A, S1, MPNW1)
CALL MPCEQ (B, S2, MPNW1)

DO J = 1, ITRMX
  CALL MPCADD (S1, S2, S0, MPNW1)
  CALL MPMULD (S0, 0.5D0, S3, MPNW1)
  CALL MPMULD (S0(MP7:), 0.5D0, S3(MP7:), MPNW1)
  CALL MPCMUL (S1, S2, S0, MPNW1)
  CALL MPCSQRT (S0, S2, MPNW1)
  CALL MPCEQ (S3, S1, MPNW1)
  CALL MPCSUB (S1, S2, S0, MPNW1)

!   Check for convergence.

  IF ((S0(2) == 0 .OR. S0(3) < 1 - MPNW1) .AND. &
    (S0(MP7+2) == 0 .OR. S0(MP7+3) < 1 - MPNW1)) GOTO 100
ENDDO

WRITE (MPLDB, 2)
2 FORMAT ('*** MPCAGM: Iteration limit exceeded.')
CALL MPABRT (5)

100 CONTINUE

CALL MPROUN (S1, MPNW)
CALL MPROUN (S1(MP7:), MPNW)
CALL MPCEQ (S1, C, MPNW)

RETURN
END SUBROUTINE MPCAGM

SUBROUTINE MPCEXP (A, B, MPNW)

!   This computes Exp[A], for MPC A.

!   The formula is:  E^a1 * (Cos[a2] + I * Sin[a2]), where a1 and a2 are
!   the real and imaginary parts of A.

IMPLICIT NONE
INTEGER LA, LB, MPNW, MPNW1
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6)

! End of declaration

LA = A(0)
LB = B(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < MPNW + 6 .OR. B(LB) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCEXP: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7

CALL MPEXP (A, S0, MPNW1)
CALL MPCSSNR (A(LA:), S1, S2, MPNW1)
CALL MPMUL (S0, S1, S3, MPNW1)
CALL MPMUL (S0, S2, S4, MPNW1)

CALL MPROUN (S3, MPNW)
CALL MPROUN (S4, MPNW)
CALL MPEQ (S3, B, MPNW)
CALL MPEQ (S4, B(LB:), MPNW)

100 CONTINUE

RETURN
END SUBROUTINE MPCEXP

SUBROUTINE MPCLOG (A, B, MPNW)

!   This computes Log[A], for MPC A.

!   The formula is:  1/2 * Log[r] + I * Theta, where r = a1^2 + a2^2,
!   Theta is the angle corresponding to (a1, a2), and a1 and a2 are the
!   real and imaginary parts of A.

IMPLICIT NONE
INTEGER LA, LB, MPNW, MPNW1
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

LA = A(0)
LB = B(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < MPNW + 6 .OR. B(LB) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCLOG: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7

CALL MPMUL (A, A, S0, MPNW1)
CALL MPMUL (A(LA:), A(LA:), S1, MPNW1)
CALL MPADD (S0, S1, S2, MPNW1)
CALL MPLOG (S2, S3, MPNW1)
CALL MPMULD (S3, 0.5D0, S0, MPNW1)
CALL MPANG (A, A(LA:), S1, MPNW1)

CALL MPROUN (S0, MPNW)
CALL MPROUN (S1, MPNW)
CALL MPEQ (S0, B, MPNW)
CALL MPEQ (S1, B(LB:), MPNW)

100 CONTINUE

RETURN
END SUBROUTINE MPCLOG

SUBROUTINE MPCPOWCC (A, B, C, MPNW)

!   This computes A^B, where A and B are MPC.

IMPLICIT NONE
INTEGER LA, LB, LC, L3, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S1(0:2*MPNW+11), S2(0:2*MPNW+11)

! End of declaration

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 &
  .OR. C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCPOWCC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

L3 = MPNW + 6
S1(0) = L3
S1(L3) = L3
S2(0) = L3
S2(L3) = L3
CALL MPCLOG (A, S1, MPNW)
CALL MPCMUL (S1, B, S2, MPNW)
CALL MPCEXP (S2, C, MPNW)

RETURN
END SUBROUTINE MPCPOWCC

SUBROUTINE MPCPOWCR (A, B, C, MPNW)

!   This computes A^B, where A is MPC and B is MPR.

IMPLICIT NONE
INTEGER LA, LC, L3, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S1(0:2*MPNW+11), S2(0:2*MPNW+11)

! End of declaration

LA = A(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 &
  .OR. C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCPOWCR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

L3 = MPNW + 6
S1(0) = L3
S1(L3) = L3
S2(0) = L3
S2(L3) = L3
CALL MPCLOG (A, S1, MPNW)
CALL MPMUL (B, S1, S2, MPNW)
CALL MPMUL (B, S1(L3:), S2(L3:), MPNW)
CALL MPCEXP (S2, C, MPNW)

RETURN
END SUBROUTINE MPCPOWCR

SUBROUTINE MPCPOWRC (A, B, C, MPNW)

!   This computes A^B, where A is MPR and and B is MPC.

IMPLICIT NONE
INTEGER LB, LC, L3, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S1(0:2*MPNW+11), S2(0:2*MPNW+11)

! End of declaration

LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 &
  .OR. C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCPOWRC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

L3 = MPNW + 6
S1(0) = L3
S2(0) = L3
S2(L3) = L3
CALL MPLOG (A, S1, MPNW)
CALL MPMUL (S1, B, S2, MPNW)
CALL MPMUL (S1, B(LB:), S2(L3:), MPNW)
CALL MPCEXP (S2, C, MPNW)

RETURN
END SUBROUTINE MPCPOWRC

SUBROUTINE MPCSSHR (A, X, Y, MPNW)

!   This computes the hyperbolic cosine and sine of the MPR number A and
!   returns the two MPR results in X and Y, respectively.  If the argument
!   is very close to zero, a Taylor series is used; otherwise this routine
!   calls mpexp.

IMPLICIT NONE
INTEGER ITRMX, J, MPNW, MPNW1, MPNW2
PARAMETER (ITRMX = 1000000)
REAL (MPRKND) T2
INTEGER (MPIKND) A(0:), F(0:9), X(0:), Y(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. A(0) < ABS (A(2)) + 4 .OR. &
  X(0) < MPNW + 6 .OR. Y(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCSSHR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
MPNW1 = MPNW + 1
F(0) = 9
F(1) = MPNW
F(2) = 1
F(3) = 0
F(4) = 1
F(5) = 0
F(6) = 0

!   If argument is very small, compute the sinh using a Taylor series.
!   This avoids accuracy loss that otherwise occurs by using exp.

IF (A(3) < -1) THEN
  CALL MPEQ (A, S0, MPNW1)
  CALL MPMUL (S0, S0, S2, MPNW1)
  MPNW2 =  MPNW1

!   The working precision used to compute each term can be linearly reduced
!   as the computation proceeds.

  DO J = 1, ITRMX
    T2 = (2.D0 * J) * (2.D0 * J + 1.D0)
    CALL MPMUL (S2, S1, S3, MPNW2)
    CALL MPDIVD (S3, T2, S1, MPNW2)
    CALL MPADD (S1, S0, S3, MPNW1)
    CALL MPEQ (S3, S0, MPNW1)

!   Check for convergence of the series, and adjust working precision
!   for the next term.

    IF (S1(2) == 0 .OR. S1(3) < S0(3) - MPNW1) GOTO 110
    MPNW2 = MIN (MAX (MPNW1 + INT (S1(3) - S0(3)) + 1, 4), MPNW1)
  ENDDO

  WRITE (MPLDB, 4)
4 FORMAT ('*** MPCSSHR: Iteration limit exceeded.')
  CALL MPABRT (29)

110 CONTINUE

  CALL MPMUL (S0, S0, S2, MPNW1)
  CALL MPADD (F, S2, S3, MPNW1)
  CALL MPSQRT (S3, S1, MPNW1)
  CALL MPROUN (S1, MPNW)
  CALL MPEQ (S1, X, MPNW)
  CALL MPROUN (S0, MPNW)
  CALL MPEQ (S0, Y, MPNW)
ELSE
  CALL MPEXP (A, S0, MPNW1)
  CALL MPDIV (F, S0, S1, MPNW1)
  CALL MPADD (S0, S1, S2, MPNW1)
  CALL MPMULD (S2, 0.5D0, S3, MPNW1)
  CALL MPROUN (S3, MPNW)
  CALL MPEQ (S3, X, MPNW)
  CALL MPSUB (S0, S1, S2, MPNW1)
  CALL MPMULD (S2, 0.5D0, S3, MPNW1)
  CALL MPROUN (S3, MPNW)
  CALL MPEQ (S3, Y, MPNW)
ENDIF

100 CONTINUE

RETURN
END SUBROUTINE MPCSSHR

SUBROUTINE MPCSSNR (A, X, Y, MPNW)

!   This computes the cosine and sine of the MPR number A and returns the
!   two MPR results in X and Y, respectively.  Pi must be precomputed to
!   at least MPNW words precision and the stored in the array MPPICON in
!   module MPMODA.

!   This routine uses the conventional Taylor series for Sin (s):

!   Sin (s) =  s - s^3 / 3! + s^5 / 5! - s^7 / 7! ...

!   where the argument S has been reduced to (-pi, pi).  To further
!   accelerate the series, the reduced argument is divided by 2^NQ, where NQ
!   is computed as int (sqrt (0.5d0 * N)), where N is the precision in bits.
!   After convergence of the series, the double-angle formulas for cos are
!   applied NQ times.

IMPLICIT NONE
INTEGER IS, ITRMX, I1, J, MPNW, MPNW1, MPNW2, NA, NQ, N1
PARAMETER (ITRMX = 1000000)
REAL (MPRKND) T1, T2
INTEGER (MPIKND) A(0:), F1(0:8), F2(0:8), X(0:), Y(0:), &
  S0(0:MPNW+6), S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6), &
  S5(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. A(0) < ABS (A(2)) + 4 .OR. &
  X(0) < MPNW + 6 .OR. Y(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCSSNR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NA = MIN (INT (ABS (A(2))), MPNW)
IF (NA == 0) THEN
  X(1) = MPNW
  X(2) = 1
  X(3) = 0
  X(4) = 1
  X(5) = 0
  X(6) = 0
  Y(1) = MPNW
  Y(2) = 0
  Y(3) = 0
  Y(4) = 0
  Y(5) = 0
  GOTO 120
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
S5(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Set f1 = 1 and f2 = 1/2.

F1(0) = 9
F1(1) = MPNW
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0
F2(0) = 9
F2(1) = MPNW
F2(2) = 1
F2(3) = -1
F2(4) = 0.5D0 * MPBDX
F2(5) = 0
F2(6) = 0

!   Check if Pi and Sqrt(2)/2 have been precomputed in data statements to the
!   requested precision.

IF (MPNW1 > MPPICON(1)) THEN
  WRITE (MPLDB, 2) MPNW1
2 FORMAT ('*** MPCSSNR: Pi must be precomputed to precision',I9,' words).'/ &
  'See documentation for details.')
  CALL MPABRT (27)
ENDIF

!   Check if argument is too large to compute meaningful cos/sin values.

CALL MPMDC (A, T1, N1, MPNW)
IF (N1 >= MPNBT * (MPNW - 1)) THEN
  WRITE (MPLDB, 3)
3 FORMAT ('*** MPCSSNR: argument is too large to compute cos or sin.')
  CALL MPABRT (28)
ENDIF

!   Reduce to between - Pi and Pi.

CALL MPMULD (MPPICON, 2.D0, S0, MPNW1)
CALL MPDIV (A, S0, S1, MPNW1)
CALL MPNINT (S1, S2, MPNW1)
CALL MPMUL (S0, S2, S4, MPNW1)
CALL MPSUB (A, S4, S3, MPNW1)

!   Check if reduced argument is zero.  If so then cos = 1 and sin = 0.

IF (S3(2) == 0) THEN
  S0(1) = MPNW1
  S0(2) = 1
  S0(3) = 0
  S0(4) = 1
  S0(5) = 0
  S0(6) = 0
  S1(1) = MPNW1
  S1(2) = 0
  S1(3) = 0
  S1(4) = 0
  S1(5) = 0
  GOTO 115
ENDIF

!   Determine nq to scale reduced argument, then divide by 2^nq.
!   If reduced argument is very close to zero, then nq = 0.

IF (S3(3) >= -1) THEN
  NQ = INT (SQRT (0.5D0 * MPNW1 * MPNBT))
ELSE
  NQ = 0
ENDIF

CALL MPDIVD (S3, 2.D0**NQ, S0, MPNW1)
CALL MPEQ (S0, S1, MPNW1)

!   Compute the sin of the reduced argument of s1 using a Taylor series.

CALL MPMUL (S0, S0, S2, MPNW1)
MPNW2 =  MPNW1
IS = S0(2)

!   The working precision used to compute each term can be linearly reduced
!   as the computation proceeds.

DO I1 = 1, ITRMX
  T2 = - (2.D0 * I1) * (2.D0 * I1 + 1.D0)
  CALL MPMUL (S2, S1, S3, MPNW2)
  CALL MPDIVD (S3, T2, S1, MPNW2)
  CALL MPADD (S1, S0, S3, MPNW1)
  CALL MPEQ (S3, S0, MPNW1)

!   Check for convergence of the series, and adjust working precision
!   for the next term.

  IF (S1(2) == 0 .OR. S1(3) < S0(3) - MPNW1) GOTO 110
  MPNW2 = MIN (MAX (MPNW1 + INT (S1(3) - S0(3)) + 1, 4), MPNW1)
ENDDO

WRITE (MPLDB, 4)
4 FORMAT ('*** MPCSSNR: Iteration limit exceeded.')
CALL MPABRT (29)

110 CONTINUE

IF (NQ > 0) THEN

!   Apply the formula cos(2*x) = 2*cos^2(x) - 1 NQ times to produce
!   the cosine of the reduced argument, except that the first iteration is
!   cos(2*x) = 1 - 2*sin^2(x), since we have computed sin(x) above.
!   Note that these calculations are performed as 2 * (cos^2(x) - 1/2) and
!   2 * (1/2 - sin^2(x)), respectively, to avoid loss of precision.

  CALL MPMUL (S0, S0, S4, MPNW1)
  CALL MPSUB (F2, S4, S5, MPNW1)
  CALL MPMULD (S5, 2.D0, S0, MPNW1)

  DO J = 2, NQ
    CALL MPMUL (S0, S0, S4, MPNW1)
    CALL MPSUB (S4, F2, S5, MPNW1)
    CALL MPMULD (S5, 2.D0, S0, MPNW1)
  ENDDO

!   Compute sin of result and correct sign.

  CALL MPMUL (S0, S0, S4, MPNW1)
  CALL MPSUB (F1, S4, S5, MPNW1)
  CALL MPSQRT (S5, S1, MPNW1)
  IF (IS < 1) S1(2) = - S1(2)
ELSE

!   In case nq = 0, compute cos of result.

  CALL MPEQ (S0, S1, MPNW1)
  CALL MPMUL (S0, S0, S4, MPNW1)
  CALL MPSUB (F1, S4, S5, MPNW1)
  CALL MPSQRT (S5, S0, MPNW1)
ENDIF

115 CONTINUE

!   Restore original precision level.

CALL MPROUN (S0, MPNW)
CALL MPROUN (S1, MPNW)
CALL MPEQ (S0, X, MPNW)
CALL MPEQ (S1, Y, MPNW)

120 CONTINUE

RETURN
END SUBROUTINE MPCSSNR

SUBROUTINE MPEGAMMAQ (EGAMMA, MPNW)

!   This computes Euler's gamma to available precision (MPNW mantissa words).
!   The algorithm is the following, which is an improvement to a scheme due to
!   Sweeney (see https://www.davidhbailey.com/dhbpapers/const.pdf):

!   Select N such that 1/(2^N * Exp(2^N)) < desired epsilon. Then compute
!   Gamma = 2^N/Exp(2^N) * (Sum_{m >= 0} 2^(m*N)/(m+1)! * H(m+1)) - N * Log(2),
!   where H(m) = 1 + 1/2 + ... + 1/m.

IMPLICIT NONE
INTEGER ITRMX, MPNW, MPNW1, M, NEPS, NN
PARAMETER (ITRMX = 1000000)
INTEGER (MPIKND) EGAMMA(0:), S0(0:MPNW+6), S1(0:MPNW+6), &
  S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6), S5(0:MPNW+6), S6(0:MPNW+6), &
  S7(0:MPNW+6), F(0:8)

! End of declaration.

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
S5(0) = MPNW + 7
S6(0) = MPNW + 7
S7(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Check if Log(2) has been precomputed.

IF (MPNW1 > MPLOG2CON(1)) THEN
  WRITE (MPLDB, 3) MPNW1
3 FORMAT ('*** MPEGAMMA: Log(2) must be precomputed to precision',I9,' words.'/ &
  'See documentation for details.')
  CALL MPABRT (35)
ENDIF

!   Compute eps and nn based on precision level.

NEPS = - MPNW1 - 1
NN = CEILING (LOG (DBLE (MPNW1 * MPNBT + MPNBT) * LOG (2.D0)) / LOG (2.D0))

!   Initialize s0 through s4 to 1.

S0(1) = MPNW
S0(2) = 1
S0(3) = 0
S0(4) = 1
S0(5) = 0
S0(6) = 0

S1(1) = MPNW
S1(2) = 1
S1(3) = 0
S1(4) = 1
S1(5) = 0
S1(6) = 0

S2(1) = MPNW
S2(2) = 1
S2(3) = 0
S2(4) = 1
S2(5) = 0
S2(6) = 0

S3(1) = MPNW
S3(2) = 1
S3(3) = 0
S3(4) = 1
S3(5) = 0
S3(6) = 0

S4(1) = MPNW
S4(2) = 1
S4(3) = 0
S4(4) = 1
S4(5) = 0
S4(6) = 0

S7(1) = MPNW
S7(2) = 1
S7(3) = 0
S7(4) = 2
S7(5) = 0
S7(6) = 0

!   Set s7 = 2^nn.

CALL MPDMC (1.D0, NN, S7, MPNW1)

!  Set f = 1.

F(0) = 9
F(1) = MPNW1
F(2) = 1
F(3) = 0
F(4) = 1
F(5) = 0
F(6) = 0

DO M = 1, ITRMX
  CALL MPMUL (S7, S0, S5, MPNW1)
  CALL MPEQ (S5, S0, MPNW1)
  CALL MPDMC (DBLE (M + 1), 0, S5, MPNW1)
  CALL MPDIV (F, S5, S6, MPNW1)
  CALL MPADD (S1, S6, S5, MPNW1)
  CALL MPEQ (S5, S1, MPNW1)
  CALL MPMULD (S2, M + 1.D0, S5, MPNW1)
  CALL MPEQ (S5, S2, MPNW1)
  CALL MPMUL (S0, S1, S5, MPNW1)
  CALL MPDIV (S5, S2, S3, MPNW1)
  CALL MPADD (S3, S4, S5, MPNW1)
  CALL MPEQ (S5, S4, MPNW1)
  IF (S3(3) - S4(3) < NEPS) GOTO 100
ENDDO

WRITE (MPLDB, 1)
1   FORMAT ('*** MPEGAMMA: Loop end error.')
    CALL MPABRT (36)

100 CONTINUE

CALL MPEXP (S7, S5, MPNW1)
CALL MPDIV (S7, S5, S6, MPNW1)
CALL MPMUL (S6, S4, S5, MPNW1)
CALL MPMULD (MPLOG2CON, DBLE (NN), S6, MPNW1)
CALL MPSUB (S5, S6, S0, MPNW1)

!   Restore original precision level.

CALL MPROUN (S0, MPNW)
CALL MPEQ (S0, EGAMMA, MPNW)

RETURN
END SUBROUTINE MPEGAMMAQ

SUBROUTINE MPEXP (A, B, MPNW)

!   This computes the exponential function of the MPR number A and returns
!   the MPR result in B.  Log(2) must be precomputed to at least MPNW words
!   precision and the stored in the array MPLOG2CON in module MPMODA.

!   This routine uses a modification of the Taylor series for Exp (t):

!   Exp (t) =  (1 + r + r^2 / 2! + r^3 / 3! + r^4 / 4! ...) ^ q * 2 ^ n

!   where the argument T has been reduced to within the closest factor of Log(2).
!   To further accelerate the series, the reduced argument is divided by 2^NQ.
!   After convergence of the series, the result is squared NQ times.  NQ = 12
!   by default.

!   If the precision level MPNW exceeds MPNWX words, this subroutine calls
!   MPEXPX instead.  By default, MPNWX = 700 (approx. 10100 digits).

IMPLICIT NONE
INTEGER ITRMX, J, MPNW, MPNW1, MPNW2, NQ, NZ, N1
REAL (MPRKND) T1, T2
PARAMETER (ITRMX = 1000000)
INTEGER (MPIKND) A(0:), B(0:), F(0:8), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. A(0) < ABS (A(2)) + 4 .OR. &
  B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPEXP: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

CALL MPMDC (A, T1, N1, MPNW)

!   Check for overflows and underflows.

IF (N1 > 30) THEN
  IF (T1 > 0.D0) THEN
    WRITE (MPLDB, 2)
2   FORMAT ('*** MPEXP: Argument is too large.')
    CALL MPABRT (34)
  ELSE
    B(1) = MPNW
    B(2) = 0
    B(3) = 0
    B(4) = 0
    B(5) = 0
    GOTO 130
  ENDIF
ENDIF

T1 = T1 * 2.D0 ** N1
IF (ABS (T1) > 1488522236.D0) THEN
  IF (T1 > 0) THEN
    WRITE (MPLDB, 2)
    CALL MPABRT (34)
  ELSE
    B(1) = MPNW
    B(2) = 0
    B(3) = 0
    B(4) = 0
    B(5) = 0
    GOTO 130
  ENDIF
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Set f1 = 1.

F(0) = 9
F(1) = MPNW1
F(2) = 1
F(3) = 0
F(4) = 1
F(5) = 0
F(6) = 0

!   Check if Log(2) has been precomputed.

IF (MPNW1 > MPLOG2CON(1)) THEN
  WRITE (MPLDB, 3) MPNW1
3 FORMAT ('*** MPLOG: Log(2) must be precomputed to precision',I9,' words.'/ &
  'See documentation for details.')
  CALL MPABRT (35)
ENDIF

!   Compute the reduced argument A' = A - Log(2) * Nint [A / Log(2)].  Save
!   NZ = Nint [A / Log(2)] for correcting the exponent of the final result.

CALL MPDIV (A, MPLOG2CON, S0, MPNW1)
CALL MPNINT (S0, S1, MPNW1)
CALL MPMDC (S1, T1, N1, MPNW1)
NZ = NINT (T1 * 2.D0 ** N1)
CALL MPMUL (MPLOG2CON, S1, S2, MPNW1)
CALL MPSUB (A, S2, S0, MPNW1)

!   Check if the reduced argument is zero.

IF (S0(2) == 0) THEN
  S0(1) = MPNW1
  S0(2) = 1
  S0(3) = 0
  S0(4) = 1
  S0(5) = 0
  S0(6) = 0
  GOTO 120
ENDIF

!   Divide the reduced argument by 2 ^ NQ.

NQ = MAX (NINT (DBLE (MPNW * MPNBT)** 0.4D0), 1)
CALL MPDIVD (S0, 2.D0**NQ, S1, MPNW1)

!   Compute Exp using the usual Taylor series.

CALL MPEQ (F, S2, MPNW1)
CALL MPEQ (F, S3, MPNW1)
MPNW2 =  MPNW1

!   The working precision used to compute each term can be linearly reduced
!   as the computation proceeds.

DO J = 1, ITRMX
  T2 = DBLE (J)
  CALL MPMUL (S2, S1, S0, MPNW2)
  CALL MPDIVD (S0, T2, S2, MPNW2)
  CALL MPADD (S3, S2, S0, MPNW1)
  CALL MPEQ (S0, S3, MPNW1)

!   Check for convergence of the series, and adjust working precision
!   for the next term.

  IF (S2(2) == 0 .OR. S2(3) < S0(3) - MPNW1) GOTO 100
  MPNW2 = MIN (MAX (MPNW1 + INT (S2(3) - S0(3)) + 1, 4), MPNW1)
ENDDO

WRITE (MPLDB, 4)
4 FORMAT ('*** MPEXP: Iteration limit exceeded.')
CALL MPABRT (36)

100 CONTINUE

!   Raise to the (2 ^ NQ)-th power.

DO J = 1, NQ
  CALL MPMUL (S0, S0, S1, MPNW1)
  CALL MPEQ (S1, S0, MPNW1)
ENDDO

!   Multiply by 2 ^ NZ.

120 CONTINUE

CALL MPDMC (1.D0, NZ, S2, MPNW1)
CALL MPMUL (S0, S2, S1, MPNW1)

!   Restore original precision level.

CALL MPROUN (S1, MPNW)
CALL MPEQ (S1, B, MPNW)

130 CONTINUE

RETURN
END SUBROUTINE MPEXP

SUBROUTINE MPINITRAN (MPNW)

!   This routine computes pi, log(2) sqrt(2)/2, and stores this data in the
!   proper arrays in module MPFUNA.  MPNW is the largest precision level
!   (in words) that will be subsequently required for this run at the user level.

IMPLICIT NONE
INTEGER MPNW, NWDS, NWDS6

!   Add three words to mpnw, since many of the routines in this module
!   increase the working precision level by one word upon entry.

NWDS = MPNW + 3

!  Compute pi, log(2) and sqrt(2)/2.

NWDS6 = NWDS + 6
MPLOG2CON(0) = NWDS6
MPLOG2CON(1) = 0
MPPICON(0) = NWDS6
MPPICON(1) = 0

CALL MPPIQ (MPPICON, NWDS)
CALL MPLOG2Q (MPPICON, MPLOG2CON, NWDS)

RETURN
END SUBROUTINE MPINITRAN

SUBROUTINE MPLOG (A, B, MPNW)

!   This computes the natural logarithm of the MPR number A and returns the MPR
!   result in B.

!   The Taylor series for Log converges much more slowly than that of Exp.
!   Thus this routine does not employ Taylor series (except if the argument
!   is extremely close to 1), but instead computes logarithms by solving
!   Exp (b) = a using the following Newton iteration:

!     x_{k+1} = x_k + [a - Exp (x_k)] / Exp (x_k)

!   These iterations are performed with a maximum precision level MPNW that
!   is dynamically changed, approximately doubling with each iteration.

IMPLICIT NONE
INTEGER IA, IQ, IS, ITRMAX, I1, K, MPNW, MPNW1, MQ, NA, NIT, N1
REAL (MPRKND) ALT, CL2, RTOL, ST, TOL, T1, T2
PARAMETER (ALT = 0.693147180559945309D0, CL2 = 1.4426950408889633D0, &
  RTOL = 0.5D0**7, ITRMAX = 1000000, NIT = 3)
INTEGER (MPIKND) A(0:), B(0:), F1(0:8), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. A(0) < ABS (A(2)) + 4 .OR. &
  B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPLOG: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)

IF (IA .LT. 0 .OR. NA == 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPLOG: Argument is less than or equal to zero.')
  CALL MPABRT (50)
ENDIF

!   Check if input is exactly one.

IF (A(2) == 1 .AND. A(3) == 0 .AND. A(4) == 1) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 130
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7

F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

!   If the argument is sufficiently close to 1, employ a Taylor series.

CALL MPSUB (A, F1, S0, MPNW1)

IF (S0(2) == 0 .OR. S0(3) <= MIN (-2.D0, - RTOL * MPNW1)) THEN
  CALL MPEQ (S0, S1, MPNW1)
  CALL MPEQ (S1, S2, MPNW1)
  I1 = 1
  IS = 1
  TOL = S0(3) - MPNW1

  DO I1 = 2, ITRMAX
    IS = - IS
    ST = IS * I1
    CALL MPMUL (S1, S2, S3, MPNW1)
    CALL MPEQ (S3, S2, MPNW1)
    CALL MPDIVD (S3, ST, S4, MPNW1)
    CALL MPADD (S0, S4, S3, MPNW1)
    CALL MPEQ (S3, S0, MPNW1)
    IF (S4(2) == 0 .OR. S4(3) < TOL) GOTO 120
  ENDDO

  WRITE (MPLDB, 3) ITRMAX
3 FORMAT ('*** MPLOG: Iteration limit exceeded',I10)
  CALL MPABRT (54)
ENDIF

!   Determine the least integer MQ such that 2 ^ MQ .GE. MPNW.

T2 = MPNW
MQ = CL2 * LOG (T2) + 2.D0 - MPRDFZ

!   Compute initial approximation of Log (A).

CALL MPMDC (A, T1, N1, MPNW)
T1 = LOG (T1) + N1 * ALT
CALL MPDMC (T1, 0, S3, MPNW)
MPNW1 = 4
IQ = 0

!   Perform the Newton-Raphson iteration described above with a dynamically
!   changing precision level MPNW (one greater than powers of two).

DO K = 0, MQ
  IF (K > 1) MPNW1 = MIN (2 * MPNW1 - 2, MPNW) + 1

110  CONTINUE

  CALL MPEXP (S3, S0, MPNW1)
  CALL MPSUB (A, S0, S1, MPNW1)
  CALL MPDIV (S1, S0, S2, MPNW1)
  CALL MPADD (S3, S2, S1, MPNW1)
  CALL MPEQ (S1, S3, MPNW1)
  IF (K == MQ - NIT .AND. IQ == 0) THEN
    IQ = 1
    GOTO 110
  ENDIF
ENDDO

!   Restore original precision level.

120 CONTINUE

CALL MPROUN (S3, MPNW)
CALL MPEQ (S3, B, MPNW)

130 CONTINUE

RETURN
END SUBROUTINE MPLOG

SUBROUTINE MPLOG2Q (PI, ALOG2, MPNW)

!   This computes log(2) to mpnw words precision, using an algorithm due to Salamin
!   and Brent:  Select n > 2^m, where m is the number of bits of desired precision
!   precision in the result.  Then

!   Log(2) = Pi / [2 AGM (1, 4/x)]

!   Where AGM (a, b) denotes the arithmetic-geometric mean:  Set a_0 = a and
!   b_0 = b, then iterate
!    a_{k+1} = (a_k + b_k)/2
!    b_{k+1} = sqrt (a_k * b_k)
!   until convergence (i.e., until a_k = b_k to available precision).

IMPLICIT NONE
INTEGER MPNW, MPNW1, N, N1, N48
REAL (MPRKND) CPI, T1
PARAMETER (CPI = 3.141592653589793238D0)
INTEGER (MPIKND) ALOG2(0:), F1(0:8), F4(0:8), PI(0:), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. PI(0) < MPNW + 4 .OR. PI(0) < ABS (PI(2)) + 4 .OR. &
  ALOG2(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPLOG2Q: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Define sections of the scratch array.

S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Unless precision is very high, just copy log2 from table.

IF (MPNW1 <= MPLOG2CON(1)) THEN
  CALL MPEQ (MPLOG2CON, ALOG2, MPNW)
  GOTO 100
ENDIF

!   Check if Pi has been precomputed.

CALL MPMDC (PI, T1, N1, MPNW)
IF (N1 /= 1 .OR. ABS (T1 * 2.D0**N1 - CPI) > MPRDFZ .OR. ABS (PI(2)) < MPNW) THEN
  WRITE (MPLDB, 2) MPNW
2 FORMAT ('*** MPLOG2Q: Pi must be precomputed to precision',I9,' words.'/ &
  'See documentation for details.')
  CALL MPABRT (53)
ENDIF

!   Define sections of the scratch array.

S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Set f1 = 1.

F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

!   Set f4 = 4.

F4(0) = 9
F4(1) = MPNW1
F4(2) = 1
F4(3) = 0
F4(4) = 4
F4(5) = 0
F4(6) = 0

!   Set s4 to 2^(n/2), where n is the number of bits desired. n48 = n/mpnbt.
!   Note that this value can be directly set in the first few words of s4,
!   avoiding explicit exponentiation.

N = MPNBT * (MPNW1 / 2 + 2)
N48 = N / MPNBT

S4(1) = MPNW1
S4(2) = 1
S4(3) = N48
S4(4) = 1
S4(5) = 0
S4(6) = 0

!   Perform AGM iterations.

CALL MPEQ (F1, S1, MPNW1)
CALL MPDIV (F4, S4, S2, MPNW1)
CALL MPAGMR (S1, S2, S3, MPNW1)

!   Set Log(2) = Pi / (2 * N * S3), where S3 is the limit of the AGM iterations.

CALL MPMULD (S3, 2.D0 * N, S1, MPNW1)
CALL MPDIV (PI, S1, S2, MPNW1)
CALL MPROUN (S2, MPNW)
CALL MPEQ (S2, ALOG2, MPNW)

100 CONTINUE

RETURN
END SUBROUTINE MPLOG2Q

SUBROUTINE MPPIQ (PI, MPNW)

!   This computes Pi to available precision (MPNW mantissa words).
!   The algorithm that is used for computing Pi, which is due to Salamin
!   and Brent, is as follows:

!   Set  A_0 = 1,  B_0 = 1/Sqrt(2)  and  D_0 = Sqrt(2) - 1/2.

!   Then from k = 1 iterate the following operations:

!   A_k = 0.5 * (A_{k-1} + B_{k-1})
!   B_k = Sqrt (A_{k-1} * B_{k-1})
!   D_k = D_{k-1} - 2^k * (A_k - B_k) ^ 2

!   Then  P_k = (A_k + B_k) ^ 2 / D_k  converges quadratically to Pi.
!   In other words, each iteration approximately doubles the number of correct
!   digits, providing all iterations are done with the maximum precision.
!   The constant cl2 (below) = 1 / log(2) (DP approximation).

IMPLICIT NONE
INTEGER K, MPNW, MPNW1, MQ
INTEGER (MPIKND) PI(0:), S0(0:MPNW+6), S1(0:MPNW+6), S2(0:MPNW+6), &
  S3(0:MPNW+6), S4(0:MPNW+6), F(0:8)
REAL (MPRKND) CL2, T1
PARAMETER (CL2 = 1.4426950408889633D0)

! End of declaration

IF (MPNW < 4 .OR. PI(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPPIQ: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Unless precision is very high, just copy pi from table.

IF (MPNW1 <= MPPICON(1)) THEN
  CALL MPEQ (MPPICON, PI, MPNW)
  GOTO 100
ENDIF

!   Determine the number of iterations required for the given precision level.
!   This formula is good only for this Pi algorithm.

T1 = MPNW1 * LOG10 (DBLE (MPBDX))
MQ = CL2 * (LOG (T1) - 1.D0) + 1.D0

!   Initialize as above.

S0(1) = MPNW
S0(2) = 1
S0(3) = 0
S0(4) = 1
S0(5) = 0
S0(6) = 0
F(0) = 9
F(1) = MPNW1
F(2) = 1
F(3) = 0
F(4) = 2
F(5) = 0
F(6) = 0
CALL MPSQRT (F, S2, MPNW1)
CALL MPMULD (S2, 0.5D0, S1, MPNW1)
F(3) = -1
F(4) = 0.5D0 * MPBDX
CALL MPSUB (S2, F, S4, MPNW1)

!   Perform iterations as described above.

DO K = 1, MQ
  CALL MPADD (S0, S1, S2, MPNW1)
  CALL MPMUL (S0, S1, S3, MPNW1)
  CALL MPSQRT (S3, S1, MPNW1)
  CALL MPMULD (S2, 0.5D0, S0, MPNW1)
  CALL MPSUB (S0, S1, S2, MPNW1)
  CALL MPMUL (S2, S2, S3, MPNW1)
  T1 = 2.D0 ** K
  CALL MPMULD (S3, T1, S2, MPNW1)
  CALL MPSUB (S4, S2, S3, MPNW1)
  CALL MPEQ (S3, S4, MPNW1)
ENDDO

!   Complete computation.

CALL MPADD (S0, S1, S2, MPNW1)
CALL MPMUL (S2, S2, S2, MPNW1)
CALL MPDIV (S2, S4, S2, MPNW1)
CALL MPEQ (S2, S0, MPNW1)

!   Restore original precision level.

CALL MPROUN (S0, MPNW)
CALL MPEQ (S0, PI, MPNW)

100 CONTINUE

RETURN
END SUBROUTINE MPPIQ

SUBROUTINE MPPOWER (A, B, C, MPNW)

!   This computes C = A ^ B, where A, B and C are MPR.  It first checks if
!   B is the quotient of two integers up to 10^7 in size, in which case it
!   calls MPNPWR and MPNRTR.  Otherwise it calls MPLOG and MPEXP.

IMPLICIT NONE
INTEGER I, MPNW, N1
REAL (MPRKND) A1, A2, A3, A4, A5, A6, Q1, T0, T1, T2, T3, MPRXX
PARAMETER (MPRXX = 5.D-10)
INTEGER (MPIKND) A(0:), B(0:), C(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6)

!  End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 4 .OR. B(0) < ABS (A(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPPOWER: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check if A <= 0 (error), or A = 1 or B = 0 or B = 1.

IF (A(2) <= 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPPOWER: A^B, where A is less than zero.')
  CALL MPABRT (61)
ELSEIF ((A(2) == 1 .AND. A(3) == 0 .AND. A(4) == 1) &
  .OR. B(2) == 0) THEN
  C(1) = MPNW
  C(2) = 1
  C(3) = 0
  C(4) = 1
  C(5) = 0
  C(6) = 0
  GOTO 200
ELSEIF (B(2) == 1 .AND. B(3) == 0 .AND. B(4) == 1) THEN
  CALL MPEQ (A, C, MPNW)
  GOTO 200
ENDIF

S0(0) = MPNW + 6
S1(0) = MPNW + 6
S2(0) = MPNW + 6

!   Check if B is rational using the extended Euclidean algorithm in DP.

CALL MPMDC (B, T1, N1, MPNW)

IF (N1 >= -MPNBT .AND. N1 <= MPNBT) THEN
  T0 = ABS (T1 * 2.D0**N1)
  T1 = MAX (T0, 1.D0)
  T2 = MIN (T0, 1.D0)
  A1 = 1.D0
  A2 = 0.D0
  A3 = 0.D0
  A4 = 1.D0

  DO I = 1, 20
    Q1 = AINT (T1 / T2)
    A5 = A1 - Q1 * A3
    A6 = A2 - Q1 * A4
    T3 = T2
    T2 = T1 - Q1 * T2
    T1 = T3
    A1 = A3
    A2 = A4
    A3 = A5
    A4 = A6
    IF (T2 < MPRXX) GOTO 100
  ENDDO

  GOTO 110
ENDIF

100 CONTINUE

A3 = ABS (A3)
A4 = ABS (A4)

!  If b = a3/a4 or a4/a3 (except for sign) or then call mpnpwr and mpnrtr.

IF (ABS (T0 - A3 / A4) / T0 < MPRDFZ) THEN
  A3 = SIGN (A3, DBLE (B(2)))
  CALL MPDMC (A3, 0, S0, MPNW)
  CALL MPDMC (A4, 0, S1, MPNW)
  CALL MPDIV (S0, S1, S2, MPNW)
  CALL MPSUB (B, S2, S0, MPNW)
  IF (S0(2) == 0 .OR. S0(3) < B(3) + 1 - MPNW) THEN
    CALL MPNPWR (A, INT (A3), S0, MPNW)
    CALL MPNRTR (S0, INT (A4), C, MPNW)
    GOTO 200
  ENDIF
ELSEIF (ABS (T0 - A4 / A3) / T0 < MPRDFZ) THEN
  A4 = SIGN (A4, DBLE (B(2)))
  CALL MPDMC (A4, 0, S0, MPNW)
  CALL MPDMC (A3, 0, S1, MPNW)
  CALL MPDIV (S0, S1, S2, MPNW)
  CALL MPSUB (B, S2, S0, MPNW)
  IF (S0(2) == 0 .OR. S0(3) < B(3) + 1 - MPNW) THEN
    CALL MPNPWR (A, INT (A4), S0, MPNW)
    CALL MPNRTR (S0, INT (A3), C, MPNW)
    GOTO 200
  ENDIF
ENDIF

110 CONTINUE

!  Call mplog and mpexp.

CALL MPLOG (A, S0, MPNW)
CALL MPMUL (S0, B, S1, MPNW)
CALL MPEXP (S1, C, MPNW)

200 CONTINUE

RETURN
END SUBROUTINE MPPOWER

END MODULE ModLib_MPFUND


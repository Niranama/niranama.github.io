!*****************************************************************************

!  MPFUN20-Fort: A thread-safe arbitrary precision computation package
!  Basic function module (module MPFUNB)

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

!  DESCRIPTION OF THIS MODULE (MPFUNB):
!    This module contains routines for: add, subtract, multiply, divide;
!    comparison; double/multi conversion; double/multi multiplication/division;
!    integer and fractional parts; nearest integer; nth power; nth root;
!    rounding and normalization, random numbers, square roots, conversions
!    to/from quad precision (if available), and routines to support FFT-based
!    multiplication and Newton division (used for very high precision). The
!    routines in this package are not intended to be directly called by the
!    user; the high-level language interface modules should be used instead.

MODULE ModLib_MPFUNB
USE ModLib_MPFUNA
IMPLICIT NONE

CONTAINS

SUBROUTINE MPABRT (IER)

!   This routine terminates execution.  Users may wish to replace the
!   default STOP with a call to a system routine that provides a traceback.

IMPLICIT NONE
INTEGER IER

! End of declaration

WRITE (MPLDB, 1) IER
1 FORMAT ('*** MPABRT: Execution terminated, error code =',I4)
STOP
END SUBROUTINE MPABRT

SUBROUTINE MPADD (A, B, C, MPNW)

!   This routine adds MPR numbers A and B to yield C.

IMPLICIT NONE
INTEGER I, IA, IB, IDB, ISH, IXA, IXB, IXD, MPNW, M1, M2, M3, M4, M5, NA, NB, &
  ND, NSH
INTEGER (MPIKND) A(0:), B(0:), C(0:)
INTEGER (MPIKND) D(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPADD: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
IB = SIGN (INT (1, MPIKND), B(2))
NA = MIN (INT (ABS (A(2))), MPNW)
NB = MIN (INT (ABS (B(2))), MPNW)

!   Check for zero inputs.

IF (NA == 0) THEN

!   A is zero -- the result is B.

  C(1) = MPNW
  C(2) = SIGN (NB, IB)

  DO I = 2, NB + 3
    C(I+1) = B(I+1)
  ENDDO

  C(NB+4) = 0
  C(NB+5) = 0
  GOTO 100
ELSEIF (NB == 0) THEN

!   B is zero -- the result is A.

  C(1) = MPNW
  C(2) = SIGN (NA, IA)

  DO I = 2, NA + 3
    C(I+1) = A(I+1)
  ENDDO

  C(NA+4) = 0
  C(NA+5) = 0
  GOTO 100
ENDIF

IF (IA == IB) THEN
  IDB = 1
ELSE
  IDB = -1
ENDIF
IXA = A(3)
IXB = B(3)
ISH = IXA - IXB

IF (ISH >= 0) THEN

!   A has greater exponent than B, so B must be shifted to the right.

!  m1 = number of initial A words to be copied without adding B.
!  m2 = end index of A words to be added to shifted B words, after end of initial A.
!  m3 = end index of A words to be copied without adding, after end of shifted B section.
!  m4 = end index of zero words after the end of A words.
!  m5 = end index of B words to be copied with a shift, after end of A words.

  M1 = MIN (NA, ISH)
  M2 = MIN (NA, NB + ISH)
  M3 = NA
  M4 = MIN (MAX (NA, ISH), MPNW + 1)
  M5 = MIN (MAX (NA, NB + ISH), MPNW + 1)

  DO I = 1, M1
    D(I+3) = A(I+3)
  ENDDO

  DO I = M1 + 1, M2
    D(I+3) = A(I+3) + IDB * B(I+2-ISH+1)
  ENDDO

  DO I = M2 + 1, M3
    D(I+3) = A(I+3)
  ENDDO

  DO I = M3 + 1, M4
    D(I+3) = 0
  ENDDO

  DO I = M4 + 1, M5
    D(I+3) = IDB * B(I+2-ISH+1)
  ENDDO

  ND = M5
  IXD = IXA
  D(ND+4) = 0
  D(ND+5) = 0
ELSE

!   B has greater exponent than A, so A must be shifted to the right.

  NSH = - ISH
  M1 = MIN (NB, NSH)
  M2 = MIN (NB, NA + NSH)
  M3 = NB
  M4 = MIN (MAX (NB, NSH), MPNW + 1)
  M5 = MIN (MAX (NB, NA + NSH), MPNW + 1)

  DO I = 1, M1
    D(I+3) = IDB * B(I+3)
  ENDDO

  DO I = M1 + 1, M2
    D(I+3) = A(I+2-NSH+1) + IDB * B(I+3)
  ENDDO

  DO I = M2 + 1, M3
    D(I+3) = IDB * B(I+3)
  ENDDO

  DO I = M3 + 1, M4
    D(I+3) = 0
  ENDDO

  DO I = M4 + 1, M5
    D(I+3) = A(I+2-NSH+1)
  ENDDO

  ND = M5
  IXD = IXB
  D(ND+4) = 0
  D(ND+5) = 0
ENDIF

!   Call mpnorm to fix up result and store in c.

D(0) = MPNW + 6
D(1) = MPNW
D(2) = SIGN (ND, IA)
D(3) = IXD

CALL MPNORM (D, C, MPNW)

100 CONTINUE

RETURN
END SUBROUTINE MPADD

SUBROUTINE MPCABS (A, B, MPNW)

!   This routine returns the absolute value of the MPC argument A (the
!   result is of type MPR).

IMPLICIT NONE
INTEGER LA, MPNW, MPNW1
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+6), S1(0:MPNW+6), &
  S2(0:MPNW+6)

! End of declaration

LA = A(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCABS: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
CALL MPMUL (A, A, S0, MPNW1)
CALL MPMUL (A(LA:), A(LA:), S1, MPNW1)
CALL MPADD (S0, S1, S2, MPNW1)
CALL MPSQRT (S2, S0, MPNW1)
CALL MPROUN (S0, MPNW)
CALL MPEQ (S0, B, MPNW)

RETURN
END SUBROUTINE MPCABS

SUBROUTINE MPCADD (A, B, C, MPNW)

!   This routine adds the MPC numbers A and B.

IMPLICIT NONE
INTEGER LA, LB, LC, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:)

! End of declaration

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 .OR. &
  C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCADD: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

CALL MPADD (A, B, C, MPNW)
CALL MPADD (A(LA:), B(LB:), C(LC:), MPNW)
RETURN
END SUBROUTINE MPCADD

SUBROUTINE MPCDIV (A, B, C, MPNW)

!   This routine divides the MPC numbers A and B.

IMPLICIT NONE
INTEGER LA, LB, LC, MPNW, MPNW1
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S0(0:MPNW+6), S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6), S4(0:MPNW+6)

! End of declaration

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 .OR. &
  C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCDIV: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7

CALL MPMUL (A, B, S0, MPNW1)
CALL MPMUL (A(LA:), B(LB:), S1, MPNW1)
CALL MPADD (S0, S1, S2, MPNW1)
CALL MPMUL (A, B(LB:), S0, MPNW1)
S0(2) = - S0(2)
CALL MPMUL (A(LA:), B, S1, MPNW1)
CALL MPADD (S0, S1, S3, MPNW1)

CALL MPMUL (B, B, S0, MPNW1)
CALL MPMUL (B(LB:), B(LB:), S1, MPNW1)
CALL MPADD (S0, S1, S4, MPNW1)
CALL MPDIV (S2, S4, S0, MPNW1)
CALL MPDIV (S3, S4, S1, MPNW1)


CALL MPROUN (S0, MPNW)
CALL MPROUN (S1, MPNW)
CALL MPEQ (S0, C, MPNW)
CALL MPEQ (S1, C(LC:), MPNW)

RETURN
END SUBROUTINE MPCDIV

SUBROUTINE MPCEQ (A, B, MPNW)

!   Sets the MPC number B equal to A.

IMPLICIT NONE
INTEGER I, IA, LA, LB, MPNW, NA
INTEGER (MPIKND) A(0:), B(0:)

! End of declaration

LA = A(0)
LB = B(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < MPNW + 6 .OR. B(LB) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCEQ: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
IF (NA == 0)  THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 110
ENDIF
B(1) = MPNW
B(2) = SIGN (NA, IA)

DO I = 2, NA + 2
  B(I+1) = A(I+1)
ENDDO

B(NA+4) = 0
B(NA+5) = 0

110 CONTINUE

IA = SIGN (INT (1, MPIKND), A(LA+2))
NA = MIN (INT (ABS (A(LA+2))), MPNW)
IF (NA == 0)  THEN
  B(LB+1) = MPNW
  B(LB+2) = 0
  B(LB+3) = 0
  B(LB+4) = 0
  B(LB+5) = 0
  GOTO 120
ENDIF
B(LB+1) = MPNW
B(LB+2) = SIGN (NA, IA)

DO I = 2, NA + 2
  B(I+LB+1) = A(I+LA+1)
ENDDO

B(NA+LB+4) = 0
B(NA+LB+5) = 0

120 CONTINUE

RETURN
END SUBROUTINE MPCEQ

SUBROUTINE MPCMUL (A, B, C, MPNW)

!   This routine multiplies the MPC numbers A and B.

IMPLICIT NONE
INTEGER LA, LB, LC, MPNW, MPNW1
INTEGER (MPIKND) A(0:), B(0:), C(0:), &
  S0(0:MPNW+6), S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 .OR. &
  C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCMUL: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7

CALL MPMUL (A, B, S0, MPNW1)
CALL MPMUL (A(LA:), B(LB:), S1, MPNW1)
CALL MPSUB (S0, S1, S2, MPNW1)
CALL MPMUL (A, B(LB:), S0, MPNW1)
CALL MPMUL (A(LA:), B, S1, MPNW1)
CALL MPADD (S0, S1, S3, MPNW1)

CALL MPROUN (S2, MPNW)
CALL MPROUN (S3, MPNW)
CALL MPEQ (S2, C, MPNW)
CALL MPEQ (S3, C(LC:), MPNW)

RETURN
END SUBROUTINE MPCMUL

SUBROUTINE MPCNPWR (A, N, B, MPNW)

!   This computes the N-th power of the MPC number A and returns the MPC result
!   in B.  When N is zero, 1 is returned.  When N is negative, the reciprocal
!   of A ^ |N| is returned.

IMPLICIT NONE
INTEGER J, KK, KN, LA, LB, LC, MN, MPNW, MPNW1, N, NA, NN
REAL (MPRKND) CL2, T1
PARAMETER (CL2 = 1.4426950408889633D0)
INTEGER (MPIKND) A(0:), B(0:), S0(0:2*MPNW+13), S1(0:2*MPNW+13), &
  S2(0:2*MPNW+13)

! End of declaration

LA = A(0)
LB = B(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 .OR. &
  B(0) < MPNW + 6 .OR. B(LB) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCNPWR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NA = MIN (INT (ABS (A(2))), MPNW)
IF (NA == 0) THEN
  IF (N >= 0) THEN
      B(1) = MPNW
    B(2) = 0
    B(3) = 0
    B(4) = 0
    B(5) = 0
    GOTO 120
  ELSE
    WRITE (MPLDB, 2)
2   FORMAT ('*** MPCNPWR: Argument is zero and N is negative or zero.')
    CALL MPABRT (57)
  ENDIF
ENDIF

MPNW1 = MPNW + 1
LC = MPNW + 7
S0(0) = MPNW + 7
S0(LC) = MPNW + 7
S1(0) = MPNW + 7
S1(LC) = MPNW + 7
S2(0) = MPNW + 7
S2(LC) = MPNW + 7

NN = ABS (N)
IF (NN == 0) THEN
  CALL MPDMC (1.D0, 0, B, MPNW)
  CALL MPDMC (0.D0, 0, B(LB:), MPNW)
  GOTO 120
ELSEIF (NN == 1) THEN
  CALL MPCEQ (A, S2, MPNW1)
  GOTO 110
ELSEIF (NN == 2) THEN
  CALL MPCMUL (A, A, S2, MPNW1)
  GOTO 110
ENDIF

!   Determine the least integer MN such that 2 ^ MN .GT. NN.

T1 = NN
MN = CL2 * LOG (T1) + 1.D0 + MPRDFZ
CALL MPDMC (1.D0, 0, S2, MPNW1)
CALL MPDMC (0.D0, 0, S2(LC:), MPNW1)
CALL MPCEQ (A, S0, MPNW1)
KN = NN

!   Compute B ^ N using the binary rule for exponentiation.

DO J = 1, MN
  KK = KN / 2
  IF (KN /= 2 * KK) THEN
    CALL MPCMUL (S2, S0, S1, MPNW1)
    CALL MPCEQ (S1, S2, MPNW1)
  ENDIF
  KN = KK
  IF (J < MN) THEN
    CALL MPCMUL (S0, S0, S1, MPNW1)
    CALL MPCEQ (S1, S0, MPNW1)
  ENDIF
ENDDO

!   Compute reciprocal if N is negative.

110 CONTINUE

IF (N < 0) THEN
  CALL MPDMC (1.D0, 0, S1, MPNW1)
  CALL MPDMC (0.D0, 0, S1(LC:), MPNW1)
  CALL MPCDIV (S1, S2, S0, MPNW1)
  CALL MPCEQ (S0, S2, MPNW1)
ENDIF

!   Restore original precision level.

CALL MPROUN (S2, MPNW)
CALL MPROUN (S2(LC:), MPNW)
CALL MPCEQ (S2, B, MPNW)

120 CONTINUE
RETURN
END SUBROUTINE MPCNPWR

SUBROUTINE MPCONJG (A, B, MPNW)

!   This routine returns the conjugate of the MPC argument A.

IMPLICIT NONE
INTEGER LA, LB, MPNW
INTEGER (MPIKND) A(0:), B(0:)

! End of declaration

LA = A(0)
LB = B(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < MPNW + 6 .OR. B(LB) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCONJ: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

CALL MPCEQ (A, B, MPNW)
B(LB+2) = - B(LB+2)
RETURN
END SUBROUTINE MPCONJG

SUBROUTINE MPCSQRT (A, B, MPNW)

!   This routine returns the square root of the MPC argument A.
!   The formula is:

!   1/Sqrt[2] * (Sqrt[r + a1] + I * a2 / Sqrt[r + a1])  if a1 >= 0, or
!   1/Sqrt[2] * (|a2| / Sqrt[r - a1] + I * Sgn[a2] * Sqrt[r - a1]) if a1 < 0,

!   where r = Sqrt[a1^2 + a2^2], and a1 and a2 are the real and imaginary
!   parts of A.

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
1 FORMAT ('*** MPCSQRT: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
S4(0) = MPNW + 7

CALL MPMUL (A, A, S0, MPNW1)
CALL MPMUL (A(LA:), A(LA:), S1, MPNW1)
CALL MPADD (S0, S1, S2, MPNW1)
CALL MPSQRT (S2, S0, MPNW1)

IF (A(2) >= 0) THEN
  CALL MPADD (S0, A, S1, MPNW1)
  CALL MPSQRT (S1, S0, MPNW1)
  CALL MPDIV (A(LA:), S0, S1, MPNW1)
ELSE
  CALL MPSUB (S0, A, S2, MPNW1)
  CALL MPSQRT (S2, S1, MPNW1)
  CALL MPDIV (A(LA:), S1, S0, MPNW1)
  S0(2) = ABS (S0(2))
  S1(2) = SIGN (S1(2), A(LA+2))
ENDIF

CALL MPDMC (0.5D0, 0, S3, MPNW1)
CALL MPSQRT (S3, S2, MPNW1)
CALL MPMUL (S0, S2, S3, MPNW1)
CALL MPMUL (S1, S2, S4, MPNW1)

CALL MPROUN (S3, MPNW)
CALL MPROUN (S4, MPNW)
CALL MPEQ (S3, B, MPNW)
CALL MPEQ (S4, B(LB:), MPNW)

RETURN
END SUBROUTINE MPCSQRT

SUBROUTINE MPCSUB (A, B, C, MPNW)

!   This routine subtracts the MPC numbers A and B.

IMPLICIT NONE
INTEGER LA, LB, LC, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:)

! End of declaration

LA = A(0)
LB = B(0)
LC = C(0)
IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(LA) < ABS (A(LA+2)) + 4 &
  .OR. B(0) < ABS (B(2)) + 4 .OR. B(LB) < ABS (B(LB+2)) + 4 .OR. &
  C(0) < MPNW + 6 .OR. C(LC) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCSUB: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

CALL MPSUB (A, B, C, MPNW)
CALL MPSUB (A(LA:), B(LB:), C(LC:), MPNW)
RETURN
END SUBROUTINE MPCSUB

SUBROUTINE MPCPR (A, B, IC, MPNW)

!   This routine compares the MPR numbers A and B and returns in IC the value
!   -1, 0, or 1 depending on whether A < B, A = B, or A > B.
!   Note that the first and second words do NOT need to be the same for the
!   result to be "equal".

IMPLICIT NONE
INTEGER IC, MPNW
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+5)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPCPR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

S0(0) = MPNW + 6
CALL MPSUB (A, B, S0, MPNW)
IF (S0(2) < 0) THEN
  IC = -1
ELSEIF (S0(2) == 0) THEN
  IC = 0
ELSE
  IC = 1
ENDIF

RETURN
END SUBROUTINE MPCPR

SUBROUTINE MPDIV (A, B, C, MPNW)

!   This divides A by B and returns the result in C.

!   This subroutine employs the following Newton-Raphson iteration, which
!   converges to 1 / B:

!    X_{k+1} = X_k + (1 - X_k * B) * X_k

!   where the multiplication () * X_k is performed with only half of the
!   normal level of precision.  These iterations are performed with a
!   working precision level MPNW that is dynamically changed, approximately
!   doubling with each iteration (except that at iteration NIT before the
!   final iteration, the iteration is repeated without doubling the
!   precision, in order to enhance accuracy).  The final iteration is
!   performed as follows (this is due to A. Karp):

!    A / B = (A * X_n) + [A - (A * X_n) * B] * X_n  (approx.)

!   where the multiplications A * X_n and [] * X_n are performed with only
!   half of the final level of precision.

IMPLICIT NONE
INTEGER IQ, K, MPNW, MPNW1, MQ, N, NA, NB, &
  NIT, NW1, NW2
REAL (MPRKND) CL2, T1, T2
PARAMETER (CL2 = 1.4426950408889633D0, NIT = 3)
INTEGER (MPIKND) A(0:), B(0:), C(0:), S0(0:MPNW+6), &
  S1(0:MPNW+6), S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPDIV: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NA = MIN (INT (ABS (A(2))), MPNW)
NB = MIN (INT (ABS (B(2))), MPNW)

IF (NA == 0) THEN
  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
  GOTO 120
ENDIF
IF (NB == 0.D0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPDIV: Divisor is zero.')
  CALL MPABRT (33)
  RETURN
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7

!   Determine the least integer MQ such that 2 ^ MQ .GE. MPNW.

T1 = MPNW
MQ = CL2 * LOG (T1) + 1.D0 - MPRDFZ

!   Compute the initial approximation of 1 / B.

CALL MPMDC (B, T1, N, MPNW)
T2 = 1.D0 / T1
CALL MPDMC (T2, -N, S2, MPNW)
CALL MPDMC (1.D0, 0, S3, MPNW)
MPNW1 = 5
IQ = 0
NW1 = MPNW1
NW2 = MPNW1

!   Perform the Newton-Raphson iteration described above with a dynamically
!   changing precision level MPNW (one greater than powers of two).

DO K = 1, MQ - 1
  IF (K > 2) THEN
    NW1 = MPNW1
    MPNW1 = MIN (2 * MPNW1 - 2, MPNW) + 1
    NW2 = MPNW1
  ENDIF

100  CONTINUE

  CALL MPMUL (B, S2, S1, NW2)
  CALL MPSUB (S3, S1, S0, NW2)
  CALL MPMUL (S2, S0, S1, NW1)
  CALL MPADD (S2, S1, S0, NW2)
  CALL MPEQ (S0, S2, NW2)

  IF (K == MQ - NIT .AND. IQ == 0) THEN
    IQ = 1
    GOTO 100
  ENDIF
ENDDO

!   Perform last iteration using Karp's trick.

NW1 = MPNW1
MPNW1 = MIN (2 * MPNW1 - 1, MPNW) + 1
NW2 = MPNW1

CALL MPMUL (A, S2, S0, NW1)
CALL MPMUL (S0, B, S1, NW2)
CALL MPSUB (A, S1, S3, NW2)
CALL MPMUL (S3, S2, S1, NW1)
CALL MPADD (S0, S1, S2, NW2)

!   Restore original precision level.

CALL MPROUN (S2, MPNW)
CALL MPEQ (S2, C, MPNW)

120 CONTINUE
RETURN
END SUBROUTINE MPDIV

SUBROUTINE MPDIVD (A, B, C, MPNW)

!   This routine divides the MPR number A by the DP number B to yield C.

!   NOTE however that the product is not fully accurate unless B is an exact
!   binary value.
!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.

IMPLICIT NONE
INTEGER I, IA, IB, J, K, MPNW, NA, NBTH, N1
PARAMETER (NBTH = MPNBT / 2)
REAL (MPRKND) B, BB, BDH, BDVD, RDH
PARAMETER (BDH = 2.D0**NBTH, RDH = 0.5D0**NBTH)
INTEGER (MPIKND) A(0:), C(0:), CC(0:2*MPNW+10), D(0:2*MPNW+10), IBB, &
  B1, B2, C11, C12, C21, C22, D1, D2, TD, T1, T2, T3

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPDIVD: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check for zero inputs.

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
IB = SIGN (1.D0, B)
IF (NA == 0) THEN
  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
  GOTO 140
ELSEIF (B == 0.D0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPDIVD: Divisor is zero.')
  CALL MPABRT (31)
ELSEIF (B == 1.D0) THEN
  CALL MPEQ (A, C, MPNW)
  GOTO 140
ENDIF
BB = ABS (B)
N1 = 0

!   Reduce BB to within 1 and MPBDX.

IF (BB >= MPBDX) THEN
  DO K = 1, 100
    BB = BB / MPBDX
    IF (BB < MPBDX) THEN
      N1 = N1 + K
      GOTO 120
    ENDIF
  ENDDO
ELSEIF (BB < 1.D0) THEN
  DO K = 1, 100
    BB = MPBDX * BB
    IF (BB >= 1.D0) THEN
      N1 = N1 - K
      GOTO 120
    ENDIF
  ENDDO
ENDIF

120  CONTINUE

IBB = BB

!   If bb is not an integer, call mpdiv instead.

IF (BB /= IBB) THEN
  D(0) = MPNW + 6
  D(1) = MPNW
  CALL MPDMC (B, 0, D, MPNW)
  CALL MPDIV (A, D, C, MPNW)
  GOTO 140
ENDIF

CC(0) = 0
CC(1) = MPNW
CC(2) = SIGN (MPNW, IA * IB)
CC(3:2*MPNW+10) = 0

!   Split D array into half-word chunks.

D(0:3) = 0

DO I = 0, NA
  C11 = SHIFTA (A(I+4), NBTH)
  C12 = A(I+4) - SHIFTL (C11, NBTH)
  D(2*I+4) = C11
  D(2*I+5) = C12
ENDDO

D(2*NA+6:2*MPNW+10) = 0
B1 = SHIFTA (IBB, NBTH)
B2 = IBB - SHIFTL (B1, NBTH)

!   Perform short division algorithm, after splitting inputs.
!   Double precision is employed to find and refine the trial divisor.

DO J = 3, 2 * MPNW + 5
  BDVD = BDH * D(J) + D(J+1) + D(J+2) * RDH
  TD = FLOOR (BDVD / BB, MPIKND)
  T1 = B1 * TD
  C11 = SHIFTA (T1, NBTH)
  C12 = T1 - SHIFTL (C11, NBTH)
  T2 = B2 * TD
  C21 = SHIFTA (T2, NBTH)
  C22 = T2 - SHIFTL (C21, NBTH)
  D1 = C12 + C21 + SHIFTL (C11, NBTH)
  D2 = C22
  D(J) = D(J) - D1
  D(J+1) = D(J+1) - D2 + SHIFTL (D(J), NBTH)
  CC(J+1) = TD
ENDDO

!  Release carries on the full cc vector.

T1 = 0

DO I = 2 * MPNW + 5, 3, -1
  T3 = T1 + CC(I+1)
  T1 = SHIFTA (T3, NBTH)
  CC(I+1) = T3 - SHIFTL (T1, NBTH)
ENDDO

CC(3) = CC(3) + T1

!  Recombine half words into full words.

C(1) = MPNW
C(2) = SIGN (MPNW, IA * IB)
C(3) = CC(3)
C(4) = CC(4)

DO I = 0, MPNW + 1
  C(I+4) = SHIFTL (CC(2*I+4), NBTH) + CC(2*I+5)
ENDDO

!   If c(3) is nonzero, shift the result one cell right.

IF (C(3) /= 0) THEN
  N1 = N1 + 1
  C(2) = SIGN (ABS (C(2)) + 1, C(2))

  DO I = MPNW + 4, 3, -1
    C(I+1) = C(I)
  ENDDO
ENDIF

C(3) = A(3) - N1
CALL MPROUN (C, MPNW)

140 CONTINUE

RETURN
END SUBROUTINE MPDIVD

SUBROUTINE MPDIVD40 (A, B, C, MPNW)

!   This routine divides the MPR number A by the DP number B to yield C.
!   In contrast to mpdivd, this routine only allows 40 significant bits
!   (approximately 12 significant decimal digits) in B.  If more nonzero bits
!   are present in B (likely due to inexact binary value), an error is flagged.

!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.


IMPLICIT NONE
INTEGER MPNW
REAL (MPRKND) B, T2
INTEGER (MPIKND) A(0:), C(0:)
REAL (MPRKND) MPMASK13
EXTERNAL MPMASK13

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. C(0) < MPNW + 6) THEN
 WRITE (MPLDB, 1)
1 FORMAT ('*** MPDIVD40: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check whether B has more than 40 significant bits (actually whether the
!   trailing 13 bits are zero).

T2 = MPMASK13 (B)
IF (T2 == ABS (B)) THEN
  CALL MPDIVD (A, B, C, MPNW)
ELSE
  WRITE (MPLDB, 2) B
2 FORMAT ('*** MPDIVD40: DP value has more than 40 significant bits:', &
  1P,D25.15/'and thus very likely represents an unintended loss of accuracy.'/ &
  'Fix the issue, or else use functions mpprod, mpquot, mpreald or mpcmplxdc.'/ &
  'See documentation for details.')
  CALL MPABRT (81)
ENDIF

RETURN
END SUBROUTINE MPDIVD40

SUBROUTINE MPDMC (A, N, B, MPNW)

!   This routine converts the DP number A * 2^N to MPR form in B.

!   NOTE however that the product is not fully accurate unless A is an exact
!   binary value.
!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.

IMPLICIT NONE
INTEGER I, K, MPNW, N, N1, N2
REAL (MPRKND) A, AA
INTEGER (MPIKND) B(0:)

! End of declaration

IF (MPNW < 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPDMC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check for zero.

IF (A == 0.D0) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 150
ENDIF
N1 = N / MPNBT
N2 = N - MPNBT * N1
AA = ABS (A) * 2.D0 ** N2

!   Reduce AA to within 1 and MPBDX.

IF (AA >= MPBDX) THEN

  DO K = 1, 100
    AA = AA / MPBDX
    IF (AA < MPBDX) THEN
      N1 = N1 + K
      GOTO 120
    ENDIF
 ENDDO

ELSEIF (AA < 1.D0) THEN

  DO K = 1, 100
    AA = AA * MPBDX
    IF (AA >= 1.D0) THEN
      N1 = N1 - K
      GOTO 120
    ENDIF
  ENDDO

ENDIF

!   Store successive sections of AA into B.

120  CONTINUE

B(3) = N1
B(4) = INT (AA, MPIKND)
AA = MPBDX * (AA - B(4))
B(5) = INT (AA, MPIKND)
B(6) = 0
B(7) = 0
B(8) = 0

DO I = 6, 3, -1
  IF (B(I+1) /= 0) GOTO 140
ENDDO

140  CONTINUE

B(1) = MPNW
AA = I - 2
B(2) = SIGN (AA, A)

150 CONTINUE

RETURN
END SUBROUTINE MPDMC

SUBROUTINE MPDMC40 (A, N, B, MPNW)

!   This routine converts the DP number A * 2^N to MPR form in B.
!   In contrast to mpdmc, this routine only allows 40 significant bits
!   (approximately 12 significant decimal digits) in A.  If more nonzero bits
!   are present in A (likely due to inexact binary value), an error is flagged.

!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.


IMPLICIT NONE
INTEGER MPNW, N
REAL (MPRKND) A, T2
INTEGER (MPIKND) B(0:)
REAL (MPRKND) MPMASK13
EXTERNAL MPMASK13

! End of declaration

IF (MPNW < 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPDMC40: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check whether A has more than 40 significant bits (actually whether
!   the trailing 13 bits are zero).

T2 = MPMASK13 (A)
IF (T2 == ABS (A)) THEN
  CALL MPDMC (A, N, B, MPNW)
ELSE
  WRITE (MPLDB, 2) A
2 FORMAT ('*** MPDMC40: DP value has more than 40 significant bits:', &
  1P,D25.15/'and thus very likely represents an unintended loss of accuracy.'/ &
  'Fix the issue, or else use functions mpprod, mpquot, mpreald or mprealdm.'/ &
  'See documentation for details.')
  CALL MPABRT (82)
ENDIF

RETURN
END SUBROUTINE MPDMC40

SUBROUTINE MPEQ (A, B, MPNW)

!   Sets the MPR number B equal to the MPR number A.

IMPLICIT NONE
INTEGER I, IA, MPNW, NA
INTEGER (MPIKND) A(0:), B(0:)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPEQ: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)

IF (NA == 0)  THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 110
ENDIF
B(1) = MPNW
B(2) = SIGN (NA, IA)

DO I = 2, NA + 2
  B(I+1) = A(I+1)
ENDDO

B(NA+4) = 0
B(NA+5) = 0

110 CONTINUE

RETURN
END SUBROUTINE MPEQ

SUBROUTINE MPINFR (A, B, C, MPNW)

!   Sets B to the integer part of the MPR number A and sets C equal to the
!   fractional part of A.  Note this is NOT the quite same as the greatest
!   integer function as often defined in some mathematical books and papers.
!   Examples:  If A = 1.75, then B = 1., C = 0.75.
!     If A = -3.25, then B = -3., C = -0.25.

IMPLICIT NONE
INTEGER I, IA, MA, MPNW, NA, NB, NC
INTEGER (MPIKND) A(0:), B(0:), C(0:)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPINFR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check if  A  is zero.

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
MA = A(3)
IF (NA == 0)  THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
  GOTO 120
ENDIF

IF (MA >= MPNW - 1) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPINFR: Argument is too large.')
  CALL MPABRT (40)
ENDIF

!   Place integer part in  B.

NB = MIN (MAX (MA + 1, 0), NA)
IF (NB == 0) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
ELSE
  B(1) = MPNW
  B(2) = SIGN (NB, IA)
  B(3) = MA
  B(NB+4) = 0
  B(NB+5) = 0

  DO I = 3, NB + 2
    B(I+1) = A(I+1)
  ENDDO
ENDIF

!   Place fractional part in C.

NC = NA - NB
IF (NC <= 0) THEN
  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
ELSE
  C(1) = MPNW
  C(2) = SIGN (NC, IA)
  C(3) = MA - NB
  C(NC+4) = 0
  C(NC+5) = 0

  DO I = 3, NC + 2
    C(I+1) = A(I+NB+1)
  ENDDO
ENDIF

!   Fix up results.  B may have trailing zeros and C may have leading zeros.

CALL MPROUN (B, MPNW)
CALL MPROUN (C, MPNW)

120  CONTINUE
RETURN
END SUBROUTINE MPINFR

SUBROUTINE MPMDC (A, B, N, MPNW)

!   This returns a DP approximation the MPR number A in the form B * 2^n.

IMPLICIT NONE
INTEGER MPNW, N, NA
REAL (MPRKND) AA, B
INTEGER (MPIKND) A(0:)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPMDC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IF (A(2) == 0.D0)  THEN
  B = 0.D0
  N = 0
  GOTO 100
ENDIF

NA = ABS (A(2))
AA = A(4)
IF (NA >= 2) AA = AA + A(5) / DBLE (MPBDX)
N = MPNBT * A(3)
B = SIGN (AA, DBLE (A(2)))

!   Reduce b to within 1 and 2.

NA = LOG (ABS (B)) / LOG (2.D0) + MPRDFZ
B = B / 2.D0**NA
N = N + NA
IF (ABS (B) < 1.D0) THEN
  B = 2.D0 * B
  N = N - 1
ELSEIF (ABS (B) > 2.D0) THEN
  B = 0.5D0 * B
  N = N + 1
ENDIF

100  CONTINUE
RETURN
END SUBROUTINE MPMDC

SUBROUTINE MPMUL (A, B, C, MPNW)

!   This routine multiplies MPR numbers A and B to yield C.

!   This routine returns up to MPNW mantissa words of the product.  If the
!   complete double-long product of A and B is desired (for example in large
!   integer applications), then MPNW must be at least as large as the sum of
!   the mantissa lengths of A and B.  In other words, if the precision levels
!   of A and B are both 64 words, then MPNW must be at least 128 words to
!   produce the complete double-long product in C.

IMPLICIT NONE

INTEGER I, IA, IB, J, J3, MPNW, N2, NA, NB, NBTH, NC
PARAMETER (NBTH = MPNBT / 2)
INTEGER (MPIKND) A(0:), B(0:), C(0:), D(0:MPNW+6), A1, A2, B1, B2, C1, C2, C3, &
  DD, T1, T3

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPMUL: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
IB = SIGN (INT (1, MPIKND), B(2))
NA = MIN (INT (ABS (A(2))), MPNW)
NB = MIN (INT (ABS (B(2))), MPNW)
NC = MIN (NA + NB, MPNW)

IF (NA == 0 .OR. NB == 0) THEN

!   One of the inputs is zero -- result is zero.

  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
  GOTO 200
ENDIF

IF (NA == 1 .AND. A(4) == 1) THEN

!   A is 1 or -1 -- result is B or -B.

  C(1) = MPNW
  C(2) = SIGN (NB, IA * IB)
  C(3) = A(3) + B(3)

  DO I = 3, NB + 2
    C(I+1) = B(I+1)
  ENDDO

  C(NB+4) = 0
  C(NB+5) = 0
  GOTO 200
ELSEIF (NB == 1 .AND. B(4) == 1) THEN

!   B is 1 or -1 -- result is A or -A.

  C(1) = MPNW
  C(2) = SIGN (NA, IA * IB)
  C(3) = A(3) + B(3)

  DO I = 3, NA + 2
    C(I+1) = A(I+1)
  ENDDO

  C(NA+4) = 0
  C(NA+5) = 0
  GOTO 200
ENDIF

!   For very high precision, call mpmulx.

IF (NA > MPMLXM .AND. NB > MPMLXM) THEN
  CALL MPMULX (A, B, C, MPNW)
  GOTO 200
ENDIF

DD = A(3) + B(3)
D(0) = MPNW + 6
D(1) = MPNW
D(2) = SIGN (NC, IA * IB)

DO I = 2, NC + 5
  D(I+1) = 0
ENDDO

!   Perform ordinary long multiplication algorithm, after splitting inputs.
!   Accumulate at most MPNW+2 mantissa words of the product.

DO J = 3, NA + 2
  J3 = J - 3
  N2 = MIN (NB + 2, MPNW + 4 - J3)
  A1 = SHIFTA (A(J+1), NBTH)
  A2 = A(J+1) - SHIFTL (A1, NBTH)

  DO I = 3, N2
    B1 = SHIFTA (B(I+1), NBTH)
    B2 = B(I+1) - SHIFTL (B1, NBTH)
    C1 = A1 * B2 + A2 * B1
    C2 = SHIFTA (C1, NBTH)
    C3 = C1 - SHIFTL (C2, NBTH)
    D(I+J3) = D(I+J3) + A1 * B1 + C2
    D(I+J3+1) = D(I+J3+1) + A2 * B2 + SHIFTL (C3, NBTH)
  ENDDO

!  Release carries on the just-computed section of the d vector.

  T1 = 0

  DO I = N2, 3, -1
    T3 = T1 + D(I+J3+1)
    T1 = SHIFTA (T3, MPNBT)
    D(I+J3+1) = T3 - SHIFTL (T1, MPNBT)
  ENDDO

  D(J3+3) = D(J3+3) + T1
ENDDO

!  Release carries on the full d vector.

T1 = 0

DO I = NC + 1, 1, -1
  T3 = T1 + D(I+3)
  T1 = SHIFTA (T3, MPNBT)
  D(I+3) = T3 - SHIFTL (T1, MPNBT)
ENDDO

D(3) = D(3) + T1

!   If d(3) is nonzero, shift the result one cell right.

IF (D(3) /= 0) THEN
  DD = DD + 1
  NC = MIN (NC + 1, MPNW)
  D(2) = SIGN (NC, INT (D(2)))

  DO I = NC + 4, 3, -1
    D(I+1) = D(I)
  ENDDO
ENDIF

D(3) = DD

DO I = 1, NC + 5
  C(I) = D(I)
ENDDO

CALL MPROUN (C, MPNW)

200 CONTINUE

RETURN
END SUBROUTINE MPMUL

SUBROUTINE MPMULD (A, B, C, MPNW)

!   This routine multiplies the MPR number A by the DP number B to yield C.

!   NOTE however that the product is not fully accurate unless B is an exact
!   binary value.
!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.

IMPLICIT NONE
INTEGER I, IA, IB, J, K, MPNW, NA, NBTH, N1
PARAMETER (NBTH = MPNBT / 2)
REAL (MPRKND) B, BB
INTEGER (MPIKND) A(0:), C(0:), D(0:MPNW+6), IBB, A1, A2, B1, B2, C1, C2, C3, &
  T1, T3

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPMULD: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check for zero inputs.

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
IB = SIGN (1.D0, B)
IF (NA == 0 .OR. B == 0.D0) THEN
  C(1) = MPNW
  C(2) = 0
  C(3) = 0
  C(4) = 0
  C(5) = 0
  GOTO 140
ELSEIF (B == 1.D0) THEN
  CALL MPEQ (A, C, MPNW)
  GOTO 140
ENDIF
BB = ABS (B)
N1 = 0

!   Reduce BB to within 1 and MPBDX.

IF (BB >= MPBDX) THEN
  DO K = 1, 100
    BB = BB / MPBDX
    IF (BB < MPBDX) THEN
      N1 = N1 + K
      GOTO 120
    ENDIF
  ENDDO
ELSEIF (BB < 1.D0) THEN
  DO K = 1, 100
    BB = MPBDX * BB
    IF (BB >= 1.D0) THEN
      N1 = N1 - K
      GOTO 120
    ENDIF
  ENDDO
ENDIF

120  CONTINUE

IBB = BB

!   If bb is not an integer, call mpmul instead.

IF (BB /= IBB) THEN
  D(0) = MPNW + 6
  D(1) = MPNW
  CALL MPDMC (B, 0, D, MPNW)
  CALL MPMUL (A, D, C, MPNW)
  GOTO 140
ENDIF

D(0) = MPNW + 6
D(1) = MPNW
D(2) = SIGN (NA, IA * IB)

DO I = 2, NA + 5
  D(I+1) = 0
ENDDO

B1 = SHIFTA (IBB, NBTH)
B2 = IBB - SHIFTL (B1, NBTH)

!   Perform short multiplication algorithm, after splitting inputs.

DO J = 3, NA + 3
  A1 = SHIFTA (A(J+1), NBTH)
  A2 = A(J+1) - SHIFTL (A1, NBTH)
  C1 = A1 * B2 + A2 * B1
  C2 = SHIFTA (C1, NBTH)
  C3 = C1 - SHIFTL (C2, NBTH)
  D(J) = D(J) + A1 * B1 + C2
  D(J+1) = D(J+1) + A2 * B2 + SHIFTL (C3, NBTH)
ENDDO

!  Release carries on the full d vector.

T1 = 0

DO I = NA + 3, 3, -1
  T3 = T1 + D(I+1)
  T1 = SHIFTA (T3, MPNBT)
  D(I+1) = T3 - SHIFTL (T1, MPNBT)
ENDDO

D(3) = D(3) + T1

!   If d(3) is nonzero, shift the result one cell right.

IF (D(3) /= 0) THEN
  N1 = N1 + 1
  D(2) = SIGN (ABS (D(2)) + 1, D(2))

  DO I = NA + 4, 3, -1
    D(I+1) = D(I)
  ENDDO
ENDIF

D(3) = A(3) + N1

!   Copy d to c and round.

DO I = 1, NA + 5
  C(I) = D(I)
ENDDO

CALL MPROUN (C, MPNW)

140 CONTINUE

RETURN
END SUBROUTINE MPMULD

SUBROUTINE MPMULD40 (A, B, C, MPNW)

!   This routine multiples the MP number A by the DP number B to yield C.
!   In contrast to mpmuld, this routine only allows 40 significant bits
!   (approximately 12 significant decimal digits) in B.  If more nonzero bits
!   are present in B (likely due to inexact binary value), an error is flagged.

!   Examples of exact binary values (good): 123456789.d0, 0.25d0, -5.3125d0.
!   Examples of inexact binary values (bad): 0.1d0, 123467.8d0, -3333.3d0.

IMPLICIT NONE
INTEGER MPNW
REAL (MPRKND) B, T2
INTEGER (MPIKND) A(0:), C(0:)
REAL (MPRKND) MPMASK13
EXTERNAL MPMASK13

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. C(0) < MPNW + 6) THEN
 WRITE (MPLDB, 1)
1 FORMAT ('*** MPMULD40: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check whether B has more than 40 significant bits (actually whether
!   the trailing 13 bits are zero).

T2 = MPMASK13 (B)
IF (T2 == ABS (B)) THEN
  CALL MPMULD (A, B, C, MPNW)
ELSE
  WRITE (MPLDB, 2) B
2 FORMAT ('*** MPMULD40: DP value has more than 40 significant bits:', &
  1P,D25.15/'and thus very likely represents an unintended loss of accuracy.'/ &
  'Fix the issue, or else use functions mpprod, mpquot, mpreald or mpcmplxdc.'/ &
  'See documentation for details.')
  CALL MPABRT (83)
ENDIF

RETURN
END SUBROUTINE MPMULD40

SUBROUTINE MPNINT (A, B, MPNW)

!   This sets B to the nearest integer to the MPR number A.
!   Examples:  If A = 1.49, B = 1.; if A = 3.5, B = 4; if A = -2.5, B = -3.

IMPLICIT NONE
INTEGER IA, MA, NA, MPNW
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+5), S1(0:MPNW+5)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPNINT: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
MA = A(3)
IF (NA == 0)  THEN

!   A is zero -- result is zero.

  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 110
ENDIF

IF (MA >= MPNW) THEN

!   A cannot be represented exactly as an integer.

  WRITE (MPLDB, 2)
2 FORMAT ('*** MPNINT: Argument is too large.')
  CALL MPABRT (56)
ENDIF

!   Add or subtract 1/2 from the input, depending on its sign, then
!   return the greatest integer.

S0(0) = MPNW + 6
S1(0) = MPNW + 6

CALL MPDMC (0.5D0, 0, S0, MPNW)
IF (IA == 1) THEN
  CALL MPADD (A, S0, S1, MPNW)
ELSE
  CALL MPSUB (A, S0, S1, MPNW)
ENDIF
CALL MPINFR (S1, B, S0, MPNW)

110 CONTINUE
RETURN
END SUBROUTINE MPNINT

SUBROUTINE MPNORM (D, A, MPNW)

!   This converts the MP number in array D to the standard normalized form
!   in A.

!   MPNORM assumes that two extra mantissa words are input at the end of D.
!   This reduces precision loss when it is necessary to shift the result to
!   the left. All words up to index A(2) + 5 in A *must* have data, even if 0.

IMPLICIT NONE
INTEGER I, IA, MPNW, NA, N4
INTEGER (MPIKND) A2
INTEGER (MPIKND) A(0:)
INTEGER (MPIKND) D(0:), T1, T3

! End of declaration

IF (MPNW < 4 .OR. D(0) < ABS (D(2)) + 4 .OR. A(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPNORM: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), D(2))
NA = MIN (INT (ABS (D(2))), MPNW)
IF (NA == 0)  THEN
  A(1) = MPNW
  A(2) = 0
  A(3) = 0
  A(4) = 0
  A(5) = 0
  GOTO 170
ENDIF
N4 = NA + 4
A2 = D(3)
D(3) = 0

110 CONTINUE

T1 = 0

DO I = N4, 3, -1
  T3 = T1 + D(I+1)
  T1 = SHIFTA (T3, MPNBT)
  D(I+1) = T3 - SHIFTL (T1, MPNBT)
ENDDO

D(3) = D(3) + T1

IF (D(3) < 0) THEN

!   D(3) is negative -- negate all words and re-normalize.

  IA = - IA
  D(4) = D(4) + MPBDX * D(3)
  D(3) = 0

  DO I = 2, N4
    D(I+1) = - D(I+1)
  ENDDO

  GOTO 110
ELSEIF (D(3) > 0) THEN

!   The fixup loops above "spilled" a nonzero number into D(3).  Shift the
!   entire number right one cell.  The exponent and length of the result
!   are increased by one.

  DO I = N4, 3, -1
    A(I+1) = D(I)
  ENDDO

  NA = MIN (NA + 1, MPNW)
  A2 = A2 + 1
ELSE
  DO I = 3, N4
    A(I+1) = D(I+1)
  ENDDO
ENDIF

!   Perform rounding and truncation.

A(1) = MPNW
A(2) = SIGN (NA, IA)
A(3) = A2

CALL MPROUN (A, MPNW)

170 CONTINUE

RETURN
END SUBROUTINE MPNORM

SUBROUTINE MPNPWR (A, N, B, MPNW)

!   This computes the N-th power of the MPR number A and returns the result
!   in B.  When N is zero, 1 is returned.  When N is negative, the reciprocal
!   of A ^ |N| is returned.

IMPLICIT NONE
INTEGER J, KK, KN, MN, MPNW, MPNW1, N, NA, NN
REAL (MPRKND) CL2, T1
PARAMETER (CL2 = 1.4426950408889633D0)
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+6), S1(0:MPNW+6), &
  S2(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPNPWR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NA = MIN (INT (ABS (A(2))), MPNW)

IF (NA == 0) THEN
  IF (N >= 0) THEN
      B(1) = MPNW
    B(2) = 0
    B(3) = 0
    B(4) = 0
    B(5) = 0
    GOTO 120
  ELSE
    WRITE (MPLDB, 2)
2   FORMAT ('*** MPNPWR: Argument is zero and N is negative or zero.')
    CALL MPABRT (57)
  ENDIF
ENDIF

MPNW1 = MPNW + 1
S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7

NN = ABS (N)
IF (NN == 0) THEN
  CALL MPDMC (1.D0, 0, B, MPNW)
  GOTO 120
ELSEIF (NN == 1) THEN
  CALL MPEQ (A, S2, MPNW1)
  GOTO 110
ELSEIF (NN == 2) THEN
  CALL MPMUL (A, A, S2, MPNW1)
  GOTO 110
ENDIF

!   Determine the least integer MN such that 2 ^ MN .GT. NN.

T1 = NN
MN = CL2 * LOG (T1) + 1.D0 + MPRDFZ
CALL MPDMC (1.D0, 0, S2, MPNW1)
CALL MPEQ (A, S0, MPNW1)
KN = NN

!   Compute B ^ N using the binary rule for exponentiation.

DO J = 1, MN
  KK = KN / 2
  IF (KN /= 2 * KK) THEN
    CALL MPMUL (S2, S0, S1, MPNW1)
    CALL MPEQ (S1, S2, MPNW1)
  ENDIF
  KN = KK
  IF (J < MN) THEN
    CALL MPMUL (S0, S0, S1, MPNW1)
    CALL MPEQ (S1, S0, MPNW1)
  ENDIF
ENDDO

!   Compute reciprocal if N is negative.

110 CONTINUE

IF (N < 0) THEN
  CALL MPDMC (1.D0, 0, S1, MPNW1)
  CALL MPDIV (S1, S2, S0, MPNW1)
  CALL MPEQ (S0, S2, MPNW1)
ENDIF

!   Restore original precision level.

CALL MPROUN (S2, MPNW)
CALL MPEQ (S2, B, MPNW)

120 CONTINUE

RETURN
END SUBROUTINE MPNPWR

SUBROUTINE MPNRTR (A, N, B, MPNW)

!   This computes the N-th root of the MPR number A and returns result inB.
!   N must be at least one and must not exceed 2 ^ 30.

!   This subroutine employs the following Newton-Raphson iteration, which
!   converges to A ^ (-1/N):

!    X_{k+1} = X_k + (X_k / N) * (1 - A * X_k^N)

!   The reciprocal of the final approximation to A ^ (-1/N) is the N-th root.
!   These iterations are performed with a maximum precision level MPNW that
!   is dynamically changed, approximately doubling with each iteration.

!   When N is large and A is very near one, the following binomial series is
!   employed instead of the Newton scheme:

!   (1 + x)^(1/N)  =  1  +  x / N  +  x^2 * (1 - N) / (2! N^2)  +  ...

!   See the comment about the parameter NIT in MPDIVX.

IMPLICIT NONE
INTEGER IA, IQ, K, MPNW, MPNW1, MQ, N, NA, NIT, &
  N1, N2, N3, N30
REAL (MPRKND) ALT, CL2, T1, T2, TN
PARAMETER (ALT = 0.693147180559945309D0, CL2 = 1.4426950408889633D0, &
  NIT = 3, N30 = 2 ** 30)
INTEGER (MPIKND) A(0:), B(0:), F1(0:8), S0(0:MPNW+7), &
  S1(0:MPNW+7), S2(0:MPNW+7), S3(0:MPNW+7)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPNRTR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)

IF (NA == 0) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 140
ENDIF
IF (IA < 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPNRTR: Argument is negative.')
  CALL MPABRT (59)
ENDIF

IF (N <= 0 .OR. N > N30) THEN
  WRITE (MPLDB, 3) N
3 FORMAT ('*** MPNRTR: Improper value of N',I10)
  CALL MPABRT (60)
ENDIF

!   If N = 1 or 2, call MPEQ or MPSQRT instead.

IF (N == 1) THEN
  CALL MPEQ (A, B, MPNW)
  GOTO 140
ELSEIF (N == 2) THEN
  CALL MPSQRT (A, B, MPNW)
  GOTO 140
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Set f1 = 1.

F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

!   Determine the least integer MQ such that 2 ^ MQ .GE. MPNW.

T1 = MPNW
MQ = CL2 * LOG (T1) + 1.D0 - MPRDFZ

!   Check how close A is to 1.

CALL MPSUB (A, F1, S0, MPNW1)
IF (S0(2) == 0) THEN
  CALL MPEQ (F1, B, MPNW)
  GOTO 140
ENDIF
CALL MPMDC (S0, T1, N1, MPNW1)
N2 = CL2 * LOG (ABS (T1))
T1 = T1 * 0.5D0 ** N2
N1 = N1 + N2

IF (N1 <= -30) THEN
  T2 = N
  N2 = CL2 * LOG (T2) + 1.D0 + MPRDFZ
  N3 = - MPNBT * MPNW1 / N1
  IF (N3 < 1.25D0 * N2) THEN

!   A is so close to 1 that it is cheaper to use the binomial series.

    CALL MPDIVD (S0, T2, S1, MPNW1)
    CALL MPADD (F1, S1, S2, MPNW1)
    K = 0

100 CONTINUE

    K = K + 1
    T1 = 1 - K * N
    T2 = (K + 1) * N
    CALL MPMULD (S1, T1, S3, MPNW1)
    CALL MPDIVD (S3, T2, S1, MPNW1)
    CALL MPMUL (S0, S1, S3, MPNW1)
    CALL MPEQ (S3, S1, MPNW1)
    CALL MPADD (S1, S2, S3, MPNW1)
    CALL MPEQ (S3, S2, MPNW1)
    IF (S1(2) /= 0 .AND. S1(3) >= - MPNW1) THEN
          GOTO 100
    ELSE
      CALL MPEQ (S2, S1, MPNW1)
      GOTO 130
    ENDIF
  ENDIF
ENDIF

!   Compute the initial approximation of A ^ (-1/N).

TN = N
CALL MPMDC (A, T1, N1, MPNW1)
N2 = - N1 / TN
T2 = EXP (-1.D0 / TN * (LOG (T1) + (N1 + TN * N2) * ALT))
CALL MPDMC (T2, N2, S2, MPNW1)
MPNW1 = 5
IQ = 0

!   Perform the Newton-Raphson iteration described above with a dynamically
!   changing precision level MPNW (one greater than powers of two).

DO K = 1, MQ
  IF (K > 2) MPNW1 = MIN (2 * MPNW1 - 2, MPNW) + 1
!  if (k > 2) mpnw1 = min (2 * mpnw1 - 1, mpnw)

110  CONTINUE

  CALL MPNPWR (S2, N, S0, MPNW1)
  CALL MPMUL (A, S0, S1, MPNW1)
  CALL MPSUB (F1, S1, S0, MPNW1)
  CALL MPMUL (S2, S0, S1, MPNW1)
  CALL MPDIVD (S1, TN, S0, MPNW1)
  CALL MPADD (S2, S0, S1, MPNW1)
  CALL MPEQ (S1, S2, MPNW1)
  IF (K == MQ - NIT .AND. IQ == 0) THEN
    IQ = 1
    GOTO 110
  ENDIF
ENDDO

!   Take the reciprocal to give final result.

CALL MPDIV (F1, S2, S1, MPNW1)

!   Restore original precision level.

130 CONTINUE

CALL MPROUN (S1, MPNW)
CALL MPEQ (S1, B, MPNW)

140 CONTINUE
RETURN
END SUBROUTINE MPNRTR

SUBROUTINE MPOUTW (IU, ANAM, A, MPNW)

!   This outputs the words of A up to the end of the active mantissa.
!   This is for internal debugging only; it should not be called by user.

IMPLICIT NONE
INTEGER I, IU, MPNW, NA
CHARACTER(*) ANAM
INTEGER (MPIKND) A(0:)

! End of declaration

NA = MIN (INT (ABS (A(2))), MPNW)
WRITE (IU, '(a)') ANAM
WRITE (IU, '(i20,",",i20,",",i20,",",i20,",")') (A(I), I = 0, NA + 5)
RETURN
END SUBROUTINE MPOUTW

SUBROUTINE MPRANDR (A, B, MPNW)

!   This returns a pseudorandom number B based the input A.
!   The result is overwritten into A, thus facilitating iterated calls.

IMPLICIT NONE
INTEGER MPNW
INTEGER (MPIKND) A(0:), B(0:), IA1(0:MPNW+6), IA2(0:MPNW+6), IA3(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPRAND: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA1(0) = MPNW + 7
IA2(0) = MPNW + 7
IA3(0) = MPNW + 7

CALL MPMULD (A, MPRANDX, IA1, MPNW + 1)
CALL MPINFR (IA1, IA2, IA3, MPNW + 1)
CALL MPEQ (IA3, B, MPNW)
CALL MPEQ (IA3, A, MPNW)
RETURN
END

SUBROUTINE MPROUN (A, MPNW)

!   This performs rounding and truncation of the MPR number A.  It is called
!   by MPNORM, and also by other subroutines when the precision level is
!   modified.  It is not intended to be directly called by the user.
!   The parameter MPEXPMX is the absolute value of the largest exponent word
!   allowed for MP numbers (see system parameters at start of this module).

IMPLICIT NONE
INTEGER I, IA, K, MPNW, NA, N4
INTEGER (MPIKND) A(0:), A2

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. A(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPROUN: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check for initial zeroes.

A2 = A(3)
A(3) = 0
IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
N4 = NA + 4

IF (A(4) == 0) THEN

!   Find the first nonzero word and shift the entire number left.  The length
!   of the result is reduced by the length of the shift.

  DO I = 4, N4
    IF (A(I+1) /= 0) GOTO 110
  ENDDO

  A(2) = 0
  A(3) = 0
  A(4) = 0
  A(5) = 0
  GOTO 170

110 CONTINUE

  K = I - 3

  DO I = 3, N4 - K
    A(I+1) = A(I+K+1)
  ENDDO

  A2 = A2 - K
  NA = NA - MAX (K - 2, 0)
  IF (K == 2) A(NA+4) = 0
ENDIF

!   Perform rounding.

IF (NA == MPNW) THEN
  IF (A(NA+4) >= 0.5D0 * MPBDX) A(NA+3) = A(NA+3) + 1

!   Release carries as far as necessary due to rounding.

  DO I = NA + 2, 3, -1
    IF (A(I+1) < MPBDX) GOTO 140
    A(I+1) = A(I+1) - MPBDX
    A(I) = A(I) + 1
  ENDDO

!   Release of carries due to rounding continued all the way to the start --
!   i.e. number was entirely 9's.

  A(4) = A(3)
  NA = 1
  A2 = A2 + 1
ENDIF

140 CONTINUE

  IF (A(NA+3) == 0) THEN

!   At least the last mantissa word is zero.  Find the last nonzero word
!   and adjust the length of the result accordingly.

  DO I = NA + 2, 3, -1
    IF (A(I+1) /= 0) GOTO 160
  ENDDO

  A(1) = MPNW
  A(2) = 0
  A(3) = 0
  A(4) = 0
  A(5) = 0
  GOTO 170

160  CONTINUE

  NA = I - 2
ENDIF

!   Check for overflow and underflow.

IF (A2 < - MPEXPMX) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPROUN: Exponent underflow.')
  CALL MPABRT (68)
ELSEIF (A2 > MPEXPMX) THEN
  WRITE (MPLDB, 3)
3 FORMAT ('*** MPROUN: Exponent overflow.')
  CALL MPABRT (69)
ENDIF

!   Check for zero.

IF (A(4) == 0) THEN
  A(1) = MPNW
  A(2) = 0
  A(3) = 0
  A(4) = 0
  A(5) = 0
ELSE
  A(1) = MPNW
  A(2) = SIGN (NA, IA)
  A(3) = A2
  A(NA+4) = 0
  A(NA+5) = 0
ENDIF

170  CONTINUE

RETURN
END SUBROUTINE MPROUN

SUBROUTINE MPSQRT (A, B, MPNW)

!   This computes the square root of the MPR number A and returns the result in B.

!   This subroutine employs the following Newton-Raphson iteration, which
!   converges to 1 / Sqrt(A):

!    X_{k+1} = X_k + 0.5 * (1 - X_k^2 * A) * X_k

!   where the multiplication () * X_k is performed with only half of the
!   normal level of precision.  These iterations are performed with a
!   working precision level MPNW that is dynamically changed, approximately
!   doubling with each iteration (except that at iteration NIT before the final
!   iteration, the iteration is repeated without doubling the precision, in order
!   to enhance accuracy) .  The final iteration is performed as follows
!   (this is due to A. Karp):

!    Sqrt(A) = (A * X_n) + 0.5 * [A - (A * X_n)^2] * X_n  (approx.)

!   where the multiplications A * X_n and [] * X_n are performed with only
!   half of the final level of precision.

IMPLICIT NONE
INTEGER IA, IQ, K, MPNW, MPNW1, MQ, N, NA, NIT, NW1, NW2, N2
REAL (MPRKND) CL2, T1, T2
PARAMETER (CL2 = 1.4426950408889633D0, NIT = 3)
INTEGER (MPIKND) A(0:), B(0:), S0(0:MPNW+6), S1(0:MPNW+6), &
  S2(0:MPNW+6), S3(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPSQRT: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)

IF (NA == 0) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 120
ENDIF
IF (IA < 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPSQRT: Argument is negative.')
  CALL MPABRT (70)
  RETURN
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
S3(0) = MPNW + 7

!   Determine the least integer MQ such that 2 ^ MQ .GE. MPNW.

T1 = MPNW
MQ = CL2 * LOG (T1) + 1.D0 - MPRDFZ

!   Compute the initial approximation of 1 / Sqrt(A).

CALL MPMDC (A, T1, N, MPNW)
N2 = - N / 2
T2 = SQRT (T1 * 2.D0 ** (N + 2 * N2))
T1 = 1.D0 / T2
CALL MPDMC (T1, N2, S2, MPNW)
CALL MPDMC (1.D0, 0, S3, MPNW)

MPNW1 = 5
IQ = 0
NW1 = MPNW1
NW2 = MPNW1

!   Perform the Newton-Raphson iteration described above with a dynamically
!   changing precision level MPNW (one greater than powers of two).

DO K = 1, MQ - 1
  IF (K > 2) THEN
    NW1 = MPNW1
    MPNW1 = MIN (2 * MPNW1 - 2, MPNW) + 1
    NW2 = MPNW1
  ENDIF

100  CONTINUE

  CALL MPMUL (S2, S2, S0, NW2)
  CALL MPMUL (A, S0, S1, NW2)
  CALL MPSUB (S3, S1, S0, NW2)
  CALL MPMUL (S2, S0, S1, NW1)
  CALL MPMULD (S1, 0.5D0, S0, NW1)
  CALL MPADD (S2, S0, S1, NW2)
  CALL MPEQ (S1, S2, NW2)

  IF (K == MQ - NIT .AND. IQ == 0) THEN
    IQ = 1
    GOTO 100
  ENDIF
ENDDO

!   Perform last iteration using Karp's trick.

NW1 = MPNW1
MPNW1 = MIN (2 * MPNW1 - 2, MPNW) + 1
NW2 = MPNW1

CALL MPMUL (A, S2, S0, NW1)
CALL MPMUL (S0, S0, S1, NW2)
CALL MPSUB (A, S1, S3, NW2)
CALL MPMUL (S3, S2, S1, NW1)
CALL MPMULD (S1, 0.5D0, S3, NW1)
CALL MPADD (S0, S3, S2, NW2)

!   Restore original precision level.

CALL MPROUN (S2, MPNW)
CALL MPEQ (S2, B, MPNW)

120 CONTINUE

RETURN
END SUBROUTINE MPSQRT

SUBROUTINE MPSUB (A, B, C, MPNW)

!   This routine subtracts MPR numbers A and B to yield C.

IMPLICIT NONE
INTEGER I, NB, MPNW
INTEGER (MPIKND) A(0:), B(0:), C(0:), S(0:MPNW+5)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPSUB: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NB = MIN (ABS (INT (B(2))), MPNW)
S(0) = MPNW + 6
S(1) = MPNW
IF (B(2) == 0) THEN
  S(2) = 0
ELSEIF (B(2) > 0) THEN
  S(2) = - NB
ELSE
  S(2) = NB
ENDIF

DO I = 3, NB + 5
  S(I) = B(I)
ENDDO

CALL MPADD (A, S, C, MPNW)

RETURN
END SUBROUTINE MPSUB

!   These three subroutines are for real(16) (quad) support:

SUBROUTINE MPMQC (A, B, N, MPNW)

!   This returns a quad precision approximation the MPR number A in the form B * 2^n.
!   If IEEE quad floating (128-bit) is not supported on the processor, an error
!   message is output.

IMPLICIT NONE
INTEGER KND, MPNW, N, NA
PARAMETER (KND = MAX (MPRKND2, KIND(1.0)))
REAL (KND) AA, B
INTEGER (MPIKND) A(0:)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPMQC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IF (MPRKND2 < 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPMQC: IEEE quad precision is not supported on this processor')
  CALL MPABRT (92)
ENDIF

IF (A(2) == 0.D0)  THEN
  B = 0.D0
  N = 0
  GOTO 100
ENDIF

NA = ABS (A(2))
AA = A(4)
IF (NA >= 2) AA = AA + A(5) / REAL (MPBDX, KND)
IF (NA >= 3) AA = AA + A(6) / REAL (MPBDX, KND)**2

N = MPNBT * A(3)
B = SIGN (AA, REAL (A(2), KND))

!   Reduce b to within 1 and 2.

NA = LOG (ABS (B)) / LOG (2.D0) + MPRDFZ
B = B / 2.D0**NA
N = N + NA
IF (ABS (B) < 1.D0) THEN
  B = 2.D0 * B
  N = N - 1
ELSEIF (ABS (B) > 2.D0) THEN
  B = 0.5D0 * B
  N = N + 1
ENDIF

100  CONTINUE
RETURN
END SUBROUTINE MPMQC

SUBROUTINE MPQMC (A, N, B, MPNW)

!   This routine converts the quad precision number A * 2^N to MPR form in B.
!   If IEEE quad floating (128-bit) is not supported on the processor, an error
!   message is output.

!   NOTE however that the conversion is not fully accurate unless A is an exact
!   binary value.
!   Examples of exact binary values (good): 123456789.q0, 0.25q0, -5.3125q0.
!   Examples of inexact binary values (bad): 0.1q0, 123467.8q0, -3333.3q0.

IMPLICIT NONE
INTEGER I, K, KND, MPNW, N, N1, N2
PARAMETER (KND = MAX (MPRKND2, KIND (1.0)))
REAL (KND) A, AA
INTEGER (MPIKND) B(0:*)

! End of declaration

IF (MPNW < 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPQMC: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IF (MPRKND2 < 0) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPQMC: IEEE quad precision is not supported on this processor')
  CALL MPABRT (92)
ENDIF

!   Check for zero.

IF (A == 0.D0) THEN
  B(1) = MPNW
  B(2) = 0
  B(3) = 0
  B(4) = 0
  B(5) = 0
  GOTO 150
ENDIF
N1 = N / MPNBT
N2 = N - MPNBT * N1
AA = ABS (A) * 2.D0 ** N2

!   Reduce AA to within 1 and MPBDX.

IF (AA >= MPBDX) THEN

  DO K = 1, 350
    AA = AA / MPBDX
    IF (AA < MPBDX) THEN
      N1 = N1 + K
      GOTO 120
    ENDIF
 ENDDO

ELSEIF (AA < 1.D0) THEN

  DO K = 1, 350
    AA = AA * MPBDX
    IF (AA >= 1.D0) THEN
      N1 = N1 - K
      GOTO 120
    ENDIF
  ENDDO

ENDIF

!   Store successive sections of AA into B.

120  CONTINUE

B(3) = N1
B(4) = INT (AA, MPIKND)
AA = MPBDX * (AA - B(3+1))
B(5) = INT (AA, MPIKND)
AA = MPBDX * (AA - B(4+1))
B(6) = INT (AA, MPIKND)
B(7) = 0
B(8) = 0

DO I = 7, 3, -1
  IF (B(I+1) /= 0) GOTO 140
ENDDO

140  CONTINUE

B(1) = MPNW
AA = I - 2
B(2) = SIGN (AA, A)

150 CONTINUE
RETURN
END SUBROUTINE MPQMC

SUBROUTINE MPQMC90 (A, N, B, MPNW)

!   This routine converts the DP number A * 2^N to MPR form in B.
!   In contrast to mpqmc, this routine only allows 90 significant bits
!   (approximately 27 significant decimal digits) in A.  If more nonzero bits
!   are present in A (likely due to inexact binary value), an error is flagged.

!   Examples of exact binary values (good): 123456789.q0, 0.25q0, -5.3125q0.
!   Examples of inexact binary values (bad): 0.1q0, 123467.8q0, -3333.3q0.


IMPLICIT NONE
INTEGER KND, MPNW, N
PARAMETER (KND = MAX (MPRKND2, KIND (1.0)))
REAL (KND) A, T2
INTEGER (MPIKND) B(0:)
REAL (KND) MPMASK23
EXTERNAL MPMASK23

! End of declaration

IF (MPNW < 4 .OR. B(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPQMC40: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check whether A has more than 90 significant bits (actually whether
!   the trailing 23 bits are zero).

T2 = MPMASK23 (A)
IF (T2 == ABS (A)) THEN
  CALL MPQMC (A, N, B, MPNW)
ELSE
  WRITE (MPLDB, 2) A
2 FORMAT ('*** MPQMC40: QP value has more than 90 significant bits:'/ &
  1P,D50.35/'and thus very likely represents an unintended loss of accuracy.'/ &
  'Fix the issue, or else use functions mpprod, mpquot, mprealq or mprealqm.'/ &
  'See documentation for details.')
  CALL MPABRT (82)
ENDIF

RETURN
END SUBROUTINE MPQMC90

! ***  The following are the extra-high precision multiply routines:

SUBROUTINE MPFFTCR (IS, M, N, NSQ, X, Y)

!   This performs an N-point complex-to-real FFT, where N = 2^M.  X is the
!   double complex input array, and Y is the double precision output array.
!   The array X is used as a scratch array in MPFFT1, and so is overwritten.
!   X and Y must be dimensioned as shown below.  IS is the sign of the FFT.
!   The arrays MPUU1 and MPUU2 must have been initialized by calling MPINIFFT.
!   This routine is not intended to be called directly by the user.

IMPLICIT NONE
INTEGER IS, K, KU, M, MX, N, NSQ, N1, N2, N4
REAL (MPRKND) Y(N)
COMPLEX (MPRKND) DC1(N/2), X(N/2+NSQ*MPNSP1+1), AI, A1, A2, X1, X2

MX = MPUU1(1)

!   Check if input parameters are invalid.

IF ((IS .NE. 1 .AND. IS .NE. -1) .OR. M .LT. 3 .OR. M .GT. MX) THEN
  WRITE (MPLDB, 1)  IS, M, MX
1 FORMAT ('*** MPFFTCR: Either the UU arrays have not been initialized'/ &
  'or else one of the input parameters is invalid', 3I5)
  CALL MPABRT (677)
ENDIF

N1 = 2 ** (M / 2)
N2 = N / 2
N4 = N / 4
AI = CMPLX (0.D0, 1.D0, MPRKND)

!   Construct the input to MPFFT1.

DC1(1) = 0.5D0 * CMPLX (REAL (X(1) + X(N2+1), MPRKND), &
  REAL (X(1) - X(N2+1), MPRKND), MPRKND)
IF (IS .EQ. 1) THEN
  DC1(N4+1) = CONJG (X(N4+1))
ELSE
  DC1(N4+1) = X(N4+1)
ENDIF
KU = N2

IF (IS .EQ. 1) THEN
  DO K = 2, N4
    X1 = X(K)
    X2 = CONJG (X(N2+2-K))
    A1 = X1 + X2
    A2 = AI * MPUU1(K+KU) * (X1 - X2)
    DC1(K) = 0.5D0 * (A1 + A2)
    DC1(N2+2-K) = 0.5D0 * CONJG (A1 - A2)
  ENDDO
ELSE
  DO K = 2, N4
    X1 = X(K)
    X2 = CONJG (X(N2+2-K))
    A1 = X1 + X2
    A2 = AI * CONJG (MPUU1(K+KU)) * (X1 - X2)
    DC1(K) = 0.5D0 * (A1 + A2)
    DC1(N2+2-K) = 0.5D0 * CONJG (A1 - A2)
  ENDDO
ENDIF

!   Perform a normal N/2-point FFT on DC1.

CALL MPFFT1 (IS, M - 1, N1, N2 / N1, DC1, X)

!   Copy DC1 to Y such that DC1(k) = Y(2k-1) + i Y(2k).

DO K = 1, N / 2
  Y(2*K-1) = REAL (DC1(K), MPRKND)
  Y(2*K) = AIMAG (DC1(K))
ENDDO

RETURN
END SUBROUTINE MPFFTCR

SUBROUTINE MPFFTRC (IS, M, N, NSQ, X, Y)

!   This performs an N-point real-to-complex FFT, where N = 2^M.  X is the
!   dobule precision input array, and Y is the double complex output array.
!   The arrays MPUU1 and MPUU2 must have been initialized by calling MPINIFFT.
!   This routine is not intended to be called directly by the user.

IMPLICIT NONE
INTEGER IS, K, KU, M, MX, N, NSQ, N1, N2, N4
REAL (MPRKND) X(N)
COMPLEX (MPRKND) DC1(N/2), Y(N/2+NSQ*MPNSP1+1), AI, A1, A2, Z1, Z2

MX = MPUU1(1)

!   Check if input parameters are invalid.

IF ((IS .NE. 1 .AND. IS .NE. -1) .OR. M .LT. 3 .OR. M .GT. MX) THEN
  WRITE (MPLDB, 1)  IS, M, MX
1 FORMAT ('*** MPFFTRC: either the UU arrays have not been initialized'/ &
  'or else one of the input parameters is invalid',3I5)
  CALL MPABRT (677)
ENDIF

N1 = 2 ** (M / 2)
N2 = N / 2
N4 = N / 4
AI = CMPLX (0.D0, -1.D0, MPRKND)

!   Copy X to DC1 such that DC1(k) = X(2k-1) + i X(2k).

DO K = 1, N2
  DC1(K) = CMPLX (X(2*K-1), X(2*K), MPRKND)
ENDDO

!   Perform a normal N/2-point FFT on DC1.

CALL MPFFT1 (IS, M - 1, N1, N2 / N1, DC1, Y)

!   Reconstruct the FFT of X.

Y(1) = CMPLX (2.D0 * (REAL (DC1(1), MPRKND) + AIMAG (DC1(1))), &
  0.D0, MPRKND)
IF (IS .EQ. 1) THEN
  Y(N4+1) = 2.D0 * DC1(N4+1)
ELSE
  Y(N4+1) = 2.D0 * CONJG (DC1(N4+1))
ENDIF
Y(N2+1) = CMPLX (2.D0 * (REAL (DC1(1), MPRKND) - AIMAG (DC1(1))), &
  0.D0, MPRKND)
KU = N2

IF (IS .EQ. 1) THEN
  DO K = 2, N4
    Z1 = DC1(K)
    Z2 = CONJG (DC1(N2+2-K))
    A1 = Z1 + Z2
    A2 = AI * MPUU1(K+KU) * (Z1 - Z2)
    Y(K) = A1 + A2
    Y(N2+2-K) = CONJG (A1 - A2)
  ENDDO
ELSE
  DO K = 2, N4
    Z1 = DC1(K)
    Z2 = CONJG (DC1(N2+2-K))
    A1 = Z1 + Z2
    A2 = AI * CONJG (MPUU1(K+KU)) * (Z1 - Z2)
    Y(K) = A1 + A2
    Y(N2+2-K) = CONJG (A1 - A2)
  ENDDO
ENDIF

RETURN
END SUBROUTINE MPFFTRC

SUBROUTINE MPFFT1 (IS, M, N1, N2, X, Y)

!   This routine performs a complex-to-complex FFT.  IS is the sign of the
!   transform, N = 2^M is the size of the transform.  N1 = 2^M1 and N2 = 2^M2,
!   where M1 and M2 are defined as below.  X is the input and output array,
!   and Y is a scratch array.  X must have at N, and Y at least N + N1*MPNSP1,
!   double complex cells.  The arrays MPUU1 and MPUU2 must have been
!   initialized by calling MPINIFFT.  This routine is not intended to be called
!   directly by the user.

!   This employs the two-pass variant of the "four-step" FFT.  See the
!   article by David H. Bailey in J. of Supercomputing, March 1990, p. 23-35.

IMPLICIT NONE
INTEGER I, IS, IU, J, J2, K, KU, M, M1, M2, N1, N2, NR1, NR2
COMPLEX (MPRKND) X(N1,N2), Y(N2+MPNSP1,N1), Z1(MPNROW+MPNSP1,N1), &
  Z2(MPNROW+MPNSP1,N1)

M1 = (M + 1) / 2
M2 = M - M1
NR1 = MIN (N1, MPNROW)
NR2 = MIN (N2, MPNROW)
KU = MPUU2(M)

DO I = 0, N1 - 1, NR1

!   Copy NR1 rows of X (treated as a N1 x N2 complex array) into Z1.

  DO J = 1, N2
    DO K = 1, NR1
      Z1(K,J) = X(I+K,J)
    ENDDO
  ENDDO

!   Perform NR1 FFTs, each of length N2.

  CALL MPFFT2 (IS, NR1, M2, N2, Z1, Z2)

!   Multiply the resulting NR1 x N2 complex block by roots of unity and
!   store transposed into the appropriate section of Y.

  IU = I + KU - N1 - 1
  IF (IS .EQ. 1) THEN
    DO J = 1, N2
      DO K = 1, NR1
        Y(J,I+K) = MPUU2(IU+K+J*N1) * Z1(K,J)
      ENDDO
    ENDDO
  ELSE
    DO J = 1, N2
      DO K = 1, NR1
        Y(J,I+K) = CONJG (MPUU2(IU+K+J*N1)) * Z1(K,J)
      ENDDO
    ENDDO
  ENDIF
ENDDO

DO I = 0, N2 - 1, NR2

!   Copy NR2 rows of the Y array into Z2.

  DO J = 1, N1
    DO K = 1, NR2
      Z2(K,J) = Y(I+K,J)
    ENDDO
  ENDDO

!   Perform NR2 FFTs, each of length N1.

  CALL MPFFT2 (IS, NR2, M1, N1, Z2, Z1)

!   Copy NR2 x N1 complex block back into X array.  It's a little more
!   complicated if M is odd.

  IF (MOD (M, 2) .EQ. 0) THEN
    DO J = 1, N1
      DO K = 1, NR2
        X(I+K,J) = Z2(K,J)
      ENDDO
    ENDDO
  ELSE
    DO J = 1, N1 / 2
      J2 = 2 * J - 1

      DO K = 1, NR2
        X(I+K,J) = Z2(K,J2)
        X(I+K+N2,J) = Z2(K,J2+1)
      ENDDO
    ENDDO
  ENDIF
ENDDO

RETURN
END SUBROUTINE MPFFT1

SUBROUTINE MPFFT2 (IS, NS, M, N, X, Y)

!   This performs NS simultaneous N-point complex-to-complex FFTs, where
!   N = 2^M.  X is the input and output array, and Y is a scratch array.
!   The arrays MPUU1 and MPUU2 must have been initialized by calling MPINIFFT.
!   This routine is not intended to be called directly by the user.

IMPLICIT NONE
INTEGER I, IS, J, L, M, N, NS
COMPLEX (MPRKND) X(MPNROW+MPNSP1,N), Y(MPNROW+MPNSP1,N)

!   Perform the second variant of the Stockham FFT.

DO L = 1, M, 2
  CALL MPFFT3 (IS, L, NS, M, N, X, Y)
  IF (L .EQ. M) GOTO 100
  CALL MPFFT3 (IS, L + 1, NS, M, N, Y, X)
ENDDO

GOTO 110

!   Copy Y to X.

100 CONTINUE

DO J = 1, N
  DO I = 1, NS
    X(I,J) = Y(I,J)
  ENDDO
ENDDO

110 CONTINUE

RETURN
END SUBROUTINE MPFFT2

SUBROUTINE MPFFT3 (IS, L, NS, M, N, X, Y)

!   This performs the L-th iteration of the second variant of the Stockham FFT
!   on the NS vectors in X.  X is input/output, and Y is a scratch array.
!   The arrays MPUU1 and MPUU2 must have been initialized by calling MPINIFFT.
!   This routine is not intended to be called directly by the user.

IMPLICIT NONE
INTEGER I, IS, I11, I12, I21, I22, J, K, L, LI, LJ, LK, KU, M, N, N1, NS
COMPLEX (MPRKND) X(MPNROW+MPNSP1,N), Y(MPNROW+MPNSP1,N), U1, X1, X2

!   Set initial parameters.

N1 = N / 2
LK = 2 ** (L - 1)
LI = 2 ** (M - L)
LJ = 2 * LK
KU = LI + 1

DO I = 0, LI - 1
  I11 = I * LK + 1
  I12 = I11 + N1
  I21 = I * LJ + 1
  I22 = I21 + LK
  IF (IS .EQ. 1) THEN
    U1 = MPUU1(I+KU)
  ELSE
    U1 = CONJG (MPUU1(I+KU))
  ENDIF

  DO K = 0, LK - 1
    DO J = 1, NS
      X1 = X(J,I11+K)
      X2 = X(J,I12+K)
      Y(J,I21+K) = X1 + X2
      Y(J,I22+K) = U1 * (X1 - X2)
    ENDDO
  ENDDO
ENDDO

RETURN
END SUBROUTINE MPFFT3

SUBROUTINE MPINIFFT (MPNW)

!   This computes the root of unity arrays UU1 and UU2, which are required by
!   the FFT routines, and places this data in the proper arrays defined in
!   module MPFUNA.  MPNW is the largest precision level (in words) that will be
!   subsequently used for this run.

IMPLICIT NONE
INTEGER I, IU, J, K, KU, LN, M, MM, MM1, MM2, MQ, NN, NN1, NN2, NQ, MPNW, NWDS
REAL (MPRKND) CL2, D1
PARAMETER (CL2 = 1.4426950408889633D0)
REAL (MPRKND) PI, T1, TI, TPN

!  Determine sizes for FFT arrays.  Three words are added to mpnw, since many
!  routines in MPFUND in particular increase the working precision upon entry.

NWDS = MPNW + 3
D1 = 2.D0 * (NWDS + 1)
M = CL2 * LOG (D1) + 1.D0 - MPRDFZ
MQ = M + 2
NQ = 2 ** MQ

IF (MQ + NQ > MPLFFTX) THEN
  WRITE (6, 1) MQ + NQ
1 FORMAT ('*** MPINIFFT: Insufficient space for arrays mpuu1 and mpuu2.'/ &
  'At least',I12,' double complex cells must be allocated for each of'/ &
  'these arrays in module mpfuna. See documentation for details.')
  CALL MPABRT (91)
ENDIF

MPUU1(1) = MQ
KU = 2
LN = 1
PI = ACOS (-1.D0)

DO J = 1, MQ
  T1 = PI / LN

  DO I = 0, LN - 1
    TI = I * T1
    MPUU1(I+KU) = CMPLX (COS (TI), SIN (TI), MPRKND)
  ENDDO

  KU = KU + LN
  LN = 2 * LN
ENDDO

! write (6, 2) ku - 1
! 2 format ('MPINIFFT: Size of table mpuu1 =',i10)

KU = MQ + 1
MPUU2(1) = MQ

DO K = 2, MQ
  MPUU2(K) = CMPLX (0.D0, 0.D0, MPRKND)
ENDDO

DO K = 2, MQ - 1
  MPUU2(K) = KU
  MM = K
  NN = 2 ** MM
  MM1 = (MM + 1) / 2
  MM2 = MM - MM1
  NN1 = 2 ** MM1
  NN2 = 2 ** MM2
  TPN = 2.D0 * PI / NN

  DO J = 0, NN2 - 1
    DO I = 0, NN1 - 1
      IU = KU + I + J * NN1
      T1 = TPN * I * J
      MPUU2(IU) = CMPLX (COS (T1), SIN (T1), MPRKND)
    ENDDO
  ENDDO

  KU = KU + NN
ENDDO

! write (6, 3) ku - 1
! 3 format ('MPINIFFT: Size of table mpuu2 =',i10)

RETURN
END SUBROUTINE MPINIFFT

SUBROUTINE MPLCONV (IQ, N, NSQ, A, B, C)

!   This computes the linear convolution of A and B, returning the result
!   in C.  If IQ is 1, then it is presumed B = A; if IQ = 2, then A /= B.
!   NSQ is a spacing parameter, which should be set to more than sqrt (3*n).

IMPLICIT NONE
INTEGER I, IQ, M1, M2, N, N1, N2, N4, NM, NSQ
REAL (MPRKND) CL2, C0, FFTERRMX
PARAMETER (CL2 = 1.4426950408889633D0, FFTERRMX = 0.375D0)
REAL (MPRKND) A(N), AN, B(N), C(2*N), D1(8*N+2), D2(8*N+2), D3(8*N+2), T1, T2
COMPLEX (MPRKND) DC1(4*N+NSQ*MPNSP1+3), DC2(4*N+NSQ*MPNSP1+3)

T1 = N
M1 = CL2 * LOG (T1) + 1.D0 - MPRDFZ
N1 = 2 ** M1
M2 = M1 + 1
N2 = 2 * N1
N4 = 2 * N2
NM = MIN (2 * N, N2)

IF (ABS (IQ) .EQ. 1) THEN

!   Compute the square of A -- only one forward FFT is needed.

  DO I = 1, N
    D1(I) = A(I)
  ENDDO

  DO I = N + 1, N2
    D1(I) = 0.D0
  ENDDO

!   Perform a forward real-to-complex FFT on the vector in A.

  CALL MPFFTRC (1, M2, N2, NSQ, D1, DC1)

!   Square the resulting complex vector.

  DO I = 1, N1 + 1
    DC1(I) = DC1(I) ** 2
  ENDDO
ELSE

!   Compute the product of A and B -- two forward FFTs are needed.

  DO I = 1, N
    D1(I) = A(I)
    D2(I) = B(I)
  ENDDO

  DO I = N + 1, N2
    D1(I) = 0.D0
    D2(I) = 0.D0
  ENDDO

!   Perform forward real-to-complex FFTs on the vectors in A and B.

  CALL MPFFTRC (1, M2, N2, NSQ, D1, DC1)
  CALL MPFFTRC (1, M2, N2, NSQ, D2, DC2)

!   Multiply the resulting complex vectors.

  DO I = 1, N1 + 1
    DC1(I) = DC1(I) * DC2(I)
  ENDDO
ENDIF

!   Perform an inverse complex-to-real FFT on the resulting data.

CALL MPFFTCR (-1, M2, N2, NSQ, DC1, D3)

!   Divide by N4 and round to nearest whole number.

AN = 1.D0 / N4
C0 = 0.D0

DO I = 1, NM
  T1 = AN * D3(I)
  T2 = ANINT (T1)
  C(I) = T2
  C0 = MAX (C0, ABS (T2 - T1))
ENDDO

IF (C0 > FFTERRMX) THEN
  WRITE (6, 1) C0
1 FORMAT ('*** MPLCONV: excessive rounding error =',F12.6)
  CALL MPABRT (55)
ENDIF

RETURN
END SUBROUTINE MPLCONV

SUBROUTINE MPMULX (A, B, C, MPNW)

!   This routine multiplies MP numbers A and B to yield the MP product C,
!   using a FFT-convolution technique.  Before calling MPMULX, the arrays
!   UU1 and UU2 must be initialized by calling MPINIFFT.  For modest levels
!   of precision, use MPMUL.

IMPLICIT NONE
INTEGER I, IA, IB, MPNW, NA, NB, NC, NN, NX
INTEGER (MPIKND) A(0:), B(0:), C(0:), D(0:MPNW+8), I0, I1, I2
REAL (MPRKND) D1(0:4*MPNW+20), D2(0:4*MPNW+20), D3(0:8*MPNW+40)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. B(0) < ABS (B(2)) + 4 .OR. &
  C(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPMULX: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
IB = SIGN (INT (1, MPIKND), B(2))
NA = MIN (INT (ABS (A(2))), MPNW)
NB = MIN (INT (ABS (B(2))), MPNW)
NC = MIN (NA + NB, MPNW)
NN = 4 * MAX (NA, NB)
NX = SQRT (4.D0 * NN) + MPRDFZ

!   Divide each word of A into four 15-bit chunks.

DO I = 0, NA - 1
  I1 = A(I+4)
  I2 = SHIFTA (I1, 45)
  D1(4*I) = I2
  I1 = I1 - SHIFTL (I2, 45)
  I2 = SHIFTA (I1, 30)
  D1(4*I+1) = I2
  I1 = I1 - SHIFTL (I2, 30)
  I2 = SHIFTA (I1, 15)
  D1(4*I+2) = I2
  I1 = I1 - SHIFTL (I2, 15)
  D1(4*I+3) = I1
ENDDO

DO I = 4 * NA, NN - 1
  D1(I) = 0.D0
ENDDO

!   Divide each word of B into four 15-bit chunks.

DO I = 0, NB - 1
  I1 = B(I+4)
  I2 = SHIFTA (I1, 45)
  D2(4*I) = I2
  I1 = I1 - SHIFTL (I2, 45)
  I2 = SHIFTA (I1, 30)
  D2(4*I+1) = I2
  I1 = I1 - SHIFTL (I2, 30)
  I2 = SHIFTA (I1, 15)
  D2(4*I+2) = I2
  I1 = I1 - SHIFTL (I2, 15)
  D2(4*I+3) = I1
ENDDO

DO I = 4 * NB, NN - 1
  D2(I) = 0.D0
ENDDO

!   Perform linear convolution.

  CALL MPLCONV (2, NN, NX, D1, D2, D3)

!   Release carries.

I0 = 0

DO I = MIN (4 * NC + 16, 2 * NN - 1), 0, -1
  I0 = I0 + D3(I)
  I1 = SHIFTA (I0, 15)
  I2 = I0 - SHIFTL (I1, 15)
  D1(I) = I2
  I0 = I1
ENDDO

!  Recombine words, with proper offset.

D(0) = 0
D(1) = 0
D(2) = 0
D(3) = 0
D(4) = SHIFTL (I0, 45) + SHIFTL (INT (D1(0), MPIKND), 30) &
  + SHIFTL (INT (D1(1), MPIKND), 15) + INT (D1(2), MPIKND)

DO I = 1, NC + 3
  D(I+4) = SHIFTL (INT (D1(4*I-1), MPIKND), 45) + SHIFTL (INT (D1(4*I), MPIKND), 30) &
    + SHIFTL (INT (D1(4*I+1), MPIKND), 15) + INT (D1(4*I+2), MPIKND)
ENDDO

D(0) = MPNW + 6
D(1) = MPNW
D(2) = SIGN (NC, IA * IB)
D(3) = A(3) + B(3) + 1

!   Fix up the result.

D1(0) = MPNW + 6
CALL MPNORM (D, C, MPNW)

190 CONTINUE

RETURN
END SUBROUTINE MPMULX

END MODULE ModLib_MPFUNB

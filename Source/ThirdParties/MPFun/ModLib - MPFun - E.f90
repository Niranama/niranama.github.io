!*****************************************************************************

!  MPFUN20-Fort: A thread-safe arbitrary precision computation package
!  Special functions module (module MPFUNE)

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

!  DESCRIPTION OF THIS MODULE (MPFUNE):
!    This module contains subroutines to perform special functions. Additional
!    functions will be added as they are completed.

MODULE ModLib_MPFUNE
USE ModLib_MPFUNA
USE ModLib_MPFUNB
USE ModLib_MPFUNC
USE ModLib_MPFUND

CONTAINS

SUBROUTINE MPBERNER (NB1, NB2, BERNE, MPNW)

!  This returns the even Bernouli numbers B(2*k), from B(2) = 1/6 up to
!  B(2*nb2).  The array berne must be dimensioned as shown below.

IMPLICIT NONE
INTEGER I, IA, K, NA, NB1, NB2, MPNW, MPNW1
INTEGER (MPIKND) BERNE(0:NB1+5,NB2), T1(0:MPNW+6), T2(0:MPNW+6), &
  T3(0:MPNW+6), T4(0:MPNW+6), T5(0:MPNW+6)

!  End of declaration

IF (MPNW < 4 .OR. BERNE(0,1) < MPNW + 4 .OR. BERNE(0,NB2) < MPNW + 4) &
  THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPBERN: uninitialized or inadequately sized arrays')
  CALL MPABRT (62)
ENDIF

MPNW1 = MPNW + 1
T1(0) = MPNW + 7
T2(0) = MPNW + 7
T3(0) = MPNW + 7
T4(0) = MPNW + 7
T5(0) = MPNW + 7
CALL MPMULD (MPPICON, 2.D0, T1, MPNW1)
CALL MPMUL (T1, T1, T2, MPNW1)
CALL MPDMC (-2.D0, 0, T1, MPNW1)

DO K = 1, NB2
  CALL MPMULD (T1, DBLE (2*K - 1), T3, MPNW1)
  CALL MPMULD (T3, DBLE (2*K), T4, MPNW1)
  CALL MPDIV (T4, T2, T1, MPNW1)
  T1(2) = - T1(2)
  CALL MPDMC (2.D0 * DBLE (K), 0, T3, MPNW1)
  CALL MPZETAR (T3, T4, MPNW1)
  CALL MPMUL (T1, T4, T5, MPNW1)
  CALL MPROUN (T5, MPNW)

!   The next few lines (to !+) are necessary, rather than a simple call to
!   mpeq, to avoid a Fortran rank-mismatch error.

!  call mpeq (t5, berne(0,k), mpnw)

  IA = SIGN (INT (1, MPIKND), T5(2))
  NA = MIN (INT (ABS (T5(2))), MPNW)
  BERNE(1,K) = MPNW
  BERNE(2,K) = SIGN (NA, IA)

  DO I = 2, NA + 2
    BERNE(I+1,K) = T5(I+1)
  ENDDO

  BERNE(NA+4,K) = 0
  BERNE(NA+5,K) = 0
!+
ENDDO

RETURN
END SUBROUTINE MPBERNER

SUBROUTINE MPBESSELJR (ANU, T, Z, MPNW)

!   This evaluates the function BesselJ (ANU, T).  ANU must be nonnegative and
!   not greater than 10^6 (this limit can be adjusted below).  To compensate
!   for an unsually large amount of internal cancelation in these formulas, all
!   computations are performed to 3*mpnw/2 words precision.

!   In the parameter statement below:
!     itrmx = limit of number of iterations in series; default = 100000.
!     dasy = factor used to decide if asymptic series is used; default = 25.
!     anumx = upper limit of anu argument; default = 1000.

IMPLICIT NONE
INTEGER I, ITRMX, MPNW, MPNW1
REAL (MPRKND) ANUMX, DASY
INTEGER (MPIKND) ANU(0:), T(0:), Z(0:), &
  T0(0:3*MPNW/2+5), T1(0:3*MPNW/2+5), T2(0:3*MPNW/2+5), T3(0:3*MPNW/2+5), &
  T4(0:3*MPNW/2+5), T5(0:3*MPNW/2+5), T6(0:3*MPNW/2+5)
PARAMETER (ITRMX = 100000, ANUMX = 1.D6, DASY = 25.D0)

! End of declaration

IF (MPNW < 4 .OR. ANU(0) < MPNW + 4 .OR. ANU(0) < ABS (ANU(2)) + 4 .OR. &
  T(0) < MPNW + 4 .OR. T(0) < ABS (T(2)) + 4 .OR. Z(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPBESSELJR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IF (ANU(2) < 0 .OR. ANU(3) > 0 .OR. &
  (ANU(3) == 0 .AND. ANU(4) > ANUMX)) THEN
  WRITE (6, 2) ANUMX
2 FORMAT ('*** MPBESSELJR: First argument must be >= 0 and <=',F10.0)
  CALL MPABRT (65)
ENDIF

MPNW1 = 3 * MPNW / 2
T0(0) = MPNW1 + 6
T1(0) = MPNW1 + 6
T2(0) = MPNW1 + 6
T3(0) = MPNW1 + 6
T4(0) = MPNW1 + 6
T5(0) = MPNW1 + 6
T6(0) = MPNW1 + 6

!   Select either the direct or the asymptotic series.

IF (T(3) < 0 .OR. T(3) == 0 .AND. T(4) < DASY * (MPNW - 2)) THEN
  T2(1) = MPNW1
  T2(2) = 1
  T2(3) = 0
  T2(4) = 1
  T2(5) = 0
  T2(6) = 0
  CALL MPADD (ANU, T2, T0, MPNW1)
  CALL MPGAMMAR (T0, T1, MPNW1)
  CALL MPDIV (T2, T1, T3, MPNW1)
  CALL MPEQ (T3, T1, MPNW1)
  CALL MPEQ (T1, T0, MPNW1)
  CALL MPMUL (T, T, T3, MPNW1)
  CALL MPMULD (T3, 0.25D0, T2, MPNW1)

  DO I = 1, ITRMX
    CALL MPMUL (T1, T2, T3, MPNW1)
    CALL MPDIVD (T3, DBLE (I), T4, MPNW1)
    CALL MPDMC (DBLE (I), 0, T5, MPNW1)
    CALL MPADD (ANU, T5, T6, MPNW1)
    CALL MPDIV (T4, T6, T1, MPNW1)
    T1(2) = - T1(2)
    CALL MPADD (T0, T1, T3, MPNW1)
    CALL MPEQ (T3, T0, MPNW1)
    IF (T1(2) == 0 .OR. T1(3) < T0(3) - MPNW1) GOTO 100
  ENDDO

  WRITE (6, 3)
3 FORMAT ('*** MPBESSELJR: loop overflow 1')
  CALL MPABRT (66)

100 CONTINUE

  CALL MPMULD (T, 0.5D0, T1, MPNW1)
  CALL MPPOWER (T1, ANU, T2, MPNW1)
  CALL MPMUL (T0, T2, T3, MPNW1)
  CALL MPEQ (T3, T0, MPNW1)
ELSE
  T0(1) = MPNW1
  T0(2) = 1
  T0(3) = 0
  T0(4) = 1
  T0(5) = 0
  T0(6) = 0
  T1(1) = MPNW1
  T1(2) = 0
  T1(3) = 0
  T1(4) = 0
  T2(1) = MPNW1
  T2(2) = 1
  T2(3) = 0
  T2(4) = 1
  T2(5) = 0
  T2(6) = 0
  CALL MPMUL (ANU, ANU, T3, MPNW1)
  CALL MPMULD (T3, 4.D0, T5, MPNW1)

  DO I = 1, ITRMX
    CALL MPDMC (DBLE (2*I - 1), 0, T4, MPNW1)
    CALL MPMUL (T4, T4, T6, MPNW1)
    CALL MPSUB (T5, T6, T4, MPNW1)
    CALL MPMUL (T2, T4, T3, MPNW1)
    CALL MPDIVD (T3, 8.D0 * DBLE (I), T4, MPNW1)
    CALL MPDIV (T4, T, T2, MPNW1)
    IF (MOD (I, 2) == 0) THEN
      CALL MPEQ (T2, T3, MPNW1)
      IF (MOD (I, 4) == 2)  T3(2) = - T3(2)
      CALL MPADD (T0, T3, T4, MPNW1)
      CALL MPEQ (T4, T0, MPNW1)
    ELSE
      CALL MPEQ (T2, T3, MPNW1)
      IF (MOD (I, 4) == 3) T3(2) = - T3(2)
      CALL MPADD (T1, T3, T4, MPNW1)
      CALL MPEQ (T4, T1, MPNW1)
    ENDIF
    IF (T2(2) == 0 .OR. (T2(3) < T0(3) - MPNW1 .AND. T2(3) < T1(3) - MPNW1)) &
      GOTO 110
  ENDDO

  WRITE (6, 4)
4 FORMAT ('*** MPBESSELJR: loop overflow 2')
  CALL MPABRT (66)

110 CONTINUE

  CALL MPEQ (MPPICON, T2, MPNW1)
  CALL MPMUL (T2, ANU, T4, MPNW1)
  CALL MPMULD (T4, 0.5D0, T3, MPNW1)
  CALL MPSUB (T, T3, T4, MPNW1)
  CALL MPMULD (T2, 0.25D0, T3, MPNW1)
  CALL MPSUB (T4, T3, T5, MPNW1)
  CALL MPCSSNR (T5, T3, T4, MPNW1)
  CALL MPMUL (T3, T0, T5, MPNW1)
  CALL MPMUL (T4, T1, T6, MPNW1)
  CALL MPSUB (T5, T6, T3, MPNW1)
  T4(1) = MPNW1
  T4(2) = 1
  T4(3) = 0
  T4(4) = 2
  T4(5) = 0
  T4(6) = 0
  CALL MPMUL (T2, T, T5, MPNW1)
  CALL MPDIV (T4, T5, T6, MPNW1)
  CALL MPSQRT (T6, T4, MPNW1)
  CALL MPMUL (T4, T3, T0, MPNW1)
ENDIF

CALL MPROUN (T0, MPNW)
CALL MPEQ (T0, Z, MPNW)

RETURN
END SUBROUTINE MPBESSELJR

SUBROUTINE MPERFR (Z, TERF, MPNW)

!   This evaluates the erf function, using a combination of two series.
!   In particular, the algorithm is (where B = (mpnw + 1) * mpnbt, and
!   dcon is a constant defined below):

!   if (t == 0) then
!     erf = 0
!   elseif (z > sqrt(B*log(2))) then
!     erf = 1
!   elseif (z < -sqrt(B*log(2))) then
!     erf = -1
!   elseif (abs(z) < B/dcon + 8) then
!     erf = 2 / (sqrt(pi)*exp(z^2)) * Sum_{k>=0} 2^k * z^(2*k+1)
!             / (1.3....(2*k+1))
!   else
!     erf = 1 - 1 / (sqrt(pi)*exp(z^2))
!             * Sum_{k>=0} (-1)^k * (1.3...(2*k-1)) / (2^k * z^(2*k+1))
!   endif

IMPLICIT NONE
INTEGER IC1, IC2, IC3, ITRMX, K, MPNW, MPNW1, NBT
REAL (MPRKND) DCON, D1, D2
PARAMETER (DCON = 100.D0, ITRMX = 100000)
INTEGER (MPIKND) EPS(0:MPNW+6), TERF(0:), &
  T1(0:MPNW+6), T2(0:MPNW+6), T3(0:MPNW+6), T4(0:MPNW+6), &
  T5(0:MPNW+6), T6(0:MPNW+6), T7(0:MPNW+6), Z(0:), Z2(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. Z(0) < MPNW + 4 .OR. Z(0) < ABS (TERF(2)) + 4 .OR. &
  TERF(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPERFR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
EPS(0) = MPNW1 + 7
T1(0) = MPNW1 + 7
T2(0) = MPNW1 + 7
T3(0) = MPNW1 + 7
T4(0) = MPNW1 + 7
T5(0) = MPNW1 + 7
T6(0) = MPNW1 + 7
T7(0) = MPNW1 + 7
Z2(0) = MPNW1 + 7

NBT = MPNW * MPNBT
D1 = AINT (1.D0 + SQRT (NBT * LOG (2.D0)))
D2 = AINT (NBT / DCON + 8.D0)
CALL MPDMC (D1, 0, T1, MPNW1)
CALL MPDMC (D2, 0, T2, MPNW1)
CALL MPCPR (Z, T1, IC1, MPNW1)
T1(2) = - T1(2)
CALL MPCPR (Z, T1, IC2, MPNW1)
CALL MPCPR (Z, T2, IC3, MPNW1)

!if (z == 0.d0) then
IF (Z(2) == 0) THEN

!  terf = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, TERF, MPNW)

!elseif (z > d2) then
ELSEIF (IC1 > 0) THEN

!  terf = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, TERF, MPNW)

!elseif (z < -d2) then
ELSEIF (IC2 < 0) THEN

!  terf = mpreal (-1.d0, nwds)
  CALL MPDMC (-1.D0, 0, TERF, MPNW)

!elseif (abs (z) < d3) then
ELSEIF (IC3 < 0) THEN

!  eps = mpreal (2.d0, nwds) ** (-nbt)
  CALL MPDMC (1.D0, -NBT, EPS, MPNW1)

!  z2 = z**2
  CALL MPMUL (Z, Z, Z2, MPNW1)

!  t1 = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, T1, MPNW1)

!  t2 = z
  CALL MPEQ (Z, T2, MPNW1)

!  t3 = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, T3, MPNW1)

!  t5 = mpreal (1.d10, 4)
  CALL MPDMC (1.D10, 0, T5, 4)

  DO K = 0, ITRMX
    IF (K > 0) THEN
!      t2 = 2.d0 * z2 * t2
      CALL MPMULD (Z2, 2.D0, T6, MPNW1)
      CALL MPMUL (T6, T2, T7, MPNW1)
      CALL MPEQ (T7, T2, MPNW1)

!      t3 = (2.d0 * k + 1.d0) * t3
      CALL MPMULD (T3, 2.D0 * K + 1.D0, T6, MPNW1)
      CALL MPEQ (T6, T3, MPNW1)
    ENDIF

!    t4 = t2 / t3
    CALL MPDIV (T2, T3, T4, MPNW1)

!    t1 = t1 + t4
    CALL MPADD (T1, T4, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)

!    t6 = abs (mpreal (t4, 4) / mpreal (t1, 4))
    CALL MPDIV (T4, T1, T6, 4)

!    if (t6 < eps .or. t6 >= t5) goto 120
    CALL MPCPR (T6, EPS, IC1, 4)
    CALL MPCPR (T6, T5, IC2, 4)
    IF (IC1 <= 0 .OR. IC2 >= 0) GOTO 120

!    t5 = t6
    CALL MPEQ (T6, T5, 4)
  ENDDO

WRITE (6, 3) 1, ITRMX
3 FORMAT ('*** MPERFR: iteration limit exceeded',2I10)
CALL MPABRT (101)

120 CONTINUE

!  terf = 2.d0 * t1 / (sqrt (mppi (nwds)) * exp (z2))
  CALL MPMULD (T1, 2.D0, T3, MPNW1)
  CALL MPSQRT (MPPICON, T4, MPNW1)
  CALL MPEXP (Z2, T5, MPNW1)
  CALL MPMUL (T4, T5, T6, MPNW1)
  CALL MPDIV (T3, T6, T7, MPNW1)
  CALL MPEQ (T7, TERF, MPNW)
ELSE

!  eps = mpreal (2.d0, nwds) ** (-nbt)
  CALL MPDMC (1.D0, -NBT, EPS, MPNW1)

!  z2 = z ** 2
  CALL MPMUL (Z, Z, Z2, MPNW1)

!  t1 = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, T1, MPNW1)

!  t2 = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, T2, MPNW1)

!  t3 = abs (z)
  CALL MPEQ (Z, T3, MPNW1)
  T3(2) = ABS (T3(2))

!  t5 = mpreal (1.d10, 4)
  CALL MPDMC (1.D10, 0, T5, 4)

  DO K = 0, ITRMX
    IF (K > 0) THEN
!      t2 = - (2.d0 * k - 1.d0) * t2 / 2.d0
      CALL MPMULD (T2, -(2.D0 * K - 1.D0), T6, MPNW1)
      CALL MPEQ (T6, T2, MPNW1)

!      t3 = z2 * t3
      CALL MPMUL (T2, T3, T6, MPNW1)
      CALL MPEQ (T6, T3, MPNW1)
    ENDIF

!    t4 = t2 / t3
    CALL MPDIV (T2, T3, T4, MPNW1)

!    t1 = t1 + t4
    CALL MPADD (T1, T4, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)

!    t6 = abs (mpreal (t4, 4) / mpreal (t1, 4))
    CALL MPDIV (T4, T1, T6, 4)

!    if (t6 < eps .or. t6 >= t5) goto 130
    CALL MPCPR (T6, EPS, IC1, 4)
    CALL MPCPR (T6, T5, IC2, 4)
    IF (IC1 <= 0 .OR. IC2 >= 0) GOTO 130

!    t5 = t6
    CALL MPEQ (T6, T5, 4)
  ENDDO

WRITE (6, 3) 2, ITRMX
CALL MPABRT (101)

130 CONTINUE

!  terf = 1.d0 - t1 / (sqrt (mppi (nwds)) * exp (z2))
  CALL MPDMC (1.D0, 0, T2, MPNW1)
  CALL MPSQRT (MPPICON, T3, MPNW1)
  CALL MPEXP (Z2, T4, MPNW1)
  CALL MPMUL (T3, T4, T5, MPNW1)
  CALL MPDIV (T1, T5, T6, MPNW1)
  CALL MPSUB (T2, T6, T7, MPNW1)
  CALL MPEQ (T7, TERF, MPNW)

!  if (z < 0.d0) terf = - terf
  IF (Z(2) < 0) TERF(2) = - TERF(2)
ENDIF

RETURN
END SUBROUTINE MPERFR

SUBROUTINE MPERFCR (Z, TERFC, MPNW)

!   This evaluates the erfc function, using a combination of two series.
!   In particular, the algorithm is (where B = (mpnw + 1) * mpnbt, and
!   dcon is a constant defined below):

!   if (t == 0) then
!     erfc = 1
!   elseif (z > sqrt(B*log(2))) then
!     erfc = 0
!   elseif (z < -sqrt(B*log(2))) then
!     erfc = 2
!   elseif (abs(z) < B/dcon + 8) then
!     erfc = 1 - 2 / (sqrt(pi)*exp(z^2)) * Sum_{k>=0} 2^k * z^(2*k+1)
!               / (1.3....(2*k+1))
!   else
!     erfc = 1 / (sqrt(pi)*exp(z^2))
!             * Sum_{k>=0} (-1)^k * (1.3...(2*k-1)) / (2^k * z^(2*k+1))
!   endif

IMPLICIT NONE
INTEGER IC1, IC2, IC3, ITRMX, K, MPNW, MPNW1, NBT
REAL (MPRKND) DCON, D1, D2
PARAMETER (DCON = 100.D0, ITRMX = 100000)
INTEGER (MPIKND) EPS(0:MPNW+6), TERFC(0:), &
  T1(0:MPNW+6), T2(0:MPNW+6), T3(0:MPNW+6), T4(0:MPNW+6), &
  T5(0:MPNW+6), T6(0:MPNW+6), T7(0:MPNW+6), Z(0:), Z2(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. Z(0) < MPNW + 4 .OR. Z(0) < ABS (TERFC(2)) + 4 .OR. &
  TERFC(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPERFR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
EPS(0) = MPNW1 + 7
T1(0) = MPNW1 + 7
T2(0) = MPNW1 + 7
T3(0) = MPNW1 + 7
T4(0) = MPNW1 + 7
T5(0) = MPNW1 + 7
T6(0) = MPNW1 + 7
T7(0) = MPNW1 + 7
Z2(0) = MPNW1 + 7

NBT = MPNW * MPNBT
D1 = AINT (1.D0 + SQRT (NBT * LOG (2.D0)))
D2 = AINT (NBT / DCON + 8.D0)
CALL MPDMC (D1, 0, T1, MPNW1)
CALL MPDMC (D2, 0, T2, MPNW1)
CALL MPCPR (Z, T1, IC1, MPNW1)
T1(2) = - T1(2)
CALL MPCPR (Z, T1, IC2, MPNW1)
CALL MPCPR (Z, T2, IC3, MPNW1)

!if (z == 0.d0) then
IF (Z(2) == 0) THEN

!  terfc = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, TERFC, MPNW)

!elseif (z > d2) then
ELSEIF (IC1 > 0) THEN

!  terfc = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, TERFC, MPNW)

!elseif (z < -d2) then
ELSEIF (IC2 < 0) THEN

!  terfc = mpreal (2.d0, nwds)
  CALL MPDMC (2.D0, 0, TERFC, MPNW)

!elseif (abs (z) < d3) then
ELSEIF (IC3 < 0) THEN

!  eps = mpreal (2.d0, nwds) ** (-nbt)
  CALL MPDMC (1.D0, -NBT, EPS, MPNW1)

!  z2 = z**2
  CALL MPMUL (Z, Z, Z2, MPNW1)

!  t1 = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, T1, MPNW1)

!  t2 = z
  CALL MPEQ (Z, T2, MPNW1)

!  t3 = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, T3, MPNW1)

!  t5 = mpreal (1.d10, 4)
  CALL MPDMC (1.D10, 0, T5, 4)

  DO K = 0, ITRMX
    IF (K > 0) THEN
!      t2 = 2.d0 * z2 * t2
      CALL MPMULD (Z2, 2.D0, T6, MPNW1)
      CALL MPMUL (T6, T2, T7, MPNW1)
      CALL MPEQ (T7, T2, MPNW1)

!      t3 = (2.d0 * k + 1.d0) * t3
      CALL MPMULD (T3, 2.D0 * K + 1.D0, T6, MPNW1)
      CALL MPEQ (T6, T3, MPNW1)
    ENDIF

!    t4 = t2 / t3
    CALL MPDIV (T2, T3, T4, MPNW1)

!    t1 = t1 + t4
    CALL MPADD (T1, T4, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)

!    t6 = abs (mpreal (t4, 4) / mpreal (t1, 4))
    CALL MPDIV (T4, T1, T6, 4)

!    if (t6 < eps .or. t6 >= t5) goto 120
    CALL MPCPR (T6, EPS, IC1, 4)
    CALL MPCPR (T6, T5, IC2, 4)
    IF (IC1 <= 0 .OR. IC2 >= 0) GOTO 120

!    t5 = t6
    CALL MPEQ (T6, T5, 4)
  ENDDO

WRITE (6, 3) 1, ITRMX
3 FORMAT ('*** MPERFR: iteration limit exceeded',2I10)
CALL MPABRT (101)

120 CONTINUE

!  terfc = 1.d0 - 2.d0 * t1 / (sqrt (mppi (nwds)) * exp (z2))
  CALL MPDMC (1.D0, 0, T2, MPNW1)
  CALL MPMULD (T1, 2.D0, T3, MPNW1)
  CALL MPSQRT (MPPICON, T4, MPNW1)
  CALL MPEXP (Z2, T5, MPNW1)
  CALL MPMUL (T4, T5, T6, MPNW1)
  CALL MPDIV (T3, T6, T7, MPNW1)
  CALL MPSUB (T2, T7, T6, MPNW1)
  CALL MPEQ (T6, TERFC, MPNW)
ELSE

!  eps = mpreal (2.d0, nwds) ** (-nbt)
  CALL MPDMC (1.D0, -NBT, EPS, MPNW1)

!  z2 = z ** 2
  CALL MPMUL (Z, Z, Z2, MPNW1)

!  t1 = mpreal (0.d0, nwds)
  CALL MPDMC (0.D0, 0, T1, MPNW1)

!  t2 = mpreal (1.d0, nwds)
  CALL MPDMC (1.D0, 0, T2, MPNW1)

!  t3 = abs (z)
  CALL MPEQ (Z, T3, MPNW1)
  T3(2) = ABS (T3(2))

!  t5 = mpreal (1.d10, 4)
  CALL MPDMC (1.D10, 0, T5, 4)

  DO K = 0, ITRMX
    IF (K > 0) THEN
!      t2 = - (2.d0 * k - 1.d0) * t2 / 2.d0
      CALL MPMULD (T2, -(2.D0 * K - 1.D0), T6, MPNW1)
      CALL MPEQ (T6, T2, MPNW1)

!      t3 = z2 * t3
      CALL MPMUL (T2, T3, T6, MPNW1)
      CALL MPEQ (T6, T3, MPNW1)
    ENDIF

!    t4 = t2 / t3
    CALL MPDIV (T2, T3, T4, MPNW1)

!    t1 = t1 + t4
    CALL MPADD (T1, T4, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)

!    t6 = abs (mpreal (t4, 4) / mpreal (t1, 4))
    CALL MPDIV (T4, T1, T6, 4)

!    if (t6 < eps .or. t6 >= t5) goto 130
    CALL MPCPR (T6, EPS, IC1, 4)
    CALL MPCPR (T6, T5, IC2, 4)
    IF (IC1 <= 0 .OR. IC2 >= 0) GOTO 130

!    t5 = t6
    CALL MPEQ (T6, T5, 4)
  ENDDO

WRITE (6, 3) 2, ITRMX
CALL MPABRT (101)

130 CONTINUE

!  terfc = t1 / (sqrt (mppi (nwds)) * exp (z2))
  CALL MPSQRT (MPPICON, T3, MPNW1)
  CALL MPEXP (Z2, T4, MPNW1)
  CALL MPMUL (T3, T4, T5, MPNW1)
  CALL MPDIV (T1, T5, T6, MPNW1)

!  if (z < 0.d0) terfc = 2.d0 - terfc
  IF (Z(2) < 0) THEN
    CALL MPDMC (2.D0, 0, T2, MPNW1)
    CALL MPSUB (T2, T6, T7, MPNW1)
    CALL MPEQ (T7, T6, MPNW1)
  ENDIF

  CALL MPEQ (T6, TERFC, MPNW)
ENDIF

RETURN
END SUBROUTINE MPERFCR

SUBROUTINE MPGAMMAR (T, Z, MPNW)

!   This evaluates the gamma function, using an algorithm of R. W. Potter.
!   The argument t must not exceed 10^8 in size (this limit is set below),
!   must not be zero, and if negative must not be integer.

!   In the parameter statement below:
!     itrmx = limit of number of iterations in series; default = 100000.
!     con1 = 1/2 * log (10) to DP accuracy.
!     dmax = maximum size of input argument.

IMPLICIT NONE
INTEGER I, ITRMX, J, MPNW, MPNW1, NT, N2, N3
REAL (MPRKND) ALPHA, AL2, DMAX, D2, D3
PARAMETER (AL2 = 0.69314718055994530942D0, DMAX = 1D8, ITRMX = 100000)
INTEGER (MPIKND) T(0:), Z(0:), F1(0:8), SUM1(0:MPNW+6), &
  SUM2(0:MPNW+6), TN(0:MPNW+6), T1(0:MPNW+6), T2(0:MPNW+6), T3(0:MPNW+6), &
  T4(0:MPNW+6), T5(0:MPNW+6), T6(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. T(0) < MPNW + 4 .OR. T(0) < ABS (T(2)) + 4 .OR. &
  Z(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPGAMMAR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IF (T(2) == 0 .OR. T(3) > 0 .OR. (T(3) == 0 .AND. T(4) > DMAX) .OR. &
  (T(2) < 0 .AND. T(3) == 0 .AND. ABS (T(2)) == 1)) THEN
  WRITE (6, 2) DMAX
2 FORMAT ('*** MPGAMMAR: input argument must have absolute value <=',F10.0,','/ &
  'must not be zero, and if negative must not be an integer.')
  CALL MPABRT (65)
ENDIF

MPNW1 = MPNW + 1
F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0
SUM1(0) = MPNW + 7
SUM2(0) = MPNW + 7
TN(0) = MPNW + 7
T1(0) = MPNW + 7
T2(0) = MPNW + 7
T3(0) = MPNW + 7
T4(0) = MPNW + 7
T5(0) = MPNW + 7
T6(0) = MPNW + 7

!   Find the integer and fractional parts of t.

CALL MPINFR (T, T2, T3, MPNW1)

IF (T3(2) == 0) THEN

!   If t is a positive integer, then apply the usual factorial recursion.

  CALL MPMDC (T2, D2, N2, MPNW1)
  NT = D2 * 2.D0 ** N2
  CALL MPEQ (F1, T1, MPNW1)

  DO I = 2, NT - 1
    CALL MPMULD (T1, DBLE (I), T2, MPNW1)
    CALL MPEQ (T2, T1, MPNW1)
  ENDDO

  CALL MPROUN (T1, MPNW)
  CALL MPEQ (T1, Z, MPNW)
  GOTO 120
ELSEIF (T(2) > 0) THEN

!   Apply the identity Gamma[t+1] = t * Gamma[t] to reduce the input argument
!   to the unit interval.

  CALL MPMDC (T2, D2, N2, MPNW1)
  NT = D2 * 2.D0 ** N2
  CALL MPEQ (F1, T1, MPNW1)
  CALL MPEQ (T3, TN, MPNW1)

  DO I = 1, NT
    CALL MPDMC (DBLE (I), 0, T4, MPNW1)
    CALL MPSUB (T, T4, T5, MPNW1)
    CALL MPMUL (T1, T5, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)
  ENDDO
ELSE

!   Apply the gamma identity to reduce a negative argument to the unit interval.

  CALL MPSUB (F1, T, T4, MPNW1)
  CALL MPINFR (T4, T3, T5, MPNW1)
  CALL MPMDC (T3, D3, N3, MPNW1)
  NT = D3 * 2.D0 ** N3

  CALL MPEQ (F1, T1, MPNW1)
  CALL MPSUB (F1, T5, T2, MPNW1)
  CALL MPEQ (T2, TN, MPNW1)

  DO I = 0, NT - 1
!    t1 = t1 / (t + dble (i))
    CALL MPDMC (DBLE (I), 0, T4, MPNW1)
    CALL MPADD (T, T4, T5, MPNW1)
    CALL MPDIV (T1, T5, T6, MPNW1)
    CALL MPEQ (T6, T1, MPNW1)
  ENDDO
ENDIF

!   Calculate alpha = bits of precision * log(2) / 2, then take the next integer
!   value mod 4, so that d2 = 0.25 * alpha^2 can be calculated exactly in DP.

ALPHA = 4.D0 * AINT ((0.5D0 * MPNBT * AL2 * (MPNW1 + 1)) / 4.D0 + 1.D0)
D2 = 0.25D0 * ALPHA**2

CALL MPEQ (TN, T2, MPNW1)
CALL MPDIV (F1, T2, T3, MPNW1)
CALL MPEQ (T3, SUM1, MPNW1)

!   Evaluate the series with t.

DO J = 1, ITRMX
  CALL MPDMC (DBLE (J), 0, T6, MPNW1)
  CALL MPADD (T2, T6, T4, MPNW1)
  CALL MPMULD (T4, DBLE (J), T5, MPNW1)
  CALL MPDIV (T3, T5, T6, MPNW1)
  CALL MPMULD (T6, D2, T3, MPNW1)
  CALL MPADD (SUM1, T3, T4, MPNW1)
  CALL MPEQ (T4, SUM1, MPNW1)
  IF (T3(2) == 0 .OR. T3(3) < SUM1(3) - MPNW1) GOTO 100
ENDDO

WRITE (6, 3) 1, ITRMX
3 FORMAT ('*** MPGAMMAR: iteration limit exceeded',2I10)
CALL MPABRT (101)

100 CONTINUE

CALL MPEQ (TN, T2, MPNW1)
T2(2) = - T2(2)
CALL MPDIV (F1, T2, T3, MPNW1)
CALL MPEQ (T3, SUM2, MPNW1)

!   Evaluate the same series with -t.

DO J = 1, ITRMX
  CALL MPDMC (DBLE (J), 0, T6, MPNW1)
  CALL MPADD (T2, T6, T4, MPNW1)
  CALL MPMULD (T4, DBLE (J), T5, MPNW1)
  CALL MPDIV (T3, T5, T6, MPNW1)
  CALL MPMULD (T6, D2, T3, MPNW1)
  CALL MPADD (SUM2, T3, T4, MPNW1)
  CALL MPEQ (T4, SUM2, MPNW1)
  IF (T3(2) == 0 .OR. T3(3) < SUM2(3) - MPNW1) GOTO 110
ENDDO

WRITE (6, 3) 2, ITRMX
CALL MPABRT (67)

110 CONTINUE

!   Compute sqrt (mppic * sum1 / (tn * sin (mppic * tn) * sum2))
!   and (alpha/2)^tn terms.

CALL MPEQ (MPPICON, T2, MPNW1)
CALL MPMUL (T2, TN, T3, MPNW1)
CALL MPCSSNR (T3, T4, T5, MPNW1)

CALL MPMUL (T5, SUM2, T6, MPNW1)
CALL MPMUL (TN, T6, T5, MPNW1)
CALL MPMUL (T2, SUM1, T3, MPNW1)
CALL MPDIV (T3, T5, T6, MPNW1)
T6(2) = - T6(2)
CALL MPSQRT (T6, T2, MPNW1)
CALL MPDMC (0.5D0 * ALPHA, 0, T3, MPNW1)
CALL MPLOG (T3, T4, MPNW1)
CALL MPMUL (TN, T4, T5, MPNW1)
CALL MPEXP (T5, T6, MPNW1)
CALL MPMUL (T2, T6, T3, MPNW1)
CALL MPMUL (T1, T3, T4, MPNW1)

!   Round to mpnw words precision.

CALL MPROUN (T4, MPNW)
CALL MPEQ (T4, Z, MPNW)

120 CONTINUE

RETURN
END SUBROUTINE MPGAMMAR

SUBROUTINE MPINCGAMMAR (S, Z, G, MPNW)

!  This returns the incomplete gamma function, using a combination of formula
!  8.7.3 of the DLMF (for modest-sized z) and formula 8.11.2 (for large z).

IMPLICIT NONE
INTEGER ITRMAX, K, MPNW, MPNW1
REAL (MPRKND) DMAX
PARAMETER (DMAX = 40.D0, ITRMAX = 1000000)
INTEGER (MPIKND) G(0:), S(0:), T0(0:MPNW+6), T1(0:MPNW+6), &
  T2(0:MPNW+6), T3(0:MPNW+6), T4(0:MPNW+6), T5(0:MPNW+6), &
  Z(0:), F1(0:8)

! End of declaration

IF (MPNW < 4 .OR. S(0) < MPNW + 4 .OR. S(0) < ABS (S(2)) + 4 .OR. &
  Z(0) < MPNW + 4 .OR. Z(0) < ABS (Z(2)) + 4 .OR. G(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPINCGAMMAR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

MPNW1 = MPNW + 1
T0(0) = MPNW + 7
T1(0) = MPNW + 7
T2(0) = MPNW + 7
T3(0) = MPNW + 7
T4(0) = MPNW + 7
T5(0) = MPNW + 7
F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

! if (abs (z) < dmax * mpnw) then

  IF (Z(3) < 0 .OR. (Z(3) == 0 .AND. Z(4) < DMAX * MPNW)) THEN

!  t1 = gamma (s)

  CALL MPGAMMAR (S, T1, MPNW1)

!  t2 = 1.d0 / (s * t1)

  CALL MPMUL (S, T1, T3, MPNW1)
  CALL MPDIV (F1, T3, T2, MPNW1)

!   t0 = t2

  CALL MPEQ (T2, T0, MPNW1)

  DO K = 1, ITRMAX

!    t2 = t2 * z / (s + dble (k))

    CALL MPMUL (T2, Z, T5, MPNW1)
    CALL MPDMC (DBLE (K), 0, T3, MPNW1)
    CALL MPADD (S, T3, T4, MPNW1)
    CALL MPDIV (T5, T4, T2, MPNW1)

!    t0 = t0 + t2

    CALL MPADD (T0, T2, T3, MPNW1)
    CALL MPEQ (T3, T0, MPNW1)

    IF (T2(2) == 0 .OR. T2(3) < T0(3) - MPNW) GOTO 100
  ENDDO

  WRITE (MPLDB, 2) 1, ITRMAX
2 FORMAT ('*** MPINCGAMMAR: iteration limit exceeded:',2I10)
  CALL MPABRT (101)

100 CONTINUE

!   gammainc = t1 * (1.d0 - z ** s / exp (z) * t0)

  CALL MPPOWER (Z, S, T2, MPNW1)
  CALL MPEXP (Z, T3, MPNW1)
  CALL MPDIV (T2, T3, T4, MPNW1)
  CALL MPMUL (T4, T0, T5, MPNW1)
  CALL MPSUB (F1, T5, T2, MPNW1)
  CALL MPMUL (T1, T2, T3, MPNW1)
  CALL MPEQ (T3, T1, MPNW1)
ELSE
!  t0 = mpreal (1.d0, mpnw)

  T0(2) = 1
  T0(3) = 0
  T0(4) = 1
  T0(5) = 0
  T0(6) = 0

!  t1 = mpreal (1.d0, mpnw)

  T1(2) = 1
  T1(3) = 0
  T1(4) = 1
  T1(5) = 0
  T1(6) = 0

  DO K = 1, ITRMAX
!    t1 = t1 * (s - dble (k)) / z

    CALL MPDMC (DBLE (K), 0, T2, MPNW1)
    CALL MPSUB (S, T2, T3, MPNW1)
    CALL MPMUL (T1, T3, T4, MPNW1)
    CALL MPDIV (T4, Z, T1, MPNW1)

!    t0 = t0 + t1

    CALL MPADD (T0, T1, T2, MPNW1)
    CALL MPEQ (T2, T0, MPNW1)

    IF (T1(2) == 0 .OR. T1(3) < T0(3) - MPNW) GOTO 110
  ENDDO

  WRITE (MPLDB, 2) 2, ITRMAX
  CALL MPABRT (101)

110 CONTINUE

!  gammainc = z ** (s - 1.d0) / exp (z) * t0

   CALL MPSUB (S, F1, T2, MPNW1)
   CALL MPPOWER (Z, T2, T3, MPNW1)
   CALL MPEXP (Z, T4, MPNW1)
   CALL MPDIV (T3, T4, T2, MPNW1)
   CALL MPMUL (T2, T0, T1, MPNW1)
ENDIF

200 CONTINUE

CALL MPROUN (T1, MPNW)
CALL MPEQ (T1, G, MPNW)

RETURN
END SUBROUTINE MPINCGAMMAR

SUBROUTINE MPZETAR (SS, ZZ, MPNW)

!   This returns the zeta function at positive real argument SS using an algorithm
!   due to Peter Borwein.

IMPLICIT NONE
INTEGER I, ITRMAX, J, MPNW, MPNW1, N
REAL (MPRKND) DFRAC, D1
PARAMETER (ITRMAX = 1000000, DFRAC = 1.D0+CEILING(MPDPW))
INTEGER (MPIKND) S(0:MPNW+6), SS(0:), ZZ(0:), &
  T1(0:MPNW+6), T2(0:MPNW+6), T3(0:MPNW+6), T4(0:MPNW+6), T5(0:MPNW+6), &
  TN(0:MPNW+6), TT(0:MPNW+6), F1(0:8)
REAL (MPRKND) SGN

!  End of declaration

IF (MPNW < 4 .OR. SS(0) < MPNW + 4 .OR. SS(0) < ABS (SS(2)) + 4 .OR. &
  ZZ(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPZETAR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check if argument is 1 -- undefined.

IF (SS(2) == 1 .AND. SS(3) == 0 .AND. SS(4) == 1) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPZETAR: argument is 1')
  CALL MPABRT (63)
ENDIF

MPNW1 = MPNW + 1
S(0) = MPNW + 7
T1(0) = MPNW + 7
T2(0) = MPNW + 7
T3(0) = MPNW + 7
T4(0) = MPNW + 7
T5(0) = MPNW + 7
TN(0) = MPNW + 7
TT(0) = MPNW + 7

!   Set f1 = 1.

F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

!   Check if argument is zero.  If so, the result is -1/2.

IF (SS(2) == 0) THEN
  CALL MPDMC (-0.5D0, 0, T1, MPNW1)
  GOTO 200
ENDIF

!   Check if argument is negative.

IF (SS(2) < 0) THEN

!   Check if argument is a negative even integer.  If so, the result is zero.

  CALL MPMULD (SS, 0.5D0, T1, MPNW1)
  CALL MPINFR (T1, T2, T3, MPNW1)
  IF (T3(2) == 0) THEN
    T1(1) = MPNW1
    T1(2) = 0
    T1(3) = 0
    T1(4) = 0
    T1(5) = 0
    GOTO 200
  ENDIF

!   Otherwise compute zeta(1-ss), and later apply the reflection formula.

  CALL MPSUB (F1, SS, TT, MPNW1)
ELSE
  CALL MPEQ (SS, TT, MPNW1)
ENDIF

!  Check if argument is large enough that computing with definition is faster.

! [old] if (tt .gt. mpreald (dlogb * mpnw / log (32.d0 * mpnw), mpnw)) then
! [old] d1 = dlogb * mpnw / log (32.d0 * mpnw)

D1 = MPNBT * MPNW * LOG (2.D0) / LOG (2.D0 * MPNBT * MPNW / 3.D0)
IF (TT(2) >= 1 .AND. (TT(3) > 1 .OR. TT(3) == 0 .AND. TT(4) > D1)) THEN

!  t1 = mpreal (1.d0, mpnw)

T1(1) = MPNW1
T1(2) = 1
T1(3) = 0
T1(4) = 1
T1(5) = 0
T1(6) = 0

  DO I = 2, ITRMAX

!    t2 = mpreal (dble (i), mpnw) ** tt

    CALL MPDMC (DBLE (I), 0, T4, MPNW1)
    CALL MPPOWER (T4, TT, T2, MPNW1)

!    t3 = 1.d0 / t2

    CALL MPDIV (F1, T2, T3, MPNW1)

!    t1 = t1 + t3

    CALL MPADD (T1, T3, T2, MPNW1)
    CALL MPEQ (T2, T1, MPNW1)

    IF (T3(2) == 0 .OR. T3(3) < - MPNW1) GOTO 200
  ENDDO

  WRITE (MPLDB, 3) 1, ITRMAX
3 FORMAT ('*** MPZETAR: iteration limit exceeded',2I10)
  CALL MPABRT (101)
ENDIF

N = DFRAC * MPNW1

! tn = mpreal (2.d0, mpnw) ** n

TN(0) = MPNW + 7
CALL MPDMC (1.D0, N, TN, MPNW1)

! t1 = - tn

CALL MPEQ (TN, T1, MPNW1)
T1(2) = - T1(2)

! t2 = mpreal (0.d0, mpnw)

T2(2) = 0
T2(3) = 0
T2(4) = 0

! s = mpreal (0.d0, mpnw)

S(1) = MPNW1
S(2) = 0
S(3) = 0
S(4) = 0

SGN = 1.D0

DO J = 0, 2 * N - 1
!  t3 = mpreal (dble (j + 1), mpnw) ** tt

  CALL MPDMC (DBLE (J + 1), 0, T4, MPNW1)
  CALL MPPOWER (T4, TT, T3, MPNW1)

!  s = s + sgn * t1 / t3

  CALL MPDIV (T1, T3, T4, MPNW1)
  IF (SGN < 0.D0) T4(2) = - T4(2)
  CALL MPADD (S, T4, T5, MPNW1)
  CALL MPEQ (T5, S, MPNW1)

  SGN = - SGN

  IF (J .LT. N - 1) THEN
!    t2 = mpreal (0.d0, mpnw)

    T2(2) = 0
    T2(3) = 0
    T2(4) = 0

  ELSEIF (J .EQ. N - 1) THEN
!    t2 = mpreal (1.d0, mpnw)

    T2(2) = 1
    T2(3) = 0
    T2(4) = 1
    T2(5) = 0
    T2(6) = 0

  ELSE
!     t2 = t2 * dble (2 * n - j) / dble (j + 1 - n)

     CALL MPMULD (T2, DBLE (2 * N - J), T3, MPNW1)
     CALL MPDIVD (T3, DBLE (J + 1 - N), T2, MPNW1)

  ENDIF
!  t1 = t1 + t2

  CALL MPADD (T1, T2, T3, MPNW1)
  CALL MPEQ (T3, T1, MPNW1)
ENDDO

! t1 = - s / (tn * (1.d0 - mpreal (2.d0, mpnw) ** (1.d0 - tt)))

CALL MPSUB (F1, TT, T3, MPNW1)
T2(2) = 1
T2(3) = 0
T2(4) = 2
T2(5) = 0
T2(6) = 0
CALL MPPOWER (T2, T3, T4, MPNW1)
CALL MPSUB (F1, T4, T2, MPNW1)
CALL MPMUL (TN, T2, T3, MPNW1)
CALL MPDIV (S, T3, T1, MPNW1)
T1(2) = - T1(2)

!   If original argument was negative, apply the reflection formula.

IF (SS(2) < 0) THEN
  CALL MPGAMMAR (TT, T3, MPNW1)
  CALL MPMUL (T1, T3, T2, MPNW1)
  CALL MPMUL (MPPICON, TT, T1, MPNW1)
  CALL MPMULD (T1, 0.5D0, T3, MPNW1)
  CALL MPCSSNR (T3, T4, T5, MPNW1)
  CALL MPMUL (T2, T4, T1, MPNW1)
  CALL MPMULD (MPPICON, 2.D0, T2, MPNW1)
  CALL MPPOWER (T2, TT, T3, MPNW1)
  CALL MPDIV (T1, T3, T2, MPNW1)
  CALL MPMULD (T2, 2.D0, T1, MPNW1)
ENDIF

200 CONTINUE

! zetapbr = t1

CALL MPROUN (T1, MPNW)
CALL MPEQ (T1, ZZ, MPNW)
RETURN
END SUBROUTINE MPZETAR

SUBROUTINE MPZETAEMR (NB1, NB2, BERNE, S, Z, MPNW)

!  This evaluates the Riemann zeta function, using the combination of
!  the definition formula (for large s), and an Euler-Maclaurin scheme
!  (see formula 25.2.9 of the DLMF.  The array berne contains precomputed
!  Bernoulli numbers.  Its dimensions must be as shown below.

IMPLICIT NONE
INTEGER I, IA, ITRMAX, K, MPNW, MPNW1, NA, NB1, NB2, NN
REAL (MPRKND) DFRAC, DLOGB, D1
PARAMETER (ITRMAX = 1000000, DFRAC = 8.5D0, DLOGB = 33.27106466687737D0)
INTEGER (MPIKND) S(0:), Z(0:), T0(0:MPNW+6), T1(0:MPNW+6), &
  T2(0:MPNW+6), T3(0:MPNW+6), T4(0:MPNW+6), T5(0:MPNW+6), T6(0:MPNW+6), &
  T7(0:MPNW+6), T8(0:MPNW+6), T9(0:MPNW+6), TT(0:MPNW+6), F1(0:8)
INTEGER (MPIKND) BERNE(0:NB1+5,NB2)

! End of declaration

IF (MPNW < 4 .OR. S(0) < MPNW + 4 .OR. S(0) < ABS (S(2)) + 4 .OR. &
  Z(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPZETAEMR: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

!   Check if argument is 1 -- undefined.

IF (S(2) == 1 .AND. S(3) == 0 .AND. S(4) == 1) THEN
  WRITE (MPLDB, 2)
2 FORMAT ('*** MPZETAEMR: argument is 1')
  CALL MPABRT (63)
ENDIF

!   Check if berne array has been initialized.

IF (BERNE(0,1) < MPNW + 4 .OR. BERNE(0,1) < ABS (BERNE(2,1)) + 4 .OR. &
  BERNE(0,NB2) < MPNW + 4 .OR. BERNE(0,NB2) < ABS (BERNE(2,NB2)) + 4 .OR. &
  NB2 < INT (MPNDPW * MPNW)) THEN
  WRITE (MPLDB, 3) INT (MPNDPW * MPNW)
3 FORMAT ('*** MPZETAEMR: Bernoulli coefficient array must be initialized'/ &
   'with at least',I8,' entries.')
  CALL MPABRT (62)
ENDIF

I = 0
K = 0
MPNW1 = MPNW + 1
T0(0) = MPNW + 7
T1(0) = MPNW + 7
T2(0) = MPNW + 7
T3(0) = MPNW + 7
T4(0) = MPNW + 7
T5(0) = MPNW + 7
T6(0) = MPNW + 7
T7(0) = MPNW + 7
T8(0) = MPNW + 7
T9(0) = MPNW + 7
TT(0) = MPNW + 7

!   Set f1 = 1.

F1(0) = 9
F1(1) = MPNW1
F1(2) = 1
F1(3) = 0
F1(4) = 1
F1(5) = 0
F1(6) = 0

!   Check if argument is zero.  If so, result is - 1/2.

IF (S(2) == 0) THEN
  CALL MPDMC (-0.5D0, 0, T1, MPNW)
  GOTO 200
ENDIF

!   Check if argument is negative.

IF (S(2) < 0) THEN

!   Check if argument is a negative even integer.  If so, the result is zero.

  CALL MPMULD (S, 0.5D0, T1, MPNW1)
  CALL MPINFR (T1, T2, T3, MPNW1)
  IF (T3(2) == 0) THEN
    T1(1) = MPNW1
    T1(2) = 0
    T1(3) = 0
    T1(4) = 0
    T1(5) = 0
    GOTO 200
  ENDIF

!   Otherwise compute zeta(1-s), and later apply the reflection formula.

  CALL MPSUB (F1, S, TT, MPNW1)
ELSE
  CALL MPEQ (S, TT, MPNW1)
ENDIF

!  Check if argument is large enough that computing with definition is faster.

! if (tt .gt. mpreald (dlogb * mpnw / log (32.d0 * mpnw), mpnw)) then

D1 = DLOGB * MPNW / LOG (32.D0 * MPNW)
IF (TT(3) > 1 .OR. (TT(3) == 0 .AND. TT(4) > D1)) THEN

!  t1 = mpreal (1.d0, mpnw)

  T1(1) = MPNW1
  T1(2) = 1
  T1(3) = 0
  T1(4) = 1
  T1(5) = 0
  T1(6) = 0

  DO I = 2, ITRMAX

!    t2 = mpreal (dble (i), mpnw) ** tt

    CALL MPDMC (DBLE (I), 0, T4, MPNW1)
    CALL MPPOWER (T4, TT, T2, MPNW1)

!    t3 = 1.d0 / t2

    CALL MPDIV (F1, T2, T3, MPNW1)

!    t1 = t1 + t3

    CALL MPADD (T1, T3, T2, MPNW1)
    CALL MPEQ (T2, T1, MPNW1)

    IF (T3(2) == 0 .OR. T3(3) < - MPNW) GOTO 200
  ENDDO

  WRITE (MPLDB, 4) 1, ITRMAX
4 FORMAT ('*** MPZETAEMR: iteration limit exceeded',2I10)
  CALL MPABRT (101)
ENDIF

! t0 = mpreal (1.d0, mpnw)

T0(1) = MPNW1
T0(2) = 1
T0(3) = 0
T0(4) = 1
T0(5) = 0
T0(6) = 0

NN = DFRAC * MPNW1

DO K = 2, NN
!  t1 = mpreal (dble (k), mpnw) ** tt

  CALL MPDMC (DBLE (K), 0, T2, MPNW1)
  CALL MPPOWER (T2, TT, T1, MPNW1)

!  t0 = t0 + 1.d0 / t1

  CALL MPDIV (F1, T1, T2, MPNW1)
  CALL MPADD (T0, T2, T3, MPNW1)
  CALL MPEQ (T3, T0, MPNW1)
ENDDO

! t0 = t0 + dble (nn) / (t1 * (tt - 1.d0)) - 0.5d0 / t1

CALL MPDMC (DBLE (NN), 0, T2, MPNW1)
CALL MPSUB (TT, F1, T3, MPNW1)
CALL MPMUL (T1, T3, T4, MPNW1)
CALL MPDIV (T2, T4, T3, MPNW1)
CALL MPADD (T0, T3, T2, MPNW1)
CALL MPDMC (0.5D0, 0, T3, MPNW1)
CALL MPDIV (T3, T1, T4, MPNW1)
CALL MPSUB (T2, T4, T0, MPNW1)

! t3 = tt

CALL MPEQ (TT, T3, MPNW1)

! t2 = t3 / (12.d0 * dble (nn) * t1)

CALL MPMULD (T1, 12.D0 * DBLE (NN), T4, MPNW1)
CALL MPDIV (T3, T4, T2, MPNW1)

! t5 = dble (nn) * t1

CALL MPMULD (T1, DBLE (NN), T5, MPNW1)

! t9 = dble (nn) ** 2

CALL MPDMC (DBLE (NN), 0, T6, MPNW1)
CALL MPMUL (T6, T6, T9, MPNW1)

DO K = 2, MIN (NB2, ITRMAX)
!  t3 = t3 * (tt + dble (2*k - 2)) * (tt + dble (2*k - 3)) / &
!         (dble (2 * k - 1) * dble (2 * k - 2))

  CALL MPDMC (DBLE (2 * K - 2), 0, T4, MPNW1)
  CALL MPADD (TT, T4, T6, MPNW1)
  CALL MPDMC (DBLE (2 * K - 3), 0, T7, MPNW1)
  CALL MPADD (TT, T7, T8, MPNW1)
  CALL MPMUL (T6, T8, T7, MPNW1)
  CALL MPMUL (T3, T7, T4, MPNW1)
  CALL MPDMC (DBLE (2 * K - 1), 0, T6, MPNW1)
  CALL MPDMC (DBLE (2 * K - 2), 0, T7, MPNW1)
  CALL MPMUL (T6, T7, T8, MPNW1)
  CALL MPDIV (T4, T8, T3, MPNW1)

!  t5 = t5 * t9

  CALL MPMUL (T5, T9, T6, MPNW1)
  CALL MPEQ (T6, T5, MPNW1)

!  t7 = t3 * berne(k) / (dble (2 * k) * t5)

!   The next few lines (to !+) are necessary, rather than a simple call
!   to mpmul, to avoid a Fortran rank-mismatch error.

!   call mpmul (t3, berne(0,k), t4, mpnw1)

  IA = SIGN (INT (1, MPIKND), BERNE(2,K))
  NA = MIN (INT (ABS (BERNE(2,K))), MPNW1)
  T8(1) = MPNW1
  T8(2) = SIGN (NA, IA)

  DO I = 2, NA + 2
    T8(I+1) = BERNE(I+1,K)
  ENDDO

  T8(NA+4) = 0
  T8(NA+5) = 0
  CALL MPMUL (T3, T8, T4, MPNW1)
!+
  CALL MPMULD (T5, DBLE (2 * K), T6, MPNW1)
  CALL MPDIV (T4, T6, T7, MPNW1)

!  t2 = t2 + t7

  CALL MPADD (T2, T7, T4, MPNW1)
  CALL MPEQ (T4, T2, MPNW1)

  IF (T7(2) == 0 .OR. T7(3) < T2(3) - MPNW) GOTO 110
ENDDO

WRITE (MPLDB, 3) 2, MIN (NB2, ITRMAX)
CALL MPABRT (101)

110 CONTINUE

! zetaem = t0 + t2

CALL MPADD (T0, T2, T1, MPNW1)

!   If original argument was negative, apply the reflection formula.

IF (S(2) < 0.D0) THEN
  CALL MPGAMMAR (TT, T3, MPNW1)
  CALL MPMUL (T1, T3, T2, MPNW1)
  CALL MPMUL (MPPICON, TT, T1, MPNW1)
  CALL MPMULD (T1, 0.5D0, T3, MPNW1)
  CALL MPCSSNR (T3, T4, T5, MPNW1)
  CALL MPMUL (T2, T4, T1, MPNW1)
  CALL MPMULD (MPPICON, 2.D0, T2, MPNW1)
  CALL MPPOWER (T2, TT, T3, MPNW1)
  CALL MPDIV (T1, T3, T2, MPNW1)
  CALL MPMULD (T2, 2.D0, T1, MPNW1)
ENDIF

200 CONTINUE

CALL MPROUN (T1, MPNW)
CALL MPEQ (T1, Z, MPNW)

RETURN
END SUBROUTINE MPZETAEMR

END MODULE ModLib_MPFUNE

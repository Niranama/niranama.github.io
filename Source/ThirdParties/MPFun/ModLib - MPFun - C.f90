!*****************************************************************************

!  MPFUN20-Fort: A thread-safe arbitrary precision computation package
!  Binary-decimal, decimal-binary and I/O functions (module MPFUNC)

!  Revision date:  31 May 2021

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

!  DESCRIPTION OF THIS MODULE (MPFUNC):
!    This module contains subroutines for binary-decimal and decimal-binary
!    conversion, together with low-level E-format and F-format conversion, and
!    basic input and output.

MODULE ModLib_MPFUNC
USE ModLib_MPFUNA
USE ModLib_MPFUNB

CONTAINS

SUBROUTINE MPCTOMP (A, N, B, MPNW)

!  Converts the character(1) array A of length N into the MPR number B.
!  Restrictions: (a) no embedded blanks; (b) a leading digit (possibly
!  zero) must be present; and (c) a period must be present.  An exponent
!  (with "d" or "e") may optionally follow the numeric value.

IMPLICIT NONE
INTEGER I, IEXP, IX, J, KDE, KEND, KEXPEND, KEXPST, &
  KEXPSGN, KNUMEND1, KNUMEND2, KNUMST1, KNUMST2, KPER, KSGN, KSTART, LEXP, &
  LEXPMX, LNUM, LNUM1, LNUM2, MPNW, MPNW1, N, N1, N2
REAL (MPRKND) D10W, T1
CHARACTER(1) A(N)
CHARACTER(10) DIGITS
CHARACTER(32) CA
PARAMETER (LEXPMX = 9, DIGITS = '0123456789', D10W = 10.D0**MPNDPW)
INTEGER (MPIKND) B(0:), F(0:8), S0(0:MPNW+6), S1(0:MPNW+6), S2(0:MPNW+6)

! write (6, *) 'mpctomp: a, n, mpnw =', n, mpnw
! write (6, '(100a1)') 'X',(a(i),i=1,n),'X'

! End of declaration

IF (MPNW < 4 .OR. B(0) < MPNW + 6) THEN
 WRITE (MPLDB, 1)
1 FORMAT ('*** MPCTOMP: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

S0(0) = MPNW + 7
S1(0) = MPNW + 7
S2(0) = MPNW + 7
F(0) = 9
F(1) = MPNW

DO I = 2, 8
  F(I) = 0
ENDDO

MPNW1 = MPNW + 1
KDE = 0
KEND = 0
KEXPEND = 0
KEXPST = 0
KEXPSGN = 0
KNUMEND1 = 0
KNUMEND2 = 0
KNUMST1 = 0
KNUMST2 = 0
KPER = 0
KSGN = 0
KSTART = 0

!   Locate:
!     kstart = index of first nonblank character.
!     kend = index of last nonblank character.

DO I = 1, N
  IF (A(I) /= ' ') GOTO 100
ENDDO

!   Input is completely blank.

WRITE (6, 2) 1
2 FORMAT ('*** MPCTOMP: Syntax error in input string; code =',I4/ &
  'Restrictions: (a) no embedded blanks; (b) a leading digit (possibly'/ &
  'zero) must be present; and (c) a period must be present.  An exponent'/ &
  '(with "d" or "e") may optionally follow the numeric value.')
CALL MPABRT (41)

100 CONTINUE

KSTART = I

DO I = N, KSTART, -1
  IF (A(I) /= ' ') GOTO 110
ENDDO

I = KSTART

110 CONTINUE

KEND = I

!   Scan input for:
!     kde = index of 'd' or 'e'.
!     kexpend = index of end of exponent.
!     kexpst = index of start of exponent.
!     kespsgn = index of sign of exponent.
!     knumend1 = index of end of numeric part prior to period.
!     knumend2 = index of end of numeric part after period.
!     knumst1 = index of start of numeric part prior to period.
!     knumst2 = index of start of numeric part after period.
!     kper = index of period.
!     ksgn = index of sign of number.

DO I = KSTART, KEND
  IF (A(I) == ' ') THEN
    WRITE (6, 2) 2
    CALL MPABRT (41)
  ELSEIF (A(I) == '+' .OR. A(I) == '-') THEN
    IF (I == KSTART) THEN
      KSGN = I
    ELSEIF (KDE > 0 .AND. KEXPSGN == 0 .AND. KEXPST == 0 .AND. I < KEND) THEN
      KEXPSGN = I
    ELSE
      WRITE (6, 2) 3
      CALL MPABRT (41)
    ENDIF
  ELSEIF (A(I) == 'e' .OR. A(I) == 'E' .OR. A(I) == 'd' .OR. A(I) == 'D') THEN
    IF (KDE == 0 .AND. KPER > 0 .AND. I < KEND) THEN
      KDE = I
      KNUMEND2 = I - 1
    ELSE
      WRITE (6, 2) 4
      CALL MPABRT (41)
    ENDIF
  ELSEIF (A(I) == '.') THEN
    IF (KPER == 0 .AND. KDE == 0 .AND. KNUMST1 > 0 .AND. KNUMST2 == 0) THEN
      KPER = I
      KNUMEND1 = I - 1
    ELSE
      WRITE (6, 2) 5
      CALL MPABRT (41)
    ENDIF
  ELSEIF (INDEX (DIGITS, A(I)) > 0) THEN
    IF (KNUMST1 == 0) THEN
      KNUMST1 = I
    ELSEIF (KPER > 0 .AND. KNUMST2 == 0 .AND. KDE ==  0) THEN
      KNUMST2 = I
    ELSEIF (KDE > 0 .AND. KEXPST == 0) THEN
      KEXPST = I
    ENDIF
    IF (I == KEND) THEN
      IF (KNUMST2 > 0 .AND. KDE == 0) THEN
        KNUMEND2 = I
      ELSEIF (KEXPST > 0) THEN
        KEXPEND = I
      ELSE
        WRITE (6, 2) 6
        CALL MPABRT (41)
      ENDIF
    ENDIF
  ELSE
    WRITE (6, 2) 7
    CALL MPABRT (41)
  ENDIF
ENDDO

! write (6, *) 'kde, kend, kexpend, kexpst =', kde, kend, kexpend, kexpst
! write (6, *) 'kexpsgn, numend1, knumend2, knumst1 =', kexpsgn, knumend1, knumend2, knumst1
! write (6, *) 'knumst2, kper, ksgn, kstart =', knumst2, kper, ksgn, kstart

!   Decode exponent.

IF (KEXPST > 0) THEN
  LEXP = KEXPEND - KEXPST + 1
  IF (LEXP > LEXPMX) THEN
    WRITE (6, 3)
3   FORMAT ('*** MPCTOMP: exponent string is too long.')
    CALL MPABRT (85)
  ENDIF

  DO I = 1, LEXP
    CA(I:I) = A(I+KEXPST-1)
  ENDDO

  IEXP = MPDIGIN (CA, LEXP)
  IF (KEXPSGN > 0) THEN
    IF (A(KEXPSGN) == '-') IEXP = -IEXP
  ENDIF
ELSE
  IEXP = 0
ENDIF

!   Determine lengths of two sections of number.

LNUM1 = KNUMEND1 - KNUMST1 + 1
IF (KNUMST2 > 0) THEN
  LNUM2 = KNUMEND2 - KNUMST2 + 1
ELSE
  LNUM2 = 0
ENDIF
LNUM = LNUM1 + LNUM2

! write (6, *) 'iexp, lnum1, lnum2 =', iexp, lnum1, lnum2

!   Determine the number of chunks of digits and the left-over.

N1 = LNUM / MPNDPW
N2 = MOD (LNUM, MPNDPW)

!   Construct first (left-over) portion, right-justified in CA.

CA(1:MPNDPW - N2) = ' '
IX = KNUMST1 - 1

DO I = 1, N2
  IX = IX + 1
  IF (IX == KPER) IX = IX + 1
  CA(I+MPNDPW-N2:I+MPNDPW-N2) = A(IX)
ENDDO

T1 = MPDIGIN (CA, MPNDPW)
IF (T1 > 0) THEN
  F(2) = 1
  F(3) = 0
  F(4) = T1
ELSE
  F(2) = 0
  F(3) = 0
  F(4) = 0
ENDIF
CALL MPEQ (F, S0, MPNW1)

!   Process remaining chunks of digits.

DO J = 1, N1
  DO I = 1, MPNDPW
    IX = IX + 1
    IF (IX == KPER) IX = IX + 1
    CA(I:I) = A(IX)
  ENDDO

  T1 = MPDIGIN (CA, MPNDPW)
  IF (T1 > 0) THEN
    F(2) = 1
    F(3) = 0
    F(4) = T1
  ELSE
    F(2) = 0
    F(3) = 0
    F(4) = 0
  ENDIF

  CALL MPMULD (S0, D10W, S1, MPNW1)
  CALL MPADD (S1, F, S0, MPNW1)
ENDDO

!  Correct exponent.

IEXP = IEXP - LNUM2
F(2) = 1
F(3) = 0
F(4) = 10
CALL MPNPWR (F, IEXP, S1, MPNW1)
CALL MPMUL (S0, S1, S2, MPNW1)
IF (KSGN > 0) THEN
  IF (A(KSGN) == '-') S2(2) = -S2(2)
ENDIF

!   Restore original precision and exit.

CALL MPROUN (S2, MPNW)
CALL MPEQ (S2, B, MPNW)

! write (6, *) 'mpctomp: output ='
! call mpout (6, 420, 400, b, mpnw)

RETURN
END SUBROUTINE MPCTOMP

REAL (MPRKND) FUNCTION MPDIGIN (CA, N)

!   This converts the string CA of nonblank length N to double precision.
!   CA may only be modest length and may only contain digits.  Blanks are ignored.
!   This is intended for internal use only.

  IMPLICIT NONE
  REAL (MPRKND) D1
  CHARACTER(*) CA
  CHARACTER(10) DIGITS
  INTEGER I, K, N
  PARAMETER (DIGITS = '0123456789')

! End of declaration

  D1 = 0.D0

  DO I = 1, N
    IF (CA(I:I) /= ' ') THEN
      K = INDEX (DIGITS, CA(I:I)) - 1
      IF (K < 0) THEN
        WRITE (MPLDB, 1) CA(I:I)
1       FORMAT ('*** MPDIGIN: non-digit in character string = ',A)
        CALL MPABRT (86)
      ELSEIF (K <= 9) THEN
        D1 = 10.D0 * D1 + K
      ENDIF
    ENDIF
  ENDDO

  MPDIGIN = D1
END FUNCTION MPDIGIN

CHARACTER(32) FUNCTION MPDIGOUT (A, N)

!   This converts the double precision input A to a character(32) string of
!   nonblank length N.  A must be a whole number, and N must be sufficient
!   to hold it.  This is intended for internal use only.

  IMPLICIT NONE
  REAL (MPRKND) A, D1, D2
  CHARACTER(32) CA
  CHARACTER(10) DIGITS
  PARAMETER (DIGITS = '0123456789')
  INTEGER I, K, N

! End of declaration

  CA = ' '
  D1 = ABS (A)

  DO I = N, 1, -1
    D2 = AINT (D1 / 10.D0)
    K = 1.D0 + (D1 - 10.D0 * D2)
    D1 = D2
    CA(I:I) = DIGITS(K:K)
  ENDDO

  MPDIGOUT = CA
  RETURN
END FUNCTION MPDIGOUT

SUBROUTINE MPEFORMAT (A, NB, ND, B, MPNW)

!   Converts the MPR number A into character form in the character(1) array B.
!   NB (input) is the length of the output string, and ND (input) is the
!   number of digits after the decimal point.  The format is analogous to
!   Fortran E format.  The result is left-justified among the NB cells of B.
!   The condition NB >= ND + 10 must hold or an error message will result.
!   NB cells must be available in array B.

IMPLICIT NONE
INTEGER I, IA, IX, IXP, I1, I2, J, K, MPNW, MPNW1, NA, NB, ND, NEXP, NL
CHARACTER(1) B(NB), B2(NB+50)
CHARACTER(10) DIGITS
PARAMETER (DIGITS = '0123456789')
CHARACTER(32) CA
REAL (MPRKND) AA, AN, D10W, T1
PARAMETER (D10W = 10.D0**MPNDPW)
INTEGER (MPIKND) A(0:), F(0:8), S0(0:MPNW+6), S1(0:MPNW+6)

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. NB < ND + 10) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPEFORMAT: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

IA = SIGN (INT (1, MPIKND), A(2))
NA = MIN (INT (ABS (A(2))), MPNW)
S0(0) = MPNW + 7
S1(0) = MPNW + 7
MPNW1 = MPNW + 1

!   Set f = 10.

F(0) = 9
F(1) = MPNW1
F(2) = 1
F(3) = 0
F(4) = 10
F(5) = 0
F(6) = 0

!   Determine power of ten for exponent, and scale input to within 1 and 10.

IF (NA > 0) THEN
  AA = A(4)
  IF (NA .GE. 2) AA = AA + DBLE (A(5)) / MPBDX
  T1 = LOG10 (2.D0) * MPNBT * A(3) + LOG10 (AA)

  IF (T1 .GE. 0.D0) THEN
    NEXP = T1
  ELSE
    NEXP = T1 - 1.D0
  ENDIF

  IF (NEXP == 0) THEN
    CALL MPEQ (A, S1, MPNW1)
  ELSEIF (NEXP > 0) THEN
    CALL MPNPWR (F, NEXP, S0, MPNW1)
    CALL MPDIV (A, S0, S1, MPNW1)
  ELSEIF (NEXP < 0) THEN
    CALL MPNPWR (F, - NEXP, S0, MPNW1)
    CALL MPMUL (A, S0, S1, MPNW1)
  ENDIF

!   If we didn't quite get it exactly right, multiply or divide by 10 to fix.

100 CONTINUE

  IF (S1(3) < 0) THEN
    NEXP = NEXP - 1
    CALL MPMULD (S1, 10.D0, S0, MPNW1)
    CALL MPEQ (S0, S1, MPNW1)
    GOTO 100
  ELSEIF (S1(4) >= 10) THEN
    NEXP = NEXP + 1
    CALL MPDIVD (S1, 10.D0, S0, MPNW1)
    CALL MPEQ (S0, S1, MPNW1)
    GOTO 100
  ENDIF

  S1(2) = ABS (S1(2))
ELSE
  NEXP = 0
  CALL MPEQ (A, S1, MPNW1)
ENDIF

!   Insert sign and first digit.

IX = 0
IF (IA == -1) THEN
  IX = IX + 1
  B2(IX) = '-'
ENDIF
IF (NA > 0) THEN
  AN = S1(4)
ELSE
  AN = 0.D0
ENDIF
CA = MPDIGOUT (AN, 1)
IX = IX + 1
B2(IX) = CA(1:1)
IX = IX + 1
B2(IX) = '.'
IXP = IX

!   Set f = an.

F(0) = 9
F(1) = MPNW1
F(2) = 1
F(3) = 0
F(4) = AN
F(5) = 0
F(6) = 0
CALL MPSUB (S1, F, S0, MPNW1)
CALL MPMULD (S0, D10W, S1, MPNW1)

!   Calculate the number of remaining chunks.

NL = ND / MPNDPW + 1

!   Insert the digits of the remaining words.

DO J = 1, NL
  IF (S1(2) /= 0 .AND. S1(3) == 0) THEN
    AN = S1(4)
    F(2) = 1
    F(3) = 0
    F(4) = AN
  ELSE
    F(2) = 0
    F(3) = 0
    F(4) = 0
    AN = 0.D0
  ENDIF

  CA = MPDIGOUT (AN, MPNDPW)

  DO I = 1, MPNDPW
    IX = IX + 1
    IF (IX > NB + 50) THEN
      WRITE (6, 2)
2     FORMAT ('MPEFORMAT: Insufficient space in B2 array.')
      CALL MPABRT (84)
    ENDIF
    B2(IX) = CA(I:I)
  ENDDO

  CALL MPSUB (S1, F, S0, MPNW1)
  CALL MPMULD (S0, D10W, S1, MPNW1)
ENDDO

!   Round the result.

IF (IX >= ND + 1) THEN
  I1 = INDEX (DIGITS, B2(ND+1)) - 1
  IF (I1 >= 5) THEN

!   Perform rounding, beginning at the last digit (position ND).  If the rounded
!   digit is 9, set to 0, then repeat at position one digit to left.  Continue
!   rounding if necessary until the decimal point is reached.

    DO I = ND, IXP + 1, -1
      I2 = INDEX (DIGITS, B2(I)) - 1
      IF (I2 <= 8) THEN
        B2(I) = DIGITS(I2+2:I2+2)
        GOTO 180
      ELSE
        B2(I) = '0'
      ENDIF
    ENDDO

!   We have rounded up all digits to the right of the decimal point.  If the
!   digit to the left of the decimal point is a 9, then set that digit to 1
!   and increase the exponent by one; otherwise increase that digit by one.

    IF (B2(IXP-1) == '9') THEN
      B2(IXP-1) = '1'
      NEXP = NEXP + 1
    ELSE
      I1 = INDEX (DIGITS, B2(IXP-1)) - 1
      B2(IXP-1) = DIGITS(I1+2:I1+2)
    ENDIF
  ENDIF
ENDIF

180 CONTINUE

!   Done with mantissa.  Insert exponent.

IX = ND + 1
B2(IX) = 'e'
IF (NEXP < 0) THEN
  IX = IX + 1
  B2(IX) = '-'
ENDIF
CA = MPDIGOUT (DBLE (ABS (NEXP)), 10)

DO K = 1, 10
  IF (CA(K:K) /= '0') GOTO 190
ENDDO

K = 10

190 CONTINUE

DO I = K, 10
  IX = IX + 1
  B2(IX) = CA(I:I)
ENDDO

DO I = IX + 1, NB
  B2(I) = ' '
ENDDO

!   Copy entire b2 array to B.

DO I = 1, NB
  B(I) = B2(I)
ENDDO

RETURN
END SUBROUTINE MPEFORMAT

SUBROUTINE MPFFORMAT (A, NB, ND, B, MPNW)

!   Converts the MPR number A into character form in the character(1) array B.
!   NB (input) is the length of the output string, and ND (input) is the
!   number of digits after the decimal point.  The format is analogous to
!   Fortran F format; the result is right-justified among the NB cells of B.
!   The condition NB >= ND + 10 must hold or an error message will result.
!   However, if it is found during execution that there is not sufficient space,
!   to hold all digits, the entire output field will be filled with asterisks.
!   NB cells of type character(1) must be available in B.

IMPLICIT NONE
INTEGER I, IXP, I1, I2, J, K, MPNW, NB, NB2, ND, NEXP
CHARACTER(1) B(NB), B2(NB+20)
CHARACTER(16) CA
REAL (MPRKND) T1
INTEGER (MPIKND) A(0:)
! character(16) mpdigout

! End of declaration

IF (MPNW < 4 .OR. A(0) < ABS (A(2)) + 4 .OR. NB < ND + 10) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPFFORMAT: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

NB2 = NB + 20
CALL MPEFORMAT (A, NB2, NB, B2, MPNW+1)

!   Trim off trailing blanks.

DO I = NB2, 1, -1
  IF (B2(I) /= ' ') GOTO 90
ENDDO

90 CONTINUE

NB2 = I

!   Look for the 'e' in B2.

DO K = 1, NB2
  IF (B2(K) == 'e') GOTO 100
ENDDO

WRITE (6, 2)
2 FORMAT ('*** MPFFORMAT: Syntax error in output of mpeformat')
CALL MPABRT (84)

100 CONTINUE

!   Check the sign of the exponent.

K = K + 1
IF (B2(K) == '-') THEN
  IXP = -1
  K = K + 1
ELSE
  IXP = 1
ENDIF
J = 0
CA = ' '

!   Copy the exponent into CA.

DO I = K, NB2
  J = J + 1
  IF (J <= 16) CA(J:J) = B2(I)
ENDDO

T1 = MPDIGIN (CA, J)

!   Check if there is enough space in the output array for all digits.

IF (T1 + ND + 3 > NB) THEN
  DO I = 1, NB
    B(I) = '*'
  ENDDO

  GOTO 210
ENDIF
NEXP = IXP * T1

!   Insert the sign of the number, if any.

I1 = 0
I2 = 0
IF (B2(1) == '-') THEN
  I1 = I1 + 1
  B(I1) = '-'
  I2 = I2 + 1
ENDIF

IF (NEXP == 0) THEN

!   Exponent is zero.  Copy first digit, period and ND more digits.

  DO I = 1, ND + 2
    I1 = I1 + 1
    I2 = I2 + 1
    B(I1) = B2(I2)
  ENDDO

  GOTO 200
ELSEIF (NEXP > 0) THEN

!   Exponent is positive.  Copy first digit, skip the period, then copy
!   nexp digits.

  I1 = I1 + 1
  I2 = I2 + 1
  B(I1) = B2(I2)
  I2 = I2 + 1

  DO I = 1, NEXP
    I1 = I1 + 1
    I2 = I2 + 1
    B(I1) = B2(I2)
  ENDDO

!   Insert the period.

  I1 = I1 + 1
  B(I1) = '.'

!   Copy nd more digits.

  DO I = 1, ND
    I1 = I1 + 1
    I2 = I2 + 1
    B(I1) = B2(I2)
  ENDDO

  GOTO 200
ELSE

!   Exponent is negative.  Insert a zero, then a period, then nexp - 1
!   zeroes, then the first digit, then the remaining digits up to ND total
!   fractional digits.

  I1 = I1 + 1
  B(I1) = '0'
  I1 = I1 + 1
  B(I1) = '.'

  DO I = 1, NEXP - 1
    I1 = I1 + 1
    B(I1) = '0'
  ENDDO

  I1 = I1 + 1
  I2 = I2 + 1
  B(I1) = B2(I2)
  I2 = I2 + 1

  DO I = NEXP, ND
    I1 = I1 + 1
    I2 = I2 + 1
    B(I1) = B2(I2)
  ENDDO
ENDIF

200 CONTINUE

!   Right-justify in field.

K = NB - I1

DO I = 1, I1
  B(NB-I+1) = B(NB-I-K+1)
ENDDO

DO I = 1, K
  B(I) = ' '
ENDDO

210 CONTINUE

RETURN
END SUBROUTINE MPFFORMAT

SUBROUTINE MPINP (IU, A, MPNW)

!   This routine reads the MPR number A from logical unit IU.  The digits of A
!   may span more than one line, provided that a "\" appears at the end of
!   a line to be continued (any characters after the "\" on the same line
!   are ignored).  Individual input lines may not exceed 2048 characters in
!   length, although this limit can be changed in the system parameters
!   (parameter mpnstr) in module MPFUNA.  Embedded blanks are allowed anywhere.
!   An exponent with "e" or "d" may optionally follow the numeric value.

!   A scratch array below (CHR1) holds character data for input to mpctomp.
!   It is dimensioned MPNW * (MPNDPW + 1) + 1000 (see below).  If more nonblank
!   input characters than this are input, they are ignored.

IMPLICIT NONE
INTEGER I, I1, IU, LNC1, LNCX, LN1, MPNW
CHARACTER(MPNSTR) LINE1
CHARACTER(18) VALIDC
PARAMETER (VALIDC = ' 0123456789+-.dDeE')
CHARACTER(1) CHR1(MPNW*INT(MPDPW+1)+1000)
INTEGER (MPIKND) A(0:)

! End of declaration

IF (MPNW < 4 .OR. A(0) < MPNW + 6) THEN
  WRITE (MPLDB, 1)
1 FORMAT ('*** MPINP: uninitialized or inadequately sized arrays')
  CALL MPABRT (99)
ENDIF

LNC1 = 0
LNCX = MPNW * INT (MPDPW + 1) + 1000

100 CONTINUE

READ (IU, '(a)', END = 200) LINE1

!   Find the last nonblank character.

DO I = MPNSTR, 1, -1
  IF (LINE1(I:I) /= ' ') GOTO 110
ENDDO

!   Input line is blank -- ignore.

GOTO 100

110 CONTINUE

LN1 = I

!   Scan input line, looking for valid characters.

DO I = 1, LN1
  IF (LINE1(I:I) == '\') GOTO 100
  I1 = INDEX (VALIDC, LINE1(I:I))
  IF (I1 == 0 .AND. LINE1(I:I) /= ' ') THEN
      WRITE (6, 2) LINE1(I:I)
2     FORMAT ('*** MPINP: Invalid input character = ',A)
      CALL MPABRT (87)
  ELSEIF (LINE1(I:I) .NE. ' ') THEN
    IF (LNC1 < LNCX) THEN
      LNC1 = LNC1 + 1
      CHR1(LNC1) = LINE1(I:I)
    ENDIF
  ENDIF
ENDDO

CALL MPCTOMP (CHR1, LNC1, A, MPNW)
GOTO 300

200  CONTINUE

WRITE (MPLDB, 4)
4 FORMAT ('*** MPINP: End-of-file encountered.')
CALL MPABRT (72)

300 RETURN
END SUBROUTINE MPINP

SUBROUTINE MPOUT (IU, LN, ND, A, MPNW)

!   This routine writes MPR number A to logical unit IU in E(LN,ND) format.
!   This is output on MPOUTL characters per line.  The value of MPOUTL is set
!   in the system parameters at the start of module MPFUNA.

IMPLICIT NONE
INTEGER I, IU, LN, LN1, MPNW, ND
CHARACTER(1) CHR1(LN)
CHARACTER(32) CFORM1, CFORM2
INTEGER (MPIKND) A(0:)

! End of declaration

CALL MPEFORMAT (A, LN, ND, CHR1, MPNW)

WRITE (CFORM1, 1) MPOUTL
1 FORMAT ('(',I8,'a1)')
WRITE (CFORM2, 2) MPOUTL
2 FORMAT ('(',I8,'a1,"\")')

IF (LN <= MPOUTL) THEN
  WRITE (IU, FMT = CFORM1) (CHR1(I), I = 1, LN)
ELSEIF (MOD (LN, MPOUTL) == 0) THEN
  LN1 = MPOUTL * (LN / MPOUTL) - MPOUTL
  WRITE (IU, FMT = CFORM2) (CHR1(I), I = 1, LN1)
  WRITE (IU, FMT = CFORM1) (CHR1(I), I = LN1 + 1, LN)
ELSE
  LN1 = MPOUTL * (LN / MPOUTL)
  WRITE (IU, FMT = CFORM2) (CHR1(I), I = 1, LN1)
  WRITE (IU, FMT = CFORM1) (CHR1(I), I = LN1 + 1, LN)
ENDIF

RETURN
END SUBROUTINE MPOUT

END MODULE ModLib_MPFUNC


 
MODULE ModLib_CHRPAK

!** PURPOSE OF THIS MODULE:
    ! contains routines that handle characters and strings

!** REFERENCES:
    ! These routines are from CHRPAK package by John Burkardt

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all routines which are placed in this utility
                    ! module should be available to other modules and routines.

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION A_TO_I4 ( CH )

!*****************************************************************************80
!
!! A_TO_I4 returns the index of an alphabetic character.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!    CH  A_TO_I4
!
!    'A'   1
!    'B'   2
!    ...
!    'Z'  26
!    'a'  27
!    'b'  28
!    ...
!    'z'  52
!    '$'   0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, a character.
!
!    Output, integer ( kind = 4 ) A_TO_I4, is the alphabetic index of the
!    character, between 1 and 26 if the character is a capital letter,
!    between 27 and 52 if it is lower case, and 0 otherwise.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) A_TO_I4
  INTEGER ( KIND = 4 ), PARAMETER :: CAP_SHIFT = 64
  CHARACTER CH
  INTEGER ( KIND = 4 ), PARAMETER :: LOW_SHIFT = 96

  IF ( LLE ( 'A', CH ) .AND. LLE ( CH, 'Z' ) ) THEN
    A_TO_I4 = IACHAR ( CH ) - CAP_SHIFT
  ELSE IF ( LLE ( 'a', CH ) .AND. LLE ( CH, 'z' ) ) THEN
    A_TO_I4 = IACHAR ( CH ) - LOW_SHIFT + 26
  ELSE
    A_TO_I4 = 0
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE B4_IEEE_TO_R4 ( WORD, R )

!*****************************************************************************80
!
!! B4_IEEE_TO_R4 converts a 4 byte IEEE word into an R4.
!
!  Discussion:
!
!    An "R4" value is simply a real number to be stored as a
!    variable of type "real ( kind = 4 )".
!
!    This routine does not seem to work reliably for unnormalized data.
!
!    The word containing the real value may be interpreted as:
!
!      /SEEEEEEE/EFFFFFFF/FFFFFFFF/FFFFFFFF/
!
!      /33222222/22222222/22222100/00000000/
!      /10987654/32109876/54321098/76543210/  <-- Bit numbering
!
!    where
!
!      S is the sign bit,
!      E are the exponent bits,
!      F are the mantissa bits.
!
!    The mantissa is usually "normalized"; that is, there is an implicit
!    leading 1 which is not stored.  However, if the exponent is set to
!    its minimum value, this is no longer true.
!
!    The exponent is "biased".  That is, you must subtract a bias value
!    from the exponent to get the true value.
!
!    If we read the three fields as integers S, E and F, then the
!    value of the resulting real number R can be determined by:
!
!    * if E = 255
!        if F is nonzero, then R = NaN;
!        if F is zero and S is 1, R = -Inf;
!        if F is zero and S is 0, R = +Inf;
!    * else if 0 < E then R = (-1)**(S) * 2**(E-127) * (1 + (F/2**24))
!    * else if E = 0
!        if F is nonzero, R = (-1)**(S) * 2**(E-126) * (F/2**24)
!        if F is zero and S is 1, R = -0;
!        if F is zero and S is 0, R = +0;
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    IEEE Standards Committee 754,
!    IEEE Standard for Binary Floating Point Arithmetic,
!    ANSI/IEEE Standard 754-1985,
!    SIGPLAN Notices,
!    Volume 22, Number 2, 1987, pages 9-25.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WORD, the word to be decoded.
!
!    Output, real ( kind = 4 ) R, the value of the real number.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) F
  INTEGER ( KIND = 4 ) I
  REAL ( KIND = 4 ) R
  INTEGER ( KIND = 4 ) S
  INTEGER ( KIND = 4 ) WORD
!
!  Read the fields.
!
  S = 0
  CALL MVBITS ( WORD, 31, 1, S, 0 )

  E = 0
  CALL MVBITS ( WORD, 23, 8, E, 0 )

  F = 0
  CALL MVBITS ( WORD, 0, 23, F, 0 )
!
!  Don't bother trying to return NaN or Inf just yet.
!
  IF ( E == 255 ) THEN
    R = 0.0E+00
  ELSE IF ( 0 < E ) THEN
    R = ( -1.0E+00 )**S * 2.0E+00**(E-127-23) * REAL ( 8388608 + F, KIND = 4 )
  ELSE IF ( E == 0 ) THEN
    R = ( -1.0E+00 )**S * 2.0E+00**(-126) * REAL ( F, KIND = 4 )
    DO I = 1, 23
      R = R / 2.0E+00
    END DO
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE B4_IEEE_TO_SEF ( WORD, S, E, F )

!*****************************************************************************80
!
!! B4_IEEE_TO_SEF converts an IEEE real word to S * 2**E * F format.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) WORD, a word containing an IEEE real number.
!
!    Output, integer ( kind = 4 ) S, the sign bit:
!    0, if R is nonnegative;
!    1, if R is negative.
!
!    Output, integer ( kind = 4 ) E, the exponent base 2.
!
!    Output, integer ( kind = 4 ) F, the mantissa.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) E2
  INTEGER ( KIND = 4 ) F
  INTEGER ( KIND = 4 ) S
  INTEGER ( KIND = 4 ) WORD

  S = 0
  CALL MVBITS ( WORD, 31, 1, S, 0 )

  E2 = 0
  CALL MVBITS ( WORD, 23, 8, E2, 0 )

  IF ( E2 == 255 ) THEN

    E = 128

    CALL MVBITS ( WORD, 0, 23, F, 0 )

    IF ( F == 0 ) THEN
      F = 0
    ELSE
      F = 2**23 - 1
    END IF

  ELSE IF ( 0 < E2 ) THEN

    E = E2 - 127 - 23
    F = 2**23
    CALL MVBITS ( WORD, 0, 23, F, 0 )

    DO WHILE ( MOD ( F, 2 ) == 0 )
      F = F / 2
      E = E + 1
    END DO

  ELSE IF ( E2 == 0 ) THEN

    E = E2 - 127 - 23
    F = 0
    CALL MVBITS ( WORD, 0, 23, F, 0 )

    IF ( F == 0 ) THEN

      E = 0

    ELSE

      DO WHILE ( 0 < F .AND. MOD ( F, 2 ) == 0 )
        F = F / 2
        E = E + 1
      END DO

    END IF

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE BASE_TO_I4 ( S, BASE, I )

!*****************************************************************************80
!
!! BASE_TO_I4 returns the value of an integer represented in some base.
!
!  Discussion:
!
!    BASE = 1 is allowed, in which case we allow the digits '1' and '0',
!    and we simply count the '1' digits for the result.
!
!    Negative bases between -16 and -2 are allowed.
!
!    The base -1 is allowed, and essentially does a parity check on
!    a string of 1's.
!
!  Example:
!
!        Input      Output
!    -------------  ------
!         S   BASE       I
!    ------  -----  ------
!      '101'     2       5
!    '-1000'     3     -27
!      '100'     4      16
!   '111111'     2      63
!   '111111'    -2      21
!   '111111'     1       6
!   '111111'    -1       0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string.  The elements of S are
!    blanks, a plus or minus sign, and digits.  Normally, the digits
!    are representations of integers between 0 and |BASE-1|.  In the
!    special case of base 1 or base -1, we allow both 0 and 1 as digits.
!
!    Input, integer ( kind = 4 ) BASE, the base in which the representation
!    is given.  Normally, 2 <= BASE <= 16.  However, there are two exceptions.
!
!    Output, integer ( kind = 4 ) I, the integer.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) BASE
  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) ISGN
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) STATE

  I = 0
  S_LENGTH = LEN_TRIM ( S )

  IF ( BASE == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'BASE_TO_I4 - Serious error!'
    WRITE ( *, '(a)' ) '  The input base is zero.'
    I = -1
    RETURN
  END IF

  IF ( 16 < ABS ( BASE ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'BASE_TO_I4 - Serious error!'
    WRITE ( *, '(a)' ) '  The input base is greater than 16!'
    I = -1
    RETURN
  END IF

  STATE = 0
  ISGN = 1
  ICHR = 1

  DO WHILE ( ICHR <= S_LENGTH )

    C = S(ICHR:ICHR)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( STATE == 2 ) THEN
        EXIT
      END IF
!
!  Sign, + or -.
!
    ELSE IF ( C == '-' ) THEN

      IF ( STATE /= 0 ) THEN
        EXIT
      END IF

      STATE = 1
      ISGN = -1

    ELSE IF ( C == '+' ) THEN

      IF ( STATE /= 0 ) THEN
        EXIT
      END IF

      STATE = 1

    ELSE
!
!  Digit?
!
      CALL HEX_DIGIT_TO_I4 ( C, IDIG )

      IF ( ABS ( BASE ) == 1 .AND. ( IDIG == 0 .OR. IDIG == 1 ) ) THEN

        I = BASE * I + IDIG
        STATE = 2

      ELSE IF ( 0 <= IDIG .AND. IDIG < ABS ( BASE ) ) THEN

        I = BASE * I + IDIG
        STATE = 2

      ELSE

        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'BASE_TO_I4 - Serious error!'
        WRITE ( *, '(a)' ) '  Illegal digit = "' // C // '"'
        WRITE ( *, '(a)' ) '  Conversion halted prematurely!'
        RETURN

      END IF

    END IF

    ICHR = ICHR + 1

  END DO
!
!  Once we're done reading information, we expect to be in state 2.
!
  IF ( STATE /= 2 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'BASE_TO_I4 - Serious error!'
    WRITE ( *, '(a)' ) '  Unable to decipher input!'
    RETURN
  END IF
!
!  Account for the sign.
!
  I = ISGN * I

  RETURN
END

!*************************************************************

SUBROUTINE BINARY_TO_I4 ( S, I )

!*****************************************************************************80
!
!! BINARY_TO_I4 converts a binary representation into an integer value.
!
!  Example:
!
!        S        I
!
!      '101'      5
!    '-1000'     -8
!        '1'      1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the binary representation.
!
!    Output, integer ( kind = 4 ) I, the integer whose representation was input.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) ISGN
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) STATE

  S_LENGTH = LEN_TRIM ( S )

  I = 0
  ICHR = 1
  STATE = 0
  ISGN = 1

  DO WHILE ( ICHR <= S_LENGTH )

    C = S(ICHR:ICHR)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( STATE == 2 ) THEN
        STATE = 3
      END IF
!
!  Sign, + or -.
!
    ELSE IF ( C == '-' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
        ISGN = -1
      ELSE
        STATE = -1
      END IF

    ELSE IF ( C == '+' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
      ELSE
        STATE = -1
      END IF
!
!  Digit, 0 or 1.
!
    ELSE IF ( C == '1' ) THEN

      I = 2 * I
      I = I + 1
      STATE = 2

    ELSE IF ( C == '0' ) THEN

      I = 2 * I
      STATE = 2
!
!  Illegal or unknown sign.
!
    ELSE

      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_I4 - Serious error!'
      WRITE ( *, '(a)' ) '  Illegal digit = "' // C // '"'
      WRITE ( *, '(a)' ) '  Conversion halted prematurely!'
      RETURN

    END IF

    IF ( STATE == -1 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_I4 - Serious error!'
      WRITE ( *, '(a)' ) '  Unable to decipher input!'
      RETURN
    END IF

    IF ( 3 <= STATE ) THEN
      EXIT
    END IF

    ICHR = ICHR + 1

  END DO
!
!  Apply the sign.
!
  I = ISGN * I

  RETURN
END

!*************************************************************

SUBROUTINE BINARY_TO_R4 ( S, R )

!*****************************************************************************80
!
!! BINARY_TO_R4 converts a binary representation into an R4.
!
!  Discussion:
!
!    An "R4" value is simply a real number to be stored as a
!    variable of type "real ( kind = 4 )".
!
!  Example:
!
!        S                         R
!
!    -1010.11                   -10.75
!    0.011011                     0.4218750
!    0.01010101010101010101010    0.3333333
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the binary representation.
!
!    Output, real ( kind = 4 ) R, the real number.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) POWER
  REAL ( KIND = 4 ) R
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) STATE

  S_LENGTH = LEN_TRIM ( S )

  INTVAL = 0
  ICHR = 1
  STATE = 0
  ISGN = 1
  R = 0.0E+00
  POWER = 0

  DO WHILE ( ICHR <= S_LENGTH )

    C = S(ICHR:ICHR)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( STATE == 4 ) THEN
        STATE = 5
      END IF
!
!  Sign, + or -.
!
    ELSE IF ( C == '-' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
        ISGN = -1
      ELSE
        STATE = -1
      END IF

    ELSE IF ( C == '+' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
      ELSE
        STATE = -1
      END IF
!
!  Digit, 0 or 1.
!
    ELSE IF ( C == '1' ) THEN

      INTVAL = 2 * INTVAL + 1

      IF ( STATE == 0 .OR. STATE == 1 ) THEN
        STATE = 2
      ELSE IF ( STATE == 3 ) THEN
        STATE = 4
      END IF

      IF ( STATE == 4 ) THEN
        POWER = POWER + 1
      END IF

    ELSE IF ( C == '0' ) THEN

      INTVAL = 2 * INTVAL

      IF ( STATE == 0 .OR. STATE == 1 ) THEN
        STATE = 2
      ELSE IF ( STATE == 3 ) THEN
        STATE = 4
      END IF

      IF ( STATE == 4 ) THEN
        POWER = POWER + 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( STATE <= 2 ) THEN
        STATE = 3
      ELSE
        STATE = -1
      END IF
!
!  Illegal or unknown sign.
!
    ELSE

      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_R4 - Serious error!'
      WRITE ( *, '(a)' ) '  Illegal character = "' // C // '"'
      WRITE ( *, '(a)' ) '  Conversion halted prematurely!'
      RETURN

    END IF

    IF ( STATE == -1 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_R4 - Serious error!'
      WRITE ( *, '(a)' ) '  Unable to decipher input!'
      RETURN
    END IF

    IF ( 5 <= STATE ) THEN
      EXIT
    END IF

    ICHR = ICHR + 1

  END DO
!
!  Apply the sign and the scale factor.
!
  R = REAL ( ISGN * INTVAL, KIND = 4 ) / 2.0E+00**POWER

  RETURN
END

!*************************************************************

SUBROUTINE BINARY_TO_R8 ( S, R )

!*****************************************************************************80
!
!! BINARY_TO_R8 converts a binary representation into an R8.
!
!  Discussion:
!
!    An "R8" value is simply a real number to be stored as a
!    variable of type "real ( kind = 8 )".
!
!  Example:
!
!        S                         R
!
!    -1010.11                   -10.75
!    0.011011                     0.4218750
!    0.01010101010101010101010    0.3333333
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the binary representation.
!
!    Output, real ( kind = 8 ) R, the real number.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) POWER
  REAL ( KIND = 8 ) R
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) STATE

  S_LENGTH = LEN_TRIM ( S )

  INTVAL = 0
  ICHR = 1
  STATE = 0
  ISGN = 1
  R = 0.0D+00
  POWER = 0

  DO WHILE ( ICHR <= S_LENGTH )

    C = S(ICHR:ICHR)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( STATE == 4 ) THEN
        STATE = 5
      END IF
!
!  Sign, + or -.
!
    ELSE IF ( C == '-' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
        ISGN = -1
      ELSE
        STATE = -1
      END IF

    ELSE IF ( C == '+' ) THEN

      IF ( STATE == 0 ) THEN
        STATE = 1
      ELSE
        STATE = -1
      END IF
!
!  Digit, 0 or 1.
!
    ELSE IF ( C == '1' ) THEN

      INTVAL = 2 * INTVAL + 1

      IF ( STATE == 0 .OR. STATE == 1 ) THEN
        STATE = 2
      ELSE IF ( STATE == 3 ) THEN
        STATE = 4
      END IF

      IF ( STATE == 4 ) THEN
        POWER = POWER + 1
      END IF

    ELSE IF ( C == '0' ) THEN

      INTVAL = 2 * INTVAL

      IF ( STATE == 0 .OR. STATE == 1 ) THEN
        STATE = 2
      ELSE IF ( STATE == 3 ) THEN
        STATE = 4
      END IF

      IF ( STATE == 4 ) THEN
        POWER = POWER + 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( STATE <= 2 ) THEN
        STATE = 3
      ELSE
        STATE = -1
      END IF
!
!  Illegal or unknown sign.
!
    ELSE

      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_R8 - Serious error!'
      WRITE ( *, '(a)' ) '  Illegal character = "' // C // '"'
      WRITE ( *, '(a)' ) '  Conversion halted prematurely!'
      RETURN

    END IF

    IF ( STATE == -1 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'BINARY_TO_R8 - Serious error!'
      WRITE ( *, '(a)' ) '  Unable to decipher input!'
      RETURN
    END IF

    IF ( 5 <= STATE ) THEN
      EXIT
    END IF

    ICHR = ICHR + 1

  END DO
!
!  Apply the sign and the scale factor.
!
  R = REAL ( ISGN * INTVAL, KIND = 8 ) / 2.0D+00**POWER

  RETURN
END

!*************************************************************

SUBROUTINE CH3_TO_CH_AMINO ( CH3, CH )

!*****************************************************************************80
!
!! CH3_TO_CH_AMINO converts a 3 character to a 1 character code for amino acids.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl Branden, John Tooze,
!    Introduction to Protein Structure,
!    Garland Publishing, 1991.
!
!  Parameters:
!
!    Input, character ( len = 3 ) CH3, presumably the 3 letter code for an
!    amino acid.  Lower and upper case letters are treated the same.
!
!    Output, character CH, the one letter code for the amino acid.
!    If the input code is not recognized, then CH will be '?'.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: N = 23

  CHARACTER CH
  CHARACTER, PARAMETER, DIMENSION ( N ) :: CH_TABLE = (/ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', &
    'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', &
    'X', 'Y', 'Z' /)
  CHARACTER ( LEN = 3 ) CH3
  CHARACTER ( LEN = 3 ), PARAMETER, DIMENSION ( N ) :: CH3_TABLE = (/ &
    'Ala', 'Asx', 'Cys', 'Asp', 'Glu', 'Phe', 'Gly', 'His', 'Ise', 'Lys', &
    'Leu', 'Met', 'Asn', 'Pro', 'Gln', 'Arg', 'Ser', 'Thr', 'Val', 'Trp', &
    'X  ', 'Tyr', 'Glx' /)
  INTEGER ( KIND = 4 ) I

  DO I = 1, N
    IF ( S_EQI ( CH3, CH3_TABLE(I) ) ) THEN
      CH = CH_TABLE(I)
      RETURN
    END IF
  END DO

  CH = '?'

  RETURN
END

!*************************************************************

SUBROUTINE CH4VEC_TO_I4VEC ( N, S, I4VEC )

!*****************************************************************************80
!
!! CH4VEC_TO_I4VEC converts an string of characters into an array of integers.
!
!  Discussion:
!
!    This routine can be useful when trying to write character data to an
!    unformatted direct access file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sets of 4 characters
!    in the string.
!
!    Input, character ( len = 4*N ) S, the string of characters.
!    Each set of 4 characters is assumed to represent an integer.
!
!    Output, integer ( kind = 4 ) I4VEC(N), the integers encoded in the string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4VEC(N)
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = 4 * N ) S

  DO I = 1, N
    J = 4 * ( I - 1 ) + 1
    CALL CH4_TO_I4 ( S(J:J+3), I4VEC(I) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CH4_TO_I4 ( CH4, I4 )

!*****************************************************************************80
!
!! CH4_TO_I4 converts a four character string to an integer.
!
!  Example:
!
!    Adam    1097097581
!    Bill    1114205292
!    Crow    1131573111
!    Dave    1147237989
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 4 ) CH4, the character value.
!
!    Output, integer ( kind = 4 ) I4, a corresponding integer value.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  CHARACTER C3
  CHARACTER C4
  CHARACTER ( LEN = 4 ) CH4
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) J2
  INTEGER ( KIND = 4 ) J3
  INTEGER ( KIND = 4 ) J4

  READ ( CH4, '(4a1)' ) C1, C2, C3, C4

  J1 = IACHAR ( C1 )
  J2 = IACHAR ( C2 )
  J3 = IACHAR ( C3 )
  J4 = IACHAR ( C4 )

  CALL MVBITS ( J1, 0, 8, I4,  0 )
  CALL MVBITS ( J2, 0, 8, I4,  8 )
  CALL MVBITS ( J3, 0, 8, I4, 16 )
  CALL MVBITS ( J4, 0, 8, I4, 24 )

  RETURN
END

!*************************************************************

SUBROUTINE CH4_TO_R4 ( CH4, R4 )

!*****************************************************************************80
!
!! CH4_TO_R4 converts a 4 character string to an R4.
!
!  Discussion:
!
!    The MVBITS routine requires the two word arguments to be of the
!    same arithmetic type, so we first need to use the TRANSFER
!    function so that the data inside an integer word can be copied
!    verbatin into a real.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 4 ) CH4, the character value.
!
!    Output, real ( kind = 4 ) R4, a corresponding real value.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  CHARACTER C3
  CHARACTER C4
  CHARACTER ( LEN = 4 ) CH4
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) J
  REAL ( KIND = 4 ) R4

  READ ( CH4, '(4a1)' ) C1, C2, C3, C4

  J = IACHAR ( C1 )
  CALL MVBITS ( J, 0, 8, I4,  0 )

  J = IACHAR ( C2 )
  CALL MVBITS ( J, 0, 8, I4,  8 )

  J = IACHAR ( C3 )
  CALL MVBITS ( J, 0, 8, I4, 16 )

  J = IACHAR ( C4 )
  CALL MVBITS ( J, 0, 8, I4, 24 )

  R4 = TRANSFER ( I4, R4 )

  RETURN
END

!*************************************************************

SUBROUTINE CHR4_TO_8 ( S1, S2 )

!*****************************************************************************80
!
!! CHR4_TO_8 replaces pairs of hexadecimal digits by a character.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be decoded.
!
!    Output, character ( len = * ) S2, the output string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) K1
  INTEGER ( KIND = 4 ) NCHAR2
  INTEGER ( KIND = 4 ) NROOM
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
!
!  Set S1_LENGTH to the number of characters to be copied.
!
  NCHAR2 = 0
  S1_LENGTH = LEN ( S1 )

  IF ( MOD ( S1_LENGTH, 2 ) == 1 ) THEN
    S1_LENGTH = S1_LENGTH - 1
  END IF

  IF ( S1_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHR4_TO_8 - Serious error!'
    WRITE ( *, '(a)' ) '  The input string has nonpositive length!'
    RETURN
  END IF
!
!  Make sure we have enough room.
!
  NROOM = LEN ( S2 )

  IF ( 2 * NROOM < S1_LENGTH ) THEN

    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHR4_TO_8 - Warning!'
    WRITE ( *, '(a)' ) '  Not enough room in the output string.'
    WRITE ( *, '(a,i8)' ) '  Positions available = ', NROOM
    WRITE ( *, '(a,i8)' ) '  Positions needed =    ', S1_LENGTH / 2
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) '  The program will drop excess characters.'

    S1_LENGTH = 2 * NROOM

  END IF

  DO I = 1, S1_LENGTH, 2

    CALL HEX_DIGIT_TO_I4 ( S1(I:I), J1 )
    CALL HEX_DIGIT_TO_I4 ( S1(I+1:I+1), K1 )
!
!  Make sure that the values of J1 and K1 are legal.  If not,
!  set I1 so that it returns a blank character.
!
    IF ( ( 0 <= J1 .AND. J1 <= 15) .AND. ( 0 <= K1 .AND. K1 <= 15) ) THEN
      I1 = 16 * J1 + K1
    ELSE
      I1 = 0
    END IF

    NCHAR2 = NCHAR2 + 1
    S2(NCHAR2:NCHAR2) = ACHAR ( I1 )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHR8_TO_4 ( S1, S2 )

!*****************************************************************************80
!
!! CHR8_TO_4 replaces characters by a pair of hexadecimal digits.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    Unprintable characters (0 through 31, or 127 through 255)
!    can be displayed.
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
!    Input, character ( len = * ) S1, the string to be replaced.
!
!    Output, character ( len = * ) S2, the output string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) K1
  INTEGER ( KIND = 4 ) NROOM
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
!
!  Set S1_LENGTH to the number of characters to be copied.
!
  S1_LENGTH = LEN ( S1 )

  IF ( S1_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHR8_TO_4 - Serious error!'
    WRITE ( *, '(a)' ) '  The input string has nonpositive length!'
    RETURN
  END IF
!
!  Make sure we have enough room.
!
  NROOM = LEN ( S2 )

  IF ( NROOM < 2 * S1_LENGTH ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHR8_TO_4 - Warning!'
    WRITE ( *, '(a)' ) '  The output string isn''T LONG ENOUGH TO HOLD'
    WRITE ( *, '(a)' ) '  all the information!'
    WRITE ( *, '(a,i8)' ) '  Positions available: ', NROOM
    WRITE ( *, '(a,i8)' ) '  Positions needed:    ', 2 * S1_LENGTH
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) '  We will do a partial conversion.'

    S1_LENGTH = NROOM / 2

  END IF

  J = 0

  DO I = 1, S1_LENGTH

    C = S1(I:I)

    I1 = IACHAR ( C )
!
!  Compute J1 and K1 so that I1 = J1*16+K1.
!
    J1 = I1 / 16
    K1 = I1 - 16 * J1

    J = J + 1
    CALL I4_TO_HEX_DIGIT ( J1, S2(J:J) )

    J = J + 1
    CALL I4_TO_HEX_DIGIT ( K1, S2(J:J) )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHRASC ( IASCII, NASCII, STRING )

!*****************************************************************************80
!
!! CHRASC converts a vector of ASCII codes into character strings.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
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
!    Input, integer ( kind = 4 ) IASCII(NASCII), a vector presumed to
!    contain entries between 0 and 255, the ASCII codes of
!    individual characters.
!
!    Input, integer ( kind = 4 ) NASCII, the number of ASCII codes input.
!
!    Output, character ( len = * ) STRING(*).  STRING is assumed to be
!    a vector of sufficient size to contain the information
!    input in IASCII.
!
!    The length of the strings is determined via the
!    LEN function.  The entries in IASCII are converted and
!    stored into the characters of STRING(1), and when that is
!    full, into STRING(2) and so on until all the entries have
!    been converted.
!
!    If any entry of IASCII is less than 0, or greater than
!    255, it is handled as though it were 0.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IASCII(*)
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ITEMP
  INTEGER ( KIND = 4 ) IX
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) NASCII
  INTEGER ( KIND = 4 ) NCHAR
  CHARACTER ( LEN = * ) STRING(*)

  NCHAR = LEN ( STRING(1) )

  IX = 0
  IHI = ( (NASCII-1) / NCHAR ) + 1

  DO I = 1, IHI
    DO J = 1, NCHAR

      IX = IX + 1

      IF ( NASCII <= IX ) THEN
        RETURN
      END IF

      ITEMP = IASCII ( IX )

      IF ( ITEMP < 0 .OR. 255 < ITEMP ) THEN
        ITEMP = 0
      END IF

      STRING(I)(J:J) = ACHAR ( ITEMP )

    END DO
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHRASS ( S, LHS, RHS )

!*****************************************************************************80
!
!! CHRASS "understands" an assignment statement of the form LHS = RHS.
!
!  Discussion:
!
!    CHRASS returns a string containing the left hand side, and another
!    string containing the right hand side.
!
!    Leading and trailing spaces are removed from the right hand side
!    and the left hand side.
!
!  Example:
!
!    S                            Rhs               Lhs
!
!    'a = 1.0'                    'a'               '1.0'
!    'n = -17'                      'n'               '-17'
!    'scale = +5.3E-2'            'scale'           '+5.3E-2'
!    'filename = myprog.f'        'filename'        'myprog.f'
!    '= A pot of gold'            ' '               'A pot of gold'
!    'Fred'                       'Fred'            ' '
!    '= Bob'                      ' '               'Bob'
!    '1=2, 2=3, 3=4'              '1'               '2, 2=3, 3=4'
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
!    Input, character ( len = * ) S, the assignment statement to be broken up.
!
!    Output, character ( len = * ) LHS.
!
!    LHS contains the left hand side of the assignment statement.
!
!    Normally, this will be the name of a variable, which is
!    assumed to be whatever appears before the first equals
!    sign in the string.
!
!    If the input line was blank, then LHS will equal ' '.
!
!    If the input line contains an equal sign, but nothing
!    before the equals sign except blanks, then LHS will be ' '.
!
!    If the input line does not contain an "=" sign, then
!    NAME will contain the text of the whole line.
!
!    If an error occurred while trying to process the
!    input line, NAME will contain the text of the line..
!
!    If the line began with "#", then NAME will contain the
!    text of the line.
!
!    If the line equals "end-of-input", then NAME will contain
!    the text of the line.
!
!    Output, character ( len = * ) RHS.
!
!    RHS contains the right hand side of the assignment statement.
!
!    RHS is whatever appears on the right hand side of the
!    first equals sign in the string.
!
!    If S is blank, then RHS is ' '.
!
!    If the string contains no equals sign, then RHS is ' '.
!
!    If the string contains nothing to the right of the first equals
!    sign, but blanks, then RHS is ' '.
!
!    The user may read the data in RHS by
!
!      calling S_TO_R8 to read real ( kind = 8 ) data,
!      calling CHRCTR to read real data,
!      calling CHRCTI to read integer data,
!      calling CHRCTL to read logical data,
!      calling CHRCTC to read complex data.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) IEQUAL
  CHARACTER ( LEN = * ) LHS
  CHARACTER ( LEN = * ) RHS
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
!
!  Set default values
!
  LHS = ' '
  RHS = ' '
!
!  Find the last nonblank.
!
  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 0 ) THEN
    RETURN
  END IF
!
!  Look for the first equals sign.
!
  IEQUAL = INDEX ( S, '=' )
!
!  If no equals sign, then LHS = S and return.
!
  IF ( IEQUAL == 0 ) THEN
    FIRST = S_FIRST_NONBLANK ( S )
    LHS = S(FIRST:S_LENGTH)
    RETURN
  END IF
!
!  Otherwise, copy LHS = S(1:IEQUAL-1), RHS = S(IEQUAL+1:).
!
  LHS = S(1:IEQUAL-1)

  IF ( IEQUAL + 1 <= S_LENGTH ) THEN
    RHS = S(IEQUAL+1:)
  END IF
!
!  Now shift the strings to the left.
!
  LHS = ADJUSTL ( LHS )
  RHS = ADJUSTL ( RHS )

  RETURN
END

!*************************************************************

SUBROUTINE CHRA_TO_S ( S1, S2 )

!*****************************************************************************80
!
!! CHRA_TO_S replaces control characters by printable symbols.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Table:
!
!    IACHAR(c)    Symbol
!    --------    ------
!      0          ^@
!      1          ^A
!    ...         ...
!     31          ^_
!     32         (space)
!    ...         ...
!    126         ~
!    127         DEL
!    128         !^@
!    ...         ...
!    159         !^_
!    160         !(space)
!    ...         ...
!    254         !~
!    255         !DEL
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
!    Input, character ( len = * ) S1, the string to be operated on.
!
!    Output, character ( len = * ) S2, a copy of S1, except that each
!    control character has been replaced by a symbol.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) LSYM
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = 4 ) SYM

  S1_LENGTH = LEN_TRIM ( S1 )
  S2 = ' '

  PUT = 1

  DO GET = 1, S1_LENGTH

    IF ( CH_IS_CONTROL ( S1(GET:GET) ) ) THEN

      CALL CH_TO_SYM ( S1(GET:GET), SYM )
      LSYM = LEN_TRIM ( SYM )

      S2(PUT:PUT+LSYM-1) = SYM(1:LSYM)
      PUT = PUT + LSYM

    ELSE

      S2(PUT:PUT) = S1(GET:GET)
      PUT = PUT + 1

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHRCTF ( S, ITOP, IBOT, IERROR, LENGTH )

!*****************************************************************************80
!
!! CHRCTF reads an integer or rational fraction from a string.
!
!  Discussion:
!
!    The integer may be in real format, for example '2.25'.  The routine
!    returns ITOP and IBOT.  If the input number is an integer, ITOP
!    equals that integer, and IBOT is 1.  But in the case of 2.25,
!    the program would return ITOP = 225, IBOT = 100.
!
!    Legal input is:
!
!      blanks,
!      initial sign,
!      blanks,
!      integer ( kind = 4 ) part,
!      decimal point,
!      fraction part,
!      'E' or 'e' or 'D' or 'd', exponent marker,
!      exponent sign,
!      exponent integer part,
!      blanks,
!      final comma or semicolon.
!
!    with most quantities optional.
!
!  Example:
!
!    S               ITOP      IBOT
!
!    '1'               1         1
!    '     1   '       1         1
!    '1A'              1         1
!    '12,34,56'        12        1
!    '  34 7'          34        1
!    '-1E2ABCD'        -100      1
!    '-1X2ABCD'        -1        1
!    ' 2E-1'           2         10
!    '23.45'           2345      100
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate when no more characters
!    can be read to form a legal integer.  Blanks, commas,
!    or other nonnumeric data will, in particular, cause
!    the conversion to halt.
!
!    Output, integer ( kind = 4 ) ITOP, the integer read from the string,
!    assuming that no negative exponents or fractional parts
!    were used.  Otherwise, the 'integer' is ITOP/IBOT.
!
!    Output, integer ( kind = 4 ) IBOT, the integer divisor required to
!    represent numbers which are in real format or have a
!    negative exponent.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0 if no errors,
!    Value of IHAVE when error occurred otherwise.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read from
!    the string to form the number.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) IBOT
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IHAVE
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) ITERM
  INTEGER ( KIND = 4 ) ITOP
  INTEGER ( KIND = 4 ) JSGN
  INTEGER ( KIND = 4 ) JTOP
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) NDIG
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  IERROR = 0
  LENGTH = -1
  ISGN = 1
  ITOP = 0
  IBOT = 1
  JSGN = 1
  JTOP = 0
  IHAVE = 1
  ITERM = 0

  DO WHILE ( LENGTH < S_LENGTH )

    LENGTH = LENGTH + 1
    C = S(LENGTH+1:LENGTH+1)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( IHAVE == 2 ) THEN

      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        ITERM = 1
      ELSE IF ( 1 < IHAVE ) THEN
        IHAVE = 11
      END IF
!
!  Comma.
!
    ELSE IF ( C == ',' .OR. C == ';' ) THEN

      IF ( IHAVE /= 1 ) THEN
        ITERM = 1
        IHAVE = 12
        LENGTH = LENGTH + 1
      END IF
!
!  Minus sign.
!
    ELSE IF ( C == '-' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
        ISGN = -1
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
        JSGN = -1
      ELSE
        ITERM = 1
      END IF
!
!  Plus sign.
!
    ELSE IF ( C == '+' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
      ELSE
        ITERM = 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( IHAVE < 4 ) THEN
        IHAVE = 4
      ELSE
        ITERM = 1
      END IF
!
!  Exponent marker.
!
    ELSE IF ( CH_EQI ( C, 'E' ) .OR. CH_EQI ( C, 'D' ) ) THEN

      IF ( IHAVE < 6 ) THEN
        IHAVE = 6
      ELSE
        ITERM = 1
      END IF
!
!  Digit.
!
    ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) .AND. IHAVE < 11 ) THEN

      IF ( IHAVE <= 2 ) THEN
        IHAVE = 3
      ELSE IF ( IHAVE == 4 ) THEN
        IHAVE = 5
      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        IHAVE = 8
      END IF

      CALL CH_TO_DIGIT ( C, NDIG )

      IF ( IHAVE == 3 ) THEN
        ITOP = 10 * ITOP + NDIG
      ELSE IF ( IHAVE == 5 ) THEN
        ITOP = 10 * ITOP + NDIG
        IBOT = 10 * IBOT
      ELSE IF ( IHAVE == 8 ) THEN
        JTOP = 10 * JTOP + NDIG
      END IF
!
!  Anything else is regarded as a terminator.
!
    ELSE
      ITERM = 1
    END IF

    IF ( ITERM == 1 ) THEN
      EXIT
    END IF

  END DO

  IF ( ITERM /= 1 .AND. LENGTH + 1 == S_LENGTH ) THEN
    LENGTH = S_LENGTH
  END IF
!
!  Number seems to have terminated.  Have we got a legal number?
!
  IF ( IHAVE == 1 .OR. IHAVE == 2 .OR. IHAVE == 6 .OR. IHAVE == 7 ) THEN
    IERROR = IHAVE
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHRCTF - Serious error!'
    WRITE ( *, '(a)' ) '  Illegal input:' // TRIM ( S )
    RETURN
  END IF
!
!  Number seems OK.  Form it.
!
  IF ( JSGN == 1 ) THEN
    ITOP = ITOP * 10**JTOP
  ELSE
    IBOT = IBOT * 10**JTOP
  END IF

  ITOP = ISGN * ITOP

  RETURN
END

!*************************************************************

SUBROUTINE CHRCTG ( S, ITOP, IBOT, IERROR, LENGTH )

!*****************************************************************************80
!
!! CHRCTG reads an integer, decimal fraction or a ratio from a string.
!
!  Discussion:
!
!    CHRCTG returns an equivalent ratio (ITOP/IBOT).
!
!    If the input number is an integer, ITOP equals that integer, and
!    IBOT is 1.   But in the case of 2.25, the program would return
!    ITOP = 225, IBOT = 100.
!
!    A ratio is either
!      a number
!    or
!      a number, "/", a number.
!
!    A "number" is defined as:
!
!      blanks,
!      initial sign,
!      integer ( kind = 4 ) part,
!      decimal point,
!      fraction part,
!      E,
!      exponent sign,
!      exponent integer part,
!      blanks,
!      final comma or semicolon,
!
!    Examples of a number:
!
!      15, 15.0, -14E-7, E2, -12.73E-98, etc.
!
!    Examples of a ratio:
!
!      15, 1/7, -3/4.9, E2/-12.73
!
!  Example:
!
!    S               ITOP      IBOT
!
!    '1'               1         1
!    '     1   '       1         1
!    '1A'              1         1
!    '12,34,56'        12        1
!    '  34 7'          34        1
!    '-1E2ABCD'        -100      1
!    '-1X2ABCD'        -1        1
!    ' 2E-1'           2         10
!    '23.45'           2345      100
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
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate when no more characters
!    can be read to form a legal integer.  Blanks, commas,
!    or other nonnumeric data will, in particular, cause
!    the conversion to halt.
!
!    Output, integer ( kind = 4 ) ITOP, the integer read from the string,
!    assuming that no negative exponents or fractional parts
!    were used.  Otherwise, the 'integer' is ITOP/IBOT.
!
!    Output, integer ( kind = 4 ) IBOT, the integer divisor required to
!    represent numbers which are in decimal format or have a
!    negative exponent.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0 if no errors,
!    Value of IHAVE in CHRCTF when error occurred otherwise.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IBOT
  INTEGER ( KIND = 4 ) IBOTB
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ITEMP
  INTEGER ( KIND = 4 ) ITOP
  INTEGER ( KIND = 4 ) ITOPB
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) LENGTH2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  ITOP = 0
  IBOT = 1
  LENGTH = 0

  CALL CHRCTF ( S, ITOP, IBOT, IERROR, LENGTH )

  IF ( IERROR /= 0) THEN
    RETURN
  END IF
!
!  The number is represented as a fraction.
!  If the next nonblank character is "/", then read another number.
!
  S_LENGTH = LEN_TRIM ( S )

  DO I = LENGTH + 1, S_LENGTH - 1

    IF ( S(I:I) == '/' ) THEN

      CALL CHRCTF ( S(I+1:), ITOPB, IBOTB, IERROR, LENGTH2 )

      IF ( IERROR /= 0 ) THEN
        RETURN
      END IF

      ITOP = ITOP * IBOTB
      IBOT = IBOT * ITOPB

      ITEMP = I4_GCD ( ITOP, IBOT )

      ITOP = ITOP / ITEMP
      IBOT = IBOT / ITEMP

      LENGTH = I + LENGTH2

      RETURN

    ELSE IF ( S(I:I) /= ' ' ) THEN

      RETURN

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHRCTI2 ( S, INTVAL, IERROR, LENGTH )

!*****************************************************************************80
!
!! CHRCTI2 finds and reads an integer from a string.
!
!  Discussion:
!
!    The routine is given a string which may contain one or more integers.
!    Starting at the first character position, it looks for the first
!    substring that could represent an integer.  If it finds such a string,
!    it returns the integer's value, and the position of the last character
!    read.
!
!  Example:
!
!    S               INTVAL      LENGTH
!
!    'Apollo 13'       13          9
!    '     1   '       1           6
!    '1A'              1           1
!    '12,34,56'        12          2
!    'A1A2A3'          1           2
!    '-1E2ABCD'        -1          2
!    '-X20ABCD'        20          4
!    '23.45'           23          2
!    ' N = 34, $'      34          7
!    'Oops!'           0           0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!    Reading will begin at position 1 and terminate at the end of the
!    string, or when no more characters can be read to form a legal integer.
!    Blanks, commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, integer ( kind = 4 ) INTVAL, the integer read from the string,
!    or 0 if there was an error.
!
!    Output, integer ( kind = 4 ) IERROR, 0 an integer was found,
!    1 if no integer found.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IHAVE
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) ITERM
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )
  IERROR = 0
  I = 0
  ISGN = 1
  INTVAL = 0
  IHAVE = 0
  ITERM = 0
!
!  Examine the next character.
!
  DO WHILE ( ITERM /= 1 )

    I = I + 1

    IF ( S_LENGTH < I ) THEN

      ITERM = 1

    ELSE

      C = S(I:I)
!
!  Minus sign.
!
      IF ( C == '-' ) THEN

        IF ( IHAVE == 0 ) THEN
          IHAVE = 1
          ISGN = -1
        ELSE
          ITERM = 1
        END IF
!
!  Plus sign.
!
      ELSE IF ( C == '+' ) THEN

        IF ( IHAVE == 0 ) THEN
          IHAVE = 1
        ELSE
          ITERM = 1
        END IF
!
!  Digit.
!
      ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN

        IHAVE = 2

        CALL CH_TO_DIGIT ( C, IDIG )

        INTVAL = 10 * INTVAL + IDIG
!
!  Blank or TAB.
!
      ELSE

        IF ( IHAVE == 2 ) THEN
          ITERM = 1
        ELSE
          IHAVE = 0
        END IF

      END IF

    END IF

  END DO

  IF ( IHAVE == 2 ) THEN
    LENGTH = I - 1
    INTVAL = ISGN * INTVAL
  ELSE
    IERROR = 0
    LENGTH = 0
    INTVAL = 0
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CHRCTP ( S, CVAL, IERROR, LENGTH )

!*****************************************************************************80
!
!! CHRCTP reads a parenthesized complex number from a string.
!
!  Discussion:
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!
!       2 left parenthesis, REQUIRED
!
!       3 blanks
!       4 '+' or '-' sign,
!       5 blanks
!       6 integer part,
!       7 decimal point,
!       8 fraction part,
!       9 'E' or 'e' or 'D' or 'd', exponent marker,
!      10 exponent sign,
!      11 exponent integer part,
!      12 exponent decimal point,
!      13 exponent fraction part,
!      14 blanks,
!
!      15 comma, REQUIRED
!
!      16 blanks
!      17 '+' or '-' sign,
!      18 blanks
!      19 integer part,
!      20 decimal point,
!      21 fraction part,
!      22 'E' or 'e' or 'D' or 'd', exponent marker,
!      23 exponent sign,
!      24 exponent integer part,
!      25 exponent decimal point,
!      26 exponent fraction part,
!      27 blanks,
!
!      28 right parenthesis, REQUIRED
!
!  Example:
!
!    S                   CVAL      IERROR     LENGTH
!
!    '(1, 1)'              1 + 1 i   0           5
!    '( 20 , 99 )'        20+99i     0          11
!    '(-1.2E+2, +30E-2)'  -120+0.3i  0          17
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
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, complex ( kind = 4 ) CVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, the string was empty.
!    2, Did not find left parenthesis.
!    3, Could not read A correctly.
!    4, Did not find the comma.
!    5, Could not read B correctly.
!    6, Did not find right parenthesis.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read.
!
  IMPLICIT NONE

  REAL ( KIND = 4 ) AVAL
  REAL ( KIND = 4 ) BVAL
  CHARACTER C
  COMPLEX   ( KIND = 4 ) CVAL
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
!
!  Initialize the return arguments.
!
  IERROR = 0
  AVAL = 0
  BVAL = 0
  CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
  LENGTH = 0
!
!  Get the length of the line, and if it's zero, return.
!
  IF ( LEN_TRIM ( S ) <= 0 ) THEN
    IERROR = 1
    RETURN
  END IF
!
!  Is the next character a left parenthesis, like it must be?
!
  CALL NEXCHR ( S, ICHR, C )

  IF ( C /= '(' ) THEN
    IERROR = 2
    RETURN
  END IF

  LENGTH = ICHR
!
!  Is the next character a comma?  Then a = 0.
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

  IF ( C == ',' ) THEN
    AVAL = 0
    LENGTH = LENGTH + ICHR
!
!  Read the A value.
!
  ELSE

    CALL S_TO_R4 ( S(LENGTH+1:), AVAL, IERROR, ICHR )

    IF ( IERROR /= 0 ) THEN
      IERROR = 3
      LENGTH = 0
      RETURN
    END IF

    LENGTH = LENGTH + ICHR
!
!  Expect to read the comma
!
    IF ( S(LENGTH:LENGTH) /= ',' ) THEN
      IERROR = 4
      LENGTH = 0
      RETURN
    END IF

  END IF
!
!  Is the next character a left parenthesis?  Then b = 0.
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

  IF ( C == ')' ) THEN
    BVAL = 0
    LENGTH = LENGTH + ICHR
!
!  Read the B value.
!
  ELSE

    CALL S_TO_R4 ( S(LENGTH+1:), BVAL, IERROR, ICHR )

    IF ( IERROR /= 0 ) THEN
      IERROR = 5
      LENGTH = 0
      RETURN
    END IF

    LENGTH = LENGTH + ICHR
!
!  Expect to read the right parenthesis.
!
    CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

    IF ( C /= ')' ) THEN
      IERROR = 6
      LENGTH = 0
      RETURN
    END IF

  END IF

  LENGTH = LENGTH + ICHR

  CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )

  RETURN
END

!*************************************************************

SUBROUTINE CHRS_TO_A ( S1, S2 )

!*****************************************************************************80
!
!! CHRS_TO_A replaces all control symbols by control characters.
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
!    Input, character ( len = * ) S1 is the string to be operated on.
!
!    Output, character ( len = * ) S2 is a copy of S1, except that each
!    control symbol has been replaced by a control character.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) NCHAR2
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN_TRIM ( S1 )
  NCHAR2 = LEN ( S2 )

  IHI = 0
  PUT = 0

  DO

    IF ( S1_LENGTH <= IHI ) THEN
      RETURN
    END IF

    ILO = IHI + 1

    CALL SYM_TO_CH ( S1(ILO:), C, IHI )

    PUT = PUT + 1

    IF ( NCHAR2 < PUT ) THEN
      EXIT
    END IF

    S2(PUT:PUT) = C

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHVEC2_PRINT ( M, A, N, B, TITLE )

!*****************************************************************************80
!
!! CHVEC2_PRINT prints two vectors of characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the length of the first sequence.
!
!    Input, character A(M), the first sequence.
!
!    Input, integer ( kind = 4 ) N, the length of the second sequence.
!
!    Input, character B(N), the second sequence.
!
!    Input, character ( len = * ), a title.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) N

  CHARACTER A(M)
  CHARACTER AI
  CHARACTER B(N)
  CHARACTER BI
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) TITLE

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( TITLE )
  WRITE ( *, '(a)' ) ' '

  DO I = 1, MAX ( M, N )

    IF ( I <= M ) THEN
      AI = A(I)
    ELSE
      AI = ' '
    END IF

    IF ( I <= N ) THEN
      BI = B(I)
    ELSE
      BI = ' '
    END IF

    WRITE ( *, '(i3,2x,a1,2x,a1)' ) I, AI, BI

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHVEC_PERMUTE ( N, A, P )

!*****************************************************************************80
!
!! CHVEC_PERMUTE permutes a character vector in place.
!
!  Discussion:
!
!    This routine permutes an array of character "objects", but the same
!    logic can be used to permute an array of objects of any arithmetic
!    type, or an array of objects of any complexity.  The only temporary
!    storage required is enough to store a single object.  The number
!    of data movements made is N + the number of cycles of order 2 or more,
!    which is never more than N + N/2.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (   2,   4,   5,   1,   3 )
!      A = (  'B', 'D', 'E', 'A', 'C' )
!
!    Output:
!
!      A    = ( 'A', 'B', 'C', 'D', 'E' ).
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
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input/output, character A(N), the array to be permuted.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER A(N)
  CHARACTER A_TEMP
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) ISTART
  INTEGER ( KIND = 4 ) P(N)

  CALL PERM_CHECK ( N, P, IERROR )

  IF ( IERROR /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CHVEC_PERMUTE - Fatal error!'
    WRITE ( *, '(a)' ) '  The input array does not represent'
    WRITE ( *, '(a)' ) '  a proper permutation.  In particular, the'
    WRITE ( *, '(a,i8)' ) '  array is missing the value ', IERROR
    RETURN
  END IF
!
!  Search for the next element of the permutation that has not been used.
!
  DO ISTART = 1, N

    IF ( P(ISTART) < 0 ) THEN

      CYCLE

    ELSE IF ( P(ISTART) == ISTART ) THEN

      P(ISTART) = -P(ISTART)
      CYCLE

    ELSE

      A_TEMP = A(ISTART)
      GET = ISTART
!
!  Copy the new value into the vacated entry.
!
      DO

        PUT = GET
        GET = P(GET)

        P(PUT) = -P(PUT)

        IF ( GET < 1 .OR. N < GET ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'CHVEC_PERMUTE - Fatal error!'
          WRITE ( *, '(a)' ) '  "get" character is out of bounds.'
          RETURN
        END IF

        IF ( GET == ISTART ) THEN
          A(PUT) = A_TEMP
          EXIT
        END IF

        A(PUT) = A(GET)

      END DO

    END IF

  END DO
!
!  Restore the signs of the entries.
!
  P(1:N) = - P(1:N)

  RETURN
END

!*************************************************************

SUBROUTINE CHVEC_PRINT ( N, A, TITLE )

!*****************************************************************************80
!
!! CHVEC_PRINT prints a character vector.
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
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, character A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER A(N)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = 255 ) STRING
  CHARACTER ( LEN = * ) TITLE

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( TITLE )
  WRITE ( *, '(a)' ) ' '

  DO ILO = 1, N, 80
    IHI = MIN ( ILO + 79, N )
    STRING = ' '
    DO I = ILO, IHI
      J = I + 1 - ILO
      IF ( CH_IS_PRINTABLE ( A(I) ) ) THEN
        STRING(J:J) = A(I)
      END IF
    END DO

    WRITE ( *, '(a)' ) TRIM ( STRING )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHVEC_REVERSE ( N, X )

!*****************************************************************************80
!
!! CHVEC_REVERSE reverses the elements of a character vector.
!
!  Example:
!
!    Input:
!
!      N = 4, X = ( 'L', 'I', 'V', 'E' ).
!
!    Output:
!
!      X = ( 'E', 'V', 'I', 'L' ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, character X(N), the array to be reversed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER CVAL
  INTEGER ( KIND = 4 ) I
  CHARACTER X(N)

  DO I = 1, N/2
    CVAL = X(I)
    X(I) = X(N+1-I)
    X(N+1-I) = CVAL
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CHVEC_TO_S ( N, CHVEC, S )

!*****************************************************************************80
!
!! CHVEC_TO_S converts a character vector to a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of characters to convert.
!
!    Input, character CHVEC(N), a vector of characters.
!
!    Output, character ( len = * ) S, a string of characters.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER CHVEC(N)
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S

  DO I = 1, MIN ( N, LEN ( S ) )
    S(I:I) = CHVEC(I)
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CH_CAP ( CH )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, the character to capitalize.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) ITEMP

  ITEMP = IACHAR ( CH )

  IF ( 97 <= ITEMP .AND. ITEMP <= 122 ) THEN
    CH = ACHAR ( ITEMP - 32 )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_CHVEC_ADD ( N, CHVEC, COUNT )

!*****************************************************************************80
!
!! CH_COUNT_CHVEC_ADD adds a character vector to a character count.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, character CHVEC(N), a vector of characters.
!
!    Input/output, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) COUNT(0:255)
  CHARACTER CHVEC(N)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J

  DO I = 1, N
    J = IACHAR ( CHVEC(I) )
    COUNT(J) = COUNT(J) + 1
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_FILE_ADD ( FILE_NAME, COUNT )

!*****************************************************************************80
!
!! CH_COUNT_FILE_ADD adds characters in a file to a character count.
!
!  Discussion:
!
!    Each line is counted up to the last nonblank.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILE_NAME, the name of the file to examine.
!
!    Output, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) COUNT(0:255)
  CHARACTER ( LEN = * ) FILE_NAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILE_NAME, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'CH_COUNT_FILE_ADD - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(a)' ) '    ' // TRIM ( FILE_NAME )
    RETURN
  END IF

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CALL CH_COUNT_S_ADD ( TRIM ( LINE ), COUNT )

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_HISTOGRAM_PRINT ( COUNT, TITLE )

!*****************************************************************************80
!
!! CH_COUNT_HISTOGRAM_PRINT prints a histogram of a set of character counts.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  IMPLICIT NONE

  CHARACTER C
  CHARACTER ( LEN = 4 ) CH4(0:255)
  INTEGER ( KIND = 4 ) COUNT(0:255)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) PERCENT
  INTEGER ( KIND = 4 ) ROW
  CHARACTER ( LEN = 4 ) S(0:255)
  CHARACTER ( LEN = * ) TITLE
  INTEGER ( KIND = 4 ) TOTAL

  TOTAL = SUM ( COUNT )

  DO I = 0, 255
    C = ACHAR ( I )
    CALL CH_TO_SYM ( C, CH4(I) )
  END DO

  DO I = 0, 255

    IF ( TOTAL == 0 ) THEN
      PERCENT = 0
    ELSE
      PERCENT = NINT ( REAL ( 100 * COUNT(I), KIND = 4 ) &
        / REAL ( TOTAL, KIND = 4 ) )
    END IF

    IF ( PERCENT == 0 ) THEN
      S(I) = '   .'
    ELSE
      WRITE ( S(I), '(i4)' ) PERCENT
    END IF

  END DO

  IF ( 0 < LEN_TRIM ( TITLE ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) TRIM ( TITLE )
  END IF

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Character Histogram (Percentages).'
  WRITE ( *, '(a)' ) ' '

  DO ROW = 1, 16
    ILO = ( ROW - 1 ) * 16
    IHI =   ROW       * 16 - 1
    WRITE ( *, '(2x,i3,a4,i3,3x,16a4)' ) ILO, ' to ', IHI, CH4(ILO:IHI)
    WRITE ( *, '(12x,16a4)' ) S(ILO:IHI)
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_INIT ( COUNT )

!*****************************************************************************80
!
!! CH_COUNT_INIT initializes a character count.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) COUNT(0:255)

  COUNT(0:255) = 0

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_PRINT ( COUNT, TITLE )

!*****************************************************************************80
!
!! CH_COUNT_PRINT prints a set of character counts.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
!    Input, character ( len = * ) TITLE, a title to be printed.
!
  IMPLICIT NONE

  CHARACTER C
  CHARACTER ( LEN = 4 ) CH4(0:255)
  INTEGER ( KIND = 4 ) COUNT(0:255)
  INTEGER ( KIND = 4 ) I
  REAL ( KIND = 4 ) PERCENT
  CHARACTER ( LEN = * ) TITLE
  INTEGER ( KIND = 4 ) TOTAL

  TOTAL = SUM ( COUNT )

  DO I = 0, 255
    C = ACHAR ( I )
    CALL CH_TO_SYM ( C, CH4(I) )
  END DO

  IF ( 0 < LEN_TRIM ( TITLE ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) TRIM ( TITLE )
  END IF

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Char  Count  Percentages.'
  WRITE ( *, '(a)' ) ' '
  DO I = 0, 255
    IF ( 0 < COUNT(I) ) THEN
      IF ( TOTAL == 0 ) THEN
        PERCENT = 0.0E+00
      ELSE
        PERCENT = REAL ( 100 * COUNT(I), KIND = 4 ) / REAL ( TOTAL, KIND = 4 )
      END IF
      WRITE ( *, '(2x,a4,2x,i8,2x,f6.3)' ) CH4(I), COUNT(I), PERCENT
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE CH_COUNT_S_ADD ( S, COUNT )

!*****************************************************************************80
!
!! CH_COUNT_S_ADD adds a character string to a character histogram.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Input/output, integer ( kind = 4 ) COUNT(0:255), the character counts.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) COUNT(0:255)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = * ) S

  DO I = 1, LEN ( S )
    J = IACHAR ( S(I:I) )
    COUNT(J) = COUNT(J) + 1
  END DO

  RETURN
END

!*************************************************************

FUNCTION CH_EQI ( C1, C2 )

!*****************************************************************************80
!
!! CH_EQI is a case insensitive comparison of two characters for equality.
!
!  Discussion:
!
!    CH_EQI ( 'A', 'a' ) is TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C1, C2, the characters to compare.
!
!    Output, logical CH_EQI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C1_CAP
  CHARACTER C2
  CHARACTER C2_CAP
  LOGICAL CH_EQI

  C1_CAP = C1
  C2_CAP = C2

  CALL CH_CAP ( C1_CAP )
  CALL CH_CAP ( C2_CAP )

  IF ( C1_CAP == C2_CAP ) THEN
    CH_EQI = .TRUE.
  ELSE
    CH_EQI = .FALSE.
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_EXTRACT ( S, CH )

!*****************************************************************************80
!
!! CH_EXTRACT extracts the next nonblank character from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string.  On output, the
!    first nonblank character has been removed, and the string
!    has been shifted left.
!
!    Output, character CH, the leading character of the string.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) GET
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LEN

  S_LEN = LEN_TRIM ( S )
  CH = ' '

  GET = 1

  DO WHILE ( GET <= S_LEN )

    IF ( S(GET:GET) /= ' ' ) THEN
      CH = S(GET:GET)
      CALL S_SHIFT_LEFT ( S, GET )
      EXIT
    END IF

    GET = GET + 1

  END DO

  RETURN
END

!*************************************************************

FUNCTION CH_INDEXI ( S, CH )

!*****************************************************************************80
!
!! CH_INDEXI: (case insensitive) first occurrence of a character in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character CH, the character to be searched for.
!
!    Output, integer ( kind = 4 ) CH_INDEXI, the location of the first
!    occurrence of the character (upper or lowercase), or -1 if it does
!    not occur.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_INDEXI
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  CH_INDEXI = -1
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( CH_EQI ( S(I:I), CH ) ) THEN
      CH_INDEXI = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION CH_INDEX_FIRST ( S, CH )

!*****************************************************************************80
!
!! CH_INDEX_FIRST is the first occurrence of a character in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character CH, the character to be searched for.
!
!    Output, integer ( kind = 4 ) CH_INDEX_FIRST, the location of the first
!    occurrence of the character in the string, or -1 if it does not occur.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_INDEX_FIRST
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  CH_INDEX_FIRST = - 1
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( S(I:I) == CH ) THEN
      CH_INDEX_FIRST = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION CH_INDEX_LAST ( S, CH )

!*****************************************************************************80
!
!! CH_INDEX_LAST is the last occurrence of a character in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character CH, the character to be searched for.
!
!    Output, integer ( kind = 4 ) CH_INDEX_LAST, the location of the last
!    occurrence of the character in the string, or -1 if it does not occur.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_INDEX_LAST
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  CH_INDEX_LAST = -1
  S_LENGTH = LEN_TRIM ( S )

  DO I = S_LENGTH, 1, -1

    IF ( S(I:I) == CH ) THEN
      CH_INDEX_LAST = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION CH_IS_ALPHA ( CH )

!*****************************************************************************80
!
!! CH_IS_ALPHA is TRUE if CH is an alphabetic character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, a character to check.
!
!    Output, logical CH_IS_ALPHA is TRUE if CH is an alphabetic character.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_ALPHA

  IF ( ( LLE ( 'a', CH ) .AND. LLE ( CH, 'z' ) ) .OR. &
       ( LLE ( 'A', CH ) .AND. LLE ( CH, 'Z' ) ) ) THEN
    CH_IS_ALPHA = .TRUE.
  ELSE
    CH_IS_ALPHA = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_ALPHANUMERIC ( CH )

!*****************************************************************************80
!
!! CH_IS_ALPHANUMERIC is TRUE if CH is alphanumeric.
!
!  Discussion:
!
!    Alphanumeric characters are 'A' through 'Z', 'a' through 'z' and
!    '0' through '9'.
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
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
!    Input, character CH, the character to be checked.
!
!    Output, logical CH_IS_ALPHANUMERIC, is TRUE if the character is
!    alphabetic or numeric.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_ALPHANUMERIC
  INTEGER ( KIND = 4 ) I

  I = IACHAR ( CH )

  IF ( ( 65 <= I .AND. I <= 90 ) .OR. &
       ( 97 <= I .AND. I <= 122 ) .OR. &
       ( 48 <= I .AND. I <= 57 ) ) THEN

    CH_IS_ALPHANUMERIC = .TRUE.

  ELSE

    CH_IS_ALPHANUMERIC = .FALSE.

  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_CONTROL ( CH )

!*****************************************************************************80
!
!! CH_IS_CONTROL is TRUE if a character is a control character.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    A "control character" has ASCII code <= 31 or 127 <= ASCII code.
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
!    Input, character CH, the character to be tested.
!
!    Output, logical CH_IS_CONTROL, TRUE if the character is a control
!    character, and FALSE otherwise.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_CONTROL

  IF ( IACHAR ( CH ) <= 31 .OR. 127 <= IACHAR ( CH ) ) THEN
    CH_IS_CONTROL = .TRUE.
  ELSE
    CH_IS_CONTROL = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_DIGIT ( CH )

!*****************************************************************************80
!
!! CH_IS_DIGIT is TRUE if a character is a decimal digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical CH_IS_DIGIT, is TRUE if the character is a digit.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_DIGIT

  IF ( LLE ( '0', CH ) .AND. LLE ( CH, '9' ) ) THEN
    CH_IS_DIGIT = .TRUE.
  ELSE
    CH_IS_DIGIT = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_FORMAT_CODE ( CH )

!*****************************************************************************80
!
!! CH_IS_FORMAT_CODE is TRUE if a character is a FORTRAN format code.
!
!  Discussion:
!
!    The format codes accepted here are not the only legal format
!    codes in FORTRAN90.  However, they are more than sufficient
!    for my needs!
!
!  Table:
!
!    A  Character
!    B  Binary digits
!    D  Real number, exponential representation
!    E  Real number, exponential representation
!    F  Real number, fixed point
!    G  General format
!    I  Integer
!    L  Logical variable
!    O  Octal digits
!    Z  Hexadecimal digits
!    *  Free format
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical CH_IS_FORMAT_CODE, is TRUE if the character is a
!    FORTRAN format code.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_FORMAT_CODE

  CH_IS_FORMAT_CODE = .TRUE.

       IF ( CH_EQI ( CH, 'A' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'B' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'D' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'E' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'F' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'G' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'I' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'L' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'O' ) ) THEN
    RETURN
  ELSE IF ( CH_EQI ( CH, 'Z' ) ) THEN
    RETURN
  ELSE IF ( CH == '*' ) THEN
    RETURN
  END IF

  CH_IS_FORMAT_CODE = .FALSE.

  RETURN
END

!*************************************************************

FUNCTION CH_IS_LOWER ( CH )

!*****************************************************************************80
!
!! CH_IS_LOWER is TRUE if a character is a lower case letter.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical CH_IS_LOWER, is TRUE if the character is a lower
!    case letter.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_LOWER

  IF ( LLE ( 'a', CH ) .AND. LLE ( CH, 'z' ) ) THEN
    CH_IS_LOWER = .TRUE.
  ELSE
    CH_IS_LOWER = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_PRINTABLE ( CH )

!*****************************************************************************80
!
!! CH_IS_PRINTABLE is TRUE if C is printable.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, a character to check.
!
!    Output, logical CH_IS_PRINTABLE is TRUE if C is a printable character.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_PRINTABLE
  INTEGER ( KIND = 4 ) I

  I = IACHAR ( CH )

  IF ( 32 <= I .AND. I <= 126 ) THEN
    CH_IS_PRINTABLE = .TRUE.
  ELSE
    CH_IS_PRINTABLE = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_SPACE ( CH )

!*****************************************************************************80
!
!! CH_IS_SPACE is TRUE if a character is a whitespace character.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    A whitespace character is a space, a form feed, a newline,
!    a carriage return, a tab, or a vertical tab.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, a character to check.
!
!    Output, logical CH_IS_SPACE is TRUE if the character is a whitespace
!    character.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_SPACE

  IF ( CH == ' ' ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE IF ( CH == ACHAR ( 12 ) ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE IF ( CH == ACHAR ( 10 ) ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE IF ( CH == ACHAR ( 13 ) ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE IF ( CH == ACHAR ( 9 ) ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE IF ( CH == ACHAR ( 11 ) ) THEN
    CH_IS_SPACE = .TRUE.
  ELSE
    CH_IS_SPACE = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_IS_UPPER ( CH )

!*****************************************************************************80
!
!! CH_IS_UPPER is TRUE if CH is an upper case letter.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be analyzed.
!
!    Output, logical CH_IS_UPPER, is TRUE if CH is an upper case letter.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_IS_UPPER

  IF ( LLE ( 'A', CH ) .AND. LLE ( CH, 'Z' ) ) THEN
    CH_IS_UPPER = .TRUE.
  ELSE
    CH_IS_UPPER = .FALSE.
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_LOW ( CH )

!*****************************************************************************80
!
!! CH_LOW lowercases a single character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantee the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, the character to be lowercased.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  I = IACHAR ( CH )

  IF ( 65 <= I .AND. I <= 90 ) THEN
    CH = ACHAR ( I + 32 )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_NEXT ( S, CH, DONE )

!*****************************************************************************80
!
!! CH_NEXT reads the next character from a string, ignoring blanks and commas.
!
!  Example:
!
!    Input:
!
!      S = ' A  B, C    DE  F'
!
!    Output:
!
!      'A', 'B', 'C', 'D', 'E', 'F', and then blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string of characters.  Blanks and
!    commas are considered insignificant.
!
!    Output, character CH.  If DONE is FALSE, then the
!    "next" character.  If DONE is TRUE, then a blank.
!
!    Input/output, logical DONE.
!    On input with a fresh value of S, the user should set
!    DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another character
!    was read, or TRUE if no more characters could be read.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL DONE
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ), SAVE :: NEXT = 1
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  IF ( DONE ) THEN
    NEXT = 1
    DONE = .FALSE.
  END IF

  S_LENGTH = LEN_TRIM ( S )

  DO I = NEXT, S_LENGTH

    IF ( S(I:I) /= ' ' .AND. S(I:I) /= ',' ) THEN
      CH = S(I:I)
      NEXT = I + 1
      RETURN
    END IF

  END DO

  DONE = .TRUE.
  NEXT = 1
  CH = ' '

  RETURN
END

!*************************************************************

FUNCTION CH_NOT_CONTROL ( CH )

!*****************************************************************************80
!
!! CH_NOT_CONTROL = CH is NOT a control character.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
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
!    Input, character CH is the character to be tested.
!
!    Output, logical CH_NOT_CONTROL, TRUE if CH is not a control character,
!    and FALSE otherwise.
!
  IMPLICIT NONE

  CHARACTER CH
  LOGICAL CH_NOT_CONTROL

  IF ( IACHAR ( CH ) <= 31 .OR. 128 <= IACHAR ( CH ) ) THEN
    CH_NOT_CONTROL = .TRUE.
  ELSE
    CH_NOT_CONTROL = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_ROMAN_TO_I4 ( CH )

!*****************************************************************************80
!
!! CH_ROMAN_TO_I4 returns the integer value of a single Roman digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, a Roman digit.
!
!    Output, integer ( kind = 4 ) CH_ROMAN_TO_I4, the value of the Roman
!    numeral.  If the Roman numeral was not recognized, 0 is returned.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_ROMAN_TO_I4
  INTEGER ( KIND = 4 ) I

  IF ( CH == 'M' .OR. CH == 'm' ) THEN
    I = 1000
  ELSE IF ( CH == 'D' .OR. CH == 'd' ) THEN
    I = 500
  ELSE IF ( CH == 'C' .OR. CH == 'c' ) THEN
    I = 100
  ELSE IF ( CH == 'L' .OR. CH == 'l' ) THEN
    I = 50
  ELSE IF ( CH == 'X' .OR. CH == 'x' ) THEN
    I = 10
  ELSE IF ( CH == 'V' .OR. CH == 'v' ) THEN
    I = 5
  ELSE IF ( CH == 'I' .OR. CH == 'i' .OR. &
            CH == 'J' .OR. CH == 'j' ) THEN
    I = 1
  ELSE
    I = 0
  END IF

  CH_ROMAN_TO_I4 = I

  RETURN
END

!*************************************************************

FUNCTION CH_SCRABBLE ( TILE )

!*****************************************************************************80
!
!! CH_SCRABBLE returns the character on a given Scrabble tile.
!
!  Discussion:
!
!    The tiles are numbered 1 to 100, and are labeled 'A' through 'Z',
!    plus two blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) TILE, the index of the desired Scrabble tile.
!
!    Output, character CH_SCRABBLE, the character on the given tile.
!
  IMPLICIT NONE

  CHARACTER CH_SCRABBLE
  CHARACTER, DIMENSION ( 1 : 100 ) :: SCRABBLE = (/ &
    'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'B', &
    'B', 'C', 'C', 'D', 'D', 'D', 'D', 'E', 'E', 'E', &
    'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'F', &
    'F', 'G', 'G', 'G', 'H', 'H', 'I', 'I', 'I', 'I', &
    'I', 'I', 'I', 'I', 'I', 'J', 'K', 'L', 'L', 'L', &
    'L', 'M', 'M', 'N', 'N', 'N', 'N', 'N', 'N', 'O', &
    'O', 'O', 'O', 'O', 'O', 'O', 'O', 'P', 'P', 'Q', &
    'R', 'R', 'R', 'R', 'R', 'R', 'S', 'S', 'S', 'S', &
    'T', 'T', 'T', 'T', 'T', 'T', 'U', 'U', 'U', 'U', &
    'V', 'V', 'W', 'W', 'X', 'X', 'Y', 'Z', ' ', ' ' /)
  INTEGER ( KIND = 4 ) TILE

  IF ( 1 <= TILE .AND. TILE <= 100 ) THEN
    CH_SCRABBLE = SCRABBLE(TILE)
  ELSE
    CH_SCRABBLE = '?'
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_SCRABBLE_FREQUENCY ( CH )

!*****************************************************************************80
!
!! CH_SCRABBLE_FREQUENCY returns the Scrabble frequency of a character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character.
!
!    Output, integer ( kind = 4 ) CH_SCRABBLE_FREQUENCY, the frequency of
!    the character.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_SCRABBLE_FREQUENCY
  INTEGER ( KIND = 4 ), DIMENSION ( 27 ) :: FREQUENCY = (/ &
     9,  2,  2,  4, 12, &
     2,  3,  2,  9,  1, &
     1,  4,  2,  6,  8, &
     2,  1,  6,  4,  6, &
     4,  2,  2,  1,  2, &
     1,  2 /)
  INTEGER ( KIND = 4 ) IC
!
!  Convert character to a Scrabble character index.
!
  IC = CH_TO_SCRABBLE ( CH )

  IF ( 1 <= IC .AND. IC <= 27 ) THEN
    CH_SCRABBLE_FREQUENCY = FREQUENCY(IC)
  ELSE
    CH_SCRABBLE_FREQUENCY = 0
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_SCRABBLE_POINTS ( CH )

!*****************************************************************************80
!
!! CH_SCRABBLE_POINTS returns the Scrabble point value of a character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character.
!
!    Output, integer ( kind = 4 ) CH_SCRABBLE_POINTS, the point value of
!    the character.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_SCRABBLE_POINTS
  INTEGER ( KIND = 4 ) IC
  INTEGER ( KIND = 4 ), DIMENSION ( 27 ) :: POINTS = (/ &
     1,  3,  3,  2,  1, &
     4,  2,  4,  1,  8, &
     5,  1,  3,  1,  1, &
     3, 10,  1,  1,  1, &
     1,  4,  4,  8,  4, &
    10,  0 /)
!
!  Convert character to a Scrabble character index.
!
  IC = CH_TO_SCRABBLE ( CH )

  IF ( 1 <= IC .AND. IC <= 27 ) THEN
    CH_SCRABBLE_POINTS = POINTS(IC)
  ELSE
    CH_SCRABBLE_POINTS = 0
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_SCRABBLE_SELECT ( SEED )

!*****************************************************************************80
!
!! CH_SCRABBLE_SELECT selects a character with the Scrabble probability.
!
!  Discussion:
!
!    There are 100 Scrabble tiles, including two blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2006
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
!    Output, character CH_SCRABBLE_SELECT, the character on a randomly
!    chosen Scrabble tile.
!
  IMPLICIT NONE

  CHARACTER CH_SCRABBLE_SELECT
  INTEGER ( KIND = 4 ) SEED
  INTEGER ( KIND = 4 ) TILE
!
!  Choose a tile between 1 and 100.
!
  TILE = I4_UNIFORM ( 1, 100, SEED )
!
!  Retrieve the character on that tile.
!
  CH_SCRABBLE_SELECT = CH_SCRABBLE ( TILE )

  RETURN
END

!*************************************************************

SUBROUTINE CH_SWAP ( CH1, CH2 )

!*****************************************************************************80
!
!! CH_SWAP swaps two characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH1, CH2.  On output, the values
!    have been interchanged.
!
  IMPLICIT NONE

  CHARACTER CH1
  CHARACTER CH2
  CHARACTER CH3

  CH3 = CH1
  CH1 = CH2
  CH2 = CH3

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_AMINO_NAME ( CH, AMINO_NAME )

!*****************************************************************************80
!
!! CH_TO_AMINO_NAME converts a character to an amino acid name.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl Branden, John Tooze,
!    Introduction to Protein Structure,
!    Garland Publishing, 1991.
!
!  Parameters:
!
!    Input, character CH, the one letter code for an amino acid.
!    Lower and upper case letters are treated the same.
!
!    Output, character ( len = * ) AMINO_NAME, the full name of the
!    corresponding amino acid.  The longest name is 27 characters.
!    If the input code is not recognized, then AMINO_NAME will be set to '???'.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: N = 23

  CHARACTER ( LEN = * ) AMINO_NAME
  CHARACTER ( LEN = 27 ), DIMENSION ( N ) :: AMINO_TABLE = (/ &
    'Alanine                    ', &
    'Aspartic acid or Asparagine', &
    'Cysteine                   ', &
    'Aspartic acid              ', &
    'Glutamic acid              ', &
    'Phenylalanine              ', &
    'Glycine                    ', &
    'Histidine                  ', &
    'Isoleucine                 ', &
    'Lysine                     ', &
    'Leucine                    ', &
    'Methionine                 ', &
    'Asparagine                 ', &
    'Proline                    ', &
    'Glutamine                  ', &
    'Arginine                   ', &
    'Serine                     ', &
    'Threonine                  ', &
    'Valine                     ', &
    'Tryptophan                 ', &
    'Undetermined amino acid    ', &
    'Tyrosine                   ', &
    'Glutamic acid or Glutamine ' /)
  CHARACTER CH
  CHARACTER, DIMENSION ( N ) :: CH_TABLE = (/ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', &
    'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', &
    'X', 'Y', 'Z' /)
  INTEGER ( KIND = 4 ) I

  DO I = 1, N
    IF ( CH_EQI ( CH, CH_TABLE(I) ) ) THEN
      AMINO_NAME = AMINO_TABLE(I)
      RETURN
    END IF
  END DO

  AMINO_NAME = '???'

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_BRAILLE ( CH, NCOL, BRAILLE )

!*****************************************************************************80
!
!! CH_TO_BRAILLE converts an ASCII character to a Braille character string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
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
!    Input, character CH, the ASCII character.
!
!    Output, integer ( kind = 4 ) NCOL, the number of columns used to represent
!    the character.
!
!    Output, character ( len = 6 ) BRAILLE(3), contains, in rows 1
!    through 3 and character columns 1 through NCOL, either a '*' or a ' '.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: NUM_SYMBOL = 37

  CHARACTER ( LEN = 6 ) BRAILLE(3)
  CHARACTER CH
  INTEGER ( KIND = 4 ) IASCII
  INTEGER ( KIND = 4 ) IBRAILLE
  INTEGER ( KIND = 4 ) NCOL
!
!       space       Aa1        Bb2        Cc3        Dd4
!       Ee5         Ff6        Gg7        Hh8        Ii9
!       Jj0         Kk         Ll         Mm         Nn
!       Oo          Pp         Qq         Rr         Ss
!       Tt          Uu         Vv         Ww         Xx
!       Yy          Zz         &          ,          ;
!       :           .          !          ()         "?
!       '           -
!
  CHARACTER ( LEN = 6 ), PARAMETER, DIMENSION ( NUM_SYMBOL ) :: SYMBOL = (/ &
    '      ',  '*     ',  '* *   ',  '**    ',  '** *  ', &
    '*  *  ',  '***   ',  '****  ',  '* **  ',  ' **   ', &
    ' ***  ',  '*   * ',  '* * * ',  '**  * ',  '** ** ', &
    '*  ** ',  '*** * ',  '***** ',  '* *** ',  ' ** * ', &
    ' **** ',  '*   **',  '* * **',  ' *** *',  '**  **', &
    '** ***',  '*  ***',  '*** **',  '  *   ',  '  * * ', &
    '  **  ',  '  ** *',  '  *** ',  '  ****',  '  * **', &
    '    * ',  '    **' /)

  NCOL = 0

  BRAILLE(1)(1:6) = ' '
  BRAILLE(2)(1:6) = ' '
  BRAILLE(3)(1:6) = ' '
!
!  A space is treated specially.
!
  IF ( CH == ' ' ) THEN

    BRAILLE(1)(1:2) = '  '
    BRAILLE(2)(1:2) = '  '
    BRAILLE(3)(1:2) = '  '
    NCOL = 2
    RETURN

  END IF
!
!  Get the ASCII numeric code of the character.
!
  IASCII = IACHAR ( CH )
!
!  Get the index of the Braille equivalent.
!
  IBRAILLE = IC_TO_IBRAILLE ( IASCII )

  IF ( 0 <= IBRAILLE ) THEN
!
!  Upper case characters are preceded by a special mark.
!
    IF ( CH_IS_UPPER ( CH ) ) THEN

      BRAILLE(1)(1:3) = '   '
      BRAILLE(2)(1:3) = '   '
      BRAILLE(3)(1:3) = ' * '

      NCOL = 3
!
!  Digits are preceded by a special mark.
!
    ELSE IF ( CH_IS_DIGIT ( CH ) ) THEN

      BRAILLE(1)(1:3) = ' * '
      BRAILLE(2)(1:3) = ' * '
      BRAILLE(3)(1:3) = '** '

      NCOL = 3

    END IF

    BRAILLE(1)(NCOL+1:NCOL+2) = SYMBOL(IBRAILLE)(1:2)
    BRAILLE(2)(NCOL+1:NCOL+2) = SYMBOL(IBRAILLE)(3:4)
    BRAILLE(3)(NCOL+1:NCOL+2) = SYMBOL(IBRAILLE)(5:6)

    NCOL = NCOL + 2
!
!  Add a trailing "half space".
!
    BRAILLE(1)(NCOL+1:NCOL+1) = ' '
    BRAILLE(2)(NCOL+1:NCOL+1) = ' '
    BRAILLE(3)(NCOL+1:NCOL+1) = ' '

    NCOL = NCOL + 1

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_CH3_AMINO ( CH, CH3 )

!*****************************************************************************80
!
!! CH_TO_CH3_AMINO converts a 1 character to a 3 character code for amino acids.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl Branden, John Tooze,
!    Introduction to Protein Structure,
!    Garland Publishing, 1991.
!
!  Parameters:
!
!    Input, character CH, the one letter code for an amino acid.
!    Lower and upper case letters are treated the same.
!
!    Output, character ( len = 3 ) CH3, the three letter code for the
!    amino acid.  If the input code is not recognized, then CH3 will be '???'.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: N = 23

  CHARACTER CH
  CHARACTER, PARAMETER, DIMENSION ( N ) :: CH_TABLE = (/ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', &
    'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', &
    'X', 'Y', 'Z' /)
  CHARACTER ( LEN = 3 ) CH3
  CHARACTER ( LEN = 3 ), PARAMETER, DIMENSION ( N ) :: CH3_TABLE = (/ &
    'Ala', 'Asx', 'Cys', 'Asp', 'Glu', 'Phe', 'Gly', 'His', 'Ise', 'Lys', &
    'Leu', 'Met', 'Asn', 'Pro', 'Gln', 'Arg', 'Ser', 'Thr', 'Val', 'Trp', &
    'X  ', 'Tyr', 'Glx' /)
  INTEGER ( KIND = 4 ) I

  DO I = 1, N
    IF ( CH_EQI ( CH, CH_TABLE(I) ) ) THEN
      CH3 = CH3_TABLE(I)
      RETURN
    END IF
  END DO

  CH3 = '???'

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_DIGIT ( CH, DIGIT )

!*****************************************************************************80
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!     CH  DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.
!    If CH was 'illegal', then DIGIT is -1.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) DIGIT

  IF ( LLE ( '0', CH ) .AND. LLE ( CH, '9' ) ) THEN

    DIGIT = IACHAR ( CH ) - 48

  ELSE IF ( CH == ' ' ) THEN

    DIGIT = 0

  ELSE

    DIGIT = - 1

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_DIGIT_BIN ( CH, DIGIT )

!*****************************************************************************80
!
!! CH_TO_DIGIT_BIN returns the integer value of a binary digit.
!
!  Discussion:
!
!    This routine handles other traditional binary pairs of "digits"
!    besides '0' and '1'.
!
!  Example:
!
!     CH  DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    'T'    1
!    'F'    0
!    'Y'    1
!    'N'    0
!    '+'    1
!    '-'    0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the binary digit.
!
!    Output, integer ( kind = 4 ) DIGIT, the corresponding integer value.
!    If CH was 'illegal', then DIGIT is -1.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) DIGIT

  IF ( CH == '0' .OR. &
       CH == 'F' .OR. &
       CH == 'f' .OR. &
       CH == '-' .OR. &
       CH == 'N' .OR. &
       CH == 'n' ) THEN

    DIGIT = 0

  ELSE IF ( CH == '1' .OR. &
            CH == 'T' .OR. &
            CH == 't' .OR. &
            CH == '+' .OR. &
            CH == 'Y' .OR. &
            CH == 'y' ) THEN

    DIGIT = 1

  ELSE

    DIGIT = -1

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_DIGIT_OCT ( CH, I )

!*****************************************************************************80
!
!! CH_TO_DIGIT_OCT returns the integer value of an octal digit.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the octal digit, '0' through '7'.
!
!    Output, integer ( kind = 4 ) I, the corresponding integer value, or
!    -1 if CH was illegal.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  I = IACHAR ( CH )

  IF ( LLE ( '0', CH ) .AND. LLE ( CH, '7' ) ) THEN

    I = I - 48

  ELSE IF ( CH == ' ' ) THEN

    I = 0

  ELSE

    I = -1

  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_TO_EBCDIC ( CH )

!*****************************************************************************80
!
!! CH_TO_EBCDIC converts a character to EBCDIC.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantee the ASCII collating sequence.
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
!    Input, character CH, the ASCII character.
!
!    Output, character CH_TO_EBCDIC, the corresponding EBCDIC character, or a
!    blank character if no correspondence holds.
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER CH_TO_EBCDIC
  INTEGER ( KIND = 4 ) I

  I = IC_TO_IEBCDIC ( IACHAR ( CH ) )

  IF ( I /= -1 ) THEN
    CH_TO_EBCDIC = ACHAR ( I )
  ELSE
    CH_TO_EBCDIC = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_MILITARY ( CH, MILITARY )

!*****************************************************************************80
!
!! CH_TO_MILITARY converts an ASCII character to a Military code word.
!
!  Example:
!
!    'A'  'Alpha'
!    'B'  'Bravo'
!    'Z'  'Zulu'
!    'a'  'alpha'
!    '7'  '7'
!    '%'  '%'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the ASCII character.
!
!    Output, character ( len = 8 ) MILITARY, the military code word.
!    If CH is not an alphabetic letter, then MILITARY is simply set equal to CH.
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER ( LEN = 8 ), PARAMETER, DIMENSION ( 26 ) :: CODE = (/ &
    'alpha   ', 'bravo   ', 'charlie ', 'delta   ', 'echo    ', &
    'foxtrot ', 'golf    ', 'hotel   ', 'india   ', 'juliet  ', &
    'kilo    ', 'lima    ', 'mike    ', 'november', 'oscar   ', &
    'papa    ', 'quebec  ', 'romeo   ', 'sierra  ', 'tango   ', &
    'uniform ', 'victor  ', 'whiskey ', 'x-ray   ', 'yankee  ', &
    'zulu    ' /)
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) MILITARY

  IF ( 'A' <= CH .AND. CH <= 'Z' ) THEN
    I = A_TO_I4 ( CH )
    MILITARY = CODE(I)
    CALL CH_CAP ( MILITARY(1:1) )
  ELSE IF ( 'a' <= CH .AND. CH <= 'z' ) THEN
    I = A_TO_I4 ( CH ) - 26
    MILITARY = CODE(I)
  ELSE
    MILITARY = CH
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_MORSE ( CH, MORSE )

!*****************************************************************************80
!
!! CH_TO_MORSE converts an ASCII character to a Morse character string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the ASCII character.
!
!    Output, character ( len = 6 ) MORSE, the Morse character string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: NUM_SYMBOL = 45

  CHARACTER CH
  INTEGER ( KIND = 4 ) IASCII
  INTEGER ( KIND = 4 ) IMORSE
  CHARACTER ( LEN = 6 ) MORSE
  CHARACTER ( LEN = 6 ), PARAMETER, DIMENSION ( NUM_SYMBOL ) :: MSYMBOL = (/ &
    '      ', '.-    ', '-...  ', '-.-.  ', '-..   ', &
    '.     ', '..-.  ', '--.   ', '....  ', '..    ', &
    '.---  ', '-.-   ', '.-..  ', '--    ', '-.    ', &
    '---   ', '.--.  ', '--.-  ', '.-.   ', '...   ', &
    '-     ', '..-   ', '...-  ', '.--   ', '-..-  ', &
    '-.--  ', '--..  ', '.---- ', '..--- ', '...-- ', &
    '....- ', '..... ', '-.... ', '--... ', '---.. ', &
    '----. ', '----- ', '.-.-.-', '--..--', '---...', &
    '..--..', '.----.', '-....-', '-..-. ', '.-..-.' /)

  IASCII = IACHAR ( CH )
  IMORSE = IC_TO_IMORSE ( IASCII )

  IF ( IMORSE == -1 ) THEN
    MORSE = ' '
  ELSE
    MORSE = MSYMBOL ( IMORSE )
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_TO_ROT13 ( CH )

!*****************************************************************************80
!
!! CH_TO_ROT13 converts a character to its ROT13 equivalent.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
!
!    Two applications of CH_TO_ROT13 to a character will return the original.
!
!    As a further scrambling, digits are similarly rotated using
!    a "ROT5" scheme.
!
!  Example:
!
!    Input:  Output:
!
!    a       n
!    C       P
!    J       W
!    1       6
!    5       0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be converted.
!
!    Output, character CH_TO_ROT13, the ROT13 equivalent of the character.
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER CH_TO_ROT13
  INTEGER ( KIND = 4 ) ITEMP

  ITEMP = IACHAR ( CH )
!
!  [0:4] -> [5:9]
!
  IF ( 48 <= ITEMP .AND. ITEMP <= 52 ) THEN
    ITEMP = ITEMP + 5
!
!  [5:9] -> [0:4]
!
  ELSE IF ( 53 <= ITEMP .AND. ITEMP <= 57 ) THEN
    ITEMP = ITEMP - 5
!
!  [A:M] -> [N:Z]
!
  ELSE IF ( 65 <= ITEMP .AND. ITEMP <= 77 ) THEN
    ITEMP = ITEMP + 13
!
!  [N:Z] -> [A:M]
!
  ELSE IF ( 78 <= ITEMP .AND. ITEMP <= 90 ) THEN
    ITEMP = ITEMP - 13
!
!  [a:m] -> [n:z]
!
  ELSE IF ( 97 <= ITEMP .AND. ITEMP <= 109 ) THEN
    ITEMP = ITEMP + 13
!
!  [n:z] -> [a:m]
!
  ELSE IF ( 110 <= ITEMP .AND. ITEMP <= 122 ) THEN
    ITEMP = ITEMP - 13
  END IF

  CH_TO_ROT13 = ACHAR ( ITEMP )

  RETURN
END

!*************************************************************

FUNCTION CH_TO_SCRABBLE ( CH )

!*****************************************************************************80
!
!! CH_TO_SCRABBLE returns the Scrabble index of a character.
!
!  Discussion:
!
!    'A' through 'Z' have indices 1 through 26, and blank is index 27.
!    Case is ignored.  All other characters return index -1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character.
!
!    Output, integer ( kind = 4 ) CH_TO_SCRABBLE, the Scrabble index of
!    the character.
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER CH_COPY
  INTEGER ( KIND = 4 ) CH_TO_SCRABBLE
  INTEGER ( KIND = 4 ) IC

  IF ( CH == ' ' ) THEN
    CH_TO_SCRABBLE = 27
    RETURN
  END IF

  CH_COPY = CH
  CALL CH_CAP ( CH_COPY )
  IC = A_TO_I4 ( CH_COPY )

  IF ( 1 <= IC .AND. IC <= 26 ) THEN
    CH_TO_SCRABBLE = IC
  ELSE
    CH_TO_SCRABBLE = -1
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_SOUNDEX ( CH, SOUNDEX )

!*****************************************************************************80
!
!! CH_TO_SOUNDEX converts an ASCII character to a Soundex character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
!
!    The soundex code is used to replace words by a code of up to four
!    digits.  Similar sounding words will often have identical soundex
!    codes.
!
!    Soundex  Letters
!    -------  ---------------
!       0     A E I O U Y H W
!       1     B B P V
!       2     C G J K Q S X Z
!       3     D T
!       4     L
!       5     M N
!       6     R
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
!    Input, character CH, the ASCII character.
!
!    Output, character SOUNDEX, the Soundex character, which is
!    '0', '1', '2', '3', '4', '5', '6', or ' '.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) IASCII
  INTEGER ( KIND = 4 ) ISOUNDEX
  CHARACTER SOUNDEX

  IASCII = IACHAR ( CH )
  ISOUNDEX = IC_TO_ISOUNDEX ( IASCII )

  IF ( ISOUNDEX == -1 ) THEN
    SOUNDEX = ' '
  ELSE
    SOUNDEX = ACHAR ( ISOUNDEX )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE CH_TO_SYM ( CH, SYM )

!*****************************************************************************80
!
!! CH_TO_SYM returns a printable symbol for any ASCII character.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the character to be represented.
!
!    Output, character ( len = 4 ) SYM, is the printable symbol for CHR.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = 4 ) SYM

  I = IACHAR ( CH )

  SYM = ' '

  PUT = 0
!
!  Characters 128-255 are symbolized with a ! prefix.
!  Then shift them down by 128.
!  Now all values of I are between 0 and 127.
!
  IF ( 128 <= I ) THEN
    I = MOD ( I, 128 )
    PUT = PUT + 1
    SYM(PUT:PUT) = '!'
  END IF
!
!  Characters 0-31 are symbolized with a ^ prefix.
!  Shift them up by 64.  Now all values of I are between 32 and 127.
!
  IF ( I <= 31 ) THEN
    I = I + 64
    PUT = PUT + 1
    SYM(PUT:PUT) = '^'
  END IF
!
!  Character 32 becomes SP.
!  Characters 32 through 126 are themselves.
!  Character 127 is DEL.
!
  IF ( I == 32 ) THEN
    PUT = PUT + 1
    SYM(PUT:PUT+1) = 'SP'
  ELSE IF ( I <= 126 ) THEN
    PUT = PUT + 1
    SYM(PUT:PUT) = ACHAR ( I )
  ELSE IF ( I == 127 ) THEN
    PUT = PUT + 1
    SYM(PUT:PUT+2) = 'DEL'
  END IF

  RETURN
END

!*************************************************************

FUNCTION CH_UNIFORM ( CLO, CHI, SEED )

!*****************************************************************************80
!
!! CH_UNIFORM returns a random character in a given range.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions,
!    which guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CLO, CHI, the minimum and maximum acceptable characters.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, character CH_UNIFORM, the randomly chosen character.
!
  IMPLICIT NONE

  CHARACTER CH_UNIFORM
  CHARACTER CHI
  CHARACTER CLO
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) SEED

  ILO = IACHAR ( CLO )
  IHI = IACHAR ( CHI )

  I = I4_UNIFORM ( ILO, IHI, SEED )

  CH_UNIFORM = ACHAR ( I )

  RETURN
END

!*************************************************************

SUBROUTINE COMMA ( S )

!*****************************************************************************80
!
!! COMMA moves commas left through blanks in a string.
!
!  Example:
!
!    Input:                    Output:
!    -----                     ------
!    "To Henry , our dog"      "To Henry,  our dog"
!    " , , ,"                  ",,,  "
!    "  14.0   ,"              "  14.0,  "
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a string in which the
!    commas are to be shifted left through blanks.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IBLANK
  INTEGER ( KIND = 4 ) ICOMMA
  CHARACTER ( LEN = * ) S

  ICOMMA = LEN_TRIM ( S )

  DO WHILE ( 1 < ICOMMA )

    IF ( S(ICOMMA:ICOMMA) == ',' ) THEN

      IBLANK = ICOMMA

      DO WHILE ( 1 < IBLANK )
        IF ( S(IBLANK-1:IBLANK-1) /= ' ' ) THEN
          EXIT
        END IF
        IBLANK = IBLANK - 1
      END DO

      IF ( ICOMMA /= IBLANK ) THEN
        S(ICOMMA:ICOMMA) = ' '
        S(IBLANK:IBLANK) = ','
      END IF

    END IF

    ICOMMA = ICOMMA - 1

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE DEC_TO_S_LEFT ( IVAL, JVAL, S )

!*****************************************************************************80
!
!! DEC_TO_S_LEFT returns a left-justified representation of IVAL * 10^JVAL.
!
!  Example:
!
!    IVAL     JVAL       S
!    ----     ----       ------
!       0        0       0
!      21        3       21000
!      -3        0       -3
!     147       -2       14.7
!      16       -5       0.00016
!      34       30       Inf
!     123      -21       0.0000000000000000012
!      34      -30       0.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IVAL, JVAL, integers which represent
!    the decimal.
!
!    Output, character ( len = * ) S, the representation of the value.
!    The string is 'Inf' or '0.0' if the value was too large
!    or small to represent with a fixed point format.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 22 ) CHRREP
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) GET1
  INTEGER ( KIND = 4 ) GET2
  INTEGER ( KIND = 4 ) PUT1
  INTEGER ( KIND = 4 ) PUT2
  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) JVAL
  INTEGER ( KIND = 4 ) NDIGIT
  INTEGER ( KIND = 4 ) NLEFT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S = ' '

  IF ( IVAL == 0 ) THEN
    S = '0'
    RETURN
  END IF

  S_LENGTH = LEN ( S )
!
!  Store a representation of IVAL in CHRREP.
!
  WRITE ( CHRREP, '(i22)' ) IVAL
  CALL S_BLANK_DELETE ( CHRREP )
  NDIGIT = LEN_TRIM ( CHRREP )
!
!  Inf if JVAL is positive, and S_LENGTH < NDIGIT + JVAL.
!
  IF ( 0 < JVAL ) THEN
    IF ( S_LENGTH < NDIGIT + JVAL ) THEN
      S = 'Inf'
      RETURN
    END IF
  END IF
!
!  Underflow if JVAL is negative, and S_LENGTH < 3 + NDIGIT - JVAL.
!
  IF ( JVAL < 0 ) THEN
    IF ( 0 < IVAL ) THEN
      IF ( S_LENGTH < 3 - NDIGIT - JVAL ) THEN
        S = '0.0'
        RETURN
      END IF
    ELSE
      IF ( S_LENGTH < 5 - NDIGIT - JVAL ) THEN
        S = '0.0'
        RETURN
      END IF
    END IF
  END IF
!
!  If JVAL is nonnegative, insert trailing zeros.
!
  IF ( 0 <= JVAL ) THEN

    S(1:NDIGIT) = CHRREP(1:NDIGIT)

    DO I = NDIGIT + 1, NDIGIT + JVAL
      S(I:I) = '0'
    END DO

  ELSE IF ( JVAL < 0 ) THEN

    PUT2 = 0
    GET2 = 0
!
!  Sign.
!
    IF ( IVAL < 0 ) THEN
      PUT1 = 1
      PUT2 = 1
      GET2 = 1
      S(PUT1:PUT2) = '-'
      NDIGIT = NDIGIT - 1
    END IF
!
!  Digits of the integral part.
!
    IF ( 0 < NDIGIT + JVAL ) THEN
      PUT1 = PUT2 + 1
      PUT2 = PUT1 + NDIGIT + JVAL -1
      GET1 = GET2 + 1
      GET2 = GET1 + NDIGIT+JVAL - 1
      S(PUT1:PUT2) = CHRREP(GET1:GET2)
    ELSE
      PUT1 = PUT2 + 1
      PUT2 = PUT1
      S(PUT1:PUT2) = '0'
    END IF
!
!  Decimal point.
!
    PUT1 = PUT2 + 1
    PUT2 = PUT1
    S(PUT1:PUT2) = '.'
!
!  Leading zeroes.
!
    DO I = 1, - JVAL - NDIGIT
      PUT1 = PUT2 + 1
      PUT2 = PUT1
      S(PUT1:PUT2) = '0'
    END DO

    NLEFT = MIN ( -JVAL, NDIGIT )
    NLEFT = MIN ( NLEFT, S_LENGTH - PUT2 )
    PUT1 = PUT2 + 1
    PUT2 = PUT1 + NLEFT - 1
    GET1 = GET2 + 1
    GET2 = GET1 + NLEFT - 1
    S(PUT1:PUT2) = CHRREP(GET1:GET2)

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE DEC_TO_S_RIGHT ( IVAL, JVAL, S )

!*****************************************************************************80
!
!! DEC_TO_S_RIGHT returns a right justified representation of IVAL * 10**JVAL.
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
!    Input, integer ( kind = 4 ) IVAL, JVAL, the integers which represent the
!    decimal fraction.
!
!    Output, character ( len = * ) S, a right justified string
!    containing the representation of the decimal fraction.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) JVAL
  CHARACTER ( LEN = * ) S

  CALL DEC_TO_S_LEFT ( IVAL, JVAL, S )
  CALL S_ADJUSTR ( S )

  RETURN
END

!*************************************************************

SUBROUTINE DIGIT_BIN_TO_CH ( I, CH )

!*****************************************************************************80
!
!! DIGIT_BIN_TO_CH returns the character representation of a binary digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the integer, between 0 and 1.
!
!    Output, character CH, the character representation of the integer.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  IF ( I == 0 ) THEN
    CH = '0'
  ELSE IF ( I == 1 ) THEN
    CH = '1'
  ELSE
    CH = '*'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE DIGIT_INC ( CH )

!*****************************************************************************80
!
!! DIGIT_INC increments a decimal digit.
!
!  Example:
!
!    Input  Output
!    -----  ------
!    '0'    '1'
!    '1'    '2'
!    ...
!    '8'    '9'
!    '9'    '0'
!    'A'    'A'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character CH, a digit to be incremented.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) DIGIT

  CALL CH_TO_DIGIT ( CH, DIGIT )

  IF ( DIGIT == -1 ) THEN
    RETURN
  END IF

  DIGIT = DIGIT + 1

  IF ( DIGIT == 10 ) THEN
    DIGIT = 0
  END IF

  CALL DIGIT_TO_CH ( DIGIT, CH )

  RETURN
END

!*************************************************************

SUBROUTINE DIGIT_OCT_TO_CH ( I, CH )

!*****************************************************************************80
!
!! DIGIT_OCT_TO_CH returns the character representation of an octal digit.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the integer, between 0 and 7.
!
!    Output, character CH, the character representation of the integer.
!
  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  IF ( 0 <= I .AND. I <= 7 ) THEN
    CH = ACHAR ( I + 48 )
  ELSE
    CH = '*'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE DIGIT_TO_CH ( DIGIT, CH )

!*****************************************************************************80
!
!! DIGIT_TO_CH returns the character representation of a decimal digit.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!    DIGIT   CH
!    -----  ---
!      0    '0'
!      1    '1'
!    ...    ...
!      9    '9'
!     17    '*'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DIGIT, the digit value between 0 and 9.
!
!    Output, character CH, the corresponding character.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) DIGIT

  IF ( 0 <= DIGIT .AND. DIGIT <= 9 ) THEN

    CH = ACHAR ( DIGIT + 48 )

  ELSE

    CH = '*'

  END IF

  RETURN
END

!*************************************************************

FUNCTION EBCDIC_TO_CH ( E )

!*****************************************************************************80
!
!! EBCDIC_TO_CH converts an EBCDIC character to ASCII.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
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
!    Input, character E, the EBCDIC character.
!
!    Output, character EBCDIC_TO_CH, the corresponding ASCII
!    character, or a blank character if no correspondence holds.
!
  IMPLICIT NONE

  CHARACTER E
  CHARACTER EBCDIC_TO_CH
  INTEGER ( KIND = 4 ) I
  I = IEBCDIC_TO_IC ( IACHAR ( E ) )

  IF ( I /= -1 ) THEN
    EBCDIC_TO_CH = ACHAR ( I )
  ELSE
    EBCDIC_TO_CH = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE EBCDIC_TO_S ( S )

!*****************************************************************************80
!
!! EBCDIC_TO_S converts a string of EBCDIC characters to ASCII.
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
!    Input/output, character ( len = * ) S.
!    On input, the EBCDIC string.
!    On output, the ASCII string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH
    S(I:I) = EBCDIC_TO_CH ( S(I:I) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE FILLCH ( S1, FIELD, S2 )

!*****************************************************************************80
!
!! FILLCH writes a string into a subfield of a string.
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
!    Input/output, character ( len = * ) S1, a string which is presumed
!    to contain, somewhere, a substring that is to be filled in.
!    The substring might be '?', for instance.
!
!    On output, the substring has been overwritten.
!
!    Input, character ( len = * ) FIELD, a substring to be searched for in
!    S, which denotes the spot where the value should be placed.
!    Trailing blanks are ignored.
!
!    Input, character ( len = * ) S2, the character string to be written
!    into the subfield.  Trailing blanks are ignored.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FIELD
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENC
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2

  I = S_INDEXI ( S1, FIELD )

  IF ( I /= 0 ) THEN

    LENC = LEN_TRIM ( FIELD )
    CALL S_CHOP ( S1, I, I+LENC-1 )

    LENC = LEN_TRIM ( S2 )
    CALL S_S_INSERT ( S1, I, S2(1:LENC) )

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILLIN ( S, FIELD, IVAL )

!*****************************************************************************80
!
!! FILLIN writes an integer into a subfield of a string.
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
!    Input/output, character ( len = * ) S, a string which is presumed
!    to contain, somewhere, a substring that is to be filled in.
!    The substring might be '?', for instance.
!
!    On output, the substring has been overwritten by the value of IVAL.
!
!    Input, character ( len = * ) FIELD, a substring to be searched for in
!    S, which denotes the spot where the value should be placed.
!    Trailing blanks are ignored.
!
!    Input, integer ( kind = 4 ) IVAL, the value to be written
!    into the subfield.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FIELD
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) LENC
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 14 ) SVAL

  I = S_INDEXI ( S, FIELD )

  IF ( I /= 0 ) THEN

    LENC = LEN_TRIM ( FIELD )
    CALL S_CHOP ( S, I, I+LENC-1 )

    CALL I4_TO_S_LEFT ( IVAL, SVAL )

    LENC = LEN_TRIM ( SVAL )
    CALL S_S_INSERT ( S, I, SVAL(1:LENC) )

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILLRL ( S, FIELD, R )

!*****************************************************************************80
!
!! FILLRL writes a real into a subfield of a string.
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
!    Input/output, character ( len = * ) S, a string which is presumed
!    to contain, somewhere, a substring that is to be filled in.
!    The substring might be '?', for instance.
!    On output, the substring has been overwritten by the value.
!
!    Input, character ( len = * ) FIELD, a substring to be searched for in
!    S, which denotes the spot where the value should be placed.
!    Trailing blanks are ignored.
!
!    Input, real  ( kind = 4 ) R, the value to be written into the subfield.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FIELD
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENC
  REAL ( KIND = 4 ) R
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 10 ) SVAL

  I = S_INDEXI ( S, FIELD )

  IF ( I /= 0 ) THEN

    LENC = LEN_TRIM ( FIELD )

    CALL S_CHOP ( S, I, I+LENC-1 )

    CALL R4_TO_S_RIGHT ( R, SVAL )
    CALL S_BLANK_DELETE ( SVAL )
    LENC = LEN_TRIM ( SVAL )

    CALL S_S_INSERT ( S, I, SVAL(1:LENC) )

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FLT_TO_S ( MANT, IEXP, NDIG, S )

!*****************************************************************************80
!
!! FLT_TO_S returns a representation of MANT * 10**IEXP.
!
!  Example:
!
!    MANT   IEXP   S
!
!       1      2   100
!     101     -1   10.1
!      23     -3   0.023
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MANT, the mantissa of the representation.
!    This is an integer whose magnitude is between 0 and
!    10**NDIG, that is, 0 <= MANT < 10**NDIG.
!
!    Input, integer ( kind = 4 ) IEXP, the exponent of 10 that multiplies MULT.
!
!    Input, integer ( kind = 4 ) NDIG, the number of digits of accuracy
!    in the representation.
!
!    Output, character ( len = * ) S, the representation of the quantity.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) JEXP
  INTEGER ( KIND = 4 ) MANT
  INTEGER ( KIND = 4 ) NDIG
  CHARACTER ( LEN = * ) S
!
!  Get the length of the string, and set it all to blanks.
!
  S = ' '
!
!  If the mantissa is zero, the number is zero, and we have
!  a special case: S = '0'.
!
  IF ( MANT == 0 ) THEN
    S = '0'
    RETURN
  ELSE IF ( 0 < MANT ) THEN
    S(1:2) = '  '
  ELSE IF ( MANT < 0 ) THEN
    S(1:2) = '- '
  END IF
!
!  Now write the mantissa into S in positions 3 to NDIG+2.
!
  CALL I4_TO_S_LEFT ( ABS ( MANT ), S(3:NDIG+2) )
!
!  Insert a decimal place after the first digit.
!
  S(2:2) = S(3:3)
  S(3:3) = '.'
!
!  Place the "e" representing the exponent.
!
  S(NDIG+3:NDIG+3) = 'e'
!
!  Write the exponent.
!
  JEXP = 0

  DO WHILE ( 10**JEXP <= ABS ( MANT ) )
    JEXP = JEXP + 1
  END DO

  JEXP = JEXP + IEXP - 1

  CALL I4_TO_S_ZERO ( JEXP, S(NDIG+4:NDIG+6) )
!
!  Remove all blanks, effectively shifting the string left too.
!
  CALL S_BLANK_DELETE ( S )

  RETURN
END

!*************************************************************

SUBROUTINE FORCOM ( S, FORTRAN, COMMENT )

!*****************************************************************************80
!
!! FORCOM splits a FORTRAN line into "fortran" and "comment".
!
!  Discussion:
!
!    The "comment" portion is everything following the first occurrence
!    of an exclamation mark (and includes the exclamation mark).
!
!    The "fortran" portion is everything before the first exclamation
!    mark.
!
!    Either or both the data and comment portions may be blank.
!
!  Example:
!
!    S                             FORTRAN           COMMENT
!
!    '      x = 1952   ! Wow'      '      x = 1952'  '! Wow'
!    '      continue'              '      continue'  ' '
!    '! Hey, Abbott!'              ' '               '! Hey, Abbott!'
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
!    Input, character ( len = * ) S, the string to be analyzed.
!
!    Output, character ( len = * ) FORTRAN, the initial portion of the string,
!    containing a FORTRAN statement.
!
!    Output, character COMMENT, the final portion of the string,
!    containing a comment.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) COMMENT
  CHARACTER ( LEN = * ) FORTRAN
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S

  I = INDEX ( S, '!' )

  IF ( I == 0 ) THEN
    FORTRAN = S
    COMMENT = ' '
  ELSE IF ( I == 1 ) THEN
    FORTRAN = ' '
    COMMENT = S
  ELSE
    FORTRAN = S ( 1:I-1)
    COMMENT = S ( I: )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE GET_UNIT ( IUNIT )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
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
!    Output, integer ( kind = 4 ) IUNIT.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5 and 6).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  LOGICAL LOPEN

  IUNIT = 0

  DO I = 1, 99

    IF ( I /= 5 .AND. I /= 6 ) THEN

      INQUIRE ( UNIT = I, OPENED = LOPEN, IOSTAT = IOS )

      IF ( IOS == 0 ) THEN
        IF ( .NOT. LOPEN ) THEN
          IUNIT = I
          RETURN
        END IF
      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE HEX_DIGIT_TO_I4 ( CH, I )

!*****************************************************************************80
!
!! HEX_DIGIT_TO_I4 converts a hexadecimal digit to an I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character CH, the hexadecimal digit, '0'
!    through '9', or 'A' through 'F', or also 'a' through 'f'
!    are allowed.
!
!    Output, integer ( kind = 4 ) I, the corresponding integer, or -1 if
!    CH was illegal.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  I = IACHAR ( CH )

  IF ( LLE ( '0', CH ) .AND. LLE ( CH, '9' ) ) THEN

    I = I - 48

  ELSE IF ( 65 <= I .AND. I <= 70 ) THEN

    I = I - 55

  ELSE IF ( 97 <= I .AND. I <= 102 ) THEN

    I = I - 87

  ELSE IF ( CH == ' ' ) THEN

    I = 0

  ELSE

    I = -1

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE HEX_TO_BINARY_DIGITS ( HEX_DIGIT, BINARY_DIGITS )

!*****************************************************************************80
!
!! HEX_TO_BINARY_DIGITS converts a hexadecimal digit to 4 binary digits.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character HEX_DIGIT, the hexadecimal digit.
!
!    Output, character ( len = 4 ) BINARY_DIGITS, the binary digits.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 4 ) BINARY_DIGITS
  CHARACTER HEX_DIGIT

  IF ( HEX_DIGIT == '0' ) THEN
    BINARY_DIGITS = '0000'
  ELSE IF ( HEX_DIGIT == '1' ) THEN
    BINARY_DIGITS = '0001'
  ELSE IF ( HEX_DIGIT == '2' ) THEN
    BINARY_DIGITS = '0010'
  ELSE IF ( HEX_DIGIT == '3' ) THEN
    BINARY_DIGITS = '0011'
  ELSE IF ( HEX_DIGIT == '4' ) THEN
    BINARY_DIGITS = '0100'
  ELSE IF ( HEX_DIGIT == '5' ) THEN
    BINARY_DIGITS = '0101'
  ELSE IF ( HEX_DIGIT == '6' ) THEN
    BINARY_DIGITS = '0110'
  ELSE IF ( HEX_DIGIT == '7' ) THEN
    BINARY_DIGITS = '0111'
  ELSE IF ( HEX_DIGIT == '8' ) THEN
    BINARY_DIGITS = '1000'
  ELSE IF ( HEX_DIGIT == '9' ) THEN
    BINARY_DIGITS = '1001'
  ELSE IF ( HEX_DIGIT == 'A' .OR. HEX_DIGIT == 'a' ) THEN
    BINARY_DIGITS = '1010'
  ELSE IF ( HEX_DIGIT == 'B' .OR. HEX_DIGIT == 'b' ) THEN
    BINARY_DIGITS = '1011'
  ELSE IF ( HEX_DIGIT == 'C' .OR. HEX_DIGIT == 'c' ) THEN
    BINARY_DIGITS = '1100'
  ELSE IF ( HEX_DIGIT == 'D' .OR. HEX_DIGIT == 'd' ) THEN
    BINARY_DIGITS = '1101'
  ELSE IF ( HEX_DIGIT == 'E' .OR. HEX_DIGIT == 'e' ) THEN
    BINARY_DIGITS = '1110'
  ELSE IF ( HEX_DIGIT == 'F' .OR. HEX_DIGIT == 'f' ) THEN
    BINARY_DIGITS = '1111'
  ELSE
    BINARY_DIGITS = '    '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE HEX_TO_I4 ( S, I4 )

!*****************************************************************************80
!
!! HEX_TO_I4 converts a hexadecimal string to its integer value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string of hexadecimal digits.
!
!    Output, integer ( kind = 4 ) I4, the corresponding integer value.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )
!
!  Determine if there is a plus or minus sign.
!
  ISGN = 1

  FIRST = S_LENGTH + 1

  DO J = 1, S_LENGTH

    IF ( S(J:J) == '-' ) THEN
      ISGN = -1
    ELSE IF ( S(J:J) == '+' ) THEN
      ISGN = + 1
    ELSE IF ( S(J:J) /= ' ' ) THEN
      FIRST = J
      EXIT
    END IF

  END DO
!
!  Read the numeric portion of the string.
!
  I4 = 0

  DO J = FIRST, S_LENGTH
    CALL HEX_DIGIT_TO_I4 ( S(J:J), IDIG )
    I4 = I4 * 16 + IDIG
  END DO

  I4 = ISGN * I4

  RETURN
END

!*************************************************************

SUBROUTINE HEX_TO_S ( HEX, S )

!*****************************************************************************80
!
!! HEX_TO_S converts a hexadecimal string into characters.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!    Input:
!
!      '414243'
!
!    Output:
!
!      'ABC'.
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
!    Input, character ( len = * ) HEX, a string of pairs of hexadecimal values.
!
!    Output, character ( len = * ) S, the corresponding character string.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) HEX
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) NDO
  INTEGER ( KIND = 4 ) NHEX
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )
  NHEX = LEN_TRIM ( HEX )
  NDO = MIN ( NHEX / 2, S_LENGTH )

  S = ' '

  DO I = 1, NDO
    J = 2 * I - 1
    CALL HEX_TO_I4 ( HEX(J:J+1), INTVAL )
    S(I:I) = ACHAR ( INTVAL )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I2_BYTE_SWAP ( IWORD, BYTES )

!*****************************************************************************80
!
!! I2_BYTE_SWAP swaps bytes in an 8-byte word.
!
!  Discussion:
!
!    This routine uses the MVBITS routines to carry out the swaps.  The
!    relationship between the bits in the word (0 through 63) and the
!    bytes (1 through 8) is machine dependent.  That is, byte 1 may
!    comprise bits 0 through 7, or bits 56 through 63.  So some
!    experimentation may be necessary the first time this routine
!    is used.
!
!    This routine was originally written simply to take the drudgery
!    out of swapping bytes in a VAX word that was to be read by
!    another machine.
!
!    The statement
!
!      call i2_byte_swap ( IWORD, (/ 1, 2, 3, 4, 5, 6, 7, 8 /) )
!
!    will do nothing to IWORD, and
!
!      call i2_byte_swap ( IWORD, (/ 8, 7, 6, 5, 4, 3, 2, 1 /) )
!
!    will reverse the bytes in IWORD, and
!
!      call i2_byte_swap ( IWORD, (/ 2, 2, 2, 2, 2, 2, 2, 2 /) )
!
!    will replace IWORD with a word containing byte(2) repeated 8 times.
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
!    Input/output, integer ( kind = 4 ) IWORD, the word whose bits are to
!    be swapped.
!
!    Input, integer ( kind = 4 ) BYTES(8), indicates which byte in the input
!    word should overwrite each byte of the output word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: BYTES_NUM = 8

  INTEGER ( KIND = 4 ) BYTES(BYTES_NUM)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IWORD
  INTEGER ( KIND = 4 ) JWORD

  JWORD = IWORD

  DO I = 1, BYTES_NUM

    IF ( BYTES(I) < 1 .OR. BYTES_NUM < BYTES(I) ) THEN
      CYCLE
    END IF

    IF ( BYTES(I) == I ) THEN
      CYCLE
    END IF

    CALL MVBITS ( JWORD, (BYTES(I)-1)*8, 8, IWORD, 0 )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4VEC_INDICATOR ( N, A )

!*****************************************************************************80
!
!! I4VEC_INDICATOR sets an I4VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, integer ( kind = 4 ) A(N), the array to be initialized.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) A(N)
  INTEGER ( KIND = 4 ) I

  DO I = 1, N
    A(I) = I
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4VEC_PRINT ( N, A, TITLE )

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
!    Input, character ( len = * ) TITLE, a title.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) A(N)
  INTEGER ( KIND = 4 ) BIG
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) TITLE

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( TITLE )

  BIG = MAXVAL ( ABS ( A(1:N) ) )

  WRITE ( *, '(a)' ) ' '
  IF ( BIG < 1000 ) THEN
    DO I = 1, N
      WRITE ( *, '(i8,1x,i4)' ) I, A(I)
    END DO
  ELSE IF ( BIG < 1000000 ) THEN
    DO I = 1, N
      WRITE ( *, '(i8,1x,i7)' ) I, A(I)
    END DO
  ELSE
    DO I = 1, N
      WRITE ( *, '(i8,i11)' ) I, A(I)
    END DO
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4VEC_TO_CH4VEC ( N, I4VEC, S )

!*****************************************************************************80
!
!! I4VEC_TO_CH4VEC converts an I4VEC into a string.
!
!  Discussion:
!
!    This routine can be useful when trying to read character data from an
!    unformatted direct access file, for instance.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of integers.
!
!    Input, integer ( kind = 4 ) I4VEC(N), the integers.
!
!    Output, character ( len = * ) S, a string of 4 * N characters
!    representing the integer information.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4VEC(N)
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) LEN_S
  CHARACTER ( LEN = * ) S

  LEN_S = LEN ( S )

  IF ( LEN_S < 4 * N ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4VEC_TO_CH4VEC - Fatal error!'
    WRITE ( *, '(a)' ) '  String S is too small for the data.'
    RETURN
  END IF

  S(1:4*N) = ' '

  DO I = 1, N
    J = 4 * ( I - 1 ) + 1
    CALL I4_TO_CH4 ( I4VEC(I), S(J:J+3) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_BYTE_SWAP ( IWORD, BYTES )

!*****************************************************************************80
!
!! I4_BYTE_SWAP swaps bytes in a 4-byte word.
!
!  Discussion:
!
!    This routine uses the MVBITS routines to carry out the swaps.  The
!    relationship between the bits in the word (0 through 31) and the
!    bytes (1 through 4) is machine dependent.  That is, byte 1 may
!    comprise bits 0 through 7, or bits 24 through 31.  So some
!    experimentation may be necessary the first time this routine
!    is used.
!
!    This routine was originally written simply to take the drudgery
!    out of swapping bytes in a VAX word that was to be read by
!    another machine.
!
!    The statement
!
!      call i4_byte_swap ( IWORD, (/ 1, 2, 3, 4 /) )
!
!    will do nothing to IWORD, and
!
!      call i4_byte_swap ( IWORD, (/ 4, 3, 2, 1 /) )
!
!    will reverse the bytes in IWORD, and
!
!      call i4_byte_swap ( IWORD, (/ 2, 2, 2, 2 /) )
!
!    will replace IWORD with a word containing byte(2) repeated 4 times.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) IWORD, the word whose bits are to
!    be swapped.
!
!    Input, integer ( kind = 4 ) BYTES(4), indicates which byte in the
!    input word should overwrite each byte of the output word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: NUM_BYTES = 4

  INTEGER ( KIND = 4 ), PARAMETER :: BIT_LENGTH = 8
  INTEGER ( KIND = 4 ) BYTES(NUM_BYTES)
  INTEGER ( KIND = 4 ) FROM_POS
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IWORD
  INTEGER ( KIND = 4 ) JWORD
  INTEGER ( KIND = 4 ) TO_POS

  JWORD = IWORD

  DO I = 1, NUM_BYTES

    IF ( BYTES(I) < 1 .OR. NUM_BYTES < BYTES(I) ) THEN
      CYCLE
    END IF

    IF ( BYTES(I) == I ) THEN
      CYCLE
    END IF

    FROM_POS = 8 * ( BYTES(I) - 1 )
    TO_POS = 8 * ( I - 1 )
    CALL MVBITS ( JWORD, FROM_POS, BIT_LENGTH, IWORD, TO_POS )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_EXTRACT ( S, I, IERROR )

!*****************************************************************************80
!
!! I4_EXTRACT "extracts" an I4 from the beginning of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S; on input, a string from
!    whose beginning an integer is to be extracted.  On output,
!    the integer, if found, has been removed.
!
!    Output, integer ( kind = 4 ) I.  If IERROR is 0, then I contains the
!    next integer read from S; otherwise I is 0.
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no error.
!    nonzero, an integer could not be extracted from the beginning of the
!    string.  I is 0 and S is unchanged.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S

  I = 0

  CALL S_TO_I4 ( S, I, IERROR, LENGTH )

  IF ( IERROR /= 0 .OR. LENGTH == 0 ) THEN
    IERROR = 1
    I = 0
  ELSE
    CALL S_SHIFT_LEFT ( S, LENGTH )
  END IF

  RETURN
END

!*************************************************************

FUNCTION I4_GCD ( I, J )

!*****************************************************************************80
!
!! I4_GCD finds the greatest common divisor of I and J.
!
!  Discussion:
!
!    Note that only the absolute values of I and J are
!    considered, so that the result is always nonnegative.
!
!    If I or J is 0, I4_GCD is returned as max ( 1, abs ( I ), abs ( J ) ).
!
!    If I and J have no common factor, I4_GCD is returned as 1.
!
!    Otherwise, using the Euclidean algorithm, I_GCD is the
!    largest common factor of I and J.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, J, two numbers whose greatest common
!    divisor is desired.
!
!    Output, integer ( kind = 4 ) I4_GCD, the greatest common divisor of
!    I and J.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4_GCD
  INTEGER ( KIND = 4 ) IP
  INTEGER ( KIND = 4 ) IQ
  INTEGER ( KIND = 4 ) IR
  INTEGER ( KIND = 4 ) J

  I4_GCD = 1
!
!  Return immediately if either I or J is zero.
!
  IF ( I == 0 ) THEN
    I4_GCD = MAX ( 1, ABS ( J ) )
    RETURN
  ELSE IF ( J == 0 ) THEN
    I4_GCD = MAX ( 1, ABS ( I ) )
    RETURN
  END IF
!
!  Set IP to the larger of I and J, IQ to the smaller.
!  This way, we can alter IP and IQ as we go.
!
  IP = MAX ( ABS ( I ), ABS ( J ) )
  IQ = MIN ( ABS ( I ), ABS ( J ) )
!
!  Carry out the Euclidean algorithm.
!
  DO

    IR = MOD ( IP, IQ )

    IF ( IR == 0 ) THEN
      EXIT
    END IF

    IP = IQ
    IQ = IR

  END DO

  I4_GCD = IQ

  RETURN
END

!*************************************************************

FUNCTION I4_HUGE ( )

!*****************************************************************************80
!
!! I4_HUGE returns a "huge" I4.
!
!  Discussion:
!
!    On an IEEE 32 bit machine, I4_HUGE should be 2**31 - 1, and its
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
  
  INTEGER ( KIND = 4 ) I4_HUGE

  I4_HUGE = 2147483647

  RETURN
END

!*************************************************************

SUBROUTINE I4_INPUT ( STRING, VALUE, IERROR )

!*****************************************************************************80
!
!! I4_INPUT prints a prompt string and reads an I4 from the user.
!
!  Discussion:
!
!    If the input line starts with a comment character ('#') or is
!    blank, the routine ignores that line, and tries to read the next one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the prompt string.
!
!    Output, integer ( kind = 4 ) VALUE, the value input by the user.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag, which is zero
!    if no error occurred.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LAST
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) STRING
  INTEGER ( KIND = 4 ) VALUE

  IERROR = 0
  VALUE = HUGE ( VALUE )
!
!  Write the prompt.
!
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( STRING )

  DO

    READ ( *, '(a)', IOSTAT = IERROR ) LINE

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF
!
!  If the line begins with a comment character, go back and read the next line.
!
    IF ( LINE(1:1) == '#' ) THEN
      CYCLE
    END IF

    IF ( LEN_TRIM ( LINE ) == 0 ) THEN
      CYCLE
    END IF
!
!  Extract integer information from the string.
!
    CALL S_TO_I4 ( LINE, VALUE, IERROR, LAST )

    IF ( IERROR /= 0 ) THEN
      VALUE = HUGE ( VALUE )
      RETURN
    END IF

    EXIT

  END DO

  RETURN
END

!*************************************************************

FUNCTION I4_LENGTH ( I4 )

!*****************************************************************************80
!
!! I4_LENGTH computes the number of characters needed to print an I4.
!
!  Example:
!
!        I4    I4_LENGTH
!
!         0       1
!         1       1
!        -1       2
!      1952       4
!    123456       6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, the integer whose length is desired.
!
!    Output, integer ( kind = 4 ) I4_LENGTH, the number of characters required
!    to print the integer.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) I4_COPY
  INTEGER ( KIND = 4 ) I4_LENGTH

  IF ( I4 < 0 ) THEN
    I4_LENGTH = 1
  ELSE IF ( I4 == 0 ) THEN
    I4_LENGTH = 1
    RETURN
  ELSE IF ( 0 < I4 ) THEN
    I4_LENGTH = 0
  END IF

  I4_COPY = ABS ( I4 )

  DO WHILE ( I4_COPY /= 0 )

    I4_LENGTH = I4_LENGTH + 1

    I4_COPY = I4_COPY / 10

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_NEXT ( S, IVAL, DONE )

!*****************************************************************************80
!
!! I4_NEXT "reads" I4's from a string, one at a time.
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
!    Input, character ( len = * ) S, a string, presumably containing
!    integer ( kind = 4 )s.  These may be separated by spaces or commas.
!
!    Output, integer ( kind = 4 ) IVAL.  If DONE is FALSE, then IVAL contains
!    the "next" integer read.  If DONE is TRUE, then IVAL is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh string, the user should set DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another integer
!    was read, or TRUE if no more integers could be read.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ), SAVE :: NEXT = 1
  CHARACTER ( LEN = * ) S

  IVAL = 0

  IF ( DONE ) THEN
    NEXT = 1
    DONE = .FALSE.
  END IF

  IF ( LEN ( S ) < NEXT ) THEN
    DONE = .TRUE.
    RETURN
  END IF

  CALL S_TO_I4 ( S(NEXT:), IVAL, IERROR, LENGTH )

  IF ( IERROR /= 0 .OR. LENGTH == 0 ) THEN
    DONE = .TRUE.
    NEXT = 1
  ELSE
    DONE = .FALSE.
    NEXT = NEXT + LENGTH
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_NEXT_READ ( S, INTVAL, IERROR )

!*****************************************************************************80
!
!! I4_NEXT_READ finds and reads the next I4 in a string.
!
!  Discussion:
!
!    This routine can be used to extract, one at a time, the integers in
!    a string.
!
!  Example:
!
!    Input:
!
!      S = 'Data set #12 extends from (5,-43) and is worth $4.56'
!      IERROR = -1
!
!    Output:
!
!      (on successive calls)
!
!      INTVAL  IERROR
!      ------  ------
!           1       0
!           2       0
!           5       0
!         -43       0
!           4       0
!          56       0
!           0       1
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) INTVAL, the next integer in the string, or 0
!    if no integer could be found.
!
!    Input/output, integer ( kind = 4 ) IERROR.
!    On the first call for a given string, set IERROR = -1.
!    Thereafter, the routine will return IERROR = 0 if another
!    integer ( kind = 4 ) was found, or 1 if no more integers were found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ), SAVE :: ISTART = 1
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S

  IF ( IERROR == -1 ) THEN
    ISTART = 1
  END IF

  IERROR = 0
  INTVAL = 0

  IF ( LEN_TRIM ( S ) < ISTART ) THEN
    IERROR = 1
    RETURN
  END IF

  CALL CHRCTI2 ( S(ISTART:), INTVAL, IERROR, LENGTH )

  IF ( IERROR == 0 .AND. 0 < LENGTH ) THEN
    ISTART = ISTART + LENGTH
  ELSE
    IERROR = 1
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_RANGE_INPUT ( STRING, VALUE1, VALUE2, IERROR )

!*****************************************************************************80
!
!! I4_RANGE_INPUT reads a pair of I4's from the user, representing a range.
!
!  Discussion:
!
!    If the input line starts with a comment character ('#') or is blank,
!    the routine ignores that line, and tries to read the next one.
!
!    The pair of integers may be separated by spaces or a comma or both.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the prompt string.
!
!    Output, integer ( kind = 4 ) VALUE1, VALUE2, the values entered by
!    the user.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag, which is zero
!    if no error occurred.
!
  IMPLICIT NONE

  CHARACTER, PARAMETER :: COMMA = ','
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LAST
  INTEGER ( KIND = 4 ) LAST2
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER, PARAMETER :: SPACE = ' '
  CHARACTER ( LEN = * ) STRING
  INTEGER ( KIND = 4 ) VALUE1
  INTEGER ( KIND = 4 ) VALUE2

  IERROR = 0
  VALUE1 = HUGE ( VALUE1 )
  VALUE2 = HUGE ( VALUE2 )
!
!  Write the prompt.
!
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( STRING )

  DO

    READ ( *, '(a)', IOSTAT = IERROR ) LINE

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF
!
!  If the line begins with a comment character, go back and read the next line.
!
    IF ( LINE(1:1) == '#' ) THEN
      CYCLE
    END IF

    IF ( LEN_TRIM ( LINE ) == 0 ) THEN
      CYCLE
    END IF
!
!  Replace commas by spaces.
!
    CALL S_REPLACE_CH ( LINE, COMMA, SPACE )
!
!  Extract integer information from the string.
!
    CALL S_TO_I4 ( LINE, VALUE1, IERROR, LAST )

    IF ( IERROR /= 0 ) THEN
      VALUE1 = HUGE ( VALUE1 )
      RETURN
    END IF

    CALL S_TO_I4 ( LINE(LAST+1:), VALUE2, IERROR, LAST2 )

    IF ( IERROR /= 0 ) THEN
      VALUE2 = HUGE ( VALUE2 )
      RETURN
    END IF

    EXIT

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_SWAP ( I, J )

!*****************************************************************************80
!
!! I4_SWAP swaps two I4's.
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
!    Input/output, integer ( kind = 4 ) I, J.  On output, the values of I and
!    J have been interchanged.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) K

  K = I
  I = J
  J = K

  RETURN
END

!*************************************************************

FUNCTION I4_TO_A ( I )

!*****************************************************************************80
!
!! I4_TO_A returns the I-th alphabetic character.
!
!  Discussion:
!
!    Instead of CHAR, we now use the ACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Example:
!
!    I  I4_TO_A
!
!   -8  ' '
!    0  ' '
!    1  'A'
!    2  'B'
!   ..
!   26  'Z'
!   27  'a'
!   52  'z'
!   53  ' '
!   99  ' '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the letter to be returned.
!    0 is a space;
!    1 through 26 requests 'A' through 'Z', (ASCII 65:90);
!    27 through 52 requests 'a' through 'z', (ASCII 97:122);
!
!    Output, character I4_TO_A, the requested alphabetic letter.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: CAP_SHIFT = 64
  INTEGER ( KIND = 4 ) I
  CHARACTER I4_TO_A
  INTEGER ( KIND = 4 ), PARAMETER :: LOW_SHIFT = 96

  IF ( I <= 0 ) THEN
    I4_TO_A = ' '
  ELSE IF ( 1 <= I .AND. I <= 26 ) THEN
    I4_TO_A = ACHAR ( CAP_SHIFT + I )
  ELSE IF ( 27 <= I .AND. I <= 52 ) THEN
    I4_TO_A = ACHAR ( LOW_SHIFT + I - 26 )
  ELSE IF ( 53 <= I ) THEN
    I4_TO_A = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_AMINO_CODE ( I, CH )

!*****************************************************************************80
!
!! I4_TO_AMINO_CODE converts an integer to an amino code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Carl Branden, John Tooze,
!    Introduction to Protein Structure,
!    Garland Publishing, 1991.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of an amino acid, between 1
!    and 23.
!
!    Output, character CH, the one letter code for an amino acid.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ), PARAMETER :: N = 23

  CHARACTER CH
  CHARACTER, DIMENSION ( N ) :: CH_TABLE = (/ &
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'K', &
    'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', &
    'X', 'Y', 'Z' /)
  INTEGER ( KIND = 4 ) I

  IF ( 1 <= I .AND. I <= 23 ) THEN
    CH = CH_TABLE(I)
  ELSE
    CH = '?'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_BASE ( I4, BASE, S )

!*****************************************************************************80
!
!! I4_TO_BASE represents an integer in any base up to 16.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!      Input            Output
!    -------------    --------
!    INTVAL   BASE           S
!
!         5    -1   '101010101'
!         5     1       '11111'
!         5     2         '101'
!         5     3          '12'
!         5     4          '11'
!        -5     5         '-10'
!         5     6           '5'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, the integer whose representation
!    is desired.
!
!    Input, integer ( kind = 4 ) BASE, the base in which the representation is
!    given.  BASE must be greater than 0 and no greater than 16.
!
!    Output, character ( len = * ) S, the string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) BASE
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) ICOPY
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) JDIG
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S = ' '
  S_LENGTH = LEN ( S )
!
!  Check the base.
!
  IF ( BASE < -1 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4_TO_BASE - Serious error!'
    WRITE ( *, '(a)' ) '  The input base is less than -1!'
    RETURN
  END IF

  IF ( BASE == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4_TO_BASE - Serious error!'
    WRITE ( *, '(a)' ) '  The input base is zero.'
    RETURN
  END IF

  IF ( 16 < BASE ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4_TO_BASE - Serious error!'
    WRITE ( *, '(a)' ) '  The input base is greater than 16!'
    RETURN
  END IF
!
!  Special treatment for base 1 and -1.
!
  IF ( BASE == 1 ) THEN
    CALL I4_TO_UNARY ( I4, S )
    RETURN
  ELSE IF ( BASE == -1 ) THEN
    CALL I4_TO_NUNARY ( I4, S )
    RETURN
  END IF
!
!  Do repeated mod's
!
  JDIG = 0
  ICOPY = ABS ( I4 )

  DO

    IF ( ( 0 <= I4 .AND. S_LENGTH <= JDIG ) .OR. &
         ( I4 < 0 .AND. S_LENGTH - 1 <= JDIG ) ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'I4_TO_BASE - Fatal error!'
      DO I = 1, S_LENGTH
        S(I:I) = '*'
      END DO
      RETURN
    END IF

    JDIG = JDIG + 1

    IDIG = MOD ( ICOPY, BASE )
    ICOPY = ( ICOPY - IDIG ) / BASE

    CALL I4_TO_HEX_DIGIT ( IDIG, S(S_LENGTH+1-JDIG:S_LENGTH+1-JDIG) )

    IF ( ICOPY == 0 ) THEN
      EXIT
    END IF

  END DO
!
!  Take care of the minus sign.
!
  IF ( I4 < 0 ) THEN
    JDIG = JDIG + 1
    S(S_LENGTH+1-JDIG:S_LENGTH+1-JDIG) = '-'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_BINARY ( I, S )

!*****************************************************************************80
!
!! I4_TO_BINARY produces the binary representation of an I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!     I       S
!    --  ------
!     1      '1'
!     2     '10'
!     3     '11'
!     4    '100'
!    -9  '-1001'
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
!    Input, integer ( kind = 4 ) I, an integer to be represented.
!
!    Output, character ( len = * ) S, the binary representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I_COPY
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = * ) S

  I_COPY = ABS ( I )
  S = ' '
  J = LEN ( S )

  DO WHILE ( 0 < J )

    IF ( MOD ( I_COPY, 2 ) == 1 ) THEN
      S(J:J) = '1'
    ELSE
      S(J:J) = '0'
    END IF

    I_COPY = I_COPY / 2

    IF ( I_COPY == 0 ) THEN
      EXIT
    END IF

    J = J - 1

  END DO

  IF ( I_COPY /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4_TO_BINARY - Serious error!'
    WRITE ( *, '(a)' ) '  Not enough room in the string to represent the value.'
  END IF

  IF ( I < 0 ) THEN

    IF ( 1 < J ) THEN
      J = J - 1
      S(J:J) = '-'
    ELSE
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'I4_TO_BINARY - Serious error!'
      WRITE ( *, '(a)' ) '  No room to prefix minus sign!'
    END IF

  END IF

  RETURN
END

!*************************************************************

FUNCTION I4_TO_BINHEX ( I )

!*****************************************************************************80
!
!! I4_TO_BINHEX returns the I-th character in the BINHEX encoding.
!
!  Example:
!
!    I  I4_TO_BINHEX
!
!    1  '!'
!    2  '"'
!    3  '#'
!   ..
!   64  'r'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the character.
!    1 <= I <= 64.
!
!    Output, character I4_TO_BINHEX, the requested character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER I4_TO_BINHEX
  CHARACTER ( LEN = 64 ), PARAMETER :: STRING = &
    '!"#$%&''()*+,-012345689@ABCDEFGHIJKLMNPQRSTVWXYZ[`ABCDEFHIJKLMNPQR'

  IF ( 1 <= I .AND. I <= 64 ) THEN
    I4_TO_BINHEX = STRING(I:I)
  ELSE
    I4_TO_BINHEX = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_CH4 ( I4, CH4 )

!*****************************************************************************80
!
!! I4_TO_CH4 converts an I4 to a 4 character string.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!    While most integers will be converted to unprintable strings,
!    here are a few nice ones:
!
!    1097097581  Adam
!    1114205292  Bill
!    1131573111  Crow
!    1147237989  Dave
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, the integer value.
!
!    Output, character ( len = 4 ) CH4, a corresponding character value.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 4 ) CH4
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) J2
  INTEGER ( KIND = 4 ) J3
  INTEGER ( KIND = 4 ) J4

  J1 = IBITS ( I4,  0, 8 )
  J2 = IBITS ( I4,  8, 8 )
  J3 = IBITS ( I4, 16, 8 )
  J4 = IBITS ( I4, 24, 8 )

  CH4 = ACHAR ( J1 ) // ACHAR ( J2 ) // ACHAR ( J3 ) // ACHAR ( J4 )

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_HEX ( I4, S )

!*****************************************************************************80
!
!! I4_TO_HEX produces the hexadecimal representation of an I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!        I4      S
!       ---     ---
!         0     '0'
!         9     '9'
!        10     'A'
!        15     'F'
!        16    '10'
!       100    '64'
!       -12    '-C'
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
!    Input, integer ( kind = 4 ) I4, the integer to be represented.
!
!    Output, character ( len = * ) S, the hexadecimal representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) INTCPY
  INTEGER ( KIND = 4 ) ISGN
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  INTCPY = I4
  ISGN = 1

  IF ( INTCPY < 0 ) THEN
    ISGN = -1
    INTCPY = - INTCPY
  END IF

  S = ' '
!
!  Point to the position just after the end of the string.
!
  ICHR = S_LENGTH + 1
!
!  Moving left, fill in the next digit of the string.
!
  DO

    ICHR = ICHR - 1

    IF ( ICHR <= 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'I4_TO_HEX - Serious error!'
      WRITE ( *, '(a)' ) '  Ran out of room in the string!'
      RETURN
    END IF

    I1 = MOD ( INTCPY, 16 )
    INTCPY = INTCPY / 16

    CALL I4_TO_HEX_DIGIT ( I1, S(ICHR:ICHR) )

    IF ( INTCPY == 0 ) THEN

      IF ( ISGN == -1 ) THEN

        IF ( 1 < ICHR ) THEN
          ICHR = ICHR - 1
          S(ICHR:ICHR) = '-'
        ELSE
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'I4_TO_HEX - Serious error!'
          WRITE ( *, '(a)' ) '  No room to prefix minus sign!'
        END IF

      END IF

      RETURN

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_HEX_DIGIT ( I, CH )

!*****************************************************************************80
!
!! I4_TO_HEX_DIGIT converts a (small) I4 to a hexadecimal digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the integer, between 0 and 15.
!
!    Output, character CH, the hexadecimal digit corresponding to the integer.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I

  IF ( 0 <= I .AND. I <= 9 ) THEN
    CH = ACHAR ( I + 48 )
  ELSE IF ( 10 <= I .AND. I <= 15 ) THEN
    CH = ACHAR ( I + 55 )
  ELSE
    CH = '*'
  END IF

  RETURN
END

!*************************************************************

FUNCTION I4_TO_ISBN ( I )

!*****************************************************************************80
!
!! I4_TO_ISBN converts an I4 to an ISBN digit.
!
!  Discussion:
!
!    Only the integers 0 through 10 can be input.  The representation
!    of 10 is 'X'.
!
!    An I4 is an integer ( kind = 4 ) value.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Book Industry Study Group,
!    The Evolution in Product Identification:
!    Sunrise 2005 and the ISBN-13,
!    http://www.bisg.org/docs/The_Evolution_in_Product_ID.pdf
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, an integer between 0 and 10.
!
!    Output, character I4_TO_ISBN, the ISBN character code of the integer.
!    If I is illegal, then I4_TO_ISBN is set to '?'.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER I4_TO_ISBN

       IF ( I == 0 ) THEN
    I4_TO_ISBN = '0'
  ELSE IF ( I == 1 ) THEN
    I4_TO_ISBN = '1'
  ELSE IF ( I == 2 ) THEN
    I4_TO_ISBN = '2'
  ELSE IF ( I == 3 ) THEN
    I4_TO_ISBN = '3'
  ELSE IF ( I == 4 ) THEN
    I4_TO_ISBN = '4'
  ELSE IF ( I == 5 ) THEN
    I4_TO_ISBN = '5'
  ELSE IF ( I == 6 ) THEN
    I4_TO_ISBN = '6'
  ELSE IF ( I == 7 ) THEN
    I4_TO_ISBN = '7'
  ELSE IF ( I == 8 ) THEN
    I4_TO_ISBN = '8'
  ELSE IF ( I == 9 ) THEN
    I4_TO_ISBN = '9'
  ELSE IF ( I == 10 ) THEN
    I4_TO_ISBN = 'X'
  ELSE
    I4_TO_ISBN = '?'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_MONTH_ABB ( M, MONTH_ABB )

!*****************************************************************************80
!
!! I4_TO_MONTH_ABB returns the 3 character abbreviation of a given month.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the index of the month, which should
!    be between 1 and 12.
!
!    Output, character ( len = 3 ) MONTH_ABB, the month abbreviation
!
  IMPLICIT NONE

  CHARACTER ( LEN = 3 ), PARAMETER, DIMENSION(12) :: ABB = (/ &
    'Jan', 'Feb', 'Mar', 'Apr', &
    'May', 'Jun', 'Jul', 'Aug', &
    'Sep', 'Oct', 'Nov', 'Dec' /)
  INTEGER ( KIND = 4 ) M
  CHARACTER ( LEN = 3 ) MONTH_ABB

  IF ( M < 1 .OR. 12 < M ) THEN

    MONTH_ABB = '???'

  ELSE

    MONTH_ABB = ABB(M)

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_MONTH_NAME ( M, MONTH_NAME )

!*****************************************************************************80
!
!! I4_TO_MONTH_NAME returns the name of a given month.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 April 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the index of the month, which should
!    be between 1 and 12.
!
!    Output, character ( len = * ) MONTH_NAME, a string containing as much
!    of the month's name as will fit.  To get the typical 3-letter abbreviations
!    for the months, simply declare
!      character ( len = 3 ) MONTH_NAME
!    or pass in MONTH_NAME(1:3).
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) M
  CHARACTER ( LEN = * ) MONTH_NAME
  CHARACTER ( LEN = 9 ), PARAMETER, DIMENSION(12) :: NAME = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)

  IF ( M < 1 .OR. 12 < M ) THEN

    DO I = 1, LEN ( MONTH_NAME )
      MONTH_NAME(I:I) = '?'
    END DO

  ELSE

    MONTH_NAME = NAME(M)

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_NUNARY ( INTVAL, S )

!*****************************************************************************80
!
!! I4_TO_NUNARY produces the "base -1" representation of an I4.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) INTVAL, an integer to be represented.
!
!    Output, character ( len = * ) S, the negative unary representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INTVAL
  CHARACTER ( LEN = * ) S

  S = ' '

  IF ( INTVAL < 0 ) THEN

    DO I = 1, ABS ( INTVAL )
      S(2*I-1:2*I) = '10'
    END DO

  ELSE IF ( INTVAL == 0 ) THEN

      S = '0'

  ELSE IF ( 0 < INTVAL ) THEN

    S(1:1) = '1'
    DO I = 2, INTVAL
      S(2*I-2:2*I-1) = '01'
    END DO

  END IF

  S = ADJUSTR ( S )

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_OCT ( I4, S )

!*****************************************************************************80
!
!! I4_TO_OCT produces the octal representation of an integer.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!        I4      S
!
!         0     '0'
!         9    '11'
!        10    '12'
!        15    '17'
!        16    '20'
!       100   '144'
!       -12   '-14'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, the integer to be represented.
!
!    Output, character ( len = * ) S, the octal representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) INTCPY
  INTEGER ( KIND = 4 ) ISGN
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  INTCPY = I4
  ISGN = 1

  IF ( INTCPY < 0 ) THEN
    ISGN = - 1
    INTCPY = -INTCPY
  END IF

  S = ' '
!
!  Point to the position just after the end of the string.
!
  ICHR = S_LENGTH + 1
!
!  Moving left, fill in the next digit of the string.
!
  DO

    ICHR = ICHR - 1

    IF ( ICHR <= 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'I4_TO_OCT - Fatal error!'
      WRITE ( *, '(a)' ) '  Ran out of room in the string!'
      RETURN
    END IF

    I1 = MOD ( INTCPY, 8 )
    INTCPY = INTCPY / 8

    CALL DIGIT_OCT_TO_CH ( I1, S(ICHR:ICHR) )

    IF ( INTCPY == 0 ) THEN

      IF ( ISGN == -1 ) THEN

        IF ( 1 < ICHR ) THEN
          ICHR = ICHR - 1
          S(ICHR:ICHR) = '-'
        ELSE
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'I4_TO_OCT - Fatal error!'
          WRITE ( *, '(a)' ) '  No room to prefix minus sign!'
          RETURN
        END IF

      END IF

      RETURN

    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION I4_TO_S32 ( I4 )

!*****************************************************************************80
!
!! I4_TO_S32 converts an I4 to an S32.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!    An S32 is a character ( len = 32 ).
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
!    Input, integer ( kind = 4 ) I4, the integer to be coded.
!
!    Output, character ( len = 32 ) I4_TO_S32, the character variable that
!    corresponds to the integer.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) I4_COPY
  CHARACTER ( LEN = 32 ) I4_TO_S32
  CHARACTER ( LEN = 32 ) S32

  I4_COPY = ABS ( I4 )
!
!  Binary digits:
!
  DO I = 32, 2, -1

    IF ( MOD ( I4_COPY, 2 ) == 1 ) THEN
      S32(I:I) = '1'
    ELSE
      S32(I:I) = '0'
    END IF

    I4_COPY = I4_COPY / 2

  END DO
!
!  Sign bit
!
  S32(1:1) = '0'
!
!  If original number was negative, then reverse all bits.
!
  IF ( I4 < 0 ) THEN
    DO I = 1, 32
      IF ( S32(I:I) == '0' ) THEN
        S32(I:I) = '1'
      ELSE
        S32(I:I) = '0'
      END IF
    END DO
  END IF

  I4_TO_S32 = S32

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_S_LEFT ( I4, S )

!*****************************************************************************80
!
!! I4_TO_S_LEFT converts an I4 to a left-justified string.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!    Assume that S is 6 characters long:
!
!        I4  S
!
!         1  1
!        -1  -1
!         0  0
!      1952  1952
!    123456  123456
!   1234567  ******  <-- Not enough room!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, an integer to be converted.
!
!    Output, character ( len = * ) S, the representation of the integer.
!    The integer will be left-justified.  If there is not enough space,
!    the string will be filled with stars.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IPOS
  INTEGER ( KIND = 4 ) IVAL
  CHARACTER ( LEN = * ) S

  S = ' '

  ILO = 1
  IHI = LEN ( S )

  IF ( IHI <= 0 ) THEN
    RETURN
  END IF
!
!  Make a copy of the integer.
!
  IVAL = I4
!
!  Handle the negative sign.
!
  IF ( IVAL < 0 ) THEN

    IF ( IHI <= 1 ) THEN
      S(1:1) = '*'
      RETURN
    END IF

    IVAL = -IVAL
    S(1:1) = '-'
    ILO = 2

  END IF
!
!  The absolute value of the integer goes into S(ILO:IHI).
!
  IPOS = IHI
!
!  Find the last digit of IVAL, strip it off, and stick it into the string.
!
  DO

    IDIG = MOD ( IVAL, 10 )
    IVAL = IVAL / 10

    IF ( IPOS < ILO ) THEN
      DO I = 1, IHI
        S(I:I) = '*'
      END DO
      RETURN
    END IF

    CALL DIGIT_TO_CH ( IDIG, C )

    S(IPOS:IPOS) = C
    IPOS = IPOS - 1

    IF ( IVAL == 0 ) THEN
      EXIT
    END IF

  END DO
!
!  Shift the string to the left.
!
  S(ILO:ILO+IHI-IPOS-1) = S(IPOS+1:IHI)
  S(ILO+IHI-IPOS:IHI) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_S_RIGHT ( INTVAL, S )

!*****************************************************************************80
!
!! I4_TO_S_RIGHT converts an I4 to a right justified string.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!    Assume that S is 6 characters long:
!
!    INTVAL       S
!
!         1       1
!        -1      -1
!         0       0
!      1952    1952
!    123456  123456
!   1234567  ******  <-- Not enough room!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) INTVAL, an integer to be converted.
!
!    Output, character ( len = * ) S, the representation of the integer.
!    The integer will be right-justified.  If there is not enough space,
!    the string will be filled with stars.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) IPOS
  INTEGER ( KIND = 4 ) IVAL
  CHARACTER ( LEN = * ) S

  S = ' '

  ILO = 1
  IHI = LEN ( S )

  IF ( IHI <= 0 ) THEN
    RETURN
  END IF
!
!  Make a copy of the integer.
!
  IVAL = INTVAL
!
!  Handle the negative sign.
!
  IF ( IVAL < 0 ) THEN

    IF ( IHI <= 1 ) THEN
      S(1:1) = '*'
      RETURN
    END IF

    IVAL = -IVAL
    S(1:1) = '-'
    ILO = 2

  END IF
!
!  The absolute value of the integer goes into S(ILO:IHI).
!
  IPOS = IHI
!
!  Find the last digit of IVAL, strip it off, and stick it into the string.
!
  DO

    IDIG = MOD ( IVAL, 10 )
    IVAL = IVAL / 10

    IF ( IPOS < ILO ) THEN
      DO I = 1, IHI
        S(I:I) = '*'
      END DO
      RETURN
    END IF

    CALL DIGIT_TO_CH ( IDIG, C )
    S(IPOS:IPOS) = C
    IPOS = IPOS - 1

    IF ( IVAL == 0 ) THEN
      EXIT
    END IF

  END DO
!
!  Shift the minus sign, if any.
!
  IF ( S(1:1) == '-' ) THEN
    IF ( IPOS /= 1 ) THEN
      S(1:1) = ' '
      S(IPOS:IPOS) = '-'
    END IF
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_S_RIGHT_COMMA ( I4, S )

!*****************************************************************************80
!
!! I4_TO_S_RIGHT_COMMA converts an I4 to a right justified string with commas.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!    Assume that S is 10 characters long:
!
!          I4           S
!
!           1           1
!          -1          -1
!           0           0
!        1952       1,952
!      123456     123,456
!     1234567   1,234,567
!    12345678  12,345,678
!   123456789  **********  <-- Not enough room!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, an integer to be converted.
!
!    Output, character ( len = * ) S, the representation of the integer.
!    The integer will be right-justified.  Commas will be used to separate
!    sets of three digits.  If there is not enough space, the string will
!    be filled with stars.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) DIGIT
  INTEGER ( KIND = 4 ) DIGIT_NUM
  INTEGER ( KIND = 4 ) HI
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) LO
  INTEGER ( KIND = 4 ) POS
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) VALUE

  S = ' '

  LO = 1
  HI = LEN ( S )

  IF ( HI <= 0 ) THEN
    RETURN
  END IF
!
!  Make a copy of the integer.
!
  VALUE = I4
!
!  Handle the negative sign.
!
  IF ( VALUE < 0 ) THEN

    IF ( HI <= 1 ) THEN
      S(1:1) = '*'
      RETURN
    END IF

    VALUE = -VALUE
    S(1:1) = '-'
    LO = 2

  END IF
!
!  The absolute value of the integer goes into S(LO:HI).
!
  POS = HI
!
!  Find the last digit of VALUE, strip it off, and stick it into the string.
!
  DIGIT_NUM = 0

  DO

    DIGIT = MOD ( VALUE, 10 )
    VALUE = VALUE / 10
    DIGIT_NUM = DIGIT_NUM + 1

    IF ( POS < LO ) THEN
      DO I = 1, HI
        S(I:I) = '*'
      END DO
      RETURN
    END IF
!
!  Insert a comma?
!
    IF ( 1 < DIGIT_NUM .AND. MOD ( DIGIT_NUM, 3 ) == 1 ) THEN

      IF ( POS < LO ) THEN
        DO I = 1, HI
          S(I:I) = '*'
        END DO
        RETURN
      END IF

      S(POS:POS) = ','
      POS = POS - 1
    END IF

    CALL DIGIT_TO_CH ( DIGIT, C )
    S(POS:POS) = C
    POS = POS - 1

    IF ( VALUE == 0 ) THEN
      EXIT
    END IF

  END DO
!
!  Shift the minus sign, if any.
!
  IF ( S(1:1) == '-' ) THEN
    IF ( POS /= 1 ) THEN
      S(1:1) = ' '
      S(POS:POS) = '-'
    END IF
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_S_ROMAN ( INTVAL, S )

!*****************************************************************************80
!
!! I4_TO_S_ROMAN converts an I4 to a string of Roman numerals.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!    To generate numbers greater than 4999, the numeral 'V' had a bar
!    above it, representing a value of 5000, a barred 'X' represented
!    10,000 and so on.
!
!    In the subtractive representation of 4 by 'IV', 9 by 'IX' and so on,
!    'I' can only subtract from 'V' or 'X',
!    'X' can only subtract from 'L' or 'C',
!    'C' can only subtract from 'D' or 'M'.
!    Under these rules, 1999 cannot be written IMM!
!
!  Example:
!
!    INTVAL  S
!
!        -2  -II <-- Not a Roman numeral
!        -1  -I  <-- Not a Roman numeral
!         0   0  <-- Not a Roman numeral
!         1   I
!         2   II
!         3   III
!         4   IV
!         5   V
!        10   X
!        20   XX
!        30   XXX
!        40   XL
!        50   L
!        60   LX
!        70   LXX
!        80   LXXX
!        90   XC
!       100   C
!       500   D
!      1000   M
!      4999   MMMMCMLXLIX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) INTVAL, an integer to be converted.  If the
!    integer has absolute value greater than 4999, the string '?' will be
!    returned.  If the integer is 0, then the string '0' will be returned.  If
!    the integer is negative, then a minus sign will precede it, even
!    though this has nothing to do with Roman numerals.
!
!    Output, character ( len = * ) S, the representation of the integer
!    as a Roman numeral.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) ICOPY
  INTEGER ( KIND = 4 ) INTVAL
  CHARACTER ( LEN = * ) S

  S = ' '
  ICOPY = INTVAL

  IF ( 4999 < ABS ( ICOPY ) ) THEN
    S = '?'
    RETURN
  END IF

  IF ( ICOPY == 0 ) THEN
    S = '0'
    RETURN
  END IF

  IF ( ICOPY <= 0 ) THEN
    S = '-'
    ICOPY = -ICOPY
  END IF

  DO WHILE ( 0 < ICOPY )

    IF ( 1000 <= ICOPY ) THEN
      CALL S_CAT ( S, 'M', S )
      ICOPY = ICOPY - 1000
    ELSE IF ( 900 <= ICOPY ) THEN
      CALL S_CAT ( S, 'CM', S )
      ICOPY = ICOPY - 900
    ELSE IF ( 500 <= ICOPY ) THEN
      CALL S_CAT ( S, 'D', S )
      ICOPY = ICOPY - 500
    ELSE IF ( 400 <= ICOPY ) THEN
      CALL S_CAT ( S, 'CD', S )
      ICOPY = ICOPY - 400
    ELSE IF ( 100 <= ICOPY ) THEN
      CALL S_CAT ( S, 'C', S )
      ICOPY = ICOPY - 100
    ELSE IF ( 90 <= ICOPY ) THEN
      CALL S_CAT ( S, 'XC', S )
      ICOPY = ICOPY - 90
    ELSE IF ( 50 <= ICOPY ) THEN
      CALL S_CAT ( S, 'L', S )
      ICOPY = ICOPY - 50
    ELSE IF ( 40 <= ICOPY ) THEN
      CALL S_CAT ( S, 'XL', S )
      ICOPY = ICOPY - 40
    ELSE IF ( 10 <= ICOPY ) THEN
      CALL S_CAT ( S, 'X', S )
      ICOPY = ICOPY - 10
    ELSE IF ( 9 <= ICOPY ) THEN
      CALL S_CAT ( S, 'IX', S )
      ICOPY = ICOPY - 9
    ELSE IF ( 5 <= ICOPY ) THEN
      CALL S_CAT ( S, 'V', S )
      ICOPY = ICOPY - 5
    ELSE IF ( 4 <= ICOPY ) THEN
      CALL S_CAT ( S, 'IV', S )
      ICOPY = ICOPY - 4
    ELSE
      CALL S_CAT ( S, 'I', S )
      ICOPY = ICOPY - 1
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_S_ZERO ( INTVAL, S )

!*****************************************************************************80
!
!! I4_TO_S_ZERO converts an I4 to a string, with zero padding.
!
!  Discussion:
!
!    An I4 is an integer ( kind = 4 ).
!
!  Example:
!
!    Assume that S is 6 characters long:
!
!    INTVAL  S
!
!         1  000001
!        -1  -00001
!         0  000000
!      1952  001952
!    123456  123456
!   1234567  ******  <-- Not enough room!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) INTVAL, an integer to be converted.
!
!    Output, character ( len = * ) S, the representation of the integer.
!    The integer will be right justified, and zero padded.
!    If there is not enough space, the string will be filled with stars.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) IPOS
  INTEGER ( KIND = 4 ) IVAL
  CHARACTER ( LEN = * ) S

  S = ' '

  ILO = 1
  IHI = LEN ( S )

  IF ( IHI <= 0 ) THEN
    RETURN
  END IF
!
!  Make a copy of the integer.
!
  IVAL = INTVAL
!
!  Handle the negative sign.
!
  IF ( IVAL < 0 ) THEN

    IF ( IHI <= 1 ) THEN
      S(1:1) = '*'
      RETURN
    END IF

    IVAL = -IVAL
    S(1:1) = '-'
    ILO = 2

  END IF
!
!  Working from right to left, strip off the digits of the integer
!  and place them into S(ILO:IHI).
!
  IPOS = IHI

  DO WHILE ( IVAL /= 0 .OR. IPOS == IHI )

    IDIG = MOD ( IVAL, 10 )
    IVAL = IVAL / 10

    IF ( IPOS < ILO ) THEN
      DO I = 1, IHI
        S(I:I) = '*'
      END DO
      RETURN
    END IF

    CALL DIGIT_TO_CH ( IDIG, C )

    S(IPOS:IPOS) = C
    IPOS = IPOS - 1

  END DO
!
!  Fill the empties with zeroes.
!
  DO I = ILO, IPOS
    S(I:I) = '0'
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE I4_TO_UNARY ( I4, S )

!*****************************************************************************80
!
!! I4_TO_UNARY produces the "base 1" representation of an I4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I4, an integer to be represented.
!
!    Output, character ( len = * ) S, the unary representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I4
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )
  S = ' '

  IF ( I4 < 0 ) THEN

    IF ( S_LENGTH < I4 + 1 ) THEN
      S = '?'
      RETURN
    END IF

    S(1:1) = '-'

    DO I = 2, ABS ( I4 ) + 1
      S(I:I) = '1'
    END DO

  ELSE IF ( I4 == 0 ) THEN

    S = '0'

  ELSE IF ( 0 < I4 ) THEN

    IF ( S_LENGTH < I4 ) THEN
      S = '?'
      RETURN
    END IF

    DO I = 1, I4
      S(I:I) = '1'
    END DO

  END IF

  S = ADJUSTR ( S )

  RETURN
END

!*************************************************************

FUNCTION I4_TO_UUDECODE ( I )

!*****************************************************************************80
!
!! I4_TO_UUDECODE returns the I-th character in the UUDECODE encoding.
!
!  Example:
!
!    I  I4_TO_UUDECODE
!
!    1  '`'
!    2  '!'
!    3  '"'
!   ..
!   64  '_'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the character.
!    1 <= I <= 64.
!
!    Output, character I4_TO_UUDECODE, the requested character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER I4_TO_UUDECODE
  CHARACTER ( LEN = 64 ), PARAMETER :: STRING = &
    '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'

  IF ( 1 <= I .AND. I <= 64 ) THEN
    I4_TO_UUDECODE = STRING(I:I)
  ELSE
    I4_TO_UUDECODE = ' '
  END IF

  RETURN
END

!*************************************************************

FUNCTION I4_TO_XXDECODE ( I )

!*****************************************************************************80
!
!! I4_TO_XXDECODE returns the I-th character in the XXDECODE encoding.
!
!  Example:
!
!    I  I4_TO_UUDECODE
!
!    1  '+'
!    2  '-'
!    3  '0'
!   ..
!   64  'z'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the character.
!    1 <= I <= 64.
!
!    Output, character I4_TO_XXDECODE, the requested character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER I4_TO_XXDECODE
  CHARACTER ( LEN = 64 ), PARAMETER :: STRING = &
    '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

  IF ( 1 <= I .AND. I <= 64 ) THEN
    I4_TO_XXDECODE = STRING(I:I)
  ELSE
    I4_TO_XXDECODE = ' '
  END IF

  RETURN
END

!*************************************************************

FUNCTION I4_UNIFORM ( A, B, SEED )

!*****************************************************************************80
!
!! I4_UNIFORM returns a scaled pseudorandom I4.
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
!    Output, integer ( kind = 4 ) I4_UNIFORM, a number between A and B.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) A
  INTEGER ( KIND = 4 ) B
  INTEGER ( KIND = 4 ) I4_UNIFORM
  INTEGER ( KIND = 4 ) K
  REAL ( KIND = 4 ) R
  INTEGER ( KIND = 4 ) SEED
  INTEGER ( KIND = 4 ) VALUE

  IF ( SEED == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'I4_UNIFORM - Fatal error!'
    WRITE ( *, '(a)' ) '  Input value of SEED = 0.'
    RETURN
  END IF

  K = SEED / 127773

  SEED = 16807 * ( SEED - K * 127773 ) - K * 2836

  IF ( SEED < 0 ) THEN
    SEED = SEED + 2147483647
  END IF

  R = REAL ( SEED, KIND = 4 ) * 4.656612875E-10
!
!  Scale R to lie between A-0.5 and B+0.5.
!
  R = ( 1.0E+00 - R ) * ( REAL ( MIN ( A, B ), KIND = 4 ) - 0.5E+00 ) &
    +             R   * ( REAL ( MAX ( A, B ), KIND = 4 ) + 0.5E+00 )
!
!  Use rounding to convert R to an integer between A and B.
!
  VALUE = NINT ( R, KIND = 4 )

  VALUE = MAX ( VALUE, MIN ( A, B ) )
  VALUE = MIN ( VALUE, MAX ( A, B ) )

  I4_UNIFORM = VALUE

  RETURN
END

!*************************************************************

FUNCTION IC_TO_IBRAILLE ( IC )

!*****************************************************************************80
!
!! IC_TO_IBRAILLE converts an ASCII integer code to a Braille code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IC, the integer code for the ASCII character.
!
!    Output, integer ( kind = 4 ) IC_TO_IBRAILLE, the integer code for
!    the Braille character, or -1 if no corresponding code is available.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IC
  INTEGER ( KIND = 4 ) IC_TO_IBRAILLE
  INTEGER ( KIND = 4 ), PARAMETER, DIMENSION ( 0:255 ) :: JUNK = (/ &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
     1, 33, 35, -1, -1, -1, 28, 36, 34, 34, -1, -1, 29, 37, 32, -1, &
    11, 02, 04, 05, 06, 07, 08, 09, 10, -1, 31, 30, -1, -1, -1, 35, &
    -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, &
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, &
    -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, &
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /)

  IF ( 0 <= IC .AND. IC <= 255 ) THEN
    IC_TO_IBRAILLE = JUNK(IC)
  ELSE
    IC_TO_IBRAILLE = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION IC_TO_IEBCDIC ( IC )

!*****************************************************************************80
!
!! IC_TO_IEBCDIC converts an ASCII character code to an EBCDIC code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IC, the integer code for the ASCII character.
!
!    Output, integer ( kind = 4 ) IC_TO_IEBCDIC, the integer code for the
!    EBCDIC character, or -1 if no corresponding EBCDIC code is available.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IC
  INTEGER ( KIND = 4 ) IC_TO_IEBCDIC
  INTEGER ( KIND = 4 ), PARAMETER, DIMENSION ( 0:255 ) :: JUNK = (/ &
     0,  1,  2,  3, 56, 45, 46, 47, 22,  5, 37, 11, 12, 13, 60, 61, &
    16, 17, 18, -1, -1, -1, 50, 38, 24, 25, 63, 39, 28, 29, 30, 31, &
    64, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96, -1, 97, &
   240,241,242,243,244,245,246,247,248,249,122, 94, 76,126,110,111, &
   124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214, &
   215,216,217,226,227,228,229,230,231,232,233, -1, -1, -1, -1,109, &
    -1,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150, &
   151,152,153,162,163,164,165,166,167,168,169, -1, 79, -1, -1,  7, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /)

  IF ( 0 <= IC .AND. IC <= 255 ) THEN
    IC_TO_IEBCDIC = JUNK(IC)
  ELSE
    IC_TO_IEBCDIC = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION IC_TO_IMORSE ( IC )

!*****************************************************************************80
!
!! IC_TO_IMORSE converts an ASCII integer code to a Morse integer code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IC, the integer code for the ASCII character.
!
!    Output, integer ( kind = 4 ) IC_TO_IMORSE, the integer code for the
!    Morse character, or -1 if no corresponding Morse code is available.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IC
  INTEGER ( KIND = 4 ) IC_TO_IMORSE
  INTEGER ( KIND = 4 ), PARAMETER, DIMENSION ( 0:255 ) :: JUNK = (/ &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
     1, -1, 45, -1, -1, -1, -1, 42, -1, -1, -1, -1, 39, 43, 38, 44, &
    37, 28, 29, 30, 31, 32, 33, 34, 35, 36, 40, -1, -1, -1, -1, 41, &
    -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, &
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, &
    -1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, &
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /)

  IF ( 0 <= IC .AND. IC <= 255 ) THEN
    IC_TO_IMORSE = JUNK(IC)
  ELSE
    IC_TO_IMORSE = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION IC_TO_ISOUNDEX ( IC )

!*****************************************************************************80
!
!! IC_TO_ISOUNDEX converts an ASCII integer code to a Soundex integer code.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IC, the integer code for the ASCII character.
!
!    Output, integer ( kind = 4 ) IC_TO_ISOUNDEX, the integer code for the
!    Soundex character, or -1 if no corresponding Soundex code is available.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IC
  INTEGER ( KIND = 4 ) IC_TO_ISOUNDEX
  INTEGER ( KIND = 4 ), PARAMETER, DIMENSION ( 0:255 ) :: JUNK = (/ &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, 48, 49, 50, 51, 48, 49, 50, 48, 48, 50, 50, 52, 53, 53, 48, &
    49, 50, 54, 50, 51, 48, 49, 48, 50, 48, 50, -1, -1, -1, -1, -1, &
    -1, 48, 49, 50, 51, 48, 49, 50, 48, 48, 50, 50, 52, 53, 53, 48, &
    49, 50, 54, 50, 51, 48, 49, 48, 50, 48, 50, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 /)

  IF ( 0 <= IC .AND. IC <= 255 ) THEN
    IC_TO_ISOUNDEX = JUNK(IC)
  ELSE
    IC_TO_ISOUNDEX = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION IEBCDIC_TO_IC ( IEBCDIC )

!*****************************************************************************80
!
!! IEBCDIC_TO_IC converts an EBCDIC character code to ASCII.
!
!  Discussion:
!
!    What this actually means is the following:
!
!    If the letter "A" is entered into a file on an EBCDIC machine,
!    it is coded internally as character 193.  Should this character
!    be read on an ASCII machine, it will not be displayed as "A",
!    but rather as an unprintable character!  But passing 193 in to
!    IEBCDIC_TO_IC, out will come 65, the ASCII code for "A".  Thus, the
!    correct character to display on an ASCII machine is
!
!      ACHAR ( IACHAR ( IEBCDIC_TO_IC ( EBCDIC Character ) ) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IEBCDIC, the integer code for the EBCDIC
!    character.
!
!    Output, integer ( kind = 4 ) IEBCDIC_TO_IC, the integer code for the
!    ASCII character, or -1 if no corresponding ASCII code is available.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IEBCDIC
  INTEGER ( KIND = 4 ) IEBCDIC_TO_IC
  INTEGER ( KIND = 4 ), PARAMETER, DIMENSION ( 0:255 ) :: JUNK = (/ &
     0,  1,  2,  3, -1,  9, -1,127, -1, -1, -1, 11, 12, 13, 14, 15, &
    16, 17, 18, -1, -1, -1,  8, -1, 24, 25, -1, -1, 28, 29, 30, 31, &
    -1, -1, -1, -1, -1, 10, 23, 27, -1, -1, -1, -1, -1,  5,  6,  7, &
    -1, -1, 22, -1, -1, -1, -1, -1,  4, -1, -1, -1, 14, 15, -1, 26, &
    32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 60, 40, 43,124, &
    38, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, 36, 42, 41, 59, -1, &
    45, 47, -1, -1, -1, -1, -1, -1, -1, -1, -1, 44, 37, 95, 62, 63, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 58, 35, 64, 39, 61, 34, &
    -1, 97, 98, 99,100,101,102,103,104,105, -1, -1, -1, -1, -1, -1, &
    -1,106,107,108,109,110,111,112,113,114, -1, -1, -1, -1, -1, -1, &
    -1, -1,115,116,117,118,119,120,121,122, -1, -1, -1, -1, -1, -1, &
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, &
    -1, 65, 66, 67, 68, 69, 70, 71, 72, 73, -1, -1, -1, -1, -1, -1, &
    -1, 74, 75, 76, 77, 78, 79, 80, 81, 82, -1, -1, -1, -1, -1, -1, &
    -1, -1, 83, 84, 85, 86, 87, 88, 89, 90, -1, -1, -1, -1, -1, -1, &
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, -1, -1, -1, -1, -1, -1 /)

  IF ( 0 <= IEBCDIC .AND. IEBCDIC <= 255 ) THEN
    IEBCDIC_TO_IC = JUNK(IEBCDIC)
  ELSE
    IEBCDIC_TO_IC = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION ISBN_TO_I4 ( C )

!*****************************************************************************80
!
!! ISBN_TO_I4 converts an ISBN character into an integer.
!
!  Discussion:
!
!    The characters '0' through '9' stand for themselves, but
!    the character 'X' or 'x' stands for 10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Book Industry Study Group,
!    The Evolution in Product Identification:
!    Sunrise 2005 and the ISBN-13,
!    http://www.bisg.org/docs/The_Evolution_in_Product_ID.pdf
!
!  Parameters:
!
!    Input, character C, the ISBN character code to be converted.
!
!    Output, integer ( kind = 4 ) ISBN_TO_I4, the numeric value of the character
!    code, between 0 and 10.  This value is returned as -1 if C is
!    not a valid character code.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) ISBN_TO_I4

       IF ( C == '0' ) THEN
    ISBN_TO_I4 = 0
  ELSE IF ( C == '1' ) THEN
    ISBN_TO_I4 = 1
  ELSE IF ( C == '2' ) THEN
    ISBN_TO_I4 = 2
  ELSE IF ( C == '3' ) THEN
    ISBN_TO_I4 = 3
  ELSE IF ( C == '4' ) THEN
    ISBN_TO_I4 = 4
  ELSE IF ( C == '5' ) THEN
    ISBN_TO_I4 = 5
  ELSE IF ( C == '6' ) THEN
    ISBN_TO_I4 = 6
  ELSE IF ( C == '7' ) THEN
    ISBN_TO_I4 = 7
  ELSE IF ( C == '8' ) THEN
    ISBN_TO_I4 = 8
  ELSE IF ( C == '9' ) THEN
    ISBN_TO_I4 = 9
  ELSE IF ( C == 'X' .OR. C == 'x' ) THEN
    ISBN_TO_I4 = 10
  ELSE
    ISBN_TO_I4 = -1
  END IF

  RETURN
END

!*************************************************************

FUNCTION ISTRCMP ( S1, S2 )

!*****************************************************************************80
!
!! ISTRCMP compares two strings, returning +1, 0, or -1.
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
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Output, integer ( kind = 4 ) ISTRCMP:
!    -1 if S1 < S2,
!     0 if S1 = S2
!    +1 if S2 < S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) ISTRCMP
  INTEGER ( KIND = 4 ) NCHAR1
  INTEGER ( KIND = 4 ) NCHAR2
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2

  NCHAR1 = LEN ( S1 )
  NCHAR2 = LEN ( S2 )
  S_LENGTH = MIN ( NCHAR1, NCHAR2 )

  IF ( LLT ( S1(1:S_LENGTH), S2(1:S_LENGTH) ) ) THEN
    ISTRCMP = -1
  ELSE IF ( LLT ( S2(1:S_LENGTH), S1(1:S_LENGTH) ) ) THEN
    ISTRCMP = 1
  ELSE IF ( S1(1:S_LENGTH) == S2(1:S_LENGTH) ) THEN

    IF ( NCHAR1 == NCHAR2 ) THEN
      ISTRCMP = 0
    ELSE IF ( NCHAR1 < NCHAR2 ) THEN
      ISTRCMP = -1
    ELSE
      ISTRCMP = 1
    END IF

  END IF

  RETURN
END

!*************************************************************

FUNCTION ISTRNCMP ( S1, S2, NCHAR )

!*****************************************************************************80
!
!! ISTRNCMP compares the start of two strings, returning +1, 0, or -1.
!
!  Discussion:
!
!    If either string is shorter than NCHAR characters, then it is
!    treated as though it were padded with blanks.
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
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Input, integer ( kind = 4 ) NCHAR, the number of characters to be compared.
!
!    Output, integer ( kind = 4 ) ISTRNCMP:
!    +1 if S1(1:NCHAR) is lexically greater than S2(1:NCHAR),
!     0 if they are equal, and
!    -1 if S1(1:NCHAR) is lexically less than S2(1:NCHAR).
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISTRNCMP
  INTEGER ( KIND = 4 ) NCHAR
  INTEGER ( KIND = 4 ) NCHAR1
  INTEGER ( KIND = 4 ) NCHAR2
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
!
!  Figure out the maximum number of characters we will examine,
!  which is the minimum of NCHAR and the lengths of the two
!  strings.
!
  ISTRNCMP = 0

  NCHAR1 = LEN ( S1 )
  NCHAR2 = LEN ( S2 )

  DO I = 1, NCHAR

    IF ( I <= NCHAR1 ) THEN
      C1 = S1(I:I)
    ELSE
      C1 = ' '
    END IF

    IF ( I <= NCHAR2 ) THEN
      C2 = S2(I:I)
    ELSE
      C2 = ' '
    END IF

    IF ( LLT ( C1, C2 ) ) THEN
      ISTRNCMP = -1
      RETURN
    ELSE IF ( LGT ( C1, C2 ) ) THEN
      ISTRNCMP = 1
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION LEN_NONNULL ( S )

!*****************************************************************************80
!
!! LEN_NONNULL returns the length of a string up to the last non-null character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to measure.
!
!    Output, integer ( kind = 4 ) LEN_NONNULL, the length of the string,
!    up to the last non-null character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LEN_NONNULL
  INTEGER ( KIND = 4 ) LEN_S
  CHARACTER, PARAMETER :: NULL = ACHAR ( 0 )
  CHARACTER ( LEN = * ) S

  LEN_S = LEN ( S )

  DO I = LEN_S, 1, -1
    IF ( S(I:I) /= NULL ) THEN
      LEN_NONNULL = I
      RETURN
    END IF
  END DO

  LEN_NONNULL = 0

  RETURN
END

!*************************************************************

FUNCTION MALPHNUM2 ( S )

!*****************************************************************************80
!
!! MALPHNUM2 is TRUE if a string contains only alphanumerics and underscores.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    Alphanumeric characters are 'A' through 'Z', 'a' through 'z',
!    '0' through '9' and the underscore character.
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
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical MALPHNUM2, is TRUE if the string contains only
!    alphabetic characters, numerals, and underscores.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ITEMP
  LOGICAL MALPHNUM2
  CHARACTER ( LEN = * ) S

  MALPHNUM2 = .FALSE.

  DO I = 1, LEN ( S )

    IF ( S(I:I) /= '_' ) THEN

      ITEMP = IACHAR ( S(I:I) )

      IF ( .NOT. ( 65 <= ITEMP .AND. ITEMP <= 90 ) ) THEN
        IF ( .NOT. ( 97 <= ITEMP .AND. ITEMP <= 122 ) ) THEN
          IF ( .NOT. ( 48 <= ITEMP .AND. ITEMP <= 57 ) ) THEN
            RETURN
          END IF
        END IF
      END IF
    END IF

  END DO

  MALPHNUM2 = .TRUE.

  RETURN
END

!*************************************************************

SUBROUTINE MILITARY_TO_CH ( MILITARY, CH )

!*****************************************************************************80
!
!! MILITARY_TO_CH converts a Military code word to an ASCII character.
!
!  Example:
!
!    'Alpha'   'A'
!    'Bravo'   'B'
!    'Zulu'    'Z'
!    'alpha'   'a'
!    '7'       '7'
!    '%'       '%'
!    'Adam'    'A'
!    'Anthrax' 'A'
!    'amoeba'  'a'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 8 ) MILITARY, the military code word.
!
!    Output, character CH, the ASCII character.  If MILITARY was not
!    a recognized military code word, then CH is set to MILITARY(1:1).
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER ( LEN = 8 ), DIMENSION ( 26 ) :: CODE = (/ &
    'alpha   ', 'bravo   ', 'charlie ', 'delta   ', 'echo    ', &
    'foxtrot ', 'golf    ', 'hotel   ', 'india   ', 'juliet  ', &
    'kilo    ', 'lima    ', 'mike    ', 'november', 'oscar   ', &
    'papa    ', 'quebec  ', 'romeo   ', 'sierra  ', 'tango   ', &
    'uniform ', 'victor  ', 'whiskey ', 'x-ray   ', 'yankee  ', &
    'zulu    ' /)
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) MILITARY

  CH = MILITARY(1:1)

  I = A_TO_I4 ( CH )

  IF ( 1 <= I .AND. I <= 26 ) THEN
    IF ( S_EQI ( MILITARY, CODE(I) ) ) THEN
      CH = MILITARY(1:1)
    END IF
  ELSE IF ( 27 <= I .AND. I <= 52 ) THEN
    IF ( S_EQI ( MILITARY, CODE(I-26) ) ) THEN
      CH = MILITARY(1:1)
    END IF
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE MONTH_NAME_TO_I4 ( MONTH_NAME, MONTH )

!*****************************************************************************80
!
!! MONTH_NAME_TO_I4 returns the month number of a given month
!
!  Discussion:
!
!    Capitalization is ignored.  The month name has to match up to
!    the unique beginning of a month name, and the rest is ignored.
!    Here are the limits:
!
!      JAnuary
!      February
!      MARch
!      APril
!      MAY
!      JUNe
!      JULy
!      AUgust
!      September
!      October
!      November
!      December
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) MONTH_NAME, a string containing a month
!    name or abbreviation.
!
!    Output, integer ( kind = 4 ) MONTH, the number of the month,
!    or -1 if the name could not be recognized.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) MONTH
  CHARACTER ( LEN = * ) MONTH_NAME
  CHARACTER ( LEN = 3 ) STRING

  STRING = MONTH_NAME
  CALL S_CAP ( STRING )

       IF ( STRING(1:2) == 'JA' ) THEN
    MONTH = 1
  ELSE IF ( STRING(1:1) == 'F' ) THEN
    MONTH = 2
  ELSE IF ( STRING(1:3) == 'MAR' ) THEN
    MONTH = 3
  ELSE IF ( STRING(1:2) == 'AP' ) THEN
    MONTH = 4
  ELSE IF ( STRING(1:3) == 'MAY' ) THEN
    MONTH = 5
  ELSE IF ( STRING(1:3) == 'JUN' ) THEN
    MONTH = 6
  ELSE IF ( STRING(1:3) == 'JUL' ) THEN
    MONTH = 7
  ELSE IF ( STRING(1:2) == 'AU' ) THEN
    MONTH = 8
  ELSE IF ( STRING(1:1) == 'S' ) THEN
    MONTH = 9
  ELSE IF ( STRING(1:1) == 'O' ) THEN
    MONTH = 10
  ELSE IF ( STRING(1:1) == 'N' ) THEN
    MONTH = 11
  ELSE IF ( STRING(1:1) == 'D' ) THEN
    MONTH = 12
  ELSE
    MONTH = -1
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE NAMEFL ( S )

!*****************************************************************************80
!
!! NAMEFL replaces "lastname, firstname" by "firstname lastname".
!
!  Discussion:
!
!    As part of the process, all commas and double blanks are
!    removed, and the first character of the output string is
!    never a blank.
!
!    A one-word name is left unchanged.
!
!  Example:
!
!      Input                      Output
!
!    Brown, Charlie             Charlie Brown
!    Cher                       Cher
!    Howell, James Thurston     James Thurston Howell
!    Shakespeare Joe Bob        Joe Bob Shakespeare
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
!    Input/output, character ( len = * ) S.
!
!    On input, a series of words separated by spaces.
!
!    On output, if S contained a single word, it is
!    unchanged.  Otherwise, the first word has been moved
!    to the end of S, and any trailing comma removed.
!
!    As part of this process, all double blanks are removed
!    from S, and the output S never begins with
!    a blank (unless the input S was entirely blank).
!
!    Any commas in the input string are deleted.
!
!    This routine cannot handle more than 255 characters in S.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 255 ) S2
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S2 = ' '
!
!  Remove all commas.
!
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( S(I:I) == ',') THEN
      S(I:I) = ' '
    END IF
  END DO
!
!  Remove double blanks.
!  This also guarantees the string is flush left.
!
  CALL S_BLANKS_DELETE ( S )
!
!  Get length of string.
!
  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 2 ) THEN
    RETURN
  END IF

  IF ( 255 < S_LENGTH ) THEN
    S_LENGTH = LEN_TRIM ( S(1:255) )
  END IF
!
!  Find the first blank in the string.
!
  DO I = 2, S_LENGTH - 1

    IF ( S(I:I) == ' ' ) THEN
      S2(1:S_LENGTH-I) = S(I+1:S_LENGTH)
      S2(S_LENGTH-I+1:S_LENGTH-I+1) = ' '
      S2(S_LENGTH-I+2:S_LENGTH) = S(1:I-1)
      S = S2(1:S_LENGTH)
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE NAMELF ( S )

!*****************************************************************************80
!
!! NAMELF replaces "firstname lastname" by "lastname, firstname".
!
!  Discussion:
!
!    A one-word name is left unchanged.
!
!  Example:
!
!      Input:                     Output:
!
!    Charlie Brown              Brown, Charlie
!    Cher                       Cher
!    James Thurston Howell      Howell, James Thurston
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
!    Input/output, character ( len = * ) S.
!
!    On input, S contains a series of words separated by spaces.
!
!    On output, if S contained a single word, it is
!    unchanged.  Otherwise, the last word has been moved
!    to the beginning of S, and followed by a comma.
!
!    As part of this process, all double blanks are removed
!    from S, and the output S never begins with
!    a blank (unless the input S was entirely blank).
!
!    Moreover, any commas in the input string are deleted.
!
!    This routine cannot handle more than 255 characters
!    in S.  If S is longer than that, only the
!    first 255 characters will be considered.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 255 ) S2
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S2 = ' '
!
!  Remove all commas.
!
  S_LENGTH = LEN_TRIM ( S )
  DO I = 1, S_LENGTH
    IF ( S(I:I) == ',' ) THEN
      S(I:I) = ' '
    END IF
  END DO
!
!  Remove all double blanks, and make string flush left.
!
  CALL S_BLANKS_DELETE ( S )
!
!  Get length of string.
!
  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 2 ) THEN
    RETURN
  END IF

  IF ( 255 < S_LENGTH ) THEN
    S_LENGTH = LEN_TRIM ( S(1:255) )
  END IF
!
!  Find the last blank in the string.
!
  DO I = S_LENGTH, 2, -1
    IF ( S(I:I) == ' ' ) THEN
      S2(1:S_LENGTH-I) = S(I+1:S_LENGTH)
      S2(S_LENGTH-I+1:S_LENGTH-I+2) = ', '
      S2(S_LENGTH-I+3:S_LENGTH+1) = S(1:I-1)
      S = S2(1:S_LENGTH+1)
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE NAMELS ( NAME, IERROR, RHS, VALUE )

!*****************************************************************************80
!
!! NAMELS reads a NAMELIST line, returning the variable name and value.
!
!  Discussion:
!
!    NAMELS is a simple program, and can only handle simple input.
!    In particular, it cannot handle:
!
!      multiple assignments on one line,
!      a single assignment extended over multiple lines,
!      assignments to character or complex variables,
!      assignments to arrays.
!
!    Typical input would be of the form:
!
!      name = value
!
!    including, for instance:
!
!      a = 1.0
!      n = -17
!      scale = +5.3E-2
!
!    Spaces are ignored, and case is not important.  Integer values
!    will be returned as real, but this is never a
!    problem as long as the integers are "small".
!
!    If a line begins with the character "#", it is assumed to be
!    a comment, and is ignored.  IERROR is returned as 6.
!
!    If a line begins with the characters "end-of-input", it is
!    assumed to be an "end-of-input" marker, and IERROR is returned
!    as 7.
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
!    Output, character ( len = * ) NAME.
!    NAME contains the left hand side of the assignment statement.
!    Normally, this will be the name of a variable.
!    If the input line was blank, then NAME will equal ' '.
!    If an error occurred while trying to process the
!    input line, NAME will contain the text of the line.
!    If the line began with "#", then NAME will contain the
!    text of the line.
!    If the line equals "end-of-input", then NAME will contain
!    the text of the line.
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no errors were detected.
!    1, the line was blank.
!    2, the line did not contain an "=" sign.
!    3, the line did not contain a variable name to the
!       left of the "=" sign.
!    4, the right hand side of the assignment did not make
!       sense.
!    5, end of input.
!    6, the line began with "#", signifying a comment.
!       The text of the line is returned in NAME.
!    7, the line began with "end-of-input".
!
!    Output, character ( len = * ) RHS.
!    RHS contains the right hand side of the assignment statement.
!
!    Output, real ( kind = 4 ) VALUE.
!    VALUE contains the right hand side of the assignment statement.
!    Normally, this will be a real value.
!    But if the input line was blank, or if an error occurred
!    while trying to process the input line, or if input
!    terminated, then VALUE will simply be set to 0.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IEQUAL
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) NAME
  INTEGER ( KIND = 4 ) POS
  CHARACTER ( LEN = * ) RHS
  REAL ( KIND = 4 ) VALUE
!
!  Set default values
!
  IERROR = 0
  NAME = ' '
  RHS = ' '
  VALUE = 0
!
!  Read a line
!
  READ ( *, '(a)', IOSTAT = IOS ) LINE

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'NAMELS - Reached end of input.'
    IERROR = 5
    RETURN
  END IF
!
!  Empty lines are OK
!
  IF ( LEN_TRIM ( LINE ) <= 0 ) THEN
    IERROR = 1
    RETURN
  END IF
!
!  Check for comment.
!
  IF ( LINE(1:1) == '#' ) THEN
    IERROR = 6
    NAME = LINE
    RETURN
  END IF
!
!  Check for "end-of-line".
!
  IF ( S_EQI ( LINE, 'END-OF-INPUT' ) ) THEN
    IERROR = 7
    NAME = LINE
    RETURN
  END IF
!
!  Does the line contain an = sign?
!
  IF ( INDEX ( LINE, '=' ) <= 0 ) THEN
    IERROR = 2
    VALUE = 0
    NAME = LINE
    RETURN
  END IF
!
!  Find the name of the variable to be assigned.
!
  IEQUAL = INDEX ( NAME, '=' )

  IF ( 0 < IEQUAL ) THEN
    RHS = LINE(IEQUAL+1:)
  ELSE
    RHS = LINE
  END IF

  CALL S_BEFORE_SS_COPY ( LINE, '=', NAME )
  CALL S_BLANK_DELETE ( NAME )

  IF ( LEN_TRIM ( NAME ) <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'NAMELS - Warning!'
    WRITE ( *, '(a)' ) '  The following input line was ignored, because'
    WRITE ( *, '(a)' ) '  there was no variable name on the left hand'
    WRITE ( *, '(a)' ) '  side of the assignment statement:'
    WRITE ( *, '(a)' ) LINE
    WRITE ( *, '(a)' ) ' '
    IERROR = 3
    RETURN
  END IF
!
!  Read the value, as a real number.
!
  POS = INDEX ( LINE, '=' )
  CALL S_TO_R4 ( LINE(POS+1:), VALUE, IERROR, LENGTH )

  IF ( IERROR /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'NAMELS - Warning!'
    WRITE ( *, '(a)' ) '  The following input line was ignored, because'
    WRITE ( *, '(a)' ) '  the right hand side of the assignment '
    WRITE ( *, '(a)' ) '  statement did not seem to make sense:'
    WRITE ( *, '(a)' ) LINE
    WRITE ( *, '(a)' ) ' '
    IERROR = 4
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE NEXCHR ( S, I, C )

!*****************************************************************************80
!
!! NEXCHR returns the next nonblank character from a string.
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) I.  If I is 0, then there were no
!    nonblank characters in the string.  Otherwise I is
!    the index of the first nonblank character in the string.
!
!    Output, character C, the first nonblank character in the string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S

  I = S_FIRST_NONBLANK ( S )

  IF ( 0 < I ) THEN
    C = S(I:I)
  ELSE
    C = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE NEXSTR ( S, NSUB, ISUB, SUB )

!*****************************************************************************80
!
!! NEXSTR returns the next nonblank characters from a string.
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Input, integer ( kind = 4 ) NSUB, the number of nonblank characters
!    desired.
!
!    Output, integer ( kind = 4 ) ISUB, the index of the NSUB-th nonblank
!    character.  However, if ISUB is 0, there were NO nonblank
!    characters.  And if there are less than NSUB nonblank characters
!    ISUB is the location of the last one of them.
!
!    Output, character ( len = NSUB ) SUB, the first NSUB nonblanks.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) NSUB

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISUB
  INTEGER ( KIND = 4 ) JSUB
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = NSUB ) SUB

  SUB = ' '
  ISUB = 0

  DO I = 1, NSUB

    JSUB = S_FIRST_NONBLANK ( S(ISUB+1:) )

    IF ( JSUB <= 0 ) THEN
      RETURN
    END IF

    ISUB = ISUB + JSUB

    SUB(I:I) = S(ISUB:ISUB)

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE NUMBER_INC ( S )

!*****************************************************************************80
!
!! NUMBER_INC increments the integer represented by a string.
!
!  Example:
!
!    Input      Output
!    -----      ------
!    '17'       '18'
!    'cat3'     'cat4'
!    '2for9'    '3for0'
!    '99thump'  '00thump'
!
!  Discussion:
!
!    If the string contains characters that are not digits, they will
!    simply be ignored.  If the integer is all 9's on input, then
!    the output will be all 0's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a string representing an integer.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S

  DO I = LEN ( S ), 1, -1

    IF ( CH_IS_DIGIT ( S(I:I) ) ) THEN

      CALL DIGIT_INC ( S(I:I) )

      IF ( S(I:I) /= '0' ) THEN
        RETURN
      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE OCT_TO_I4 ( S, INTVAL )

!*****************************************************************************80
!
!! OCT_TO_I4 converts an octal string to its integer value.
!
!  Warning:
!
!    If too many digits are strung together, the computation will overflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string of digits.
!
!    Output, integer ( kind = 4 ) INTVAL, the corresponding integer value.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) ISGN
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )
!
!  Determine if there is a plus or minus sign.
!
  ISGN = 1
  FIRST = S_LENGTH

  DO I = 1, S_LENGTH - 1

    IF ( S(I:I) == '-' ) THEN
      ISGN = -1
    ELSE IF ( S(I:I) == '+' ) THEN
      ISGN = + 1
    ELSE IF ( S(I:I) /= ' ' ) THEN
      FIRST = I
      EXIT
    END IF

  END DO
!
!  Read the numeric portion of the string.
!
  INTVAL = 0

  DO I = FIRST, S_LENGTH
    CALL CH_TO_DIGIT_OCT ( S(I:I), IDIG )
    INTVAL = INTVAL * 8 + IDIG
  END DO

  INTVAL = ISGN * INTVAL

  RETURN
END

!*************************************************************

SUBROUTINE PERM_CHECK ( N, P, IERROR )

!*****************************************************************************80
!
!! PERM_CHECK checks that a vector represents a permutation.
!
!  Discussion:
!
!    The routine verifies that each of the integers from 1
!    to N occurs among the N entries of the permutation.
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
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Input, integer ( kind = 4 ) P(N), the permutation, in standard index form.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, the array does represent a permutation.
!    nonzero, the array does not represent a permutation.  The smallest
!    missing value is equal to IERROR.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IFIND
  INTEGER ( KIND = 4 ) ISEEK
  INTEGER ( KIND = 4 ) P(N)

  IERROR = 0

  DO ISEEK = 1, N

    IERROR = ISEEK

    DO IFIND = 1, N
      IF ( P(IFIND) == ISEEK ) THEN
        IERROR = 0
        EXIT
      END IF
    END DO

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE PERM_INVERSE3 ( N, PERM, PERM_INV )

!*****************************************************************************80
!
!! PERM_INVERSE3 produces the inverse of a given permutation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items permuted.
!
!    Input, integer ( kind = 4 ) PERM(N), a permutation.
!
!    Output, integer ( kind = 4 ) PERM_INV(N), the inverse permutation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) PERM(N)
  INTEGER ( KIND = 4 ) PERM_INV(N)

  DO I = 1, N
    PERM_INV(PERM(I)) = I
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE PERM_UNIFORM ( N, SEED, P )

!*****************************************************************************80
!
!! PERM_UNIFORM selects a random permutation of N objects.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) P(N), the permutation.  P(I) is the "new"
!    location of the object originally at I.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) P(N)
  INTEGER ( KIND = 4 ) SEED

  CALL I4VEC_INDICATOR ( N, P )

  DO I = 1, N
    J = I4_UNIFORM ( I, N, SEED )
    CALL I4_SWAP ( P(I), P(J) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_B4_IEEE ( R, WORD )

!*****************************************************************************80
!
!! R4_TO_B4_IEEE converts an R4 to a 4 byte IEEE word.
!
!  Discussion:
!
!    This routine does not seem to working reliably for unnormalized data.
!
!  Example:
!
!    0 00000000 00000000000000000000000 =  0
!    1 00000000 00000000000000000000000 = -0
!
!    0 11111111 00000000000000000000000 =  Infinity
!    1 11111111 00000000000000000000000 = -Infinity
!
!    0 11111111 00000100000000000000000 = NaN
!    1 11111111 00100010001001010101010 = NaN
!
!    0 01111110 00000000000000000000000 = +1 * 2**(126-127) * 1.0   = 0.5
!    0 01111111 00000000000000000000000 = +1 * 2**(127-127) * 1.0   = 1
!    0 10000000 00000000000000000000000 = +1 * 2**(128-127) * 1.0   = 2
!    0 10000001 00000000000000000000000 = +1 * 2**(129-127) * 1.0   = 4
!
!    0 10000001 10100000000000000000000 = +1 * 2**(129-127) * 1.101 =  6.5
!    1 10000001 10100000000000000000000 = -1 * 2**(129-127) * 1.101 = -6.5
!
!    0 00000001 00000000000000000000000 = +1 * 2**(  1-127) * 1.0 = 2**(-126)
!    0 00000000 10000000000000000000000 = +1 * 2**(  0-126) * 0.1 = 2**(-127)
!    0 00000000 00000000000000000000001 = +1 * 2**(  0-126) *
!                                          0.00000000000000000000001 =
!                                          2**(-149)  (Smallest positive value)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    IEEE Standards Committee 754,
!    IEEE Standard for Binary Floating Point Arithmetic,
!    ANSI/IEEE Standard 754-1985,
!    SIGPLAN Notices,
!    Volume 22, Number 2, 1987, pages 9-25.
!
!  Parameters:
!
!    Input, real ( kind = 4 ) R, the real number to be converted.
!
!    Output, integer ( kind = 4 ) WORD, the IEEE representation of the number.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) F
  REAL ( KIND = 4 ) R
  REAL ( KIND = 4 ) R_COPY
  INTEGER ( KIND = 4 ) S
  INTEGER ( KIND = 4 ) WORD

  R_COPY = R
!
!  Determine S, the sign bit.
!
  IF ( 0.0E+00 <= R_COPY ) THEN
    S = 0
  ELSE
    S = 1
    R_COPY = -R_COPY
  END IF
!
!  Determine E, the exponent.
!  (FOR NOW, IGNORE UNNORMALIZED NUMBERS)
!
  E = 0

  IF ( R == 0.0E+00 ) THEN

  ELSE

    DO WHILE ( 2.0E+00 <= R_COPY )
      E = E + 1
      R_COPY = R_COPY / 2.0E+00
    END DO

    DO WHILE ( R_COPY < 1.0E+00 .AND. -127 < E )
      E = E - 1
      R_COPY = R_COPY * 2.0E+00
    END DO

    E = E + 127

  END IF
!
!  Determine F, the fraction.
!
  IF ( R == 0.0E+00 ) THEN

    F = 0

  ELSE IF ( 0 < E) THEN

    R_COPY = R_COPY - 1.0E+00
    F = INT ( R_COPY * 2.0E+00**23 )

  ELSE IF ( E == 0 ) THEN

    F = INT ( R_COPY * 2.0E+00**23 )

  END IF
!
!  Set the bits corresponding to S, E, F.
!
  CALL MVBITS ( S, 0,  1, WORD, 31 )
  CALL MVBITS ( E, 0,  8, WORD, 23 )
  CALL MVBITS ( F, 0, 23, WORD,  0 )

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_BINARY ( R, S )

!*****************************************************************************80
!
!! R4_TO_BINARY represents an R4 as a string of binary digits.
!
!  Discussion:
!
!    No check is made to ensure that the string is long enough.
!
!    The binary digits are a faithful representation of the real
!    number in base 2.
!
!  Example:
!
!      R           S
!
!    -10.75000    -1010.11
!      0.4218750  0.011011
!      0.3333333  0.01010101010101010101010
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
!    Input, real ( kind = 4 ) R, the real number.
!
!    Output, character ( len = * ) S, the binary representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) ILO
  REAL ( KIND = 4 ) R
  REAL ( KIND = 4 ) RCOPY
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH < 1 ) THEN
    RETURN
  END IF

  RCOPY = R
  S = ' '

  IF ( RCOPY == 0.0E+00 ) THEN
    S = '0'
    RETURN
  END IF

  ILO = 0

  IF ( RCOPY < 0.0E+00 ) THEN
    ILO = 1
    S(ILO:ILO) = '-'
    RCOPY = -RCOPY
  END IF
!
!  Figure out the divisor.
!
  IEXP = 0

  DO WHILE ( 1.0E+00 <= RCOPY )
    RCOPY = RCOPY / 2.0E+00
    IEXP = IEXP + 1
  END DO

  DO WHILE ( RCOPY < 0.5E+00 )
    RCOPY = RCOPY * 2.0E+00
    IEXP = IEXP - 1
  END DO
!
!  Now 0.5 <= RCOPY < 1.
!
!  If IEXP < 0, print leading zeroes.
!
  IF ( IEXP == 0 ) THEN
    ILO = ILO + 1
    S(ILO:ILO) = '0'
  ELSE IF ( IEXP < 0 ) THEN
    ILO = ILO + 1
    S(ILO:ILO) = '0'
    ILO = ILO + 1
    S(ILO:ILO) = '.'
    DO I = 1, -IEXP
      ILO = ILO + 1
      S(ILO:ILO) = '0'
    END DO

  END IF
!
!  Now repeatedly double RCOPY.
!  Every time you exceed 1, that's a '1' digit.
!
  IEXP = IEXP + 1

  DO

    RCOPY = 2.0E+00 * RCOPY

    IEXP = IEXP - 1

    IF ( IEXP == 0 ) THEN
      ILO = ILO + 1
      S(ILO:ILO) = '.'
      IF ( S_LENGTH <= ILO ) THEN
        RETURN
      END IF
    END IF

    ILO = ILO + 1

    IF ( 1.0E+00 <= RCOPY ) THEN
      RCOPY = RCOPY - 1.0E+00
      S(ILO:ILO) = '1'
    ELSE
      S(ILO:ILO) = '0'
    END IF

    IF ( S_LENGTH <= ILO ) THEN
      RETURN
    END IF

    IF ( RCOPY == 0.0E+00 ) THEN
      EXIT
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_CH4 ( R4, CH4 )

!*****************************************************************************80
!
!! R4_TO_CH4 converts an R4 to a 4 character string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 May 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) R4, the real value.
!
!    Output, character ( len = 4 ) CH4, a corresponding character value.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 4 ) CH4
  INTEGER ( KIND = 4 ) I4
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) J2
  INTEGER ( KIND = 4 ) J3
  INTEGER ( KIND = 4 ) J4
  REAL ( KIND = 4 ) R4

  I4 = TRANSFER ( R4, I4 )

  J1 = IBITS ( I4,  0, 8 )
  J2 = IBITS ( I4,  8, 8 )
  J3 = IBITS ( I4, 16, 8 )
  J4 = IBITS ( I4, 24, 8 )

  CH4 = ACHAR ( J1 ) // ACHAR ( J2 ) // ACHAR ( J3 ) // ACHAR ( J4 )

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_FLT ( R4, ISGN, MANT, IEXP, NDIG )

!*****************************************************************************80
!
!! R4_TO_FLT computes the scientific representation of an R4.
!
!  Discussion:
!
!    The routine is given a real number R and computes a sign ISGN,
!    an integer mantissa MANT and an integer exponent IEXP so
!    that
!
!      R4 = ISGN * MANT * 10 ** IEXP
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
!    Input, real ( kind = 4 ) R4, the real number whose scientific
!    representation is desired.
!
!    Output, integer ( kind = 4 ) ISGN, the sign of the number:
!    -1, if R4 is negative.
!     0, if R4 is zero.
!     1, if R4 is positive.
!
!    Output, integer ( kind = 4 ) MANT, the mantissa of the representation.
!    This is an integer between 0 and 10**NDIG, that is,
!    0 <= MANT < 10**NDIG.
!
!    Output, integer ( kind = 4 ) IEXP, the exponent of 10 that multiplies MULT.
!
!    Input, integer ( kind = 4 ) NDIG, the number of decimal digits.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) MANT
  INTEGER ( KIND = 4 ) NDIG
  REAL ( KIND = 4 ) RMANT
  REAL ( KIND = 4 ) R4

  MANT = 0
  IEXP = 0
  ISGN = 0
!
!  Find the first digit.
!  That is, write the value in the form RMANT * 10**IEXP
!  where 1/10 < RMANT <= 1.
!
  IF ( R4 == 0.0E+00 ) THEN
    RETURN
  ELSE IF ( R4 < 0.0E+00 ) THEN
    ISGN = -1
    RMANT = ABS ( R4 )
  ELSE
    ISGN = 1
    RMANT = R4
  END IF

  DO WHILE ( 1.0E+00 < RMANT )
    RMANT = RMANT / 10.0E+00
    IEXP = IEXP + 1
  END DO

  DO WHILE ( RMANT <= 0.1E+00 )
    RMANT = RMANT * 10.0E+00
    IEXP = IEXP - 1
  END DO
!
!  Now read off NDIG digits of RMANT.
!
  DO I = 1, NDIG
    RMANT = RMANT * 10.0E+00
    IDIG = INT ( RMANT )
    RMANT = RMANT - IDIG
    MANT = 10 * MANT + IDIG
    IEXP = IEXP - 1
  END DO
!
!  Now do rounding.
!
  IDIG = INT ( RMANT * 10.0E+00 )
  MANT = 10 * MANT + IDIG
  MANT =  NINT ( REAL ( MANT, KIND = 4 ) / 10.0E+00 )
!
!  Now chop off trailing zeroes.
!
  DO WHILE ( MOD ( MANT, 10 ) == 0 )
    MANT = MANT / 10
    IEXP = IEXP + 1
  END DO

  RETURN
END

!*************************************************************

FUNCTION R4_TO_S32 ( R4 )

!*****************************************************************************80
!
!! R4_TO_S32 encodes an R4 as 32 characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) R4, the real number to be coded.
!
!    Output, character ( len = 32 ) R4_TO_S32, the character variable that
!    corresponds to the real number.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 32 ) CHR32
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) II
  INTEGER ( KIND = 4 ) J
  REAL ( KIND = 4 ) R4
  CHARACTER ( LEN = 32 ) R4_TO_S32
  REAL ( KIND = 4 ) RCOPY

  RCOPY = R4
!
!  Sign bit
!
  IF ( RCOPY < 0.0E+00 ) THEN
    CHR32(1:1) = '1'
  ELSE
    CHR32(1:1) = '0'
  END IF

  RCOPY = ABS ( RCOPY )
!
!  Exponent: 'excess 128' format, legal values of IEXP are 1 to 255.
!
  IF ( RCOPY == 0.0E+00 ) THEN

    IEXP = 0

  ELSE

    IEXP = 128

    IF ( RCOPY < 1.0E+00 ) THEN

      DO WHILE ( 1 < IEXP )
        RCOPY = 2.0E+00 * RCOPY
        IEXP = IEXP - 1
      END DO

    ELSE IF ( 2.0E+00 <= RCOPY ) THEN

      DO WHILE ( IEXP < 255 )
        RCOPY = 0.5E+00 * RCOPY
        IEXP = IEXP + 1
      END DO

    END IF

  END IF
!
!  Write characters 2 through 9 that represent exponent.
!
  DO I = 1, 8
    II = 10 - I
    J = MOD ( IEXP, 2 )
    IEXP = IEXP / 2
    IF ( J == 0 ) THEN
      CHR32(II:II) = '0'
    ELSE
      CHR32(II:II) = '1'
    END IF
  END DO
!
!  Write mantissa in positions 10 through 32.
!  Note that, unless exponent equals 0, the most significant bit is
!  assumed to be 1 and hence is not stored.
!
  IF ( RCOPY /= 0.0E+00 ) THEN
    RCOPY = RCOPY - 1.0E+00
  END IF

  DO I = 10, 32
    RCOPY = 2.0E+00 * RCOPY
    IF ( 1.0E+00 <= RCOPY ) THEN
      CHR32(I:I) = '1'
      RCOPY = RCOPY - 1.0E+00
    ELSE
      CHR32(I:I) = '0'
    END IF
  END DO

  R4_TO_S32 = CHR32

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_SEF ( R4, S, E, F )

!*****************************************************************************80
!
!! R4_TO_SEF represents an R4 as R = S * 2**E * F.
!
!  Discussion:
!
!    Assuming no arithmetic problems, in fact, this equality should be
!    exact, that is, S, E and F should exactly express the value
!    as stored on the computer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) R4, the real number.
!
!    Output, integer ( kind = 4 ) S, the sign bit:
!    0, if R is nonnegative;
!    1, if R is negative.
!
!    Output, integer ( kind = 4 ) E, the exponent base 2.
!
!    Output, integer ( kind = 4 ) F, the mantissa.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) F
  REAL ( KIND = 4 ) R4
  REAL ( KIND = 4 ) R4_COPY
  INTEGER ( KIND = 4 ) S

  IF ( R4 == 0.0E+00 ) THEN
    S = 0
    E = 0
    F = 0
    RETURN
  END IF

  R4_COPY = R4
!
!  Set S.
!
  IF ( 0.0E+00 <= R4_COPY ) THEN
    S = 0
  ELSE
    S = 1
    R4_COPY = -R4_COPY
  END IF
!
!  Extracting the exponent leaves 0.5 <= R4_COPY < 1.0.
!
  E = 0

  DO WHILE ( R4_COPY < 0.5E+00 )
    R4_COPY = R4_COPY * 2.0E+00
    E = E - 1
  END DO

  DO WHILE ( 1.0E+00 <= R4_COPY )
    R4_COPY = R4_COPY / 2.0E+00
    E = E + 1
  END DO
!
!  Get the binary mantissa, adjusting the exponent as you go.
!
  F = 0
  E = E + 1

  DO

    F = 2 * F
    E = E - 1

    IF ( 1.0E+00 <= R4_COPY ) THEN
      F = F + 1
      R4_COPY = R4_COPY - 1.0E+00
    END IF

    IF ( R4_COPY == 0.0E+00 ) THEN
      EXIT
    END IF

    R4_COPY = 2.0E+00 * R4_COPY

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_S_LEFT ( R4, S )

!*****************************************************************************80
!
!! R4_TO_S_LEFT writes an R4 into a left justified character string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) R4, the real number to be written into the string.
!
!    Output, character ( len = * ) S, the string into which
!    the real number is to be written.
!
  IMPLICIT NONE

  REAL ( KIND = 4 ) R4
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 14 ) S2

  IF ( REAL ( INT ( R4 ), KIND = 4 ) == R4 ) THEN
    WRITE ( S2, '(i14)' ) INT ( R4 )
  ELSE IF ( ABS ( R4 ) < 999999.5E+00 ) THEN
    WRITE ( S2, '(f14.6)' ) R4
  ELSE
    WRITE ( S2, '(g14.6)' ) R4
  END IF

  S = ADJUSTL ( S2 )

  RETURN
END

!*************************************************************

SUBROUTINE R4_TO_S_RIGHT ( R4, S )

!*****************************************************************************80
!
!! R4_TO_S_RIGHT writes an R4 into a right justified character string.
!
!  Discussion:
!
!    Thanks to Bill Richmond for pointing out a programming error
!    that stored the data in S2, and then failed to copy it to the
!    output quantity S.
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
!    Input, real ( kind = 4 ) R4, the real number to be written into the string.
!
!    Output, character ( len = * ) S, the string into which
!    the real number is to be written.
!
  IMPLICIT NONE

  REAL ( KIND = 4 ) R4
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 14 ) S2

  IF ( REAL ( INT ( R4 ), KIND = 4 ) == R4 ) THEN
    WRITE ( S2, '(i14)' ) INT ( R4 )
  ELSE IF ( ABS ( R4 ) < 999999.5E+00 ) THEN
    WRITE ( S2, '(f14.6)' ) R4
  ELSE
    WRITE ( S2, '(g14.6)' ) R4
  END IF

  S = ' '
  S(1:14) = S2(1:14)

  CALL S_ADJUSTR ( S )

  RETURN
END

!*************************************************************

FUNCTION R4_UNIFORM_01 ( SEED )

!*****************************************************************************80
!
!! R4_UNIFORM_01 returns a unit pseudorandom R4.
!
!  Discussion:
!
!    An R4 is a real ( kind = 4 ) value.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2**31 - 1 )
!      r4_uniform_01 = seed / ( 2**31 - 1 )
!
!    The integer arithmetic never requires more than 32 bits,
!    including a sign bit.
!
!    If the initial seed is 12345, then the first three computations are
!
!      Input     Output      R4_UNIFORM_01
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
!    31 May 2007
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
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 4 ) R4_UNIFORM_01, a new pseudorandom variate,
!    strictly between 0 and 1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) K
  INTEGER ( KIND = 4 ) SEED
  REAL ( KIND = 4 ) R4_UNIFORM_01

  IF ( SEED == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'R4_UNIFORM_01 - Fatal error!'
    WRITE ( *, '(a)' ) '  Input value of SEED = 0.'
    RETURN
  END IF

  K = SEED / 127773

  SEED = 16807 * ( SEED - K * 127773 ) - K * 2836

  IF ( SEED < 0 ) THEN
    SEED = SEED + I4_HUGE ( )
  END IF

  R4_UNIFORM_01 = REAL ( SEED, KIND = 4 ) * 4.656612875E-10

  RETURN
END

!*************************************************************

SUBROUTINE R8VEC_TO_S ( N, X, S )

!*****************************************************************************80
!
!! R8VEC_TO_S "writes" an R8VEC into a string.
!
!  Discussion:
!
!    An R8VEC is a vector of "real ( kind = 8 )" values.
!
!    The values will be separated by commas and a single space.
!    If the string is too short, then data will be lost.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the dimension of X.
!
!    Input, real ( kind = 8 ) X(N), a vector to be written to a string.
!
!    Output, character ( len = * ) S, a string to which the real vector
!    has been written.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 14 ) S2
  REAL ( KIND = 8 ) X(N)

  DO I = 1, N

    IF ( X(I) == 0.0D+00 ) THEN
      S2 = '0'
    ELSE IF ( 1.0D+10 <= ABS ( X(I) ) ) THEN
      WRITE ( S2, '(g14.6)' ) X(I)
      CALL S_TRIM_ZEROS ( S2 )
    ELSE IF ( REAL ( INT ( X(I) ), KIND = 8 ) == X(I) ) THEN
      WRITE ( S2, '(i12)' ) INT ( X(I) )
    ELSE
      WRITE ( S2, '(g14.6)' ) X(I)
      CALL S_TRIM_ZEROS ( S2 )
    END IF

    IF ( I == 1 ) THEN
      S = ADJUSTL ( S2 )
    ELSE
      S = TRIM ( S ) // ', ' // ADJUSTL ( S2 )
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R8_EXTRACT ( S, R8, IERROR )

!*****************************************************************************80
!
!! R8_EXTRACT "extracts" an R8 from the beginning of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S; on input, a string from
!    whose beginning a real is to be extracted.  On output,
!    the real, if found, has been removed.
!
!    Output, real ( kind = 8 ) R8.  If IERROR is 0, then R4 contains the
!    next real read from the string; otherwise R4 is 0.
!
!    Output, integer ( kind = 4 ) IERROR.
!    0, no error.
!    nonzero, a real could not be extracted from the beginning of the
!    string.  R4 is 0.0 and S is unchanged.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  REAL ( KIND = 8 ) R8
  CHARACTER ( LEN = * ) S

  R8 = 0.0D+00

  CALL S_TO_R8 ( S, R8, IERROR, LENGTH )

  IF ( IERROR /= 0 .OR. LENGTH == 0 ) THEN
    IERROR = 1
    R8 = 0.0D+00
  ELSE
    CALL S_SHIFT_LEFT ( S, LENGTH )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE R8_INPUT ( STRING, VALUE, IERROR )

!*****************************************************************************80
!
!! R8_INPUT prints a prompt string and reads an R8 from the user.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    If the input line starts with a comment character ('#') or is blank,
!    the routine ignores that line, and tries to read the next one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the prompt string.
!
!    Output, real ( kind = 8 ) VALUE, the value input by the user.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag, which is zero
!    if no error occurred.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) STRING
  REAL ( KIND = 8 ) VALUE

  IERROR = 0
  VALUE = HUGE ( VALUE )
!
!  Write the prompt.
!
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( STRING )

  DO

    READ ( *, '(a)', IOSTAT = IERROR ) LINE

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF
!
!  If the line begins with a comment character, go back and read the next line.
!
    IF ( LINE(1:1) == '#' ) THEN
      CYCLE
    END IF

    IF ( LEN_TRIM ( LINE ) == 0 ) THEN
      CYCLE
    END IF
!
!  Extract integer information from the string.
!
    CALL S_TO_R8 ( LINE, VALUE, IERROR, LENGTH )

    IF ( IERROR /= 0 ) THEN
      VALUE = HUGE ( VALUE )
      RETURN
    END IF

    EXIT

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R8_NEXT ( S, R, DONE )

!*****************************************************************************80
!
!! R8_NEXT "reads" R8's from a string, one at a time.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
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
!    Input, character ( len = * ) S, a string, presumably containing real
!    numbers.  These may be separated by spaces or commas.
!
!    Output, real ( kind = 8 ) R.  If DONE is FALSE, then R contains the
!    "next" real value read from the string.  If DONE is TRUE, then
!    R is zero.
!
!    Input/output, logical DONE.
!    On input with a fresh string, the user should set DONE to TRUE.
!    On output, the routine sets DONE to FALSE if another real
!    value was read, or TRUE if no more reals could be read.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ), SAVE :: NEXT = 1
  REAL ( KIND = 8 ) R
  CHARACTER ( LEN = * ) S

  R = 0.0D+00

  IF ( DONE ) THEN
    NEXT = 1
    DONE = .FALSE.
  END IF

  IF ( LEN ( S ) < NEXT ) THEN
    DONE = .TRUE.
    RETURN
  END IF

  CALL S_TO_R8 ( S(NEXT:), R, IERROR, LENGTH )

  IF ( IERROR /= 0 ) THEN
    DONE = .TRUE.
    NEXT = 1
  ELSE IF ( LENGTH == 0 ) THEN
    DONE = .TRUE.
    NEXT = 1
  ELSE
    DONE = .FALSE.
    NEXT = NEXT + LENGTH
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE R8_TO_BINARY ( R, S )

!*****************************************************************************80
!
!! R8_TO_BINARY represents an R8 as a string of binary digits.
!
!  Discussion:
!
!    No check is made to ensure that the string is long enough.
!
!    The binary digits are a faithful representation of the real
!    number in base 2.
!
!  Example:
!
!      R           S
!
!    -10.75000    -1010.11
!      0.4218750  0.011011
!      0.3333333  0.01010101010101010101010
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the real number.
!
!    Output, character ( len = * ) S, the binary representation.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) ILO
  REAL ( KIND = 8 ) R
  REAL ( KIND = 8 ) RCOPY
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH < 1 ) THEN
    RETURN
  END IF

  RCOPY = R
  S = ' '

  IF ( RCOPY == 0.0D+00 ) THEN
    S = '0'
    RETURN
  END IF

  ILO = 0

  IF ( RCOPY < 0.0D+00 ) THEN
    ILO = 1
    S(ILO:ILO) = '-'
    RCOPY = -RCOPY
  END IF
!
!  Figure out the divisor.
!
  IEXP = 0

  DO WHILE ( 1.0D+00 <= RCOPY )
    RCOPY = RCOPY / 2.0D+00
    IEXP = IEXP + 1
  END DO

  DO WHILE ( RCOPY < 0.5D+00 )
    RCOPY = RCOPY * 2.0D+00
    IEXP = IEXP - 1
  END DO
!
!  Now 0.5 <= RCOPY < 1.
!
!  If IEXP < 0, print leading zeroes.
!
  IF ( IEXP == 0 ) THEN
    ILO = ILO + 1
    S(ILO:ILO) = '0'
  ELSE IF ( IEXP < 0 ) THEN
    ILO = ILO + 1
    S(ILO:ILO) = '0'
    ILO = ILO + 1
    S(ILO:ILO) = '.'
    DO I = 1, -IEXP
      ILO = ILO + 1
      S(ILO:ILO) = '0'
    END DO

  END IF
!
!  Now repeatedly double RCOPY.
!  Every time you exceed 1, that's a '1' digit.
!
  IEXP = IEXP + 1

  DO

    RCOPY = 2.0D+00 * RCOPY

    IEXP = IEXP - 1

    IF ( IEXP == 0 ) THEN
      ILO = ILO + 1
      S(ILO:ILO) = '.'
      IF ( S_LENGTH <= ILO ) THEN
        RETURN
      END IF
    END IF

    ILO = ILO + 1

    IF ( 1.0D+00 <= RCOPY ) THEN
      RCOPY = RCOPY - 1.0D+00
      S(ILO:ILO) = '1'
    ELSE
      S(ILO:ILO) = '0'
    END IF

    IF ( S_LENGTH <= ILO ) THEN
      RETURN
    END IF

    IF ( RCOPY == 0.0D+00 ) THEN
      EXIT
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE R8_TO_S_LEFT ( R8, S )

!*****************************************************************************80
!
!! R8_TO_S_LEFT writes an R8 into a left justified string.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    A 'G14.6' format is used with a WRITE statement.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R8, the number to be written into the string.
!
!    Output, character ( len = * ) S, the string into which
!    the real number is to be written.  If the string is less than 14
!    characters long, it will will be returned as a series of asterisks.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  REAL ( KIND = 8 ) R8
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = 14 ) S2

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH < 14 ) THEN

    DO I = 1, S_LENGTH
      S(I:I) = '*'
    END DO

  ELSE IF ( R8 == 0.0D+00 ) THEN
    S(1:14) = '     0.0      '
  ELSE
    WRITE ( S2, '(g14.6)' ) R8
    S(1:14) = S2
  END IF
!
!  Shift the string left.
!
  S = ADJUSTL ( S )

  RETURN
END

!*************************************************************

SUBROUTINE R8_TO_S_RIGHT ( D, S )

!*****************************************************************************80
!
!! R8_TO_S_LEFT writes an R8 into a right justified string.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    A 'G14.6' format is used with a WRITE statement.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) D, the number to be written into the string.
!
!    Output, character ( len = * ) S, the string into which
!    the real number is to be written.  If the string is less than 14
!    characters long, it will will be returned as a series of asterisks.
!
  IMPLICIT NONE

  REAL ( KIND = 8 ) D
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = 14 ) S2

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH < 14 ) THEN

    DO I = 1, S_LENGTH
      S(I:I) = '*'
    END DO

  ELSE IF ( D == 0.0D+00 ) THEN
    S(1:14) = '     0.0      '
  ELSE
    WRITE ( S2, '(g14.6)' ) D
    S(1:14) = S2
  END IF
!
!  Shift the string right.
!
  CALL S_ADJUSTR ( S )

  RETURN
END

!*************************************************************

FUNCTION R8_UNIFORM_01 ( SEED )

!*****************************************************************************80
!
!! R8_UNIFORM_01 returns a unit pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    This routine implements the recursion
!
!      seed = 16807 * seed mod ( 2**31 - 1 )
!      r8_uniform_01 = seed / ( 2**31 - 1 )
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

  INTEGER ( KIND = 4 ) K
  REAL ( KIND = 8 ) R8_UNIFORM_01
  INTEGER ( KIND = 4 ) SEED

  IF ( SEED == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
    WRITE ( *, '(a)' ) '  Input value of SEED = 0.'
    RETURN
  END IF

  K = SEED / 127773

  SEED = 16807 * ( SEED - K * 127773 ) - K * 2836

  IF ( SEED < 0 ) THEN
    SEED = SEED + 2147483647
  END IF
!
!  Although SEED can be represented exactly as a 32 bit integer,
!  it generally cannot be represented exactly as a 32 bit real number!
!
  R8_UNIFORM_01 = REAL ( SEED, KIND = 8 ) * 4.656612875D-10

  RETURN
END

!*************************************************************

SUBROUTINE RANGER ( S, MAXVAL, NVAL, IVAL )

!*****************************************************************************80
!
!! RANGER "understands" a range defined by a string like '4:8'.
!
!  Discussion:
!
!    The range can be much more complicated, as in
!
!      '4:8 10 2 14:20'
!
!    or (commas are optional)
!
!      '4:8,10, 2 , 14:20'
!
!    RANGER will return the values
!
!      4, 5, 6, 7, 8, 10, 2, 14, 15, 16, 17, 18, 19, 20
!
!    0 and negative integers are acceptable.  So are pairs
!    of values that are equal, as in '4:4', which just represents
!    4, and pairs that represent descending sequences, as
!    in '4:-2'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, contains a string of integers,
!    representing themselves, and pairs of integers representing
!    themselves and all integers between them.
!
!    Input, integer ( kind = 4 ) MAXVAL, the dimension of the IVAL vector,
!    which represents the maximum number of integers that may
!    be read from the string.
!
!    Output, integer ( kind = 4 ) NVAL, the number of integers read from
!    the string.
!
!    Output, integer ( kind = 4 ) IVAL(MAXVAL).  The first NVAL entries of
!    IVAL contain the integers read from the string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) MAXVAL

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) INC
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) IVAL(MAXVAL)
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) LENS
  INTEGER ( KIND = 4 ) NEXT
  INTEGER ( KIND = 4 ) NVAL
  CHARACTER ( LEN = * ) S

  NVAL = 0
!
!  Replace all commas by blanks.
!
  CALL S_CH_BLANK ( S, ',' )
!
!  Replace multiple consecutive blanks by one blank.
!
  CALL S_BLANKS_DELETE ( S )
!
!  Get the length of the string to the last nonblank.
!
  LENS = LEN_TRIM ( S )
!
!  Set a pointer to the next location to be examined.
!
  NEXT = 1

  DO WHILE ( NEXT <= LENS )
!
!  Find the next integer in the string.
!
    CALL S_TO_I4 ( S(NEXT:), INTVAL, IERROR, LENGTH )

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF
!
!  Move the pointer.
!
    NEXT = NEXT + LENGTH
!
!  If there's room, add the value to the list.
!
    IF ( MAXVAL <= NVAL ) THEN
      RETURN
    END IF

    NVAL = NVAL + 1
    IVAL(NVAL) = INTVAL
!
!  Have we reached the end of the string?
!
    IF ( LENS < NEXT ) THEN
      RETURN
    END IF
!
!  Skip past the next character if it is a space.
!
    IF ( S(NEXT:NEXT) == ' ' ) THEN
      NEXT = NEXT + 1
      IF ( LENS < NEXT ) THEN
        RETURN
      END IF
    END IF
!
!  Is the next character a colon?
!
    IF ( S(NEXT:NEXT) /= ':' ) THEN
      CYCLE
    END IF
!
!  Increase the pointer past the colon.
!
    NEXT = NEXT + 1

    IF ( LENS < NEXT ) THEN
      RETURN
    END IF
!
!  Find the next integer in the string.
!
    CALL S_TO_I4 ( S(NEXT:), INTVAL, IERROR, LENGTH )

    IF ( IERROR /= 0 ) THEN
      RETURN
    END IF
!
!  Move the pointer.
!
    NEXT = NEXT + LENGTH
!
!  Generate integers between the two values.
!
    ILO = IVAL(NVAL)

    IF ( ILO <= INTVAL ) THEN
      INC = + 1
    ELSE
      INC = -1
    END IF

    DO I = ILO+INC, INTVAL, INC

      IF ( MAXVAL <= NVAL ) THEN
        RETURN
      END IF

      NVAL = NVAL + 1
      IVAL(NVAL) = I

    END DO

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE RAT_TO_S_LEFT ( IVAL, JVAL, S )

!*****************************************************************************80
!
!! RAT_TO_S_LEFT returns a left-justified representation of IVAL/JVAL.
!
!  Discussion:
!
!    If the ratio is negative, a minus sign precedes IVAL.
!    A slash separates IVAL and JVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IVAL, JVAL, the integers whose ratio
!    IVAL/JVAL is to be represented.
!
!    If IVAL is nonzero and JVAL is 0, the string will be returned as "Inf"
!    or "-Inf" (Infinity), and if both IVAL and JVAL are zero, the string
!    will be returned as "NaN" (Not-a-Number).
!
!    Output, character ( len = * ) S, a left-justified string
!    containing the representation of IVAL/JVAL.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) IVAL2
  INTEGER ( KIND = 4 ) JVAL
  INTEGER ( KIND = 4 ) JVAL2
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 22 ) S2
!
!  Take care of simple cases right away.
!
  IF ( IVAL == 0 ) THEN

    IF ( JVAL /= 0 ) THEN
      S2 = '0'
    ELSE
      S2 = 'NaN'
    END IF

  ELSE IF ( JVAL == 0 ) THEN

    IF ( 0 < IVAL ) THEN
      S2 = 'Inf'
    ELSE
      S2 = '-Inf'
    END IF
!
!  Make copies of IVAL and JVAL.
!
  ELSE

    IVAL2 = IVAL
    JVAL2 = JVAL

    IF ( JVAL2 == 1 ) THEN
      WRITE ( S2, '(i11)' ) IVAL2
    ELSE
      WRITE ( S2, '(i11, ''/'', I10)' ) IVAL2, JVAL2
    END IF

    CALL S_BLANK_DELETE ( S2 )

  END IF

  S = S2

  RETURN
END

!*************************************************************

SUBROUTINE RAT_TO_S_RIGHT ( IVAL, JVAL, S )

!*****************************************************************************80
!
!! RAT_TO_S_RIGHT returns a right-justified representation of IVAL/JVAL.
!
!  Discussion:
!
!    If the ratio is negative, a minus sign precedes IVAL.
!    A slash separates IVAL and JVAL.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IVAL, JVAL, the two integers whose
!    ratio IVAL/JVAL is to be represented.
!
!    Note that if IVAL is nonzero and JVAL is 0, the string will
!    be returned as "Inf" or "-Inf" (Infinity), and if both
!    IVAL and JVAL are zero, the string will be returned as "NaN"
!    (Not-a-Number).
!
!    Output, character ( len = * ) S, a right-justified string
!    containing the representation of IVAL/JVAL.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) JVAL
  CHARACTER ( LEN = * ) S

  CALL RAT_TO_S_LEFT ( IVAL, JVAL, S )
  CALL S_ADJUSTR ( S )

  RETURN
END

!*************************************************************

FUNCTION S32_TO_I4 ( S32 )

!*****************************************************************************80
!
!! S32_TO_I4 returns an I4 equivalent to a 32 character string.
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
!    Input, character ( len = 32 ) S32, the character value.
!
!    Output, integer ( kind = 4 ) S32_TO_I4, a corresponding integer value.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INTVAL
  CHARACTER ( LEN = 32 ) S32
  INTEGER ( KIND = 4 ) S32_TO_I4
  CHARACTER ( LEN = 32 ) SCOPY

  SCOPY = S32

  IF ( SCOPY(1:1) == '1' ) THEN

    DO I = 2, 32

      IF ( SCOPY(I:I) == '0' ) THEN
        SCOPY(I:I) = '1'
      ELSE
        SCOPY(I:I) = '0'
      END IF

    END DO

  END IF

  INTVAL = 0

  DO I = 2, 32

    INTVAL = 2 * INTVAL

    IF ( SCOPY(I:I) == '1' ) THEN
      INTVAL = INTVAL + 1
    END IF

  END DO

  IF ( SCOPY(1:1) == '1' ) THEN
    INTVAL = - INTVAL
  END IF

  S32_TO_I4 = INTVAL

  RETURN
END

!*************************************************************

FUNCTION S32_TO_R4 ( S32 )

!*****************************************************************************80
!
!! S32_TO_R4 converts a 32-character variable into an R4.
!
!  Discussion:
!
!    An "R4" value is simply a real number to be stored as a
!    variable of type "real ( kind = 4 )".
!
!    The first bit is 1 for a negative real, or 0 for a
!    positive real.  Bits 2 through 9 are the exponent.  Bits 10
!    through 32 are used for a normalized representation of the
!    mantissa. Since it is assumed that normalization means the first
!    digit of the mantissa is 1, this 1 is in fact not stored.
!
!    The special case of 0 is represented by all 0 bits.
!
!    It is believed that this method corresponds to the format used
!    in VMS FORTRAN for reals.
!
!    Because of the limits on the mantissa, many Cray numbers are not
!    representable at all by this method.  These numbers are very big
!    or very small in magnitude.  Other numbers will simply be
!    represented with less accuracy.
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
!    Input, character ( len = 32 ) S32, the character variable to be decoded.
!
!    Output, real ( kind = 4 ) RCHAR32, the corresponding real value.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IEXP
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) MANT
  CHARACTER ( LEN = 32 ) S32
  REAL ( KIND = 4 ) S32_TO_R4
  REAL ( KIND = 4 ) SGN
!
!  Read sign bit.
!
  IF ( S32(1:1) == '1' ) THEN
    SGN = -1.0E+00
  ELSE
    SGN = 1.0E+00
  END IF
!
!  Construct exponent from bits 2 through 9, subtract 128.
!
  IEXP = 0

  DO I = 2, 9

    IF ( S32(I:I) == '0' ) THEN
      J = 0
    ELSE
      J = 1
    END IF

    IEXP = 2 * IEXP + J

  END DO

  IF ( IEXP == 0 ) THEN
    S32_TO_R4 = 0.0E+00
    RETURN
  END IF

  IEXP = IEXP - 128
!
!  Read mantissa from positions 10 through 32.
!  Note that, unless exponent equals 0, the most significant bit is
!  assumed to be 1 and hence is not stored.
!
  MANT = 1

  DO I = 10, 32
    MANT = 2 * MANT
    IF ( S32(I:I) == '1' ) THEN
      MANT = MANT + 1
    END IF
  END DO

  S32_TO_R4 = SGN * MANT * ( 2.0E+00 ** ( IEXP - 23 ) )

  RETURN
END

!*************************************************************

SUBROUTINE SEF_TO_B4_IEEE  ( S, E, F, WORD )

!*****************************************************************************80
!
!! SEF_TO_B4_IEEE converts SEF information to a 4 byte IEEE real word.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the sign bit:
!    0, if R is nonnegative;
!    1, if R is negative.
!
!    Input, integer ( kind = 4 ) E, the exponent, base 2.
!    Normally, -127 < E <= 127.
!    If E = 128, then the data is interpreted as NaN, Inf, or -Inf.
!    If -127 < E <= 127, the data is a normalized value.
!    If E < -127, then the data is a denormalized value.
!
!    Input, integer ( kind = 4 ) F, the mantissa.
!
!    Output, integer ( kind = 4 ) WORD, the real number stored in IEEE format.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) E2
  INTEGER ( KIND = 4 ) F
  INTEGER ( KIND = 4 ), PARAMETER :: F_MAX = 2**24
  INTEGER ( KIND = 4 ), PARAMETER :: F_MIN = 2**23
  INTEGER ( KIND = 4 ) F2
  INTEGER ( KIND = 4 ) S
  INTEGER ( KIND = 4 ) S2
  INTEGER ( KIND = 4 ) WORD

  S2 = S
  E2 = E
  F2 = F
!
!  Handle +Inf and -Inf.
!
  IF ( F /= 0 .AND. E == 128 ) THEN
    E2 = E2 + 127
    F2 = 2**23 - 1
    CALL MVBITS ( S2, 0,  1, WORD, 31 )
    CALL MVBITS ( E2, 0,  8, WORD, 23 )
    CALL MVBITS ( F2, 0, 23, WORD,  0 )
    RETURN
  END IF
!
!  Handle NaN.
!
  IF ( F == 0 .AND. E == 128 ) THEN
    E2 = E2 + 127
    F2 = 0
    CALL MVBITS ( S2, 0,  1, WORD, 31 )
    CALL MVBITS ( E2, 0,  8, WORD, 23 )
    CALL MVBITS ( F2, 0, 23, WORD,  0 )
    RETURN
  END IF
!
!  Handle +0 and -0.
!
  IF ( F == 0 ) THEN
    E2 = 0
    CALL MVBITS ( S2, 0,  1, WORD, 31 )
    CALL MVBITS ( E2, 0,  8, WORD, 23 )
    CALL MVBITS ( F2, 0, 23, WORD,  0 )
    RETURN
  END IF
!
!  Normalize.
!
  IF ( F < 0 ) THEN
    S2 = 1 - S2
    F2 = -F2
  END IF

  E2 = E2 + 127 + 23

  DO WHILE ( F_MAX <= F2 )
    F2 = F2 / 2
    E2 = E2 + 1
  END DO

  DO WHILE ( F2 < F_MIN )
    F2 = F2 * 2
    E2 = E2 - 1
  END DO
!
!  The biased exponent cannot be negative.
!  Shift it up to zero, and reduce F2.
!
  DO WHILE ( E2 < 0 .AND. F2 /= 0 )
    E2 = E2 + 1
    F2 = F2 / 2
  END DO
!
!  Normalized values drop the leading 1.
!
  IF ( 0 < E2 ) THEN
    CALL MVBITS ( S2, 0,  1, WORD, 31 )
    CALL MVBITS ( E2, 0,  8, WORD, 23 )
    F2 = F2 - F_MIN
    CALL MVBITS ( F2, 0, 23, WORD,  0 )
!
!  Denormalized values have a biased exponent of 0.
!
  ELSE
    CALL MVBITS ( S2, 0,  1, WORD, 31 )
    CALL MVBITS ( E2, 0,  8, WORD, 23 )
    CALL MVBITS ( F2, 0, 23, WORD,  0 )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE SEF_TO_R4 ( S, E, F, R )

!*****************************************************************************80
!
!! SEF_TO_R4 converts SEF information to an R4 = S * 2.0**E * F.
!
!  Discussion:
!
!    An "R4" value is simply a real number to be stored as a
!    variable of type "real ( kind = 4 )".
!
!    Assuming no arithmetic problems, in fact, this equality should be
!    exact, that is, S, E and F should exactly express the value
!    as stored on the computer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 November 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) S, the sign bit:
!    0, if R is nonnegative;
!    1, if R is negative.
!
!    Input, integer ( kind = 4 ) E, the exponent, base 2.
!
!    Input, integer ( kind = 4 ) F, the mantissa.
!
!    Output, real ( kind = 4 ) R, the real number.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) E
  INTEGER ( KIND = 4 ) F
  INTEGER ( KIND = 4 ) I
  REAL ( KIND = 4 ) R
  INTEGER ( KIND = 4 ) S

  IF ( F == 0 ) THEN
    R = 0.0E+00
    RETURN
  END IF

  IF ( S == 0 ) THEN
    R = 1.0E+00
  ELSE IF ( S == 1 ) THEN
    R = -1.0E+00
  ELSE
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'SEF_TO_R4 - Fatal error!'
    WRITE ( *, '(a,i8)' ) '  Illegal input value of S = ', S
    RETURN
  END IF

  R = R * REAL ( F, KIND = 4 )

  IF ( 0 < E ) THEN
    DO I = 1, E
      R = R * 2.0E+00
    END DO
  ELSE IF ( E < 0 ) THEN
    DO I = 1, -E
      R = R / 2.0E+00
    END DO
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE SORT_HEAP_EXTERNAL ( N, INDX, I, J, ISGN )

!*****************************************************************************80
!
!! SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
!
!  Discussion:
!
!    The actual list of data is not passed to the routine.  Hence this
!    routine may be used to sort integers, reals, numbers, names,
!    dates, shoe sizes, and so on.  After each call, the routine asks
!    the user to compare or interchange two items, until a special
!    return value signals that the sorting is completed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 February 2004
!
!  Author:
!
!    Original version by Albert Nijenhuis, Herbert Wilf.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Albert Nijenhuis, Herbert Wilf,
!    Combinatorial Algorithms for Computers and Calculators,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6,
!    LC: QA164.N54.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of items to be sorted.
!
!    Input/output, integer ( kind = 4 ) INDX, the main communication signal.
!
!    The user must set INDX to 0 before the first call.
!    Thereafter, the user should not change the value of INDX until
!    the sorting is done.
!
!    On return, if INDX is
!
!      greater than 0,
!      * interchange items I and J;
!      * call again.
!
!      less than 0,
!      * compare items I and J;
!      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
!      * call again.
!
!      equal to 0, the sorting is done.
!
!    Output, integer ( kind = 4 ) I, J, the indices of two items.
!    On return with INDX positive, elements I and J should be interchanged.
!    On return with INDX negative, elements I and J should be compared, and
!    the result reported in ISGN on the next call.
!
!    Input, integer ( kind = 4 ) ISGN, results of comparison of elements
!    I and J. (Used only when the previous call returned INDX less than 0).
!    ISGN <= 0 means I is less than or equal to J;
!    0 <= ISGN means I is greater than or equal to J.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ), SAVE :: I_SAVE = 0
  INTEGER ( KIND = 4 ) INDX
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ), SAVE :: J_SAVE = 0
  INTEGER ( KIND = 4 ), SAVE :: K = 0
  INTEGER ( KIND = 4 ), SAVE :: K1 = 0
  INTEGER ( KIND = 4 ) N
  INTEGER ( KIND = 4 ), SAVE :: N1 = 0
!
!  INDX = 0: This is the first call.
!
  IF ( INDX == 0 ) THEN

    I_SAVE = 0
    J_SAVE = 0
    K = N / 2
    K1 = K
    N1 = N
!
!  INDX < 0: The user is returning the results of a comparison.
!
  ELSE IF ( INDX < 0 ) THEN

    IF ( INDX == -2 ) THEN

      IF ( ISGN < 0 ) THEN
        I_SAVE = I_SAVE + 1
      END IF

      J_SAVE = K1
      K1 = I_SAVE
      INDX = -1
      I = I_SAVE
      J = J_SAVE
      RETURN

    END IF

    IF ( 0 < ISGN ) THEN
      INDX = 2
      I = I_SAVE
      J = J_SAVE
      RETURN
    END IF

    IF ( K <= 1 ) THEN

      IF ( N1 == 1 ) THEN
        I_SAVE = 0
        J_SAVE = 0
        INDX = 0
      ELSE
        I_SAVE = N1
        N1 = N1 - 1
        J_SAVE = 1
        INDX = 1
      END IF

      I = I_SAVE
      J = J_SAVE
      RETURN

    END IF

    K = K - 1
    K1 = K
!
!  0 < INDX, the user was asked to make an interchange.
!
  ELSE IF ( INDX == 1 ) THEN

    K1 = K

  END IF

  DO

    I_SAVE = 2 * K1

    IF ( I_SAVE == N1 ) THEN
      J_SAVE = K1
      K1 = I_SAVE
      INDX = -1
      I = I_SAVE
      J = J_SAVE
      RETURN
    ELSE IF ( I_SAVE <= N1 ) THEN
      J_SAVE = I_SAVE + 1
      INDX = -2
      I = I_SAVE
      J = J_SAVE
      RETURN
    END IF

    IF ( K <= 1 ) THEN
      EXIT
    END IF

    K = K - 1
    K1 = K

  END DO

  IF ( N1 == 1 ) THEN
    I_SAVE = 0
    J_SAVE = 0
    INDX = 0
    I = I_SAVE
    J = J_SAVE
  ELSE
    I_SAVE = N1
    N1 = N1 - 1
    J_SAVE = 1
    INDX = 1
    I = I_SAVE
    J = J_SAVE
  END IF

  RETURN
END

!*************************************************************

FUNCTION STATE_ID ( STATE )

!*****************************************************************************80
!
!! STATE_ID returns the 2 letter Postal Code for one of the 50 states.
!
!  Discussion:
!
!    The states are listed in order of their admission to the union.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) STATE, the index of a state.
!
!    Output, character ( len = 2 ) STATE_ID, the 2 letter code.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 2 ), PARAMETER, DIMENSION ( 50 ) :: ID = (/ &
    'DE', 'PA', 'NJ', 'GA', 'CT', &
    'MA', 'MD', 'SC', 'NH', 'VA', &
    'NY', 'NC', 'RI', 'VT', 'KY', &
    'TN', 'OH', 'LA', 'IN', 'MS', &
    'IL', 'AL', 'ME', 'MO', 'AR', &
    'MI', 'FL', 'TX', 'IA', 'WI', &
    'CA', 'MN', 'OR', 'KS', 'WV', &
    'NV', 'NE', 'CO', 'ND', 'SD', &
    'MT', 'WA', 'ID', 'WY', 'UT', &
    'OK', 'NM', 'AZ', 'AL', 'HI' /)
  INTEGER ( KIND = 4 ) STATE
  CHARACTER ( LEN = 2 ) STATE_ID

  IF ( STATE < 1 ) THEN
    STATE_ID = '??'
  ELSE IF ( STATE <= 50 ) THEN
    STATE_ID = ID(STATE)
  ELSE
    STATE_ID = '??'
  END IF

  RETURN
END

!*************************************************************

FUNCTION STATE_NAME ( STATE )

!*****************************************************************************80
!
!! STATE_NAME returns the name of one of the 50 states.
!
!  Discussion:
!
!    The states are listed in order of their admission to the union.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) STATE, the index of a state.
!
!    Output, character ( len = 14 ) STATE_NAME, the name of the state.
!
  IMPLICIT NONE

  CHARACTER ( LEN = 14 ), PARAMETER, DIMENSION ( 50 ) :: NAME = (/ &
  'Delaware      ', &
  'Pennsylvania  ', &
  'New Jersey    ', &
  'Georgia       ', &
  'Connecticut   ', &
  'Massachusetts ', &
  'Maryland      ', &
  'South Carolina', &
  'New Hampshire ', &
  'Virginia      ', &
  'New York      ', &
  'North Carolina', &
  'Rhode Island  ', &
  'Vermont       ', &
  'Kentucky      ', &
  'Tennessee     ', &
  'Ohio          ', &
  'Louisiana     ', &
  'Indiana       ', &
  'Missippi      ', &
  'Illinois      ', &
  'Alabama       ', &
  'Maine         ', &
  'Missouri      ', &
  'Arkansas      ', &
  'Michigan      ', &
  'Florida       ', &
  'Texas         ', &
  'Iowa          ', &
  'Wisconsin     ', &
  'California    ', &
  'Minnesota     ', &
  'Oregon        ', &
  'Kansas        ', &
  'West Virginia ', &
  'Nevada        ', &
  'Nebraska      ', &
  'Colorado      ', &
  'North Dakota  ', &
  'South Dakota  ', &
  'Montana       ', &
  'Washington    ', &
  'Idaho         ', &
  'Wyoming       ', &
  'Utah          ', &
  'Oklahoma      ', &
  'New Mexico    ', &
  'Arizona       ', &
  'Alaska        ', &
  'Hawaii        ' /)
  INTEGER ( KIND = 4 ) STATE
  CHARACTER ( LEN = 14 ) STATE_NAME

  IF ( STATE < 1 ) THEN
    STATE_NAME = '??????????????'
  ELSE IF ( STATE <= 50 ) THEN
    STATE_NAME = NAME(STATE)
  ELSE
    STATE_NAME = '??????????????'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE SVECI_SEARCH_BINARY_A ( N, A, B, INDX )

!*****************************************************************************80
!
!! SVECI_SEARCH_BINARY_A: search ascending sorted implicitly capitalized SVEC
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.9,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the vector.
!
!    Input, character ( len = * ) A(N), the array to be searched.  A must
!    be sorted in increasing order.
!
!    Input, character ( len = * ) B, the value to be searched for.
!
!    Output, integer ( kind = 4 ) INDX, the result of the search.
!    0, B does not occur in A.
!    I, A(I) = B, ignoring capitalization.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  CHARACTER ( LEN = * ) B
  INTEGER ( KIND = 4 ) HIGH
  INTEGER ( KIND = 4 ) INDX
  INTEGER ( KIND = 4 ) LOW
  INTEGER ( KIND = 4 ) MID

  INDX = 0

  LOW = 1
  HIGH = N

  DO WHILE ( LOW <= HIGH )

    MID = ( LOW + HIGH ) / 2

    IF ( S_EQI ( A(MID), B ) ) THEN
      INDX = MID
      EXIT
    ELSE IF ( S_LTI ( A(MID), B ) ) THEN
      LOW = MID + 1
    ELSE IF ( S_GTI ( A(MID), B ) ) THEN
      HIGH = MID - 1
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVECI_SORT_HEAP_A ( N, SARRAY )

!*****************************************************************************80
!
!! SVECI_SORT_HEAP_A heap sorts an SVEC of implicitly capitalized strings.
!
!  Discussion:
!
!    The characters in an implicitly capitalized string are treated as
!    though they had been capitalized.  Thus, the letters 'a' and 'A'
!    are considered equal, both 'a' and 'A' precede 'B', and
!    'Fox' and 'fOx' are considered equal.
!
!    The ASCII collating sequence is used, except that all
!    alphabetic characters are treated as though they were uppercase.
!
!    This means
!
!      A = a < B = b < C = c < .... < Y = y < Z = z.
!
!    Numbers and other symbols may also occur, and will be sorted
!    according to the ASCII ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in SARRAY.
!
!    Input/output, character ( len = * ) SARRAY(N), the array to be sorted.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) L
  INTEGER ( KIND = 4 ) L1
  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) N1
  CHARACTER ( LEN = * ) SARRAY(N)
  CHARACTER ( LEN = 255 ) S

  N1 = N
  L = N / 2
  S = SARRAY(L)
  L1 = L

  DO

    M = 2 * L1

    IF ( M <= N1 ) THEN

      IF ( M < N1 ) THEN
        IF ( S_GEI ( SARRAY(M+1), SARRAY(M) ) ) THEN
          M = M + 1
        END IF
      END IF

      IF ( S_LTI ( S, SARRAY(M) ) ) THEN
        SARRAY(L1) = SARRAY(M)
        L1 = M
        CYCLE
      END IF

    END IF

    SARRAY(L1) = S

    IF ( 1 < L ) THEN
      L = L - 1
      S = SARRAY(L)
      L1 = L
      CYCLE
    END IF

    IF ( N1 < 2 ) THEN
      EXIT
    END IF

    S = SARRAY(N1)
    SARRAY(N1) = SARRAY(1)

    N1 = N1 - 1
    L1 = L

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVECI_SORT_HEAP_A_INDEX ( N, SARRAY, INDX )

!*****************************************************************************80
!
!! SVECI_SORT_HEAP_A_INDEX index heap sorts an SVECI.
!
!  Discussion:
!
!    The sorting is not actually carried out,
!    but rather an index vector is returned, which defines the
!    sorting.  This index vector may be used to sort the array, or
!    to sort related arrays keyed on the first one.
!
!    The ASCII collating sequence is used, except that all
!    alphabetic characters are treated as though they were uppercase.
!
!    This means
!
!      A = a < B = b < C = c < .... < Y = y < Z = z.
!
!    Numbers and other symbols may also occur, and will be sorted according to
!    the ASCII ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in SARRAY.
!
!    Input, character ( len = * ) SARRAY(N), an array to be sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), contains the sort index.  The
!    I-th element of the sorted array is SARRAY ( INDX(I) ).
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INDX(N)
  INTEGER ( KIND = 4 ) INDXT
  INTEGER ( KIND = 4 ) IR
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) L
  CHARACTER ( LEN = * ) SARRAY(N)
  CHARACTER ( LEN = 255 ) S

  DO I = 1, N
    INDX(I) = I
  END DO

  L = N / 2 + 1
  IR = N

  DO

    IF ( 1 < L ) THEN

      L = L - 1
      INDXT = INDX(L)
      S = SARRAY(INDXT)

    ELSE

      INDXT = INDX(IR)
      S = SARRAY(INDXT)
      INDX(IR) = INDX(1)
      IR = IR - 1

      IF ( IR == 1 ) THEN
        INDX(1) = INDXT
        RETURN
      END IF

    END IF

    I = L
    J = L + L

    DO WHILE ( J <= IR )

      IF ( J < IR ) THEN
        IF ( S_LTI ( SARRAY ( INDX(J) ), SARRAY ( INDX(J+1) ) ) ) THEN
          J = J + 1
        END IF
      END IF

      IF ( S_LTI ( S, SARRAY ( INDX(J) ) ) ) THEN
        INDX(I) = INDX(J)
        I = J
        J = J + J
      ELSE
        J = IR + 1
      END IF

    END DO

    INDX(I) = INDXT

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_LAB ( N, NUNIQ, SVEC, IDENT )

!*****************************************************************************80
!
!! SVEC_LAB makes an index array for an array of (repeated) strings.
!
!  Discussion:
!
!    The routine is given an array of strings.  It assigns an integer
!    to each unique string, and returns an equivalent array of
!    these values.
!
!    Note that blank strings are treated specially.  Any blank
!    string gets an identifier of 0.  Blank strings are not
!    counted in the value of NUNIQ.
!
!  Example:
!
!    SVEC    IDENT
!
!    ALPHA       1
!    ALPHA      -1
!    BETA        2
!    ALPHA      -1
!    BETA       -2
!    GAMMA       3
!    ALPHA      -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries.
!
!    Output, integer ( kind = 4 ) NUNIQ, the number of unique nonblank entries.
!
!    Input, character ( len = * ) SVEC(N), the list of strings.
!
!    Output, integer ( kind = 4 ) IDENT(N), the identifiers assigned to the
!    strings.  If SVEC(I) is blank, then IDENT(I) is 0.
!    Otherwise, if SVEC(I) is the first occurrence of a
!    given string, then it is assigned a positive identifier.
!    If SVEC(I) is a later occurrence of a string, then
!    it is assigned a negative identifier, whose absolute
!    value is the identifier of the first occurrence.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDENT(N)
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) MATCH
  INTEGER ( KIND = 4 ) NUNIQ
  CHARACTER ( LEN = * ) SVEC(N)

  NUNIQ = 0

  DO I = 1, N

    IF ( SVEC(I) == ' ' ) THEN

      IDENT(I) = 0

    ELSE

      MATCH = 0

      DO J = 1, I-1
        IF ( 0 < IDENT(J) ) THEN
          IF ( SVEC(J) == SVEC(I) ) THEN
            IDENT(I) = -IDENT(J)
            MATCH = J
            EXIT
          END IF
        END IF
      END DO

      IF ( MATCH == 0 ) THEN
        NUNIQ = NUNIQ + 1
        IDENT(I) = NUNIQ
      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_MERGE_A ( NA, A, NB, B, NC, C )

!*****************************************************************************80
!
!! SVEC_MERGE_A merges two ascending sorted string arrays.
!
!  Discussion:
!
!    The elements of A and B should be sorted in ascending order.
!
!    The elements in the output array C will be in ascending order, and unique.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NA, the dimension of A.
!
!    Input, character ( len = * ) A(NA), the first sorted array.
!
!    Input, integer ( kind = 4 ) NB, the dimension of B.
!
!    Input, character ( len = * ) B(NB), the second sorted array.
!
!    Output, integer ( kind = 4 ) NC, the number of elements in the output
!    array.  Note that C should usually be dimensioned at least NA+NB in the
!    calling routine.
!
!    Output, character ( len = * ) C(NC), the merged unique sorted array.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) NA
  INTEGER ( KIND = 4 ) NB

  CHARACTER ( LEN = * ) A(NA)
  CHARACTER ( LEN = * ) B(NB)
  CHARACTER ( LEN = * ) C(NA+NB)
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) JA
  INTEGER ( KIND = 4 ) JB
  INTEGER ( KIND = 4 ) NA2
  INTEGER ( KIND = 4 ) NB2
  INTEGER ( KIND = 4 ) NC

  NA2 = NA
  NB2 = NB

  JA = 0
  JB = 0
  NC = 0

  DO
!
!  If we've used up all the entries of A, stick the rest of B on the end.
!
    IF ( NA2 <= JA ) THEN

      DO J = 1, NB2 - JB
        JB = JB + 1
        IF ( NC == 0 ) THEN
          NC = NC + 1
          C(NC) = B(JB)
        ELSE IF ( LLT ( C(NC), B(JB) ) ) THEN
          NC = NC + 1
          C(NC) = B(JB)
        END IF
      END DO

      EXIT
!
!  If we've used up all the entries of B, stick the rest of A on the end.
!
    ELSE IF ( NB2 <= JB ) THEN

      DO J = 1, NA2 - JA
        JA = JA + 1
        IF ( NC == 0 ) THEN
          NC = NC + 1
          C(NC) = A(JA)
        ELSE IF ( LLT ( C(NC), A(JA) ) ) THEN
          NC = NC + 1
          C(NC) = A(JA)
        END IF
      END DO

      EXIT
!
!  Otherwise, if the next entry of A is smaller, that's our candidate.
!
    ELSE IF ( LLE ( A(JA+1), B(JB+1) ) ) THEN

      JA = JA + 1
      IF ( NC == 0 ) THEN
        NC = NC + 1
        C(NC) = A(JA)
      ELSE IF ( LLT ( C(NC), A(JA) ) ) THEN
        NC = NC + 1
        C(NC) = A(JA)
      END IF
!
!  ...or if the next entry of B is the smaller, consider that.
!
    ELSE

      JB = JB + 1
      IF ( NC == 0 ) THEN
        NC = NC + 1
        C(NC) = B(JB)
      ELSE IF ( LLT ( C(NC), B(JB) ) ) THEN
        NC = NC + 1
        C(NC) = B(JB)
      END IF
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_PERMUTE ( N, A, P )

!*****************************************************************************80
!
!! SVEC_PERMUTE permutes a string vector in place.
!
!  Example:
!
!    Input:
!
!      N = 5
!      P = (  3,     2,     4,       2,      1 )
!      A = ( 'ONE', 'TWO', 'THREE', 'FOUR', 'FIVE' )
!
!    Output:
!
!      A    = ( 'FIVE', 'FOUR', 'ONE', 'THREE', 'TWO' ).
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
!    Input, integer ( kind = 4 ) N, the number of objects.
!
!    Input/output, character ( len = * ) A(N), the array to be permuted.
!
!    Input, integer ( kind = 4 ) P(N), the permutation.  P(I) = J means
!    that the I-th element of the output array should be the J-th
!    element of the input array.  P must be a legal permutation
!    of the integers from 1 to N, otherwise the algorithm will
!    fail catastrophically.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  CHARACTER ( LEN = 255 ) A_TEMP
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IGET
  INTEGER ( KIND = 4 ) IPUT
  INTEGER ( KIND = 4 ) ISTART
  INTEGER ( KIND = 4 ) P(N)

  CALL PERM_CHECK ( N, P, IERROR )

  IF ( IERROR /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'SVEC_PERMUTE - Fatal error!'
    WRITE ( *, '(a)' ) '  The input array does not represent'
    WRITE ( *, '(a)' ) '  a proper permutation.  In particular, the'
    WRITE ( *, '(a,i8)' ) '  array is missing the value ', IERROR
    RETURN
  END IF
!
!  Search for the next element of the permutation that has not been used.
!
  DO ISTART = 1, N

    IF ( P(ISTART) < 0 ) THEN

      CYCLE

    ELSE IF ( P(ISTART) == ISTART ) THEN

      P(ISTART) = -P(ISTART)
      CYCLE

    ELSE

      A_TEMP = A(ISTART)
      IGET = ISTART
!
!  Copy the new value into the vacated entry.
!
      DO

        IPUT = IGET
        IGET = P(IGET)

        P(IPUT) = -P(IPUT)

        IF ( IGET < 1 .OR. N < IGET ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'SVEC_PERMUTE - Fatal error!'
          WRITE ( *, '(a)' ) '  A permutation index is out of range.'
          WRITE ( *, '(a,i8,a,i8)' ) '  P(', IPUT, ') = ', IGET
          RETURN
        END IF

        IF ( IGET == ISTART ) THEN
          A(IPUT) = A_TEMP
          EXIT
        END IF

        A(IPUT) = A(IGET)

      END DO

    END IF

  END DO
!
!  Restore the signs of the entries.
!
  P(1:N) = -P(1:N)

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_REVERSE ( N, A )

!*****************************************************************************80
!
!! SVEC_REVERSE reverses the elements of a string vector.
!
!  Example:
!
!    Input:
!
!      N = 4,
!      A = ( 'Bob', 'Carol', 'Ted', 'Alice' ).
!
!    Output:
!
!      A = ( 'Alice', 'Ted', 'Carol', 'Bob' ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, character ( len = * ) A(N), the array to be reversed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  CHARACTER ( LEN = 255 ) A_TEMP
  INTEGER ( KIND = 4 ) I

  DO I = 1, N / 2
    A_TEMP   = A(I)
    A(I)     = A(N+1-I)
    A(N+1-I) = A_TEMP
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_SEARCH_BINARY_A ( N, A, B, INDX )

!*****************************************************************************80
!
!! SVEC_SEARCH_BINARY_A searches an ascending sorted string vector.
!
!  Discussion:
!
!    Binary search is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Kreher, Douglas Simpson,
!    Algorithm 1.9,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the vector.
!
!    Input, character ( len = * ) A(N), the array to be searched.  A must
!    be sorted in increasing order.
!
!    Input, character ( len = * ) B, the value to be searched for.
!
!    Output, integer ( kind = 4 ) INDX, the result of the search.
!    0, B does not occur in A.
!    I, A(I) = B.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  CHARACTER ( LEN = * ) B
  INTEGER ( KIND = 4 ) HIGH
  INTEGER ( KIND = 4 ) INDX
  INTEGER ( KIND = 4 ) LOW
  INTEGER ( KIND = 4 ) MID

  INDX = 0

  LOW = 1
  HIGH = N

  DO WHILE ( LOW <= HIGH )

    MID = ( LOW + HIGH ) / 2

    IF ( A(MID) == B ) THEN
      INDX = MID
      EXIT
    ELSE IF ( LLT ( A(MID), B ) ) THEN
      LOW = MID + 1
    ELSE IF ( LGT ( A(MID), B ) ) THEN
      HIGH = MID - 1
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_SORTED_UNIQUE ( N, A, UNIQUE_NUM )

!*****************************************************************************80
!
!! SVEC_SORTED_UNIQUE: number of unique entries in a sorted SVEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the array.
!
!    Input/output, character ( len = * ) A(N).
!    On input, the sorted list of strings.
!    On output, the unique elements, in sorted order.
!
!    Output, integer ( kind = 4 ) UNIQUE_NUM, the number of unique elements
!    in the array.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  INTEGER ( KIND = 4 ) ITEST
  INTEGER ( KIND = 4 ) UNIQUE_NUM

  IF ( N <= 0 ) THEN
    UNIQUE_NUM = 0
    RETURN
  END IF

  UNIQUE_NUM = 1

  DO ITEST = 2, N

    IF ( A(ITEST) /= A(UNIQUE_NUM) ) THEN
      UNIQUE_NUM = UNIQUE_NUM + 1
      A(UNIQUE_NUM) = A(ITEST)
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_SORT_HEAP_A ( N, A )

!*****************************************************************************80
!
!! SVEC_SORT_HEAP_A ascending sorts an SVEC using heap sort.
!
!  Discussion:
!
!    The ASCII collating sequence is used.  This means
!      A < B < C < .... < Y < Z < a < b < .... < z.
!    Numbers and other symbols may also occur, and will be sorted according to
!    the ASCII ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of strings
!
!    Input/output, character ( len = * ) A(N);
!    On input, an array of strings to be sorted.
!    On output, the sorted array.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) A(N)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INDX
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) J
!
!  Do the sorting using the external heap sort routine.
!
  I = 0
  INDX = 0
  ISGN = 0
  J = 0

  DO

    CALL SORT_HEAP_EXTERNAL ( N, INDX, I, J, ISGN )

    IF ( 0 < INDX ) THEN

      CALL S_SWAP ( A(I), A(J) )

    ELSE IF ( INDX < 0 ) THEN

      IF ( LLE ( A(I), A(J) ) ) THEN
        ISGN = -1
      ELSE
        ISGN = +1
      END IF

    ELSE IF ( INDX == 0 ) THEN

      EXIT

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SVEC_SORT_HEAP_A_INDEX ( N, SARRAY, INDX )

!*****************************************************************************80
!
!! SVEC_SORT_HEAP_A_INDEX: case-sensitive indexed heap sort of an SVEC.
!
!  Discussion:
!
!    The sorting is not actually carried out.
!    Rather an index array is created which defines the sorting.
!    This array may be used to sort or index the array, or to sort or
!    index related arrays keyed on the original array.
!
!    The ASCII collating sequence is used, and case is significant.
!    This means
!
!      A < B < C < .... < Y < Z < a < b < .... < z.
!
!    Numbers and other symbols may also occur, and will be sorted according to
!    the ASCII ordering.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in SARRAY.
!
!    Input, character ( len = * ) SARRAY(N), an array to be sorted.
!
!    Output, integer ( kind = 4 ) INDX(N), contains the sort index.  The
!    I-th element of the sorted array is SARRAY ( INDX(I) ).
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INDX(N)
  INTEGER ( KIND = 4 ) INDXT
  INTEGER ( KIND = 4 ) IR
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) L
  CHARACTER ( LEN = * ) SARRAY(N)
  CHARACTER ( LEN = 255 ) STRING

  DO I = 1, N
    INDX(I) = I
  END DO

  L = N / 2 + 1
  IR = N

  DO

    IF ( 1 < L ) THEN

      L = L - 1
      INDXT = INDX(L)
      STRING = SARRAY(INDXT)

    ELSE

      INDXT = INDX(IR)
      STRING = SARRAY(INDXT)
      INDX(IR) = INDX(1)
      IR = IR - 1

      IF ( IR == 1 ) THEN
        INDX(1) = INDXT
        RETURN
      END IF

    END IF

    I = L
    J = L + L

    DO WHILE ( J <= IR )

      IF ( J < IR ) THEN
        IF ( LLT ( SARRAY ( INDX(J) ), SARRAY ( INDX(J+1) ) ) ) THEN
          J = J + 1
        END IF
      END IF

      IF ( LLT ( STRING, SARRAY ( INDX(J) ) ) ) THEN
        INDX(I) = INDX(J)
        I = J
        J = J + J
      ELSE
        J = IR + 1
      END IF

    END DO

    INDX(I) = INDXT

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE SYM_TO_CH ( SYM, C, IHI )

!*****************************************************************************80
!
!! SYM_TO_CH returns the character represented by a symbol.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) SYM is a string containing printable symbols.
!
!    Output, character C, is the ASCII character represented by the
!    first symbol in SYM.
!
!    Output, integer ( kind = 4 ) IHI, C is represented by SYM(1:IHI).
!    IHI = 0 if there was a problem.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) IALT
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) ICTL
  INTEGER ( KIND = 4 ) IHI
  CHARACTER ( LEN = * ) SYM
  INTEGER ( KIND = 4 ) SYM_LENGTH

  C = ' '
  SYM_LENGTH = LEN_TRIM ( SYM )

  IF ( SYM_LENGTH <= 0 ) THEN
    C = ' '
    IHI = 0
    RETURN
  END IF

  IALT = 0
  ICTL = 0
  IHI = 1
!
!  Could it be an ALT character?
!
  IF ( SYM(IHI:IHI) == '!' .AND. IHI < SYM_LENGTH ) THEN
    IALT = 1
    IHI = IHI + 1
  END IF
!
!  Could it be a control character?
!
  IF ( SYM(IHI:IHI) == '^' .AND. IHI < SYM_LENGTH ) THEN
    ICTL = 1
    IHI = IHI + 1
  END IF
!
!  Could it be a DEL character?
!
  ICHR = IACHAR ( SYM(IHI:IHI) )

  IF ( IHI+2 <= SYM_LENGTH ) THEN
    IF ( S_EQI ( SYM(IHI:IHI+2), 'DEL' ) ) THEN
      ICHR = 127
      IHI = IHI + 2
    END IF
  END IF
!
!  Could it be an SP character?
!
  IF ( IHI + 1 <= SYM_LENGTH ) THEN
    IF ( S_EQI ( SYM(IHI:IHI+1), 'SP' ) ) THEN
      ICHR = 32
      IHI = IHI + 1
    END IF
  END IF
!
!  Interpret the character.
!
  IF ( IALT == 1 ) THEN
    ICHR = ICHR + 128
  END IF

  IF ( ICTL == 1 ) THEN
    ICHR = ICHR - 64
  END IF

  C = ACHAR ( ICHR )

  RETURN
END

!*************************************************************

SUBROUTINE S_ADJUSTL ( S )

!*****************************************************************************80
!
!! S_ADJUSTL flushes a string left.
!
!  Discussion:
!
!    Both blanks and tabs are treated as "white space".
!
!    This routine is similar to the FORTRAN90 ADJUSTL routine.
!
!  Example:
!
!    Input             Output
!
!    '     Hello'      'Hello     '
!    ' Hi there!  '    'Hi there!   '
!    'Fred  '          'Fred  '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.
!    On input, S is a string of characters.
!    On output, any initial blank or tab characters have been cut.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) NONB
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
!
!  Check the length of the string to the last nonblank.
!  If nonpositive, return.
!
  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 0 ) THEN
    RETURN
  END IF
!
!  Find NONB, the location of the first nonblank, nontab.
!
  NONB = 0

  DO I = 1, S_LENGTH

    IF ( S(I:I) /= ' ' .AND. S(I:I) /= TAB ) THEN
      NONB = I
      EXIT
    END IF

  END DO

  IF ( NONB == 0 ) THEN
    S = ' '
    RETURN
  END IF
!
!  Shift the string left.
!
  IF ( 1 < NONB ) THEN
    DO I = 1, S_LENGTH + 1 - NONB
      S(I:I) = S(I+NONB-1:I+NONB-1)
    END DO
  END IF
!
!  Blank out the end of the string.
!
  S(S_LENGTH+2-NONB:S_LENGTH) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_ADJUSTR ( S )

!*****************************************************************************80
!
!! S_ADJUSTR flushes a string right.
!
!  Example:
!
!    Input             Output
!    'Hello     '      '     Hello'
!    ' Hi there!  '    '   Hi there!'
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
!    Input/output, character ( len = * ) S, on output, trailing blank
!    characters have been cut, and pasted back onto the front.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) NONB
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
!
!  Check the full length of the string.
!
  S_LENGTH = LEN ( S )
!
!  Find the occurrence of the last nonblank.
!
  NONB = LEN_TRIM ( S )
!
!  Shift the string right.
!
  DO I = S_LENGTH, S_LENGTH + 1 - NONB, -1
    S(I:I) = S(I-S_LENGTH+NONB:I-S_LENGTH+NONB)
  END DO
!
!  Blank out the beginning of the string.
!
  S(1:S_LENGTH-NONB) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_AFTER_SS_COPY ( S1, SS, S2 )

!*****************************************************************************80
!
!! S_AFTER_SS_COPY copies a string after a given substring.
!
!  Discussion:
!
!    S1 and S2 can be the same object, in which case the string is
!    overwritten by a copy of itself after the substring.
!
!  Example:
!
!    Input:
!
!      S1 = 'ABCDEFGH'
!      SS = 'EF'
!
!    Output:
!
!      S2 = 'GH'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be copied.
!
!    Input, character ( len = * ) SS, the substring after which the copy begins.
!
!    Output, character ( len = * ) S2, the copied portion of S.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) LAST
  INTEGER ( KIND = 4 ) LAST_S2
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = * ) SS
!
!  Find the first occurrence of the substring.
!
  FIRST = INDEX ( S1, SS )
!
!  If the substring doesn't occur at all, then S2 is blank.
!
  IF ( FIRST == 0 ) THEN
    S2 = ' '
    RETURN
  END IF
!
!  Redefine FIRST to point to the first character to copy after
!  the substring.
!
  FIRST = FIRST + LEN ( SS )
!
!  Measure the two strings.
!
  S1_LENGTH = LEN ( S1 )
  LAST_S2 = LEN ( S2 )
!
!  Adjust effective length of S if S2 is short.
!
  LAST = MIN ( S1_LENGTH, LAST_S2 + FIRST - 1 )
!
!  Copy the string.
!
  S2(1:S1_LENGTH+1-FIRST) = S1(FIRST:S1_LENGTH)
!
!  Clear out the rest of the copy.
!
  S2(S1_LENGTH+2-FIRST:LAST_S2) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_ALPHA_LAST ( S, ILOC )

!*****************************************************************************80
!
!! S_ALPHA_LAST returns the location of the last alphabetic character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Output, integer ( kind = 4 ) ILOC, the location of the last alphabetic
!    character in the string.  If there are no alphabetic
!    characters, ILOC is returned as 0.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ILOC
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = S_LENGTH, 1, -1
    IF ( CH_IS_ALPHA ( S(I:I) ) ) THEN
      ILOC = I
      RETURN
    END IF
  END DO

  ILOC = 0

  RETURN
END

!*************************************************************

FUNCTION S_ANY_ALPHA ( S )

!*****************************************************************************80
!
!! S_ANY_ALPHA is TRUE if a string contains any alphabetic character.
!
!  Example:
!
!    Input         Output
!
!    Riding Hood   TRUE
!    123 + 34      FALSE
!    Seven Eleven  TRUE
!    1.0E+11       TRUE
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be checked.
!
!    Output, logical S_ANY_ALPHA is TRUE if any character in string
!    is an alphabetic character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  LOGICAL S_ANY_ALPHA
  INTEGER ( KIND = 4 ) S_LENGTH

  S_ANY_ALPHA = .TRUE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( CH_IS_ALPHA ( S(I:I) ) ) THEN
      RETURN
    END IF
  END DO

  S_ANY_ALPHA = .FALSE.

  RETURN
END

!*************************************************************

FUNCTION S_ANY_CONTROL ( S )

!*****************************************************************************80
!
!! S_ANY_CONTROL is TRUE if a string contains any control characters.
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
!    Input, character ( len = * ) S, is the string to check.
!
!    Output, logical S_ANY_CONTROL, is TRUE if any character is a control
!    character.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  LOGICAL S_ANY_CONTROL
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( CH_IS_CONTROL ( S(I:I) ) ) THEN
      S_ANY_CONTROL = .TRUE.
      RETURN
    END IF
  END DO

  S_ANY_CONTROL = .FALSE.

  RETURN
END

!*************************************************************

SUBROUTINE S_B2U ( S )

!*****************************************************************************80
!
!! S_B2U replaces interword blanks by underscores.
!
!  Discussion:
!
!    Initial blanks are deleted by shifting the string to be
!    flush left.
!
!    This routine is useful for making a multiword name look
!    like a single blank-delimited string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be
!    transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S = ADJUSTL ( S )
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( S(I:I) == ' ' ) THEN
      S(I:I) = '_'
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_BEFORE_SS_COPY ( S, SS, S2 )

!*****************************************************************************80
!
!! S_BEFORE_SS_COPY copies a string up to a given substring.
!
!  Discussion:
!
!    S and S2 can be the same object, in which case the string is
!    overwritten by a copy of itself up to the substring, followed
!    by blanks.
!
!  Example:
!
!    Input:
!
!      S = 'ABCDEFGH'
!      SS = 'EF'
!
!    Output:
!
!      S2 = 'ABCD'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be copied.
!
!    Input, character ( len = * ) SS, the substring before which the copy stops.
!
!    Output, character ( len = * ) S2, the copied portion of S.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) LAST
  INTEGER ( KIND = 4 ) LAST_S2
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = * ) SS
!
!  Find the first occurrence of the substring.
!
  LAST = INDEX ( S, SS )
!
!  If the substring doesn't occur at all, behave as though it begins
!  just after the string terminates.
!
!  Now redefine LAST to point to the last character to copy before
!  the substring begins.
!
  IF ( LAST == 0 ) THEN
    LAST = LEN ( S )
  ELSE
    LAST = LAST - 1
  END IF
!
!  Now adjust again in case the copy holder is "short".
!
  LAST_S2 = LEN ( S2 )

  LAST = MIN ( LAST, LAST_S2 )
!
!  Copy the beginning of the string.
!  Presumably, compilers now understand that if LAST is 0, we don't
!  copy anything.
!  Clear out the rest of the copy.
!
  S2(1:LAST) = S(1:LAST)
  S2(LAST+1:LAST_S2) = ' '

  RETURN
END

!*************************************************************

FUNCTION S_BEGIN ( S1, S2 )

!*****************************************************************************80
!
!! S_BEGIN is TRUE if one string matches the beginning of the other.
!
!  Discussion:
!
!    The strings are compared, ignoring blanks, spaces and capitalization.
!
!  Example:
!
!     S1              S2      S_BEGIN
!
!    'Bob'          'BOB'     TRUE
!    '  B  o b '    ' bo b'   TRUE
!    'Bob'          'Bobby'   TRUE
!    'Bobo'         'Bobb'    FALSE
!    ' '            'Bob'     FALSE    (Do not allow a blank to match
!                                       anything but another blank string.)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to be compared.
!
!    Output, logical S_BEGIN, is TRUE if the strings match up to
!    the end of the shorter string, ignoring case.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  LOGICAL S_BEGIN
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN_TRIM ( S1 )
  S2_LENGTH = LEN_TRIM ( S2 )
!
!  If either string is blank, then both must be blank to match.
!  Otherwise, a blank string matches anything, which is not
!  what most people want.
!
  IF ( S1_LENGTH == 0 .OR. S2_LENGTH == 0 ) THEN

    IF ( S1_LENGTH == 0 .AND. S2_LENGTH == 0 ) THEN
      S_BEGIN = .TRUE.
    ELSE
      S_BEGIN = .FALSE.
    END IF

    RETURN

  END IF

  I1 = 0
  I2 = 0
!
!  Find the next nonblank in S1.
!
  DO

    DO

      I1 = I1 + 1

      IF ( S1_LENGTH < I1 ) THEN
        S_BEGIN = .TRUE.
        RETURN
      END IF

      IF ( S1(I1:I1) /= ' ' ) THEN
        EXIT
      END IF

    END DO
!
!  Find the next nonblank in S2.
!
    DO

      I2 = I2 + 1

      IF ( S2_LENGTH < I2 ) THEN
        S_BEGIN = .TRUE.
        RETURN
      END IF

      IF ( S2(I2:I2) /= ' ' ) THEN
        EXIT
      END IF

    END DO
!
!  If the characters match, get the next pair.
!
    IF ( .NOT. CH_EQI ( S1(I1:I1), S2(I2:I2) ) ) THEN
      EXIT
    END IF

  END DO

  S_BEGIN = .FALSE.

  RETURN
END

!*************************************************************

SUBROUTINE S_BEHEAD_SUBSTRING ( S, SUB )

!*****************************************************************************80
!
!! S_BEHEAD_SUBSTRING "beheads" a string, removing a given substring.
!
!  Discussion:
!
!    Initial blanks in the string are removed first.
!
!    Then, if the initial part of the string matches the substring,
!    that part is removed and the remainder shifted left.
!
!    Initial blanks in the substring are NOT ignored.
!
!    Capitalization is ignored.
!
!    If the substring is equal to the string, then the resultant
!    string is returned as a single blank.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 January 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character ( len = * ) SUB, the substring to be removed from
!    the beginning of the string.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB
  INTEGER ( KIND = 4 ) SUB_LENGTH
!
!  Remove leading blanks from the string.
!
  S = ADJUSTL ( S )
!
!  Get lengths.
!
  S_LENGTH = LEN_TRIM ( S )
  SUB_LENGTH = LEN_TRIM ( SUB )

  IF ( S_LENGTH < SUB_LENGTH ) THEN
    RETURN
  END IF
!
!  If the string begins with the substring, chop it off.
!
  IF ( S_EQI ( S(1:SUB_LENGTH), SUB(1:SUB_LENGTH) ) ) THEN

    IF ( SUB_LENGTH < S_LENGTH ) THEN
      S = S(SUB_LENGTH+1:S_LENGTH)
      S = ADJUSTL ( S )
    ELSE
      S = ' '
    END IF

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_BLANKS_DELETE ( S )

!*****************************************************************************80
!
!! S_BLANKS_DELETE replaces consecutive blanks by one blank.
!
!  Discussion:
!
!    Thanks to Bill Richmond for pointing out a programming flaw which
!    meant that, as characters were slid to the left through multiple
!    blanks, their original images were not blanked out.  This problem
!    is easiest resolved by using a copy of the string.
!
!    The remaining characters are left justified and right padded with blanks.
!    TAB characters are converted to spaces.
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  CHARACTER NEWCHR
  CHARACTER OLDCHR
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = LEN ( S ) ) S_COPY
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )

  S_LENGTH = LEN ( S )

  J = 0
  S_COPY(1:S_LENGTH) = S(1:S_LENGTH)
  S(1:S_LENGTH) = ' '

  NEWCHR = ' '

  DO I = 1, S_LENGTH

    OLDCHR = NEWCHR
    NEWCHR = S_COPY(I:I)

    IF ( NEWCHR == TAB ) THEN
      NEWCHR = ' '
    END IF

    IF ( OLDCHR /= ' ' .OR. NEWCHR /= ' ' ) THEN
      J = J + 1
      S(J:J) = NEWCHR
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_BLANKS_INSERT ( S, ILO, IHI )

!*****************************************************************************80
!
!! S_BLANKS_INSERT inserts blanks into a string, sliding old characters over.
!
!  Discussion:
!
!    Characters at the end of the string "drop off" and are lost.
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ( kind = 4 ) ILO, the location where the first blank
!    is to be inserted.
!
!    Input, integer ( kind = 4 ) IHI, the location where the last blank
!    is to be inserted.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IMAX
  INTEGER ( KIND = 4 ) IMIN
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) NMOVE
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( IHI < ILO .OR. S_LENGTH < ILO ) THEN
    RETURN
  END IF

  IF ( IHI <= S_LENGTH ) THEN
    IMAX = IHI
  ELSE
    IMAX = S_LENGTH
  END IF

  IF ( 1 <= ILO ) THEN
    IMIN = ILO
  ELSE
    IMIN = 1
  END IF

  NMOVE = S_LENGTH - IMAX

  DO I = 1, NMOVE
    PUT = S_LENGTH + 1 - I
    GET = S_LENGTH - IMAX + IMIN - I
    CH = S(GET:GET)
    S(PUT:PUT) = CH
  END DO

  DO I = IMIN, IMAX
    S(I:I) = ' '
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_BLANK_DELETE ( S )

!*****************************************************************************80
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!  Discussion:
!
!    All TAB characters are also removed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )

  PUT = 0
  S_LENGTH = LEN_TRIM ( S )

  DO GET = 1, S_LENGTH

    CH = S(GET:GET)

    IF ( CH /= ' ' .AND. CH /= TAB ) THEN
      PUT = PUT + 1
      S(PUT:PUT) = CH
    END IF

  END DO

  S(PUT+1:S_LENGTH) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_CAP ( S )

!*****************************************************************************80
!
!! S_CAP replaces any lowercase letters by uppercase ones in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    CH = S(I:I)
    CALL CH_CAP ( CH )
    S(I:I) = CH

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_CAT ( S1, S2, S3 )

!*****************************************************************************80
!
!! S_CAT concatenates two strings to make a third string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the "prefix" string.
!
!    Input, character ( len = * ) S2, the "postfix" string.
!
!    Output, character ( len = * ) S3, the string made by
!    concatenating S1 and S2, ignoring any trailing blanks.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = * ) S3

  IF ( S1 == ' ' .AND. S2 == ' ' ) THEN
    S3 = ' '
  ELSE IF ( S1 == ' ' ) THEN
    S3 = S2
  ELSE IF ( S2 == ' ' ) THEN
    S3 = S1
  ELSE
    S3 = TRIM ( S1 ) // TRIM ( S2 )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_CAT1 ( S1, S2, S3 )

!*****************************************************************************80
!
!! S_CAT1 concatenates two strings, with a single blank separator.
!
!  Example:
!
!    S1       S2       S
!
!    'cat'    'dog'    'cat dog'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the "prefix" string.
!
!    Input, character ( len = * ) S2, the "postfix" string.
!
!    Output, character ( len = * ) S3, the string made by concatenating
!    S1, a blank, and S2, ignoring any trailing blanks.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = * ) S3

  IF ( S1 == ' ' .AND. S2 == ' ' ) THEN
    S3 = ' '
  ELSE IF ( S1 == ' ' ) THEN
    S3 = S2
  ELSE IF ( S2 == ' ' ) THEN
    S3 = S1
  ELSE
    S3 = TRIM ( S1 ) // ' ' // TRIM ( S2 )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_CENTER ( S )

!*****************************************************************************80
!
!! S_CENTER centers the non-blank portion of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 October 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.  On input, a string to be
!    centered.  On output, the centered string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) L1
  INTEGER ( KIND = 4 ) L2
  INTEGER ( KIND = 4 ) N1
  INTEGER ( KIND = 4 ) N2
  INTEGER ( KIND = 4 ) N3
  CHARACTER ( LEN = * ) S
!
!  How much space is in the string?
!
  N1 = LEN ( S )
!
!  Shift the string flush left and find the last nonblank.
!
  S = ADJUSTL ( S )
  N2 = LEN_TRIM ( S )

  IF ( N2 <= 0 ) THEN
    RETURN
  END IF

  IF ( N2 == N1 .OR. N2 == N1 - 1 ) THEN
    RETURN
  END IF

  N3 = N1 - N2
  L1 = N3 / 2
  L2 = L1 + N2 + 1

  S(L1+1:L2-1) = S(1:N2)

  S(1:L1) = ' '
  S(L2:N1) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_CENTER_INSERT ( S1, S2 )

!*****************************************************************************80
!
!! S_CENTER_INSERT inserts one string into the center of another.
!
!  Discussion:
!
!    The receiving string is not blanked out first.  Therefore, if there is
!    already information in it, some of it may still be around
!    after the insertion.
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
!    Input, character ( len = * ) S1, a string to be inserted into S2.
!
!    Output, character ( len = * ) S2, the string to receive S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) JHI
  INTEGER ( KIND = 4 ) JLO
  INTEGER ( KIND = 4 ) M
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN_TRIM ( S1 )
  S2_LENGTH = LEN ( S2 )

  IF ( S1_LENGTH < S2_LENGTH ) THEN

    M = S2_LENGTH - S1_LENGTH
    ILO = 1
    IHI = S1_LENGTH
    JLO = ( M / 2 ) + 1
    JHI = JLO + S1_LENGTH - 1

  ELSE IF ( S2_LENGTH < S1_LENGTH ) THEN

    M = S1_LENGTH - S2_LENGTH
    ILO = ( M / 2 ) + 1
    IHI = ILO + S2_LENGTH - 1
    JLO = 1
    JHI = S2_LENGTH

  ELSE

    ILO = 1
    IHI = S1_LENGTH
    JLO = 1
    JHI = S2_LENGTH

  END IF

  S2(JLO:JHI) = S1(ILO:IHI)

  RETURN
END

!*************************************************************

SUBROUTINE S_CHOP ( S, ILO, IHI )

!*****************************************************************************80
!
!! S_CHOP "chops out" a portion of a string, and closes up the hole.
!
!  Example:
!
!    S = 'Fred is not a jerk!'
!
!    call s_chop ( S, 9, 12 )
!
!    S = 'Fred is a jerk!    '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ( kind = 4 ) ILO, IHI, the locations of the first and last
!    characters to be removed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) IHI2
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) ILO2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  ILO2 = MAX ( ILO, 1 )
  IHI2 = MIN ( IHI, S_LENGTH )

  IF ( IHI2 < ILO2 ) THEN
    RETURN
  END IF

  S(ILO2:S_LENGTH+ILO2-IHI2-1) = S(IHI2+1:S_LENGTH)
  S(S_LENGTH+ILO2-IHI2:S_LENGTH) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_CH_BLANK ( S, CH )

!*****************************************************************************80
!
!! S_CH_BLANK replaces each occurrence of a particular character by a blank.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character CH, the character to be removed.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( S(I:I) == CH ) THEN
      S(I:I) = ' '
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_CH_COUNT ( S, CH, CH_COUNT )

!*****************************************************************************80
!
!! S_CH_COUNT counts occurrences of a particular character in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string.
!
!    Input, character CH, the character to be counted.
!
!    Output, integer ( kind = 4 ) CH_COUNT, the number of occurrences.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_COUNT
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  CH_COUNT = 0

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH

    IF ( S(I:I) == CH ) THEN
      CH_COUNT = CH_COUNT + 1
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_CH_DELETE ( S, CH )

!*****************************************************************************80
!
!! S_CH_DELETE removes all occurrences of a character from a string.
!
!  Discussion:
!
!    Each time the given character is found in the string, the characters
!    to the right of the string are shifted over one position.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character CH, the character to be removed.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  PUT = 1

  DO GET = 1, S_LENGTH

    IF ( S(GET:GET) == CH ) THEN

    ELSE IF ( PUT == GET ) THEN
      PUT = PUT + 1
    ELSE
      S(PUT:PUT) = S(GET:GET)
      PUT = PUT + 1
    END IF

  END DO

  S(PUT:S_LENGTH) = ' '

  RETURN
END

!*************************************************************

FUNCTION S_CH_LAST ( S )

!*****************************************************************************80
!
!! S_CH_LAST returns the last nonblank character in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, character S_CH_LAST, the last nonblank character in S,
!    or ' ' if S is all blank.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S
  CHARACTER S_CH_LAST
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  IF ( 0 < S_LENGTH ) THEN
    S_CH_LAST = S(S_LENGTH:S_LENGTH)
  ELSE
    S_CH_LAST = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_COMPARE ( S1, S2, ORDER )

!*****************************************************************************80
!
!! S_COMPARE compares two strings.
!
!  Discussion:
!
!    The FORTRAN function LLT ( S1, S2 ) returns TRUE if S1 is lexically
!    strictly less than S2, and FALSE otherwise.
!
!    There are related functions LLE, LGT, LGE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2010
!
!  Author:
!
!   John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, integer ( kind = 4 ) ORDER:
!    -1, S1 < S2.
!     0, S1 = S2
!    +1, S1 > S2
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ORDER
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LEN
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LEN

  S1_LEN = S_LEN_TRIM ( S1 )
  S2_LEN = S_LEN_TRIM ( S2 )

  ORDER = 0

  DO I = 1, MIN ( S1_LEN, S2_LEN )
    IF ( S1(I:I) < S2(I:I) ) THEN
      ORDER = -1
      RETURN
    ELSE IF ( S2(I:I) < S1(I:I) ) THEN
      ORDER = +1
      RETURN
    END IF

  END DO
!
!  If one string is actually longer than the other, and nonblank,
!  it must come after the other.
!
  IF ( S1_LEN < S2_LEN ) THEN
    ORDER = -1
    RETURN
  ELSE IF ( S2_LEN < S1_LEN ) THEN
    ORDER = +1
    RETURN
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_CONTROL_BLANK ( S )

!*****************************************************************************80
!
!! S_CONTROL_BLANK replaces control characters with blanks.
!
!  Discussion:
!
!    A "control character" has ASCII code <= 31 or 127 <= ASCII code.
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( CH_IS_CONTROL ( S(I:I) ) ) THEN
      S(I:I) = ' '
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_CONTROL_COUNT ( S, IFOUND )

!*****************************************************************************80
!
!! S_CONTROL_COUNT returns the number of control characters in a string.
!
!  Discussion:
!
!    A "control character" has ASCII code <= 31 or 127 <= ASCII code.
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
!    Input, character ( len = * ) S, the string to be searched.
!
!    Output, integer ( kind = 4 ) IFOUND, the number of control characters.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IFOUND
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  IFOUND = 0
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( CH_IS_CONTROL ( S(I:I) ) ) THEN
      IFOUND = IFOUND + 1
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_CONTROL_DELETE ( S )

!*****************************************************************************80
!
!! S_CONTROL_DELETE removes all control characters from a string.
!
!  Discussion:
!
!    The string is collapsed to the left, and padded on the right with
!    blanks to replace the removed characters.
!
!    A "control character" has ASCII code <= 31 or 127 <= ASCII code.
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
!    Input/output, character ( len = * ) S, is the string to be transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  PUT = 0
  S_LENGTH = LEN_TRIM ( S )

  DO GET = 1, S_LENGTH

    IF ( .NOT. CH_IS_CONTROL ( S(GET:GET) ) ) THEN
      PUT = PUT + 1
      S(PUT:PUT) = S(GET:GET)
    END IF

  END DO
!
!  Pad the end of the string with blanks
!
  S(PUT+1:) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_COPY ( S1, S2 )

!*****************************************************************************80
!
!! S_COPY copies one string into another.
!
!  Discussion:
!
!    If S1 is shorter than S2, the rest of S2 is blank.
!    If S1 is longer than S2, then the excess information is lost.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be copied.
!
!    Output, character ( len = * ) S2, the copy.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2

  S2(1:MIN(LEN(S1),LEN(S2))) = S1(1:MIN(LEN(S1),LEN(S2)))
  S2(LEN(S1)+1:LEN(S2)) = ' '

  RETURN
END

!*************************************************************

SUBROUTINE S_DETAG ( S )

!*****************************************************************************80
!
!! S_DETAG removes from a string all substrings marked by angle brackets.
!
!  Example:
!
!    Input:
!
!      S = '<I>This is Italic</I> whereas this is <B>boldly</B> not!'
!
!    Output:
!
!      S = ' whereas this is  not!'
!
!  Discussion:
!
!    This routine was written to help extract some data that was hidden
!    inside an elaborate HTML table.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  DO

    S_LENGTH = LEN_TRIM ( S )

    IF ( LEN_TRIM ( S ) == 0 ) THEN
      EXIT
    END IF

    I1 = INDEX ( S, '<' )

    IF ( I1 <= 0 .OR. S_LENGTH <= I1 ) THEN
      EXIT
    END IF

    I2 = INDEX ( S(I1+1:), '>' )

    IF ( I2 == 0 ) THEN
      EXIT
    END IF

    I2 = I2 + I1
!
!  Shift.
!
    S(I1:S_LENGTH+I1-I2-1) = S(I2+1:S_LENGTH)
!
!  Pad.
!
    S(S_LENGTH+I1-I2:) = ' '

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_DETROFF ( S )

!*****************************************************************************80
!
!! S_DETROFF removes obnoxious "character" + backspace pairs from a string.
!
!  Discussion:
!
!    Given the string of characters:
!      'AB#C#D#E'
!    where we are using "#" to represent a backspace, the returned string
!    will be
!      'AE'.
!
!    This function was written for use in "cleaning up" UNICOS MAN pages.
!    These MAN pages were formatted in the Byzantine TROFF printing format.
!    Although the files were text, and would seem to "print" correctly to
!    the screen, an unholy mess would emerge if the same file was sent
!    to the printer.  This is because the screen handled the backspaces
!    by backspacing, but most printers don't know anymore how to handle
!    TROFF's backspaces, and so they just print them as blobs, instead of,
!    say, spacing back.
!
!    In particular:
!
!      Passages which are to be underlined are written so:
!      "_#T_#e_#x_#t" when what is meant is that "Text" is to be
!      underlined if possible.  Note that the seemingly equivalent
!      "T#_e#_x#_t#_" is NOT used.  This is because, in the olden
!      days, certain screen terminals could backspace, but would only
!      display the new character, obliterating rather than
!      overwriting the old one.  This convention allows us to know
!      that we want to delete "character" + Backspace, rather than
!      Backspace + "character".
!
!      Passages which are meant to be in BOLDFACE are written so:
!      "U#U#U#Ug#g#g#gl#l#l#ly#y#y#y", when what is meant is that
!      "Ugly" is to be printed as boldly as possible.  These boldface
!      passages may also be cleaned up using the same rule of
!      removing all occurrences of "character" + Backspace.
!
!    It is truly a fright to look at the text of one of these MAN
!    pages with all the ugly Backspace's, which display on VMS as ^H.
!    These files print or type properly, but look awful in an editor.
!    Moreoever, the lavish use of boldface means that text that is
!    meant to fit in 80 columns can sometimes require 7 times as much
!    space to describe.  This can cause a VMS editor to abort, or to
!    skip the line, since 255 characters is the maximum for EDT.
!
!    A FORTRAN program that tries to read a long line like that will
!    also fail if not careful, since a formatted sequential file
!    on VMS has a default maximum record length of something like
!    133 characters.
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
!    Input/output, character ( len = * ) S, the line of text to
!    be de-TROFF'ed.
!
  IMPLICIT NONE

  CHARACTER, PARAMETER :: BS = ACHAR ( 8 )
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )
  I = 1

  DO WHILE ( I <= S_LENGTH )

    IF ( S(I:I) == BS ) THEN

      IF ( I == 1 ) THEN
        S(1:S_LENGTH-1) = S(2:S_LENGTH)
        S(S_LENGTH:S_LENGTH) = ' '
        S_LENGTH = S_LENGTH - 1
        I = I - 1
      ELSE
        S(I-1:S_LENGTH-2) = S(I+1:S_LENGTH)
        S(S_LENGTH-1:S_LENGTH) = ' '
        S_LENGTH = S_LENGTH - 2
        I = I - 2
      END IF

    END IF

    I = I + 1

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_EQI ( S1, S2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!  Discussion:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_EQI
  CHARACTER ( LEN = *  ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = *  ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN ( S1 )
  S2_LENGTH = LEN ( S2 )
  LENC = MIN ( S1_LENGTH, S2_LENGTH )

  S_EQI = .FALSE.

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( C1 /= C2 ) THEN
      RETURN
    END IF

  END DO

  DO I = LENC + 1, S1_LENGTH
    IF ( S1(I:I) /= ' ' ) THEN
      RETURN
    END IF
  END DO

  DO I = LENC + 1, S2_LENGTH
    IF ( S2(I:I) /= ' ' ) THEN
      RETURN
    END IF
  END DO

  S_EQI = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_EQIDB ( S1, S2 )

!*****************************************************************************80
!
!! S_EQIDB compares two strings, ignoring case and blanks.
!
!  Example:
!
!    S_EQIDB ( 'Nor Way', 'NORway' ) is TRUE.
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
!  Modified:
!
!    19 July 1998
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQIDB, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  INTEGER ( KIND = 4 ) LEN2
  LOGICAL S_EQIDB
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
!
!  Get the length of each string to the last nonblank.
!
  S1_LENGTH = LEN_TRIM ( S1 )
  LEN2 = LEN_TRIM ( S2 )
!
!  Assume we're going to fail.
!
  S_EQIDB = .FALSE.
!
!  Initialize the pointers to characters in each string.
!
  I1 = 0
  I2 = 0

  DO
!
!  If we've matched all the nonblank characters in both strings,
!  then return with S_EQIDB = TRUE.
!
    IF ( I1 == S1_LENGTH .AND. I2 == LEN2 ) THEN
      S_EQIDB = .TRUE.
      RETURN
    END IF
!
!  Get the next nonblank character in the first string.
!
    DO

      I1 = I1 + 1

      IF ( S1_LENGTH < I1 ) THEN
        RETURN
      END IF

      IF ( S1(I1:I1) /= ' ' ) THEN
        EXIT
      END IF

    END DO

    C1 = S1(I1:I1)
    CALL CH_CAP ( C1 )
!
!  Get the next nonblank character in the second string.
!
    DO

      I2 = I2 + 1
      IF ( LEN2 < I2 ) THEN
        RETURN
      END IF

      C2 = S2(I2:I2)

      IF ( C2 /= ' ' ) THEN
        EXIT
      END IF

    END DO

    CALL CH_CAP ( C2 )

    IF ( C1 /= C2 ) THEN
      EXIT
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_ESCAPE_TEX ( S1, S2 )

!*****************************************************************************80
!
!! S_ESCAPE_TEX de-escapes TeX escape sequences.
!
!  Discussion:
!
!    In particular, every occurrence of the characters '\', '_',
!    '^', '{' and '}' will be replaced by '\\', '\_', '\^',
!    '\{' and '\}'.  A TeX interpreter, on seeing these character
!    strings, is then likely to return the original characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be de-escaped.
!
!    Output, character ( len = * ) S2, a copy of the string,
!    modified to avoid TeX escapes.
!
  IMPLICIT NONE

  CHARACTER CH
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  INTEGER ( KIND = 4 ) S1_POS
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_POS

  S1_LENGTH = LEN_TRIM ( S1 )

  S1_POS = 0
  S2_POS = 0
  S2 = ' '

  DO WHILE ( S1_POS < S1_LENGTH )

    S1_POS = S1_POS + 1
    CH = S1(S1_POS:S1_POS)

    IF ( CH == '\' .OR. &
         CH == '_' .OR. &
         CH == '^' .OR. &
         CH == '{' .OR. &
         CH == '}' ) THEN

      S2_POS = S2_POS + 1
      S2(S2_POS:S2_POS) = '\'

    END IF

    S2_POS = S2_POS + 1
    S2(S2_POS:S2_POS) = CH

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_FILL ( S, CH )

!*****************************************************************************80
!
!! S_FILL overwrites every character of a string by a given character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character ( len = * ) S, the string to be overwritten.
!
!    Input, character CH, the overwriting character.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH
    S(I:I) = CH
  END DO

  RETURN
END

!*************************************************************

FUNCTION S_FIRST_NONBLANK ( S )

!*****************************************************************************80
!
!! S_FIRST_NONBLANK returns the location of the first nonblank.
!
!  Discussion:
!
!    If all characters are blanks, a 0 is returned.
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) S_FIRST_NONBLANK, the location of the first
!    nonblank character in the string, or 0 if all are blank.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_FIRST_NONBLANK
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH

    IF ( S(I:I) /= ' ' ) THEN
      S_FIRST_NONBLANK = I
      RETURN
    END IF

  END DO

  S_FIRST_NONBLANK = 0

  RETURN
END

!*************************************************************

FUNCTION S_GEI ( S1, S2 )

!*****************************************************************************80
!
!! S_GEI = ( S1 is lexically greater than or equal to S2 ).
!
!  Discussion:
!
!    The comparison is done in a case-insensitive way.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_GEI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_GEI
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN_TRIM ( S1 )
  S2_LENGTH = LEN_TRIM ( S2 )
  LENC = MIN ( S1_LENGTH, S2_LENGTH )

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( LGT ( C1, C2 ) ) THEN
      S_GEI = .TRUE.
      RETURN
    ELSE IF ( LLT ( C1, C2 ) ) THEN
      S_GEI = .FALSE.
      RETURN
    END IF

  END DO

  IF ( S1_LENGTH < S2_LENGTH ) THEN
    S_GEI = .FALSE.
  ELSE
    S_GEI = .TRUE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION S_GTI ( S1, S2 )

!*****************************************************************************80
!
!! S_GTI = S1 is lexically greater than S2.
!
!  Discussion:
!
!    The comparison is done in a case-insensitive way.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_GTI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_GTI
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN ( S1 )
  S2_LENGTH = LEN ( S2 )
  LENC = MIN ( S1_LENGTH, S2_LENGTH )

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( LGT ( C1, C2 ) ) THEN
      S_GTI = .TRUE.
      RETURN
    ELSE IF ( LLT ( S1, S2 ) ) THEN
      S_GTI = .FALSE.
      RETURN
    END IF

  END DO

  IF ( S1_LENGTH <= S2_LENGTH ) THEN
    S_GTI = .FALSE.
  ELSE
    S_GTI = .TRUE.
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_INC_C ( S )

!*****************************************************************************80
!
!! S_INC_C "increments" the characters in a string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    The routine tries to produce the next string, in dictionary order,
!    following the input value of a string.  Digits, spaces, and other
!    nonalphabetic characters are ignored.  Case is respected; in other
!    words, the case of every alphabetic character on input will be the
!    same on output.
!
!    The following error conditions can occur:
!
!      There are no alphabetic characters in the string.  No
!      incrementing is possible.
!
!      All alphabetic characters are equal to 'Z' or 'z'.  In this
!      the string is also "wrapped around" so that all alphabetic
!      characters are "A" or "a".
!
!    If the word "Tax" were input, the successive outputs would be
!    "Tay", "Taz", "Tba", "Tbb", ...  If the input word "January 4, 1989"
!    were input, the output would be "Januarz 4, 1989".
!
!    This routine could be useful when trying to create a unique file
!    name or variable name at run time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string whose
!    alphabetic successor is desired.  On output, S has been replaced
!    by its "successor".
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) ILOC
  CHARACTER ( LEN = * ) S

  ILO = 1
  IHI = LEN ( S )
!
!  Find the last alphabetic character in the string.
!
  DO

    CALL S_ALPHA_LAST ( S(ILO:IHI), ILOC )
!
!  If there is no alphabetic character, we can't help.
!
    IF ( ILOC == 0 ) THEN
      RETURN
    END IF

    IF ( S(ILOC:ILOC) == ACHAR ( 122 ) ) THEN

      S(ILOC:ILOC) = ACHAR ( 97 )
      IHI = ILOC - 1

      IF ( IHI <= 0 ) THEN
        EXIT
      END IF

    ELSE IF ( S(ILOC:ILOC) == ACHAR ( 90 ) ) THEN

      S(ILOC:ILOC) = ACHAR ( 65 )
      IHI = ILOC - 1

      IF ( IHI <= 0 ) THEN
        RETURN
      END IF

    ELSE

      S(ILOC:ILOC) = ACHAR ( IACHAR ( S(ILOC:ILOC) ) + 1 )
      EXIT

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_INC_N ( S )

!*****************************************************************************80
!
!! S_INC_N increments the digits in a string.
!
!  Discussion:
!
!    Instead of CHAR and ICHAR, we now use the ACHAR and IACHAR functions, which
!    guarantees the ASCII collating sequence.
!
!    It is assumed that the digits in the name, whether scattered or
!    connected, represent a number that is to be increased by 1 on
!    each call.  If this number is all 9's on input, the output number
!    is all 0's.  Non-numeric letters of the name are unaffected.
!
!    If the name is empty, then the routine stops.
!
!    If the name contains no digits, the empty string is returned.
!
!  Example:
!
!      Input            Output
!      -----            ------
!      'a7to11.txt'     'a7to12.txt'
!      'a7to99.txt'     'a8to00.txt'
!      'a9to99.txt'     'a0to00.txt'
!      'cat.txt'        ' '
!      ' '              STOP!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 September 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.
!    On input, a character string to be incremented.
!    On output, the incremented string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) CHANGE
  INTEGER ( KIND = 4 ) DIGIT
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_INC_N - Fatal error!'
    WRITE ( *, '(a)' ) '  The input string is empty.'
    RETURN
  END IF

  CHANGE = 0

  DO I = S_LENGTH, 1, -1

    C = S(I:I)

    IF ( LGE ( C, '0' ) .AND. LLE ( C, '9' ) ) THEN

      CHANGE = CHANGE + 1

      DIGIT = IACHAR ( C ) - 48
      DIGIT = DIGIT + 1

      IF ( DIGIT == 10 ) THEN
        DIGIT = 0
      END IF

      C = ACHAR ( DIGIT + 48 )

      S(I:I) = C

      IF ( C /= '0' ) THEN
        RETURN
      END IF

    END IF

  END DO

  IF ( CHANGE == 0 ) THEN
    S = ' '
    RETURN
  END IF

  RETURN
END

!*************************************************************

FUNCTION S_INDEX ( S, SUB )

!*****************************************************************************80
!
!! S_INDEX seeks the first occurrence of a substring.
!
!  Discussion:
!
!    The function returns the location in the string at which the
!    substring SUB is first found, or 0 if the substring does not
!    occur at all.
!
!    The routine is trailing blank insensitive.  This is very
!    important for those cases where you have stored information in
!    larger variables.  If S is of length 80, and SUB is of
!    length 80, then if S = 'FRED' and SUB = 'RED', a match would
!    not be reported by the standard FORTRAN INDEX, because it treats
!    both variables as being 80 characters long!  This routine assumes that
!    trailing blanks represent garbage!
!
!    Because of the suppression of trailing blanks, this routine cannot be
!    used to find, say, the first occurrence of the two-character
!    string 'A '.  However, this routine treats as a special case the
!    occurrence where S or SUB is entirely blank.  Thus you can
!    use this routine to search for occurrences of double or triple blanks
!    in a string, for example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character ( len = * ) SUB, the substring to search for.
!
!    Output, integer ( kind = 4 ) S_INDEX.  0 if SUB does not occur in
!    the string.  Otherwise S(S_INDEX:S_INDEX+LENS-1) = SUB,
!    where LENS is the length of SUB, and is the first place
!    this happens.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_INDEX
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB
  INTEGER ( KIND = 4 ) SUB_LENGTH

  S_INDEX = 0

  S_LENGTH = LEN_TRIM ( S )
  SUB_LENGTH = LEN_TRIM ( SUB )
!
!  In case S or SUB is blanks, use LEN.
!
  IF ( S_LENGTH == 0 ) THEN
    S_LENGTH = LEN ( S )
  END IF

  IF ( SUB_LENGTH == 0 ) THEN
    SUB_LENGTH = LEN ( SUB )
  END IF

  IF ( S_LENGTH < SUB_LENGTH ) THEN
    RETURN
  END IF

  DO I = 1, S_LENGTH + 1 - SUB_LENGTH

    IF ( S(I:I+SUB_LENGTH-1) == SUB(1:SUB_LENGTH) ) THEN
      S_INDEX = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_INDEXI ( S, SUB )

!*****************************************************************************80
!
!! S_INDEXI is a case-insensitive INDEX function.
!
!  Discussion:
!
!    The function returns the location in the string at which the
!    substring SUB is first found, or 0 if the substring does not
!    occur at all.
!
!    The routine is also trailing blank insensitive.  This is very
!    important for those cases where you have stored information in
!    larger variables.  If S is of length 80, and SUB is of
!    length 80, then if S = 'FRED' and SUB = 'RED', a match would
!    not be reported by the standard FORTRAN INDEX, because it treats
!    both variables as being 80 characters long!  This routine assumes that
!    trailing blanks represent garbage!
!
!    Because of the suppression of trailing blanks, this routine cannot be
!    used to find, say, the first occurrence of the two-character
!    string 'A '.  However, this routine treats as a special case the
!    occurrence where S or SUB is entirely blank.  Thus you can
!    use this routine to search for occurrences of double or triple blanks
!    in a string, for example, although INDEX itself would be just as
!    suitable for that problem.
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
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character ( len = * ) SUB, the substring to search for.
!
!    Output, integer ( kind = 4 ) S_INDEXI.  0 if SUB does not occur in
!    the string.  Otherwise S(S_INDEXI:S_INDEXI+LENS-1) = SUB,
!    where LENS is the length of SUB, and is the first place
!    this happens.  However, note that this routine ignores case,
!    unlike the standard FORTRAN INDEX function.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LLEN2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_INDEXI
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB

  S_INDEXI = 0

  S_LENGTH = LEN_TRIM ( S )
  LLEN2 = LEN_TRIM ( SUB )
!
!  In case S or SUB is blanks, use LEN.
!
  IF ( S_LENGTH == 0 ) THEN
    S_LENGTH = LEN ( S )
  END IF

  IF ( LLEN2 == 0 ) THEN
    LLEN2 = LEN ( SUB )
  END IF

  IF ( S_LENGTH < LLEN2 ) THEN
    RETURN
  END IF

  DO I = 1, S_LENGTH + 1 - LLEN2

    IF ( S_EQI ( S(I:I+LLEN2-1), SUB ) ) THEN
      S_INDEXI = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_INDEX_LAST ( S, SUB )

!*****************************************************************************80
!
!! S_INDEX_LAST finds the LAST occurrence of a given substring.
!
!  Discussion:
!
!    It returns the location in the string at which the substring SUB is
!    first found, or 0 if the substring does not occur at all.
!
!    The routine is also trailing blank insensitive.  This is very
!    important for those cases where you have stored information in
!    larger variables.  If S is of length 80, and SUB is of
!    length 80, then if S = 'FRED' and SUB = 'RED', a match would
!    not be reported by the standard FORTRAN INDEX, because it treats
!    both variables as being 80 characters long!  This routine assumes that
!    trailing blanks represent garbage!
!
!    This means that this routine cannot be used to find, say, the last
!    occurrence of a substring 'A ', since it assumes the blank space
!    was not specified by the user, but is, rather, padding by the
!    system.  However, as a special case, this routine can properly handle
!    the case where either S or SUB is all blanks.
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
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character ( len = * ) SUB, the substring to search for.
!
!    Output, integer ( kind = 4 ) S_INDEX_LAST.  0 if SUB does not occur in
!    the string.  Otherwise S_INDEX_LAST = I, where S(I:I+LENS-1) = SUB,
!    where LENS is the length of SUB, and is the last place
!    this happens.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) LLEN2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_INDEX_LAST
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB

  S_INDEX_LAST = 0

  S_LENGTH = LEN_TRIM ( S )
  LLEN2 = LEN_TRIM ( SUB )
!
!  In case S or SUB is blanks, use LEN.
!
  IF ( S_LENGTH == 0 ) THEN
    S_LENGTH = LEN ( S )
  END IF

  IF ( LLEN2 == 0 ) THEN
    LLEN2 = LEN ( SUB )
  END IF

  IF ( S_LENGTH < LLEN2 ) THEN
    RETURN
  END IF

  DO J = 1, S_LENGTH + 1 - LLEN2

    I = S_LENGTH + 2 - LLEN2 - J

    IF ( S(I:I+LLEN2-1) == SUB ) THEN
      S_INDEX_LAST = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_INDEX_LAST_C ( S, C )

!*****************************************************************************80
!
!! S_INDEX_LAST_C finds the LAST occurrence of a given character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 December 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, character C, the character to search for.
!
!    Output, integer ( kind = 4 ) S_INDEX_LAST_C, the index in S where C occurs
!    last, or -1 if it does not occur.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) S_INDEX_LAST_C

  IF ( C == ' ' ) THEN
    S_LENGTH = LEN ( S )
  ELSE
    S_LENGTH = LEN_TRIM ( S )
  END IF

  DO I = S_LENGTH, 1, -1

    IF ( S(I:I) == C ) THEN
      S_INDEX_LAST_C = I
      RETURN
    END IF

  END DO

  S_INDEX_LAST_C = -1

  RETURN
END

!*************************************************************

FUNCTION S_INDEX_SET ( S1, S2 )

!*****************************************************************************80
!
!! S_INDEX_SET searches a string for any of a set of characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be examined.
!
!    Input, character ( len = * ) S2, the characters to search for.
!
!    Output, integer ( kind = 4 ) S_INDEX_SET, the first location of a
!    character from S2 in S1, or 0 if no character from S2 occurs in S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) K
  CHARACTER ( LEN = *  ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = *  ) S2
  INTEGER ( KIND = 4 ) S_INDEX_SET

  S1_LENGTH = LEN ( S1 )

  J = S1_LENGTH + 1

  DO I = 1, LEN ( S2 )
    K = INDEX ( S1, S2(I:I) )
    IF ( K /= 0 ) THEN
      J = MIN ( J, K )
    END IF
  END DO

  IF ( J == S1_LENGTH + 1 ) THEN
    J = 0
  END IF

  S_INDEX_SET = J

  RETURN
END

!*************************************************************

SUBROUTINE S_INPUT ( STRING, VALUE, IERROR )

!*****************************************************************************80
!
!! S_INPUT prints a prompt string and reads a string from the user.
!
!  Discussion:
!
!    If the input line starts with a comment character ('#'), or is blank,
!    the routine ignores that line, and tries to read the next one.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) STRING, the prompt string.
!
!    Output, character ( len = * ) VALUE, the value input by the user.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag, which is 0
!    if no error occurred.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  CHARACTER ( LEN = * ) STRING
  CHARACTER ( LEN = * ) VALUE

  IERROR = 0
  VALUE = ' '
!
!  Write the prompt.
!
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) TRIM ( STRING )

  DO

    READ ( *, '(a)', IOSTAT = IERROR ) VALUE

    IF ( IERROR /= 0 ) THEN
      VALUE = 'S_INPUT: Input error!'
      RETURN
    END IF
!
!  If the line begins with a comment character, go back and read the next line.
!
    IF ( VALUE(1:1) == '#' ) THEN
      CYCLE
    END IF

    IF ( LEN_TRIM ( VALUE ) == 0 ) THEN
      CYCLE
    END IF

    EXIT

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_IS_ALPHA ( S )

!*****************************************************************************80
!
!! S_IS_ALPHA returns TRUE if the string contains only alphabetic characters.
!
!  Discussion:
!
!    Here, alphabetic characters are 'A' through 'Z' and 'a' through 'z'.
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
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_ALPHA, is TRUE if the string contains only
!    alphabetic characters.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_ALPHA
  INTEGER ( KIND = 4 ) S_LENGTH

  S_IS_ALPHA = .FALSE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( .NOT. CH_IS_ALPHA ( S(I:I) ) ) THEN
      RETURN
    END IF

  END DO

  S_IS_ALPHA = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_IS_ALPHANUMERIC ( S )

!*****************************************************************************80
!
!! S_IS_ALPHANUMERIC = string contains only alphanumeric characters.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    Alphanumeric characters are 'A' through 'Z', 'a' through 'z' and
!    '0' through '9'.
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
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_ALPHANUMERIC, is TRUE if the string contains only
!    alphabetic characters and numerals.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ITEMP
  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_ALPHANUMERIC
  INTEGER ( KIND = 4 ) S_LENGTH

  S_IS_ALPHANUMERIC = .FALSE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    ITEMP = IACHAR ( S(I:I) )

    IF ( .NOT. ( 65 <= ITEMP .AND. ITEMP <= 90 ) ) THEN
      IF ( .NOT. ( 97 <= ITEMP .AND. ITEMP <= 122 ) ) THEN
        IF ( .NOT. ( 48 <= ITEMP .AND. ITEMP <= 57 ) ) THEN
          RETURN
        END IF
      END IF
    END IF

  END DO

  S_IS_ALPHANUMERIC = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_IS_DIGIT ( S )

!*****************************************************************************80
!
!! S_IS_DIGIT returns TRUE if a string contains only decimal digits.
!
!  Discussion:
!
!    This is a strict comparison.
!    The check is made from the first character to the last nonblank.
!    Each character in between must be one of '0', '1', ..., '9'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_DIGIT, is TRUE if S contains only digits.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_DIGIT
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  S_IS_DIGIT = .FALSE.

  DO I = 1, S_LENGTH
    C = S(I:I)
    IF ( LLT ( C, '0' ) .OR. LGT ( C, '9' ) ) THEN
      RETURN
    END IF
  END DO

  S_IS_DIGIT = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_IS_F77_NAME ( S )

!*****************************************************************************80
!
!! S_IS_F77_NAME = input string represent a legal FORTRAN77 identifier.
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, logical S_IS_F77_NAME, is TRUE if the string is a legal FORTRAN77
!    identifier.  That is, the string must begin with an alphabetic
!    character, and all subsequent characters must be alphanumeric.
!    The string may terminate with blanks.  No underscores are allowed.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_F77_NAME
  INTEGER ( KIND = 4 ) S_LENGTH

  S_IS_F77_NAME = .FALSE.

  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 0 ) THEN
    RETURN
  END IF

  IF ( .NOT. CH_IS_ALPHA ( S(1:1) ) ) THEN
    RETURN
  END IF

  IF ( .NOT. S_IS_ALPHANUMERIC ( S(2:S_LENGTH) ) ) THEN
    RETURN
  END IF

  S_IS_F77_NAME = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_IS_F90_NAME ( S )

!*****************************************************************************80
!
!! S_IS_F90_NAME = input string represent a legal FORTRAN90 identifier.
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
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, logical S_IS_F90_NAME, is TRUE if the string is a legal
!    FORTRAN90 identifier.  That is, the string must begin with an alphabetic
!    character, and all subsequent characters must be alphanumeric
!    or underscores.  The string may terminate with blanks.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_F90_NAME
  INTEGER ( KIND = 4 ) S_LENGTH

  S_IS_F90_NAME = .FALSE.

  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH <= 0 ) THEN
    RETURN
  END IF

  IF ( .NOT. CH_IS_ALPHA ( S(1:1) ) ) THEN
    RETURN
  END IF

  DO I = 2, S_LENGTH
    IF ( .NOT. MALPHNUM2 ( S(I:I) ) ) THEN
      RETURN
    END IF
  END DO

  S_IS_F90_NAME = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_IS_I ( S, I )

!*****************************************************************************80
!
!! S_IS_I is TRUE if a string represents an integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, integer ( kind = 4 ) I.  If the string represents an integer,
!    I is the integer represented.  Otherwise I is 0.
!
!    Output, logical S_IS_I, is TRUE if the string represents an integer.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
  LOGICAL S_IS_I
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  CALL S_TO_I4 ( S, I, IERROR, LENGTH )

  IF ( IERROR == 0 .AND. S_LENGTH <= LENGTH ) THEN
    S_IS_I = .TRUE.
  ELSE
    S_IS_I = .FALSE.
    I = 0
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_IS_R ( S, R, LVAL )

!*****************************************************************************80
!
!! S_IS_R is TRUE if a string represents a real number.
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
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, real ( kind = 4 ) R.  If the string represents a real number,
!    then R is the real number represented.  Otherwise R is 0.
!
!    Output, logical LVAL, is TRUE if the string represents a real number.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  LOGICAL LVAL
  REAL ( KIND = 4 ) R
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  CALL S_TO_R4 ( S, R, IERROR, LENGTH )

  IF ( IERROR == 0 .AND. S_LENGTH <= LENGTH ) THEN
    LVAL = .TRUE.
  ELSE
    LVAL = .FALSE.
    R = 0.0E+00
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_I_APPEND ( S, I, DONE )

!*****************************************************************************80
!
!! S_I_APPEND appends an integer to a string.
!
!  Discussion:
!
!    A blank space will separate the integer from the text already
!    in the line.
!
!    The routine warns the user if the integer will not fit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a line of text.
!    On input, the current string.  On output, the current string
!    with the integer value appended.
!
!    Input, integer ( kind = 4 ) I, an integer to be appended to the line.
!
!    Output, logical DONE, is FALSE if there was not enough room
!    to append the integer.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENTS
  INTEGER ( KIND = 4 ) LENW
  INTEGER ( KIND = 4 ) NEXT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = 13 ) W

  DONE = .FALSE.

  S_LENGTH = LEN ( S )
  LENTS = LEN_TRIM ( S )

  CALL I4_TO_S_LEFT ( I, W )

  LENW = LEN_TRIM ( W )

  IF ( LENTS == 0 ) THEN
    IF ( S_LENGTH < LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  ELSE
    IF ( S_LENGTH < LENTS + 1 + LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  END IF

  IF ( LENTS == 0 ) THEN
    NEXT = 1
  ELSE
    NEXT = LENTS + 1
    S(NEXT:NEXT) = ' '
    NEXT = NEXT + 1
  END IF

  S(NEXT:NEXT+LENW-1) = W(1:LENW)

  RETURN
END

!*************************************************************

SUBROUTINE S_LEFT_INSERT ( S1, S2 )

!*****************************************************************************80
!
!! S_LEFT_INSERT inserts one string flush left into another.
!
!  Discussion:
!
!    S2 is not blanked out first.  Therefore, if there is
!    already information in S2, some of it may still be around
!    after S1 is written into S2.  Users may want to first
!    assign S2 = ' ' if this is not the effect desired.
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
!    Input, character ( len = * ) S1, a string to be inserted into S2.  Only
!    the portion of S1 up to the last nonblank will be used.
!
!    Output, character ( len = * ) S2, a string which will contain,
!    on output, a left flush copy of S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) JHI
  INTEGER ( KIND = 4 ) JLO
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN_TRIM ( S1 )
  S2_LENGTH = LEN ( S2 )

  IF ( S1_LENGTH < S2_LENGTH ) THEN
    ILO = 1
    IHI = S1_LENGTH
    JLO = 1
    JHI = S1_LENGTH
  ELSE IF ( S2_LENGTH < S1_LENGTH ) THEN
    ILO = 1
    IHI = S2_LENGTH
    JLO = 1
    JHI = S2_LENGTH
  ELSE
    ILO = 1
    IHI = S1_LENGTH
    JLO = 1
    JHI = S2_LENGTH
  END IF

  S2(JLO:JHI) = S1(ILO:IHI)

  RETURN
END

!*************************************************************

FUNCTION S_LEI ( S1, S2 )

!*****************************************************************************80
!
!! S_LEI = ( S1 is lexically less than or equal to S2 ).
!
!  Discussion:
!
!    The comparison is done in a case-insensitive way.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_LEI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_LEI
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN ( S1 )
  LEN2 = LEN ( S2 )
  LENC = MIN ( S1_LENGTH, LEN2 )

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( LLT ( C1, C2 ) ) THEN
      S_LEI = .TRUE.
      RETURN
    ELSE IF ( LGT ( C1, C2 ) ) THEN
      S_LEI = .FALSE.
      RETURN
    END IF

  END DO

  IF ( S1_LENGTH <= LEN2 ) THEN
    S_LEI = .TRUE.
  ELSE
    S_LEI = .FALSE.
  END IF

  RETURN
END

!*************************************************************
FUNCTION S_LEN_TRIM ( S )

!*********************************************************************72
!
!! S_LEN_TRIM returns the length of a string to the last nonblank.
!
!  Discussion:
!
!    The FORTRAN90 function "len_trim()" should be used instead of
!    this emulation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 July 2015
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    character ( len = * ) S, a string.
!
!  Output:
!
!    integer S_LEN_TRIM, the length of the string to the last nonblank.
!
    IMPLICIT NONE

    INTEGER I
    CHARACTER ( LEN = * ) S
    INTEGER S_LEN
    INTEGER S_LEN_TRIM

    S_LEN = LEN ( S )

    DO I = S_LEN, 1, -1

        IF ( S(I:I) .NE. ' ' ) THEN
            S_LEN_TRIM = i
            RETURN
        END IF

    END DO

    S_LEN_TRIM = 0

    RETURN
END

!*************************************************************

SUBROUTINE S_LOW ( S )

!*****************************************************************************80
!
!! S_LOW replaces all uppercase letters by lowercase ones.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be
!    transformed.  On output, the string is all lowercase.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    CALL CH_LOW ( S(I:I) )
  END DO

  RETURN
END

!*************************************************************

FUNCTION S_LTI ( S1, S2 )

!*****************************************************************************80
!
!! S_LTI = ( S1 is lexically less than S2 ).
!
!  Discussion:
!
!    The comparison is done in a case-insensitive way.
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
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_LTI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_LTI
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN ( S1 )
  LEN2 = LEN ( S2 )
  LENC = MIN ( S1_LENGTH, LEN2 )

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( LLT ( C1, C2 ) ) THEN
      S_LTI = .TRUE.
      RETURN
    ELSE IF ( LGT ( C1, C2 ) ) THEN
      S_LTI = .FALSE.
      RETURN
    END IF

  END DO

  IF ( S1_LENGTH < LEN2 ) THEN
    S_LTI = .TRUE.
  ELSE
    S_LTI = .FALSE.
  END IF

  RETURN
END

!*************************************************************

FUNCTION S_NEQI ( S1, S2 )

!*****************************************************************************80
!
!! S_NEQI compares two strings for non-equality, ignoring case.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_NEQI, the result of the comparison.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LENC
  LOGICAL S_NEQI
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN ( S1 )
  LEN2 = LEN ( S2 )
  LENC = MIN ( S1_LENGTH, LEN2 )

  S_NEQI = .TRUE.

  DO I = 1, LENC

    C1 = S1(I:I)
    C2 = S2(I:I)
    CALL CH_CAP ( C1 )
    CALL CH_CAP ( C2 )

    IF ( C1 /= C2 ) THEN
      RETURN
    END IF

  END DO

  DO I = LENC + 1, S1_LENGTH
    IF ( S1(I:I) /= ' ' ) THEN
      RETURN
    END IF
  END DO

  DO I = LENC + 1, LEN2
    IF ( S2(I:I) /= ' ' ) THEN
      RETURN
    END IF
  END DO

  S_NEQI = .FALSE.

  RETURN
END

!*************************************************************

SUBROUTINE S_NONALPHA_DELETE ( S )

!*****************************************************************************80
!
!! S_NONALPHA_DELETE removes nonalphabetic characters from a string.
!
!  Discussion:
!
!    The remaining characters are left justified and blank padded.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  PUT = 0
  S_LENGTH = LEN_TRIM ( S )

  DO GET = 1, S_LENGTH

    CH = S(GET:GET)

    IF ( ( LLE ( 'A', CH ) .AND. LLE ( CH, 'Z' ) ) .OR. &
         ( LLE ( 'a', CH ) .AND. LLE ( CH, 'z' ) ) ) THEN
      PUT = PUT + 1
      S(PUT:PUT) = CH
    END IF

  END DO

  S(PUT+1:S_LENGTH) = ' '

  RETURN
END

!*************************************************************

FUNCTION S_NO_CONTROL ( S )

!*****************************************************************************80
!
!! S_NO_CONTROL = string contains no control characters.
!
!  Discussion:
!
!    Non-control characters are ASCII codes 32 through 127 inclusive.
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
!    Input, character ( len = * ) S, is the string to be checked.
!
!    Output, logical S_NO_CONTROL, is TRUE if S contains only printable
!    characters, FALSE otherwise.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  LOGICAL S_NO_CONTROL
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_NO_CONTROL = .FALSE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( CH_IS_CONTROL ( S(I:I) ) ) THEN
      RETURN
    END IF
  END DO

  S_NO_CONTROL = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_OF_I4 ( I )

!*****************************************************************************80
!
!! S_OF_I4 converts an integer to a left-justified string.
!
!  Example:
!
!         I  S
!
!         1  1
!        -1  -1
!         0  0
!      1952  1952
!    123456  123456
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, an integer to be converted.
!
!    Output, character ( len = 11 ) S_OF_I4, the representation of the
!    integer ( kind = 4 ).  The integer will be left-justified.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDIG
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IPOS
  INTEGER ( KIND = 4 ) IVAL
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = 11 ) S
  CHARACTER ( LEN = 11 ) S_OF_I4

  S = ' '

  ILO = 1
  IHI = 11
!
!  Make a copy of the integer.
!
  IVAL = I
!
!  Handle the negative sign.
!
  IF ( IVAL < 0 ) THEN

    IF ( IHI <= 1 ) THEN
      S(1:1) = '*'
      RETURN
    END IF

    IVAL = -IVAL
    S(1:1) = '-'
    ILO = 2

  END IF
!
!  The absolute value of the integer goes into S(ILO:IHI).
!
  IPOS = IHI
!
!  Find the last digit, strip it off, and stick it into the string.
!
  DO

    IDIG = MOD ( IVAL, 10 )
    IVAL = IVAL / 10

    IF ( IPOS < ILO ) THEN
      DO J = 1, IHI
        S(J:J) = '*'
      END DO
      RETURN
    END IF

    CALL DIGIT_TO_CH ( IDIG, C )

    S(IPOS:IPOS) = C
    IPOS = IPOS - 1

    IF ( IVAL == 0 ) THEN
      EXIT
    END IF

  END DO
!
!  Shift the string to the left.
!
  S(ILO:ILO+IHI-IPOS-1) = S(IPOS+1:IHI)
  S(ILO+IHI-IPOS:IHI) = ' '

  S_OF_I4 = S

  RETURN
END

!*************************************************************

FUNCTION S_ONLY_ALPHAB ( S )

!*****************************************************************************80
!
!! S_ONLY_ALPHAB checks if a string is only alphabetic and blanks.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    Acceptable characters are 'A' through 'Z' and 'a' through 'z' and blanks.
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
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_ONLY_ALPHAB, is TRUE if the string contains only
!    alphabetic characters and blanks.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ITEMP
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  LOGICAL S_ONLY_ALPHAB

  S_ONLY_ALPHAB = .FALSE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    C = S(I:I)

    IF ( C /= ' ' ) THEN

      ITEMP = IACHAR ( C )

      IF ( .NOT. ( 65 <= ITEMP .AND. ITEMP <= 90 ) ) THEN
        IF ( .NOT. ( 97 <= ITEMP .AND. ITEMP <= 122 ) ) THEN
          RETURN
        END IF
      END IF

    END IF

  END DO

  S_ONLY_ALPHAB = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_ONLY_DIGITB ( S )

!*****************************************************************************80
!
!! S_ONLY_DIGITB returns TRUE if the string contains only digits or blanks.
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
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_ONLY_DIGITB, is TRUE if the string contains only digits
!    and blanks.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  LOGICAL S_ONLY_DIGITB

  S_ONLY_DIGITB = .FALSE.
  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    C = S(I:I)

    IF ( C /= ' ' ) THEN
      IF ( LLT ( C, '0' ) .OR. LGT ( C, '9' ) ) THEN
        RETURN
      END IF
    END IF

  END DO

  S_ONLY_DIGITB = .TRUE.

  RETURN
END

!*************************************************************

SUBROUTINE S_OVERLAP ( S1, S2, OVERLAP )

!*****************************************************************************80
!
!! S_OVERLAP determines the overlap between two strings.
!
!  Discussion:
!
!    To determine the overlap, write the first word followed immediately
!    by the second word.  Find the longest substring S which is both
!    a suffix of S1 and a prefix of S2.  The length of this substring
!    is the overlap.
!
!  Example:
!
!    S1              S2        OVERLAP
!
!    'timber'        'beret'   3
!    'timber'        'timber'  6
!    'beret'         'timber'  1
!    'beret'         'berets'  5
!    'beret'         'berth'   0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to be checked.
!
!    Output, integer ( kind = 4 ) OVERLAP, the length of the overlap.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LEN3
  INTEGER ( KIND = 4 ) OVERLAP
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  OVERLAP = 0

  S1_LENGTH = LEN_TRIM ( S1 )
  LEN2 = LEN_TRIM ( S2 )
  LEN3 = MIN ( S1_LENGTH, LEN2 )

  DO I = 1, LEN3
    IF ( S1(S1_LENGTH+1-I:S1_LENGTH) == S2(1:I) ) THEN
      OVERLAP = I
    END IF
  END DO

  RETURN
END

!*************************************************************

FUNCTION S_PAREN_CHECK ( S )

!*****************************************************************************80
!
!! S_PAREN_CHECK checks the parentheses in a string.
!
!  Discussion:
!
!    Blanks are removed from the string, and then the following checks
!    are made:
!
!    1) as we read the string left to right, there must never be more
!       right parentheses than left ones;
!    2) there must be an equal number of left and right parentheses;
!    3) there must be no occurrences of the abutting packages '...)(...'.
!    4) there must be no occurrences of the empty package '()'.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to check.
!
!    Output, logical S_PAREN_CHECK is TRUE if the string passed the checks.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISUM
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 255 ) S_COPY
  INTEGER ( KIND = 4 ) S_LENGTH
  LOGICAL  S_PAREN_CHECK

  S_COPY = S
  CALL S_BLANK_DELETE ( S_COPY)

  S_LENGTH = LEN_TRIM ( S_COPY )
!
!  1) Letting '(' = +1 and ')' = -1, check that the running parentheses sum
!  is always nonnegative.
!
  ISUM = 0
  DO I = 1, S_LENGTH

    IF ( S_COPY(I:I) == '(' ) THEN
      ISUM = ISUM + 1
    END IF

    IF ( S_COPY(I:I) == ')' ) THEN

      ISUM = ISUM - 1

      IF ( ISUM < 0 ) THEN
        S_PAREN_CHECK = .FALSE.
        RETURN
      END IF

    END IF

  END DO
!
!  2) Check that the final parentheses sum is zero.
!
  IF ( ISUM /= 0 ) THEN
    S_PAREN_CHECK = .FALSE.
    RETURN
  END IF
!
!  3) Check that there are no ")(" pairs.
!
  DO I = 2, S_LENGTH
    IF ( S_COPY(I-1:I) == ')(' ) THEN
      S_PAREN_CHECK = .FALSE.
      RETURN
    END IF
  END DO
!
!  4) Check that there are no "()" pairs.
!
  DO I = 2, S_LENGTH

    IF ( S_COPY(I-1:I) == '()' ) THEN
      S_PAREN_CHECK = .FALSE.
      RETURN
    END IF

  END DO
!
!  The checks were passed.
!
  S_PAREN_CHECK = .TRUE.

  RETURN
END

!*************************************************************

SUBROUTINE S_REPLACE ( S, SUB1, SUB2, IREP )

!*****************************************************************************80
!
!! S_REPLACE replaces all occurrences of SUB1 by SUB2 in a string.
!
!  Discussion:
!
!    This is not always true if SUB2 is longer than SUB1.  The
!    replacement is NOT recursive.  In other words, replacing all
!    occurrences of "ab" by "a" in "abbbbb" will return "abbbb"
!    rather than "a".
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
!    Input/output, character ( len = * ) S.  On input,
!    the string in which occurrences are to be replaced.
!    On output, the revised string.
!
!    Input, character ( len = * ) SUB1, the string which is to be replaced.
!    Trailing blank characters are ignored.  The routine is case sensitive.
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, integer ( kind = 4 ) IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space.
!    (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would have
!    fallen off the end)
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IREP
  INTEGER ( KIND = 4 ) LEN1
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LENS
  INTEGER ( KIND = 4 ) LOC
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) SUB1
  CHARACTER ( LEN = * ) SUB2

  IREP = 0
  LENS = LEN ( S )

  IF ( LENS <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE - Serious error!'
    WRITE ( *, '(a)' ) '  Null string not allowed!'
    RETURN
  END IF

  LEN1 = LEN_TRIM ( SUB1 )

  IF ( LEN1 <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE - Serious error!'
    WRITE ( *, '(a)' ) '  Null SUB1 not allowed!'
    RETURN
  END IF

  LEN2 = LEN_TRIM ( SUB2 )

  IF ( LEN2 == LEN1 ) THEN

    IF ( SUB1(1:LEN1) == SUB2(1:LEN2) ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'S_REPLACE - Warning!'
      WRITE ( *, '(a)' ) '  Replacement = original!'
      RETURN
    END IF

    ILO = 1

    DO

      LOC = INDEX ( S(ILO:LENS), SUB1(1:LEN1) )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      LOC = LOC + ILO - 1
      IREP = IREP + 1
      S(LOC:LOC+LEN1-1) = SUB2(1:LEN2)
      ILO = LOC + LEN1

      IF ( LENS < ILO ) THEN
        EXIT
      END IF

    END DO

  ELSE IF ( LEN2 < LEN1 ) THEN

    ILO = 1

    DO

      LOC = INDEX ( S(ILO:LENS), SUB1(1:LEN1) )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      IREP = IREP + 1
      LOC = LOC + ILO - 1
      S(LOC:LOC+LEN2-1) = SUB2(1:LEN2)
      CALL S_CHOP ( S, LOC+LEN2, LOC+LEN1-1 )
      ILO = LOC + LEN2

      IF ( LENS < ILO ) THEN
        EXIT
      END IF

    END DO

  ELSE

    ILO = 1

    DO

      LOC = INDEX ( S(ILO:LENS), SUB1(1:LEN1) )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      LOC = LOC + ILO - 1
      IREP = IREP + 1

      IF ( LENS < LOC + LEN2 - 1 ) THEN
        IREP = -IREP
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'S_REPLACE - Warning!'
        WRITE ( *, '(a)' ) '  Some replaceable elements remain!'
        EXIT
      END IF

      CALL S_BLANKS_INSERT ( S, LOC, LOC+LEN2-1-LEN1 )

      S(LOC:LOC+LEN2-1) = SUB2(1:LEN2)
      ILO = LOC + LEN2

    END DO

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_REPLACE_CH ( S, C1, C2 )

!*****************************************************************************80
!
!! S_REPLACE_CH replaces all occurrences of one character by another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 March 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string.
!
!    Input, character C1, C2, the character to be replaced, and the
!    replacement character.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( S(I:I) == C1 ) THEN
      S(I:I) = C2
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_REPLACE_I ( S, SUB1, SUB2 )

!*****************************************************************************80
!
!! S_REPLACE_I replaces all occurrences of SUB1 by SUB2 in a string.
!
!  Discussion:
!
!    Matches are made without regard to case.
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
!    Input/output, character ( len = * ) S.  On input,
!    the string in which occurrences are to be replaced.
!    On output, the revised string.
!
!    Input, character ( len = * ) SUB1, the string which is to be replaced.
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, integer ( kind = 4 ) IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space.
!    (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would have
!    fallen off the end)
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) LEN1
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LENS
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) SUB1
  CHARACTER ( LEN = * ) SUB2

  LENS = LEN ( S )

  IF ( LENS <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE_I - Serious error!'
    WRITE ( *, '(a)' ) '  Null string not allowed!'
    RETURN
  END IF

  LEN1 = LEN ( SUB1 )

  IF ( LEN1 <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE_I - Serious error!'
    WRITE ( *, '(a)' ) '  Null SUB1 not allowed!'
    RETURN
  END IF

  LEN2 = LEN ( SUB2 )

  ILO = S_INDEXI ( S, SUB1 )
!
!  If the match string has been found, then insert the replacement.
!
  IF ( ILO /= 0 ) THEN
    S(ILO+LEN2:LENS+LEN2-LEN1) = S(ILO+LEN1:LENS)
    S(ILO:ILO+LEN2-1) = SUB2(1:LEN2)
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_REPLACE_ONE ( S1, SUB1, SUB2, S2 )

!*****************************************************************************80
!
!! S_REPLACE_ONE replaces the first occurrence of SUB1 with SUB2.
!
!  Discussion:
!
!    The input and output strings may coincide.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 November 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the initial string.
!
!    Input, character ( len = * ) SUB1, the string to be replaced.
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, character ( len = * ) S2, the final string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  INTEGER ( KIND = 4 ) I3
  INTEGER ( KIND = 4 ) I4
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = 255 ) S3
  CHARACTER ( LEN = * ) SUB1
  CHARACTER ( LEN = * ) SUB2

  S3 = ' '

  I1 = INDEX ( S1, SUB1 )

  IF ( I1 == 0 ) THEN

    S3 = S1

  ELSE

    S3(1:I1-1) = S1(1:I1-1)

    I2 = LEN_TRIM ( SUB2 )
    S3(I1:I1+I2-1) = SUB2(1:I2)

    I3 = I1 + LEN_TRIM ( SUB1 )
    I4 = LEN_TRIM ( S1 )

    S3(I1+I2:I1+I2+1+I4-I3) = S1(I3:I4)

  END IF

  S2 = S3

  RETURN
END

!*************************************************************

SUBROUTINE S_REPLACE_REC ( S, SUB1, SUB2, IREP )

!*****************************************************************************80
!
!! S_REPLACE_REC is a recursive replacement of one string by another.
!
!  Discussion:
!
!    All occurrences of SUB1 should be replaced by SUB2.
!    This is not always true if SUB2 is longer than SUB1.
!    The replacement is recursive.  In other words, replacing all
!    occurrences of "ab" by "a" in "abbbbb" will return "a" rather
!    than "abbbb".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.  On input,
!    the string in which occurrences are to be replaced.  On
!    output, the revised string.
!
!    Input, character ( len = * ) SUB1, the string which is to be replaced.
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, integer ( kind = 4 ) IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space in
!    S.  (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would
!    have fallen off the end)
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IREP
  INTEGER ( KIND = 4 ) LEN1
  INTEGER ( KIND = 4 ) LEN2
  INTEGER ( KIND = 4 ) LOC
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB1
  CHARACTER ( LEN = * ) SUB2

  IREP = 0
  S_LENGTH = LEN ( S )

  IF ( S_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE_REC - Serious error!'
    WRITE ( *, '(a)' ) '  Null string not allowed!'
    RETURN
  END IF

  LEN1 = LEN ( SUB1 )

  IF ( LEN1 <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_REPLACE_REC - Serious error!'
    WRITE ( *, '(a)' ) '  Null SUB1 not allowed!'
    RETURN
  END IF

  LEN2 = LEN ( SUB2 )

  IF ( LEN2 == LEN1 ) THEN

    IF ( SUB1 == SUB2 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'S_REPLACE_REC - Warning!'
      WRITE ( *, '(a)' ) '  Replacement = original!'
      RETURN
    END IF

    DO

      LOC = INDEX ( S, SUB1 )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      IREP = IREP + 1
      S(LOC:LOC+LEN1-1) = SUB2

    END DO

  ELSE IF ( LEN2 < LEN1 ) THEN

    DO

      LOC = INDEX ( S, SUB1 )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      IREP = IREP + 1
      S(LOC:LOC+LEN2-1) = SUB2
      CALL S_CHOP ( S, LOC+LEN2, LOC+LEN1-1 )

    END DO

  ELSE

    DO

      LOC = INDEX ( S, SUB1 )

      IF ( LOC == 0 ) THEN
        EXIT
      END IF

      IREP = IREP + 1

      IF ( S_LENGTH < LOC + LEN2 - 1 ) THEN
        IREP = -IREP
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'S_REPLACE_REC - Warning!'
        WRITE ( *, '(a)' ) '  Some replaceable elements remain!'
        RETURN
      END IF

      CALL S_BLANKS_INSERT ( S, LOC, LOC+LEN2-1-LEN1 )
      S(LOC:LOC+LEN2-1) = SUB2

    END DO

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_REVERSE ( S )

!*****************************************************************************80
!
!! S_REVERSE reverses the characters in a string.
!
!  Example:
!
!    Input        Output
!
!    ' Cat'       'taC '
!    'Goo gol  '  'log ooG  '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to reverse.
!    Trailing blanks are ignored.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH / 2
    J = S_LENGTH + 1 - I
    CH     = S(I:I)
    S(I:I) = S(J:J)
    S(J:J) = CH
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_RIGHT_INSERT ( S1, S2 )

!*****************************************************************************80
!
!! S_RIGHT_INSERT inserts a string flush right into another.
!
!  Discussion:
!
!    S2 is not blanked out first.  If there is already information in S2,
!    some of it may still be around after S1 is written into S2.  Users may
!    want to first assign S2 = ' ' if this is not the effect desired.
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
!    Input, character ( len = * ) S1, a string which is to be
!    inserted into S2.  Only the portion of S1 up to the last
!    nonblank will be used.
!
!    Output, character ( len = * ) S2, a string whose length
!    will be determined by a call to LEN, and which will
!    contain, on output, a right flush copy of S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) JHI
  INTEGER ( KIND = 4 ) JLO
  INTEGER ( KIND = 4 ) LEN1
  INTEGER ( KIND = 4 ) LEN2
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2

  LEN1 = LEN_TRIM ( S1 )
  LEN2 = LEN ( S2 )

  IF ( LEN1 < LEN2 ) THEN
    ILO = 1
    IHI = LEN1
    JLO = LEN2 + 1 - LEN1
    JHI = LEN2
  ELSE IF ( LEN2 < LEN1 ) THEN
    ILO = LEN1 + 1 - LEN2
    IHI = LEN1
    JLO = 1
    JHI = LEN2
  ELSE
    ILO = 1
    IHI = LEN1
    JLO = 1
    JHI = LEN2
  END IF

  S2(JLO:JHI) = S1(ILO:IHI)

  RETURN
END

!*************************************************************

SUBROUTINE S_ROMAN_TO_I4 ( S, I )

!*****************************************************************************80
!
!! S_ROMAN_TO_I4 converts a Roman numeral to an integer.
!
!  Example:
!
!    S      I
!
!    X      10
!    XIX    19
!    MI     1001
!    CXC    190
!
!  Discussion:
!
!    The subroutine does not check carefully as to whether the Roman numeral
!    is properly formed.  In particular, it will accept a string like 'IM'
!    and return 999, even though this is not a well formed Roman numeral.
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
!    Input, character ( len = * ) S, a string containing a Roman numeral.
!
!    Output, integer ( kind = 4 ) I, the corresponding value.
!
  IMPLICIT NONE

  CHARACTER C1
  CHARACTER C2
  LOGICAL DONE
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  CHARACTER ( LEN = * ) S

  I = 0
  DONE = .TRUE.

  DO

    CALL CH_NEXT ( S, C2, DONE )

    IF ( DONE ) THEN
      RETURN
    END IF

    I2 = CH_ROMAN_TO_I4 ( C2 )

    IF ( I2 == 0 .AND. C2 /= ' ' ) THEN
      RETURN
    END IF

    DO

      C1 = C2
      I1 = I2

      CALL CH_NEXT ( S, C2, DONE )

      IF ( DONE ) THEN
        I = I + I1
        RETURN
      END IF

      I2 = CH_ROMAN_TO_I4 ( C2 )

      IF ( I2 == 0 .AND. C2 /= ' ' ) THEN
        I = I + I1
        RETURN
      END IF

      IF ( I1 < I2 ) THEN
        I = I + I2 - I1
        C1 = ' '
        C2 = ' '
        EXIT
      END IF

      I = I + I1

    END DO

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_R_APPEND ( S, R, DONE )

!*****************************************************************************80
!
!! S_R_APPEND appends a real number to a string.
!
!  Discussion:
!
!    A blank space will separate the value from the text already
!    in the line.
!
!    The routine warns the user if the value will not fit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a line of text.
!    On input, the current string.  On output, the current string
!    with the integer value appended.
!
!    Input, real ( kind = 4 ) R, the real number to be appended to the line.
!
!    Output, logical DONE, is FALSE if there was not enough room
!    to append the data.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) LENS
  INTEGER ( KIND = 4 ) LENTS
  INTEGER ( KIND = 4 ) LENW
  INTEGER ( KIND = 4 ) NEXT
  REAL ( KIND = 4 ) R
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 14 ) W

  DONE = .FALSE.

  LENS = LEN ( S )
  LENTS = LEN_TRIM ( S )

  CALL R4_TO_S_LEFT ( R, W )

  LENW = LEN_TRIM ( W )

  IF ( LENTS == 0 ) THEN
    IF ( LENS < LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  ELSE
    IF ( LENS < LENTS + 1 + LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  END IF

  IF ( LENTS == 0 ) THEN
    NEXT = 1
  ELSE
    NEXT = LENTS + 1
    S(NEXT:NEXT) = ' '
    NEXT = NEXT + 1
  END IF

  S(NEXT:NEXT+LENW-1) = W(1:LENW)

  RETURN
END

!*************************************************************

SUBROUTINE S_SET_DELETE ( S1, S2 )

!*****************************************************************************80
!
!! S_SET_DELETE removes any characters in one string from another string.
!
!  Discussion:
!
!    When an element is removed, the rest of the string is shifted to the
!    left, and padded with blanks on the right.
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
!    Input, character ( len = * ) S1, the string to be examined.
!
!    Input, character ( len = * ) S2, the characters to be removed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) NSET
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN ( S1 )
  NSET = LEN ( S2 )

  I = 0

  DO WHILE ( I < S1_LENGTH )

    I = I + 1

    DO J = 1, NSET
      IF ( S1(I:I) == S2(J:J) ) THEN
        CALL S_CHOP ( S1, I, I )
        S1_LENGTH = S1_LENGTH - 1
        I = I - 1
        EXIT
      END IF
    END DO

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_SHIFT_CIRCULAR ( S, ISHFT )

!*****************************************************************************80
!
!! S_SHIFT_CIRCULAR circular shifts the characters in a string to the right.
!
!  Discussion:
!
!    Thus, a shift of -1 would change "Violin" to "iolinV", and a shift
!    of 1 would change it to "nVioli".
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
!    Input/output, character ( len = * ) S, the string to be shifted.
!
!    Input, integer ( kind = 4 ) ISHFT, the number of positions to the
!    right to shift the characters.
!
  IMPLICIT NONE

  CHARACTER CHRIN
  CHARACTER CHROUT
  INTEGER ( KIND = 4 ) ICYCLE
  INTEGER ( KIND = 4 ) IDID
  INTEGER ( KIND = 4 ) IGOTO
  INTEGER ( KIND = 4 ) IMOVE
  INTEGER ( KIND = 4 ) ISHFT
  INTEGER ( KIND = 4 ) JSHFT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_SHIFT_CIRCULAR - Serious error!'
    WRITE ( *, '(a)' ) '  String has nonpositive length!'
    RETURN
  END IF
!
!  Force the shift to be positive and between 0 and S_LENGTH.
!
  JSHFT = ISHFT

  DO WHILE ( JSHFT < 0 )
    JSHFT = JSHFT + S_LENGTH
  END DO

  DO WHILE ( S_LENGTH < JSHFT )
    JSHFT = JSHFT - S_LENGTH
  END DO

  IF ( JSHFT == 0 ) THEN
    RETURN
  END IF
!
!  Shift the first character.  Shift the character that got
!  displaced by the first character...Repeat until you've shifted
!  all, or have "cycled" back to the first character early.
!
!  If you've cycled, start again at the second character, and
!  so on.
!
  ICYCLE = 0
  IDID = 0
  IMOVE = 0

  DO WHILE ( IDID < S_LENGTH )

    IF ( IMOVE == ICYCLE ) THEN
      IMOVE = IMOVE + 1
      ICYCLE = ICYCLE + 1
      CHRIN = S(IMOVE:IMOVE)
    END IF

    IDID = IDID + 1
    IGOTO = IMOVE + JSHFT

    IF ( S_LENGTH < IGOTO ) THEN
      IGOTO = IGOTO - S_LENGTH
    END IF

    CHROUT = S(IGOTO:IGOTO)
    S(IGOTO:IGOTO) = CHRIN
    CHRIN = CHROUT

    IMOVE = IGOTO

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_SHIFT_LEFT ( S, ISHFT )

!*****************************************************************************80
!
!! S_SHIFT_LEFT shifts the characters in a string to the left and blank pads.
!
!  Discussion:
!
!    A shift of 2 would change "Violin" to "olin  ".
!    A shift of -2 would change "Violin" to "  Violin".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be shifted.
!
!    Input, integer ( kind = 4 ) ISHFT, the number of positions to the
!    left to shift the characters.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISHFT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( 0 < ISHFT ) THEN

    DO I = 1, S_LENGTH - ISHFT
      S(I:I) = S(I+ISHFT:I+ISHFT)
    END DO

    DO I = S_LENGTH - ISHFT + 1, S_LENGTH
      S(I:I) = ' '
    END DO

  ELSE IF ( ISHFT < 0 ) THEN

    DO I = S_LENGTH, - ISHFT + 1, - 1
      S(I:I) = S(I+ISHFT:I+ISHFT)
    END DO

    DO I = -ISHFT, 1, -1
      S(I:I) = ' '
    END DO

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_SHIFT_RIGHT ( S, ISHFT )

!*****************************************************************************80
!
!! S_SHIFT_RIGHT shifts the characters in a string to the right and blank pads.
!
!  Discussion:
!
!    A shift of 2 would change "Violin" to "  Viol".
!    A shift of -2 would change "Violin" to "olin  ".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be shifted.
!
!    Input, integer ( kind = 4 ) ISHFT, the number of positions to the
!    right to shift the characters.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISHFT
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  IF ( S_LENGTH <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_SHIFT_RIGHT - Serious error!'
    WRITE ( *, '(a)' ) '  String has nonpositive length!'
    RETURN
  END IF

  IF ( 0 < ISHFT ) THEN

    DO I = S_LENGTH, ISHFT + 1, - 1
      S(I:I) = S(I-ISHFT:I-ISHFT)
    END DO

    DO I = ISHFT, 1, -1
      S(I:I) = ' '
    END DO

  ELSE IF ( ISHFT < 0 ) THEN

    DO I = 1, S_LENGTH + ISHFT
      S(I:I) = S(I-ISHFT:I-ISHFT)
    END DO

    DO I = S_LENGTH + ISHFT + 1, S_LENGTH
      S(I:I) = ' '
    END DO

  END IF

END

!*************************************************************

FUNCTION S_SKIP_SET ( S1, S2 )

!*****************************************************************************80
!
!! S_SKIP_SET finds the first entry of a string that is NOT in a set.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the string to be examined.
!
!    Input, character ( len = * ) S2, the characters to skip.
!
!    Output, integer ( kind = 4 ) S_SKIP_SET, the location of the first
!    character in S1 that is not in S2, or 0 if no such index was found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) S_SKIP_SET
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2

  S1_LENGTH = LEN_TRIM ( S1 )

  DO I = 1, S1_LENGTH

    IF ( INDEX ( S2, S1(I:I) ) == 0 ) THEN
      S_SKIP_SET = I
      RETURN
    END IF

  END DO

  S_SKIP_SET = 0

  RETURN
END

!*************************************************************

SUBROUTINE S_SORT_A ( S )

!*****************************************************************************80
!
!! S_SORT_A sorts a string into ascending order.
!
!  Discussion:
!
!    The string is assumed to be short, and so a simple bubble sort is used.
!
!    ALL the characters are sorted, including blanks and punctuation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be sorted.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) K
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH - 1

    C = S(I:I)
    J = I

    DO K = I + 1, S_LENGTH
      IF ( IACHAR ( S(K:K) ) < IACHAR ( S(J:J) ) ) THEN
        J = K
      END IF
    END DO

    IF ( I /= J ) THEN
      S(I:I) = S(J:J)
      S(J:J) = C
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_SPLIT ( S, SUB, S1, S2, S3 )

!*****************************************************************************80
!
!! S_SPLIT divides a string into three parts, given the middle.
!
!  Discussion:
!
!    This version of the routine is case-insensitive.
!
!  Example:
!
!    Input:
!
!      S = 'aBCdEfgh'
!      S2 = 'eF'
!
!    Output:
!
!      S1 = 'aBCd'
!      S2 =  'gh'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be analyzed.
!
!    Input, character ( len = * ) SUB, the substring used to "split" S.
!    Trailing blanks in SUB are ignored.
!
!    Output, character ( len = * ) S1, the entries in the string, up
!    to, but not including, the first occurrence, if any,
!    of SUB.  If SUB occurs immediately, then S1 = ' '.
!    If SUB is not long enough, trailing entries will be lost.
!
!    Output, character ( len = * ) S2, the part of the string that matched SUB.
!    If S2 is ' ', then there wasn't a match.
!
!    Output, character ( len = * ) S3, the part of the string after the match.
!    If there was no match, then S3 is blank.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENM
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = * ) S3
  CHARACTER ( LEN = * ) SUB

  S_LENGTH = LEN_TRIM ( S )

  LENM = LEN_TRIM ( SUB )

  IF ( LENM == 0 ) THEN
    LENM = 1
  END IF

  I = S_INDEXI ( S, SUB )
!
!  The substring did not occur.
!
  IF ( I == 0 ) THEN
    S1 = S
    S2 = ' '
    S3 = ' '
!
!  The substring begins immediately.
!
  ELSE IF ( I == 1 ) THEN
    S1 = ' '
    S2 = S(1:LENM)
    S3 = S(LENM+1:)
!
!  What am I checking here?
!
  ELSE IF ( S_LENGTH < I + LENM ) THEN
    S1 = S
    S2 = ' '
    S3 = ' '
!
!  The substring occurs in the middle.
!
  ELSE
    S1 = S(1:I-1)
    S2 = S(I:I+LENM-1)
    S3 = S(I+LENM: )
  END IF
!
!  Drop leading blanks.
!
  S1 = ADJUSTL ( S1 )
  S2 = ADJUSTL ( S2 )
  S3 = ADJUSTL ( S3 )

  RETURN
END

!*************************************************************

SUBROUTINE S_SWAP ( S1, S2 )

!*****************************************************************************80
!
!! S_SWAP swaps two strings.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S1, S2.  On output, the values of S1
!    and S2 have been interchanged.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2
  CHARACTER ( LEN = 255 ) S3

  S3 = S1
  S1 = S2
  S2 = S3

  RETURN
END

!*************************************************************

SUBROUTINE S_S_DELETE ( S, SUB, IREP )

!*****************************************************************************80
!
!! S_S_DELETE removes all occurrences of a substring from a string.
!
!  Discussion:
!
!    The remainder is left justified and padded with blanks.
!
!    The deletion is not recursive.  Removing all occurrences of "ab" from
!    "aaaaabbbbbQ" results in "aaaabbbbQ" rather than "Q".
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character ( len = * ) SUB1, the substring to be removed.
!
!    Output, integer ( kind = 4 ) IREP, the number of occurrences of SUB1
!    which were found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IREP
  INTEGER ( KIND = 4 ) LOC
  INTEGER ( KIND = 4 ) NSUB
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) SUB

  NSUB = LEN_TRIM ( SUB )

  IREP = 0
  ILO = 1
  IHI = LEN_TRIM ( S )

  DO WHILE ( ILO <= IHI )

    LOC = INDEX ( S(ILO:IHI), SUB )

    IF ( LOC == 0 ) THEN
      RETURN
    END IF

    IREP = IREP + 1
    LOC = LOC + ILO - 1
    CALL S_CHOP ( S, LOC, LOC+NSUB-1 )
    ILO = LOC
    IHI = IHI - NSUB

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_S_DELETE2 ( S, SUB, IREP )

!*****************************************************************************80
!
!! S_S_DELETE2 recursively removes a substring from a string.
!
!  Discussion:
!
!    The remainder is left justified and padded with blanks.
!
!    The substitution is recursive, so
!    that, for example, removing all occurrences of "ab" from
!    "aaaaabbbbbQ" results in "Q".
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character ( len = * ) SUB, the substring to be removed.
!
!    Output, integer ( kind = 4 ) IREP, the number of occurrences of
!    the substring.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) IREP
  INTEGER ( KIND = 4 ) LOC
  INTEGER ( KIND = 4 ) NSUB
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) SUB

  S_LENGTH = LEN ( S )
  NSUB = LEN ( SUB )

  IREP = 0
  IHI = S_LENGTH

  DO WHILE ( 0 < IHI )

    LOC = INDEX ( S(1:IHI), SUB )

    IF ( LOC == 0 ) THEN
      RETURN
    END IF

    IREP = IREP + 1
    CALL S_CHOP ( S, LOC, LOC+NSUB-1 )
    IHI = IHI - NSUB

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_S_INSERT ( S1, IPOS, S2 )

!*****************************************************************************80
!
!! S_S_INSERT inserts a substring into a string.
!
!  Discussion:
!
!    Characters in the string are moved to the right to make room, and
!    hence the trailing characters, if any, are lost.
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
!    Input/output, character ( len = * ) S1, the string into which
!    the second string is to be inserted.
!
!    Input, integer ( kind = 4 ) IPOS, the position in S at which S2 is
!    to be inserted.
!
!    Input, character ( len = * ) S2, the string to be inserted.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) IPOS
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S1_LENGTH = LEN ( S1 )
  S2_LENGTH = LEN_TRIM ( S2 )

  IHI = MIN ( S1_LENGTH, IPOS+S2_LENGTH-1 )

  CALL S_BLANKS_INSERT ( S1, IPOS, IHI )

  S1(IPOS:IHI) = S2

  RETURN
END

!*************************************************************

FUNCTION S_S_SUBANAGRAM ( S1, S2 )

!*****************************************************************************80
!
!! S_S_SUBANAGRAM determines if S2 is a "subanagram" of S1.
!
!  Discussion:
!
!    S2 is an anagram of S1 if S2 can be formed by permuting the letters
!    of S1
!
!    S2 is an subanagram of S1 if S2 can be formed by selecting SOME of
!    the letters of S1 and permuting them.
!
!    Blanks (trailing or otherwise), punctuation, and capitalization
!    are all significant, so be sure to input exactly the information
!    you want to check.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the master string.
!
!    Input, character ( len = * ) S2, the second string.
!
!    Output, logical S_S_SUBANAGRAM is TRUE if S2 is a subanagram of S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  LOGICAL S_S_SUBANAGRAM
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S_S_SUBANAGRAM = .FALSE.
!
!  Sort both.
!
  CALL S_SORT_A ( S1 )
  CALL S_SORT_A ( S2 )

  S1_LENGTH = LEN ( S1 )
  S2_LENGTH = LEN ( S2 )

  I1 = 0

  DO I2 = 1, S2_LENGTH

    DO

      I1 = I1 + 1
!
!  Ran out of S1 before finishing.  No match is possible.
!
      IF ( S1_LENGTH < I1 ) THEN
        RETURN
      END IF
!
!  The current character in S1 is already greater than the character in S2.
!  No match is possible.
!
      IF ( LLT ( S2(I2:I2), S1(I1:I1) ) ) THEN
        RETURN
      END IF
!
!  Found an exact match for current character.  Keep going.
!
      IF ( S1(I1:I1) == S2(I2:I2) ) THEN
        EXIT
      END IF
!
!  Didn't find a match, but one might be possible if we increase I1.
!

    END DO

  END DO
!
!  We matched every character of S2 with something in S1.
!
  S_S_SUBANAGRAM = .TRUE.

  RETURN
END

!*************************************************************

FUNCTION S_S_SUBANAGRAM_SORTED ( S1, S2 )

!*****************************************************************************80
!
!! S_S_SUBANAGRAM_SORTED determines if S2 is a "subanagram" of S1.
!
!  Discussion:
!
!    This routine assumes that S1 and S2 have already been sorted.
!
!    S2 is an anagram of S1 if S2 can be formed by permuting the letters
!    of S1
!
!    S2 is an subanagram of S1 if S2 can be formed by selecting SOME of
!    the letters of S1 and permuting them.
!
!    Blanks (trailing or otherwise), punctuation, and capitalization
!    are all significant, so be sure to input exactly the information
!    you want to check.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the master string.
!
!    Input, character ( len = * ) S2, the second string.
!
!    Output, logical S_S_SUBANAGRAM_SORTED is TRUE if S2 is a subanagram of S1.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  LOGICAL S_S_SUBANAGRAM_SORTED
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_LENGTH

  S_S_SUBANAGRAM_SORTED = .FALSE.

  S1_LENGTH = LEN ( S1 )
  S2_LENGTH = LEN ( S2 )

  I1 = 0

  DO I2 = 1, S2_LENGTH

    DO

      I1 = I1 + 1
!
!  Ran out of S1 before finishing.  No match is possible.
!
      IF ( S1_LENGTH < I1 ) THEN
        RETURN
      END IF
!
!  The current character in S1 is already greater than the character in S2.
!  No match is possible.
!
      IF ( LLT ( S2(I2:I2), S1(I1:I1) ) ) THEN
        RETURN
      END IF
!
!  Found an exact match for current character.  Keep going.
!
      IF ( S1(I1:I1) == S2(I2:I2) ) THEN
        EXIT
      END IF
!
!  Didn't find a match, but one might be possible if we increase I1.
!
    END DO

  END DO
!
!  We matched every character of S2 with something in S1.
!
  S_S_SUBANAGRAM_SORTED = .TRUE.

  RETURN
END

!*************************************************************

SUBROUTINE S_TAB_BLANK ( S )

!*****************************************************************************80
!
!! S_TAB_BLANK replaces each TAB character by one space.
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
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH

    IF ( S(I:I) == TAB ) THEN
      S(I:I) = ' '
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TAB_BLANKS ( S )

!*****************************************************************************80
!
!! S_TAB_BLANKS replaces TAB characters by 6 spaces.
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
!    Input/output, character ( len = * ) S, the string to be modified.  On
!    output, some significant characters at the end of S may have
!    been lost.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) LENC
  INTEGER ( KIND = 4 ) LENS
  INTEGER ( KIND = 4 ) NTAB
  CHARACTER ( LEN = * ) S
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
!
!  If no TAB's occur in the line, there is nothing to do.
!
  IF ( INDEX ( S, TAB ) == 0 ) THEN
    RETURN
  END IF
!
!  Otherwise, find out how long the string is.
!
  LENC = LEN_TRIM ( S )
  LENS = LEN ( S )
!
!  Count the TAB's.
!
  NTAB = 0
  DO I = 1, LENC
    IF ( S(I:I) == TAB ) THEN
      NTAB = NTAB + 1
    END IF
  END DO
!
!  Now copy the string onto itself, going backwards.
!  As soon as we've processed the first TAB, we're done.
!
  PUT = LENC + 5 * NTAB

  DO GET = LENC, 1, - 1

    IF ( S(GET:GET) /= TAB ) THEN

      IF ( PUT <= LENS ) THEN
        S(PUT:PUT) = S(GET:GET)
      END IF

      PUT = PUT - 1

    ELSE

      DO I = PUT, PUT - 5, -1
        IF ( I <= LENS ) THEN
          S(I:I) = ' '
        END IF
      END DO

      PUT = PUT - 6
      NTAB = NTAB - 1

      IF ( NTAB == 0 ) THEN
        RETURN
      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TOKEN_EQUAL ( S, SET, NSET, ISET )

!*****************************************************************************80
!
!! S_TOKEN_EQUAL checks whether a string is equal to any of a set of strings.
!
!  Discussion:
!
!    The comparison is case-insensitive.
!
!    Trailing blanks in S and the elements of SET are ignored.
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
!    Input, character ( len = * ) S, the string to check.
!
!    Input, character ( len = * ) SET(NSET), the set of strings.
!
!    Input, integer ( kind = 4 ) NSET, the number of elements of SET.
!
!    Output, integer ( kind = 4 ) ISET, equals 0 if no element of SET
!    equals S.  If ISET is nonzero, then SET(ISET) equals
!    S, disregarding case.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) ISET
  INTEGER ( KIND = 4 ) NSET
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) SET(*)

  ISET = 0
  DO I = 1, NSET

    IF ( S_EQI ( S, SET(I) ) ) THEN
      ISET = I
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TOKEN_MATCH ( S, TOKEN_NUM, TOKEN, MATCH )

!*****************************************************************************80
!
!! S_TOKEN_MATCH matches the beginning of a string and a set of tokens.
!
!  Example:
!
!    Input:
!
!      S = 'TOMMYGUN'
!      TOKEN = 'TOM', 'ZEBRA', 'TOMMY', 'TOMMYKNOCKER'
!
!    Output:
!
!      MATCH = 3
!
!  Discussion:
!
!    The longest possible match is taken.
!    Matching is done without regard to case or trailing blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be examined.
!
!    Input, integer ( kind = 4 ) TOKEN_NUM, the number of tokens to be compared.
!
!    Input, character ( len = * ) TOKEN(TOKEN_NUM), the tokens.
!
!    Output, integer ( kind = 4 ) MATCH, the index of the (longest)
!    token that matched the string, or 0 if no match was found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) TOKEN_NUM

  INTEGER ( KIND = 4 ) MATCH
  INTEGER ( KIND = 4 ) MATCH_LENGTH
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) TOKEN_I
  INTEGER ( KIND = 4 ) TOKEN_LENGTH
  CHARACTER ( LEN = * ) TOKEN(TOKEN_NUM)

  MATCH = 0
  MATCH_LENGTH = 0

  S_LENGTH = LEN_TRIM ( S )

  DO TOKEN_I = 1, TOKEN_NUM

    TOKEN_LENGTH = LEN_TRIM ( TOKEN ( TOKEN_I ) )

    IF ( MATCH_LENGTH < TOKEN_LENGTH ) THEN

      IF ( TOKEN_LENGTH <= S_LENGTH ) THEN

        IF ( S_EQI ( S(1:TOKEN_LENGTH), TOKEN(TOKEN_I)(1:TOKEN_LENGTH) ) ) THEN
          MATCH_LENGTH = TOKEN_LENGTH
          MATCH = TOKEN_I
        END IF

      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_C4 ( S, CVAL, IERROR, LENGTH )

!*****************************************************************************80
!
!! S_TO_C4 reads a complex number from a string.
!
!  Discussion:
!
!    A C4 is simply a complex number to be stored as a
!    "complex ( kind = 4 )" value.
!
!    This routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!  Legal input is:
!
!     1 blanks,
!     2 '+' or '-' sign,
!     3 integer part,
!     4 decimal point,
!     5 fraction part,
!     6 'E' or 'e' or 'D' or 'd', exponent marker,
!     7 exponent sign,
!     8 exponent integer part,
!     9 exponent decimal point,
!    10 exponent fraction part,
!    11 blanks,
!    12 '+' or '-' sign,
!    13 integer part,
!    14 decimal point,
!    15 fraction part,
!    16 'E' or 'e' or 'D' or 'd', exponent marker,
!    17 exponent sign,
!    18 exponent integer part,
!    19 exponent decimal point,
!    20 exponent fraction part,
!    21 blanks,
!    22 "*"
!    23 spaces
!    24 I
!    25 comma or semicolon
!
!    with most quantities optional.
!
!  Example:
!
!    S               CVAL      IERROR     LENGTH
!
!    '1'               1         0          1
!    '1+I'             1 + 1 i   0          3
!    '1+1 i'           1 + 1 i   0          5
!    '1+1*i'           1 + 1 i   0          5
!    'i'                   1 i   0          1
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
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, complex ( kind = 4 ) CVAL, the value that was read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, the string was empty.
!    2, could not read A correctly.
!    3, could not read B correctly.
!    4, could not read I correctly.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read from
!    the string to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  IMPLICIT NONE

  REAL ( KIND = 4 ) AVAL
  REAL ( KIND = 4 ) BVAL
  CHARACTER C
  CHARACTER C2
  COMPLEX   ( KIND = 4 ) CVAL
  INTEGER ( KIND = 4 ) ICHR
  INTEGER ( KIND = 4 ) ICHR2
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
!
!  Initialize the return arguments.
!
  IERROR = 0
  AVAL = 0.0E+00
  BVAL = 0.0E+00
  CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
  LENGTH = 0
!
!  Get the length of the line, and if it's zero, return.
!
  IF ( LEN_TRIM ( S ) <= 0 ) THEN
    IERROR = 1
    RETURN
  END IF

  CALL NEXCHR ( S, ICHR, C )
!
!  If the next character is "I", then this number is 0+I.
!
  IF ( CH_EQI ( C, 'I' ) ) THEN
    AVAL = 0.0E+00
    BVAL = 1.0E+00
    LENGTH = LENGTH + ICHR
    CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
    RETURN
  END IF
!
!  OK, the next string has to be a number!
!
  CALL S_TO_R4 ( S, AVAL, IERROR, ICHR )

  IF ( IERROR /= 0 ) THEN
    IERROR = 2
    LENGTH = 0
    RETURN
  END IF

  LENGTH = LENGTH + ICHR
!
!  See if this is a pure real number, because:
!
!    1) There's no more input left.
!
  IF ( LEN_TRIM ( S(LENGTH+1:) ) == 0 ) THEN
    CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
    RETURN
  END IF
!
!    2) The last character read was a comma.
!
  IF ( S(LENGTH:LENGTH) == ',' .OR. S(LENGTH:LENGTH) == ';' ) THEN
    CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
    RETURN
  END IF
!
!  If the very next character is "I", then this is a pure
!  imaginary number!
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

  IF ( CH_EQI ( C, 'I' ) ) THEN
    BVAL = AVAL
    AVAL = 0.0E+00
    LENGTH = LENGTH + ICHR
    CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
    RETURN
  END IF
!
!  If the very next character is "*" and the one after that is
!  "I", then this is a pure imaginary number!
!
  IF ( C == '*' ) THEN

    CALL NEXCHR ( S(LENGTH+ICHR+1:), ICHR2, C2 )

    IF ( CH_EQI ( C2, 'I' ) ) THEN
      BVAL = AVAL
      AVAL = 0.0E+00
      LENGTH = LENGTH + ICHR + ICHR2
    END IF

    CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
    RETURN

  END IF
!
!  OK, now we've got A.  We have to be careful because the next
!  thing we see MIGHT be "+ I" or "- I" which we can't let CHRCTR
!  see, because it will have fits.  So let's check these two
!  possibilities.
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )
  CALL NEXCHR ( S(LENGTH+1+ICHR:), ICHR2, C2 )

  IF ( CH_EQI ( C2, 'I' ) ) THEN

    IF ( C == '+' ) THEN
      BVAL = 1
      LENGTH = LENGTH + ICHR + ICHR2
      CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
      RETURN
    ELSE IF ( C == '-' ) THEN
      BVAL = -1
      LENGTH = LENGTH + ICHR + ICHR2
      CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )
      RETURN
    END IF

  END IF
!
!  Read the next real number.
!
  CALL S_TO_R4 ( S(LENGTH+1:), BVAL, IERROR, ICHR )

  IF ( IERROR /= 0 ) THEN
    IERROR = 3
    LENGTH = 0
    RETURN
  END IF

  LENGTH = LENGTH + ICHR
!
!  If the next character is a "*", that's OK, advance past it.
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

  IF ( C == '*' ) THEN
    LENGTH = LENGTH + ICHR
  END IF
!
!  Now we really do want the next character to be "I".
!
  CALL NEXCHR ( S(LENGTH+1:), ICHR, C )

  IF ( S_NEQI ( C, 'I' ) ) THEN
    IERROR = 4
    LENGTH = 0
    RETURN
  END IF
!
!  Form the complex number.
!
  CVAL = CMPLX ( AVAL, BVAL, KIND = 4 )

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_CHVEC ( S, N, CHVEC )

!*****************************************************************************80
!
!! S_TO_CHVEC converts a string to a character vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 March 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string of characters.
!
!    Input/output, integer ( kind = 4 ) N.
!    if N is -1, extract characters from 1 to len(S);
!    if N is 0, extract characters up to the last nonblank;
!    if N is positive, extract characters from 1 to N.
!
!    On output, N is the number of characters successfully extracted.
!
!    Output, character CHVEC(N), the characters extracted from S.
!
  IMPLICIT NONE

  CHARACTER CHVEC(*)
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) N
  CHARACTER ( LEN = * ) S

  IF ( N <= - 1 ) THEN
    N = LEN ( S )
  ELSE IF ( N == 0 ) THEN
    N = LEN_TRIM ( S )
  ELSE
    N = MIN ( N, LEN ( S ) )
  END IF

  DO I = 1, N
    CHVEC(I) = S(I:I)
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_DATE ( S1, S2 )

!*****************************************************************************80
!
!! S_TO_DATE converts the F90 date string to a more usual format.
!
!  Example:
!
!    S1        S2
!    --------  ----------------
!    20010204  4 February 2001
!    17760704  4 July 1776
!    19520310  10 March 1952
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = 8 ) S1, the F90 date string returned by
!    the routine DATE_AND_TIME.
!
!    Output, character ( len = * ) S2, a more usual format for the date.
!    Allowing 16 characters for S2 should be sufficient for the
!    forseeable future.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) M
  CHARACTER ( LEN = 8 ) MONTH
  CHARACTER ( LEN = * ) S1
  CHARACTER ( LEN = * ) S2

  IF ( S1(7:7) == '0' ) THEN
    S2(1:1) = S1(8:8)
    I = 1
  ELSE
    S2(1:2) = S1(7:8)
    I = 2
  END IF

  I = I + 1
  S2(I:I) = ' '

  READ ( S1(5:6), '(i2)' ) M
  CALL I4_TO_MONTH_NAME ( M, MONTH )

  S2(I+1:) = MONTH
  I = I + LEN_TRIM ( MONTH )

  I = I + 1
  S2(I:I) = ' '

  S2(I+1:I+4) = S1(1:4)
  I = I + 4

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_DEC ( S, ITOP, IBOT, LENGTH )

!*****************************************************************************80
!
!! S_TO_DEC reads a number from a string, returning a decimal result.
!
!  Discussion:
!
!    The integer may be in real format, for example '2.25'.  It
!    returns ITOP and IBOT.  If the input number is an integer, ITOP
!    equals that integer, and IBOT is 1.  But in the case of 2.25,
!    the program would return ITOP = 225, IBOT = 100.
!
!    Legal input is
!
!          blanks,
!       2  initial sign,
!          blanks,
!       3  whole number,
!       4  decimal point,
!       5  fraction,
!       6  'E' or 'e' or 'D' or 'd', exponent marker,
!       7  exponent sign,
!       8  exponent,
!          blanks
!       9  comma or semicolon
!      10end of information
!
!  Example:
!
!    S                 ITOP      IBOT     Length  Meaning
!
!    '1'                  1         0          1        1
!    '     1   '          1         0          6        1
!    '1A'                 1         0          1        1
!    '12,34,56'          12         0          3       12
!    '  34 7'            34         0          4       34
!    '-1E2ABCD'          -1         2          4     -100
!    '-1X2ABCD'          -1         0          2       -1
!    ' 2E-1'              2        -1          5      0.2
!    '23.45'           2345        -2          5    23.45
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading begins at position 1 and
!    terminate when no more characters
!    can be read to form a legal integer.  Blanks, commas,
!    or other nonnumeric data will, in particular, cause
!    the conversion to halt.
!
!    Output, integer ( kind = 4 ) ITOP, the integer read from the string,
!    assuming that no negative exponents or fractional parts
!    were used.  Otherwise, the 'integer' is ITOP/IBOT.
!
!    Output, integer ( kind = 4 ) IBOT, the integer divisor required to
!    represent numbers which are in real format or have a
!    negative exponent.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters used.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) DIGIT
  INTEGER ( KIND = 4 ) EXPONENT
  INTEGER ( KIND = 4 ) EXPONENT_SIGN
  INTEGER ( KIND = 4 ) IBOT
  INTEGER ( KIND = 4 ) IHAVE
  INTEGER ( KIND = 4 ) ITERM
  INTEGER ( KIND = 4 ) ITOP
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) MANTISSA_SIGN
  CHARACTER ( LEN = * ) S

  ITOP = 0
  IBOT = 0

  IF ( LEN ( S ) <= 0 ) THEN
    LENGTH = 0
    RETURN
  END IF

  LENGTH = -1
  EXPONENT_SIGN = 0
  MANTISSA_SIGN = 1
  EXPONENT = 0
  IHAVE = 1
  ITERM = 0
!
!  Consider the next character in the string.
!
  DO

    LENGTH = LENGTH + 1
    C = S(LENGTH+1:LENGTH+1)
!
!  Blank.
!
    IF ( C == ' ' ) THEN

      IF ( IHAVE == 1 ) THEN

      ELSE IF ( IHAVE == 2 ) THEN

      ELSE
        ITERM = 1
      END IF
!
!  Comma or semicolon.
!
    ELSE IF ( C == ',' .OR. C == ';' ) THEN

      IF ( IHAVE /= 1 ) THEN
        ITERM = 1
        IHAVE = 9
        LENGTH = LENGTH + 1
      END IF
!
!  Minus sign.
!
    ELSE IF ( C == '-' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
        MANTISSA_SIGN = -1
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
        EXPONENT_SIGN = -1
      ELSE
        ITERM = 1
      END IF
!
!  Plus sign.
!
    ELSE IF ( C == '+' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
        MANTISSA_SIGN = +1
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
        EXPONENT_SIGN = +1
      ELSE
        ITERM = 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( IHAVE < 4 ) THEN
        IHAVE = 4
      ELSE
        ITERM = 1
      END IF
!
!  Exponent marker.
!
    ELSE IF ( S_EQI ( C, 'E' ) .OR. S_EQI ( C, 'D' ) ) THEN

      IF ( IHAVE < 6 ) THEN
        IHAVE = 6
      ELSE
        ITERM = 1
      END IF
!
!  Digit.
!
    ELSE IF ( CH_IS_DIGIT ( C ) ) THEN

      IF ( IHAVE <= 3 ) THEN
        IHAVE = 3
        CALL CH_TO_DIGIT ( C, DIGIT )
        ITOP = 10 * ITOP + DIGIT
      ELSE IF ( IHAVE <= 5 ) THEN
        IHAVE = 5
        CALL CH_TO_DIGIT ( C, DIGIT )
        ITOP = 10 * ITOP + DIGIT
        IBOT = IBOT - 1
      ELSE IF ( IHAVE <= 8 ) THEN
        IHAVE = 8
        CALL CH_TO_DIGIT ( C, DIGIT )
        EXPONENT = 10 * EXPONENT + DIGIT
      ELSE
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'S_TO_DEC: Fatal error!'
        WRITE ( *, '(a,i8)' ) '  IHAVE = ', IHAVE
        RETURN
      END IF
!
!  Anything else is regarded as a terminator.
!
    ELSE
      ITERM = 1
    END IF

    IF ( ITERM == 1 ) THEN
      EXIT
    END IF

    IF ( LEN ( S ) <= LENGTH + 1 ) THEN
      LENGTH = LEN ( S )
      EXIT
    END IF

  END DO
!
!  Number seems to have terminated.
!  Have we got a legal number?
!
  IF ( IHAVE == 1 ) THEN
    RETURN
  ELSE IF ( IHAVE == 2 .OR. IHAVE == 6 .OR. IHAVE == 7 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_TO_DEC - Serious error!'
    WRITE ( *, '(a)' ) '  Illegal or nonnumeric input:'
    WRITE ( *, '(a)' ) TRIM ( S )
    RETURN
  END IF
!
!  Normalize.
!
  IF ( 0 < ITOP ) THEN

    DO WHILE ( MOD ( ITOP, 10 ) == 0 )
      ITOP = ITOP / 10
      IBOT = IBOT + 1
    END DO

  END IF
!
!  Consolidate the number in the form ITOP * 10**IBOT.
!
  IBOT = IBOT + EXPONENT_SIGN * EXPONENT
  ITOP = MANTISSA_SIGN * ITOP

  IF ( ITOP == 0 ) THEN
   IBOT = 0
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_EBCDIC ( S )

!*****************************************************************************80
!
!! S_TO_EBCDIC converts a character string from ASCII to EBCDIC.
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
!    Input/output, character ( len = * ) S.  On input, the ASCII
!    string, on output, the EBCDIC string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN ( S )

  DO I = 1, S_LENGTH
    S(I:I) = CH_TO_EBCDIC ( S(I:I) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_FORMAT ( S, R, CODE, W, M )

!*****************************************************************************80
!
!! S_TO_FORMAT reads a FORTRAN format from a string.
!
!  Discussion:
!
!    This routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the format.  This routine is limited in its ability to
!    recognize FORTRAN formats.  In particular, we are only expecting
!    a single format specification, and cannot handle extra features
!    such as 'ES' and 'EN' codes, '5X' spacing, and so on.
!
!    Legal input is:
!
!       0 nothing
!       1 blanks
!       2 optional '('
!       3 blanks
!       4 optional repeat factor R
!       5 blanks
!       6 CODE ( 'A', 'B', 'E', 'F', 'G', 'I', 'L', 'O', 'Z', '*' )
!       7 blanks
!       8 width W
!       9 optional decimal point
!      10 optional mantissa M
!      11 blanks
!      12 optional ')'
!      13 blanks
!
!  Example:
!
!    S                 R   CODE   W    M
!
!    'I12              1   I      12   0
!    'E8.0'            1   E       8   0
!    'F10.5'           1   F      10   5
!    '2G14.6'          2   G      14   6
!    '*'               1   *      -1  -1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read.
!
!    Output, integer ( kind = 4 ) R, the repetition factor, which defaults to 1.
!
!    Output, character CODE, the format code.
!
!    Output, integer ( kind = 4 ) W, the field width.
!
!    Output, integer ( kind = 4 ) M, the mantissa width.
!
  IMPLICIT NONE

  CHARACTER C
  CHARACTER CODE
  INTEGER ( KIND = 4 ) D
  LOGICAL, PARAMETER ::  DEBUG = .TRUE.
  INTEGER ( KIND = 4 ), PARAMETER :: LEFT = 1
  INTEGER ( KIND = 4 ) M
  INTEGER ( KIND = 4 ) PAREN_SUM
  INTEGER ( KIND = 4 ) POS
  INTEGER ( KIND = 4 ) R
  INTEGER ( KIND = 4 ), PARAMETER :: RIGHT = -1
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  INTEGER ( KIND = 4 ) STATE
  INTEGER ( KIND = 4 ) W

  STATE = 0
  PAREN_SUM = 0
  POS = 0
  S_LENGTH = LEN_TRIM ( S )

  R = 0
  W = 0
  CODE = '?'
  M = 0

  DO WHILE ( POS < S_LENGTH )

    POS = POS + 1
    C = S(POS:POS)
!
!  BLANK character:
!
    IF ( C == ' ' ) THEN

      IF ( STATE == 4 ) THEN
        STATE = 5
      ELSE IF ( STATE == 6 ) THEN
        STATE = 7
      ELSE IF ( STATE == 10 ) THEN
        STATE = 11
      ELSE IF ( STATE == 12 ) THEN
        STATE = 13
      END IF
!
!  LEFT PAREN
!
    ELSE IF ( C == '(' ) THEN

      IF ( STATE < 2 ) THEN
        PAREN_SUM = PAREN_SUM + LEFT
      ELSE
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF
!
!  DIGIT (R, F, or W)
!
    ELSE IF ( CH_IS_DIGIT ( C ) ) THEN

      IF ( STATE <= 3 ) THEN
        STATE = 4
        CALL CH_TO_DIGIT ( C, R )
      ELSE IF ( STATE == 4 ) THEN
        CALL CH_TO_DIGIT ( C, D )
        R = 10 * R + D
      ELSE IF ( STATE == 6 .OR. STATE == 7 ) THEN
        IF ( CODE == '*' ) THEN
          IF ( DEBUG ) THEN
            WRITE ( *, '(a)' ) ' '
            WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
            WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
            WRITE ( *, '(a,i8)' ) '  Current code = "' // CODE // '".'
            WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
          END IF
          STATE = -1
          EXIT
        END IF
        STATE = 8
        CALL CH_TO_DIGIT ( C, W )
      ELSE IF ( STATE == 8 ) THEN
        CALL CH_TO_DIGIT ( C, D )
        W = 10 * W + D
      ELSE IF ( STATE == 9 ) THEN
        STATE = 10
        CALL CH_TO_DIGIT ( C, M )
      ELSE IF ( STATE == 10 ) THEN
        CALL CH_TO_DIGIT ( C, D )
        M = 10 * M + D
      ELSE
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF
!
!  DECIMAL POINT
!
    ELSE IF ( C == '.' ) THEN

      IF ( STATE == 8 ) THEN
        STATE = 9
      ELSE
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF
!
!  RIGHT PAREN
!
    ELSE IF ( C == ')' ) THEN

      PAREN_SUM = PAREN_SUM + RIGHT

      IF ( PAREN_SUM /= 0 ) THEN
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current paren sum = ', PAREN_SUM
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF

      IF ( STATE == 6 .AND. CODE == '*' ) THEN
        STATE = 12
      ELSE IF ( 6 <= STATE ) THEN
        STATE = 12
      ELSE
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF
!
!  Code
!
    ELSE IF ( CH_IS_FORMAT_CODE ( C ) ) THEN

      IF ( STATE < 6 ) THEN
        STATE = 6
        CODE = C
      ELSE
        IF ( DEBUG ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
          WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
          WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
        END IF
        STATE = -1
        EXIT
      END IF
!
!  Unexpected character
!
    ELSE

      IF ( DEBUG ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
        WRITE ( *, '(a,i8)' ) '  Current state = ', STATE
        WRITE ( *, '(a)' ) '  Input character = "' // C // '".'
      END IF
      STATE = -1
      EXIT

    END IF

  END DO

  IF ( PAREN_SUM /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
    WRITE ( *, '(a)' ) '  Parentheses mismatch.'
    RETURN
  END IF

  IF ( STATE < 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_TO_FORMAT - Fatal error!'
    WRITE ( *, '(a)' ) '  Parsing error.'
    RETURN
  END IF

  IF ( R == 0 ) THEN
    R = 1
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_HEX ( S, HEX )

!*****************************************************************************80
!
!! S_TO_HEX replaces a character string by a hexadecimal representation.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!    The string 'ABC' causes the hexadecimal string '414243' to be returned.
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
!    Input, character ( len = * ) S, the string of characters.
!
!    Output, character ( len = * ) HEX, the string of hex values.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) HEX
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INTVAL
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) NDO
  INTEGER ( KIND = 4 ) NHEX
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )
  NHEX = LEN ( HEX )

  NDO = MIN ( NHEX / 2, S_LENGTH )
  HEX = ' '

  DO I = 1, NDO

    J = 2 * I - 1
    INTVAL = IACHAR ( S(I:I) )

    CALL I4_TO_HEX ( INTVAL, HEX(J:J+1) )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_I4 ( S, VALUE, IERROR, LENGTH )

!*****************************************************************************80
!
!! S_TO_I4 reads an integer value from a string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) VALUE, the integer value read from the string.
!    If the string is blank, then VALUE will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters
!    of S used to make the integer.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) STATE
  CHARACTER :: TAB = ACHAR ( 9 )
  INTEGER ( KIND = 4 ) VALUE

  VALUE = 0
  IERROR = 0
  LENGTH = 0

  STATE = 0
  ISGN = 1

  DO I = 1, LEN_TRIM ( S )

    C = S(I:I)
!
!  STATE = 0, haven't read anything.
!
    IF ( STATE == 0 ) THEN

      IF ( C == ' ' .OR. C == TAB ) THEN

      ELSE IF ( C == '-' ) THEN
        STATE = 1
        ISGN = -1
      ELSE IF ( C == '+' ) THEN
        STATE = 1
        ISGN = +1
      ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN
        STATE = 2
        VALUE = IACHAR ( C ) - IACHAR ( '0' )
      ELSE
        IERROR = 1
        RETURN
      END IF
!
!  STATE = 1, have read the sign, expecting digits or spaces.
!
    ELSE IF ( STATE == 1 ) THEN

      IF ( C == ' ' .OR. C == TAB ) THEN

      ELSE IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN
        STATE = 2
        VALUE = IACHAR ( C ) - IACHAR ( '0' )
      ELSE
        IERROR = 1
        RETURN
      END IF
!
!  STATE = 2, have read at least one digit, expecting more.
!
    ELSE IF ( STATE == 2 ) THEN

      IF ( LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN

        VALUE = 10 * VALUE + IACHAR ( C ) - IACHAR ( '0' )

      ELSE

        VALUE = ISGN * VALUE
        IERROR = 0
        LENGTH = I - 1
        RETURN

      END IF

    END IF

  END DO
!
!  If we read all the characters in the string, see if we're OK.
!
  IF ( STATE == 2 ) THEN

    VALUE = ISGN * VALUE
    IERROR = 0
    LENGTH = LEN_TRIM ( S )

  ELSE

    VALUE = 0
    IERROR = 1
    LENGTH = 0

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_I4VEC ( S, N, I4VEC, IERROR )

!*****************************************************************************80
!
!! S_TO_I4VEC reads an integer vector from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Input, integer ( kind = 4 ) N, the number of values expected.
!
!    Output, integer ( kind = 4 ) I4VEC(N), the values read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    -K, could not read data for entries -K through N.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) I4VEC(N)
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = * ) S

  I = 0
  IERROR = 0
  ILO = 1

  DO WHILE ( I < N )

    I = I + 1

    CALL S_TO_I4 ( S(ILO:), I4VEC(I), IERROR, LENGTH )

    IF ( IERROR /= 0 ) THEN
      IERROR = -I
      EXIT
    END IF

    ILO = ILO + LENGTH

  END DO

  RETURN
END

!*************************************************************

FUNCTION S_TO_L ( S )

!*****************************************************************************80
!
!! S_TO_L reads a logical value from a string.
!
!  Discussion:
!
!    There are several ways of representing logical data that this routine
!    recognizes:
!
!      False   True
!      -----   ----
!
!      0       1
!      F       T
!      f       t
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 2010
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Output, logical S_TO_L, the logical value read from the string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  LOGICAL S_TO_L

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( S(I:I) == '0' .OR. S(I:I) == 'F' .OR. S(I:I) == 'f' ) THEN
      S_TO_L = .FALSE.
      RETURN
    ELSE IF ( S(I:I) == '1' .OR. S(I:I) == 'T' .OR. S(I:I) == 't' ) THEN
      S_TO_L = .TRUE.
      RETURN
    END IF
  END DO

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) 'S_TO_L - Fatal error!'
  WRITE ( *, '(a)' ) '  Input text did not contain logical data.'

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_R4 ( S, R, IERROR, LENGTH )

!*****************************************************************************80
!
!! S_TO_R4 reads an R4 value from a string.
!
!  Discussion:
!
!    An "R4" value is simply a real number to be stored as a
!    variable of type "real ( kind = 4 )".
!
!    This routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the real number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 spaces
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon.
!
!    with most quantities optional.
!
!  Example:
!
!    S                 R
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 4 ) R, the real value that was read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read from
!    the string to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IHAVE
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) ITERM
  INTEGER ( KIND = 4 ) JBOT
  INTEGER ( KIND = 4 ) JSGN
  INTEGER ( KIND = 4 ) JTOP
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) NDIG
  REAL ( KIND = 4 ) R
  REAL ( KIND = 4 ) RBOT
  REAL ( KIND = 4 ) REXP
  REAL ( KIND = 4 ) RTOP
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )

  S_LENGTH = LEN_TRIM ( S )
  IERROR = 0
  R = 0.0E+00
  LENGTH = -1
  ISGN = 1
  RTOP = 0.0E+00
  RBOT = 1.0E+00
  JSGN = 1
  JTOP = 0
  JBOT = 1
  IHAVE = 1
  ITERM = 0

  DO

    LENGTH = LENGTH + 1
    C = S(LENGTH+1:LENGTH+1)
!
!  Blank or TAB character.
!
    IF ( C == ' ' .OR. C == TAB ) THEN

      IF ( IHAVE == 2 ) THEN

      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        ITERM = 1
      ELSE IF ( 1 < IHAVE ) THEN
        IHAVE = 11
      END IF
!
!  Comma.
!
    ELSE IF ( C == ',' .OR. C == ';' ) THEN

      IF ( IHAVE /= 1 ) THEN
        ITERM = 1
        IHAVE = 12
        LENGTH = LENGTH + 1
      END IF
!
!  Minus sign.
!
    ELSE IF ( C == '-' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
        ISGN = -1
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
        JSGN = -1
      ELSE
        ITERM = 1
      END IF
!
!  Plus sign.
!
    ELSE IF ( C == '+' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
      ELSE
        ITERM = 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( IHAVE < 4 ) THEN
        IHAVE = 4
      ELSE IF ( 6 <= IHAVE .AND. IHAVE <= 8 ) THEN
        IHAVE = 9
      ELSE
        ITERM = 1
      END IF
!
!  Exponent marker.
!
    ELSE IF ( CH_EQI ( C, 'E' ) .OR. CH_EQI ( C, 'D' ) ) THEN

      IF ( IHAVE < 6 ) THEN
        IHAVE = 6
      ELSE
        ITERM = 1
      END IF
!
!  Digit.
!
    ELSE IF ( IHAVE < 11 .AND. LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN

      IF ( IHAVE <= 2 ) THEN
        IHAVE = 3
      ELSE IF ( IHAVE == 4 ) THEN
        IHAVE = 5
      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        IHAVE = 8
      ELSE IF ( IHAVE == 9 ) THEN
        IHAVE = 10
      END IF

      CALL CH_TO_DIGIT ( C, NDIG )

      IF ( IHAVE == 3 ) THEN
        RTOP = 10.0E+00 * RTOP + REAL ( NDIG, KIND = 4 )
      ELSE IF ( IHAVE == 5 ) THEN
        RTOP = 10.0E+00 * RTOP + REAL ( NDIG, KIND = 4 )
        RBOT = 10.0E+00 * RBOT
      ELSE IF ( IHAVE == 8 ) THEN
        JTOP = 10 * JTOP + NDIG
      ELSE IF ( IHAVE == 10 ) THEN
        JTOP = 10 * JTOP + NDIG
        JBOT = 10 * JBOT
      END IF
!
!  Anything else is regarded as a terminator.
!
    ELSE
      ITERM = 1
    END IF
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    IF ( ITERM == 1 .OR. S_LENGTH <= LENGTH + 1 ) THEN
      EXIT
    END IF

  END DO
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to S_LENGTH.
!
  IF ( ITERM /= 1 .AND. LENGTH + 1 == S_LENGTH ) THEN
    LENGTH = S_LENGTH
  END IF
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  IF ( IHAVE == 1 .OR. IHAVE == 2 .OR. IHAVE == 6 .OR. IHAVE == 7 ) THEN

    IERROR = IHAVE

    RETURN
  END IF
!
!  Number seems OK.  Form it.
!
  IF ( JTOP == 0 ) THEN
    REXP = 1.0E+00
  ELSE

    IF ( JBOT == 1 ) THEN
      REXP = 10.0E+00**( JSGN * JTOP )
    ELSE
      REXP = JSGN * JTOP
      REXP = REXP / JBOT
      REXP = 10.0E+00**REXP
    END IF

  END IF

  R = ISGN * REXP * RTOP / RBOT

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_R4VEC ( S, N, R4VEC, IERROR )

!*****************************************************************************80
!
!! S_TO_R4VEC reads an R4VEC from a string.
!
!  Discussion:
!
!    An R4VEC is a vector of real values, of type "real ( kind = 4 )".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Input, integer ( kind = 4 ) N, the number of values expected.
!
!    Output, real ( kind = 4 ) R4VEC(N), the values read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    -K, could not read data for entries -K through N.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) LENGTH
  REAL ( KIND = 4 ) R4VEC(N)
  CHARACTER ( LEN = * ) S

  I = 0
  IERROR = 0
  ILO = 1

  DO WHILE ( I < N )

    I = I + 1

    CALL S_TO_R4 ( S(ILO:), R4VEC(I), IERROR, LENGTH )

    IF ( IERROR /= 0 ) THEN
      IERROR = -I
      EXIT
    END IF

    ILO = ILO + LENGTH

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_R8 ( S, DVAL, IERROR, LENGTH )

!*****************************************************************************80
!
!! S_TO_R8 reads an R8 value from a string.
!
!  Discussion:
!
!    An "R8" value is simply a real number to be stored as a
!    variable of type "real ( kind = 8 )".
!
!    The routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 blanks
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon,
!
!    with most quantities optional.
!
!  Example:
!
!    S                 DVAL
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real ( kind = 8 ) DVAL, the value read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters read
!    to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
  IMPLICIT NONE

  CHARACTER C
  REAL ( KIND = 8 ) DVAL
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IHAVE
  INTEGER ( KIND = 4 ) ISGN
  INTEGER ( KIND = 4 ) ITERM
  INTEGER ( KIND = 4 ) JBOT
  INTEGER ( KIND = 4 ) JSGN
  INTEGER ( KIND = 4 ) JTOP
  INTEGER ( KIND = 4 ) LENGTH
  INTEGER ( KIND = 4 ) NDIG
  REAL ( KIND = 8 ) RBOT
  REAL ( KIND = 8 ) REXP
  REAL ( KIND = 8 ) RTOP
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER  :: TAB = ACHAR ( 9 )

  S_LENGTH = LEN_TRIM ( S )

  IERROR = 0
  DVAL = 0.0D+00
  LENGTH = -1
  ISGN = 1
  RTOP = 0
  RBOT = 1
  JSGN = 1
  JTOP = 0
  JBOT = 1
  IHAVE = 1
  ITERM = 0

  DO

    LENGTH = LENGTH + 1

    IF ( S_LENGTH < LENGTH + 1 ) THEN
      EXIT
    END IF

    C = S(LENGTH+1:LENGTH+1)
!
!  Blank character.
!
    IF ( C == ' ' .OR. C == TAB ) THEN

      IF ( IHAVE == 2 ) THEN

      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        ITERM = 1
      ELSE IF ( 1 < IHAVE ) THEN
        IHAVE = 11
      END IF
!
!  Comma.
!
    ELSE IF ( C == ',' .OR. C == ';' ) THEN

      IF ( IHAVE /= 1 ) THEN
        ITERM = 1
        IHAVE = 12
        LENGTH = LENGTH + 1
      END IF
!
!  Minus sign.
!
    ELSE IF ( C == '-' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
        ISGN = -1
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
        JSGN = -1
      ELSE
        ITERM = 1
      END IF
!
!  Plus sign.
!
    ELSE IF ( C == '+' ) THEN

      IF ( IHAVE == 1 ) THEN
        IHAVE = 2
      ELSE IF ( IHAVE == 6 ) THEN
        IHAVE = 7
      ELSE
        ITERM = 1
      END IF
!
!  Decimal point.
!
    ELSE IF ( C == '.' ) THEN

      IF ( IHAVE < 4 ) THEN
        IHAVE = 4
      ELSE IF ( 6 <= IHAVE .AND. IHAVE <= 8 ) THEN
        IHAVE = 9
      ELSE
        ITERM = 1
      END IF
!
!  Scientific notation exponent marker.
!
    ELSE IF ( CH_EQI ( C, 'E' ) .OR. CH_EQI ( C, 'D' ) ) THEN

      IF ( IHAVE < 6 ) THEN
        IHAVE = 6
      ELSE
        ITERM = 1
      END IF
!
!  Digit.
!
    ELSE IF (  IHAVE < 11 .AND. LLE ( '0', C ) .AND. LLE ( C, '9' ) ) THEN

      IF ( IHAVE <= 2 ) THEN
        IHAVE = 3
      ELSE IF ( IHAVE == 4 ) THEN
        IHAVE = 5
      ELSE IF ( IHAVE == 6 .OR. IHAVE == 7 ) THEN
        IHAVE = 8
      ELSE IF ( IHAVE == 9 ) THEN
        IHAVE = 10
      END IF

      CALL CH_TO_DIGIT ( C, NDIG )

      IF ( IHAVE == 3 ) THEN
        RTOP = 10.0D+00 * RTOP + REAL ( NDIG, KIND = 8 )
      ELSE IF ( IHAVE == 5 ) THEN
        RTOP = 10.0D+00 * RTOP + REAL ( NDIG, KIND = 8 )
        RBOT = 10.0D+00 * RBOT
      ELSE IF ( IHAVE == 8 ) THEN
        JTOP = 10 * JTOP + NDIG
      ELSE IF ( IHAVE == 10 ) THEN
        JTOP = 10 * JTOP + NDIG
        JBOT = 10 * JBOT
      END IF
!
!  Anything else is regarded as a terminator.
!
    ELSE
      ITERM = 1
    END IF
!
!  If we haven't seen a terminator, and we haven't examined the
!  entire string, go get the next character.
!
    IF ( ITERM == 1 ) THEN
      EXIT
    END IF

  END DO
!
!  If we haven't seen a terminator, and we have examined the
!  entire string, then we're done, and LENGTH is equal to S_LENGTH.
!
  IF ( ITERM /= 1 .AND. LENGTH + 1 == S_LENGTH ) THEN
    LENGTH = S_LENGTH
  END IF
!
!  Number seems to have terminated.  Have we got a legal number?
!  Not if we terminated in states 1, 2, 6 or 7!
!
  IF ( IHAVE == 1 .OR. IHAVE == 2 .OR. IHAVE == 6 .OR. IHAVE == 7 ) THEN
    IERROR = IHAVE
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'S_TO_R8 - Serious error!'
    WRITE ( *, '(a)' ) '  Illegal or nonnumeric input:'
    WRITE ( *, '(a)' ) '    ' // TRIM ( S )
    RETURN
  END IF
!
!  Number seems OK.  Form it.
!
  IF ( JTOP == 0 ) THEN
    REXP = 1.0D+00
  ELSE
    IF ( JBOT == 1 ) THEN
      REXP = 10.0D+00 ** ( JSGN * JTOP )
    ELSE
      REXP = 10.0D+00 ** ( REAL ( JSGN * JTOP, KIND = 8 ) &
        / REAL ( JBOT, KIND = 8 ) )
    END IF
  END IF

  DVAL = REAL ( ISGN, KIND = 8 ) * REXP * RTOP / RBOT

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_R8VEC ( S, N, R8VEC, IERROR )

!*****************************************************************************80
!
!! S_TO_R8VEC reads an R8VEC from a string.
!
!  Discussion:
!
!    An R8VEC is a vector of real values, of type "real ( kind = 8 )".
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be read.
!
!    Input, integer ( kind = 4 ) N, the number of values expected.
!
!    Output, real ( kind = 8 ) R8VEC(N), the values read from the string.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no errors occurred.
!    -K, could not read data for entries -K through N.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) LCHAR
  REAL ( KIND = 8 ) R8VEC(N)
  CHARACTER ( LEN = * ) S

  I = 0
  IERROR = 0
  ILO = 1

  DO WHILE ( I < N )

    I = I + 1

    CALL S_TO_R8 ( S(ILO:), R8VEC(I), IERROR, LCHAR )

    IF ( IERROR /= 0 ) THEN
      IERROR = -I
      EXIT
    END IF

    ILO = ILO + LCHAR

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_ROT13 ( S )

!*****************************************************************************80
!
!! S_TO_ROT13 "rotates" the alphabetical characters in a string by 13 positions.
!
!  Discussion:
!
!    Two applications of the routine will return the original string.
!
!  Example:
!
!    Input:                      Output:
!
!    abcdefghijklmnopqrstuvwxyz  nopqrstuvwxyzabcdefghijklm
!    Cher                        Pure
!    James Thurston Howell       Wnzrf Guhefgba Ubjryy
!    0123456789                  5678901234
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 March 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a string to be "rotated".
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    S(I:I) = CH_TO_ROT13 ( S(I:I) )
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_SOUNDEX ( S, CODE )

!*****************************************************************************80
!
!! S_TO_SOUNDEX computes the Soundex code of a string.
!
!  Example:
!
!    Input:                      Output:
!
!    Ellery                      E460
!    Euler                       E460
!    Gauss                       G200
!    Ghosh                       G200
!    Heilbronn                   H416
!    Hilbert                     H416
!    Kant                        K530
!    Knuth                       K530
!    Ladd                        L300
!    Lloyd                       L300
!    Lissajous                   L222
!    Lukasiewicz                 L222
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Donald Knuth,
!    The Art of Computer Programming,
!    Volume 3, Sorting and Searching,
!    Second Edition,
!    Addison Wesley, 1998,
!    ISBN: 0201896850,
!    LC: QA76.6.K64.
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be converted.
!
!    Output, character ( len = 4 ) CODE, the Soundex code for the string.
!
  IMPLICIT NONE

  CHARACTER C
  CHARACTER C_PUT
  CHARACTER CH_S
  CHARACTER CH_S_OLD
  CHARACTER ( LEN = 4 ) CODE
  INTEGER ( KIND = 4 ) GET
  INTEGER ( KIND = 4 ) NGET
  INTEGER ( KIND = 4 ) PUT
  CHARACTER ( LEN = * ) S

  CH_S = '0'
  CODE = ' '
  NGET = LEN_TRIM ( S )
  GET = 0
!
!  Try to fill position PUT of the code.
!
  DO PUT = 1, 4

    DO

      IF ( NGET <= GET ) THEN
        C_PUT = '0'
        EXIT
      END IF

      GET = GET + 1
      C = S(GET:GET)
      CALL CH_CAP ( C )

      IF ( .NOT. CH_IS_ALPHA ( C ) ) THEN
        CYCLE
      END IF

      CH_S_OLD = CH_S

      CALL CH_TO_SOUNDEX ( C, CH_S )

      IF ( PUT == 1 ) THEN
        C_PUT = C
        EXIT
      ELSE IF ( CH_S /= CH_S_OLD .AND. CH_S /= '0' ) THEN
        C_PUT = CH_S
        EXIT
      END IF

    END DO

    CODE(PUT:PUT) = C_PUT

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TO_W ( S, W, IERROR, LAST )

!*****************************************************************************80
!
!! S_TO_W reads the next blank-delimited word from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 November 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, character ( len = * ) W, the word that was read.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LAST, the last character of S used to make W.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LAST
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) STATE
  CHARACTER ( LEN = * ) W

  W = ' '
  IERROR = 0
  STATE = 0
  FIRST = 0
  LAST = 0
  I = 0

  DO

    I = I + 1

    IF ( LEN_TRIM ( S ) < I ) THEN

      IF ( STATE == 0 ) THEN
        IERROR = 1
        LAST = 0
      ELSE
        LAST = I-1
        W = S(FIRST:LAST)
      END IF

      EXIT

    END IF

    C = S(I:I)

    IF ( STATE == 0 ) THEN

      IF ( C /= ' ' ) THEN
        FIRST = I
        STATE = 1
      END IF

    ELSE IF ( STATE == 1 ) THEN

      IF ( C == ' ' ) THEN
        LAST = I - 1
        W = S(FIRST:LAST)
        EXIT
      END IF

    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_TRIM_ZEROS ( S )

!*****************************************************************************80
!
!! S_TRIM_ZEROS removes trailing zeros from a string.
!
!  Example:
!
!    Input:
!
!      S = '1401.072500'
!
!    Output:
!
!      S = '1401.0725'
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
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be operated on.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO WHILE ( 0 < S_LENGTH .AND. S(S_LENGTH:S_LENGTH) == '0' )
    S(S_LENGTH:S_LENGTH) = ' '
    S_LENGTH = S_LENGTH - 1
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_U2B ( S )

!*****************************************************************************80
!
!! S_U2B replaces underscores by blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be
!    transformed.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  DO I = 1, S_LENGTH
    IF ( S(I:I) == '_' ) THEN
      S(I:I) = ' '
    END IF
  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_APPEND ( S, W, DONE )

!*****************************************************************************80
!
!! S_WORD_APPEND appends a word to a string.
!
!  Discussion:
!
!    A blank space will separate the word from the text already
!    in the line.
!
!    The routine warns the user if the word will not fit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    04 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, a line of text.
!    On input, the current string.  On output, the current string
!    with the integer value appended.
!
!    Input, character ( len = * ) W, a word to be appended.
!    Trailing blanks in the word are ignored.
!
!    Output, logical DONE, is FALSE if there was not enough room
!    to append the word.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) LENS
  INTEGER ( KIND = 4 ) LENTS
  INTEGER ( KIND = 4 ) LENW
  INTEGER ( KIND = 4 ) NEXT
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) W

  DONE = .FALSE.
  LENS = LEN ( S )
  LENTS = LEN_TRIM ( S )

  LENW = LEN_TRIM ( W )

  IF ( LENTS == 0 ) THEN
    IF ( LENS < LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  ELSE
    IF ( LENS < LENTS + 1 + LENW ) THEN
      DONE = .TRUE.
      RETURN
    END IF
  END IF

  IF ( LENTS == 0 ) THEN
    NEXT = 1
  ELSE
    NEXT = LENTS + 1
    S(NEXT:NEXT) = ' '
    NEXT = NEXT + 1
  END IF

  S(NEXT:NEXT+LENW-1) = W(1:LENW)

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_CAP ( S )

!*****************************************************************************80
!
!! S_WORD_CAP capitalizes the first character of each word in a string.
!
!  Example:
!
!    Input:
!
!      S = 'it is time to turn the page.'
!
!    Output:
!
!      S = 'It Is Time To Turn The Page.'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 August 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
  IMPLICIT NONE

  LOGICAL BLANK
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH

  S_LENGTH = LEN_TRIM ( S )

  BLANK = .TRUE.

  DO I = 1, S_LENGTH

    IF ( BLANK ) THEN
      CALL CH_CAP ( S(I:I) )
    ELSE
      CALL CH_LOW ( S(I:I) )
    END IF

    BLANK = ( S(I:I) == ' ' )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_COUNT ( S, WORD_NUM )

!*****************************************************************************80
!
!! S_WORD_COUNT counts the number of "words" in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 February 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be examined.
!
!    Output, integer ( kind = 4 ) WORD_NUM, the number of "words" in the
!    string.  Words are presumed to be separated by one or more blanks.
!
  IMPLICIT NONE

  LOGICAL BLANK
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
  INTEGER ( KIND = 4 ) WORD_NUM

  WORD_NUM = 0
  S_LENGTH = LEN ( S )

  IF ( S_LENGTH <= 0 ) THEN
    RETURN
  END IF

  BLANK = .TRUE.

  DO I = 1, S_LENGTH

    IF ( S(I:I) == ' ' .OR. S(I:I) == TAB ) THEN
      BLANK = .TRUE.
    ELSE IF ( BLANK ) THEN
      WORD_NUM = WORD_NUM + 1
      BLANK = .FALSE.
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_EXTRACT_FIRST ( S, W )

!*****************************************************************************80
!
!! S_WORD_EXTRACT_FIRST extracts the first word from a string.
!
!  Discussion:
!
!    A "word" is a string of characters terminated by a blank or
!    the end of the string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 January 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string.  On output, the first
!    word has been removed, and the remaining string has been shifted left.
!
!    Output, character ( len = * ) W, the leading word of the string.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) GET1
  INTEGER ( KIND = 4 ) GET2
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LENGTH
  CHARACTER ( LEN = * ) W

  W = ' '

  S_LENGTH = LEN_TRIM ( S )

  IF ( S_LENGTH < 1 ) THEN
    RETURN
  END IF
!
!  Find the first nonblank.
!
  GET1 = 0

  DO

    GET1 = GET1 + 1

    IF ( S_LENGTH < GET1 ) THEN
      RETURN
    END IF

    IF ( S(GET1:GET1) /= ' ' ) THEN
      EXIT
    END IF

  END DO
!
!  Look for the last contiguous nonblank.
!
  GET2 = GET1

  DO

    IF ( S_LENGTH <= GET2 ) THEN
      EXIT
    END IF

    IF ( S(GET2+1:GET2+1) == ' ' ) THEN
      EXIT
    END IF

    GET2 = GET2 + 1

  END DO
!
!  Copy the word.
!
  W = S(GET1:GET2)
!
!  Shift the string.
!
  S(1:GET2) = ' '
  S = ADJUSTL ( S )

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_FIND ( S, IWORD, WORD, NCHAR )

!*****************************************************************************80
!
!! S_WORD_FIND finds the word of a given index in a string.
!
!  Discussion:
!
!    A "word" is any string of nonblank characters, separated from other
!    words by one or more blanks or TABS.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be searched.
!
!    Input, integer ( kind = 4 ) IWORD, the index of the word to be
!    searched for.  If IWORD is positive, then the IWORD-th
!    word is sought.  If IWORD is zero or negative, then
!    assuming that the string has N words in it, the
!    N+IWORD-th word will be sought.
!
!    Output, character ( len = * ) WORD, the IWORD-th word of the
!    string, or ' ' if the WORD could not be found.
!
!    Output, integer ( kind = 4 ) NCHAR, the number of characters in WORD,
!    or 0 if the word could not be found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IBLANK
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IWORD
  INTEGER ( KIND = 4 ) JHI
  INTEGER ( KIND = 4 ) JLO
  INTEGER ( KIND = 4 ) JWORD
  INTEGER ( KIND = 4 ) KWORD
  INTEGER ( KIND = 4 ) NCHAR
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LEN
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
  CHARACTER ( LEN = * ) WORD

  ILO = 0
  IHI = 0
  S_LEN = LEN_TRIM ( S )
  NCHAR = 0
  WORD = ''

  IF ( S_LEN <= 0 ) THEN
    RETURN
  END IF

  IF ( 0 < IWORD ) THEN

    IF ( S(1:1) == ' ' .OR. S(1:1) == TAB ) THEN
      IBLANK = 1
      JWORD = 0
      JLO = 0
      JHI = 0
    ELSE
      IBLANK = 0
      JWORD = 1
      JLO = 1
      JHI = 1
    END IF

    I = 1

    DO

      I = I + 1

      IF ( S_LEN < I ) THEN

        IF ( JWORD == IWORD ) THEN
          ILO = JLO
          IHI = S_LEN
          NCHAR = S_LEN + 1 - JLO
          WORD = S(ILO:IHI)
        ELSE
          ILO = 0
          IHI = 0
          NCHAR = 0
          WORD = ' '
        END IF

        RETURN

      END IF

      IF ( ( S(I:I) == ' ' .OR. S(I:I) == TAB ) .AND. IBLANK == 0 ) THEN

        JHI = I - 1
        IBLANK = 1
        IF ( JWORD == IWORD ) THEN
          ILO = JLO
          IHI = JHI
          NCHAR = JHI + 1 - JLO
          WORD = S(ILO:IHI)
          RETURN
        END IF

      ELSE IF ( S(I:I) /= ' ' .AND. S(I:I) /= TAB .AND. IBLANK == 1 ) THEN

        JLO = I
        JWORD = JWORD + 1
        IBLANK = 0

      END IF

    END DO

  ELSE

    IBLANK = 0
    KWORD = 1 - IWORD
    JWORD = 1
    JLO = S_LEN
    JHI = S_LEN
    I = S_LEN

    DO

      I = I - 1

      IF ( I <= 0 ) THEN

        IF ( JWORD == KWORD ) THEN
          ILO = 1
          IHI = JHI
          NCHAR = JHI
          WORD = S(ILO:IHI)
        ELSE
          ILO = 0
          IHI = 0
          NCHAR = 0
          WORD = ' '
        END IF

        RETURN

      END IF

      IF ( ( S(I:I) == ' ' .OR. S == TAB ) .AND. IBLANK == 0 ) THEN

        JLO = I + 1
        IBLANK = 1

        IF ( JWORD == KWORD ) THEN
          ILO = JLO
          IHI = JHI
          NCHAR = JHI + 1 - JLO
          WORD = S(ILO:IHI)
          RETURN
        END IF

      ELSE IF ( S(I:I) /= ' ' .AND. S(I:I) /= TAB .AND. IBLANK == 1 ) THEN

        JHI = I
        JWORD = JWORD + 1
        IBLANK = 0

      END IF

    END DO

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_INDEX ( S, INDX, ILO, IHI )

!*****************************************************************************80
!
!! S_WORD_INDEX finds the word of a given index in a string.
!
!  Discussion:
!
!    The routine returns in ILO and IHI the beginning and end of the INDX-th
!    word, or 0 and 0 if there is no INDX-th word.
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
!    Input, character ( len = * ) S is the string of words to be analyzed.
!
!    Input, integer ( kind = 4 ) INDX is the index of the desired token.
!
!    Output, integer ( kind = 4 ) ILO is the index of the first character
!    of the INDX-th word, or 0 if there was no INDX-th word.
!
!    Output, integer ( kind = 4 ) IHI is the index of the last character
!    of the INDX-th word, or 0 if there was no INDX-th word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) INDX
  CHARACTER ( LEN = * ) S

  IHI = 0
  ILO = 0

  DO I = 1, INDX

    CALL WORD_NEXT ( S, ILO, IHI )

    IF ( ILO == 0 ) THEN
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_NEXT ( S, WORD, DONE )

!*****************************************************************************80
!
!! S_WORD_NEXT "reads" words from a string, one at a time.
!
!  Special cases:
!
!    The following characters are considered to be a single word,
!    whether surrounded by spaces or not:
!
!      " ( ) { } [ ]
!
!    Also, if there is a trailing comma on the word, it is stripped off.
!    This is to facilitate the reading of lists.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string, presumably containing words
!    separated by spaces.
!
!    Output, character ( len = * ) WORD.
!    If DONE is FALSE, then WORD contains the "next" word read.
!    If DONE is TRUE, then WORD is blank, because there was no more to read.
!
!    Input/output, logical DONE.
!    On input with a fresh string, set DONE to TRUE.
!    On output, the routine sets DONE:
!      FALSE if another word was read,
!      TRUE if no more words could be read.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ), SAVE :: NEXT = 1
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ), SAVE :: S_LENGTH = 0
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
  CHARACTER ( LEN = * ) WORD
!
!  We "remember" S_LENGTH and NEXT from the previous call.
!
!  An input value of DONE = TRUE signals a new line of text to examine.
!
  IF ( DONE ) THEN

    NEXT = 1
    DONE = .FALSE.
    S_LENGTH = LEN_TRIM ( S )

    IF ( S_LENGTH <= 0 ) THEN
      DONE = .TRUE.
      WORD = ' '
      RETURN
    END IF

  END IF
!
!  Beginning at index NEXT, search the string for the next nonblank,
!  which signals the beginning of a word.
!
  ILO = NEXT
!
!  ...S(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
!
  DO

    IF ( S_LENGTH < ILO ) THEN
      WORD = ' '
      DONE = .TRUE.
      NEXT = S_LENGTH + 1
      RETURN
    END IF
!
!  If the current character is blank, skip to the next one.
!
    IF ( S(ILO:ILO) /= ' ' .AND. S(ILO:ILO) /= TAB ) THEN
      EXIT
    END IF

    ILO = ILO + 1

  END DO
!
!  ILO is the index of the next nonblank character in the string.
!
!  If this initial nonblank is a special character,
!  then that's the whole word as far as we're concerned,
!  so return immediately.
!
  IF ( S(ILO:ILO) == '"' .OR. &
       S(ILO:ILO) == '(' .OR. &
       S(ILO:ILO) == ')' .OR. &
       S(ILO:ILO) == '{' .OR. &
       S(ILO:ILO) == '}' .OR. &
       S(ILO:ILO) == '[' .OR. &
       S(ILO:ILO) == ']' ) THEN

    WORD = S(ILO:ILO)
    NEXT = ILO + 1
    RETURN

  END IF
!
!  Now search for the last contiguous character that is not a
!  blank, TAB, or special character.
!
  NEXT = ILO + 1

  DO WHILE ( NEXT <= S_LENGTH )

    IF ( S(NEXT:NEXT) == ' ' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == TAB ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '"' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '(' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == ')' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '{' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '}' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '[' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == ']' ) THEN
      EXIT
    END IF

    NEXT = NEXT + 1

  END DO
!
!  Ignore a trailing comma.
!
  IF ( S(NEXT-1:NEXT-1) == ',' ) THEN
    WORD = S(ILO:NEXT-2)
  ELSE
    WORD = S(ILO:NEXT-1)
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE S_WORD_PERMUTE ( S1, N, PERM, S2 )

!*****************************************************************************80
!
!! S_WORD_PERMUTE permutes the words in a string.
!
!  Discussion:
!
!    A word is a blank-delimited sequence of characters.
!
!    The string is assumed to contain N "words".  If more words are
!    in the string, their position is not affected.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 January 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S1, a line of text.
!
!    Input, integer ( kind = 4 ) N, the number of words to permute.
!
!    Input, integer ( kind = 4 ) PERM(N), the permutation.  PERM(1) is the new
!    location of the item whose original location was 1.
!
!    Output, character ( len = * ) S2, a copy of S1 with the
!    first N words permuted.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER C1
  CHARACTER C2
  INTEGER ( KIND = 4 ) INDEX1
  INTEGER ( KIND = 4 ) INDEX2
  INTEGER ( KIND = 4 ) PERM(N)
  INTEGER ( KIND = 4 ) PERM_INV(N)
  CHARACTER ( LEN = * ) S1
  INTEGER ( KIND = 4 ) S1_LENGTH
  INTEGER ( KIND = 4 ) S1_POS
  INTEGER ( KIND = 4 ) S1_WORD_INDEX(N)
  INTEGER ( KIND = 4 ) S1_WORD_LENGTH(N)
  CHARACTER ( LEN = * ) S2
  INTEGER ( KIND = 4 ) S2_POS
  INTEGER ( KIND = 4 ) WORD_LENGTH
!
!  Set up word position and length vectors.
!
  S1_LENGTH = LEN ( S1 )

  S1_WORD_LENGTH(1:N) = 0
  S1_WORD_INDEX(1:N) = 0

  INDEX1 = 0
  C2 = ' '

  DO S1_POS = 1, S1_LENGTH

    C1 = C2
    C2 = S1(S1_POS:S1_POS)

    IF ( S1_POS == 1 .OR. ( C1 /= ' ' .AND. C2 == ' ' ) ) THEN

      IF ( N <= INDEX1 ) THEN
        EXIT
      END IF

      INDEX1 = INDEX1 + 1

      S1_WORD_INDEX(INDEX1) = S1_POS

    END IF

    S1_WORD_LENGTH(INDEX1) = S1_WORD_LENGTH(INDEX1) + 1

  END DO
!
!  Invert the permutation.
!
  CALL PERM_INVERSE3 ( N, PERM, PERM_INV )
!
!  Copy S1 into S2, so we get any trailing information.
!
  CALL S_COPY ( S1, S2 )
!
!  Copy the first N words of S1 into S2 in permuted order.
!
  S2_POS = 1

  DO INDEX2 = 1, N

    INDEX1 = PERM_INV(INDEX2)

    S1_POS = S1_WORD_INDEX(INDEX1)

    WORD_LENGTH = S1_WORD_LENGTH(INDEX1)

    S2(S2_POS:S2_POS+WORD_LENGTH-1) = S1(S1_POS:S1_POS+WORD_LENGTH-1)

    S2_POS = S2_POS + WORD_LENGTH

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE TOKEN_EXPAND ( S, TOKENS )

!*****************************************************************************80
!
!! TOKEN_EXPAND makes sure certain tokens have spaces surrounding them.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be examined.
!
!    Input, character ( len = * ) TOKENS, a string of characters.  Every
!    occurrence of a character from TOKENS in S must be
!    preceded and followed by a blank space, except if the occurrence
!    is in the first or last positions of S, in which a
!    preceding or trailing blank space is implicit.
!
  IMPLICIT NONE

  CHARACTER  C1
  CHARACTER  C2
  CHARACTER  C3
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) PUT
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) LENC
  INTEGER ( KIND = 4 ) LENT
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 255 ) S2
  CHARACTER ( LEN = * ) TOKENS

  LENC = LEN_TRIM ( S )
  LENT = LEN_TRIM ( TOKENS )
  S2 = ' '
  PUT = 0
  C2 = ' '
  C3 = S(1:1)

  DO I = 1, LENC

    C1 = C2
    C2 = C3

    IF ( I < LENC ) THEN
      C3 = S(I+1:I+1)
    ELSE
      C3 = ' '
    END IF

    DO J = 1, LENT

      IF ( C2 == TOKENS(J:J) ) THEN
        IF ( C1 /= ' ' ) THEN
          PUT = PUT + 1
          IF ( PUT <= 255 ) THEN
            S2(PUT:PUT) = ' '
          END IF
        END IF
      END IF

    END DO

    PUT = PUT + 1

    IF ( PUT <= 255 ) THEN
      S2(PUT:PUT) = C2
    END IF

    DO J = 1, LENT

      IF ( C2 == TOKENS(J:J) ) THEN
        IF ( C3 /= ' ' ) THEN
          PUT = PUT + 1
          IF ( PUT <= 255 ) THEN
            S2(PUT:PUT) = ' '
          END IF
        END IF
      END IF

    END DO

  END DO

  S = S2

  RETURN
END

!*************************************************************

SUBROUTINE TOKEN_EXTRACT ( S, TOKEN_NUM, TOKEN, MATCH )

!*****************************************************************************80
!
!! TOKEN_EXTRACT "extracts" a token from the beginning of a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S; on input, a string from
!    whose beginning a token is to be extracted.  On output,
!    the token, if found, has been removed.
!
!    Input, integer ( kind = 4 ) TOKEN_NUM, the number of tokens to be
!    compared.
!
!    Input, character ( len = * ) TOKEN(TOKEN_NUM), the tokens.
!
!    Output, integer ( kind = 4 ) MATCH, the index of the (longest) token
!    that matched the string, or 0 if no match was found.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) TOKEN_NUM

  INTEGER ( KIND = 4 ) LEFT
  INTEGER ( KIND = 4 ) MATCH
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) TOKEN(TOKEN_NUM)

  CALL S_TOKEN_MATCH ( S, TOKEN_NUM, TOKEN, MATCH )

  IF ( MATCH /= 0 ) THEN
    LEFT = LEN_TRIM ( TOKEN(MATCH) )
    CALL S_SHIFT_LEFT ( S, LEFT )
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE TOKEN_INDEX ( S, INDX, ILO, IHI )

!*****************************************************************************80
!
!! TOKEN_INDEX finds the N-th FORTRAN variable name in a string.
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
!    Input, character ( len = * ) S is the string of words to be analyzed.
!
!    Input, integer ( kind = 4 ) INDX is the index of the desired token.
!
!    Output, integer ( kind = 4 ) ILO is the index of the first character
!    of the INDX-th token, or 0 if there was no INDX-th token.
!
!    Output, integer ( kind = 4 ) IHI is the index of the last character
!    of the INDX-th token, or 0 if there was no INDX-th token.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) INDX
  CHARACTER ( LEN = * ) S

  IHI = 0
  ILO = 0

  DO I = 1, INDX

    CALL TOKEN_NEXT ( S, ILO, IHI)

    IF ( ILO == 0 ) THEN
      RETURN
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE TOKEN_NEXT ( S, ILO, IHI )

!*****************************************************************************80
!
!! TOKEN_NEXT finds the next FORTRAN variable name in a string.
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
!  Parameters:
!
!    Input, character ( len = * ) S is the string of words to be analyzed.
!
!    Output, integer ( kind = 4 ) ILO is the location of the first character
!    of the next word, or 0 if there was no next word.
!
!    Input/output, integer ( kind = 4 ) IHI.
!    On input, IHI is taken to be the LAST character of the
!    PREVIOUS word, or 0 if the first word is sought.
!
!    On output, IHI is the index of the last character of
!    the next word, or 0 if there was no next word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LEN

  S_LEN = LEN_TRIM ( S )

  ILO = IHI

  IF ( ILO < 0 ) THEN
    ILO = 0
  END IF
!
!  Find ILO, the index of the next alphabetic character.
!
  DO

    ILO = ILO + 1

    IF ( S_LEN < ILO ) THEN
      ILO = 0
      IHI = 0
      RETURN
    END IF

    IF ( S_ONLY_ALPHAB ( S(ILO:ILO) ) ) THEN
      EXIT
    END IF

  END DO
!
!  Find the index of the next character which is neither
!  alphabetic nor numeric.
!
  IHI = ILO

  DO

    IHI = IHI + 1

    IF ( S_LEN < IHI ) THEN
      IHI = S_LEN
      RETURN
    END IF

    IF ( .NOT. ( S_ONLY_ALPHAB ( S(IHI:IHI) ) ) .AND. &
         .NOT. ( S_ONLY_DIGITB ( S(IHI:IHI) ) ) ) THEN
      EXIT
    END IF

  END DO

  IHI = IHI - 1

  RETURN
END

!*************************************************************

SUBROUTINE WORD_BOUNDS ( LINE, WORD_NUM, WORD_START, WORD_END )

!*****************************************************************************80
!
!! WORD_BOUNDS returns the start and end of each word in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 October 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string containing words
!    separated by spaces.
!
!    Input, integer ( kind = 4 ) WORD_NUM, the number of words in the line.
!
!    Output, integer ( kind = 4 ) WORD_START(WORD_NUM), WORD_END(WORD_NUM),
!    the locations in LINE of the beginning and end of each word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) WORD_NUM

  LOGICAL BLANK
  CHARACTER C
  LOGICAL, PARAMETER :: DEBUG = .TRUE.
  INTEGER ( KIND = 4 ) I
  CHARACTER ( LEN = * ) LINE
  INTEGER ( KIND = 4 ) LINE_LEN
  INTEGER ( KIND = 4 ) W
  INTEGER ( KIND = 4 ) WORD_END(WORD_NUM)
  INTEGER ( KIND = 4 ) WORD_START(WORD_NUM)

  I = 0
  W = 0
  BLANK = .TRUE.

  LINE_LEN = LEN_TRIM ( LINE )

  DO I = 1, LINE_LEN + 1

    IF ( I <= LINE_LEN ) THEN
      C = LINE(I:I)
    ELSE
      C = ' '
    END IF

    IF ( C == ' ' ) THEN

      IF ( .NOT. BLANK ) THEN
        WORD_END(W) = I-1
        IF ( W == WORD_NUM ) THEN
          EXIT
        END IF
      END IF

      BLANK = .TRUE.

    ELSE

      IF ( BLANK ) THEN
        W = W + 1
        WORD_START(W) = I
      END IF

      BLANK = .FALSE.

    END IF

  END DO

  IF ( W /= WORD_NUM ) THEN
    IF ( DEBUG ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'WORD_BOUNDS - Warning:'
      WRITE ( *, '(a)' ) '  Found fewer words than requested.'
    END IF
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE WORD_LAST_READ ( S, WORD )

!*****************************************************************************80
!
!! WORD_LAST_READ returns the last word from a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string containing words separated
!    by spaces.
!
!    Output, character ( len = * ) WORD, the last word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FIRST
  INTEGER ( KIND = 4 ) LAST
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = * ) WORD

  LAST = LEN_TRIM ( S )

  IF ( LAST <= 0 ) THEN
    WORD = ' '
    RETURN
  END IF

  FIRST = LAST

  DO

    IF ( FIRST <= 1 ) THEN
      EXIT
    END IF

    IF ( S(FIRST-1:FIRST-1) == ' ' ) THEN
      EXIT
    END IF

    FIRST = FIRST - 1

  END DO

  WORD = S(FIRST:LAST)

  RETURN
END

!*************************************************************

SUBROUTINE WORD_NEXT ( S, ILO, IHI )

!*****************************************************************************80
!
!! WORD_NEXT finds the next (blank separated) word in a string.
!
!  Discussion:
!
!    This routine is usually used repetitively on a fixed string.  On each
!    call, it accepts IHI, the index of the last character of the
!    previous word extracted from the string.
!
!    It then computes ILO and IHI, the first and last characters of
!    the next word in the string.
!
!    It is assumed that words are separated by one or more spaces.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string of words to be analyzed.
!
!    Output, integer ( kind = 4 ) ILO is the location of the first character
!    of the next word, or 0 if there was no next word.
!
!    Input/output, integer ( kind = 4 ) IHI.
!    On input, IHI is taken to be the LAST character of the
!    PREVIOUS word, or 0 if the first word is sought.
!    On output, IHI is the index of the last character of
!    the next word, or 0 if there was no next word.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  CHARACTER ( LEN = * ) S
  INTEGER ( KIND = 4 ) S_LEN

  S_LEN = LEN_TRIM ( S )
!
!  Find ILO, the index of the first nonblank character after
!  (the old value of) IHI.
!
  IF ( IHI < 0 ) THEN
    ILO = 0
  ELSE
    ILO = IHI
  END IF

  DO

    ILO = ILO + 1

    IF ( S_LEN < ILO ) THEN
      ILO = 0
      IHI = 0
      RETURN
    END IF

    IF ( S(ILO:ILO) /= ' ') THEN
      EXIT
    END IF

  END DO
!
!  Find IHI, the index of the next blank character, or end of line.
!
  IHI = ILO

  DO

    IHI = IHI + 1

    IF ( S_LEN <= IHI ) THEN
      IHI = S_LEN
      RETURN
    END IF

    IF ( S(IHI:IHI) == ' ' ) THEN
      EXIT
    END IF

  END DO
!
!  Decrement IHI to point to the previous, nonblank, character.
!
  IHI = IHI - 1

  RETURN
END

!*************************************************************

SUBROUTINE WORD_NEXT2 ( S, FIRST, LAST )

!*****************************************************************************80
!
!! WORD_NEXT2 returns the first word in a string.
!
!  Discussion:
!
!    "Words" are any string of characters, separated by commas or blanks.
!
!    The routine returns:
!    * FIRST, the first string of nonblank, noncomma characters;
!    * LAST, the characters of the string that occur after FIRST and
!      the commas and blanks.
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
!    Input, character ( len = * ) S, the string of words to be analyzed.
!
!    Output, character ( len = * ) FIRST, the next word in the string.
!
!    Output, character ( len = * ) LAST, the remaining string.
!
  IMPLICIT NONE

  CHARACTER C
  CHARACTER ( LEN = * ) FIRST
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IDO
  INTEGER ( KIND = 4 ) IFIRST
  INTEGER ( KIND = 4 ) ILAST
  CHARACTER ( LEN = * ) LAST
  INTEGER ( KIND = 4 ) LENF
  INTEGER ( KIND = 4 ) LENL
  INTEGER ( KIND = 4 ) LENS
  CHARACTER ( LEN = * ) S

  FIRST = ' '
  LAST = ' '

  IFIRST = 0
  ILAST = 0

  LENS = LEN_TRIM ( S )
  LENF = LEN ( FIRST )
  LENL = LEN ( LAST )

  IDO = 0

  DO I = 1, LENS

    C = S(I:I)

    IF ( IDO == 0 ) THEN
      IF ( C /= ' ' .AND. C /= ',' ) THEN
        IDO = 1
      END IF
    END IF

    IF ( IDO == 1 ) THEN
      IF ( C /= ' ' .AND. C /= ',' ) THEN
        IFIRST = IFIRST + 1
        IF ( IFIRST <= LENF ) THEN
          FIRST(IFIRST:IFIRST) = C
        END IF
      ELSE
        IDO = 2
      END IF
    END IF

    IF ( IDO == 2 ) THEN
      IF ( C /= ' ' .AND. C /= ',' ) THEN
        IDO = 3
      END IF
    END IF

    IF ( IDO == 3 ) THEN
      ILAST = ILAST + 1
      IF ( ILAST <= LENL ) THEN
        LAST(ILAST:ILAST) = C
      END IF
    END IF

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE WORD_NEXT_READ ( S, WORD, DONE )

!*****************************************************************************80
!
!! WORD_NEXT_READ "reads" words from a string, one at a time.
!
!  Discussion:
!
!    This routine was written to process tokens in a file.
!    A token is considered to be an alphanumeric string delimited
!    by whitespace, or any of various "brackets".
!
!    The following characters are considered to be a single word,
!    whether surrounded by spaces or not:
!
!      " ( ) { } [ ]
!
!    Also, if there is a trailing comma on the word, it is stripped off.
!    This is to facilitate the reading of lists.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string, presumably containing words
!    separated by spaces.
!
!    Output, character ( len = * ) WORD.
!    If DONE is FALSE, then WORD contains the "next" word read.
!    If DONE is TRUE, then WORD is blank, because there was no more to read.
!
!    Input/output, logical DONE.
!    On input with a fresh string, set DONE to TRUE.
!    On output, the routine sets DONE:
!      FALSE if another word was read,
!      TRUE if no more words could be read.
!
  IMPLICIT NONE

  LOGICAL DONE
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ), SAVE :: LENC = 0
  INTEGER ( KIND = 4 ), SAVE :: NEXT = 1
  CHARACTER ( LEN = * ) S
  CHARACTER, PARAMETER :: TAB = ACHAR ( 9 )
  CHARACTER ( LEN = * ) WORD
!
!  We "remember" LENC and NEXT from the previous call.
!
!  An input value of DONE = TRUE signals a new line of text to examine.
!
  IF ( DONE ) THEN

    NEXT = 1
    DONE = .FALSE.
    LENC = LEN_TRIM ( S )

    IF ( LENC <= 0 ) THEN
      DONE = .TRUE.
      WORD = ' '
      RETURN
    END IF

  END IF
!
!  Beginning at index NEXT, search the string for the next nonblank,
!  which signals the beginning of a word.
!
  ILO = NEXT
!
!  ...S(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
!
  DO

    IF ( LENC < ILO ) THEN
      WORD = ' '
      DONE = .TRUE.
      NEXT = LENC + 1
      RETURN
    END IF
!
!  If the current character is blank, skip to the next one.
!
    IF ( S(ILO:ILO) /= ' ' .AND. S(ILO:ILO) /= TAB ) THEN
      EXIT
    END IF

    ILO = ILO + 1

  END DO
!
!  ILO is the index of the next nonblank character in the string.
!
!  If this initial nonblank is a special character,
!  then that's the whole word as far as we're concerned,
!  so return immediately.
!
  IF ( S(ILO:ILO) == '"' .OR. &
       S(ILO:ILO) == '(' .OR. &
       S(ILO:ILO) == ')' .OR. &
       S(ILO:ILO) == '{' .OR. &
       S(ILO:ILO) == '}' .OR. &
       S(ILO:ILO) == '[' .OR. &
       S(ILO:ILO) == ']' ) THEN

    WORD = S(ILO:ILO)
    NEXT = ILO + 1
    RETURN

  END IF
!
!  Now search for the last contiguous character that is not a
!  blank, TAB, or special character.
!
  NEXT = ILO + 1

  DO WHILE ( NEXT <= LENC )

    IF ( S(NEXT:NEXT) == ' ' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == TAB ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '"' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '(' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == ')' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '{' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '}' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == '[' ) THEN
      EXIT
    ELSE IF ( S(NEXT:NEXT) == ']' ) THEN
      EXIT
    END IF

    NEXT = NEXT + 1

  END DO

  IF ( S(NEXT-1:NEXT-1) == ',' ) THEN
    WORD = S(ILO:NEXT-2)
  ELSE
    WORD = S(ILO:NEXT-1)
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE WORD_SWAP ( S, I1, I2 )

!*****************************************************************************80
!
!! WORD_SWAP swaps two words in a given string.
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
!    Input/output, character ( len = * ) S, a string of characters.
!    "Words" in the string are presumed to be separated by blanks.
!
!    Input, integer ( kind = 4 ) I1, I2, the indices of the words to be swapped.
!    If either I1 or I2 is nonpositive, or greater than the number of
!    words in the string, then nothing is done to the string.  Otherwise,
!    words I1 and I2 are swapped.
!
  IMPLICIT NONE

  LOGICAL BLANK
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) I1
  INTEGER ( KIND = 4 ) I2
  INTEGER ( KIND = 4 ) J1
  INTEGER ( KIND = 4 ) J1BEG
  INTEGER ( KIND = 4 ) J1END
  INTEGER ( KIND = 4 ) J2
  INTEGER ( KIND = 4 ) J2BEG
  INTEGER ( KIND = 4 ) J2END
  INTEGER ( KIND = 4 ) LENS
  CHARACTER ( LEN = * ) S
  CHARACTER ( LEN = 255 ) S2
  INTEGER ( KIND = 4 ) WORD_NUM

  LENS = LEN_TRIM ( S )
  IF ( LENS <= 0 ) THEN
    RETURN
  END IF

  IF ( 80 < LENS ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'WORD_SWAP - Warning!'
    WRITE ( *, '(a)' ) '  The internal temporary string is too short'
    WRITE ( *, '(a)' ) '  to copy your string.  Errors may result!'
    RETURN
  END IF
!
!  We need to make a copy of the input arguments, because we
!  might alter them.  We want to ensure that J1 <= J2.
!
  J1 = MIN ( I1, I2)
  J2 = MAX ( I1, I2)

  IF ( J1 <= 0 ) THEN
    RETURN
  ELSE IF ( J2 <= 0 ) THEN
    RETURN
  ELSE IF ( J1 == J2 ) THEN
    RETURN
  END IF

  J1BEG = 0
  J1END = 0
  J2BEG = 0
  J2END = 0
  WORD_NUM = 0
  BLANK = .TRUE.

  DO I = 1, LENS

    IF ( S(I:I) == ' ' ) THEN

      IF ( J1BEG /= 0 .AND. J1END == 0 ) THEN
        J1END = I - 1
      ELSE IF ( J2BEG /= 0 .AND. J2END == 0 ) THEN
        J2END = I - 1
      END IF

      BLANK = .TRUE.

    ELSE IF ( BLANK ) THEN

      WORD_NUM = WORD_NUM + 1

      IF ( WORD_NUM == J1 ) THEN
        J1BEG = I
      ELSE IF ( WORD_NUM == J2 ) THEN
        J2BEG = I
      END IF

      BLANK = .FALSE.

    END IF

  END DO

  IF ( J1BEG /= 0 .AND. J1END == 0 ) THEN
    J1END = LENS
  ELSE IF ( J2BEG /= 0 .AND. J2END == 0 ) THEN
    J2END = LENS
  END IF

  IF ( WORD_NUM < J1 .OR. WORD_NUM < J2 ) THEN
    RETURN
  END IF
!
!  OK, we can swap words J1 and J2.
!
  S2 = S
!
!  Copy word 2.
!
  S( J1BEG : J1BEG + J2END - J2BEG ) = S2 ( J2BEG : J2END )
!
!  Copy (possibly null) string between word 1 and word 2.
!
  S ( J1BEG + J2END - J2BEG + 1 : J1BEG + J2END - J1END - 1 ) &
    = S2 ( J1END + 1 : J2BEG - 1 )
!
!  Copy word 1.
!
  S ( J1BEG + J2END - J1END : J2END ) = S2 ( J1BEG : J1END )

  RETURN
END

!*************************************************************

END MODULE ModLib_CHRPAK

!******************************************************************************

 
MODULE ModLib_FILUM

!** PURPOSE OF THIS MODULE:
    ! contains routines that handle files

!** REFERENCES:
    ! These routines are from FILUM package by John Burkardt

!** USE STATEMENTS:
    USE ModLib_CHRPAK

    IMPLICIT NONE       ! Enforce explicit typing of all variables

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

SUBROUTINE FILENAME_APPEND ( FILENAME, APPEND )

!*****************************************************************************80
!
!! FILENAME_APPEND appends a string to a filename, before the extension.
!
!  Discussion:
!
!    The "extension" of a filename is the string of characters
!    that appears after the LAST period in the name.
!
!    A file with no period, or with a period as the last character
!    in the name, has a "null" extension.
!
!    The idea is that the string in APPEND is to be appended to
!    the part of the filename that precedes the extension.
!
!    The intended purpose of this routine is to be able to easily
!    generate a filename that indicates its relation to another file.
!
!  Example:
!
!          Input             Output
!    ===================     =========
!    filename    APPEND     filename
!
!    bob.for      6          bob6.for
!    bob.bob.bob  JOB        bob.bobJOB.bob
!    bob          yak        bobyak
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 March 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) FILENAME, a file name.
!    On output, the file name has been modified.
!
!    Input, character ( len = * ) APPEND, the string to be appended.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) APPEND
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) LEN_APPEND
  INTEGER ( KIND = 4 ) LEN_NAME

  CALL FILENAME_EXT_GET ( FILENAME, I, J )
!
!  If there is no extension, then simply slap APPEND on the end.
!
  IF ( I == -1 ) THEN

    LEN_NAME = LEN_TRIM ( FILENAME )
    FILENAME(LEN_NAME+1:) = APPEND
!
!  If there is an extension, then insert APPEND.
!
  ELSE

    LEN_APPEND = LEN_TRIM ( APPEND )
    FILENAME(I:) = APPEND(1:LEN_APPEND) // FILENAME(I:J)

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILENAME_DEC ( FILENAME )

!*****************************************************************************80
!
!! FILENAME_DEC decrements a partially numeric file name.
!
!  Discussion:
!
!    It is assumed that the digits in the name, whether scattered or
!    connected, represent a number that is to be decreased by 1 on
!    each call.  If this number is all 0's on input, the output number
!    is all 9's.  Non-numeric letters of the name are unaffected.
!
!    If the name is empty, then the routine stops.
!
!    If the name contains no digits, the empty string is returned.
!
!  Example:
!
!      Input            Output
!      -----            ------
!      'a7to12.txt'     'a7to11.txt' (typical case.  Last digit decremented)
!      'a7to00.txt'     'a8to99.txt' (last digit decremented, with carry.)
!      'a0to00.txt'     'a9to99.txt' (wrap around)
!      'cat.txt'        ' '          (no digits in input name.)
!      ' '              STOP!        (error.)
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
!    Input/output, character ( len = * ) FILENAME.
!    On input, a character string to be decremented.
!    On output, the decremented string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) CHANGE
  INTEGER ( KIND = 4 ) DIGIT
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENS

  LENS = LEN_TRIM ( FILENAME )

  IF ( LENS <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILENAME_DEC - Fatal error!'
    WRITE ( *, '(a)' ) '  The input filename is empty.'
    RETURN
  END IF

  CHANGE = 0

  DO I = LENS, 1, -1

    C = FILENAME(I:I)

    IF ( LGE ( C, '0' ) .AND. LLE ( C, '9' ) ) THEN

      CHANGE = CHANGE + 1

      DIGIT = ICHAR ( C ) - 48
      DIGIT = DIGIT - 1

      IF ( DIGIT == -1 ) THEN
        DIGIT = 9
      END IF

      C = CHAR ( DIGIT + 48 )

      FILENAME(I:I) = C

      IF ( C /= '9' ) THEN
        RETURN
      END IF

    END IF

  END DO

  IF ( CHANGE == 0 ) THEN
    FILENAME = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILENAME_EXT_GET ( FILENAME, I, J )

!*****************************************************************************80
!
!! FILENAME_EXT_GET determines the "extension" of a file name.
!
!  Discussion:
!
!    The "extension" of a filename is the string of characters
!    that appears after the LAST period in the name.  A file
!    with no period, or with a period as the last character
!    in the name, has a "null" extension.
!
!    Blanks are unusual in filenames.  This routine ignores all
!    trailing blanks, but will treat initial or internal blanks
!    as regular characters acceptable in a file name.
!
!  Example:
!
!    filename   I  J
!
!    bob.for     4  7
!    N.B.C.D     6  7
!    Naomi.      6  6
!    Arthur     -1 -1
!    .com        1  1
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, a file name to be examined.
!
!    Output, integer ( kind = 4 ) I, J, the indices of the first and
!    last characters in the file extension.
!    If no period occurs in filename, then
!      I = J = -1;
!    Otherwise,
!      I is the position of the LAST period in filename, and J is the
!      position of the last nonblank character following the period.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J

  I = CH_INDEX_LAST ( FILENAME, '.' )

  IF ( I == -1 ) THEN

    J = -1

  ELSE

    J = LEN_TRIM ( FILENAME )

  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILENAME_EXT_SWAP ( FILENAME, EXT )

!*****************************************************************************80
!
!! FILENAME_EXT_SWAP replaces the current "extension" of a file name.
!
!  Discussion:
!
!    The "extension" of a filename is the string of characters
!    that appears after the LAST period in the name.  A file
!    with no period, or with a period as the last character
!    in the name, has a "null" extension.
!
!  Example:
!
!          Input           Output
!    ================     =========
!    filename    EXT     filename
!
!    bob.for      obj     bob.obj
!    bob.bob.bob  txt     bob.bob.txt
!    bob          yak     bob.yak
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
!    Input/output, character ( len = * ) filename, a file name.
!    On output, the extension of the file has been changed.
!
!    Input, character ( len = * ) EXT, the extension to be used on the output
!    copy of filename, replacing the current extension if any.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) EXT
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) J
  INTEGER ( KIND = 4 ) LEN_MAX
  INTEGER ( KIND = 4 ) LEN_NAME

  LEN_MAX = LEN ( FILENAME )
  LEN_NAME = LEN_TRIM ( FILENAME )

  CALL FILENAME_EXT_GET ( FILENAME, I, J )

  IF ( I == -1 ) THEN

    IF ( LEN_MAX < LEN_NAME + 1 ) THEN
      RETURN
    END IF

    LEN_NAME = LEN_NAME + 1
    FILENAME(LEN_NAME:LEN_NAME) = '.'
    I = LEN_NAME + 1

  ELSE

    I = I + 1
    FILENAME(I:J) = ' '

  END IF

  FILENAME(I:) = EXT

  RETURN
END

!*************************************************************

SUBROUTINE FILENAME_INC ( FILENAME )

!*****************************************************************************80
!
!! FILENAME_INC increments a partially numeric filename.
!
!  Discussion:
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
!    19 September 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) FILENAME.
!    On input, a character string to be incremented.
!    On output, the incremented string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) CHANGE
  INTEGER ( KIND = 4 ) DIGIT
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENS

  LENS = LEN_TRIM ( FILENAME )

  IF ( LENS <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILENAME_INC - Fatal error!'
    WRITE ( *, '(a)' ) '  The input string is empty.'
    RETURN
  END IF

  CHANGE = 0

  DO I = LENS, 1, -1

    C = FILENAME(I:I)

    IF ( LGE ( C, '0' ) .AND. LLE ( C, '9' ) ) THEN

      CHANGE = CHANGE + 1

      DIGIT = ICHAR ( C ) - 48
      DIGIT = DIGIT + 1

      IF ( DIGIT == 10 ) THEN
        DIGIT = 0
      END IF

      C = CHAR ( DIGIT + 48 )

      FILENAME(I:I) = C

      IF ( C /= '0' ) THEN
        RETURN
      END IF

    END IF

  END DO
!
!  No digits were found.  Return blank.
!
  IF ( CHANGE == 0 ) THEN
    FILENAME = ' '
    RETURN
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILENAME_INC_NOWRAP ( FILENAME )

!*****************************************************************************80
!
!! FILENAME_INC_NOWRAP increments a partially numeric filename.
!
!  Discussion:
!
!    It is assumed that the digits in the name, whether scattered or
!    connected, represent a number that is to be increased by 1 on
!    each call.  Non-numeric letters of the name are unaffected.
!
!    If the (nonempty) name contains no digits, or all the digits are
!    9, then the empty string is returned.
!
!    If the empty string is input, the routine stops.
!
!  Example:
!
!      Input            Output
!      -----            ------
!      'a7to11.txt'     'a7to12.txt'
!      'a7to99.txt'     'a8to00.txt'
!      'a8to99.txt'     'a9to00.txt'
!      'a9to99.txt'     ' '
!      'cat.txt'        ' '
!      ' '              STOP!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) FILENAME.
!    On input, a character string to be incremented.
!    On output, the incremented string.
!
  IMPLICIT NONE

  CHARACTER C
  INTEGER ( KIND = 4 ) CARRY
  INTEGER ( KIND = 4 ) CHANGE
  INTEGER ( KIND = 4 ) DIGIT
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) LENS

  LENS = LEN_TRIM ( FILENAME )

  IF ( LENS <= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILENAME_INC_NOWRAP - Fatal error!'
    WRITE ( *, '(a)' ) '  The input string is empty.'
    RETURN
  END IF

  CHANGE = 0
  CARRY = 0

  DO I = LENS, 1, -1

    C = FILENAME(I:I)

    IF ( LGE ( C, '0' ) .AND. LLE ( C, '9' ) ) THEN

      CHANGE = CHANGE + 1
      CARRY = 0

      DIGIT = ICHAR ( C ) - 48
      DIGIT = DIGIT + 1

      IF ( DIGIT == 10 ) THEN
        DIGIT = 0
        CARRY = 1
      END IF

      C = CHAR ( DIGIT + 48 )

      FILENAME(I:I) = C

      IF ( C /= '0' ) THEN
        RETURN
      END IF

    END IF

  END DO
!
!  Unsatisfied carry.  The input digits were all 9.  Return blank.
!
  IF ( CARRY == 1 ) THEN
    FILENAME = ' '
    RETURN
  END IF
!
!  No digits were found.  Return blank.
!
  IF ( CHANGE == 0 ) THEN
    FILENAME = ' '
    RETURN
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILE_ADVANCE_TO_STRING ( IUNIT, S, LINE, IERROR )

!*****************************************************************************80
!
!! FILE_ADVANCE_TO_STRING searches ahead in a text file for a string.
!
!  Discussion:
!
!    The file should already have been opened.
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
!    Input, integer ( kind = 4 ) IUNIT, the unit number associated
!    with the open file.
!
!    Input, character ( len = * ) S, a string to search for.
!
!    Output, character ( len = * ) LINE:
!    If IERROR = 0, the line of the file that was just read,
!      and which contains the string.
!    If IERROR = 1, then LINE is blank.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error, the string was found.
!    1, error, the end of the file was reached.
!
  IMPLICIT NONE

  LOGICAL, PARAMETER :: DEBUG = .TRUE.
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = * ) LINE
  INTEGER ( KIND = 4 ) LINE_NUM
  CHARACTER ( LEN = * ) S

  IERROR = 0
  LINE_NUM = 0

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LINE_NUM = LINE_NUM + 1

    IF ( INDEX ( LINE, TRIM ( S ) ) /= 0 ) THEN
      RETURN
    END IF

  END DO

  LINE = ' '
  IERROR = 1

  IF ( DEBUG ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_ADVANCE_TO_STRING - Warning!'
    WRITE ( *, '(a)' ) '  Did not find the string:'
    WRITE ( *, '(a)' ) '    ' // TRIM ( S )
    WRITE ( *, '(a,i8)' ) '  Number of lines read was ', LINE_NUM
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILE_APPEND ( FILENAME, IERROR, IUNIT, REC_NUM )

!*****************************************************************************80
!
!! FILE_APPEND allows a user to append new information to an old file.
!
!  Discussion:
!
!    This routine was created to address the fact that ANSI FORTRAN
!    does not let one easily append information to a sequential
!    access file once it has been closed.  In order to allow a user
!    to append new information, we create a new, writeable copy
!    of the file by means of a temporary copy.
!
!    On input, the file should not be open.  On output, the file is
!    open, the file is writeable, and the file I/O pointer is
!    ready to write a new line following the last line of the
!    original contents of the file.
!
!    It is assumed that each line of the file is no longer than
!    256 characters.
!
!    The copied lines will not have any trailing blanks.
!
!    A temporary file will be created with the name "append.tmp".
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
!    Input, character ( len = * ) FILENAME, the name of the file to
!    which more information is to be appended.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error occurred.
!    1, an error occurred while trying to open the new file.
!
!    Output, integer ( kind = 4 ) IUNIT, the FORTRAN unit number associated
!    with the file.
!
!    Output, integer ( kind = 4 ) REC_NUM, the number of records (that is,
!    lines) that are already in the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  CHARACTER ( LEN = 255 ) FILE_TEMP
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  INTEGER ( KIND = 4 ) IUNIT2
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) REC_NUM

  IERROR = 0

  FILE_TEMP = 'append.tmp'
!
!  Open old file as readable.  If it doesn't exist, we can
!  skip ahead.  Otherwise, also open new file as writeable.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN

    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_APPEND - Note:'
    WRITE ( *, '(a)' ) '  This is a new file.'

    OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'new', IOSTAT = IOS )

    IF ( IOS /= 0 ) THEN
      IERROR = 4
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_APPEND - Fatal error!'
      WRITE ( *, '(a)' ) '  Unexpected error while opening the file:'
      WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
      RETURN
    END IF

    REC_NUM = 0

    RETURN
  END IF

  REWIND IUNIT

  CALL GET_UNIT ( IUNIT2 )

  OPEN ( UNIT = IUNIT2, FILE = FILE_TEMP, STATUS = 'new', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    IERROR = 4
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_APPEND - Fatal error!'
    WRITE ( *, '(a)' ) '  Unexpected error while opening the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILE_TEMP ) // '".'
    RETURN
  END IF
!
!  Copy data from old file into temporary file.
!
  REC_NUM = 0

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    REC_NUM = REC_NUM + 1
    WRITE ( IUNIT2, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = IUNIT )

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) 'FILE_APPEND - Note:'
  WRITE ( *, '(a,i8)' ) '  The number of records in the file is ', REC_NUM
!
!  Delete the old copy of the file.
!
  CALL FILE_DELETE ( FILENAME )
!
!  Mark the end of the temporary file, close it,
!  then reopen it with "OLD" status.
!
  ENDFILE ( UNIT = IUNIT2 )

  CLOSE ( UNIT = IUNIT2 )

  OPEN ( UNIT = IUNIT2, FILE = FILE_TEMP, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    IERROR = 4
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_APPEND - Fatal error!'
    WRITE ( *, '(a)' ) '  Unexpected error while opening the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILE_TEMP ) // '".'
    RETURN
  END IF

  REWIND IUNIT2
!
!  Create a new version of the old file, opening it with
!  "STATUS = 'NEW'" so that it is writeable.
!
  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'new', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    IERROR = 4
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_APPEND - Fatal error!'
    WRITE ( *, '(a)' ) '  Unexpected error while opening the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read each line from the temporary file, and copy it
!  back into the old file.
!
  DO

    READ ( IUNIT2, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    WRITE ( IUNIT, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = IUNIT2 )
!
!  Delete the temporary file, and return.
!
  CALL FILE_DELETE ( FILE_TEMP )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_CHAR_COUNT ( FILENAME, CHAR_NUM )

!*****************************************************************************80
!
!! FILE_CHAR_COUNT counts the number of characters in a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
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
!    Input, character ( len = * ) FILENAME, the name of the file.
!
!    Output, integer ( kind = 4 ) CHAR_NUM, the number of characters in
!    the file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) CHAR_NUM
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE

  CHAR_NUM = 0
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    CHAR_NUM = -1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_CHAR_COUNT - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read the lines, count the characters.
!
!  Right now, I don't know how to determine which characters in LINE
!  were read from the file and which represent padding.
!
!  I tried going to the last nonnull, but it looks like, at least on
!  my system, LINE gets filled with blanks.  So I'll just count
!  til the last non-blank, but that doesn't distinguish between a
!  blank that really was in the original file and should be counted,
!  and a blank that is just padding.
!
  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CHAR_NUM = CHAR_NUM + LEN_TRIM ( LINE )

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_COLUMN_COUNT ( FILENAME, COLUMN_NUM )

!*****************************************************************************80
!
!! FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Most lines of the file are presumed to consist of COLUMN_NUM words,
!    separated by spaces.  There may also be some blank lines, and some
!    comment lines, which have a "#" in column 1.
!
!    The routine tries to find the first non-comment non-blank line and
!    counts the number of words in that line.
!
!    If all lines are blanks or comments, it goes back and tries to analyze
!    a comment line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILENAME, the name of the file.
!
!    Output, integer ( kind = 4 ) COLUMN_NUM, the number of columns in the file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) COLUMN_NUM
  CHARACTER ( LEN = * ) FILENAME
  LOGICAL GOT_ONE
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    COLUMN_NUM = -1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COLUMN_COUNT - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read one line, but skip blank lines and comment lines.
!
  GOT_ONE = .FALSE.

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    IF ( LEN_TRIM ( LINE ) == 0 ) THEN
      CYCLE
    END IF

    IF ( LINE(1:1) == '#' ) THEN
      CYCLE
    END IF

    GOT_ONE = .TRUE.
    EXIT

  END DO

  IF ( .NOT. GOT_ONE ) THEN

    REWIND ( IUNIT )

    DO

      READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

      IF ( IOS /= 0 ) THEN
        EXIT
      END IF

      IF ( LEN_TRIM ( LINE ) == 0 ) THEN
        CYCLE
      END IF

      GOT_ONE = .TRUE.
      EXIT

    END DO

  END IF

  CLOSE ( UNIT = IUNIT )

  IF ( .NOT. GOT_ONE ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning!'
    WRITE ( *, '(a)' ) '  The file does not seem to contain any data.'
    COLUMN_NUM = -1
    RETURN
  END IF

  CALL S_WORD_COUNT ( LINE, COLUMN_NUM )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_COLUMN_RANGE ( FILENAME, COLUMN_NUM, COL_MIN, COL_MAX )

!*****************************************************************************80
!
!! FILE_COLUMN_RANGE determines the minimum and maximum ranges of each column.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Each line of the file is presumed to consist of COLUMN_NUM real numbers,
!    separated by spaces.
!
!    The routine computes the range of each column.
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
!    Input, character ( len = * ) filename, the name of the file.
!
!    Input, integer ( kind = 4 ) COLUMN_NUM, the number of columns assumed
!    to be in the file.
!
!    Output, real ( kind = 8 ) COL_MIN(COLUMN_NUM), COL_MAX(COLUMN_NUM),
!    the minimum and maximum for each column.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) COLUMN_NUM

  REAL ( KIND = 8 ) COL_MAX(COLUMN_NUM)
  REAL ( KIND = 8 ) COL_MIN(COLUMN_NUM)
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  INTEGER ( KIND = 4 ) J
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) NROW
  REAL ( KIND = 8 ) X(COLUMN_NUM)
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    COLUMN_NUM = -1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COLUMN_RANGE - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF

  NROW = 0
  COL_MIN(1:COLUMN_NUM) = 0.0D+00
  COL_MAX(1:COLUMN_NUM) = 0.0D+00

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CALL S_TO_R8VEC ( LINE, COLUMN_NUM, X, IERROR )

    IF ( IERROR /= 0 ) THEN
      EXIT
    END IF

    NROW = NROW + 1

    IF ( NROW == 1 ) THEN
      COL_MIN(1:COLUMN_NUM) = X(1:COLUMN_NUM)
      COL_MAX(1:COLUMN_NUM) = X(1:COLUMN_NUM)
    ELSE
      DO J = 1, COLUMN_NUM
        COL_MIN(J) = MIN ( COL_MIN(J), X(J) )
        COL_MAX(J) = MAX ( COL_MAX(J), X(J) )
      END DO
    END IF

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_COPY ( OLD_FILENAME, NEW_FILENAME, IERROR )

!*****************************************************************************80
!
!! FILE_COPY makes a copy of a file.
!
!  Discussion:
!
!    The file is assumed to be sequential access, with variable
!    length records.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OLD_filename, the name of the file
!    to be copied.
!
!    Input, character ( len = * ) NEW_filename, the name of the copy of
!    the file.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error occurred.
!    1, the file names are the same.
!    2, a free unit number could not be found for the old file.
!    3, the routine could not open the old file.
!    4, a free unit number could not be found for the new file.
!    5, the routine could not open the new file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) NEW_FILENAME
  INTEGER ( KIND = 4 ) NEW_UNIT
  CHARACTER ( LEN = * ) OLD_FILENAME
  INTEGER ( KIND = 4 ) OLD_UNIT

  IERROR = 0
!
!  Does the original file exist?
!
  IF ( .NOT. FILE_EXIST ( OLD_FILENAME ) ) THEN
    IERROR = 1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  The old file does not exist.'
    RETURN
  END IF
!
!  Is the original file open?
!
  IF ( FILE_IS_OPEN ( OLD_FILENAME ) ) THEN
    IERROR = 1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  The old file is open.'
    WRITE ( *, '(a)' ) '  It must be closed before it can be copied.'
    RETURN
  END IF
!
!  Make sure the file names aren't the same.
!
  IF ( NEW_FILENAME == OLD_FILENAME ) THEN
    IERROR = 1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  The old and new file names are identical.'
    RETURN
  END IF
!
!  Does the new file exist?
!
  IF ( FILE_EXIST ( NEW_FILENAME ) ) THEN

    IF ( FILE_IS_OPEN ( NEW_FILENAME ) ) THEN
      IERROR = 1
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
      WRITE ( *, '(a)' ) '  A file is already open with the new name.'
      WRITE ( *, '(a)' ) '  It must be closed before it can be overwritten.'
      RETURN
    END IF

    CALL FILE_DELETE ( NEW_FILENAME )

  END IF
!
!  At this point:
!
!    The old file exists, and is not open.
!    The new file does not exist, and has a different name.
!
!  Open the old file.
!
  CALL GET_UNIT ( OLD_UNIT )

  IF ( OLD_UNIT == 0 ) THEN
    IERROR = 2
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not get a unit number for the old file.'
    RETURN
  END IF

  OPEN ( UNIT = OLD_UNIT, FILE = OLD_FILENAME, STATUS = 'old', &
    IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    IERROR = 3
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the old file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( OLD_FILENAME ) // '".'
    RETURN
  END IF
!
!  Open the new file.
!
  CALL GET_UNIT ( NEW_UNIT )

  IF ( NEW_UNIT == 0 ) THEN
    IERROR = 4
    CLOSE ( UNIT = OLD_UNIT )
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not get a free unit for the copy file.'
    RETURN
  END IF

  OPEN ( UNIT = NEW_UNIT, FILE = NEW_FILENAME, STATUS = 'replace', &
    IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    IERROR = 5
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_COPY - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the new file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( NEW_FILENAME ) // '".'
    CLOSE ( UNIT = OLD_UNIT )
    RETURN
  END IF
!
!  Read an old line, write a new line.
!
  DO

    READ ( OLD_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    WRITE ( NEW_UNIT, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = OLD_UNIT )

  ENDFILE ( UNIT = NEW_UNIT )
  CLOSE ( UNIT = NEW_UNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_DELETE ( FILENAME )

!*****************************************************************************80
!
!! FILE_DELETE deletes a named file if it exists.
!
!  Discussion:
!
!    You might want to call this routine to get rid of any old copy
!    of a file, before trying to open a new copy with the OPEN argument:
!      status = 'new'.
!
!    It's not always safe to open a file with " STATUS = 'UNKNOWN' ".
!    For instance, on the SGI, the most recent version of the FORTRAN
!    compiler seems to go crazy when I open an unformatted direct
!    access file this way.  It creates an enormous file (of somewhat
!    random size).  The problem goes away if I delete any old copy
!    using this routine, and then open a fresh copy with
!    " STATUS = 'NEW' ".  It's a scary world.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  LOGICAL, PARAMETER :: VERBOSE = .FALSE.
!
!  Does the file exist?
!
  IF ( .NOT. FILE_EXIST ( FILENAME ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_DELETE - Warning!'
    WRITE ( *, '(a)' ) '  There is no file of the given name.'
    RETURN
  END IF
!
!  Is the file open?
!
  IF ( FILE_IS_OPEN ( FILENAME ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_DELETE - Warning!'
    WRITE ( *, '(a)' ) '  The file is currently open.'
    WRITE ( *, '(a)' ) '  It must be closed before it can be deleted.'
    RETURN
  END IF
!
!  Get a free unit number.
!
  CALL GET_UNIT ( IUNIT )

  IF ( IUNIT == 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_DELETE: Warning!'
    WRITE ( *, '(a)' ) '  A free FORTRAN unit could not be found.'
    RETURN
  END IF

  IF ( VERBOSE ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_DELETE:'
    WRITE ( *, '(a)' ) '  Deleting "' // TRIM ( FILENAME ) // '".'
  END IF

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_DELETE: Warning!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF

  CLOSE ( UNIT = IUNIT, STATUS = 'delete' )

  RETURN
END

!*************************************************************

FUNCTION FILE_EXIST ( FILENAME )

!*****************************************************************************80
!
!! FILE_EXIST reports whether a file exists.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Output, logical FILE_EXIST, is TRUE if the file exists.
!
  IMPLICIT NONE

  LOGICAL FILE_EXIST
  CHARACTER ( LEN = * ) FILENAME

  INQUIRE ( FILE = FILENAME, EXIST = FILE_EXIST )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_GET_NEXT_INTEGER ( FILE_UNIT, MORE, VALUE )

!*****************************************************************************80
!
!! FILE_GET_NEXT_INTEGER returns the next integer from a file.
!
!  Discussion:
!
!    The file should have been opened before calling this routine.
!
!    The routine will read ANY string of characters that looks like an integer,
!    and does not require a specific separate.  Garbage characters are ignored.
!
!    If a very long string of digits is encountered, the routine will trigger
!    integer overflow as it tries to pack them all into one integer.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 January 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer ( kind = 4 ) FILE_UNIT, the unit number associated with
!    the file.
!
!    Input/output, logical MORE.  On first call, the user should set MORE to
!    be FALSE, which signals the routine to initialize.  Each time the
!    routine reads another integer from the file, it returns it in VALUE,
!    and returns MORE as TRUE.  When no more integers can be read, MORE
!    is returned as FALSE.
!
!    Output, integer ( kind = 4 ) VALUE, the next integer in the file.  However,
!    if MORE is FALSE on output, then VALUE is set to 0.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FILE_UNIT
  INTEGER ( KIND = 4 ) FILE_STATUS
  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) LENGTH
  CHARACTER ( LEN = 255 ) LINE
  LOGICAL MORE
  INTEGER ( KIND = 4 ) VALUE

  SAVE LINE

  IF ( .NOT. MORE ) THEN
    LINE = ' '
    MORE = .TRUE.
  END IF
!
!  Try to extract an integer from the current line.
!
  DO
!
!  If LINE is blank, read another line from the file.
!
    DO WHILE ( LEN_TRIM ( LINE ) == 0 )

      READ ( FILE_UNIT, '(a)', IOSTAT = FILE_STATUS ) LINE

      IF ( FILE_STATUS /= 0 ) THEN
        MORE = .FALSE.
        VALUE = 0
        LINE = ' '
        RETURN
      END IF

    END DO
!
!  Try to extract the next integer from LINE.
!
    CALL S_TO_I4 ( LINE, VALUE, IERROR, LENGTH )
!
!  If we got a value, then chop out the used bits, and return.
!
    IF ( IERROR == 0 ) THEN
      LINE(1:LENGTH) = ' '
      LINE = ADJUSTL ( LINE )
      EXIT
    END IF
!
!  If we could not read an integer value, this line is useless or exhausted.
!  Loop again.
!
    LINE = ' '

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE FILE_GET_NEXT_WORD ( IUNIT, WORD, LINE, LINE_NUM, IERROR )

!*****************************************************************************80
!
!! FILE_GET_NEXT_WORD returns the next word and trailing context from a file.
!
!  Discussion:
!
!    The file should have been opened before calling this routine.
!    The file should contain ASCII text, which can be thought of as
!    words separated by one or more blanks.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters
!
!    Input, integer ( kind = 4 ) IUNIT, the unit number associated with the file.
!
!    Output, character ( len = * ) WORD, the next word in the file.  If the
!    current line of the file is blank, or if the file has been exhausted,
!    WORD will be set to ' '.
!
!    Input/output, character ( len = * ) LINE, the remaining text of the line
!    that contains the information in WORD.  On each call, the next word
!    in LINE is extracted until LINE is empty, when it is refilled by
!    reading another line from the file.  Because LINE contains information
!    needed by this routine, it should not be altered by the user
!    between calls.
!
!    Input/output, integer ( kind = 4 ) LINE_NUM, the number of lines read from
!    the file.  Before the first call to this routine, the user should set
!    LINE_NUM to 0.
!
!    Output, integer ( kind = 4 ) IERROR, error flag.
!    0, no error, another word was read, and returned in WORD.
!    1, end of file.  WORD and LINE were set to ' '.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) IERROR
  INTEGER ( KIND = 4 ) IHI
  INTEGER ( KIND = 4 ) ILO
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  INTEGER ( KIND = 4 ) LENC
  CHARACTER ( LEN = * ) LINE
  INTEGER ( KIND = 4 ) LINE_NUM
  CHARACTER ( LEN = * ) WORD

  IERROR = 0
!
!  If LINE_NUM is zero, then initialize LINE.
!
  IF ( LINE_NUM <= 0 ) THEN
    LINE_NUM = 0
    LINE = ' '
  END IF
!
!  If LINE is blank, try to read a new line from the file.
!
  IF ( LINE == ' ' ) THEN

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      IERROR = 1
      WORD = ' '
      LINE = ' '
      RETURN
    END IF

    LINE_NUM = LINE_NUM + 1

    IF ( LINE == ' ' ) THEN
      WORD = ' '
      RETURN
    END IF

  END IF
!
!  Extract the next word from LINE into WORD and return.
!
  LENC = LEN_TRIM ( LINE )
!
!  Find ILO, the index of the first nonblank in LINE.
!
  ILO = 1

  DO WHILE ( LINE(ILO:ILO) == ' ' )
    ILO = ILO + 1
  END DO
!
!  Find IHI, the index of the last consecutive nonblank after the one at ILO.
!
  IHI = ILO

  DO WHILE ( IHI + 1 <= LENC )
    IF ( LINE(IHI+1:IHI+1) == ' ' ) THEN
      EXIT
    END IF
    IHI = IHI + 1
  END DO
!
!  Set WORD.
!
  WORD = LINE(ILO:IHI)
!
!  Slide TEXT to the left.
!
  IF ( IHI + 1 <= LENC ) THEN
    LINE = LINE(IHI+1:)
  ELSE
    LINE = ' '
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILE_INSERT ( INPUT_UNIT, OUTPUT_UNIT, LINE_NUM )

!*****************************************************************************80
!
!! FILE_INSERT copies the contents of an input file into an output file.
!
!  Discussion:
!
!    Both the input and output files should already be opened by the
!    user.  The routine simply reads a line from the input file and
!    writes it to the output file.  The input file is assumed to be
!    a simple, sequential access text file, with records no longer
!    than 256 characters.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) INPUT_UNIT, the unit number associated
!    with the input file.
!
!    Input, integer ( kind = 4 ) OUTPUT_UNIT, the unit number associated
!    with the output file.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of records copied
!    from the input file to the output file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) INPUT_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) LINE_NUM
  INTEGER ( KIND = 4 ) OUTPUT_UNIT

  LINE_NUM = 0
!
!  Make sure the file names aren't the same.
!
  IF ( INPUT_UNIT == OUTPUT_UNIT ) THEN
    RETURN
  END IF

  DO

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      RETURN
    END IF

    LINE_NUM = LINE_NUM + 1
    WRITE ( OUTPUT_UNIT, '(a)' ) TRIM ( LINE )

  END DO

  RETURN
END

!*************************************************************

FUNCTION FILE_IS_OPEN ( FILENAME )

!*****************************************************************************80
!
!! FILE_IS_OPEN reports whether a file (specified by filename) is open.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Output, logical FILE_IS_OPEN, is TRUE if the file is open.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  LOGICAL FILE_IS_OPEN

  INQUIRE ( FILE = FILENAME, OPENED = FILE_IS_OPEN )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_LINES_UNIFORM ( FILENAME, SEED, N, LINE, LINE_INDEX, LINE_NUM )

!*****************************************************************************80
!
!! FILE_LINES_UNIFORM selects N random lines from a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    The algorithm used is interesting because it does not require
!    the number of lines in the file to be known in advance, and it
!    only reads the file once.
!
!    If the number of lines requested is more than the number of lines
!    in the file, then all the lines in the file will be selected, and
!    extra blank lines will be appended, with index -1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tom Christiansen and Nathan Torkington,
!    "8.6: Picking a Random Line from a File",
!    Perl Cookbook, pages 284-285,
!    O'Reilly, 1999.
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Input, integer ( kind = 4 ) N, the number of lines to be extracted.
!
!    Output, character ( len = * ) LINE(N), N random lines from the file.
!
!    Output,  integer ( kind = 4 ) LINE_INDEX(N), the indices of the lines.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of lines in the file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) N

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = * ) LINE(N)
  INTEGER ( KIND = 4 ) LINE_INDEX(N)
  INTEGER ( KIND = 4 ) LINE_NUM
  CHARACTER ( LEN = 255 ) LINE_READ
  REAL ( KIND = 8 ) R
  INTEGER ( KIND = 4 ) SEED

  LINE_NUM = 0
  LINE_INDEX(1:N) = -1
  DO I = 1, N
    LINE(I) = ' '
  END DO
  LINE_READ = ' '
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_LINES_UNIFORM - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read the lines.
!
  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE_READ

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LINE_NUM = LINE_NUM + 1

    IF ( LINE_NUM <= N ) THEN

      I = LINE_NUM
      LINE(I) = LINE_READ
      LINE_INDEX(I) = LINE_NUM

    ELSE

      R = R8_UNIFORM_01 ( SEED )

      IF ( R * REAL ( LINE_NUM, KIND = 8 ) <= REAL ( N ) ) THEN

        I = I4_UNIFORM ( 1, N, SEED )
        LINE(I) = LINE_READ
        LINE_INDEX(I) = LINE_NUM

      END IF

    END IF

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_LINE_GET ( FILENAME, LINE_INDEX, LINE )

!*****************************************************************************80
!
!! FILE_LINE_GET gets a particular line of a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Input, integer ( kind = 4 ) LINE_INDEX, the index of the line to be read.
!
!    Output, character ( len = * ) LINE, the text of the line.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = * ) LINE
  INTEGER ( KIND = 4 ) LINE_INDEX
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    LINE = ' '
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_LINE_GET - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Count the lines.
!
  DO I = 1, LINE_INDEX

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      LINE = ' '
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_LINE_GET - Fatal error!'
      WRITE ( *, '(a)' ) '  Unexpected end of file.'
      RETURN
    END IF

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_LINE_UNIFORM ( FILENAME, SEED, LINE, LINE_INDEX, LINE_NUM )

!*****************************************************************************80
!
!! FILE_LINE_UNIFORM returns a random line from a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    The algorithm used is interesting because it does not require
!    the number of lines in the file to be known in advance, and it
!    only reads the file once.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Tom Christiansen and Nathan Torkington,
!    "8.6: Picking a Random Line from a File",
!    Perl Cookbook, pages 284-285,
!    O'Reilly, 1999.
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, character ( len = * ) LINE, a random line from the file.
!
!    Output, integer ( kind = 4 ) LINE_INDEX, the index of the chosen line.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of lines in the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = * ) LINE
  INTEGER ( KIND = 4 ) LINE_INDEX
  INTEGER ( KIND = 4 ) LINE_NUM
  CHARACTER ( LEN = 255 ) LINE_READ
  REAL ( KIND = 8 ) R
  INTEGER ( KIND = 4 ) SEED

  LINE_NUM = 0
  LINE_INDEX = -1
  LINE = ' '
  LINE_READ = ' '
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_LINE_UNIFORM - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read the lines.
!
  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE_READ

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LINE_NUM = LINE_NUM + 1

    R = R8_UNIFORM_01 ( SEED )

    IF ( R * REAL ( LINE_NUM, KIND = 8 ) <= 1.0D+00 ) THEN

      LINE = LINE_READ
      LINE_INDEX = LINE_NUM

    END IF

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

FUNCTION FILE_LINE_WIDTH ( FILENAME )

!*****************************************************************************80
!
!! FILE_LINE_WIDTH returns the length of the longest line in a file.
!
!  Discussion:
!
!    Whether or not this routine works properly depends on some
!    system dependent matters.  For instance, the routine tries to
!    access the file a character at a time.  To do this, it seems
!    necessary to treat the file, presumably a text file, as though
!    it were an unformatted file, with direct access.  Direct access
!    requires a record length whose units are left undecided by the
!    standard.  We here assume that the unit is a byte.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file to be read.
!
!    Output, integer ( kind = 4 ) FILE_LINE_WIDTH, the length of the
!    longest line.
!
  IMPLICIT NONE

  CHARACTER CH
  INTEGER ( KIND = 4 ) CH_NUM
  INTEGER ( KIND = 4 ) FILE_LINE_WIDTH
  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) FILE_UNIT
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) LINE_WIDTH
  INTEGER ( KIND = 4 ) RECORD
  INTEGER ( KIND = 4 ) VALUE

  VALUE = -1
!
!  Open the file.
!
!  The smallest amount of information we can write at a time is
!  1 word = 4 bytes = 32 bits.
!
  CALL GET_UNIT ( FILE_UNIT )

  OPEN ( UNIT = FILE_UNIT, FILE = FILENAME, STATUS = 'old', &
    FORM = 'unformatted', ACCESS = 'direct', RECL = 1 )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_LINE_WIDTH - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    FILE_LINE_WIDTH = VALUE
    CLOSE (  UNIT = FILE_UNIT )
    RETURN
  END IF

  CH_NUM = 0
  RECORD = 0
  LINE_WIDTH = 0
  VALUE = 0

  DO

    RECORD = RECORD + 1
    READ ( FILE_UNIT, REC = RECORD, IOSTAT = IOS ) CH

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CH_NUM = CH_NUM + 1
!
!  Character 10 is the LF character.
!  Character 13 is the CR character.
!
    IF ( IACHAR ( CH ) == 10 .OR. IACHAR ( CH ) == 13 ) THEN
      LINE_WIDTH = 0
    ELSE
      LINE_WIDTH = LINE_WIDTH + 1
      VALUE = MAX ( VALUE, LINE_WIDTH )
    END IF

  END DO

  CLOSE ( UNIT = FILE_UNIT )

  FILE_LINE_WIDTH = VALUE

  RETURN
END

!*************************************************************

SUBROUTINE FILE_MERGE ( FILENAME_1, FILENAME_2, FILENAME_3, N1, N2, N3 )

!*****************************************************************************80
!
!! FILE_MERGE merges two sorted files into a third.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename_1, filename_2, the names of the
!    two input files to be merged.
!
!    Input, character ( len = * ) filename_3, the name of the output file to
!    be created.
!
!    Output, integer ( kind = 4 ) N1, N2, N3, the number of lines of text in the
!    two input files and the output file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME_1
  CHARACTER ( LEN = * ) FILENAME_2
  CHARACTER ( LEN = * ) FILENAME_3
  INTEGER ( KIND = 4 ) FILE_UNIT_1
  INTEGER ( KIND = 4 ) FILE_UNIT_2
  INTEGER ( KIND = 4 ) FILE_UNIT_3
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) N1
  INTEGER ( KIND = 4 ) N2
  INTEGER ( KIND = 4 ) N3
  CHARACTER ( LEN = 255 ) WORD1
  CHARACTER ( LEN = 255 ) WORD2
  CHARACTER ( LEN = 255 ) WORD3

  N1 = 0
  N2 = 0
  N3 = 0

  CALL GET_UNIT ( FILE_UNIT_1 )

  OPEN ( UNIT = FILE_UNIT_1, FILE = FILENAME_1, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_MERGE - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file #1:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME_1 ) // '".'
    RETURN
  END IF

  CALL GET_UNIT ( FILE_UNIT_2 )

  OPEN ( UNIT = FILE_UNIT_2, FILE = FILENAME_2, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_MERGE - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file #2:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME_2 ) // '".'
    RETURN
  END IF

  CALL GET_UNIT ( FILE_UNIT_3 )

  OPEN ( UNIT = FILE_UNIT_3, FILE = FILENAME_3, STATUS = 'replace', &
    IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_MERGE - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the output file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME_3 ) // '".'
    RETURN
  END IF

  WORD1 = ' '
  WORD2 = ' '

  DO

    IF ( WORD1 == ' ' ) THEN

      READ ( FILE_UNIT_1, '(a)', IOSTAT = IOS ) WORD1

      IF ( IOS == 0 ) THEN
        CALL S_LOW ( WORD1 )
        N1 = N1 + 1
      END IF

      IF ( WORD1 == ' ' ) THEN
        WORD1 = '_END_'
        CLOSE ( UNIT = FILE_UNIT_1 )
      END IF

    END IF

    IF ( WORD2 == ' ' ) THEN

      READ ( FILE_UNIT_2, '(a)', IOSTAT = IOS ) WORD2

      IF ( IOS == 0 ) THEN
        CALL S_LOW ( WORD2 )
        N2 = N2 + 1
      END IF

      IF ( WORD2 == ' ' ) THEN
        WORD2 = '_END_'
        CLOSE ( UNIT = FILE_UNIT_2 )
      END IF

    END IF

    IF ( WORD1 == '_END_' .AND. WORD2 == '_END_' ) THEN

      EXIT

    ELSE IF ( WORD1 /= '_END_' .AND. WORD2 == '_END_' ) THEN

      WORD3 = WORD1
      WORD1 = ' '

    ELSE IF ( WORD1 == '_END_' .AND. WORD2 /= '_END_' ) THEN

      WORD3 = WORD2
      WORD2 = ' '

    ELSE

      IF ( LLT ( WORD1, WORD2 ) ) THEN

        WORD3 = WORD1
        WORD1 = ' '

      ELSE IF ( WORD1 == WORD2 ) THEN

        WORD3 = WORD1
        WORD1 = ' '
        WORD2 = ' '

      ELSE

        WORD3 = WORD2
        WORD2 = ' '

      END IF

    END IF

    WRITE ( FILE_UNIT_3, '(a)' ) TRIM ( WORD3 )
    N3 = N3 + 1

  END DO

  ENDFILE ( UNIT = FILE_UNIT_3 )

  CLOSE ( UNIT = FILE_UNIT_3 )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_PARA_COUNT ( FILENAME, PARA_NUM )

!*****************************************************************************80
!
!! FILE_PARA_COUNT counts the number of paragraphs in a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.  A paragraph is
!    a sequence of nonblank lines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Output, integer ( kind = 4 ) PARA_NUM, the number of paragraphs found in
!    the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  INTEGER ( KIND = 4 ) LENC
  INTEGER ( KIND = 4 ) LENC_OLD
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) PARA_NUM

  PARA_NUM = 0
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    PARA_NUM = -1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_PARA_COUNT - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Count the paragraphs.
!
  LENC = 0

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LENC_OLD = LENC
    LENC = LEN_TRIM ( LINE )

    IF ( 0 < LENC .AND. LENC_OLD <= 0 ) THEN
      PARA_NUM = PARA_NUM + 1
    END IF

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_PAREN_CHECK ( FILENAME )

!*****************************************************************************80
!
!! FILE_PAREN_CHECK checks a file for generalized parenthesis errors.
!
!  Discussion:
!
!    The check made is that the current number of left parentheses read must
!    always be at least as great as the number of right parentheses read.
!    Moreover, when we reach the end of the file, the numbers must be equal.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) FILE_UNIT
  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) LINE_LEN
  INTEGER ( KIND = 4 ) LINE_NUM
  INTEGER ( KIND = 4 ) SUM_P

  SUM_P = 0
!
!  Open the file.
!
  CALL GET_UNIT ( FILE_UNIT )

  OPEN ( UNIT = FILE_UNIT, FILE = FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_PAREN_CHECK - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF

  LINE_NUM = 0

  DO

    READ ( FILE_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LINE_NUM = LINE_NUM + 1

    LINE_LEN = LEN_TRIM ( LINE )

    DO I = 1, LINE_LEN

      IF ( LINE(I:I) == '(' ) THEN

        SUM_P = SUM_P + 1

      ELSE IF ( LINE(I:I) == ')' ) THEN

        SUM_P = SUM_P - 1

        IF ( SUM_P < 0 ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'FILE_PAREN_CHECK - Warning!'
          WRITE ( *, '(a)' ) '  Parenthesis error in the file:'
          WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
          WRITE ( *, '(a,i8)' ) &
            '  An illegal right parenthesis occurs on line', LINE_NUM
          WRITE ( *, '(a)' ) '    ' // TRIM ( LINE )
          CLOSE ( UNIT = FILE_UNIT )
          RETURN
        END IF

      END IF

    END DO

  END DO

  CLOSE ( UNIT = FILE_UNIT )

  IF ( SUM_P /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_PAREN_CHECK - Warning!'
    WRITE ( *, '(a)' ) '  Parenthesis error in the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    WRITE ( *, '(a,i8)' ) '  Number of missing right parentheses: ', SUM_P
  ELSE
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_PAREN_CHECK - Note:'
    WRITE ( *, '(a)' ) '  Parenthesis checks passed for file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILE_PRINT ( FILENAME )

!*****************************************************************************80
!
!! FILE_PRINT prints the contents of a text file.
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
!    Input, character ( len = * ) filename, the name of the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_PRINT - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF

  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    WRITE ( *, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_RENAME ( FILENAME_OLD, FILENAME_NEW )

!*****************************************************************************80
!
!! FILE_RENAME renames a file.
!
!  Discussion:
!
!    Actually, this routine copies the file, and deletes the original.
!    But to the user, it should look like a rename, just a little slower.
!
!    If a file already exists with the new name, it is deleted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename_OLD, the name of the original file.
!
!    Output, character ( len = * ) filename_NEW, the name of the new file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME_NEW
  CHARACTER ( LEN = * ) FILENAME_OLD
  INTEGER ( KIND = 4 ) IERROR
!
!  Does the old file exist?
!
  IF ( .NOT. FILE_EXIST ( FILENAME_OLD ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_RENAME - Error!'
    WRITE ( *, '(a)' ) '  The original file to be renamed does not exist.'
    RETURN
  END IF
!
!  Is the old file open?
!
  IF ( FILE_IS_OPEN ( FILENAME_OLD ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_RENAME - Error!'
    WRITE ( *, '(a)' ) '  The original file is open.'
    WRITE ( *, '(a)' ) '  It must be closed before it can be renamed.'
    RETURN
  END IF
!
!  Does old file name = new file name?
!
  IF ( S_EQI ( FILENAME_NEW, FILENAME_OLD ) ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_RENAME: Warning!'
    WRITE ( *, '(a)' ) '  The old and new file names are the same.'
    WRITE ( *, '(a)' ) '  I suppose this means there is nothing to do.'
    RETURN
  END IF
!
!  Does the new file exist?
!
  IF ( FILE_EXIST ( FILENAME_NEW ) ) THEN
!
!  Is the new file open?
!
    IF ( FILE_IS_OPEN ( FILENAME_NEW ) ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_RENAME - Error!'
      WRITE ( *, '(a)' ) '  The new file is already open.'
      WRITE ( *, '(a)' ) '  It must be closed before it can be overwritten.'
      RETURN
    END IF

    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_RENAME:'
    WRITE ( *, '(a)' ) '  Deleting pre-existing file with new name.'
    CALL FILE_DELETE ( FILENAME_NEW )

  END IF
!
!  Copy old into new.
!
  CALL FILE_COPY ( FILENAME_OLD, FILENAME_NEW, IERROR )

  IF ( IERROR /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_RENAME: Warning!'
    WRITE ( *, '(a)' ) '  Could not copy the old file!'
    RETURN
  END IF
!
!  Delete the old file.
!
  CALL FILE_DELETE ( FILENAME_OLD )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_REVERSE_COLUMNS ( INPUT_FILENAME, OUTPUT_FILENAME )

!*****************************************************************************80
!
!! FILE_REVERSE_COLUMNS makes a copy of a file with each lines reversed.
!
!  Example:
!
!    Input file:
!
!      This is the tale
!      of three little pigs
!      and their tails.
!
!    Output file:
!
!      elat eht si sihT
!      sgip elttil eerht fo
!      .sliat rieht dna
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
!    Input, character ( len = * ) INPUT_filename, the name of the file to
!    be reversed.
!
!    Input, character ( len = * ) OUTPUT_filename, the name of the file to be
!    created, contained a copy of the input file with the columns reversed.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) INPUT_FILENAME
  INTEGER ( KIND = 4 ) INPUT_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) OUTPUT_FILENAME
  INTEGER ( KIND = 4 ) OUTPUT_UNIT
!
!  Open the input file.
!
  CALL GET_UNIT ( INPUT_UNIT )

  OPEN ( UNIT = INPUT_UNIT, FILE = INPUT_FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_REVERSE_COLUMNS - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( INPUT_FILENAME ) // '".'
    RETURN
  END IF
!
!  Open the output file.
!
  CALL GET_UNIT ( OUTPUT_UNIT )

  OPEN ( UNIT = OUTPUT_UNIT, FILE = OUTPUT_FILENAME, STATUS = 'replace', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_REVERSE_COLUMNS - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the output file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( OUTPUT_FILENAME ) // '".'
    RETURN
  END IF

  DO

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CALL S_REVERSE ( LINE )

    WRITE ( OUTPUT_UNIT, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = INPUT_UNIT )

  ENDFILE ( UNIT = OUTPUT_UNIT )
  CLOSE ( UNIT = OUTPUT_UNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_REVERSE_ROWS ( INPUT_FILENAME, OUTPUT_FILENAME )

!*****************************************************************************80
!
!! FILE_REVERSE_ROWS makes a copy of a file with the lines in reverse order.
!
!  Example:
!
!    Input file:
!
!      This is the tale
!      of three little pigs
!      and their tails.
!
!    Output file:
!
!      and their tails.
!      of three little pigs
!      This is the tale
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
!    Input, character ( len = * ) INPUT_filename, the name of the file to
!    be reversed.
!
!    Input, character ( len = * ) OUTPUT_filename, the name of the file to be
!    created, contained a reversed copy of the input file.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) I
  INTEGER ( KIND = 4 ) INPUT_COUNT
  CHARACTER ( LEN = * ) INPUT_FILENAME
  INTEGER ( KIND = 4 ) INPUT_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) OUTPUT_FILENAME
  INTEGER ( KIND = 4 ) OUTPUT_UNIT
!
!  Open the input file.
!
  CALL GET_UNIT ( INPUT_UNIT )

  OPEN ( UNIT = INPUT_UNIT, FILE = INPUT_FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_REVERSE_ROWS - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( INPUT_FILENAME ) // '".'
    RETURN
  END IF
!
!  Move to the end of the input file.
!
  INPUT_COUNT = 0

  DO

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    INPUT_COUNT = INPUT_COUNT + 1

  END DO
!
!  Open the output file.
!
  CALL GET_UNIT ( OUTPUT_UNIT )

  OPEN ( UNIT = OUTPUT_UNIT, FILE = OUTPUT_FILENAME, STATUS = 'replace', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_REVERSE_ROWS - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the output file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( OUTPUT_FILENAME ) // '".'
    RETURN
  END IF
!
!  Read backwards.
!
  BACKSPACE ( UNIT = INPUT_UNIT, IOSTAT = IOS )

  DO I = INPUT_COUNT, 1, -1

    BACKSPACE ( UNIT = INPUT_UNIT, IOSTAT = IOS )

    IF ( IOS /= 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_REVERSE_ROWS - Fatal error!'
      WRITE ( *, '(a)' ) '  IOS nonzero on backspace.'
      EXIT
    END IF

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)') 'FILE_REVERSE_ROWS - Fatal error!'
      WRITE ( *, '(a)' ) '  IOS nonzero in read'
      EXIT
    END IF

    WRITE ( OUTPUT_UNIT, '(a)' ) TRIM ( LINE )

    BACKSPACE ( UNIT = INPUT_UNIT )

  END DO

  CLOSE ( UNIT = INPUT_UNIT )

  ENDFILE ( UNIT = OUTPUT_UNIT )
  CLOSE ( UNIT = OUTPUT_UNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_ROT13 ( INPUT_FILENAME, OUTPUT_FILENAME )

!*****************************************************************************80
!
!! FILE_ROT13 makes a ROT13-encoded copy of a file.
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
!    Input, character ( len = * ) INPUT_filename, the name of the file to
!    be reversed.
!
!    Input, character ( len = * ) OUTPUT_filename, the name of the file to be
!    created, contained a ROT13-encoded copy of the input file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) INPUT_FILENAME
  INTEGER ( KIND = 4 ) INPUT_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = * ) OUTPUT_FILENAME
  INTEGER ( KIND = 4 ) OUTPUT_UNIT
!
!  Open the input file.
!
  CALL GET_UNIT ( INPUT_UNIT )

  OPEN ( UNIT = INPUT_UNIT, FILE = INPUT_FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_ROT13 - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( INPUT_FILENAME ) // '".'
    RETURN
  END IF
!
!  Open the output file.
!
  CALL GET_UNIT ( OUTPUT_UNIT )

  OPEN ( UNIT = OUTPUT_UNIT, FILE = OUTPUT_FILENAME, STATUS = 'replace', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_ROT13 - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the output file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( OUTPUT_FILENAME ) // '".'
    RETURN
  END IF

  DO

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CALL S_TO_ROT13 ( LINE )

    WRITE ( OUTPUT_UNIT, '(a)' ) TRIM ( LINE )

  END DO

  CLOSE ( UNIT = INPUT_UNIT )

  ENDFILE ( UNIT = OUTPUT_UNIT )
  CLOSE ( UNIT = OUTPUT_UNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_ROW_COUNT ( FILENAME, LINE_NUM )

!*****************************************************************************80
!
!! FILE_ROW_COUNT counts the number of rows in a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
!
!    Blank lines and comment lines, which begin with '#', are not counted.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Output, integer ( kind = 4 ) LINE_NUM, the number of lines found in the
!    file.  If the file could not be opened, then LINE_NUM is returned as -1.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) LINE_NUM
  LOGICAL, PARAMETER :: VERBOSE = .FALSE.

  LINE_NUM = 0
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN

    LINE_NUM = -1

    IF ( VERBOSE ) THEN
      WRITE ( *, '(a)' ) ' '
      WRITE ( *, '(a)' ) 'FILE_ROW_COUNT - Fatal error!'
      WRITE ( *, '(a)' ) '  Could not open the file:'
      WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    END IF

    RETURN

  END IF
!
!  Count the lines.
!
  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    IF ( LEN_TRIM ( LINE ) == 0 ) THEN
      CYCLE
    END IF

    IF ( LINE(1:1) == '#' ) THEN
      CYCLE
    END IF

    LINE_NUM = LINE_NUM + 1

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_SEQUENCE_DELETE ( FILENAME, DELETE_COUNT )

!*****************************************************************************80
!
!! FILE_SEQUENCE_DELETE deletes a file sequence.
!
!  Discussion:
!
!    We suppose the user has a set of files whose names differ only
!    in some numeric tag that is sequentially increasing, as, perhaps,
!    "file001.txt", "file002.txt" through "file137.txt", say.
!
!    The user specifies filename as the name of the first file in the
!    sequence.  This function deletes that file, generates the next
!    name in the sequence, and, if a file with that name exists, it
!    deletes it as well.  The process continues until a file name is
!    reached for which there is no existing file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the first file
!    in the sequence.
!
!    Output, integer ( kind = 4 ) DELETE_COUNT, the number of files deleted.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) DELETE_COUNT
  CHARACTER ( LEN = * ) FILENAME
  CHARACTER ( LEN = 255 ) FILENAME2

  DELETE_COUNT = 0
  FILENAME2 = FILENAME

  DO WHILE ( FILE_EXIST ( FILENAME2 ) )

    CALL FILE_DELETE ( FILENAME2 )

    DELETE_COUNT = DELETE_COUNT + 1

    CALL FILENAME_INC ( FILENAME2 )

  END DO

  RETURN
END

!*************************************************************

SUBROUTINE FILE_SEQUENCE_SIZE ( FILENAME, FILE_DIM, FILE_NUM )

!*****************************************************************************80
!
!! FILE_SEQUENCE_SIZE sizes a file sequence.
!
!  Discussion:
!
!    We suppose the user has a set of files whose names differ only
!    in some numeric tag that is sequentially increasing, as, perhaps,
!    "file001.txt", "file002.txt" through "file137.txt", say.
!
!    The user specifies the name of the first file in the sequence.
!    This function determines the number of files in the sequence,
!    and makes a guess for the "dimension" of the files, that is, the number
!    of numeric data items.
!
!    Note that the function only checks the dimension of the data in
!    the first file.  It is up to the user to determine whether this
!    dimension is used for every file in the sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the first file in
!    the sequence.
!
!    Output, integer ( kind = 4 ) FILE_DIM, the dimension of the data
!    in one file.
!
!    Output, integer ( kind = 4 ) FILE_NUM, the number of files.
!
  IMPLICIT NONE

  INTEGER ( KIND = 4 ) FILE_DIM
  CHARACTER ( LEN = * ) FILENAME
  CHARACTER ( LEN = 255 ) FILENAME2
  INTEGER ( KIND = 4 ) FILE_NUM

  FILE_NUM = 0
  FILE_DIM = 0
  FILENAME2 = FILENAME

  DO

    IF ( .NOT. FILE_EXIST ( FILENAME2 ) ) THEN
      EXIT
    END IF

    FILE_NUM = FILE_NUM + 1

    IF ( FILE_NUM == 1 ) THEN

      CALL FILE_WORD_COUNT ( FILENAME2, FILE_DIM )

    END IF

    CALL FILENAME_INC ( FILENAME2 )

  END DO

  RETURN
END

!*************************************************************

FUNCTION FILE_TAG_CHECK ( FILENAME, LEFT, RIGHT )

!*****************************************************************************80
!
!! FILE_TAG_CHECK checks a file for generalized parenthesis errors.
!
!  Discussion:
!
!    The check made is that the current number of left "parentheses" read must
!    always be at least as great as the number of right "parentheses" read.
!    Moreover, when we reach the end of the file, the numbers must be equal.
!
!    Typical examples of left and right parentheses might be:
!
!    (), [], {}, <>, <P> </P>, 'do' 'end do'
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) filename, the name of the file.
!
!    Input, character ( len = * ) LEFT, RIGHT, the left and right
!    parentheses marks.
!
!    Output, logical FILE_TAG_CHECK, is true if the file passed the check.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  LOGICAL FILE_TAG_CHECK
  INTEGER ( KIND = 4 ) FILE_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = * ) LEFT
  INTEGER ( KIND = 4 ) LEFT_LEN
  INTEGER ( KIND = 4 ) LEFT_POS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = 255 ) LINE_COPY
  INTEGER ( KIND = 4 ) LINE_LEN
  INTEGER ( KIND = 4 ) LINE_NUM
  CHARACTER ( LEN = * ) RIGHT
  INTEGER ( KIND = 4 ) RIGHT_LEN
  INTEGER ( KIND = 4 ) RIGHT_POS
  INTEGER ( KIND = 4 ) SUM_P

  SUM_P = 0
  LEFT_LEN = LEN ( LEFT )
  RIGHT_LEN = LEN ( RIGHT )
!
!  Open the file.
!
  CALL GET_UNIT ( FILE_UNIT )

  OPEN ( UNIT = FILE_UNIT, FILE = FILENAME, STATUS = 'old', &
    FORM = 'formatted', ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_TAG_CHECK - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the input file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    FILE_TAG_CHECK = .FALSE.
    RETURN
  END IF

  LINE_NUM = 0

  DO

    READ ( FILE_UNIT, '(a)', IOSTAT = IOS ) LINE
    LINE_COPY = LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    LINE_NUM = LINE_NUM + 1

    LINE_LEN = LEN_TRIM ( LINE )

    DO

      LEFT_POS = INDEX ( LINE, LEFT )
      IF ( LEFT_POS == 0 ) THEN
        LEFT_POS = LINE_LEN + 1
      END IF

      RIGHT_POS = INDEX ( LINE, RIGHT )
      IF ( RIGHT_POS == 0 ) THEN
        RIGHT_POS = LINE_LEN + 1
      END IF

      IF ( LEFT_POS < RIGHT_POS ) THEN

        SUM_P = SUM_P + 1
        LINE = ADJUSTL ( LINE(LEFT_POS+LEFT_LEN:) )
        LINE_LEN = LEN_TRIM ( LINE )

      ELSE IF ( RIGHT_POS < LEFT_POS ) THEN

        SUM_P = SUM_P - 1
        LINE = ADJUSTL ( LINE(RIGHT_POS+RIGHT_LEN:) )
        LINE_LEN = LEN_TRIM ( LINE )

        IF ( SUM_P < 0 ) THEN
          WRITE ( *, '(a)' ) ' '
          WRITE ( *, '(a)' ) 'FILE_TAG_CHECK - Warning!'
          WRITE ( *, '(a)' ) '  Tag error in the file:'
          WRITE ( *, '(a)' ) '    "' // TRIM ( FILENAME ) // '".'
          WRITE ( *, '(a,i8)' ) '  An illegal right tag occurs on line', &
            LINE_NUM
          WRITE ( *, '(a)' ) '    ' // TRIM ( LINE_COPY )
          CLOSE ( UNIT = FILE_UNIT )
          FILE_TAG_CHECK = .FALSE.
          RETURN
        END IF

      ELSE

        EXIT

      END IF

    END DO

  END DO

  CLOSE ( UNIT = FILE_UNIT )

  IF ( SUM_P /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_TAG_CHECK - Warning!'
    WRITE ( *, '(a)' ) '  Tag error in the file:'
    WRITE ( *, '(a)' ) '    "' // TRIM ( FILENAME ) // '".'
    WRITE ( *, '(a,i8)' ) '  Number of missing right tags: ', SUM_P
    FILE_TAG_CHECK = .FALSE.
  ELSE
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_TAG_CHECK - Note:'
    WRITE ( *, '(a)' ) '  Tag checks passed for file:'
    WRITE ( *, '(a)' ) '    "' // TRIM ( FILENAME ) // '".'
    FILE_TAG_CHECK = .TRUE.
  END IF

  RETURN
END

!*************************************************************

SUBROUTINE FILE_UNIQUE_LINES ( INPUT_FILENAME, OUTPUT_FILENAME, &
  INPUT_LINE_NUM, OUTPUT_LINE_NUM )

!*****************************************************************************80
!
!! FILE_UNIQUE_LINES makes a copy of the unique lines of a sorted file.
!
!  Discussion:
!
!    Actually, the input file doesn't have to be sorted.  The routine
!    simply reads each line of the input file, and writes it to the
!    output file if it is distinct from the previous input line.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 February 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_filename, the name of the input file.
!
!    Input, character ( len = * ) OUTPUT_filename, the name of the output file.
!
!    Output, integer ( kind = 4 ) INPUT_LINE_NUM, the number of lines in the
!    input file.
!
!    Output, integer ( kind = 4 ) OUTPUT_LINE_NUM, the number of lines in the
!    output file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) INPUT_FILENAME
  INTEGER ( KIND = 4 ) INPUT_LINE_NUM
  INTEGER ( KIND = 4 ) INPUT_UNIT
  INTEGER ( KIND = 4 ) IOS
  CHARACTER ( LEN = 255 ) LINE
  CHARACTER ( LEN = 255 ) LINE_OLD
  CHARACTER ( LEN = * ) OUTPUT_FILENAME
  INTEGER ( KIND = 4 ) OUTPUT_LINE_NUM
  INTEGER ( KIND = 4 ) OUTPUT_UNIT

  INPUT_LINE_NUM = 0
  OUTPUT_LINE_NUM = 0
  LINE_OLD = ' '

  CALL GET_UNIT ( INPUT_UNIT )

  OPEN ( UNIT = INPUT_UNIT, FILE = INPUT_FILENAME, STATUS = 'old', &
    IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_UNIQUE_LINES - Error!'
    WRITE ( *, '(a)' ) '  Could not open the input file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( INPUT_FILENAME ) // '".'
    RETURN
  END IF

  CALL GET_UNIT ( OUTPUT_UNIT )

  OPEN ( UNIT = OUTPUT_UNIT, FILE = OUTPUT_FILENAME, STATUS = 'replace', &
    IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_UNIQUE_LINES - Error!'
    WRITE ( *, '(a)' ) '  Could not open the output file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( OUTPUT_FILENAME ) // '".'
    CLOSE ( UNIT = INPUT_UNIT )
    RETURN
  END IF

  DO

    READ ( INPUT_UNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    INPUT_LINE_NUM = INPUT_LINE_NUM + 1

    IF ( INPUT_LINE_NUM == 1 .OR. LINE /= LINE_OLD ) THEN

      WRITE ( OUTPUT_UNIT, '(a)' ) TRIM ( LINE )
      OUTPUT_LINE_NUM = OUTPUT_LINE_NUM + 1
      LINE_OLD = LINE

    END IF

  END DO

  CLOSE ( UNIT = INPUT_UNIT )

  ENDFILE ( UNIT = OUTPUT_UNIT )
  CLOSE ( UNIT = OUTPUT_UNIT )

  RETURN
END

!*************************************************************

SUBROUTINE FILE_WORD_COUNT ( FILENAME, WORD_NUM )

!*****************************************************************************80
!
!! FILE_WORD_COUNT counts the number of words in a file.
!
!  Discussion:
!
!    The file is assumed to be a simple text file.
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
!    Input, character ( len = * ) filename, the name of the file.
!
!    Output, integer ( kind = 4 ) WORD_NUM, the number of words found in the file.
!
  IMPLICIT NONE

  CHARACTER ( LEN = * ) FILENAME
  INTEGER ( KIND = 4 ) IOS
  INTEGER ( KIND = 4 ) IUNIT
  CHARACTER ( LEN = 255 ) LINE
  INTEGER ( KIND = 4 ) NPLUS
  INTEGER ( KIND = 4 ) WORD_NUM

  WORD_NUM = 0
!
!  Open the file.
!
  CALL GET_UNIT ( IUNIT )

  OPEN ( UNIT = IUNIT, FILE = FILENAME, STATUS = 'old', FORM = 'formatted', &
    ACCESS = 'sequential', IOSTAT = IOS )

  IF ( IOS /= 0 ) THEN
    WORD_NUM = -1
    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) 'FILE_WORD_COUNT - Fatal error!'
    WRITE ( *, '(a)' ) '  Could not open the file:'
    WRITE ( *, '(4x,a)' ) '"' // TRIM ( FILENAME ) // '".'
    RETURN
  END IF
!
!  Read the lines.
!
  DO

    READ ( IUNIT, '(a)', IOSTAT = IOS ) LINE

    IF ( IOS /= 0 ) THEN
      EXIT
    END IF

    CALL S_WORD_COUNT ( LINE, NPLUS )

    WORD_NUM = WORD_NUM + NPLUS

  END DO

  CLOSE ( UNIT = IUNIT )

  RETURN
END

!*************************************************************

END MODULE ModLib_FILUM

!******************************************************************************

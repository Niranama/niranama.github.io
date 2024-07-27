!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_string_utilities.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  JSON-Fortran support module for string manipulation.
!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    MODULE ModLib_JSONString

    USE,INTRINSIC :: IEEE_ARITHMETIC
    USE ModLib_JSONKinds
    USE ModLib_JSONParameters

    IMPLICIT NONE

    PRIVATE

!******************************************************
!>
!  Convert a 'DEFAULT' kind character input to
!  'ISO_10646' kind and return it
    INTERFACE TO_UNICODE
        MODULE PROCEDURE TO_UNI, TO_UNI_VEC
    END INTERFACE
!******************************************************

!- 61


    PUBLIC :: INTEGER_TO_STRING
    PUBLIC :: REAL_TO_STRING
    PUBLIC :: STRING_TO_INTEGER
    PUBLIC :: STRING_TO_REAL
    PUBLIC :: VALID_JSON_HEX
    PUBLIC :: TO_UNICODE
    PUBLIC :: ESCAPE_STRING
    PUBLIC :: UNESCAPE_STRING
    PUBLIC :: LOWERCASE_STRING
    PUBLIC :: REPLACE_STRING
    PUBLIC :: DECODE_RFC6901
    PUBLIC :: ENCODE_RFC6901

    CONTAINS
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert an integer to a string.

    PURE SUBROUTINE INTEGER_TO_STRING(IVAL,INT_FMT,STR)

    IMPLICIT NONE

    INTEGER(IK),INTENT(IN)               :: IVAL    !! integer value.
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: INT_FMT !! format for integers
    CHARACTER(KIND=CK,LEN=*),INTENT(OUT) :: STR     !! `ival` converted to a string.

    INTEGER(IK) :: ISTAT

    WRITE(STR,FMT=INT_FMT,IOSTAT=ISTAT) IVAL

    IF (ISTAT==0) THEN
        STR = ADJUSTL(STR)
    ELSE
        STR = REPEAT(STAR,LEN(STR))
    END IF

    END SUBROUTINE INTEGER_TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert a string into an integer.
!
!!- History
!  * Jacob Williams : 12/10/2013 : Rewrote original `parse_integer` routine.
!    Added error checking.
!  * Modified by Izaak Beekman
!  * Jacob Williams : 2/4/2017 : moved core logic to this routine.

    SUBROUTINE STRING_TO_INTEGER(STR,IVAL,STATUS_OK)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR        !! the string to convert to an integer
    INTEGER(IK),INTENT(OUT)             :: IVAL       !! the integer value
    LOGICAL(LK),INTENT(OUT)             :: STATUS_OK  !! true if there were no errors

    CHARACTER(KIND=CDK,LEN=:),ALLOCATABLE :: DIGITS
    INTEGER(IK) :: NDIGITS_DIGITS,NDIGITS,IERR

! Compute how many digits we need to read
    NDIGITS = 2*LEN_TRIM(STR)
    IF (NDIGITS/=0) THEN
        NDIGITS_DIGITS = NINT(LOG10(REAL(NDIGITS)))+1
        ALLOCATE(CHARACTER(KIND=CDK,LEN=NDIGITS_DIGITS) :: DIGITS)
        WRITE(DIGITS,'(I0)') NDIGITS !gfortran will have a runtime error with * edit descriptor here
! gfortran bug: '*' edit descriptor for ISO_10646 strings does bad stuff.
        READ(STR,'(I'//TRIM(DIGITS)//')',IOSTAT=IERR) IVAL   !string to integer
! error check:
        STATUS_OK = (IERR==0)
    ELSE
        STATUS_OK = .FALSE.
    END IF
    IF (.NOT. STATUS_OK) IVAL = 0_IK

    END SUBROUTINE STRING_TO_INTEGER
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 12/4/2013
!
!  Convert a real value to a string.
!
!--- Modified
!  * Izaak Beekman  : 02/24/2015 : added the compact option.
!  * Jacob Williams : 10/27/2015 : added the star option.
!  * Jacob Williams : 07/07/2019 : added null and ieee options.

    SUBROUTINE REAL_TO_STRING(RVAL,REAL_FMT,COMPACT_REAL,NON_NORMALS_TO_NULL,STR)

    IMPLICIT NONE

    REAL(RK),INTENT(IN)                  :: RVAL         !! real value.
    CHARACTER(KIND=CDK,LEN=*),INTENT(IN) :: REAL_FMT     !! format for real numbers
    LOGICAL(LK),INTENT(IN)               :: COMPACT_REAL !! compact the string so that it is
!! displayed with fewer characters
    LOGICAL(LK),INTENT(IN)               :: NON_NORMALS_TO_NULL !! If True, NaN, Infinity, or -Infinity are reTURNED AS `NULL`.
!! If False, the string value will be returned in quotes
!! (e.g., "NaN", "Infinity", or "-Infinity" )
    CHARACTER(KIND=CK,LEN=*),INTENT(OUT) :: STR          !! `rval` converted to a string.

    INTEGER(IK) :: ISTAT !! write `iostat` flag

    IF (IEEE_IS_FINITE(RVAL) .AND. .NOT. IEEE_IS_NAN(RVAL)) THEN

! normal real numbers

        IF (REAL_FMT==STAR) THEN
            WRITE(STR,FMT=*,IOSTAT=ISTAT) RVAL
        ELSE
            WRITE(STR,FMT=REAL_FMT,IOSTAT=ISTAT) RVAL
        END IF

        IF (ISTAT==0) THEN
!in this case, the default string will be compacted,
! so that the same value is displayed with fewer characters.
            IF (COMPACT_REAL) CALL COMPACT_REAL_STRING(STR)
        ELSE
            STR = REPEAT(STAR,LEN(STR)) ! error
        END IF

    ELSE
! special cases for NaN, Infinity, and -Infinity

        IF (NON_NORMALS_TO_NULL) THEN
! return it as a JSON null value
            STR = NULL_STR
        ELSE
! Let the compiler do the real to string conversion
! like before, but put the result in quotes so it
! gets printed as a string
            WRITE(STR,FMT=*,IOSTAT=ISTAT) RVAL
            IF (ISTAT==0) THEN
                STR = QUOTATION_MARK//TRIM(ADJUSTL(STR))//QUOTATION_MARK
            ELSE
                STR = REPEAT(STAR,LEN(STR)) ! error
            END IF
        END IF

    END IF

    END SUBROUTINE REAL_TO_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/19/2014
!
!  Convert a string into a `real(RK)`.
!
!!- History
!  * Jacob Williams, 10/27/2015 : Now using `fmt=*`, rather than
!    `fmt=real_fmt`, since it doesn't work for some unusual cases
!    (e.g., when `str='1E-5'`).
!  * Jacob Williams : 2/6/2017 : moved core logic to this routine.

    SUBROUTINE STRING_TO_REAL(STR,USE_QUIET_NAN,RVAL,STATUS_OK)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR           !! the string to convert to a real
    LOGICAL(LK),INTENT(IN)              :: USE_QUIET_NAN !! if true, return NaN's as `ieee_quiet_nan`.
!! otherwise, use `ieee_signaling_nan`.
    REAL(RK),INTENT(OUT)                :: RVAL          !! `str` converted to a real value
    LOGICAL(LK),INTENT(OUT)             :: STATUS_OK     !! true if there were no errors

    INTEGER(IK) :: IERR  !! read iostat error code

    READ(STR,FMT=*,IOSTAT=IERR) RVAL
    STATUS_OK = (IERR==0)
    IF (.NOT. STATUS_OK) THEN
        RVAL = 0.0_RK
    ELSE
        IF (IEEE_SUPPORT_NAN(RVAL)) THEN
            IF (IEEE_IS_NAN(RVAL)) THEN
! make sure to return the correct NaN
                IF (USE_QUIET_NAN) THEN
                    RVAL = IEEE_VALUE(RVAL,IEEE_QUIET_NAN)
                ELSE
                    RVAL = IEEE_VALUE(RVAL,IEEE_SIGNALING_NAN)
                END IF
            END IF
        END IF
    END IF

    END SUBROUTINE STRING_TO_REAL
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!  date: 02/24/2015
!
!  Compact a string representing a real number, so that
!  the same value is displayed with fewer characters.
!
!!- See also
!  * [[real_to_string]]

    SUBROUTINE COMPACT_REAL_STRING(STR)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(INOUT) :: STR  !! string representation of a real number.

    CHARACTER(KIND=CK,LEN=LEN(STR)) :: SIGNIFICAND
    CHARACTER(KIND=CK,LEN=LEN(STR)) :: EXPNT
    CHARACTER(KIND=CK,LEN=2) :: SEPARATOR
    INTEGER(IK) :: EXP_START
    INTEGER(IK) :: DECIMAL_POS
    INTEGER(IK) :: SIG_TRIM
    INTEGER(IK) :: EXP_TRIM
    INTEGER(IK) :: I  !! counter

    STR = ADJUSTL(STR)
    EXP_START = SCAN(STR,CK_'eEdD')
    IF (EXP_START == 0) EXP_START = SCAN(STR,CK_'-+',BACK=.TRUE.)
    DECIMAL_POS = SCAN(STR,CK_'.')
    IF (EXP_START /= 0) SEPARATOR = STR(EXP_START:EXP_START)

    IF ( EXP_START < DECIMAL_POS ) THEN !possibly signed, exponent-less float

        SIGNIFICAND = STR
        SIG_TRIM = LEN(TRIM(SIGNIFICAND))
        DO I = LEN(TRIM(SIGNIFICAND)),DECIMAL_POS+2,-1 !look from right to left at 0s
!but save one after the decimal place
            IF (SIGNIFICAND(I:I) == '0') THEN
                SIG_TRIM = I-1
            ELSE
                EXIT
            END IF
        END DO
        STR = TRIM(SIGNIFICAND(1:SIG_TRIM))

    ELSE IF (EXP_START > DECIMAL_POS) THEN !float has exponent

        SIGNIFICAND = STR(1:EXP_START-1)
        SIG_TRIM = LEN(TRIM(SIGNIFICAND))
        DO I = LEN(TRIM(SIGNIFICAND)),DECIMAL_POS+2,-1 !look from right to left at 0s
            IF (SIGNIFICAND(I:I) == '0') THEN
                SIG_TRIM = I-1
            ELSE
                EXIT
            END IF
        END DO
        EXPNT = ADJUSTL(STR(EXP_START+1:))
        IF (EXPNT(1:1) == '+' .OR. EXPNT(1:1) == '-') THEN
            SEPARATOR = TRIM(ADJUSTL(SEPARATOR))//EXPNT(1:1)
            EXP_START = EXP_START + 1
            EXPNT     = ADJUSTL(STR(EXP_START+1:))
        END IF
        EXP_TRIM = 1
        DO I = 1,(LEN(TRIM(EXPNT))-1) !look at exponent leading zeros saving last
            IF (EXPNT(I:I) == '0') THEN
                EXP_TRIM = I+1
            ELSE
                EXIT
            END IF
        END DO
        STR = TRIM(ADJUSTL(SIGNIFICAND(1:SIG_TRIM)))// &
              TRIM(ADJUSTL(SEPARATOR))// &
              TRIM(ADJUSTL(EXPNT(EXP_TRIM:)))

!else ! mal-formed real, BUT this code should be unreachable

    END IF

    END SUBROUTINE COMPACT_REAL_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 1/21/2014
!
!  Add the escape characters to a string for adding to JSON.

    SUBROUTINE ESCAPE_STRING(STR_IN, STR_OUT, ESCAPE_SOLIDUS)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN)              :: STR_IN
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT) :: STR_OUT
    LOGICAL(LK),INTENT(IN) :: ESCAPE_SOLIDUS  !! if the solidus (forward slash)
!! is also to be escaped

    INTEGER(IK) :: I    !! counter
    INTEGER(IK) :: IPOS !! accumulated string size
!! (so we can allocate it in chunks for
!! greater runtime efficiency)
    CHARACTER(KIND=CK,LEN=1) :: C  !! for reading `str_in` one character at a time.
!- 359

    LOGICAL :: TO_BE_ESCAPED !! if there are characters to be escaped

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: SPECIALS_NO_SLASH = QUOTATION_MARK//&
                                                     BACKSLASH//&
                                                     BSPACE//&
                                                     FORMFEED//&
                                                     NEWLINE//&
                                                     CARRIAGE_RETURN//&
                                                     HORIZONTAL_TAB

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: SPECIALS = SPECIALS_NO_SLASH//SLASH

!Do a quick scan for the special characters,
! if any are present, then process the string,
! otherwise, return the string as is.
    IF (ESCAPE_SOLIDUS) THEN
        TO_BE_ESCAPED = SCAN(STR_IN,SPECIALS)>0
    ELSE
        TO_BE_ESCAPED = SCAN(STR_IN,SPECIALS_NO_SLASH)>0
    END IF

    IF (TO_BE_ESCAPED) THEN

        STR_OUT = REPEAT(SPACE,CHUNK_SIZE)
        IPOS = 1

!go through the string and look for special characters:
        DO I=1,LEN(STR_IN)

            C = STR_IN(I:I)    !get next character in the input string

!if the string is not big enough, then add another chunk:
            IF (IPOS+3>LEN(STR_OUT)) STR_OUT = STR_OUT // BLANK_CHUNK

            SELECT CASE(C)
            CASE(BACKSLASH)

!test for unicode sequence: '\uXXXX'
![don't add an extra '\' for those]
                IF (I+5<=LEN(STR_IN)) THEN
                    IF (STR_IN(I+1:I+1)==CK_'u' .AND. &
                        VALID_JSON_HEX(STR_IN(I+2:I+5))) THEN
                        STR_OUT(IPOS:IPOS) = C
                        IPOS = IPOS + 1
                        CYCLE
                    END IF
                END IF

                STR_OUT(IPOS:IPOS+1) = BACKSLASH//C
                IPOS = IPOS + 2

            CASE(QUOTATION_MARK)
                STR_OUT(IPOS:IPOS+1) = BACKSLASH//C
                IPOS = IPOS + 2
            CASE(SLASH)
                IF (ESCAPE_SOLIDUS) THEN
                    STR_OUT(IPOS:IPOS+1) = BACKSLASH//C
                    IPOS = IPOS + 2
                ELSE
                    STR_OUT(IPOS:IPOS) = C
                    IPOS = IPOS + 1
                END IF
            CASE(BSPACE)
                STR_OUT(IPOS:IPOS+1) = '\b'
                IPOS = IPOS + 2
            CASE(FORMFEED)
                STR_OUT(IPOS:IPOS+1) = '\f'
                IPOS = IPOS + 2
            CASE(NEWLINE)
                STR_OUT(IPOS:IPOS+1) = '\n'
                IPOS = IPOS + 2
            CASE(CARRIAGE_RETURN)
                STR_OUT(IPOS:IPOS+1) = '\r'
                IPOS = IPOS + 2
            CASE(HORIZONTAL_TAB)
                STR_OUT(IPOS:IPOS+1) = '\t'
                IPOS = IPOS + 2
            CASE DEFAULT
                STR_OUT(IPOS:IPOS) = C
                IPOS = IPOS + 1
            END SELECT

        END DO

!trim the string if necessary:
        IF (IPOS<LEN(STR_OUT)+1) THEN
            IF (IPOS==1) THEN
                STR_OUT = CK_''
            ELSE
!- 452

                STR_OUT = STR_OUT(1:IPOS-1)  !original

            END IF
        END IF

    ELSE

        STR_OUT = STR_IN

    END IF

    END SUBROUTINE ESCAPE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Remove the escape characters from a JSON string and return it.
!
!  The escaped characters are denoted by the `\` character:
!
!  * `\"`        - quotation mark
!  * `\\`        - reverse solidus
!  * `\/`        - solidus
!  * `\b`        - backspace
!  * `\f`        - formfeed
!  * `\n`        - newline (LF)
!  * `\r`        - carriage return (CR)
!  * `\t`        - horizontal tab
!  * `\uXXXX`    - 4 hexadecimal digits

    SUBROUTINE UNESCAPE_STRING(STR, ERROR_MESSAGE)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(INOUT) :: STR           !! * in: string as stored
!!   in a [[json_value]].
!! * out: decoded string.
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(OUT)   :: ERROR_MESSAGE !! will be allocated if
!! there was an error

    INTEGER :: I   !! counter
    INTEGER :: N   !! length of `str`
    INTEGER :: M   !! length of `str_tmp`
    CHARACTER(KIND=CK,LEN=1) :: C  !! for scanning each character in string
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR_TMP !! temp decoded string (if the input
!! string contains an escape character
!! and needs to be decoded).

    IF (SCAN(STR,BACKSLASH)>0) THEN

!there is at least one escape character, so process this string:

        N = LEN(STR)
        STR_TMP = REPEAT(SPACE,N) !size the output string (will be trimmed later)
        M = 0  !counter in str_tmp
        I = 0  !counter in str

        DO

            I = I + 1
            IF (I>N) EXIT ! finished
            C = STR(I:I) ! get next character in the string

            IF (C == BACKSLASH) THEN

                IF (I<N) THEN

                    I = I + 1
                    C = STR(I:I) !character after the escape

                    SELECT CASE(C)
                    CASE (QUOTATION_MARK,BACKSLASH,SLASH)
!use d as is
                        M = M + 1
                        STR_TMP(M:M) = C
                    CASE (CK_'b')
                        C = BSPACE
                        M = M + 1
                        STR_TMP(M:M) = C
                    CASE (CK_'f')
                        C = FORMFEED
                        M = M + 1
                        STR_TMP(M:M) = C
                    CASE (CK_'n')
                        C = NEWLINE
                        M = M + 1
                        STR_TMP(M:M) = C
                    CASE (CK_'r')
                        C = CARRIAGE_RETURN
                        M = M + 1
                        STR_TMP(M:M) = C
                    CASE (CK_'t')
                        C = HORIZONTAL_TAB
                        M = M + 1
                        STR_TMP(M:M) = C

                    CASE (CK_'u') ! expecting 4 hexadecimal digits after
! the escape character    [\uXXXX]

!for now, we are just returning them as is
![not checking to see if it is a valid hex value]
!
! Example:
!   123456
!   \uXXXX

                        IF (I+4<=N) THEN

! validate the hex string:
                            IF (VALID_JSON_HEX(STR(I+1:I+4))) THEN
                                M = M + 1
                                STR_TMP(M:M+5) = STR(I-1:I+4)
                                I = I + 4
                                M = M + 5
                            ELSE
                                ERROR_MESSAGE = 'Error in unescape_string:'//&
                                                ' Invalid hexadecimal sequence in string "'//&
                                                TRIM(STR)//'" ['//STR(I-1:I+4)//']'
                                IF (ALLOCATED(STR_TMP)) DEALLOCATE(STR_TMP)
                                RETURN
                            END IF
                        ELSE
                            ERROR_MESSAGE = 'Error in unescape_string:'//&
                                            ' Invalid hexadecimal sequence in string "'//&
                                            TRIM(STR)//'" ['//STR(I-1:)//']'
                            IF (ALLOCATED(STR_TMP)) DEALLOCATE(STR_TMP)
                            RETURN
                        END IF

                    CASE DEFAULT

!unknown escape character
                        ERROR_MESSAGE = 'Error in unescape_string:'//&
                                        ' unknown escape sequence in string "'//&
                                        TRIM(STR)//'" ['//BACKSLASH//C//']'
                        IF (ALLOCATED(STR_TMP)) DEALLOCATE(STR_TMP)
                        RETURN

                    END SELECT

                ELSE
! an escape character is the last character in
! the string. This is an error.
                    ERROR_MESSAGE = 'Error in unescape_string:'//&
                                    ' invalid escape character in string "'//&
                                    TRIM(STR)//'"'
                    IF (ALLOCATED(STR_TMP)) DEALLOCATE(STR_TMP)
                    RETURN
                END IF

            ELSE
                M = M + 1
                STR_TMP(M:M) = C
            END IF

        END DO

!trim trailing space:
        STR = STR_TMP(1:M)

    END IF

    END SUBROUTINE UNESCAPE_STRING
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!  date: 6/14/2014
!
!  Returns true if the string is a valid 4-digit hex string.
!
!!- Examples
!```fortran
!    valid_json_hex('0000')  !returns true
!    valid_json_hex('ABC4')  !returns true
!    valid_json_hex('AB')    !returns false (< 4 characters)
!    valid_json_hex('WXYZ')  !returns false (invalid characters)
!```

    PURE FUNCTION VALID_JSON_HEX(STR) RESULT(VALID)

    IMPLICIT NONE

    LOGICAL(LK)                         :: VALID  !! is str a value 4-digit hex string
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR    !! the string to check.

    INTEGER(IK) :: N  !! length of `str`
    INTEGER(IK) :: I  !! counter

!> an array of the valid hex characters
    CHARACTER(KIND=CK,LEN=1),DIMENSION(22),PARAMETER :: VALID_CHARS = &
        [ (ACHAR(I),I=48,57), & ! decimal digits
          (ACHAR(I),I=65,70), & ! capital A-F
          (ACHAR(I),I=97,102) ] ! lowercase a-f

!initialize
    VALID = .FALSE.

!check all the characters in the string:
    N = LEN(STR)
    IF (N==4) THEN
        DO I=1,N
            IF (.NOT. ANY(STR(I:I)==VALID_CHARS)) RETURN
        END DO
        VALID = .TRUE.    !all are in the set, so it is OK
    END IF

    END FUNCTION VALID_JSON_HEX
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert string to unicode (CDK to CK).

    PURE FUNCTION TO_UNI(STR)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: STR
    CHARACTER(KIND=CK,LEN=LEN(STR))       :: TO_UNI

    TO_UNI = STR

    END FUNCTION TO_UNI
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  Convert array of strings to unicode (CDK to CK).
!
!@note JW: may be able to remove this by making [[to_uni]] PURE ELEMENTAL ?

    PURE FUNCTION TO_UNI_VEC(STR)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*), DIMENSION(:), INTENT(IN)   :: STR
    CHARACTER(KIND=CK,LEN=LEN(STR)), DIMENSION(SIZE(STR)) :: TO_UNI_VEC

    TO_UNI_VEC = STR

    END FUNCTION TO_UNI_VEC
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CK`//`CDK` operator.

    PURE FUNCTION UCS4_JOIN_DEFAULT(UCS4_STR,DEF_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    CHARACTER(KIND=CK,LEN=(LEN(UCS4_STR)+LEN(DEF_STR))) :: RES

    RES = UCS4_STR//TO_UNICODE(DEF_STR)

    END FUNCTION UCS4_JOIN_DEFAULT
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CDK`//`CK` operator.

    PURE FUNCTION DEFAULT_JOIN_UCS4(DEF_STR,UCS4_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    CHARACTER(KIND=CK,LEN=(LEN(DEF_STR)+LEN(UCS4_STR))) :: RES

    RES = TO_UNICODE(DEF_STR)//UCS4_STR

    END FUNCTION DEFAULT_JOIN_UCS4
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CK`==`CDK` operator.

    PURE ELEMENTAL FUNCTION UCS4_COMP_DEFAULT(UCS4_STR,DEF_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    LOGICAL(LK) :: RES

    RES = ( UCS4_STR == TO_UNICODE(DEF_STR) )

    END FUNCTION UCS4_COMP_DEFAULT
!*****************************************************************************************

!*****************************************************************************************
!> author: Izaak Beekman
!
!  `CDK`==`CK` operator.

    PURE ELEMENTAL FUNCTION DEFAULT_COMP_UCS4(DEF_STR,UCS4_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    LOGICAL(LK) :: RES

    RES = (TO_UNICODE(DEF_STR) == UCS4_STR)

    END FUNCTION DEFAULT_COMP_UCS4
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  `CK`/=`CDK` operator.

    PURE ELEMENTAL FUNCTION UCS4_NEQ_DEFAULT(UCS4_STR,DEF_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    LOGICAL(LK) :: RES

    RES = ( UCS4_STR /= TO_UNICODE(DEF_STR) )

    END FUNCTION UCS4_NEQ_DEFAULT
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  `CDK`/=`CK` operator.

    PURE ELEMENTAL FUNCTION DEFAULT_NEQ_UCS4(DEF_STR,UCS4_STR) RESULT(RES)

    IMPLICIT NONE

    CHARACTER(KIND=CDK,LEN=*), INTENT(IN) :: DEF_STR
    CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: UCS4_STR
    LOGICAL(LK) :: RES

    RES = (TO_UNICODE(DEF_STR) /= UCS4_STR)

    END FUNCTION DEFAULT_NEQ_UCS4
!*****************************************************************************************

!*****************************************************************************************
!> author: Jacob Williams
!
!  Returns lowercase version of the `CK` string.

    PURE FUNCTION LOWERCASE_STRING(STR) RESULT(S_LOWER)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR      !! input string
    CHARACTER(KIND=CK,LEN=(LEN(STR)))   :: S_LOWER  !! lowercase version of the string

    INTEGER :: I  !! counter
    INTEGER :: J  !! index of uppercase character

    S_LOWER = STR

    DO I = 1, LEN_TRIM(STR)
        J = INDEX(UPPER,S_LOWER(I:I))
        IF (J>0) S_LOWER(I:I) = LOWER(J:J)
    END DO

    END FUNCTION LOWERCASE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Replace all occurrences of `s1` in `str` with `s2`.
!
!  A case-sensitive match is used.
!
!@note `str` must be allocated.

    PURE SUBROUTINE REPLACE_STRING(STR,S1,S2)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE,INTENT(INOUT) :: STR
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: S1
    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: S2

    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: TMP  !! temporary string for accumulating result
    INTEGER(IK) :: I      !! counter
    INTEGER(IK) :: N      !! for accumulating the string
    INTEGER(IK) :: ILEN   !! length of `str` string
    INTEGER(IK) :: ILEN1  !! length of `s1` string

    IF (LEN(STR)>0) THEN

        TMP = CK_''  ! initialize
        ILEN1 = LEN(S1)

!     .
! '123ab789'

        DO
            ILEN = LEN(STR)
            I = INDEX(STR,S1)
            IF (I>0) THEN
                IF (I>1) TMP = TMP//STR(1:I-1)
                TMP = TMP//S2 ! replace s1 with s2 in new string
                N = I+ILEN1   ! start of remainder of str to keep
                IF (N<=ILEN) THEN
                    STR = STR(N:ILEN)
                ELSE
! done
                    EXIT
                END IF
            ELSE
! done: get remainder of string
                TMP = TMP//STR
                EXIT
            END IF
        END DO

        STR = TMP

    END IF

    END SUBROUTINE REPLACE_STRING
!*****************************************************************************************

!*****************************************************************************************
!>
!  Decode a string from the "JSON Pointer" RFC 6901 format.
!
!  It replaces `~1` with `/` and `~0` with `~`.

    PURE FUNCTION DECODE_RFC6901(STR) RESULT(STR_OUT)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR_OUT

    STR_OUT = STR

    CALL REPLACE_STRING(STR_OUT,TILDE//CK_'1',SLASH)
    CALL REPLACE_STRING(STR_OUT,TILDE//CK_'0',TILDE)

    END FUNCTION DECODE_RFC6901
!*****************************************************************************************

!*****************************************************************************************
!>
!  Encode a string into the "JSON Pointer" RFC 6901 format.
!
!  It replaces `~` with `~0` and `/` with `~1`.

    PURE FUNCTION ENCODE_RFC6901(STR) RESULT(STR_OUT)

    IMPLICIT NONE

    CHARACTER(KIND=CK,LEN=*),INTENT(IN) :: STR
    CHARACTER(KIND=CK,LEN=:),ALLOCATABLE :: STR_OUT

    STR_OUT = STR

    CALL REPLACE_STRING(STR_OUT,TILDE,TILDE//CK_'0')
    CALL REPLACE_STRING(STR_OUT,SLASH,TILDE//CK_'1')

    END FUNCTION ENCODE_RFC6901
!*****************************************************************************************

    END MODULE ModLib_JSONString
!*****************************************************************************************

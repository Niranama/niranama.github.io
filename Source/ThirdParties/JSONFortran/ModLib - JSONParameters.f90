!- 1 "C:\Research\Projects-2024\ThirdParties\JSON Fortran\src\json_parameters.F90"
!*****************************************************************************************
!> author: Jacob Williams
!  license: BSD
!
!  Other parameters used by JSON-Fortran.
!  This is a low-level module not meant to be used by a JSON-Fortran user.
!
!--- License
!  * JSON-Fortran is released under a BSD-style license.
!    See the [LICENSE](https://github.com/jacobwilliams/json-fortran/blob/master/LICENSE)
!    file for details.

    MODULE ModLib_JSONParameters

    USE ModLib_JSONKinds

    IMPLICIT NONE

    PUBLIC

    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: JSON_EXT = '.json'   !! JSON file extension

! The types of JSON data.
    INTEGER(IK),PARAMETER :: JSON_UNKNOWN   = 0  !! Unknown JSON data type
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_NULL      = 1  !! Null JSON data type
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_OBJECT    = 2  !! Object JSON data type
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_ARRAY     = 3  !! Array JSON data type
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_LOGICAL   = 4  !! Logical JSON data type (`logical(LK)`)
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_INTEGER   = 5  !! Integer JSON data type (`integer(IK)`)
!! (see [[json_file_variable_info]] and [[json_info]]).
    INTEGER(IK),PARAMETER :: JSON_REAL      = 6  !! Real number JSON data type (`real(RK)`)
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_STRING    = 7  !! String JSON data type (`character(kind=CK)`)
!! (see [[json_file_variable_info]] and [[json_info]])
    INTEGER(IK),PARAMETER :: JSON_DOUBLE    = JSON_REAL  !! Equivalent to `json_real` for
!! backward compatibility.

!special JSON characters
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: SPACE           = CK_' '  !! space character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: START_OBJECT    = CK_'{'  !! start of a JSON object
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: END_OBJECT      = CK_'}'  !! end of a JSON object
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: START_ARRAY     = CK_'['  !! start of a JSON array
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: END_ARRAY       = CK_']'  !! end of a JSON array
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: DELIMITER       = CK_','  !! delimiter for JSON
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: COLON_CHAR      = CK_':'  !! colon character for JSON
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: START_ARRAY_ALT = CK_'('  !! alternate start of JSON array for
!! [[json_get_by_path_default]]
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: END_ARRAY_ALT   = CK_')'  !! alternate end of JSON array for
!! [[json_get_by_path_default]]
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: ROOT            = ACHAR(36, KIND=CK)  !! (`$`) root for [[json_get_bY_PATH_DEFAULT]]
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: THIS            = CK_'@'  !! 'this' for [[json_get_by_path_default]]
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: DOT             = CK_'.'  !! path separator for [[json_get_by_path_dEFAULT]]
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: TILDE           = CK_'~'  !! RFC 6901 escape character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: SINGLE_QUOTE    = CK_"'"  !! for JSONPath bracket-notation
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: QUOTATION_MARK  = CK_'"'  !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: BSPACE          = ACHAR(8,  KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: HORIZONTAL_TAB  = ACHAR(9,  KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: NEWLINE         = ACHAR(10, KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: FORMFEED        = ACHAR(12, KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: CARRIAGE_RETURN = ACHAR(13, KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: SLASH           = ACHAR(47, KIND=CK) !! JSON special character
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: BACKSLASH       = ACHAR(92, KIND=CK) !! JSON special character

!> default real number format statement (for writing real values to strings and files).
!  Note that this can be overridden by calling [[json_initialize]].
!- 75

    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: DEFAULT_REAL_FMT = '(ss,E27.17E4)'


    CHARACTER(KIND=CK,LEN=*),PARAMETER :: STAR = CK_'*' !! for invalid numbers and
!! list-directed real output

!- 86

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: UPPER = CK_'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !! uppercase characters
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: LOWER = CK_'abcdefghijklmnopqrstuvwxyz' !! lowercase characters


!- 96

    CHARACTER(KIND=CK,LEN=*),PARAMETER :: NULL_STR  = CK_'null'  !! JSON Null variable string
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: TRUE_STR  = CK_'true'  !! JSON logical True string
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: FALSE_STR = CK_'false' !! JSON logical False string


    INTEGER, PRIVATE :: I_      !! just a counter for `control_chars` array
    CHARACTER(KIND=CK,LEN=*),DIMENSION(32),PARAMETER :: CONTROL_CHARS = &
        [(ACHAR(I_,KIND=CK),I_=1,31), ACHAR(127,KIND=CK)] !! Control characters, possibly in unicode

!find out the precision of the floating point number system
!and set safety factors
    INTEGER(IK),PARAMETER :: RP_SAFETY_FACTOR = 1_IK
    INTEGER(IK),PARAMETER :: RP_ADDL_SAFETY = 2_IK
    INTEGER(IK),PARAMETER :: REAL_PRECISION = RP_SAFETY_FACTOR*PRECISION(1.0_RK) + &
                                              RP_ADDL_SAFETY

!Get the number of possible digits in the exponent when using decimal number system
    INTEGER(IK),PARAMETER :: MAXEXP = MAXEXPONENT(1.0_RK)
    INTEGER(IK),PARAMETER :: MINEXP = MINEXPONENT(1.0_RK)
    INTEGER(IK),PARAMETER :: REAL_EXPONENT_DIGITS = FLOOR( 1_IK + LOG10( &
                                  REAL(MAX(MAXEXP,ABS(MAXEXP)),&
                                  KIND=RK) ) )

    INTEGER(IK),PARAMETER :: MAX_NUMERIC_STR_LEN = REAL_PRECISION + REAL_EXPONENT_DIGITS + 6_IK
!! 6 = sign + leading 0 + decimal + 'E' + exponent sign + 1 extra
    CHARACTER(KIND=CDK,LEN=*),PARAMETER :: INT_FMT  = '(ss,I0)' !! minimum width format for integers

    INTEGER(IK),PARAMETER :: MAX_INTEGER_STR_LEN = 256_IK !! maximum string length of an integer.
!! This is totally arbitrary (any way
!! to get the compiler to tell us this?)

    INTEGER(IK),PARAMETER :: CHUNK_SIZE = 256_IK  !! for allocatable strings: allocate chunks of this size
    INTEGER(IK),PARAMETER :: UNIT2STR = -1_IK  !! unit number to cause stuff to be
!! output to strings rather than files.
!! See 9.5.6.12 in the F2003/08 standard
    CHARACTER(KIND=CK,LEN=*),PARAMETER :: BLANK_CHUNK = REPEAT(SPACE, CHUNK_SIZE) !! a blank string

    INTEGER(IK),PARAMETER :: SEQ_CHUNK_SIZE = 256_IK !! chunk size for reading sequential files

    INTEGER(IK),PARAMETER :: STREAM_CHUNK_SIZE = 256_IK !! chunk size for reading stream files

    INTEGER(IK),PARAMETER :: PRINT_STR_CHUNK_SIZE = 1000_IK !! chunk size for writing JSON to a string

    INTEGER(IK),PARAMETER :: PUSHED_CHAR_SIZE = 10_IK !! size for `pushed_char`
!! array in [[json_core(type)]]

    END MODULE ModLib_JSONParameters
!*****************************************************************************************
